%%%-------------------------------------------------------------------
%%% @author Auction System
%%% @doc
%%% Auction handler process - manages individual auction lifecycle,
%%% bid ordering using logical timestamps, countdown timer resets,
%%% and winner determination.
%%% @end
%%%-------------------------------------------------------------------
-module(auction_handler).
-author("auction_system").

-behavior(gen_server).

%% API
-export([
    start_link/1,
    start_auction/1,
    place_bid/3,
    join_as_participant/2,
    join_as_spectator/2,
    leave_auction/2,
    get_auction_state/1,
    force_end/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

-record(state, {
    auction_id,
    item_name,
    min_bid,
    bid_increment,
    duration,               % Total duration in seconds
    remaining_time,         % Remaining seconds
    start_time,             % Timestamp when auction started
    status,                 % waiting | active | completed
    participants = [],      % List of {Username, Pid} tuples
    spectators = [],        % List of {Username, Pid} tuples
    bids = [],             % List of {Username, Amount, Timestamp} tuples (ordered)
    highest_bid = none,     % {Username, Amount, Timestamp}
    timer_ref = none,       % Reference to countdown timer
    creator,
    logical_clock = 0       % Lamport logical clock for bid ordering
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(AuctionId) ->
    gen_server:start_link(?MODULE, [AuctionId], []).

%% @doc Start the auction (transition from waiting to active)
start_auction(Pid) ->
    gen_server:call(Pid, start_auction).

%% @doc Place a bid on the auction
place_bid(Pid, Username, Amount) ->
    gen_server:call(Pid, {place_bid, Username, Amount}).

%% @doc Join auction as a participant (must be in waitlist)
join_as_participant(Pid, Username) ->
    gen_server:call(Pid, {join_participant, Username, self()}).

%% @doc Join auction as a spectator
join_as_spectator(Pid, Username) ->
    gen_server:call(Pid, {join_spectator, Username, self()}).

%% @doc Leave the auction
leave_auction(Pid, Username) ->
    gen_server:cast(Pid, {leave, Username}).

%% @doc Get current auction state
get_auction_state(Pid) ->
    gen_server:call(Pid, get_state).

%% @doc Force end the auction (for testing/admin)
force_end(Pid) ->
    gen_server:cast(Pid, force_end).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([AuctionId]) ->
    io:format("[AUCTION ~p] Initializing handler~n", [AuctionId]),
    
    %% Load auction details from mnesia
    case mnesia_db:get_auction(AuctionId) of
        {ok, Auction} ->
            StartTime = element(7, Auction),  % start_time field
            Duration = element(6, Auction),   % duration field
            CurrentTime = erlang:system_time(second),
            
            %% Calculate status based on timestamps
            Status = calculate_status(StartTime, Duration, CurrentTime),
            
            %% Calculate remaining time
            EndTime = StartTime + Duration,
            RemainingTime = max(0, EndTime - CurrentTime),
            
            State = #state{
                auction_id = AuctionId,
                item_name = element(4, Auction),      % item_name field
                creator = element(3, Auction),         % creator field
                min_bid = element(5, Auction),         % min_bid field
                bid_increment = 1,                     % Default value, no longer in DB
                duration = Duration,
                remaining_time = RemainingTime,
                start_time = StartTime,
                status = Status,
                participants = [],
                spectators = [],
                bids = [],
                highest_bid = none
            },
            
            %% Start timer if auction is active
            NewState = case Status of
                active ->
                    TimerRef = erlang:send_after(1000, self(), tick),
                    State#state{timer_ref = TimerRef};
                _ ->
                    State
            end,
            
            io:format("[AUCTION ~p] Handler initialized: ~p (status: ~p)~n", 
                     [AuctionId, State#state.item_name, Status]),
            {ok, NewState};
        {error, Reason} ->
            io:format("[AUCTION ~p] Failed to load: ~p~n", [AuctionId, Reason]),
            {stop, {error, Reason}}
    end.

%%%===================================================================
%%% Helper Functions
%%%===================================================================

%% @doc Calculate auction status based on timestamps
calculate_status(StartTime, Duration, CurrentTime) ->
    EndTime = StartTime + Duration,
    if
        CurrentTime < StartTime -> waiting;
        CurrentTime >= EndTime -> completed;
        true -> active
    end.

%%%===================================================================
%%% gen_server callbacks - Auction Control
%%%===================================================================

handle_call(start_auction, _From, State = #state{status = waiting}) ->
    io:format("[AUCTION ~p] Starting auction~n", [State#state.auction_id]),
    
    %% Start countdown timer (tick every second)
    TimerRef = erlang:send_after(1000, self(), tick),
    
    NewState = State#state{
        status = active,
        timer_ref = TimerRef,
        start_time = erlang:system_time(second)
    },
    
    %% Notify all participants and spectators
    broadcast_state_update(NewState),
    
    {reply, ok, NewState};

handle_call(start_auction, _From, State) ->
    {reply, {error, not_waiting}, State};

handle_call({place_bid, Username, Amount}, _From, State = #state{status = active}) ->
    io:format("[AUCTION ~p] Bid received: ~p bids ~p~n", 
              [State#state.auction_id, Username, Amount]),
    
    %% Check if user is a participant
    case lists:keyfind(Username, 1, State#state.participants) of
        false ->
            {reply, {error, not_participant}, State};
        _ ->
            %% Validate bid amount
            case validate_bid(Amount, State) of
                ok ->
                    %% Increment logical clock
                    NewClock = State#state.logical_clock + 1,
                    Timestamp = {NewClock, node(), erlang:monotonic_time()},
                    
                    %% Check user balance
                    case mnesia_db:get_user_balance(Username) of
                        {ok, Balance} when Balance >= Amount ->
                            %% Add bid to mnesia
                            mnesia_db:add_bid(State#state.auction_id, Username, Amount, Timestamp),
                            
                            %% Update state with new bid
                            NewBid = {Username, Amount, Timestamp},
                            NewBids = [NewBid | State#state.bids],
                            
                            %% Reset countdown timer
                            cancel_timer(State#state.timer_ref),
                            NewTimerRef = erlang:send_after(1000, self(), tick),
                            
                            NewState = State#state{
                                bids = NewBids,
                                highest_bid = NewBid,
                                remaining_time = State#state.duration,  % Reset timer!
                                timer_ref = NewTimerRef,
                                logical_clock = NewClock
                            },
                            
                            io:format("[AUCTION ~p] Bid accepted! Timer reset. New highest: ~p~n", 
                                     [State#state.auction_id, Amount]),
                            
                            %% Broadcast to all participants and spectators
                            broadcast_state_update(NewState),
                            
                            {reply, {ok, accepted}, NewState};
                        {ok, Balance} ->
                            io:format("[AUCTION ~p] Insufficient balance: ~p < ~p~n", 
                                     [State#state.auction_id, Balance, Amount]),
                            {reply, {error, insufficient_balance}, State};
                        _Error ->
                            {reply, {error, balance_check_failed}, State}
                    end;
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end
    end;

handle_call({place_bid, _Username, _Amount}, _From, State) ->
    {reply, {error, auction_not_active}, State};

handle_call({join_participant, Username, ClientPid}, _From, State) ->
    io:format("[AUCTION ~p] Participant joining: ~p~n", [State#state.auction_id, Username]),
    
    %% Add to participants (no waitlist check needed)
    NewParticipants = [{Username, ClientPid} | State#state.participants],
    NewState = State#state{participants = NewParticipants},
    
    %% Monitor the client process
    erlang:monitor(process, ClientPid),
    
    io:format("[AUCTION ~p] Participant added: ~p~n", 
             [State#state.auction_id, Username]),
    {reply, {ok, get_state_info(NewState)}, NewState};

handle_call({join_spectator, Username, ClientPid}, _From, State) ->
    io:format("[AUCTION ~p] Spectator joining: ~p~n", [State#state.auction_id, Username]),
    
    NewSpectators = [{Username, ClientPid} | State#state.spectators],
    NewState = State#state{spectators = NewSpectators},
    
    %% Monitor the client process
    erlang:monitor(process, ClientPid),
    
    {reply, {ok, get_state_info(NewState)}, NewState};

handle_call(get_state, _From, State) ->
    {reply, {ok, get_state_info(State)}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({leave, Username}, State) ->
    io:format("[AUCTION ~p] User leaving: ~p~n", [State#state.auction_id, Username]),
    
    NewParticipants = lists:keydelete(Username, 1, State#state.participants),
    NewSpectators = lists:keydelete(Username, 1, State#state.spectators),
    
    NewState = State#state{
        participants = NewParticipants,
        spectators = NewSpectators
    },
    {noreply, NewState};

handle_cast(force_end, State) ->
    io:format("[AUCTION ~p] Forcing end~n", [State#state.auction_id]),
    cancel_timer(State#state.timer_ref),
    NewState = end_auction(State),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% Handle countdown tick
handle_info(tick, State) ->
    CurrentTime = erlang:system_time(second),
    NewStatus = calculate_status(State#state.start_time, State#state.duration, CurrentTime),
    
    case NewStatus of
        active ->
            %% Calculate remaining time
            EndTime = State#state.start_time + State#state.duration,
            RemainingTime = max(0, EndTime - CurrentTime),
            NewState = State#state{remaining_time = RemainingTime, status = active},
            
            %% Schedule next tick
            TimerRef = erlang:send_after(1000, self(), tick),
            
            %% Broadcast time update every 5 seconds or last 10 seconds
            case (RemainingTime rem 5 == 0) orelse (RemainingTime =< 10) of
                true -> broadcast_state_update(NewState);
                false -> ok
            end,
            
            {noreply, NewState#state{timer_ref = TimerRef}};
        completed ->
            %% Auction ended
            io:format("[AUCTION ~p] Time's up! Ending auction~n", [State#state.auction_id]),
            NewState = end_auction(State),
            {noreply, NewState};
        _ ->
            %% Waiting or other status, ignore tick
            {noreply, State}
    end;

%% Handle monitored process going down
handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    io:format("[AUCTION ~p] Client disconnected: ~p~n", [State#state.auction_id, Pid]),
    
    %% Remove from participants and spectators
    NewParticipants = lists:filter(fun({_, P}) -> P =/= Pid end, State#state.participants),
    NewSpectators = lists:filter(fun({_, P}) -> P =/= Pid end, State#state.spectators),
    
    NewState = State#state{
        participants = NewParticipants,
        spectators = NewSpectators
    },
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, State) ->
    io:format("[AUCTION ~p] Terminating: ~p~n", [State#state.auction_id, Reason]),
    cancel_timer(State#state.timer_ref),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc Validate that a bid meets requirements
validate_bid(Amount, State) ->
    case State#state.highest_bid of
        none ->
            %% First bid must be at least minimum
            if Amount >= State#state.min_bid -> ok;
               true -> {error, below_minimum}
            end;
        {_User, CurrentHighest, _Timestamp} ->
            %% Subsequent bids must exceed current by increment
            Required = CurrentHighest + State#state.bid_increment,
            if Amount >= Required -> ok;
               true -> {error, insufficient_increment}
            end
    end.

%% @doc End the auction and determine winner
end_auction(State) ->
    cancel_timer(State#state.timer_ref),
    
    case State#state.highest_bid of
        none ->
            io:format("[AUCTION ~p] No bids received~n", [State#state.auction_id]),
            mnesia_db:update_auction_winner(State#state.auction_id, none, 0),
            broadcast_auction_ended(State, none, 0);
        {Winner, Amount, _Timestamp} ->
            io:format("[AUCTION ~p] Winner: ~p with bid ~p~n", 
                     [State#state.auction_id, Winner, Amount]),
            mnesia_db:update_auction_winner(State#state.auction_id, Winner, Amount),
            broadcast_auction_ended(State, Winner, Amount)
    end,
    
    State#state{
        status = completed,
        timer_ref = none
    }.

%% @doc Get auction state as a map for clients
get_state_info(State) ->
    #{
        auction_id => State#state.auction_id,
        item_name => State#state.item_name,
        creator => State#state.creator,
        min_bid => State#state.min_bid,
        bid_increment => State#state.bid_increment,
        duration => State#state.duration,
        remaining_time => State#state.remaining_time,
        status => State#state.status,
        highest_bid => case State#state.highest_bid of
            none -> none;
            {User, Amount, _} -> #{username => User, amount => Amount}
        end,
        bid_count => length(State#state.bids),
        participant_count => length(State#state.participants),
        spectator_count => length(State#state.spectators)
    }.

%% @doc Broadcast state update to all connected clients
broadcast_state_update(State) ->
    StateInfo = get_state_info(State),
    Message = {auction_update, StateInfo},
    
    %% Send to all participants
    lists:foreach(fun({_Username, Pid}) ->
        Pid ! Message
    end, State#state.participants),
    
    %% Send to all spectators
    lists:foreach(fun({_Username, Pid}) ->
        Pid ! Message
    end, State#state.spectators).

%% @doc Broadcast auction end notification
broadcast_auction_ended(State, Winner, Amount) ->
    Message = {auction_ended, #{
        auction_id => State#state.auction_id,
        winner => Winner,
        winning_bid => Amount
    }},
    
    lists:foreach(fun({_Username, Pid}) ->
        Pid ! Message
    end, State#state.participants),
    
    lists:foreach(fun({_Username, Pid}) ->
        Pid ! Message
    end, State#state.spectators).

%% @doc Cancel a timer if it exists
cancel_timer(none) -> ok;
cancel_timer(TimerRef) ->
    erlang:cancel_timer(TimerRef),
    ok.
