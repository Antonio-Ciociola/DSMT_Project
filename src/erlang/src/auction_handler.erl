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
    start_link/3,
    start_auction/1,
    place_bid/3,
    join_as_participant/3,
    join_as_spectator/3,
    leave_auction/2,
    get_auction_state/1,
    force_end/1,
    post_auction_finish_to_java/4
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
    duration,               % Initial duration in seconds
    total_duration,         % Total duration including all time extensions
    remaining_time,         % Remaining seconds
    start_time,             % Timestamp when auction started
    status,                 % waiting | active | completed
    participants = [],      % List of {Username, Pid} tuples
    spectators = [],        % List of {Username, Pid} tuples
    bids = [],             % List of {Username, Amount, Timestamp} tuples (ordered)
    highest_bid = none,     % {Username, Amount, Timestamp}
    timer_ref = none,       % Reference to countdown timer
    creator,
    logical_clock = 0,      % Lamport logical clock for bid ordering
    time_increment_bid = 0  % Time to add when bid is placed (seconds)
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start auction handler with default parameters (for backward compatibility)
start_link(AuctionId) ->
    start_link(AuctionId, 1, 0).

%% @doc Start auction handler with bid increment and time increment
start_link(AuctionId, MinIncrementBid, TimeIncrementBid) ->
    gen_server:start_link(?MODULE, [AuctionId, MinIncrementBid, TimeIncrementBid], []).

%% @doc Start the auction (transition from waiting to active)
start_auction(Pid) ->
    gen_server:call(Pid, start_auction).

%% @doc Place a bid on the auction
place_bid(Pid, Username, Amount) ->
    gen_server:call(Pid, {place_bid, Username, Amount}).

%% @doc Join auction as a participant
join_as_participant(Pid, Username, ClientPid) ->
    gen_server:call(Pid, {join_participant, Username, ClientPid}).

%% @doc Join auction as a spectator
join_as_spectator(Pid, Username, ClientPid) ->
    gen_server:call(Pid, {join_spectator, Username, ClientPid}).

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
    init([AuctionId, 1, 0]);

init([AuctionId, MinIncrementBid, TimeIncrementBid]) ->
    io:format("[AUCTION ~p] Initializing handler~n", [AuctionId]),
    
    %% Register this process with a name based on auction ID
    ProcessName = list_to_atom("auction_" ++ AuctionId),
    register(ProcessName, self()),
    
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
                bid_increment = MinIncrementBid,
                duration = Duration,
                total_duration = Duration,             % Initialize with base duration
                remaining_time = RemainingTime,
                start_time = StartTime,
                status = Status,
                participants = [],
                spectators = [],
                bids = [],
                highest_bid = none,
                time_increment_bid = TimeIncrementBid
            },
            
            %% Start timer if auction is active
            NewState = case Status of
                active ->
                    TimerRef = erlang:send_after(1000, self(), tick),
                    io:format("[AUCTION ~p] Timer started! Remaining time: ~ps~n", 
                              [AuctionId, RemainingTime]),
                    State#state{timer_ref = TimerRef};
                _ ->
                    io:format("[AUCTION ~p] Timer NOT started (status: ~p)~n", 
                              [AuctionId, Status]),
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
                            
                            %% Update auction winner in Mnesia
                            mnesia_db:update_auction_winner(State#state.auction_id, Username, Amount),
                            
                            %% Update state with new bid
                            NewBid = {Username, Amount, Timestamp},
                            NewBids = [NewBid | State#state.bids],
                            
                            %% Add time increment to remaining time (instead of full reset)
                            NewRemainingTime = State#state.remaining_time + State#state.time_increment_bid,
                            
                            %% Update total duration to include this time extension
                            NewTotalDuration = State#state.total_duration + State#state.time_increment_bid,
                            
                            %% Reset countdown timer
                            cancel_timer(State#state.timer_ref),
                            NewTimerRef = erlang:send_after(1000, self(), tick),
                            
                            NewState = State#state{
                                bids = NewBids,
                                highest_bid = NewBid,
                                remaining_time = NewRemainingTime,
                                total_duration = NewTotalDuration,
                                timer_ref = NewTimerRef,
                                logical_clock = NewClock
                            },
                            
                            io:format("[AUCTION ~p] Bid accepted! Time added: ~p seconds. New remaining: ~p~n", 
                                     [State#state.auction_id, State#state.time_increment_bid, NewRemainingTime]),
                            io:format("[AUCTION ~p] Current participants: ~p~n",
                                     [State#state.auction_id, NewState#state.participants]),
                            io:format("[AUCTION ~p] Current spectators: ~p~n",
                                     [State#state.auction_id, NewState#state.spectators]),
                            
                            %% Broadcast to all participants and spectators on THIS node
                            broadcast_state_update(NewState),
                            
                            %% Notify ALL nodes with this auction by sending to registered name
                            io:format("[AUCTION ~p] About to broadcast to all nodes~n", [State#state.auction_id]),
                            broadcast_to_all_nodes(State#state.auction_id, {bid_update, Username, Amount}),
                            
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
    
    %% Add to participants
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
    io:format("[AUCTION ~p] Tick received. Remaining: ~ps, Status: ~p~n", 
              [State#state.auction_id, State#state.remaining_time, State#state.status]),
    
    %% Only process tick if status is active
    case State#state.status of
        active ->
            %% Decrement remaining time
            NewRemainingTime = max(0, State#state.remaining_time - 1),
            
            %% Check if auction should end based on remaining time
            case NewRemainingTime of
                0 ->
                    %% Time's up - end the auction
                    io:format("[AUCTION ~p] Timer expired! Ending auction~n", [State#state.auction_id]),
                    NewState = end_auction(State#state{remaining_time = 0}),
                    broadcast_state_update(NewState),
                    {noreply, NewState};
                _ ->
                    %% Continue countdown
                    NewState = State#state{remaining_time = NewRemainingTime, status = active},
                    
                    %% Schedule next tick
                    TimerRef = erlang:send_after(1000, self(), tick),
                    
                    %% Broadcast time update every 5 seconds or when close to end
                    ShouldBroadcast = (NewRemainingTime rem 5 == 0) orelse (NewRemainingTime < 10),
                    case ShouldBroadcast of
                        true -> broadcast_state_update(NewState);
                        false -> ok
                    end,
                    
                    {noreply, NewState#state{timer_ref = TimerRef}}
            end;
        _ ->
            %% Not active, ignore tick
            io:format("[AUCTION ~p] Tick ignored (status: ~p)~n", 
                      [State#state.auction_id, State#state.status]),
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

%% Handle bid event from other nodes
handle_info({bid_update, Username, Amount}, State) ->
    io:format("[AUCTION ~p] Received bid update from cluster: ~p bid ~p~n", 
             [State#state.auction_id, Username, Amount]),
    
    %% Reload auction state from Mnesia to get latest winner
    case mnesia_db:get_auction(State#state.auction_id) of
        {ok, {auction, _, _, _, _, _, _, Winner, WinningBid, _, _}} ->
            %% Update local state with new highest bid from Mnesia
            NewBid = case Winner of
                none -> none;
                _ -> {Winner, WinningBid, erlang:system_time(second)}
            end,
            
            NewState = State#state{highest_bid = NewBid},
            
            %% Broadcast to local participants
            broadcast_state_update(NewState),
            {noreply, NewState};
        _ ->
            {noreply, State}
    end;

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
    
    io:format("~n========================================~n"),
    io:format("[AUCTION ENDED] ID: ~p~n", [State#state.auction_id]),
    io:format("[AUCTION ENDED] Item: ~p~n", [State#state.item_name]),
    io:format("[AUCTION ENDED] Total Bids: ~p~n", [length(State#state.bids)]),
    io:format("[AUCTION ENDED] Participants: ~p~n", [length(State#state.participants)]),
    
    case State#state.highest_bid of
        none ->
            io:format("[AUCTION ENDED] Result: NO WINNER (no bids)~n"),
            io:format("========================================~n~n"),
            mnesia_db:update_auction_winner(State#state.auction_id, none, 0),
            broadcast_auction_ended(State, none, 0),
            notify_master_auction_complete(State#state.auction_id, none, 0, State#state.total_duration);
        {Winner, Amount, _Timestamp} ->
            io:format("[AUCTION ENDED] WINNER: ~p~n", [Winner]),
            io:format("[AUCTION ENDED] Winning Bid: $~p~n", [Amount]),
            io:format("========================================~n~n"),
            mnesia_db:update_auction_winner(State#state.auction_id, Winner, Amount),
            broadcast_auction_ended(State, Winner, Amount),
            notify_master_auction_complete(State#state.auction_id, Winner, Amount, State#state.total_duration)
    end,
    
    State#state{
        status = completed,
        timer_ref = none
    }.

%% @doc Get auction state as a map for clients
get_state_info(State) ->
    #{
        auction_id => list_to_binary(State#state.auction_id),
        item_name => list_to_binary(State#state.item_name),
        creator => list_to_binary(State#state.creator),
        min_bid => State#state.min_bid,
        bid_increment => State#state.bid_increment,
        duration => State#state.duration,
        total_duration => State#state.total_duration,
        remaining_time => State#state.remaining_time,
        status => State#state.status,
        highest_bid => case State#state.highest_bid of
            none -> none;
            {User, Amount, _} -> #{username => list_to_binary(User), amount => Amount}
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
    %% Count total bids and participants
    BidCount = length(State#state.bids),
    ParticipantCount = length(State#state.participants),
    TotalDuration = State#state.total_duration,
    
    %% Convert winner to user_id (string)
    WinnerUserId = case Winner of
        none -> <<"none">>;
        UserId when is_list(UserId) -> list_to_binary(UserId);
        UserId when is_binary(UserId) -> UserId
    end,
    
    Message = {auction_ended, #{
        auction_id => State#state.auction_id,
        winner_user_id => WinnerUserId,
        winning_bid => Amount,
        bid_count => BidCount,
        participant_count => ParticipantCount,
        total_duration => TotalDuration
    }},
    
    %% Send to all participants
    lists:foreach(fun({_Username, Pid}) ->
        Pid ! Message
    end, State#state.participants),
    
    %% Send to all spectators
    lists:foreach(fun({_Username, Pid}) ->
        Pid ! Message
    end, State#state.spectators).

%% @doc Notify master node about auction completion
notify_master_auction_complete(AuctionId, Winner, Amount, TotalDuration) ->
    %% Only notify if we're on a slave node
    case node() of
        'auction@erlang-master' ->
            %% We ARE the master, handle it directly
            io:format("[AUCTION ~p] Master node handling auction completion directly~n", [AuctionId]),
            post_auction_finish_to_java(AuctionId, Winner, Amount, TotalDuration);
        _SlaveNode ->
            %% We're a slave, notify the master
            MasterNode = 'auction@erlang-master',
            io:format("[AUCTION ~p] Notifying master node ~p of completion~n", [AuctionId, MasterNode]),
            case rpc:call(MasterNode, slave_manager, handle_auction_complete, [AuctionId, Winner, Amount, TotalDuration]) of
                ok ->
                    io:format("[AUCTION ~p] Master notified successfully~n", [AuctionId]);
                {error, Reason} ->
                    io:format("[AUCTION ~p] Failed to notify master: ~p~n", [AuctionId, Reason]);
                Other ->
                    io:format("[AUCTION ~p] Unexpected response from master: ~p~n", [AuctionId, Other])
            end
    end.

%% @doc POST auction finish to Java backend
post_auction_finish_to_java(AuctionId, Winner, Amount, TotalDuration) ->
    spawn(fun() ->
        %% Java backend URL
        JavaUrl = "http://backend:8080/api/finish-auction",
        
        %% Convert AuctionId to string without "auction_" prefix if present
        AuctionIdStr = case AuctionId of
            "auction_" ++ Id -> Id;
            Id when is_list(Id) -> Id;
            Id when is_binary(Id) -> binary_to_list(Id)
        end,
        
        %% Prepare POST data
        {WinnerId, FinalPrice} = case Winner of
            none -> 
                {"0", "0.00"};
            UserId when is_list(UserId) -> 
                PriceStr = if 
                    is_float(Amount) -> float_to_list(Amount, [{decimals, 2}]);
                    is_integer(Amount) -> float_to_list(float(Amount), [{decimals, 2}]);
                    true -> lists:flatten(io_lib:format("~p", [Amount]))
                end,
                {UserId, PriceStr};
            UserId when is_binary(UserId) -> 
                PriceStr = if 
                    is_float(Amount) -> float_to_list(Amount, [{decimals, 2}]);
                    is_integer(Amount) -> float_to_list(float(Amount), [{decimals, 2}]);
                    true -> lists:flatten(io_lib:format("~p", [Amount]))
                end,
                {binary_to_list(UserId), PriceStr}
        end,
        
        TotalDurationStr = integer_to_list(TotalDuration),
        
        PostData = "auctionId=" ++ AuctionIdStr ++ 
                   "&winnerId=" ++ WinnerId ++ 
                   "&finalPrice=" ++ FinalPrice ++
                   "&totalDuration=" ++ TotalDurationStr,
        
        io:format("[AUCTION ~p] POSTing to Java: ~p~n", [AuctionId, PostData]),
        
        %% Make HTTP POST request
        case httpc:request(post, 
                          {JavaUrl, [], "application/x-www-form-urlencoded", PostData},
                          [{timeout, 5000}],
                          []) of
            {ok, {{_, 200, _}, _, ResponseBody}} ->
                io:format("[AUCTION ~p] Java finish-auction success: ~p~n", [AuctionId, ResponseBody]);
            {ok, {{_, StatusCode, _}, _, ResponseBody}} ->
                io:format("[AUCTION ~p] Java finish-auction failed (~p): ~p~n", [AuctionId, StatusCode, ResponseBody]);
            {error, Reason} ->
                io:format("[AUCTION ~p] Failed to POST to Java: ~p~n", [AuctionId, Reason])
        end
    end).

%% @doc Cancel a timer if it exists
cancel_timer(none) -> ok;
cancel_timer(TimerRef) ->
    erlang:cancel_timer(TimerRef),
    ok.

%% @doc Broadcast message to all auction handlers on all nodes
broadcast_to_all_nodes(AuctionId, Message) ->
    %% Send to all connected nodes
    Nodes = [node() | nodes()],
    io:format("[AUCTION ~p] Broadcasting to nodes: ~p~n", [AuctionId, Nodes]),
    lists:foreach(fun(Node) ->
        %% Try to send to auction handler on each node
        ProcessName = list_to_atom("auction_" ++ AuctionId),
        io:format("[AUCTION ~p] Looking for process ~p on node ~p~n", [AuctionId, ProcessName, Node]),
        case rpc:call(Node, erlang, whereis, [ProcessName]) of
            Pid when is_pid(Pid) ->
                io:format("[AUCTION ~p] Sending ~p to PID ~p on node ~p~n", [AuctionId, Message, Pid, Node]),
                Pid ! Message,
                io:format("[AUCTION ~p] Message sent successfully~n", [AuctionId]);
            undefined ->
                io:format("[AUCTION ~p] Process ~p not found on node ~p~n", [AuctionId, ProcessName, Node]);
            Other ->
                io:format("[AUCTION ~p] Unexpected whereis result on node ~p: ~p~n", [AuctionId, Node, Other])
        end
    end, Nodes).
