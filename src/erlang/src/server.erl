%%%-------------------------------------------------------------------
%%% @author Auction System
%%% @doc
%%% Main distributed server module. Each node runs this server to
%%% manage users, auctions, and coordinate with other nodes.
%%% Uses gen_server for reliability and distributed operation.
%%% @end
%%%-------------------------------------------------------------------
-module(server).
-author("auction_system").

-behavior(gen_server).

%% API
-export([
    start_link/0,
    stop/0,
    
    %% User management
    register_user/3,
    authenticate_user/2,
    get_balance/1,
    deposit_funds/2,
    
    %% External POST endpoints
    register_auction/3,
    register_auction_user/3,
    
    %% Auction management
    cancel_auction/1,
    get_active_auctions/0,
    get_completed_auctions/0,
    
    %% Auction participation
    start_auction/1,
    connect_to_auction/3,
    place_bid/3,
    
    %% Node management
    join_cluster/1,
    get_nodes/0,
    get_node_stats/0
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {
    node_name,
    auction_handlers = [],  % List of {AuctionId, Pid} managed by this node
    connected_users = []    % List of {Username, Pid} connected to this node
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start the server
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Stop the server
stop() ->
    gen_server:stop(?MODULE).

%% @doc Register a new user
register_user(Username, Password, InitialBalance) ->
    gen_server:call(?MODULE, {register_user, Username, Password, InitialBalance}).

%% @doc Authenticate user
authenticate_user(Username, Password) ->
    gen_server:call(?MODULE, {authenticate, Username, Password}).

%% @doc Get user balance
get_balance(Username) ->
    gen_server:call(?MODULE, {get_balance, Username}).

%% @doc Deposit funds to user account
deposit_funds(Username, Amount) ->
    gen_server:call(?MODULE, {deposit, Username, Amount}).

%% @doc Register an auction from external POST endpoint
register_auction(AuctionId, StartingPrice, MinDuration) ->
    gen_server:call(?MODULE, {register_auction, AuctionId, StartingPrice, MinDuration}).

%% @doc Register a user as active for an auction from external POST endpoint
register_auction_user(AuctionId, UserId, Balance) ->
    gen_server:call(?MODULE, {register_auction_user, AuctionId, UserId, Balance}).

%% @doc Cancel an auction (must be in waiting state)
cancel_auction(AuctionId) ->
    gen_server:call(?MODULE, {cancel_auction, AuctionId}).

%% @doc Get all active auctions
get_active_auctions() ->
    gen_server:call(?MODULE, get_active_auctions).

%% @doc Get all completed auctions
get_completed_auctions() ->
    gen_server:call(?MODULE, get_completed_auctions).

%% @doc Start a waiting auction
start_auction(AuctionId) ->
    gen_server:call(?MODULE, {start_auction, AuctionId}).

%% @doc Connect user to an auction (as participant or spectator)
connect_to_auction(AuctionId, Username, Role) ->
    gen_server:call(?MODULE, {connect_auction, AuctionId, Username, Role}).

%% @doc Place a bid in an auction
place_bid(AuctionId, Username, Amount) ->
    gen_server:call(?MODULE, {place_bid, AuctionId, Username, Amount}).

%% @doc Join an existing cluster
join_cluster(MasterNode) ->
    gen_server:call(?MODULE, {join_cluster, MasterNode}).

%% @doc Get list of nodes in cluster
get_nodes() ->
    gen_server:call(?MODULE, get_nodes).

%% @doc Get node statistics
get_node_stats() ->
    gen_server:call(?MODULE, get_stats).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    io:format("[SERVER] Initializing node ~p~n", [node()]),
    
    State = #state{
        node_name = node(),
        auction_handlers = [],
        connected_users = []
    },
    
    io:format("[SERVER] Node ~p ready~n", [node()]),
    {ok, State}.

%%%===================================================================
%%% User management handlers
%%%===================================================================

handle_call({register_user, Username, Password, InitialBalance}, _From, State) ->
    io:format("[SERVER] Registering user: ~p~n", [Username]),
    Result = mnesia_db:add_user(Username, Password, InitialBalance),
    {reply, Result, State};

handle_call({authenticate, Username, Password}, _From, State) ->
    io:format("[SERVER] Authenticating user: ~p~n", [Username]),
    case mnesia_db:get_user(Username) of
        {ok, User} ->
            StoredPassword = element(3, User),  % password field
            if StoredPassword == Password ->
                {reply, {ok, authenticated}, State};
            true ->
                {reply, {error, invalid_password}, State}
            end;
        Error ->
            {reply, Error, State}
    end;

handle_call({get_balance, Username}, _From, State) ->
    Result = mnesia_db:get_user_balance(Username),
    {reply, Result, State};

handle_call({deposit, Username, Amount}, _From, State) ->
    case mnesia_db:get_user_balance(Username) of
        {ok, CurrentBalance} ->
            NewBalance = CurrentBalance + Amount,
            Result = mnesia_db:update_user_balance(Username, NewBalance),
            {reply, Result, State};
        Error ->
            {reply, Error, State}
    end;

handle_call({add_balance, Username, Amount}, _From, State) ->
    Result = mnesia_db:add_balance(Username, Amount),
    {reply, Result, State};

handle_call({withdraw_balance, Username, Amount}, _From, State) ->
    Result = mnesia_db:withdraw_balance(Username, Amount),
    {reply, Result, State};

%%%===================================================================
%%% Auction management handlers
%%%===================================================================

handle_call({create_auction, Creator, ItemName, MinBid, BidIncrement, Duration, StartTime}, _From, State) ->
    %% Generate unique auction ID
    AuctionId = generate_auction_id(ItemName, Creator),
    io:format("[SERVER] Creating auction: ~p~n", [AuctionId]),
    
    %% Create auction in database
    case mnesia_db:add_auction(AuctionId, Creator, ItemName, MinBid, BidIncrement, Duration, StartTime) of
        {ok, _} ->
            %% Start auction handler process
            case auction_handler:start_link(AuctionId) of
                {ok, Pid} ->
                    NewHandlers = [{AuctionId, Pid} | State#state.auction_handlers],
                    io:format("[SERVER] Auction handler started: ~p -> ~p~n", [AuctionId, Pid]),
                    {reply, {ok, AuctionId}, State#state{auction_handlers = NewHandlers}};
                Error ->
                    io:format("[SERVER] Failed to start auction handler: ~p~n", [Error]),
                    {reply, {error, handler_failed}, State}
            end;
        Error ->
            {reply, Error, State}
    end;

handle_call({cancel_auction, AuctionId}, _From, State) ->
    io:format("[SERVER] Cancelling auction: ~p~n", [AuctionId]),
    Result = mnesia_db:cancel_auction(AuctionId),
    
    %% Stop the handler if it exists
    case lists:keyfind(AuctionId, 1, State#state.auction_handlers) of
        {AuctionId, Pid} ->
            gen_server:stop(Pid),
            NewHandlers = lists:keydelete(AuctionId, 1, State#state.auction_handlers),
            {reply, Result, State#state{auction_handlers = NewHandlers}};
        false ->
            {reply, Result, State}
    end;



handle_call(get_waiting_auctions, _From, State) ->
    Result = mnesia_db:get_waiting_auctions(),
    {reply, Result, State};

handle_call(get_active_auctions, _From, State) ->
    Result = mnesia_db:get_active_auctions(),
    {reply, Result, State};

handle_call(get_completed_auctions, _From, State) ->
    Result = mnesia_db:get_completed_auctions(),
    {reply, Result, State};

%%%===================================================================
%%% POST endpoint handlers
%%%===================================================================

handle_call({register_auction, AuctionId, StartingPrice, MinDuration}, _From, State) ->
    io:format("[SERVER] Registering auction via POST: ~p~n", [AuctionId]),
    
    %% Create auction in database with system as creator
    case mnesia_db:add_auction(AuctionId, "system", AuctionId, StartingPrice, 0, MinDuration, 0) of
        {ok, _} ->
            %% Start auction handler process
            case auction_handler:start_link(AuctionId) of
                {ok, Pid} ->
                    NewHandlers = [{AuctionId, Pid} | State#state.auction_handlers],
                    io:format("[SERVER] Auction handler started: ~p -> ~p~n", [AuctionId, Pid]),
                    {reply, ok, State#state{auction_handlers = NewHandlers}};
                Error ->
                    io:format("[SERVER] Failed to start auction handler: ~p~n", [Error]),
                    {reply, {error, handler_failed}, State}
            end;
        Error ->
            {reply, Error, State}
    end;

handle_call({register_auction_user, AuctionId, UserId, Balance}, _From, State) ->
    io:format("[SERVER] Registering user ~p for auction ~p with balance ~p~n", 
              [UserId, AuctionId, Balance]),
    
    %% Ensure user exists in database
    case mnesia_db:get_user(UserId) of
        {error, not_found} ->
            %% Create user with specified balance
            case mnesia_db:add_user(UserId, "no_password", Balance) of
                {ok, _} ->
                    io:format("[SERVER] Created user ~p with balance ~p~n", [UserId, Balance]),
                    {reply, ok, State};
                Error ->
                    {reply, Error, State}
            end;
        {ok, _User} ->
            %% User exists, update balance
            io:format("[SERVER] User ~p already exists, updating balance to ~p~n", [UserId, Balance]),
            case mnesia_db:update_user_balance(UserId, Balance) of
                ok ->
                    {reply, ok, State};
                Error ->
                    {reply, Error, State}
            end;
        Error ->
            {reply, Error, State}
    end;

%%%===================================================================
%%% Auction interaction handlers
%%%===================================================================

handle_call({start_auction, AuctionId}, _From, State) ->
    io:format("[SERVER] Starting auction: ~p~n", [AuctionId]),
    
    case lists:keyfind(AuctionId, 1, State#state.auction_handlers) of
        {AuctionId, Pid} ->
            Result = auction_handler:start_auction(Pid),
            {reply, Result, State};
        false ->
            {reply, {error, handler_not_found}, State}
    end;

handle_call({connect_auction, AuctionId, Username, Role}, {ClientPid, _Tag}, State) ->
    io:format("[SERVER] Connecting ~p to auction ~p as ~p~n", [Username, AuctionId, Role]),
    
    case lists:keyfind(AuctionId, 1, State#state.auction_handlers) of
        {AuctionId, Pid} ->
            Result = case Role of
                participant ->
                    auction_handler:join_as_participant(Pid, Username, ClientPid);
                spectator ->
                    auction_handler:join_as_spectator(Pid, Username, ClientPid)
            end,
            
            %% Track connected user
            NewUsers = [{Username, ClientPid} | State#state.connected_users],
            {reply, Result, State#state{connected_users = NewUsers}};
        false ->
            {reply, {error, auction_not_found}, State}
    end;

handle_call({place_bid, AuctionId, Username, Amount}, _From, State) ->
    io:format("[SERVER] Bid from ~p on ~p: ~p~n", [Username, AuctionId, Amount]),
    
    case lists:keyfind(AuctionId, 1, State#state.auction_handlers) of
        {AuctionId, Pid} ->
            Result = auction_handler:place_bid(Pid, Username, Amount),
            {reply, Result, State};
        false ->
            {reply, {error, auction_not_found}, State}
    end;

%%%===================================================================
%%% Cluster management handlers
%%%===================================================================

handle_call({join_cluster, MasterNode}, _From, State) ->
    io:format("[SERVER] Joining cluster with master: ~p~n", [MasterNode]),
    
    %% Connect to master node
    case net_adm:ping(MasterNode) of
        pong ->
            io:format("[SERVER] Connected to master node~n"),
            %% Join mnesia cluster
            Result = mnesia_db:join_cluster(MasterNode),
            {reply, Result, State};
        pang ->
            io:format("[SERVER] Failed to connect to master node~n"),
            {reply, {error, connection_failed}, State}
    end;

handle_call(get_nodes, _From, State) ->
    Nodes = [node() | nodes()],
    {reply, {ok, Nodes}, State};

handle_call(get_stats, _From, State) ->
    Stats = #{
        node => State#state.node_name,
        auction_count => length(State#state.auction_handlers),
        connected_users => length(State#state.connected_users),
        cluster_nodes => [node() | nodes()]
    },
    {reply, {ok, Stats}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%%%===================================================================
%%% Cast handlers
%%%===================================================================

handle_cast(_Msg, State) ->
    {noreply, State}.

%%%===================================================================
%%% Info handlers
%%%===================================================================

handle_info({'DOWN', _Ref, process, Pid, Reason}, State) ->
    io:format("[SERVER] Process down: ~p (reason: ~p)~n", [Pid, Reason]),
    
    %% Remove from connected users
    NewUsers = lists:filter(fun({_, P}) -> P =/= Pid end, State#state.connected_users),
    
    %% Check if it's an auction handler
    case lists:keyfind(Pid, 2, State#state.auction_handlers) of
        {AuctionId, Pid} ->
            io:format("[SERVER] Auction handler crashed: ~p~n", [AuctionId]),
            %% TODO: Implement restart logic here
            NewHandlers = lists:keydelete(AuctionId, 1, State#state.auction_handlers),
            {noreply, State#state{auction_handlers = NewHandlers, connected_users = NewUsers}};
        false ->
            {noreply, State#state{connected_users = NewUsers}}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

%%%===================================================================
%%% Termination
%%%===================================================================

terminate(Reason, State) ->
    io:format("[SERVER] Terminating node ~p: ~p~n", [State#state.node_name, Reason]),
    
    %% Stop all auction handlers
    lists:foreach(fun({AuctionId, Pid}) ->
        io:format("[SERVER] Stopping auction handler: ~p~n", [AuctionId]),
        gen_server:stop(Pid)
    end, State#state.auction_handlers),
    
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc Generate unique auction ID from item name and creator
generate_auction_id(ItemName, Creator) ->
    Timestamp = erlang:system_time(millisecond),
    CleanName = re:replace(ItemName, "\\s+", "_", [global, {return, list}]),
    lists:flatten(io_lib:format("~s_~s_~p", [CleanName, Creator, Timestamp])).
