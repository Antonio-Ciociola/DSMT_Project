%%%-------------------------------------------------------------------
%%% @author Auction System
%%% @doc
%%% Slave node manager - handles auction assignment and load balancing
%%% @end
%%%-------------------------------------------------------------------
-module(slave_manager).
-author("auction_system").

-export([
    get_available_slaves/0,
    assign_auction_to_slave/5,
    get_auction_slave_url/1,
    handle_auction_complete/4,
    migrate_auctions_from_dead_slave/1
]).

%% @doc Get list of available slave nodes with their ports
get_available_slaves() ->
    %% Hardcoded slave configuration - could be made dynamic
    [
        {'auction_slave1@erlang-slave1', 8082},
        {'auction_slave2@erlang-slave2', 8083}
    ].

%% @doc Assign an auction to the least loaded slave and start it there
assign_auction_to_slave(AuctionId, StartingPrice, MinDuration, MinIncrementBid, TimeIncrementBid) ->
    AvailableSlaves = get_available_slaves(),
    
    %% Filter to only connected slaves
    ConnectedSlaves = lists:filter(fun({SlaveNode, _Port}) ->
        case net_adm:ping(SlaveNode) of
            pong -> true;
            pang -> 
                io:format("[SLAVE_MGR] Slave ~p not reachable~n", [SlaveNode]),
                false
        end
    end, AvailableSlaves),
    
    case ConnectedSlaves of
        [] ->
            {error, no_slaves_available};
        _ ->
            %% Get least loaded slave
            {SlaveNode, SlavePort} = mnesia_db:get_least_loaded_slave(ConnectedSlaves),
            
            io:format("[SLAVE_MGR] Assigning auction ~p to slave ~p (port ~p)~n", 
                     [AuctionId, SlaveNode, SlavePort]),
            
            %% Store assignment in Mnesia
            ok = mnesia_db:assign_auction_to_slave(AuctionId, {SlaveNode, SlavePort}),
            
            %% Start auction handler on the slave node
            case start_auction_on_slave(SlaveNode, AuctionId, StartingPrice, MinDuration, 
                                       MinIncrementBid, TimeIncrementBid) of
                ok ->
                    WsUrl = build_ws_url(SlavePort),
                    {ok, {SlaveNode, SlavePort, WsUrl}};
                Error ->
                    io:format("[SLAVE_MGR] Failed to start auction on slave: ~p~n", [Error]),
                    Error
            end
    end.

%% @doc Get the WebSocket URL for an auction
get_auction_slave_url(AuctionId) ->
    case mnesia_db:get_auction_assignment(AuctionId) of
        {ok, Assignment} ->
            Port = element(4, Assignment),  % auction_assignment.slave_port (4th field)
            {ok, build_ws_url(Port)};
        Error ->
            Error
    end.

%% @doc Start auction handler on a slave node via RPC
start_auction_on_slave(SlaveNode, AuctionId, StartingPrice, MinDuration, MinIncrementBid, TimeIncrementBid) ->
    io:format("[SLAVE_MGR] Starting auction ~p on slave ~p~n", [AuctionId, SlaveNode]),
    
    %% Use server:register_auction to properly initialize the auction on the slave
    %% This ensures the auction is registered in the server's state
    case rpc:call(SlaveNode, server, register_auction, 
                  [AuctionId, StartingPrice, MinDuration, MinIncrementBid, TimeIncrementBid], 10000) of
        ok ->
            io:format("[SLAVE_MGR] Auction registered successfully on ~p~n", [SlaveNode]),
            ok;
        {badrpc, Reason} ->
            io:format("[SLAVE_MGR] RPC failed: ~p~n", [Reason]),
            {error, {rpc_failed, Reason}};
        Error ->
            io:format("[SLAVE_MGR] Failed to register auction: ~p~n", [Error]),
            Error
    end.

%% @doc Build WebSocket URL from port
build_ws_url(Port) ->
    lists:flatten(io_lib:format("ws://localhost:~p/ws", [Port])).

%% @doc Handle auction completion notification from slave node
handle_auction_complete(AuctionId, Winner, Amount, TotalDuration) ->
    io:format("[SLAVE_MGR] Received auction completion: ~p, Winner: ~p, Amount: ~p, Duration: ~p~n", 
             [AuctionId, Winner, Amount, TotalDuration]),
    
    %% Forward to auction_handler to POST to Java
    auction_handler:post_auction_finish_to_java(AuctionId, Winner, Amount, TotalDuration),
    ok.

%% @doc Migrate all auctions from a dead slave to other available slaves
migrate_auctions_from_dead_slave(DeadNode) ->
    io:format("[SLAVE_MGR] Starting migration from dead slave: ~p~n", [DeadNode]),
    
    %% Get all auctions assigned to the dead slave
    case mnesia_db:get_auctions_by_slave_node(DeadNode) of
        {ok, AuctionIds} ->
            io:format("[SLAVE_MGR] Found ~p auctions to migrate from ~p~n", 
                     [length(AuctionIds), DeadNode]),
            
            %% Migrate each auction
            lists:foreach(fun(AuctionId) ->
                case migrate_single_auction(AuctionId, DeadNode) of
                    ok ->
                        io:format("[SLAVE_MGR] Successfully migrated auction ~p~n", [AuctionId]);
                    {error, Reason} ->
                        io:format("[SLAVE_MGR] Failed to migrate auction ~p: ~p~n", 
                                 [AuctionId, Reason])
                end
            end, AuctionIds),
            
            ok;
        {error, Reason} ->
            io:format("[SLAVE_MGR] Failed to get auctions for dead slave: ~p~n", [Reason]),
            {error, Reason}
    end.

%% @doc Migrate a single auction to a new slave
migrate_single_auction(AuctionId, DeadNode) ->
    io:format("[SLAVE_MGR] Migrating auction ~p from dead node ~p~n", [AuctionId, DeadNode]),
    
    %% Read auction data from Mnesia
    case mnesia_db:get_auction(AuctionId) of
        {ok, Auction} ->
            %% Get all bids for this auction
            {ok, Bids} = mnesia_db:get_auction_bids(AuctionId),
            BidCount = length(Bids),
            
            %% Extract auction details (element access for auction record)
            BaseDuration = element(6, Auction),      % auction.duration
            StartTime = element(7, Auction),         % auction.start_time
            BidTimeIncrement = element(10, Auction), % auction.bid_time_increment
            
            %% Calculate remaining time using bid count
            CurrentTime = erlang:system_time(second),
            TotalDuration = BaseDuration + (BidCount * BidTimeIncrement),
            EndTime = StartTime + TotalDuration,
            RemainingTime = max(0, EndTime - CurrentTime),
            
            io:format("[SLAVE_MGR] Auction ~p: BidCount=~p, BaseDuration=~p, TotalDuration=~p, RemainingTime=~p~n",
                     [AuctionId, BidCount, BaseDuration, TotalDuration, RemainingTime]),
            
            %% Only migrate if auction still has time remaining
            if
                RemainingTime > 0 ->
                    %% Get available slaves (excluding the dead one)
                    AvailableSlaves = get_available_slaves(),
                    ConnectedSlaves = lists:filter(fun({SlaveNode, _Port}) ->
                        SlaveNode =/= DeadNode andalso net_adm:ping(SlaveNode) =:= pong
                    end, AvailableSlaves),
                    
                    case ConnectedSlaves of
                        [] ->
                            io:format("[SLAVE_MGR] No available slaves for migration~n"),
                            {error, no_slaves_available};
                        _ ->
                            %% Get least loaded slave
                            {NewSlaveNode, NewSlavePort} = mnesia_db:get_least_loaded_slave(ConnectedSlaves),
                            
                            io:format("[SLAVE_MGR] Migrating auction ~p to slave ~p (port ~p)~n",
                                     [AuctionId, NewSlaveNode, NewSlavePort]),
                            
                            %% Delete old assignment
                            ok = mnesia_db:delete_auction_assignment(AuctionId),
                            
                            %% Create new assignment
                            ok = mnesia_db:assign_auction_to_slave(AuctionId, {NewSlaveNode, NewSlavePort}),
                            
                            %% Start auction on new slave with reconstructed state
                            case start_migrated_auction_on_slave(NewSlaveNode, AuctionId, RemainingTime) of
                                ok ->
                                    %% Notify Java backend about the new WebSocket URL
                                    NewWsUrl = build_ws_url(NewSlavePort),
                                    notify_java_auction_migrated(AuctionId, NewWsUrl),
                                    ok;
                                Error ->
                                    io:format("[SLAVE_MGR] Failed to start migrated auction: ~p~n", [Error]),
                                    Error
                            end
                    end;
                true ->
                    io:format("[SLAVE_MGR] Auction ~p has expired, skipping migration~n", [AuctionId]),
                    ok
            end;
        {error, Reason} ->
            io:format("[SLAVE_MGR] Failed to read auction ~p: ~p~n", [AuctionId, Reason]),
            {error, Reason}
    end.

%% @doc Start a migrated auction on a new slave node
start_migrated_auction_on_slave(SlaveNode, AuctionId, RemainingTime) ->
    io:format("[SLAVE_MGR] Starting migrated auction ~p on slave ~p with ~p seconds remaining~n",
             [AuctionId, SlaveNode, RemainingTime]),
    
    %% Call server on the slave to start the migrated auction
    case rpc:call(SlaveNode, server, start_migrated_auction, 
                  [AuctionId, RemainingTime], 10000) of
        ok ->
            io:format("[SLAVE_MGR] Migrated auction started successfully on ~p~n", [SlaveNode]),
            ok;
        {badrpc, Reason} ->
            io:format("[SLAVE_MGR] RPC failed: ~p~n", [Reason]),
            {error, {rpc_failed, Reason}};
        Error ->
            io:format("[SLAVE_MGR] Failed to start migrated auction: ~p~n", [Error]),
            Error
    end.

%% @doc Notify Java backend about auction migration
notify_java_auction_migrated(AuctionId, NewWebsocketUrl) ->
    io:format("[SLAVE_MGR] Notifying Java about migration: ~p -> ~p~n", 
             [AuctionId, NewWebsocketUrl]),
    
    %% Java endpoint URL
    JavaUrl = "http://auction-backend:8080/api/auction-migrated",
    
    %% Build JSON payload
    Payload = lists:flatten(io_lib:format(
        "{\"auctionId\":\"~s\",\"websocketUrl\":\"~s\"}",
        [AuctionId, NewWebsocketUrl]
    )),
    
    %% Get API key
    ApiKey = os:getenv("ERLANG_API_KEY", "auction_secret_key_2026"),
    
    %% Make HTTP POST request
    case httpc:request(post, 
                      {JavaUrl, 
                       [{"Content-Type", "application/json"},
                        {"X-API-Key", ApiKey}],
                       "application/json",
                       Payload},
                      [{timeout, 5000}],
                      []) of
        {ok, {{_, 200, _}, _, _}} ->
            io:format("[SLAVE_MGR] Java notified successfully~n"),
            ok;
        {ok, {{_, StatusCode, _}, _, ResponseBody}} ->
            io:format("[SLAVE_MGR] Java notification failed: ~p, ~p~n", 
                     [StatusCode, ResponseBody]),
            {error, {java_notification_failed, StatusCode}};
        {error, Reason} ->
            io:format("[SLAVE_MGR] HTTP request failed: ~p~n", [Reason]),
            {error, {http_failed, Reason}}
    end.


