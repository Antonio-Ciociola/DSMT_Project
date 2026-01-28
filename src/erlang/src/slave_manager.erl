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
    handle_auction_complete/4
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

