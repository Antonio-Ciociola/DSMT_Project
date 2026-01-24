%%%-------------------------------------------------------------------
%%% @author Auction System
%%% @doc
%%% Application module for the distributed auction system.
%%% Provides easy startup and configuration for cluster nodes.
%%% @end
%%%-------------------------------------------------------------------
-module(auction_app).
-author("auction_system").

-behavior(application).

%% Application callbacks
-export([start/2, stop/1]).

%% Convenience API
-export([
    start_node/0,
    start_node/1,
    join_cluster/1
]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

start(_StartType, _StartArgs) ->
    io:format("[APP] Starting auction application~n"),
    
    %% Start HTTP server on configured port
    Port = application:get_env(auction_app, http_port, 8080),
    io:format("[APP] Starting HTTP server on port ~p~n", [Port]),
    http_server:start(Port),
    
    auction_supervisor:start_link().

stop(_State) ->
    io:format("[APP] Stopping auction application~n"),
    catch http_server:stop(),
    ok.

%%%===================================================================
%%% Convenience API
%%%===================================================================

%% @doc Start a node (automatically detects if schema exists)
start_node() ->
    start_node(8080).

%% @doc Start a node with custom HTTP port
start_node(Port) ->
    io:format("~n===========================================~n"),
    io:format("  STARTING AUCTION NODE~n"),
    io:format("  Node: ~p~n", [node()]),
    io:format("  HTTP Port: ~p~n", [Port]),
    io:format("===========================================~n~n"),
    
    %% Start dependencies
    io:format("[APP] Starting dependencies...~n"),
    start_cowboy_deps(),
    
    %% Start Mnesia
    io:format("[APP] Starting Mnesia...~n"),
    case application:start(mnesia) of
        ok -> 
            io:format("[APP] Mnesia started~n");
        {error, {already_started, mnesia}} ->
            io:format("[APP] Mnesia already started~n");
        MnesiaError ->
            io:format("[APP] Failed to start Mnesia: ~p~n", [MnesiaError]),
            erlang:error(MnesiaError)
    end,
    
    %% Check if schema exists, create if first node
    SchemaExists = filelib:is_dir("Mnesia." ++ atom_to_list(node())),
    case SchemaExists of
        false ->
            io:format("[APP] No schema found, initializing as first node...~n"),
            mnesia_db:create_schema([node()]),
            mnesia:start(),
            mnesia_db:create_tables();
        true ->
            io:format("[APP] Schema exists, starting Mnesia...~n"),
            mnesia:start()
    end,
    
    %% Set HTTP port
    application:set_env(auction_app, http_port, Port),
    
    %% Start the application
    case application:start(auction_app) of
        ok -> 
            io:format("[APP] Application started~n");
        {error, {already_started, auction_app}} ->
            io:format("[APP] Application already started~n");
        StartError ->
            io:format("[APP] Failed to start application: ~p~n", [StartError]),
            erlang:error(StartError)
    end,
    
    %% Wait for server to be registered
    wait_for_server(30),
    
    io:format("~n[APP] Node ready!~n"),
    io:format("[APP] WebSocket: ws://localhost:~p/ws~n", [Port]),
    io:format("[APP] Health check: http://localhost:~p/health~n~n", [Port]).

%% @doc Join an existing cluster
join_cluster(RemoteNode) ->
    io:format("~n===========================================~n"),
    io:format("  JOINING CLUSTER~n"),
    io:format("  This node: ~p~n", [node()]),
    io:format("  Remote node: ~p~n", [RemoteNode]),
    io:format("===========================================~n~n"),
    
    %% Connect to remote node
    case net_adm:ping(RemoteNode) of
        pong ->
            io:format("[APP] Connected to ~p~n", [RemoteNode]);
        pang ->
            io:format("[APP] ERROR: Cannot connect to ~p~n", [RemoteNode]),
            erlang:error({cannot_connect, RemoteNode})
    end,
    
    %% Stop mnesia temporarily
    mnesia:stop(),
    
    %% Delete local schema if exists
    mnesia:delete_schema([node()]),
    
    %% Add remote node to extra_db_nodes
    io:format("[APP] Joining cluster...~n"),
    mnesia:start(),
    mnesia:change_config(extra_db_nodes, [RemoteNode]),
    
    %% Copy schema and tables
    {ok, _} = mnesia:change_table_copy_type(schema, node(), disc_copies),
    
    Tables = mnesia:system_info(tables) -- [schema],
    lists:foreach(fun(Table) ->
        case mnesia:add_table_copy(Table, node(), disc_copies) of
            {atomic, ok} ->
                io:format("[APP] Replicated table ~p~n", [Table]);
            {aborted, {already_exists, _, _}} ->
                io:format("[APP] Table ~p already exists~n", [Table]);
            Error ->
                io:format("[APP] Failed to replicate ~p: ~p~n", [Table, Error])
        end
    end, Tables),
    
    io:format("~n[APP] Successfully joined cluster!~n"),
    io:format("[APP] Available nodes: ~p~n~n", [mnesia:system_info(db_nodes)]).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc Start Cowboy dependencies
start_cowboy_deps() ->
    Apps = [crypto, asn1, public_key, ssl, jsx, cowlib, ranch, cowboy],
    lists:foreach(fun(App) ->
        case application:start(App) of
            ok -> 
                io:format("[APP] Started ~p~n", [App]);
            {error, {already_started, _}} ->
                ok;
            Error ->
                io:format("[APP] Failed to start ~p: ~p~n", [App, Error])
        end
    end, Apps).

%% @doc Wait for the server process to be registered
wait_for_server(0) ->
    io:format("[APP] ERROR: Server failed to start after waiting~n"),
    erlang:error(server_not_started);
wait_for_server(Attempts) ->
    case whereis(server) of
        undefined ->
            timer:sleep(200),
            wait_for_server(Attempts - 1);
        _Pid ->
            io:format("[APP] Server is ready~n"),
            ok
    end.
