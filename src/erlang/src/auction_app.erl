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
    start_master_node/0,
    start_master_node/1,
    start_slave_node/1,
    start_slave_node/2,
    join_cluster/1
]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

% Callback to start the application
start(_StartType, _StartArgs) ->

    %% Log application start
    io:format("[APP] Starting auction application~n"),
    
    %% Get node role and port - supervisor will read these
    Role = application:get_env(auction_app, node_role, master),
    Port = application:get_env(auction_app, http_port, 8081),
    
    %% Log node role and port
    io:format("[APP] Node role: ~p~n", [Role]),
    io:format("[APP] HTTP server will start on port ~p~n", [Port]),
    
    %% Start supervisor (which will start HTTP server and server.erl)
    auction_supervisor:start_link().

% Callback to stop the application
stop(_State) ->
    io:format("[APP] Stopping auction application~n"),
    catch http_server:stop(),
    ok.

%%%===================================================================
%%% Convenience API
%%%===================================================================

%% @doc Start a node (automatically detects if schema exists)
start_node() ->
    start_node(8081).

%% @doc Start master node on default port 8081
start_master_node() ->
    start_master_node(8081).

%% @doc Start master node with custom HTTP port
start_master_node(Port) ->
    io:format("~n===========================================~n"),
    io:format("  STARTING MASTER NODE~n"),
    io:format("  Node: ~p~n", [node()]),
    io:format("  HTTP Port: ~p~n", [Port]),
    io:format("  Role: MASTER (POST endpoints)~n"),
    io:format("===========================================~n~n"),
    
    %% Set role to master
    application:set_env(auction_app, node_role, master),
    start_node_internal(Port).

%% @doc Start slave node (must provide master node for clustering)
start_slave_node(Port) ->
    %% erlang-master is the host name, solved by Docker DNS whereas 
    %% auction is the node name on that machine
    start_slave_node(Port, 'auction@erlang-master').

%% @doc Start slave node with custom port and master node
start_slave_node(Port, MasterNode) ->

    %% Log slave startup
    io:format("~n===========================================~n"),
    io:format("  STARTING SLAVE NODE~n"),
    io:format("  Node: ~p~n", [node()]),
    io:format("  HTTP Port: ~p~n", [Port]),
    io:format("  Role: SLAVE (WebSocket only)~n"),
    io:format("  Master: ~p~n", [MasterNode]),
    io:format("===========================================~n~n"),
    
    %% Set role to slave
    application:set_env(auction_app, node_role, slave),
    start_node_internal(Port),
    
    %% Join master's cluster
    io:format("[APP] Joining master cluster: ~p~n", [MasterNode]),
    join_cluster(MasterNode).

%% @doc Start a node with custom HTTP port
start_node(Port) ->
    start_node_internal(Port).

%% @doc Internal function to start node with custom HTTP port
start_node_internal(Port) ->
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
    
    %% Stop Mnesia to check/create schema
    mnesia:stop(),
    
    %% Check if schema exists, create if not
    SchemaDir = "Mnesia." ++ atom_to_list(node()),
    SchemaExists = filelib:is_dir(SchemaDir) andalso 
                   filelib:is_regular(filename:join(SchemaDir, "schema.DAT")),
    
    case SchemaExists of
        false ->
            io:format("[APP] No schema found, creating schema...~n"),
            case mnesia:create_schema([node()]) of
                ok -> 
                    io:format("[APP] Schema created~n");
                {error, {_, {already_exists, _}}} ->
                    io:format("[APP] Schema already exists~n");
                SchemaError ->
                    io:format("[APP] Schema creation error: ~p~n", [SchemaError])
            end;
        true ->
            io:format("[APP] Schema exists~n")
    end,
    
    %% Start Mnesia
    mnesia:start(),
    
    %% Wait for Mnesia tables to be ready
    mnesia:wait_for_tables(mnesia:system_info(local_tables), 5000),
    
    %% Check if tables exist, create if not
    Tables = mnesia:system_info(tables),
    case lists:member(auction, Tables) of
        false ->
            io:format("[APP] Tables not found, creating...~n"),
            mnesia_db:create_tables();
        true ->
            io:format("[APP] Tables already exist~n")
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

%% @doc Join an existing cluster abd replicate Mnesia data
join_cluster(RemoteNode) ->
    io:format("~n===========================================~n"),
    io:format("  JOINING CLUSTER~n"),
    io:format("  This node: ~p~n", [node()]),
    io:format("  Remote node: ~p~n", [RemoteNode]),
    io:format("===========================================~n~n"),
    
    %% Ping remote node with retries
    case ping_with_retry(RemoteNode, 10, 2000) of
        pong ->
            io:format("[APP] Connected to ~p~n", [RemoteNode]);
        pang ->
            io:format("[APP] ERROR: Cannot connect to ~p after retries~n", [RemoteNode]),
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
    case mnesia:change_table_copy_type(schema, node(), disc_copies) of
        {atomic, ok} ->
            io:format("[APP] Schema copied to local node~n");
        {aborted, {already_exists, schema, _, disc_copies}} ->
            io:format("[APP] Schema already exists as disc_copies~n");
        SchemaError ->
            io:format("[APP] Schema copy warning: ~p~n", [SchemaError])
    end,
    
    %% Copy all other tables
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
    Apps = [crypto, asn1, public_key, ssl, inets, jsx, jose, cowlib, ranch, cowboy],
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

%% @doc Ping remote node with retries
ping_with_retry(_Node, 0, _Delay) ->
    pang;
ping_with_retry(Node, Retries, Delay) ->
    case net_adm:ping(Node) of
        pong ->
            pong;
        pang ->
            io:format("[APP] Waiting for ~p... (~p retries left)~n", [Node, Retries - 1]),
            timer:sleep(Delay),
            ping_with_retry(Node, Retries - 1, Delay)
    end.
