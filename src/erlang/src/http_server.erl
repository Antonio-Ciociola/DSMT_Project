%%%-------------------------------------------------------------------
%%% @author Auction System
%%% @doc
%%% HTTP/WebSocket router for Cowboy. Sets up routes and starts
%%% the HTTP listener.
%%% @end
%%%-------------------------------------------------------------------
-module(http_server).
-author("auction_system").

-export([start/0, start/1, start_link/2, start_master/1, start_slave/1, stop/0]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start the HTTP server under supervision
start_link(Role, Port) ->
    io:format("[HTTP] start_link called with Role=~p, Port=~p~n", [Role, Port]),
    case Role of
        master -> start_master(Port);
        slave -> start_slave(Port)
    end,
    %% Return a dummy supervisor-compatible result
    %% Cowboy manages its own process tree, so we return ignore
    ignore.

%% @doc Start the HTTP server on default port 8081 (auto-detect role)
start() ->
    start(8081).

%% @doc Start the HTTP server with role detection
start(Port) ->
    Role = application:get_env(auction_app, node_role, master),
    case Role of
        master -> start_master(Port);
        slave -> start_slave(Port)
    end.

%% @doc Start master node - handles POST endpoints only
start_master(Port) ->
    io:format("[HTTP] Starting MASTER node on port ~p~n", [Port]),
    
    %% Master routes: POST endpoints only
    Dispatch = cowboy_router:compile([
        {'_', [
            %% POST endpoint for auction creation
            {"/erlangapi/auction", auction_post_handler, []},
            
            %% Health check
            {"/health", health_handler, []},
            
            %% Catch all - 404
            {'_', not_found_handler, []}
        ]}
    ]),
    
    start_cowboy_listener(Port, Dispatch).

%% @doc Start slave node - handles WebSocket endpoints only
start_slave(Port) ->
    io:format("[HTTP] Starting SLAVE node on port ~p~n", [Port]),
    
    %% Slave routes: WebSocket only
    Dispatch = cowboy_router:compile([
        {'_', [
            %% WebSocket endpoint
            {"/ws", ws_handler, []},
            
            %% Health check
            {"/health", health_handler, []},
            
            %% Catch all - 404
            {'_', not_found_handler, []}
        ]}
    ]),
    
    start_cowboy_listener(Port, Dispatch).

%% @doc Internal function to start Cowboy listener
start_cowboy_listener(Port, Dispatch) ->
    %% Start Cowboy HTTP listener with unique name per port
    ListenerName = list_to_atom("http_listener_" ++ integer_to_list(Port)),
    {ok, _} = cowboy:start_clear(
        ListenerName,
        [{port, Port}],
        #{
            env => #{dispatch => Dispatch},
            idle_timeout => infinity,       % WebSocket idle timeout (infinity = never timeout)
            request_timeout => 600000,      % HTTP request timeout (10 minutes)
            max_keepalive => infinity,      % Maximum number of keepalive requests
            inactivity_timeout => infinity  % Connection inactivity timeout
        }
    ),
    
    io:format("[HTTP] Server started successfully on port ~p~n", [Port]),
    ok.

%% @doc Stop the HTTP server
stop() ->
    io:format("[HTTP] Stopping HTTP server~n"),
    cowboy:stop_listener(http_listener).
