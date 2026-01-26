%%%-------------------------------------------------------------------
%%% @author Auction System
%%% @doc
%%% HTTP/WebSocket router for Cowboy. Sets up routes and starts
%%% the HTTP listener.
%%% @end
%%%-------------------------------------------------------------------
-module(http_server).
-author("auction_system").

-export([start/0, start/1, stop/0]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start the HTTP server on default port 8080
start() ->
    start(8080).

%% @doc Start the HTTP server with WebSocket support on custom port
start(Port) ->
    io:format("[HTTP] Starting HTTP server on port ~p~n", [Port]),
    
    %% Define routes
    Dispatch = cowboy_router:compile([
        {'_', [
            %% WebSocket endpoint
            {"/ws", ws_handler, []},
            
            %% POST endpoints for external system
            {"/erlangapi/auction", auction_post_handler, []},
            {"/erlangapi/user", user_post_handler, []},
            
            %% Static health check
            {"/health", health_handler, []},
            
            %% Catch all - 404
            {'_', not_found_handler, []}
        ]}
    ]),
    
    %% Start Cowboy HTTP listener with unique name per port
    ListenerName = list_to_atom("http_listener_" ++ integer_to_list(Port)),
    {ok, _} = cowboy:start_clear(
        ListenerName,
        [{port, Port}],
        #{
            env => #{dispatch => Dispatch},
            idle_timeout => infinity,  % WebSocket idle timeout (infinity = never timeout)
            request_timeout => 600000   % HTTP request timeout (10 minutes)
        }
    ),
    
    io:format("[HTTP] Server started successfully on http://localhost:~p~n", [Port]),
    io:format("[HTTP] WebSocket endpoint: ws://localhost:~p/ws~n", [Port]),
    ok.

%% @doc Stop the HTTP server
stop() ->
    io:format("[HTTP] Stopping HTTP server~n"),
    cowboy:stop_listener(http_listener).
