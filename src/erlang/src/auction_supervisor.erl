%%%-------------------------------------------------------------------
%%% @author Auction System
%%% @doc
%%% Supervisor for the auction system. Provides fault tolerance by
%%% monitoring and restarting failed processes (server, auction handlers).
%%% @end
%%%-------------------------------------------------------------------
-module(auction_supervisor).
-author("auction_system").

-behavior(supervisor).

%% API
-export([
    start_link/0,
    start_auction_handler/3
]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Start the supervisor
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% @doc Dynamically start an auction handler under supervision
start_auction_handler(AuctionId, MinIncrementBid, TimeIncrementBid) ->
    ChildSpec = #{
        id => {auction_handler, AuctionId},
        start => {auction_handler, start_link, [AuctionId, MinIncrementBid, TimeIncrementBid]},
        restart => transient,  % Only restart if abnormal termination
        shutdown => 5000,
        type => worker,
        modules => [auction_handler]
    },
    supervisor:start_child(?SERVER, ChildSpec).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    io:format("[SUPERVISOR] Initializing~n"),
    
    %% Get node role and port from application environment
    Role = application:get_env(auction_app, node_role, master),
    Port = application:get_env(auction_app, http_port, 8081),
    
    io:format("[SUPERVISOR] Starting with Role=~p, Port=~p~n", [Role, Port]),
    
    SupFlags = #{
        strategy => one_for_one,  % Restart only failed child
        intensity => 10,          % Max 10 restarts
        period => 60              % Within 60 seconds
    },
    
    %% HTTP server child specification
    HttpServerChild = #{
        id => http_server,
        start => {http_server, start_link, [Role, Port]},
        restart => permanent,  % Always restart
        shutdown => 5000,
        type => worker,
        modules => [http_server]
    },
    
    %% Main server child specification
    ServerChild = #{
        id => server,
        start => {server, start_link, []},
        restart => permanent,  % Always restart
        shutdown => 5000,
        type => worker,
        modules => [server]
    },
    
    ChildSpecs = [HttpServerChild, ServerChild],
    
    {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
