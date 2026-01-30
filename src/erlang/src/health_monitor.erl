%%%-------------------------------------------------------------------
%%% @doc
%%% Health monitor for slave nodes - Master only
%%% Periodically pings slave nodes to detect failures and trigger migrations
%%% @end
%%%-------------------------------------------------------------------
-module(health_monitor).
-behavior(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(HEALTH_CHECK_INTERVAL, 5000).  % 5 seconds

-record(state, {
    slave_nodes = [] :: [node()]
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    io:format("[HEALTH_MONITOR] Starting health monitoring on master node~n"),
    
    %% Schedule first health check
    erlang:send_after(?HEALTH_CHECK_INTERVAL, self(), health_check),
    
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(health_check, State) ->
    %% Get all slave nodes from configuration
    AllSlaves = application:get_env(auction_app, slave_nodes, []),
    
    %% Ping each slave and collect alive nodes
    AliveSlaves = lists:filter(fun({Node, _Port}) ->
        case net_adm:ping(Node) of
            pong -> 
                true;
            pang -> 
                io:format("[HEALTH_MONITOR] Dead slave detected: ~p~n", [Node]),
                false
        end
    end, AllSlaves),
    
    %% Detect newly dead slaves
    PreviousSlaves = State#state.slave_nodes,
    AliveSlaveNodes = [Node || {Node, _Port} <- AliveSlaves],
    DeadSlaves = PreviousSlaves -- AliveSlaveNodes,
    
    %% Trigger migration for each dead slave
    lists:foreach(fun(DeadNode) ->
        io:format("[HEALTH_MONITOR] Triggering migration for dead slave: ~p~n", [DeadNode]),
        spawn(fun() -> 
            slave_manager:migrate_auctions_from_dead_slave(DeadNode) 
        end)
    end, DeadSlaves),
    
    %% Schedule next health check
    erlang:send_after(?HEALTH_CHECK_INTERVAL, self(), health_check),
    
    {noreply, State#state{slave_nodes = AliveSlaveNodes}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
