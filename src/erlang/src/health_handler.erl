%%%-------------------------------------------------------------------
%%% @author Auction System
%%% @doc
%%% Simple health check handler for monitoring
%%% @end
%%%-------------------------------------------------------------------
-module(health_handler).
-export([init/2]).

init(Req0, State) ->
    Req = cowboy_req:reply(200, 
        #{<<"content-type">> => <<"application/json">>},
        jsx:encode(#{
            status => <<"ok">>,
            node => node(),
            time => erlang:system_time(second)
        }),
        Req0
    ),
    {ok, Req, State}.
