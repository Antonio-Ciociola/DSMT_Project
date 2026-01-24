%%%-------------------------------------------------------------------
%%% @author Auction System
%%% @doc
%%% 404 handler for unknown routes
%%% @end
%%%-------------------------------------------------------------------
-module(not_found_handler).
-export([init/2]).

init(Req0, State) ->
    Req = cowboy_req:reply(404,
        #{<<"content-type">> => <<"application/json">>},
        jsx:encode(#{
            error => <<"Not found">>,
            path => cowboy_req:path(Req0)
        }),
        Req0
    ),
    {ok, Req, State}.
