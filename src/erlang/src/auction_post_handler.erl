%%%-------------------------------------------------------------------
%%% @author Auction System
%%% @doc
%%% HTTP POST handler for receiving auction data from external system
%%% @end
%%%-------------------------------------------------------------------
-module(auction_post_handler).
-author("auction_system").

-export([init/2]).

%% @doc Handle POST request to create/update auction
init(Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"POST">> ->
            handle_post(Req0, State);
        <<"OPTIONS">> ->
            %% Handle CORS preflight request
            Req = cowboy_req:reply(200, #{
                <<"access-control-allow-origin">> => <<"*">>,
                <<"access-control-allow-methods">> => <<"POST, OPTIONS">>,
                <<"access-control-allow-headers">> => <<"content-type">>
            }, <<>>, Req0),
            {ok, Req, State};
        _ ->
            Req = cowboy_req:reply(405, #{
                <<"content-type">> => <<"application/json">>
            }, jsx:encode(#{error => <<"Method not allowed">>}), Req0),
            {ok, Req, State}
    end.

handle_post(Req0, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    
    try
        %% Parse JSON body
        Data = jsx:decode(Body, [return_maps]),
        #{
            <<"id">> := AuctionId,
            <<"starting_price">> := StartingPrice,
            <<"min_duration">> := MinDuration
        } = Data,
        
        io:format("[POST] Received auction: ~p, price: ~p, duration: ~p~n", 
                  [AuctionId, StartingPrice, MinDuration]),
        
        %% Store auction in system
        case server:register_auction(
            binary_to_list(AuctionId),
            StartingPrice,
            MinDuration
        ) of
            ok ->
                Req = cowboy_req:reply(200, #{
                    <<"content-type">> => <<"application/json">>,
                    <<"access-control-allow-origin">> => <<"*">>
                }, jsx:encode(#{
                    success => true,
                    auction_id => AuctionId
                }), Req1),
                {ok, Req, State};
            {error, Reason} ->
                Req = cowboy_req:reply(400, #{
                    <<"content-type">> => <<"application/json">>,
                    <<"access-control-allow-origin">> => <<"*">>
                }, jsx:encode(#{
                    success => false,
                    error => format_error(Reason)
                }), Req1),
                {ok, Req, State}
        end
    catch
        _:Error ->
            io:format("[POST] Error processing auction: ~p~n", [Error]),
            Req2 = cowboy_req:reply(400, #{
                <<"content-type">> => <<"application/json">>,
                <<"access-control-allow-origin">> => <<"*">>
            }, jsx:encode(#{
                success => false,
                error => <<"Invalid request format">>
            }), Req1),
            {ok, Req2, State}
    end.

format_error(Reason) when is_atom(Reason) ->
    list_to_binary(atom_to_list(Reason));
format_error(Reason) when is_list(Reason) ->
    list_to_binary(Reason);
format_error(Reason) when is_binary(Reason) ->
    Reason;
format_error(Reason) ->
    list_to_binary(io_lib:format("~p", [Reason])).
