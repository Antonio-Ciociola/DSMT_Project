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
    %% Validate API key
    ApiKey = os:getenv("ERLANG_API_KEY", "auction_secret_key_2026"),
    ProvidedKey = cowboy_req:header(<<"x-api-key">>, Req0),
    
    case ProvidedKey of
        undefined ->
            io:format("[POST] Unauthorized - no API key provided~n"),
            Req = cowboy_req:reply(401, #{
                <<"content-type">> => <<"application/json">>
            }, jsx:encode(#{error => <<"Unauthorized - API key required">>}), Req0),
            {ok, Req, State};
        _ ->
            case binary_to_list(ProvidedKey) =:= ApiKey of
                false ->
                    io:format("[POST] Unauthorized - invalid API key~n"),
                    Req = cowboy_req:reply(401, #{
                        <<"content-type">> => <<"application/json">>
                    }, jsx:encode(#{error => <<"Unauthorized - Invalid API key">>}), Req0),
                    {ok, Req, State};
                true ->
                    handle_authenticated_post(Req0, State)
            end
    end.

handle_authenticated_post(Req0, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    
    try
        %% Parse JSON body
        Data = jsx:decode(Body, [return_maps]),
        #{
            <<"id">> := AuctionId,
            <<"starting_price">> := StartingPrice,
            <<"min_duration">> := MinDuration,
            <<"min_increment_bid">> := MinIncrementBid,
            <<"time_increment_bid">> := TimeIncrementBid
        } = Data,
        
        io:format("[POST] Received auction: ~p, price: ~p, duration: ~p, min_inc: ~p, time_inc: ~p~n", 
                  [AuctionId, StartingPrice, MinDuration, MinIncrementBid, TimeIncrementBid]),
        
        %% Assign auction to least loaded slave (slave will create it in Mnesia)
        AuctionIdStr = binary_to_list(AuctionId),
        case slave_manager:assign_auction_to_slave(AuctionIdStr, StartingPrice, MinDuration, 
                                                   MinIncrementBid, TimeIncrementBid) of
            {ok, {SlaveNode, SlavePort, WsUrl}} ->
                io:format("[POST] Auction ~p assigned to ~p (port ~p)~n", 
                         [AuctionId, SlaveNode, SlavePort]),
                Req = cowboy_req:reply(200, #{
                    <<"content-type">> => <<"application/json">>,
                    <<"access-control-allow-origin">> => <<"*">>
                }, jsx:encode(#{
                    success => true,
                    auction_id => AuctionId,
                    websocket_url => list_to_binary(WsUrl),
                    slave_node => list_to_binary(atom_to_list(SlaveNode)),
                    slave_port => SlavePort
                }), Req1),
                {ok, Req, State};
            {error, Reason} ->
                Req = cowboy_req:reply(500, #{
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

format_error(no_slaves_available) ->
    <<"No slave nodes available to handle auction">>;
format_error(Reason) when is_atom(Reason) ->
    list_to_binary(atom_to_list(Reason));
format_error(Reason) when is_list(Reason) ->
    list_to_binary(Reason);
format_error(Reason) when is_binary(Reason) ->
    Reason;
format_error(Reason) ->
    list_to_binary(io_lib:format("~p", [Reason])).
