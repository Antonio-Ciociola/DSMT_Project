%%%-------------------------------------------------------------------
%%% @author Auction System
%%% @doc
%%% WebSocket handler for client connections. Handles JSON messages
%%% for user authentication, auction operations, and real-time updates.
%%% @end
%%%-------------------------------------------------------------------
-module(ws_handler).
-author("auction_system").

-export([
    init/2,
    websocket_init/1,
    websocket_handle/2,
    websocket_info/2,
    terminate/3
]).

-record(state, {
    user_id = undefined :: undefined | binary(),
    subscriptions = [] :: [term()]  % List of auction IDs subscribed to
}).

%%%===================================================================
%%% Cowboy WebSocket Callbacks
%%%===================================================================

%% @doc Initialize the WebSocket connection
init(Req, _State) ->
    io:format("[WS] New WebSocket connection~n"),
    %% Extract JWT token from query parameter
    #{token := Token} = cowboy_req:match_qs([{token, [], undefined}], Req),
    
    case Token of
        undefined ->
            io:format("[WS] No JWT token provided~n"),
            {ok, cowboy_req:reply(401, #{}, <<"No JWT token">>, Req), #state{}};
        _ ->
            %% Verify JWT and extract user_id
            case verify_jwt(Token) of
                {ok, UserId} ->
                    io:format("[WS] JWT verified for user: ~p~n", [UserId]),
                    {cowboy_websocket, Req, #state{user_id = list_to_binary(UserId)}};
                {error, Reason} ->
                    io:format("[WS] JWT verification failed: ~p~n", [Reason]),
                    {ok, cowboy_req:reply(401, #{}, <<"Invalid JWT">>, Req), #state{}}
            end
    end.

%% @doc Called after WebSocket upgrade
websocket_init(State) ->
    case State#state.user_id of
        undefined ->
            io:format("[WS] WebSocket initialized without user~n"),
            {ok, State};
        UserId ->
            io:format("[WS] WebSocket initialized for user: ~p~n", [UserId]),
            %% Ensure user exists in database, create if not
            ensure_user_exists(binary_to_list(UserId)),
            {ok, State}
    end.

%% @doc Handle incoming WebSocket messages from client
websocket_handle({text, Json}, State) ->
    io:format("[WS] Received message: ~p~n", [Json]),
    try
        Msg = jsx:decode(Json, [return_maps]),
        handle_message(Msg, State)
    catch
        Type:Error:Stacktrace ->
            io:format("[WS] Error processing message: ~p:~p~nStacktrace: ~p~n", [Type, Error, Stacktrace]),
            ErrorResp = jsx:encode(#{
                type => <<"error">>,
                message => <<"Invalid JSON format">>
            }),
            {reply, {text, ErrorResp}, State}
    end;
websocket_handle(_Frame, State) ->
    {ok, State}.

%% @doc Handle Erlang messages sent to this process
websocket_info({auction_update, StateInfo}, State) ->
    %% Forward auction updates to connected clients
    Msg = jsx:encode(#{
        type => <<"auction_update">>,
        auction_state => StateInfo
    }),
    {reply, {text, Msg}, State};
websocket_info({notification, Data}, State) ->
    %% Send notifications to client
    Msg = jsx:encode(#{
        type => <<"notification">>,
        data => Data
    }),
    {reply, {text, Msg}, State};
websocket_info(Info, State) ->
    io:format("[WS] Received unknown info: ~p~n", [Info]),
    {ok, State}.

%% @doc Called when WebSocket connection closes
terminate(_Reason, _Req, State) ->
    io:format("[WS] WebSocket closed for user: ~p~n", [State#state.user_id]),
    %% Cleanup: unsubscribe from auctions, etc.
    ok.

%%%===================================================================
%%% Internal Message Handlers
%%%===================================================================

%% Get user balance
handle_message(#{<<"type">> := <<"get_balance">>}, State) ->
    case State#state.user_id of
        undefined ->
            error_response(<<"Not authenticated">>, State);
        UserId ->
            case server:get_balance(binary_to_list(UserId)) of
                {ok, Balance} ->
                    Response = jsx:encode(#{
                        type => <<"balance_response">>,
                        balance => Balance
                    }),
                    {reply, {text, Response}, State};
                {error, Reason} ->
                    error_response(format_error(Reason), State)
            end
    end;

%% Connect to auction (for participants/spectators)
handle_message(#{<<"type">> := <<"connect_auction">>,
                 <<"auction_id">> := AuctionId,
                 <<"role">> := Role}, State) ->
    case State#state.user_id of
        undefined ->
            error_response(<<"Not authenticated">>, State);
        UserId ->
            %% Convert role string to atom safely
            RoleAtom = case Role of
                <<"participant">> -> participant;
                <<"spectator">> -> spectator;
                _ -> undefined
            end,
            case RoleAtom of
                undefined ->
                    error_response(<<"Invalid role. Use 'participant' or 'spectator'">>, State);
                _ ->
                    case server:connect_to_auction(binary_to_list(AuctionId), 
                                                  binary_to_list(UserId), 
                                                  RoleAtom) of
                        {ok, AuctionState} ->
                            %% Subscribe to auction updates
                            NewSubs = [AuctionId | State#state.subscriptions],
                            Response = jsx:encode(#{
                                type => <<"connect_auction_response">>,
                                success => true,
                                auction_state => AuctionState
                            }),
                            {reply, {text, Response}, State#state{subscriptions = NewSubs}};
                        {error, Reason} ->
                            Response = jsx:encode(#{
                                type => <<"connect_auction_response">>,
                                success => false,
                                error => format_error(Reason)
                            }),
                            {reply, {text, Response}, State}
                    end
            end
    end;

%% Place bid
handle_message(#{<<"type">> := <<"place_bid">>,
                 <<"auction_id">> := AuctionId,
                 <<"amount">> := Amount}, State) ->
    case State#state.user_id of
        undefined ->
            error_response(<<"Not authenticated">>, State);
        UserId ->
            case server:place_bid(binary_to_list(AuctionId), 
                                 binary_to_list(UserId), 
                                 Amount) of
                {ok, accepted} ->
                    Response = jsx:encode(#{
                        type => <<"place_bid_response">>,
                        success => true
                    }),
                    {reply, {text, Response}, State};
                {error, Reason} ->
                    Response = jsx:encode(#{
                        type => <<"place_bid_response">>,
                        success => false,
                        error => format_error(Reason)
                    }),
                    {reply, {text, Response}, State}
            end
    end;

%% Get active auctions
handle_message(#{<<"type">> := <<"get_active_auctions">>}, State) ->
    case server:get_active_auctions() of
        {ok, Auctions} ->
            Response = jsx:encode(#{
                type => <<"active_auctions_response">>,
                auctions => format_auctions(Auctions)
            }),
            {reply, {text, Response}, State};
        {error, Reason} ->
            error_response(format_error(Reason), State)
    end;

%% Get completed auctions
handle_message(#{<<"type">> := <<"get_completed_auctions">>}, State) ->
    case server:get_completed_auctions() of
        {ok, Auctions} ->
            Response = jsx:encode(#{
                type => <<"completed_auctions_response">>,
                auctions => format_auctions(Auctions)
            }),
            {reply, {text, Response}, State};
        {error, Reason} ->
            error_response(format_error(Reason), State)
    end;

%% Unknown message type
handle_message(Msg, State) ->
    io:format("[WS] Unknown message type: ~p~n", [Msg]),
    error_response(<<"Unknown message type">>, State).

%%%===================================================================
%%% Helper Functions
%%%===================================================================

error_response(Message, State) ->
    Response = jsx:encode(#{
        type => <<"error">>,
        message => Message
    }),
    {reply, {text, Response}, State}.

format_error(Reason) when is_atom(Reason) ->
    list_to_binary(atom_to_list(Reason));
format_error(Reason) when is_list(Reason) ->
    list_to_binary(Reason);
format_error(Reason) when is_binary(Reason) ->
    Reason;
format_error(Reason) ->
    list_to_binary(io_lib:format("~p", [Reason])).

format_auctions(Auctions) ->
    [format_auction(A) || A <- Auctions].

format_auction({auction, AuctionId, Creator, ItemName, MinBid, 
                Duration, StartTime, Winner, WinningBid, _NodePid}) ->
    #{
        auction_id => list_to_binary(AuctionId),
        creator => list_to_binary(Creator),
        item_name => list_to_binary(ItemName),
        min_bid => MinBid,
        duration => Duration,
        start_time => StartTime,
        winner => format_winner(Winner),
        winning_bid => WinningBid
    };
format_auction(_) ->
    #{}.

format_winner(none) -> null;
format_winner(Winner) when is_list(Winner) -> list_to_binary(Winner);
format_winner(Winner) -> Winner.

%% @doc Verify JWT token and extract user_id
verify_jwt(Token) ->
    %% Check for test token first
    case binary:split(Token, <<".">>, [global]) of
        [<<"TEST">>, Payload, _Sig] ->
            %% Decode test token (NOT FOR PRODUCTION!)
            verify_test_token(Payload);
        _ ->
            %% Try real JWT verification
            verify_real_jwt(Token)
    end.

%% Verify test token (for development)
verify_test_token(Payload) ->
    try
        PayloadJson = base64:decode(Payload),
        #{<<"userid">> := UserId, <<"expiredate">> := ExpireDate} = 
            jsx:decode(PayloadJson, [return_maps]),
        
        %% Check expiration
        CurrentTime = os:system_time(second),
        case ExpireDate > CurrentTime of
            true ->
                {ok, binary_to_list(UserId)};
            false ->
                {error, token_expired}
        end
    catch
        _:_ -> 
            {error, invalid_token_format}
    end.

%% Verify real JWT with public key
verify_real_jwt(Token) ->
    try
        case application:get_env(auction_app, jwt_public_key) of
            {ok, PublicKeyPem} ->
                %% Parse the PEM key
                JWK = jose_jwk:from_pem(PublicKeyPem),
                
                %% Verify and decode JWT
                case jose_jwt:verify(JWK, Token) of
                    {true, PayloadBin, _JWS} ->
                        %% Extract claims
                        Claims = jsx:decode(PayloadBin, [return_maps]),
                        #{<<"userid">> := UserId, <<"expiredate">> := ExpireDate} = Claims,
                        
                        %% Check expiration
                        CurrentTime = os:system_time(second),
                        case ExpireDate > CurrentTime of
                            true ->
                                {ok, binary_to_list(UserId)};
                            false ->
                                {error, token_expired}
                        end;
                    {false, _, _} ->
                        {error, invalid_signature}
                end;
            undefined ->
                {error, no_public_key_configured}
        end
    catch
        _:Error ->
            io:format("[WS] JWT verification error: ~p~n", [Error]),
            {error, jwt_verification_failed}
    end.

%% @doc Ensure user exists in database
ensure_user_exists(UserId) ->
    case mnesia_db:get_user(UserId) of
        {ok, _User} ->
            io:format("[WS] User ~p exists~n", [UserId]),
            ok;
        {error, not_found} ->
            io:format("[WS] User ~p not found - must be registered via POST /api/user first~n", [UserId]),
            {error, user_not_registered};
        {error, Reason} ->
            io:format("[WS] Error checking user ~p: ~p~n", [UserId, Reason]),
            {error, Reason}
    end.
