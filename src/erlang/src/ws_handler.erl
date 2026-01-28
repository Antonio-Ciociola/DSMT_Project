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
    auction_id = undefined :: undefined | binary(),
    balance = 0.0 :: float(),
    is_guest = false :: boolean(),
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
            %% Verify JWT and extract user info
            case verify_jwt(Token) of
                {ok, Username, AuctionId, Balance, IsGuest} ->
                    io:format("[WS] JWT verified - User: ~p, Auction: ~p, Balance: ~p, Guest: ~p~n", 
                             [Username, AuctionId, Balance, IsGuest]),
                    
                    %% Verify this slave manages the auction
                    case verify_auction_assignment(AuctionId) of
                        ok ->
                            {cowboy_websocket, Req, #state{
                                user_id = list_to_binary(Username),
                                auction_id = list_to_binary(AuctionId),
                                balance = Balance,
                                is_guest = IsGuest
                            }};
                        {error, wrong_slave} ->
                            io:format("[WS] Auction ~p not managed by this slave~n", [AuctionId]),
                            {ok, cowboy_req:reply(403, #{}, <<"Auction not managed by this slave">>, Req), #state{}};
                        {error, Reason} ->
                            io:format("[WS] Auction verification failed: ~p~n", [Reason]),
                            {ok, cowboy_req:reply(404, #{}, <<"Auction not found">>, Req), #state{}}
                    end;
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
        Username ->
            AuctionId = State#state.auction_id,
            Balance = State#state.balance,
            IsGuest = State#state.is_guest,
            io:format("[WS] WebSocket initialized for user: ~p, auction: ~p, balance: ~p, guest: ~p~n", 
                     [Username, AuctionId, Balance, IsGuest]),
            
            %% Guests join as spectators, registered users as participants
            ConnectionType = case IsGuest of
                true -> spectator;
                false -> participant
            end,
            
            %% Register user with auction only if not a guest
            RegisterResult = case IsGuest of
                true -> 
                    io:format("[WS] Guest user - skipping registration~n"),
                    ok;
                false ->
                    server:register_auction_user(
                        binary_to_list(AuctionId),
                        binary_to_list(Username),
                        Balance
                    )
            end,
            
            case RegisterResult of
                ok ->
                    %% Connect to auction
                    case server:connect_to_auction(
                        binary_to_list(AuctionId),
                        binary_to_list(Username),
                        ConnectionType
                    ) of
                        {ok, AuctionState} ->
                            %% Subscribe to updates
                            NewSubs = [AuctionId | State#state.subscriptions],
                            
                            %% Send connection confirmation first
                            ConnectedMsg = jsx:encode(#{
                                type => <<"connected">>
                            }),
                            
                            %% Then send initial auction state as update
                            UpdateMsg = jsx:encode(#{
                                type => <<"auction_update">>,
                                auction_state => AuctionState
                            }),
                            
                            {reply, [{text, ConnectedMsg}, {text, UpdateMsg}], State#state{subscriptions = NewSubs}};
                        {error, Reason} ->
                            io:format("[WS] Failed to connect to auction: ~p~n", [Reason]),
                            ErrorMsg = jsx:encode(#{
                                type => <<"error">>,
                                message => format_error(Reason)
                            }),
                            {reply, {text, ErrorMsg}, State}
                    end;
                {error, Reason} ->
                    io:format("[WS] Failed to register user: ~p~n", [Reason]),
                    ErrorMsg = jsx:encode(#{
                        type => <<"error">>,
                        message => format_error(Reason)
                    }),
                    {reply, {text, ErrorMsg}, State}
            end
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
websocket_info({auction_ended, EndInfo}, State) ->
    %% Forward auction end notification to client
    io:format("[WS] Forwarding auction_ended to client: ~p~n", [EndInfo]),
    Msg = jsx:encode(#{
        type => <<"auction_ended">>,
        winner_user_id => maps:get(winner_user_id, EndInfo, <<"none">>),
        winning_bid => maps:get(winning_bid, EndInfo, 0),
        bid_count => maps:get(bid_count, EndInfo, 0),
        participant_count => maps:get(participant_count, EndInfo, 0),
        total_duration => maps:get(total_duration, EndInfo, 0)
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
        Username ->
            case server:get_balance(binary_to_list(Username)) of
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
        Username ->
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
                                                  binary_to_list(Username), 
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
                 <<"amount">> := Amount}, State) ->
    case State#state.user_id of
        undefined ->
            error_response(<<"Not authenticated">>, State);
        Username ->
            %% Check if user is a guest
            case State#state.is_guest of
                true ->
                    Response = jsx:encode(#{
                        type => <<"place_bid_response">>,
                        success => false,
                        error => <<"Guests cannot place bids. Please log in to participate.">>
                    }),
                    {reply, {text, Response}, State};
                false ->
                    %% Use auction_id from State (from JWT token)
                    case State#state.auction_id of
                        undefined ->
                            error_response(<<"No auction associated with this connection">>, State);
                        AuctionId ->
                            case server:place_bid(binary_to_list(AuctionId), 
                                                 binary_to_list(Username), 
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
                    end
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

%% Keepalive ping
handle_message(#{<<"type">> := <<"ping">>}, State) ->
    Response = jsx:encode(#{type => <<"pong">>}),
    {reply, {text, Response}, State};

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

%% Helper to determine type
type_of(X) when is_binary(X) -> binary;
type_of(X) when is_list(X) -> list;
type_of(X) when is_integer(X) -> integer;
type_of(X) when is_float(X) -> float;
type_of(X) when is_atom(X) -> atom;
type_of(_) -> unknown.

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
        Data = jsx:decode(PayloadJson, [return_maps]),
        #{<<"userid">> := UserId, 
          <<"expiredate">> := ExpireDate,
          <<"auctionid">> := AuctionId,
          <<"balance">> := Balance} = Data,
        
        %% Check expiration
        CurrentTime = os:system_time(second),
        case ExpireDate > CurrentTime of
            true ->
                {ok, binary_to_list(UserId), binary_to_list(AuctionId), Balance};
            false ->
                {error, token_expired}
        end
    catch
        _:_ -> 
            {error, invalid_token_format}
    end.

%% Verify real JWT with HMAC secret key
verify_real_jwt(Token) ->
    try
        %% Get JWT secret from environment (same as Java backend)
        JwtSecret = os:getenv("JWT_SECRET", "DSMT_PROJECT_SECRETKEY_MINIMUM_48_BYTES_FOR_HS384"),
        io:format("[WS] Using JWT secret: ~p~n", [JwtSecret]),
        io:format("[WS] Token to verify: ~p (type: ~p)~n", [Token, type_of(Token)]),
        
        %% Convert token to binary if needed
        TokenBin = case Token of
            T when is_binary(T) -> T;
            T when is_list(T) -> list_to_binary(T);
            T -> T
        end,
        io:format("[WS] Token as binary: ~p~n", [TokenBin]),
        
        %% Create HMAC key for verification
        JWK = jose_jwk:from_oct(list_to_binary(JwtSecret)),
        io:format("[WS] JWK created successfully~n"),
        
        %% Verify and decode JWT
        io:format("[WS] Calling jose_jwt:verify...~n"),
        VerifyResult = jose_jwt:verify(JWK, TokenBin),
        io:format("[WS] jose_jwt:verify result: ~p~n", [VerifyResult]),
        
        case VerifyResult of
            {true, JWT, _JWS} ->
                %% Extract claims - JWT is {jose_jwt, ClaimsMap}
                io:format("[WS] JWT structure: ~p~n", [JWT]),
                Claims = case JWT of
                    {jose_jwt, ClaimsMap} when is_map(ClaimsMap) -> 
                        ClaimsMap;
                    _ -> 
                        #{}
                end,
                io:format("[WS] JWT claims: ~p~n", [Claims]),
                
                %% Extract username from 'sub' claim (JWT subject)
                UsernameRaw = maps:get(<<"sub">>, Claims, undefined),
                io:format("[WS] UsernameRaw: ~p (type: ~p)~n", [UsernameRaw, type_of(UsernameRaw)]),
                
                Username = case UsernameRaw of
                    undefined -> undefined;
                    UName when is_binary(UName) -> binary_to_list(UName);
                    UName when is_list(UName) -> UName
                end,
                io:format("[WS] Username converted: ~p~n", [Username]),
                
                AuctionIdRaw = maps:get(<<"auctionId">>, Claims, maps:get(<<"auctionid">>, Claims, undefined)),
                io:format("[WS] AuctionIdRaw: ~p (type: ~p)~n", [AuctionIdRaw, type_of(AuctionIdRaw)]),
                
                AuctionId = case AuctionIdRaw of
                    undefined -> undefined;
                    AId when is_binary(AId) -> binary_to_list(AId);
                    AId when is_list(AId) -> AId;
                    AId when is_integer(AId) -> integer_to_list(AId)
                end,
                io:format("[WS] AuctionId converted: ~p~n", [AuctionId]),
                
                Balance = maps:get(<<"balance">>, Claims, 0.0),
                IsGuest = maps:get(<<"guest">>, Claims, false),
                io:format("[WS] IsGuest: ~p~n", [IsGuest]),
                
                %% Check expiration (exp claim is in seconds since epoch)
                case maps:get(<<"exp">>, Claims, undefined) of
                    undefined ->
                        {error, no_expiration};
                    ExpireDate ->
                        CurrentTime = os:system_time(second),
                        case ExpireDate > CurrentTime of
                            true ->
                                {ok, Username, AuctionId, Balance, IsGuest};
                            false ->
                                {error, token_expired}
                        end
                end;
            {false, _, _} ->
                io:format("[WS] JWT signature verification failed~n"),
                {error, invalid_signature}
        end
    catch
        Error:Reason:Stacktrace ->
            io:format("[WS] JWT verification exception - Error: ~p, Reason: ~p~n", [Error, Reason]),
            io:format("[WS] Stacktrace: ~p~n", [Stacktrace]),
            {error, jwt_verification_failed}
    end.

%% @doc Verify this slave node manages the specified auction
verify_auction_assignment(AuctionId) ->
    case mnesia_db:get_auction_assignment(AuctionId) of
        {ok, Assignment} ->
            %% Check if assigned slave matches current node
            AssignedNode = element(3, Assignment),  % auction_assignment.slave_node (3rd field)
            CurrentNode = node(),
            
            if
                AssignedNode =:= CurrentNode ->
                    io:format("[WS] Auction ~p correctly assigned to this node (~p)~n", 
                             [AuctionId, CurrentNode]),
                    ok;
                true ->
                    io:format("[WS] Auction ~p assigned to ~p but connected to ~p~n", 
                             [AuctionId, AssignedNode, CurrentNode]),
                    {error, wrong_slave}
            end;
        {error, not_found} ->
            io:format("[WS] Auction ~p not found in assignments~n", [AuctionId]),
            {error, auction_not_found};
        {error, Reason} ->
            {error, Reason}
    end.
