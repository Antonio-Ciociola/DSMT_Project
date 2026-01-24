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
    {cowboy_websocket, Req, #state{}}.

%% @doc Called after WebSocket upgrade
websocket_init(State) ->
    io:format("[WS] WebSocket initialized~n"),
    {ok, State}.

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
websocket_info({auction_update, AuctionId, Data}, State) ->
    %% Forward auction updates to subscribed clients
    case lists:member(AuctionId, State#state.subscriptions) of
        true ->
            Msg = jsx:encode(#{
                type => <<"auction_update">>,
                auction_id => AuctionId,
                data => Data
            }),
            {reply, {text, Msg}, State};
        false ->
            {ok, State}
    end;
websocket_info({notification, Data}, State) ->
    %% Send notifications to client
    Msg = jsx:encode(#{
        type => <<"notification">>,
        data => Data
    }),
    {reply, {text, Msg}, State};
websocket_info(_Info, State) ->
    {ok, State}.

%% @doc Called when WebSocket connection closes
terminate(_Reason, _Req, State) ->
    io:format("[WS] WebSocket closed for user: ~p~n", [State#state.user_id]),
    %% Cleanup: unsubscribe from auctions, etc.
    ok.

%%%===================================================================
%%% Internal Message Handlers
%%%===================================================================

%% Register new user
handle_message(#{<<"type">> := <<"register">>, 
                 <<"user_id">> := UserId,
                 <<"password">> := Password,
                 <<"balance">> := Balance}, State) ->
    io:format("[SERVER] Registering user: ~p~n", [UserId]),
    case server:register_user(binary_to_list(UserId), binary_to_list(Password), Balance) of
        {ok, _Username} ->
            Response = jsx:encode(#{
                type => <<"register_response">>,
                success => true,
                user_id => UserId
            }),
            {reply, {text, Response}, State#state{user_id = UserId}};
                {error, Reason} ->
            Response = jsx:encode(#{
                type => <<"register_response">>,
                success => false,
                error => format_error(Reason)
            }),
            {reply, {text, Response}, State}
    end;

%% Authenticate user
handle_message(#{<<"type">> := <<"login">>,
                 <<"user_id">> := UserId,
                 <<"password">> := Password}, State) ->
    case server:authenticate_user(binary_to_list(UserId), binary_to_list(Password)) of
        {ok, authenticated} ->
            Response = jsx:encode(#{
                type => <<"login_response">>,
                success => true,
                user_id => UserId
            }),
            {reply, {text, Response}, State#state{user_id = UserId}};
        {error, Reason} ->
            Response = jsx:encode(#{
                type => <<"login_response">>,
                success => false,
                error => format_error(Reason)
            }),
            {reply, {text, Response}, State}
    end;

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

%% Create auction
handle_message(#{<<"type">> := <<"create_auction">>,
                 <<"item_name">> := ItemName,
                 <<"start_price">> := StartPrice,
                 <<"duration">> := Duration,
                 <<"starting_date">> := StartingDate}, State) ->
    case State#state.user_id of
        undefined ->
            error_response(<<"Not authenticated">>, State);
        UserId ->
            %% BidIncrement = 1 (default increment), StartingDate from client
            case server:create_auction(binary_to_list(UserId), 
                                      binary_to_list(ItemName),
                                      StartPrice, 1, 
                                      Duration, StartingDate) of
                {ok, AuctionId} ->
                    Response = jsx:encode(#{
                        type => <<"create_auction_response">>,
                        success => true,
                        auction_id => list_to_binary(AuctionId)
                    }),
                    {reply, {text, Response}, State};
                {error, Reason} ->
                    Response = jsx:encode(#{
                        type => <<"create_auction_response">>,
                        success => false,
                        error => format_error(Reason)
                    }),
                    {reply, {text, Response}, State}
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

%% Get waiting auctions
handle_message(#{<<"type">> := <<"get_waiting_auctions">>}, State) ->
    case server:get_waiting_auctions() of
        {ok, Auctions} ->
            Response = jsx:encode(#{
                type => <<"waiting_auctions_response">>,
                auctions => format_auctions(Auctions)
            }),
            {reply, {text, Response}, State};
        {error, Reason} ->
            error_response(format_error(Reason), State)
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
                Duration, StartTime, Winner, WinningBid, _NodePid, Waitlist}) ->
    #{
        auction_id => list_to_binary(AuctionId),
        creator => list_to_binary(Creator),
        item_name => list_to_binary(ItemName),
        min_bid => MinBid,
        duration => Duration,
        start_time => StartTime,
        winner => format_winner(Winner),
        winning_bid => WinningBid,
        waitlist_count => length(Waitlist)
    };
format_auction(_) ->
    #{}.

format_winner(none) -> null;
format_winner(Winner) when is_list(Winner) -> list_to_binary(Winner);
format_winner(Winner) -> Winner.
