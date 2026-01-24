%%%-------------------------------------------------------------------
%%% @author Auction System
%%% @doc
%%% Distributed Mnesia database module for the auction system.
%%% Handles users, auctions, bids with distributed replication.
%%% @end
%%%-------------------------------------------------------------------
-module(mnesia_db).
-author("auction_system").

%% API exports
-export([
    create_schema/1,
    create_tables/0,
    start_mnesia/0,
    stop_mnesia/0,
    init_tables/0,
    join_cluster/1,
    add_table_copy/2,
    
    %% User operations
    add_user/3,
    get_user/1,
    get_user_balance/1,
    update_user_balance/2,
    add_balance/2,
    withdraw_balance/2,
    
    %% Auction operations
    add_auction/7,
    get_auction/1,
    get_active_auctions/0,
    get_waiting_auctions/0,
    get_completed_auctions/0,
    update_auction_winner/3,
    cancel_auction/1,
    add_user_to_waitlist/2,
    get_auction_waitlist/1,
    
    %% Bid operations
    add_bid/4,
    get_auction_bids/1,
    get_highest_bid/1
]).

-record(user, {
    username,           % Primary key
    password,
    balance = 0.0
}).

-record(auction, {
    auction_id,         % Primary key (unique name)
    creator,            % Username of creator
    item_name,
    min_bid,
    duration,           % Duration in seconds
    start_time,         % Timestamp when auction starts
    winner = none,      % Username of winner
    winning_bid = 0.0,
    node_pid,           % Process handling this auction
    waitlist = []       % List of usernames waiting to participate
}).

-record(bid, {
    bid_id,             % {AuctionId, Timestamp, Username}
    auction_id,
    username,
    amount,
    timestamp,          % Erlang monotonic time for ordering
    node                % Node where bid originated
}).

%%%===================================================================
%%% Schema and initialization
%%%===================================================================

%% @doc Create the mnesia schema on given nodes
create_schema(Nodes) ->
    mnesia:stop(),
    case mnesia:create_schema(Nodes) of
        ok -> 
            io:format("[MNESIA] Schema created on nodes: ~p~n", [Nodes]),
            ok;
        {error, {_, {already_exists, _}}} ->
            io:format("[MNESIA] Schema already exists~n"),
            ok;
        Error ->
            io:format("[MNESIA] Error creating schema: ~p~n", [Error]),
            Error
    end.

%% @doc Start mnesia application
start_mnesia() ->
    case application:start(mnesia) of
        ok -> 
            io:format("[MNESIA] Started on node ~p~n", [node()]),
            ok;
        {error, {already_started, mnesia}} ->
            io:format("[MNESIA] Already started~n"),
            ok;
        Error ->
            io:format("[MNESIA] Error starting: ~p~n", [Error]),
            Error
    end.

%% @doc Stop mnesia application
stop_mnesia() ->
    application:stop(mnesia).

%% @doc Create tables (same as init_tables but more intuitive name)
create_tables() ->
    init_tables().

%% @doc Initialize all tables with distributed replication
init_tables() ->
    %% Create user table
    case mnesia:create_table(user, [
        {attributes, record_info(fields, user)},
        {disc_copies, [node()]},
        {type, set}
    ]) of
        {atomic, ok} -> io:format("[MNESIA] User table created~n");
        {aborted, {already_exists, user}} -> io:format("[MNESIA] User table exists~n");
        Error1 -> io:format("[MNESIA] Error creating user table: ~p~n", [Error1])
    end,
    
    %% Create auction table
    case mnesia:create_table(auction, [
        {attributes, record_info(fields, auction)},
        {disc_copies, [node()]},
        {type, set}
    ]) of
        {atomic, ok} -> io:format("[MNESIA] Auction table created~n");
        {aborted, {already_exists, auction}} -> io:format("[MNESIA] Auction table exists~n");
        Error2 -> io:format("[MNESIA] Error creating auction table: ~p~n", [Error2])
    end,
    
    %% Create bid table with ordered_set for timestamp ordering
    case mnesia:create_table(bid, [
        {attributes, record_info(fields, bid)},
        {disc_copies, [node()]},
        {type, bag}  % Allow multiple bids per auction
    ]) of
        {atomic, ok} -> io:format("[MNESIA] Bid table created~n");
        {aborted, {already_exists, bid}} -> io:format("[MNESIA] Bid table exists~n");
        Error3 -> io:format("[MNESIA] Error creating bid table: ~p~n", [Error3])
    end,
    
    ok.

%% @doc Join an existing mnesia cluster
join_cluster(MasterNode) ->
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    
    %% Create schema as part of cluster
    case mnesia:create_schema([node(), MasterNode]) of
        ok -> io:format("[MNESIA] Schema created for cluster~n");
        {error, {_, {already_exists, _}}} -> io:format("[MNESIA] Schema exists~n");
        SchemaError -> io:format("[MNESIA] Schema error: ~p~n", [SchemaError])
    end,
    
    mnesia:start(),
    
    case mnesia:change_config(extra_db_nodes, [MasterNode]) of
        {ok, [MasterNode]} ->
            io:format("[MNESIA] Successfully connected to master: ~p~n", [MasterNode]),
            
            %% Wait a bit for schema sync
            timer:sleep(1000),
            
            %% Copy schema to local node
            case mnesia:change_table_copy_type(schema, node(), disc_copies) of
                {atomic, ok} -> 
                    io:format("[MNESIA] Schema copied to local disc~n");
                SchemaErr ->
                    io:format("[MNESIA] Schema copy warning: ~p~n", [SchemaErr])
            end,
            
            %% Copy tables from master node
            add_table_copy(user, node()),
            add_table_copy(auction, node()),
            add_table_copy(bid, node()),
            
            io:format("[MNESIA] Cluster join complete!~n"),
            ok;
        {ok, []} ->
            io:format("[MNESIA] Warning: Could not connect to master node ~p~n", [MasterNode]),
            {error, no_connection};
        Error ->
            io:format("[MNESIA] Error joining cluster: ~p~n", [Error]),
            Error
    end.

%% @doc Add a table copy to a node
add_table_copy(Table, Node) ->
    case mnesia:add_table_copy(Table, Node, disc_copies) of
        {atomic, ok} ->
            io:format("[MNESIA] Added ~p table copy to node ~p~n", [Table, Node]),
            ok;
        {aborted, {already_exists, Table, Node}} ->
            io:format("[MNESIA] Table ~p already exists on node ~p~n", [Table, Node]),
            ok;
        Error ->
            io:format("[MNESIA] Error adding table copy: ~p~n", [Error]),
            Error
    end.

%%%===================================================================
%%% User operations
%%%===================================================================

%% @doc Add a new user to the system
add_user(Username, Password, InitialBalance) ->
    F = fun() ->
        case mnesia:read(user, Username) of
            [] ->
                User = #user{
                    username = Username,
                    password = Password,
                    balance = InitialBalance
                },
                mnesia:write(User),
                {ok, Username};
            [_] ->
                {error, user_exists}
        end
    end,
    case mnesia:transaction(F) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

%% @doc Get user information
get_user(Username) ->
    F = fun() -> mnesia:read(user, Username) end,
    case mnesia:transaction(F) of
        {atomic, [User]} -> {ok, User};
        {atomic, []} -> {error, not_found};
        {aborted, Reason} -> {error, Reason}
    end.

%% @doc Get user balance
get_user_balance(Username) ->
    case get_user(Username) of
        {ok, User} -> {ok, User#user.balance};
        Error -> Error
    end.

%% @doc Update user balance
update_user_balance(Username, NewBalance) ->
    F = fun() ->
        case mnesia:read(user, Username) of
            [User] ->
                UpdatedUser = User#user{balance = NewBalance},
                mnesia:write(UpdatedUser),
                ok;
            [] ->
                {error, user_not_found}
        end
    end,
    case mnesia:transaction(F) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

%% @doc Add amount to user balance
add_balance(Username, Amount) when Amount > 0 ->
    F = fun() ->
        case mnesia:read(user, Username) of
            [User] ->
                NewBalance = User#user.balance + Amount,
                UpdatedUser = User#user{balance = NewBalance},
                mnesia:write(UpdatedUser),
                {ok, NewBalance};
            [] ->
                {error, user_not_found}
        end
    end,
    case mnesia:transaction(F) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end;
add_balance(_Username, _Amount) ->
    {error, invalid_amount}.

%% @doc Withdraw amount from user balance
withdraw_balance(Username, Amount) when Amount > 0 ->
    F = fun() ->
        case mnesia:read(user, Username) of
            [User] ->
                case User#user.balance >= Amount of
                    true ->
                        NewBalance = User#user.balance - Amount,
                        UpdatedUser = User#user{balance = NewBalance},
                        mnesia:write(UpdatedUser),
                        {ok, NewBalance};
                    false ->
                        {error, insufficient_balance}
                end;
            [] ->
                {error, user_not_found}
        end
    end,
    case mnesia:transaction(F) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end;
withdraw_balance(_Username, _Amount) ->
    {error, invalid_amount}.

%%%===================================================================
%%% Auction operations
%%%===================================================================

%% @doc Create a new auction
add_auction(AuctionId, Creator, ItemName, MinBid, _BidIncrement, Duration, StartTime) ->
    F = fun() ->
        case mnesia:read(auction, AuctionId) of
            [] ->
                %% If StartTime is 0, use current timestamp
                ActualStartTime = case StartTime of
                    0 -> erlang:system_time(second);
                    _ -> StartTime
                end,
                Auction = #auction{
                    auction_id = AuctionId,
                    creator = Creator,
                    item_name = ItemName,
                    min_bid = MinBid,
                    duration = Duration,
                    start_time = ActualStartTime,
                    waitlist = []
                },
                mnesia:write(Auction),
                {ok, AuctionId};
            [_] ->
                {error, auction_exists}
        end
    end,
    case mnesia:transaction(F) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

%% @doc Get auction details
get_auction(AuctionId) ->
    F = fun() -> mnesia:read(auction, AuctionId) end,
    case mnesia:transaction(F) of
        {atomic, [Auction]} -> {ok, Auction};
        {atomic, []} -> {error, not_found};
        {aborted, Reason} -> {error, Reason}
    end.

%% @doc Get all active auctions (based on timestamps)
get_active_auctions() ->
    F = fun() ->
        CurrentTime = erlang:system_time(second),
        AllAuctions = mnesia:match_object(#auction{_ = '_'}),
        %% Filter auctions that are currently active
        lists:filter(fun(Auction) ->
            StartTime = Auction#auction.start_time,
            Duration = Auction#auction.duration,
            EndTime = StartTime + Duration,
            (CurrentTime >= StartTime) andalso (CurrentTime < EndTime)
        end, AllAuctions)
    end,
    case mnesia:transaction(F) of
        {atomic, Auctions} -> {ok, Auctions};
        {aborted, Reason} -> {error, Reason}
    end.

%% @doc Get all waiting auctions (based on timestamps)
get_waiting_auctions() ->
    F = fun() ->
        CurrentTime = erlang:system_time(second),
        AllAuctions = mnesia:match_object(#auction{_ = '_'}),
        %% Filter auctions that haven't started yet
        lists:filter(fun(Auction) ->
            StartTime = Auction#auction.start_time,
            CurrentTime < StartTime
        end, AllAuctions)
    end,
    case mnesia:transaction(F) of
        {atomic, Auctions} -> {ok, Auctions};
        {aborted, Reason} -> {error, Reason}
    end.

%% @doc Get all completed auctions (based on timestamps)
get_completed_auctions() ->
    F = fun() ->
        CurrentTime = erlang:system_time(second),
        AllAuctions = mnesia:match_object(#auction{_ = '_'}),
        %% Filter auctions that have ended
        lists:filter(fun(Auction) ->
            StartTime = Auction#auction.start_time,
            Duration = Auction#auction.duration,
            EndTime = StartTime + Duration,
            CurrentTime >= EndTime
        end, AllAuctions)
    end,
    case mnesia:transaction(F) of
        {atomic, Auctions} -> {ok, Auctions};
        {aborted, Reason} -> {error, Reason}
    end.

%% @doc Update auction winner and winning bid
update_auction_winner(AuctionId, Winner, WinningBid) ->
    F = fun() ->
        case mnesia:read(auction, AuctionId) of
            [Auction] ->
                UpdatedAuction = Auction#auction{
                    winner = Winner,
                    winning_bid = WinningBid
                },
                mnesia:write(UpdatedAuction),
                ok;
            [] ->
                {error, auction_not_found}
        end
    end,
    case mnesia:transaction(F) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

%% @doc Cancel an auction (only if not yet started)
cancel_auction(AuctionId) ->
    F = fun() ->
        CurrentTime = erlang:system_time(second),
        case mnesia:read(auction, AuctionId) of
            [Auction] when Auction#auction.start_time > CurrentTime ->
                mnesia:delete({auction, AuctionId}),
                ok;
            [_] ->
                {error, cannot_cancel};
            [] ->
                {error, auction_not_found}
        end
    end,
    case mnesia:transaction(F) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

%% @doc Add user to auction waitlist
add_user_to_waitlist(AuctionId, Username) ->
    F = fun() ->
        CurrentTime = erlang:system_time(second),
        case mnesia:read(auction, AuctionId) of
            [Auction] when Auction#auction.start_time > CurrentTime ->
                Waitlist = Auction#auction.waitlist,
                case lists:member(Username, Waitlist) of
                    false ->
                        UpdatedAuction = Auction#auction{
                            waitlist = Waitlist ++ [Username]
                        },
                        mnesia:write(UpdatedAuction),
                        ok;
                    true ->
                        {error, already_in_waitlist}
                end;
            [_] ->
                {error, auction_not_waiting};
            [] ->
                {error, auction_not_found}
        end
    end,
    case mnesia:transaction(F) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

%% @doc Get auction waitlist
get_auction_waitlist(AuctionId) ->
    case get_auction(AuctionId) of
        {ok, Auction} -> {ok, Auction#auction.waitlist};
        Error -> Error
    end.

%%%===================================================================
%%% Bid operations
%%%===================================================================

%% @doc Add a new bid (with logical timestamp for ordering)
add_bid(AuctionId, Username, Amount, Timestamp) ->
    F = fun() ->
        BidId = {AuctionId, Timestamp, Username},
        Bid = #bid{
            bid_id = BidId,
            auction_id = AuctionId,
            username = Username,
            amount = Amount,
            timestamp = Timestamp,
            node = node()
        },
        mnesia:write(Bid),
        ok
    end,
    case mnesia:transaction(F) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

%% @doc Get all bids for an auction, ordered by timestamp
get_auction_bids(AuctionId) ->
    F = fun() ->
        MatchHead = #bid{auction_id = AuctionId, _ = '_'},
        Bids = mnesia:match_object(MatchHead),
        %% Sort by timestamp
        lists:sort(fun(B1, B2) -> 
            B1#bid.timestamp =< B2#bid.timestamp 
        end, Bids)
    end,
    case mnesia:transaction(F) of
        {atomic, Bids} -> {ok, Bids};
        {aborted, Reason} -> {error, Reason}
    end.

%% @doc Get the highest bid for an auction
get_highest_bid(AuctionId) ->
    case get_auction_bids(AuctionId) of
        {ok, []} -> {ok, none};
        {ok, Bids} ->
            %% Find bid with highest amount
            HighestBid = lists:foldl(fun(Bid, Acc) ->
                case Acc of
                    none -> Bid;
                    _ -> 
                        if Bid#bid.amount > Acc#bid.amount -> Bid;
                           true -> Acc
                        end
                end
            end, none, Bids),
            {ok, HighestBid};
        Error -> Error
    end.
