%% Auction application resource file
{application, auction_app, [
    {description, "Distributed Auction System"},
    {vsn, "1.0.0"},
    {modules, [
        auction_app,
        auction_supervisor,
        server,
        auction_handler,
        mnesia_db
    ]},
    {registered, [auction_supervisor, server]},
    {applications, [kernel, stdlib, mnesia]},
    {mod, {auction_app, []}},
    {env, []}
]}.
