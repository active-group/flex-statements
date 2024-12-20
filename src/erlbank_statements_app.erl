%%%-------------------------------------------------------------------
%% @doc erlbank_statements public API
%% @end
%%%-------------------------------------------------------------------

-module(erlbank_statements_app).

-behaviour(application).

-export([start/2, stop/1]).




start_cowboy() ->
    %% Cowboy test code
    Dispatch = cowboy_router:compile([{'_', [{"/", web_frontend, index},
                                             {"/statements/request", web_frontend, request_statement}]}]),

    {ok, _} = cowboy:start_clear(my_http_listener,
                                 [{port, 8002}],
                                 #{env => #{dispatch => Dispatch}}).


start(_StartType, _StartArgs) ->
    database:init_database(),
    start_cowboy(),
    start_accounts_consumer(),
    start_transfers_consumer(),
    erlbank_statements_sup:start_link().

stop(_State) ->
    ok.

% Startet die Komponente zur Abfrage der neu eröffneten Konten.
start_accounts_consumer() ->
    AccountNode = node_util:node_from_env(accounts, "ACCOUNTS_HOST"),
    accounts_consumer:start(AccountNode).

start_transfers_consumer() ->
    TransferNode = node_util:node_from_env(transfers, "TRANSFERS_HOST"),
    transfers_consumer:start(TransferNode).