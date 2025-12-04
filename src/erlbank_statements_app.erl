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

    AccountNode = node_util:node_from_env(accounts, "ACCOUNTS_HOST"),
    TransferNode = node_util:node_from_env(transfers, "TRANSFERS_HOST"),
    logger:info("Consuming accounts on: ~p~n", [AccountNode]),
    logger:info("Consuming transfers on: ~p~n", [TransferNode]),

    event_sink:start({ event_sender, AccountNode }),
    event_sink:start({ transfers, TransferNode }),
    erlbank_statements_sup:start_link().

stop(_State) ->
    ok.

