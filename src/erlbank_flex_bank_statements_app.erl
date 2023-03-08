-module(erlbank_flex_bank_statements_app).
-behaviour(application).
-export([start/2, stop/1]).


-spec check_is_set(string()) -> ok.
check_is_set(Var) ->
    case os:getenv(Var) of
        false ->
            io:format("Missing var ~s~n", [Var]),
            halt(1);
        _ -> ok
    end.

start_cowboy() ->
    %% Cowboy test code
    Dispatch = cowboy_router:compile([
                                      {'_', [{"/", web_frontend, index},
                                             {"/bank-statements/request", web_frontend, request}]}]),
    {ok, _} = cowboy:start_clear(my_http_listener,
                                 [{port, 8002}],
                                 #{env => #{dispatch => Dispatch}}).


start(_StartType, _StartArgs) ->

    lager:info("Starting bank-statement-service: ~p~n", [node()]),

    start_cowboy(),

    database:init_database(),

    Res = erlbank_flex_bank_statements_sup:start_link(),

    Res.

stop(_State) ->
    database:destroy_tables().

%% internal functions
