-module(erlbank_flex_statements_app).
-behaviour(application).
-export([start/2, stop/1]).


start_cowboy() ->
    %% Cowboy test code
    Dispatch = cowboy_router:compile([{'_', [{"/", web_frontend, index},
                                            {"/statements", web_frontend, index},
                                             {"/statements/request", web_frontend, request_statement}]}]),
    {ok, _} = cowboy:start_clear(my_http_listener,
                                 [{port, 8002}],
                                 #{env => #{dispatch => Dispatch}}).


start(_StartType, _StartArgs) ->

    logger:info("Starting accounts-service: ~p~n", [node()]),

    start_cowboy(),
    database:init_database(),

    Res = erlbank_flex_statements_sup:start_link(),
    logger:info("Started account feed: ~p~n", [node()]),
    Res.


stop(_State) ->
    %% database:destroy_tables().
    ok.

%% internal functions
