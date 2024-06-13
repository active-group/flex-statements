%%%-------------------------------------------------------------------
%% @doc erlbank_monolithic public API
%% @end
%%%-------------------------------------------------------------------

-module(erlbank_monolithic_app).

-behaviour(application).

-export([start/2, stop/1]).

start_cowboy() ->
    %% Cowboy test code
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", web_frontend, index},
            {"/statements/request", web_frontend, request_statement}
        ]}
    ]),

    {ok,Pid} = messaging:receive_server_start(),
    register(statements, Pid),
    io:format("-----> Messaging Pid: ~w~n",[Pid]),

    {ok, _} = cowboy:start_clear(
        my_http_listener,
        [{port, 8002}],
        #{env => #{dispatch => Dispatch}}
    ).

start(_StartType, _StartArgs) ->
    database:init_database(),
    start_cowboy(),
    erlbank_monolithic_sup:start_link().

stop(_State) ->
    ok.
