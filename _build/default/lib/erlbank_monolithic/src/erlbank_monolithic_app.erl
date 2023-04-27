%%%-------------------------------------------------------------------
%% @doc erlbank_monolithic public API
%% @end
%%%-------------------------------------------------------------------

-module(erlbank_monolithic_app).

-behaviour(application).

-export([start/2, stop/1, subscribe/2]).




start_cowboy() ->
    %% Cowboy test code
    Dispatch = cowboy_router:compile([{'_', [{"/", web_frontend, index},
%                                             {"/accounts/open", web_frontend, open_account},
%                                             {"/transfers/create", web_frontend, create_transfer},
                                             {"/statements/request", web_frontend, request_statement}]}]),

    {ok, _} = cowboy:start_clear(my_http_listener,
                                 [{port, 8002}],
                                 #{env => #{dispatch => Dispatch}}).

-type service_name() :: account_service | accounts_mock .
-spec subscribe(pid(), service_name()) -> ok | error.

subscribe(Pid, ServiceName) -> 
    gen_server:call(ServiceName, Pid).

start(_StartType, _StartArgs) ->
    database:init_database(),
    start_cowboy(),
    accounts_mock:start_demo_link(),
    subscribe(self(), account_service),
    events:init_events(),
    erlbank_monolithic_sup:start_link()
    .

stop(_State) ->
    ok.

