%%%-------------------------------------------------------------------
%% @doc erlbank_monolithic public API
%% @end
%%%-------------------------------------------------------------------

-module(erlbank_monolithic_app).
-include("events.hrl").

-behaviour(application).

-export([start/2, stop/1, subscribe/3]).

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
-spec subscribe(pid(),list(#event{}), service_name()) -> ok | error.

subscribe(Pid, [] , ServiceName) -> 
    gen_server:call(ServiceName, {Pid, no_events});
subscribe(Pid, [Event | _Rest], ServiceName) -> 
    gen_server:call(ServiceName, {Pid, Event}).

getenv(Var, Cont, Default) ->
    case os:getenv(Var) of
        false -> Default;
        Value -> Cont(Value)
    end.

start(_StartType, _StartArgs) ->
    AccountNode = getenv("ACCOUNTS_HOST",
                         fun (AccountsHost) -> list_to_atom("accounts@" ++ AccountsHost) end,
                         node()),
    TransferNode = getenv("TRANSFERS_HOST",
                          fun (TransfersHost) -> list_to_atom("transfers@" ++ TransfersHost) end,
                          node()),
    database:init_database(),
    start_cowboy(),
    accounts_mock:start_demo_link(),
    {ok, Pid} = events:init_events() ,
    {ok, _UpdaterPid} = transfer_feed:start_link(TransferNode),
    {ok, Events} = subscribe(Pid, events:last_used_account_event_number(), {account_service, AccountNode}),
    events:handle_missing_events(Events),
    erlbank_monolithic_sup:start_link()
    .

stop(_State) ->
    ok.

