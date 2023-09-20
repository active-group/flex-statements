-module(sync).
-behavior(gen_server).

-record(lastServiceIds,{
    accountServiceId :: integer(),
    transferServiceId :: integer()}).


-spec init(string()) -> {ok, #lastServiceIds{}}.
init(LastServiceIds) ->
    {ok, #lastServiceIds{accountServiceId = 0, transferServiceId = 0}}.

-spec start(string()) -> {ok,pid()}.
start(ownSerivceName) ->
    gen_server:start(ownSerivceName,
                    #lastServiceIds{accountServiceId = 0, transferServiceId = 0},
                    [{debug, [trace]}]).

handle_account_event(State,Message)->ok.
handle_transfer_event(State,Message)->ok.

process_message(State,Message)->ok.

handle_cast(Message, State) ->
    {noreply, process_message(State,Message)}.