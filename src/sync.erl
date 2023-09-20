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

%
% Wir nehmen Events an.
% unter dem Atom accounts erwarten wir ein Event nach der Definition aus https://github.com/active-group/flex-accounts/blob/2023-09/README.md
% unter dem Atom transfers erwarten wir ein Event nach der Definition aus https://github.com/active-group/flex-transfers/blob/main/README.md
% 
process_message(State,{accounts,Message}) ->
    handle_account_event(State,Message),
    State;
process_message(State,{transfers,Message}) ->
    handle_transfer_event(State,Message),
    State;

handle_cast(Message, State) ->
    {noreply, process_message(State,Message)}.