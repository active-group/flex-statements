-module(sync).
-include("data.hrl").
-behavior(gen_server).
-export([start/3,handle_call/3,handle_cast/2,init/1]).

-record(state,{
    accountLastEventId :: integer(),
    transferLastEventId :: integer(),
    accountsServiceName :: string(),
    transfersServiceName :: string()
}).

interval_milliseconds()-> 30000.
initTrigger()->
  timer:send_interval(interval_milliseconds(), interval),
  {ok}.


handle_info(_interval,State)->
  registerForEvents(State),
  {noreply}.

registerForEvents(#state{
    accountLastEventId = AccountLastEventId,
    transferLastEventId = TransferLastEventId,
    accountsServiceName = AccountsServiceName,
    transfersServiceName= TransfersServiceName}) ->
  % bei Account Service nachfragen
  gen_server:cast(AccountsServiceName,get_account_events_since,[#getLastType{
        since= AccountLastEventId,
        receiver_pid= self()
    }]),
  % bei Transfer Service nachfragen
  gen_server:cast(TransfersServiceName,get_transfer_events_since,[#getLastType{
            since= TransferLastEventId,
            receiver_pid= self()
        }]).



-spec init(string()) -> {ok, #state{}}.
init(State) ->
    initTrigger(),
    logger:info("Started own-service ~p~n",[self()]),
    {ok, State}.

-spec start(string(),string(),string()) -> {ok,pid()}.
start(OwnServiceName, AccountsServiceName, TransfersServiceName) ->
    logger:info("Starting own-service ~n"),
    gen_server:start(
                    [local, OwnServiceName],
                    sync,
                    #state{
                        accountLastEventId = 0, 
                        transferLastEventId = 0,
                        accountsServiceName = AccountsServiceName,
                        transfersServiceName= TransfersServiceName
                    },
                    [{debug, [trace]}]).

handle_account_event(State,#accountEvent{id=Id}=Message)->

    business_logic:make_account(Message).
handle_transfer_event(State,Message)->ok.

%
% Wir nehmen Events an.
% unter dem Atom accounts erwarten wir ein Event nach der Definition aus https://github.com/active-group/flex-accounts/blob/2023-09/README.md
% unter dem Atom transfers erwarten wir ein Event nach der Definition aus https://github.com/active-group/flex-transfers/blob/main/README.md
% 
process_message(State,#accountEvent{id=Id}=Message) ->
    business_logic:make_account(Message),
    State#state{accountLastEventId=Id};
process_message(State,#transferEvent{eventId=Id}=Message) ->
    business_logic:transfer(Message),
    State#state{transferLastEventId=Id}.

handle_cast(Message, State)  ->
    {noreply, process_message(State,Message)}.

handle_call(_Request, _From,State) -> {noreply,State}.