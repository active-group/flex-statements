-module(sync).
-include("data.hrl").
-behavior(gen_server).
-export([start/3,handle_call/3,handle_cast/2,init/1,handle_info/2]).

-record(state,{
    accountLastEventId :: integer(),
    transferLastEventId :: integer(),
    accountsServiceName :: string(),
    transfersServiceName :: string()
}).

interval_milliseconds()-> 10000.
initTrigger()->
  timer:send_interval(interval_milliseconds(), interval),
  {ok}.


handle_info(_interval,State)->
  registerForEvents(State),
  {noreply,State}.

registerForEvents(#state{
    accountLastEventId = AccountLastEventId,
    transferLastEventId = TransferLastEventId,
    accountsServiceName = AccountsServiceName,
    transfersServiceName= TransfersServiceName}) ->
  % bei Account Service nachfragen
  MessageAccounts = #get_account_events_since{
    since= AccountLastEventId,
    receiver_pid= self()
  },
  logger:info("Sending ~p to ~p~n",[MessageAccounts,AccountsServiceName]),
  gen_server:cast(AccountsServiceName,MessageAccounts),
  % bei Transfer Service nachfragen
  % 
  logger:info("Sending ~p to ~p~n",[#get_transfer_events_since{
    since= TransferLastEventId,
    receiver_pid= self()
},TransfersServiceName]),
  gen_server:cast(TransfersServiceName,#get_transfer_events_since{
            since= TransferLastEventId,
            receiver_pid= self()
        }).



-spec init(string()) -> {ok, #state{}}.
init(State) ->
    initTrigger(),
    logger:info("Started own-service ~p~n",[self()]),
    registerForEvents(State),
    {ok, State}.

-spec start(string(),string(),string()) -> {ok,pid()}.
start(OwnServiceName, AccountsServiceName, TransfersServiceName) ->
    logger:info("Starting own-service ~n"),
    gen_server:start(
                    {local, OwnServiceName},
                    sync,
                    #state{
                        accountLastEventId = 0, 
                        transferLastEventId = 0,
                        accountsServiceName = AccountsServiceName,
                        transfersServiceName= TransfersServiceName
                    },
                    [{debug, [trace]}]).

handle_account_event(State,#account_event{id=Id}=Message)->

    business_logic:make_account(Message).
handle_transfer_event(State,Message)->ok.

%
% Wir nehmen Events an.
% unter dem Atom accounts erwarten wir ein Event nach der Definition aus https://github.com/active-group/flex-accounts/blob/2023-09/README.md
% unter dem Atom transfers erwarten wir ein Event nach der Definition aus https://github.com/active-group/flex-transfers/blob/main/README.md
% 
process_message(State,#account_event{id=Id, eventType=account_created}=Message) ->
    Ret = business_logic:make_account(Message),
    logger:info("Account created~p~n",[Ret]),
    State#state{accountLastEventId=Id};
process_message(State,#transfer_event{eventId=Id, source=transfer_service}=Message) ->
    Ret = business_logic:transfer(Message),
    logger:info("Transfer created~p~n",[Ret]),
    State#state{transferLastEventId=Id};
process_message(State,Message)->
    logger:info("Unknown Event ~p~n",[Message]),
    State.


handle_cast(Message, State)  ->
    logger:info("---->New Event ~p~n",[Message]),
    {noreply, process_message(State,Message)}.

handle_call(_Request, _From,State) -> {noreply,State}.