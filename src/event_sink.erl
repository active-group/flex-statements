-module(event_sink).
-include("event.hrl").
-export([start/1, handle_call/3, init/1, handle_cast/2]).
-behavior(gen_server).

-type state() :: state.

call(Pid, Request) ->
  case gen_server:call(Pid, Request) of
    ok -> ok;
    _ -> call(Pid, Request)
  end.

-spec start(pid()) -> any().
start(SourcePid) ->
  gen_server:start(?MODULE, SourcePid, [{debug, [trace]}]).

-spec init(pid()) -> {ok, state()}.
init(SourcePid) ->
  call(SourcePid , #register{}),
  {ok, state}.

-spec handle_call(any(), pid(), state()) -> {reply, any(), state()}.
handle_call(#account_creation_event{person_id = PersonId, account_number = AccountNumber}, _From, State) ->
  case business_logic:get_person(PersonId) of
    {error, _} -> {reply, {error, "Person not found"}, State};
    {ok, Person} -> 
      business_logic:make_account(AccountNumber, Person),
      {reply, ok, State}
  end;
handle_call(#person_creation_event{id = Id, given_name = GivenName, surname = Surname}, _From, State) ->
  business_logic:make_person(Id, GivenName, Surname),
  {reply, ok, State};
handle_call(#transfer_creation_event{id = Id, from_account_number = From, to_account_number = To, amount = Amount}, _From, State) ->
  business_logic:transfer(Id, From, To, Amount),
  {reply, ok, State};
handle_call(_Any, _From, State) ->
  {reply, ok, State}.


-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast(_Any, State) ->
  {noreply, State}.
