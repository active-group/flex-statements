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
handle_call(#account_creation_event{}, _From, State) ->
  case business_logic:get_person(#account_creation_event.person_id) of
    {error, _} -> {reply, {error, "Person not found"}, State};
    {ok, Person} ->
      {reply, {ok, business_logic:make_account(#account_creation_event.account_number, Person)}, State}
  end;
handle_call(#person_creation_event{}, _From, State) ->
  {reply, { ok, business_logic:make_person(
    #person_creation_event.id,
    #person_creation_event.given_name,
    #person_creation_event.surname)}, State};
handle_call(#transfer_creation_event{}, _From, State) ->
  {reply, {ok, business_logic:transfer(
    #transfer_creation_event.id,
    #transfer_creation_event.from_account_number,
    #transfer_creation_event.to_account_number,
    #transfer_creation_event.amount)}, State};
handle_call(_Any, _From, State) ->
  {reply, ok, State}.


-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast(_Any, State) ->
  {noreply, State}.
