-module(event_server).
-include("data.hrl").

-export([init/1, transfer_created/2, person_created/2, account_created/2, start/0, handle_call/3, handle_cast/2, account_created_call/2, person_created_call/2, transfer_created_call/2]).

-record(person_creation_event,
  {id :: unique_id(),
  given_name :: binary(),
  surname :: binary()}).

-record(account_creation_event,
{account_number :: account_number(),
  person_id :: unique_id()}).

-record(transfer_creation_event,
{id :: unique_id(),
  timestamp :: erlang:timestamp(),
  from_account_number :: account_number(),
  to_account_number :: account_number(),
  amount :: money()}).

-behavior(gen_server).


-record(account_created, {creation_event :: #account_creation_event{}}).
-record(person_created, {creation_event :: #person_creation_event{}}).
-record(transfer_created, {creation_event :: #transfer_creation_event{}}).

-type cast_message() :: #account_created{} | #person_created{} | #transfer_created{}.

start() ->
  gen_server:start(event_server, [], [{debug, [trace]}]). % Module, Args, Options

-spec init(_Args) -> {ok}.
init(_Args) -> {ok, {}}.

-spec account_created(pid(), #account_creation_event{}) -> any().
account_created(Pid, AccountCreatedEvent) ->
  gen_server:cast(Pid, #account_created {creation_event = AccountCreatedEvent}).

-spec person_created(pid(), #person_creation_event{}) -> any().
person_created(Pid, PersonCreatedEvent) ->
  gen_server:cast(Pid, #person_created {creation_event = PersonCreatedEvent}).

-spec transfer_created(pid(), #transfer_creation_event{}) -> any().
transfer_created(Pid, TransferCreatedEvent) ->
  gen_server:cast(Pid, #transfer_created {creation_event = TransferCreatedEvent}).

-spec account_created_call(pid(), #account_creation_event{}) -> any().
account_created_call(Pid, AccountCreatedEvent) ->
  gen_server:call(Pid, #account_created {creation_event = AccountCreatedEvent}).

-spec person_created_call(pid(), #person_creation_event{}) -> any().
person_created_call(Pid, PersonCreatedEvent) ->
  gen_server:call(Pid, #person_created {creation_event = PersonCreatedEvent}).

-spec transfer_created_call(pid(), #transfer_creation_event{}) -> any().
transfer_created_call(Pid, TransferCreatedEvent) ->
  gen_server:call(Pid, #transfer_created {creation_event = TransferCreatedEvent}).

-spec handle_cast(cast_message(), any()) -> {noreply, any()}.
handle_cast(#account_created { creation_event = AccountCreatedEvent}, _N) ->
  case business_logic:get_person(AccountCreatedEvent#account_creation_event.person_id) of
    {error, _} -> {noreply, "Person does not exist"};
    {ok, Person} -> {noreply, business_logic:make_account(AccountCreatedEvent#account_creation_event.account_number, Person)}
  end;
handle_cast(#person_created { creation_event = PersonCreatedEvent}, _N) ->
  io:format("Create person: ~p~n", [PersonCreatedEvent]),
  {noreply, business_logic:make_person(
    PersonCreatedEvent#person_creation_event.id,
    PersonCreatedEvent#person_creation_event.given_name,
    PersonCreatedEvent#person_creation_event.surname)};
handle_cast(#transfer_created { creation_event = TransferCreatedEvent}, _N) ->
  {noreply, business_logic:transfer(
    TransferCreatedEvent#transfer_creation_event.id,
    TransferCreatedEvent#transfer_creation_event.from_account_number,
    TransferCreatedEvent#transfer_creation_event.to_account_number,
    TransferCreatedEvent#transfer_creation_event.amount)}.

-spec handle_call(cast_message(), pid(), any()) -> {reply, any(), any()}.
handle_call(#account_created { creation_event = AccountCreatedEvent}, _From, _N) ->
  case business_logic:get_person(AccountCreatedEvent#account_creation_event.person_id) of
    {error, _} -> {reply, {error, "Person does not exist"}, {}};
    {ok, Person} -> {reply, {ok, business_logic:make_account(AccountCreatedEvent#account_creation_event.account_number, Person)}, {}}
  end;
handle_call(#person_created { creation_event = PersonCreatedEvent}, _From, _N) ->
  {reply, { ok, business_logic:make_person(
    PersonCreatedEvent#person_creation_event.id,
    PersonCreatedEvent#person_creation_event.given_name,
    PersonCreatedEvent#person_creation_event.surname)}, {}};
handle_call(#transfer_created { creation_event = TransferCreatedEvent}, _From, _N) ->
  {reply, {ok, business_logic:transfer(
    TransferCreatedEvent#transfer_creation_event.id,
    TransferCreatedEvent#transfer_creation_event.from_account_number,
    TransferCreatedEvent#transfer_creation_event.to_account_number,
    TransferCreatedEvent#transfer_creation_event.amount)}, {}}.
