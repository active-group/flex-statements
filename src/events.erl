-module(events).
-export([init_events/0, put_event/1, get_all_events/0, get_events_from/1
,remember_account_event_number/1
,last_used_account_event_number/0
,handle_cast/2
,init/1]).
-include("events.hrl").
-include("data.hrl").

-behaviour(gen_server).
% call after database: init_database/0

-type state() :: [#event{}].

-spec init(state()) -> {ok, state()}.

-type message() :: #account{} | #person{}.
-spec handle_cast(message(), state()) -> {noreply, state()}.

handle_cast(Account = #account{}, _S) ->
    {noreply, business_logic:create_account(Account)};
handle_cast(Person = #person{}, _S) ->
    {noreply, business_logic:create_person(Person)}.
    

init(InitialState) -> 
    {ok, InitialState}.



init_events()->
    dets:close(event),
    file:delete("event.dets"),
    {ok, event} = dets:open_file(event, [{type, set}, {file, "event.dets"}]),
    dets:insert(table_id, {event, 0}),
    gen_server:start(?MODULE, get_all_events(), [{debug, [trace]}]).


-spec unique_event_number() -> non_neg_integer().
unique_event_number() -> dets:update_counter(table_id, event, 1).

-spec put_event(term()) -> #event{}.
put_event(Payload) ->
    Number = unique_event_number(),
    database:write(event, {Number, Payload}),
    #event{number = Number, payload = Payload}.

-spec deserialize_event({non_neg_integer(), term()}) -> #event{}.
deserialize_event({Number, Payload}) ->
    #event{number = Number, payload = Payload}.

-spec last_used_account_event_number() -> [#event{}].

last_used_account_event_number() ->
    dets:lookup(table_id, account_event).

-spec remember_account_event_number(non_neg_integer()) -> ok.
remember_account_event_number(Id) ->
    dets:insert(table_id, account_event, Id).



-spec get_all_events() -> [#event{}].
get_all_events() ->
    database:read_all(event, fun deserialize_event/1).

-spec get_events_from(non_neg_integer()) -> [#event{}].
get_events_from(Number) ->
    Res = dets:select(event,
                        [{'$1',
                        [{'>=', {element, 1, '$1'}, Number}],
                        ['$_']}]),
    Events = lists:map(fun deserialize_event/1, Res),
    lists:sort(fun (#event{number = Number1}, #event{number = Number2}) -> Number1 =< Number2 end, Events).
