-module(events).
-export([init_events/0, put_event/1, get_all_events/0, get_events_from/1
,remember_account_event_number/1
,last_used_account_event_number/0
,handle_cast/2
,handle_missing_events/1
,handle_payload/1
,get_transfer_feed/0
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
    {noreply, business_logic:create_person(Person)};
handle_cast(Event = #event{number = Number, payload = _Payload}, _S) ->
    handle_payload(Event),
    {noreply, remember_account_event_number(Number)}.

handle_payload(#event{number = Number, payload = Payload} ) -> 
    io:format("Payload ~n~w", [Payload]),
    case Payload of
       #account{} -> business_logic:create_account(Payload);
       #person{} -> business_logic:create_person(Payload);
       #new_transfer_event{id = Id , timestamp = Timestamp , from_account_number = From_account_number, to_account_number = To_account_number, amount = Amount} -> remember_transfer_event_number(Number),
       business_logic:create_transfer(#transfer{id = Id, timestamp = Timestamp, from_account_number = From_account_number, to_account_number = To_account_number, amount = Amount});
       true -> ok
    end.

-spec handle_missing_events(list(#event{})) -> ok.
handle_missing_events([]) -> ok;
handle_missing_events([First | Rest]) -> 
    handle_payload(First), 
    handle_missing_events(Rest)
    .

-spec get_transfer_feed() -> {reply, list(#event{}) } .
 
get_transfer_feed() -> 
    gen_server:call(transfer_feed, {events, events:last_used_transfer_number() +1})
    .

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


-spec remember_transfer_event_number(non_neg_integer()) -> ok.
remember_transfer_event_number(Id) ->
    dets:insert(table_id, transfer_event, Id).



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
