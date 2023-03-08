%% This module represents the database layer

-module(database).
-include("data.hrl").
-export([destroy_tables/0, put_event/1, init_database/0, transfer_events/0,  account/1, next_event_index/0, account_events/0]).

%% destroy tables in case they already existed
destroy_tables() ->
    dets:close(event), % better safe than sorry
    file:delete("event.dets"),
    dets:close(table_id),
    file:delete("table_id.dets").

create_tables() ->
    {ok, event} = dets:open_file(event, [{keypos, #event.index}, {file, "event.dets"}]),
    {ok, table_id} = dets:open_file(table_id, [{file, "table_id.dets"}]),
    ok = dets:insert(table_id, {event, 0}).

init_database() ->
    ok = create_tables(),
    ok.

put_event(Event) ->
    dets:insert(event, Event).

-spec get_all_events(atom()) -> list(#event{}).
get_all_events(Type) ->
    dets:select(event, [{'$1',
                        [{'==', {element, #event.type, '$1'}, Type}],
                        ['$_']}]).

transfer_events() -> get_all_events(new_transfer_event).
account_events() -> get_all_events(new_account_event).
person_events() -> get_all_events(new_person_event).

-spec person(unique_id()) -> #person{}.
person(PersonId)->
    PersonEvents = person_events(),
    [Event] = lists:filter(fun(#event{content = {OPersonId, _, _}}) -> OPersonId == PersonId end, PersonEvents),
    {event, _, _, {_, FirstName, LastName}} = Event,
    #person{first_name = FirstName, last_name = LastName}.

-spec sort_transfer_events(list(#event{})) -> list(#event{}).
sort_transfer_events(Transfers) ->
    lists:sort(fun (#event{index = ID1}, #event{index = ID2}) -> ID1 > ID2 end, Transfers).

-spec transfer_events_for(account_number()) -> list(#event{}).
transfer_events_for(AccountNumber) ->
    TransferEvents = transfer_events(),
    lists:filter(fun(#event{content = {_, _, Sender, Receiver, _}}) ->
                         (Sender == AccountNumber)
                             orelse (Receiver == AccountNumber) end, 
                TransferEvents).

-spec squash_transfer(#transfer{}, money(), account_number()) -> money().
squash_transfer(#transfer{from_acc_nr = Sender, amount= TxAmount}, Amount, AccountNumber) ->
    if (Sender == AccountNumber) -> Amount - TxAmount;
       true -> Amount + TxAmount
    end.

-spec squash_transfers(money(), account_number(), list(#transfer{})) -> money().
squash_transfers(Amount, AccountNumber, Txs) ->
    lists:foldl(fun(Tx, TmpAmount) -> squash_transfer(Tx, TmpAmount, AccountNumber) end,
                Amount, Txs).

-spec event_to_transfer(#event{}) -> #transfer{}.
event_to_transfer(#event{content = {TransferId, Timestamp, FromId, ToId, Amount}}) ->
    #transfer{id = TransferId,
                 timestamp = Timestamp,
                 from_acc_nr = FromId,
                 to_acc_nr = ToId,
                 amount = Amount}.

-spec transfers(account_number()) -> list(#transfer{}).
transfers(AccountNumber) ->
    MatchingTransfers = transfer_events_for(AccountNumber),
    SortedTransfers = sort_transfer_events(MatchingTransfers),
    lists:map(fun event_to_transfer/1, SortedTransfers).

-spec account_events_for(account_number()) -> list(#event{}).
account_events_for(AccountNumber) ->
    AccountEvents = account_events(),
    %% We can assume, that there is only one event here
    lists:filter(fun({event, _, _, {Num, _, _}}) ->
                         Num == AccountNumber end, AccountEvents).

-spec account(account_number()) -> {ok, #account{}} | {error, account_not_found}.
account(AccountNumber) ->
    case account_events_for(AccountNumber) of
        [#event{content = {_, PersonId, InitialAmount}}] ->
            Txs = transfers(AccountNumber),
            RealAmount = squash_transfers(InitialAmount, AccountNumber, Txs),
            {ok, #account{person = person(PersonId),
                          transfers = Txs,
                          amount = RealAmount}};
        _ -> {error, account_not_found}
    end.

-spec next_event_index() -> non_neg_integer().
next_event_index() -> dets:update_counter(table_id, event, 1).
