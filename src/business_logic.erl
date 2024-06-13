%% This module represents the business logic layer

-module(business_logic).
-include("data.hrl").
-export([open_account/4, get_account/1, get_person/1, transfer/5, sort_transfers/1, get_transfers/1, transfer_exists/1]).

%% Opens an account, that is creates a new account containing a new person
%% Writes them into database.
-type open_account_result() :: {ok, #account{}} | {error, account_exists}.
-spec open_account(binary(), binary(), account_number(), money()) -> open_account_result().
open_account(GivenName, Surname, AccountNr, Amount) ->
    case get_account(AccountNr) of
        % OK, account does not yet exist
        {error, not_found} ->
            make_account(
                make_person(
                    GivenName, Surname, AccountNr
                ),
                Amount,
                AccountNr
            );
        % Error, account already exists
        {ok, _} ->
            {error, account_exists}
    end.

-spec get_account(account_number()) -> {ok, #account{}} | {error, not_found}.
get_account(AccountNumber) -> database:get_account(AccountNumber).

-spec make_person(binary(), binary(), number()) -> #person{}.
make_person(GivenName, Surname, AccountNr) ->
    Person = #person{
        id = AccountNr,
        given_name = GivenName,
        surname = Surname
    },
    database:put_person(Person),
    Person.

-spec get_person(unique_id()) -> {ok, #person{} | {error, any()}}.
get_person(Id) -> database:get_person(Id).

-spec make_account(#person{}, number(), number()) -> #account{}.
make_account(Person, Amount, AccountNr) ->
    AccountNumber = AccountNr,
    Account = #account{
        account_number = AccountNumber,
        person_id = Person#person.id,
        amount = Amount
    },
    database:put_account(Account),
    Account.

-spec get_transfers(unique_id()) -> list(#transfer{}).
get_transfers(Id) ->
    database:get_all_transfers(Id).

-spec transfer_exists(unique_id()) -> boolean().
transfer_exists(Tid) ->
    case database:get_transfer(Tid) of
        {ok, _} -> true;
        {error, _} -> false
    end.

%% Takes a sender & receiver account number and an amount and transfers 
%% that amount from sender to receiver.
%% Crashes if accounts do not exist.
%% Returns {ok, tid}, where tid is the id of the stored transfer
%% or {error, insufficient_funds} when there is not enough money in the sender account.

-spec transfer(unique_id(), account_number(), account_number(), money(), erlang:timestamp()) -> 
     {error, sender_account_not_found | receiver_account_not_found | insufficient_funds | tid_occupied}
   | {ok, unique_id()}.
transfer(TransferId, SenderAccountNumber, ReceiverAccountNumber, Amount, Timestamp) ->
    MaybeAccountSender = database:get_account(SenderAccountNumber),
    MaybeAccountReceiver = database:get_account(ReceiverAccountNumber),
    MaybeTransactionId = transfer_exists(TransferId),
    case {MaybeAccountSender, MaybeAccountReceiver, MaybeTransactionId} of
        {{error, not_found}, _, _} ->
            {error, sender_account_not_found};
        {_, {error, not_found}, _} ->
            {error, receiver_account_not_found};
        {_, _, true} ->
            {error, tid_occupied};
        {{ok, AccountSender}, {ok, AccountReceiver}, false} ->
            AccountSenderAmount = AccountSender#account.amount,
            AccountReceiverAmount = AccountReceiver#account.amount,

            if
                AccountSenderAmount - Amount >= 0 ->
                    Transfer = #transfer{
                        id = TransferId,
                        timestamp = Timestamp,
                        from_account_number = SenderAccountNumber,
                        to_account_number = ReceiverAccountNumber,
                        amount = Amount
                    },
                    NewAccountSender = AccountSender#account{amount = (AccountSenderAmount - Amount)},
                    NewAccountReceiver = AccountReceiver#account{amount = (AccountReceiverAmount + Amount)},
                    database:put_transfer(Transfer),
                    database:put_account(NewAccountSender),
                    database:put_account(NewAccountReceiver),
                    {ok, TransferId};
                true ->
                    {error, insufficient_funds}
            end
    end.

%% Takes a list of transfers and returns them sorted by their id (asc)

sort_transfers(Transfers) ->
    lists:sort(fun(Transfer1, Transfer2) -> Transfer2#transfer.id < Transfer1#transfer.id end, Transfers).
