%% This module represents the business logic layer

-module(business_logic).
-include("data.hrl").
-export([get_account/1, get_person/1, transfer/4, sort_transfers/1, get_transfers/1, make_person/3, make_account/2]).


%% Opens an account, that is creates a new account containing a new person 
%% Writes them into database.

-spec get_account(account_number()) -> {ok, #account{}} | {error, any()}.
get_account(AccountNumber) -> database:get_account(AccountNumber).

-spec make_person(unique_id(), binary(), binary()) -> #person{}.
make_person(PersonId, GivenName, Surname) ->
    Person = #person{id = PersonId,
                   given_name = GivenName,
                   surname = Surname},
    database:put_person(Person),
    Person.

-spec get_person(unique_id()) -> {ok, #person{} | {error, any()}}.
get_person(Id) -> database:get_person(Id).

-spec make_account(unique_id(), #person{}) -> #account{}.
make_account(AccountNumber, Person) ->
    Account = #account{account_number = AccountNumber,
                   person_id = Person#person.id,
                   amount = 1000},
    database:put_account(Account),
    Account.

-spec get_transfers(unique_id()) -> list(#transfer{}).
get_transfers(Id) ->
     database:get_all_transfers(Id).

%% Takes a sender & receiver account number and an amount and transfers 
%% that amount from sender to receiver.
%% Crashes if accounts do not exist.
%% Returns {ok, tid}, where tid is the id of the stored transfer
%% or {error, insufficient_funds} when there is not enough money in the sender account.

-spec transfer(unique_id(), account_number(), account_number(), money()) ->
     {error, sender_account_not_found | receiver_account_not_found | insufficient_funds}
   | {ok, unique_id()}.
transfer(TransferId, SenderAccountNumber, ReceiverAccountNumber, Amount) ->

    TransferFunction =
      fun() -> 
        MaybeAccountSender = database:get_account(SenderAccountNumber),
        MaybeAccountReceiver = database:get_account(ReceiverAccountNumber),
        case {MaybeAccountSender, MaybeAccountReceiver} of
            {{error, not_found}, _} -> {error, sender_account_not_found};
            {_, {error, not_found}} -> {error, receiver_account_not_found};

            {{ok, AccountSender}, {ok, AccountReceiver}} ->
                AccountSenderAmount = AccountSender#account.amount,
                AccountReceiverAmount = AccountReceiver#account.amount,

                if
                    AccountSenderAmount - Amount >= 0 ->
                        Transfer = #transfer{id = TransferId,
                                            timestamp = erlang:timestamp(),
                                            from_account_number = SenderAccountNumber,
                                            to_account_number = ReceiverAccountNumber,
                                            amount = Amount},
                        NewAccountSender = AccountSender#account{amount = (AccountSenderAmount - Amount)},
                        NewAccountReceiver = AccountReceiver#account{amount = (AccountReceiverAmount + Amount)},
                        database:put_transfer(Transfer),
                        database:put_account(NewAccountSender),
                        database:put_account(NewAccountReceiver),
                        {ok, TransferId};
                    true ->
                        {error, insufficient_funds}
                end
        end
      end,

    database:atomically(TransferFunction).

%% Takes a list of transfers and returns them sorted by their id (asc)

sort_transfers(Transfers) ->
    lists:sort(fun(Transfer1, Transfer2) -> Transfer2#transfer.id < Transfer1#transfer.id end, Transfers).
