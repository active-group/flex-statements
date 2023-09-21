%% This module represents the business logic layer

-module(business_logic).
-include("data.hrl").
-export([get_account/1, transfer/1, sort_transfers/1, get_transfers/1, make_account/1 ]).


-spec get_account(account_number()) -> {ok, #account{}} | {error, any()}.
get_account(AccountNumber) -> database:get_account(AccountNumber).


-spec make_account(#accountEvent{}) -> #account{}.
make_account(AccountEvent) ->
    Account = #account{
      account_number = AccountEvent#accountEvent.account_number,
      amount = 0,
      given_name = AccountEvent#accountEvent.givenName,
      surname = AccountEvent#accountEvent.surname
      },
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

-spec transfer(#transferEvent{}) ->
     {error, sender_account_not_found | receiver_account_not_found | insufficient_funds}
   | {ok, unique_id()}.
transfer(#transferEvent{ accountIdSender = SenderAccountNumber,
  accountIdReceiver = ReceiverAccountNumber,
  amount = Amount,
  timestamp = Timestamp}) ->
    TransferId = database:unique_transfer_id(),
    Transfer = #transfer{id = TransferId,
                        timestamp = Timestamp,
                        from_account_number = SenderAccountNumber,
                        to_account_number = ReceiverAccountNumber,
                        amount = Amount},
    database:put_transfer(Transfer),
    {ok, TransferId}.

%% Takes a list of transfers and returns them sorted by their id (asc)

sort_transfers(Transfers) ->
    lists:sort(fun(Transfer1, Transfer2) -> Transfer2#transfer.id < Transfer1#transfer.id end, Transfers).
