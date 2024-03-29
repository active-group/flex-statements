%% This module represents the business logic layer

-module(business_logic).
-include("data.hrl").
-export([get_account/1, transfer/1, sort_transfers/1, get_transfers/1, make_account/1, get_amount/1 ]).


-spec get_account(account_number()) -> {ok, #account{}} | {error, any()}.
get_account(AccountNumber) -> database:get_account(AccountNumber).


-spec make_account(#account_event{}) -> #account{}.
make_account(Account_event) ->
    Account = #account{
      account_number = Account_event#account_event.account_number,
      amount = 0,
      given_name = Account_event#account_event.givenName,
      surname = Account_event#account_event.surname
      },
    database:put_account(Account),
    Account.

-spec get_transfers(unique_id()) -> list(#transfer{}).
get_transfers(Id) ->
     database:get_all_transfers(Id).
  
get_amount(AccountNumber) ->
  List = get_transfers(AccountNumber),
  logger:info("~p~n",[List]),
  Amounts = lists:map(fun (#transfer{amount=Amount,from_account_number=From}) -> 
    if From == AccountNumber -> 0 - Amount;
    true -> Amount
    end
  end,List),
  logger:info("Amounts : ~p~n",[Amounts]),
  lists:sum(Amounts).

%% Takes a sender & receiver account number and an amount and transfers 
%% that amount from sender to receiver.
%% Crashes if accounts do not exist.
%% Returns {ok, tid}, where tid is the id of the stored transfer
%% or {error, insufficient_funds} when there is not enough money in the sender account.

-spec transfer(#transfer_event{}) ->
     {error, sender_account_not_found | receiver_account_not_found | insufficient_funds}
   | {ok, unique_id()}.
transfer(#transfer_event{ accountIdSender = SenderAccountNumber,
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
