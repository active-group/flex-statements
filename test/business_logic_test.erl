-module(business_logic_test).
-include_lib("eunit/include/eunit.hrl").
-include("../src/data.hrl").


setup() ->
    ok.

cleanup(_) ->
    ok.

main_test_() ->
    {inorder,
     {foreach,
      fun setup/0,
      fun cleanup/1,
      [fun transfer_test/1]
     }}.


transfer_test(_) ->
    SenderAccountNumber = 1234,
    ReceiverAccountNumber = 5678,
    Amount = 100,
    Timestamp = erlang:now(),

    %% Test case where sender account not found
    database:stub_get_account(SenderAccountNumber, {error, not_found}),
    database:stub_get_account(ReceiverAccountNumber, {ok, #account{}}),
    ?assertEqual({error, sender_account_not_found}, business_logic:transfer(#transfer_event{
        accountIdSender = SenderAccountNumber,
        accountIdReceiver = ReceiverAccountNumber,
        amount = Amount,
        timestamp = Timestamp
    })),

    %% Test case where receiver account not found
    database:stub_get_account(SenderAccountNumber, {ok, #account{}}),
    database:stub_get_account(ReceiverAccountNumber, {error, not_found}),
    ?assertEqual({error, receiver_account_not_found}, business_logic:transfer(#transfer_event{
        accountIdSender = SenderAccountNumber,
        accountIdReceiver = ReceiverAccountNumber,
        amount = Amount,
        timestamp = Timestamp
    })),

    %% Test case where sender has insufficient funds
    database:stub_get_account(SenderAccountNumber, {ok, #account{amount = 50}}),
    database:stub_get_account(ReceiverAccountNumber, {ok, #account{}}),
    ?assertEqual({error, insufficient_funds}, business_logic:transfer(#transfer_event{
        accountIdSender = SenderAccountNumber,
        accountIdReceiver = ReceiverAccountNumber,
        amount = Amount,
        timestamp = Timestamp
    })),

    %% Test case where transfer is successful
    database:stub_get_account(SenderAccountNumber, {ok, #account{amount = 200}}),
    database:stub_get_account(ReceiverAccountNumber, {ok, #account{}}),
    database:stub_unique_transfer_id(123),
    ?assertEqual({ok, 123}, business_logic:transfer(#transfer_event{
        accountIdSender = SenderAccountNumber,
        accountIdReceiver = ReceiverAccountNumber,
        amount = Amount,
        timestamp = Timestamp
    })),

    ok.