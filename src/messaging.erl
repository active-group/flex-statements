-module(messaging).

-behaviour(gen_server).

-include("data.hrl").

-export([handle_call/3]).

-type receive_server_state() :: list().
-type account_number_dto() :: number().
-type transaction_number_dto() :: number().

-record(account_created, {
    account_number :: account_number_dto(),
    given_name :: binary(),
    surname :: binary(),
    amount :: number()
}).

-record(transaction_succeeded, {
    transaction_id :: transaction_number_dto(),
    from_account_number :: number(),
    to_account_number :: number(),
    amount :: number(),
    timestamp :: erlang:timestamp()
}).

-type mutation_message() :: #account_created{} | #transaction_succeeded{}.

-spec handle_call(mutation_message(), pid(), receive_server_state()) ->
    {reply, {ok, account_number_dto()}, receive_server_state()}
    | {reply, {ok, transaction_number_dto()}, receive_server_state()}
    | {reply, {error, term()}}.
handle_call(
    #account_created{
        account_number = AccountNr,
        given_name = GivenName,
        surname = Surname,
        amount = Amount
    },
    _From,
    []
) ->
    business_logic:open_account(GivenName, Surname, AccountNr, Amount),
    {reply,
        % Antwort
        {ok, AccountNr},
        % neuer Zustand
        []};
% transfer(account_number(), account_number(), money())
handle_call(
    #transaction_succeeded{
        transaction_id = TransactionId,
        from_account_number = FromAccountNr,
        to_account_number = ToAccountNr,
        amount = Amount,
        timestamp = Timestamp
    },
    _From,
    State
) ->
    case business_logic:transfer(FromAccountNr, ToAccountNr, Amount, Timestamp) of
        {ok, _Tid} -> {reply, {ok, TransactionId}, State};
        {error, Cause} -> {reply, {error, Cause}}
    end.
