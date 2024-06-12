-module(messaging).

-behaviour(gen_server).

-include("data.hrl").

-export([]).

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
    | {reply, {ok, transaction_number_dto()}, receive_server_state()}.
handle_call(
    #account_created{
        account_number = AccountNr,
        given_name = GivenName,
        surname = Surname,
        amount = Amount
    },
    From,
    State
) ->
    todo;
handle_call(
    #transaction_succeeded{
        transaction_id = TransactionId,
        from_account_number = FromAccountNr,
        to_account_number = ToAccountNr,
        amount = Amount,
        timestamp = Timestamp
    },
    From,
    State
) ->
    todo.
