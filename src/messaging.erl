-module(messaging).

-behaviour(gen_server).

-include("data.hrl").

-export([handle_call/3, init/1, handle_cast/2, receive_server_start/0, receive_server_start_link/0]).

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

init(InitialN) ->
    {ok, InitialN}.

-type mutation_message() :: #account_created{} | #transaction_succeeded{}.

-spec handle_call(mutation_message(), gen_server:from(), receive_server_state()) ->
    {reply, {ok, account_number_dto()}, receive_server_state()}
    | {reply, {ok, transaction_number_dto()}, receive_server_state()}
    | {reply, {error, term()}}.
handle_call(
    % gen_server:call(<0.263.0>, {account_created,3,"Peter","Lustig",2000}).
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
    % gen_server:call(<0.263.0>, {transaction_succeeded,101,1,2,1000,erlang:timestamp()}).
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
    case business_logic:transfer(TransactionId, FromAccountNr, ToAccountNr, Amount, Timestamp) of
        {ok, _Tid} -> {reply, {ok, TransactionId}, State};
        {error, Cause} -> {reply, {error, Cause}, State}
    end.

-spec handle_cast(term(), receive_server_state()) ->
    {noreply, receive_server_state()}.
handle_cast(_, State) ->
    {noreply, State}.

receive_server_start() ->
    gen_server:start(?MODULE, [], [{debug, [trace]}]).

receive_server_start_link() ->
    gen_server:start(?MODULE, [], [{debug, [trace]}]).
