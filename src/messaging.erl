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
    amount :: number(),
    person_id :: number()
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

-spec handle_cast(mutation_message(), receive_server_state()) ->
    {noreply, receive_server_state()}.
handle_cast(
    % gen_server:call(<0.263.0>, {account_created,3,"Peter","Lustig",2000}).
    #account_created{
        account_number = AccountNr,
        given_name = GivenName,
        surname = Surname,
        amount = Amount,
        person_id = _PersonId
    },
    State
) ->
    case business_logic:open_account(GivenName, Surname, AccountNr, Amount) of
        {ok, _Account} ->
            io:format("Konto ~w gespeichert~n", [AccountNr]),
            notify_accounts_processed(AccountNr),
            % neuer Zustand (keine Änderung)
            {noreply, State};
        {error, account_exists} ->
            io:format("Kontonummer ~w existiert bereits - schade~n", [AccountNr]),
            notify_accounts_processed(AccountNr),
            {noreply, State}
    end;
% transfer(account_number(), account_number(), money())
handle_cast(
    % gen_server:call(<0.263.0>, {transaction_succeeded,101,1,2,1000,erlang:timestamp()}).
    #transaction_succeeded{
        transaction_id = TransactionId,
        from_account_number = FromAccountNr,
        to_account_number = ToAccountNr,
        amount = Amount,
        timestamp = Timestamp
    },
    State
) ->
    case business_logic:transfer(TransactionId, FromAccountNr, ToAccountNr, Amount, Timestamp) of
        {ok, _Tid} ->
            io:format("Banktransfer gespeichert ~w~n", [TransactionId]),
            notify_transfers_processed(TransactionId),
            {noreply, State};
        {error, tid_occupied} ->
            io:format("Transaktionsnummer ~w leider bereits belegt~n", [TransactionId]),
            notify_transfers_processed(TransactionId),
            {reply, State};
        {error, Cause} ->
            io:format("Leider ist der Fehler ~w passiert bei Verarbeitung von Transaktionsnummer ~w~n", [Cause, TransactionId]),
            notify_transfers_processed(TransactionId),
            {reply, State}
    end.

-spec notify_transfers_processed(unique_id()) -> ok.
notify_transfers_processed(TransactionId) ->
    notify_processed(transfer_succeeded_ack, "TRANSFERS_HOST", TransactionId).

-spec notify_accounts_processed(unique_id()) -> ok.
notify_accounts_processed(AccountId) ->
    notify_processed(accounts, "ACCOUNTS_HOST", AccountId).

-spec notify_processed(atom(), string(), unique_id()) -> ok.
notify_processed(Sender, EnvVar, MessageId) ->
    ReplyAdr = node_util:node_from_env(Sender, EnvVar),
    io:format("Sende Bestätigung an ~w~n", [ReplyAdr]),
    gen_server:cast(ReplyAdr, {ok, <<"statements">>, MessageId}).

-spec handle_call(term(), gen_server:from(), receive_server_state()) ->
    {noreply, receive_server_state()}.
handle_call(_, _, State) ->
    {noreply, State}.

receive_server_start() ->
    gen_server:start(?MODULE, [], [{debug, [trace]}]).

receive_server_start_link() ->
    gen_server:start(?MODULE, [], [{debug, [trace]}]).
