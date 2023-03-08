-module(client).
-include("data.hrl").
-export([bank_statement/1]).


-spec pretty_name(#person{}) -> string().
pretty_name({person, FirstName, LastName}) ->
    binary_to_list(FirstName) ++ " " ++ binary_to_list(LastName).

%% prints the header of a bank statement, namely the full name and the
%% current balance, associated with the account number to stdout.
-spec print_head(#account{}) -> ok.
print_head(Account) ->
    Person = Account#account.person,
    Name = pretty_name(Person),
    io:format("~nBank statement for: ~s~n", [Name]),
    io:format("---------------------------------------------------- ~n", []),
    io:format("Balance: ~p~n", [Account#account.amount]),
    io:format("---------------------------------------------------- ~n", []).


%% takes an transfer record and prints it to stdout.
-spec print_tx(#transfer{}) -> ok.
print_tx(Tx) ->
    P1 = business_logic:person_by_accnr(Tx#transfer.from_acc_nr),
    P2 = business_logic:person_by_accnr(Tx#transfer.to_acc_nr),
    Name1 = pretty_name(P1),
    Name2 = pretty_name(P2),
    Amount = Tx#transfer.amount,
    Id = Tx#transfer.id,
    io:format("#~p\t ~p\t ~s \t -> ~s ~n", [Id, Amount, Name1, Name2]).

%% takes a list of transfers records and prints them to stdout
-spec print_txs(list(#transfer{})) -> ok.
print_txs(Txs) ->
    lists:foreach(fun print_tx/1, Txs).

%% takes an account number and prints a bank statement to stdout.
%% That is a full name, the current balance, and a list of
%% transfers associated with the account.
-spec bank_statement(account_number()) -> ok.
bank_statement(AccountNumber) ->
    {ok, Account} = business_logic:account(AccountNumber),
    Txs = Account#account.transfers,

    %% SortedRelevantTxs = business_logic:sort_tx(RelevantTxs),

    print_head(Account),
    print_txs(Txs),

    io:format("~n~n", []).
