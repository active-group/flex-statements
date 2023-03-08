-module(business_logic).
-include("data.hrl").
-export([account/1, person_by_accnr/1]).


-spec account(account_number()) -> {ok, #account{}} | {error, account_not_found}.
account(AccountNumber) -> database:account(AccountNumber).

-spec person_by_accnr(account_number()) -> #person{}.
person_by_accnr(AccountNumber) ->
    {ok, Acc} = account(AccountNumber),
    Acc#account.person.
