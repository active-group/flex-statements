-module(messaging).

%-behaviour(gen_server).

-include("data.hrl").

-export([]).

-type person_id() :: number().
-type account_id() :: number().
-type balance() :: number().
-record(account_created, {
    person_id :: account_id(),
    surname :: string(),
    name :: string(),
    init_balance :: balance()
}).

%-spec handle_cast()
