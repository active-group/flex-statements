-module(messaging).

%-behaviour(gen_server).

-include("data.hrl").

-export([]).

-type account_id() :: number().
-type balance() :: number().
-record(account_opened, {
    id :: account_id(),
    surname :: string(),
    name :: string(),
    init_balance :: balance()
}).

%-spec handle_cast()
