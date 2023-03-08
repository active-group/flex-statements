-module(formatter_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link(formatter_sup, []).

init(_Args) ->
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
    ChildSpecs = [#{id => exchange_service,
                    start => {exchange_service, start_link, []}},
                  #{id => number_formatter,
                    start => {number_formatter, start_link, []}},
                  #{id => date_formatter,
                    start => {date_formatter, start_link, []}}],
    {ok, {SupFlags, ChildSpecs}}.
