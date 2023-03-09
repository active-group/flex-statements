%%%-------------------------------------------------------------------
%% @doc erlbank_flex_bank_statement top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(erlbank_flex_bank_statements_sup).

-behaviour(supervisor).

-export([start_link/2]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link(AccountNode, TransferNode) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, {AccountNode, TransferNode}).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init({AccountNode, TransferNode}) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [#{id => formatter_sup,
                    start => {formatter_sup, start_link, []}},
                  #{id => accounts_consumer,
                    start => {accounts_consumer, start_link, [AccountNode]}}],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
