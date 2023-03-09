-module(accounts_consumer).
-behaviour(gen_server).
-include("data.hrl").
-export([init/1, handle_call/3, start_link/1, handle_cast/2, handle_info/2, terminate/2, code_change/3, process_events/2,
         make_state/2, update_pointer/2]).

-record(state, {accounts_node :: node(), pointer :: non_neg_integer()}).

%% for testing
-spec make_state(node(), non_neg_integer()) -> #state{}.
make_state(AccountsNode, Pointer) ->
    #state{accounts_node = AccountsNode,
           pointer = Pointer}.

-spec update_pointer(#state{}, non_neg_integer()) -> #state{}.
update_pointer(State, Pointer) ->
    State#state{ pointer = Pointer}.

%% Handles only a single message (via !) named fetch 
%% that triggers a fetch, which it sends to itself.

-spec persist_event(#event{}) -> ok.
persist_event(#event{type = new_account_event, content = {AccountNumber, PersonId, Amount}}) ->
    lager:info("Persisting account with number: ~p~n", [AccountNumber]),
    database:put_event(
      #event{index = database:next_event_index(),
             type = new_account_event,
             content = {AccountNumber, PersonId, Amount}});

persist_event(#event{type = new_person_event, content = {PersonId, FirstName, LastName}}) ->
    lager:info("Persisting person with id: ~p~n", [PersonId]),
    database:put_event(
      #event{index = database:next_event_index(),
             type = new_person_event,
             content = {PersonId, FirstName, LastName}}).



%% @doc Computes the maximum event id plus 1
-spec next_index(list(#event{}), non_neg_integer()) -> non_neg_integer().
next_index([], Pointer) -> Pointer;
next_index(Events, _) -> lists:max(lists:map(fun(Event) -> Event#event.index end, Events)) + 1.


-spec update_data(list(#event{})) -> ok.
update_data(Events) -> lists:foreach(fun persist_event/1, Events).

-spec process_events(list(event), #state{}) -> #state{}.
process_events([], State) -> State;
process_events(Events, State) ->
    Pointer = State#state.pointer,
    E = lists:filter(fun(Ev) -> Ev#event.index >= Pointer end, Events),
    update_data(E),
    State#state { pointer = next_index(Events, Pointer)}.

start_link(AccountsNode) ->
    gen_server:start_link({local, accounts_consumer}, ?MODULE, [AccountsNode], []).

init([AccountsNode]) ->
    timer:send_interval(10000, fetch),
    {ok, #state{accounts_node = AccountsNode,
                pointer = 0}}.

handle_cast(Msg, State) ->
    lager:error("Received illegal cast: ~p~n", [Msg]),
    {noreply, State}.

handle_call(Msg, _, State) ->
    lager:error("Received illegal call: ~p~n", [Msg]),
    {reply, undefined, State}.


handle_info(fetch, State) ->
    try
        AccountsNode = State#state.accounts_node,
        Pointer = State#state.pointer,
        Events = gen_server:call({account_feed, AccountsNode}, {events, Pointer}),
        NextState = process_events(Events, State),
        {noreply, NextState}
    catch
        Error:Reason ->
            lager:error("Error consuming account events: ~p: ~p", [Error, Reason]),
            {noreply, State}
    end;

handle_info(Info, State) ->
    lager:error("Received illegal info: ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
