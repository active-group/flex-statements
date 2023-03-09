-module(transfer_consumer).
-behaviour(gen_server).
-include("data.hrl").
-export([init/1, handle_call/3, start_link/1, handle_cast/2, handle_info/2, terminate/2, code_change/3, process_events/2,
         make_state/2, update_pointer/2]).

-record(state, {transfers_node :: node(), pointer :: non_neg_integer()}).

%% for testing
-spec make_state(node(), non_neg_integer()) -> #state{}.
make_state(TransfersNode, Pointer) ->
    #state{transfers_node = TransfersNode,
           pointer = Pointer}.

-spec update_pointer(#state{}, non_neg_integer()) -> #state{}.
update_pointer(State, Pointer) ->
    State#state{ pointer = Pointer}.


-spec persist_event(#event{}) -> ok.
persist_event(#event{type = new_transfer_event, content = Content}) ->
    {TxId, _, _, _, _} = Content,
    lager:info("Persisting transfer with id ~p ~n", [TxId]),
    database:put_event(
      #event{index = database:next_event_index(),
             type = new_transfer_event,
             content = Content}).


-spec next_index(list(#event{}), non_neg_integer()) -> non_neg_integer().
next_index([], Pointer) -> Pointer;
next_index(Events, _) -> lists:max(lists:map(fun(Event) -> Event#event.index end, Events)) + 1.

-spec update_data(list(#event{})) -> ok.
update_data(Events) -> lists:foreach(fun persist_event/1, Events).

-spec process_events(list(#event{}), #state{}) -> #state{}.
process_events([], State) -> State;
process_events(Events, State) ->
    Pointer = State#state.pointer,
    E = lists:filter(fun(Ev) -> Ev#event.index >= Pointer end, Events),
    update_data(E),
    State#state{pointer = next_index(Events, Pointer)}.

start_link(TransfersNode) ->
    gen_server:start_link({local, transfer_consumer}, ?MODULE, [TransfersNode], []).

init([TransfersNode]) ->
    timer:send_interval(10000, fetch),
    {ok, #state{transfers_node = TransfersNode,
                pointer = 0}}.

handle_cast(Msg, State) ->
    lager:error("Received illegal cast: ~p~n", [Msg]),
    {noreply, State}.

handle_call(Msg, _, State) ->
    lager:error("Received illegal call: ~p~n", [Msg]),
    {reply, undefined, State}.

handle_info(fetch, State) ->
    try
        TransferNode = State#state.transfers_node,
        Pointer = State#state.pointer,
        Events = gen_server:call({transfer_feed, TransferNode}, {events, Pointer}),
        NextState = process_events(Events, State),
        {noreply, NextState}
    catch
        Error:Reason ->
            lager:error("Error reaching transfer feed: ~p : ~p~n", [Error, Reason]),
            {noreply, State}
    end;
handle_info(Info, State) ->
    lager:error("Received illegal info: ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
