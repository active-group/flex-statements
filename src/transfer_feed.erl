-module(transfer_feed).
-behavior(gen_server).
-export([start_link/1, init/1, handle_cast/2, handle_call/3]).

start_link(TransfersNode) ->
    gen_server:start_link({local, transfers_updater}, ?MODULE, TransfersNode, []).

init(TransfersNode) ->
    {ok, TransfersNode}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call(update, _From, TransfersNode) ->
    LastNumber = events:last_used_transfer_event_number(),
    logger:info("Last number is: ~p~n", [LastNumber]),
    {reply,
     gen_server:call({transfer_feed, TransfersNode}, {events, LastNumber +1}),
     TransfersNode}.
