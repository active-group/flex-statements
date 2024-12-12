-module(transfers_consumer).
-behavior(gen_server).
-include("data.hrl").

-export([init/1, handle_cast/2, handle_call/3, handle_info/2,
         start/1]).

-record(state, {last_transfer_number :: number(), receiver_node :: term(), get_transfers_from_timer :: timer:tref()}).
-record(get_transfers_from_timer, {}).
-record(get_all_transfers_from,{start_transfer :: number()}).

% Verbindet sich mit dem Modul account_server des Prozesses mit dem short name "accounts".
start(ReceiverNode) ->
    gen_server:start(?MODULE, ReceiverNode, [{debug, [trace]}]).

init(ReceiverNode) ->
    {ok, GetTransfersFromTimer} = timer:send_interval(10000, #get_transfers_from_timer{}),
    {ok, #state{ last_transfer_number = 0, receiver_node = ReceiverNode, get_transfers_from_timer = GetTransfersFromTimer }}.

handle_info(#get_transfers_from_timer{}, State) ->
    StartTransfer = State#state.last_transfer_number + 1,
    try gen_server:call({transfers_server, State#state.receiver_node}, #get_all_transfers_from{start_transfer = StartTransfer}) of
        Results ->
            {ok, LastTransferNumber} = handle_transfers(Results, State#state.last_transfer_number),
            NewState = create_new_state(State, LastTransferNumber), 
            {noreply, NewState}
    catch _:_ ->
        logger:info("Call to transfers_server failed!"),
        {noreply, State}
    end.

create_new_state(State, NewLastTransferNumber) ->
    #state{ last_transfer_number = NewLastTransferNumber, receiver_node = State#state.receiver_node, get_transfers_from_timer = State#state.get_transfers_from_timer }.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

-spec handle_transfers(list(#transfer{}), number()) -> {ok, number()}.
handle_transfers([], LastTransferNumber) -> {ok, LastTransferNumber};
handle_transfers([First | Rest], LastAccountNumber) ->
    business_logic:insert_transfer(First#transfer.id,
                                  First#transfer.timestamp,
                                  First#transfer.from_account_number,
                                  First#transfer.to_account_number,
                                  First#transfer.amount),
    NewLastTransferNumber = max(LastAccountNumber, First#transfer.id),
    handle_transfers(Rest, NewLastTransferNumber).