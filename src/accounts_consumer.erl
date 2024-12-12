-module(accounts_consumer).
-behavior(gen_server).

-export([init/1, handle_cast/2, handle_call/3, handle_info/2,
         start/1]).

-record(state, {last_account_number :: number, receiver_node :: term(), subscription_timer :: timer:tref()}).
-record(update_subscription_timer, {}).
-record(subscribe_message, {last_account_number :: number()}).
-record(account_dto, {account_number :: number(),
                     person_id :: number(),
                     given_name :: string(),
                     surname :: string(),
                     amount :: number()}).
-record(account_dtos, {account_dtos :: list(#account_dto{})}).

% Verbindet sich mit dem Modul account_server des Prozesses mit dem short name "accounts".
start(ReceiverNode) ->
    gen_server:start(?MODULE, ReceiverNode, [{debug, [trace]}]).

init(ReceiverNode) ->
    {ok, SubscriptionTimer} = timer:send_interval(5000, #update_subscription_timer{}),
    {ok, #state{ last_account_number = 0, receiver_node = ReceiverNode, subscription_timer = SubscriptionTimer }}.

handle_info(#update_subscription_timer{}, State) ->
    {reply, Result, _} = gen_server:call({account_server, State#state.receiver_node}, #subscribe_message{last_account_number = State#state.last_account_number}),
    {ok, LastAccountNumber} = handle_account_dtos(Result#account_dtos.account_dtos, State#state.last_account_number),
    NewState = create_new_state(State, LastAccountNumber), 
    {noreply, NewState}.

create_new_state(State, NewLastAccountNumber) ->
    #state{ last_account_number = NewLastAccountNumber, receiver_node = State#state.receiver_node, subscription_timer = State#state.subscription_timer }.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(#account_dtos{account_dtos = AccountDtos}, State) ->
    {ok, LastAccountNumber} = handle_account_dtos(AccountDtos, State#state.last_account_number),
    NewState = create_new_state(State, LastAccountNumber), 
    {noreply, NewState}.

-spec handle_account_dtos(list(#account_dto{}), number()) -> {ok, number()}.
handle_account_dtos([], LastAccountNumber) -> {ok, LastAccountNumber};
handle_account_dtos([First | Rest], LastAccountNumber) ->
    business_logic:insert_account(First#account_dto.person_id,
                                  First#account_dto.given_name,
                                  First#account_dto.surname,
                                  First#account_dto.account_number,
                                  First#account_dto.amount),
    NewLastAccountNumber = max(LastAccountNumber, First#account_dto.account_number),
    handle_account_dtos(Rest, NewLastAccountNumber).