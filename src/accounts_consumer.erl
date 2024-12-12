-module(accounts_consumer).
-behavior(gen_server).

-export([init/1, handle_cast/2, handle_call/3, handle_info/2,
         start/1]).

-record(state, {last_account_number :: number, receiver_node :: term(), subscription_timer :: timer:tref()}).
-record(update_subscription_timer, {}).
-record(subscribe, {last_account_number :: number(), client_pid :: number()}).
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
    {ok, SubscriptionTimer} = timer:send_interval(10000, #update_subscription_timer{}),
    {ok, #state{ last_account_number = 0, receiver_node = ReceiverNode, subscription_timer = SubscriptionTimer }}.

handle_info({welcome, WelcomeMessage, _NewAccounts}, State) ->
    io:format("welcome message received:~s~n", [WelcomeMessage]),
    {noreply, State};

handle_info(#update_subscription_timer{}, State) ->
    Results = gen_server:call({account_server, State#state.receiver_node}, #subscribe{last_account_number = State#state.last_account_number, client_pid = self()}),
    
    case Results of
        ok -> {noreply, State};
        [] -> 
            io:format("empty account list received~n"),
            {noreply, State};
        _ ->
            io:format("accounts received~n"),
            {ok, LastAccountNumber} = handle_account_dtos(Results, State#state.last_account_number),
    NewState = create_new_state(State, LastAccountNumber), 
    {noreply, NewState}
    end;

handle_info(#account_dtos{account_dtos = AccountDtos}, State) ->
    {ok, LastAccountNumber} = handle_account_dtos(AccountDtos, State#state.last_account_number),
    NewState = create_new_state(State, LastAccountNumber), 
    {noreply, NewState}.

create_new_state(State, NewLastAccountNumber) ->
    #state{ last_account_number = NewLastAccountNumber, receiver_node = State#state.receiver_node, subscription_timer = State#state.subscription_timer }.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_, State) ->
    {noreply, State}.

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