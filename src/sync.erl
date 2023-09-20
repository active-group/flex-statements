-module(sync).
-behavior(gen_server).



handle_account_event(State,Message)->ok.
handle_transfer_event(State,Message)->ok.

process_message(State,Message)->ok.

handle_cast(Message, State) ->
    {noreply, process_message(State,Message)}.