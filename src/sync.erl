-module(sync).
-behavior(gen_server).




process_message(State,Message)->ok.

handle_cast(Message, State) ->
    {noreply, process_message(State,Message)}.