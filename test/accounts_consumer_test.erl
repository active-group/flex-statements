-module(accounts_consumer_test).
-include_lib("eunit/include/eunit.hrl").


setup() ->
    database:destroy_tables(),
    database:init_database(),
    timer:sleep(400).

cleanup(_) ->
    database:destroy_tables().


main_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun events_are_stored/1,
      fun same_events_are_overridden/1,
      fun keeps_state_if_timeout/1
     ]}.



events_are_stored(_) ->
    fun() ->
            State = accounts_consumer:make_state(node_node, 0),

            State1 = accounts_consumer:process_events([{event, 0, new_account_event, {0, something, 1000}}], State),

            ?assertEqual(State1, accounts_consumer:update_pointer(State, 1)),

            Events = database:account_events(),
            ?assertEqual(1, length(Events)),

            State2 = accounts_consumer:process_events([{event, 1, new_account_event, {0, something, 1000}}], State1),

            ?assertEqual(State2, accounts_consumer:update_pointer(State, 2)),

            Events2 = database:account_events(),
            ?assertEqual(2, length(Events2))
    end.


same_events_are_overridden(_) ->
    fun() ->
            State = accounts_consumer:make_state(no_node, 0),

            State1 = accounts_consumer:process_events([{event, 0, new_account_event, {0, something, 1000}}], State),

            ?assertEqual(State1, accounts_consumer:update_pointer(State, 1)),

            Events = database:account_events(),
            ?assertEqual(1, length(Events)),

            State2 = accounts_consumer:process_events([{event, 0, new_account_event, {0, something, 1000}}], State1),

            ?assertEqual(State2, accounts_consumer:update_pointer(State, 1)),

            Events2 = database:account_events(),

            ?assertEqual(1, length(Events2))
    end.

keeps_state_if_timeout(_) ->
    fun() ->
            State = #{accounts_node => no_node,
                      pointer => 0},

            {noreply, State1} = accounts_consumer:handle_info(fetch, State),

            ?assertEqual(State, State1)
    end.
