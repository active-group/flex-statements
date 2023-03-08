-module(business_logic_test).
-include("data.hrl").
-include_lib("eunit/include/eunit.hrl").


setup() ->
    database:destroy_tables(),
    database:init_database(),
    database:put_event(
      #event{index = 0,
             type = new_account_event,
             content = {0, 0, 1000}}),
    database:put_event(
      #event{index = 1,
             type = new_account_event,
             content = {1, 1, 1000}}),
    database:put_event(
      #event{index = 2,
             type = new_person_event,
             content = {0, "Matt", "Damon"}}),
    database:put_event(
      #event{index = 3,
             type = new_person_event,
             content = {1, "Ben", "Affleck"}}),
    database:put_event(
      #event{index = 4,
             type = new_transfer_event,
             content = {10, some_date, 0, 1, 500}}),
    timer:sleep(400).

cleanup(_) ->
    database:destroy_tables().


main_test_() ->
    {inorder,
      {foreach,
      fun setup/0,
      fun cleanup/1,
      [
       fun reads_account_properly/1
      ]}}.



reads_account_properly(_) ->
    fun() ->
            {ok, Acc1} = business_logic:account(0),
            {ok, Acc2} = business_logic:account(1),
            P1 = Acc1#account.person,
            P2 = Acc2#account.person,
            ?assertEqual(P1#person.first_name, "Matt"),
            ?assertEqual(P2#person.first_name, "Ben"),
            ?assertEqual(Acc1#account.amount, 500),
            ?assertEqual(Acc2#account.amount, 1500),

            database:put_event(
              #event{index = 5,
                     type = new_transfer_event,
                     content = {10, some_date, 0, 1, 500}}),

            {ok, Acc3} = business_logic:account(0),
            {ok, Acc4} = business_logic:account(1),
            ?assertEqual(Acc3#account.amount, 0),
            ?assertEqual(Acc4#account.amount, 2000)
    end.
