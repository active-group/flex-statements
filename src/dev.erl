
-module(dev).
-include("data.hrl").
-export([setup/0]).

%% Meant for adding data to database in rebar3 shell env

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
             content = {0, <<"Matt">>, <<"Damon">>}}),
    database:put_event(
      #event{index = 3,
             type = new_person_event,
             content = {1, <<"Ben">>, <<"Affleck">>}}),
    database:put_event(
      #event{index = 4,
             type = new_transfer_event,
             content = {10, os:timestamp(), 0, 1, 500}}).
