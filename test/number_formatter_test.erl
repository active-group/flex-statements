-module(number_formatter_test).
-include("data.hrl").
-include_lib("eunit/include/eunit.hrl").


setup() ->
    {ok, Pid} = number_formatter:start_link(),
    Pid.
cleanup(Pid) ->
    gen_server:stop(Pid).


main_test_() ->
    {inorder,
      {foreach,
      fun setup/0,
      fun cleanup/1,
      [
       fun formats_number_de/1,
       fun formats_number_en/1
      ]}}.


formats_number_de(_) ->
    fun() ->
            GermanNumber = number_formatter:format(de, 1000),
            ?assertEqual(GermanNumber, "1.000,00"),
            GermanNumber2 = number_formatter:format(de,1000000),
            ?assertEqual(GermanNumber2, "1.000.000,00"),
            GermanNumber3 = number_formatter:format(de, 1.2333),
            ?assertEqual(GermanNumber3, "1,23")
    end.

formats_number_en(_) ->
    fun() ->
            EnglishNumber = number_formatter:format(en, 1000),
            ?assertEqual(EnglishNumber, "1,000.00"),
            EnglishNumber2 = number_formatter:format(en, 1000000),
            ?assertEqual(EnglishNumber2, "1,000,000.00"),
            EnglishNumber3 = number_formatter:format(en, 1000000.231),
            ?assertEqual(EnglishNumber3, "1,000,000.23")
    end.
