-module(date_formatter_test).
-include("data.hrl").
-include_lib("eunit/include/eunit.hrl").


setup() ->
    ok.
cleanup(_) ->
    ok.


main_test_() ->
    {inorder,
      {foreach,
      fun setup/0,
      fun cleanup/1,
      [
       fun formats_date_en/1,
       fun formats_date_de/1
      ]}}.



formats_date_de(_) ->
    fun() ->
            GermanDate = date_formatter:format_de({2019, 8, 23}),
            ?assertEqual(GermanDate, "23. August, 2019"),
            GermanDate2 = date_formatter:format_de({1999, 1, 1}),
            ?assertEqual(GermanDate2, "1. Januar, 1999")
    end.

formats_date_en(_) ->
    fun() ->
            EnglishDate = date_formatter:format_en({2019, 8, 23}),
            ?assertEqual(EnglishDate, "August 23th, 2019"),
            EnglishDate2 = date_formatter:format_en({1999, 1, 1}),
            ?assertEqual(EnglishDate2, "January 1st, 1999")
    end.
