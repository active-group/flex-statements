-module(date_formatter).
-behaviour(gen_server).

-export([format/2, init/1, handle_call/3, start_link/0, handle_cast/2,
         format_de/1, format_en/1,
         handle_info/2, terminate/2, code_change/3]).
-export_type([year/0, month_number/0, day_number/0, timestamp/0]).

-type year() :: integer().
-type month_number() :: pos_integer().
-type day_number() :: pos_integer().
-type timestamp() :: {year(), month_number(), day_number()}.

-spec month_en(month_number()) -> string().
month_en(N) ->
    case N of
        1 -> "January";
        2 -> "February";
        3 -> "March";
        4 -> "April";
        5 -> "May";
        6 -> "June";
        7 -> "July";
        8 -> "August";
        9 -> "September";
        10 -> "October";
        11 -> "November";
        12 -> "December"
    end.

-spec month_de(month_number()) -> string().
month_de(N) ->
    case N of
        1 -> "Januar";
        2 -> "Februar";
        3 -> "März";
        4 -> "April";
        5 -> "Mai";
        6 -> "Juni";
        7 -> "Juli";
        8 -> "August";
        9 -> "September";
        10 -> "Oktober";
        11 -> "November";
        12 -> "Dezember"
    end.

-spec day_en(day_number()) -> string().
day_en(D) ->
    case D of
        1 -> "1st";
        2 -> "2nd";
        _ -> integer_to_list(D) ++ "th"
    end.



-spec format_en(timestamp()) -> string().
format_en({Y, M, D}) ->
    month_en(M) ++ " " ++ day_en(D) ++ ", " ++ integer_to_list(Y).


-spec format_de(timestamp()) -> string().
format_de({Y, M, D}) ->
    integer_to_list(D) ++ ". " ++ month_de(M) ++ ", " ++ integer_to_list(Y).


start_link() ->
    gen_server:start_link({local, date_formatter}, ?MODULE, [], []).

init([]) -> {ok, #{}}.

handle_cast(Msg, State) ->
    lager:error("Received illegal cast: ~p~n", [Msg]),
    {noreply, State}.

handle_call({format, en, Date}, _, State) ->
    {reply, format_en(Date), State};
handle_call({format, de, Date}, _, State) ->
    {reply, format_de(Date), State};
handle_call(Msg, _, State) ->
    lager:error("Received illegal call: ~p~n", [Msg]),
    {reply, undefined, State}.

handle_info(Info, State) ->
    lager:error("Received illegal info: ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


-spec format(en | de, erlang:timestamp()) -> string().
format(Lang, Timestamp) ->
    {Date, _} = calendar:now_to_datetime(Timestamp),
    gen_server:call(date_formatter, {format, Lang, Date}).
