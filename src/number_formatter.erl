-module(number_formatter).
-behaviour(gen_server).

-export([format/2, init/1, handle_call/3, start_link/0, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export_type([locale/0]).

-type locale() :: en | de.

part(List) ->
    part(List, []).
part([], Acc) ->
    lists:reverse(Acc);
part([H], Acc) ->
    lists:reverse([[H]|Acc]);
part([H1,H2,H3|T], Acc) ->
    part(T, [[H1,H2, H3]|Acc]);
part([H1,H2|T], Acc) ->
    part(T, [[H1,H2]|Acc]).

r(Number) ->
    round(Number * 100) / 100.

sep(Str, ThousandSep, DecimalSep) ->
    [B, F] = string:split(Str, "."),
    Formatted = B ++ DecimalSep ++ string:join(part(F), ThousandSep),
    lists:reverse(Formatted).

format_separated(N, ThousandSep, DecimalSep) ->
    Rounded = r(N),
    RoundedStr = io_lib:format("~.2f", [Rounded]),
    Rev = lists:reverse(RoundedStr),
    sep(Rev, ThousandSep, DecimalSep).

%% @doc Prints a number, localized, rounded to 2 decimal digits, with 1000s separators.
-spec format_de(number()) -> string().
format_de(Number) -> format_separated(Number, ".", ",").

-spec format_en(number()) -> string().
format_en(Number) -> format_separated(Number, ",", ".").

start_link() ->
    gen_server:start_link({local, number_formatter}, ?MODULE, [], []).

init([]) -> {ok, #{}}.

handle_cast(Msg, State) ->
    lager:error("Received illegal cast: ~p~n", [Msg]),
    {noreply, State}.

handle_call({format, en, Number}, _, State) ->
    {reply, format_en(Number), State};
handle_call({format, de, Number}, _, State) ->
    {reply, format_de(Number), State};
handle_call(Msg, _, State) ->
    lager:error("Received illegal call: ~p~n", [Msg]),
    {reply, undefined, State}.

handle_info(Info, State) ->
    lager:error("Received illegal info: ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


format(en, Number) ->
    gen_server:call(number_formatter, {format, en, Number});
format(de, Number) ->
    gen_server:call(number_formatter, {format, de, Number}).
