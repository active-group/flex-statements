-module(exchange_service).
-behaviour(gen_server).
-include("data.hrl").

-export([exchange/2, knows_currency/1, init/1, handle_call/3, start_link/0, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


fetch() ->
    R = httpc:request("https://www.ecb.europa.eu/stats/eurofxref/eurofxref-daily.xml"),
    spawn(
      fun() ->
              case R of
                  {ok, {{_, 200, _}, _, Body}} ->
                      {XML, _} = xmerl_scan:string(Body, [{space, normalize}]),
                      XMLSimplified = xmerl_lib:simplify_element(XML),
                      gen_server:cast(exchange_service,
                                      {set_state, parse_xml_content(XMLSimplified)});
                  E ->
                      lager:error("Error in request to ecb: ~p~n", [E])
              end
      end).

-spec string_to_num(string()) -> number().
string_to_num(N) ->
    case string:to_float(N) of
        {error,no_float} -> list_to_integer(N);
        {F,_Rest} -> F
    end.

extract_currencies([]) -> [];
extract_currencies([X | XS]) ->
    case X of
        " " -> extract_currencies(XS);
        {'Cube', CR, _} -> [CR | extract_currencies(XS)]
    end.

parse_currencies(Raw) ->
    lists:map(fun([{_, Currency}, {_, Rate}]) ->
                      {Currency, string_to_num(Rate)}
              end, Raw).

parse_xml_content({_, _, Content}) ->
    %% dont do this at home :)
    [_,_,_,_,_,{_, _, [_ , {_, _, R} | _]} | _] = Content,
    Raw = extract_currencies(R),
    Parsed = parse_currencies(Raw),
    maps:from_list(Parsed).


start_link() ->
    gen_server:start_link({local, exchange_service}, ?MODULE, [], []).

init([]) ->
    fetch(),
    timer:send_interval(1000 * 60 * 5, fetch),
    {ok, #{}}.

handle_cast({set_state, NewState}, _) ->
    lager:info("Fetched exchange rates: ~p~n", [NewState]),
    {noreply, NewState};
handle_cast(Msg, State) ->
    lager:error("Received illegal cast: ~p~n", [Msg]),
    {noreply, State}.

handle_call({knows_currency, Symbol}, _, State) ->
    R = case maps:get(Symbol, State, undefined) of
            undefined -> false;
            _ -> true
        end,
    {reply, R, State};
handle_call({exchange, Symbol, Amount}, _, State) ->
    R = case maps:get(Symbol, State, undefined) of
            undefined -> {error, currency_not_found};
            Rate -> {ok, Rate * Amount}
        end,
    {reply, R, State};
handle_call(Msg, _, State) ->
    lager:error("Received illegal call: ~p~n", [Msg]),
    {reply, undefined, State}.


handle_info(fetch, State) ->
    fetch(),
    {noreply, State};
handle_info(Info, State) ->
    lager:error("Received illegal info: ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


-spec exchange(string(), money()) -> {ok, money()} | {error, currency_not_found}.
exchange(Symbol, Amount) ->
    case Symbol of
        "EUR" -> {ok, Amount};
        _ -> gen_server:call(exchange_service, {exchange, Symbol, Amount})
    end.

-spec knows_currency(string()) -> boolean().
knows_currency(Symbol) ->
    case Symbol of
        "EUR" -> true;
        _ -> gen_server:call(exchange_service, {knows_currency, Symbol})
    end.
