-module(web_frontend).
-export([init/2]).


success() ->
    << "
      <p> Account with account number ~p was opened successfully. </p> ~n
      <p> It could take several minutes until the account is ready for transfers. </p>
      <a href=\"/\"> Back </a>
    " >>.


form() ->
  << "
<h3> Request statement </h3>
<form method=\"post\" action=\"/statements/request\">
<table>
     <tr>
     <td>
       <label for=\"statement_accountnumber\"> Account number </label>
     </td>
     <td>
       <input type=\"text\" id=\"statement_accountnumber\" 
              name=\"statement_accountnumber\" />
     </td>
     </tr>

     <tr>
     <td>
       <label for=\"statement_currency\"> Currency </label>
     </td>
     <td>
       <input type=\"text\" id=\"statement_currency\" name=\"statement_currency\"
              value=\"EUR\" size=3/>
     </td>
     </tr>

     <tr>
     <td>
       <label for=\"statement_format\"> Number & date formatting </label>
     </td>
     <td>
       <select id=\"statement_format\" name=\"statement_format\">
         <option value=\"de\" selected>DE</option>
         <option value=\"en\">EN</option>
       </select>
     </td>
     </tr>
 </table>

<input type=\"submit\" value=\"Request statement\" />
</form>" >>.

welcome() ->
  << "<h1> Welcome to the Erlang Bank </h1>" >>.

index() ->
  io_lib:format("~s~s",
                [welcome(),
                 form()]).


back_button() ->
  "<a href=\"/\">Back </a>".

-spec bin_to_int(binary()) -> integer().
bin_to_int(B) ->
    erlang:list_to_integer(binary:bin_to_list(B)).

    
% -spec statement(#account{}, list(#transfer{}), string(), number_formatter:locale()) -> string().
statement(Account, Transfers, Currency, Format) ->
    % TODO append all transfers
    <<"">>.


%% /statements/request
init(Request, request_statement) ->

  {ok, KeyValuesL, _} = cowboy_req:read_urlencoded_body(Request),

  KeyValues = maps:from_list(KeyValuesL),
  AccountNumber = bin_to_int(maps:get(<<"statement_accountnumber">>, KeyValues)),
  Currency = binary_to_list(maps:get(<<"statement_currency">>, KeyValues)),
  Format = list_to_atom(
             binary_to_list(maps:get(<<"statement_format">>, KeyValues))),

  logger:info("Bank statement request: account number ~p~n", [AccountNumber]),
  Ret = business_logic:get_account(AccountNumber),
  KnowsCurrency = exchange_service:knows_currency(Currency),
  case {KnowsCurrency, Ret} of
      {true, {ok, Account}} ->
          Transfers = business_logic:get_transfers(AccountNumber),
          Body = statement(Account, Transfers, Currency, Format),
          Reply = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>},
                                  Body, Request),
          {ok, Reply, []};
      {false, _} ->
          Reply = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>},
                                  "Currency not found.<br/>" ++ back_button(), Request),
          {ok, Reply, []};
      {_, {error, not_found}} ->
          Reply = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>},
                                  "Account not found.<br/>" ++ back_button(), Request),
          {ok, Reply, []}
  end;


init(Req0, index) ->
    Req = cowboy_req:reply(200,
                           #{<<"content-type">> => <<"text/html">>},
                           index(),
                           Req0),
    {ok, Req, []}.
