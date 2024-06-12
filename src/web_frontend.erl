-module(web_frontend).
-include("data.hrl").
-export([init/2]).

prologue() ->
    <<
        "\n"
        "<!DOCTYPE html>"
        "<html>"
        "<head>"
        "  <title>erlbank - more functional than perlbank!</title>"
        "  <link rel=\"stylesheet\" href=\"https://cdn.jsdelivr.net/npm/bulma@1.0.0/css/bulma.min.css\">"
        "</head>"
        "<body>"
        "  <nav class=\"navbar\" role=\"navigation\" aria-label=\"main navigation\">"
        "    <div class=\"navbar-brand\">"
        "      <a class\"navbar-item\" href=\"/\">"
        "        <svg width=\"103\" height=\"65\" viewBox=\"0 0 610 386\" xmlns=\"http://www.w3.org/2000/svg\">"
        "          <g id=\"Gruppe\">"
        "             <path id=\"Pfad\" fill=\"#a90533\" stroke=\"none\" d=\"M 95.978302 385.834717 C 48.701324 335.742004 21.041595 266.539917 21.112 183.970551 C 21.049164 110.952087 43.992493 48.186035 83.438141 0.423828 L 83.372772 0.448975 L 0.469025 0.448975 L 0.469025 385.839752 L 95.917938 385.839752 L 95.975769 385.832245 Z M 532.593079 385.867401 C 552.957031 364.05542 571.204102 338.369202 587.739075 308.994904 L 495.975769 263.113251 C 463.750488 315.506317 416.62439 363.726074 351.454834 364.050385 C 256.606812 363.728577 219.34082 282.624908 219.546997 178.235992 L 573.977051 178.235992 C 574.449707 166.450043 574.449707 160.969391 573.977051 155.295166 C 576.290039 93.190247 559.830444 40.985718 529.887939 0.333313 L 529.734619 0.448975 L 609.533508 0.448975 L 609.533508 385.844788 L 532.419617 385.844788 L 532.593079 385.869934 Z\"/>"
        "             <path id=\"path1\" fill=\"#a90533\" stroke=\"none\" d=\"M 225.284058 88.768005 C 229.206055 41.503601 266.471985 9.720825 310.163879 9.625305 C 354.152466 9.720825 385.937744 41.503601 387.013794 88.768005 Z\"/>"
        "         </g>"
        "         <path id=\"ERLBANK\" fill=\"#929292\" fill-rule=\"evenodd\" stroke=\"none\" d=\"M 568.602661 251 L 568.602661 180.089996 L 583.51239 180.089996 L 583.51239 209.066803 L 594.840393 180.089996 L 609.416931 180.089996 L 596.922791 212.130814 L 610 251 L 594.840393 251 L 585.094971 219.659531 L 583.51239 222.460907 L 583.51239 251 Z M 508.547424 251 L 508.547424 180.089996 L 519.042542 180.089996 L 532.869385 214.231857 L 532.869385 180.089996 L 545.19696 180.089996 L 545.19696 251 L 535.118286 251 L 521.291443 214.231857 L 521.291443 251 Z M 446.07663 251 L 458.820648 180.089996 L 475.812714 180.089996 L 488.390167 251 L 474.646606 251 L 472.397644 236.030106 L 462.485626 236.030106 L 460.153351 251 Z M 463.818329 226.662979 L 470.981628 226.662979 L 467.399994 198.999329 Z M 387.187469 251 L 387.187469 180.089996 L 404.762573 180.089996 C 407.650116 180.089996 410.384949 180.323456 412.967102 180.790344 C 415.549194 181.257248 417.839783 182.147247 419.838867 183.460403 C 421.837982 184.77356 423.406647 186.641144 424.545013 189.063171 C 425.68335 191.485214 426.252533 194.680527 426.252533 198.649155 C 426.252533 201.508911 425.849945 203.916336 425.044769 205.871475 C 424.239594 207.826614 423.101257 209.387787 421.62973 210.555038 C 420.158173 211.72229 418.395142 212.510162 416.340515 212.918701 C 418.950409 213.268875 421.143829 214.15889 422.920746 215.58876 C 424.697723 217.018661 426.044312 218.929993 426.960571 221.322861 C 427.876801 223.715714 428.3349 226.604614 428.3349 229.989624 C 428.3349 233.666458 427.876801 236.832581 426.960571 239.488068 C 426.044312 242.14357 424.725494 244.317535 423.004059 246.01004 C 421.282623 247.702545 419.158661 248.957321 416.632019 249.774399 C 414.105438 250.591476 411.204041 251 407.927765 251 Z M 402.097137 240.057098 L 405.26236 240.057098 C 408.538605 240.057098 410.801392 239.181671 412.050842 237.430801 C 413.300262 235.679916 413.924957 233.112015 413.924957 229.72699 C 413.924957 227.042328 413.647339 224.91214 413.092041 223.336349 C 412.536743 221.760559 411.620514 220.607925 410.343292 219.878387 C 409.066101 219.148865 407.344727 218.784088 405.179077 218.784088 L 402.097137 218.784088 Z M 402.097137 208.453995 L 405.012451 208.453995 C 407.289215 208.453995 409.01059 208.089249 410.176727 207.359711 C 411.342865 206.630188 412.134155 205.565079 412.550629 204.164383 C 412.967102 202.763687 413.175293 201.071198 413.175293 199.086884 C 413.175293 197.219284 412.856018 195.658112 412.217438 194.403305 C 411.578827 193.148529 410.634857 192.20015 409.385437 191.558151 C 408.136017 190.916183 406.567291 190.595184 404.679291 190.595184 L 402.097137 190.595184 Z M 336.960907 251 L 336.960907 180.089996 L 351.870636 180.089996 L 351.870636 240.407272 L 367.363403 240.407272 L 367.363403 251 Z M 273.657196 251 L 273.657196 180.089996 L 292.231842 180.089996 C 296.785309 180.089996 300.727875 180.61525 304.059662 181.665771 C 307.391418 182.716309 309.987427 184.598465 311.847656 187.312317 C 313.707916 190.026169 314.638031 193.863434 314.638031 198.824249 C 314.638031 201.742371 314.415955 204.339462 313.97168 206.615601 C 313.527466 208.891724 312.708405 210.861435 311.514496 212.524765 C 310.320618 214.18808 308.613068 215.574173 306.391876 216.68306 L 315.637604 251 L 300.228088 251 L 292.814911 219.221817 L 288.566895 219.221817 L 288.566895 251 Z M 288.566895 210.204865 L 292.731598 210.204865 C 294.841736 210.204865 296.479858 209.810928 297.645996 209.023026 C 298.812134 208.235138 299.631165 207.097092 300.103149 205.608841 C 300.575165 204.120605 300.811188 202.325989 300.811188 200.22493 C 300.811188 197.190094 300.283661 194.826447 299.228577 193.133942 C 298.173523 191.441422 296.230011 190.595184 293.39798 190.595184 L 288.566895 190.595184 Z M 223.097473 251 L 223.097473 180.089996 L 253.416672 180.089996 L 253.416672 190.770279 L 238.007187 190.770279 L 238.007187 208.366455 L 249.751694 208.366455 L 249.751694 219.221817 L 238.007187 219.221817 L 238.007187 240.407272 L 253.583252 240.407272 L 253.583252 251 Z\"/>"
        "     </svg>"
        "      </a>"
        "    </div>"
        "    <div class=\"navbar-start\">"
        "      <a class=\"navbar-item\" href=\"/\">Request Statement</a>"
        "    </div>"
        "  </nav>"
        "    "
    >>.

epilogue() ->
    <<
        "\n"
        "</body>"
        "</html>"
        "    "
    >>.

-spec bin_to_int(binary()) -> integer().
bin_to_int(B) ->
    erlang:list_to_integer(binary:bin_to_list(B)).

statement_form() ->
    <<
        "<nav class=\"panel is-link\" style=\"margin: 1rem; margin-bottom: 20rem;\">"
        "  <p class=\"panel-heading\" id=\"request-statement\">Request statement</p>\n"
        "  <div class\"panel-block\" style=\"margin: 0.75rem 1rem;\">"
        "    <form id=\"statement_form\" method=\"post\" action=\"/statements/request\">\n"
        "      <div class=\"field\">"
        "        <label class=\"label\" for=\"statement_accountnumber\"> Account number </label>\n"
        "        <div class=\"control\">"
        "          <input class=\"input\" type=\"text\" id=\"statement_accountnumber\" name=\"statement_accountnumber\" placeholder=\"Account Number\" />\n"
        "        </div>"
        "      </div>"
        "      <div class=\"field\">"
        "        <label class=\"label\" for=\"statement_currency\"> Currency </label>\n"
        "        <div class=\"control\">"
        "          <input class=\"input\" type=\"text\" id=\"statement_currency\" name=\"statement_currency\" value=\"EUR\" size=3 />\n"
        "        </div>"
        "      </div>"
        "      <div class=\"field\">"
        "        <label class=\"label\" for=\"statement_format\"> Number & date formatting </label>\n"
        "        <div class=\"control\">"
        "          <div class=\"select\">"
        "            <select id=\"statement_format\" name=\"statement_format\">\n"
        "              <option value=\"de\" selected>DE</option>\n"
        "              <option value=\"en\">EN</option>\n"
        "            </select>\n"
        "          </div>"
        "        </div>"
        "      </div>"
        "    </form>"
        "  </div>"
        "  <div class=\"panel-block\">"
        "    <div class=\"field is-grouped\">"
        "      <div class=\"control\">"
        "        <button form=\"statement_form\" class=\"button is-link\" type=\"submit\">Request statement</button>\n"
        "      </div>"
        "      <div class=\"control\">"
        "        <button form=\"statement_form\" class=\"button is-danger\" type=\"reset\">Reset</button>\n"
        "      </div>"
        "    </div>"
        "  </div>"
        "</nav>"
    >>.

-spec transfer_template() -> string().
transfer_template() ->
    "<tr>\n"
    "      <td> ~p </td>\n"
    "      <td> ~s </td>\n"
    "      <td> ~s </td>\n"
    "      <td> ~s </td>\n"
    "      <td> ~s </td>\n"
    "    </tr>".

-spec amount_to_string(money(), string(), number_formatter:locale()) -> string().
amount_to_string(Amount, Currency, Format) ->
    {ok, AmountExchanged} = exchange_service:exchange(Currency, Amount),
    AmountFormatted = number_formatter:format(Format, AmountExchanged),
    AmountFormatted ++ " " ++ Currency.

%% returns the name of the person associated to the account nr
%% given by account number.
-spec name_by_account_number(unique_id()) -> string().
name_by_account_number(AccountNumber) ->
    {ok, Account} = business_logic:get_account(AccountNumber),
    name_by_account(Account).

%% returns the name of the person associated to the account
%% given by account.
-spec name_by_account(#account{}) -> string().
name_by_account(Account) ->
    {ok, Person} = business_logic:get_person(Account#account.person_id),
    io_lib:format("~s ~s", [Person#person.given_name, Person#person.surname]).

-spec transfer(#transfer{}, string(), number_formatter:locale()) -> string().
transfer(Transfer, Currency, Format) ->
    Name1 = name_by_account_number(Transfer#transfer.from_account_number),
    Name2 = name_by_account_number(Transfer#transfer.to_account_number),
    Amount = amount_to_string(Transfer#transfer.amount, Currency, Format),
    Date = date_formatter:format(Format, Transfer#transfer.timestamp),
    Id = Transfer#transfer.id,
    io_lib:format(transfer_template(), [Id, Date, Amount, Name1, Name2]).

head_template() ->
    "<p> Name: ~s </p>\n"
    "     <p> Balance: ~s </p>\n"
    "     <table class=\"table\">\n"
    "       <tbody>"
    "         <tr>\n"
    "           <th>ID</th>\n"
    "           <th>Date</th>\n"
    "           <th>Amount</th>\n"
    "           <th>Sender</th>\n"
    "           <th>Receiver</th>\n"
    "         </tr> "
    "       </tbody>".

back_button() ->
    "<a href=\"/\">Back </a>".

footer_template() ->
    "</table>" ++ back_button().

-spec head(#account{}, string(), number_formatter:locale()) -> string().
head(Account, Currency, Format) ->
    Amount = amount_to_string(Account#account.amount, Currency, Format),
    Name = name_by_account(Account),
    io_lib:format(head_template(), [Name, Amount]).

-spec statement(#account{}, list(#transfer{}), string(), number_formatter:locale()) -> string().
statement(Account, Transfers, Currency, Format) ->
    % TODO append all transfers
    TransfersString = lists:foldl(
        fun(Transfer, Acc) -> Acc ++ transfer(Transfer, Currency, Format) end, "", Transfers
    ),
    io_lib:format("~s ~s ~s ~s", [
        prologue(),
        head(Account, Currency, Format),
        TransfersString,
        footer_template()
    ]).

index() ->
    io_lib:format(
        "~s~s~s",
        [
            prologue(),
            statement_form(),
            epilogue()
        ]
    ).

%% /statements/request
init(Request, request_statement) ->
    {ok, KeyValuesL, _} = cowboy_req:read_urlencoded_body(Request),

    KeyValues = maps:from_list(KeyValuesL),
    AccountNumber = bin_to_int(maps:get(<<"statement_accountnumber">>, KeyValues)),
    Currency = binary_to_list(maps:get(<<"statement_currency">>, KeyValues)),
    Format = list_to_atom(
        binary_to_list(maps:get(<<"statement_format">>, KeyValues))
    ),

    logger:info("Bank statement request: account number ~p~n", [AccountNumber]),
    Ret = business_logic:get_account(AccountNumber),
    KnowsCurrency = exchange_service:knows_currency(Currency),
    case {KnowsCurrency, Ret} of
        {true, {ok, Account}} ->
            Transfers = business_logic:get_transfers(AccountNumber),
            Body = statement(Account, Transfers, Currency, Format),
            Reply = cowboy_req:reply(
                200,
                #{<<"content-type">> => <<"text/html">>},
                Body,
                Request
            ),
            {ok, Reply, []};
        {false, _} ->
            Reply = cowboy_req:reply(
                200,
                #{<<"content-type">> => <<"text/html">>},
                "Currency not found.<br/>" ++ back_button(),
                Request
            ),
            {ok, Reply, []};
        {_, {error, not_found}} ->
            Reply = cowboy_req:reply(
                200,
                #{<<"content-type">> => <<"text/html">>},
                "Account not found.<br/>" ++ back_button(),
                Request
            ),
            {ok, Reply, []}
    end;
%% /index
init(Request, index) ->
    Reply = cowboy_req:reply(
        200,
        #{<<"content-type">> => <<"text/html">>},
        index(),
        Request
    ),
    {ok, Reply, []}.
