Definitions.

D = [0-9]
L = [a-zA-Z_]
LABEL = ({L}|{D}|:)
WHITESPACE = [\s\t\n\r]

Rules.

/\*([^*]|[\r\n]|(\*+([^*/]|[\r\n])))*\*+/ : {token, {comment, TokenChars}}.
L?\"(\\.|[^\\"])*\" : {token, {string, lists:sublist(TokenChars, 2, TokenLen-2)}}.
\^{L}+ : {token, {pointer, lists:sublist(TokenChars, 2, TokenLen-1)}}.
L?\<(\\.|[^\\>])*\> : {token, {unit, lists:sublist(TokenChars, 2, TokenLen-2)}}.
{WHITESPACE}+ : skip_token.
{LABEL}+        : {token, {label, string:strip(TokenChars)}}.
= : {token, assignment}.
{D}+-{D}+-{D}+ : {token, {date, parse_date(TokenChars)}}.
{D}+-{D}+-{D}+T{D}+:{D}+:{D}+.{D}+ : {token, {timestamp, parse_timestamp(TokenChars)}}.
-?{D}+ :   {token, {integer, list_to_integer(TokenChars)}}.
-?{D}*\.{D}*((E|e)(\+|\-)?{D}+)? : {token, {float, parse_float(TokenChars)}}.
\( : {token, begin_tuple}.
\) : {token, end_tuple}.
, : {token, tuple_separator}.

Erlang code.

parse_float(L) when is_list(L) -> parse_float(list_to_binary(L));
parse_float(<<"-.", B/binary>>) ->
    binary_to_float(<<"-0.", B/binary>>);
parse_float(<<".", B/binary>>) ->
    binary_to_float(<<"0.", B/binary>>);
parse_float(<<B/binary>>) ->
    % Some floats have trailing decimal point, no zero
    B1 = case binary:last(B) of
        $. -> <<B/binary, "0">>;
        _ -> B
    end,
    binary_to_float(B1).

parse_date(Date) when is_list(Date) -> parse_date(list_to_binary(Date));
parse_date(Date) when is_binary(Date) ->
    [Y, M, D] = binary:split(Date, <<"-">>, [global]),
    {binary_to_integer(Y), binary_to_integer(M), binary_to_integer(D)}.

parse_time(Time) when is_list(Time) -> parse_time(list_to_binary(Time));
parse_time(Time) when is_binary(Time) ->
    [H, M, S] = binary:split(Time, <<":">>, [global]),
    {binary_to_integer(H), binary_to_integer(M), binary_to_float(S)}.

parse_timestamp(Timestamp) when is_list(Timestamp) -> parse_timestamp(list_to_binary(Timestamp));
parse_timestamp(Timestamp) when is_binary(Timestamp) ->
    [Date, Time] = binary:split(Timestamp, <<"T">>, [global]),
    {parse_date(Date), parse_time(Time)}.
