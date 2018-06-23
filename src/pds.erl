-module(pds).
-export([
    parse/1
]).

parse(B) when is_binary(B) ->
    parse(binary_to_list(B));
parse(L) when is_list(L) ->
    {ok, Tokens, _} = pds_leex:string(L),
    Properties = tokens_to_proplist(Tokens),
    {ok, Properties}.

tokens_to_proplist(T) -> tokens_to_proplist(T, []).
tokens_to_proplist([{label, "END"}], Acc) -> Acc;
tokens_to_proplist(Tokens, Acc) ->
    case Tokens of
        [{comment, _}|Rest] ->
            tokens_to_proplist(Rest, Acc);
        [{pointer, L}|[assignment|[{_Type, V}|Rest]]] ->
            {Rest1, Term} = case Rest of
                [{unit, Unit}|R] ->
                    {R, {L, {V, Unit}}};
                _ -> {Rest, {L, V}}
            end,
            tokens_to_proplist(Rest1, [Term|Acc]);
        [{label, L}|[assignment|[{_Type, V}|Rest]]] ->
            {Rest1, Term} = case Rest of
                [{unit, Unit}|R] ->
                    {R, {L, {V, Unit}}};
                _ -> {Rest, {L, V}}
            end,
            tokens_to_proplist(Rest1, [Term|Acc]);
        [{label, L}|[assignment|[begin_tuple|Rest]]] ->
            {Rest1, TupleElements} = consume_tuple(Rest),
            tokens_to_proplist(Rest1, [{L, TupleElements}|Acc])
    end.

consume_tuple(Tokens) -> consume_tuple(Tokens, []).
consume_tuple([end_tuple|Rest], Acc) -> {Rest, list_to_tuple(Acc)};
consume_tuple([tuple_separator|Rest], Acc) -> consume_tuple(Rest, Acc);
consume_tuple([{_Type, Value}|Rest], Acc) -> consume_tuple(Rest, [Value|Acc]).
