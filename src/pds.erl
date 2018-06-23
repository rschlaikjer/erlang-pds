-module(pds).
-include_lib("pds/include/records.hrl").
-export([
    parse/1
]).

parse(B) when is_binary(B) ->
    parse(binary_to_list(B));
parse(L) when is_list(L) ->
    {ok, Tokens, _} = pds_leex:string(L),
    {Props, Pointers, Objects} = tokens_to_proplist(Tokens),
    Doc = #pds_document{
        properties=Props,
        pointers=Pointers,
        objects=Objects
    },
    {ok, Doc}.

tokens_to_proplist(T) ->
    tokens_to_proplist(T, [], [], []).
tokens_to_proplist([{label, "END"}], TermAcc, PointerAcc, ObjectAcc) ->
    {TermAcc, PointerAcc, ObjectAcc};
tokens_to_proplist(Tokens, TermAcc, PointerAcc, ObjectAcc) ->
    case Tokens of
        [{comment, _}|Rest] ->
            tokens_to_proplist(Rest, TermAcc, PointerAcc, ObjectAcc);
        [{pointer, L}|[assignment|[{_Type, V}|Rest]]] ->
            {Rest1, Term} = case Rest of
                [{unit, Unit}|R] ->
                    {R, {L, {V, Unit}}};
                _ -> {Rest, {L, V}}
            end,
            tokens_to_proplist(Rest1, TermAcc, [Term|PointerAcc], ObjectAcc);
        [{label, "OBJECT"}|[assignment|[{label, ObjectName}|Rest]]] ->
            {Rest1, ObjProps} = consume_object(ObjectName, Rest),
            Object = {ObjectName, ObjProps},
            tokens_to_proplist(Rest1, TermAcc, PointerAcc, [Object|ObjectAcc]);
        [{label, L}|[assignment|[{_Type, V}|Rest]]] ->
            {Rest1, Term} = case Rest of
                [{unit, Unit}|R] ->
                    {R, {L, {V, Unit}}};
                _ -> {Rest, {L, V}}
            end,
            tokens_to_proplist(Rest1, [Term|TermAcc], PointerAcc, ObjectAcc);
        [{label, L}|[assignment|[begin_tuple|Rest]]] ->
            {Rest1, T} = consume_tuple(Rest),
            tokens_to_proplist(Rest1, [{L, T}|TermAcc], PointerAcc, ObjectAcc)
    end.

consume_tuple(Tokens) -> consume_tuple(Tokens, []).
consume_tuple([end_tuple|Rest], Acc) -> {Rest, list_to_tuple(Acc)};
consume_tuple([tuple_separator|Rest], Acc) -> consume_tuple(Rest, Acc);
consume_tuple([{_Type, Value}|Rest], Acc) -> consume_tuple(Rest, [Value|Acc]).

consume_object(ObjectName, Tokens) ->
    consume_object(ObjectName, Tokens, []).
consume_object(ObjectName, Tokens, Acc) ->
    case Tokens of
        [{label, "END_OBJECT"}|[assignment|[{label, ObjectName}|Rest]]] ->
            {Rest, Acc};
        [{label, L}|[assignment|[{_Type, V}|Rest]]] ->
            {Rest1, Term} = case Rest of
                [{unit, Unit}|R] ->
                    {R, {L, {V, Unit}}};
                _ -> {Rest, {L, V}}
            end,
            consume_object(ObjectName, Rest1, [Term|Acc]);
        [{label, L}|[assignment|[begin_tuple|Rest]]] ->
            {Rest1, TupleElements} = consume_tuple(Rest),
            consume_object(Rest1, [{L, TupleElements}|Acc])
    end.
