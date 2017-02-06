-module(inferred_type).

-export([parse_type/1, reduce_type/1, card/1]).

-export_type([tp/0]).

-type tp() :: {int}
            | {bool}
            | {arrow, tp(), tp()}
            | {star, [tp()]}
            | {list, tp()}
            | {empty_list}.


parse_type(Toks) ->
    Collect_types = vector:new(),
    Op_stack = stack:new(),
    parse_type_helper(Toks, Collect_types, Op_stack).

-spec reduce_type(tp()) -> tp().
reduce_type({star, [Tp]}) -> Tp;
reduce_type(Tp) -> Tp.

-spec card(tp()) -> integer().
card({star, Tps}) -> length(Tps);
card(_) -> 1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parse_type_helper([], Collect_types, Op_stack) ->
    Poped = stack:pop_until(Op_stack, fun (_) -> false end),
    ok = vector:append_list(Collect_types, Poped),
    {build_type(vector:to_list(Collect_types)), []};
parse_type_helper(['int'|R], Collect_types, Op_stack) ->
    ok = vector:append(Collect_types, {int}),
    parse_type_helper(R, Collect_types, Op_stack);
parse_type_helper(['bool'|R], Collect_types, Op_stack) ->
    ok = vector:append(Collect_types, {bool}),
    parse_type_helper(R, Collect_types, Op_stack);
parse_type_helper(['('|R], Collect_types, Op_stack) ->
    Pos = vector:insert_pos(Collect_types),
    ok = stack:push(Op_stack, {'(', Pos}),
    parse_type_helper(R, Collect_types, Op_stack);
parse_type_helper(['['|R], Collect_types, Op_stack) ->
    Pos = vector:insert_pos(Collect_types),
    ok = stack:push(Op_stack, {'[', Pos}),
    parse_type_helper(R, Collect_types, Op_stack);
parse_type_helper(['*'|R], Collect_types, Op_stack) ->
    ok = stack:push(Op_stack, '*'),
    parse_type_helper(R, Collect_types, Op_stack);
parse_type_helper(['->'|R], Collect_types, Op_stack) ->
    case stack:is_empty(Op_stack) of
        true ->
            ok = stack:push(Op_stack, '->'),
            parse_type_helper(R, Collect_types, Op_stack);
        false ->
            Poped = stack:pop_until(Op_stack,
                                    fun (E) -> E /= '*' end),
            ok = vector:append_list(Collect_types, Poped),
            stack:push(Op_stack, '->'),
            parse_type_helper(R, Collect_types, Op_stack)
    end;
parse_type_helper([')'|R], Collect_types, Op_stack) ->
    %% take care that ')' may be illegal token!
    Poped = stack:pop_until(Op_stack,
                            fun (X) ->
                                    case X of
                                        {'(', Pos} when is_integer(Pos) -> true;
                                        _ -> false
                                    end
                            end),
    ok = vector:append_list(Collect_types, Poped),
    Top = stack:top(Op_stack),
    case Top of
        {'(', Pos} ->
            Tp = build_type(vector:remove_from(Collect_types, Pos)),
            ok = vector:append(Collect_types, Tp),
            Top = stack:pop(Op_stack),
            parse_type_helper(R, Collect_types, Op_stack);
        _ ->
            {build_type(vector:to_list(Collect_types)), [')'|R]}
    end;
parse_type_helper([']'|R], Collect_types, Op_stack) ->
    %% take care that ')' may be illegal token!
    Poped = stack:pop_until(Op_stack,
                            fun (X) ->
                                    case X of
                                        {'[', Pos} when is_integer(Pos) -> true;
                                        _ -> false
                                    end
                            end),
    ok = vector:append_list(Collect_types, Poped),
    Top = stack:top(Op_stack),
    case Top of
        {'[', Pos} ->
            Tp = {list, build_type(vector:remove_from(Collect_types, Pos))},
            ok = vector:append(Collect_types, Tp),
            Top = stack:pop(Op_stack),
            parse_type_helper(R, Collect_types, Op_stack);
        _ ->
            {build_type(vector:to_list(Collect_types)), [']'|R]}
    end;
parse_type_helper(R, Collect_types, Op_stack) ->
    Empty = vector:is_empty(Collect_types) and stack:is_empty(Op_stack),
    case Empty of
        false ->
            Poped = stack:pop_until(Op_stack, fun (_) -> false end),
            ok = vector:append_list(Collect_types, Poped),
            {build_type(vector:to_list(Collect_types)), R};
        true ->
            erlang:error(not_a_type)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
build_type(Tlist) ->
    Stack = stack:new(),
    build_type(Tlist, Stack).

build_type([], Stack) ->
    stack:pop(Stack);
build_type(['->'|Tlist], Stack) ->
    T1 = stack:pop(Stack),
    T2 = stack:pop(Stack),
    Type = {arrow, T2, T1},
    ok = stack:push(Stack, Type),
    build_type(Tlist, Stack);
build_type(['*'|Tlist], Stack) ->
    T1 = stack:pop(Stack),
    T2 = stack:pop(Stack),
    Type = type_product(T2, T1),
    ok = stack:push(Stack, Type),
    build_type(Tlist, Stack);
build_type([T|Tlist], Stack) ->
    ok = stack:push(Stack, T),
    build_type(Tlist, Stack).

type_product({star, Tp_list1}, {star, Tp_list2}) ->
    {star, Tp_list1 ++ Tp_list2};
type_product({star, Tp_list}, Tp) ->
    {star, Tp_list ++ [Tp]};
type_product(Tp, {star, Tp_list}) ->
    {star, [Tp|Tp_list]};
type_product(Tp1, Tp2) ->
    {star, [Tp1, Tp2]}.
