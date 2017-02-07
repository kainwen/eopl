-module(unifier).

-export([unifier/4, unifier_list/4]).

-type type_with_var() :: subst:type_with_var().

-type subst() :: subst:subst().

-type exp() :: inferred_parse:exp().

unifier(T1, T2, Subst, Exp) ->
    Tp1 = subst:apply_subst(Subst, T1),
    Tp2 = subst:apply_subst(Subst, T2),
    unifier_helper(Tp1, Tp2, Subst, Exp).

-spec unifier_helper(type_with_var(), type_with_var(), subst(), exp()) -> subst().
unifier_helper(T1, T2, Subst, _Exp) when T1 == T2 -> Subst;
unifier_helper(Tv1, T2, Subst, Exp) when is_atom(Tv1)->
    case subst:is_safe(Tv1, T2) of
        true ->
            subst:add_to_subst(Subst, Tv1, T2);
        false ->
            erlang:error({unifier_fail, Tv1, T2, Exp})
    end;
unifier_helper(T1, Tv2, Subst, Exp) when is_atom(Tv2)->
    case subst:is_safe(Tv2, T1) of
        true ->
            subst:add_to_subst(Subst, Tv2, T1);
        false ->
            erlang:error({unifier_fail, T1, Tv2, Exp})
    end;
unifier_helper({int}, {int}, Subst, _E) -> Subst;
unifier_helper({int}, T2, _Subst, Exp) ->
    erlang:error({unifier_fail, {int}, T2, Exp});
unifier_helper({bool}, {bool}, Subst, _E) -> Subst;
unifier_helper({bool}, T2, _Subst, Exp) ->
    erlang:error({unifier_fail, {bool}, T2, Exp});
unifier_helper({arrow, T11, T12}, {arrow, T21, T22}, Subst, Exp) ->
    S1 = unifier(T11, T21, Subst, Exp),
    unifier(T12, T22, S1, Exp);
unifier_helper(T1={arrow, _, _}, T2, _S, Exp) ->
    erlang:error({unifier_fail, T1, T2, Exp});
unifier_helper({star, Tps1}, {star, Tps2}, Subst, Exp) ->
    unifier_list(Tps1, Tps2, Subst, Exp);
unifier_helper(T1={star, _}, T2, _Subst, Exp) ->
    erlang:error({unifier_fail, T1, T2, Exp});
unifier_helper({list, T1}, {list, T2}, Subst, Exp) ->
    unifier(T1, T2, Subst, Exp);
unifier_helper(T1={list, _}, T2, _Subst, Exp) ->
    erlang:error({unifier_fail, T1, T2, Exp}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
unifier_list([], [], Subst, _Exp) -> Subst;
unifier_list([Tp1|Tps1], [Tp2|Tps2], Subst, Exp) ->
    New_subst = unifier(Tp1, Tp2, Subst, Exp),
    unifier_list(Tps1, Tps2, New_subst, Exp).
