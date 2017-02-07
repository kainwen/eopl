-module(inferred_type_inference).

-export([typer/1]).

type_of(Exp={number, _}, Tenv, Subst) ->
    type_of_number(Exp, Tenv, Subst);
type_of(Exp={var, _}, Tenv, Subst) ->
    type_of_var(Exp, Tenv, Subst);
type_of(Exp={diff_exp, _, _}, Tenv, Subst) ->
    type_of_diff(Exp, Tenv, Subst);
type_of(Exp={if_exp, _, _, _}, Tenv, Subst) ->
    type_of_if(Exp, Tenv, Subst);
type_of(Exp={test_zero_exp, _}, Tenv, Subst) ->
    type_of_test_zero(Exp, Tenv, Subst);
type_of(Exp={let_exp, _, _, _}, Tenv, Subst) ->
    type_of_let(Exp, Tenv, Subst);
type_of(Exp={proc_exp, _, _}, Tenv, Subst) ->
    type_of_proc(Exp, Tenv, Subst);
type_of(Exp={apply_exp, _, _}, Tenv, Subst) ->
    type_of_apply(Exp, Tenv, Subst);
type_of(Exp={letrec_exp, _, _, _}, Tenv, Subst) ->
    type_of_letrec(Exp, Tenv, Subst);
type_of(Exp={tuple_exp, _}, Tenv, Subst) ->
    type_of_tuple(Exp, Tenv, Subst);
type_of(Exp={match_tuple_exp, _, _, _}, Tenv, Subst) ->
    type_of_match(Exp, Tenv, Subst);
type_of(Exp={list_exp, _}, Tenv, Subst) ->
    type_of_list(Exp, Tenv, Subst);
type_of(Exp={cons_exp, _, _}, Tenv, Subst) ->
    type_of_cons(Exp, Tenv, Subst);
type_of(Exp={car_exp, _}, Tenv, Subst) ->
    type_of_car(Exp, Tenv, Subst);
type_of(Exp={cdr_exp, _}, Tenv, Subst) ->
    type_of_cdr(Exp, Tenv, Subst);
type_of(Exp={test_null_exp, _}, Tenv, Subst) ->
    type_of_test_null(Exp, Tenv, Subst).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
type_of_number({number, _}, _Tenv, Subst) ->
    {{int}, Subst}.

type_of_var({var, V}, Tenv, Subst) ->
    Tp = tenv:apply_tenv(Tenv, V),
    {Tp, Subst}.

type_of_diff({diff_exp, E1, E2}, Tenv, Subst) ->
    {Tp1, S1} = type_of(E1, Tenv, Subst),
    S2 = unifier:unifier(Tp1, {int}, S1, E1),
    {Tp2, S3} = type_of(E2, Tenv, S2),
    S4 = unifier:unifier(Tp2, {int}, S3, E2),
    {{int}, S4}.

type_of_if(Exp={if_exp, A, B, C}, Tenv, Subst) ->
    {TpA, S1} = type_of(A, Tenv, Subst),
    S2 = unifier:unifier(TpA, {bool}, S1, A),
    {TpB, S3} = type_of(B, Tenv, S2),
    {TpC, S4} = type_of(C, Tenv, S3),
    S5 = unifier:unifier(TpB, TpC, S4, Exp),
    {TpC, S5}.

type_of_test_zero({test_zero_exp, E}, Tenv, Subst) ->
    {Tp, S1} = type_of(E, Tenv, Subst),
    S2 = unifier:unifier(Tp, {int}, S1, E),
    {{bool}, S2}.

type_of_let({let_exp, Vars, Exps, Body}, Tenv, Subst) ->
    {Tps, S} = type_of_list_exps(Exps, Tenv, Subst, []),
    New_tenv = tenv:extend_tenv(Tenv, lists:zip(Vars, Tps)),
    type_of(Body, New_tenv, S).

type_of_proc({proc_exp, Paras, Body}, Tenv, Subst) ->
    Paras_with_type = lists:map(fun reform_optional_type/1, Paras),
    Tps = [T || {_, T} <- Paras_with_type],
    Body_tenv = tenv:extend_tenv(Tenv, Paras_with_type),
    {Tp_body, S} = type_of(Body, Body_tenv, Subst),
    {{arrow, inferred_type:reduce_type({star, Tps}), Tp_body}, S}.

type_of_apply(Exp={apply_exp, Operator, Operands}, Tenv, Subst) ->
    {Tp_operator, S1} = type_of(Operator, Tenv, Subst),
    {Tp_oprands, S2} = type_of_list_exps(Operands, Tenv, S1, []),
    Return_type = type_var_server:new_type_var(),
    S3 = unifier:unifier({arrow,
                          inferred_type:reduce_type({star, Tp_oprands}),
                          Return_type},
                         Tp_operator,
                         S2,
                         Exp),
    {Return_type, S3}.

type_of_letrec(Exp={letrec_exp, Fns, Procs, Body}, Tenv, Subst) ->
    Fns_with_type = lists:map(fun reform_optional_type/1, Fns),
    Input_types = lists:map(fun get_proc_input_type/1, Procs),
    Proc_types = [{Fn, {arrow, It, Rt}}
                  || {{Fn, Rt}, It} <- lists:zip(Fns_with_type, Input_types)],
    New_tenv = tenv:extend_tenv(Tenv, Proc_types),
    {Inferred_proc_types, S} = type_of_list_exps(Procs, New_tenv, Subst, []),
    S1 = unifier:unifier(inferred_type:reduce_type({star,
                                                    [Pt|| {_, Pt} <- Proc_types]}),
                         inferred_type:reduce_type({star, Inferred_proc_types}),
                         S,
                         Exp),
    type_of(Body, New_tenv, S1).

type_of_tuple({tuple_exp, Exps}, Tenv, Subst) ->
    {Tps, S1} = type_of_list_exps(Exps, Tenv, Subst, []),
    {inferred_type:reduce_type({star, Tps}), S1}.

type_of_match(Exp={match_tuple_exp, Vars, Tuple_exp, Body}, Tenv, Subst) ->
    {Tp_tuple, S1} = type_of(Tuple_exp, Tenv, Subst),
    Type_vars = [ type_var_server:new_type_var() || _V <- Vars],
    Vars_type = inferred_type:reduce_type({star, Type_vars}),
    S2 = unifier:unifier(Vars_type, Tp_tuple, S1, Exp),
    New_tenv = tenv:extend_tenv(Tenv, lists:zip(Vars, Type_vars)),
    type_of(Body, New_tenv, S2).

type_of_list(Exp={list_exp, Exps}, Tenv, Subst) ->
    Tv = type_var_server:new_type_var(),
    {Tps, S} = type_of_list_exps(Exps, Tenv, Subst, []),
    S1 = unifier:unifier_list(Tps,
                              lists:duplicate(length(Tps), Tv),
                              S,
                              Exp),
    {{list, Tv}, S1}.

type_of_cons(Exp={cons_exp, E1, E2}, Tenv, Subst) ->
    {T1, S1} = type_of(E1, Tenv, Subst),
    {T2, S2} = type_of(E2, Tenv, S1),
    S3 = unifier:unifier({list, T1}, T2, S2, Exp),
    {{list, T1}, S3}.

type_of_car(Exp={car_exp, E}, Tenv, Subst) ->
    {T, S1} = type_of(E, Tenv, Subst),
    Tv = type_var_server:new_type_var(),
    S2 = unifier:unifier({list, Tv}, T, S1, Exp),
    {Tv, S2}.

type_of_cdr({cdr_exp, E}, Tenv, Subst) ->
    type_of(E, Tenv, Subst).

type_of_test_null(Exp={test_null_exp, E}, Tenv, Subst) ->
    {T, S1} = type_of(E, Tenv, Subst),
    Tv = type_var_server:new_type_var(),
    S2 = unifier:unifier({list, Tv}, T, S1, Exp),
    {{bool}, S2}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

type_of_list_exps([], _Tenv, Subst, Result) ->
    {lists:reverse(Result), Subst};
type_of_list_exps([Exp|Exps], Tenv, Subst, Result) ->
    {Tp, S} = type_of(Exp, Tenv, Subst),
    type_of_list_exps(Exps, Tenv, S, [Tp|Result]).

reform_optional_type({V, '?'}) ->
    {V, type_var_server:new_type_var()};
reform_optional_type({V, Tp}) -> {V, Tp}.

get_proc_input_type({proc_exp, Paras, _Body}) ->
    Paras_with_type = lists:map(fun reform_optional_type/1, Paras),
    inferred_type:reduce_type({star, [T || {_, T} <- Paras_with_type]}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
typer(Fn) ->
    Exp = inferred_parse:scan_and_parse_file(Fn),
    type_var_server:start(),
    {Tp, Subst} = type_of(Exp, tenv:empty_tenv(), subst:new()),
    Result_type = subst:apply_subst(Subst, Tp),
    print_type(Result_type).

print_type({int}) -> "int";
print_type({bool}) -> "bool";
print_type(Tv) when is_atom(Tv) -> atom_to_list(Tv);
print_type({arrow, T1, T2}) ->
    S1 = print_type(T1),
    S2 = print_type(T2),
    string:join(["(", S1, " -> ", S2, ")"], "");
print_type({star, Tps}) ->
    string:join([print_type(Tp) || Tp <- Tps], "*");
print_type({list, Tp}) ->
    S = print_type(Tp),
    string:join([S, "list"], " ").
