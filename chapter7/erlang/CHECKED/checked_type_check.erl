-module(checked_type_check).

-export([typer/1]).

-type exp() :: checked_parse:exp().
-type tp() :: checked_type:tp().
-type tenv() :: tenv:tenv().

-spec type_of(exp(), tenv()) -> tp().
type_of(Exp={number, _}, Tenv) ->
    type_of_number(Exp, Tenv);
type_of(Exp={var, _}, Tenv) ->
    type_of_var(Exp, Tenv);
type_of(Exp={diff_exp, _, _}, Tenv) ->
    type_of_diff(Exp, Tenv);
type_of(Exp={if_exp, _, _, _}, Tenv) ->
    type_of_if(Exp, Tenv);
type_of(Exp={test_zero_exp, _}, Tenv) ->
    type_of_test_zero(Exp, Tenv);
type_of(Exp={let_exp, _, _, _}, Tenv) ->
    type_of_let(Exp, Tenv);
type_of(Exp={proc_exp, _, _}, Tenv) ->
    type_of_proc(Exp, Tenv);
type_of(Exp={apply_exp, _, _}, Tenv) ->
    type_of_apply(Exp, Tenv);
type_of(Exp={letrec_exp, _, _, _}, Tenv) ->
    type_of_letrec(Exp, Tenv).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
type_of_number({number, _N}, _Tenv) ->
    {int}.

type_of_var({var, V}, Tenv) ->
    tenv:apply_tenv(Tenv, V).

type_of_diff({diff_exp, E1, E2}, Tenv) ->
    T1 = type_of(E1, Tenv),
    assert_type(T1, {int}, E1),
    T2 = type_of(E2, Tenv),
    assert_type(T2, {int}, E2),
    {int}.

type_of_if(Exp={if_exp, A, B, C}, Tenv) ->
    Ta = type_of(A, Tenv),
    assert_type(Ta, {bool}, A),
    Tb = type_of(B, Tenv),
    Tc = type_of(C, Tenv),
    assert_type(Tb, Tc, Exp),
    Tb.

type_of_test_zero({test_zero_exp, Exp}, Tenv) ->
    T = type_of(Exp, Tenv),
    assert_type(T, {int}, Exp),
    {bool}.

type_of_let({let_exp, Vars, Exps, Body}, Tenv) ->
    Tps = [type_of(Exp, Tenv) || Exp <- Exps],
    Var_tp_pairs = lists:zip(Vars, Tps),
    New_tenv = tenv:extend_tenv(Tenv, Var_tp_pairs),
    type_of(Body, New_tenv).

type_of_proc({proc_exp, Paras_with_type, Body}, Tenv) ->
    New_tenv = tenv:extend_tenv(Tenv, Paras_with_type),
    Body_type = type_of(Body, New_tenv),
    {arrow,
     checked_type:reduce_type({star, [Tp || {_, Tp} <- Paras_with_type]}),
     Body_type}.

type_of_apply({apply_exp, Operator, Operands}, Tenv) ->
    T_operator = type_of(Operator, Tenv),
    T_operands = checked_type:reduce_type({star, [type_of(Op, Tenv) || Op <- Operands]}),
    case T_operator of
        {arrow, T1, T2} ->
            assert_type(T1, T_operands, Operands),
			T2;
        _ ->
            io:format("~p is not a procedure!~n", [Operator]),
            erlang:error(type_error)
    end.

type_of_letrec(Exp={letrec_exp, Fns_with_return_type, Proc_defs, Body}, Tenv) ->
    Proc_input_types = [checked_type:reduce_type({star,
                                                  [Tp || {_, Tp} <- Paras_with_type]})
                        || {proc_exp, Paras_with_type, _} <- Proc_defs],
    Proc_types = [{Fn, {arrow, It, Rt}}
                  || {{Fn, Rt}, It}
                         <- lists:zip(Fns_with_return_type, Proc_input_types)],
    Letrec_tenv = tenv:extend_tenv(Tenv, Proc_types),
    Infered_proc_types = [type_of(Proc_exp, Letrec_tenv) || Proc_exp <- Proc_defs],
    [assert_type(Tp, Ptp, Exp)
     || {{_, Tp}, Ptp} <- lists:zip(Proc_types, Infered_proc_types)],
    type_of(Body, Letrec_tenv).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
assert_type(T1, T2, Exp) ->
    case T1 =:= T2 of
        true -> type_check;
        false ->
            io:format("Type didn't match: ~p != ~p in EXP: ~p~n",
                      [T1, T2, Exp]),
            erlang:error(type_error)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
typer(Fn) ->
    Exp = checked_parse:scan_and_parse_file(Fn),
    type_of(Exp, tenv:empty_tenv()).
