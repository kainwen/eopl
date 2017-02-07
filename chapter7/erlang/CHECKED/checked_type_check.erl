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
    type_of_letrec(Exp, Tenv);
type_of(Exp={tuple_exp, _}, Tenv) ->
    type_of_tuple(Exp, Tenv);
type_of(Exp={match_tuple_exp, _, _, _}, Tenv) ->
    type_of_match_tuple(Exp, Tenv);
type_of(Exp={list_exp, _}, Tenv) ->
    type_of_list(Exp, Tenv);
type_of(Exp={cons_exp, _, _}, Tenv) ->
    type_of_cons(Exp, Tenv);
type_of(Exp={car_exp, _}, Tenv) ->
    type_of_car(Exp, Tenv);
type_of(Exp={cdr_exp, _}, Tenv) ->
    type_of_cdr(Exp, Tenv);
type_of(Exp={test_null_exp, _}, Tenv) ->
    type_of_test_null(Exp, Tenv).

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

type_of_apply(Exp={apply_exp, Operator, Operands}, Tenv) ->
    T_operator = type_of(Operator, Tenv),
    T_operands = checked_type:reduce_type({star, [type_of(Op, Tenv) || Op <- Operands]}),
    case T_operator of
        {arrow, T1, T2} ->
            assert_arg_num(checked_type:card(T1), length(Operands), Exp),
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

type_of_tuple({tuple_exp, Exps}, Tenv) ->
    Tps = [type_of(E, Tenv) || E <- Exps],
    checked_type:reduce_type({star, Tps}).

type_of_match_tuple(E={match_tuple_exp, Vars, Exp, Body}, Tenv) ->
    Tp_exp = type_of(Exp, Tenv),
    Card_of_tp_exp = checked_type:card(Tp_exp),
    case length(Vars) =:= Card_of_tp_exp of
        false ->
            io:format("match_tuple var number dose not match TUPLE: ~p", [E]),
            erlang:error(type_error);
        true ->
            case Tp_exp of
                {star, Tps} ->
                    New_tenv = tenv:extend_tenv(Tenv, lists:zip(Vars, Tps)),
                    type_of(Body, New_tenv);
                _ ->
                    [V] = Vars,
                    New_tenv = tenv:extend_tenv(Tenv, [{V, Tp_exp}]),
                    type_of(Body, New_tenv)
            end
    end.

type_of_list(Exp={list_exp, Exps}, Tenv) ->
    Tps = [type_of(E, Tenv) || E <- Exps],
    case Tps of
        [] -> {empty_list};
        _ ->
            [Tp|_] = Tps,
            [assert_type(Tp, T, Exp)|| T <- Tps],
            {list, Tp}
    end.

type_of_cons(Exp={cons_exp, E1, E2}, Tenv) ->
    Tp1 = type_of(E1, Tenv),
    Tp2 = type_of(E2, Tenv),
    case Tp2 of
        {empty_list} ->
            {list, Tp1};
        {list, Tplist} ->
            assert_type(Tp1, Tplist, Exp),
            Tp2;
        _ ->
            io:format("cons to a value which is not a list: ~p~n", [Exp]),
            erlang:error(type_error)
    end.

type_of_car(Exp={car_exp, E}, Tenv) ->
    Tp = type_of(E, Tenv),
    case Tp of
        {empty_list} ->
            io:format("car an empty list: ~p~n", [Exp]),
            erlang:error(type_error);
        {list, Tplist} -> Tplist;
        _ ->
            io:format("car a value which is not a list: ~p~n", [Exp])
    end.

type_of_cdr(Exp={cdr_exp, E}, Tenv) ->
    Tp = type_of(E, Tenv),
    case Tp of
        {empty_list} ->
            io:format("cdr an empty list: ~p~n", [Exp]),
            erlang:error(type_error);
        {list, Tplist} ->
            {list, Tplist};
        _ ->
            io:format("cdr a value which is not a list: ~p~n", [Exp])
    end.

type_of_test_null(Exp={test_null_exp, E}, Tenv) ->
    Tp = type_of(E, Tenv),
    case Tp of
        {empty_list} ->
            {bool};
        {list, _Tplist} ->
            {bool};
        _ ->
            io:format("null? to a value which is not a list: ~p~n", [Exp]),
            erlang:error(type_error)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
assert_type({list, _}, {empty_list}, _) -> type_check;
assert_type({empty_list}, {list, _}, _) -> type_check;
assert_type(T1, T2, Exp) ->
    case T1 =:= T2 of
        true -> type_check;
        false ->
            io:format("Type didn't match: ~p != ~p in EXP: ~p~n",
                      [checked_type:print_type(T1),
                       checked_type:print_type(T2),
                       Exp]),
            erlang:error(type_error)
    end.

assert_arg_num(Pn, An, Exp) ->
    case Pn =:= An of
        true -> arg_num_match;
        false ->
            io:format("Arguments number didn't match when calling function: ~p~n",
                      [Exp]),
            io:format("Want ~p, Get ~p~n", [Pn, An]),
            erlang:error(type_error)
    end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
typer(Fn) ->
    Exp = checked_parse:scan_and_parse_file(Fn),
    Tp = type_of(Exp, tenv:empty_tenv()),
    checked_type:print_type(Tp).
