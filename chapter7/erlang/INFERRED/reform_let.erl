-module(reform_let).

-export([reform/1]).

-type exp() :: inferred_parse:exp().

-spec reform(exp()) -> exp().
reform(Exp) ->
    reform(Exp, []).

reform({number, N}, _) -> {number, N};
reform({var, V}, Env) ->
    case proplists:get_value(V, Env) of
        '*' -> {var, V};
        E -> E
    end;
reform({diff_exp, E1, E2}, Env) ->
    {diff_exp, reform(E1, Env), reform(E2, Env)};
reform({if_exp, A, B, C}, Env) ->
    {if_exp,
     reform(A, Env),
     reform(B, Env),
     reform(C, Env)};
reform({test_zero_exp, E}, Env) ->
    {test_zero_exp, reform(E, Env)};
reform({let_exp, Vars, Exps, Body}, Env) ->
    New_exps = [reform(Exp, Env) || Exp <- Exps],
    New_env = lists:zip(Vars, New_exps) ++ Env,
    {let_exp, Vars, New_exps, reform(Body, New_env)};
reform({proc_exp, Paras_with_type, Body}, Env) ->
    New_env = [{V, '*'} || {V, _} <- Paras_with_type] ++ Env,
    {proc_exp, Paras_with_type, reform(Body, New_env)};
reform({apply_exp, Operator, Operands}, Env) ->
    Caller = reform(Operator, Env),
    Callees = [reform(O, Env) || O <- Operands],
    {apply_exp, Caller, Callees};
reform({letrec_exp, Fns_with_rn, Exps, Body}, Env) ->
    New_env = [{Fn, '*'}|| {Fn, _} <- Fns_with_rn] ++ Env,
    New_exps = [reform(Exp, New_env) || Exp <- Exps],
    {letrec_exp, Fns_with_rn, New_exps, reform(Body, New_env)};
reform({tuple_exp, Exps}, Env) ->
    {tuple_exp, [reform(Exp, Env) || Exp <- Exps]};
reform({match_tuple_exp, Vars, Exp, Body}, Env) ->
    New_exp = reform(Exp, Env),
    New_env = [{V, '*'} || V <- Vars] ++ Env,
    {match_tuple_exp, Vars, New_exp, reform(Body, New_env)};
reform({list_exp, Exps}, Env) ->
    {list_exp, [reform(Exp, Env) || Exp <- Exps]};
reform({car_exp, E}, Env) ->
    {car_exp, reform(E, Env)};
reform({cdr_exp, E}, Env) ->
    {cdr_exp, reform(E, Env)};
reform({cons_exp, E1, E2}, Env) ->
    {cons_exp, reform(E1, Env), reform(E2, Env)};
reform({test_null_exp, E}, Env) ->
    {test_null_exp, reform(E, Env)}.
