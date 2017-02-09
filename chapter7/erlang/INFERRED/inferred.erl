-module(inferred).

-export([eval_script/1]).


eval(Exp={number, _}, Env) ->
    eval_number(Exp, Env);
eval(Exp={var, _}, Env) ->
    eval_var(Exp, Env);
eval(Exp={diff_exp, _, _}, Env) ->
    eval_diff(Exp, Env);
eval(Exp={if_exp, _, _, _}, Env) ->
    eval_if(Exp, Env);
eval(Exp={test_zero_exp, _}, Env) ->
    eval_test_zero(Exp, Env);
eval(Exp={let_exp, _, _, _}, Env) ->
    eval_let(Exp, Env);
eval(Exp={proc_exp, _, _}, Env) ->
    eval_proc(Exp, Env);
eval(Exp={apply_exp, _, _}, Env) ->
    eval_apply(Exp, Env);
eval(Exp={letrec_exp, _, _, _}, Env) ->
    eval_letrec(Exp, Env);
eval(Exp={tuple_exp, _}, Env) ->
    eval_tuple(Exp, Env);
eval(Exp={match_tuple_exp, _, _, _}, Env) ->
    eval_match_tuple(Exp, Env);
eval(Exp={list_exp, _}, Env) ->
    eval_list(Exp, Env);
eval(Exp={cons_exp, _, _}, Env) ->
    eval_cons(Exp, Env);
eval(Exp={car_exp, _}, Env) ->
    eval_car(Exp, Env);
eval(Exp={cdr_exp, _}, Env) ->
    eval_cdr(Exp, Env);
eval(Exp={test_null_exp, _}, Env) ->
    eval_test_null(Exp, Env).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
eval_number({number, N}, _Env) ->
    {num_val, N}.

eval_var({var, V}, Env) ->
    env:apply_env(Env, V).

eval_diff({diff_exp, E1, E2}, Env) ->
    {num_val, N1} = eval(E1, Env),
    {num_val, N2} = eval(E2, Env),
    {num_val, N1-N2}.

eval_if({if_exp, A, B, C}, Env) ->
    {bool_val, Bool} = eval(A, Env),
    case Bool of
        true -> eval(B, Env);
        false -> eval(C, Env)
    end.

eval_test_zero({test_zero_exp, E}, Env) ->
    {num_val, N} = eval(E, Env),
    {bool_val, N =:= 0}.

eval_let({let_exp, Vars, Exps, Body}, Env) ->
    Vals = [eval(Exp, Env) || Exp <- Exps],
    New_env = env:extend_env_by_list(Env, lists:zip(Vars, Vals)),
    eval(Body, New_env).

eval_proc({proc_exp, Paras_with_type, Body}, Env) ->
    Paras = [V || {V, _} <- Paras_with_type],
    {proc_val, Paras, Body, Env}.

eval_apply({apply_exp, Operator, Operands}, Env) ->
    {proc_val, Paras, Body, Lex_env} = eval(Operator, Env),
    Args = [eval(O, Env) || O <- Operands],
    New_env = env:extend_env_by_list(Lex_env, lists:zip(Paras, Args)),
    eval(Body, New_env).

eval_letrec({letrec_exp, Fns_with_type, Procs, Body}, Env) ->
    Fns = [N || {N, _} <- Fns_with_type],
    Proc_defs = [eval(P, Env) || P <- Procs],
    Proc_name_defs = [{Fn, {Paras, Proc_body}}
                      || {Fn, {proc_val, Paras, Proc_body, _Lex_env}}
                             <- lists:zip(Fns, Proc_defs)],
    New_env = env:extend_env_rec(Env, Proc_name_defs),
    eval(Body, New_env).

eval_tuple({tuple_exp, Exps}, Env) ->
    Vals = [eval(Exp, Env) || Exp <- Exps],
    {tuple_val, Vals}.

eval_match_tuple({match_tuple_exp, Vars, Texp, Body}, Env) ->
    {tuple_val, Vals} = eval(Texp, Env),
    New_env = env:extend_env_by_list(Env, lists:zip(Vars, Vals)),
    eval(Body, New_env).

eval_list({list_exp, Exps}, Env) ->
    Vals = [eval(Exp, Env) || Exp <- Exps],
    {list_val, Vals}.

eval_cons({cons_exp, E1, E2}, Env) ->
    V1 = eval(E1, Env),
    {list_val, V2} = eval(E2, Env),
    {list_val, [V1|V2]}.

eval_car({car_exp, E}, Env) ->
    {list_val, [V|_]} = eval(E, Env),
    V.

eval_cdr({cdr_exp, E}, Env) ->
    {list_val, [_|V]} = eval(E, Env),
    {list_val, V}.

eval_test_null({test_null_exp, E}, Env) ->
    {list_val, V} = eval(E, Env),
    {bool_val, length(V) =:= 0}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
eval_script(Fn) ->
    Exp = inferred_parse:scan_and_parse_file(Fn),
    eval(Exp, env:empty_env()).
