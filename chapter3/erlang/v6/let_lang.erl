-module(let_lang).

-include_lib("eunit/include/eunit.hrl").

-export([eval_script/1]).

-export_type([expval/0]).

-type expval() :: {num_val, integer()}
                | {bool_val, boolean()}
                | {proc_val, let_lang_trans:nameless_abt(), nameless_env:nameless_env()}.


eval_abt({number_exp, N}, _Nameless_env) -> {num_val, N};
eval_abt({nameless_var_exp, Pos}, Nameless_env) ->
    nameless_env:apply_nameless_env(Nameless_env, Pos);
eval_abt({check_zero_exp, Abt}, Nameless_env) ->
    {num_val, N} = eval_abt(Abt, Nameless_env),
    {bool_val, N =:= 0};
eval_abt({if_exp, Q, A, E}, Nameless_env) ->
    {bool_val, B} = eval_abt(Q, Nameless_env),
    case B of
        true -> eval_abt(A, Nameless_env);
        false -> eval_abt(E, Nameless_env)
    end;
eval_abt({diff_exp, E1, E2}, Nameless_env) ->
    {num_val, N1} = eval_abt(E1, Nameless_env),
    {num_val, N2} = eval_abt(E2, Nameless_env),
    {num_val, N1-N2};
eval_abt({nameless_let_exp, Exps, Body}, Nameless_env) ->
    Vals = [eval_abt(E, Nameless_env) || E <- Exps],
    eval_abt(Body, nameless_env:extend_nameless_env(Nameless_env, Vals));
eval_abt({nameless_proc_exp, Body}, Nameless_env) ->
    {proc_val, Body, Nameless_env};
eval_abt({apply_exp, Operator, Operands}, Nameless_env) ->
    {proc_val, Body, Lex_env} = eval_abt(Operator, Nameless_env),
    Args = [eval_abt(Op, Nameless_env) || Op <- Operands],
    eval_abt(Body, nameless_env:extend_nameless_env(Lex_env, Args));
eval_abt({nameless_letrec_exp, Exps, Body}, Nameless_env) ->
    Procs = [eval_abt(E, Nameless_env) || E <- Exps ],
    eval_abt(Body, nameless_env:extend_nameless_env_rec(Nameless_env, Procs)).


eval_script(Fn) ->
    Abt = let_lang_parse:scan_and_parse_file(Fn),
    Nameless_abt = let_lang_trans:trans(Abt, senv:empty_senv()),
    eval_abt(Nameless_abt, nameless_env:empty_nameless_env()).

eval_test() ->
    [
     eval_script("code/code0") =:= {num_val, -100},
     eval_script("code/code1") =:= {num_val, 12},
     eval_script("code/code2") =:= {num_val, 12},
     eval_script("code/code3") =:= {num_val, 1},
     eval_script("code/code4") =:= {num_val, 4}
    ].
