-module(let_lang).

-compile(export_all).

-export_type([expval/0]).

-type expval() :: {num_val, integer()} | {bool_val, boolean()}.

-spec eval_abt(let_lang_parse:abstract_syntax_tree(), env:environment()) -> expval().
eval_abt({number_exp, N}, _Env) -> {num_val, N};
eval_abt({var_exp, Id}, Env) -> env:apply_env(Env, Id);
eval_abt({check_zero_exp, Exp}, Env) ->
    {num_val, N} = eval_abt(Exp, Env),
    {bool_val, N =:= 0};
eval_abt({if_exp, Question, Answer, Alternate}, Env) ->
    {bool_val, R} = eval_abt(Question, Env),
    case R of
        true -> eval_abt(Answer, Env);
        false -> eval_abt(Alternate, Env)
    end;
eval_abt({diff_exp, Exp1, Exp2}, Env) ->
    {num_val, N1} = eval_abt(Exp1, Env),
    {num_val, N2} = eval_abt(Exp2, Env),
    {num_val, N1-N2};
eval_abt({let_exp, {var_exp, Var}, Exp, Body}, Env) ->
    Val = eval_abt(Exp, Env),
    New_env = env:extend_env(Var, Val, Env),
    eval_abt(Body, New_env).

-spec eval_code(string()) -> expval().
eval_code(Code) ->
	Abt = let_lang_parse:scan_and_parse(Code),
	eval_abt(Abt, env:empty_env()).
