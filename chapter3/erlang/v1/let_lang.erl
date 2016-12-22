-module(let_lang).

-export([eval_code/1]).

-export_type([expval/0]).

-type expval() :: {num_val, integer()}
                | {bool_val, boolean()}
                | {list_val, [expval()]}.

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
eval_abt({let_exp, Bindlist, Body}, Env) ->
    Binding_pairs = [{Id, eval_abt(E, Env)} || {{var_exp, Id}, E} <- Bindlist],
    New_env = env:extend_env_by_list(Env, Binding_pairs),
    eval_abt(Body, New_env);
eval_abt({emptylist}, _Env) ->
    {list_val, []};
eval_abt({cons_exp, Exp1, Exp2}, Env) ->
    V1 = eval_abt(Exp1, Env),
    {list_val, Vs} = eval_abt(Exp2, Env),
    {list_val, [V1|Vs]};
eval_abt({unpack_exp, Vars, Exp, Body}, Env) ->
    {list_val, Vals} = eval_abt(Exp, Env),
    Pairs = lists:zip([Id || {var_exp, Id} <- Vars], Vals),
    New_env = env:extend_env_by_list(Env, Pairs),
    eval_abt(Body, New_env).


-spec eval_code(string()) -> expval().
eval_code(Code) ->
	Abt = let_lang_parse:scan_and_parse(Code),
	eval_abt(Abt, env:empty_env()).
