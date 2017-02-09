-module(env).

-export([empty_env/0, extend_env/3, apply_env/2,
         extend_env_by_list/2, extend_env_rec/2
        ]).


empty_env() -> {empty_env}.

extend_env(Var, Val, Env) ->
    {extend_env, Var, Val, Env}.

extend_env_rec(Env, Name_procs) ->
    {extend_env_rec, Name_procs, Env}.

extend_env_by_list(Env, []) -> Env;
extend_env_by_list(Env, [{Var, Val}|Rems]) ->
    New_env = extend_env(Var, Val, Env),
    extend_env_by_list(New_env, Rems).

apply_env({empty_env}, _Var) -> erlang:error(can_not_find_variable);
apply_env({extend_env, V, Val, Saved_env}, Var) ->
    case V =:= Var of
        true -> Val;
        false -> apply_env(Saved_env, Var)
    end;
apply_env(Env={extend_env_rec, Name_procs, Saved_env}, Var) ->
    case proplists:get_value(Var, Name_procs) of
        undefined -> apply_env(Saved_env, Var);
        {Paras, Proc_Body} -> {proc_val, Paras, Proc_Body, Env}
    end.
