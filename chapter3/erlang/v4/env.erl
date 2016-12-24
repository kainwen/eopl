-module(env).

-export([empty_env/0, extend_env/3, apply_env/2,
         extend_env_by_list/2, extend_env_rec/4,
         extend_env_rec_by_list/2
        ]).

-export_type([environment/0]).

-type environment() :: {empty_env}
                     | {extend_env, atom(), let_lang:expval(), environment()}
                     | {extend_env_rec,
                        atom(),   %proc_name
                        [atom()], %parameter list
                        let_lang_parse:abstract_syntax_tree(), %proc_body
                        environment()}.         %saved-env

-spec empty_env() -> environment().
empty_env() -> {empty_env}.

-spec extend_env(atom(), let_lang:expval(), environment()) -> environment().
extend_env(Var, Val, Env) ->
    {extend_env, Var, Val, Env}.

extend_env_rec(Proc_name, Paras, Proc_body, Env) ->
    {extend_env_rec, Proc_name, Paras, Proc_body, Env}.

extend_env_rec_by_list(Env, []) -> Env;
extend_env_rec_by_list(Env, [{Name, Paras, Proc_body, _Env}|Rems]) ->
    New_env = extend_env_rec(Name, Paras, Proc_body, Env),
    extend_env_rec_by_list(New_env, Rems).

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
apply_env(Env={extend_env_rec, Proc_name, Paras, Proc_body, Saved_env}, Var) ->
    case Proc_name =:= Var of
        true -> {proc_val, {Paras, Proc_body, Env}};
        false -> apply_env(Saved_env, Var)
    end.
