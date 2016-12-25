-module(env).

-export_type([environment/0]).

-type environment() :: fun((atom()) -> let_lang:expval()).

-export([empty_env/0, extend_env/3, apply_env/2,
         extend_env_by_list/2, extend_env_rec/2
        ]).

-spec empty_env() -> environment().
empty_env() ->
    fun (_Var) ->
            erlang:error(can_not_find_the_var)
    end.

-spec extend_env(atom(), let_lang:expval(), environment()) -> environment().
extend_env(Var, Val, Env) ->
    fun (Search_var) ->
            case Var =:= Search_var of
                true -> Val;
                false -> apply_env(Env, Search_var)
            end
    end.

-spec extend_env_by_list(environment(), [{atom(), let_lang:expval()}]) -> environment().
extend_env_by_list(Env, []) -> Env;
extend_env_by_list(Env, [{Var, Val}|Rems]) ->
    New_env = extend_env(Var, Val, Env),
    extend_env_by_list(New_env, Rems).

-spec extend_env_rec(environment(), [{atom(), {[atom()], let_lang_parse:abstract_syntax_tree()}}]) -> environment().
extend_env_rec(Env, Name_procs) ->
    fun (Search_var) ->
            case proplists:get_value(Search_var, Name_procs) of
                undefined -> apply_env(Env, Search_var);
                {Paras, Proc_Body} -> {proc_val, {Paras, Proc_Body, Env}}
            end
    end.

-spec apply_env(environment(), atom()) -> let_lang:expval().
apply_env(Env, Search_var) ->
    Env(Search_var).
