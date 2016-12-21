-module(env).

-export([empty_env/0, extend_env/3, apply_env/2]).

-export_type([environment/0]).

-type environment() :: [{atom(), let_lang:expval()}].

-spec empty_env() -> environment().
empty_env() -> [].

-spec extend_env(atom(), let_lang:expval(), environment()) -> environment().
extend_env(Var, Val, Env) ->
    [{Var, Val} | Env].

-spec apply_env(environment(), atom()) -> let_lang:expval().
apply_env([], _) -> erlang:error(can_not_find_variable);
apply_env([{Var, Val}|_Rems], Var) -> Val;
apply_env([_|Rems], Var) -> apply_env(Rems, Var).
