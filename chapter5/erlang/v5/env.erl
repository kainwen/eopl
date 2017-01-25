-module(env).

-export([empty_env/0, extend_env/2, apply_env/2]).

-export_type([env/0]).

-type ref() :: store:ref().

-type env() :: {empty_env}
             | {extend_env, [{Var::atom(), Ref::ref()}], Saved_env::env()}.

-spec empty_env() -> env().
empty_env() ->
    {empty_env}.

-spec extend_env([{V::atom(), R::ref()}], E::env()) -> env().
extend_env(Var_refs, E) ->
    {extend_env, Var_refs, E}.

-spec apply_env(E::env(), V::atom()) -> ref().
apply_env({empty_env}, _V) -> erlang:error(can_not_find_var_in_env);
apply_env({extend_env, Var_refs, Saved_env}, V) ->
    case proplists:get_value(V, Var_refs) of
        undefined -> apply_env(Saved_env, V);
        Ref -> Ref
    end.
