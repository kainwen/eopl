-module(senv).

-export([empty_senv/0, get_curr_depth/1, extend_senv/2, get_var_depth/2]).

-export_type([senv/0, pair/0]).

-type var() :: atom().
-type depth() :: integer().

-type pair() :: {var(), depth()}.

-type senv() :: [pair()].

-spec empty_senv() -> senv().
empty_senv() ->
    [].

-spec get_curr_depth(senv()) -> integer().
get_curr_depth(Senv) ->
    length(Senv).

-spec extend_senv(senv(), [pair()]) -> senv().
extend_senv(Senv, Vars_with_depth) ->
    [Vars_with_depth|Senv].

-spec get_var_depth(senv(), var()) -> integer().
get_var_depth([], _Var) ->
    erlang:error(can_not_find_the_var);
get_var_depth(Senv=[Pair_list|Saved_env], Var) ->
    case proplists:get_value(Var, Pair_list) of
        undefined -> get_var_depth(Saved_env, Var);
        _ -> length(Senv)
    end.
