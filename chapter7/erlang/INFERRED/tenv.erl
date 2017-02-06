-module(tenv).

-export([empty_tenv/0, extend_tenv/2, apply_tenv/2]).

empty_tenv() ->
    {empty_tenv}.

extend_tenv(Tenv, Pairs) ->
    {tenv, Pairs, Tenv}.

apply_tenv({empty_tenv}, _) ->
    erlang:error(can_not_find_var_type_in_tenv);
apply_tenv({tenv, Var_tps, Saved_tenv}, Var) ->
    case proplists:get_value(Var, Var_tps) of
        undefined -> apply_tenv(Saved_tenv, Var);
        Tp -> Tp
    end.
