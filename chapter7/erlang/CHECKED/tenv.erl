-module(tenv).

-export_type([tenv/0]).

-export([empty_tenv/0, extend_tenv/2, apply_tenv/2]).

-type tp() :: checked_type:tp().
-type tenv() :: {empty_tenv}
              | {tenv, [{atom(), tp()}], tenv()}.

-spec empty_tenv() -> tenv().
empty_tenv() ->
    {empty_tenv}.

-spec extend_tenv(tenv(), [{atom(), tp()}]) -> tenv().
extend_tenv(Tenv, Pairs) ->
    {tenv, Pairs, Tenv}.

-spec apply_tenv(tenv(), atom()) -> tp().
apply_tenv({empty_tenv}, _) ->
    erlang:error(can_not_find_var_type_in_tenv);
apply_tenv({tenv, Var_tps, Saved_tenv}, Var) ->
    case proplists:get_value(Var, Var_tps) of
        undefined -> apply_tenv(Saved_tenv, Var);
        Tp -> Tp
    end.
