-module(senv).

-compile(export_all).

-type vars() :: [{atom(), integer()}].
-type senv() :: [vars()].

-spec empty_senv() -> senv().
empty_senv() ->
    [].

-spec extend_senv(senv(), vars()) -> senv().
extend_senv(Senv, Vars) ->
    [Vars|Senv].

-spec apply_senv(senv(), atom()) -> integer().
apply_senv([], _V) ->
    erlang:error(can_not_find_the_var);
apply_senv([Vars|Saved_senv], V) ->
    case proplists:get_value(V, Vars) of
        undefined -> apply_senv(Saved_senv, V);
        Ref -> Ref
    end.
