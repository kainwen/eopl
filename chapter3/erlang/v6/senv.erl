-module(senv).

-include_lib("eunit/include/eunit.hrl").

-export_type([senv/0]).

-export([empty_senv/0, extend_senv/2, apply_senv/2]).

-type env_element() :: atom() | [atom()].
-type senv() :: [env_element()].


-spec empty_senv() -> senv().
empty_senv() -> [].

-spec extend_senv(senv(), env_element()) -> senv().
extend_senv(Senv, Ele) -> [Ele|Senv].

-spec apply_senv(senv(), atom()) -> integer().
apply_senv([], _Var) -> erlang:error(can_not_find_the_var);
apply_senv([A|Rem_senv], Var) when is_list(A) ->
    case index_in_list(A, Var) of
        not_find -> length(A) + apply_senv(Rem_senv, Var);
        L -> L
    end;
apply_senv([A|Rem_senv], Var) when is_atom(A) ->
    case A =:= Var of
        true -> 0;
        false -> 1 + apply_senv(Rem_senv, Var)
    end.


index_in_list([], _Var) -> not_find;
index_in_list([A|_Rems], Var) when A =:= Var -> 0;
index_in_list([A|Rems], Var) when A /= Var ->
    case index_in_list(Rems, Var) of
        not_find -> not_find;
        L -> L + 1
    end.


senv_test() ->
    E = extend_senv(empty_senv(),
                    [a, b, c]),
    E1 = extend_senv(E, d),
    E2 = extend_senv(E1, [e,f,g]),
    [
     ?assert(apply_senv(E, a) =:= 0),
     ?assert(apply_senv(E, b) =:= 1),
     ?assert(apply_senv(E1, d) =:= 0),
     ?assert(apply_senv(E1, c) =:= 3),
     ?assert(apply_senv(E2, e) =:= 0),
     ?assert(apply_senv(E2, f) =:= 1),
     ?assert(apply_senv(E2, d) =:= 3),
     ?assert(apply_senv(E2, c) =:= 6)
    ].
