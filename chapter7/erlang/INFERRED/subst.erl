-module(subst).

-export([
         new/0, add_to_subst/3, apply_subst/2, subst_one/3, is_safe/2
        ]).

-export_type([type_var/0, type_with_var/0, subst/0]).

-type type_var() :: atom().

-type type_with_var() :: {int}
                       | {bool}
                       | type_var()
                       | {arrow, type_with_var(), type_with_var()}
                       | {star, [type_with_var()]}
                       | {list, type_with_var()}.

-type subst() :: [{type_var(), type_with_var()}].

-spec new() -> subst().
new() -> [].

-spec add_to_subst(Subst::subst(),
                   Tp_var::type_var(),
                   Tp::type_with_var()) -> subst().
add_to_subst(Subst, Tp_var, Tp) ->
    case proplists:get_value(Tp_var, Subst) of
        undefined ->
            New_subst = [{Tv, subst_one(T, Tp_var, Tp)}
                         || {Tv, T} <- Subst],
            [{Tp_var, Tp}|New_subst];
        _ -> Subst
    end.

-spec apply_subst(Subst::subst(), Tp::type_with_var()) -> type_with_var().
apply_subst(_Subst, {int}) -> {int};
apply_subst(_Subst, {bool}) -> {bool};
apply_subst(Subst, {arrow, T1, T2}) ->
    {arrow, apply_subst(Subst, T1), apply_subst(Subst, T2)};
apply_subst(Subst, {star, Tps}) ->
    {star, [apply_subst(Subst, Tp)|| Tp <- Tps]};
apply_subst(Subst, {list, Tp}) ->
    {list, apply_subst(Subst, Tp)};
apply_subst(Subst, Tv) ->
    case proplists:get_value(Tv, Subst) of
        undefined -> Tv;
        Tp -> Tp
    end.

-spec subst_one(T::type_with_var(),
                Tp_var::type_var(),
                Tp::type_with_var()) -> type_with_var().
subst_one({int}, _Tp_var, _Tp) -> {int};
subst_one({bool}, _Tp_var, _Tp) -> {bool};
subst_one({arrow, T1, T2}, Tp_var, Tp) ->
    {arrow,
     subst_one(T1, Tp_var, Tp),
     subst_one(T2, Tp_var, Tp)};
subst_one({star, Tps}, Tp_var, Tp) ->
    {star, [subst_one(T, Tp_var, Tp) || T <- Tps]};
subst_one({list, T}, Tp_var, Tp) ->
    {list, subst_one(T, Tp_var, Tp)};
subst_one(Tp_var, Tp_var, Tp) -> Tp;
subst_one(Tv, _Tp_var, _Tp) -> Tv.

-spec is_safe(Tp_var::type_var(), type_with_var()) -> boolean().
is_safe(_Tp_var, {int}) -> true;
is_safe(_Tp_var, {bool}) -> true;
is_safe(Tp_var, {arrow, T1, T2}) ->
    is_safe(Tp_var, T1) and is_safe(Tp_var, T2);
is_safe(Tp_var, {star, Tps}) ->
    lists:all(fun (X) -> X end,
              [{is_safe(Tp_var, Tp)} || Tp <- Tps]);
is_safe(Tp_var, {list, Tp}) ->
    is_safe(Tp_var, Tp);
is_safe(Tp_var, Tv) when is_atom(Tv) ->
    Tp_var /= Tv.
