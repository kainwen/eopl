-module(cmp_type).

-export([cmp_type/2]).

cmp_type(T1, T2) ->
    type_var_server:start(),
    Tp1 = rename_type(T1),
    type_var_server:start(),
    Tp2 = rename_type(T2),
    Tp1 == Tp2.

rename_type({int}) -> {int};
rename_type({bool}) -> {bool};
rename_type(Tv) when is_atom(Tv) ->
    type_var_server:new_type_var();
rename_type({arrow, T1, T2}) ->
    {arrow, rename_type(T1), rename_type(T2)};
rename_type({star, Tps}) ->
    {star, [rename_type(Tp) || Tp <- Tps]};
rename_type({list, Tp}) ->
    {list, rename_type(Tp)}.
