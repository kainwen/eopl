-module(c1).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

% 1.15
duple(0, _) -> [];
duple(N, X) when N > 0 ->
    [X|duple(N-1, X)].

duple_test() ->
    [
     ?_assert(duple(0, a) =:= []),
     ?_assert(duple(2, 3) =:= [3, 3]),
     ?_assert(duple(4, [ha, ha]) =:= [[ha, ha], [ha, ha], [ha, ha], [ha, ha]])
    ].

% 1.16
invert([]) -> [];
invert([[A, B]|Rem]) ->
    [[B, A]|invert(Rem)].

invert_test() ->
    [
     ?_assert(invert([[a, 1], [a, 2], [1, b], [2, b]]) =:= [[1, a], [2, a], [b, 1], [b, 2]])
    ].

% 1.17
down([]) -> [];
down([A|Rem]) ->
    [[A]|down(Rem)].

down_test() ->
    [
     ?assert(down([1, 2, 3]) =:= [[1], [2], [3]]),
     ?assert(down([[a], [fine], [idea]]) =:= [[[a]], [[fine]], [[idea]]]),
     ?assert(down([a, [more, [complicated]], object]) =:= [[a], [[more, [complicated]]], [object]])
    ].

% 1.18
swapper(_, _, []) -> [];
swapper(S1, S2, [S1|Rem]) ->
    [S2|swapper(S1, S2, Rem)];
swapper(S1, S2, [S2|Rem]) ->
    [S1|swapper(S1, S2, Rem)];
swapper(S1, S2, [S|Rem]) when is_list(S) ->
    [swapper(S1, S2, S)|swapper(S1, S2, Rem)];
swapper(S1, S2, [S|Rem]) ->
    [S|swapper(S1, S2, Rem)].


swapper_test() ->
    [
     ?assert(swapper(a, d, [a, b, c, d]) =:= [d, b, c, a]),
     ?assert(swapper(a, d, [a, d, [], c, d]) =:= [d, a, [], c, a]),
     ?assert(swapper(x, y, [[x], y, [z, [x]]]) =:= [[y], x, [z, [y]]])
    ].

% 1.19
list_set([_|Rem], 0, X) ->
    [X|Rem];
list_set([Y|Rem], N, X) when N > 0 ->
    [Y|list_set(Rem, N-1, X)].


list_set_test() ->
    [
     ?assert(list_set([a, b, c, d], 2, [1, 2]) =:= [a, b, [1, 2], d]),
     ?assert(lists:nth(4, list_set([a, b, c, d], 3, [1, 5, 10])) =:= [1, 5, 10])
    ].

% 1.20
%count_occurrences(S, Slist) ->
count_occurrences(_, []) -> 0;
count_occurrences(S, [S|Rem]) ->
    1 + count_occurrences(S, Rem);
count_occurrences(S, [L|Rem]) when is_list(L) ->
    count_occurrences(S, L) + count_occurrences(S, Rem);
count_occurrences(S, [_|Rem]) ->
    count_occurrences(S, Rem).

count_occurrences_test() ->
    [
     ?assert(count_occurrences(x, [[f, x], y, [[[x, z], x]]]) =:= 3),
     ?assert(count_occurrences(x, [[f, x], y, [[[x, z], [], x]]]) =:= 3),
     ?assert(count_occurrences(w, [[f, x], y, [[[x, z], x]]]) =:= 0)
    ].

% 1.21
product([],_) -> [];
product(_, []) ->[];
product([A|Rem], Sos2) ->
    L1 = lists:map(fun(S2) -> [A, S2] end, Sos2),
    L1 ++ product(Rem, Sos2).

product_test() ->
    [
     ?assert(product([a, b, c], [x, y]) =:= [[a, x], [a, y], [b, x], [b, y], [c, x], [c, y]])
    ].

%1.22
filter_in(_, []) -> [];
filter_in(Pred, [A|Rems]) ->
    case Pred(A) of
        true -> [A|filter_in(Pred, Rems)];
        false -> filter_in(Pred, Rems)
    end.

filter_in_test() ->
    [
     ?assert(filter_in(fun (A) -> is_number(A) end, [a, 2, [1, 3], b, 7]) =:= [2, 7]),
     ?assert(filter_in(fun (A) -> is_atom(A) end, [a, [b, c], 17, foo]) =:= [a, foo])
    ].

%1.23
list_index_helper(_, [], _) -> false;
list_index_helper(Pred, [A|Rem], Index) ->
    case Pred(A) of
        true -> Index;
        false -> list_index_helper(Pred, Rem, Index+1)
    end.

list_index(Pred, List) ->
    list_index_helper(Pred, List, 0).

list_index_test() ->
    [
     ?assert(list_index(fun (A) -> is_number(A) end, [a, 2, [1, 3], b, 7]) =:= 1),
     ?assert(list_index(fun (A) -> is_atom(A) end, [a, [b, c], 17, foo]) =:= 0),
     ?assert(list_index(fun (A) -> is_atom(A) end, [1, 2, [a, b], 3]) =:= false)
    ].

%1.24
every(_, []) -> true;
every(Pred, [A|Rem]) ->
    case Pred(A) of
        true -> every(Pred, Rem);
        false -> false
    end.

every_test() ->
    [
     ?assert(every(fun (A) -> is_number(A) end, [a, b, c, 3, e]) =:= false),
     ?assert(every(fun (A) -> is_number(A) end, [1, 2, 3, 5, 4]) =:= true)
    ].

%1.25
exists(_, []) -> false;
exists(Pred, [A|Rem]) ->
    case Pred(A) of
        true -> true;
        false -> exists(Pred, Rem)
    end.

exists_test() ->
    [
     ?assert(exists(fun (A) -> is_number(A) end, [a, b , c, 3, e]) =:= true),
     ?assert(exists(fun (A) -> is_number(A) end, [a, b , c, d, e]) =:= false)
    ].

%1.26
up([]) -> [];
up([A|Rem]) when is_list(A) ->
    lists:append(A, up(Rem));
up([A|Rem]) ->
    [A|up(Rem)].

up_test() ->
    [
     ?assert(up([[1, 2], [3, 4]]) =:= [1, 2, 3, 4]),
     ?assert(up([[x, [y]], z]) =:= [x, [y], z])
    ].

%1.27
flatten([]) -> [];
flatten([S|Slist]) when is_list(S) ->
    lists:append(flatten(S),
                 flatten(Slist));
flatten([S|Slist]) ->
    [S|flatten(Slist)].

flatten_test() ->
    [
     ?assert(flatten([a, b, c]) =:= [a, b, c]),
     ?assert(flatten([[a], [], [b, []], [], [c]]) =:= [a, b, c]),
     ?assert(flatten([[a, b], c, [[[d]], e]]) =:= [a, b, c, d, e]),
     ?assert(flatten([a, b, [[], [c]]]) =:= [a, b, c])
    ].

%1.28
%merge(Loi1, Loi2)
merge([], Loi2) -> Loi2;
merge(Loi1, []) -> Loi1;
merge(Loi1=[A|_], [B|Rem2]) when A > B ->
    [B|merge(Loi1, Rem2)];
merge([A|Rem1], Loi2) ->
    [A|merge(Rem1, Loi2)].

merge_test() ->
    [
     ?assert(merge([1, 4], [1, 2, 8]) =:= [1, 1, 2, 4, 8]),
     ?assert(merge([35, 62, 81, 90, 91], [3, 83, 85, 90]) =:= [3, 35, 62, 81, 83, 85, 90, 90, 91])
    ].

%1.29
insert(A, []) -> [A];
insert(A, [B|Rem]) when A > B ->
    [B|insert(A, Rem)];
insert(A, L) -> [A|L].

sort([]) -> [];
sort([A|Rem]) ->
    Order_Rem = sort(Rem),
    insert(A, Order_Rem).

sort_test() ->
    [
     ?assert(sort([8, 2, 5, 2, 3]) =:= [2, 2, 3, 5, 8])
    ].

%1.30
insert_predict(_, A, []) -> [A];
insert_predict(Pred, A, L=[B|Rem]) ->
    case Pred(A, B) of
        true -> [A|L];
        false -> [B|insert_predict(Pred, A, Rem)]
    end.

sort_predict(_, []) -> [];
sort_predict(Pred, [A|Rem]) ->
    Order_Rem = sort_predict(Pred, Rem),
    insert_predict(Pred, A, Order_Rem).

sort_predict_test() ->
    [
     ?assert(sort_predict(fun(A, B) -> A < B end, [8, 2, 5, 2, 3]) =:= [2, 2, 3, 5, 8]),
     ?assert(sort_predict(fun(A, B) -> A > B end, [8, 2, 5, 2, 3]) =:= [8, 5, 3, 2, 2])
    ].

%1.31
-type binary_tree() :: integer() | {atom(), binary_tree(), binary_tree()}.

-spec leaf(integer()) -> binary_tree().
leaf(N) -> N.

-spec interior_node(atom(), binary_tree(), binary_tree()) -> binary_tree().
interior_node(S, Lson, Rson) ->
    {S, Lson, Rson}.

-spec is_leaf(binary_tree()) -> boolean().
is_leaf(T) -> is_integer(T).

-spec lson(binary_tree()) -> binary_tree().
lson({_, Lson, _}) -> Lson.

-spec rson(binary_tree()) -> binary_tree().
rson({_, _, Rson}) -> Rson.

-spec contents_of(binary_tree()) -> atom()|integer().
contents_of({S, _, _}) -> S;
contents_of(N) when is_integer(N) -> N.

ex31_test() ->
    [
     ?assert(leaf(31) =:= 31),
     ?assert(interior_node(a, 1, 2) =:= {a, 1, 2}),
     ?assert(is_leaf(31) =:= true),
     ?assert(is_leaf(interior_node(a, 1, 2)) =:= false),
     ?assert(lson(interior_node(a, 1, 2)) =:= 1),
     ?assert(rson(interior_node(a, 1, 2)) =:= 2),
     ?assert(contents_of(leaf(31)) =:= 31),
     ?assert(contents_of(interior_node(a, 1, 2)) =:= a)
    ].

%1.32
-spec double_tree(binary_tree()) -> binary_tree().
double_tree(N) when is_integer(N) -> 2*N;
double_tree({S, Lson, Rson}) ->
    interior_node(S, double_tree(Lson), double_tree(Rson)).

double_tree_test() ->
    [
     ?assert(double_tree(leaf(5)) =:= 10),
     ?assert(double_tree(interior_node(a, leaf(1), leaf(2))) =:= {a, 2, 4})
    ].

%1.33
make_red_depth_helper(N, Num) when is_integer(N) -> leaf(Num);
make_red_depth_helper({red, Lson, Rson}, Num) ->
    {red, make_red_depth_helper(Lson, Num+1), make_red_depth_helper(Rson, Num+1)};
make_red_depth_helper({S, Lson, Rson}, Num) ->
    {S, make_red_depth_helper(Lson, Num), make_red_depth_helper(Rson, Num)}.

make_leaves_with_red_depth(T) ->
    make_red_depth_helper(T, 0).

make_leaves_with_red_depth_test() ->
    [
     ?assert(make_leaves_with_red_depth(
               interior_node(red,
                             interior_node(bar, leaf(26), leaf(12)),
                             interior_node(red,
                                           leaf(11),
                                           interior_node(quxx,
                                                        leaf(117),
                                                        leaf(14))))) =:=
            {red, {bar, 1, 1}, {red, 2, {quxx, 2, 2}}})
    ].

%1.34
-type bst() :: {} | {integer(), bst(), bst()}.

path_helper(_, {}, _) -> [];
path_helper(N, {N, _, _}, Path_list) -> Path_list;
path_helper(N, {M, Lson, _}, Path_list) when N < M ->
    path_helper(N, Lson, [left|Path_list]);
path_helper(N, {M, _, Rson}, Path_list) when N > M ->
    path_helper(N, Rson, [right|Path_list]).

path(N, Tree) ->
    lists:reverse(path_helper(N, Tree, [])).

path_test() ->
    [
     ?assert(path(17, {14,
                       {7, {}, {12, {}, {}}},
                       {26,
                        {20, {17, {}, {}}, {}},
                        {31, {}, {}}}}) =:=
                 [right, left, left])
    ].

%1.35
number_leave_helper(N, Start_index) when is_integer(N) ->
    {leaf(Start_index), Start_index+1};
number_leave_helper({S, Lson, Rson}, Start_index) ->
    {New_ltree, Next_index} = number_leave_helper(Lson, Start_index),
    {New_rtree, Index} = number_leave_helper(Rson, Next_index),
    {interior_node(S, New_ltree, New_rtree), Index}.

number_leaves(Tree) ->
    {T, _} = number_leave_helper(Tree, 0),
    T.

number_leaves_test() ->
    [
     ?assert(number_leaves(interior_node(foo, {bar, 26, 12}, {baz, 11, {quux, 117, 14}})) =:= interior_node(foo,
                                                                                                            interior_node(bar, 0, 1),
                                                                                                            interior_node(baz,
                                                                                                                          2,
                                                                                                                          interior_node(quux, 3, 4))))
    ].
