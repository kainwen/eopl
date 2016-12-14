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
