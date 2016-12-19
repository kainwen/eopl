-module(diff_tree).

-include_lib("eunit/include/eunit.hrl").

-export([zero/0, successor/1, predecessor/1, is_zero/1]).

-type diff_tree() :: {one} | {diff, diff_tree(), diff_tree()}.

one() ->
    {one}.

zero() ->
    {diff, {one}, {one}}.

is_zero(N) ->
    diff_tree_to_int(N) =:= 0.

minus_one() ->
    {diff, zero(), {one}}.

successor(N) ->
    {diff, N, minus_one()}.

predecessor(N) ->
    {diff, N, one()}.

diff_tree_plus(N1, N2) ->
    {diff, N1,
     {diff, zero(), N2}}.

diff_tree_to_int({one}) -> 1;
diff_tree_to_int({diff, A, B}) ->
    diff_tree_to_int(A) - diff_tree_to_int(B).

diff_tree_to_int_test() ->
    N1 = predecessor(zero()),
    N2 = successor(successor(successor(zero()))),
    [
     ?assert(diff_tree_to_int(one()) =:= 1),
     ?assert(diff_tree_to_int(zero()) =:= 0),
     ?assert(diff_tree_to_int(successor(zero())) =:= 1),
     ?assert(diff_tree_to_int(predecessor(zero())) =:= -1),
     ?assert(diff_tree_to_int(diff_tree_plus(N1, N2)) =:= 2)
    ].
