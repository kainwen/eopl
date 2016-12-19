-module(bigits).

-include_lib("eunit/include/eunit.hrl").

-export([
         zero/0, is_zero/1, successor/1, predecessor/1
        ]).

-define(Base, 16).

int_to_bigits(0) -> [];
int_to_bigits(N) when N < ?Base -> [N];
int_to_bigits(N) when N > 0 ->
    [N rem ?Base | int_to_bigits(N div ?Base)].

bigits_to_int_helper(Acc, _, []) -> Acc;
bigits_to_int_helper(Acc, Num, [A|Rem]) ->
    bigits_to_int_helper(Acc + Num*A, Num * ?Base, Rem).

bigits_to_int([]) -> 0;
bigits_to_int(Bigits) ->
    bigits_to_int_helper(0, 1, Bigits).

zero() ->
    int_to_bigits(0).

is_zero(Bigits) ->
    Bigits =:= [].

successor(Bigits) ->
    int_to_bigits(bigits_to_int(Bigits) + 1).

predecessor(Bigits) ->
    int_to_bigits(bigits_to_int(Bigits) - 1).


bigits_test() ->
    [
     ?assert(int_to_bigits(0) =:= []),
     ?assert(bigits_to_int([]) =:= 0),
     ?assert(int_to_bigits(33) =:= [1, 2]),
     ?assert(bigits_to_int([1, 2]) =:= 33),
     ?assert(int_to_bigits(258) =:= [2, 0, 1]),
     ?assert(bigits_to_int([2, 0, 1]) =:= 258),
     ?assert(bigits_to_int(zero()) =:= 0),
     ?assert(bigits_to_int(successor(zero())) =:= 1),
     ?assert(bigits_to_int(predecessor(int_to_bigits(33))) =:= 32)
    ].
