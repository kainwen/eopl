-module(random_queue).

-export_type([queue/0]).

-export([is_empty/1, in/2, out/1, new/0]).

-type thread() :: scheduler:thread().
-type queue() :: [thread()].

-spec new() -> queue().
new() ->
    [].

-spec is_empty(queue()) -> boolean().
is_empty([]) -> true;
is_empty(_) -> false.

-spec in(queue(), thread()) -> queue().
in(Q, Term) ->
    [Term|Q].

-spec out(queue()) -> {thread(), queue()}.
out([]) -> erlang:error(empty_random_queue_cannot_dequeue);
out(Q) ->
    Len = length(Q),
    Index_to_remove = rand:uniform(Len),
    remove_nth(Q, Index_to_remove).

%% Internal functions
-spec remove_nth(queue(), integer()) -> {thread(), queue()}.
remove_nth([], _) ->
    erlang:error(can_not_remove_from_empty_queue);
remove_nth([A|Rems], 1) ->
    {A, Rems};
remove_nth([A|Rems], N) when N > 1 ->
    {Term, Rem_queue} = remove_nth(Rems, N-1),
    {Term, [A|Rem_queue]}.
