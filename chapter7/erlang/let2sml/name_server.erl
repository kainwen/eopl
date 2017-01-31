-module(name_server).

-export([new_id/1, start/0, loop/1]).

-type name_server() :: name_server().

-spec new_id(name_server()) -> integer().
new_id(Ns) ->
    Ns ! {new_id, self()},
    receive
        Id -> Id
    end.

start() ->
    spawn(?MODULE, loop, [0]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
loop(N) ->
    receive
        {new_id, From} ->
            From ! N
    end,
    loop(N+1).
