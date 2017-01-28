-module(var_name).

-export([new/1, start/0, loop/1]).

new(Pid) ->
    Pid ! {new, self()},
    receive
        Name -> Name
    end.

start() ->
    spawn(?MODULE, loop, [0]).

loop(N) ->
    receive
        {new, From} ->
            S = string:join(["F", integer_to_list(N)], ""),
            From ! S
    end,
    loop(N+1).
