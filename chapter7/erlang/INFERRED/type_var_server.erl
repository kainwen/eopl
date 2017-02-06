-module(type_var_server).

-export([start/0, new_type_var/0, loop/1]).

%% APIs
start() ->
    Pid = spawn(?MODULE, loop, [0]),
    register(tv_server, Pid).

new_type_var() ->
    tv_server ! {new, self()},
    receive
        Tv -> Tv
    end.

loop(N) ->
    receive
        {new, From} ->
            Tv = string:join(["t", integer_to_list(N)], "_"),
            From ! list_to_atom(Tv)
    end,
    loop(N+1).
