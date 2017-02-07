-module(type_var_server).

-export([start/0, new_type_var/0, loop/1]).

%% APIs
start() ->
    case whereis(tv_server) of
        undefined ->
            P = spawn(?MODULE, loop, [0]),
            register(tv_server, P);
        _ ->
            kill(),
            S = spawn(?MODULE, loop, [0]),
            register(tv_server, S)
    end.

kill() ->
    tv_server ! {kill, self()},
    receive
        ok -> ok
    end.

new_type_var() ->
    tv_server ! {new, self()},
    receive
        Tv -> Tv
    end.

loop(N) ->
    receive
        {new, From} ->
            Tv = string:join(["t", integer_to_list(N)], "_"),
            From ! list_to_atom(Tv);
        {kill, From} ->
            From ! ok,
            erlang:exit(ok)
    end,
    loop(N+1).
