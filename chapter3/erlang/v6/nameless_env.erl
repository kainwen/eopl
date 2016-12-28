-module(nameless_env).

-export([empty_nameless_env/0,
         extend_nameless_env/2,
         extend_nameless_env_rec/2,
         apply_nameless_env/2]).

-export_type([nameless_env/0]).

-type nameless_env_element() :: let_lang:expval() | [let_lang:expval()].

-type nameless_env() :: {empty_nameless_env}
                      | {extend_nameless_env,
                         nameless_env_element(),
                         nameless_env()}
                      | {extend_nameless_env_rec,
                         nameless_env_element(),
                         nameless_env()}.

-spec empty_nameless_env() -> nameless_env().
empty_nameless_env() -> {empty_nameless_env}.

-spec extend_nameless_env(nameless_env(),nameless_env_element()) -> nameless_env().
extend_nameless_env(Nameless_env, Vals) ->
    {extend_nameless_env, Vals, Nameless_env}.

-spec extend_nameless_env_rec(nameless_env(), [let_lang:expval()]) -> nameless_env().
extend_nameless_env_rec(Nameless_env, Vals) ->
    {extend_nameless_env_rec, Vals, Nameless_env}.

-spec apply_nameless_env(nameless_env(), integer()) -> let_lang:expval().
apply_nameless_env({empty_nameless_env}, _N) -> erlang:error(can_not_find_pos);
apply_nameless_env({extend_nameless_env, Vals, Nameless_env}, N) when is_list(Vals) ->
    L = length(Vals),
    case L > N of
        false -> apply_nameless_env(Nameless_env, N-L);
        true -> lists:nth(N+1, Vals)
    end;
apply_nameless_env({extend_nameless_env, Vals, _Nameless_env}, 0) -> Vals;
apply_nameless_env({extend_nameless_env, _Vals, Nameless_env}, N) when N > 0 ->
    apply_nameless_env(Nameless_env, N-1);
apply_nameless_env(NE={extend_nameless_env_rec, Procs, Nameless_env}, N) ->
    L = length(Procs),
    case L > N of
        false -> apply_nameless_env(Nameless_env, N-L);
        true ->
            {proc_val, Proc_body, _E} = lists:nth(N+1, Procs),
            {proc_val, Proc_body, NE}
    end.
