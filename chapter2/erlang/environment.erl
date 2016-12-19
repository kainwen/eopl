%% We could use different representations for environment
%% We simply ignore the situation that two symbol might be the same(suppose
%% they are renamed in some preprocess stage).

-module(environment).

-include_lib("eunit/include/eunit.hrl").

-behaviour(gen_server).

-export([empty_env/0, apply_env/2, extend_env/3, is_empty/1, extend_list/3]).

-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).


%%===================%%
%% APIs for client   %%
%%===================%%
empty_env() ->
    {ok, Env} = gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
    Env.

apply_env(Env, Var) ->
    gen_server:call(Env, {apply, Var}).

extend_env(Var, Val, Env) ->
    gen_server:call(Env, {extend, Var, Val}).

is_empty(Env) ->
    gen_server:call(Env, {is_empty}).

extend_list(Vars, Vals, Env) when is_list(Vars) and is_list(Vals) ->
    gen_server:call(Env, {extend_list, Vars, Vals}).


%%================%%
%%  callbacks     %%
%%================%%
init([]) ->
    Env = dict:new(),
    {ok, Env}.

handle_call({apply, Var}, _From, Env) ->
    case dict:find(Var, Env) of
        {ok, Val} -> {reply, Val, Env};
        error -> {reply, {error, "cannot find the symbol"}, Env}
    end;
handle_call({extend, Var, Val}, _From, Env) ->
    New_env = dict:store(Var, Val, Env),
    {reply, ok, New_env};
handle_call({extend_list, Vars, Vals}, _From, Env) ->
    New_env = store_list(Vars, Vals, Env),
    {reply, ok, New_env};
handle_call({is_empty}, _From, Env) ->
    {reply, dict:size(Env) =:= 0, Env}.

handle_cast(_Message, E) -> {noreply, E}.
handle_info(_Message, E) -> {noreply, E}.
terminate(_Reason, _E) -> ok.
code_change(_OldVersion, E, _Extra) -> {ok, E}.

%%=================================%%
%%     Internal Function           %%
%%=================================%%
store_list([], [], Dict) -> Dict;
store_list([Var|Var_rem], [Val|Val_rem], Dict) ->
    New_dict = dict:store(Var, Val, Dict),
    store_list(Var_rem, Val_rem, New_dict).


%%==================================%%
%%        Unit Test                 %%
%%==================================%%
env_test() ->
    Env = empty_env(),
    ?assert(is_empty(Env) =:= true),
    ?assert(apply_env(Env, a) =:= {error,"cannot find the symbol"}),
    ok = extend_env(a, 1, Env),
    ok = extend_env(b, 2, Env),
    ok = extend_env(c, 3, Env),
    ?assert(is_empty(Env) =:= false),
    ?assert(apply_env(Env, a) =:= 1),
    ?assert(apply_env(Env, b) =:= 2),
    ?assert(apply_env(Env, c) =:= 3),
    ok = extend_list([d, e, f], [5, 6, 7], Env),
    ?assert(apply_env(Env, d) =:= 5),
    ?assert(apply_env(Env, e) =:= 6),
    ?assert(apply_env(Env, f) =:= 7).
