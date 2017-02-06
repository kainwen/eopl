-module(stack).

-behaviour(gen_server).

-export([
         new/0, pop/1, push/2, pop_until/2, is_empty/1, top/1
        ]).

-export([
         init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3
        ]).

-export_type([stack/0]).

-type stack() :: pid().

%% APIs
-spec new() -> stack().
new() ->
    {ok, Pid} = gen_server:start_link(?MODULE, [], []),
    Pid.

-spec pop(stack()) -> any().
pop(Stack) ->
    gen_server:call(Stack, {pop}).

-spec push(stack(), any()) -> ok.
push(Stack, E) ->
    gen_server:call(Stack, {push, E}).

-spec pop_until(stack(), fun((any()) -> boolean())) -> [any()].
pop_until(Stack, Fun) ->
    gen_server:call(Stack, {pop_until, Fun}).

-spec is_empty(stack()) -> boolean().
is_empty(Stack) ->
    gen_server:call(Stack, {is_empty}).

-spec top(stack()) -> any().
top(Stack) ->
    gen_server:call(Stack, {top}).

%% Callbacks
init([]) ->
    {ok, []}.

handle_call({pop}, _From, []) ->
    {reply, can_not_pop_empty_stack, []};
handle_call({pop}, _From, [E|Stack]) ->
    {reply, E, Stack};
handle_call({push, E}, _From, Stack) ->
    {reply, ok, [E|Stack]};
handle_call({pop_until, Fun}, _From, Stack) ->
    {Rem_stack, Poped} = pop_stack_until(Stack, Fun, []),
    {reply, Poped, Rem_stack};
handle_call({is_empty}, _From, Stack) ->
    {reply, length(Stack) =:= 0, Stack};
handle_call({top}, _From, []) ->
    {reply, can_not_get_top_of_empty_stack, []};
handle_call({top}, _From, St=[E|_]) ->
    {reply, E, St}.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
{ok, State}.

%% Internal functions
-spec pop_stack_until([any()], fun((any()) -> boolean()), [any()]) -> {[any()],[any()]}.
pop_stack_until([], _F, Result) -> {[], Result};
pop_stack_until([S|Stack], F, Result) ->
    case F(S) of
        true -> {[S|Stack], Result};
        false -> pop_stack_until(Stack, F, Result ++ [S])
    end.
