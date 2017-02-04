-module(vector).

-behaviour(gen_server).

-export([
         new/0, append_list/2, append/2, remove_from/2, insert_pos/1,
         to_list/1
        ]).

-export([
         init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3
        ]).

-export_type([vector/0]).

-type vector() :: pid().


%% APIs
-spec new() -> vector().
new() ->
    {ok, Pid} = gen_server:start_link(?MODULE, [], []),
    Pid.

-spec append_list(vector(), [any()]) -> ok.
append_list(Vector, Lst) ->
    gen_server:call(Vector, {append_list, Lst}).

-spec append(vector(), any()) -> ok.
append(Vector, E) ->
    gen_server:call(Vector, {append, E}).

-spec remove_from(vector(), integer()) -> [any()].
remove_from(Vector, Pos) ->
    gen_server:call(Vector, {remove_from, Pos}).

-spec insert_pos(vector()) -> integer().
insert_pos(Vector) ->
    gen_server:call(Vector, {insert_pos}).

-spec to_list(vector()) -> [any()].
to_list(Vector) ->
    gen_server:call(Vector, {to_list}).

%% Callbacks
init([]) ->
    {ok, []}.

handle_call({append_list, Lst}, _From, State) ->
    {reply, ok, State ++ Lst};
handle_call({append, E}, _From, State) ->
    {reply, ok, State ++ [E]};
handle_call({remove_from, Pos}, _From, State) ->
    Len = length(State) - Pos + 1,
    Pop_list = lists:sublist(State, Pos, Len),
    New_state = lists:sublist(State, Pos-1),
    {reply, Pop_list, New_state};
handle_call({insert_pos}, _From, State) ->
    {reply, length(State) + 1, State};
handle_call({to_list}, _From, State) ->
    {reply, State, State}.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
{ok, State}.
