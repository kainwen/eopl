-module(store).

-behaviour(gen_server).

-export([init_store/1, newref/2, deref/2, setref/3, stop_store/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
terminate/2, code_change/3]).

-export_type([store/0, ref/0]).

-type store() :: pid().
-type ref() :: integer().

-type stored_term() :: any().


%% APIs
-spec init_store(atom()) -> store().
init_store(Name) ->
    {ok, Store} = gen_server:start_link(?MODULE, [Name], []),
    Store.

-spec newref(store(), stored_term()) -> ref().
newref(Store, Val) ->
    {ok, Ref} = gen_server:call(Store, {newref, Val}),
    Ref.

-spec deref(store(), ref()) -> stored_term().
deref(Store, Ref) ->
    {ok, Val} = gen_server:call(Store, {deref, Ref}),
    Val.

-spec setref(store(), ref(), stored_term()) -> any().
setref(Store, Ref, Val) ->
    {ok, true} = gen_server:call(Store, {setref, Ref, Val}),
    {num_val, 42}.

stop_store(Store) ->
    gen_server:cast(Store, stop).

%% Callbacks
init([Name]) ->
    Tab = ets:new(Name, [set, named_table, private]),
    {ok, {Tab, 0}}.

handle_call({newref, Val}, _From, {Tab, N}) ->
    New_key = N,
    true = ets:insert(Tab, {New_key, Val}),
    {reply, {ok, New_key}, {Tab, N+1}};
handle_call({deref, Ref}, _From, {Tab, N}) ->
    Val = ets:lookup_element(Tab, Ref, 2),
    {reply, {ok, Val}, {Tab, N}};
handle_call({setref, Ref, Val}, _From, {Tab, N}) ->
    Result = ets:update_element(Tab, Ref, {2, Val}),
    {reply, {ok, Result}, {Tab, N}}.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
