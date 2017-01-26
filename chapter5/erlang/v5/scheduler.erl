-module(scheduler).

-behaviour(gen_server).

-export_type([thread/0, scheduler/0]).

-export([
         init_scheduler/1, is_time_expired/1, place_on_ready_queue/2,
         run_next_thread/1, decrement_time/1, set_final_answer/2
        ]).

-export([run_thread/1]).

-export([
         init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3
        ]).


-record(state, {the_ready_queue :: queue(),
                the_final_answer,
                the_max_time_slice :: integer(),
                the_time_remaining :: integer()
               }).

-type exp() :: thread_lang_parse:exp().
-type store() :: store:store().
-type cont() :: cont:cont().
-type val() :: thread_lang:expval().
-type queue() :: random_queue:queue().
-type scheduler() :: pid().

-type thread() :: {just_spawn, Proc::exp(), Store::store(), Sched::scheduler()}
                | {intermid, Cont::cont(), Val::val(), Store::store(), Sched::scheduler()}.

%% APIs
-spec init_scheduler(integer()) -> scheduler().
init_scheduler(Ticks) ->
    {ok, Scheduler} = gen_server:start_link({local, ?MODULE}, ?MODULE, [Ticks], []),
    Scheduler.

-spec is_time_expired(scheduler()) -> boolean().
is_time_expired(Scheduler) ->
    gen_server:call(Scheduler, {is_time_expired}).

-spec place_on_ready_queue(scheduler(), thread()) -> ok.
place_on_ready_queue(Scheduler, Thread) ->
    gen_server:call(Scheduler, {place_on_ready_queue, Thread}).

run_next_thread(Scheduler) ->
    case gen_server:call(Scheduler, {run_next_thread}) of
        {final_answer, Fa} -> Fa;
        {thread, Thread} -> run_thread(Thread)
    end.

decrement_time(Scheduler) ->
    ok = gen_server:call(Scheduler, {decrement_time}),
    ok.

set_final_answer(Scheduler, Fa) ->
    ok = gen_server:call(Scheduler, {set_final_answer, Fa}),
    ok.

%% Callbacks
init([Ticks]) ->
    State = #state{the_ready_queue=random_queue:new(),
                   the_final_answer=not_defined,
                   the_max_time_slice=Ticks,
                   the_time_remaining=Ticks},
    {ok, State}.

handle_call({is_time_expired}, _From, St) ->
    Remain_time = St#state.the_time_remaining,
    {reply, Remain_time < 0, St};
handle_call({place_on_ready_queue, Thread}, _From, St) ->
    Old_queue = St#state.the_ready_queue,
    New_queue = random_queue:in(Old_queue, Thread),
    New_state = St#state{the_ready_queue=New_queue},
    {reply, ok, New_state};
handle_call({run_next_thread}, _From, St) ->
    Queue = St#state.the_ready_queue,
    case random_queue:is_empty(Queue) of
        true ->
            {reply, {final_answer, St#state.the_final_answer}, St};
        false ->
            {Thread, New_queue} = random_queue:out(Queue),
            New_state = St#state{the_ready_queue=New_queue,
                                 the_time_remaining=St#state.the_max_time_slice},
            {reply, {thread, Thread}, New_state}
    end;
handle_call({decrement_time}, _From, St) ->
    Old_remain_time = St#state.the_time_remaining,
    New_remain_time = Old_remain_time - 1,
    New_state = St#state{the_time_remaining=New_remain_time},
    {reply, ok, New_state};
handle_call({set_final_answer, Fa}, _From, St) ->
    New_state = St#state{the_final_answer=Fa},
    {reply, ok, New_state}.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
{ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec run_thread(thread()) -> val().
run_thread({just_spawn, {proc_val, [], Body, Lex_env}, Store, Sched}) ->
    thread_lang:eval(Body, Lex_env, cont:end_subthread_cont(), Store, Sched);
run_thread({intermid, Cont, Val, Store, Sched}) ->
    cont:apply_cont(Cont, Val, Store, Sched).
