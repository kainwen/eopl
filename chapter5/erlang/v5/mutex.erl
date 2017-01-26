-module(mutex).

-compile(export_all).

-export_type([mutex/0]).

-type ref() :: store:ref().
-type store() :: store:store().

-record(mutex, {is_closed :: ref(),
                wait_queue :: ref()}).

-type mutex() :: #mutex{}.

-spec new_mutex(store()) -> mutex().
new_mutex(Store) ->
    Is_closed = false,
    Ref_is_closed = store:newref(Store, Is_closed),
    Wait_queue = random_queue:new(),
    Ref_wait_queue = store:newref(Store, Wait_queue),
    #mutex{is_closed=Ref_is_closed,
           wait_queue=Ref_wait_queue}.

wait_for_mutex(Store, Sched, Mutex, Thread) ->
    Ref_is_closed = Mutex#mutex.is_closed,
    Is_closed = store:deref(Store, Ref_is_closed),
    case Is_closed of
        true ->
            Ref_wait_queue = Mutex#mutex.wait_queue,
            Wait_queue = store:deref(Store, Ref_wait_queue),
            New_wait_queue = random_queue:in(Wait_queue, Thread),
            store:setref(Store, Ref_wait_queue, New_wait_queue),
            scheduler:run_next_thread(Sched);
        false ->
            store:setref(Store, Ref_is_closed, true),
            scheduler:run_thread(Thread)
    end.

signal_mutex(Store, Sched, Mutex, Thread) ->
    #mutex{is_closed=Ref_is_closed, wait_queue=Ref_wait_queue} = Mutex,
    Is_closed = store:deref(Store, Ref_is_closed),
    Wait_queue = store:deref(Store, Ref_wait_queue),
    case Is_closed of
        true ->
            case random_queue:is_empty(Wait_queue) of
                true ->
                    store:setref(Store, Ref_is_closed, false),
                    scheduler:run_thread(Thread);
                false ->
                    {Thd, Rem_wait_queue} = random_queue:out(Wait_queue),
                    scheduler:place_on_ready_queue(Sched, Thd),
                    store:setref(Store, Ref_wait_queue, Rem_wait_queue),
                    scheduler:run_thread(Thread)
            end;
        false ->
            scheduler:run_thread(Thread)
    end.
