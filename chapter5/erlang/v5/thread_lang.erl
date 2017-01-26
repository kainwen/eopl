-module(thread_lang).

-export([eval_script/3, eval/5, eval_exps/5]).

-export_type([expval/0, tmp_proc/0]).

-type exp() :: thread_lang_parse:exp().
-type cont() :: cont:cont().
-type env() :: env:env().
-type store() :: store:store().
-type sched() :: scheduler:scheduler().
-type mutex() :: mutex:mutex().

-type expval() :: {num_val, integer()}
                | {bool_val, boolean()}
                | {list_val, [expval()]}
                | {proc_val, [atom()], exp(), env()}
                | {mutex_val, mutex()}.

-type tmp_proc() :: {Paras::[atom()], Body::exp()}.


-spec eval(exp(), env(), cont(), store(), sched()) -> expval().
eval(Exp={number, _}, Env, Cont, Store, Sched) ->
    eval_number(Exp, Env, Cont, Store, Sched);
eval(Exp={var, _}, Env, Cont, Store, Sched) ->
    eval_var(Exp, Env, Cont, Store, Sched);
eval(Exp={diff_exp, _, _}, Env, Cont, Store, Sched) ->
    eval_diff(Exp, Env, Cont, Store, Sched);
eval(Exp={if_exp, _, _, _}, Env, Cont, Store, Sched) ->
    eval_if(Exp, Env, Cont, Store, Sched);
eval(Exp={test_zero_exp, _}, Env, Cont, Store, Sched) ->
    eval_test_zero(Exp, Env, Cont, Store, Sched);
eval(Exp={let_exp, _, _, _}, Env, Cont, Store, Sched) ->
    eval_let(Exp, Env, Cont, Store, Sched);
eval(Exp={proc_exp, _, _}, Env, Cont, Store, Sched) ->
    eval_proc(Exp, Env, Cont, Store, Sched);
eval(Exp={apply_exp, _, _}, Env, Cont, Store, Sched) ->
    eval_apply(Exp, Env, Cont, Store, Sched);
eval(Exp={letrec_exp, _, _, _}, Env, Cont, Store, Sched) ->
    eval_letrec(Exp, Env, Cont, Store, Sched);
eval(Exp={list_exp, _}, Env, Cont, Store, Sched) ->
    eval_list(Exp, Env, Cont, Store, Sched);
eval(Exp={car_exp, _}, Env, Cont, Store, Sched) ->
    eval_car(Exp, Env, Cont, Store, Sched);
eval(Exp={cdr_exp, _}, Env, Cont, Store, Sched) ->
    eval_cdr(Exp, Env, Cont, Store, Sched);
eval(Exp={test_null_exp, _}, Env, Cont, Store, Sched) ->
    eval_test_null(Exp, Env, Cont, Store, Sched);
eval(Exp={assign_exp, _, _}, Env, Cont, Store, Sched) ->
    eval_assign(Exp, Env, Cont, Store, Sched);
eval(Exp={block_exp, _}, Env, Cont, Store, Sched) ->
    eval_block(Exp, Env, Cont, Store, Sched);
eval(Exp={print_exp, _}, Env, Cont, Store, Sched) ->
    eval_print(Exp, Env, Cont, Store, Sched);
eval(Exp={spawn_exp, _}, Env, Cont, Store, Sched) ->
    eval_spawn(Exp, Env, Cont, Store, Sched);
eval(Exp={mutex_exp}, Env, Cont, Store, Sched) ->
    eval_mutex(Exp, Env, Cont, Store, Sched);
eval(Exp={wait_exp, _}, Env, Cont, Store, Sched) ->
    eval_wait(Exp, Env, Cont, Store, Sched);
eval(Exp={signal_exp, _}, Env, Cont, Store, Sched) ->
    eval_signal(Exp, Env, Cont, Store, Sched).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
eval_number({number, N}, _Env, Cont, Store, Sched) ->
    Val = {num_val, N},
    cont:apply_cont(Cont, Val, Store, Sched).

eval_var({var, V}, Env, Cont, Store, Sched) ->
    Ref = env:apply_env(Env, V),
    Val = store:deref(Store, Ref),
    cont:apply_cont(Cont, Val, Store, Sched).

eval_diff({diff_exp, E1, E2}, Env, Cont, Store, Sched) ->
    New_cont = cont:diff1_cont(Cont, E2, Env),
    eval(E1, Env, New_cont, Store, Sched).

eval_if({if_exp, A, B, C}, Env, Cont, Store, Sched) ->
    New_cont = cont:if_cont(Cont, Env, B, C),
    eval(A, Env, New_cont, Store, Sched).

eval_test_zero({test_zero_exp, Exp}, Env, Cont, Store, Sched) ->
    New_cont = cont:test_zero_cont(Cont),
    eval(Exp, Env, New_cont, Store, Sched).

eval_let({let_exp, Vars, Exps, Body}, Env, Cont, Store, Sched) ->
    New_cont = cont:let_cont(Cont, Env, Body, Vars),
    eval_exps(Exps, Env, New_cont, Store, Sched).

eval_proc({proc_exp, Paras, Body}, Env, Cont, Store, Sched) ->
    Proc_val = {proc_val, Paras, Body, Env},
    cont:apply_cont(Cont, Proc_val, Store, Sched).

eval_apply({apply_exp, Operator, Operands}, Env, Cont, Store, Sched) ->
    New_cont = cont:apply1_cont(Cont, Env, Operands),
    eval(Operator, Env, New_cont, Store, Sched).

eval_letrec({letrec_exp, Proc_names, Exps, Body}, Env, Cont, Store, Sched) ->
    New_cont = cont:letrec_cont(Cont, Proc_names, Body, Env),
    eval_exps(Exps, Env, New_cont, Store, Sched).

eval_list({list_exp, Exps}, Env, Cont, Store, Sched) ->
    New_cont = cont:list_cont(Cont),
    eval_exps(Exps, Env, New_cont, Store, Sched).

eval_car({car_exp, Exp}, Env, Cont, Store, Sched) ->
    New_cont = cont:car_cont(Cont),
    eval(Exp, Env, New_cont, Store, Sched).

eval_cdr({cdr_exp, Exp}, Env, Cont, Store, Sched) ->
    New_cont = cont:cdr_cont(Cont),
    eval(Exp, Env, New_cont, Store, Sched).

eval_test_null({test_null_exp, Exp}, Env, Cont, Store, Sched) ->
    New_cont = cont:test_null_cont(Cont),
    eval(Exp, Env, New_cont, Store, Sched).

eval_assign({assign_exp, V, Exp}, Env, Cont, Store, Sched) ->
    New_cont = cont:assign_cont(Cont, V, Env),
    eval(Exp, Env, New_cont, Store, Sched).

eval_block({block_exp, Exps}, Env, Cont, Store, Sched) when length(Exps) =:= 1 ->
    [Exp] = Exps,
    eval(Exp, Env, Cont, Store, Sched);
eval_block({block_exp, BExps}, Env, Cont, Store, Sched) when length(BExps) > 1 ->
    [Exp|Exps] = BExps,
    New_cont = cont:block_cont(Cont, Env, Exps),
    eval(Exp, Env, New_cont, Store, Sched).

eval_print({print_exp, Exp}, Env, Cont, Store, Sched) ->
    New_cont = cont:print_cont(Cont),
    eval(Exp, Env, New_cont, Store, Sched).

eval_spawn({spawn_exp, Exp}, Env, Cont, Store, Sched) ->
    New_cont = cont:spawn_cont(Cont),
    eval(Exp, Env, New_cont, Store, Sched).

eval_mutex({mutex_exp}, _Env, Cont, Store, Sched) ->
    Val = {mutex_val, mutex:new_mutex(Store)},
    cont:apply_cont(Cont, Val, Store, Sched).

eval_wait({wait_exp, Exp}, Env, Cont, Store, Sched) ->
    New_cont = cont:wait_cont(Cont),
    eval(Exp, Env, New_cont, Store, Sched).

eval_signal({signal_exp, Exp}, Env, Cont, Store, Sched) ->
    New_cont = cont:signal_cont(Cont),
    eval(Exp, Env, New_cont, Store, Sched).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
eval_exps([], _Env, Cont, Store, Sched) ->
    Val = {list_val, []},
    cont:apply_cont(Cont, Val, Store, Sched);
eval_exps([Exp|Exps], Env, Cont, Store, Sched) ->
    New_cont = cont:exps_cont(Cont, Env, Exps, []),
    eval(Exp, Env, New_cont, Store, Sched).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
eval_script(Fn, Name, Ticks) ->
    Exp = thread_lang_parse:scan_and_parse_file(Fn),
    Store = store:init_store(Name),
    Env = env:empty_env(),
    Sched = scheduler:init_scheduler(Ticks),
    Cont = cont:end_mainthread_cont(),
    eval(Exp, Env, Cont, Store, Sched).
