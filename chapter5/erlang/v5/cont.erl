-module(cont).

-export([
         end_mainthread_cont/0, end_subthread_cont/0,
         diff1_cont/3, if_cont/4, test_zero_cont/1,
         exps_cont/4, let_cont/4, apply1_cont/3, letrec_cont/4, list_cont/1,
         car_cont/1, cdr_cont/1, test_null_cont/1, assign_cont/3, block_cont/3,
         print_cont/1, spawn_cont/1, wait_cont/1, signal_cont/1,
         apply_cont/4
        ]).

-export_type([cont/0]).

-type exp() :: thread_lang_parse:exp().
-type env() :: env:env().
-type val() :: thread_lang:expval().
-type var() :: atom().
-type store() :: store:store().
-type sched() :: scheduler:scheduler().

-type cont() :: {end_mainthread_cont}
              | {end_subthread_cont}
              | {diff1_cont, C::cont(), E::exp(), Env::env()}
              | {diff2_cont, C::cont(), V1::val()}
              | {if_cont, C::cont(), Env::env(), B::exp(), C::exp()}
              | {test_zero_cont, C::cont()}
              | {let_cont, C::cont(), Env::env(), Body::exp(), Vars::[var()]}
              | {exps_cont, C::cont(), Env::env(), Exps::[exp()], Acc::[val()]}
              | {apply1_cont, C::cont(), Env::env(), Operands::[exp()]}
              | {apply2_cont, C::cont(), Proc_val::exp()}
              | {letrec_cont, C::cont(), Proc_names::[atom()], Body::exp(), Env::env()}
              | {list_cont, C::cont()}
              | {car_cont, C::cont()}
              | {cdr_cont, C::cont()}
              | {test_null_cont, C::cont()}
              | {assign_cont, C::cont(), V::var(), Env::env()}
              | {block_cont, C::cont(), Env::env(), Exps::[exp()]}
              | {print_cont, C::cont()}
              | {spawn_cont, C::cont()}
              | {wait_cont, C::cont()}
              | {signal_cont, C::cont()}.

-spec end_mainthread_cont() -> cont().
end_mainthread_cont() ->
    {end_mainthread_cont}.

-spec end_subthread_cont() -> cont().
end_subthread_cont() ->
    {end_subthread_cont}.

-spec diff1_cont(cont(), exp(), env()) -> cont().
diff1_cont(Cont, E2, Env) ->
    {diff1_cont, Cont, E2, Env}.

-spec diff2_cont(cont(), val()) -> cont().
diff2_cont(Cont, Val1) ->
    {diff2_cont, Cont, Val1}.

-spec if_cont(cont(), env(), exp(), exp()) -> cont().
if_cont(Cont, Env, B, C) ->
    {if_cont, Cont, Env, B, C}.

-spec test_zero_cont(cont()) -> cont().
test_zero_cont(Cont) ->
    {test_zero_cont, Cont}.

-spec let_cont(cont(), env(), exp(), [var()]) -> cont().
let_cont(Cont, Env, Body, Vars) ->
    {let_cont, Cont, Env, Body, Vars}.

-spec apply1_cont(cont(), env(), [exp()]) -> cont().
apply1_cont(Cont, Env, Operands) ->
    {apply1_cont, Cont, Env, Operands}.

-spec apply2_cont(cont(), val()) -> cont().
apply2_cont(Cont, Proc_val) ->
    {apply2_cont, Cont, Proc_val}.

-spec letrec_cont(cont(), [atom()], exp(), env()) -> cont().
letrec_cont(Cont, Proc_names, Body, Env) ->
    {letrec_cont, Cont, Proc_names, Body, Env}.

-spec list_cont(cont()) -> cont().
list_cont(Cont) ->
    {list_cont, Cont}.

-spec car_cont(cont()) -> cont().
car_cont(Cont) ->
    {car_cont, Cont}.

-spec cdr_cont(cont()) -> cont().
cdr_cont(Cont) ->
    {cdr_cont, Cont}.

-spec test_null_cont(cont()) -> cont().
test_null_cont(Cont) ->
    {test_null_cont, Cont}.

-spec assign_cont(cont(), atom(), env()) -> cont().
assign_cont(Cont, V, Env) ->
    {assign_cont, Cont, V, Env}.

-spec block_cont(cont(), env(), [exp()]) -> cont().
block_cont(Cont, Env, Exps) ->
    {block_cont, Cont, Env, Exps}.

-spec print_cont(cont()) -> cont().
print_cont(Cont) ->
    {print_cont, Cont}.

-spec spawn_cont(cont()) -> cont().
spawn_cont(Cont) ->
    {spawn_cont, Cont}.

-spec wait_cont(cont()) -> cont().
wait_cont(Cont) ->
    {wait_cont, Cont}.

-spec signal_cont(cont()) -> cont().
signal_cont(Cont) ->
    {signal_cont, Cont}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec exps_cont(cont(), env(), [exp()], [val()]) -> cont().
exps_cont(Cont, Env, Exps, Acc_list) ->
    {exps_cont, Cont, Env, Exps, Acc_list}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec apply_cont(cont(), val(), store(), sched()) -> val().
apply_cont(Cont, Val, Store, Sched) ->
    case scheduler:is_time_expired(Sched) of
        true ->
            Thread = {intermid, Cont, Val, Store, Sched},
            ok = scheduler:place_on_ready_queue(Sched, Thread),
            scheduler:run_next_thread(Sched);
        false ->
            ok = scheduler:decrement_time(Sched),
            case Cont of
                {end_mainthread_cont} ->
                    ok = scheduler:set_final_answer(Sched, Val),
                    scheduler:run_next_thread(Sched);
                {end_subthread_cont} ->
                    scheduler:run_next_thread(Sched);
                {diff1_cont, Saved_cont, E2, Env} ->
                    New_cont = diff2_cont(Saved_cont, Val),
                    thread_lang:eval(E2, Env, New_cont, Store, Sched);
                {diff2_cont, Saved_cont, Val1} ->
                    {num_val, N1} = Val1,
                    {num_val, N2} = Val,
                    Result = {num_val, N1 - N2},
                    apply_cont(Saved_cont, Result, Store, Sched);
                {if_cont, Saved_cont, Env, B, C} ->
                    Branch = case Val of
                                 {bool_val, true} -> B;
                                 {bool_val, false} -> C
                             end,
                    thread_lang:eval(Branch, Env, Saved_cont, Store, Sched);
                {test_zero_cont, Saved_cont} ->
                    {num_val, N} = Val,
                    Result = case N of
                                 0 -> {bool_val, true};
                                 _ -> {bool_val, false}
                             end,
                    apply_cont(Saved_cont, Result, Store, Sched);
                {let_cont, Saved_cont, Env, Body, Vars} ->
					{list_val, Vals} = Val,
                    Var_vals = lists:zip(Vars, Vals),
                    Var_refs = [{Variable, store:newref(Store, Value)}
                                || {Variable, Value} <- Var_vals],
                    New_env = env:extend_env(Var_refs, Env),
                    thread_lang:eval(Body, New_env, Saved_cont, Store, Sched);
                {exps_cont, Saved_cont, _Env, [], Acc_list} ->
                    Result = {list_val, lists:reverse([Val|Acc_list])},
                    apply_cont(Saved_cont, Result, Store, Sched);
                {exps_cont, Saved_cont, Env, [Exp|Exps], Acc_list} ->
                    New_acc_list = [Val|Acc_list],
                    New_cont =exps_cont(Saved_cont, Env, Exps, New_acc_list),
                    thread_lang:eval(Exp, Env, New_cont, Store, Sched);
                {apply1_cont, Saved_cont, Env, Operands} ->
                    New_cont = apply2_cont(Saved_cont, get_proc(Val, Env)),
                    thread_lang:eval_exps(Operands, Env, New_cont, Store, Sched);
                {apply2_cont, Saved_cont, Proc_val} ->
                    {list_val, Args} = Val,
                    {proc_val, Paras, Body, Lex_env} = Proc_val,
                    Paras_args = lists:zip(Paras, Args),
                    Var_refs = [{Variable, store:newref(Store, Value)}
                                || {Variable, Value} <- Paras_args],
                    New_env = env:extend_env(Var_refs, Lex_env),
                    thread_lang:eval(Body, New_env, Saved_cont, Store, Sched);
                {letrec_cont, Saved_cont, Proc_names, Body, Env} ->
                    {list_val, Proc_defs} = Val,
                    Reformed_proc_defs = [{Paras, Proc_body}
                                          || {proc_val, Paras, Proc_body, _E} <- Proc_defs],
                    Var_vals = lists:zip(Proc_names, Reformed_proc_defs),
                    Var_refs = [{Variable, store:newref(Store, Value)}
                                || {Variable, Value} <- Var_vals],
                    New_env = env:extend_env(Var_refs, Env),
                    thread_lang:eval(Body, New_env, Saved_cont, Store, Sched);
                {list_cont, Saved_cont} ->
                    apply_cont(Saved_cont, Val, Store, Sched);
                {car_cont, Saved_cont} ->
                    {list_val, [A|_R]} = Val,
                    apply_cont(Saved_cont, A, Store, Sched);
                {cdr_cont, Saved_cont} ->
                    {list_val, [_|R]} = Val,
                    apply_cont(Saved_cont, {list_val, R}, Store, Sched);
                {test_null_cont, Saved_cont} ->
                    {list_val, L} = Val,
                    Bool = case L of
                               [] -> true;
                               _ -> false
                           end,
                    apply_cont(Saved_cont, {bool_val, Bool}, Store, Sched);
                {assign_cont, Saved_cont, V, Env} ->
                    Ref = env:apply_env(Env, V),
                    Result = store:setref(Store, Ref, Val),
                    apply_cont(Saved_cont, Result, Store, Sched);
                {block_cont, Saved_cont, Env, Exps} ->
                    thread_lang:eval({block_exp, Exps},
                                     Env, Saved_cont, Store, Sched);
                {print_cont, Saved_cont} ->
                    print_val(Val),
                    apply_cont(Saved_cont, {num_val, 7}, Store, Sched);
                {spawn_cont, Saved_cont} ->
                    Thread = {just_spawn, Val, Store, Sched},
                    ok = scheduler:place_on_ready_queue(Sched, Thread),
                    apply_cont(Saved_cont, {num_val, 29}, Store, Sched);
                {wait_cont, Saved_cont} ->
                    {mutex_val, Mutex} = Val,
                    Thread = {intermid, Saved_cont, {num_val, 53}, Store, Sched},
                    mutex:wait_for_mutex(Store, Sched, Mutex, Thread);
                {signal_cont, Saved_cont} ->
                    {mutex_val, Mutex} = Val,
                    Thread = {intermid, Saved_cont, {num_val, 53}, Store, Sched},
                    mutex:signal_mutex(Store, Sched, Mutex, Thread)
            end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_proc(P={proc_val, _Paras, _Body, _Env}, _E) -> P;
get_proc({Paras, Body}, Env) ->
    {proc_val, Paras, Body, Env}.

print_val(V) ->
    io:format("<~p>~n", [V]).
