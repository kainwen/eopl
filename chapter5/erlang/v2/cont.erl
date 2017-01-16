-module(cont).

-include("common.hrl").

-compile(export_all).


end_cont() ->
    fun () ->
            io:format("end of the computation.~n"),
            reg:get_reg(val)
    end.

diff1_cont(Cont, Env, E2) ->
    fun () ->
            Val = reg:get_reg(val),
            New_cont = diff2_cont(Cont, Val),
            reg:set_reg(cont, New_cont),
            reg:set_reg(env, Env),
            reg:set_reg(exp, E2),
            let_lang:eval()
    end.

diff2_cont(Cont, Val1) ->
    fun () ->
            {num_val, N1} = Val1,
            {num_val, N2} = reg:get_reg(val),
            Val = {num_val, N1 - N2},
            reg:set_reg(cont, Cont),
            reg:set_reg(val, Val),
            apply_cont()
    end.

if_cont(Cont, Env, A, B) ->
    fun () ->
            {bool_val, Bool} = reg:get_reg(val),
            Branch = case Bool of
                         true -> A;
                         false -> B
                     end,
            reg:set_reg(exp, Branch),
            reg:set_reg(env, Env),
            reg:set_reg(cont, Cont),
            let_lang:eval()
    end.

test_zero(Cont) ->
    fun () ->
            {num_val, N} = reg:get_reg(val),
            Result = {bool_val, N =:= 0},
            reg:set_reg(cont, Cont),
            reg:set_reg(val, Result),
            apply_cont()
    end.

let_cont(Cont, Env, Vars, Body) ->
    fun () ->
            Vals = reg:get_reg(val),
            New_env = env:extend_env_by_list(Env, lists:zip(Vars, Vals)),
            reg:set_reg(env, New_env),
            reg:set_reg(exp, Body),
            reg:set_reg(cont, Cont),
            let_lang:eval()
    end.

apply1_cont(Cont, Operands, Env) ->
    fun () ->
            Proc = reg:get_reg(val),
            New_cont = apply2_cont(Cont, Proc),
            reg:set_reg(cont, New_cont),
            reg:set_reg(exp, Operands),
            reg:set_reg(env, Env),
            let_lang:eval_exps_as_list()
    end.

apply2_cont(Cont, Proc) ->
    fun () ->
            Args = reg:get_reg(val),
            {proc_val, Paras, Body, Lex_env} = Proc,
            New_env = env:extend_env_by_list(Lex_env, lists:zip(Paras, Args)),
            reg:set_reg(env, New_env),
            reg:set_reg(exp, Body),
            reg:set_reg(cont, Cont),
            let_lang:eval()
    end.

letrec_cont(Cont, Env, Proc_names, Body) ->
    fun () ->
            Procs = reg:get_reg(val),
            Proc_name_and_def = lists:zip(Proc_names, Procs),
            Name_procs = [{Proc_name, {Paras, Proc_body}}
                          || {Proc_name, {proc_val, Paras, Proc_body, _}} <- Proc_name_and_def],
            New_env = env:extend_env_rec(Env, Name_procs),
            reg:set_reg(env, New_env),
            reg:set_reg(cont, Cont),
            reg:set_reg(exp, Body),
            let_lang:eval()
    end.

eval_exps_as_list_cont(Cont, _Env, [], Acc_list) ->
    fun () ->
            Val = reg:get_reg(val),
            Val_list = lists:reverse([Val|Acc_list]),
            reg:set_reg(val, Val_list),
            reg:set_reg(cont, Cont),
            apply_cont()
    end;
eval_exps_as_list_cont(Cont, Env, [Exp|Exps], Acc_list) ->
    fun () ->
            Val = reg:get_reg(val),
            New_acc_list = [Val|Acc_list],
            New_cont = eval_exps_as_list_cont(Cont, Env, Exps, New_acc_list),
            reg:set_reg(cont, New_cont),
            reg:set_reg(env, Env),
            reg:set_reg(exp, Exp),
            let_lang:eval()
    end.


apply_cont() ->
    Cont = reg:get_reg(cont),
    Cont().
