-module(cont).

-compile(export_all).


end_cont() ->
    fun (Val) ->
            io:format("end of the computation.~n"),
            Val
    end.

diff1_cont(Cont, Env, E2) ->
    fun (Val) ->
            New_cont = diff2_cont(Cont, Val),
            let_lang:eval(E2, Env, New_cont)
    end.

diff2_cont(Cont, Val1) ->
    fun (Val2) ->
            {num_val, N1} = Val1,
            {num_val, N2} = Val2,
            Val = {num_val, N1 - N2},
            apply_cont(Cont, Val)
    end.

if_cont(Cont, Env, A, B) ->
    fun (Val) ->
            {bool_val, Bool} = Val,
            Branch = case Bool of
                         true -> A;
                         false -> B
                     end,
            let_lang:eval(Branch, Env, Cont)
    end.

test_zero(Cont) ->
    fun (Val) ->
            {num_val, N} = Val,
            Result = {bool_val, N =:= 0},
            apply_cont(Cont, Result)
    end.

let_cont(Cont, Env, Vars, Body) ->
    fun (Vals) ->
            New_env = env:extend_env_by_list(Env, lists:zip(Vars, Vals)),
            let_lang:eval(Body, New_env, Cont)
    end.

apply1_cont(Cont, Operands, Env) ->
    fun (Proc) ->
            New_cont = apply2_cont(Cont, Proc),
            let_lang:eval_multiple_exps_as_list(Operands, Env, New_cont)
    end.

apply2_cont(Cont, Proc) ->
    fun (Args) ->
            {proc_val, Paras, Body, Lex_env} = Proc,
            New_env = env:extend_env_by_list(Lex_env, lists:zip(Paras, Args)),
            let_lang:eval(Body, New_env, Cont)
    end.

letrec_cont(Cont, Env, Proc_names, Body) ->
    fun (Procs) ->
            Proc_name_and_def = lists:zip(Proc_names, Procs),
            Name_procs = [{Proc_name, {Paras, Proc_body}}
                          || {Proc_name, {proc_val, Paras, Proc_body, _}} <- Proc_name_and_def],
            New_env = env:extend_env_rec(Env, Name_procs),
            let_lang:eval(Body, New_env, Cont)
    end.

multiple_exps_as_list_cont(Cont, _Env, [], Acc_list) ->
    fun (Val) ->
            Val_list = lists:reverse([Val|Acc_list]),
            apply_cont(Cont, Val_list)
    end;
multiple_exps_as_list_cont(Cont, Env, [Exp|Exps], Acc_list) ->
    fun (Val) ->
            New_acc_list = [Val|Acc_list],
            New_cont = multiple_exps_as_list_cont(Cont, Env, Exps, New_acc_list),
            let_lang:eval(Exp, Env, New_cont)
    end.


apply_cont(Cont, Val) ->
    Cont(Val).
