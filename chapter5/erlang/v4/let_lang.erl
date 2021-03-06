-module(let_lang).

-include_lib("eunit/include/eunit.hrl").

-export([eval_script/1, eval/3, eval_multiple_exps_as_list/3]).

-export_type([expval/0]).

-type expval() :: {num_val, integer()}
                | {bool_val, boolean()}
                | {proc_val, [atom()], let_lang_parse:exp(), env:environment()}
                | {list_val, [expval()]}
                | {cont_val, cont:cont()}.

-spec eval(let_lang_parse:exp(), env:environment(), cont:cont()) -> expval().
eval(Exp={number, _}, Env, Cont) ->
    eval_number(Exp, Env, Cont);
eval(Exp={var, _}, Env, Cont) ->
    eval_var(Exp, Env, Cont);
eval(Exp={diff_exp, _, _}, Env, Cont) ->
    eval_diff(Exp, Env, Cont);
eval(Exp={if_exp, _, _, _}, Env, Cont) ->
    eval_if(Exp, Env, Cont);
eval(Exp={test_zero_exp, _}, Env, Cont) ->
    eval_test_zero(Exp, Env, Cont);
eval(Exp={let_exp, _, _, _}, Env, Cont) ->
    eval_let(Exp, Env, Cont);
eval(Exp={proc_exp, _, _}, Env, Cont) ->
    eval_proc(Exp, Env, Cont);
eval(Exp={apply_exp, _, _}, Env, Cont) ->
    eval_apply(Exp, Env, Cont);
eval(Exp={letrec_exp, _, _, _}, Env, Cont) ->
    eval_letrec(Exp, Env, Cont);
eval(Exp={list_exp, _}, Env, Cont) ->
    eval_list(Exp, Env, Cont);
eval(Exp={try_exp, _, _, _, _}, Env, Cont) ->
    eval_try(Exp, Env, Cont);
eval(Exp={raise_exp, _}, Env, Cont) ->
    eval_raise(Exp, Env, Cont);
eval(Exp={car_exp, _}, Env, Cont) ->
    eval_car(Exp, Env, Cont);
eval(Exp={cdr_exp, _}, Env, Cont) ->
    eval_cdr(Exp, Env, Cont);
eval(Exp={cons_exp, _, _}, Env, Cont) ->
    eval_cons(Exp, Env, Cont);
eval(Exp={test_null_exp, _}, Env, Cont) ->
    eval_test_null(Exp, Env, Cont);
eval(Exp={invoke_cont_exp, _, _}, Env, Cont) ->
    eval_invoke_cont(Exp, Env, Cont);
eval(Exp={letcc_exp, _, _}, Env, Cont) ->
    eval_letcc(Exp, Env, Cont);
eval(Exp={throw_exp, _, _}, Env, Cont) ->
    eval_throw(Exp, Env, Cont).


%% handlers
eval_number({number, N}, _Env, Cont) ->
    Val = {num_val, N},
    cont:apply_cont(Cont, Val).

eval_var({var, V}, Env, Cont) ->
    Val = env:apply_env(Env, V),
    cont:apply_cont(Cont, Val).

eval_diff({diff_exp, E1, E2}, Env, Cont) ->
    New_cont = cont:diff1_cont(Cont, Env, E2),
    eval(E1, Env, New_cont).

eval_if({if_exp, Q, A, B}, Env, Cont) ->
    New_cont = cont:if_cont(Cont, Env, A, B),
    eval(Q, Env, New_cont).

eval_test_zero({test_zero_exp, Exp}, Env, Cont) ->
    New_cont = cont:test_zero_cont(Cont),
    eval(Exp, Env, New_cont).

eval_let({let_exp, Vars, Exps, Body}, Env, Cont) ->
    New_cont = cont:let_cont(Cont, Env, Vars, Body),
    eval_multiple_exps_as_list(Exps, Env, New_cont).

eval_proc({proc_exp, Paras, Body}, Env, Cont) ->
    Val = {proc_val, Paras, Body, Env},
    cont:apply_cont(Cont, Val).

eval_apply({apply_exp, Operator, Operands}, Env, Cont) ->
    New_cont = cont:apply1_cont(Cont, Operands, Env),
    eval(Operator, Env, New_cont).

eval_letrec({letrec_exp, Proc_names, Procs, Body}, Env, Cont) ->
    New_cont = cont:letrec_cont(Cont, Env, Proc_names, Body),
    eval_multiple_exps_as_list(Procs, Env, New_cont).

eval_list({list_exp, Exps}, Env, Cont) ->
    New_cont = cont:list_cont(Cont),
    eval_multiple_exps_as_list(Exps, Env, New_cont).

eval_try({try_exp, Exp, V, C, Handler_exp}, Env, Cont) ->
    New_cont = cont:try_cont(Cont, Env, V, C, Handler_exp),
    eval(Exp, Env, New_cont).

eval_raise({raise_exp, Exp}, Env, Cont) ->
    New_cont = cont:raise_cont(Cont),
    eval(Exp, Env, New_cont).

eval_car({car_exp, Exp}, Env, Cont) ->
    New_cont = cont:car_cont(Cont),
    eval(Exp, Env, New_cont).

eval_cdr({cdr_exp, Exp}, Env, Cont) ->
    New_cont = cont:cdr_cont(Cont),
    eval(Exp, Env, New_cont).

eval_cons({cons_exp, E1, E2}, Env, Cont) ->
    New_cont = cont:cons1_cont(Cont, Env, E2),
    eval(E1, Env, New_cont).

eval_test_null({test_null_exp, Exp}, Env, Cont) ->
    New_cont = cont:test_null_cont(Cont),
    eval(Exp, Env, New_cont).

eval_invoke_cont({invoke_cont_exp, E1, E2}, Env, Cont) ->
    New_cont = cont:invoke1_cont(Cont, Env, E2),
    eval(E1, Env, New_cont).

eval_letcc({letcc_exp, V, Exp}, Env, Cont) ->
    New_env = env:extend_env(V, {cont_val, Cont}, Env),
    eval(Exp, New_env, Cont).

eval_throw({throw_exp, Exp1, Exp2}, Env, Cont) ->
    New_cont = cont:throw1_cont(Cont, Exp2, Env),
    eval(Exp1, Env, New_cont).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

eval_multiple_exps_as_list([], _Env, Cont) ->
% Cont is a function wait_for A list of expvals
    Val = [],
    cont:apply_cont(Cont, Val);
eval_multiple_exps_as_list([Exp|Exps], Env, Cont) ->
    New_cont = cont:multiple_exps_as_list_cont(Cont, Env, Exps, []),
    eval(Exp, Env, New_cont).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

eval_script(Fn) ->
    Exp = let_lang_parse:scan_and_parse_file(Fn),
    eval(Exp, env:empty_env(), cont:end_cont()).

eval_test() ->
    [
     eval_script("code/code0") =:= {num_val, -100},
     eval_script("code/code1") =:= {num_val, 12},
     eval_script("code/code2") =:= {num_val, 12},
     eval_script("code/code3") =:= {num_val, 1},
     eval_script("code/code4") =:= {list_val,[{num_val,1},{num_val,2},{num_val,3}]},
     eval_script("code/code5") =:= {num_val,100},
     eval_script("code/code6") =:= {num_val,-1},
     eval_script("code/code7") =:= {num_val,-1},
     eval_script("code/code8") =:= {num_val, 128},
     eval_script("code/code9") =:= {num_val, 101},
     eval_script("code/code10") =:= {num_val, -22},
     eval_script("code/code11") =:= {num_val, 8}
    ].
