-module(let_lang).

-include_lib("eunit/include/eunit.hrl").

-export([eval_script/1, eval/3, eval_multiple_exps_as_list/3]).

-export_type([expval/0]).

-type expval() :: {num_val, integer()}
                | {bool_val, boolean()}
                | {proc_val, [atom()], let_lang_parse:exp(), env:environment()}.

-spec eval(let_lang_parse:exp(), env:environment(), cont:continuation()) -> expval().
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
    eval_letrec(Exp, Env, Cont).


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
    New_cont = cont:test_zero(Cont),
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
     eval_script("code/code3") =:= {num_val, 1}
    ].
