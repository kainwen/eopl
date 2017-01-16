-module(let_lang).

-include("common.hrl").

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

-export_type([expval/0]).

-type expval() :: {num_val, integer()}
                | {bool_val, boolean()}
                | {proc_val, [atom()], let_lang_parse:exp(), env:environment()}.


eval() ->
    Exp = reg:get_reg(exp),
    case Exp of
        {number, N} ->
            Val = {num_val, N},
            reg:set_reg(val, Val),
            cont:apply_cont();
        {var, V} ->
            Env = reg:get_reg(env),
            Val = env:apply_env(Env, V),
            reg:set_reg(val, Val),
            cont:apply_cont();
        {diff_exp, E1, E2} ->
            Env = reg:get_reg(env),
            Cont = reg:get_reg(cont),
            New_cont = cont:diff1_cont(Cont, Env, E2),
            reg:set_reg(cont, New_cont),
            reg:set_reg(exp, E1),
            eval();
        {test_zero_exp, E} ->
            Cont = reg:get_reg(cont),
            New_cont = cont:test_zero(Cont),
            reg:set_reg(cont, New_cont),
            reg:set_reg(exp, E),
            eval();
        {if_exp, Q, A, B} ->
            Cont = reg:get_reg(cont),
            Env = reg:get_reg(env),
            New_cont = cont:if_cont(Cont, Env, A, B),
            reg:set_reg(cont, New_cont),
            reg:set_reg(exp, Q),
            eval();
        {let_exp, Vars, Exps, Body} ->
            Cont = reg:get_reg(cont),
            Env = reg:get_reg(env),
            New_cont = cont:let_cont(Cont, Env, Vars, Body),
            reg:set_reg(cont, New_cont),
            reg:set_reg(exp, Exps),
            eval_exps_as_list();
        {proc_exp, Paras, Body} ->
            Env = reg:get_reg(env),
            Val = {proc_val, Paras, Body, Env},
            reg:set_reg(val, Val),
            cont:apply_cont();
        {apply_exp, Operator, Operands} ->
            Cont = reg:get_reg(cont),
            Env = reg:get_reg(env),
            New_cont = cont:apply1_cont(Cont, Operands, Env),
            reg:set_reg(exp, Operator),
            reg:set_reg(cont, New_cont),
            eval();
        {letrec_exp, Proc_names, Proc_bodys, Body} ->
            Cont = reg:get_reg(cont),
            Env = reg:get_reg(env),
            New_cont = cont:letrec_cont(Cont, Env, Proc_names, Body),
            reg:set_reg(exp, Proc_bodys),
            reg:set_reg(cont, New_cont),
            eval_exps_as_list()
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init() ->
    ets:new(?ETS_TAB, [named_table, set, {keypos, #machine.id}]),
    New_machine = #machine{id=?ID},
    ets:insert(?ETS_TAB, New_machine),
    ok.


eval_program(Fn) ->
    init(),
    Exp = let_lang_parse:scan_and_parse_file(Fn),
    Env = env:empty_env(),
    Cont = cont:end_cont(),
    reg:set_reg(exp, Exp),
    reg:set_reg(env, Env),
    reg:set_reg(cont, Cont),
    eval().

eval_exps_as_list() ->
    Exps = reg:get_reg(exp),
    case Exps of
        [] ->
            reg:set_reg(val, []),
            cont:apply_cont();
        [Exp|Rem_exps] ->
            Cont = reg:get_reg(cont),
            Env = reg:get_reg(env),
            New_cont = cont:eval_exps_as_list_cont(Cont, Env, Rem_exps, []),
            reg:set_reg(exp, Exp),
            reg:set_reg(cont, New_cont),
            eval()
    end.
