-module(let_lang_parse).

-export([scan_and_parse_file/1]).

-export_type([exp/0]).

-type exp() :: {number, integer()}
             | {var, atom()}
             | {diff_exp, exp(), exp()}
             | {if_exp, exp(), exp(), exp()}
             | {test_zero_exp, exp()}
             | {let_exp, [atom()], [exp()], exp()}
             | {proc_exp, [atom()], exp()}
             | {apply_exp, exp(), [exp()]}
             | {letrec_exp, [atom()], [exp()], exp()}.

-type token() :: keywords()
                | {integer, integer()}
                | {id, atom()}.

-type keywords() :: 'if' | 'then' | 'else' | 'zero?' | 'let' | '=' | 'in'
                  | 'proc' | 'letrec'
                  | '-' | '(' | ')' | ','.

-spec parse([token()]) -> {exp(), [token()]}.
parse(Toks=[{integer, _}|_Rem_toks]) ->
    parse_number(Toks);
parse(Toks=[{id, _}|_Rem_toks]) ->
    parse_var(Toks);
parse(Toks=['-'|_Rem_toks]) ->
    parse_diff(Toks);
parse(Toks=['zero?'|_Rem_toks]) ->
    parse_test_zero(Toks);
parse(Toks=['if'|_Rem_toks]) ->
    parse_if(Toks);
parse(Toks=['proc'|_Rem_toks]) ->
    parse_proc(Toks);
parse(Toks=['('|_Rem_toks]) ->
    parse_apply(Toks);
parse(Toks=['let'|_Rem_toks]) ->
    parse_let(Toks);
parse(Toks=['letrec'|_Rem_toks]) ->
    parse_letrec(Toks).


%% handlers
parse_number([{integer, N}|R]) ->
    {{number, N}, R}.

parse_var([{id, V}|R]) ->
    {{var, V}, R}.

parse_diff(['-'|R]) ->
    R1 = wait_for('(', R),
    {Exp1, R2} = parse(R1),
    R3 = wait_for(',', R2),
    {Exp2, R4} = parse(R3),
    R5 = wait_for(')', R4),
    {{diff_exp, Exp1, Exp2}, R5}.

parse_test_zero(['zero?'|R]) ->
    R1 = wait_for('(', R),
    {Exp, R2} = parse(R1),
    R3 = wait_for(')', R2),
    {{test_zero_exp, Exp}, R3}.

parse_if(['if'|R]) ->
    {Question, R1} = parse(R),
    R2 = wait_for('then', R1),
    {Answer, R3} = parse(R2),
    R4 = wait_for('else', R3),
    {Alternate, R5} = parse(R4),
    {{if_exp, Question, Answer, Alternate}, R5}.

parse_proc(['proc'|R]) ->
    R1 = wait_for('(', R),
    {Paras, R2} = parse_multiple_with_delim(fun parse_var/1, R1, ','),
    R3 = wait_for(')', R2),
    {Body, R4} = parse(R3),
    {{proc_exp, [V || {var, V} <- Paras], Body}, R4}.

parse_apply(['('|R]) ->
    {Operator, R1} = parse(R),
    {Operands, R2} = parse_multiple_with_delim(fun parse/1, R1, ''),
    R3 = wait_for(')', R2),
    {{apply_exp, Operator, Operands}, R3}.

parse_let(['let'|R]) ->
    {Bindings, R1} = parse_multiple_with_delim(fun parse_binding/1, R, ''),
    R2 = wait_for('in', R1),
    {Body, R3} = parse(R2),
    Vars = [Var ||{Var, _} <- Bindings],
    Exps = [Exp ||{_, Exp} <- Bindings],
    {{let_exp, Vars, Exps, Body}, R3}.

parse_letrec(['letrec'|R]) ->
    {Proc_defs, R1} = parse_multiple_with_delim(fun parse_proc_def/1, R, ''),
    R2 = wait_for('in', R1),
    {Body, R3} = parse(R2),
    Proc_names = [Proc_name || {Proc_name, _} <- Proc_defs],
    Proc_bodys = [Proc_body || {_, Proc_body} <- Proc_defs],
    {{letrec_exp, Proc_names, Proc_bodys, Body}, R3}.

%% internal helper functions
wait_for(Atom, [Atom|Toks]) -> Toks.

parse_proc_def(Toks) -> % f(x) = -(x, 1)
    {{var, V}, R1} = parse_var(Toks),
    R2 = wait_for('(', R1),
    {Paras, R3} = parse_multiple_with_delim(fun parse_var/1, R2, ''),
    R4 = wait_for('=', wait_for(')', R3)),
    {Proc_body, R5} = parse(R4),
    {{V, {proc_exp, [P || {var, P} <- Paras], Proc_body}}, R5}.

parse_binding(Toks) ->
    {{var, V}, R1} = parse_var(Toks),
    R2 = wait_for('=', R1),
    {Exp, R3} = parse(R2),
    {{V, Exp}, R3}.

parse_multiple_with_delim(Fun, Toks, Delim) ->
    parse_multiple_with_delim_helper(Fun, Toks, Delim, []).

parse_multiple_with_delim_helper(Fun, [Delim|Toks], Delim, Acc_list) ->
    parse_multiple_with_delim_helper(Fun, Toks, Delim, Acc_list);
parse_multiple_with_delim_helper(Fun, Toks, Delim, Acc_list) ->
    try Fun(Toks) of
        {Term, R} -> parse_multiple_with_delim_helper(Fun, R, Delim, [Term|Acc_list])
    catch
        _:_ -> {lists:reverse(Acc_list), Toks}
    end.

%% APIs
-spec scan_and_parse(string()) -> exp().
scan_and_parse(Code) ->
    {ok, Toks, _} = let_lang_tok:string(Code),
	{Exp, []} = parse(Toks),
	Exp.

scan_and_parse_file(Fn) ->
    {ok, Data} = file:read_file(Fn),
    Code = binary_to_list(Data),
    scan_and_parse(Code).
