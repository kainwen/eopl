-module(let_lang_parse).

-export([scan_and_parse/1, scan_and_parse_file/1]).

-export_type([exp/0, tokens/0, keywords/0]).

-type tokens() :: keywords()
                | {integer, integer()}
                | {id, atom()}.

-type keywords() :: 'if' | 'then' | 'else' | 'zero?' | 'let' | '=' | 'in'
                  | 'proc' | '-' | '(' | ')' | ','.

-type exp() :: {number_exp, integer()}
             | {var_exp, atom()}
             | {check_zero_exp, exp()}
             | {if_exp, exp(), exp(), exp()}
             | {diff_exp, exp(), exp()}
             | {let_exp, [{{var_exp, atom()}, exp()}], exp()}
             | {proc_exp, [{var_exp, atom()}], exp()}
             | {apply_exp, exp(), [exp()]}.


-spec parse_exp([tokens]) -> {exp(), [tokens]}.
parse_exp(Tks=[{id, _Id}|_Rem_toks]) ->
    parse_var(Tks);
parse_exp(Tks=[{integer, _N}|_Rem_toks]) ->
    parse_integer(Tks);
parse_exp(Tks=['if'|_Rem_toks]) ->
    parse_if(Tks);
parse_exp(Tks=['zero?'|_Rem_toks]) ->
    parse_check_zero(Tks);
parse_exp(Tks=['-','('|_Rem_toks]) ->
    parse_diff(Tks);
parse_exp(Tks=['let'|_Rem_toks]) ->
    parse_let(Tks);
parse_exp(Tks=['proc'|_Rem_toks]) ->
    parse_proc(Tks);
parse_exp(Tks=['('|_Rem_toks]) ->
    parse_apply(Tks).

%% internal parse helper functions
parse_var([{id, Id}|Rem_toks]) ->
    {{var_exp, Id}, Rem_toks}.

parse_integer([{integer, N}|Rem_toks]) ->
    {{number_exp, N}, Rem_toks}.

parse_if(Toks) ->
    Rem_toks = wait_for('if', Toks),
    {Question, R1} = parse_exp(Rem_toks),
    R2 = wait_for('then', R1),
    {Answer, R3} = parse_exp(R2),
    R4 = wait_for('else', R3),
    {Alternate, R5} = parse_exp(R4),
    {{if_exp, Question, Answer, Alternate}, R5}.

parse_check_zero(Tks) ->
    Rem_toks = wait_for('(', wait_for('zero?', Tks)),
    {E, R} = parse_exp(Rem_toks),
    R1 = wait_for(')', R),
    {{check_zero_exp, E}, R1}.

parse_diff(Tks) ->
    R = wait_for('(', wait_for('-', Tks)),
    {E1, R1} = parse_exp(R),
    R2 = wait_for(',', R1),
    {E2, R3} = parse_exp(R2),
    R4 = wait_for(')', R3),
    {{diff_exp, E1, E2}, R4}.

parse_let(Tks) ->
    R = wait_for('let', Tks),
    {BLs, R1} = parse_multiple(fun parse_binding/1, R, []),
    R2 = wait_for('in', R1),
    {Body, R3} = parse_exp(R2),
    {{let_exp, BLs, Body}, R3}.

parse_proc(Tks) ->
    R1 = wait_for('(', wait_for('proc', Tks)),
    {Parameters, R2} = parse_multiple(fun parse_var/1, R1, []),
    R3 = wait_for(')', R2),
    {Body, R4} = parse_exp(R3),
    {{proc_exp, Parameters, Body}, R4}.

parse_apply(Tks) ->
    R1 = wait_for('(', Tks),
    {Operator, R2} = parse_exp(R1),
    {Operands, R3} = parse_multiple(fun parse_exp/1, R2, []),
    R4 = wait_for(')', R3),
    {{apply_exp, Operator, Operands}, R4}.

%% internal helpers

parse_multiple(F, Toks, Result) ->
    try F(Toks) of
        {Var, R} -> parse_multiple(F, R, [Var|Result])
    catch
        _:_ -> {lists:reverse(Result), Toks}
    end.

parse_binding(Tks=[{id, _}|_R]) ->
    {Var, R1} = parse_var(Tks),
    R2 = wait_for('=', R1),
    {Exp, R3} = parse_exp(R2),
    {{Var, Exp}, R3}.

wait_for(Atom, [Atom|Toks]) -> Toks.


-spec parse([tokens]) -> exp().
parse(Toks) ->
    {E, _} = parse_exp(Toks),
    E.

-spec scan_and_parse(string()) -> exp().
scan_and_parse(Code) ->
    {ok, Toks, _} = let_lang_tok:string(Code),
    parse(Toks).

scan_and_parse_file(Fn) ->
    {ok, Data} = file:read_file(Fn),
    Code = binary_to_list(Data),
    scan_and_parse(Code).
