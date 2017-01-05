-module(let_lang_parse).

-export([scan_and_parse/1, scan_and_parse_file/1]).

-export_type([abstract_syntax_tree/0, tokens/0, keywords/0]).

-type tokens() :: keywords()
                | {integer, integer()}
                | {id, atom()}.

-type keywords() :: 'if' | 'then' | 'else' | 'zero?' | 'let' | '=' | 'in'
                  | 'cons' | 'unpack' | 'proc' | 'letrec' | 'set' | 'begin'
				  | 'end' | ';' 
                  | '-' | '(' | ')' | ','.

-type var() :: {var_exp, atom()}.
-type var_list() :: [var()].

-type abstract_syntax_tree() :: {number_exp, integer()}
                              | {var_exp, atom()}
                              | {check_zero_exp, abstract_syntax_tree()}
                              | {if_exp, abstract_syntax_tree(), abstract_syntax_tree(), abstract_syntax_tree()}
                              | {diff_exp, abstract_syntax_tree(), abstract_syntax_tree()}
                              | {let_exp, [{{var_exp, atom()}, abstract_syntax_tree()}], abstract_syntax_tree()}
                              | {unpack_exp, [{var_exp, atom()}], abstract_syntax_tree(), abstract_syntax_tree()}
                              | {emptylist}
                              | {cons_exp, abstract_syntax_tree(), abstract_syntax_tree()}
                              | {proc_exp, [{var_exp, atom()}], abstract_syntax_tree()}
                              | {apply_exp, abstract_syntax_tree(), [abstract_syntax_tree()]}
                              | {letrec_exp, [{var(), var_list(), abstract_syntax_tree()}], abstract_syntax_tree()}
                              | {seq_exps, [abstract_syntax_tree()]}
                              | {assign_exp, var(), abstract_syntax_tree()}.




-spec parse_exp([tokens]) -> {abstract_syntax_tree(), [tokens]}.
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
parse_exp(Tks=['unpack'|_Rem_toks]) ->
    parse_unpack(Tks);
parse_exp(Tks=['emptylist'|_Rem_toks]) ->
    parse_emptylist(Tks);
parse_exp(Tks=['cons'|_Rem_toks]) ->
    parse_cons(Tks);
parse_exp(Tks=['proc'|_Rem_toks]) ->
    parse_proc(Tks);
parse_exp(Tks=['('|_Rem_toks]) ->
    parse_apply(Tks);
parse_exp(Tks=['letrec'|_Rem_toks]) ->
    parse_letrec(Tks);
parse_exp(Tks=['begin'|_Rem_toks]) ->
    parse_seq(Tks);
parse_exp(Tks=['set'|_Rem_toks]) ->
    parse_assign(Tks).


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

parse_unpack(Tks) ->
    R = wait_for('unpack', Tks),
    {Vars, R1} = parse_multiple(fun parse_var/1, R, []),
    R2 = wait_for('=', R1),
    {Exp, R3} = parse_exp(R2),
    R4 = wait_for('in', R3),
    {Body, R5} = parse_exp(R4),
    {{unpack_exp, Vars, Exp, Body}, R5}.

parse_emptylist(['emptylist'|Rem_toks]) ->
    {{emptylist}, Rem_toks}.

parse_cons(Tks) ->
    R1 = wait_for('(', wait_for('cons', Tks)),
    {E1, R2} = parse_exp(R1),
    R3 = wait_for(',', R2),
    {E2, R4} = parse_exp(R3),
    R5 = wait_for(')', R4),
    {{cons_exp, E1, E2}, R5}.

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

parse_letrec(Tks) ->
    R1 = wait_for('letrec', Tks),
    {Proc_binding_list, R2} = parse_multiple(fun parse_proc_binding/1, R1, []),
    R3 = wait_for('in', R2),
    {Body, R4} = parse_exp(R3),
    {{letrec_exp, Proc_binding_list, Body}, R4}.

parse_seq(Tks) ->
    R1 = wait_for('begin', Tks),
    {Exps, R2} = parse_multiple(fun parse_sub_exp/1, R1, []),
    R3 = wait_for('end', R2),
    {{seq_exps, Exps}, R3}.

parse_assign(Tks) ->
    R1 = wait_for('set', Tks),
    {Var, R2} = parse_var(R1),
    R3 = wait_for('=', R2),
    {Exp, R4} = parse_exp(R3),
    {{assign_exp, Var, Exp}, R4}.

%% internal helpers
parse_sub_exp(['end'|_Toks]) ->
    erlang:error(meet_end);
parse_sub_exp([';'|Toks]) ->
    parse_sub_exp(Toks);
parse_sub_exp(Toks) ->
    parse_exp(Toks).

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

parse_proc_binding(Toks) ->
    {Var, R1} = parse_var(Toks),
    R2 = wait_for('(', R1),
    {Paras, R3} = parse_multiple(fun parse_var/1, R2, []),
    R4 = wait_for('=', wait_for(')', R3)),
    {Body, R5} = parse_exp(R4),
    {{Var, Paras, Body}, R5}.

wait_for(Atom, [Atom|Toks]) -> Toks.


-spec parse([tokens]) -> abstract_syntax_tree().
parse(Toks) ->
    {E, _} = parse_exp(Toks),
    E.

-spec scan_and_parse(string()) -> abstract_syntax_tree().
scan_and_parse(Code) ->
    {ok, Toks, _} = let_lang_tok:string(Code),
    parse(Toks).

scan_and_parse_file(Fn) ->
    {ok, Data} = file:read_file(Fn),
    Code = binary_to_list(Data),
    scan_and_parse(Code).
