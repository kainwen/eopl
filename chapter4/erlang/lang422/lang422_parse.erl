-module(lang422_parse).

-export_type([program/0, var/0, statement/0, exp/0]).

-export([scan_and_parse/1, scan_and_parse_file/1]).

-type program() :: statement().

-type var() :: {var, atom()}.

-type statement() :: {assign_st, var(), exp()} % Id = Exp
                   | {print_st, exp()} % print Exp
                   | {block_st, [statement()]} % {st1;st2...}
                   | {if_st, exp(), statement(), statement()} %if exp st1 st2
                   | {while_st, exp(), statement()} % while exp st
                   | {var_declare_st, [var()], statement()}.

-type exp() :: {number_exp, integer()}
             | {var, atom()}
             | {proc_exp, [var()], exp()}
             | {test_zero, exp()}
             | {diff_exp, exp(), exp()}
             | {add_exp, exp(), exp()}
             | {multiply_exp, exp(), exp()}
             | {not_exp, exp()}
             | {apply_exp, exp(), [exp()]}.

-type keyword() :: 'print' | 'if' | '=' | 'while' | 'var' | 'proc'
                 | 'zero?' | 'not' | '-' | '*' | '{' | '}' | '(' | ')' | '+'
                 | ';' | ','.

-type token() :: keyword()
                | {integer, integer()}
                | {id, atom()}.

-spec parse([token()]) -> {statement(), [token()]}.
parse(Toks=[{id, _}|_R]) ->
    parse_assign(Toks);
parse(Toks=['print'|_R]) ->
    parse_print(Toks);
parse(Toks=['{'|_R]) ->
    parse_block(Toks);
parse(Toks=['if'|_R]) ->
    parse_if(Toks);
parse(Toks=['while'|_R]) ->
    parse_while(Toks);
parse(Toks=['var'|_R]) ->
    parse_var_declare(Toks).


%sub branch
parse_assign(Toks) ->
    {Var, R1} = parse_var(Toks),
    R2 = wait_for('=', R1),
    {Exp, R3} = parse_exp(R2),
    {{assign_st, Var, Exp}, R3}.

parse_print(Toks) ->
    R1 = wait_for('print', Toks),
    {Exp, R2} = parse_exp(R1),
    {{print_st, Exp}, R2}.

parse_block(Toks) ->
    R1 = wait_for('{', Toks),
    {Statements, R2} = parse_multiple_with_sep(R1, fun parse/1, ';', '}'),
    {{block_st, Statements}, R2}.

parse_if(Toks) ->
    R1 = wait_for('if', Toks),
    {Exp, R2} = parse_exp(R1),
    {St1, R3} = parse(R2),
    {St2, R4} = parse(R3),
    {{if_st, Exp, St1, St2}, R4}.

parse_while(Toks) ->
    R1 = wait_for('while', Toks),
    {Exp, R2} = parse_exp(R1),
    {St, R3} = parse(R2),
    {{while_st, Exp, St}, R3}.

parse_var_declare(Toks) ->
    R1 = wait_for('var', Toks),
    {Vars, R2} = parse_multiple_with_sep(R1, fun parse_var/1, ',', ';'),
    {St, R3} = parse(R2),
    {{var_declare_st, Vars, St}, R3}.

parse_var([{id, V}|R]) ->
    {{var, V}, R}.

parse_exp(Toks=[{integer, _N}|_R]) ->
    parse_number(Toks);
parse_exp(Toks=[{id, _}|_R]) ->
    parse_var(Toks);
parse_exp(Toks=['proc'|_R]) ->
    parse_proc(Toks);
parse_exp(Toks=['zero?'|_R]) ->
    parse_test_zero(Toks);
parse_exp(Toks=['-'|_R]) ->
    parse_diff(Toks);
parse_exp(Toks=['*'|_R]) ->
    parse_multiply(Toks);
parse_exp(Toks=['+'|_R]) ->
    parse_add(Toks);
parse_exp(Toks=['not'|_R]) ->
    parse_not(Toks);
parse_exp(Toks=['('|_R]) ->
    parse_apply(Toks).


parse_number([{integer, N}|R]) ->
    {{number_exp, N}, R}.

parse_proc(Toks) ->
    R1 = wait_for('(', wait_for('proc', Toks)),
    {Paras, R2} = parse_multiple_with_sep(R1, fun parse_var/1, ',', ')'),
    {Body, R3} = parse_exp(R2),
    {{proc_exp, Paras, Body}, R3}.

parse_test_zero(Toks) ->
    R1 = wait_for('(', wait_for('zero?', Toks)),
    {Exp, R2} = parse_exp(R1),
    R3 = wait_for(')', R2),
    {{test_zero, Exp}, R3}.

parse_diff(Toks) ->
    R1 = wait_for('(', wait_for('-', Toks)),
    {[Exp1, Exp2], R2} = parse_multiple_with_sep(R1, fun parse_exp/1, ',', ')'),
    {{diff_exp, Exp1, Exp2}, R2}.

parse_multiply(Toks) ->
    R1 = wait_for('(', wait_for('*', Toks)),
    {[Exp1, Exp2], R2} = parse_multiple_with_sep(R1, fun parse_exp/1, ',', ')'),
    {{multiply_exp, Exp1, Exp2}, R2}.

parse_add(Toks) ->
    R1 = wait_for('(', wait_for('+', Toks)),
    {[Exp1, Exp2], R2} = parse_multiple_with_sep(R1, fun parse_exp/1, ',', ')'),
    {{add_exp, Exp1, Exp2}, R2}.

parse_not(Toks) ->
    R1 = wait_for('(', wait_for('not', Toks)),
    {Exp, R2} = parse_exp(R1),
    R3 = wait_for(')', R2),
    {{not_exp, Exp}, R3}.

parse_apply(Toks) ->
    R1 = wait_for('(', Toks),
    {Operator, R2} = parse_exp(R1),
    {Args, R3} = parse_multiple(R2, fun parse_exp/1, ')'),
    {{apply_exp, Operator, Args}, R3}.

%% internal helpers
parse_multiple_with_sep([Sep|Toks], Fun, Sep, End) ->
    parse_multiple_with_sep(Toks, Fun, Sep, End);
parse_multiple_with_sep([End|Toks], _Fun, _Sep, End) ->
    {[], Toks};
parse_multiple_with_sep(Toks, Fun, Sep, End) ->
    {Result, R1} = Fun(Toks),
    {Rs, R} = parse_multiple_with_sep(R1, Fun, Sep, End),
    {[Result|Rs], R}.

parse_multiple([End|Toks], _Fun, End) ->
    {[], Toks};
parse_multiple(Toks, Fun, End) ->
    {Result, R1} = Fun(Toks),
    {Rs, R} = parse_multiple(R1, Fun, End),
    {[Result|Rs], R}.


wait_for(_, []) -> erlang:error(nothing__wait_for);
wait_for(Tok, [Tk|R]) when Tk =:= Tok -> R;
wait_for(Tok, [Tk|_R]) when Tk /= Tok -> erlang:error(nothing__wait_for).

%% APIs
scan_and_parse(Code) ->
    {ok, Toks, _} = lang422_tok:string(Code),
    {St, []} = parse(Toks),
    St.

scan_and_parse_file(Fn) ->
    {ok, Data} = file:read_file(Fn),
    Code = binary_to_list(Data),
    scan_and_parse(Code).
