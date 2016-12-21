-module(let_lang_parse).

-export([scan_and_parse/1]).

-export_type([abstract_syntax_tree/0, tokens/0, keywords/0]).

-type tokens() :: keywords()
                | {integer, integer()}
                | {id, atom()}.

-type keywords() :: 'if' | 'then' | 'else' | 'zero?' | 'let' | '=' | 'in'
                    | '-' | '(' | ')' | ','.

-type abstract_syntax_tree() :: {number_exp, integer()}
                              | {var_exp, atom()}
                              | {check_zero_exp, abstract_syntax_tree()}
                              | {if_exp, abstract_syntax_tree(), abstract_syntax_tree(), abstract_syntax_tree()}
                              | {diff_exp, abstract_syntax_tree(), abstract_syntax_tree()}
                              | {let_exp, {var_exp, atom()}, abstract_syntax_tree(), abstract_syntax_tree()}.

-spec parse_helper([tokens]) -> {abstract_syntax_tree(), [tokens]}.
parse_helper([{id, Id}|Rem_toks]) ->
    {{var_exp, Id}, Rem_toks};
parse_helper([{integer, N}|Rem_toks]) ->
    {{number_exp, N}, Rem_toks};
parse_helper(['if'|Rem_toks]) ->
    {Question, ['then'|R]} = parse_helper(Rem_toks),
    {Answer, ['else'|RR]} = parse_helper(R),
    {Alternate, RRR} = parse_helper(RR),
    {{if_exp, Question, Answer, Alternate}, RRR};
parse_helper(['zero?'|Rem_toks]) ->
    {Exp, R} = parse_helper(Rem_toks),
    {{check_zero_exp, Exp}, R};
parse_helper(['-','('|Rem_toks]) ->
    {Exp1, [','|R]} = parse_helper(Rem_toks),
    {Exp2, [')'|RR]} = parse_helper(R),
    {{diff_exp, Exp1, Exp2}, RR};
parse_helper(['let'|Rem_toks]) ->
    {Var, ['='|R]} = parse_helper(Rem_toks),
    {Exp, ['in'|RR]} = parse_helper(R),
    {Body, RRR} = parse_helper(RR),
    {{let_exp, Var, Exp, Body}, RRR}.

-spec parse([tokens]) -> abstract_syntax_tree().
parse(Toks) ->
    {E, _} = parse_helper(Toks),
    E.

-spec scan_and_parse(string()) -> abstract_syntax_tree().
scan_and_parse(Code) ->
    {ok, Toks, _} = let_lang_tok:string(Code),
    parse(Toks).
