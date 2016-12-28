-module(let_lang_trans).

-export([trans/2]).

-export_type([nameless_abt/0]).

-type nameless_abt() :: {number_exp, integer()}
                      | {nameless_var_exp, integer()}
                      | {check_zero_exp, nameless_abt()}
                      | {if_exp, nameless_abt(), nameless_abt(), nameless_abt()}
                      | {diff_exp, nameless_abt(), nameless_abt()}
                      | {nameless_let_exp, [nameless_abt()], nameless_abt()}
                      | {nameless_proc_exp, nameless_abt()}
                      | {apply_exp, nameless_abt(), nameless_abt()}
                      | {nameless_letrec_exp, [nameless_abt()], nameless_abt()}.

-spec trans(let_lang_parse:abstract_syntax_tree(), senv:senv()) -> nameless_abt().
trans({number_exp, N}, _Senv) -> {number_exp, N};
trans({var_exp, Var}, Senv) -> {nameless_var_exp, senv:apply_senv(Senv, Var)};
trans({check_zero_exp, E}, Senv) ->
    {check_zero_exp, trans(E, Senv)};
trans({if_exp, Q, A, E}, Senv) ->
    {if_exp, trans(Q, Senv), trans(A, Senv), trans(E, Senv)};
trans({diff_exp, E1, E2}, Senv) ->
    {diff_exp, trans(E1, Senv), trans(E2, Senv)};
trans({let_exp, Paras, Body}, Senv) ->
    Vars = [Id || {{_, Id}, _} <- Paras],
    Exps = [E || {_, E} <- Paras],
    {nameless_let_exp,
     [trans(E, Senv) || E <- Exps],
     trans(Body, senv:extend_senv(Senv, Vars))};
trans({unpack_exp, Vars, Cons_exp, Body}, Senv) ->
    Es = cons_to_list(Cons_exp),
    trans({let_exp, lists:zip(Vars, Es), Body}, Senv);
trans({proc_exp, Vars, Body}, Senv) ->
    Ids = [Id || {_, Id} <- Vars],
    {nameless_proc_exp, trans(Body, senv:extend_senv(Senv, Ids))};
trans({apply_exp, Operator, Operands}, Senv) ->
    {apply_exp,
     trans(Operator, Senv),
     [trans(Op, Senv) || Op <- Operands]};
trans({letrec_exp, Proc_list, Body}, Senv) ->
    Proc_names = [Name || {{_, Name}, _, _} <- Proc_list],
    New_senv = senv:extend_senv(Senv, Proc_names),
    Exps = [trans({proc_exp, Paras, Proc_body}, New_senv)
            || {_, Paras, Proc_body} <- Proc_list],
    {nameless_letrec_exp, Exps, trans(Body, New_senv)}.


cons_to_list({emptylist}) -> [];
cons_to_list({cons_exp, E1, E2}) ->
    [E1 | cons_to_list(E2)].
