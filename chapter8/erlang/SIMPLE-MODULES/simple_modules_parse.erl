-module(simple_modules_parse).

-compile(export_all).

-type program() :: [exp()].

-type tp() :: {int}
            | {bool}
            | {arrow, tp(), tp()}
            | {tuple, [tp()]}
            | {list, tp()}
            | {module, [{atom(), tp()}]}.

-type tok() :: keywords()
             | {integer, integer()}
             | {id, atom()}.

-type keywords() :: 'if' | 'then' | 'else' | 'zero?' | 'let' | '=' | 'in'
                  | 'proc' | 'letrec' | ':' | '{' | '}' | 'match_tuple'
                  | '-' | '(' | ')' | ',' | '.'
                  | 'int' | 'bool' | '->' | '*' | '[' | ']'
                  | 'list' | 'cons' | 'cdr' | 'null?'.

-type exp() :: {num_exp, integer()}
             | {var_exp, atom()}
             | {test_zero_exp, exp()}
             | {if_exp, exp(), exp(), exp()}
             | {diff_exp, exp(), exp()}
             | {let_exp,
                Vars::[atom()],
                Defs::[exp()],
                Body::exp()}
             | {proc_exp, [{atom(), tp()}], Body::exp()}
             | {apply_exp, Operator::exp(), Operands::[exp()]}
             | {letrec_exp,
                [{atom(), Return_type::tp()}],
                Function_defs::[exp()],
                Body::exp()}
             | {tuple_exp, [exp()]}
             | {match_tuple_exp, [atom()], exp(), exp()}
             | {list_exp, [exp()]}
             | {car_exp, exp()}
             | {cons_exp, exp(), exp()}
             | {cdr_exp, exp()}
             | {test_null_exp, exp()}
             | {from_take_exp, Module_name::atom(), [Symbol_name::atom()]}
             | {access_mod_element_exp, Module_name::atom(), Symbol_name::atom()}
             | {module_exp,
                Module_name::atom(),
                Sig::tp(),
                {Exps::[exp()], Vars::[atom()], Binding_exps::[exp()]}}.


-spec parse_exp([tok()]) -> {exp(), [tok()]}.
parse_exp(Toks=[{id, _}|_Rem_toks]) ->
    parse_var(Toks);
parse_exp(Toks=[{integer, _}|_Rem_toks]) ->
    parse_num(Toks);
parse_exp(Toks=['zero?'|_Rem_toks]) ->
    parse_test_zero(Toks);
parse_exp(Toks=['if'|_Rem_toks]) ->
    parse_if(Toks);
parse_exp(Toks=['-'|_Rem_toks]) ->
    parse_diff(Toks);
parse_exp(Toks=['let'|_Rem_toks]) ->
    parse_let(Toks);
parse_exp(Toks=['proc'|_Rem_toks]) ->
    parse_proc(Toks);
parse_exp(Toks=['('|_Rem_toks]) ->
    parse_apply(Toks);
parse_exp(Toks=['letrec'|_Rem_toks]) ->
    parse_letrec(Toks);
parse_exp(Toks=['{'|_Rem_toks]) ->
    parse_tuple(Toks);
parse_exp(Toks=['match_tuple'|_Rem_toks]) ->
    parse_match_tuple(Toks);
parse_exp(Toks=['list'|_Rem_toks]) ->
    parse_list(Toks);
parse_exp(Toks=['cons'|_Rem_toks]) ->
    parse_cons(Toks);
parse_exp(Toks=['car'|_Rem_toks]) ->
    parse_car(Toks);
parse_exp(Toks=['cdr'|_Rem_toks]) ->
    parse_cdr(Toks);
parse_exp(Toks=['null?'|_Rem_toks]) ->
    parse_test_null(Toks);
parse_exp(Toks=['module'|_Rem_toks]) ->
    parse_module(Toks);
parse_exp(Toks=['from'|_Rem_toks]) ->
    parse_from_take(Toks).

%%%%%%%%%%%%%%parse exp sub handlers%%%%%%%%%%%%%%
parse_var([{id, Mod}, '.', {id, Var}|Rem_toks]) ->
    {{access_mod_element_exp, Mod, Var}, Rem_toks};
parse_var([{id, V}|Rem_toks]) ->
    {{var_exp, V}, Rem_toks}.

parse_num([{integer, N}|Rem_toks]) ->
    {{num_exp, N}, Rem_toks}.

parse_test_zero(['zero?', '('|Rem_toks]) ->
    {Exp, [')'|R]} = parse_exp(Rem_toks),
    {{test_zero_exp, Exp}, R}.

parse_if(['if'|Rem_toks]) ->
    {A, R1} = parse_exp(Rem_toks),
    {B, R2} = parse_exp(R1),
    {C, R3} = parse_exp(R2),
    {{if_exp, A, B, C}, R3}.

parse_diff(['-', '('|Rem_toks]) ->
    {E1, R1} = parse_exp(Rem_toks),
    R2 = wait_for(',', R1),
    {E2, R3} = parse_exp(R2),
    R4 = wait_for(')', R3),
    {{diff_exp, E1, E2}, R4}.

parse_let(['let'|Rem_toks]) ->
    {Bindings, R1} = parse_multiple_with_delim(fun parse_binding/1, Rem_toks, ''),
    R2 = wait_for('in', R1),
    Vars = [Var ||{Var, _} <- Bindings],
    Exps = [Exp ||{_, Exp} <- Bindings],
    {Body, R3} = parse_exp(R2),
    {{let_exp, Vars, Exps, Body}, R3}.

parse_proc(['proc', '('|Rem_toks]) ->
    {Paras_with_type, R1} = parse_multiple_with_delim(fun parse_var_with_type/1,
                                                      Rem_toks,
                                                      ','),
    R2 = wait_for(')', R1),
    {Body, R3} = parse_exp(R2),
    {{proc_exp, Paras_with_type, Body}, R3}.

parse_apply(['('|Rem_toks]) ->
    {Operator, R1} = parse_exp(Rem_toks),
    {Operands, [')'|R2]} = parse_multiple_with_delim(fun parse_exp/1, R1, ','),
    {{apply_exp, Operator, Operands}, R2}.

parse_letrec(['letrec'|Rem_toks]) ->
    {Proc_defs, R1} = parse_multiple_with_delim(fun parse_proc_def/1, Rem_toks, ''),
    R2 = wait_for('in', R1),
    {Body, R3} = parse_exp(R2),
    Proc_names_with_return_type = [{Proc_name, Rt}
                                   || {Proc_name, Rt, _} <- Proc_defs],
    Proc_bodys = [Proc_body || {_, _, Proc_body} <- Proc_defs],
    {{letrec_exp, Proc_names_with_return_type, Proc_bodys, Body}, R3}.

parse_tuple(['{'|R]) ->
    {Tp_elements, R1} = parse_multiple_with_delim(fun parse_exp/1, R, ','),
    R2 = wait_for('}', R1),
    {{tuple_exp, Tp_elements}, R2}.

parse_match_tuple(['match_tuple'|R]) ->
    {Vars, R1} = parse_multiple_with_delim(fun parse_var/1, R, ''),
    R2 = wait_for('=', R1),
    {Tuple, R3} = parse_exp(R2),
    R4 = wait_for('in', R3),
    {Body, R5} = parse_exp(R4),
    {{match_tuple_exp, [V || {var, V} <- Vars], Tuple, Body}, R5}.

parse_list(['list', '('|R]) ->
    {Exps, R1} = parse_multiple_with_delim(fun parse_exp/1, R, ','),
    R2 = wait_for(')', R1),
    {{list_exp, Exps}, R2}.

parse_cons(['cons', '('|R]) ->
    {[Exp1, Exp2], R1} = parse_multiple_with_delim(fun parse_exp/1, R, ','),
    R2 = wait_for(')', R1),
    {{cons_exp, Exp1, Exp2}, R2}.

parse_car(['car', '('|R]) ->
    {Exp, R1} = parse_exp(R),
    R2 = wait_for(')', R1),
    {{car_exp, Exp}, R2}.

parse_cdr(['cdr', '('|R]) ->
    {Exp, R1} = parse_exp(R),
    R2 = wait_for(')', R1),
    {{cdr_exp, Exp}, R2}.

parse_test_null(['null?', '('|R]) ->
    {Exp, R1} = parse_exp(R),
    R2 = wait_for(')', R1),
    {{test_null_exp, Exp}, R2}.

parse_module(['module'|R]) ->
    {{var_exp, Module_name}, R1} = parse_var(R),
    R2 = wait_for('interface', R1),
    {Sig, R3} = type:parse_type(R2),
    R4 = wait_for('body', R3),
    {Exps, R5} = parse_multiple_with_delim(fun parse_exp/1, R4, ''),
    R6 = wait_for('<', R5),
    {Bindings, R7} = parse_multiple_with_delim(fun parse_binding/1, R6, ''),
    R8 = wait_for('>', R7),
    Vars = [Var || {Var, _} <- Bindings],
    Binding_exps = [Exp || {_, Exp} <- Bindings],
    {{module_exp, Module_name, Sig, {Exps, Vars, Binding_exps}}, R8}.

parse_from_take(['from'|R]) ->
    {{var_exp, M}, R1} = parse_var(R),
    {Vars, R2} = parse_multiple_with_delim(fun parse_var/1, R1, 'take'),
    {{from_take_exp, M, [V || {var_exp, V} <- Vars]}, R2}.

%%%%%%%%%%%%%%%%% parse helper function %%%%%%%%%%%%%%%%%%%%%%%
wait_for(Atom, [Atom|Toks]) -> Toks;
wait_for(A, Toks) -> erlang:error({miss, A, Toks}).

parse_proc_def(Toks) -> % f(x) = -(x, 1)
    {Return_type, R} = type:parse_type(Toks),
    {{var_exp, V}, R1} = parse_var(R),
    R2 = wait_for('(', R1),
    {Paras, R3} = parse_multiple_with_delim(fun parse_var_with_type/1, R2, ''),
    R4 = wait_for('=', wait_for(')', R3)),
    {Proc_body, R5} = parse_exp(R4),
    {{V, Return_type, {proc_exp, Paras, Proc_body}}, R5}.

parse_var_with_type(Toks) ->
    {{var_exp, V}, R1} = parse_var(Toks),
    R2 = wait_for(':', R1),
    {Tp, R3} = type:parse_type(R2),
    {{V, Tp}, R3}.

parse_binding(Toks) ->
    {{var_exp, V}, R1} = parse_var(Toks),
    R2 = wait_for('=', R1),
    {Exp, R3} = parse_exp(R2),
    {{V, Exp}, R3}.

parse_multiple_with_delim(Fun, Toks, Delim) ->
    parse_multiple_with_delim_helper(Fun, Toks, Delim, []).

parse_multiple_with_delim_helper(Fun, [Delim|Toks], Delim, Acc_list) ->
    parse_multiple_with_delim_helper(Fun, Toks, Delim, Acc_list);
parse_multiple_with_delim_helper(Fun, Toks, Delim, Acc_list) ->
    try Fun(Toks) of
        {Term, R} ->
            parse_multiple_with_delim_helper(Fun, R, Delim, [Term|Acc_list])
    catch
        _:_ ->
            {lists:reverse(Acc_list), Toks}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parse(Toks) ->
    {Exps, []} = parse_multiple_with_delim(fun parse_exp/1, Toks, ''),
    Exps.

-spec scan_and_parse(string()) -> program().
scan_and_parse(Code) ->
    {ok, Toks, _} = simple_modules_tok:string(Code),
    parse(Toks).

-spec scan_and_parse_file(string()) -> program().
scan_and_parse_file(Fn) ->
    {ok, Data} = file:read_file(Fn),
    Code = binary_to_list(Data),
    scan_and_parse(Code).
