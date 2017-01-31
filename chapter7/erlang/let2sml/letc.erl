-module(letc).

-compile(export_all).

-type exp() :: let_lang_parse:exp().

-spec compile2ml(exp()) -> string().
compile2ml({number_exp, N}) ->
    case N >= 0 of
        true -> integer_to_list(N);
        false ->
            string:concat("~~", integer_to_list(-1*N))
    end;
compile2ml({var_exp, V}) ->
    atom_to_list(V);
compile2ml({check_zero_exp, Exp}) ->
    Exp_code = compile2ml(Exp),
    string:join([Exp_code, "0"], " = ");
compile2ml({if_exp, A, B, C}) ->
    A_code = compile2ml(A),
    B_code = compile2ml(B),
    C_code = compile2ml(C),
    string:join([
                 "if",
                 A_code,
                 "then",
                 B_code,
                 "else",
                 C_code
                ], "\n");
compile2ml({diff_exp, A, B}) ->
    A_code = compile2ml(A),
    B_code = compile2ml(B),
    string:join([
                 A_code,
                 "-",
                 B_code
                ], " ");
compile2ml({let_exp, Bindings, Body}) ->
    Bds = [{atom_to_list(V), compile2ml(Exp)}
           || {{var_exp, V}, Exp} <- Bindings],
    New_body = compile2ml(Body),
    string:join([
                 "let",
                 generate_var_def(Bds),
                 "in",
                 New_body,
                 "end"
                ], "\n");
compile2ml({proc_exp, Paras, Body}) ->
    New_body = compile2ml(Body),
    string:join(["fn (",
                 string:join([atom_to_list(V) || {var_exp, V} <- Paras], ", "),
                 ") => ",
                 New_body
                ], "");
compile2ml({apply_exp, Operator, Operands}) ->
    New_operator = compile2ml(Operator),
    New_operands = [compile2ml(Operand) || Operand <- Operands],
    string:join([
                 New_operator,
                 "(",
                 string:join(New_operands, ", "),
                 ")"
                ], "").

compile(Fn) ->
    Exp = let_lang_parse:scan_and_parse_file(Fn),
    Senv = senv:empty_senv(),
    Ns = name_server:start(),
    Renamed_exp = rename:rename_with_senv(Exp, Senv, Ns),
    Code = compile2ml(Renamed_exp),
    io:format(Code).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
generate_var_def(Bds) ->
    string:join([string:join(["val", V, "=", Exp_code], " ")
                 || {V, Exp_code} <- Bds], "\n").
