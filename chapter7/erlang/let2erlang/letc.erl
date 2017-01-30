-module(letc).

-export([compile_file/2]).

-type exp() :: let_lang_parse:exp().

-type string_rep() :: {Code :: string(),
                       Var :: string()}.

-spec compile2erl(exp(), pid()) -> string_rep().
compile2erl({number_exp, N}, Ns) ->
    Var = var_name:new(Ns),
    Total = safe_join([Var, integer_to_list(N)], " = "),
    {Total, Var};
compile2erl({var_exp, V}, _Ns) ->
    Vn = atom_to_var(V),
    {"", Vn};
compile2erl({check_zero_exp, Exp}, Ns) ->
    {Exp_string, One_var} = compile2erl(Exp, Ns),
    Final_answer = var_name:new(Ns),
    Code = safe_join([Final_answer,
                        " = ",
                        "(",
                        safe_join([One_var, "0"], " =:= "),
                        ")"], ""),
    Final_code = safe_join([Exp_string, Code], ",\n"),
    {Final_code, Final_answer};
compile2erl({if_exp, A, B, C}, Ns) ->
    {A_code, Var_A} = compile2erl(A, Ns),
    {B_code, Var_B} = compile2erl(B, Ns),
    {C_code, Var_C} = compile2erl(C, Ns),
    Final_answer = var_name:new(Ns),
    Case_line = safe_join(["case",
                             Var_A,
                             "of"], " "),
    True_line = safe_join(["true ->",
                             safe_join([B_code, Var_B], ",\n"),
                             ";"
                            ], "\n"),
    False_line = safe_join(["false ->",
                              safe_join([C_code, Var_C], ",\n")
                             ], "\n"),
    End_line = "end",
    Code = safe_join([Case_line, True_line, False_line, End_line], "\n"),
    Final_code = safe_join([
                              A_code,
                              safe_join([Final_answer, "=", Code], " ")
                             ], ",\n"),
    {Final_code, Final_answer};
compile2erl({diff_exp, A, B}, Ns) ->
    {A_code, Var_A} = compile2erl(A, Ns),
    {B_code, Var_B} = compile2erl(B, Ns),
    Final_answer = var_name:new(Ns),
    Code = safe_join([Var_A, "-", Var_B], " "),
    Final_code = safe_join([A_code,
                              B_code,
                              safe_join([Final_answer, "=", Code], " ")
                             ], ",\n"),
    {Final_code, Final_answer};
compile2erl({let_exp, Bindings, Body}, Ns) ->
    Bds = [{atom_to_var(V), compile2erl(Exp, Ns)}
           || {{var_exp, V}, Exp}<- Bindings],
    Exps_code = safe_join([Fc || {_V, {Fc, _Fv}} <- Bds],
                                ",\n"),
    Bindings_code = safe_join([safe_join([V, "=", Fv], " ")
                                 || {V, {_, Fv}} <- Bds],
                                ",\n"),
    {Body_code, Body_var} = compile2erl(Body, Ns),
    Final_code = safe_join([
                              Exps_code,
                              Bindings_code,
                              Body_code
                             ], ",\n"),
    {Final_code, Body_var};
compile2erl({proc_exp, Paras, Body}, Ns) ->
    Final_answer = var_name:new(Ns),
    Func_line = safe_join([
                             "fun (",
                             safe_join([atom_to_var(V) || {_, V} <- Paras],
                                         ", "),
                             ") ->"
                            ], " "),
    {Body_code, Body_var} = compile2erl(Body, Ns),
    End_line = "end",
    Final_code = safe_join([
                              Final_answer,
                              " = ",
                              safe_join([
                                           Func_line,
                                           safe_join([Body_code, Body_var],
                                                       ",\n"),
                                           End_line
                                          ], "\n")
                             ], ""),
    {Final_code, Final_answer};
compile2erl({apply_exp, Operator, Operands}, Ns) ->
    {Operator_code, Operator_var} = compile2erl(Operator, Ns),
    Operand_tmp_results = [compile2erl(Exp, Ns) || Exp <- Operands],
    Final_answer = var_name:new(Ns),
    Call_line = safe_join([
                             Operator_var,
                             "(",
                             safe_join([Rand_var
                                           || {_, Rand_var} <- Operand_tmp_results],
                                         ","),
                             ")"
                            ], ""),
    Final_code = safe_join([
                              Operator_code,
                              safe_join([C|| {C, _} <- Operand_tmp_results], "\n"),
                              safe_join([
                                           Final_answer,
                                           " = ",
                                           Call_line
                                          ], "")
                             ],
                             ",\n"),
    {Final_code, Final_answer}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
compile_file(Fn, Output) ->
    Ns = var_name:start(),
    Name_server = name_server:start(),
    Senv = senv:empty_senv(),
    Exp = rename:rename_with_senv(let_lang_parse:scan_and_parse_file(Fn),
                                  Senv,
                                  Name_server),
    {A, B} = compile2erl(Exp, Ns),
    Code = safe_join([
                        head(atom_to_list(Output)),
                        enclose("main", {A, B})
                       ], "\n"),
    file:write_file(string:concat(atom_to_list(Output), ".erl"),
                    Code).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
atom_to_var(A) ->
    string:to_upper(atom_to_list(A)).

head(Module) ->
    safe_join([
               string:join(["-module(", Module, ")."], ""),
               "-export([main/0]).\n"
              ], "\n").

enclose(Func_name, {Body, V}) ->
    safe_join([
               safe_join([Func_name, "() ->"], " "),
               string:concat(Body, ","),
               string:concat(V, ".")
              ], "\n").

safe_join(Lst, Delim) ->
    Safe_list = lists:filter(fun (S) ->
                                     S /= ""
                             end,
                             Lst),
    string:join(Safe_list, Delim).
