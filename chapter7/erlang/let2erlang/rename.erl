-module(rename).

-export([rename_with_senv/2]).

-type senv() :: senv:senv().
-type exp() :: let_lang_parse:exp().

-spec rename_with_senv(exp(), senv()) -> exp().
rename_with_senv({number_exp, N}, _Senv) ->
    {number_exp, N};
rename_with_senv({var_exp, V}, Senv) ->
    Depth = senv:get_var_depth(Senv, V),
    New_var = rename_var_with_depth(V, Depth),
    {var_exp, New_var};
rename_with_senv({check_zero_exp, Exp}, Senv) ->
    {check_zero_exp, rename_with_senv(Exp, Senv)};
rename_with_senv({if_exp, A, B, C}, Senv) ->
    {if_exp,
     rename_with_senv(A, Senv),
     rename_with_senv(B, Senv),
     rename_with_senv(C, Senv)};
rename_with_senv({diff_exp, A, B}, Senv) ->
    {diff_exp,
     rename_with_senv(A, Senv),
     rename_with_senv(B, Senv)};
rename_with_senv({let_exp, Paras, Body}, Senv) ->
    Curr_depth = senv:get_curr_depth(Senv),
    New_bindings = [{{var_exp, V, Curr_depth+1},
                     rename_with_senv(Binding_exp, Senv)}
                    || {{var_exp, V}, Binding_exp} <- Paras],
    New_senv = senv:extend_senv(Senv,
                                [{V, D}|| {{var_exp, V, D}, _E} <- New_bindings]),
    New_body = rename_with_senv(Body, New_senv),
    {let_exp,
     [{{var_exp, rename_var_with_depth(V, D)}, E}
      || {{var_exp, V, D}, E} <- New_bindings],
     New_body};
rename_with_senv({proc_exp, Paras, Body}, Senv) ->
    Curr_depth = senv:get_curr_depth(Senv),
    New_args = [{V, Curr_depth+1} || {var_exp, V} <- Paras],
    New_senv = senv:extend_senv(Senv, New_args),
    New_body = rename_with_senv(Body, New_senv),
    {proc_exp,
     [{var_exp, rename_var_with_depth(V, Curr_depth+1)}
      ||{var_exp, V} <- Paras],
     New_body};
rename_with_senv({apply_exp, Operator, Operands}, Senv) ->
    New_operator = rename_with_senv(Operator, Senv),
    New_operands = [rename_with_senv(Op, Senv) || Op <- Operands],
    {apply_exp,
     New_operator,
     New_operands}.

%% Internal functions
-spec rename_var_with_depth(atom(), integer()) -> atom().
rename_var_with_depth(V, Depth) ->
    D_string = integer_to_list(Depth),
    V_string = atom_to_list(V),
    New_name = string:concat(V_string, D_string),
    list_to_atom(New_name).
