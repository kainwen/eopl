-module(rename).

-export([rename_with_senv/3]).

rename_with_senv({number_exp, N}, _Senv, _Ns) ->
    {number_exp, N};
rename_with_senv({var_exp, V}, Senv, _Ns) ->
    D = senv:apply_senv(Senv, V),
    New_var = rename_var(V, D),
    {var_exp, New_var};
rename_with_senv({check_zero_exp, Exp}, Senv, Ns) ->
    {check_zero_exp, rename_with_senv(Exp, Senv, Ns)};
rename_with_senv({if_exp, A, B, C}, Senv, Ns) ->
    {if_exp,
     rename_with_senv(A, Senv, Ns),
     rename_with_senv(B, Senv, Ns),
     rename_with_senv(C, Senv, Ns)
    };
rename_with_senv({diff_exp, A, B}, Senv, Ns) ->
    {diff_exp,
     rename_with_senv(A, Senv, Ns),
     rename_with_senv(B, Senv, Ns)
    };
rename_with_senv({let_exp, Paras, Body}, Senv, Ns) ->
    Id = name_server:new_id(Ns),
    Vars = [{V, Id} || {{var_exp, V}, _} <- Paras],
    New_senv = senv:extend_senv(Senv, Vars),
    New_body = rename_with_senv(Body, New_senv, Ns),
    Binding_exps = [rename_with_senv(E, Senv, Ns) || {_, E} <- Paras],
    New_paras = [{{var_exp, rename_var(V, I)}, Bd_exp}
                 || {{V, I}, Bd_exp} <- lists:zip(Vars, Binding_exps)],
    {let_exp, New_paras, New_body};
rename_with_senv({proc_exp, Paras, Body}, Senv, Ns) ->
    Id = name_server:new_id(Ns),
    Vars = [{V, Id} || {var_exp, V} <- Paras],
    New_senv = senv:extend_senv(Senv, Vars),
    New_body = rename_with_senv(Body, New_senv, Ns),
    {proc_exp,
     [{var_exp, rename_var(V, I)} || {V, I} <- Vars],
     New_body
    };
rename_with_senv({apply_exp, Operator, Operands}, Senv, Ns) ->
    New_operator = rename_with_senv(Operator, Senv, Ns),
    New_operands = [rename_with_senv(Op, Senv, Ns) || Op <- Operands],
    {apply_exp, New_operator, New_operands}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec rename_var(atom(), integer()) -> atom().
rename_var(V, D) ->
    V_string = atom_to_list(V),
    D_string = integer_to_list(D),
    New_var_string = string:join([V_string, "_", D_string], ""),
    list_to_atom(New_var_string).
