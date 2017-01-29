-module(rename).

-export([rename_with_senv/3]).

rename_with_senv({number_exp, N}, _Senv, _Store) ->
    {number_exp, N};
rename_with_senv({var_exp, V}, Senv, _Store) ->
    D = senv:apply_senv(Senv, V),
    New_var = rename_var(V, D),
    {var_exp, New_var};
rename_with_senv({check_zero_exp, Exp}, Senv, Store) ->
    {check_zero_exp, rename_with_senv(Exp, Senv, Store)};
rename_with_senv({if_exp, A, B, C}, Senv, Store) ->
    {if_exp,
     rename_with_senv(A, Senv, Store),
     rename_with_senv(B, Senv, Store),
     rename_with_senv(C, Senv, Store)
    };
rename_with_senv({diff_exp, A, B}, Senv, Store) ->
    {diff_exp,
     rename_with_senv(A, Senv, Store),
     rename_with_senv(B, Senv, Store)
    };
rename_with_senv(Let_exp={let_exp, Paras, Body}, Senv, Store) ->
    Ref = store:newref(Store, Let_exp),
    Vars = [{V, Ref} || {{var_exp, V}, _} <- Paras],
    New_senv = senv:extend_senv(Senv, Vars),
    New_body = rename_with_senv(Body, New_senv, Store),
    Binding_exps = [rename_with_senv(E, Senv, Store) || {_, E} <- Paras],
    New_paras = [{{var_exp, rename_var(V, Rf)}, Bd_exp}
                 || {{V, Rf}, Bd_exp} <- lists:zip(Vars, Binding_exps)],
    {let_exp, New_paras, New_body};
rename_with_senv(Proc_exp={proc_exp, Paras, Body}, Senv, Store) ->
    Ref = store:newref(Store, Proc_exp),
    Vars = [{V, Ref} || {var_exp, V} <- Paras],
    New_senv = senv:extend_senv(Senv, Vars),
    New_body = rename_with_senv(Body, New_senv, Store),
    {proc_exp,
     [{var_exp, rename_var(V, Rf)} || {V, Rf} <- Vars],
     New_body
    };
rename_with_senv({apply_exp, Operator, Operands}, Senv, Store) ->
    New_operator = rename_with_senv(Operator, Senv, Store),
    New_operands = [rename_with_senv(Op, Senv, Store) || Op <- Operands],
    {apply_exp, New_operator, New_operands}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec rename_var(atom(), integer()) -> atom().
rename_var(V, D) ->
    V_string = atom_to_list(V),
    D_string = integer_to_list(D),
    New_var_string = string:join([V_string, "_", D_string], ""),
    list_to_atom(New_var_string).
