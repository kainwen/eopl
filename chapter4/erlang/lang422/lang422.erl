-module(lang422).

-export([eval_script/2]).

-export_type([expval/0]).

-type expval() :: {num_val, integer()}
                | {proc_val, [atom()], lang422_parse:exp(), env:environment()}
                | {bool_val, boolean()}.


eval_statement({assign_st, {var, Var}, Exp}, Env, Store) ->
    Val = eval_exp(Exp, Env, Store),
    Ref = env:apply_env(Env, Var),
    store:setref(Store, Ref, Val),
    {num_val, 42};
eval_statement({print_st, Exp}, Env, Store) ->
    Val = eval_exp(Exp, Env, Store),
    print_val(Val);
eval_statement({block_st, []}, _Env, _Store) -> {num_val, 42};
eval_statement({block_st, [St1|Sts]}, Env, Store) ->
    eval_statement(St1, Env, Store),
    eval_statement({block_st, Sts}, Env, Store);
eval_statement({if_st, Exp, St1, St2}, Env, Store) ->
    {bool_val, B} = eval_exp(Exp, Env, Store),
    case B of
        true -> eval_statement(St1, Env, Store);
        false -> eval_statement(St2, Env, Store)
    end;
eval_statement({while_st, Exp, St}, Env, Store) ->
    {bool_val, End} = eval_exp(Exp, Env, Store),
    case End of
        false -> {num_val, 42};
        true ->
            eval_statement(St, Env, Store),
            eval_statement({while_st, Exp, St}, Env, Store)
    end;
eval_statement({var_declare_st, Vars, St}, Env, Store) ->
    Vs = [V || {var, V} <- Vars],
    Refs = [store:newref(Store, 0) || _V <- Vs],
    New_env = env:extend_env_by_list(Env, lists:zip(Vs, Refs)),
    eval_statement(St, New_env, Store).

%%%%%%%

eval_exp({number_exp, N}, _E, _S) -> {num_val, N};
eval_exp({var, V}, Env, Store) ->
    Ref = env:apply_env(Env, V),
    store:deref(Store, Ref);
eval_exp({proc_exp, Paras, Body}, Env, _Store) ->
    {proc_val, [P || {_, P} <- Paras], Body, Env};
eval_exp({test_zero, Exp}, Env, Store) ->
    {num_val, N} = eval_exp(Exp, Env, Store),
    {bool_val, N =:= 0};
eval_exp({diff_exp, E1, E2}, Env, Store) ->
    {num_val, N1} = eval_exp(E1, Env, Store),
    {num_val, N2} = eval_exp(E2, Env, Store),
    {num_val, N1-N2};
eval_exp({add_exp, E1, E2}, Env, Store) ->
    {num_val, N1} = eval_exp(E1, Env, Store),
    {num_val, N2} = eval_exp(E2, Env, Store),
    {num_val, N1+N2};
eval_exp({multiply_exp, E1, E2}, Env, Store) ->
    {num_val, N1} = eval_exp(E1, Env, Store),
    {num_val, N2} = eval_exp(E2, Env, Store),
    {num_val, N1*N2};
eval_exp({not_exp, Exp}, Env, Store) ->
    {bool_val, B} = eval_exp(Exp, Env, Store),
    {bool_val, not B};
eval_exp({apply_exp, Exp, Exps}, Env, Store) ->
    {proc_val, Paras, Body, Lex_env} = eval_exp(Exp, Env, Store),
    Args = [eval_exp(E, Env, Store) || E <- Exps],
    Refs = [store:newref(Store, A) || A <- Args],
    New_env = env:extend_env_by_list(Lex_env, lists:zip(Paras, Refs)),
    eval_exp(Body, New_env, Store).

%%%%%
print_val({num_val, N}) ->
    io:format("~p~n", [N]);
print_val({proc_val, _Args, _Body, _L}) ->
    io:format("<proc>~n");
print_val({bool_val, B}) ->
    io:format(atom_to_list(B)).

%%%%
eval_script(Fn, S) ->
    St = lang422_parse:scan_and_parse_file(Fn),
    eval_statement(St, env:empty_env(), store:init_store(S)).
