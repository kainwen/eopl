-module(let_lang).

-include_lib("eunit/include/eunit.hrl").

-export([eval_code/2, eval_script/2]).

-export_type([expval/0, temp_proc/0]).

-type expval() :: {num_val, integer()}
                | {bool_val, boolean()}
                | {list_val, [expval()]}
                | {proc_val, {[atom()],
                              let_lang_parse:abstract_syntax_tree(),
                              env:environment()}}
                | {arr_val, [store:ref()], integer()}.

-type temp_proc() :: {[atom()], let_lang_parse:abstract_syntax_tree()}.
%-type denval() :: {ref_val, expval()}.

-spec eval_abt(let_lang_parse:abstract_syntax_tree(), env:environment(), store:store()) -> expval().
eval_abt({number_exp, N}, _Env, _Store) -> {num_val, N};
eval_abt({var_exp, Id}, Env, Store) ->
    Ref = env:apply_env(Env, Id),
    Val = store:deref(Store, Ref),
    Val;
eval_abt({check_zero_exp, Exp}, Env, Store) ->
    {num_val, N} = eval_abt(Exp, Env, Store),
    {bool_val, N =:= 0};
eval_abt({if_exp, Question, Answer, Alternate}, Env, Store) ->
    {bool_val, R} = eval_abt(Question, Env, Store),
    case R of
        true -> eval_abt(Answer, Env, Store);
        false -> eval_abt(Alternate, Env, Store)
    end;
eval_abt({diff_exp, Exp1, Exp2}, Env, Store) ->
    {num_val, N1} = eval_abt(Exp1, Env, Store),
    {num_val, N2} = eval_abt(Exp2, Env, Store),
    {num_val, N1-N2};
eval_abt({let_exp, Bindlist, Body}, Env, Store) ->
    Binding_pairs = [{Id, store:newref(Store, eval_abt(E, Env, Store))}
                     || {{var_exp, Id}, E} <- Bindlist],
    New_env = env:extend_env_by_list(Env, Binding_pairs),
    eval_abt(Body, New_env, Store);
eval_abt({emptylist}, _Env, _Store) ->
    {list_val, []};
eval_abt({cons_exp, Exp1, Exp2}, Env, Store) ->
    V1 = eval_abt(Exp1, Env, Store),
    {list_val, Vs} = eval_abt(Exp2, Env, Store),
    {list_val, [V1|Vs]};
eval_abt({unpack_exp, Vars, Exp, Body}, Env, Store) ->
    {list_val, Vals} = eval_abt(Exp, Env, Store),
    Pairs = lists:zip([Id || {var_exp, Id} <- Vars], Vals),
    New_pairs = [{Id, store:newref(Store, Val)} || {Id, Val} <- Pairs],
    New_env = env:extend_env_by_list(Env, New_pairs),
    eval_abt(Body, New_env, Store);
eval_abt({'proc_exp', Paras, Body}, Env, _Store) ->
    {proc_val, {[Id || {var_exp, Id} <- Paras], Body, Env}};
eval_abt({'apply_exp', Operator, Operands}, Env, Store) ->
    {proc_val, {Vars, Body, Lex_env}} = get_proc(eval_abt(Operator, Env, Store),
                                                 Env),
    Vals = [eval_abt(E, Env, Store) || E <- Operands],
    Pairs = lists:zip(Vars, Vals),
    New_pairs = [{Var, store:newref(Store, Val)}|| {Var, Val} <- Pairs],
    New_env = env:extend_env_by_list(Lex_env, New_pairs),
    eval_abt(Body, New_env, Store);
eval_abt({'letrec_exp', Proc_binding_list, Body}, Env, Store) ->
    Name_procs = [{Name,
                   {[Id || {var_exp, Id} <- Paras],
                    Proc_body}}
                  || {{var_exp, Name}, Paras, Proc_body} <- Proc_binding_list],
    New_name_procs = [{Name, store:newref(Store, Val)}||{Name, Val} <- Name_procs],
    New_env = env:extend_env_rec(Env, New_name_procs),
    eval_abt(Body, New_env, Store);
eval_abt({seq_exps, Exps}, Env, Store) ->
    Vals = [eval_abt(E, Env, Store) || E <- Exps],
    lists:last(Vals);
eval_abt({assign_exp, {var_exp, Var}, Exp}, Env, Store) ->
    Val = eval_abt(Exp, Env, Store),
    Ref = env:apply_env(Env, Var),
    store:setref(Store, Ref, Val);
eval_abt({newarray_exp, Len_exp, Val_exp}, Env, Store) ->
    {num_val, L} = eval_abt(Len_exp, Env, Store),
    Val = eval_abt(Val_exp, Env, Store),
    Arr_refs = [store:newref(Store, Val) || _I <- lists:seq(0, L-1)],
    {arr_val, Arr_refs, L};
eval_abt({arrayref_exp, Arr_exp, Index_exp}, Env, Store) ->
    {arr_val, Arr_refs, _L} = eval_abt(Arr_exp, Env, Store),
    {num_val, N} = eval_abt(Index_exp, Env, Store),
    Ref = lists:nth(N+1, Arr_refs),
    store:deref(Store, Ref);
eval_abt({arrayset_exp, Arr_exp, Index_exp, Val_exp}, Env, Store) ->
    {arr_val, Arr_refs, _L} = eval_abt(Arr_exp, Env, Store),
    {num_val, N} = eval_abt(Index_exp, Env, Store),
    Val = eval_abt(Val_exp, Env, Store),
    Ref = lists:nth(N+1, Arr_refs),
    store:setref(Store, Ref, Val);
eval_abt({arraylength_exp, Arr_exp}, Env, Store) ->
    {arr_val, _Arr_refs, L} = eval_abt(Arr_exp, Env, Store),
    {num_val, L}.

%% internal help function
get_proc(V={proc_val, _}, _Env) -> V;
get_proc({Paras, Body}, Env) ->
    {proc_val, {Paras, Body, Env}}.


%% APIs
-spec eval_code(string(), atom()) -> expval().
eval_code(Code, Name) ->
	Abt = let_lang_parse:scan_and_parse(Code),
	eval_abt(Abt, env:empty_env(), store:init_store(Name)).

eval_script(Fn, Name) ->
    Abt = let_lang_parse:scan_and_parse_file(Fn),
    eval_abt(Abt, env:empty_env(), store:init_store(Name)).

eval_test() ->
    [
     eval_script("code/code0", store1) =:= {num_val, -100},
     eval_script("code/code1", store2) =:= {num_val, 12},
     eval_script("code/code2", store3) =:= {num_val, 12},
     eval_script("code/code3", store4) =:= {num_val, 1},
     eval_script("code/code4", store5) =:= {num_val, 1},
     eval_script("code/code5", store6) =:= {num_val, 2},
     eval_script("code/code6", store7) =:= {num_val, 3}
    ].
