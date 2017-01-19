-module(cont).

-export([
         end_cont/0, diff1_cont/3, if_cont/4, test_zero_cont/1,
         let_cont/4, apply1_cont/3, letrec_cont/4, car_cont/1, cdr_cont/1,
         list_cont/1, try_cont/4, raise_cont/1, multiple_exps_as_list_cont/4,
         cons1_cont/3, test_null_cont/1
        ]).

-export([apply_cont/2]).

-export_type([cont/0]).

-type exp() :: let_lang_parse:exp().
-type env() :: env:environment().
-type val() :: let_lang:expval().
-type var() :: atom().
%multiple_exps_as_list_cont(Cont, Env, Exps, Acc_list)
-type cont_element() :: {end_cont, Hanlder_pos::integer(), {}}
                      | {diff1_cont, Hanlder_pos::integer(), {Env::env(), E2::exp()}}
                      | {diff2_cont, Handler_pos::integer(), {val()}}
                      | {if_cont, Handler_pos::integer(), {Env::env(), A::exp(), B::exp()}}
                      | {test_zero_cont, Handler_pos::integer(), {}}
                      | {let_cont, Handler_pos::integer(), {env(), [var()], exp()}}
                      | {apply1_cont, Handler_pos::integer(), {Operands::[exp()], Env::env()}}
                      | {apply2_cont, Handler_pos::integer(), {val()}}
                      | {letrec_cont, Handler_pos::integer(), {env(), [var()], exp()}}
                      | {list_cont, Handler_pos::integer(), {}}
                      | {try_cont, Handler_pos::integer(), {Env::env(), V::var(), Handler_exp::exp()}}
                      | {raise_cont, Handler_pos::integer(), {}}
                      | {multiple_exps_as_list_cont, Handler_pos::integer(), {env(), exp(), [exp()], [val()]}}
                      | {list_finish_cont, Handler_pos::integer(), {[val()]}}
                      | {car_cont, Handler_pos::integer(), {}}
                      | {cdr_cont, Handler_pos::integer(), {}}
                      | {cons1_cont, Handler_pos::integer(), {Env::env(), E2::exp()}}
                      | {cons2_cont, Handler_pos::integer(), {val()}}
                      | {test_null_cont, Handler_pos::integer(), {}}.

-type cont() :: array:array(cont_element()).

-spec end_cont() -> cont().
end_cont() ->
    Conts = array:new(),
    array:set(0, {end_cont, -1, {}}, Conts).

-spec diff1_cont(cont(), env(), exp()) -> cont().
diff1_cont(Cont, Env, E2) ->
    Handler_pos = get_handler_pos(Cont),
    Cont_element = {diff1_cont, Handler_pos, {Env, E2}},
    push_cont(Cont_element, Cont).

-spec diff2_cont(cont(), val()) -> cont().
diff2_cont(Cont, Val1) ->
    Handler_pos = get_handler_pos(Cont),
    Cont_element = {diff2_cont, Handler_pos, {Val1}},
    push_cont(Cont_element, Cont).

-spec if_cont(cont(), env(), exp(), exp()) -> cont().
if_cont(Cont, Env, A, B) ->
    Handler_pos = get_handler_pos(Cont),
    Cont_element = {if_cont, Handler_pos, {Env, A, B}},
    push_cont(Cont_element, Cont).

-spec test_zero_cont(cont()) -> cont().
test_zero_cont(Cont) ->
    Handler_pos = get_handler_pos(Cont),
    Cont_element = {test_zero_cont, Handler_pos, {}},
    push_cont(Cont_element, Cont).

-spec let_cont(cont(), env(), [var()], exp()) -> cont().
let_cont(Cont, Env, Vars, Body) ->
    Handler_pos = get_handler_pos(Cont),
    Cont_element = {let_cont, Handler_pos, {Env, Vars, Body}},
    push_cont(Cont_element, Cont).

-spec apply1_cont(cont(), [exp()], env()) -> cont().
apply1_cont(Cont, Operands, Env) ->
    Handler_pos = get_handler_pos(Cont),
    Cont_element = {apply1_cont, Handler_pos, {Operands, Env}},
    push_cont(Cont_element, Cont).

-spec apply2_cont(cont(), val()) -> cont().
apply2_cont(Cont, Proc) ->
    Handler_pos = get_handler_pos(Cont),
    Cont_element = {apply2_cont, Handler_pos, {Proc}},
    push_cont(Cont_element, Cont).

-spec letrec_cont(cont(), env(), [var()], exp()) -> cont().
letrec_cont(Cont, Env, Proc_names, Body) ->
    Handler_pos = get_handler_pos(Cont),
    Cont_element = {letrec_cont, Handler_pos, {Env, Proc_names, Body}},
    push_cont(Cont_element, Cont).

-spec list_cont(cont()) -> cont().
list_cont(Cont) ->
    Handler_pos = get_handler_pos(Cont),
    Cont_element = {list_cont, Handler_pos, {}},
    push_cont(Cont_element, Cont).

-spec try_cont(cont(), env(), var(), exp()) -> cont().
try_cont(Cont, Env, V, Handler_exp) ->
    Len = array:size(Cont),
    Cont_element = {try_cont, Len, {Env, V, Handler_exp}},
    push_cont(Cont_element, Cont).

-spec raise_cont(cont()) -> cont().
raise_cont(Cont) ->
    Handler_pos = get_handler_pos(Cont),
    Cont_element = {raise_cont, Handler_pos, {}},
    push_cont(Cont_element, Cont).

-spec car_cont(cont()) -> cont().
car_cont(Cont) ->
    Handler_pos = get_handler_pos(Cont),
    Cont_element = {car_cont, Handler_pos, {}},
    push_cont(Cont_element, Cont).

-spec cdr_cont(cont()) -> cont().
cdr_cont(Cont) ->
    Handler_pos = get_handler_pos(Cont),
    Cont_element = {cdr_cont, Handler_pos, {}},
    push_cont(Cont_element, Cont).

-spec cons1_cont(cont(), env(), exp()) -> cont().
cons1_cont(Cont, Env, E2) ->
    Handler_pos = get_handler_pos(Cont),
    Cont_element = {cons1_cont, Handler_pos, {Env, E2}},
    push_cont(Cont_element, Cont).

-spec cons2_cont(cont(), val()) -> cont().
cons2_cont(Cont, Val1) ->
    Handler_pos = get_handler_pos(Cont),
    Cont_element = {cons2_cont, Handler_pos, {Val1}},
    push_cont(Cont_element, Cont).

%{test_null_cont, Handler_pos::integer(), {}}.
-spec test_null_cont(cont()) -> cont().
test_null_cont(Cont) ->
    Handler_pos = get_handler_pos(Cont),
    Cont_element = {test_null_cont, Handler_pos, {}},
    push_cont(Cont_element, Cont).

-spec multiple_exps_as_list_cont(cont(), env(), [exp()], [val()]) -> cont().
multiple_exps_as_list_cont(Cont, _Env, [], Acc_list) ->
    Handler_pos = get_handler_pos(Cont),
    Cont_element = {list_finish_cont, Handler_pos, {Acc_list}},
    push_cont(Cont_element, Cont);
multiple_exps_as_list_cont(Cont, Env, [Exp|Exps], Acc_list) ->
    Handler_pos = get_handler_pos(Cont),
    Cont_element = {multiple_exps_as_list_cont, Handler_pos, {Env, Exp, Exps, Acc_list}},
    push_cont(Cont_element, Cont).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec apply_cont(cont(), val()|[val()]) -> val().
apply_cont(Cont, Val) ->
    Len = array:size(Cont),
    Cont_element = array:get(Len-1, Cont),
    case Cont_element of
        {end_cont, _, _} ->
            io:format("end of computation.~n"),
            Val;
        {diff1_cont, _Hanlder_pos, {Env, E2}} ->
            Saved_cont = array:resize(Len-1, Cont),
            New_cont = diff2_cont(Saved_cont, Val),
            let_lang:eval(E2, Env, New_cont);
        {diff2_cont, _Handler_pos, {Val1}} ->
            Saved_cont = array:resize(Len-1, Cont),
            {num_val, N1} = Val1,
            {num_val, N2} = Val,
            apply_cont(Saved_cont, {num_val, N1-N2});
        {if_cont, _Handler_pos, {Env, A, B}} ->
            Saved_cont = array:resize(Len-1, Cont),
            {bool_val, Bool} = Val,
            Branch = case Bool of
                         true -> A;
                         false -> B
                     end,
            let_lang:eval(Branch, Env, Saved_cont);
        {test_zero_cont, _Handler_pos, {}} ->
            Saved_cont = array:resize(Len-1, Cont),
            {num_val, N} = Val,
            apply_cont(Saved_cont, {bool_val, N =:= 0});
        {let_cont, _Handler_pos, {Env, Vars, Body}} ->
            Saved_cont = array:resize(Len-1, Cont),
            New_env = env:extend_env_by_list(Env, lists:zip(Vars, Val)),
            let_lang:eval(Body, New_env, Saved_cont);
        {apply1_cont, _Handler_pos, {Operands, Env}} ->
            Saved_cont = array:resize(Len-1, Cont),
            New_cont = apply2_cont(Saved_cont, Val),
            let_lang:eval_multiple_exps_as_list(Operands, Env, New_cont);
        {apply2_cont, _Handler_pos, {{proc_val, Paras, Body, Lex_env}}} ->
            Saved_cont = array:resize(Len-1, Cont),
            case length(Val) =:= length(Paras) of
                true ->
                    New_env = env:extend_env_by_list(Lex_env, lists:zip(Paras, Val)),
                    let_lang:eval(Body, New_env, Saved_cont);
                false ->
                    let_lang:eval({raise_exp, {number, 128}}, env:empty_env(), Saved_cont)
            end;
        {list_cont, _Handler_pos, {}} ->
            Saved_cont = array:resize(Len-1, Cont),
            apply_cont(Saved_cont, {list_val, Val});
        {letrec_cont, _Handler_pos, {Env, Proc_names, Body}} ->
            Saved_cont = array:resize(Len-1, Cont),
            Name_procs = [{Proc_name, {Paras, Proc_body}}
                          || {Proc_name, {proc_val, Paras, Proc_body, _E}}
                                 <- lists:zip(Proc_names, Val)],
            New_env = env:extend_env_rec(Env, Name_procs),
            let_lang:eval(Body, New_env, Saved_cont);
        {try_cont, _L, _} ->
            Saved_cont = array:resize(Len-1, Cont),
            apply_cont(Saved_cont, Val);
        {raise_cont, _Handler_pos, {}} ->
            Saved_cont = array:resize(Len-1, Cont),
            apply_handler(Saved_cont, Val);
        {multiple_exps_as_list_cont, _Handler_pos, {Env, Exp, Exps, Acc_list}} ->
            Saved_cont = array:resize(Len-1, Cont),
            New_acc_list = [Val|Acc_list],
            New_cont = multiple_exps_as_list_cont(Saved_cont, Env, Exps, New_acc_list),
            let_lang:eval(Exp, Env, New_cont);
        {list_finish_cont, _Handler_pos, {Acc_list}} ->
            Saved_cont = array:resize(Len-1, Cont),
            Val_list = lists:reverse([Val|Acc_list]),
            apply_cont(Saved_cont, Val_list);
        {car_cont, _Handler_pos, {}} ->
            Saved_cont = array:resize(Len-1, Cont),
            {list_val, [Car|_]} = Val,
            apply_cont(Saved_cont, Car);
        {cdr_cont, _Handler_pos, {}} ->
            Saved_cont = array:resize(Len-1, Cont),
            {list_val, [_|Cdr]} = Val,
            apply_cont(Saved_cont, {list_val, Cdr});
        {cons1_cont, _Handler_pos, {Env, E2}} ->
            Saved_cont = array:resize(Len-1, Cont),
            New_cont = cons2_cont(Saved_cont, Val),
            let_lang:eval(E2, Env, New_cont);
        {cons2_cont, _Handler_pos, {Val1}} ->
            Saved_cont = array:resize(Len-1, Cont),
            {list_val, Lst} = {list_val, Val},
            Cons_val = {list_val, [Val1|Lst]},
            apply_cont(Saved_cont, Cons_val);
        {test_null_cont, _Handler_pos, {}} ->
            Saved_cont = array:resize(Len-1, Cont),
            {list_val, Lst} = Val,
            Bool = case Lst of
                       [] -> true;
                       _ -> false
                   end,
            apply_cont(Saved_cont, {bool_val, Bool})
    end.

-spec apply_handler(cont(), val()) -> val().
apply_handler(Cont, Val) ->
    Pos = get_handler_pos(Cont),
    case Pos >= 0 of
        true ->
            {try_cont, Pos, {Env, V, Handler_exp}} = array:get(Pos, Cont),
            New_env = env:extend_env(V, Val, Env),
            Saved_cont = array:resize(Pos, Cont),
            let_lang:eval(Handler_exp, New_env, Saved_cont);
        false ->
            erlang:error(Val)
    end.

%%%%%%%%%%%%%%%%%%%%%%
get_handler_pos(Cont) ->
    Len = array:size(Cont),
    {_Cont_name, Handler_pos, _} = array:get(Len-1, Cont),
    Handler_pos.

push_cont(Cont_element, Cont) ->
    Len = array:size(Cont),
    array:set(Len, Cont_element, Cont).
