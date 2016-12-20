-module(polish_prefix).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

-type prefix_exp() :: [prefix_term()].

-type prefix_term() :: integer() | '-'.

-type prefix_exp_abt() :: integer() | diff_exp(_Op1, _Op2).

-type diff_exp(Op1, Op2) :: {diff, Op1, Op2}.

-type parse_result(Abt, Rems) :: {Abt, Rems}.

-spec parse_polish_prefix_help(prefix_exp()) -> parse_result(_A, _R).
parse_polish_prefix_help([]) -> {[], []};
parse_polish_prefix_help(['-'|Rems]) ->
    {Op1, R} = parse_polish_prefix_help(Rems),
    {Op2, RR} = parse_polish_prefix_help(R),
    {{diff, Op1, Op2}, RR};
parse_polish_prefix_help([N|Rems]) when is_integer(N) ->
    {N, Rems}.


-spec parse_polish_prefix(prefix_exp()) -> prefix_exp_abt().
parse_polish_prefix(Prefix_exp) ->
    {Result, _Rems} = parse_polish_prefix_help(Prefix_exp),
    Result.


-spec unparse_polish_prefix(prefix_exp_abt()) -> prefix_exp().
unparse_polish_prefix(N) when is_integer(N) -> [N];
unparse_polish_prefix({diff, Op1, Op2}) ->
    R = ['-'],
    R1 = unparse_polish_prefix(Op1),
    R2 = unparse_polish_prefix(Op2),
    lists:append(lists:append(R, R1), R2).

polish_test() ->
    [
     ?assert(unparse_polish_prefix(parse_polish_prefix(['-' , '-', 3, 2, '-', 4, '-', 12, 7])) =:= ['-' , '-', 3, 2, '-', 4, '-', 12, 7])
    ].
