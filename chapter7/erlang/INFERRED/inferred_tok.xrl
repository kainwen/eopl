Definitions.


Rules.

proc                     : {token, 'proc'}.

if                       : {token, 'if'}.
then                     : {token, 'then'}.
else                     : {token, 'else'}.

zero[?]                  : {token, 'zero?'}.

let                      : {token, 'let'}.
=                        : {token, '='}.
in                       : {token, 'in'}.

letrec                   : {token, 'letrec'}.

[{]                      : {token, '{'}.
[}]                      : {token, '}'}.
match_tuple              : {token, 'match_tuple'}.

list                     : {token, 'list'}.
cons                     : {token, 'cons'}.
cdr                      : {token, 'cdr'}.
null[?]                  : {token, 'null?'}.

int                      : {token, 'int'}.
bool                     : {token, 'bool'}.
->                       : {token, '->'}.
[*]                      : {token, '*'}.
[[]                      : {token, '['}.
[]]                      : {token, ']'}.
:                        : {token, ':'}.

-                        : {token, '-'}.
[(),]                    : {token, list_to_atom(TokenChars)}.



-?[0-9]+                 : {token, {integer, list_to_integer(TokenChars)}}.
[_a-zA-Z][_a-zA-Z0-9]*     : {token, {id, list_to_atom(TokenChars)}}.

\t                       : skip_token.
\n                       : skip_token.
\s                       : skip_token.


Erlang code.
