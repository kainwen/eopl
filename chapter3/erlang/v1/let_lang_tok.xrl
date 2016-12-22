Definitions.


Rules.

if                       : {token, 'if'}.
then                     : {token, 'then'}.
else                     : {token, 'else'}.

zero[?]                  : {token, 'zero?'}.

let                      : {token, 'let'}.
=                        : {token, '='}.
in                       : {token, 'in'}.

unpack                   : {token, 'unpack'}.

cons                     : {token, 'cons'}.
emptylist                : {token, 'emptylist'}.

-                        : {token, '-'}.
[(),]                    : {token, list_to_atom(TokenChars)}.

[0-9]+                   : {token, {integer, list_to_integer(TokenChars)}}.
[a-zA-Z][a-zA-Z0-9]*     : {token, {id, list_to_atom(TokenChars)}}.

\t              : skip_token.
\n              : skip_token.
\s              : skip_token.


Erlang code.

