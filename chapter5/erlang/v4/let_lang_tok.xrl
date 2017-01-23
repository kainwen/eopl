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

try                      : {token, 'try'}.
catch                    : {token, 'catch'}.
raise                    : {token, 'raise'}.
invoke_cont              : {token, 'invoke_cont'}.

letcc                    : {token, 'letcc'}.
throw                    : {token, 'throw'}.
to                       : {token, 'to'}.

list                     : {token, 'list'}.
car                      : {token, 'car'}.
cdr                      : {token, 'cdr'}.
cons                     : {token, 'cons'}.
null[?]                  : {token, 'null?'}.

-                        : {token, '-'}.
[(),]                    : {token, list_to_atom(TokenChars)}.

-?[0-9]+                   : {token, {integer, list_to_integer(TokenChars)}}.
[a-zA-Z][a-zA-Z0-9]*     : {token, {id, list_to_atom(TokenChars)}}.

\t              : skip_token.
\n              : skip_token.
\s              : skip_token.


Erlang code.
