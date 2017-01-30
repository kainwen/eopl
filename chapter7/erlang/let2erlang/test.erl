-module(test).
-export([main/0]).

main () ->
F0 = fun ( X_1 ) ->
F1 = 1,
F2 = X_1 - F1,
F2
end,
X_0 = F0,
F3 = 1,
F4 = X_0(F3),
F4.