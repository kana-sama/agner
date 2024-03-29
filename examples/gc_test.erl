-module(main).
-export([main/0]).

f(N) -> f(N, 0).
f(0, Acc) -> Acc;
f(N, Acc) ->
  f(N-1, Acc + sum(replicate(10000))).

replicate(N) -> replicate(N, []).
replicate(0, Acc) -> Acc;
replicate(N, Acc) -> replicate(N-1, [N|Acc]).

sum(List) -> sum(List, 0).
sum([H|T], Acc) -> sum(T, Acc + H);
sum([], Acc) -> Acc.

main() ->
  agner:println(f(100)).
