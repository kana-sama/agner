-module(lists).

-export([reverse/1, map/2, flatmap/2, filter/2, words/1, lines/1, intersperse/2, foldl/3]).

reverse(List) -> reverse(List, []).
reverse([H|T], Acc) -> reverse(T, [H|Acc]);
reverse([], Acc) -> Acc.

map(F, List) -> map(F, List, Acc=[]).
map(F, [H|T], Acc) -> map(F, T, [F(H)|Acc]);
map(F, [], Acc) -> reverse(Acc).

flatmap(F, List) -> flatmap(F, List, Acc=[]).
flatmap(F, [H|T], Acc) -> flatmap(F, T, F(H) ++ Acc);
flatmap(F, [], Acc) -> reverse(Acc).

filter(F, List) -> filter(F, List, Acc=[]).
filter(F, [H|T], Acc) ->
  case F(H) of
    true -> filter(F, T, [H|Acc]);
    false -> filter(F, T, Acc)
  end;
filter(F, [], Acc) -> reverse(Acc).

words(S) -> reverse(words(S, [""])).
words(" " ++ S, Acc=[""|_]) -> words(S, Acc);
words(" " ++ S, [W|Acc]) -> words(S, ["", reverse(W)|Acc]);
words([C|S], [W|Acc]) -> words(S, [[C|W]|Acc]);
words([], [""|Acc]) -> Acc;
words([], [W|Acc]) -> [reverse(W)|Acc].

lines(S) -> reverse(lines(S, [""])).
lines("\n" ++ S, Acc=[""|_]) -> lines(S, Acc);
lines("\n" ++ S, [W|Acc]) -> lines(S, ["", reverse(W)|Acc]);
lines([C|S], [W|Acc]) -> lines(S, [[C|W]|Acc]);
lines([], [""|Acc]) -> Acc;
lines([], [W|Acc]) -> [reverse(W)|Acc].

intersperse(X, List) ->
  case intersperse(X, List, []) of
    [] -> [];
    [_|T] -> T
  end.
intersperse(X, [], Acc) -> reverse(Acc);
intersperse(X, [H|T], Acc) -> intersperse(X, T, [H,X|Acc]).

foldl(Fun, Acc, [H|T]) -> foldl(Fun, Fun(H, Acc), T);
foldl(Fun, Acc, []) -> Acc.
