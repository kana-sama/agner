-module(main).
-export([main/0]).

-import(agner, [println/1]).

% same arity
f1(A) -> f2(A).
f2(A) -> println(A).

% from less to more
f3(A) -> f4(A, A + 1, A + 2).
f4(A, B, C) -> println({A, B, C}).

% from more to less
f5(A, B, C) -> f6(A + B + C).
f6(A) -> println(A).

even(0) -> true;
even(N) -> odd(N - 1).

odd(0) -> false;
odd(N) -> even(N - 1).

even_(0) -> true;
even_(N) -> (fun(X) -> odd_(X) end)(N - 1).

odd_(0) -> false;
odd_(N) -> (fun even/1)(N - 1).

main() ->
  f1(1),
  f3(1),
  f5(10, 100, 1000),
  println({even(10), odd(11)}),
  println({even_(10), odd_(11)}),
  ok.
