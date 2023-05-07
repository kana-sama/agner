-module(main).
-export([main/0]).

f() ->
  agner:println("hello").

main() ->
  A = [1,2,3,4,{5,ok}],
  spawn(fun() ->
    agner:println(A)
  end),
  garbage_collect(),
  spawn(fun f/0),
  ok.
