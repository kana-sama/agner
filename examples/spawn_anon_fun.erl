-module(main).
-export([main/0]).

main() ->
  A = [1,2,3,4,{5,ok}],
  spawn(fun() ->
    agner:println(A)
  end),
  garbage_collect().
