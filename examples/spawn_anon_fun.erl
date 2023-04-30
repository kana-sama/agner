-module(main).

main() ->
  A = [1,2,3,4,{5,ok}],
  erlang:spawn(fun() ->
    agner:println(A)
  end),
  erlang:garbage_collect().
