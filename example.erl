-module(main).

main() ->
  agner:println(maps:to_list(#{a => 1, b => two})),
  agner:println([ {X, Y} || X <- [1, 2, 3], X rem 2 == 1, Y <- [a, b] ]).
  
