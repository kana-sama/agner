-module(main).
-export([main/0]).
-import(lists, [map/2]).

main() ->
  agner:println(map(fun(X) -> X + 1 end, [1, 2, 3])).
