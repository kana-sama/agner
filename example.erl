-module(main).
-export([main/0]).
-import(lists, [map/2]).

-record(user, {name, age}).

main() ->
  agner:println(#user.name),
  agner:println(#user.age),
  agner:println(map(fun(X) -> X + 1 end, [1, 2, 3])).
