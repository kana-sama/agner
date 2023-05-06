-module(main).
-export([main/0]).
-import(lists, [map/2]).

-record(user, {name, age}).

f(#user{age = Age}) -> {Age, Name = 1}.

main() ->
  A = #user{name = "NAME", age = "AGE"},
  agner:println(A),
  agner:println(#user.name),
  agner:println(#user.age),
  agner:println(A#user.name),
  agner:println(A#user.age),
  agner:println({user, 1, 2}#user{name = "NOT NAME", age = "NOT AGE"}),
  agner:println(f(A)),
  ok.
