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
  agner:println({user, 1, 2}#user{age = "NOT AGE", name = "NOT NAME"}),
  agner:println(f(A)),
  if
    A#user.name == "NAME2" -> agner:println("first");
    A#user.name == "NAME", A#user.age == "AGE", #user.age == 3 -> agner:println("second")
  end,
  ok.
