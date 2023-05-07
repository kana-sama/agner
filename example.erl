-module(main).
-export([main/0]).

main() ->
  agner:println(catch begin
    agner:println(before_throw),
    throw(exception),
    agner:println(after_throw)
  end),
  agner:println("HERE"),
  throw(42),
  agner:println(finally).
