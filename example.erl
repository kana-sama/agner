-module(main).
-export([main/0]).

-import(agner, [println/1]).

take_messages(0) -> [];
take_messages(N) -> receive A -> [A | take_messages(N - 1)] end.

main() ->
  println(start),
  Main = self(),
  spawn(fun() -> timer:sleep(100), Main ! 1 end),
  spawn(fun() -> timer:sleep( 50), Main ! 2 end),
  spawn(fun() -> timer:sleep(150), Main ! 3 end),
  spawn(fun() -> timer:sleep( 20), Main ! 4 end),
  [4, 2, 1, 3] = take_messages(4).
