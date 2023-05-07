-module(main).
-export([main/0]).

main() ->
  catch throw(42),
  agner:println(finally).
