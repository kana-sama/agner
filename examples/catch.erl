-module(main).
-export([main/0]).

main() ->
  {'EXIT', {1, []}} = catch error(1),
  1 = catch throw(1),
  2 = catch 2.
