-module(main).
-export([main/0]).

ref(X) ->
  receive
    {set, Y}  -> ref(Y);
    {get, To} -> To ! X, ref(X);
    kill -> ok
  end.

new_ref(X) ->
  spawn(fun () -> ref(X) end).

set_ref(Ref, X) ->
  Ref ! {set, X}.

get_ref(Ref) ->
  Ref ! {get, self()},
  receive X -> X end.

kill_ref(Ref) ->
  Ref ! kill.

main() ->
  A = new_ref(42),
  agner:println(get_ref(A)),
  set_ref(A, 10),
  agner:println(get_ref(A)),
  kill_ref(A).
