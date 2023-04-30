-module(main).

ref(X) ->
  receive
    {set, Y}  -> ref(Y);
    {get, To} -> To ! X, ref(X);
    kill -> ok
  end.

new_ref(X) ->
  erlang:spawn(fun () -> ref(X) end).

set_ref(Ref, X) ->
  Ref ! {set, X}.

get_ref(Ref) ->
  Ref ! {get, erlang:self()},
  receive X -> X end.

kill_ref(Ref) ->
  Ref ! kill.

with_ref(Value, Body) ->
  Ref = new_ref(Value),
  Result = Body(Ref),
  kill_ref(Ref),
  Result.

main() ->
  with_ref(42, fun(A) ->
    agner:println(get_ref(A)),
    set_ref(A, 10),
    agner:println(get_ref(A))
  end).
