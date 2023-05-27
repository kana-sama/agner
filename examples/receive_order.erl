-module(main).
-export([main/0]).

f(Pid) ->
  receive e -> ok end,
  receive
    a -> Pid ! {received, a};
    f -> Pid ! {received, f}
  end.

main() ->
  Self = self(),
  A = spawn(fun () -> f(Self) end),
  A ! a,
  A ! b,
  A ! c,
  A ! e,
  A ! f,
  a = receive {received, X} -> X end.
