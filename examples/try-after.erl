-module(main).
-export([main/0]).

test1() ->
  catched = catch
    try throw(err), success
    catch err -> catched
    after self() ! after_done
    end,
  receive after_done -> ok end.


test2() ->
  success = catch
    try success
    after self() ! after_done
    end,
    receive after_done -> ok end.

test3() ->
  in_after = catch
    try throw(in_try)
    after throw(in_after)
    end.

test4() ->
  in_after = catch
    try throw(in_try)
    catch in_try -> catched
    after throw(in_after)
    end.

main() ->
  test1(),
  test2(),
  test3(),
  test4().
