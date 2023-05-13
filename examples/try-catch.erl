-module(main).
-export([main/0]).

test1() ->
  success = try
    % throw(second),
    success
  catch
    error:first -> catched_first;
    second -> catched_second
  end,
  agner:println("test1 - ok").

test2() ->
  catched_second = try
    throw(second),
    success
  catch
    error:first -> catched_first;
    second -> catched_second
  end,
  agner:println("test2 - ok").

test3() ->
  2 = catch
    try throw(1)
    catch
      throw:1 ->
        throw(2)
    end.

main() ->
  test1(),
  test2(),
  test3().
