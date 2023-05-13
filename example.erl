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

main() ->
  test1(),
  test2().
