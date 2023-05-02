-module(main).
-export([main/0]).

example(X) ->
  agner:println(maybe
    {ok, A} ?= {ok, 1},
    {ok, B} ?= X,
    {ok, C} ?= {ok, A + B},
    A + B + C
  end).

example_with_else(X) ->
  agner:println(
    maybe
      {ok, A} ?= {ok, 1},
      {ok, B} ?= X,
      {ok, C} ?= {ok, A + B},
      A + B + C
    else
      err -> plain_error;
      {err, V} -> {error_with_value, V}
    end
  ).

main() ->
  example({ok, 2}),
  example({err, 2}),
  example_with_else({ok, 2}),
  example_with_else(err),
  example_with_else({err, 2}),
  example_with_else("unknown error"),
  ok.
  
