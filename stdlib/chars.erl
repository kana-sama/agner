-module(chars).

-export([is_upper/1]).

is_upper(C) when erlang:is_integer(C) ->
  (C >= $A) and (C =< $Z).
