-module(chars).

is_upper(C) when erlang:is_integer(C) ->
  (C >= $A) and (C =< $Z).
