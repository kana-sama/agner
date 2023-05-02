-module(sets).

-export([new/0, is_element/2, add_element/2, union/2]).

new() -> [].

is_element(X, [X|_]) -> true;
is_element(X, [_|T]) -> is_element(X, T);
is_element(X, []) -> false.

add_element(X, Set) ->
  case is_element(X, Set) of
    true -> Set;
    false -> [X|Set]
  end.

union(Set1, Set2) ->
  lists:foldl(fun add_element/2, Set2, Set1).
