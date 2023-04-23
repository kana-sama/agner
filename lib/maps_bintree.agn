-module(maps_bintree).

new() ->
  empty.

put(NK, NV, empty) ->
  {node, empty, empty, NK, NV};
put(NK, NV, {node, L, R, K, V}) -> if
  NK == K -> {node, L, R, K, NV};
  NK <  K -> {node, put(NK, NV, L), R, K, V};
  NK >  K -> {node, L, put(NK, NV, R), K, V}
end.

update(NK, NV, {node, L, R, K, V}) -> if
  NK == K -> {node, L, R, K, NV};
  NK <  K -> {node, update(NK, NV, L), R, K, V};
  NK >  K -> {node, L, update(NK, NV, R), K, V}
end.

from_list(KVs) ->
  from_list(KVs, new()).
from_list([{K, V}|KVs], Acc) ->
  from_list(KVs, put(K, V, Acc));
from_list([], Acc) -> Acc.

to_list(empty) -> [];
to_list({node, L, R, K, V}) ->
  [{K, V}] ++ (to_list(L) ++ to_list(R)).
