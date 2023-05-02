-module(maps).

-export([assert/1, new/0, put/3, update/3, from_list/1, to_list/1]).

wrap(M) -> agner:boxed_to_map({M}).
unwrap(M) -> {M_} = agner:map_to_boxed(M), M_.

assert(M) -> wrap(unwrap(M)).
new() -> wrap(maps_bintree:new()).
put(K, V, M) -> wrap(maps_bintree:put(K, V, unwrap(M))).
update(K, V, M) -> wrap(maps_bintree:update(K, V, unwrap(M))).
from_list(KVs) -> wrap(maps_bintree:from_list(KVs)).
to_list(M) -> maps_bintree:to_list(unwrap(M)).
