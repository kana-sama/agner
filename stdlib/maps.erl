-module(maps).

wrap(M) -> agner:boxed_to_map({M}).
unwrap(M) -> {M_} = agner:map_to_boxed(M), M_.

assert(M) -> wrap(unwrap(M)).
new() -> wrap(maps_bintree:new()).
put(K, V, M) -> wrap(maps_bintree:put(K, V, unwrap(M))).
update(K, V, M) -> wrap(maps_bintree:update(K, V, unwrap(M))).
from_list(KVs) -> wrap(maps_bintree:from_list(KVs)).
to_list(M) -> maps_bintree:to_list(unwrap(M)).
