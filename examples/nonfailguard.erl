-module(main).
-export([main/0]).

-record(user, {name}).

f(A) when A#user.name == kana -> kana;
f(_) -> not_kana.

main() ->
  not_kana = f(1),
  not_kana = f(#user{name = not_kana}),
  kana = f(#user{name = kana}).
