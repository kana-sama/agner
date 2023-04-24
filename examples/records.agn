-module(main).

-record(user, {name, age}).

main() ->
  A = #user{name = kana},
  agner:println(A#user.name), % kana
  agner:println(A#user.age),  % undefined
  agner:println(A),           % {user, kana, undefined}

  B = A#user{age = 25},
  agner:println(B),           % {user, kana, 25}
  
  #user{age = Age} = B,
  agner:println(Age).         % 25
