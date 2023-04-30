-module(main).

main() ->
  agner:println(maps:to_list(
    #{ Y => X || K := X <- #{a => 1, b => 2}, Y <- [c, d] }
  )).
  
