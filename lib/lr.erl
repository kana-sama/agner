-module(lr).

-record(ctor, {raw}).
-record(termin, {raw}).
-record(entity, {raw}).
-record(rule, {ctor, entity, points}).
-record(grammar, {in_order, rules}).

parse_ctor("_") -> unwrap;
parse_ctor(Raw) -> #ctor{raw=Raw}.

pretty_ctor(unwrap) -> [{fg, magenta, ["_"]}];
pretty_ctor(#ctor{raw=Raw}) -> [{fg, magenta, [Raw]}].

parse_point(Raw=[C|_]) when chars:is_upper(C) -> #entity{raw=Raw};
parse_point(Raw) -> #termin{raw=Raw}.

pretty_point(#termin{raw=Raw}) -> [{fg,  blue, [Raw]}];
pretty_point(#entity{raw=Raw}) -> [{fg, green, [Raw]}].

pretty_rule(Rule) -> [
  pretty_ctor(Rule#rule.ctor),
  {fg, red, ["\t. "]},
  pretty_point(Rule#rule.entity),
  {fg, red, ["\t= "]},
  lists:intersperse(" ", lists:map(fun pretty_point/1, Rule#rule.points))
].

parse_rule(S) ->
  [Ctor, ".", E, "=" | Points] = lists:words(S),
  #rule{
    ctor = parse_ctor(Ctor),
    entity = #entity{raw=E},
    points = lists:map(fun parse_point/1, Points)
  }.

grammar_from_rules(InOrder) ->
  Rules = [],
  #grammar{in_order = InOrder, rules = Rules}.

pretty_grammar(Grammar) ->
  lists:intersperse("\n", lists:map(fun pretty_rule/1, Grammar#grammar.in_order)).

parse_grammar(S) ->
  grammar_from_rules(lists:map(fun parse_rule/1, lists:lines(S))).

main() ->
  Grammar =
    " _    . S = Expr0

      _    . Expr0 = Expr1
      plus . Expr0 = Expr0 + Expr1

      _    . Expr1 = x
      _    . Expr1 = y
      _    . Expr1 = z
      _    . Expr1 = ( Expr0 ) ",
  sgr:println(pretty_grammar(parse_grammar(Grammar))),

  Toks = ["x"],
  ok.
