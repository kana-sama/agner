% -import(erlang, [is_list/1, is_integer/1, is_atom/1, is_tuple/1, is_function/1, is_pid/1]).
% -import(erlang, [integer_to_list/1, atom_to_list/1, tuple_to_list/1, pid_to_list/1]).

is_integer(Integer) -> erlang:is_integer(Integer).
is_atom(Atom) -> erlang:is_atom(Atom).
is_list(List) -> erlang:is_list(List).
is_tuple(Tuple) -> erlang:is_tuple(Tuple).
is_function(Function) -> erlang:is_function(Function).
is_pid(Pid) -> erlang:is_pid(Pid).
atom_to_list(Atom) -> erlang:atom_to_list(Atom).
integer_to_list(Integer) -> erlang:integer_to_list(Integer).
tuple_to_list(Tuple) -> erlang:tuple_to_list(Tuple).
pid_to_list(Pid) -> erlang:pid_to_list(Pid).

printable_latin1_list([C|Cs]) when is_integer(C), C >= $\040, C =< $\176 ->
  printable_latin1_list(Cs);
printable_latin1_list([C|Cs]) when is_integer(C), C >= $\240, C =< $\377 ->
  printable_latin1_list(Cs);
printable_latin1_list([$\n|Cs]) -> printable_latin1_list(Cs);
printable_latin1_list([$\r|Cs]) -> printable_latin1_list(Cs);
printable_latin1_list([$\t|Cs]) -> printable_latin1_list(Cs);
printable_latin1_list([$\v|Cs]) -> printable_latin1_list(Cs);
printable_latin1_list([$\b|Cs]) -> printable_latin1_list(Cs);
printable_latin1_list([$\f|Cs]) -> printable_latin1_list(Cs);
printable_latin1_list([$\e|Cs]) -> printable_latin1_list(Cs);
printable_latin1_list([]) -> true;
printable_latin1_list(_) -> false.

show_char($\n) -> "\\n";
show_char($\r) -> "\\r";
show_char($\t) -> "\\t";
show_char($\v) -> "\\v";
show_char($\b) -> "\\b";
show_char($\f) -> "\\f";
show_char($\e) -> "\\e";
show_char(C) -> [C].

show_string([]) -> "";
show_string([C|Cs]) -> show_char(C) ++ show_string(Cs).

show_values([]) -> "";
show_values([H]) -> show(H);
show_values([H|T]) when is_list(T) ->
  show(H) ++ "," ++ show_values(T);
show_values([H|T]) ->
  show(H) ++ "|" ++ show(T).

show(Atom) when is_atom(Atom) ->
  atom_to_list(Atom);
show(Integer) when is_integer(Integer) ->
  integer_to_list(Integer);
show(String) when printable_latin1_list(String) ->
  "\"" ++ show_string(String) ++ "\"";
show(List) when is_list(List) ->
  "[" ++ show_values(List) ++ "]";
show(Tuple) when is_tuple(Tuple) ->
  "{" ++ show_values(tuple_to_list(Tuple)) ++ "}";
show(Pid) when is_pid(Pid) ->
  pid_to_list(Pid);
show(Fun) when is_function(Fun) ->
  erlang:fun_to_list(Fun).

parse_format("~p" ++ Format) -> [value|parse_format(Format)];
parse_format("~s" ++ Format) -> [string|parse_format(Format)];
parse_format([C|Rest]) -> [{char, C}|parse_format(Rest)];
parse_format([]) -> [].

build([value|Format], [V|Values]) ->
  agner:put_str(show(V)), build(Format, Values);
build([string|Format], [V|Values]) ->
  agner:put_str(V), build(Format, Values);
build([{char, C}|Format], Values) ->
  agner:put_char(C), build(Format, Values);
build([], []) -> ok.

format(Format, Values) ->
  build(parse_format(Format), Values).

print_example(Value) ->
  format("show(~p) -> ~s\n", [Value, show(Value)]).

main() ->
  print_example(42),
  print_example(atom),
  print_example([1, hello]),
  print_example([1 | hello]),
  print_example("string"),
  print_example({1, hello, {world, {}}}),
  print_example(fun main/0),
  print_example(self()).