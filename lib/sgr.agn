-module(sgr).

color_number(black) -> 0;
color_number(red) -> 1;
color_number(green) -> 2;
color_number(yellow) -> 3;
color_number(blue) -> 4;
color_number(magenta) -> 5;
color_number(cyan) -> 6;
color_number(white) -> 7.

code(N) -> [$\033] ++ "[" ++ erlang:integer_to_list(N) ++ "m".

reset() -> code(0).
fg(Color) -> code(30 + color_number(Color)).
bg(Color) -> code(40 + color_number(Color)).
bold(bold) -> code(1); bold(_) -> "".

% bold()

get_fg({Fg, _Bg, _Bold}) -> Fg.
get_bg({_Fg, Bg, _Bold}) -> Bg.
get_bold({_Fg, _Bg, Bold}) -> Bold.

set_fg({_Fg, Bg, Bold}, Fg)   -> {Fg, Bg, Bold}.
set_bg({Fg, _Bg, Bold}, Bg)   -> {Fg, Bg, Bold}.
set_bold({Fg, Bg, _Bold}, Bold) -> {Fg, Bg, Bold}.

apply_state({Fg, Bg, Bold}) ->
  reset()
    ++ case Fg   of none -> []; _ -> fg(Fg)   end
    ++ case Bg   of none -> []; _ -> bg(Bg)   end
    ++ case Bold of none -> []; _ -> bold(Bold) end.

node(State, {fg, Color, Content}) ->
  NewState = set_fg(State, Color),
  apply_state(NewState) ++ build(NewState, Content) ++ apply_state(State);
node(State, {bg, Color, Content}) ->
  NewState = set_bg(State, Color),
  apply_state(NewState) ++ build(NewState, Content) ++ apply_state(State);
node(State, {bold, Content}) ->
  NewState = set_bold(State, bold),
  apply_state(NewState) ++ build(NewState, Content) ++ apply_state(State);
node(State, []) -> [];
node(State, Cs=[C|_]) when erlang:is_integer(C) -> Cs;
node(State, Cs=[C|_]) -> build(State, Cs).

build(Markup) -> build({none, none, none}, Markup).
build(State, []) -> "";
build(State, [H|T]) ->
  node(State, H) ++ build(State, T).

println(Markup) -> agner:put_str(build(Markup)), agner:put_str("\n").
