# pragma once

# include "value.h"

typedef fun_meta_t* bif_context_t;

value_t _agner__print__1(bif_context_t, value_t);
value_t _agner__println__1(bif_context_t, value_t);
value_t _agner__put_char__1(bif_context_t, value_t);
value_t _agner__put_str__1(bif_context_t, value_t);

// value_t _timer__sleep(bif_context_t, value_t);

value_t _erlang__spawn__1(bif_context_t, value_t);
value_t _erlang__self__0(bif_context_t);
value_t _erlang__send__2(bif_context_t, value_t, value_t);
value_t _erlang__garbage_collect__0(bif_context_t);

_Noreturn value_t _erlang__throw__1(bif_context_t, value_t);
_Noreturn value_t _erlang__error__1(bif_context_t, value_t);
_Noreturn value_t _agner__raise__2(bif_context_t, value_t class, value_t value);

// value_t _erlang__is_atom(bif_context_t, value_t);
value_t _erlang__is_integer__1(bif_context_t, value_t);
value_t _erlang__is_list__1(bif_context_t, value_t);
// value_t _erlang__is_tuple(bif_context_t, value_t);
// value_t _erlang__is_function(bif_context_t, value_t);
// value_t _erlang__is_pid(bif_context_t, value_t);

// value_t _erlang__atom_to_list(bif_context_t, value_t);
value_t _erlang__integer_to_list__1(bif_context_t, value_t);
// value_t _erlang__tuple_to_list(bif_context_t, value_t);
// value_t _erlang__fun_to_list(bif_context_t, value_t);
// value_t _erlang__pid_to_list(bif_context_t, value_t);

value_t _agner__map_to_boxed__1(bif_context_t, value_t);
value_t _agner__boxed_to_map__1(bif_context_t, value_t);
