# pragma once

# include "value.h"

typedef fun_meta_t* bif_context_t;

value_t _agner__print(bif_context_t, value_t);
value_t _agner__println(bif_context_t, value_t);
value_t _agner__put_char(bif_context_t, value_t);
value_t _agner__put_str(bif_context_t, value_t);

value_t _timer__sleep(bif_context_t, value_t);

value_t _erlang__error(bif_context_t, value_t);
value_t _erlang__spawn(bif_context_t, value_t);
value_t _erlang__self(bif_context_t);
value_t _erlang__send(bif_context_t, value_t, value_t);
value_t _erlang__garbage_collect(bif_context_t);

value_t _erlang__is_atom(bif_context_t, value_t);
value_t _erlang__is_list(bif_context_t, value_t);
value_t _erlang__is_integer(bif_context_t, value_t);
value_t _erlang__is_tuple(bif_context_t, value_t);
value_t _erlang__is_function(bif_context_t, value_t);
value_t _erlang__is_pid(bif_context_t, value_t);

value_t _erlang__atom_to_list(bif_context_t, value_t);
value_t _erlang__integer_to_list(bif_context_t, value_t);
value_t _erlang__tuple_to_list(bif_context_t, value_t);
value_t _erlang__fun_to_list(bif_context_t, value_t);
value_t _erlang__pid_to_list(bif_context_t, value_t);
