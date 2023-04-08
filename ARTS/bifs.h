# pragma once

# include "value.h"

value_t _agner__print(value_t);
value_t _agner__println(value_t);
value_t _agner__put_char(value_t);
value_t _agner__put_str(value_t);

value_t _timer__sleep(value_t);

value_t _erlang__error(value_t);
value_t _erlang__spawn(value_t);
value_t _erlang__self();
value_t _erlang__send(value_t, value_t);

value_t _erlang__is_atom__1(value_t);
value_t _erlang__is_list__1(value_t);
value_t _erlang__is_integer__1(value_t);
value_t _erlang__is_tuple__1(value_t);
value_t _erlang__is_function__1(value_t);
value_t _erlang__is_pid__1(value_t);

value_t _erlang__atom_to_list__1(value_t);
value_t _erlang__integer_to_list__1(value_t);
value_t _erlang__tuple_to_list__1(value_t);
value_t _erlang__fun_to_list__1(value_t);
value_t _erlang__pid_to_list__1(value_t);
