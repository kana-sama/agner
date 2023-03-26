# pragma once

# include "value.h"

value_t _runtime__calling_context[10];

value_t _agner__print(value_t);

value_t _timer__sleep(value_t);

value_t _erlang__error(value_t);
value_t _erlang__spawn(value_t);
value_t _erlang__self();
value_t _erlang__send(value_t, value_t);