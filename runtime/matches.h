# pragma once

# include <stdint.h>
# include <stdbool.h>

# include "value.h"

bool     _match__integer(value_t, int64_t i);
bool     _match__atom(value_t, value_t atom);
bool     _match__variable(value_t, value_t* mem);
value_t* _match__tuple(value_t, int64_t size);
bool     _match__nil(value_t);
value_t* _match__cons(value_t);
