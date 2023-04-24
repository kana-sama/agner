# pragma once

# include "value.h"

void _assert__bound(value_t, char* var);
void _assert__bool(value_t);
void _assert__fun(value_t, int64_t arity);
void _assert__map(value_t);
void _assert__record(value_t, char* record_name, int64_t record_size);
