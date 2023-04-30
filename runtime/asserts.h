# pragma once

# include "value.h"

typedef int64_t fun_kind_t; // FUN_KIND_STATIC | FUN_KIND_CLOSURE

void       _assert__bound(value_t, char* var);
void       _assert__bool(value_t);
fun_kind_t _assert__fun(value_t, int64_t arity); // arity == -1 -> any arity
void       _assert__map(value_t);
void       _assert__record(value_t, char* record_name, int64_t record_size);
