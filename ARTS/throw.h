# pragma once

# include "value.h"

_Noreturn void _THROW_badarity(value_t fun, int64_t args);
_Noreturn void _THROW_badfun(value_t value);
_Noreturn void _THROW_function_clause(fun_meta_t* meta, value_t* args);
_Noreturn void _THROW_badmatch(value_t value);
_Noreturn void _THROW_badarith(value_t l, value_t r, char* op);
_Noreturn void _THROW_badarg_send(value_t l, value_t r);