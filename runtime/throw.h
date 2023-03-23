# pragma once

# include "value.h"

void _THROW_badarity(value_t fun, int64_t args);
void _THROW_badfun(value_t value);
void _THROW_function_clause(fun_meta_t* meta, value_t* args);
void _THROW_badmatch(value_t value);
void _THROW_badarith(value_t l, value_t r, char* op);