# pragma once

# include "value.h"

_Noreturn void _throw__unbound(char* var);
_Noreturn void _throw__badrecord(value_t value);
_Noreturn void _throw__badarity(value_t fun, int64_t args);
_Noreturn void _throw__badfun(value_t value);
_Noreturn void _throw__badmap(value_t value);
_Noreturn void _throw__function_clause(fun_meta_t* meta, value_t* args);
_Noreturn void _throw__badmatch(value_t value);
_Noreturn void _throw__badarith(value_t l, value_t r, char* op);
_Noreturn void _throw__badarith_unary(value_t x, char* op);
_Noreturn void _throw__badarg(fun_meta_t* meta, value_t* args);
_Noreturn void _throw__badarg_single(value_t arg);
_Noreturn void _throw__badarg_unop(value_t x, char* op);
_Noreturn void _throw__badarg_binop(value_t l, value_t r, char* op);
_Noreturn void _throw__case_clause(value_t);
