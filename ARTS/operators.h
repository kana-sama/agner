# pragma once

# include "value.h"

value_t _unop__plus(value_t);
value_t _unop__minus(value_t);
value_t _unop__bnot(value_t);

value_t _binop__plus(value_t, value_t);
value_t _binop__minus(value_t, value_t);
value_t _binop__times(value_t, value_t);
value_t _binop__div(value_t, value_t);
value_t _binop__rem(value_t, value_t);

value_t _binop__band(value_t, value_t);
value_t _binop__bor(value_t, value_t);
value_t _binop__bxor(value_t, value_t);
value_t _binop__bsl(value_t, value_t);
value_t _binop__bsr(value_t, value_t);

value_t _binop__plusplus(value_t, value_t);
value_t _binop__gte(value_t, value_t, value_t, value_t);
value_t _binop__lte(value_t, value_t, value_t, value_t);
