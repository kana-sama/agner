# pragma once

# include "value.h"

// booleans
value_t _unop__not(value_t);
value_t _binop__and(value_t, value_t);
value_t _binop__or(value_t, value_t);
value_t _binop__xor(value_t, value_t);

// numbers
value_t _unop__plus(value_t);
value_t _unop__minus(value_t);
value_t _binop__plus(value_t, value_t);
value_t _binop__minus(value_t, value_t);
value_t _binop__times(value_t, value_t);
value_t _binop__div(value_t, value_t);
value_t _binop__rem(value_t, value_t);

// bitwise
value_t _unop__bnot(value_t);
value_t _binop__band(value_t, value_t);
value_t _binop__bor(value_t, value_t);
value_t _binop__bxor(value_t, value_t);
value_t _binop__bsl(value_t, value_t);
value_t _binop__bsr(value_t, value_t);

// lists
value_t _binop__plus_plus(value_t, value_t);
value_t _binop__minus_minus(value_t, value_t);

// comparison
value_t _binop__eq_eq(value_t, value_t);
value_t _binop__slash_eq(value_t, value_t);
value_t _binop__eq_less(value_t, value_t);
value_t _binop__less(value_t, value_t);
value_t _binop__greater_eq(value_t, value_t);
value_t _binop__greater(value_t, value_t);
value_t _binop__eq_colon_eq(value_t, value_t);
value_t _binop__eq_slash_eq(value_t, value_t);
