# pragma once

# include "value.h"
# include "primitives.h"

// // booleans
value_t _agner__not__1(bif_context_t, value_t);
value_t _agner__and__2(bif_context_t, value_t, value_t);
// value_t _binop__or(bif_context_t, value_t, value_t);
// value_t _binop__xor(bif_context_t, value_t, value_t);

// // numbers
// value_t _unop__plus(bif_context_t, value_t);
// value_t _unop__minus(bif_context_t, value_t);
value_t _agner__plus__2(bif_context_t, value_t, value_t);
value_t _agner__minus__2(bif_context_t, value_t, value_t);
// value_t _binop__times(bif_context_t, value_t, value_t);
// value_t _binop__div(bif_context_t, value_t, value_t);
value_t _agner__rem__2(bif_context_t, value_t, value_t);

// // bitwise
// value_t _unop__bnot(bif_context_t, value_t);
// value_t _binop__band(bif_context_t, value_t, value_t);
// value_t _binop__bor(bif_context_t, value_t, value_t);
// value_t _binop__bxor(bif_context_t, value_t, value_t);
// value_t _binop__bsl(bif_context_t, value_t, value_t);
// value_t _binop__bsr(bif_context_t, value_t, value_t);

// // lists
value_t _binop__plus_plus__2(bif_context_t, value_t, value_t);
// value_t _binop__minus_minus(bif_context_t, value_t, value_t);

// // comparison
value_t _binop__eq_eq__2(bif_context_t, value_t, value_t);
// value_t _binop__slash_eq(bif_context_t, value_t, value_t);
value_t _binop__eq_less__2(bif_context_t, value_t, value_t);
value_t _binop__less__2(bif_context_t, value_t, value_t);
value_t _binop__greater_eq__2(bif_context_t, value_t, value_t);
value_t _binop__greater__2(bif_context_t, value_t, value_t);
// value_t _binop__eq_colon_eq(bif_context_t, value_t, value_t);
// value_t _binop__eq_slash_eq(bif_context_t, value_t, value_t);
