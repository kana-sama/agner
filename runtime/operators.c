# include "operators.h"

# include <stdbool.h>

# include "value.h"
# include "runtime.h"
# include "tags.h"
# include "throw.h"
# include "containers/list.h"
# include "shared.h"



// // booleans

// value_t _unop__not(bif_context_t ctx, value_t value) {
//   if (value == shared_true()) return shared_false();
//   if (value == shared_false()) return shared_true();
//   _throw__badarg_unop(value, "not");
// }

// value_t _binop__and(bif_context_t ctx, value_t l, value_t r) {
//   if (l == shared_true()  && r == shared_true())  return shared_true();
//   if (l == shared_false() && r == shared_true())  return shared_false();
//   if (l == shared_true()  && r == shared_false()) return shared_false();
//   if (l == shared_false() && r == shared_false()) return shared_false();
//   _throw__badarg_binop(l, r, "and");
// }

// value_t _binop__or(bif_context_t ctx, value_t l, value_t r) {
//   if (l == shared_true()  && r == shared_true())  return shared_true();
//   if (l == shared_false() && r == shared_true())  return shared_true();
//   if (l == shared_true()  && r == shared_false()) return shared_true();
//   if (l == shared_false() && r == shared_false()) return shared_false();
//   _throw__badarg_binop(l, r, "or");
// }

// value_t _binop__xor(bif_context_t ctx, value_t l, value_t r) {
//   if (l == shared_true()  && r == shared_true())  return shared_false();
//   if (l == shared_false() && r == shared_true())  return shared_true();
//   if (l == shared_true()  && r == shared_false()) return shared_true();
//   if (l == shared_false() && r == shared_false()) return shared_false();
//   _throw__badarg_binop(l, r, "xor");
// }



// // numbers

// value_t _unop__plus(bif_context_t ctx, value_t x) {
//   if (is_integer(x)) return x;
//   _throw__badarith_unary(x, "+");
// }

// value_t _unop__minus(bif_context_t ctx, value_t x) {
//   if (is_integer(x)) return encode_integer(-decode_integer(x));
//   _throw__badarith_unary(x, "-");
// }

value_t _agner__plus__2(bif_context_t ctx, value_t l, value_t r) {
  if (is_integer(l) && is_integer(r)) return encode_integer(decode_integer(l) + decode_integer(r));
  _throw__badarith(l, r, "+");
}

// value_t _binop__minus(bif_context_t ctx, value_t l, value_t r) {
//   if (is_integer(l) && is_integer(r)) return encode_integer(decode_integer(l) - decode_integer(r));
//   _throw__badarith(l, r, "-");
// }

// value_t _binop__times(bif_context_t ctx, value_t l, value_t r) {
//   if (is_integer(l) && is_integer(r)) return encode_integer(decode_integer(l) * decode_integer(r));
//   _throw__badarith(l, r, "*");
// }

// value_t _binop__div(bif_context_t ctx, value_t l, value_t r) {
//   if (is_integer(l) && is_integer(r)) return encode_integer(decode_integer(l) / decode_integer(r));
//   _throw__badarith(l, r, "div");
// }

// value_t _binop__rem(bif_context_t ctx, value_t l, value_t r) {
//   if (is_integer(l) && is_integer(r)) return encode_integer(decode_integer(l) % decode_integer(r));
//   _throw__badarith(l, r, "rem");
// }



// // bitwise

// value_t _unop__bnot(bif_context_t ctx, value_t x) {
//   if (is_integer(x)) return encode_integer(~decode_integer(x));
//   _throw__badarith_unary(x, "-");
// }

// value_t _binop__band(bif_context_t ctx, value_t l, value_t r) {
//   if (is_integer(l) && is_integer(r)) return encode_integer(decode_integer(l) & decode_integer(r));
//   _throw__badarith(l, r, "band");
// }

// value_t _binop__bor(bif_context_t ctx, value_t l, value_t r) {
//   if (is_integer(l) && is_integer(r)) return encode_integer(decode_integer(l) | decode_integer(r));
//   _throw__badarith(l, r, "bor");
// }

// value_t _binop__bxor(bif_context_t ctx, value_t l, value_t r) {
//   if (is_integer(l) && is_integer(r)) return encode_integer(decode_integer(l) ^ decode_integer(r));
//   _throw__badarith(l, r, "bxor");
// }

// value_t _binop__bsl(bif_context_t ctx, value_t l, value_t r) {
//   if (is_integer(l) && is_integer(r)) return encode_integer(decode_integer(l) << decode_integer(r));
//   _throw__badarith(l, r, "bsl");
// }

// value_t _binop__bsr(bif_context_t ctx, value_t l, value_t r) {
//   if (is_integer(l) && is_integer(r)) return encode_integer(decode_integer(l) >> decode_integer(r));
//   _throw__badarith(l, r, "bsr");
// }



// // lists

// value_t _binop__plus_plus(bif_context_t ctx, value_t l_, value_t r_) {
//   enter_scope();

//   value_t* l = add_to_scope(l_);
//   value_t* r = add_to_scope(r_);

//   if (!(is_proper_list(*l))) _throw__badarg_binop(*l, *r, "++");

//   list_t* values = list_reverse(proper_list_values(*l));
//   while (!list_null(values))
//     *r = _alloc__cons((value_t)list_shift(values), *r);
//   list_free(values);

//   value_t result = *r;
//   leave_scope();
//   return result;
// }

// value_t _binop__minus_minus(bif_context_t ctx, value_t l, value_t r) {
//   puts("--/2 is not defined");
//   exit(-1);
// }


// // comparison

// static inline value_t from_bool(bool b) { return b ? shared_true() : shared_false(); }

// // ==
// value_t _binop__eq_eq(bif_context_t ctx, value_t l, value_t r) {
//   return from_bool(
//     value_lte(l, r) && value_lte(r, l)
//   );
// }

// // /=
// value_t _binop__slash_eq(bif_context_t ctx, value_t l, value_t r) {
//   return from_bool(
//     !value_lte(l, r) || !value_lte(r, l)
//   );
// }

// // =<
// value_t _binop__eq_less(bif_context_t ctx, value_t l, value_t r) {
//   return from_bool(
//     value_lte(l, r)
//   );
// }

// // <
// value_t _binop__less(bif_context_t ctx, value_t l, value_t r) {
//   return from_bool(
//     value_lte(l, r) && !value_lte(r, l)
//   );
// }

// // >=
// value_t _binop__greater_eq(bif_context_t ctx, value_t l, value_t r) {
//   return from_bool(
//     value_lte(r, l)
//   );
// }

// // >
// value_t _binop__greater(bif_context_t ctx, value_t l, value_t r) {
//   return from_bool(
//     value_lte(r, l) && !value_lte(l, r)
//   );
// }

// // =:=
// value_t _binop__eq_colon_eq(bif_context_t ctx, value_t l, value_t r) {
//   return from_bool(
//     value_lte(l, r) && value_lte(r, l)
//   );
// }

// // =/=
// value_t _binop__eq_slash_eq(bif_context_t ctx, value_t l, value_t r) {
//   return from_bool(
//     !value_lte(l, r) || !value_lte(r, l)
//   );
// }
