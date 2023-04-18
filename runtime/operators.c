# include "operators.h"

# include <stdbool.h>

# include "value.h"
# include "runtime.h"
# include "tags.h"
# include "throw.h"
# include "shared_atoms.h"



// booleans

value_t _unop__not(value_t value) {
  if (value == shared_atom_true()) return shared_atom_false();
  if (value == shared_atom_false()) return shared_atom_true();
  _throw__badarg_unop(value, "not");
}

value_t _binop__and(value_t l, value_t r) {
  if (l == shared_atom_true()  && r == shared_atom_true())  return shared_atom_true();
  if (l == shared_atom_false() && r == shared_atom_true())  return shared_atom_false();
  if (l == shared_atom_true()  && r == shared_atom_false()) return shared_atom_false();
  if (l == shared_atom_false() && r == shared_atom_false()) return shared_atom_false();
  _throw__badarg_binop(l, r, "and");
}

value_t _binop__or(value_t l, value_t r) {
  if (l == shared_atom_true()  && r == shared_atom_true())  return shared_atom_true();
  if (l == shared_atom_false() && r == shared_atom_true())  return shared_atom_true();
  if (l == shared_atom_true()  && r == shared_atom_false()) return shared_atom_true();
  if (l == shared_atom_false() && r == shared_atom_false()) return shared_atom_false();
  _throw__badarg_binop(l, r, "or");
}

value_t _binop__xor(value_t l, value_t r) {
  if (l == shared_atom_true()  && r == shared_atom_true())  return shared_atom_false();
  if (l == shared_atom_false() && r == shared_atom_true())  return shared_atom_true();
  if (l == shared_atom_true()  && r == shared_atom_false()) return shared_atom_true();
  if (l == shared_atom_false() && r == shared_atom_false()) return shared_atom_false();
  _throw__badarg_binop(l, r, "xor");
}



// numbers

value_t _unop__plus(value_t x) {
  if (is_integer(x)) return x;
  _throw__badarith_unary(x, "+");
}

value_t _unop__minus(value_t x) {
  if (is_integer(x)) return encode_integer(-decode_integer(x));
  _throw__badarith_unary(x, "-");
}

value_t _binop__plus(value_t l, value_t r) {
  if (is_integer(l) && is_integer(r)) return encode_integer(decode_integer(l) + decode_integer(r));
  _throw__badarith(l, r, "+");
}

value_t _binop__minus(value_t l, value_t r) {
  if (is_integer(l) && is_integer(r)) return encode_integer(decode_integer(l) - decode_integer(r));
  _throw__badarith(l, r, "-");
}

value_t _binop__times(value_t l, value_t r) {
  if (is_integer(l) && is_integer(r)) return encode_integer(decode_integer(l) * decode_integer(r));
  _throw__badarith(l, r, "*");
}

value_t _binop__div(value_t l, value_t r) {
  if (is_integer(l) && is_integer(r)) return encode_integer(decode_integer(l) / decode_integer(r));
  _throw__badarith(l, r, "div");
}

value_t _binop__rem(value_t l, value_t r) {
  if (is_integer(l) && is_integer(r)) return encode_integer(decode_integer(l) % decode_integer(r));
  _throw__badarith(l, r, "rem");
}



// bitwise

value_t _unop__bnot(value_t x) {
  if (is_integer(x)) return encode_integer(~decode_integer(x));
  _throw__badarith_unary(x, "-");
}

value_t _binop__band(value_t l, value_t r) {
  if (is_integer(l) && is_integer(r)) return encode_integer(decode_integer(l) & decode_integer(r));
  _throw__badarith(l, r, "band");
}

value_t _binop__bor(value_t l, value_t r) {
  if (is_integer(l) && is_integer(r)) return encode_integer(decode_integer(l) | decode_integer(r));
  _throw__badarith(l, r, "bor");
}

value_t _binop__bxor(value_t l, value_t r) {
  if (is_integer(l) && is_integer(r)) return encode_integer(decode_integer(l) ^ decode_integer(r));
  _throw__badarith(l, r, "bxor");
}

value_t _binop__bsl(value_t l, value_t r) {
  if (is_integer(l) && is_integer(r)) return encode_integer(decode_integer(l) << decode_integer(r));
  _throw__badarith(l, r, "bsl");
}

value_t _binop__bsr(value_t l, value_t r) {
  if (is_integer(l) && is_integer(r)) return encode_integer(decode_integer(l) >> decode_integer(r));
  _throw__badarith(l, r, "bsr");
}



// lists

value_t _binop__plusplus(value_t l, value_t r) {
  if (!(is_list(l))) _throw__badarg_binop(l, r, "++");

  int64_t result_is_list = is_list(r);
  
  // case []
  if (l == NIL_TAG) return r;

  // case [H|T]
  value_t new = _alloc__cons();
  boxed_value_t* ref = cast_to_boxed_value(l);
  boxed_value_t* cur = cast_to_boxed_value(new);

  while (true) {
    cur->cons.head = ref->cons.head;
    cur->cons.is_list = result_is_list;
    if (ref->cons.tail == NIL_TAG) {
      cur->cons.tail = r;
      break;
    } else {
      cur->cons.tail = _alloc__cons();
      cur = cast_to_boxed_value(cur->cons.tail);
      ref = cast_to_boxed_value(ref->cons.tail);
    }
  }

  return new;
}



// comparison

static inline value_t from_bool(bool b) { return b ? shared_atom_true() : shared_atom_false(); }

// ==
value_t _binop__eq_eq(value_t l, value_t r) {
  return from_bool(
    value_lte(l, r) && value_lte(r, l)
  );
}

// /=
value_t _binop__slash_eq(value_t l, value_t r) {
  return from_bool(
    !value_lte(l, r) || !value_lte(r, l)
  );
}

// =<
value_t _binop__eq_less(value_t l, value_t r) {
  return from_bool(
    value_lte(l, r)
  );
}

// <
value_t _binop__less(value_t l, value_t r) {
  return from_bool(
    value_lte(l, r) && !value_lte(r, l)
  );
}

// >=
value_t _binop__greater_eq(value_t l, value_t r) {
  return from_bool(
    value_lte(r, l)
  );
}

// >
value_t _binop__greater(value_t l, value_t r) {
  return from_bool(
    value_lte(r, l) && !value_lte(l, r)
  );
}

// =:=
value_t _binop__eq_colon_eq(value_t l, value_t r) {
  return _binop__eq_eq(l, r);
}

// =/=
value_t _binop__eq_slash_eq(value_t l, value_t r) {
  return _binop__slash_eq(l, r);
}
