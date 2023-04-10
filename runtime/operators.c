# include "operators.h"

# include "value.h"
# include "runtime.h"
# include "tags.h"
# include "throw.h"
# include "shared_atoms.h"



// booleans

value_t _unop__not(value_t value) {
  if (value == shared_atom_true()) return shared_atom_false();
  if (value == shared_atom_false()) return shared_atom_true();
  _THROW_badarg_unop(value, "not");
}

value_t _binop__and(value_t l, value_t r) {
  if (l == shared_atom_true()  && r == shared_atom_true())  return shared_atom_true();
  if (l == shared_atom_false() && r == shared_atom_true())  return shared_atom_false();
  if (l == shared_atom_true()  && r == shared_atom_false()) return shared_atom_false();
  if (l == shared_atom_false() && r == shared_atom_false()) return shared_atom_false();
  _THROW_badarg_binop(l, r, "and");
}

value_t _binop__or(value_t l, value_t r) {
  if (l == shared_atom_true()  && r == shared_atom_true())  return shared_atom_true();
  if (l == shared_atom_false() && r == shared_atom_true())  return shared_atom_true();
  if (l == shared_atom_true()  && r == shared_atom_false()) return shared_atom_true();
  if (l == shared_atom_false() && r == shared_atom_false()) return shared_atom_false();
  _THROW_badarg_binop(l, r, "or");
}

value_t _binop__xor(value_t l, value_t r) {
  if (l == shared_atom_true()  && r == shared_atom_true())  return shared_atom_false();
  if (l == shared_atom_false() && r == shared_atom_true())  return shared_atom_true();
  if (l == shared_atom_true()  && r == shared_atom_false()) return shared_atom_true();
  if (l == shared_atom_false() && r == shared_atom_false()) return shared_atom_false();
  _THROW_badarg_binop(l, r, "xor");
}



// numbers

value_t _unop__plus(value_t x) {
  if (is_integer(x)) return x;
  _THROW_badarith_unary(x, "+");
}

value_t _unop__minus(value_t x) {
  if (is_integer(x)) return encode_integer(-decode_integer(x));
  _THROW_badarith_unary(x, "-");
}

value_t _binop__plus(value_t l, value_t r) {
  if (is_integer(l) && is_integer(r)) return encode_integer(decode_integer(l) + decode_integer(r));
  _THROW_badarith(l, r, "+");
}

value_t _binop__minus(value_t l, value_t r) {
  if (is_integer(l) && is_integer(r)) return encode_integer(decode_integer(l) - decode_integer(r));
  _THROW_badarith(l, r, "-");
}

value_t _binop__times(value_t l, value_t r) {
  if (is_integer(l) && is_integer(r)) return encode_integer(decode_integer(l) * decode_integer(r));
  _THROW_badarith(l, r, "*");
}

value_t _binop__div(value_t l, value_t r) {
  if (is_integer(l) && is_integer(r)) return encode_integer(decode_integer(l) / decode_integer(r));
  _THROW_badarith(l, r, "div");
}

value_t _binop__rem(value_t l, value_t r) {
  if (is_integer(l) && is_integer(r)) return encode_integer(decode_integer(l) % decode_integer(r));
  _THROW_badarith(l, r, "rem");
}



// bitwise

value_t _unop__bnot(value_t x) {
  if (is_integer(x)) return encode_integer(~decode_integer(x));
  _THROW_badarith_unary(x, "-");
}

value_t _binop__band(value_t l, value_t r) {
  if (is_integer(l) && is_integer(r)) return encode_integer(decode_integer(l) & decode_integer(r));
  _THROW_badarith(l, r, "band");
}

value_t _binop__bor(value_t l, value_t r) {
  if (is_integer(l) && is_integer(r)) return encode_integer(decode_integer(l) | decode_integer(r));
  _THROW_badarith(l, r, "bor");
}

value_t _binop__bxor(value_t l, value_t r) {
  if (is_integer(l) && is_integer(r)) return encode_integer(decode_integer(l) ^ decode_integer(r));
  _THROW_badarith(l, r, "bxor");
}

value_t _binop__bsl(value_t l, value_t r) {
  if (is_integer(l) && is_integer(r)) return encode_integer(decode_integer(l) << decode_integer(r));
  _THROW_badarith(l, r, "bsl");
}

value_t _binop__bsr(value_t l, value_t r) {
  if (is_integer(l) && is_integer(r)) return encode_integer(decode_integer(l) >> decode_integer(r));
  _THROW_badarith(l, r, "bsr");
}



// lists

value_t _binop__plusplus(value_t l, value_t r) {
  if (!(is_list(l))) _THROW_badarg_binop(l, r, "++");

  int64_t result_is_list = is_list(r);
  
  // case []
  if (l == NIL_TAG) return r;

  // case [H|T]
  value_t new = _runtime__alloc_cons();
  boxed_value_t* ref = cast_to_boxed_value(l);
  boxed_value_t* cur = cast_to_boxed_value(new);

  while (true) {
    cur->cons.head = ref->cons.head;
    cur->cons.is_list = result_is_list;
    if (ref->cons.tail == NIL_TAG) {
      cur->cons.tail = r;
      break;
    } else {
      cur->cons.tail = _runtime__alloc_cons();
      cur = cast_to_boxed_value(cur->cons.tail);
      ref = cast_to_boxed_value(ref->cons.tail);
    }
  }

  return new;
}



// comparision

value_t _binop__gte(value_t l, value_t r) {
  return l >= r ? shared_atom_true() : shared_atom_false();
}

value_t _binop__lte(value_t l, value_t r) {
  return l <= r ? shared_atom_true() : shared_atom_false();
}