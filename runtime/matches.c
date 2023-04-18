# include "matches.h"
# include "tags.h"

bool _match__integer(value_t value, int64_t i) {
  return encode_integer(i) == value;
}

bool _match__atom(value_t value, value_t atom) {
  return value == atom;
}

bool _match__variable(value_t value, value_t* mem) {
  if (*mem == UNBOUND_TAG) {
    *mem = value;
    return true;
  } else {
    return *mem == value;
  }
}

value_t* _match__tuple(value_t value, int64_t size) {
  boxed_value_t* ref = cast_to_boxed_value(value);
  if (ref && ref->super.header == TUPLE_HEADER && ref->tuple.size == size) {
    return ref->tuple.values;
  } else {
    return NULL;
  }
}

bool _match__nil(value_t value) {
  return value == NIL_TAG;
}

value_t* _match__cons(value_t value) {
  boxed_value_t* ref = cast_to_boxed_value(value);
  if (ref && ref->super.header == CONS_HEADER) {
    return &ref->cons.head;
  } else {
    return NULL;
  }
}
