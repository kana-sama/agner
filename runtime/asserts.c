# include "asserts.h"

# include "tags.h"
# include "throw.h"
# include "shared.h"

void _assert__bound(value_t value, char* var) {
  if (value == UNBOUND_TAG) {
    _throw__unbound(var);
    exit(-1);
  }
}

void _assert__bool(value_t value) {
  if (value == shared_true()) return;
  if (value == shared_false()) return;
  _throw__badarg_single(value);
}

fun_kind_t _assert__fun(value_t value, int64_t arity) {
  bool should_check_arity = arity != -1;

  if ((value & TAG_MASK) == FUN_TAG)
    if (!should_check_arity || should_check_arity && get_fun_meta(value)->arity == arity)
      return FUN_KIND_STATIC;

  boxed_value_t* ref = cast_to_boxed_value(value);
  if (ref && ref->super.header == CLOSURE_HEADER)
    if (!should_check_arity || should_check_arity && get_fun_meta(ref->closure.body)->arity == arity)
      return FUN_KIND_CLOSURE;

  _throw__badfun(value);
}

void _assert__map(value_t value) {
  if ((value & TAG_MASK) != MAP_TAG)
    _throw__badmap(value);
}

void _assert__record(value_t value, char* record_name, int64_t record_size) {
  boxed_value_t* ref = cast_to_boxed_value(value);

  if (!ref)
    _throw__badrecord(value);

  if (ref->super.header != TUPLE_HEADER)
    _throw__badrecord(value);

  if (ref->tuple.size != record_size + 1)
    _throw__badrecord(value);

  if (ref->tuple.values[0] != (value_t)record_name)
    _throw__badrecord(value);
}
