# include "asserts.h"

# include "tags.h"
# include "throw.h"
# include "shared.h"

void _assert__bool(value_t value) {
  if (value == shared_true()) return;
  if (value == shared_false()) return;
  _throw__badarg_single(value);
}

void _assert__fun(value_t value, int64_t arity) {
  if ((value & TAG_MASK) != FUN_TAG)
    _throw__badfun(value);

  if (get_fun_meta(value)->arity != arity)
    _throw__badarity(value, arity);
}

void _assert__map(value_t value) {
  if ((value & TAG_MASK) != MAP_TAG)
    _throw__badmap(value);
}
