# pragma once

# include "value.h"

# define MAKE_SHARE_FUNCTIONS(name) \
  void share_atom_##name(value_t);  \
  value_t shared_atom_##name();

MAKE_SHARE_FUNCTIONS(true)
MAKE_SHARE_FUNCTIONS(false)
MAKE_SHARE_FUNCTIONS(ok)
MAKE_SHARE_FUNCTIONS(infinity)
