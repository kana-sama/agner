# pragma once

# include "value.h"

# define MAKE_SHARE(name) \
  void share_##name(value_t); \
  value_t shared_##name();


MAKE_SHARE(true)
MAKE_SHARE(false)
MAKE_SHARE(ok)
MAKE_SHARE(infinity)
MAKE_SHARE(throw)
MAKE_SHARE(error)
MAKE_SHARE(exit)

MAKE_SHARE(badrecord)
