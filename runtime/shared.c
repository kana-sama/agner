# include "shared.h"

# include <stdio.h>

# include "value.h"
# include "tags.h"

# undef MAKE_SHARE
# define MAKE_SHARE(name) \
  static value_t _shared_##name = UNBOUND_VALUE; \
  \
  void share_##name(value_t value) { \
    _shared_##name = value; \
  } \
  \
  value_t shared_##name() { \
    if (_shared_##name == UNBOUND_VALUE) { \
      printf("Unshared " #name "\n"); \
      exit(-1); \
    } \
    return _shared_##name; \
  }


MAKE_SHARE(true)
MAKE_SHARE(false)
MAKE_SHARE(ok)
MAKE_SHARE(infinity)
MAKE_SHARE(throw)
MAKE_SHARE(error)
MAKE_SHARE(exit)

MAKE_SHARE(timeout_value)
MAKE_SHARE(badrecord)
