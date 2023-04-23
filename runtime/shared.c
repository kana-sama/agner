# include "shared.h"

# include <stdio.h>

# include "value.h"
# include "tags.h"

# undef MAKE_SHARE
# define MAKE_SHARE(name) \
  static value_t _shared_##name = UNBOUND_TAG; \
  \
  void share_##name(value_t value) { \
    _shared_##name = value; \
  } \
  \
  value_t shared_##name() { \
    if (_shared_##name == UNBOUND_TAG) { \
      printf("Unshared " #name "\n"); \
      exit(-1); \
    } \
    return _shared_##name; \
  }


MAKE_SHARE(true)
MAKE_SHARE(false)
MAKE_SHARE(ok)
MAKE_SHARE(infinity)
