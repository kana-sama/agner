# include "shared_atoms.h"

# include <stdio.h>

# include "value.h"

# undef MAKE_SHARE_FUNCTIONS
# define MAKE_SHARE_FUNCTIONS(name)        \
  static value_t _shared_atom_##name = 0;  \
                                           \
  void share_atom_##name(value_t value) {  \
    _shared_atom_##name = value;           \
  }                                        \
                                           \
  value_t shared_atom_##name() {           \
    if (_shared_atom_##name == 0) {        \
      printf("Unshared atom " #name "\n"); \
      exit(1);                             \
    }                                      \
    return _shared_atom_##name;            \
  }


MAKE_SHARE_FUNCTIONS(true)
MAKE_SHARE_FUNCTIONS(false)
MAKE_SHARE_FUNCTIONS(ok)
MAKE_SHARE_FUNCTIONS(infinity)
