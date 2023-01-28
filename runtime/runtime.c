# include <stdio.h>
# include <stdint.h>

# include "../src/Language/Agner/X64.h"

extern void _print_value(int64_t value) {
  switch (value & TAG_MASK) {
    case NUMBER_TAG:
      printf("%lld\n", value >> 3);
      break;
    case UNBOUND_TAG:
      printf("unbound var");
      break;
    case ATOM_TAG:
      printf("%s\n", (char *) value);
      break;
  }
  fflush(stdout);
}
