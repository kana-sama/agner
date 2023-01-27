# include <stdio.h>
# include <stdint.h>

# include "../src/Language/Agner/X64.h"

extern void _print_value(int64_t n) {
  int64_t tag = n & TAG_SIZE;
  switch (tag) {
    case NUMBER_TAG:
      printf("%lld\n", n >> 3);
      break;
    case UNBOUND_TAG:
      printf("unbound var");
      break;
    case ATOM_TAG:
      printf("%s\n", (char *) n);
      break;
  }
  fflush(stdout);
}
