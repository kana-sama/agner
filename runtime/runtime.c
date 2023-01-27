# include <stdio.h>
# include <stdint.h>

# define WORD_SIZE 8
# define TAG_SIZE 3
# define TAG_MASK 0b111
# define NUMBER_TAG 0b000
# define UNBOUND_TAG 0b001
# define ATOM_TAG 0b010

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
