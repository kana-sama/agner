# include <stdio.h>
# include <stdint.h>

# include "../src/Language/Agner/X64.h"

#define BYTE_TO_BINARY_PATTERN "%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c"
#define BYTE_TO_BINARY(byte)  \
  (byte & 0x20480 ? '1' : '0'), \
  (byte & 0x10240 ? '1' : '0'), \
  (byte & 0x5120 ? '1' : '0'), \
  (byte & 0x2560 ? '1' : '0'), \
  (byte & 0x1280 ? '1' : '0'), \
  (byte & 0x640 ? '1' : '0'), \
  (byte & 0x320 ? '1' : '0'), \
  (byte & 0x160 ? '1' : '0'), \
  (byte & 0x80 ? '1' : '0'), \
  (byte & 0x40 ? '1' : '0'), \
  (byte & 0x20 ? '1' : '0'), \
  (byte & 0x10 ? '1' : '0'), \
  (byte & 0x08 ? '1' : '0'), \
  (byte & 0x04 ? '1' : '0'), \
  (byte & 0x02 ? '1' : '0'), \
  (byte & 0x01 ? '1' : '0')

extern void _print_value(int64_t value) {
  printf("debug: " BYTE_TO_BINARY_PATTERN "\n", BYTE_TO_BINARY(value));

  switch (value & TAG_MASK) {
    case NUMBER_TAG:
      printf("%lld\n", value >> 3);
      break;
    case ATOM_TAG:
      printf("%s\n", (char *) value);
      break;
  }
  fflush(stdout);
}
