# include <stdio.h>

# include "../src/Language/Agner/X64.h"

typedef int64_t Value;

Value RUNTIME_call_context;

extern void _print_value(const int64_t value) {
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

extern void _print_error_message(const char * msg) {
  printf("error 🌚: %s\n", msg);
}


// BiFs

// RUNTIME_call_context should be "ok" atom
extern Value _agner__print(const Value value) {
  _print_value(value);
  return RUNTIME_call_context;
}