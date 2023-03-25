# pragma once

# include <stdint.h>
# include <stdbool.h>

typedef struct options_t {
  uint64_t initial_heap;
  uint64_t initial_vstack;
  uint64_t initial_nstack;
  uint64_t fuel;
  bool     stat;
  char*    ylog;
} options_t;

options_t options;

void read_env_options();