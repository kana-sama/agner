# include "options.h"

# include <stdint.h>
# include <stdbool.h>
# include <stdio.h>
# include <stdlib.h>

options_t options;

static
uint64_t getenv_uint(char* var, uint64_t def) {
  char* value = getenv(var);
  if (value == NULL) return def;

  char* rest;
  int64_t parsed = strtoll(value, &rest, 10);

  if (rest == value || rest[0] != 0 || parsed < 0) {
    printf("Invalid value for %s, should be non-negative integer\n", var);
    exit(1);
  }
  
  return parsed;
}

static
bool getenv_bool(char* var, bool def) {
  char* value = getenv(var);
  if (value == NULL) return def;

  char* rest;
  int64_t parsed = strtoll(value, &rest, 10);

  if (rest == value || rest[0] != 0 || (parsed != 0 && parsed != 1)) {
    printf("Invalid value for %s, should be 0 or 1\n", var);
    exit(1);
  }
  
  return parsed == 1;
}

void read_env_options() {
  options.initial_heap   = getenv_uint("ARTS_HEAP"  , 64);
  options.initial_vstack = getenv_uint("ARTS_VSTACK", 64);
  options.initial_nstack = getenv_uint("ARTS_NSTACK", 64);
  options.fuel = getenv_uint("ARTS_FUEL", 4000);
  options.stat = getenv_bool("ARTS_STAT", false);
  options.ylog = getenv("ARTS_YLOG");
}
