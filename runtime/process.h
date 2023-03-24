# pragma once

# include <stdio.h>
# include <stdlib.h>
# include <stdbool.h>
# include <setjmp.h>

# include "value.h"
# include "heap.h"

typedef int64_t PID_t;
typedef void (*action_t)();

typedef struct process_t {
  PID_t         pid;
  heap_t*       heap;
  char*         stack;
  char*         stack_beg;
  value_t*      vstack;
  jmp_buf*      context;
  bool          is_alive;
} process_t;

process_t* process_new();
void       process_free(process_t* process);
process_t* process_lookup(PID_t pid);
