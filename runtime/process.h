# pragma once

# include <stdio.h>
# include <stdlib.h>
# include <stdbool.h>
# include <setjmp.h>

# include "value.h"
# include "heap.h"
# include "mailbox.h"
# include "scopes.h"

typedef int64_t PID_t;
typedef void (*action_t)();

typedef struct process_t {
  PID_t      pid;
  heap_t*    heap;
  char*      stack;
  char*      stack_beg;
  value_t*   vstack;
  value_t*   vstack_head;
  mailbox_t* mailbox;
  scopes_t*  scopes;
  jmp_buf*   context;
  bool       is_alive;
} process_t;

process_t* process_new();
void       process_free(process_t*);
process_t* process_lookup(PID_t);
gc_ctx_t   process_gc_ctx(process_t*);
void       process_send(PID_t, value_t);
