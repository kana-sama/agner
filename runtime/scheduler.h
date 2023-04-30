# pragma once

# undef _FORTIFY_SOURCE
# define _FORTIFY_SOURCE 0

# include "containers/list.h"
# include "process.h"

typedef void (*action_t)(process_t* process, void* arg);

typedef struct scheduler_t {
  list_t*    queue;
  list_t*    to_release;
  process_t* current;
  jmp_buf*   exit;
  int64_t    fuel;
} scheduler_t;

scheduler_t* scheduler_new();
process_t*   scheduler_spawn(scheduler_t*, action_t, void*);
void scheduler_free(scheduler_t*);
void scheduler_next(scheduler_t*);
void scheduler_yield(scheduler_t*);
void scheduler_run(scheduler_t*, action_t, void*);
