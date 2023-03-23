# pragma once

# undef _FORTIFY_SOURCE
# define _FORTIFY_SOURCE 0

# include "process.h"
# include "tasks_queue.h"

typedef struct scheduler_t {
  tasks_queue_t* queue;
  tasks_queue_t* to_release;
  process_t*     current;
  jmp_buf*       exit;
  int64_t        fuel;
} scheduler_t;

scheduler_t* scheduler_new();
void scheduler_free(scheduler_t*);
PID_t scheduler_spawn(scheduler_t*, action_t);
void scheduler_yield(scheduler_t*);
void scheduler_run(scheduler_t*, action_t);