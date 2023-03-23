# undef _FORTIFY_SOURCE
# define _FORTIFY_SOURCE 0

# include "scheduler.h"

# include <stdio.h>
# include <stdlib.h>
# include <stdbool.h>
# include <setjmp.h>

# include "process.h"
# include "tasks_queue.h"

# define AVAILABLE_FUEL 10

scheduler_t* scheduler_new() {
  scheduler_t* scheduler = malloc(sizeof(struct scheduler_t));
  scheduler->queue      = tasks_queue_new();
  scheduler->to_release = tasks_queue_new();
  scheduler->current    = NULL;
  scheduler->exit       = malloc(sizeof(jmp_buf));
  scheduler->fuel       = 0;
  return scheduler;
}

void scheduler_free(scheduler_t* scheduler) {
  tasks_queue_free(scheduler->queue);
  tasks_queue_free(scheduler->to_release);
  free(scheduler->exit);
  free(scheduler);
}

static
void scheduler_collect(scheduler_t* scheduler) {
  process_t* process;
  while ((process = tasks_queue_dequeue(scheduler->to_release))) {
    process_free(process);
  }
}

static _Noreturn
void scheduler_switch(scheduler_t* scheduler) {
  scheduler->current = tasks_queue_dequeue(scheduler->queue);

  if (scheduler->current) {
    scheduler->fuel = AVAILABLE_FUEL;
    longjmp(*scheduler->current->context, 1);
  } else {
    longjmp(*scheduler->exit, 1);
  }
}

static _Noreturn
void action_wrapper(scheduler_t* scheduler, action_t action, jmp_buf* spawner, process_t* process) {
  tasks_queue_enqueue(scheduler->queue, process);

  if (setjmp(*process->context) == 0)
    longjmp(*spawner, 1);

  action();
  tasks_queue_enqueue(scheduler->to_release, process);
  scheduler_switch(scheduler);
}

PID_t scheduler_spawn(scheduler_t* scheduler, action_t action) {
  jmp_buf*   spawner = malloc(sizeof(jmp_buf));
  process_t* process = process_new();

  if (setjmp(*spawner) == 0) asm(
    "movq  %[stack_beg], %%rsp \n"
    "movq  %[vstack]   , %%r12 \n"

    "movq  %[scheduler], %%rdi \n"
    "movq  %[action]   , %%rsi \n"
    "movq  %[spawner]  , %%rdx \n"
    "movq  %[process]  , %%rcx \n"
    
    "jmpq *%[action_wrapper]   \n"
    :
    : [stack_beg]      "r" (process->stack_beg)
    , [vstack]         "r" (process->vstack)

    , [scheduler]      "r" (scheduler)
    , [action]         "r" (action)
    , [spawner]        "r" (spawner)
    , [process]        "r" (process)
    , [action_wrapper] "r" (action_wrapper)
    : "rdi", "rsi", "rdx", "rcx", "memory", "r12", "r13"
  );

  return process->pid;
}

void scheduler_yield(scheduler_t* scheduler) {
  scheduler->fuel -= 1;
  if (scheduler->fuel <= 0) {
    tasks_queue_enqueue(scheduler->queue, scheduler->current);

    if (setjmp(*scheduler->current->context) == 0) {
      scheduler_switch(scheduler);
    }
  }

  scheduler_collect(scheduler);
}

void scheduler_run(scheduler_t* scheduler, action_t action) {
  if (setjmp(*scheduler->exit) == 0) {
    scheduler_spawn(scheduler, action);
    scheduler_switch(scheduler);
  }

  scheduler_collect(scheduler);
}
