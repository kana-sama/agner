# undef _FORTIFY_SOURCE
# define _FORTIFY_SOURCE 0

# include "scheduler.h"

# include <stdio.h>
# include <stdlib.h>
# include <stdbool.h>
# include <setjmp.h>

# include "containers/list.h"
# include "options.h"
# include "process.h"

scheduler_t* scheduler_new() {
  scheduler_t* scheduler = malloc(sizeof(struct scheduler_t));
  scheduler->queue      = list_new();
  scheduler->to_release = list_new();
  scheduler->current    = NULL;
  scheduler->exit       = malloc(sizeof(jmp_buf));
  scheduler->fuel       = 0;
  return scheduler;
}

void scheduler_free(scheduler_t* scheduler) {
  list_free(scheduler->queue);
  list_free(scheduler->to_release);
  free(scheduler->exit);
  free(scheduler);
}

static
void scheduler_collect(scheduler_t* scheduler) {
  process_t* process;
  while ((process = list_shift(scheduler->to_release))) {
    process_free(process);
  }
}

static _Noreturn
void scheduler_switch(scheduler_t* scheduler) {
  scheduler->current = list_shift(scheduler->queue);

  if (scheduler->current) {
    scheduler->fuel = options.fuel;
    longjmp(*scheduler->current->context, 1);
  } else {
    longjmp(*scheduler->exit, 1);
  }
}

void action_wrapper_wrapper(process_t*, void*, value_t*, action_t);

asm(
  ".align 16 \n"
  ".globl action_wrapper_wrapper \n"
    "action_wrapper_wrapper: \n"
    "pushq %rbx \n"
    "pushq %r12 \n"
    "pushq %r13 \n"
    // process in rdi
    // arg     in rsi
    "movq %rdx, %r12 \n"
    "call *%rcx \n"
    "popq %r13 \n"
    "popq %r12 \n"
    "popq %rbx \n"
    "ret"
);

static _Noreturn
void action_wrapper(scheduler_t* scheduler, action_t action, jmp_buf* spawner, process_t* process, void* arg, char* vstack) {
  list_append(scheduler->queue, process);

  if (setjmp(*process->context) == 0)
    longjmp(*spawner, 1);

  action_wrapper_wrapper(process, arg, process->vstack, action);
  
  process->is_alive = false;
  list_append(scheduler->to_release, process);
  scheduler_switch(scheduler);
}

process_t* scheduler_spawn(scheduler_t* scheduler, action_t action, void* arg) {
  jmp_buf*   spawner = malloc(sizeof(jmp_buf));
  process_t* process = process_new();

  if (setjmp(*spawner) == 0) asm(
    "movq  %[stack_beg], %%rsp \n"


    "movq  %[scheduler], %%rdi \n"
    "movq  %[action]   , %%rsi \n"
    "movq  %[spawner]  , %%rdx \n"
    "movq  %[process]  , %%rcx \n"
    "movq  %[arg]      , %%r8  \n"
    "movq  %[vstack]   , %%r9 \n"
    "jmpq *%[action_wrapper]   \n"
    :
    : [stack_beg] "r" (process->stack_beg)
    , [vstack]    "r" (process->vstack)

    , [scheduler] "r" (scheduler)
    , [action]    "r" (action)
    , [spawner]   "r" (spawner)
    , [process]   "r" (process)
    , [arg]       "r" (arg)

    , [action_wrapper] "r" (action_wrapper)
    : "rdi", "rsi", "rdx", "rcx", "r8", "r9", "memory"
  );

  return process;
}

void scheduler_next(scheduler_t* scheduler) {
  list_append(scheduler->queue, scheduler->current);

  if (setjmp(*scheduler->current->context) == 0) {
    scheduler_switch(scheduler);
  }

  scheduler_collect(scheduler);
}

void scheduler_yield(scheduler_t* scheduler) {
  scheduler->fuel -= 1;

  if (scheduler->fuel <= 0) {
    scheduler_next(scheduler);
  }
}

void scheduler_run(scheduler_t* scheduler, action_t action, void* arg) {
  if (setjmp(*scheduler->exit) == 0) {
    scheduler_spawn(scheduler, action, arg);
    scheduler_switch(scheduler);
  }

  scheduler_collect(scheduler);
}
