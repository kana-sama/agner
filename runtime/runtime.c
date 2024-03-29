# include "runtime.h"

# include <stdio.h>
# include <stdlib.h>
# include <stdbool.h>
# include <time.h>
# include <string.h>

# include "tags.h"
# include "options.h"
# include "value.h"
# include "throw.h"
# include "heap.h"
# include "scheduler.h"
# include "shared.h"

scheduler_t* scheduler;
FILE* ylog;

void _runtime__start(value_t entry) {
  read_env_options();

  if (options.ylog) {
    ylog = fopen(options.ylog, "w");
    fprintf(ylog, "id,pid,function,fuel,vstack_size\n");
  }

  struct timespec rt_start, rt_end;
  clock_gettime(CLOCK_MONOTONIC_RAW, &rt_start);

  if ((entry & TAG_MASK) != FUN_TAG) _throw__badfun(entry);
  scheduler = scheduler_new();
  scheduler_run(scheduler, (action_t)entry, NULL);
  scheduler_free(scheduler);

  clock_gettime(CLOCK_MONOTONIC_RAW, &rt_end);

  if (ylog) fclose(ylog);

  if (options.stat) {
    uint64_t total_time = (rt_end.tv_sec - rt_start.tv_sec) * 1000 + (rt_end.tv_nsec - rt_start.tv_nsec) / 1000000;
    printf("total time: %lldms\n"
           "   gc time: %lldms\n", total_time, gc_time());
  }
}

void _runtime__yield(char* name) {
  if (options.ylog) {
    static int64_t log_line_id = 0; log_line_id++;
    fprintf(ylog, "%lld,%lld,%s,%lld,%lld\n",
      log_line_id,
      scheduler->current->pid,
      name,
      scheduler->fuel,
      (int64_t)(scheduler->current->vstack_head - scheduler->current->vstack)
    );
    fflush(ylog);
  }

  if (list_size(scheduler->current->scopes->values) != 0 ||
      list_size(scheduler->current->scopes->frames) != 0) {
    puts("Non-empty heap.extra_values");
    exit(-1);
  }

  scheduler_yield(scheduler);
}

void _runtime__save_vstack() {
  asm(
    "movq %%r12, %[stack_head]"
    : [stack_head] "=m" (scheduler->current->vstack_head)
    :
    : "rdi", "rsi", "rdx", "rcx", "memory", "r12", "r13"
  );
}

void enter_scope() {
  scopes_enter(scheduler->current->scopes);
}

void leave_scope() {
  scopes_leave(scheduler->current->scopes);
}

value_t* add_to_scope(value_t value) {
  return scopes_declare(scheduler->current->scopes, value);
}

void* allocate(int64_t size) {
  return heap_allocate(&scheduler->current->heap, size, process_gc_ctx(scheduler->current));
}

value_t _alloc__integer(int64_t i) {
  return (i << TAG_SIZE) | INTEGER_TAG;
}

value_t _alloc__tuple(int64_t size, value_t* values_) {
  enter_scope();

  value_t* values[size];
  for (int i = 0; i < size; i++)
    values[i] = add_to_scope(values_[i]);

  boxed_tuple_t* tuple = allocate(sizeof(boxed_tuple_t)/WORD_SIZE + size);
  tuple->super.header = TUPLE_HEADER;
  tuple->size = size;
  for (int i = 0; i < size; i++)
    tuple->values[i] = *values[i];

  leave_scope();
  return (value_t)tuple | BOX_TAG;
}

value_t _alloc__cons(value_t head_, value_t tail_) {
  enter_scope();
  value_t* head = add_to_scope(head_);
  value_t* tail = add_to_scope(tail_);

  boxed_cons_t* cons = allocate(sizeof(boxed_cons_t) / WORD_SIZE);
  cons->super.header = CONS_HEADER;
  cons->head = *head;
  cons->tail = *tail;
  cons->proper_list_length = is_proper_list(*tail) ? proper_list_length(*tail) + 1 : -1;

  leave_scope();
  return (value_t)cons | BOX_TAG;
}

value_t _alloc__closure(value_t body, int64_t env_size, value_t* env) {
  enter_scope();

  value_t* values[env_size];
  for (int i = 0; i < env_size; i++)
    values[i] = add_to_scope(env[i]);

  boxed_closure_t* closure = allocate(sizeof(boxed_closure_t)/WORD_SIZE + env_size);
  closure->super.header = CLOSURE_HEADER;
  closure->env_size = env_size;
  closure->body = body;

  for (int i = 0; i < env_size; i++)
    closure->env[i] = *values[i];

  leave_scope();
  return (value_t)(closure) | BOX_TAG;
}

void _receive__set_timeout(value_t value) {
  if (value == shared_infinity())
    return;

  if (is_integer(value))
    return mailbox_set_timeout(scheduler->current->mailbox, decode_integer(value));

  process_raise(scheduler->current, shared_error(), shared_timeout_value());
}

static ms_time_t now() {
  struct timespec time;
  clock_gettime(CLOCK_MONOTONIC_RAW, &time);
  return time.tv_sec * 1000 + time.tv_nsec / 1000000;
}


value_t _receive__pick() {
  while (true) {
    value_t* value = mailbox_pick(scheduler->current->mailbox);
    if (value) {
      return *value;
    } else {
      if (mailbox_is_timed_out(scheduler->current->mailbox)) {
        mailbox_unpick(scheduler->current->mailbox);
        mailbox_remove_timeout(scheduler->current->mailbox);
        return TIMED_OUT_VALUE;
      }

      scheduler_next(scheduler);
    }
  }
}

void _receive__success() {
  mailbox_drop_picked(scheduler->current->mailbox);
  mailbox_unpick(scheduler->current->mailbox);
  mailbox_remove_timeout(scheduler->current->mailbox);
}


value_t* _closure__get_env(value_t value) {
  return cast_to_boxed_value(value)->closure.env;
}

value_t _closure__get_fun(value_t value) {
  return cast_to_boxed_value(value)->closure.body;
}


void _runtime__catch(handler_action_t handler_action, void* stack_head) {
  process_add_handler(scheduler->current, handler_action, stack_head);
}

void _runtime__uncatch() {
  process_remove_handler(scheduler->current);
}

_Noreturn
void _runtime__raise(value_t class, value_t value) {
  process_raise(scheduler->current, class, value);
}
