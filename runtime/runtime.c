# include "runtime.h"

# include <stdio.h>
# include <stdlib.h>
# include <stdbool.h>
# include <time.h>
# include <string.h>

# include "tags.h"
# include "value.h"
# include "throw.h"
# include "heap.h"
# include "scheduler.h"

struct timespec rt_start, rt_end;
value_t _runtime__calling_context[10];
scheduler_t* scheduler;

void _runtime__start(value_t entry) {
  clock_gettime(CLOCK_MONOTONIC_RAW, &rt_start);
  if ((entry & TAG_MASK) != FUN_TAG) _THROW_badfun(entry);

  scheduler = scheduler_new();
  scheduler_run(scheduler, (action_t)entry);
  scheduler_free(scheduler);

  clock_gettime(CLOCK_MONOTONIC_RAW, &rt_end);
  uint64_t total_time = (rt_end.tv_sec - rt_start.tv_sec) * 1000 + (rt_end.tv_nsec - rt_start.tv_nsec) / 1000000;
  printf("total time: %lldms\n"
         "   gc time: %lldms\n", total_time, gc_time());
}

void _runtime__yield() {
  scheduler_yield(scheduler);
}

static
value_t* current_vstack_head() {
  value_t* stack_head;
  asm("movq %%r12, %0" : "=rm"(stack_head));
  return stack_head;
}

value_t _runtime__alloc_tuple(int64_t size) {
  boxed_tuple_t* tuple = allocate(
    &scheduler->current->heap,
    scheduler->current->vstack,
    current_vstack_head(),
    sizeof(boxed_tuple_t)/WORD_SIZE + size
  );
  tuple->super.header = TUPLE_HEADER;
  tuple->size = size;
  return (value_t)tuple | BOX_TAG;
}

void _runtime__fill_tuple(value_t value, int64_t size, value_t* values) {
  boxed_tuple_t* tuple = (boxed_tuple_t*)(value ^ BOX_TAG);
  for (int i = 0; i < size; i++) {
    tuple->values[i] = values[i];
  }
}

value_t* _runtime__match_tuple(value_t value, int64_t size) {
  if ((value & TAG_MASK) != BOX_TAG) return 0;
  boxed_value_t* ref = (boxed_value_t*)(value ^ BOX_TAG);
  if (ref->super.header != TUPLE_HEADER) return 0;
  if (ref->tuple.size != size) return 0;
  return ref->tuple.values;
}

value_t _runtime__alloc_cons() {
  boxed_cons_t* cons = allocate(
    &scheduler->current->heap,
    scheduler->current->vstack,
    current_vstack_head(),
    sizeof(boxed_cons_t)/WORD_SIZE
  );
  cons->super.header = CONS_HEADER;
  return (value_t)cons | BOX_TAG;
}

void _runtime__fill_cons(value_t value, value_t head, value_t tail) {
  boxed_cons_t* cons = (boxed_cons_t*)(value ^ BOX_TAG);
  cons->values.head = head;
  cons->values.tail = tail;
  cons->is_list = is_list(tail) ? 1 : 0;
}

value_t* _runtime__match_cons(value_t value) {
  if ((value & TAG_MASK) != BOX_TAG) return 0;
  boxed_value_t* ref = (boxed_value_t*)(value ^ BOX_TAG);
  if (ref->super.header != CONS_HEADER) return 0;
  return (value_t*)(&ref->cons.values);
}


// // BiFs


// _runtime__calling_context[0] should be "ok" atom
value_t _agner__print(value_t value) {
  _runtime__yield();
  
  print_value(value);
  printf("\n");
  fflush(stdout);
  return _runtime__calling_context[0];
}

value_t _global__error(value_t value) {
  _runtime__yield();

  printf("** exception error: ");
  print_value(value);
  printf("\n");
  exit(-1);
}

// _runtime__calling_context[0] should be "ok" atom
// _runtime__calling_context[1] should be "infinity" atom
static fun_meta_t _timer__sleep__meta = {.arity = 1, .name = "timer:sleep"};
value_t _timer__sleep(value_t duration) {
  _runtime__yield();

  switch (duration & TAG_MASK) {
    case NUMBER_TAG: {
      struct timespec ts;
      int64_t msec = duration >> TAG_SIZE;
      int res;

      ts.tv_sec = msec / 1000;
      ts.tv_nsec = (msec % 1000) * 1000000;

      do res = nanosleep(&ts, &ts); while (res);
      return _runtime__calling_context[0];
    }
    case ATOM_TAG: {
      if (duration == _runtime__calling_context[1]) {
        while (true);
        return _runtime__calling_context[0];
      }
    }
    default: {
      value_t args[1] = {duration};
      _THROW_function_clause(&_timer__sleep__meta, args);
      return _runtime__calling_context[0];
    }
  }
}

value_t _global__spawn(value_t value) {
  _runtime__yield();

  if ((value & TAG_MASK) != FUN_TAG) _THROW_badfun(value);
  action_t action = (action_t)value;
  PID_t pid = scheduler_spawn(scheduler, action);
  return pid << TAG_SIZE | PID_TAG;
}

value_t _global__self() {
  _runtime__yield();

  return scheduler->current->pid << TAG_SIZE | PID_TAG;
}
