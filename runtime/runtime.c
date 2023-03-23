# include <stdio.h>
# include <stdlib.h>
# include <stdbool.h>
# include <time.h>
# include <string.h>

# include "../src/Language/Agner/X64.h"
# include "value.h"
# include "throw.h"
# include "heap.h"
# include "runtime.h"

struct timespec rt_start, rt_end;

typedef struct native_stack_t {
  int64_t* mem;
  int64_t* mem_head;
  int64_t* mem_end;
} native_stack_t;

typedef struct process_t {
  PID_t           pid;
  heap_t*         heap;
  value_t*        stack;
  native_stack_t* native_stack;
} process_t;

process_t* current_process;
value_t _runtime__calling_context[10];

static value_t* mk_stack(int64_t size) {
  return malloc(size * sizeof(value_t));
}

static native_stack_t* mk_native_stack(int64_t size) {
  native_stack_t* stack = malloc(sizeof(native_stack_t));
  stack->mem = malloc(size * sizeof(int64_t));
  stack->mem_head = stack->mem;
  stack->mem_end = stack->mem + size;
  return stack;
}




static value_t* get_stack_head() {
  value_t* stack_head;
  asm("\t movq %%r12, %0" : "=r"(stack_head));
  return stack_head;
}


# define HEAP_INITIAL_SIZE (1024 * 1024)
# define STACK_INITIAL_SIZE (1024 * 1024 * 64)
# define NATIVE_STACK_SIZE (1024 * 1024 * 64)
static process_t* mk_process() {
  process_t* process = malloc(sizeof(process_t));
  process->heap = mk_heap(HEAP_INITIAL_SIZE);
  process->stack = mk_stack(STACK_INITIAL_SIZE);
  process->native_stack = mk_native_stack(NATIVE_STACK_SIZE);
  return process;
}


extern value_t* _runtime__init() {
  clock_gettime(CLOCK_MONOTONIC_RAW, &rt_start);
  current_process = mk_process();
  return current_process->stack;
}

extern void _runtime__finalize() {
  clock_gettime(CLOCK_MONOTONIC_RAW, &rt_end);
  uint64_t total_time = (rt_end.tv_sec - rt_start.tv_sec) * 1000 + (rt_end.tv_nsec - rt_start.tv_nsec) / 1000000;
  printf("total time: %lldms\n"
         "   gc time: %lldms\n", total_time, gc_time());
}

extern void _runtime__yield() {

}


extern value_t _runtime__alloc_tuple(int64_t size) {
  boxed_tuple_t* tuple = allocate(
    &current_process->heap,
    current_process->stack,
    get_stack_head(),
    sizeof(boxed_tuple_t)/WORD_SIZE + size
  );
  tuple->super.header = TUPLE_HEADER;
  tuple->size = size;
  return (value_t)tuple | BOX_TAG;
}

extern void _runtime__fill_tuple(value_t value, int64_t size, value_t* values) {
  boxed_tuple_t* tuple = (boxed_tuple_t*)(value ^ BOX_TAG);
  for (int i = 0; i < size; i++) {
    tuple->values[i] = values[i];
  }
}

extern value_t* _runtime__match_tuple(value_t value, int64_t size) {
  if ((value & TAG_MASK) != BOX_TAG) return 0;
  boxed_value_t* ref = (boxed_value_t*)(value ^ BOX_TAG);
  if (ref->super.header != TUPLE_HEADER) return 0;
  if (ref->tuple.size != size) return 0;
  return ref->tuple.values;
}

extern value_t _runtime__alloc_cons() {
  boxed_cons_t* cons = allocate(
    &current_process->heap,
    current_process->stack,
    get_stack_head(),
    sizeof(boxed_cons_t)/WORD_SIZE
  );
  cons->super.header = CONS_HEADER;
  return (value_t)cons | BOX_TAG;
}

extern void _runtime__fill_cons(value_t value, value_t head, value_t tail) {
  boxed_cons_t* cons = (boxed_cons_t*)(value ^ BOX_TAG);
  cons->values.head = head;
  cons->values.tail = tail;
  cons->is_list = is_list(tail) ? 1 : 0;
}

extern value_t* _runtime__match_cons(value_t value) {
  if ((value & TAG_MASK) != BOX_TAG) return 0;
  boxed_value_t* ref = (boxed_value_t*)(value ^ BOX_TAG);
  if (ref->super.header != CONS_HEADER) return 0;
  return (value_t*)(&ref->cons.values);
}


// BiFs


// _runtime__calling_context[0] should be "ok" atom
extern value_t _agner__print(value_t value) {
  print_value(value);
  printf("\n");
  fflush(stdout);
  return _runtime__calling_context[0];
}

extern value_t _global__error(value_t value) {
  printf("** exception error: ");
  print_value(value);
  printf("\n");
  exit(-1);
}

// _runtime__calling_context[0] should be "ok" atom
// _runtime__calling_context[1] should be "infinity" atom
static fun_meta_t _timer__sleep__meta = {.arity = 1, .name = "timer:sleep"};
extern value_t _timer__sleep(value_t duration) {
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

extern value_t _global__spawn(value_t cb) {
  return 41 << TAG_SIZE | PID_TAG;
}

extern value_t _global__self() {
  return 42 << TAG_SIZE | PID_TAG;
}