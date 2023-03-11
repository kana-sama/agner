# include <stdio.h>
# include <stdlib.h>
# include <stdbool.h>
# include <time.h>
# include <string.h>
# include "../src/Language/Agner/X64.h"

struct timespec rt_start, rt_end;
uint64_t rt_in_gc = 0;

typedef int64_t value_t;

void print_value(value_t value);

typedef struct fun_meta_t {
  int64_t arity;
  char name[];
} __attribute__((packed)) fun_meta_t;

typedef struct boxed_super_t {
  int64_t header;
  int64_t gc_offset;
} boxed_super_t;

typedef struct boxed_tuple_t {
  boxed_super_t super;
  int64_t size;
  value_t values[];
} __attribute__((packed)) boxed_tuple_t;

typedef struct boxed_cons_t {
  boxed_super_t super;
  int64_t is_list;
  struct { value_t head; value_t tail; } values;
} __attribute__((packed)) boxed_cons_t;

typedef union boxed_value_t {
  boxed_super_t super;
  boxed_tuple_t tuple;
  boxed_cons_t cons;
} boxed_value_t;

typedef struct call_stack_t {
  int64_t size;
  fun_meta_t* fun;
  value_t* stack_frame;
  struct call_stack_t* next;
} __attribute__((packed)) call_stack_t;

typedef struct heap_t {
  value_t* mem;
  value_t* mem_head;
  value_t* mem_end;
  int64_t size;
} heap_t;

heap_t* mk_heap(int64_t size);

value_t _runtime__calling_context[10];
heap_t* _runtime__heap;
value_t* _runtime__stack;
call_stack_t* _runtime__call_stack;

typedef struct gc_traverse_stack_t {
  boxed_value_t* value;
  int64_t size;
  struct gc_traverse_stack_t* next;
} gc_traverse_stack_t;

gc_traverse_stack_t* gc_traverse_stack__push(boxed_value_t* value, gc_traverse_stack_t* stack) {
  gc_traverse_stack_t* new_stack = malloc(sizeof(gc_traverse_stack_t));
  new_stack->value = value;
  new_stack->next = stack;
  new_stack->size = stack == NULL ? 1 : stack->size + 1;
  return new_stack;
}

gc_traverse_stack_t* gc_traverse_stack__pop(boxed_value_t** out, gc_traverse_stack_t* stack) {
  *out = stack->value;
  gc_traverse_stack_t* next = stack->next;
  free(stack);
  return next;
}

int64_t boxed_value_size(boxed_value_t* ref) {
  switch (ref->super.header) {
    case TUPLE_HEADER:
      return sizeof(boxed_tuple_t)/sizeof(value_t) + ref->tuple.size;
    case CONS_HEADER:
      return sizeof(boxed_cons_t)/sizeof(value_t);
    default:
      printf("Unknown boxed value header: %lld\n", ref->super.header);
      exit(-1);
  }
}

typedef struct boxed_value_children_t {
  value_t* values;
  int64_t count;
} boxed_value_children_t;

boxed_value_children_t boxed_value_children(boxed_value_t* ref) {
  switch (ref->super.header) {
    case TUPLE_HEADER:
      return (boxed_value_children_t){ .values=ref->tuple.values, .count=ref->tuple.size };
    case CONS_HEADER:
      return (boxed_value_children_t){ .values=(value_t*)&ref->cons.values, .count=2 };
    default:
      printf("Unknown boxed value header: %lld\n", ref->super.header);
      exit(-1);
  }
}

gc_traverse_stack_t* boxed_value_traverse(boxed_value_t* ref, gc_traverse_stack_t* stack) {
  switch (ref->super.header) {
    case TUPLE_HEADER:
      for (int i = 0; i < ref->tuple.size; i++)
        if ((ref->tuple.values[i] & TAG_MASK) == BOX_TAG)
          stack = gc_traverse_stack__push((boxed_value_t*)(ref->tuple.values[i] ^ BOX_TAG), stack);
      return stack;

    case CONS_HEADER:
      if ((ref->cons.values.head & TAG_MASK) == BOX_TAG)
        stack = gc_traverse_stack__push((boxed_value_t*)(ref->cons.values.head ^ BOX_TAG), stack);
      if ((ref->cons.values.tail & TAG_MASK) == BOX_TAG)
        stack = gc_traverse_stack__push((boxed_value_t*)(ref->cons.values.tail ^ BOX_TAG), stack);
      return stack;

    default:
      printf("Unknown boxed value header: %lld\n", ref->super.header);
      exit(-1);
  }
}

// mark and copy algorithm
heap_t* gc(
  value_t* stack, int64_t stack_size,
  heap_t* heap
) {
  struct timespec gc_start, gc_end;
  clock_gettime(CLOCK_MONOTONIC_RAW, &gc_start);

  int64_t offset = 0;
  gc_traverse_stack_t* traverse_stack = NULL;

  for (int i = 0; i < stack_size; i++) {
    value_t value = stack[i];
    if ((value & TAG_MASK) != BOX_TAG) continue;
    boxed_value_t* ref = (boxed_value_t*)(value ^ BOX_TAG);

    if (ref->super.gc_offset == 0) {
      ref->super.gc_offset = offset;
      offset += boxed_value_size(ref);
      traverse_stack = boxed_value_traverse(ref, traverse_stack);
    }
  }

  {
    boxed_value_t* ref;
    while (traverse_stack != NULL) {
      traverse_stack = gc_traverse_stack__pop(&ref, traverse_stack);

      if (ref->super.gc_offset == 0) {
        ref->super.gc_offset = offset;
        offset += boxed_value_size(ref);
        traverse_stack = boxed_value_traverse(ref, traverse_stack);
      }
    }
  }

  heap_t* new_heap = mk_heap(offset * 2);
  new_heap->mem_head = new_heap->mem + offset;

  for (int i = 0; i < stack_size; i++) {
    value_t value = stack[i];
    if ((value & TAG_MASK) == BOX_TAG) {
      boxed_value_t* ref = (boxed_value_t*)(value ^ BOX_TAG);
      stack[i] = (value_t)(new_heap->mem + ref->super.gc_offset) | BOX_TAG;
      
      traverse_stack = gc_traverse_stack__push(ref, traverse_stack);
    }
  }

  {
    boxed_value_t* ref;
    while (traverse_stack != NULL) {
      traverse_stack = gc_traverse_stack__pop(&ref, traverse_stack);

      void* dst = (void*)(new_heap->mem + ref->super.gc_offset);
      void* src = (void*)ref;
      size_t count = boxed_value_size(ref) * sizeof(value_t);
      memcpy(dst, src, count);

      boxed_value_t* new_ref = (boxed_value_t*)dst;
      new_ref->super.gc_offset = 0;

      boxed_value_children_t children = boxed_value_children(new_ref);
      for (int i = 0; i < children.count; i++) {
        if ((children.values[i] & TAG_MASK) == BOX_TAG) {
          boxed_value_t* ref = (boxed_value_t*)(children.values[i] ^ BOX_TAG);
          children.values[i] = (value_t)(new_heap->mem + ref->super.gc_offset) | BOX_TAG;
        }
      }
      
      traverse_stack = boxed_value_traverse(ref, traverse_stack);
    }
  }

  // TODO
  // printf("GC collected %lld values\n", (heap->mem_end - heap->mem) - offset);

  free(heap->mem);
  free(heap);

  clock_gettime(CLOCK_MONOTONIC_RAW, &gc_end);
  rt_in_gc += (gc_end.tv_sec - gc_start.tv_sec) * 1000 + (gc_end.tv_nsec - gc_start.tv_nsec) / 1000000;

  return new_heap;
}


heap_t* mk_heap(int64_t size) {
  // TODO
  // printf("Creating heap with size of %lld words\n", size);
  heap_t* heap = malloc(sizeof(heap_t));
  heap->mem = malloc(size * sizeof(value_t));
  heap->mem_head = heap->mem;
  heap->mem_end = heap->mem + size;
  heap->size = size;
  return heap;
}

value_t* mk_stack(int64_t size) {
  return malloc(size * sizeof(value_t));
}

fun_meta_t* get_fun_meta(value_t fun) {
  int64_t fun_size = *((int64_t*)fun - 1);
  return ((void*)fun + fun_size);
}

void print_value_(value_t value, bool trancated) {
  switch (value & TAG_MASK) {
    case NUMBER_TAG: {
      printf("%lld", value >> TAG_SIZE);
      break;
    }
    case ATOM_TAG: {
      printf("%s", (char*) value);
      break;
    }
    case FUN_TAG: {
      fun_meta_t* meta = get_fun_meta(value);
      printf("fun %s/%lld", meta->name, meta->arity);
      break;
    }
    case NIL_TAG: {
      printf("[]");
      break;
    }
    case BOX_TAG: {
      boxed_value_t* ref = (boxed_value_t*)(value ^ BOX_TAG);
      
      switch (ref->super.header) {
        case TUPLE_HEADER: {
          printf("{");
          for (int i = 0; i < ref->tuple.size; i++) {
            print_value_(ref->tuple.values[i], trancated);
            if (i != ref->tuple.size - 1) printf(",");
          }
          printf("}");
          break;
        }
        case CONS_HEADER: {
          printf("[");
          if (trancated) {
            printf("...");
          } else if (ref->cons.is_list == 1) {
            value_t value = (value_t)ref | BOX_TAG;
            while (value != NIL_TAG) {
              boxed_cons_t* cons = (boxed_cons_t*)(value ^ BOX_TAG);
              print_value_(cons->values.head, trancated);
              if (cons->values.tail != NIL_TAG) printf(",");
              value = cons->values.tail;
            }
          } else {
            print_value_(ref->cons.values.head, trancated);
            printf("|");
            print_value_(ref->cons.values.tail, trancated);
          }
          printf("]");
          break;
        }
      }

      break;
    }
  }
}

void print_value(value_t value) {
  print_value_(value, false);
}

void print_value_trancated(value_t value) {
  print_value_(value, true);
}

int64_t get_stack_size() {
  value_t* stack_head;
  asm("\t movq %%r12, %0" : "=r"(stack_head));
  return stack_head - _runtime__stack;
}

void _runtime__push_callstack(fun_meta_t* meta, value_t* stack_frame) {
  call_stack_t* node = malloc(sizeof(call_stack_t));
  node->fun = meta;
  node->stack_frame = stack_frame;
  node->size = (_runtime__call_stack == NULL ? 0 : _runtime__call_stack->size) + 1;
  node->next = _runtime__call_stack;
  _runtime__call_stack = node;
}

void _runtime__drop_callstack() {
  if (_runtime__call_stack == NULL) return;
  call_stack_t* node = _runtime__call_stack;
  _runtime__call_stack = node->next;
  free(node);
}

void print_call_stack() {
  printf("CallStack:\n");
  call_stack_t* call_stack = _runtime__call_stack;
  int limit = 5;
  while (call_stack != NULL && limit > 0) {
    fun_meta_t* meta = call_stack->fun;
    printf("  %s(", meta->name);
    for (int i = 0; i < meta->arity; i++) {
      print_value_trancated(call_stack->stack_frame[i]);
      if (i < meta->arity - 1) printf(", ");
    }
    printf(")\n");

    limit -= 1;
    call_stack = call_stack->next;
  }

  if (limit == 0 && call_stack != NULL) {
    printf("  ... (%lld more) \n", call_stack->size);
  }
}

void* allocate(int64_t size) {
  if (_runtime__heap->mem_end - _runtime__heap->mem_head < size) {
    _runtime__heap = gc(_runtime__stack, get_stack_size(), _runtime__heap);
  }

  value_t* target = _runtime__heap->mem_head;
  _runtime__heap->mem_head += size;

  for (int i = 0; i < size; i++) {
    target[i] = 0;
  }

  return target;
}

extern void _runtime__print_value(value_t value) {
  print_value(value);
  printf("\n");
  fflush(stdout);
}

extern value_t _runtime__alloc_tuple(int64_t size) {
  boxed_tuple_t* tuple = allocate(sizeof(boxed_tuple_t)/WORD_SIZE + size);
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

int64_t is_list(value_t value) {
  if ((value & TAG_MASK) == NIL_TAG) return 1;
  if ((value & TAG_MASK) != BOX_TAG) return 0;
  boxed_value_t* ref = (boxed_value_t*)(value ^ BOX_TAG);
  if (ref->super.header != CONS_HEADER) return 0;
  return ref->cons.is_list;
}

extern value_t _runtime__alloc_cons() {
  boxed_cons_t* cons = allocate(sizeof(boxed_cons_t)/WORD_SIZE);
  cons->super.header = CONS_HEADER;
  return (value_t)cons | BOX_TAG;
}

extern void _runtime__fill_cons(value_t value, value_t head, value_t tail) {
  boxed_cons_t* cons = (boxed_cons_t*)(value ^ BOX_TAG);
  cons->values.head = head;
  cons->values.tail = tail;
  cons->is_list = is_list(tail);
}

extern value_t* _runtime__match_cons(value_t value) {
  if ((value & TAG_MASK) != BOX_TAG) return 0;
  boxed_value_t* ref = (boxed_value_t*)(value ^ BOX_TAG);
  if (ref->super.header != CONS_HEADER) return 0;
  return (value_t*)(&ref->cons.values);
}

char* format_args(int64_t n) {
  switch (n) {
    case 0: return "no arguments";
    case 1: return "one argument";
    case 2: return "two arguments";
    default: {
      char* format = "%lld arguments";
      size_t size = snprintf(NULL, 0, format, n) + 1;
      char* str = malloc(size);
      sprintf(str, format, n);
      return str;
    }
  }
}

extern void _THROW_badarity(value_t fun, int64_t args) {
  fun_meta_t* meta = get_fun_meta(fun);
  printf("** exception error: %s/%lld called with %s\n", meta->name, meta->arity, format_args(args));
  print_call_stack();
  exit(-1);
}

extern void _THROW_badfun(value_t value) {
  printf("** exception error: bad function ");
  print_value_trancated(value);
  printf("\n");
  print_call_stack();
  exit(-1);
}

extern void _THROW_function_clause(fun_meta_t* meta, value_t* args) {
  printf("** exception error: no function clause matching %s(", meta->name);

  for (int i = 0; i < meta->arity; i++) {
    print_value_trancated(args[i]);
    if (i < meta->arity - 1) printf(", ");
  }

  printf(")\n");
  print_call_stack();
  exit(-1);
}

extern void _THROW_badmatch(value_t value) {
  printf("** exception error: no match of right hand side value ");
  print_value_trancated(value);
  printf("\n");
  print_call_stack();
  exit(-1);
}

extern void _THROW_badarith(value_t l, value_t r, char* op) {
  printf("** exception error: an error occurred when evaluating an arithmetic expression\n");
  printf("     in operator  %s/2\n", op);
  printf("        called as "); print_value_trancated(l); printf(" %s ", op); print_value_trancated(r); printf("\n");
  print_call_stack();
  exit(-1); 
}

# define HEAP_INITIAL_SIZE (1024 * 1024)
# define STACK_INITIAL_SIZE (1024 * 1024 * 64)
extern value_t* _runtime__init() {
  clock_gettime(CLOCK_MONOTONIC_RAW, &rt_start);

  _runtime__heap = mk_heap(HEAP_INITIAL_SIZE);
  _runtime__stack = mk_stack(STACK_INITIAL_SIZE);
  _runtime__call_stack = NULL;
  return _runtime__stack;
}

extern void _runtime__finalize() {
  clock_gettime(CLOCK_MONOTONIC_RAW, &rt_end);
  uint64_t total_time = (rt_end.tv_sec - rt_start.tv_sec) * 1000 + (rt_end.tv_nsec - rt_start.tv_nsec) / 1000000;
  printf("total time: %lldms\n"
         "   gc time: %lldms\n", total_time, rt_in_gc);
}

// BiFs

// _runtime__calling_context[0] should be "ok" atom
extern value_t _agner__print(value_t value) {
  _runtime__print_value(value);
  return _runtime__calling_context[0];
}

extern void _global__error(value_t value) {
  printf("** exception error: ");
  print_value(value);
  printf("\n");
  print_call_stack();
  exit(-1);
}

// _runtime__calling_context[0] should be "ok" atom
// _runtime__calling_context[1] should be "infinity" atom
fun_meta_t _timer__sleep__meta = {1, "timer:sleep"};
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
