# include <stdio.h>
# include <stdlib.h>
# include <stdbool.h>
# include "../src/Language/Agner/X64.h"

typedef int64_t value_t;

typedef struct fun_meta_t {
  int64_t arity;
  char name[];
} __attribute__((packed)) fun_meta_t;

typedef struct boxed_tuple_t {
  int64_t header;
  int64_t size;
  value_t values[];
} __attribute__((packed)) boxed_tuple_t;

typedef struct boxed_cons_t {
  int64_t header;
  int64_t is_list;
  union {
    value_t as_array[2];
    struct { value_t head; value_t tail; } as_pair;
  } values;
} __attribute__((packed)) boxed_cons_t;

typedef union boxed_value_t {
  struct { int64_t header; } super;
  boxed_tuple_t tuple;
  boxed_cons_t cons;
} boxed_value_t;

typedef struct heap_node_t {
  value_t* mem;
  value_t* mem_head;
  value_t* mem_end;
  struct heap_node_t* next;
} __attribute__((packed)) heap_node_t;

typedef struct call_stack_t {
  int64_t size;
  fun_meta_t* fun;
  value_t* stack_frame;
  struct call_stack_t* next;
} __attribute__((packed)) call_stack_t;

value_t _runtime__calling_context;
heap_node_t* _runtime__heap;
value_t* _runtime__stack;
call_stack_t* _runtime__call_stack;

# define HEAP_CHUNK_SIZE (1024 * 1024 * 1024)
heap_node_t* mk_heap() {
  heap_node_t* heap = malloc(sizeof(heap_node_t));
  heap->mem = malloc(HEAP_CHUNK_SIZE);
  heap->mem_head = heap->mem;
  heap->mem_end = heap->mem + HEAP_CHUNK_SIZE/WORD_SIZE;
  heap->next = NULL;
  return heap;
}

# define STACK_SIZE (1024 * 1024 * 512)
value_t* mk_stack() {
  return malloc(STACK_SIZE);
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
              print_value_(cons->values.as_pair.head, trancated);
              if (cons->values.as_pair.tail != NIL_TAG) printf(",");
              value = cons->values.as_pair.tail;
            }
          } else {
            print_value_(ref->cons.values.as_pair.head, trancated);
            printf("|");
            print_value_(ref->cons.values.as_pair.tail, trancated);
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
    _runtime__heap->next = mk_heap();
    _runtime__heap = _runtime__heap->next;
  }

  value_t* target = _runtime__heap->mem_head;
  _runtime__heap->mem_head += size;
  return target;
}

extern void _runtime__print_value(value_t value) {
  print_value(value);
  printf("\n");
  fflush(stdout);
}

extern value_t _runtime__alloc_tuple(int64_t size, value_t* values) {
  boxed_tuple_t* tuple = allocate(sizeof(boxed_tuple_t)/WORD_SIZE + size);
  tuple->header = TUPLE_HEADER;
  tuple->size = size;
  for (int i = 0; i < size; i++) {
    tuple->values[i] = values[i];
  }

  return (value_t)tuple | BOX_TAG;
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

extern value_t _runtime__alloc_cons(value_t head, value_t tail) {
  boxed_cons_t* cons = allocate(sizeof(boxed_cons_t)/WORD_SIZE);
  cons->header = CONS_HEADER;
  cons->is_list = is_list(tail);
  cons->values.as_pair.head = head;
  cons->values.as_pair.tail = tail;

  return (value_t)cons | BOX_TAG;
}

extern value_t* _runtime__match_cons(value_t value) {
  if ((value & TAG_MASK) != BOX_TAG) return 0;
  boxed_value_t* ref = (boxed_value_t*)(value ^ BOX_TAG);
  if (ref->super.header != CONS_HEADER) return 0;
  return ref->cons.values.as_array;
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

extern void _THROW_function_clause(value_t fun, value_t* args) {
  fun_meta_t* meta = get_fun_meta(fun);

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

value_t* _runtime__init() {
  _runtime__heap = mk_heap();
  _runtime__stack = mk_stack();
  _runtime__call_stack = NULL;
  return _runtime__stack;
}

// BiFs

// RUNTIME_call_context should be "ok" atom
extern value_t _agner__print(value_t value) {
  _runtime__print_value(value);
  return _runtime__calling_context;
}

extern void _global__error(value_t value) {
  printf("** exception error: ");
  print_value(value);
  printf("\n");
  print_call_stack();
  exit(-1);
}