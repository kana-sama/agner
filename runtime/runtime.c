# include <stdio.h>
# include <stdlib.h>
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

typedef union boxed_value_t {
  struct { int64_t header; } super;
  boxed_tuple_t tuple;
} boxed_value_t;

value_t RUNTIME_call_context;

// 1 gb of heap
value_t RUNTIME_heap[125000000] __attribute__((aligned (8)));
value_t* RUNTIME_heap_head = RUNTIME_heap;

fun_meta_t* get_fun_meta(value_t fun) {
  int64_t fun_size = *((int64_t*)fun - 1);
  return ((void*)fun + fun_size);
}

void print_value(value_t value) {
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
    case BOX_TAG: {
      boxed_value_t* ref = (boxed_value_t*)(value ^ BOX_TAG);
      
      switch (ref->super.header) {
        case TUPLE_HEADER: {
          printf("{");
          for (int i = 0; i < ref->tuple.size; i++) {
            print_value(ref->tuple.values[i]);
            if (i != ref->tuple.size - 1) printf(",");
          }
          printf("}");
          break;
        }
      }

      break;
    }
  }
}

extern void _print_value(value_t value) {
  print_value(value);
  printf("\n");
  fflush(stdout);
}

extern value_t _alloc_tuple(int64_t size, value_t* values) {
  boxed_tuple_t* tuple = (boxed_tuple_t*)RUNTIME_heap_head;
  RUNTIME_heap_head += sizeof(boxed_tuple_t)/WORD_SIZE + size;
  tuple->header = TUPLE_HEADER;
  tuple->size = size;
  for (int i = 0; i < size; i++) {
    tuple->values[i] = values[i];
  }

  return (value_t)tuple | BOX_TAG;
}

extern value_t* _match_tuple(value_t value, int64_t size) {
  if ((value & TAG_MASK) != BOX_TAG) return 0;
  boxed_value_t* ref = (boxed_value_t*)(value ^ BOX_TAG);
  if (ref->super.header != TUPLE_HEADER) return 0;
  if (ref->tuple.size != size) return 0;
  return ref->tuple.values;
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
  exit(-1);
}

extern void _THROW_badfun(value_t value) {
  printf("** exception error: bad function ");
  print_value(value);
  printf("\n");
  exit(-1);
}

extern void _THROW_function_clause(value_t fun, value_t* args) {
  fun_meta_t* meta = get_fun_meta(fun);

  printf("** exception error: no function clause matching %s(", meta->name);

  for (int i = meta->arity; i > 0; i--) {
    value_t arg = *(args - i - (meta->arity-1));
    print_value(arg);
    if (i > 1) printf(", ");
  }

  printf(")\n");
  exit(-1);
}

extern void _THROW_badmatch(value_t value) {
  printf("** exception error: no match of right hand side value ");
  print_value(value);
  printf("\n");
  exit(-1);
}

extern void _THROW_badarith(value_t l, value_t r, char* op) {
  printf("** exception error: an error occurred when evaluating an arithmetic expression\n");
  printf("     in operator  %s/2\n", op);
  printf("        called as "); print_value(l); printf(" %s ", op); print_value(r); printf("\n");
  exit(-1); 
}


// BiFs

// RUNTIME_call_context should be "ok" atom
extern value_t _agner__print(const value_t value) {
  _print_value(value);
  return RUNTIME_call_context;
}