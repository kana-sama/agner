# include <stdio.h>
# include <stdlib.h>
# include "../src/Language/Agner/X64.h"

typedef int64_t value_t;

typedef struct fun_meta_t {
  int64_t arity;
  char name[];
} __attribute__((packed)) fun_meta_t;

value_t RUNTIME_call_context;

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
      printf("fun %s/%lld\n", meta->name, meta->arity);
      break;
    }
  }
}

extern void _print_value(value_t value) {
  print_value(value);
  printf("\n");
  fflush(stdout);
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