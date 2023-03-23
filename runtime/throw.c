# include <stdio.h>
# include <stdlib.h>

# include "value.h"
# include "throw.h"

static char* format_args(int64_t n) {
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
  print_value_trancated(value);
  printf("\n");
  exit(-1);
}

extern void _THROW_function_clause(fun_meta_t* meta, value_t* args) {
  printf("** exception error: no function clause matching %s(", meta->name);

  for (int i = 0; i < meta->arity; i++) {
    print_value_trancated(args[i]);
    if (i < meta->arity - 1) printf(", ");
  }

  printf(")\n");
  exit(-1);
}

extern void _THROW_badmatch(value_t value) {
  printf("** exception error: no match of right hand side value ");
  print_value_trancated(value);
  printf("\n");
  exit(-1);
}

extern void _THROW_badarith(value_t l, value_t r, char* op) {
  printf("** exception error: an error occurred when evaluating an arithmetic expression\n");
  printf("     in operator  %s/2\n", op);
  printf("        called as "); print_value_trancated(l); printf(" %s ", op); print_value_trancated(r); printf("\n");
  exit(-1); 
}