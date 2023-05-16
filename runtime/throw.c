# include "throw.h"

# include <stdio.h>
# include <stdlib.h>

# include "value.h"
# include "shared.h"
# include "runtime.h"

_Noreturn
void _throw__unbound(char* var_atom) {
  printf("** exception error: variable ");
  print_value((value_t)var_atom);
  printf(" is unbound\n");
  exit(-1);
}

_Noreturn
void _throw__badrecord(value_t value) {
  value_t values[] = {shared_badrecord(), value};
  _runtime__raise(shared_error(), _alloc__tuple(2, values));
}

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

_Noreturn
void _throw__badarity(value_t fun, int64_t args) {
  fun_meta_t* meta = get_fun_meta(fun);
  printf("** exception error: %s/%lld called with %s\n", meta->name, meta->arity, format_args(args));
  exit(-1);
}

_Noreturn
void _throw__badfun(value_t value) {
  printf("** exception error: bad function ");
  print_value_trancated(value);
  printf("\n");
  exit(-1);
}

_Noreturn
void _throw__badmap(value_t value) {
  printf("** exception error: bad map ");
  print_value_trancated(value);
  printf("\n");
  exit(-1);
}

_Noreturn
void _throw__function_clause(fun_meta_t* meta, value_t* args) {
  printf("** exception error: no function clause matching %s(", meta->name);

  for (int i = 0; i < meta->arity; i++) {
    print_value_trancated(args[i]);
    if (i < meta->arity - 1) printf(", ");
  }

  printf(")\n");
  exit(-1);
}

_Noreturn
void _throw__badmatch(value_t value) {
  printf("** exception error: no match of right hand side value ");
  print_value_trancated(value);
  printf("\n");
  exit(-1);
}

_Noreturn
void _throw__badarith(value_t l, value_t r, char* op) {
  printf("** exception error: an error occurred when evaluating an arithmetic expression\n");
  printf("     in operator  %s/2\n", op);
  printf("        called as "); print_value_trancated(l); printf(" %s ", op); print_value_trancated(r); printf("\n");
  exit(-1);
}

_Noreturn
void _throw__badarith_unary(value_t x, char* op) {
  printf("** exception error: an error occurred when evaluating an arithmetic expression\n");
  printf("     in operator  %s/1\n", op);
  printf("        called as %s ", op); print_value_trancated(x); printf("\n");
  exit(-1);
}

_Noreturn
void _throw__badarg(fun_meta_t* meta, value_t* args) {
  printf("** exception error: bad argument\n");
  printf("     in function  %s/%lld\n", meta->name, meta->arity);
  printf("        called as %s(", meta->name);

  for (int i = 0; i < meta->arity; i++) {
    print_value_trancated(args[i]);
    if (i < meta->arity - 1) printf(", ");
  }

  printf(")\n");
  exit(-1);
}

_Noreturn
void _throw__badarg_single(value_t arg) {
  printf("** exception error: bad argument: ");
  print_value_trancated(arg);
  printf("\n");
  exit(-1);
}

_Noreturn
void _throw__badarg_unop(value_t x, char* op) {
  printf("** exception error: bad argument\n");
  printf("     in operator  %s/1\n", op);
  printf("        called as %s ", op); print_value_trancated(x); printf("\n");
  exit(-1);
}

_Noreturn
void _throw__badarg_binop(value_t l, value_t r, char* op) {
  printf("** exception error: bad argument\n");
  printf("     in operator  %s/2\n", op);
  printf("        called as "); print_value_trancated(l); printf(" %s ", op); print_value_trancated(r); printf("\n");
  exit(-1);
}

_Noreturn
void _throw__case_clause(value_t value) {
  printf("** exception error: no case clause matching ");
  print_value_trancated(value);
  printf("\n");
  exit(-1);
}
