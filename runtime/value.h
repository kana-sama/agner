# pragma once

# include <stdlib.h>
# include <stdbool.h>

typedef int64_t value_t;
typedef int64_t PID_t;

typedef struct fun_meta_t {
  int64_t arity;
  char    name[];
} __attribute__((packed)) fun_meta_t;

typedef struct boxed_super_t {
  int64_t header;
  int64_t gc_offset;
} boxed_super_t;

typedef struct boxed_tuple_t {
  boxed_super_t super;
  int64_t       size;
  value_t       values[];
} __attribute__((packed)) boxed_tuple_t;

typedef struct boxed_cons_t {
  boxed_super_t super;
  int64_t       is_list;
  struct { value_t head; value_t tail; } values;
} __attribute__((packed)) boxed_cons_t;

typedef union boxed_value_t {
  boxed_super_t super;
  boxed_tuple_t tuple;
  boxed_cons_t  cons;
} boxed_value_t;

fun_meta_t* get_fun_meta(value_t fun);
bool is_list(value_t value);
void print_value(value_t value);
void print_value_trancated(value_t value);
