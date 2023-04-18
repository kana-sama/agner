# pragma once

# include <stdlib.h>
# include <stdbool.h>

typedef int64_t value_t;
typedef int64_t PID_t;

typedef struct fun_meta_t {
  int64_t arity;
  char    name[];
} fun_meta_t;

typedef struct boxed_super_t {
  int64_t header;
  int64_t gc_offset;
} boxed_super_t;

typedef struct boxed_tuple_t {
  boxed_super_t super;
  int64_t size;
  value_t values[];
} boxed_tuple_t;

typedef struct boxed_cons_t {
  boxed_super_t super;
  int64_t is_list;
  value_t head;
  value_t tail;
} boxed_cons_t;

typedef union boxed_value_t {
  boxed_super_t super;
  boxed_tuple_t tuple;
  boxed_cons_t  cons;
} boxed_value_t;

typedef struct boxed_value_children_t {
  value_t* values;
  int64_t count;
} boxed_value_children_t;

fun_meta_t* get_fun_meta(value_t fun);
bool is_list(value_t value);
void print_value(value_t value);
void print_value_trancated(value_t value);
int64_t boxed_value_size(boxed_value_t*);
boxed_value_children_t boxed_value_children(boxed_value_t*);

boxed_value_t* cast_to_boxed_value(value_t);


bool printable_latin1_list(value_t);

void dump_value(value_t);

bool is_number(value_t);
bool is_integer(value_t);
int64_t decode_integer(value_t);
value_t encode_integer(int64_t);

bool value_lte(value_t, value_t);
bool value_eq(value_t, value_t);
