# include <stdio.h>
# include <stdlib.h>
# include <stdbool.h>

# include "tags.h"
# include "value.h"

static char control_char_alias(int64_t c) {
  if (c == '\n') return 'n';
  if (c == '\r') return 'r';
  if (c == '\t') return 't';
  if (c == '\v') return 'v';
  if (c == '\b') return 'b';
  if (c == '\f') return 'f';
  if (c == '\e') return 'e';
  return 0;
}

static bool printable_latin1(value_t value) {
  if ((value & TAG_MASK) != INTEGER_TAG) return false;
  int64_t c = value >> TAG_SIZE;

  if (c >= 32 && c <= 126) return true;
  if (c >= 160 && c < 255) return true;
  if (control_char_alias(c)) return true;

  return false;
}

bool printable_latin1_list(value_t value) {
  if (value == NIL_TAG) return true;

  boxed_value_t* ref = cast_to_boxed_value(value);
  if (ref->super.header != CONS_HEADER) return false;
  if (!ref->cons.is_list) return false;

  while (ref) {
    if (!printable_latin1(ref->cons.head)) return false;
    if (ref->cons.tail == NIL_TAG) break;
    ref = cast_to_boxed_value(ref->cons.tail);
  }

  return true;
}

fun_meta_t* get_fun_meta(value_t fun) {
  int64_t fun_size = *((int64_t*)fun - 1);
  return ((void*)fun + fun_size);
}

bool is_list(value_t value) {
  if ((value & TAG_MASK) == NIL_TAG) return 1;
  if ((value & TAG_MASK) != BOX_TAG) return 0;
  boxed_value_t* ref = (boxed_value_t*)(value ^ BOX_TAG);
  if (ref->super.header != CONS_HEADER) return 0;
  return ref->cons.is_list;
}

static void print_char(int c) {
  if (c == '"')
    printf("\\\"");
  else if (c == '\\')
    printf("\\\\");
  else if (control_char_alias(c))
    printf("\\%c", control_char_alias(c));
  else
    printf("%lc", c);
}

static void print_string(boxed_value_t* ref) {
  printf("\"");
  while (ref) {
    print_char(ref->cons.head >> TAG_SIZE);
    if (ref->cons.tail == NIL_TAG) break;
    ref = cast_to_boxed_value(ref->cons.tail);
  }
  printf("\"");
}

static void print_list(boxed_value_t* ref) {
  printf("[");
  value_t value = (value_t)ref | BOX_TAG;
  while (value != NIL_TAG) {
    boxed_cons_t* cons = (boxed_cons_t*)(value ^ BOX_TAG);
    print_value(cons->head);
    if (cons->tail != NIL_TAG) printf(",");
    value = cons->tail;
  }
  printf("]");
}

static void print_cons(boxed_value_t* ref) {
  printf("[");
  print_value(ref->cons.head);
  printf("|");
  print_value(ref->cons.tail);
  printf("]");
}

void print_value_(value_t value, bool trancated) {
  switch (value & TAG_MASK) {
    case UNBOUND_TAG:
      printf("[UNBOUND]"); break;
    case INTEGER_TAG:
      printf("%lld", value >> TAG_SIZE); break;
    case ATOM_TAG:
      printf("%s", (char*) value); break;
    case FUN_TAG: {
      fun_meta_t* meta = get_fun_meta(value);
      printf("fun %s/%lld", meta->name, meta->arity);
      break;
    }
    case NIL_TAG:
      printf("[]"); break;
    case PID_TAG:
      printf("<%lld>", value >> TAG_SIZE); break;
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
          if (printable_latin1_list(value))
            print_string(cast_to_boxed_value(value));
          else if (trancated)
            printf("[...]");
          else if (ref->cons.is_list)
            print_list(ref);
          else
            print_cons(ref);
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

boxed_value_children_t boxed_value_children(boxed_value_t* ref) {
  switch (ref->super.header) {
    case TUPLE_HEADER:
      return (boxed_value_children_t){.values=ref->tuple.values, .count=ref->tuple.size};
    case CONS_HEADER:
      return (boxed_value_children_t){.values=(value_t*)&ref->cons.head, .count=2};
    default:
      printf("Unknown boxed value header: %lld\n", ref->super.header);
      exit(-1);
  }
}

boxed_value_t* cast_to_boxed_value(value_t value) {
  if ((value & TAG_MASK) != BOX_TAG) return NULL;
  return (boxed_value_t*)(value ^ BOX_TAG);
}

void dump_value(value_t value) {
  printf("%lld", value);
  
  boxed_value_t* ref = cast_to_boxed_value(value);
  if (ref) {
    printf("[");
    int64_t size = boxed_value_size(ref);
    for (int64_t i = 0; i < size; i++) {
      printf("%llx ", ((int64_t*)ref)[i]);
    }
    printf("]");
  }

  printf("\n");
}
