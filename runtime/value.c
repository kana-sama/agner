# include <stdio.h>
# include <stdlib.h>
# include <stdbool.h>

# include "../src/Language/Agner/X64.h"
# include "value.h"

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
    case PID_TAG: {
      printf("<%lld>", value >> TAG_SIZE);
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
