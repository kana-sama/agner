# include <stdio.h>
# include <stdlib.h>
# include <stdbool.h>
# include <string.h>

# include "tags.h"
# include "containers/list.h"
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
  if (ref->cons.proper_list_length == -1) return false;

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

bool is_proper_list(value_t value) {
  if ((value & TAG_MASK) == NIL_TAG) return 1;
  if ((value & TAG_MASK) != BOX_TAG) return 0;
  boxed_value_t* ref = (boxed_value_t*)(value ^ BOX_TAG);
  if (ref->super.header != CONS_HEADER) return 0;
  return ref->cons.proper_list_length != -1;
}

int64_t proper_list_length(value_t value) {
  if (is_proper_list(value)) {
    if ((value & TAG_MASK) == NIL_TAG) return 0;
    return cast_to_boxed_value(value)->cons.proper_list_length;
  } else {
    return -1;
  }
}

list_t* proper_list_values(value_t value) {
  list_t* values = list_new();

  while (value != NIL_TAG) {
    boxed_value_t* ref = cast_to_boxed_value(value);
    list_append(values, (void*)ref->cons.head);
    value = ref->cons.tail;
  }
  
  return values;
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

void print_value_(value_t value, bool trancated, int available_depth) {
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
          if (trancated && available_depth == 0) {
            printf("...");
          } else {
            for (int i = 0; i < ref->tuple.size; i++) {
              print_value_(ref->tuple.values[i], trancated, available_depth - 1);
              if (i != ref->tuple.size - 1) printf(",");
            }
          }
          printf("}");
          break;
        }
        case CONS_HEADER: {
          if (printable_latin1_list(value))
            print_string(cast_to_boxed_value(value));
          else if (trancated)
            printf("[...]");
          else if (ref->cons.proper_list_length != -1)
            print_list(ref);
          else
            print_cons(ref);
          break;
        }
        case CLOSURE_HEADER: {
          puts("unimplemted: closures printed");
          exit(-1);

          break;
        }
      }

      break;
    }
  }
}

void print_value(value_t value) {
  print_value_(value, false, 3);
}

void print_value_trancated(value_t value) {
  print_value_(value, true, 3);
}

int64_t boxed_value_size(boxed_value_t* ref) {
  switch (ref->super.header) {
    case TUPLE_HEADER:
      return sizeof(boxed_tuple_t)/sizeof(value_t) + ref->tuple.size;
    case CONS_HEADER:
      return sizeof(boxed_cons_t)/sizeof(value_t);
    case CLOSURE_HEADER:
      return sizeof(boxed_closure_t)/sizeof(value_t) + ref->closure.env_size;
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
    case CLOSURE_HEADER:
      return (boxed_value_children_t){.values=(value_t*)&ref->closure.env, .count=ref->closure.env_size};
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


bool    is_number     (value_t value) { return is_integer(value);                 }
bool    is_integer    (value_t value) { return (value & TAG_MASK) == INTEGER_TAG; }
int64_t decode_integer(value_t value) { return value >> TAG_SIZE;                 }
value_t encode_integer(int64_t value) { return (value << TAG_SIZE) | INTEGER_TAG; }


// comparison

enum value_kind_t {
  KIND_NUMBER =  0,
  KIND_ATOM   =  1,
//KIND_REF    =  2,
  KIND_FUN    =  3,
//KIND_PORT   =  4,
  KIND_PID    =  5,
  KIND_TUPLE  =  6,
//KIND_MAP    =  7,
  KIND_NIL    =  8,
  KIND_LIST   =  9,
//KIND_BITS   = 10,
};

static enum value_kind_t get_value_kind(value_t value) {
  switch (value & TAG_MASK) {
    case INTEGER_TAG: return KIND_NUMBER;
    case ATOM_TAG:    return KIND_ATOM;
    case NIL_TAG:     return KIND_NIL;
    case FUN_TAG:     return KIND_FUN;
    case PID_TAG:     return KIND_PID;
    case BOX_TAG: {
      boxed_value_t* ref = cast_to_boxed_value(value);
      switch (ref->super.header) {
        case TUPLE_HEADER:   return KIND_TUPLE;
        case CONS_HEADER:    return KIND_LIST;
        case CLOSURE_HEADER: return KIND_FUN;
        default: printf("get_value_kind: unknown boxed value with tag %lld", ref->super.header); exit(-1);
      }
    }
    default: printf("get_value_kind: unknown value with tag %lld", value & TAG_MASK); exit(-1);
  }
}

bool value_lte(value_t l, value_t r) {
  if (l == r) return true;

  enum value_kind_t l_kind = get_value_kind(l);
  enum value_kind_t r_kind = get_value_kind(r);

  if (l_kind < r_kind) return true;
  if (l_kind > r_kind) return false;

  switch (l_kind) {
    case KIND_NUMBER:
      return l <= r;

    case KIND_ATOM:
      return strcmp((char*)l, (char*)r) <= 0;

    // case KIND_REF:
    //   return true;

    case KIND_FUN:
      return l <= r;

    // case KIND_PORT:
    //   return true;

    case KIND_PID:
      return l <= r;

    case KIND_TUPLE: {
      boxed_value_t* l_ref = cast_to_boxed_value(l);
      boxed_value_t* r_ref = cast_to_boxed_value(r);

      if (l_ref->tuple.size < r_ref->tuple.size) return true;
      if (l_ref->tuple.size > r_ref->tuple.size) return false;

      for (size_t i = 0; i < l_ref->tuple.size; i++) {
        if (l_ref->tuple.values[i] < r_ref->tuple.values[i]) return true;
        if (l_ref->tuple.values[i] > r_ref->tuple.values[i]) return false;
      }

      return true;
    }

    // case KIND_MAP:
    //   return true;

    case KIND_NIL:
      return true;

    case KIND_LIST: {
      boxed_value_t* l_ref = cast_to_boxed_value(l);
      boxed_value_t* r_ref = cast_to_boxed_value(r);

      value_t l_head = l_ref->cons.head;
      value_t r_head = l_ref->cons.head;

      if (value_lte(l_ref->cons.head, r_ref->cons.head)) {
        if (value_lte(r_ref->cons.head, l_ref->cons.head)) {
          __attribute__((musttail))
          return value_lte(l_ref->cons.tail, r_ref->cons.tail);
        } else {
          return true;
        }
      } else {
        return false;
      }
    }

    // case KIND_BITS:
    //   return true;

    default:
      printf("value of unknown kind %d", l_kind);
      exit(-1);
  }
}

bool value_eq(value_t l, value_t r) {
  return value_lte(l, r) && value_lte(r, l);
}

bool is_nil(value_t value) {
  return value == NIL_TAG;
}

value_t nil() {
  return NIL_TAG;
}
