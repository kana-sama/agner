# include "bifs.h"

# include <stdio.h>
# include <string.h>
# include <time.h>

# include "value.h"
# include "runtime.h"
# include "tags.h"
# include "throw.h"
# include "scheduler.h"
# include "shared_atoms.h"

static
fun_meta_t* get_meta(bif_context_t ctx) {
  return ctx;
}

value_t _agner__print(bif_context_t ctx, value_t value) {
  print_value(value);
  return shared_atom_ok();
}

value_t _agner__println(bif_context_t ctx, value_t value) {
  print_value(value);
  printf("\n");
  fflush(stdout);
  return shared_atom_ok();
}

value_t _agner__put_char(bif_context_t ctx, value_t value) {
  value_t args[1] = { value };
  if ((value & TAG_MASK) != INTEGER_TAG) _throw__badarg(get_meta(ctx), args);
  printf("%lc", (int)value >> TAG_SIZE);
  return shared_atom_ok();
}

value_t _agner__put_str(bif_context_t ctx, value_t value) {
  value_t args[1] = { value };
  if (!is_proper_list(value) || !printable_latin1_list(value)) _throw__badarg(get_meta(ctx), args);

  if (value != NIL_TAG) {
    boxed_value_t* ref = cast_to_boxed_value(value);
    while (true) {
      printf("%lc", (int)ref->cons.head >> TAG_SIZE);
      if (ref->cons.tail == NIL_TAG) break;
      ref = cast_to_boxed_value(ref->cons.tail);
    }
  }

  return shared_atom_ok();
}

value_t _erlang__error(bif_context_t ctx, value_t value) {
  printf("** exception error: ");
  print_value(value);
  printf("\n");
  exit(-1);
}

value_t _timer__sleep(bif_context_t ctx, value_t duration) {
  switch (duration & TAG_MASK) {
    case INTEGER_TAG: {
      struct timespec ts;
      int64_t msec = duration >> TAG_SIZE;
      int res;

      ts.tv_sec = msec / 1000;
      ts.tv_nsec = (msec % 1000) * 1000000;

      do res = nanosleep(&ts, &ts); while (res);
      return shared_atom_ok();
    }
    case ATOM_TAG: {
      if (duration == shared_atom_infinity()) {
        while (true);
        return shared_atom_ok();
      }
    }
    default: {
      value_t args[1] = {duration};
      _throw__function_clause(get_meta(ctx), args);
      return shared_atom_ok();
    }
  }
}

value_t _erlang__spawn(bif_context_t ctx, value_t value) {
  if ((value & TAG_MASK) != FUN_TAG) _throw__badfun(value);
  action_t action = (action_t)value;
  PID_t pid = scheduler_spawn(scheduler, action);
  return pid << TAG_SIZE | PID_TAG;
}

value_t _erlang__self(bif_context_t ctx) {
  return scheduler->current->pid << TAG_SIZE | PID_TAG;
}

value_t _erlang__send(bif_context_t ctx, value_t target, value_t msg) {
  if ((target & TAG_MASK) != PID_TAG) _throw__badarg_binop(target, msg, "!");
  PID_t pid = target >> TAG_SIZE;
  process_send(pid, msg);
  return msg;
}

value_t _erlang__garbage_collect(bif_context_t ctx) {
  allocate(0);
  return shared_atom_true();
}

value_t _erlang__is_atom(bif_context_t ctx, value_t value) {
  if ((value & TAG_MASK) == ATOM_TAG)
    return shared_atom_true();
  else
    return shared_atom_false();
}

value_t _erlang__is_list(bif_context_t ctx, value_t value) {
  if (value == NIL_TAG) return shared_atom_true();

  boxed_value_t* ref = cast_to_boxed_value(value);
  if (ref && ref->super.header == CONS_HEADER) return shared_atom_true();

  return shared_atom_false();
}

value_t _erlang__is_integer(bif_context_t ctx, value_t value) {
  if ((value & TAG_MASK) == INTEGER_TAG)
    return shared_atom_true();
  else
    return shared_atom_false();
}

value_t _erlang__is_tuple(bif_context_t ctx, value_t value) {
  boxed_value_t* ref = cast_to_boxed_value(value);
  if (ref && ref->super.header == TUPLE_HEADER)
    return shared_atom_true();
  else
    return shared_atom_false();
}

value_t _erlang__is_function(bif_context_t ctx, value_t value) {
  if ((value & TAG_MASK) == FUN_TAG)
    return shared_atom_true();
  else
    return shared_atom_false();
}

value_t _erlang__is_pid(bif_context_t ctx, value_t value) {
  if ((value & TAG_MASK) == PID_TAG)
    return shared_atom_true();
  else
    return shared_atom_false();
}



// value_t _erlang__atom_to_list(bif_context_t ctx, value_t value) {
//   if ((value & TAG_MASK) != ATOM_TAG) {
//     value_t args[1] = {value};
//     _throw__badarg(get_meta(ctx), args);
//   }

//   char* name = (char*)value;
//   char* name_end = name + strlen(name);
//   value_t list = NIL_TAG;
//   for (char* c = name + strlen(name) - 1; c >= name; c--) {
//     value_t new_list = _alloc__cons();
//     _fill__cons(new_list, encode_integer(*c), list);
//     list = new_list;
//   }

//   return list;
// }

// value_t _erlang__integer_to_list(bif_context_t ctx, value_t value) {
//   if ((value & TAG_MASK) != INTEGER_TAG) {
//     value_t args[1] = {value};
//     _throw__badarg(get_meta(ctx), args);
//   }

//   char* str;
//   asprintf(&str, "%lld", value >> TAG_SIZE);

//   value_t list = NIL_TAG;
//   for (int i = strlen(str) - 1; i >= 0; i--) {
//     value_t new_list = _alloc__cons();
//     _fill__cons(new_list, encode_integer(str[i]), list);
//     list = new_list;
//   }

//   free(str);

//   return list;
// }

// value_t _erlang__tuple_to_list(bif_context_t ctx, value_t value) {
//   boxed_value_t* ref = cast_to_boxed_value(value);
//   if (!ref || ref->super.header != TUPLE_HEADER) {
//     value_t args[1] = {value};
//     _throw__badarg(get_meta(ctx), args);
//   }

//   value_t list = NIL_TAG;
//   for (value_t *v = ref->tuple.values + ref->tuple.size - 1; v >= ref->tuple.values; v -= 1) {
//     value_t new_list = _alloc__cons();
//     _fill__cons(new_list, *v, list);
//     list = new_list;
//   }
//   return list;
// }

// value_t _erlang__fun_to_list(bif_context_t ctx, value_t value) {
//   if ((value & TAG_MASK) != FUN_TAG) {
//     value_t args[1] = {value};
//     _throw__badarg(get_meta(ctx), args);
//   }

//   fun_meta_t* meta = get_fun_meta(value);
//   char* str;
//   asprintf(&str, "fun %s/%lld", meta->name, meta->arity);

//   value_t list = NIL_TAG;
//   for (int i = strlen(str) - 1; i >= 0; i--) {
//     value_t new_list = _alloc__cons();
//     _fill__cons(new_list, encode_integer(str[i]), list);
//     list = new_list;
//   }

//   free(str);

//   return list;
// }

// value_t _erlang__pid_to_list(bif_context_t ctx, value_t value) {
//   if ((value & TAG_MASK) != PID_TAG) {
//     value_t args[1] = {value};
//     _throw__badarg(get_meta(ctx), args);
//   }

//   char* str;
//   asprintf(&str, "<%lld>", value >> TAG_SIZE);

//   value_t list = NIL_TAG;
//   for (int i = strlen(str) - 1; i >= 0; i--) {
//     value_t new_list = _alloc__cons();
//     _fill__cons(new_list, encode_integer(str[i]), list);
//     list = new_list;
//   }

//   free(str);

//   return list;
// }
