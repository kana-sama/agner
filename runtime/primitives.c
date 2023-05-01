# include "primitives.h"

# include <stdio.h>
# include <string.h>
# include <time.h>

# include "value.h"
# include "heap.h"
# include "process.h"
# include "asserts.h"
# include "runtime.h"
# include "tags.h"
# include "throw.h"
# include "scheduler.h"
# include "shared.h"

static
fun_meta_t* get_meta(bif_context_t ctx) {
  return ctx;
}

value_t _agner__print__1(bif_context_t ctx, value_t value) {
  print_value(value);
  return shared_ok();
}

value_t _agner__println__1(bif_context_t ctx, value_t value) {
  print_value(value);
  printf("\n");
  fflush(stdout);
  return shared_ok();
}

value_t _agner__put_char__1(bif_context_t ctx, value_t value) {
  if ((value & TAG_MASK) != INTEGER_TAG) {
    value_t args[1] = { value };
    _throw__badarg(get_meta(ctx), args);
  }

  printf("%lc", (int)decode_integer(value));
  return shared_ok();
}

value_t _agner__put_str__1(bif_context_t ctx, value_t value) {
  if (!is_proper_list(value) || !printable_latin1_list(value)) {
    value_t args[1] = { value };
    _throw__badarg(get_meta(ctx), args);
  }

  while (!is_nil(value)) {
    boxed_cons_t cons = cast_to_boxed_value(value)->cons;
    printf("%lc", (int)decode_integer(cons.head));
    value = cons.tail;
  }

  return shared_ok();
}

value_t _erlang__error__1(bif_context_t ctx, value_t value) {
  printf("** exception error: ");
  print_value(value);
  printf("\n");
  exit(-1);
}

// value_t _timer__sleep(bif_context_t ctx, value_t duration) {
//   switch (duration & TAG_MASK) {
//     case INTEGER_TAG: {
//       struct timespec ts;
//       int64_t msec = duration >> TAG_SIZE;
//       int res;

//       ts.tv_sec = msec / 1000;
//       ts.tv_nsec = (msec % 1000) * 1000000;

//       do res = nanosleep(&ts, &ts); while (res);
//       return shared_ok();
//     }
//     case ATOM_TAG: {
//       if (duration == shared_infinity()) {
//         while (true);
//         return shared_ok();
//       }
//     }
//     default: {
//       value_t args[1] = {duration};
//       _throw__function_clause(get_meta(ctx), args);
//       return shared_ok();
//     }
//   }
// }

static void spawn_wrapper(process_t* self, value_t* value_) {
  value_t  value = *value_;
  action_t action;

  switch (_assert__fun(value, 0)) {
    case FUN_KIND_STATIC:
      action = (action_t)value;
      break;

    case FUN_KIND_CLOSURE: {
      boxed_value_t* ref = cast_to_boxed_value(value);
      asm(" movq %[env], %%r13"
        :
        : [env] "r" (ref->closure.env)
        : "r13"
      );
      action = (action_t)(ref->closure.body);
      break;
    }
  }

  action(self, NULL);
}

value_t _erlang__spawn__1(bif_context_t ctx, value_t value) {
  _assert__fun(value, -1);

  value_t* copied_value = malloc(sizeof(value_t));
  process_t* spawned = scheduler_spawn(scheduler, (action_t)spawn_wrapper, copied_value);
  *copied_value = copy_to_heap(value, &spawned->heap, process_gc_ctx(spawned));
  
  return (spawned->pid) << TAG_SIZE | PID_TAG;
}

value_t _erlang__self__0(bif_context_t ctx) {
  return scheduler->current->pid << TAG_SIZE | PID_TAG;
}

value_t _erlang__send__2(bif_context_t ctx, value_t target, value_t msg) {
  if ((target & TAG_MASK) != PID_TAG) _throw__badarg_binop(target, msg, "!");
  PID_t pid = target >> TAG_SIZE;
  process_send(pid, msg);
  return msg;
}

value_t _erlang__garbage_collect__0(bif_context_t ctx) {
  scheduler->current->heap = heap_gc(scheduler->current->heap, process_gc_ctx(scheduler->current));
  return shared_true();
}

// value_t _erlang__is_atom(bif_context_t ctx, value_t value) {
//   if ((value & TAG_MASK) == ATOM_TAG)
//     return shared_true();
//   else
//     return shared_false();
// }

value_t _erlang__is_integer__1(bif_context_t ctx, value_t value) {
  if ((value & TAG_MASK) == INTEGER_TAG)
    return shared_true();
  else
    return shared_false();
}

value_t _erlang__is_list__1(bif_context_t ctx, value_t value) {
  if (is_nil(value)) return shared_true();

  boxed_value_t* ref = cast_to_boxed_value(value);
  if (ref && ref->super.header == CONS_HEADER) return shared_true();

  return shared_false();
}

// value_t _erlang__is_tuple(bif_context_t ctx, value_t value) {
//   boxed_value_t* ref = cast_to_boxed_value(value);
//   if (ref && ref->super.header == TUPLE_HEADER)
//     return shared_true();
//   else
//     return shared_false();
// }

// value_t _erlang__is_function(bif_context_t ctx, value_t value) {
//   if ((value & TAG_MASK) == FUN_TAG)
//     return shared_true();
//   else
//     return shared_false();
// }

// value_t _erlang__is_pid(bif_context_t ctx, value_t value) {
//   if ((value & TAG_MASK) == PID_TAG)
//     return shared_true();
//   else
//     return shared_false();
// }



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

value_t _erlang__integer_to_list__1(bif_context_t ctx, value_t value) {
  if (!is_integer(value)) {
    value_t args[1] = {value};
    _throw__badarg(get_meta(ctx), args);
  }

  char* str; asprintf(&str, "%lld", decode_integer(value));

  value_t list = nil();
  for (int i = strlen(str) - 1; i >= 0; i--)
    list = _alloc__cons(encode_integer(str[i]), list);

  free(str);

  return list;
}

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


value_t _agner__map_to_boxed__1(bif_context_t ctx, value_t value) {
  _assert__map(value);
  return value - MAP_TAG + BOX_TAG;
}

value_t _agner__boxed_to_map__1(bif_context_t ctx, value_t value) {
  if ((value & TAG_MASK) == BOX_TAG) {
    return value - BOX_TAG + MAP_TAG;
  } else {
    puts("expected boxed value");
    exit(-1);
  }
}
