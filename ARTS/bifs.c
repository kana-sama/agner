# include "bifs.h"

# include <stdio.h>
# include <string.h>
# include <time.h>

# include "value.h"
# include "runtime.h"
# include "tags.h"
# include "throw.h"
# include "scheduler.h"

value_t _runtime__calling_context[10];

// _runtime__calling_context[0] should be "ok" atom
value_t _agner__print(value_t value) {
  _runtime__yield("agner:print/1");
  
  print_value(value);
  return _runtime__calling_context[0];
}

// _runtime__calling_context[0] should be "ok" atom
value_t _agner__println(value_t value) {
  _runtime__yield("agner:println/1");
  
  print_value(value);
  printf("\n");
  fflush(stdout);
  return _runtime__calling_context[0];
}

// _runtime__calling_context[0] should be "ok" atom
static fun_meta_t _agner__put_char__meta = {.arity = 1, .name = "agner:put_char"};
value_t _agner__put_char(value_t value) {
  _runtime__yield("agner:put_char/1");

  value_t args[1] = { value };
  if ((value & TAG_MASK) != INTEGER_TAG) _THROW_badarg(&_agner__put_char__meta, args);
  printf("%lc", (int)value >> TAG_SIZE);
  return _runtime__calling_context[0];
}

// _runtime__calling_context[0] should be "ok" atom
static fun_meta_t _agner__put_str__meta = {.arity = 1, .name = "agner:put_str"};
value_t _agner__put_str(value_t value) {
  _runtime__yield("agner:put_str/1");

  value_t args[1] = { value };
  if (!is_list(value) || !printable_latin1_list(value)) _THROW_badarg(&_agner__put_str__meta, args);

  if (value != NIL_TAG) {
    boxed_value_t* ref = cast_to_boxed_value(value);
    while (true) {
      printf("%lc", (int)ref->cons.head >> TAG_SIZE);
      if (ref->cons.tail == NIL_TAG) break;
      ref = cast_to_boxed_value(ref->cons.tail);
    }
  }

  return _runtime__calling_context[0];
}

value_t _erlang__error(value_t value) {
  _runtime__yield("erlang:error/1");

  printf("** exception error: ");
  print_value(value);
  printf("\n");
  exit(-1);
}

// _runtime__calling_context[0] should be "ok" atom
// _runtime__calling_context[1] should be "infinity" atom
static fun_meta_t _timer__sleep__meta = {.arity = 1, .name = "timer:sleep"};
value_t _timer__sleep(value_t duration) {
  _runtime__yield("timer:sleep/1");

  switch (duration & TAG_MASK) {
    case INTEGER_TAG: {
      struct timespec ts;
      int64_t msec = duration >> TAG_SIZE;
      int res;

      ts.tv_sec = msec / 1000;
      ts.tv_nsec = (msec % 1000) * 1000000;

      do res = nanosleep(&ts, &ts); while (res);
      return _runtime__calling_context[0];
    }
    case ATOM_TAG: {
      if (duration == _runtime__calling_context[1]) {
        while (true);
        return _runtime__calling_context[0];
      }
    }
    default: {
      value_t args[1] = {duration};
      _THROW_function_clause(&_timer__sleep__meta, args);
      return _runtime__calling_context[0];
    }
  }
}

value_t _erlang__spawn(value_t value) {
  _runtime__yield("erlang:spawn/1");

  if ((value & TAG_MASK) != FUN_TAG) _THROW_badfun(value);
  action_t action = (action_t)value;
  PID_t pid = scheduler_spawn(scheduler, action);
  return pid << TAG_SIZE | PID_TAG;
}

value_t _erlang__self() {
  _runtime__yield("erlang:self/0");

  return scheduler->current->pid << TAG_SIZE | PID_TAG;
}

value_t _erlang__send(value_t target, value_t msg) {
  if ((target & TAG_MASK) != PID_TAG) _THROW_badarg_op(target, msg, "!");
  PID_t pid = target >> TAG_SIZE;
  process_send(pid, msg);
  return msg;
}

// _runtime__calling_context[0] should be "true" atom
// _runtime__calling_context[1] should be "false" atom
value_t _erlang__is_atom__1(value_t value) {
  value_t _true  = _runtime__calling_context[0];
  value_t _false = _runtime__calling_context[1];
  
  if ((value & TAG_MASK) == ATOM_TAG)
    return _true;
  else
    return _false;
}

// _runtime__calling_context[0] should be "true" atom
// _runtime__calling_context[1] should be "false" atom
value_t _erlang__is_list__1(value_t value) {
  value_t _true  = _runtime__calling_context[0];
  value_t _false = _runtime__calling_context[1];

  if (value == NIL_TAG) return _true;

  boxed_value_t* ref = cast_to_boxed_value(value);
  if (ref && ref->super.header == CONS_HEADER) return _true;

  return _false;
}

// _runtime__calling_context[0] should be "true" atom
// _runtime__calling_context[1] should be "false" atom
value_t _erlang__is_integer__1(value_t value) {
  value_t _true  = _runtime__calling_context[0];
  value_t _false = _runtime__calling_context[1];

  if ((value & TAG_MASK) == INTEGER_TAG)
    return _true;
  else
    return _false;
}

// _runtime__calling_context[0] should be "true" atom
// _runtime__calling_context[1] should be "false" atom
value_t _erlang__is_tuple__1(value_t value) {
  value_t _true  = _runtime__calling_context[0];
  value_t _false = _runtime__calling_context[1];

  boxed_value_t* ref = cast_to_boxed_value(value);
  if (ref && ref->super.header == TUPLE_HEADER)
    return _true;
  else
    return _false;
}

// _runtime__calling_context[0] should be "true" atom
// _runtime__calling_context[1] should be "false" atom
value_t _erlang__is_function__1(value_t value) {
  value_t _true  = _runtime__calling_context[0];
  value_t _false = _runtime__calling_context[1];

  if ((value & TAG_MASK) == FUN_TAG)
    return _true;
  else
    return _false;
}

// _runtime__calling_context[0] should be "true" atom
// _runtime__calling_context[1] should be "false" atom
value_t _erlang__is_pid__1(value_t value) {
  value_t _true  = _runtime__calling_context[0];
  value_t _false = _runtime__calling_context[1];

  if ((value & TAG_MASK) == PID_TAG)
    return _true;
  else
    return _false;
}


static fun_meta_t _erlang__atom_to_list__1__meta = {.arity = 1, .name = "erlang:atom_to_list"};
value_t _erlang__atom_to_list__1(value_t value) {
  if ((value & TAG_MASK) != ATOM_TAG) {
    value_t args[1] = {value};
    _THROW_badarg(&_erlang__atom_to_list__1__meta, args);
  }

  char* name = (char*)value;
  char* name_end = name + strlen(name);
  value_t list = NIL_TAG;
  for (char* c = name + strlen(name) - 1; c >= name; c--) {
    value_t new_list = _runtime__alloc_cons();
    boxed_cons_t* cons = &cast_to_boxed_value(new_list)->cons;
    cons->is_list = 1;
    cons->head = (int64_t)(*c) << TAG_SIZE | INTEGER_TAG;
    cons->tail = list;
    list = new_list;
  }

  return list;
}

static fun_meta_t _erlang__integer_to_list__1__meta = {.arity = 1, .name = "erlang:integer_to_list"};
value_t _erlang__integer_to_list__1(value_t value) {
  if ((value & TAG_MASK) != INTEGER_TAG) {
    value_t args[1] = {value};
    _THROW_badarg(&_erlang__integer_to_list__1__meta, args);
  }

  char* str;
  asprintf(&str, "%lld", value >> TAG_SIZE);

  value_t list = NIL_TAG;
  for (int i = strlen(str) - 1; i >= 0; i--) {
    value_t new_list = _runtime__alloc_cons();
    boxed_value_t* new_list_ref = cast_to_boxed_value(new_list);
    new_list_ref->cons.is_list = 1;
    new_list_ref->cons.head = str[i] << TAG_SIZE | INTEGER_TAG;
    new_list_ref->cons.tail = list;
    list = new_list;
  }

  free(str);

  return list;
}

static fun_meta_t _erlang__tuple_to_list__1__meta = {.arity = 1, .name = "erlang:tuple_to_list"};
value_t _erlang__tuple_to_list__1(value_t value) {
  boxed_value_t* ref = cast_to_boxed_value(value);
  if (!ref || ref->super.header != TUPLE_HEADER) {
    value_t args[1] = {value};
    _THROW_badarg(&_erlang__tuple_to_list__1__meta, args);
  }

  value_t list = NIL_TAG;
  for (value_t *v = ref->tuple.values + ref->tuple.size - 1; v >= ref->tuple.values; v -= 1) {
    value_t new_list = _runtime__alloc_cons();
    boxed_value_t* new_list_ref = cast_to_boxed_value(new_list);
    new_list_ref->cons.is_list = 1;
    new_list_ref->cons.head = *v;
    new_list_ref->cons.tail = list;
    list = new_list;
  }
  return list;
}

static fun_meta_t _erlang__fun_to_list__1__meta = {.arity = 1, .name = "erlang:fun_to_list"};
value_t _erlang__fun_to_list__1(value_t value) {
  if ((value & TAG_MASK) != FUN_TAG) {
    value_t args[1] = {value};
    _THROW_badarg(&_erlang__fun_to_list__1__meta, args);
  }

  fun_meta_t* meta = get_fun_meta(value);
  char* str;
  asprintf(&str, "fun %s/%lld", meta->name, meta->arity);

  value_t list = NIL_TAG;
  for (int i = strlen(str) - 1; i >= 0; i--) {
    value_t new_list = _runtime__alloc_cons();
    boxed_value_t* new_list_ref = cast_to_boxed_value(new_list);
    new_list_ref->cons.is_list = 1;
    new_list_ref->cons.head = str[i] << TAG_SIZE | INTEGER_TAG;
    new_list_ref->cons.tail = list;
    list = new_list;
  }

  free(str);

  return list;
}

static fun_meta_t _erlang__pid_to_list__1__meta = {.arity = 1, .name = "erlang:pid_to_list"};
value_t _erlang__pid_to_list__1(value_t value) {
  if ((value & TAG_MASK) != PID_TAG) {
    value_t args[1] = {value};
    _THROW_badarg(&_erlang__pid_to_list__1__meta, args);
  }

  char* str;
  asprintf(&str, "<%lld>", value >> TAG_SIZE);

  value_t list = NIL_TAG;
  for (int i = strlen(str) - 1; i >= 0; i--) {
    value_t new_list = _runtime__alloc_cons();
    boxed_value_t* new_list_ref = cast_to_boxed_value(new_list);
    new_list_ref->cons.is_list = 1;
    new_list_ref->cons.head = str[i] << TAG_SIZE | INTEGER_TAG;
    new_list_ref->cons.tail = list;
    list = new_list;
  }

  free(str);

  return list;
}


value_t _binop__plusplus(value_t l, value_t r) {
  if (!(is_list(l))) _THROW_badarg_op(l, r, "++");

  int64_t result_is_list = is_list(r);
  
  // case []
  if (l == NIL_TAG) return r;

  // case [H|T]
  value_t new = _runtime__alloc_cons();
  boxed_value_t* ref = cast_to_boxed_value(l);
  boxed_value_t* cur = cast_to_boxed_value(new);

  while (true) {
    cur->cons.head = ref->cons.head;
    cur->cons.is_list = result_is_list;
    if (ref->cons.tail == NIL_TAG) {
      cur->cons.tail = r;
      break;
    } else {
      cur->cons.tail = _runtime__alloc_cons();
      cur = cast_to_boxed_value(cur->cons.tail);
      ref = cast_to_boxed_value(ref->cons.tail);
    }
  }

  return new;
}

value_t _binop__gte(value_t l, value_t r, value_t _true, value_t _false) {
  return l >= r ? _true : _false;
}

value_t _binop__lte(value_t l, value_t r, value_t _true, value_t _false) {
  return l <= r ? _true : _false;
}