# include "bifs.h"

# include <stdio.h>
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
  printf("\n");
  fflush(stdout);
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
    case NUMBER_TAG: {
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
  if ((target & TAG_MASK) != PID_TAG) _THROW_badarg_send(target, msg);
  PID_t pid = target >> TAG_SIZE;
  process_send(pid, msg);
  return msg;
}