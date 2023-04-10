# include "runtime.h"

# include <stdio.h>
# include <stdlib.h>
# include <stdbool.h>
# include <time.h>
# include <string.h>

# include "tags.h"
# include "options.h"
# include "value.h"
# include "throw.h"
# include "heap.h"
# include "scheduler.h"
# include "shared_atoms.h"

scheduler_t* scheduler;
FILE* ylog;

void _runtime__start(value_t entry) {
  read_env_options();

  if (options.ylog) {
    ylog = fopen(options.ylog, "w");
    fprintf(ylog, "id,pid,function,fuel,vstack_size\n");
  }

  struct timespec rt_start, rt_end;
  clock_gettime(CLOCK_MONOTONIC_RAW, &rt_start);

  if ((entry & TAG_MASK) != FUN_TAG) _THROW_badfun(entry);
  scheduler = scheduler_new();
  scheduler_run(scheduler, (action_t)entry);
  scheduler_free(scheduler);

  clock_gettime(CLOCK_MONOTONIC_RAW, &rt_end);

  if (ylog) fclose(ylog);
  
  if (options.stat) {
    uint64_t total_time = (rt_end.tv_sec - rt_start.tv_sec) * 1000 + (rt_end.tv_nsec - rt_start.tv_nsec) / 1000000;
    printf("total time: %lldms\n"
           "   gc time: %lldms\n", total_time, gc_time());
  }
}

void _runtime__yield(char* name) {
  if (options.ylog) {
    static int64_t log_line_id = 0; log_line_id++;
    fprintf(ylog, "%lld,%lld,%s,%lld,%lld\n",
      log_line_id,
      scheduler->current->pid,
      name,
      scheduler->fuel,
      (int64_t)(scheduler->current->vstack_head - scheduler->current->vstack)
    );
  }

  scheduler_yield(scheduler);
}

void _runtime__save_vstack(value_t* vstack_head) {
  scheduler->current->vstack_head = vstack_head;
}

void _runtime__print_value(value_t value) {
  print_value(value);
  puts("");
}

value_t _runtime__alloc_tuple(int64_t size) {
  boxed_tuple_t* tuple = allocate(
    &scheduler->current->heap,
    scheduler->current->vstack,
    scheduler->current->vstack_head,
    sizeof(boxed_tuple_t)/WORD_SIZE + size
  );
  tuple->super.header = TUPLE_HEADER;
  tuple->size = size;
  return (value_t)tuple | BOX_TAG;
}

void _runtime__fill_tuple(value_t value, int64_t size, value_t* values) {
  boxed_tuple_t* tuple = (boxed_tuple_t*)(value ^ BOX_TAG);
  for (int i = 0; i < size; i++) {
    tuple->values[i] = values[i];
  }
}

value_t* _runtime__match_tuple(value_t value, int64_t size) {
  if ((value & TAG_MASK) != BOX_TAG) return 0;
  boxed_value_t* ref = (boxed_value_t*)(value ^ BOX_TAG);
  if (ref->super.header != TUPLE_HEADER) return 0;
  if (ref->tuple.size != size) return 0;
  return ref->tuple.values;
}

value_t _runtime__alloc_cons() {
  boxed_cons_t* cons = allocate(
    &scheduler->current->heap,
    scheduler->current->vstack,
    scheduler->current->vstack_head,
    sizeof(boxed_cons_t)/WORD_SIZE
  );
  cons->super.header = CONS_HEADER;
  return (value_t)cons | BOX_TAG;
}

void _runtime__fill_cons(value_t value, value_t head, value_t tail) {
  boxed_cons_t* cons = (boxed_cons_t*)(value ^ BOX_TAG);
  cons->head = head;
  cons->tail = tail;
  cons->is_list = is_list(tail) ? 1 : 0;
}

value_t* _runtime__match_cons(value_t value) {
  if ((value & TAG_MASK) != BOX_TAG) return 0;
  boxed_value_t* ref = (boxed_value_t*)(value ^ BOX_TAG);
  if (ref->super.header != CONS_HEADER) return 0;
  return &ref->cons.head;
}

value_t _runtime__assert_bool_arg(value_t value) {
  if (value == shared_atom_true()) return shared_atom_true();
  if (value == shared_atom_false()) return shared_atom_false();
  _THROW_badarg_single(value);
}

void _runtime__receive_pick() {
  while (true) {
    mailbox_pick(scheduler->current->mailbox);
    if (mailbox_picked(scheduler->current->mailbox)) {
      break;
    } else {
      mailbox_unpick(scheduler->current->mailbox);
      scheduler_next(scheduler);
    }
  }
}

value_t _runtime__receive_picked() {
  return *mailbox_picked(scheduler->current->mailbox);
}

void _runtime__receive_success() {
  mailbox_drop_picked(scheduler->current->mailbox);
  mailbox_unpick(scheduler->current->mailbox);
}
