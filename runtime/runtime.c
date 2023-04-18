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

  if ((entry & TAG_MASK) != FUN_TAG) _throw__badfun(entry);
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

void _runtime__save_vstack() {
  asm(
    "movq %%r12, %[stack_head]"
    : [stack_head] "=m" (scheduler->current->vstack_head) 
    :
    : "rdi", "rsi", "rdx", "rcx", "memory", "r12", "r13"
  );
}

void _runtime__print_value(value_t value) {
  print_value(value);
  puts("");
}

value_t _alloc__integer(int64_t i) {
  return (i << TAG_SIZE) | INTEGER_TAG;
}

value_t _alloc__tuple(int64_t size) {
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

value_t _alloc__cons() {
  boxed_cons_t* cons = allocate(
    &scheduler->current->heap,
    scheduler->current->vstack,
    scheduler->current->vstack_head,
    sizeof(boxed_cons_t)/WORD_SIZE
  );
  cons->super.header = CONS_HEADER;
  return (value_t)cons | BOX_TAG;
}

void _fill__tuple(value_t value, int64_t size, value_t* values) {
  boxed_tuple_t* tuple = (boxed_tuple_t*)(value ^ BOX_TAG);
  for (int i = 0; i < size; i++) {
    tuple->values[i] = values[i];
  }
}

void _fill__cons(value_t value, value_t head, value_t tail) {
  boxed_cons_t* cons = (boxed_cons_t*)(value ^ BOX_TAG);
  cons->head = head;
  cons->tail = tail;
  cons->proper_list_length = is_proper_list(tail) ? proper_list_length(tail) + 1 : -1;
}

value_t _receive__pick() {
  while (true) {
    value_t* value = mailbox_pick(scheduler->current->mailbox);
    if (value) {
      return *value;
    } else {
      mailbox_unpick(scheduler->current->mailbox);
      scheduler_next(scheduler);
    }
  }
}

void _receive__success() {
  mailbox_drop_picked(scheduler->current->mailbox);
  mailbox_unpick(scheduler->current->mailbox);
}
