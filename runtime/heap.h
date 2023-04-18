# pragma once

# include <stdint.h>
# include <stdlib.h>

# include "value.h"

typedef struct heap_t {
  value_t* mem;
  value_t* mem_head;
  value_t* mem_end;
  int64_t  size;
} heap_t;

uint64_t gc_time();

heap_t*  heap_new(int64_t size);
void     heap_free(heap_t* heap);
heap_t*  collect_garbage(heap_t* heap, value_t* stack, value_t* stack_head);
void*    allocate(heap_t**, value_t* stack, value_t* stack_head, int64_t size);
value_t  copy_to_heap(value_t, heap_t**, value_t* stack, value_t* stack_head);
