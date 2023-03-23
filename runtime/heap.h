# pragma once

# include <stdlib.h>

# include "value.h"

typedef struct heap_t {
  value_t* mem;
  value_t* mem_head;
  value_t* mem_end;
  int64_t  size;
} heap_t;

heap_t*  mk_heap(int64_t size);
heap_t*  collect_garbage(heap_t* heap, value_t* stack, value_t* stack_head);
void*    allocate(heap_t** heap, value_t* stack, value_t* stack_head, int64_t size);
uint64_t gc_time();