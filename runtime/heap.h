# pragma once

# include <stdint.h>
# include <stdlib.h>

# include "value.h"
# include "scopes.h"
# include "mailbox.h"

typedef struct heap_t {
  value_t* mem;
  value_t* mem_head;
  value_t* mem_end;
  int64_t  size;
} heap_t;

typedef struct gc_ctx_t {
  value_t*   vstack;
  value_t*   vstack_head;
  scopes_t*  scopes;
  mailbox_t* mailbox;
} gc_ctx_t;

uint64_t gc_time();

heap_t*  heap_new(int64_t size);
void     heap_free(heap_t* heap);
heap_t*  heap_gc(heap_t* heap, gc_ctx_t);
void*    heap_allocate(heap_t**, int64_t size, gc_ctx_t);
value_t  copy_to_heap(value_t, heap_t**, gc_ctx_t);
