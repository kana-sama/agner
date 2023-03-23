# include <stdlib.h>
# include <stdio.h>
# include <time.h>
# include <string.h>

# include "../src/Language/Agner/X64.h"
# include "value.h"
# include "heap.h"

uint64_t gc_time_ = 0;

typedef struct gc_traverse_stack_t {
  boxed_value_t* value;
  int64_t size;
  struct gc_traverse_stack_t* next;
} gc_traverse_stack_t;

static gc_traverse_stack_t* gc_traverse_stack__push(boxed_value_t* value, gc_traverse_stack_t* stack) {
  gc_traverse_stack_t* new_stack = malloc(sizeof(gc_traverse_stack_t));
  new_stack->value = value;
  new_stack->next = stack;
  new_stack->size = stack == NULL ? 1 : stack->size + 1;
  return new_stack;
}

static gc_traverse_stack_t* gc_traverse_stack__pop(boxed_value_t** out, gc_traverse_stack_t* stack) {
  *out = stack->value;
  gc_traverse_stack_t* next = stack->next;
  free(stack);
  return next;
}

static int64_t boxed_value_size(boxed_value_t* ref) {
  switch (ref->super.header) {
    case TUPLE_HEADER:
      return sizeof(boxed_tuple_t)/sizeof(value_t) + ref->tuple.size;
    case CONS_HEADER:
      return sizeof(boxed_cons_t)/sizeof(value_t);
    default:
      printf("Unknown boxed value header: %lld\n", ref->super.header);
      exit(-1);
  }
}

typedef struct boxed_value_children_t {
  value_t* values;
  int64_t count;
} boxed_value_children_t;

static boxed_value_children_t boxed_value_children(boxed_value_t* ref) {
  switch (ref->super.header) {
    case TUPLE_HEADER:
      return (boxed_value_children_t){ .values=ref->tuple.values, .count=ref->tuple.size };
    case CONS_HEADER:
      return (boxed_value_children_t){ .values=(value_t*)&ref->cons.values, .count=2 };
    default:
      printf("Unknown boxed value header: %lld\n", ref->super.header);
      exit(-1);
  }
}

static gc_traverse_stack_t* boxed_value_traverse(boxed_value_t* ref, gc_traverse_stack_t* stack) {
  switch (ref->super.header) {
    case TUPLE_HEADER:
      for (int i = 0; i < ref->tuple.size; i++)
        if ((ref->tuple.values[i] & TAG_MASK) == BOX_TAG)
          stack = gc_traverse_stack__push((boxed_value_t*)(ref->tuple.values[i] ^ BOX_TAG), stack);
      return stack;

    case CONS_HEADER:
      if ((ref->cons.values.head & TAG_MASK) == BOX_TAG)
        stack = gc_traverse_stack__push((boxed_value_t*)(ref->cons.values.head ^ BOX_TAG), stack);
      if ((ref->cons.values.tail & TAG_MASK) == BOX_TAG)
        stack = gc_traverse_stack__push((boxed_value_t*)(ref->cons.values.tail ^ BOX_TAG), stack);
      return stack;

    default:
      printf("Unknown boxed value header: %lld\n", ref->super.header);
      exit(-1);
  }
}

// mark and copy algorithm
extern heap_t* collect_garbage(
  heap_t* heap,
  value_t* stack,
  value_t* stack_head
) {
  struct timespec gc_start, gc_end;
  clock_gettime(CLOCK_MONOTONIC_RAW, &gc_start);
  
  int64_t stack_size = stack_head - stack;
  int64_t offset = 0;
  gc_traverse_stack_t* traverse_stack = NULL;

  for (int i = 0; i < stack_size; i++) {
    value_t value = stack[i];
    if ((value & TAG_MASK) != BOX_TAG) continue;
    boxed_value_t* ref = (boxed_value_t*)(value ^ BOX_TAG);

    if (ref->super.gc_offset == 0) {
      ref->super.gc_offset = offset;
      offset += boxed_value_size(ref);
      traverse_stack = boxed_value_traverse(ref, traverse_stack);
    }
  }

  {
    boxed_value_t* ref;
    while (traverse_stack != NULL) {
      traverse_stack = gc_traverse_stack__pop(&ref, traverse_stack);

      if (ref->super.gc_offset == 0) {
        ref->super.gc_offset = offset;
        offset += boxed_value_size(ref);
        traverse_stack = boxed_value_traverse(ref, traverse_stack);
      }
    }
  }

  heap_t* new_heap = mk_heap(offset * 2);
  new_heap->mem_head = new_heap->mem + offset;

  for (int i = 0; i < stack_size; i++) {
    value_t value = stack[i];
    if ((value & TAG_MASK) == BOX_TAG) {
      boxed_value_t* ref = (boxed_value_t*)(value ^ BOX_TAG);
      stack[i] = (value_t)(new_heap->mem + ref->super.gc_offset) | BOX_TAG;
      
      traverse_stack = gc_traverse_stack__push(ref, traverse_stack);
    }
  }

  {
    boxed_value_t* ref;
    while (traverse_stack != NULL) {
      traverse_stack = gc_traverse_stack__pop(&ref, traverse_stack);

      void* dst = (void*)(new_heap->mem + ref->super.gc_offset);
      void* src = (void*)ref;
      size_t count = boxed_value_size(ref) * sizeof(value_t);
      memcpy(dst, src, count);

      boxed_value_t* new_ref = (boxed_value_t*)dst;
      new_ref->super.gc_offset = 0;

      boxed_value_children_t children = boxed_value_children(new_ref);
      for (int i = 0; i < children.count; i++) {
        if ((children.values[i] & TAG_MASK) == BOX_TAG) {
          boxed_value_t* ref = (boxed_value_t*)(children.values[i] ^ BOX_TAG);
          children.values[i] = (value_t)(new_heap->mem + ref->super.gc_offset) | BOX_TAG;
        }
      }
      
      traverse_stack = boxed_value_traverse(ref, traverse_stack);
    }
  }

  free(heap->mem);
  free(heap);

  clock_gettime(CLOCK_MONOTONIC_RAW, &gc_end);
  gc_time_ += (gc_end.tv_sec - gc_start.tv_sec) * 1000 + (gc_end.tv_nsec - gc_start.tv_nsec) / 1000000;

  return new_heap;
}

extern heap_t* mk_heap(int64_t size) {
  heap_t* heap = malloc(sizeof(heap_t));
  heap->mem = malloc(size * sizeof(value_t));
  heap->mem_head = heap->mem;
  heap->mem_end = heap->mem + size;
  heap->size = size;
  return heap;
}

extern void* allocate(heap_t** heap, value_t* stack, value_t* stack_head, int64_t size) {
  if ((*heap)->mem_end - (*heap)->mem_head < size) {
    *heap = collect_garbage(*heap, stack, stack_head);
  }

  value_t* target = (*heap)->mem_head;
  (*heap)->mem_head += size;
  for (int i = 0; i < size; i++) target[i] = 0;

  return target;
}

extern uint64_t gc_time() {
  return gc_time_;
}