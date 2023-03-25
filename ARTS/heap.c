# include "heap.h"

# include <stdlib.h>
# include <stdio.h>
# include <time.h>
# include <string.h>

# include "tags.h"
# include "value.h"
# include "list.h"

static uint64_t gc_time_ = 0;
uint64_t gc_time() { return gc_time_; }

static void append_children(list_t* queue, boxed_value_t* ref) {
  boxed_value_children_t children = boxed_value_children(ref);
  for (int64_t i = 0; i < children.count; i++) {
    boxed_value_t* child = cast_to_boxed(children.values[i]); if (!child) continue;
    list_append(queue, child);
  }
}

// mark and copy algorithm
heap_t* collect_garbage(
  heap_t* heap,
  value_t* stack,
  value_t* stack_head
) {
  struct timespec gc_start, gc_end;
  clock_gettime(CLOCK_MONOTONIC_RAW, &gc_start);
  
  int64_t stack_size = stack_head - stack;
  int64_t offset = 0;
  list_t* queue = list_new();

  for (int i = 0; i < stack_size; i++) {
    boxed_value_t* ref = cast_to_boxed(stack[i]); if (!ref) continue;;

    if (ref->super.gc_offset == 0) {
      ref->super.gc_offset = offset;
      offset += boxed_value_size(ref);
      append_children(queue, ref);
    }
  }

  while (!list_null(queue)) {
    boxed_value_t* ref = list_shift(queue);

    if (ref->super.gc_offset == 0) {
      ref->super.gc_offset = offset;
      offset += boxed_value_size(ref);
      append_children(queue, ref);
    }
  }

  heap_t* new_heap = heap_new(offset * 2);
  new_heap->mem_head = new_heap->mem + offset;

  for (int i = 0; i < stack_size; i++) {
    value_t value = stack[i];
    boxed_value_t* ref = cast_to_boxed(stack[i]); if (!ref) continue;
    stack[i] = (value_t)(new_heap->mem + ref->super.gc_offset) | BOX_TAG;
    list_append(queue, ref);
  }

  while (!list_null(queue)) {
    boxed_value_t* ref = list_shift(queue);

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
    
    append_children(queue, ref);
  }

  heap_free(heap);
  list_free(queue);

  clock_gettime(CLOCK_MONOTONIC_RAW, &gc_end);
  gc_time_ += (gc_end.tv_sec - gc_start.tv_sec) * 1000 + (gc_end.tv_nsec - gc_start.tv_nsec) / 1000000;

  return new_heap;
}

heap_t* heap_new(int64_t size) {
  heap_t* heap = malloc(sizeof(heap_t));
  heap->mem = malloc(size * sizeof(value_t));
  heap->mem_head = heap->mem;
  heap->mem_end = heap->mem + size;
  heap->size = size;
  return heap;
}

void heap_free(heap_t* heap) {
  free(heap->mem);
  free(heap);
}

void* allocate(heap_t** heap, value_t* stack, value_t* stack_head, int64_t size) {
  if ((*heap)->mem_end - (*heap)->mem_head < size) {
    *heap = collect_garbage(*heap, stack, stack_head);
  }

  value_t* target = (*heap)->mem_head;
  (*heap)->mem_head += size;
  for (int i = 0; i < size; i++) target[i] = 0;

  return target;
}

value_t copy_to_heap(value_t value, heap_t** heap, value_t* stack, value_t* stack_head) {
  boxed_value_t* ref = cast_to_boxed(value);
  if (ref == NULL) return value;

  int64_t size = boxed_value_size(ref);
  void* dst = allocate(heap, stack, stack_head, size);
  memcpy(dst, ref, size * sizeof(value_t));

  boxed_value_children_t children = boxed_value_children(ref);
  for (int i = 0; i < children.count; i++) {
    children.values[i] = copy_to_heap(children.values[i], heap, stack, stack_head);
  }

  return (value_t)(dst) | BOX_TAG;
}