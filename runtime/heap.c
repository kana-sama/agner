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
    boxed_value_t* child = cast_to_boxed_value(children.values[i]); if (!child) continue;
    list_append(queue, child);
  }
}

static void mark_value(list_t* queue, int64_t* offset, boxed_value_t* ref) {
  if (ref && ref->super.gc_offset == 0) {
    ref->super.gc_offset = *offset;
    *offset += boxed_value_size(ref);
    append_children(queue, ref);
  }
}

static void update_value(list_t* queue, heap_t* new_heap, value_t* value) {
  boxed_value_t* ref = cast_to_boxed_value(*value);
  if (!ref) return;
  *value = (value_t)(new_heap->mem + ref->super.gc_offset) | BOX_TAG;
  list_append(queue, ref);
}

static boxed_value_t* move_ref(heap_t* new_heap, boxed_value_t* ref) {
  void* dst = (void*)(new_heap->mem + ref->super.gc_offset);
  void* src = (void*)ref;
  size_t count = boxed_value_size(ref) * sizeof(value_t);
  memcpy(dst, src, count);

  boxed_value_t* new_ref = (boxed_value_t*)dst;
  new_ref->super.gc_offset = 0;
  
  return new_ref;
}

// mark and copy algorithm
heap_t* heap_gc(
  heap_t* heap,
  gc_ctx_t ctx
) {
  struct timespec gc_start, gc_end;
  clock_gettime(CLOCK_MONOTONIC_RAW, &gc_start);
  
  int64_t offset = 0;
  list_t* queue = list_new();

  for (value_t* i = ctx.vstack; i < ctx.vstack_head; i++)
    mark_value(queue, &offset, cast_to_boxed_value(*i));
  for (node_t* i = ctx.scopes->values->beg; i; i = i->next)
    mark_value(queue, &offset, cast_to_boxed_value(*(value_t*)i->value));
  for (node_t* i = ctx.mailbox->messages->beg; i; i = i->next)
    mark_value(queue, &offset, cast_to_boxed_value(*(value_t*)i->value));
  for (node_t* i = ctx.mailbox->picked->beg; i; i = i->next)
    mark_value(queue, &offset, cast_to_boxed_value(*(value_t*)i->value));
  while (!list_null(queue))
    mark_value(queue, &offset, list_shift(queue));

  heap_t* new_heap = heap_new(offset * 2);
  new_heap->mem_head = new_heap->mem + offset;

  for (value_t* i = ctx.vstack; i < ctx.vstack_head; i++)
    update_value(queue, new_heap, i);
  for (node_t* i = ctx.scopes->values->beg; i; i = i->next)
    update_value(queue, new_heap, (value_t*)i->value);
  for (node_t* i = ctx.mailbox->messages->beg; i; i = i->next)
    update_value(queue, new_heap, (value_t*)i->value);
  for (node_t* i = ctx.mailbox->picked->beg; i; i = i->next)
    update_value(queue, new_heap, (value_t*)i->value);
  while (!list_null(queue)) {
    boxed_value_t* new_ref = move_ref(new_heap, list_shift(queue));
    boxed_value_children_t children = boxed_value_children(new_ref);
    for (int i = 0; i < children.count; i++)
      update_value(queue, new_heap, &children.values[i]);
  }

  // на всякий случай, чтобы получить ошибку если кто-то будет ссылаться на старый хип
  memset(heap->mem, 0, sizeof(value_t) * heap->size);
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

void* heap_allocate(heap_t** heap, int64_t size, gc_ctx_t ctx) {
  if ((*heap)->mem_end - (*heap)->mem_head < size)
    *heap = heap_gc(*heap, ctx);

  value_t* target = (*heap)->mem_head;
  (*heap)->mem_head += size;
  memset(target, 0, size * sizeof(value_t));
  
  return target;
}

value_t copy_to_heap(value_t value, heap_t** heap, gc_ctx_t ctx) {
  boxed_value_t* ref = cast_to_boxed_value(value);
  if (ref == NULL) return value;

  int64_t size = boxed_value_size(ref);
  void* dst = heap_allocate(heap, size, ctx);
  memcpy(dst, ref, size * sizeof(value_t));

  boxed_value_children_t children = boxed_value_children(ref);
  for (int i = 0; i < children.count; i++) {
    children.values[i] = copy_to_heap(children.values[i], heap, ctx);
  }

  return (value_t)(dst) | BOX_TAG;
}
