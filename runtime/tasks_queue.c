# include "tasks_queue.h"

# include <stdbool.h>

# include "process.h"

typedef struct tasks_queue_node_t {
  process_t*                 head;
  struct tasks_queue_node_t* tail;
} tasks_queue_node_t;

typedef struct tasks_queue_t {
  tasks_queue_node_t* beg;
  tasks_queue_node_t* end;
} tasks_queue_t;

extern tasks_queue_t* tasks_queue_new() {
  tasks_queue_t* queue = malloc(sizeof(tasks_queue_t));
  queue->beg = NULL;
  queue->end = NULL;
  return queue;
}

extern void tasks_queue_free(tasks_queue_t* queue) {
  tasks_queue_node_t* node = queue->beg;
  while (node) {
    tasks_queue_node_t* next_node = node->tail;
    free(node);
    node = next_node;
  }
  free(queue);
}

extern bool tasks_queue_is_empty(tasks_queue_t* queue) {
  return queue->beg == NULL;
}

extern void tasks_queue_enqueue(tasks_queue_t* queue, process_t* task) {
  tasks_queue_node_t* node = malloc(sizeof(tasks_queue_node_t));
  node->head = task;
  node->tail = NULL;

  if (tasks_queue_is_empty(queue)) {
    queue->beg = node;
    queue->end = node;
  } else {
    queue->end->tail = node;
    queue->end = node;
  }
}

extern process_t* tasks_queue_dequeue(tasks_queue_t* queue) {
  if (tasks_queue_is_empty(queue)) return NULL;

  tasks_queue_node_t* node = queue->beg;
  process_t* process = node->head;

  queue->beg = node->tail;
  if (node->tail == NULL)
    queue->end = NULL;

  free(node);
  return process;
}