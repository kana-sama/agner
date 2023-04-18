# include "list.h"

# include <stdlib.h>
# include <stdint.h>

list_t* list_new() {
  list_t* list = malloc(sizeof(list_t));
  list->beg = NULL;
  list->end = NULL;
  return list;
}

void list_free(list_t* list) {
  node_t* node = list->beg;
  while (node) {
    node_t* tail = node->tail;
    free(node);
    node = tail;
  }

  free(list);
}

bool list_null(list_t* list) {
  return list->beg == NULL;
}

void list_append(list_t* list, void* value) {
  node_t* node = malloc(sizeof(node_t));
  node->head = value;
  node->tail = NULL;

  if (list_null(list)) {
    list->beg = node;
  } else {
    list->end->tail = node;
  }

  list->end = node;
}

void list_prepend(list_t* list, void* value) {
  node_t* node = malloc(sizeof(node_t));
  node->head = value;
  node->tail = list->beg;
  list->beg = node;

  if (list_null(list)) { 
    list->end = node;
  }
}

void* list_shift(list_t* list) {
  if (list_null(list)) return NULL;

  node_t* node = list->beg;
  void* value = node->head;

  list->beg = node->tail;
  if (node->tail == NULL)
    list->end = NULL;

  free(node);
  return value;
}

void list_remove(list_t* list, void* value) {
  if (list_null(list)) return;
  if (list->beg->head == value) {
    list_shift(list);
    return;
  }
  
  node_t* node = list->beg;
  while (node->tail) {
    if (node->tail->head == value) {
      node_t* to_remove = node->tail;

      if (node->tail == list->end) {
        list->end = node;
        node->tail = NULL;
      } else {
        node->tail = node->tail->tail;
      }

      free(to_remove);

      return;
    }

    node = node->tail;
  }
}

int64_t list_size(list_t* list) {
  node_t* node = list->beg;
  int64_t size = 0;

  while (node) {
    size += 1;
    node = node->tail;
  }

  return size;
}

void list_foreach(list_t* list, void(*action)(void*)) {
  node_t* node = list->beg;

  while (node) {
    action(node->head);
    node = node->tail;
  }
};
