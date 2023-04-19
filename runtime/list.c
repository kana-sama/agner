# include "list.h"

# include <stdlib.h>
# include <stdint.h>
# include <stdio.h>

list_t* list_new() {
  list_t* list = malloc(sizeof(list_t));
  list->beg = NULL;
  list->end = NULL;
  return list;
}

void list_free(list_t* list) {
  node_t* node = list->beg;
  while (node) {
    node_t* next = node->next;
    free(node);
    node = next;
  }

  free(list);
}

bool list_null(list_t* list) {
  return list->beg == NULL;
}

void list_append(list_t* list, void* value) {
  node_t* node = malloc(sizeof(node_t));
  node->prev = list->end;
  node->value = value;
  node->next = NULL;

  if (list_null(list)) {
    list->beg = node;
  } else {
    list->end->next = node;
  }

  list->end = node;
}

void list_prepend(list_t* list, void* value) {
  node_t* node = malloc(sizeof(node_t));
  node->prev = NULL;
  node->value = value;
  node->next = list->beg;

  if (list_null(list)) { 
    list->beg = node;
    list->end = node;
  } else {
    list->beg->prev = node;
    list->beg = node;
  }
}

void* list_shift(list_t* list) {
  if (list_null(list)) return NULL;

  node_t* node = list->beg;
  void* value = node->value;

  list->beg = node->next;
  if (node->next == NULL)
    list->end = NULL;
  else
    list->beg->prev = NULL;

  free(node);
  return value;
}

list_t* list_reverse(list_t* list) {
  list_t* result = list_new();

  while (!list_null(list)) {
    list_prepend(result, list_shift(list));
  }

  free(list);
  return result;
}

void list_remove_by(list_t* list, void* value, eq_fun_t eq) {
  if (list_null(list)) return;
  if (eq(list->beg->value, value)) {
    list_shift(list);
    return;
  }
  
  node_t* node = list->beg;
  while (node->next) {
    if (eq(node->next->value, value)) {
      node_t* to_remove = node->next;

      if (node->next == list->end) {
        list->end = node;
        node->next = NULL;
      } else {
        node->next = node->next->next;
      }

      free(to_remove);
      return;
    }

    if (node->next == list->end && eq(node->next->value, value)) {
      free(node->next);
      node->next = NULL;
      list->end = node;
    } else {
      node = node->next;
    }
  }
}

static bool equality(void* a, void* b) { return a == b; }
void list_remove(list_t* list, void* value) {
  list_remove_by(list, value, equality);
}

int64_t list_size(list_t* list) {
  node_t* node = list->beg;
  int64_t size = 0;

  while (node) {
    size += 1;
    node = node->next;
  }

  return size;
}

void list_foreach(list_t* list, list_foreach_t action) {
  for (node_t* i = list->beg; i; i = i->next)
    action(i->value);
}

void list_map(list_t* list, list_map_t action) {
  for (node_t* i = list->beg; i; i = i->next)
    i->value = action(i->value);
}
