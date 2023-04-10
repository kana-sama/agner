# pragma once

# include <stdbool.h>
# include <stdint.h>

typedef struct node_t {
  void* head;
  struct node_t* tail;
} node_t;

typedef struct list_t {
  node_t* beg;
  node_t* end;
} list_t;

list_t* list_new();
void    list_free(list_t*);
bool    list_null(list_t*);
void    list_append(list_t*, void*);
void    list_prepend(list_t*, void*);
void*   list_shift(list_t*);
void    list_remove(list_t*, void*);
int64_t list_size(list_t*);
void    list_foreach(list_t*, void(*)(void*));
