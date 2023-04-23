# pragma once

# include <stdbool.h>
# include <stdint.h>

typedef struct node_t {
  struct node_t* prev;
  void*  value;
  struct node_t* next;
} node_t;

typedef struct list_t {
  node_t* beg;
  node_t* end;
} list_t;

typedef bool(*eq_fun_t)(void*, void*);

list_t* list_new();
void    list_free(list_t*);
bool    list_null(list_t*);
void    list_append(list_t*, void*);
void    list_prepend(list_t*, void*);
void*   list_shift(list_t*);
list_t* list_reverse(list_t*);
void    list_remove(list_t*, void*);
void    list_remove_by(list_t*, void*, eq_fun_t);
int64_t list_size(list_t*);

typedef void(*list_foreach_t)(void*);
void list_foreach(list_t*, list_foreach_t);

typedef void*(*list_map_t)(void*);
void list_map(list_t*, list_map_t);
