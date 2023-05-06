# include "scopes.h"
# include "containers/list.h"
# include "value.h"

# include <stdio.h>
# include <stdint.h>

scopes_t* scopes_new() {
  scopes_t* scopes = malloc(sizeof(scopes_t));
  scopes->frames = list_new();
  scopes->values = list_new();
  return scopes;
}

void dump_scopes(scopes_t* scopes) {
  printf("\nscopes: ");
  node_t* values = scopes->values->beg;
  node_t* frames = scopes->frames->beg;
  while (frames) {
    int64_t x = (int64_t)(frames->value);
    printf("|");
    while (x--) {
      printf("["); print_value(*(value_t*)(values->value)); printf("] ");
      values = values->next;
    }
    frames = frames->next;
  }
  puts(" end of scope");
}

void scopes_free(scopes_t* scopes) {
  list_foreach(scopes->values, free);
  list_free(scopes->values);
  list_free(scopes->frames);
  free(scopes);
}

void scopes_enter(scopes_t* scopes) {
  list_prepend(scopes->frames, (void*)(int64_t)0);
}

void scopes_leave(scopes_t* scopes) {
  int64_t frame_size = (int64_t)list_shift(scopes->frames);
  while (frame_size--) free(list_shift(scopes->values));
}

value_t* scopes_declare(scopes_t* scopes, value_t value) {
  scopes->frames->beg->value = (void*)((int64_t)scopes->frames->beg->value + 1);

  value_t* value_ref = malloc(sizeof(value_t));
  *value_ref = value;
  list_prepend(scopes->values, value_ref);

  return value_ref;
}
