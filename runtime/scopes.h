# pragma once

# include "value.h"
# include "list.h"

typedef struct scopes_t {
  list_t* frames;
  list_t* values;
} scopes_t;

scopes_t* scopes_new();
void      scopes_free(scopes_t*);
void      scopes_enter(scopes_t*);
void      scopes_leave(scopes_t*);
value_t*  scopes_declare(scopes_t*, value_t);
