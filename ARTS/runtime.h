# include <stdlib.h>
# include <stdint.h>

# include "value.h"
# include "scheduler.h"

scheduler_t* scheduler;
value_t _runtime__calling_context[10];

void _runtime__start(value_t entry);
void _runtime__yield(char*);
void _runtime__save_vstack(value_t*);

value_t  _runtime__alloc_tuple(int64_t size);
void     _runtime__fill_tuple(value_t value, int64_t size, value_t* values);
value_t* _runtime__match_tuple(value_t, int64_t size);

value_t  _runtime__alloc_cons();
void     _runtime__fill_cons(value_t cons, value_t head, value_t tail);
value_t* _runtime__match_cons(value_t);

void    _runtime__receive_pick();
value_t _runtime__receive_picked();
void    _runtime__receive_success();
