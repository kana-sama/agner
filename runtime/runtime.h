# include <stdlib.h>
# include <stdint.h>

# include "value.h"
# include "scheduler.h"

scheduler_t* scheduler;

void     enter_scope();
void     leave_scope();
value_t* add_to_scope(value_t);
void*    allocate(int64_t);

void _runtime__start(value_t entry);
void _runtime__yield(char*);
void _runtime__save_vstack();

value_t _alloc__integer(int64_t);
value_t _alloc__tuple(int64_t size, value_t* values);
value_t _alloc__cons(value_t head, value_t tail);

value_t _receive__pick();
void    _receive__success();

value_t _record__get(value_t, int64_t field_ix);
value_t _record__set(value_t, int64_t field_ix, value_t field_value);

