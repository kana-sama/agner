# include <stdlib.h>
# include <stdint.h>

# include "value.h"
# include "scheduler.h"

scheduler_t* scheduler;

void _runtime__start(value_t entry);
void _runtime__yield(char*);
void _runtime__save_vstack();

value_t _alloc__integer(int64_t);
value_t _alloc__tuple(int64_t size);
value_t _alloc__cons();

void _fill__tuple(value_t value, int64_t size, value_t* values);
void _fill__cons(value_t cons, value_t head, value_t tail);

value_t _receive__pick();
void    _receive__success();

