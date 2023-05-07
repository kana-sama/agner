# include <stdlib.h>
# include <stdint.h>

# include "value.h"
# include "scheduler.h"

extern scheduler_t* scheduler;

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
value_t _alloc__closure(value_t body, int64_t env_size, value_t* env);

value_t _receive__pick();
void    _receive__success();

value_t* _closure__get_env(value_t);
value_t  _closure__get_fun(value_t);

void _runtime__catch(handler_action_t);
