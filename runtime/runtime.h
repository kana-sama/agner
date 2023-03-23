# include <stdlib.h>

# include "value.h"

void _runtime__init(value_t entry);
void _runtime__finalize();
void _runtime__yield();

value_t _runtime__alloc_tuple(int64_t size);
void _runtime__fill_tuple(value_t value, int64_t size, value_t* values);
value_t* _runtime__match_tuple(value_t, int64_t size);

value_t _runtime__alloc_cons();
void _runtime__fill_cons(value_t cons, value_t head, value_t tail);
value_t* _runtime__match_cons(value_t);

value_t _agner__print(value_t);
value_t _timer__sleep(value_t);
value_t _global__error(value_t);
value_t _global__spawn(value_t);
value_t _global__self();