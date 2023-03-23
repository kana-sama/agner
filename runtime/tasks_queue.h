# pragma once

# include <stdbool.h>

# include "process.h"

typedef struct tasks_queue_t tasks_queue_t;

tasks_queue_t* tasks_queue_new();
void tasks_queue_free(tasks_queue_t*);
bool tasks_queue_is_empty(tasks_queue_t*);
void tasks_queue_enqueue(tasks_queue_t*, process_t*);
process_t* tasks_queue_dequeue(tasks_queue_t*);