# pragma once

# include <stdint.h>

# include "containers/list.h"
# include "value.h"

typedef uint64_t ms_time_t;

typedef struct mailbox_t {
  ms_time_t timeout_at; // when to after, 0 - no timeout
  list_t* messages;
  list_t* picked;
} mailbox_t;

mailbox_t* mailbox_new();
void mailbox_free(mailbox_t*);
void mailbox_push(mailbox_t*, value_t);

value_t* mailbox_pick(mailbox_t*);
void mailbox_drop_picked(mailbox_t*);
void mailbox_unpick(mailbox_t*);

void mailbox_set_timeout(mailbox_t*, ms_time_t delta_ms);
void mailbox_remove_timeout(mailbox_t*);
bool mailbox_is_timed_out(mailbox_t*);
