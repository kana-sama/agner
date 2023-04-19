# pragma once

# include "list.h"
# include "value.h"

typedef struct mailbox_t {
  list_t* messages;
  list_t* picked;
} mailbox_t;

mailbox_t* mailbox_new();
void mailbox_free(mailbox_t*);
void mailbox_push(mailbox_t*, value_t);

value_t* mailbox_pick(mailbox_t*);
void mailbox_drop_picked(mailbox_t*);
void mailbox_unpick(mailbox_t*);
