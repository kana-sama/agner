# include "mailbox.h"

# include "containers/list.h"
# include "value.h"

mailbox_t* mailbox_new() {
  mailbox_t* mailbox = malloc(sizeof(mailbox_t));
  mailbox->messages = list_new();
  mailbox->picked   = list_new();
  return mailbox;
}

void mailbox_free(mailbox_t* mailbox) {
  list_foreach(mailbox->messages, free);
  list_free(mailbox->messages);

  list_foreach(mailbox->picked, free);
  list_free(mailbox->picked);
}

void mailbox_push(mailbox_t* mailbox, value_t value) {
  value_t* message = malloc(sizeof(value_t));
  *message = value;

  list_append(mailbox->messages, message);
}


value_t* mailbox_pick(mailbox_t* mailbox) {
  value_t* message = list_shift(mailbox->messages);
  if (message) list_prepend(mailbox->picked, message);
  return message;
}

void mailbox_drop_picked(mailbox_t* mailbox) {
  list_shift(mailbox->picked);
}

void mailbox_unpick(mailbox_t* mailbox) {
  while (!list_null(mailbox->picked))
    list_prepend(mailbox->messages, list_shift(mailbox->picked));
}
