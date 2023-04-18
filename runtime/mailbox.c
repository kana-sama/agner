# include "mailbox.h"

# include "list.h"
# include "value.h"

mailbox_t* mailbox_new() {
  mailbox_t* mailbox = malloc(sizeof(mailbox_t));
  mailbox->to_match    = list_new();
  mailbox->not_matched = list_new();
  mailbox->picked      = NULL;
  return mailbox;
}

void mailbox_free(mailbox_t* mailbox) {
  list_foreach(mailbox->to_match, free);
  list_free(mailbox->to_match);

  list_foreach(mailbox->not_matched, free);
  list_free(mailbox->not_matched);

  free(mailbox);
}

void mailbox_push(mailbox_t* mailbox, value_t value) {
  value_t* message = malloc(sizeof(value_t));
  *message = value;

  list_append(mailbox->to_match, message);
}


value_t* mailbox_pick(mailbox_t* mailbox) {
  if (mailbox->picked) {
    list_prepend(mailbox->not_matched, mailbox->picked);
  }

  value_t* message = list_shift(mailbox->to_match);
  mailbox->picked = message;
  return message;
}

void mailbox_drop_picked(mailbox_t* mailbox) {
  mailbox->picked = NULL;
}

void mailbox_unpick(mailbox_t* mailbox) {
  if (mailbox->picked) {
    list_prepend(mailbox->to_match, mailbox->picked);
    mailbox->picked = NULL;
  }

  while (!list_null(mailbox->not_matched)) {
    list_prepend(mailbox->to_match, list_shift(mailbox->not_matched));
  }
}
