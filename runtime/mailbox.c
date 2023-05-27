# include "mailbox.h"

# include "time.h"

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

static ms_time_t now() {
  struct timespec time;
  clock_gettime(CLOCK_MONOTONIC_RAW, &time);
  return time.tv_sec * 1000 + time.tv_nsec / 1000000;
}

void mailbox_set_timeout(mailbox_t* mailbox, ms_time_t delta) {
  mailbox->timeout_at = now() + delta;
}

void mailbox_remove_timeout(mailbox_t* mailbox) {
  mailbox->timeout_at = 0;
}

bool mailbox_is_timed_out(mailbox_t* mailbox) {
  if (mailbox->timeout_at == 0) return false;
  return mailbox->timeout_at < now();
}
