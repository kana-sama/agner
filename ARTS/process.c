# include "process.h"

# include <stdlib.h>
# include <setjmp.h>

# include "options.h"
# include "heap.h"
# include "value.h"
# include "list.h"
# include "mailbox.h"

# define MB (1024 * 1024)

static int64_t fresh_pid = 0;

list_t* registry;

static void reg(process_t* process) {
  if (registry == NULL) registry = list_new();
  list_append(registry, process);
}

static void unreg(process_t* process) {
  if (registry == NULL) registry = list_new();
  list_remove(registry, process);
}

process_t* process_new() {
  process_t* process = malloc(sizeof(process_t));
  process->pid         = fresh_pid++;
  process->heap        = heap_new(options.initial_heap * MB / sizeof(value_t));
  process->stack       = aligned_alloc(16, options.initial_nstack * MB);
  process->stack_beg   = process->stack + options.initial_nstack * MB - 8;
  process->vstack      = calloc(options.initial_vstack * MB, sizeof(value_t));
  process->vstack_head = process->vstack;
  process->mailbox     = mailbox_new();
  process->context     = malloc(sizeof(jmp_buf));
  process->is_alive    = true;

  reg(process);

  return process;
}

void process_free(process_t* process) {
  unreg(process);

  heap_free(process->heap);
  mailbox_free(process->mailbox);
  free(process->stack);
  free(process->vstack);
  free(process->context);
  free(process);
}

process_t* process_lookup(PID_t pid) {
  node_t* node = registry->beg;
  while (node) {
    process_t* process = node->head;
    if (process->pid == pid) return process;
    node = node->tail;
  }
  return NULL;
}

void process_send(PID_t pid, value_t message) {
  process_t* process = process_lookup(pid);
  if (process == NULL || !process->is_alive) return;
  value_t message_copy = copy_to_heap(message, &process->heap, process->vstack, process->vstack_head);
  mailbox_push(process->mailbox, message_copy);
}

void process_save_vstack_head(process_t* process) {
  asm("movq %%r12, %0" : "=rm"(process->vstack_head));
}