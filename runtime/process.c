# include "process.h"

# include <stdlib.h>
# include <setjmp.h>

# include "options.h"
# include "heap.h"
# include "value.h"
# include "containers/list.h"
# include "mailbox.h"
# include "scopes.h"
# include "macros.h"

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
  process->scopes      = scopes_new();
  process->context     = malloc(sizeof(jmp_buf));
  process->handlers    = list_new();
  process->is_alive    = true;

  reg(process);

  return process;
}

void process_free(process_t* process) {
  unreg(process);

  heap_free(process->heap);
  mailbox_free(process->mailbox);
  scopes_free(process->scopes);
  free(process->stack);
  free(process->vstack);
  free(process->context);
  list_free(process->handlers);
  free(process);
}

process_t* process_lookup(PID_t pid) {
  node_t* node = registry->beg;
  while (node) {
    process_t* process = node->value;
    if (process->pid == pid) return process;
    node = node->next;
  }
  return NULL;
}

gc_ctx_t process_gc_ctx(process_t* process) {
  return (gc_ctx_t){
    .vstack = process->vstack,
    .vstack_head = process->vstack_head,
    .scopes = process->scopes,
    .mailbox = process->mailbox,
  };
}

void process_send(PID_t pid, value_t message) {
  process_t* process = process_lookup(pid);
  if (process == NULL || !process->is_alive) return;
  value_t message_copy = copy_to_heap(message, &process->heap, process_gc_ctx(process));
  mailbox_push(process->mailbox, message_copy);
}

typedef struct handler_t {
  handler_action_t handler_action;
  value_t*         vstack_head;
  void*            stack_head;
} handler_t;

void process_add_handler(process_t* process, handler_action_t handler_action, void* stack_head) {
  handler_t* handler = malloc(sizeof(handler_t));
  handler->handler_action = handler_action;
  handler->vstack_head = process->vstack_head;
  handler->stack_head  = stack_head;
  list_prepend(process->handlers, handler);
}

void process_remove_handler(process_t* process) {
  list_shift(process->handlers);
}

void process_throw_wrapper(value_t exception, handler_action_t handler_action, value_t* vstack_head, void* stack_head); asm(
  ASM_FUN(process_throw_wrapper)
    "movq %rdx, %r12  \n"
    "movq %rcx, %rsp  \n"
    // rdi is exception
    "jmpq *%rsi       \n"
);

void process_throw(process_t* process, value_t value) {
  if (list_null(process->handlers)) {
    printf("** exception error: ");
    print_value(value);
    printf("\n");
    exit(-1);
  }

  handler_t* handler_ = list_shift(process->handlers);
  handler_t handler = *handler_;
  free(handler_);

  return process_throw_wrapper(value, handler.handler_action, handler.vstack_head, handler.stack_head);
}
