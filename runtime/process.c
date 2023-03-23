# include "process.h"

# include <stdlib.h>
# include <setjmp.h>

# include "heap.h"
# include "value.h"

# define HEAP_SIZE   (1024 * 1024)
# define VSTACK_SIZE (64 * 1024 * 1024)
# define STACK_SIZE  (64 * 1024 * 1024)

static int64_t fresh_pid = 0;

process_t* process_new() {
  process_t* process   = malloc(sizeof(process_t));
  process->pid         = fresh_pid++;
  process->heap        = heap_new(HEAP_SIZE);
  process->stack       = aligned_alloc(16, STACK_SIZE);
  process->stack_beg   = process->stack + STACK_SIZE - 8;
  process->vstack      = calloc(VSTACK_SIZE, sizeof(value_t));
  process->context     = malloc(sizeof(jmp_buf));
  return process;
}

void process_free(process_t* process) {
  heap_free(process->heap);
  free(process->stack);
  free(process->vstack);
  free(process->context);
  free(process);
}
