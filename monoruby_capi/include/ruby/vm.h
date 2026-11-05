#ifndef RUBY_VM_H
#define RUBY_VM_H 1
/* CRuby's ruby/vm.h: the VM handle and the exit hook extensions use to
   free process-wide state (rbs's constant pool). */
#include "ruby/ruby.h"

#if defined(__cplusplus)
extern "C" {
#endif

typedef struct rb_vm_struct ruby_vm_t;

/* Runs `func` when the process exits, after the `at_exit` handlers (CRuby
   runs it when the VM is destructed). monoruby passes NULL for the VM. */
void ruby_vm_at_exit(void (*func)(ruby_vm_t *));

#if defined(__cplusplus)
}
#endif

#endif /* RUBY_VM_H */
