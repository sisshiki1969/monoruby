/*
 * ruby/fiber/scheduler.h — the part of CRuby's fiber scheduler interface
 * the layer provides: the current scheduler and the hooks an extension
 * calls on it, each the scheduler's Ruby method.
 */
#ifndef MONORUBY_CAPI_RUBY_FIBER_SCHEDULER_H
#define MONORUBY_CAPI_RUBY_FIBER_SCHEDULER_H 1
#include "ruby/ruby.h"

#ifdef __cplusplus
extern "C" {
#endif
/* `Fiber.scheduler` when the current fiber is non-blocking, else nil. */
VALUE rb_fiber_scheduler_current(void);
VALUE rb_fiber_scheduler_get(void);
VALUE rb_fiber_scheduler_address_resolve(VALUE scheduler, VALUE hostname);
VALUE rb_fiber_scheduler_io_wait(VALUE scheduler, VALUE io, VALUE events, VALUE timeout);
VALUE rb_fiber_scheduler_kernel_sleep(VALUE scheduler, VALUE duration);
#ifdef __cplusplus
}
#endif
#endif
