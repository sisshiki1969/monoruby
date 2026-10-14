/*
 * ruby/thread.h — releasing the interpreter around a blocking call.
 *
 * rb_thread_call_without_gvl runs `func(data1)` on a worker thread while
 * the calling Ruby thread waits (monoruby_ext.h's call_blocking), so
 * `func` must not touch Ruby. The unblocking function is not called: an
 * interrupt ends the wait and is delivered once the call returns.
 */
#ifndef MONORUBY_CAPI_RUBY_THREAD_H
#define MONORUBY_CAPI_RUBY_THREAD_H 1

#include "ruby/ruby.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef void rb_unblock_function_t(void *);
#define RUBY_UBF_IO ((rb_unblock_function_t *)-1)
#define RUBY_UBF_PROCESS ((rb_unblock_function_t *)-1)

void *rb_thread_call_without_gvl(void *(*func)(void *), void *data1,
                                 rb_unblock_function_t *ubf, void *data2);
void *rb_thread_call_without_gvl2(void *(*func)(void *), void *data1,
                                  rb_unblock_function_t *ubf, void *data2);
void *rb_nogvl(void *(*func)(void *), void *data1, rb_unblock_function_t *ubf,
               void *data2, int flags);
#define RB_NOGVL_INTR_FAIL 0x1
#define RB_NOGVL_UBF_ASYNC_SAFE 0x2

#ifdef __cplusplus
}
#endif

#endif /* MONORUBY_CAPI_RUBY_THREAD_H */
