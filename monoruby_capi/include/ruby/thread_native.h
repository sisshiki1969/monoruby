/*
 * ruby/thread_native.h — CRuby's native-thread primitives, which are
 * pthreads on every platform monoruby builds for.
 */
#ifndef MONORUBY_CAPI_RUBY_THREAD_NATIVE_H
#define MONORUBY_CAPI_RUBY_THREAD_NATIVE_H 1
#include "ruby/ruby.h"
#include <pthread.h>
#include <time.h>
#include <errno.h>

typedef pthread_t rb_nativethread_id_t;
typedef pthread_mutex_t rb_nativethread_lock_t;
typedef pthread_cond_t rb_nativethread_cond_t;

static inline rb_nativethread_id_t rb_nativethread_self(void) { return pthread_self(); }
static inline void rb_nativethread_lock_initialize(rb_nativethread_lock_t *l) { pthread_mutex_init(l, NULL); }
static inline void rb_nativethread_lock_destroy(rb_nativethread_lock_t *l) { pthread_mutex_destroy(l); }
static inline void rb_nativethread_lock_lock(rb_nativethread_lock_t *l) { pthread_mutex_lock(l); }
static inline void rb_nativethread_lock_unlock(rb_nativethread_lock_t *l) { pthread_mutex_unlock(l); }
#define rb_native_mutex_initialize rb_nativethread_lock_initialize
#define rb_native_mutex_destroy rb_nativethread_lock_destroy
#define rb_native_mutex_lock rb_nativethread_lock_lock
#define rb_native_mutex_unlock rb_nativethread_lock_unlock
static inline int rb_native_mutex_trylock(rb_nativethread_lock_t *l) { return pthread_mutex_trylock(l); }
static inline void rb_native_cond_initialize(rb_nativethread_cond_t *c) { pthread_cond_init(c, NULL); }
static inline void rb_native_cond_destroy(rb_nativethread_cond_t *c) { pthread_cond_destroy(c); }
static inline void rb_native_cond_signal(rb_nativethread_cond_t *c) { pthread_cond_signal(c); }
static inline void rb_native_cond_broadcast(rb_nativethread_cond_t *c) { pthread_cond_broadcast(c); }
static inline void rb_native_cond_wait(rb_nativethread_cond_t *c, rb_nativethread_lock_t *l) { pthread_cond_wait(c, l); }
/* Waits at most `msec` milliseconds (CLOCK_REALTIME deadline). */
static inline void
rb_native_cond_timedwait(rb_nativethread_cond_t *c, rb_nativethread_lock_t *l, unsigned long msec)
{
    struct timespec ts;
    clock_gettime(CLOCK_REALTIME, &ts);
    ts.tv_sec += (time_t)(msec / 1000);
    ts.tv_nsec += (long)(msec % 1000) * 1000000L;
    if (ts.tv_nsec >= 1000000000L) {
        ts.tv_sec++;
        ts.tv_nsec -= 1000000000L;
    }
    pthread_cond_timedwait(c, l, &ts);
}
#endif
