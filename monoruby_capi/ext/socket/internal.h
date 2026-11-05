/*
 * internal.h and internal/*.h — ext/socket is built with CRuby's private
 * headers. These stand-ins declare the few internal functions it uses;
 * the C API layer defines them (capi.c, "ext/socket").
 */
#ifndef MONORUBY_CAPI_SOCKET_INTERNAL_H
#define MONORUBY_CAPI_SOCKET_INTERNAL_H 1
#include "ruby/ruby.h"
#include "ruby/io.h"
#include "ruby/thread.h"

/* internal/compilers.h: a flexible array member's declared length. */
#ifndef FLEX_ARY_LEN
#define FLEX_ARY_LEN /* VLA */
#endif

/* internal.h */
#ifndef numberof
#define numberof(array) ((int)(sizeof(array) / sizeof((array)[0])))
#endif

/* internal/gc.h: collect when out of descriptors or memory (EMFILE,
 * ENFILE, ENOMEM); nonzero if it did. */
int rb_gc_for_fd(int err);

/* internal/error.h */
NORETURN(void rb_syserr_fail_path(int err, VALUE path));

/* internal/string.h: a frozen view of `str` for a blocking call. */
VALUE rb_str_tmp_frozen_acquire(VALUE str);
void rb_str_tmp_frozen_release(VALUE orig, VALUE tmp);

/* internal/io.h, internal/thread.h */
int rb_reserved_fd_p(int fd);
void rb_maygvl_fd_fix_cloexec(int fd);
#endif
