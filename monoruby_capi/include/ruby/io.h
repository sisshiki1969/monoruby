/*
 * ruby/io.h — the IO functions of monoruby's C API layer.
 *
 * monoruby's IO has no rb_io_t a C extension could reach into; the
 * functions here (declared in ruby/ruby.h) go through IO's methods.
 */
#ifndef MONORUBY_CAPI_RUBY_IO_H
#define MONORUBY_CAPI_RUBY_IO_H 1

#include "ruby/ruby.h"
#include "ruby/encoding.h"

#ifdef __cplusplus
extern "C" {
#endif

int rb_io_extract_encoding_option(VALUE opt, rb_encoding **enc_p, rb_encoding **enc2_p, int *fmode_p);

#ifdef __cplusplus
}
#endif

#endif /* MONORUBY_CAPI_RUBY_IO_H */
