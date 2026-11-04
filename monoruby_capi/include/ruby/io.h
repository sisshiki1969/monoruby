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

/* The open-mode bits of io.c, with CRuby's values (an extension may keep
 * them in its own structs, as stringio does). */
#define FMODE_READABLE              0x00000001
#define FMODE_WRITABLE              0x00000002
#define FMODE_READWRITE             (FMODE_READABLE|FMODE_WRITABLE)
#define FMODE_BINMODE               0x00000004
#define FMODE_SYNC                  0x00000008
#define FMODE_TTY                   0x00000010
#define FMODE_DUPLEX                0x00000020
#define FMODE_APPEND                0x00000040
#define FMODE_CREATE                0x00000080
#define FMODE_EXCL                  0x00000400
#define FMODE_TRUNC                 0x00000800
#define FMODE_TEXTMODE              0x00001000
#define FMODE_EXTERNAL              0x00010000
#define FMODE_SETENC_BY_BOM         0x00100000

/* `rb_io_extract_modeenc`'s encoding result (CRuby's `struct
 * rb_io_encoding`, also spelled `rb_io_enc_t`). */
struct rb_io_encoding {
    rb_encoding *enc;
    rb_encoding *enc2;
    int ecflags;
    VALUE ecopts;
};
typedef struct rb_io_encoding rb_io_enc_t;

int rb_io_extract_encoding_option(VALUE opt, rb_encoding **enc_p, rb_encoding **enc2_p, int *fmode_p);
void rb_io_extract_modeenc(VALUE *vmode_p, VALUE *vperm_p, VALUE opthash,
                           int *oflags_p, int *fmode_p, struct rb_io_encoding *convconfig_p);
int rb_io_modestr_fmode(const char *modestr);
int rb_io_modestr_oflags(const char *modestr);
int rb_io_oflags_fmode(int oflags);

#ifdef __cplusplus
}
#endif

#endif /* MONORUBY_CAPI_RUBY_IO_H */
