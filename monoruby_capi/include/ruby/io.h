/*
 * ruby/io.h — the IO functions of monoruby's C API layer.
 *
 * monoruby's IO has no rb_io_t of its own. `GetOpenFile` / `MakeOpenFile`
 * hand an extension a `struct rb_io` the layer owns for the length of the
 * Ruby call: filled from the IO object (descriptor, mode bits, buffered
 * input) and written back to it when the call returns or the layer next
 * hands the object to the interpreter (capi.c, "rb_io_t"). The other
 * functions here go through IO's methods.
 */
#ifndef MONORUBY_CAPI_RUBY_IO_H
#define MONORUBY_CAPI_RUBY_IO_H 1

#include "ruby/ruby.h"
#include "ruby/encoding.h"
#include <stdio.h>
#include <sys/types.h>

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

#define RB_WAITFD_IN  0x001
#define RB_WAITFD_PRI 0x002
#define RB_WAITFD_OUT 0x004

enum rb_io_event {
    RUBY_IO_READABLE = RB_WAITFD_IN,
    RUBY_IO_WRITABLE = RB_WAITFD_OUT,
    RUBY_IO_PRIORITY = RB_WAITFD_PRI
};
typedef enum rb_io_event rb_io_event_t;

/* The default timeout argument of `rb_io_wait`: the IO's own. */
#define RUBY_IO_TIMEOUT_DEFAULT Qnil

typedef struct rb_io_internal_buffer {
    char *ptr;
    int off;
    int len;
    int capa;
} rb_io_buffer_t;

/* CRuby's `struct rb_io`, field for field. An extension reads and writes
 * `fd`, `mode`, `rbuf` (and `self`, `pathv`); the rest is zero. */
#ifndef HAVE_RB_IO_T
#define HAVE_RB_IO_T 1
struct rb_io {
    VALUE self;
    FILE *stdio_file;
    int fd;
    int mode;
    rb_pid_t pid;
    int lineno;
    VALUE pathv;
    void (*finalize)(struct rb_io *, int);
    rb_io_buffer_t wbuf, rbuf;
    VALUE tied_io_for_writing;
    struct rb_io_encoding encs;
    rb_econv_t *readconv;
    rb_io_buffer_t cbuf;
    rb_econv_t *writeconv;
    VALUE writeconv_asciicompat;
    int writeconv_initialized;
    int writeconv_pre_ecflags;
    VALUE writeconv_pre_ecopts;
    VALUE write_lock;
    VALUE timeout;
};
#endif
typedef struct rb_io rb_io_t;

rb_io_t *rb_io_get_fptr(VALUE io);
/* `RFILE(io)->fptr`: the same `struct rb_io` GetOpenFile hands out. */
struct RFile {
    struct RBasic basic;
    struct rb_io *fptr;
};
struct RFile *rb_monoruby_rfile(VALUE io);
#define RFILE(obj) rb_monoruby_rfile((VALUE)(obj))
rb_io_t *rb_io_make_open_file(VALUE obj);
#define RB_IO_POINTER(obj, fp) rb_io_check_closed((fp) = rb_io_get_fptr(rb_io_taint_check(obj)))
#define GetOpenFile RB_IO_POINTER
#define RB_IO_OPEN(obj, fp) ((fp) = rb_io_make_open_file(obj))
#define MakeOpenFile RB_IO_OPEN

void rb_io_check_closed(rb_io_t *fptr);
void rb_io_check_initialized(rb_io_t *fptr);
void rb_io_check_readable(rb_io_t *fptr);
void rb_io_check_char_readable(rb_io_t *fptr);
void rb_io_check_byte_readable(rb_io_t *fptr);
void rb_io_check_writable(rb_io_t *fptr);
void rb_io_set_nonblock(rb_io_t *fptr);
void rb_io_synchronized(rb_io_t *fptr);
int rb_io_read_pending(rb_io_t *fptr);
void rb_io_read_check(rb_io_t *fptr);
FILE *rb_io_stdio_file(rb_io_t *fptr);
int rb_io_descriptor(VALUE io);
int rb_io_mode(VALUE io);
VALUE rb_io_path(VALUE io);
VALUE rb_io_get_write_io(VALUE io);
VALUE rb_io_set_write_io(VALUE io, VALUE w);
VALUE rb_io_timeout(VALUE io);
VALUE rb_io_set_timeout(VALUE io, VALUE timeout);
VALUE rb_io_wait(VALUE io, VALUE events, VALUE timeout);
VALUE rb_io_maybe_wait(int error, VALUE io, VALUE events, VALUE timeout);
int rb_io_maybe_wait_readable(int error, VALUE io, VALUE timeout);
int rb_io_maybe_wait_writable(int error, VALUE io, VALUE timeout);
int rb_io_wait_readable(int fd);
int rb_io_wait_writable(int fd);
int rb_wait_for_single_fd(int fd, int events, struct timeval *tv);
int rb_thread_wait_fd(int fd);
int rb_thread_fd_writable(int fd);
VALUE rb_io_fdopen(int fd, int flags, const char *path);
VALUE rb_io_open_descriptor(VALUE klass, int descriptor, int mode, VALUE path, VALUE timeout, struct rb_io_encoding *encoding);
VALUE rb_io_ascii8bit_binmode(VALUE io);
void rb_update_max_fd(int fd);
void rb_fd_fix_cloexec(int fd);
void rb_maygvl_fd_fix_cloexec(int fd);
int rb_cloexec_open(const char *pathname, int flags, mode_t mode);
int rb_cloexec_dup(int oldfd);
int rb_cloexec_dup2(int oldfd, int newfd);
int rb_cloexec_pipe(int fildes[2]);
int rb_cloexec_fcntl_dupfd(int fd, int minfd);
/* `func(arg)` with the GVL released, interruptible as a read on `fptr`. */
VALUE rb_io_blocking_region(rb_io_t *fptr, VALUE (*func)(void *), void *arg);
/* The waiting module the error is extended with (IO::WaitReadable /
 * IO::WaitWritable), as io.c raises IO::EAGAINWaitReadable and friends. */
enum rb_io_wait_readwrite { RB_IO_WAIT_READABLE, RB_IO_WAIT_WRITABLE };
NORETURN(void rb_readwrite_sys_fail(enum rb_io_wait_readwrite waiting, const char *mesg));
NORETURN(void rb_readwrite_syserr_fail(enum rb_io_wait_readwrite waiting, int n, const char *mesg));
RUBY_EXTERN VALUE rb_eIOTimeoutError;
RUBY_EXTERN VALUE rb_mWaitReadable;
RUBY_EXTERN VALUE rb_mWaitWritable;

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
