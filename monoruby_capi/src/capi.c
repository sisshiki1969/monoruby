/*
 * capi.c — CRuby's C API over monoruby's extension ABI.
 *
 * Linked into every extension built against monoruby_capi/include. The
 * interpreter finds `monoruby_capi_init` in the loaded library, hands it
 * a context and the extension's `Init_<name>`, and from then on every
 * method the extension defines is entered through `capi_dispatch`.
 *
 * Three pieces of state make the CRuby model work on top of an ABI whose
 * functions take an explicit context and return errors:
 *
 *   - The current frame (`capi_cur`, per OS thread): the context of the
 *     Ruby call that entered C, the block it was given, and a jmp_buf.
 *     Every Ruby->C entry (`capi_dispatch`, `monoruby_capi_init`) and
 *     every `rb_protect` pushes one. A function that calls back into
 *     Ruby restores `capi_cur` afterwards, so a thread switch inside the
 *     call cannot leave another thread's frame current.
 *   - Exceptions: an API call that fails leaves the error pending in the
 *     interpreter and `capi_unwind` longjmps to the innermost frame. Only
 *     the extension's own C frames are crossed; `capi_dispatch` then
 *     returns MR_UNDEF with the error still pending, and `rb_protect`
 *     takes it as an exception object.
 *   - Rooting: every heap VALUE the layer hands to C is pushed on the
 *     interpreter's temp stack, which the interpreter truncates when the
 *     extension method returns. That keeps alive whatever C holds in
 *     locals — CRuby gets the same from scanning the C stack.
 *
 * And one representation rule: a VALUE in C has CRuby's bit layout
 * (ruby/ruby.h), an MrValue the interpreter's. The two agree on Fixnums,
 * flonums and heap pointers and differ on nil / true / false / undef and
 * on static Symbols, so every value crossing the ABI goes through `M()`
 * (to the interpreter) or `R()` (from it): the arguments and results of
 * every `api->` call, the arguments and result of every C function the
 * interpreter calls (`capi_dispatch`, `block_dispatch`, the native
 * `mark`s), and the elements behind `RARRAY_CONST_PTR`, which are a
 * converted copy owned by the current Ruby call. Only MrValues the layer
 * never shows to C (block handles, native-object ids) stay as they are.
 */
/* This file calls libc's snprintf / vsnprintf itself (ruby/subst.h). */
#define RUBY_DONT_SUBST 1
#include "ruby.h"
#include "ruby/encoding.h"
#include "ruby/io.h"
#include "ruby/thread.h"
#include "ruby/util.h"
#include "ruby/vm.h"
#include "monoruby_ext.h"

#include <errno.h>
#include <math.h>
#include <setjmp.h>
#include <string.h>
#include <stdio.h>

/* The header's convenience macros would rename the definitions below. */
#undef rb_define_method
#undef rb_define_private_method
#undef rb_define_protected_method
#undef rb_define_singleton_method
#undef rb_define_module_function
#undef rb_define_global_function
#undef rb_define_method_id
#undef rb_hash_foreach
#undef rb_check_arity

#define CAPI_NORETURN __attribute__((__noreturn__))

/* ---------------------------------------------------------------------
 * State
 * --------------------------------------------------------------------- */

static const MrApi *api;

/* Whether the running monoruby fills the appended table entry `member`
 * (entries are only ever appended; `size` says how far the table goes). */
#define HAVE_API(member) (api->size >= offsetof(MrApi, member) + sizeof(api->member))

struct capi_buf {
    struct capi_buf *next;
    char data[];
};

/*
 * The elements of an Array as VALUEs, for RARRAY_CONST_PTR: a converted
 * copy, refreshed on every request and owned by the Ruby call. A call
 * holds up to CAPI_ARY_SLOTS such copies at once (one per distinct
 * Array, least recently taken replaced), so pointers into a few Arrays
 * can be live together, as they are in CRuby.
 */
#define CAPI_ARY_SLOTS 8
struct capi_ary_slot {
    MrValue ary;
    VALUE *buf;
    size_t cap;
};

struct capi_method;

struct capi_frame {
    MrContext *ctx;
    MrValue block;
    struct capi_frame *prev;
    /* The method a dispatch frame runs (`rb_frame_this_func`); NULL for
     * the other frames. */
    const struct capi_method *method;
    /* The frame of the Ruby call this one belongs to (itself for a
     * dispatch frame); it owns the C strings and the Array copies
     * handed out during the call. */
    struct capi_frame *owner;
    /* A dispatch frame's receiver and its Data record, once data_of has
     * looked it up: self is alive for the whole call, so the record
     * cannot change under it. */
    VALUE self_val;
    struct capi_data *self_data;
    struct capi_buf *bufs;
    /* Only ary[0 .. ary_init) is initialized: most calls hand out no
     * Array pointer, and the frame is set up on every dispatch. */
    struct capi_ary_slot ary[CAPI_ARY_SLOTS];
    unsigned ary_init;
    unsigned ary_next;
    jmp_buf jb;
};

static __thread struct capi_frame *capi_cur;
/* The `struct rb_io`s handed out by GetOpenFile / MakeOpenFile and not
 * yet written back (capi.c, "rb_io_t"). */
struct capi_io;
static __thread struct capi_io *capi_ios;
static void io_out_all(void);
static void io_in_all(void);
static void io_release(struct capi_frame *f);
static __thread VALUE capi_errinfo = Qnil;
static __thread MrMarker *capi_marker;

static struct capi_frame *
frame(void)
{
    if (!capi_cur) {
        fprintf(stderr, "[monoruby C API] a Ruby C API function was called outside a Ruby call\n");
        abort();
    }
    return capi_cur;
}

static MrContext *
ctx(void)
{
    return frame()->ctx;
}

/* A frame about to be pushed, under the current one. `owner` NULL makes
 * it its own (a Ruby call entering C); a frame opened inside a call
 * (rb_protect) passes that call's. */
static void
frame_init(struct capi_frame *fr, MrContext *c, MrValue block,
           const struct capi_method *method, struct capi_frame *owner)
{
    fr->ctx = c;
    fr->block = block;
    fr->prev = capi_cur;
    fr->owner = owner ? owner : fr;
    fr->method = method;
    fr->bufs = NULL;
    fr->self_val = Qundef;
    fr->self_data = NULL;
    fr->ary_init = 0;
    fr->ary_next = 0;
}

/* Leave for the innermost frame; the error is pending in the interpreter.
   _setjmp / _longjmp throughout, never setjmp / longjmp: on macOS the
   latter save and restore the signal mask, and an unwind could leave the
   main thread with the asynchronous signals blocked, as every other Ruby
   thread has them, so a trapped signal was never delivered again (CRuby
   avoids the mask-saving variants too). */
static CAPI_NORETURN void
capi_unwind(void)
{
    _longjmp(frame()->jb, 1);
}

/*
 * VALUE <-> MrValue. The interpreter's immediates (value.rs): nil 0x04,
 * false 0x14, true 0x1c, undef 0, a Symbol `id << 32 | 0x0c`; CRuby's
 * (ruby/ruby.h): false 0, nil 0x04, true 0x14, undef 0x24, a Symbol
 * `id << 8 | 0x0c`. So nil is the same bits on both sides, CRuby's true
 * is the interpreter's false, and the rest is a table. Fixnums
 * (`n << 1 | 1`), flonums (the same rotation, low bits `10`) and heap
 * pointers (8-aligned, nonzero) are identical. Neither side's heap
 * pointer can look like the other's Symbol: 0x0c in the low byte is not
 * 8-aligned.
 */
#define MR_Qnil ((MrValue)0x04)
#define MR_Qfalse ((MrValue)0x14)
#define MR_Qtrue ((MrValue)0x1c)
#define MR_SYMBOL_FLAG ((MrValue)0x0c)

static inline MrValue
M(VALUE v)
{
    switch (v) {
    case Qfalse: return MR_Qfalse;
    case Qnil: return MR_Qnil;
    case Qtrue: return MR_Qtrue;
    case Qundef: return MR_UNDEF;
    }
    if ((v & 0xff) == RUBY_SYMBOL_FLAG)
        return ((MrValue)(v >> RUBY_SPECIAL_SHIFT) << 32) | MR_SYMBOL_FLAG;
    return (MrValue)v;
}

static inline VALUE
R(MrValue v)
{
    switch (v) {
    case MR_Qfalse: return Qfalse;
    case MR_Qnil: return Qnil;
    case MR_Qtrue: return Qtrue;
    case MR_UNDEF: return Qundef;
    }
    if ((v & 0xffffffffu) == MR_SYMBOL_FLAG)
        return ((VALUE)(v >> 32) << RUBY_SPECIAL_SHIFT) | RUBY_SYMBOL_FLAG;
    return (VALUE)v;
}

/* Keep `v` alive until the current Ruby call returns. */
static VALUE
root(VALUE v)
{
    if (!RB_SPECIAL_CONST_P(v))
        api->temp_push(ctx(), M(v));
    return v;
}

/* A value from the API: MR_UNDEF means an error is pending. */
static VALUE
check(MrValue v)
{
    if (v == MR_UNDEF)
        capi_unwind();
    return root(R(v));
}

/* Up to this many arguments are converted on the stack. */
#define CAPI_SMALL_ARGS 16

static char *frame_buf(size_t len);

/* `argv` as MrValues, in `small` or in a buffer of the current call. */
static const MrValue *
mr_args(int argc, const VALUE *argv, MrValue *small)
{
    if (argc <= 0)
        return NULL;
    MrValue *out = argc <= CAPI_SMALL_ARGS ? small
                                           : (MrValue *)frame_buf((size_t)argc * sizeof(MrValue));
    for (int i = 0; i < argc; i++)
        out[i] = M(argv[i]);
    return out;
}

/* `argv` as VALUEs, likewise. */
static VALUE *
rb_args(int argc, const MrValue *argv, VALUE *small)
{
    VALUE *out = argc <= CAPI_SMALL_ARGS ? small
                                         : (VALUE *)frame_buf((size_t)argc * sizeof(VALUE));
    for (int i = 0; i < argc; i++)
        out[i] = R(argv[i]);
    return out;
}

static void
check_rc(int rc)
{
    if (rc != 0)
        capi_unwind();
}

static char *
frame_buf(size_t len)
{
    struct capi_frame *owner = frame()->owner;
    struct capi_buf *b = malloc(sizeof(struct capi_buf) + len);
    if (!b)
        rb_memerror();
    b->next = owner->bufs;
    owner->bufs = b;
    return b->data;
}

static void
free_bufs(struct capi_frame *f)
{
    struct capi_buf *b = f->bufs;
    while (b) {
        struct capi_buf *next = b->next;
        free(b);
        b = next;
    }
    f->bufs = NULL;
    if (capi_ios)
        io_release(f);
    for (unsigned i = 0; i < f->ary_init; i++)
        free(f->ary[i].buf);
    f->ary_init = 0;
}

/* ---------------------------------------------------------------------
 * Global classes
 * --------------------------------------------------------------------- */

VALUE rb_mKernel, rb_mComparable, rb_mEnumerable, rb_mErrno, rb_mMath,
    rb_mProcess, rb_mGC;
VALUE rb_cBasicObject, rb_cObject, rb_cArray, rb_cClass, rb_cComplex,
    rb_cDir, rb_cEncoding, rb_cEnumerator, rb_cFalseClass, rb_cFile,
    rb_cFloat, rb_cHash, rb_cIO, rb_cInteger, rb_cMatch, rb_cMethod,
    rb_cModule, rb_cNilClass, rb_cNumeric, rb_cProc, rb_cRandom, rb_cRange,
    rb_cRational, rb_cRegexp, rb_cSet, rb_cStat, rb_cString, rb_cStruct,
    rb_cSymbol, rb_cThread, rb_cTime, rb_cTrueClass, rb_cUnboundMethod,
    rb_cBinding;
VALUE rb_eException, rb_eStandardError, rb_eSystemExit, rb_eInterrupt,
    rb_eSignal, rb_eFatal, rb_eArgError, rb_eEOFError, rb_eIndexError,
    rb_eStopIteration, rb_eKeyError, rb_eRangeError, rb_eIOError,
    rb_eRuntimeError, rb_eFrozenError, rb_eSecurityError,
    rb_eSystemCallError, rb_eThreadError, rb_eTypeError, rb_eZeroDivError,
    rb_eNotImpError, rb_eNoMemError, rb_eNoMethodError,
    rb_eFloatDomainError, rb_eLocalJumpError, rb_eSysStackError,
    rb_eRegexpError, rb_eEncodingError, rb_eEncCompatError,
    rb_eNoMatchingPatternError, rb_eNoMatchingPatternKeyError,
    rb_eScriptError, rb_eNameError, rb_eSyntaxError, rb_eLoadError,
    rb_eMathDomainError, rb_eIOTimeoutError;
VALUE rb_mWaitReadable, rb_mWaitWritable;
VALUE rb_stdin, rb_stdout, rb_stderr;

static const struct {
    VALUE *slot;
    const char *path;
} capi_globals[] = {
    {&rb_mKernel, "Kernel"}, {&rb_mComparable, "Comparable"},
    {&rb_mEnumerable, "Enumerable"}, {&rb_mErrno, "Errno"},
    {&rb_mMath, "Math"}, {&rb_mProcess, "Process"}, {&rb_mGC, "GC"},
    {&rb_cBasicObject, "BasicObject"}, {&rb_cObject, "Object"},
    {&rb_cArray, "Array"}, {&rb_cClass, "Class"}, {&rb_cComplex, "Complex"},
    {&rb_cDir, "Dir"}, {&rb_cEncoding, "Encoding"},
    {&rb_cEnumerator, "Enumerator"}, {&rb_cFalseClass, "FalseClass"},
    {&rb_cFile, "File"}, {&rb_cFloat, "Float"}, {&rb_cHash, "Hash"},
    {&rb_cIO, "IO"}, {&rb_cInteger, "Integer"}, {&rb_cMatch, "MatchData"},
    {&rb_cMethod, "Method"}, {&rb_cModule, "Module"},
    {&rb_cNilClass, "NilClass"}, {&rb_cNumeric, "Numeric"},
    {&rb_cProc, "Proc"}, {&rb_cRandom, "Random"}, {&rb_cRange, "Range"},
    {&rb_cRational, "Rational"}, {&rb_cRegexp, "Regexp"}, {&rb_cSet, "Set"},
    {&rb_cStat, "File::Stat"}, {&rb_cString, "String"},
    {&rb_cStruct, "Struct"}, {&rb_cSymbol, "Symbol"},
    {&rb_cThread, "Thread"}, {&rb_cTime, "Time"},
    {&rb_cTrueClass, "TrueClass"}, {&rb_cUnboundMethod, "UnboundMethod"},
    {&rb_cBinding, "Binding"},
    {&rb_eException, "Exception"}, {&rb_eStandardError, "StandardError"},
    {&rb_eSystemExit, "SystemExit"}, {&rb_eInterrupt, "Interrupt"},
    {&rb_eSignal, "SignalException"}, {&rb_eFatal, "Exception"},
    {&rb_eArgError, "ArgumentError"}, {&rb_eEOFError, "EOFError"},
    {&rb_eIndexError, "IndexError"}, {&rb_eStopIteration, "StopIteration"},
    {&rb_eKeyError, "KeyError"}, {&rb_eRangeError, "RangeError"},
    {&rb_eIOError, "IOError"}, {&rb_eRuntimeError, "RuntimeError"},
    {&rb_eFrozenError, "FrozenError"}, {&rb_eSecurityError, "SecurityError"},
    {&rb_eSystemCallError, "SystemCallError"},
    {&rb_eThreadError, "ThreadError"}, {&rb_eTypeError, "TypeError"},
    {&rb_eZeroDivError, "ZeroDivisionError"},
    {&rb_eNotImpError, "NotImplementedError"},
    {&rb_eNoMemError, "NoMemoryError"}, {&rb_eNoMethodError, "NoMethodError"},
    {&rb_eFloatDomainError, "FloatDomainError"},
    {&rb_eLocalJumpError, "LocalJumpError"},
    {&rb_eSysStackError, "SystemStackError"},
    {&rb_eRegexpError, "RegexpError"}, {&rb_eEncodingError, "EncodingError"},
    {&rb_eEncCompatError, "Encoding::CompatibilityError"},
    {&rb_eNoMatchingPatternError, "NoMatchingPatternError"},
    {&rb_eNoMatchingPatternKeyError, "NoMatchingPatternKeyError"},
    {&rb_eScriptError, "ScriptError"}, {&rb_eNameError, "NameError"},
    {&rb_eSyntaxError, "SyntaxError"}, {&rb_eLoadError, "LoadError"},
    {&rb_eMathDomainError, "Math::DomainError"},
    {&rb_eIOTimeoutError, "IO::TimeoutError"},
    {&rb_mWaitReadable, "IO::WaitReadable"},
    {&rb_mWaitWritable, "IO::WaitWritable"},
};

/* `A::B::C` by constant lookup alone; nil when any part is missing. */
static VALUE
lookup_path(const char *path)
{
    VALUE cur = Qundef;
    char part[128];
    const char *p = path;
    for (;;) {
        const char *sep = strstr(p, "::");
        size_t n = sep ? (size_t)(sep - p) : strlen(p);
        if (n >= sizeof(part))
            return Qnil;
        memcpy(part, p, n);
        part[n] = 0;
        MrValue v = api->const_get(ctx(), M(cur), part);
        if (v == MR_UNDEF)
            return Qnil;
        cur = R(v);
        if (!sep)
            return cur;
        p = sep + 2;
    }
}

static void
init_globals(void)
{
    for (size_t i = 0; i < sizeof(capi_globals) / sizeof(capi_globals[0]); i++)
        *capi_globals[i].slot = lookup_path(capi_globals[i].path);
    /* The standard streams as the process started with them. */
    rb_stdin = lookup_path("STDIN");
    rb_stdout = lookup_path("STDOUT");
    rb_stderr = lookup_path("STDERR");
}

/* ---------------------------------------------------------------------
 * Entry from Ruby
 * --------------------------------------------------------------------- */

static void func_names_reset(void);

struct capi_method {
    VALUE (*func)(ANYARGS);
    int arity;
    ID mid;
};

typedef VALUE (*cf0)(VALUE);
typedef VALUE (*cf1)(VALUE, VALUE);
typedef VALUE (*cf2)(VALUE, VALUE, VALUE);
typedef VALUE (*cf3)(VALUE, VALUE, VALUE, VALUE);
typedef VALUE (*cf4)(VALUE, VALUE, VALUE, VALUE, VALUE);
typedef VALUE (*cf5)(VALUE, VALUE, VALUE, VALUE, VALUE, VALUE);
typedef VALUE (*cf6)(VALUE, VALUE, VALUE, VALUE, VALUE, VALUE, VALUE);
typedef VALUE (*cf7)(VALUE, VALUE, VALUE, VALUE, VALUE, VALUE, VALUE, VALUE);
typedef VALUE (*cf8)(VALUE, VALUE, VALUE, VALUE, VALUE, VALUE, VALUE, VALUE, VALUE);
typedef VALUE (*cf9)(VALUE, VALUE, VALUE, VALUE, VALUE, VALUE, VALUE, VALUE, VALUE, VALUE);
typedef VALUE (*cf10)(VALUE, VALUE, VALUE, VALUE, VALUE, VALUE, VALUE, VALUE, VALUE, VALUE, VALUE);
typedef VALUE (*cf11)(VALUE, VALUE, VALUE, VALUE, VALUE, VALUE, VALUE, VALUE, VALUE, VALUE, VALUE, VALUE);
typedef VALUE (*cf12)(VALUE, VALUE, VALUE, VALUE, VALUE, VALUE, VALUE, VALUE, VALUE, VALUE, VALUE, VALUE, VALUE);
typedef VALUE (*cf13)(VALUE, VALUE, VALUE, VALUE, VALUE, VALUE, VALUE, VALUE, VALUE, VALUE, VALUE, VALUE, VALUE, VALUE);
typedef VALUE (*cf14)(VALUE, VALUE, VALUE, VALUE, VALUE, VALUE, VALUE, VALUE, VALUE, VALUE, VALUE, VALUE, VALUE, VALUE, VALUE);
typedef VALUE (*cf15)(VALUE, VALUE, VALUE, VALUE, VALUE, VALUE, VALUE, VALUE, VALUE, VALUE, VALUE, VALUE, VALUE, VALUE, VALUE, VALUE);
typedef VALUE (*cfv)(int, VALUE *, VALUE);

static VALUE
call_cfunc(const struct capi_method *m, VALUE self, int argc, VALUE *a)
{
    switch (m->arity) {
    case -2: return ((cf1)m->func)(self, rb_ary_new_from_values(argc, a));
    case -1: return ((cfv)m->func)(argc, a, self);
    case 0: return ((cf0)m->func)(self);
    case 1: return ((cf1)m->func)(self, a[0]);
    case 2: return ((cf2)m->func)(self, a[0], a[1]);
    case 3: return ((cf3)m->func)(self, a[0], a[1], a[2]);
    case 4: return ((cf4)m->func)(self, a[0], a[1], a[2], a[3]);
    case 5: return ((cf5)m->func)(self, a[0], a[1], a[2], a[3], a[4]);
    case 6: return ((cf6)m->func)(self, a[0], a[1], a[2], a[3], a[4], a[5]);
    case 7: return ((cf7)m->func)(self, a[0], a[1], a[2], a[3], a[4], a[5], a[6]);
    case 8: return ((cf8)m->func)(self, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7]);
    case 9: return ((cf9)m->func)(self, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8]);
    case 10: return ((cf10)m->func)(self, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9]);
    case 11: return ((cf11)m->func)(self, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10]);
    case 12: return ((cf12)m->func)(self, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10], a[11]);
    case 13: return ((cf13)m->func)(self, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10], a[11], a[12]);
    case 14: return ((cf14)m->func)(self, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10], a[11], a[12], a[13]);
    case 15: return ((cf15)m->func)(self, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10], a[11], a[12], a[13], a[14]);
    }
    rb_raise(rb_eArgError, "unsupported arity %d", m->arity);
}

static void materialize_payload(VALUE self);

/* The MrMethodDataFn of every method defined through this layer. */
static MrValue
capi_dispatch(MrContext *c, MrValue self, int argc, const MrValue *argv, MrValue block, void *data)
{
    struct capi_frame fr;
    struct capi_frame *volatile fp = &fr;
    volatile MrValue result = MR_UNDEF;
    frame_init(&fr, c, block, (const struct capi_method *)data, NULL);
    fr.self_val = R(self);
    capi_cur = &fr;
    if (_setjmp(fr.jb) == 0) {
        VALUE small[CAPI_SMALL_ARGS];
        VALUE *a = rb_args(argc, argv, small);
        VALUE recv = R(self);
        materialize_payload(recv);
        result = M(call_cfunc((const struct capi_method *)data, recv, argc, a));
    }
    free_bufs(fp);
    capi_cur = fp->prev;
    return result;
}

/*
 * Called by the interpreter once the library is loaded: bind to the
 * table, fill in the global classes and run `Init_<name>`.
 */
__attribute__((visibility("default"))) int
monoruby_capi_init(MrContext *c, void (*init)(void))
{
    if (c->api->abi_version != MR_ABI_VERSION ||
        c->api->size < offsetof(MrApi, gvar_hook) + sizeof(void *))
        return -2;
    api = c->api;
    func_names_reset();
    struct capi_frame fr;
    struct capi_frame *volatile fp = &fr;
    volatile int rc = -1;
    frame_init(&fr, c, MR_UNDEF, NULL, NULL);
    capi_cur = &fr;
    if (_setjmp(fr.jb) == 0) {
        init_globals();
        init();
        rc = 0;
    }
    free_bufs(fp);
    capi_cur = fp->prev;
    return rc;
}

/* ---------------------------------------------------------------------
 * Formatting: printf with CRuby's PRIsVALUE
 * --------------------------------------------------------------------- */

struct sbuf {
    char *p;
    size_t len, cap;
};

static void
sb_add(struct sbuf *b, const char *s, size_t n)
{
    if (b->len + n + 1 > b->cap) {
        size_t cap = b->cap ? b->cap : 64;
        while (cap < b->len + n + 1)
            cap *= 2;
        char *p = realloc(b->p, cap);
        if (!p) {
            free(b->p);
            rb_memerror();
        }
        b->p = p;
        b->cap = cap;
    }
    memcpy(b->p + b->len, s, n);
    b->len += n;
    b->p[b->len] = 0;
}

static void
sb_printf(struct sbuf *b, const char *spec, ...)
{
    char small[128];
    va_list ap, ap2;
    va_start(ap, spec);
    va_copy(ap2, ap);
    int n = vsnprintf(small, sizeof(small), spec, ap);
    va_end(ap);
    if (n < 0) {
        va_end(ap2);
        return;
    }
    if ((size_t)n < sizeof(small)) {
        sb_add(b, small, (size_t)n);
    } else {
        char *big = malloc((size_t)n + 1);
        if (!big) {
            va_end(ap2);
            rb_memerror();
        }
        vsnprintf(big, (size_t)n + 1, spec, ap2);
        sb_add(b, big, (size_t)n);
        free(big);
    }
    va_end(ap2);
}

enum { LEN_NONE, LEN_HH, LEN_H, LEN_L, LEN_LL, LEN_Z, LEN_J, LEN_T, LEN_BIG_L };

static void
capi_vformat(struct sbuf *b, const char *fmt, va_list ap)
{
    const char *p = fmt;
    while (*p) {
        if (*p != '%') {
            const char *q = strchr(p, '%');
            size_t n = q ? (size_t)(q - p) : strlen(p);
            sb_add(b, p, n);
            p += n;
            continue;
        }
        const char *start = p++;
        if (*p == '%') {
            sb_add(b, "%", 1);
            p++;
            continue;
        }
        int plus = 0, nstar = 0, w[2] = {0, 0};
        while (*p && strchr("-+ #0", *p)) {
            if (*p == '+')
                plus = 1;
            p++;
        }
        if (*p == '*') {
            w[nstar++] = va_arg(ap, int);
            p++;
        } else {
            while (*p >= '0' && *p <= '9')
                p++;
        }
        if (*p == '.') {
            p++;
            if (*p == '*') {
                w[nstar++] = va_arg(ap, int);
                p++;
            } else {
                while (*p >= '0' && *p <= '9')
                    p++;
            }
        }
        int len = LEN_NONE;
        switch (*p) {
        case 'h':
            len = LEN_H;
            if (*++p == 'h') {
                len = LEN_HH;
                p++;
            }
            break;
        case 'l':
            len = LEN_L;
            if (*++p == 'l') {
                len = LEN_LL;
                p++;
            }
            break;
        case 'z': len = LEN_Z; p++; break;
        case 'j': len = LEN_J; p++; break;
        case 't': len = LEN_T; p++; break;
        case 'L': len = LEN_BIG_L; p++; break;
        }
        char conv = *p;
        if (!conv)
            break;
        p++;
        if (conv == 'i' && len == LEN_L && *p == '\v') {
            /* PRIsVALUE: to_s, or inspect with the `+` flag. */
            p++;
            VALUE v = va_arg(ap, VALUE);
            VALUE s = plus ? rb_inspect(v) : rb_obj_as_string(v);
            sb_add(b, RSTRING_PTR(s), (size_t)RSTRING_LEN(s));
            continue;
        }
        char spec[64];
        size_t speclen = (size_t)(p - start);
        if (speclen >= sizeof(spec))
            speclen = sizeof(spec) - 1;
        memcpy(spec, start, speclen);
        spec[speclen] = 0;
#define EMIT(T)                                                           \
    do {                                                                  \
        T x = va_arg(ap, T);                                              \
        if (nstar == 0) sb_printf(b, spec, x);                            \
        else if (nstar == 1) sb_printf(b, spec, w[0], x);                 \
        else sb_printf(b, spec, w[0], w[1], x);                           \
    } while (0)
        switch (conv) {
        case 'd':
        case 'i':
            switch (len) {
            case LEN_L: EMIT(long); break;
            case LEN_LL: EMIT(long long); break;
            case LEN_Z: EMIT(ssize_t); break;
            case LEN_J: EMIT(intmax_t); break;
            case LEN_T: EMIT(ptrdiff_t); break;
            default: EMIT(int); break;
            }
            break;
        case 'u':
        case 'o':
        case 'x':
        case 'X':
            switch (len) {
            case LEN_L: EMIT(unsigned long); break;
            case LEN_LL: EMIT(unsigned long long); break;
            case LEN_Z: EMIT(size_t); break;
            case LEN_J: EMIT(uintmax_t); break;
            case LEN_T: EMIT(ptrdiff_t); break;
            default: EMIT(unsigned int); break;
            }
            break;
        case 'c': EMIT(int); break;
        case 's': EMIT(const char *); break;
        case 'p': EMIT(void *); break;
        case 'f': case 'F': case 'e': case 'E':
        case 'g': case 'G': case 'a': case 'A':
            if (len == LEN_BIG_L) {
                EMIT(long double);
            }
            else {
                double x = va_arg(ap, double);
                if (isinf(x) || isnan(x)) {
                    /* CRuby's spellings (BSD vfprintf), not libc's
                     * "inf" / "nan"; a width in the spec pads on the left. */
                    char t[8];
                    snprintf(t, sizeof(t), "%s%s", isnan(x) ? "" : x < 0 ? "-" : plus ? "+" : "",
                             isnan(x) ? "NaN" : "Inf");
                    const char *q = start + 1;
                    while (*q && strchr("+- #0", *q))
                        q++;
                    int width = 0;
                    if (*q == '*')
                        width = nstar ? w[0] : 0;
                    else
                        while (*q >= '0' && *q <= '9')
                            width = width * 10 + (*q++ - '0');
                    sb_printf(b, "%*s", width, t);
                }
                else if (nstar == 0) {
                    sb_printf(b, spec, x);
                }
                else if (nstar == 1) {
                    sb_printf(b, spec, w[0], x);
                }
                else {
                    sb_printf(b, spec, w[0], w[1], x);
                }
            }
            break;
        case 'n': (void)va_arg(ap, void *); break;
        default:
            sb_add(b, start, (size_t)(p - start));
            break;
        }
#undef EMIT
    }
}

/* The formatted message in a per-call buffer. */
static const char *
vformat_buf(const char *fmt, va_list ap, size_t *lenp)
{
    struct sbuf b = {NULL, 0, 0};
    capi_vformat(&b, fmt, ap);
    char *out = frame_buf(b.len + 1);
    if (b.p)
        memcpy(out, b.p, b.len + 1);
    else
        out[0] = 0;
    free(b.p);
    *lenp = b.len;
    return out;
}

VALUE
rb_vsprintf(const char *fmt, va_list ap)
{
    size_t len;
    const char *s = vformat_buf(fmt, ap, &len);
    return rb_str_new(s, (long)len);
}

VALUE
rb_sprintf(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    VALUE s = rb_vsprintf(fmt, ap);
    va_end(ap);
    return s;
}

VALUE
rb_str_vcatf(VALUE str, const char *fmt, va_list ap)
{
    size_t len;
    const char *s = vformat_buf(fmt, ap, &len);
    return rb_str_cat(str, s, (long)len);
}

VALUE
rb_str_catf(VALUE str, const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    rb_str_vcatf(str, fmt, ap);
    va_end(ap);
    return str;
}

VALUE
rb_str_format(int argc, const VALUE *argv, VALUE fmt)
{
    VALUE args = rb_ary_new_from_values(argc, argv);
    return rb_funcall(rb_mKernel, rb_intern("format"), 2, fmt, args);
}

/* ---------------------------------------------------------------------
 * Exceptions
 * --------------------------------------------------------------------- */

void
rb_exc_raise(VALUE exc)
{
    api->raise_exception(ctx(), M(exc));
    capi_unwind();
}

void
rb_raise(VALUE klass, const char *fmt, ...)
{
    va_list ap;
    size_t len;
    va_start(ap, fmt);
    const char *msg = vformat_buf(fmt, ap, &len);
    va_end(ap);
    api->raise(ctx(), M(klass), msg, len);
    capi_unwind();
}

static CAPI_NORETURN void
vbug(const char *what, const char *fmt, va_list ap)
{
    fprintf(stderr, "[%s] ", what);
    vfprintf(stderr, fmt, ap);
    fputc('\n', stderr);
    abort();
}

void
rb_bug(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    vbug("BUG", fmt, ap);
}

void
rb_fatal(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    vbug("FATAL", fmt, ap);
}

void
rb_memerror(void)
{
    rb_raise(rb_eNoMemError, "failed to allocate memory");
}

VALUE
rb_exc_new(VALUE klass, const char *ptr, long len)
{
    return rb_funcall(klass, rb_intern("new"), 1, rb_str_new(ptr, len));
}

VALUE
rb_exc_new_cstr(VALUE klass, const char *ptr)
{
    return rb_exc_new(klass, ptr, (long)strlen(ptr));
}

VALUE
rb_exc_new_str(VALUE klass, VALUE str)
{
    return rb_funcall(klass, rb_intern("new"), 1, str);
}

VALUE
rb_syserr_new(int err, const char *msg)
{
    VALUE m = msg ? rb_str_new_cstr(msg) : Qnil;
    return rb_funcall(rb_eSystemCallError, rb_intern("new"), 2, m, INT2FIX(err));
}

void
rb_syserr_fail(int err, const char *msg)
{
    rb_exc_raise(rb_syserr_new(err, msg));
}

void
rb_syserr_fail_str(int err, VALUE msg)
{
    rb_exc_raise(rb_funcall(rb_eSystemCallError, rb_intern("new"), 2, msg, INT2FIX(err)));
}

void
rb_sys_fail(const char *msg)
{
    rb_syserr_fail(errno, msg);
}

void
rb_sys_fail_str(VALUE msg)
{
    rb_syserr_fail_str(errno, msg);
}

void
rb_notimplement(void)
{
    rb_raise(rb_eNotImpError, "function is unimplemented on this machine");
}

/* The body of every generated stub in libmonoruby_capi_stubs.a (build.rs
 * `build_capi_stubs_archive`): a CRuby API function the layer does not
 * implement resolves to a stub that raises here when called, so an
 * extension that merely references it still loads. */
void
capi_unimplemented(const char *name)
{
    rb_raise(rb_eNotImpError, "%s() is not implemented in monoruby's C API", name);
}

void
rb_error_arity(int argc, int min, int max)
{
    if (min == max)
        rb_raise(rb_eArgError, "wrong number of arguments (given %d, expected %d)", argc, min);
    else if (max == UNLIMITED_ARGUMENTS)
        rb_raise(rb_eArgError, "wrong number of arguments (given %d, expected %d+)", argc, min);
    else
        rb_raise(rb_eArgError, "wrong number of arguments (given %d, expected %d..%d)", argc, min, max);
}

static void
vwarn(const char *fmt, va_list ap)
{
    size_t len;
    const char *msg = vformat_buf(fmt, ap, &len);
    VALUE s = rb_utf8_str_new_cstr("warning: ");
    rb_str_cat(s, msg, (long)len);
    rb_funcall(rb_mKernel, rb_intern("warn"), 1, s);
}

void
rb_warn(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    vwarn(fmt, ap);
    va_end(ap);
}

void
rb_warning(const char *fmt, ...)
{
    if (rb_gv_get("$VERBOSE") != Qtrue)
        return;
    va_list ap;
    va_start(ap, fmt);
    vwarn(fmt, ap);
    va_end(ap);
}

VALUE
rb_protect(VALUE (*func)(VALUE), VALUE arg, int *state)
{
    struct capi_frame *outer = frame();
    struct capi_frame fr;
    volatile VALUE result = Qnil;
    frame_init(&fr, outer->ctx, outer->block, NULL, outer->owner);
    capi_cur = &fr;
    if (_setjmp(fr.jb) == 0) {
        result = func(arg);
        capi_cur = outer;
        if (state)
            *state = 0;
        return result;
    }
    capi_cur = outer;
    MrValue err = api->error_take(outer->ctx);
    capi_errinfo = err == MR_UNDEF ? Qnil : root(R(err));
    if (state)
        *state = TAG_RAISE;
    return Qnil;
}

void
rb_jump_tag(int state)
{
    (void)state;
    VALUE exc = capi_errinfo;
    capi_errinfo = Qnil;
    if (NIL_P(exc))
        rb_raise(rb_eRuntimeError, "rb_jump_tag: no pending exception");
    rb_exc_raise(exc);
}

VALUE
rb_errinfo(void)
{
    return capi_errinfo;
}

void
rb_set_errinfo(VALUE err)
{
    capi_errinfo = err;
}

VALUE
rb_ensure(VALUE (*b_proc)(VALUE), VALUE data1, VALUE (*e_proc)(VALUE), VALUE data2)
{
    int state;
    VALUE result = rb_protect(b_proc, data1, &state);
    VALUE pending = capi_errinfo;
    e_proc(data2);
    if (state) {
        capi_errinfo = pending;
        rb_jump_tag(state);
    }
    return result;
}

static VALUE
vrescue(VALUE (*b_proc)(VALUE), VALUE data1, VALUE (*r_proc)(VALUE, VALUE), VALUE data2,
        const VALUE *classes, int nclasses)
{
    int state;
    VALUE result = rb_protect(b_proc, data1, &state);
    if (!state)
        return result;
    VALUE exc = capi_errinfo;
    for (int i = 0; i < nclasses; i++) {
        if (api->is_kind_of(ctx(), M(exc), M(classes[i]))) {
            /* `$!` is the exception while the rescue function runs, as in
             * CRuby (zlib's read fallback asks rb_errinfo which it was),
             * and is cleared once it returns. */
            if (!r_proc) {
                capi_errinfo = Qnil;
                return Qnil;
            }
            VALUE r = r_proc(data2, exc);
            capi_errinfo = Qnil;
            return r;
        }
    }
    rb_jump_tag(state);
}

VALUE
rb_vrescue2(VALUE (*b_proc)(VALUE), VALUE data1, VALUE (*r_proc)(VALUE, VALUE), VALUE data2, va_list ap)
{
    VALUE classes[16];
    int n = 0;
    for (;;) {
        VALUE k = va_arg(ap, VALUE);
        if (k == 0 || n == 16)
            break;
        classes[n++] = k;
    }
    return vrescue(b_proc, data1, r_proc, data2, classes, n);
}

VALUE
rb_rescue2(VALUE (*b_proc)(VALUE), VALUE data1, VALUE (*r_proc)(VALUE, VALUE), VALUE data2, ...)
{
    va_list ap;
    va_start(ap, data2);
    VALUE result = rb_vrescue2(b_proc, data1, r_proc, data2, ap);
    va_end(ap);
    return result;
}

VALUE
rb_rescue(VALUE (*b_proc)(VALUE), VALUE data1, VALUE (*r_proc)(VALUE, VALUE), VALUE data2)
{
    VALUE k = rb_eStandardError;
    return vrescue(b_proc, data1, r_proc, data2, &k, 1);
}


/* ---------------------------------------------------------------------
 * Arguments
 * --------------------------------------------------------------------- */

int
rb_keyword_given_p(void)
{
    return api->keyword_given(ctx());
}

/*
 * The trailing Hash of a call is its keywords, when the format asks for
 * them — the interpreter hands an extension method its keywords as a
 * last positional Hash.
 */
static int
scan_args_v(int kw_flag, int argc, const VALUE *argv, const char *fmt, va_list ap)
{
    const char *p = fmt;
    int n_lead = 0, n_opt = 0, n_trail = 0, f_var = 0, f_hash = 0, f_block = 0;
    if (*p >= '0' && *p <= '9') {
        n_lead = *p++ - '0';
        if (*p >= '0' && *p <= '9')
            n_opt = *p++ - '0';
    }
    if (*p == '*') {
        f_var = 1;
        p++;
    }
    if (*p >= '0' && *p <= '9')
        n_trail = *p++ - '0';
    if (*p == ':') {
        f_hash = 1;
        p++;
    }
    if (*p == '&') {
        f_block = 1;
        p++;
    }
    if (*p)
        rb_raise(rb_eArgError, "bad scan arg format: %s", fmt);

    /* Under RB_SCAN_ARGS_PASS_CALLED_KEYWORDS (plain rb_scan_args) the
     * trailing Hash is the keywords only when the call passed keywords;
     * a Hash passed positionally stays an argument. */
    VALUE hash = Qnil;
    if (f_hash && argc > 0 && RB_TYPE_P(argv[argc - 1], T_HASH)) {
        int take;
        switch (kw_flag) {
        case RB_SCAN_ARGS_KEYWORDS:
        case RB_SCAN_ARGS_LAST_HASH_KEYWORDS:
            take = 1;
            break;
        case RB_SCAN_ARGS_EMPTY_KEYWORDS:
            take = 0;
            break;
        default:
            take = rb_keyword_given_p();
            break;
        }
        if (take) {
            hash = argv[argc - 1];
            argc--;
        }
    }
    int n_mand = n_lead + n_trail;
    if (argc < n_mand || (!f_var && argc > n_mand + n_opt))
        rb_error_arity(argc, n_mand, f_var ? UNLIMITED_ARGUMENTS : n_mand + n_opt);

    int i = 0;
    for (int j = 0; j < n_lead; j++) {
        VALUE *v = va_arg(ap, VALUE *);
        if (v) *v = argv[i];
        i++;
    }
    int n_opt_given = argc - n_mand;
    if (n_opt_given > n_opt) n_opt_given = n_opt;
    for (int j = 0; j < n_opt; j++) {
        VALUE *v = va_arg(ap, VALUE *);
        if (j < n_opt_given) {
            if (v) *v = argv[i];
            i++;
        } else if (v) {
            *v = Qnil;
        }
    }
    if (f_var) {
        int n_var = argc - i - n_trail;
        VALUE *v = va_arg(ap, VALUE *);
        if (v) *v = rb_ary_new_from_values(n_var, argv + i);
        i += n_var;
    }
    for (int j = 0; j < n_trail; j++) {
        VALUE *v = va_arg(ap, VALUE *);
        if (v) *v = argv[i];
        i++;
    }
    if (f_hash) {
        VALUE *v = va_arg(ap, VALUE *);
        if (v) *v = hash;
    }
    if (f_block) {
        VALUE *v = va_arg(ap, VALUE *);
        if (v) *v = rb_block_proc();
    }
    return argc;
}

int
rb_scan_args(int argc, const VALUE *argv, const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    int n = scan_args_v(RB_SCAN_ARGS_PASS_CALLED_KEYWORDS, argc, argv, fmt, ap);
    va_end(ap);
    return n;
}

int
rb_scan_args_kw(int kw_flag, int argc, const VALUE *argv, const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    int n = scan_args_v(kw_flag, argc, argv, fmt, ap);
    va_end(ap);
    return n;
}

static void
keyword_error(const char *what, VALUE keys)
{
    long n = RARRAY_LEN(keys);
    VALUE msg = rb_sprintf("%s keyword%s: ", what, n > 1 ? "s" : "");
    for (long i = 0; i < n; i++) {
        if (i)
            rb_str_cat_cstr(msg, ", ");
        rb_str_append(msg, rb_inspect(rb_ary_entry(keys, i)));
    }
    rb_exc_raise(rb_exc_new_str(rb_eArgError, msg));
}

int
rb_get_kwargs(VALUE hash, const ID *table, int required, int optional, VALUE *values)
{
    int rest = optional < 0;
    if (rest)
        optional = -1 - optional;
    int found = 0;
    VALUE missing = Qnil;
    for (int i = 0; i < required + optional; i++) {
        VALUE key = ID2SYM(table[i]);
        VALUE v = Qundef;
        if (!NIL_P(hash) && RTEST(rb_hash_has_key(hash, key))) {
            v = rb_hash_aref(hash, key);
            if (values)
                rb_hash_delete(hash, key);
            found++;
        } else if (i < required) {
            if (NIL_P(missing))
                missing = rb_ary_new();
            rb_ary_push(missing, key);
        }
        if (values)
            values[i] = v;
    }
    if (!NIL_P(missing))
        keyword_error("missing", missing);
    if (!rest && !NIL_P(hash) && values && rb_hash_size_num(hash) > 0)
        keyword_error("unknown", rb_hash_keys(hash));
    return found;
}

VALUE
rb_extract_keywords(VALUE *orighash)
{
    VALUE h = *orighash;
    *orighash = 0;
    return h;
}

/* ---------------------------------------------------------------------
 * Calling Ruby
 * --------------------------------------------------------------------- */

static VALUE
funcall_block(VALUE recv, ID mid, int argc, const VALUE *argv, MrValue block)
{
    struct capi_frame *saved = frame();
    MrValue small[CAPI_SMALL_ARGS];
    if (capi_ios)
        io_out_all();
    MrValue r = api->funcall_sym(saved->ctx, M(recv), (MrSym)mid, argc,
                                 mr_args(argc, argv, small), block);
    capi_cur = saved;
    if (capi_ios)
        io_in_all();
    return check(r);
}

/* funcall_block, with the last argument passed as keywords when
 * `kw_splat` is set (and it is a Hash). */
static VALUE
funcall_block_kw(VALUE recv, ID mid, int argc, const VALUE *argv, MrValue block, int kw_splat)
{
    if (!kw_splat || argc == 0 || !RB_TYPE_P(argv[argc - 1], T_HASH) ||
        api->size < offsetof(MrApi, funcall_kw) + sizeof(api->funcall_kw))
        return funcall_block(recv, mid, argc, argv, block);
    struct capi_frame *saved = frame();
    MrValue small[CAPI_SMALL_ARGS];
    if (capi_ios)
        io_out_all();
    MrValue r = api->funcall_kw(saved->ctx, M(recv), (MrSym)mid, argc,
                                mr_args(argc, argv, small), block, 1);
    capi_cur = saved;
    if (capi_ios)
        io_in_all();
    return check(r);
}

VALUE
rb_funcallv_kw(VALUE recv, ID mid, int argc, const VALUE *argv, int kw_splat)
{
    return funcall_block_kw(recv, mid, argc, argv, MR_UNDEF, kw_splat);
}

VALUE
rb_funcall_passing_block_kw(VALUE recv, ID mid, int argc, const VALUE *argv, int kw_splat)
{
    return funcall_block_kw(recv, mid, argc, argv, frame()->block, kw_splat);
}

VALUE
rb_funcall_with_block_kw(VALUE recv, ID mid, int argc, const VALUE *argv, VALUE procval, int kw_splat)
{
    return funcall_block_kw(recv, mid, argc, argv, NIL_P(procval) ? MR_UNDEF : M(procval), kw_splat);
}

VALUE
rb_funcallv(VALUE recv, ID mid, int argc, const VALUE *argv)
{
    return funcall_block(recv, mid, argc, argv, MR_UNDEF);
}

VALUE
rb_funcallv_public(VALUE recv, ID mid, int argc, const VALUE *argv)
{
    return rb_funcallv(recv, mid, argc, argv);
}

VALUE
rb_funcall2(VALUE recv, ID mid, int argc, const VALUE *argv)
{
    return rb_funcallv(recv, mid, argc, argv);
}

VALUE
rb_funcall3(VALUE recv, ID mid, int argc, const VALUE *argv)
{
    return rb_funcallv(recv, mid, argc, argv);
}

VALUE
rb_funcall(VALUE recv, ID mid, int n, ...)
{
    VALUE argv[16];
    VALUE *a = argv;
    if (n > 16) {
        a = (VALUE *)frame_buf(sizeof(VALUE) * (size_t)n);
    }
    va_list ap;
    va_start(ap, n);
    for (int i = 0; i < n; i++)
        a[i] = va_arg(ap, VALUE);
    va_end(ap);
    return rb_funcallv(recv, mid, n, a);
}

VALUE
rb_funcall_passing_block(VALUE recv, ID mid, int argc, const VALUE *argv)
{
    return funcall_block(recv, mid, argc, argv, frame()->block);
}

VALUE
rb_funcall_with_block(VALUE recv, ID mid, int argc, const VALUE *argv, VALUE procval)
{
    return funcall_block(recv, mid, argc, argv, NIL_P(procval) ? MR_UNDEF : M(procval));
}

VALUE
rb_apply(VALUE recv, ID mid, VALUE args)
{
    return rb_funcallv(recv, mid, (int)RARRAY_LEN(args), RARRAY_CONST_PTR(args));
}

VALUE
rb_yield_values2(int argc, const VALUE *argv)
{
    struct capi_frame *saved = frame();
    MrValue small[CAPI_SMALL_ARGS];
    MrValue r = api->yield_block(saved->ctx, saved->block, argc, mr_args(argc, argv, small));
    capi_cur = saved;
    return check(r);
}

VALUE
rb_yield(VALUE val)
{
    if (val == Qundef)
        return rb_yield_values2(0, NULL);
    return rb_yield_values2(1, &val);
}

VALUE
rb_yield_values(int n, ...)
{
    VALUE argv[16];
    if (n > 16)
        rb_raise(rb_eArgError, "rb_yield_values: too many values (%d)", n);
    va_list ap;
    va_start(ap, n);
    for (int i = 0; i < n; i++)
        argv[i] = va_arg(ap, VALUE);
    va_end(ap);
    return rb_yield_values2(n, argv);
}

VALUE
rb_yield_splat(VALUE args)
{
    VALUE ary = rb_check_array_type(args);
    if (NIL_P(ary))
        rb_raise(rb_eArgError, "not an array");
    return rb_yield_values2((int)RARRAY_LEN(ary), RARRAY_CONST_PTR(ary));
}

int
rb_block_given_p(void)
{
    return frame()->block != MR_UNDEF;
}

void
rb_need_block(void)
{
    if (!rb_block_given_p())
        rb_raise(rb_eLocalJumpError, "no block given");
}

VALUE
rb_block_proc(void)
{
    struct capi_frame *f = frame();
    if (f->block == MR_UNDEF)
        return Qnil;
    return check(api->block_to_proc(f->ctx, f->block));
}

VALUE
rb_block_lambda(void)
{
    return rb_block_proc();
}

VALUE
rb_proc_call(VALUE proc, VALUE args)
{
    return rb_proc_call_with_block(proc, (int)RARRAY_LEN(args), RARRAY_CONST_PTR(args), Qnil);
}

VALUE
rb_proc_call_with_block(VALUE proc, int argc, const VALUE *argv, VALUE passed_proc)
{
    if (!NIL_P(passed_proc))
        return rb_funcall_with_block(proc, rb_intern("call"), argc, argv, passed_proc);
    struct capi_frame *saved = frame();
    MrValue small[CAPI_SMALL_ARGS];
    MrValue r = api->proc_call(saved->ctx, M(proc), argc, mr_args(argc, argv, small));
    capi_cur = saved;
    return check(r);
}

VALUE
rb_obj_is_proc(VALUE obj)
{
    return rb_obj_is_kind_of(obj, rb_cProc);
}

VALUE
rb_obj_is_method(VALUE obj)
{
    return rb_obj_is_kind_of(obj, rb_cMethod);
}

/* ---------------------------------------------------------------------
 * Types
 * --------------------------------------------------------------------- */

int
rb_monoruby_heap_type(VALUE obj)
{
    switch (api->type_of(ctx(), M(obj))) {
    case MR_T_UNDEF: return T_UNDEF;
    case MR_T_NIL: return T_NIL;
    case MR_T_TRUE: return T_TRUE;
    case MR_T_FALSE: return T_FALSE;
    case MR_T_INTEGER: return RB_FIXNUM_P(obj) ? T_FIXNUM : T_BIGNUM;
    case MR_T_FLOAT: return T_FLOAT;
    case MR_T_SYMBOL: return T_SYMBOL;
    case MR_T_STRING: return T_STRING;
    case MR_T_ARRAY: return T_ARRAY;
    case MR_T_HASH: return T_HASH;
    case MR_T_CLASS: return T_CLASS;
    case MR_T_MODULE: return T_MODULE;
    case MR_T_PROC: return T_DATA;
    case MR_T_NATIVE: return T_DATA;
    case MR_T_EXCEPTION: return T_OBJECT;
    case MR_T_OBJECT:
        break;
    }
    static const struct {
        VALUE *klass;
        int type;
    } kinds[] = {
        {&rb_cRegexp, T_REGEXP}, {&rb_cStruct, T_STRUCT}, {&rb_cRange, T_STRUCT},
        {&rb_cMatch, T_MATCH}, {&rb_cRational, T_RATIONAL},
        {&rb_cComplex, T_COMPLEX}, {&rb_cIO, T_FILE},
    };
    for (size_t i = 0; i < sizeof(kinds) / sizeof(kinds[0]); i++) {
        VALUE k = *kinds[i].klass;
        if (!NIL_P(k) && api->is_kind_of(ctx(), M(obj), M(k)))
            return kinds[i].type;
    }
    return T_OBJECT;
}

static const char *
builtin_type_name(int t)
{
    switch (t) {
    case T_NIL: return "nil";
    case T_TRUE: return "true";
    case T_FALSE: return "false";
    case T_FIXNUM: return "Integer";
    case T_BIGNUM: return "Integer";
    case T_FLOAT: return "Float";
    case T_STRING: return "String";
    case T_SYMBOL: return "Symbol";
    case T_ARRAY: return "Array";
    case T_HASH: return "Hash";
    case T_CLASS: return "Class";
    case T_MODULE: return "Module";
    case T_REGEXP: return "Regexp";
    case T_STRUCT: return "Struct";
    case T_DATA: return "Data";
    case T_FILE: return "File";
    case T_MATCH: return "MatchData";
    case T_RATIONAL: return "Rational";
    case T_COMPLEX: return "Complex";
    case T_OBJECT: return "Object";
    }
    return NULL;
}

void
rb_check_type(VALUE v, int t)
{
    int actual = TYPE(v);
    if (actual == t)
        return;
    if (t == T_FIXNUM && actual == T_BIGNUM)
        return;
    const char *expected = builtin_type_name(t);
    const char *got;
    if (NIL_P(v)) got = "nil";
    else if (v == Qtrue) got = "true";
    else if (v == Qfalse) got = "false";
    else got = rb_obj_classname(v);
    if (expected)
        rb_raise(rb_eTypeError, "wrong argument type %s (expected %s)", got, expected);
    rb_raise(rb_eTypeError, "wrong argument type %s", got);
}

/* ---------------------------------------------------------------------
 * Memory and GC
 * --------------------------------------------------------------------- */

void *
ruby_xmalloc(size_t size)
{
    void *p = malloc(size ? size : 1);
    if (!p)
        rb_memerror();
    return p;
}

void *
ruby_xmalloc2(size_t n, size_t size)
{
    if (size && n > SIZE_MAX / size)
        rb_raise(rb_eArgError, "malloc: possible integer overflow");
    return ruby_xmalloc(n * size);
}

void *
ruby_xcalloc(size_t n, size_t size)
{
    void *p = calloc(n ? n : 1, size ? size : 1);
    if (!p)
        rb_memerror();
    return p;
}

void *
ruby_xrealloc(void *ptr, size_t size)
{
    void *p = realloc(ptr, size ? size : 1);
    if (!p)
        rb_memerror();
    return p;
}

void *
ruby_xrealloc2(void *ptr, size_t n, size_t size)
{
    if (size && n > SIZE_MAX / size)
        rb_raise(rb_eArgError, "realloc: possible integer overflow");
    return ruby_xrealloc(ptr, n * size);
}

void
ruby_xfree(void *ptr)
{
    free(ptr);
}

/*
 * The C globals registered with rb_gc_register_address hold VALUEs in
 * CRuby's layout, so the interpreter cannot read them itself: the layer
 * keeps the list and reports them, converted, from the `mark` of one
 * pinned native object.
 */
static VALUE **capi_roots;
static size_t capi_nroots, capi_roots_cap;
static int capi_roots_made;

static void
roots_mark(void *p, MrMarker *marker)
{
    (void)p;
    for (size_t i = 0; i < capi_nroots; i++) {
        VALUE v = *capi_roots[i];
        if (!RB_SPECIAL_CONST_P(v))
            api->gc_mark(marker, M(v));
    }
}

static const MrNativeOps roots_ops = { "roots", roots_mark, NULL, NULL };

void
rb_gc_register_address(VALUE *addr)
{
    if (!addr)
        return;
    if (!capi_roots_made) {
        VALUE klass = rb_class_new(rb_cObject);
        api->gc_pin(ctx(), M(klass));
        check_rc(api->class_set_alloc(ctx(), M(klass), MR_ALLOC_NATIVE));
        MrValue obj = api->native_new(ctx(), M(klass), &capi_roots, &roots_ops);
        if (obj == MR_UNDEF)
            capi_unwind();
        api->gc_pin(ctx(), obj);
        capi_roots_made = 1;
    }
    if (capi_nroots == capi_roots_cap) {
        size_t cap = capi_roots_cap ? capi_roots_cap * 2 : 16;
        VALUE **n = realloc(capi_roots, cap * sizeof(*n));
        if (!n)
            rb_memerror();
        capi_roots = n;
        capi_roots_cap = cap;
    }
    capi_roots[capi_nroots++] = addr;
}

void
rb_gc_unregister_address(VALUE *addr)
{
    for (size_t i = 0; i < capi_nroots; i++) {
        if (capi_roots[i] == addr) {
            capi_roots[i] = capi_roots[--capi_nroots];
            return;
        }
    }
}

void
rb_global_variable(VALUE *addr)
{
    rb_gc_register_address(addr);
}

void
rb_gc_register_mark_object(VALUE obj)
{
    if (!RB_SPECIAL_CONST_P(obj))
        api->gc_pin(ctx(), M(obj));
}

void
rb_gc_mark(VALUE obj)
{
    if (capi_marker && !RB_SPECIAL_CONST_P(obj))
        api->gc_mark(capi_marker, M(obj));
}

void
rb_gc_mark_maybe(VALUE obj)
{
    rb_gc_mark(obj);
}

void
rb_gc_mark_movable(VALUE obj)
{
    rb_gc_mark(obj);
}

VALUE
rb_gc_location(VALUE obj)
{
    return obj;
}

void
rb_gc(void)
{
    rb_gc_start();
}

VALUE
rb_gc_start(void)
{
    return rb_funcall(rb_mGC, rb_intern("start"), 0);
}

VALUE
rb_gc_enable(void)
{
    return rb_funcall(rb_mGC, rb_intern("enable"), 0);
}

VALUE
rb_gc_disable(void)
{
    return rb_funcall(rb_mGC, rb_intern("disable"), 0);
}

void
rb_gc_writebarrier(VALUE a, VALUE b)
{
    (void)a;
    (void)b;
}

void
rb_gc_writebarrier_unprotect(VALUE obj)
{
    (void)obj;
}

int
rb_during_gc(void)
{
    return 0;
}

/* ---------------------------------------------------------------------
 * Symbols and IDs
 * --------------------------------------------------------------------- */

ID
rb_intern2(const char *name, long len)
{
    MrSym s = api->intern(ctx(), (const uint8_t *)name, (size_t)len);
    if (s == MR_NO_SYM)
        rb_raise(rb_eEncodingError, "invalid symbol name (not UTF-8): %.*s", (int)len, name);
    return (ID)s;
}

ID
(rb_intern)(const char *name)
{
    return rb_intern2(name, (long)strlen(name));
}

ID
rb_intern_str(VALUE str)
{
    return rb_intern2(RSTRING_PTR(str), RSTRING_LEN(str));
}

/* Names live as long as the process, as CRuby's symbol table does. */
static struct {
    ID id;
    char *name;
} *id_names;
static size_t id_names_cap, id_names_len;

const char *
rb_id2name(ID id)
{
    if (id == 0)
        return NULL;
    if (id_names_cap) {
        for (size_t i = (size_t)(id * 2654435761u) & (id_names_cap - 1);; i = (i + 1) & (id_names_cap - 1)) {
            if (id_names[i].id == id)
                return id_names[i].name;
            if (id_names[i].id == 0)
                break;
        }
    }
    VALUE s = rb_sym2str(ID2SYM(id));
    long len = RSTRING_LEN(s);
    char *name = malloc((size_t)len + 1);
    if (!name)
        rb_memerror();
    memcpy(name, RSTRING_PTR(s), (size_t)len);
    name[len] = 0;
    if ((id_names_len + 1) * 2 > id_names_cap) {
        size_t cap = id_names_cap ? id_names_cap * 2 : 64;
        void *table = calloc(cap, sizeof(*id_names));
        if (!table)
            rb_memerror();
        __typeof__(id_names) t = table;
        for (size_t i = 0; i < id_names_cap; i++) {
            if (!id_names[i].id)
                continue;
            size_t j = (size_t)(id_names[i].id * 2654435761u) & (cap - 1);
            while (t[j].id)
                j = (j + 1) & (cap - 1);
            t[j] = id_names[i];
        }
        free(id_names);
        id_names = t;
        id_names_cap = cap;
    }
    size_t i = (size_t)(id * 2654435761u) & (id_names_cap - 1);
    while (id_names[i].id)
        i = (i + 1) & (id_names_cap - 1);
    id_names[i].id = id;
    id_names[i].name = name;
    id_names_len++;
    return name;
}

VALUE
rb_id2sym(ID id)
{
    return ID2SYM(id);
}

ID
rb_sym2id(VALUE sym)
{
    if (!SYMBOL_P(sym))
        rb_raise(rb_eTypeError, "wrong argument type %s (expected Symbol)", rb_obj_classname(sym));
    return SYM2ID(sym);
}

VALUE
rb_sym2str(VALUE sym)
{
    return check(api->sym_to_str(ctx(), M(sym)));
}

VALUE
rb_id2str(ID id)
{
    /* CRuby answers 0 (= Qfalse) for the null ID, as `rb_id2name` answers NULL. */
    if (id == 0)
        return Qfalse;
    return rb_sym2str(ID2SYM(id));
}

VALUE
rb_sym_to_s(VALUE sym)
{
    return rb_sym2str(sym);
}

ID
rb_to_id(VALUE name)
{
    if (SYMBOL_P(name))
        return SYM2ID(name);
    if (RB_TYPE_P(name, T_STRING))
        return rb_intern_str(name);
    VALUE s = rb_check_string_type(name);
    if (NIL_P(s))
        rb_raise(rb_eTypeError, "%+" PRIsVALUE " is not a symbol nor a string", name);
    return rb_intern_str(s);
}

VALUE
rb_to_symbol(VALUE name)
{
    return ID2SYM(rb_to_id(name));
}

ID
rb_check_id(volatile VALUE *namep)
{
    VALUE name = *namep;
    if (SYMBOL_P(name))
        return SYM2ID(name);
    if (RB_TYPE_P(name, T_STRING))
        return rb_intern_str(name);
    return 0;
}

VALUE
rb_check_symbol(volatile VALUE *namep)
{
    ID id = rb_check_id(namep);
    return id ? ID2SYM(id) : Qnil;
}

/* The ID of a name that is already a symbol, 0 if it is not one yet: the
   name is not interned (CRuby's rb_check_id_cstr). Without the ABI entry
   every name counts as new. */
ID
rb_check_id_cstr(const char *ptr, long len, rb_encoding *enc)
{
    (void)enc;
    if (!HAVE_API(intern_check))
        return 0;
    MrSym s = api->intern_check(ctx(), (const uint8_t *)ptr, (size_t)len);
    return s == MR_NO_SYM ? 0 : (ID)s;
}

VALUE
rb_check_symbol_cstr(const char *ptr, long len, rb_encoding *enc)
{
    ID id = rb_check_id_cstr(ptr, len, enc);
    return id ? ID2SYM(id) : Qnil;
}

/* ---------------------------------------------------------------------
 * Classes and methods
 * --------------------------------------------------------------------- */

VALUE
rb_define_class_under(VALUE outer, const char *name, VALUE super)
{
    return check(api->define_class(ctx(), M(outer), name, M(super), 0));
}

VALUE
rb_define_class(const char *name, VALUE super)
{
    return check(api->define_class(ctx(), MR_UNDEF, name, M(super), 0));
}

VALUE
rb_define_module_under(VALUE outer, const char *name)
{
    return check(api->define_module(ctx(), M(outer), name));
}

VALUE
rb_define_module(const char *name)
{
    return check(api->define_module(ctx(), MR_UNDEF, name));
}

static int
is_module(VALUE v)
{
    int t = TYPE(v);
    return t == T_CLASS || t == T_MODULE;
}

/* The C functions defined as methods so far, keyed by (function, class,
 * arity): the visibility and name of their first definition
 * (`define_cfunc`). Names live as long as the methods do; the table is
 * per load (`monoruby_capi_init` empties it). */
static struct {
    VALUE (*func)(ANYARGS);
    VALUE klass;
    int arity;
    uint32_t flags;
    ID mid;
} *func_names;
static size_t func_names_cap, func_names_len;

static size_t
func_names_slot(VALUE (*func)(ANYARGS), VALUE klass, int arity)
{
    size_t i = (((uintptr_t)func >> 4) ^ ((uintptr_t)klass >> 3) ^ (size_t)arity) & (func_names_cap - 1);
    while (func_names[i].func &&
           !(func_names[i].func == func && func_names[i].klass == klass && func_names[i].arity == arity))
        i = (i + 1) & (func_names_cap - 1);
    return i;
}

/* The first definition of `func` on `klass` with this arity, if there
 * was one; `*flags` gets its visibility. Otherwise this definition is
 * recorded (with `mid` 0 until `record_func_name`) and 0 returned. */
static ID
defined_func_name(VALUE klass, VALUE (*func)(ANYARGS), int arity, uint32_t *flags)
{
    if (!func_names_cap || (func_names_len + 1) * 2 > func_names_cap) {
        size_t cap = func_names_cap ? func_names_cap * 2 : 256;
        void *table = calloc(cap, sizeof(*func_names));
        if (!table)
            rb_memerror();
        __typeof__(func_names) old = func_names;
        size_t old_cap = func_names_cap;
        func_names = table;
        func_names_cap = cap;
        for (size_t i = 0; i < old_cap; i++)
            if (old[i].func)
                func_names[func_names_slot(old[i].func, old[i].klass, old[i].arity)] = old[i];
        free(old);
    }
    size_t i = func_names_slot(func, klass, arity);
    if (func_names[i].func) {
        if (func_names[i].mid)
            *flags = func_names[i].flags;
        return func_names[i].mid;
    }
    func_names[i].func = func;
    func_names[i].klass = klass;
    func_names[i].arity = arity;
    func_names[i].flags = *flags;
    func_names[i].mid = 0;
    func_names_len++;
    return 0;
}

/* The table is per load (`monoruby_capi_init`): a function defined in
 * another extension's Init_ is another function, and a class VALUE from
 * before this load may be stale. */
static void
func_names_reset(void)
{
    free(func_names);
    func_names = NULL;
    func_names_cap = func_names_len = 0;
}

static void
record_func_name(VALUE klass, VALUE (*func)(ANYARGS), int arity, uint32_t flags, ID mid)
{
    size_t i = func_names_slot(func, klass, arity);
    if (func_names[i].func && !func_names[i].mid) {
        func_names[i].flags = flags;
        func_names[i].mid = mid;
    }
}

static void
define_cfunc(VALUE klass, const char *name, VALUE (*func)(ANYARGS), int argc, uint32_t flags)
{
    if (argc < -2 || argc > 15)
        rb_raise(rb_eArgError, "arity out of range: %d for -2..15", argc);
    /* The same function defined again on the same class with the same
     * arity becomes an alias of the first definition, with the visibility
     * asked for now: in CRuby both names denote one method
     * (`Date#mday == Date#day`), and an alias is how monoruby expresses
     * that. */
    ID mid = rb_intern(name);
    uint32_t first_flags = flags;
    ID first = defined_func_name(klass, func, argc, &first_flags);
    if (first && first != mid) {
        VALUE owner = (flags & MR_METHOD_SINGLETON) ? rb_singleton_class(klass) : klass;
        rb_funcall(owner, rb_intern("alias_method"), 2, ID2SYM(mid), ID2SYM(first));
        if ((flags ^ first_flags) & MR_METHOD_PRIVATE)
            rb_funcall(owner, rb_intern((flags & MR_METHOD_PRIVATE) ? "private" : "public"), 1, ID2SYM(mid));
        return;
    }
    struct capi_method *m = malloc(sizeof(*m));
    if (!m)
        rb_memerror();
    m->func = func;
    m->arity = argc;
    m->mid = mid;
    /* `rb_add_method` makes these private whatever the caller asked. */
    if (!(flags & MR_METHOD_SINGLETON) &&
        (strcmp(name, "initialize") == 0 || strcmp(name, "initialize_copy") == 0 ||
         strcmp(name, "initialize_clone") == 0 || strcmp(name, "initialize_dup") == 0 ||
         strcmp(name, "respond_to_missing?") == 0))
        flags |= MR_METHOD_PRIVATE;
    api->define_method_data(ctx(), M(klass), name, capi_dispatch, m,
                            argc < 0 ? MR_ARGC_VARIADIC : argc, flags);
    if (api->error_pending(ctx())) {
        /* Not defined, so nothing refers to it. */
        free(m);
        capi_unwind();
    }
    record_func_name(klass, func, argc, flags, mid);
}

void
rb_define_method(VALUE klass, const char *name, VALUE (*func)(ANYARGS), int argc)
{
    define_cfunc(klass, name, func, argc, 0);
}

void
rb_define_method_id(VALUE klass, ID mid, VALUE (*func)(ANYARGS), int argc)
{
    define_cfunc(klass, rb_id2name(mid), func, argc, 0);
}

void
rb_define_private_method(VALUE klass, const char *name, VALUE (*func)(ANYARGS), int argc)
{
    define_cfunc(klass, name, func, argc, MR_METHOD_PRIVATE);
}

void
rb_define_protected_method(VALUE klass, const char *name, VALUE (*func)(ANYARGS), int argc)
{
    define_cfunc(klass, name, func, argc, 0);
    rb_funcall(klass, rb_intern("protected"), 1, ID2SYM(rb_intern(name)));
}

void
rb_define_singleton_method(VALUE obj, const char *name, VALUE (*func)(ANYARGS), int argc)
{
    if (is_module(obj))
        define_cfunc(obj, name, func, argc, MR_METHOD_SINGLETON);
    else
        define_cfunc(rb_singleton_class(obj), name, func, argc, 0);
}

void
rb_define_module_function(VALUE mod, const char *name, VALUE (*func)(ANYARGS), int argc)
{
    define_cfunc(mod, name, func, argc, MR_METHOD_PRIVATE);
    define_cfunc(mod, name, func, argc, MR_METHOD_SINGLETON);
}

void
rb_define_global_function(const char *name, VALUE (*func)(ANYARGS), int argc)
{
    rb_define_module_function(rb_mKernel, name, func, argc);
}

void
rb_define_alias(VALUE klass, const char *name1, const char *name2)
{
    rb_funcall(klass, rb_intern("alias_method"), 2, ID2SYM(rb_intern(name1)), ID2SYM(rb_intern(name2)));
}

void
rb_alias(VALUE klass, ID name1, ID name2)
{
    rb_funcall(klass, rb_intern("alias_method"), 2, ID2SYM(name1), ID2SYM(name2));
}

void
rb_undef_method(VALUE klass, const char *name)
{
    rb_funcall(klass, rb_intern("undef_method"), 1, ID2SYM(rb_intern(name)));
}

void
rb_undef(VALUE klass, ID id)
{
    rb_funcall(klass, rb_intern("undef_method"), 1, ID2SYM(id));
}

void
rb_define_attr(VALUE klass, const char *name, int read, int write)
{
    /* `attr_*` called from C would take the visibility in effect where
     * the extension was required (private at the top level); CRuby's
     * rb_define_attr always defines public accessors. */
    VALUE sym = ID2SYM(rb_intern(name));
    if (read) {
        rb_funcall(klass, rb_intern("attr_reader"), 1, sym);
        rb_funcall(klass, rb_intern("public"), 1, sym);
    }
    if (write) {
        VALUE setter = rb_str_cat_cstr(rb_str_new_cstr(name), "=");
        rb_funcall(klass, rb_intern("attr_writer"), 1, sym);
        rb_funcall(klass, rb_intern("public"), 1, rb_str_intern(setter));
    }
}

void
rb_define_const(VALUE klass, const char *name, VALUE val)
{
    api->const_set(ctx(), M(klass), name, M(val));
    if (api->error_pending(ctx()))
        capi_unwind();
}

void
rb_define_global_const(const char *name, VALUE val)
{
    rb_define_const(rb_cObject, name, val);
}

VALUE
rb_const_get(VALUE klass, ID id)
{
    return rb_funcall(klass, rb_intern("const_get"), 1, ID2SYM(id));
}

VALUE
rb_const_get_at(VALUE klass, ID id)
{
    return rb_funcall(klass, rb_intern("const_get"), 2, ID2SYM(id), Qfalse);
}

int
rb_const_defined(VALUE klass, ID id)
{
    return RTEST(rb_funcall(klass, rb_intern("const_defined?"), 1, ID2SYM(id)));
}

int
rb_const_defined_at(VALUE klass, ID id)
{
    return RTEST(rb_funcall(klass, rb_intern("const_defined?"), 2, ID2SYM(id), Qfalse));
}

void
rb_const_set(VALUE klass, ID id, VALUE val)
{
    rb_define_const(klass, rb_id2name(id), val);
}

VALUE
rb_path2class(const char *path)
{
    return rb_funcall(rb_cObject, rb_intern("const_get"), 1, rb_str_new_cstr(path));
}

VALUE
rb_path_to_class(VALUE path)
{
    return rb_funcall(rb_cObject, rb_intern("const_get"), 1, path);
}

void
rb_include_module(VALUE klass, VALUE module)
{
    rb_funcall(klass, rb_intern("include"), 1, module);
}

void
rb_extend_object(VALUE obj, VALUE module)
{
    rb_funcall(obj, rb_intern("extend"), 1, module);
}

void
rb_prepend_module(VALUE klass, VALUE module)
{
    rb_funcall(klass, rb_intern("prepend"), 1, module);
}

VALUE
rb_class_new_instance(int argc, const VALUE *argv, VALUE klass)
{
    /* allocate + initialize, as CRuby's does: not `klass.new`, which an
     * extension's own `new` (StringIO.new) calls this to implement. The
     * calling method's block goes on to initialize. */
    VALUE obj = rb_obj_alloc(klass);
    rb_obj_call_init(obj, argc, argv);
    return obj;
}

VALUE
rb_obj_alloc(VALUE klass)
{
    return rb_funcall(klass, rb_intern("allocate"), 0);
}

VALUE
rb_mod_name(VALUE mod)
{
    return rb_funcall(mod, rb_intern("name"), 0);
}

VALUE
rb_class_name(VALUE klass)
{
    VALUE name = rb_mod_name(klass);
    return NIL_P(name) ? rb_obj_as_string(klass) : name;
}

VALUE
rb_class_path(VALUE klass)
{
    return rb_class_name(klass);
}

const char *
rb_class2name(VALUE klass)
{
    VALUE name = rb_class_name(klass);
    long len = RSTRING_LEN(name);
    char *buf = frame_buf((size_t)len + 1);
    memcpy(buf, RSTRING_PTR(name), (size_t)len);
    buf[len] = 0;
    return buf;
}

const char *
rb_obj_classname(VALUE obj)
{
    return rb_class2name(rb_obj_class(obj));
}

VALUE
rb_class_superclass(VALUE klass)
{
    return rb_funcall(klass, rb_intern("superclass"), 0);
}

VALUE
rb_class_real(VALUE klass)
{
    return klass;
}

VALUE
rb_singleton_class(VALUE obj)
{
    return rb_funcall(obj, rb_intern("singleton_class"), 0);
}

VALUE
rb_class_of(VALUE obj)
{
    return check(api->class_of(ctx(), M(obj)));
}

VALUE
rb_obj_class(VALUE obj)
{
    /* CRuby's rb_class_real(CLASS_OF(obj)): a class's own class is
       Class, not its metaclass (`no implicit conversion of Class`). */
    if (HAVE_API(obj_class))
        return check(api->obj_class(ctx(), M(obj)));
    return rb_class_of(obj);
}

VALUE
rb_obj_is_kind_of(VALUE obj, VALUE klass)
{
    if (!is_module(klass))
        rb_raise(rb_eTypeError, "class or module required");
    return api->is_kind_of(ctx(), M(obj), M(klass)) ? Qtrue : Qfalse;
}

VALUE
rb_obj_is_instance_of(VALUE obj, VALUE klass)
{
    return rb_obj_class(obj) == klass ? Qtrue : Qfalse;
}

VALUE
rb_class_inherited_p(VALUE mod, VALUE arg)
{
    return rb_funcall(mod, rb_intern("<="), 1, arg);
}

int
rb_respond_to(VALUE obj, ID id)
{
    return RTEST(rb_funcall(obj, rb_intern("respond_to?"), 1, ID2SYM(id)));
}

int
rb_obj_respond_to(VALUE obj, ID id, int priv)
{
    return RTEST(rb_funcall(obj, rb_intern("respond_to?"), 2, ID2SYM(id), priv ? Qtrue : Qfalse));
}

VALUE
rb_check_funcall(VALUE recv, ID mid, int argc, const VALUE *argv)
{
    if (!rb_obj_respond_to(recv, mid, 1))
        return Qundef;
    return rb_funcallv(recv, mid, argc, argv);
}

/* ---------------------------------------------------------------------
 * Objects
 * --------------------------------------------------------------------- */

/* `Object#name` as an UnboundMethod, kept for the process. */
static VALUE
object_method(const char *name, VALUE *cache)
{
    if (!*cache) {
        *cache = rb_funcall(rb_cObject, rb_intern("instance_method"), 1, ID2SYM(rb_intern(name)));
        rb_gc_register_mark_object(*cache);
    }
    return *cache;
}

/*
 * The primitives, not the methods: an extension that defines its own
 * `freeze` (msgpack's Factory) calls rb_obj_freeze from it, as CRuby's
 * sets the flag directly.
 */
VALUE
rb_obj_freeze(VALUE obj)
{
    if (HAVE_API(obj_freeze)) {
        api->obj_freeze(ctx(), M(obj));
        return obj;
    }
    static VALUE um;
    rb_funcall(object_method("freeze", &um), rb_intern("bind_call"), 1, obj);
    return obj;
}

VALUE
rb_obj_frozen_p(VALUE obj)
{
    if (api->size >= offsetof(MrApi, obj_frozen) + sizeof(api->obj_frozen))
        return api->obj_frozen(ctx(), M(obj)) ? Qtrue : Qfalse;
    static VALUE um;
    return rb_funcall(object_method("frozen?", &um), rb_intern("bind_call"), 1, obj);
}

void
rb_check_frozen(VALUE obj)
{
    if (OBJ_FROZEN(obj)) {
        VALUE exc = rb_exc_new_str(rb_eFrozenError,
                                   rb_sprintf("can't modify frozen %" PRIsVALUE ": %+" PRIsVALUE,
                                              rb_obj_class(obj), obj));
        rb_exc_raise(exc);
    }
}

/* OBJ_INIT_COPY's callee: the checks `Object#initialize_copy` makes
 * before an extension copies its own state. */
VALUE
rb_obj_init_copy(VALUE obj, VALUE orig)
{
    if (obj == orig)
        return obj;
    rb_check_frozen(obj);
    if (rb_obj_class(obj) != rb_obj_class(orig))
        rb_raise(rb_eTypeError, "initialize_copy should take same class object");
    return obj;
}

VALUE
rb_obj_dup(VALUE obj)
{
    return rb_funcall(obj, rb_intern("dup"), 0);
}

VALUE
rb_obj_clone(VALUE obj)
{
    return rb_funcall(obj, rb_intern("clone"), 0);
}

VALUE
rb_obj_id(VALUE obj)
{
    return rb_funcall(obj, rb_intern("object_id"), 0);
}

VALUE
rb_obj_hide(VALUE obj)
{
    return obj;
}

VALUE
rb_obj_reveal(VALUE obj, VALUE klass)
{
    (void)klass;
    return obj;
}

VALUE
rb_equal(VALUE a, VALUE b)
{
    if (a == b)
        return Qtrue;
    return RTEST(rb_funcall(a, rb_intern("=="), 1, b)) ? Qtrue : Qfalse;
}

int
rb_eql(VALUE a, VALUE b)
{
    if (a == b)
        return 1;
    return RTEST(rb_funcall(a, rb_intern("eql?"), 1, b));
}

VALUE
rb_hash(VALUE obj)
{
    return rb_funcall(obj, rb_intern("hash"), 0);
}

VALUE
rb_inspect(VALUE obj)
{
    return rb_obj_as_string(rb_funcall(obj, rb_intern("inspect"), 0));
}

VALUE
rb_obj_as_string(VALUE obj)
{
    if (RB_TYPE_P(obj, T_STRING))
        return obj;
    VALUE s = rb_funcall(obj, rb_intern("to_s"), 0);
    if (!RB_TYPE_P(s, T_STRING))
        return rb_any_to_s(obj);
    return s;
}

VALUE
rb_any_to_s(VALUE obj)
{
    /* Kernel#to_s's own "#<Class:0x...>" for the real class, as object.c's
     * rb_any_to_s builds it. */
    static VALUE um;
    return rb_funcall(object_method("to_s", &um), rb_intern("bind_call"), 1, obj);
}

void
rb_p(VALUE obj)
{
    rb_funcall(rb_mKernel, rb_intern("p"), 1, obj);
}

VALUE
rb_eval_string(const char *str)
{
    return rb_funcall(rb_mKernel, rb_intern("eval"), 1, rb_str_new_cstr(str));
}

static VALUE
eval_string_body(VALUE str)
{
    return rb_funcall(rb_mKernel, rb_intern("eval"), 1, str);
}

VALUE
rb_eval_string_protect(const char *str, int *state)
{
    return rb_protect(eval_string_body, rb_str_new_cstr(str), state);
}

VALUE
rb_require(const char *feature)
{
    return rb_funcall(rb_mKernel, rb_intern("require"), 1, rb_str_new_cstr(feature));
}

VALUE
rb_f_require(VALUE self, VALUE feature)
{
    (void)self;
    return rb_funcall(rb_mKernel, rb_intern("require"), 1, feature);
}

VALUE
rb_obj_instance_variables(VALUE obj)
{
    return rb_funcall(obj, rb_intern("instance_variables"), 0);
}

/* The instance variables #instance_variables lists: the hidden ones
 * (ivar_name below) are not among them. */
void
rb_ivar_foreach(VALUE obj, int (*func)(ID, VALUE, st_data_t), st_data_t arg)
{
    VALUE names = rb_obj_instance_variables(obj);
    long n = RARRAY_LEN(names);
    for (long i = 0; i < n; i++) {
        ID id = SYM2ID(rb_ary_entry(names, i));
        if (func(id, rb_ivar_get(obj, id), arg) == ST_STOP)
            break;
    }
}

/* The block form takes the block of the running C method, as CRuby's
 * does. */
VALUE
rb_obj_instance_eval(int argc, const VALUE *argv, VALUE self)
{
    return funcall_block(self, rb_intern("instance_eval"), argc, argv,
                         argc == 0 ? frame()->block : MR_UNDEF);
}

VALUE
rb_obj_instance_exec(int argc, const VALUE *argv, VALUE self)
{
    return funcall_block(self, rb_intern("instance_exec"), argc, argv, frame()->block);
}

VALUE
rb_mod_module_eval(int argc, const VALUE *argv, VALUE mod)
{
    return funcall_block(mod, rb_intern("module_eval"), argc, argv,
                         argc == 0 ? frame()->block : MR_UNDEF);
}

VALUE
rb_mod_module_exec(int argc, const VALUE *argv, VALUE mod)
{
    return funcall_block(mod, rb_intern("module_exec"), argc, argv, frame()->block);
}

/* A name without '@' is CRuby's hidden instance variable (sqlite3's
 * "-aggregators"): monoruby keeps those under a '/' prefix, which
 * #instance_variables, Marshal and inspect leave out. */
static const char *
ivar_name(const char *name, char buf[256])
{
    if (name[0] == '@' || name[0] == '/')
        return name;
    size_t len = strlen(name);
    if (len > 254)
        rb_raise(rb_eNameError, "hidden instance variable name too long");
    buf[0] = '/';
    memcpy(buf + 1, name, len + 1);
    return buf;
}

VALUE
rb_iv_get(VALUE obj, const char *name)
{
    char buf[256];
    return check(api->ivar_get(ctx(), M(obj), ivar_name(name, buf)));
}

VALUE
rb_iv_set(VALUE obj, const char *name, VALUE val)
{
    char buf[256];
    check_rc(api->ivar_set(ctx(), M(obj), ivar_name(name, buf), M(val)));
    return val;
}

VALUE
rb_ivar_get(VALUE obj, ID id)
{
    return rb_iv_get(obj, rb_id2name(id));
}

VALUE
rb_ivar_set(VALUE obj, ID id, VALUE val)
{
    return rb_iv_set(obj, rb_id2name(id), val);
}

VALUE
rb_ivar_defined(VALUE obj, ID id)
{
    /* instance_variable_defined? rejects a hidden name; the table has no
     * "defined" query, so a hidden one set to nil reads as undefined. */
    const char *name = rb_id2name(id);
    if (name[0] != '@')
        return NIL_P(rb_iv_get(obj, name)) ? Qfalse : Qtrue;
    return rb_funcall(obj, rb_intern("instance_variable_defined?"), 1, ID2SYM(id));
}

VALUE
rb_attr_get(VALUE obj, ID id)
{
    return rb_ivar_get(obj, id);
}

VALUE
rb_cvar_get(VALUE klass, ID id)
{
    return rb_funcall(klass, rb_intern("class_variable_get"), 1, ID2SYM(id));
}

void
rb_cvar_set(VALUE klass, ID id, VALUE val)
{
    rb_funcall(klass, rb_intern("class_variable_set"), 2, ID2SYM(id), val);
}

VALUE
rb_cv_get(VALUE klass, const char *name)
{
    return rb_cvar_get(klass, rb_intern(name));
}

void
rb_cv_set(VALUE klass, const char *name, VALUE val)
{
    rb_cvar_set(klass, rb_intern(name), val);
}

static int
valid_gvar_name(const char *name)
{
    if (name[0] != '$' || !name[1])
        return 0;
    for (const char *p = name + 1; *p; p++) {
        if (!(*p == '_' || (*p >= 'a' && *p <= 'z') || (*p >= 'A' && *p <= 'Z') ||
              (*p >= '0' && *p <= '9') || (unsigned char)*p >= 0x80))
            return 0;
    }
    return 1;
}

/* `$name`, with or without the `$`, in a buffer of `size` bytes. */
static const char *
gvar_name(const char *name, char *buf, size_t size)
{
    snprintf(buf, size, "%s%s", name[0] == '$' ? "" : "$", name);
    if (!valid_gvar_name(buf))
        rb_raise(rb_eNameError, "'%s' is not allowed as a global variable name", buf);
    return buf;
}

VALUE
rb_gv_get(const char *name)
{
    char buf[256];
    if (api->size >= offsetof(MrApi, gvar_get) + sizeof(api->gvar_get)) {
        snprintf(buf, sizeof(buf), "%s%s", name[0] == '$' ? "" : "$", name);
        return R(api->gvar_get(ctx(), buf));
    }
    return rb_eval_string(gvar_name(name, buf, sizeof(buf)));
}

VALUE
rb_gv_set(const char *name, VALUE val)
{
    if (api->size >= offsetof(MrApi, gvar_set) + sizeof(api->gvar_set)) {
        char buf[256];
        snprintf(buf, sizeof(buf), "%s%s", name[0] == '$' ? "" : "$", name);
        check_rc(api->gvar_set(ctx(), buf, M(val)));
        return val;
    }
    char gv[256], code[300];
    snprintf(code, sizeof(code), "->(v) { %s = v }", gvar_name(name, gv, sizeof(gv)));
    return rb_funcall(rb_eval_string(code), rb_intern("call"), 1, val);
}

/* ---------------------------------------------------------------------
 * Hooked global variables (rb_define_variable and its relatives)
 * --------------------------------------------------------------------- */

/*
 * One per name handed to the interpreter's `gvar_hook`: the interpreter
 * calls the two dispatchers below with the record, and they call the
 * extension's getter and setter with CRuby's arguments. `var` is what
 * CRuby keeps as the entry's data: the extension's VALUE slot for
 * rb_define_variable / rb_define_hooked_variable, NULL for a virtual
 * variable, and the value itself (cast) once rb_gvar_undef_setter made
 * the variable a plain one. The marker keeps what `var` reaches alive
 * from one pinned native object.
 */
struct capi_gvar {
    ID id;
    VALUE *var;
    rb_gvar_getter_t *getter;
    rb_gvar_setter_t *setter;
    rb_gvar_marker_t *marker;
    struct capi_gvar *next;
};

static struct capi_gvar *capi_gvars;
static int capi_gvars_rooted;

static struct capi_gvar *
gvar_record(ID id)
{
    for (struct capi_gvar *g = capi_gvars; g; g = g->next)
        if (g->id == id)
            return g;
    return NULL;
}

static void
gvars_mark(void *p, MrMarker *marker)
{
    (void)p;
    MrMarker *saved = capi_marker;
    capi_marker = marker;
    for (struct capi_gvar *g = capi_gvars; g; g = g->next)
        g->marker(g->var);
    capi_marker = saved;
}

static const MrNativeOps gvars_ops = { "gvars", gvars_mark, NULL, NULL };

/* The hooks run under a frame of their own, as a method does: a raise
 * inside one lands here and is left pending for the interpreter. */
static MrValue
gvar_get_dispatch(MrContext *c, void *data)
{
    struct capi_gvar *g = data;
    struct capi_frame fr;
    struct capi_frame *volatile fp = &fr;
    volatile MrValue result = MR_UNDEF;
    frame_init(&fr, c, MR_UNDEF, NULL, NULL);
    capi_cur = &fr;
    if (_setjmp(fr.jb) == 0)
        result = M(g->getter(g->id, g->var));
    free_bufs(fp);
    capi_cur = fp->prev;
    return result;
}

static int
gvar_set_dispatch(MrContext *c, void *data, MrValue val)
{
    struct capi_gvar *g = data;
    struct capi_frame fr;
    struct capi_frame *volatile fp = &fr;
    volatile int rc = -1;
    frame_init(&fr, c, MR_UNDEF, NULL, NULL);
    capi_cur = &fr;
    if (_setjmp(fr.jb) == 0) {
        g->setter(R(val), g->id, g->var);
        rc = 0;
    }
    free_bufs(fp);
    capi_cur = fp->prev;
    return rc;
}

VALUE
rb_gvar_undef_getter(ID id, VALUE *data)
{
    (void)data;
    rb_warning("global variable '%s' not initialized", rb_id2name(id));
    return Qnil;
}

/* The first assignment to a virtual variable without a setter of its
 * own makes it a plain one holding the value, as in CRuby. */
void
rb_gvar_undef_setter(VALUE val, ID id, VALUE *data)
{
    (void)data;
    struct capi_gvar *g = gvar_record(id);
    if (!g)
        rb_raise(rb_eNameError, "%s is not a hooked global variable", rb_id2name(id));
    g->getter = rb_gvar_val_getter;
    g->setter = rb_gvar_val_setter;
    g->marker = rb_gvar_val_marker;
    g->var = (VALUE *)val;
}

void
rb_gvar_undef_marker(VALUE *var)
{
    (void)var;
}

/* A plain variable keeps its value in the data pointer itself. */
VALUE
rb_gvar_val_getter(ID id, VALUE *data)
{
    (void)id;
    return (VALUE)data;
}

void
rb_gvar_val_setter(VALUE val, ID id, VALUE *data)
{
    (void)data;
    struct capi_gvar *g = gvar_record(id);
    if (!g)
        rb_raise(rb_eNameError, "%s is not a hooked global variable", rb_id2name(id));
    g->var = (VALUE *)val;
}

void
rb_gvar_val_marker(VALUE *var)
{
    VALUE v = (VALUE)var;
    if (v)
        rb_gc_mark_maybe(v);
}

VALUE
rb_gvar_var_getter(ID id, VALUE *var)
{
    (void)id;
    return var ? *var : Qnil;
}

void
rb_gvar_var_setter(VALUE val, ID id, VALUE *var)
{
    (void)id;
    *var = val;
}

void
rb_gvar_var_marker(VALUE *var)
{
    if (var)
        rb_gc_mark_maybe(*var);
}

void
rb_gvar_readonly_setter(VALUE val, ID id, VALUE *var)
{
    (void)val;
    (void)var;
    rb_raise(rb_eNameError, "%s is a read-only variable", rb_id2name(id));
}

/*
 * The one registration behind the four `rb_define_*_variable`: as in
 * CRuby, a missing getter reads `*var`, a missing setter writes it. The
 * read-only setter is left to the interpreter, whose NameError is the
 * same.
 */
void
rb_define_hooked_variable(const char *name, VALUE *var, rb_gvar_getter_t *getter, rb_gvar_setter_t *setter)
{
    char buf[256];
    ID id = rb_intern(gvar_name(name, buf, sizeof(buf)));
    if (!capi_gvars_rooted) {
        VALUE klass = rb_class_new(rb_cObject);
        api->gc_pin(ctx(), M(klass));
        check_rc(api->class_set_alloc(ctx(), M(klass), MR_ALLOC_NATIVE));
        MrValue obj = api->native_new(ctx(), M(klass), &capi_gvars, &gvars_ops);
        if (obj == MR_UNDEF)
            capi_unwind();
        api->gc_pin(ctx(), obj);
        capi_gvars_rooted = 1;
    }
    struct capi_gvar *g = gvar_record(id);
    if (!g) {
        g = malloc(sizeof(*g));
        if (!g)
            rb_memerror();
        g->id = id;
        g->next = capi_gvars;
        capi_gvars = g;
    }
    g->var = var;
    g->getter = getter ? getter : rb_gvar_var_getter;
    g->setter = setter ? setter : rb_gvar_var_setter;
    g->marker = rb_gvar_var_marker;
    api->gvar_hook(ctx(), (MrSym)id, g, gvar_get_dispatch,
                   g->setter == rb_gvar_readonly_setter ? NULL : gvar_set_dispatch);
}

void
rb_define_virtual_variable(const char *name, rb_gvar_getter_t *getter, rb_gvar_setter_t *setter)
{
    rb_define_hooked_variable(name, NULL, getter ? getter : rb_gvar_val_getter,
                              setter ? setter : rb_gvar_readonly_setter);
}

void
rb_define_variable(const char *name, VALUE *var)
{
    rb_define_hooked_variable(name, var, NULL, NULL);
}

void
rb_define_readonly_variable(const char *name, const VALUE *var)
{
    rb_define_hooked_variable(name, (VALUE *)var, NULL, rb_gvar_readonly_setter);
}

VALUE
rb_Integer(VALUE v)
{
    return rb_funcall(rb_mKernel, rb_intern("Integer"), 1, v);
}

VALUE
rb_Float(VALUE v)
{
    return rb_funcall(rb_mKernel, rb_intern("Float"), 1, v);
}

VALUE
rb_String(VALUE v)
{
    return rb_funcall(rb_mKernel, rb_intern("String"), 1, v);
}

VALUE
rb_Complex(VALUE real, VALUE imag)
{
    return rb_funcall(rb_mKernel, rb_intern("Complex"), 2, real, imag);
}

VALUE
rb_complex_new(VALUE real, VALUE imag)
{
    return rb_Complex(real, imag);
}

VALUE
rb_Rational(VALUE num, VALUE den)
{
    return rb_funcall(rb_mKernel, rb_intern("Rational"), 2, num, den);
}

VALUE
rb_rational_new(VALUE num, VALUE den)
{
    return rb_Rational(num, den);
}

/* File.path(obj): `to_path` when the object responds to it, then a
 * String, as CRuby's FilePathValue coerces a path argument. */
VALUE
rb_get_path(VALUE obj)
{
    return rb_funcall(rb_cFile, rb_intern("path"), 1, obj);
}

VALUE
rb_rational_raw(VALUE num, VALUE den)
{
    return rb_Rational(num, den);
}

VALUE
rb_rational_num(VALUE rat)
{
    return rb_funcall(rat, rb_intern("numerator"), 0);
}

VALUE
rb_rational_den(VALUE rat)
{
    return rb_funcall(rat, rb_intern("denominator"), 0);
}

/* What CRuby names in a failed coercion / comparison: special constants
 * and Floats by value, everything else by class. */
static VALUE
coerce_desc(VALUE y)
{
    if (RB_SPECIAL_CONST_P(y) || RB_FLOAT_TYPE_P(y))
        return rb_inspect(y);
    return rb_obj_class(y);
}

/* `y.coerce(x)` into `*x`, `*y`; 0 (or TypeError, with `err`) when `y`
 * has no `coerce` or it does not answer a pair. */
static int
num_coerce(VALUE *x, VALUE *y, int err)
{
    VALUE ary = rb_check_funcall(*y, rb_intern("coerce"), 1, x);
    if (ary == Qundef) {
        if (err)
            rb_raise(rb_eTypeError, "%" PRIsVALUE " can't be coerced into %" PRIsVALUE,
                     coerce_desc(*y), rb_obj_class(*x));
        return 0;
    }
    if (!RB_TYPE_P(ary, T_ARRAY) || RARRAY_LEN(ary) != 2) {
        if (err)
            rb_raise(rb_eTypeError, "coerce must return [x, y]");
        return 0;
    }
    *x = rb_ary_entry(ary, 0);
    *y = rb_ary_entry(ary, 1);
    return 1;
}

VALUE
rb_num_coerce_bin(VALUE x, VALUE y, ID func)
{
    num_coerce(&x, &y, 1);
    return rb_funcall(x, func, 1, y);
}

VALUE
rb_num_coerce_cmp(VALUE x, VALUE y, ID func)
{
    if (!num_coerce(&x, &y, 0))
        return Qnil;
    return rb_funcall(x, func, 1, y);
}

VALUE
rb_num_coerce_relop(VALUE x, VALUE y, ID func)
{
    VALUE x0 = x, y0 = y, c;
    if (!num_coerce(&x, &y, 0) || NIL_P(c = rb_funcall(x, func, 1, y)))
        rb_raise(rb_eArgError, "comparison of %" PRIsVALUE " with %" PRIsVALUE " failed",
                 rb_obj_class(x0), coerce_desc(y0));
    return c;
}

VALUE
rb_cstr_to_inum(const char *str, int base, int badcheck)
{
    return rb_str_to_inum(rb_str_new_cstr(str), base, badcheck);
}

VALUE
rb_int_positive_pow(long x, unsigned long y)
{
    return rb_funcall(LONG2NUM(x), rb_intern("**"), 1, ULONG2NUM(y));
}

VALUE
rb_marshal_dump(VALUE obj, VALUE port)
{
    VALUE marshal = rb_const_get(rb_cObject, rb_intern("Marshal"));
    if (NIL_P(port))
        return rb_funcall(marshal, rb_intern("dump"), 1, obj);
    return rb_funcall(marshal, rb_intern("dump"), 2, obj, port);
}

VALUE
rb_marshal_load(VALUE port)
{
    return rb_funcall(rb_const_get(rb_cObject, rb_intern("Marshal")), rb_intern("load"), 1, port);
}


VALUE
rb_backref_get(void)
{
    if (HAVE_API(backref_get))
        return R(api->backref_get(ctx()));
    return rb_funcall(rb_cRegexp, rb_intern("last_match"), 0);
}

void
rb_backref_set(VALUE md)
{
    if (HAVE_API(backref_set))
        api->backref_set(ctx(), M(md));
}

/* CRuby copies the *generic* instance variables -- those of an object
 * with no ivar slots of its own (T_DATA, T_STRING, ...); a T_OBJECT's
 * are not generic and are left alone. */
void
rb_copy_generic_ivar(VALUE clone, VALUE obj)
{
    if (RB_SPECIAL_CONST_P(obj) || RB_TYPE_P(obj, T_OBJECT))
        return;
    VALUE names = rb_obj_instance_variables(obj);
    long i, n = RARRAY_LEN(names);
    for (i = 0; i < n; i++) {
        ID id = rb_to_id(rb_ary_entry(names, i));
        rb_ivar_set(clone, id, rb_ivar_get(obj, id));
    }
}

VALUE
rb_enumeratorize_with_size_kw(VALUE obj, VALUE meth, int argc, const VALUE *argv,
                              rb_enumerator_size_func *size_fn, int kw_splat)
{
    VALUE *args = ALLOCA_N(VALUE, argc + 1);
    (void)size_fn;
    args[0] = meth;
    if (argc > 0)
        memcpy(args + 1, argv, sizeof(VALUE) * argc);
    return rb_funcallv_kw(obj, rb_intern("to_enum"), argc + 1, args, kw_splat);
}

VALUE
rb_enumeratorize_with_size(VALUE obj, VALUE meth, int argc, const VALUE *argv,
                           rb_enumerator_size_func *size_fn)
{
    return rb_enumeratorize_with_size_kw(obj, meth, argc, argv, size_fn, 0);
}

VALUE
rb_enumeratorize(VALUE obj, VALUE meth, int argc, const VALUE *argv)
{
    return rb_enumeratorize_with_size_kw(obj, meth, argc, argv, NULL, 0);
}

ID
rb_frame_this_func(void)
{
    const struct capi_method *m = frame()->owner->method;
    return m ? m->mid : 0;
}

ID
rb_frame_callee(void)
{
    return rb_frame_this_func();
}

VALUE
rb_Array(VALUE v)
{
    return rb_funcall(rb_mKernel, rb_intern("Array"), 1, v);
}

VALUE
rb_Hash(VALUE v)
{
    return rb_funcall(rb_mKernel, rb_intern("Hash"), 1, v);
}

VALUE
rb_to_int(VALUE v)
{
    if (RB_INTEGER_TYPE_P(v))
        return v;
    int64_t i;
    if (!api->int_get(ctx(), M(v), &i))
        capi_unwind();
    return LONG2NUM(i);
}

VALUE
rb_to_float(VALUE v)
{
    if (RB_FLOAT_TYPE_P(v))
        return v;
    return rb_float_new(rb_num2dbl(v));
}

VALUE
rb_check_to_integer(VALUE v, const char *method)
{
    if (RB_INTEGER_TYPE_P(v))
        return v;
    if (!rb_respond_to(v, rb_intern(method)))
        return Qnil;
    VALUE r = rb_funcall(v, rb_intern(method), 0);
    return RB_INTEGER_TYPE_P(r) ? r : Qnil;
}

VALUE
rb_check_convert_type(VALUE v, int type, const char *tname, const char *method)
{
    if (TYPE(v) == type)
        return v;
    /* An immediate (an Integer port, a Symbol family, nil) without the
     * method is answered by method lookup alone: that is the common case
     * (the socket extension asks every argument for `to_str`), and a
     * `respond_to?` call costs a dispatch. */
    if (RB_SPECIAL_CONST_P(v) && !api->respond_to(ctx(), M(v), method))
        return Qnil;
    if (!rb_respond_to(v, rb_intern(method)))
        return Qnil;
    VALUE r = rb_funcall(v, rb_intern(method), 0);
    if (NIL_P(r))
        return Qnil;
    if (TYPE(r) != type)
        rb_raise(rb_eTypeError, "can't convert %s to %s (%s#%s gives %s)", rb_obj_classname(v),
                 tname, rb_obj_classname(v), method, rb_obj_classname(r));
    return r;
}

static const char *
implicit_name(VALUE v)
{
    if (NIL_P(v)) return "nil";
    if (v == Qtrue) return "true";
    if (v == Qfalse) return "false";
    return NULL;
}

VALUE
rb_convert_type(VALUE v, int type, const char *tname, const char *method)
{
    if (TYPE(v) == type)
        return v;
    VALUE r = rb_check_convert_type(v, type, tname, method);
    if (NIL_P(r)) {
        const char *special = implicit_name(v);
        if (special)
            rb_raise(rb_eTypeError, "no implicit conversion of %s into %s", special, tname);
        rb_raise(rb_eTypeError, "no implicit conversion of %s into %s", rb_obj_classname(v), tname);
    }
    return r;
}

int
rb_cmpint(VALUE val, VALUE a, VALUE b)
{
    if (NIL_P(val))
        rb_raise(rb_eArgError, "comparison of %" PRIsVALUE " with %" PRIsVALUE " failed",
                 rb_obj_class(a), rb_obj_class(b));
    if (FIXNUM_P(val)) {
        long l = FIX2LONG(val);
        return l > 0 ? 1 : l < 0 ? -1 : 0;
    }
    if (RTEST(rb_funcall(val, rb_intern(">"), 1, INT2FIX(0))))
        return 1;
    if (RTEST(rb_funcall(val, rb_intern("<"), 1, INT2FIX(0))))
        return -1;
    return 0;
}

/* ---------------------------------------------------------------------
 * Numbers
 * --------------------------------------------------------------------- */

VALUE
rb_int2inum(intptr_t n)
{
    if (RB_FIXABLE(n))
        return INT2FIX(n);
    return root(R(api->int_new((int64_t)n)));
}

VALUE
rb_int2big(intptr_t n)
{
    return rb_int2inum(n);
}

VALUE
rb_ll2inum(long long n)
{
    return rb_int2inum((intptr_t)n);
}

VALUE
rb_uint2inum(uintptr_t n)
{
    if (n <= (uintptr_t)INTPTR_MAX)
        return rb_int2inum((intptr_t)n);
    /* 2**63 and above: (n >> 1) * 2 + (n & 1). */
    VALUE half = rb_int2inum((intptr_t)(n >> 1));
    VALUE twice = rb_funcall(half, rb_intern("*"), 1, INT2FIX(2));
    return rb_funcall(twice, rb_intern("+"), 1, INT2FIX(n & 1));
}

VALUE
rb_uint2big(uintptr_t n)
{
    return rb_uint2inum(n);
}

VALUE
rb_ull2inum(unsigned long long n)
{
    return rb_uint2inum((uintptr_t)n);
}

long
rb_num2long(VALUE v)
{
    if (FIXNUM_P(v))
        return FIX2LONG(v);
    if (NIL_P(v))
        rb_raise(rb_eTypeError, "no implicit conversion from nil to integer");
    int64_t i;
    if (!api->int_get(ctx(), M(v), &i))
        capi_unwind();
    return (long)i;
}

/* CRuby names the C type in the range error, so a `long long` ask on a
 * value `long` cannot hold says 'long long', not 'long'. */
long long
rb_num2ll(VALUE v)
{
    /* A Fixnum is 63-bit here, so the outer sixty-fourth of `long long`'s
     * range is T_BIGNUM; `rb_num2long` takes those, and what it rejects
     * is reported in `long long`'s name. */
    if (RB_TYPE_P(v, T_BIGNUM)) {
        VALUE two63 = rb_funcall(INT2FIX(1), rb_intern("<<"), 1, INT2FIX(63));
        VALUE min = rb_funcall(two63, rb_intern("-@"), 0);
        if (RTEST(rb_funcall(v, rb_intern("<"), 1, min)) ||
            !RTEST(rb_funcall(v, rb_intern("<"), 1, two63)))
            rb_raise(rb_eRangeError, "bignum too big to convert into 'long long'");
    }
    return rb_num2long(v);
}

/* An Integer in 2**63...2**64, as its low 64 bits; 0 when `v` is not. */
static int
num2u64_big(VALUE v, uint64_t *out)
{
    if (!RB_TYPE_P(v, T_BIGNUM))
        return 0;
    VALUE two64 = rb_funcall(INT2FIX(1), rb_intern("<<"), 1, INT2FIX(64));
    if (RTEST(rb_funcall(v, rb_intern("<"), 1, INT2FIX(0))) ||
        !RTEST(rb_funcall(v, rb_intern("<"), 1, two64)))
        return 0;
    VALUE hi = rb_funcall(v, rb_intern(">>"), 1, INT2FIX(32));
    VALUE lo = rb_funcall(v, rb_intern("&"), 1, LONG2NUM(0xffffffffL));
    *out = ((uint64_t)FIX2LONG(hi) << 32) | (uint64_t)FIX2LONG(lo);
    return 1;
}

unsigned long
rb_num2ulong(VALUE v)
{
    uint64_t u;
    if (num2u64_big(v, &u))
        return (unsigned long)u;
    return (unsigned long)rb_num2long(v);
}

unsigned long long
rb_num2ull(VALUE v)
{
    uint64_t u;
    if (num2u64_big(v, &u))
        return u;
    if (RB_TYPE_P(v, T_BIGNUM))
        rb_raise(rb_eRangeError, "bignum too big to convert into 'unsigned long long'");
    return (unsigned long long)rb_num2long(v);
}

static void
out_of_range(long n, const char *ty)
{
    rb_raise(rb_eRangeError, "integer %ld too %s to convert to '%s'", n, n < 0 ? "small" : "big", ty);
}

long
rb_num2int(VALUE v)
{
    long n = rb_num2long(v);
    if (n < INT_MIN || n > INT_MAX)
        out_of_range(n, "int");
    return n;
}

long
rb_fix2int(VALUE v)
{
    return rb_num2int(v);
}

unsigned long
rb_num2uint(VALUE v)
{
    long n = rb_num2long(v);
    if (n < INT_MIN || n > (long)UINT_MAX)
        out_of_range(n, "unsigned int");
    return (unsigned long)(unsigned int)n;
}

unsigned long
rb_fix2uint(VALUE v)
{
    return rb_num2uint(v);
}

short
rb_num2short(VALUE v)
{
    long n = rb_num2long(v);
    if (n < SHRT_MIN || n > SHRT_MAX)
        out_of_range(n, "short");
    return (short)n;
}

unsigned short
rb_num2ushort(VALUE v)
{
    long n = rb_num2long(v);
    if (n < SHRT_MIN || n > USHRT_MAX)
        out_of_range(n, "unsigned short");
    return (unsigned short)n;
}

double
rb_num2dbl(VALUE v)
{
    double d;
    if (!api->float_get(ctx(), M(v), &d))
        capi_unwind();
    return d;
}

VALUE
rb_float_new(double d)
{
    return root(R(api->float_new(d)));
}

VALUE
rb_float_new_in_heap(double d)
{
    return rb_float_new(d);
}

double
rb_float_value(VALUE v)
{
    return rb_num2dbl(v);
}

char
rb_num2chr(VALUE v)
{
    if (RB_TYPE_P(v, T_STRING) && RSTRING_LEN(v) >= 1)
        return RSTRING_PTR(v)[0];
    return (char)(NUM2INT(v) & 0xff);
}

/* ---------------------------------------------------------------------
 * Strings
 * --------------------------------------------------------------------- */

char *
rb_monoruby_rstring_ptr(VALUE str)
{
    size_t len;
    uint8_t *p = api->size >= offsetof(MrApi, str_rptr) + sizeof(api->str_rptr)
                     ? api->str_rptr(ctx(), M(str), &len)
                     : api->str_cptr(ctx(), M(str), &len);
    if (!p)
        capi_unwind();
    return (char *)p;
}

long
rb_monoruby_rstring_len(VALUE str)
{
    size_t len;
    if (!api->str_ptr(ctx(), M(str), &len))
        capi_unwind();
    return (long)len;
}

static VALUE
force_encoding(VALUE str, const char *name)
{
    rb_funcall(str, rb_intern("force_encoding"), 1, rb_str_new_cstr(name));
    return str;
}

VALUE
rb_str_new(const char *ptr, long len)
{
    if (len < 0)
        rb_raise(rb_eArgError, "negative string size (or size too big)");
    if (!ptr && len > 0) {
        /* `len` zero bytes, with room for the terminator RSTRING_PTR
         * writes: made from a zeroed buffer at once when it is small, which
         * spares growing the String twice (recv buffers). */
        static const uint8_t zeros[1024];
        if ((size_t)len < sizeof(zeros)) {
            VALUE s = check(api->bytes_new(ctx(), zeros, (size_t)len + 1));
            check_rc(api->str_resize(ctx(), M(s), (size_t)len));
            return s;
        }
        VALUE s = check(api->bytes_new(ctx(), (const uint8_t *)"", 0));
        check_rc(api->str_resize(ctx(), M(s), (size_t)len));
        return s;
    }
    return check(api->bytes_new(ctx(), (const uint8_t *)ptr, (size_t)len));
}

/* A hidden-in-CRuby scratch String; here an ordinary one of `len` zero bytes. */
VALUE
rb_str_tmp_new(long len)
{
    return rb_str_new(NULL, len);
}

VALUE
rb_str_new_cstr(const char *ptr)
{
    return rb_str_new(ptr, (long)strlen(ptr));
}

VALUE
rb_str_new_static(const char *ptr, long len)
{
    return rb_str_new(ptr, len);
}

VALUE
rb_str_new_frozen(VALUE str)
{
    if (OBJ_FROZEN(str))
        return str;
    return rb_obj_freeze(rb_str_dup(str));
}

VALUE
rb_str_new_shared(VALUE str)
{
    return rb_str_dup(str);
}

VALUE
rb_str_new_with_class(VALUE obj, const char *ptr, long len)
{
    (void)obj;
    return rb_str_new(ptr, len);
}

VALUE
rb_str_buf_new(long capa)
{
    /* An empty String whose buffer already holds `capa` (zeroed) bytes:
     * C fills it through RSTRING_PTR and then rb_str_set_len, which
     * keeps what was written (the ABI's `str_resize` contract). */
    VALUE s = rb_str_new("", 0);
    if (capa > 0) {
        rb_str_resize(s, capa);
        rb_str_set_len(s, 0);
    }
    return s;
}

VALUE
rb_str_buf_new_cstr(const char *ptr)
{
    return rb_str_new_cstr(ptr);
}

VALUE
rb_usascii_str_new(const char *ptr, long len)
{
    VALUE str = rb_str_new(ptr, len);
    if (HAVE_API(str_set_enc)) {
        int rc = api->str_set_enc(ctx(), M(str), "US-ASCII");
        if (rc == 0)
            return str;
        if (rc == -1)
            capi_unwind();
    }
    return force_encoding(str, "US-ASCII");
}

VALUE
rb_usascii_str_new_cstr(const char *ptr)
{
    return rb_usascii_str_new(ptr, (long)strlen(ptr));
}

VALUE
rb_utf8_str_new(const char *ptr, long len)
{
    if (len < 0)
        rb_raise(rb_eArgError, "negative string size (or size too big)");
    if (!ptr)
        return force_encoding(rb_str_new(NULL, len), "UTF-8");
    return check(api->str_new(ctx(), (const uint8_t *)ptr, (size_t)len));
}

VALUE
rb_utf8_str_new_static(const char *ptr, long len)
{
    return rb_utf8_str_new(ptr, len);
}

VALUE
rb_utf8_str_new_cstr(const char *ptr)
{
    return rb_utf8_str_new(ptr, (long)strlen(ptr));
}

VALUE
rb_external_str_new(const char *ptr, long len)
{
    return rb_utf8_str_new(ptr, len);
}

VALUE
rb_external_str_new_cstr(const char *ptr)
{
    return rb_utf8_str_new_cstr(ptr);
}

VALUE
rb_locale_str_new(const char *ptr, long len)
{
    return rb_utf8_str_new(ptr, len);
}

VALUE
rb_locale_str_new_cstr(const char *ptr)
{
    return rb_utf8_str_new_cstr(ptr);
}

VALUE
rb_str_cat(VALUE str, const char *ptr, long len)
{
    if (len < 0)
        rb_raise(rb_eArgError, "negative string size (or size too big)");
    check_rc(api->str_cat(ctx(), M(str), (const uint8_t *)ptr, (size_t)len));
    return str;
}

VALUE
rb_str_cat_cstr(VALUE str, const char *ptr)
{
    return rb_str_cat(str, ptr, (long)strlen(ptr));
}

VALUE
rb_str_buf_append(VALUE str, VALUE str2)
{
    StringValue(str2);
    /* The cases string.c appends without a change of encoding: the same
     * encoding, or ASCII-only text onto an ASCII-compatible string. */
    if (HAVE_API(str_cat) && HAVE_API(str_rptr) && str != str2 && RB_TYPE_P(str, T_STRING)) {
        rb_encoding *e1 = rb_enc_get(str), *e2 = rb_enc_get(str2);
        if (e1 == e2 ||
            (rb_enc_asciicompat(e1) && rb_enc_str_coderange(str2) == ENC_CODERANGE_7BIT)) {
            size_t len;
            const uint8_t *p = api->str_rptr(ctx(), M(str2), &len);
            if (!p)
                capi_unwind();
            check_rc(api->str_cat(ctx(), M(str), p, len));
            return str;
        }
    }
    return rb_funcall(str, rb_intern("<<"), 1, str2);
}

VALUE
rb_str_append(VALUE str, VALUE str2)
{
    StringValue(str2);
    return rb_funcall(str, rb_intern("<<"), 1, str2);
}

VALUE
rb_str_concat(VALUE str, VALUE obj)
{
    return rb_funcall(str, rb_intern("<<"), 1, obj);
}

VALUE
rb_str_plus(VALUE a, VALUE b)
{
    return rb_funcall(a, rb_intern("+"), 1, b);
}

VALUE
rb_str_times(VALUE str, VALUE times)
{
    return rb_funcall(str, rb_intern("*"), 1, times);
}

VALUE
rb_str_dup(VALUE str)
{
    if (HAVE_API(str_dup) && RB_TYPE_P(str, T_STRING))
        return check(api->str_dup(ctx(), M(str)));
    return rb_funcall(str, rb_intern("dup"), 0);
}

VALUE
rb_str_resurrect(VALUE str)
{
    return rb_str_dup(str);
}

VALUE
rb_str_freeze(VALUE str)
{
    return rb_obj_freeze(str);
}

VALUE
rb_str_length(VALUE str)
{
    return rb_funcall(str, rb_intern("length"), 0);
}

long
rb_str_strlen(VALUE str)
{
    return NUM2LONG(rb_str_length(str));
}

VALUE
rb_str_substr(VALUE str, long beg, long len)
{
    return rb_funcall(str, rb_intern("[]"), 2, LONG2NUM(beg), LONG2NUM(len));
}

VALUE
rb_str_subseq(VALUE str, long beg, long len)
{
    if (HAVE_API(str_subseq) && RB_TYPE_P(str, T_STRING))
        return check(api->str_subseq(ctx(), M(str), (int64_t)beg, (int64_t)len));
    return rb_funcall(str, rb_intern("byteslice"), 2, LONG2NUM(beg), LONG2NUM(len));
}

VALUE
rb_str_equal(VALUE a, VALUE b)
{
    return rb_equal(a, b);
}

int
rb_str_cmp(VALUE a, VALUE b)
{
    return rb_cmpint(rb_funcall(a, rb_intern("<=>"), 1, b), a, b);
}

st_index_t
rb_str_hash(VALUE str)
{
    return (st_index_t)NUM2LONG(rb_hash(str));
}

/* CRuby seeds this per process; what callers rely on is that equal
 * bytes hash equal within one process, which st_hash gives. */
st_index_t
rb_memhash(const void *ptr, long len)
{
    return rb_st_hash(ptr, (size_t)len, 0);
}

VALUE
rb_str_inspect(VALUE str)
{
    return rb_inspect(str);
}

VALUE
rb_str_dump(VALUE str)
{
    return rb_funcall(str, rb_intern("dump"), 0);
}

VALUE
rb_str_split(VALUE str, const char *sep)
{
    return rb_funcall(str, rb_intern("split"), 1, rb_str_new_cstr(sep));
}

VALUE
rb_str_intern(VALUE str)
{
    return rb_funcall(str, rb_intern("to_sym"), 0);
}

VALUE
rb_str_to_str(VALUE obj)
{
    return rb_convert_type(obj, T_STRING, "String", "to_str");
}

VALUE
rb_string_value(volatile VALUE *ptr)
{
    VALUE s = *ptr;
    if (!RB_TYPE_P(s, T_STRING)) {
        s = rb_str_to_str(s);
        *ptr = s;
    }
    return s;
}

char *
rb_string_value_ptr(volatile VALUE *ptr)
{
    VALUE s = rb_string_value(ptr);
    return RSTRING_PTR(s);
}

char *
rb_string_value_cstr(volatile VALUE *ptr)
{
    VALUE s = rb_string_value(ptr);
    /* The NUL-terminated buffer: a shared view is detached first. */
    size_t n;
    char *p = (char *)api->str_cptr(ctx(), M(s), &n);
    if (!p)
        capi_unwind();
    long len = (long)n;
    if (memchr(p, 0, (size_t)len))
        rb_raise(rb_eArgError, "string contains null byte");
    return p;
}

VALUE
rb_check_string_type(VALUE obj)
{
    return rb_check_convert_type(obj, T_STRING, "String", "to_str");
}

VALUE
rb_str_resize(VALUE str, long len)
{
    if (len < 0)
        rb_raise(rb_eArgError, "negative string size (or size too big)");
    check_rc(api->str_resize(ctx(), M(str), (size_t)len));
    return str;
}

void
rb_str_set_len(VALUE str, long len)
{
    rb_str_resize(str, len);
}

void
rb_str_modify(VALUE str)
{
    rb_check_frozen(str);
    /* Detach a shared buffer (RSTRING_PTR hands a shared view's bytes out
     * in place), so the bytes RSTRING_PTR gives from here on are this
     * string's own to write. `str_modify` writes no terminator, which
     * would land on a byte the extension wrote past the length (recv into
     * an `rb_str_modify_expand`ed buffer, then rb_str_modify +
     * rb_str_set_len). */
    if (HAVE_API(str_modify)) {
        if (api->str_modify(ctx(), M(str)) != 0)
            capi_unwind();
        return;
    }
    size_t len;
    if (!api->str_cptr(ctx(), M(str), &len))
        capi_unwind();
}

void
rb_str_modify_expand(VALUE str, long expand)
{
    long len = RSTRING_LEN(str);
    rb_str_modify(str);
    if (expand > 0) {
        rb_str_resize(str, len + expand);
        rb_str_resize(str, len);
    }
}

VALUE
rb_str_to_inum(VALUE str, int base, int badcheck)
{
    if (badcheck)
        return rb_funcall(rb_mKernel, rb_intern("Integer"), 2, str, INT2FIX(base));
    return rb_funcall(str, rb_intern("to_i"), 1, INT2FIX(base));
}

VALUE
rb_str2inum(VALUE str, int base)
{
    return rb_str_to_inum(str, base, 0);
}

VALUE
rb_cstr2inum(const char *str, int base)
{
    return rb_str_to_inum(rb_str_new_cstr(str), base, 1);
}

double
rb_str_to_dbl(VALUE str, int badcheck)
{
    if (badcheck)
        return rb_num2dbl(rb_Float(str));
    return rb_num2dbl(rb_funcall(str, rb_intern("to_f"), 0));
}

double
rb_cstr_to_dbl(const char *p, int badcheck)
{
    return rb_str_to_dbl(rb_str_new_cstr(p), badcheck);
}

/* ---------------------------------------------------------------------
 * Arrays
 * --------------------------------------------------------------------- */

VALUE
rb_ary_new(void)
{
    return check(api->ary_new(ctx()));
}

VALUE
rb_ary_new_capa(long capa)
{
    (void)capa;
    return rb_ary_new();
}

VALUE
rb_ary_tmp_new(long capa)
{
    return rb_ary_new_capa(capa);
}

VALUE
rb_ary_new_from_values(long n, const VALUE *elts)
{
    VALUE ary = rb_ary_new();
    for (long i = 0; i < n; i++)
        check_rc(api->ary_push(ctx(), M(ary), M(elts[i])));
    return ary;
}

VALUE
rb_ary_new_from_args(long n, ...)
{
    VALUE ary = rb_ary_new();
    va_list ap;
    va_start(ap, n);
    for (long i = 0; i < n; i++)
        check_rc(api->ary_push(ctx(), M(ary), M(va_arg(ap, VALUE))));
    va_end(ap);
    return ary;
}

VALUE
rb_ary_push(VALUE ary, VALUE item)
{
    check_rc(api->ary_push(ctx(), M(ary), M(item)));
    return ary;
}

VALUE
rb_ary_cat(VALUE ary, const VALUE *train, long len)
{
    for (long i = 0; i < len; i++)
        rb_ary_push(ary, train[i]);
    return ary;
}

VALUE
rb_ary_pop(VALUE ary)
{
    return rb_funcall(ary, rb_intern("pop"), 0);
}

VALUE
rb_ary_shift(VALUE ary)
{
    return rb_funcall(ary, rb_intern("shift"), 0);
}

VALUE
rb_ary_unshift(VALUE ary, VALUE item)
{
    return rb_funcall(ary, rb_intern("unshift"), 1, item);
}

void
rb_ary_store(VALUE ary, long idx, VALUE val)
{
    rb_funcall(ary, rb_intern("[]="), 2, LONG2NUM(idx), val);
}

long
rb_array_len(VALUE ary)
{
    size_t len;
    if (!api->ary_ptr(ctx(), M(ary), &len))
        capi_unwind();
    return (long)len;
}

const VALUE *
rb_array_const_ptr(VALUE ary)
{
    size_t len;
    MrValue a = M(ary);
    const MrValue *p = api->ary_ptr(ctx(), a, &len);
    if (!p)
        capi_unwind();
    struct capi_frame *owner = frame()->owner;
    struct capi_ary_slot *slot = NULL;
    for (unsigned i = 0; i < owner->ary_init; i++) {
        if (owner->ary[i].buf && owner->ary[i].ary == a) {
            slot = &owner->ary[i];
            break;
        }
    }
    if (!slot && owner->ary_init < CAPI_ARY_SLOTS) {
        slot = &owner->ary[owner->ary_init++];
        slot->buf = NULL;
        slot->cap = 0;
    }
    else if (!slot) {
        slot = &owner->ary[owner->ary_next];
        owner->ary_next = (owner->ary_next + 1) % CAPI_ARY_SLOTS;
    }
    if (!slot->buf || slot->cap < len) {
        size_t cap = len ? len : 1;
        VALUE *buf = realloc(slot->buf, cap * sizeof(VALUE));
        if (!buf)
            rb_memerror();
        slot->buf = buf;
        slot->cap = cap;
    }
    slot->ary = a;
    for (size_t i = 0; i < len; i++)
        slot->buf[i] = R(p[i]);
    return slot->buf;
}

VALUE
rb_ary_entry(VALUE ary, long offset)
{
    size_t len;
    const MrValue *p = api->ary_ptr(ctx(), M(ary), &len);
    if (!p)
        capi_unwind();
    if (offset < 0)
        offset += (long)len;
    if (offset < 0 || (size_t)offset >= len)
        return Qnil;
    return R(p[offset]);
}

VALUE
rb_ary_aref(int argc, const VALUE *argv, VALUE ary)
{
    return rb_funcallv(ary, rb_intern("[]"), argc, argv);
}

VALUE
rb_ary_dup(VALUE ary)
{
    return rb_funcall(ary, rb_intern("dup"), 0);
}

VALUE
rb_ary_clear(VALUE ary)
{
    return rb_funcall(ary, rb_intern("clear"), 0);
}

VALUE
rb_ary_concat(VALUE x, VALUE y)
{
    return rb_funcall(x, rb_intern("concat"), 1, y);
}

VALUE
rb_ary_plus(VALUE x, VALUE y)
{
    return rb_funcall(x, rb_intern("+"), 1, y);
}

VALUE
rb_ary_join(VALUE ary, VALUE sep)
{
    if (NIL_P(sep))
        return rb_funcall(ary, rb_intern("join"), 0);
    return rb_funcall(ary, rb_intern("join"), 1, sep);
}

VALUE
rb_ary_reverse(VALUE ary)
{
    return rb_funcall(ary, rb_intern("reverse!"), 0);
}

VALUE
rb_ary_sort(VALUE ary)
{
    return rb_funcall(ary, rb_intern("sort"), 0);
}

VALUE
rb_ary_sort_bang(VALUE ary)
{
    return rb_funcall(ary, rb_intern("sort!"), 0);
}

VALUE
rb_ary_includes(VALUE ary, VALUE item)
{
    return rb_funcall(ary, rb_intern("include?"), 1, item);
}

VALUE
rb_ary_delete(VALUE ary, VALUE item)
{
    return rb_funcall(ary, rb_intern("delete"), 1, item);
}

VALUE
rb_ary_delete_at(VALUE ary, long pos)
{
    return rb_funcall(ary, rb_intern("delete_at"), 1, LONG2NUM(pos));
}

VALUE
rb_ary_subseq(VALUE ary, long beg, long len)
{
    return rb_funcall(ary, rb_intern("[]"), 2, LONG2NUM(beg), LONG2NUM(len));
}

VALUE
rb_ary_to_ary(VALUE obj)
{
    VALUE ary = rb_check_array_type(obj);
    return NIL_P(ary) ? rb_ary_new_from_values(1, &obj) : ary;
}

VALUE
rb_ary_to_s(VALUE ary)
{
    return rb_inspect(ary);
}

VALUE
rb_ary_freeze(VALUE ary)
{
    return rb_obj_freeze(ary);
}

VALUE
rb_ary_resize(VALUE ary, long len)
{
    long cur = RARRAY_LEN(ary);
    if (len < cur)
        rb_funcall(ary, rb_intern("slice!"), 2, LONG2NUM(len), LONG2NUM(cur - len));
    else if (len > cur)
        rb_ary_store(ary, len - 1, Qnil);
    return ary;
}

VALUE
rb_ary_assoc(VALUE ary, VALUE key)
{
    return rb_funcall(ary, rb_intern("assoc"), 1, key);
}

VALUE
rb_check_array_type(VALUE obj)
{
    return rb_check_convert_type(obj, T_ARRAY, "Array", "to_ary");
}

/* ---------------------------------------------------------------------
 * Hashes
 * --------------------------------------------------------------------- */

VALUE
rb_hash_new(void)
{
    return check(api->hash_new(ctx()));
}

VALUE
rb_hash_new_capa(long capa)
{
    (void)capa;
    return rb_hash_new();
}

VALUE
rb_hash_aset(VALUE hash, VALUE key, VALUE val)
{
    /* A String key is copied and frozen, which `Hash#[]=` does. */
    if (RB_TYPE_P(key, T_STRING))
        rb_funcall(hash, rb_intern("[]="), 2, key, val);
    else
        check_rc(api->hash_set(ctx(), M(hash), M(key), M(val)));
    return val;
}

VALUE
rb_hash_aref(VALUE hash, VALUE key)
{
    return rb_funcall(hash, rb_intern("[]"), 1, key);
}

VALUE
rb_hash_lookup2(VALUE hash, VALUE key, VALUE def)
{
    MrValue v = api->hash_get(ctx(), M(hash), M(key));
    if (v == MR_UNDEF) {
        if (api->error_pending(ctx()))
            capi_unwind();
        return def;
    }
    return root(R(v));
}

VALUE
rb_hash_lookup(VALUE hash, VALUE key)
{
    return rb_hash_lookup2(hash, key, Qnil);
}

VALUE
rb_hash_fetch(VALUE hash, VALUE key)
{
    return rb_funcall(hash, rb_intern("fetch"), 1, key);
}

VALUE
rb_hash_delete(VALUE hash, VALUE key)
{
    return rb_funcall(hash, rb_intern("delete"), 1, key);
}

VALUE
rb_hash_clear(VALUE hash)
{
    return rb_funcall(hash, rb_intern("clear"), 0);
}

VALUE
rb_hash_size(VALUE hash)
{
    return rb_funcall(hash, rb_intern("size"), 0);
}

size_t
rb_hash_size_num(VALUE hash)
{
    return (size_t)NUM2LONG(rb_hash_size(hash));
}

VALUE
rb_hash_dup(VALUE hash)
{
    return rb_funcall(hash, rb_intern("dup"), 0);
}

VALUE
rb_hash_freeze(VALUE hash)
{
    return rb_obj_freeze(hash);
}

VALUE
rb_hash_keys(VALUE hash)
{
    return rb_funcall(hash, rb_intern("keys"), 0);
}

VALUE
rb_hash_values(VALUE hash)
{
    return rb_funcall(hash, rb_intern("values"), 0);
}

VALUE
rb_hash_has_key(VALUE hash, VALUE key)
{
    return rb_funcall(hash, rb_intern("key?"), 1, key);
}

VALUE
rb_hash_set_ifnone(VALUE hash, VALUE ifnone)
{
    rb_funcall(hash, rb_intern("default="), 1, ifnone);
    return hash;
}

VALUE
rb_check_hash_type(VALUE obj)
{
    return rb_check_convert_type(obj, T_HASH, "Hash", "to_hash");
}

void
rb_hash_foreach(VALUE hash, int (*func)(VALUE, VALUE, VALUE), VALUE arg)
{
    VALUE keys = rb_hash_keys(hash);
    long n = RARRAY_LEN(keys);
    for (long i = 0; i < n; i++) {
        VALUE key = rb_ary_entry(keys, i);
        MrValue v = api->hash_get(ctx(), M(hash), M(key));
        if (v == MR_UNDEF) {
            if (api->error_pending(ctx()))
                capi_unwind();
            continue;
        }
        int r = func(key, root(R(v)), arg);
        if (r == ST_STOP)
            break;
        if (r == ST_DELETE)
            rb_hash_delete(hash, key);
    }
}

/* ---------------------------------------------------------------------
 * Ranges
 * --------------------------------------------------------------------- */

VALUE
rb_range_new(VALUE beg, VALUE end, int exclude_end)
{
    return rb_funcall(rb_cRange, rb_intern("new"), 3, beg, end, exclude_end ? Qtrue : Qfalse);
}

int
rb_range_values(VALUE range, VALUE *begp, VALUE *endp, int *exclp)
{
    if (!RTEST(rb_obj_is_kind_of(range, rb_cRange))) {
        if (!rb_respond_to(range, rb_intern("begin")) || !rb_respond_to(range, rb_intern("end")) ||
            !rb_respond_to(range, rb_intern("exclude_end?")))
            return 0;
    }
    *begp = rb_funcall(range, rb_intern("begin"), 0);
    *endp = rb_funcall(range, rb_intern("end"), 0);
    *exclp = RTEST(rb_funcall(range, rb_intern("exclude_end?"), 0));
    return 1;
}

/* CRuby's range.c: `range` as (begin, length) over a sequence of `len`
 * elements. Qfalse for a non-range; Qnil (err 0) or RangeError (err 1)
 * when out of range; err 2 clamps like err 0 but raises. */
VALUE
rb_range_beg_len(VALUE range, long *begp, long *lenp, long len, int err)
{
    VALUE b, e;
    int excl;
    if (!rb_range_values(range, &b, &e, &excl))
        return Qfalse;
    long beg = NIL_P(b) ? 0 : NUM2LONG(b);
    long end = NIL_P(e) ? -1 : NUM2LONG(e);
    if (NIL_P(e))
        excl = 0;
    long origbeg = beg, origend = end;
    if (beg < 0) {
        beg += len;
        if (beg < 0)
            goto out_of_range;
    }
    if (end < 0)
        end += len;
    if (!excl)
        end++;
    if (err == 0 || err == 2) {
        if (beg > len)
            goto out_of_range;
        if (end > len)
            end = len;
    }
    len = end - beg;
    if (len < 0)
        len = 0;
    *begp = beg;
    *lenp = len;
    return Qtrue;
out_of_range:
    if (err)
        rb_raise(rb_eRangeError, "%ld..%s%ld out of range", origbeg, excl ? "." : "", origend);
    return Qnil;
}

/* ---------------------------------------------------------------------
 * Encodings
 * --------------------------------------------------------------------- */

static rb_encoding *encodings[256];
static int n_encodings;

static rb_encoding *
enc_register(const char *name)
{
    for (int i = 0; i < n_encodings; i++) {
        if (strcmp(encodings[i]->name, name) == 0)
            return encodings[i];
    }
    if (n_encodings == 256)
        rb_raise(rb_eEncodingError, "too many encodings");
    rb_encoding *enc = malloc(sizeof(rb_encoding));
    char *copy = malloc(strlen(name) + 1);
    if (!enc || !copy)
        rb_memerror();
    strcpy(copy, name);
    enc->name = copy;
    enc->index = n_encodings;
    encodings[n_encodings++] = enc;
    return enc;
}

static void
enc_init(void)
{
    if (n_encodings == 0) {
        enc_register("ASCII-8BIT");
        enc_register("UTF-8");
        enc_register("US-ASCII");
    }
}

/* The canonical name, as `Encoding#name` gives it. */
static rb_encoding *
enc_of_name(VALUE name)
{
    enc_init();
    const char *s = StringValueCStr(name);
    if (strcmp(s, "BINARY") == 0)
        s = "ASCII-8BIT";
    return enc_register(s);
}

rb_encoding *
rb_ascii8bit_encoding(void)
{
    enc_init();
    return encodings[0];
}

rb_encoding *
rb_utf8_encoding(void)
{
    enc_init();
    return encodings[1];
}

rb_encoding *
rb_usascii_encoding(void)
{
    enc_init();
    return encodings[2];
}

rb_encoding *
rb_locale_encoding(void)
{
    return rb_utf8_encoding();
}

rb_encoding *
rb_filesystem_encoding(void)
{
    return rb_utf8_encoding();
}

rb_encoding *
rb_default_external_encoding(void)
{
    return rb_enc_from_index(rb_to_encoding_index(rb_funcall(rb_cEncoding, rb_intern("default_external"), 0)));
}

rb_encoding *
rb_default_internal_encoding(void)
{
    VALUE e = rb_funcall(rb_cEncoding, rb_intern("default_internal"), 0);
    return NIL_P(e) ? NULL : rb_to_encoding(e);
}

int
rb_ascii8bit_encindex(void)
{
    return 0;
}

int
rb_utf8_encindex(void)
{
    return 1;
}

int
rb_usascii_encindex(void)
{
    return 2;
}

/* `api->str_enc_name` hands out one pointer per encoding for the life of
 * the process, so the rb_encoding it names is cached by that pointer. */
static struct {
    const char *name;
    rb_encoding *enc;
} enc_name_cache[64];

static rb_encoding *
enc_of_static_name(const char *name)
{
    size_t h = ((uintptr_t)name >> 4) % 64;
    if (enc_name_cache[h].name == name)
        return enc_name_cache[h].enc;
    enc_init();
    rb_encoding *enc = enc_register(strcmp(name, "BINARY") == 0 ? "ASCII-8BIT" : name);
    enc_name_cache[h].name = name;
    enc_name_cache[h].enc = enc;
    return enc;
}

/*
 * A String cell's encoding (string.rs `Encoding`, `repr(u8)`) is its tag
 * byte at RB_MONORUBY_STR_ENC_OFFSET, and for the variants that carry one
 * (RB_MONORUBY_ENC_PAYLOAD_TAGS) the payload byte after it; the payload
 * byte of the others is padding. string.rs asserts the numbers. The
 * rb_encoding an encoding names is cached by those bytes.
 */
#define RB_MONORUBY_STR_ENC_OFFSET 56
#define RB_MONORUBY_ENC_PAYLOAD_TAGS \
    ((1u << 1) | (1u << 7) | (1u << 8) | (1u << 9) | (1u << 11) | (1u << 12))
static struct {
    unsigned key; /* tag | payload << 8, plus one; 0 = empty */
    rb_encoding *enc;
} enc_tag_cache[64];

static rb_encoding *enc_of_static_name(const char *name);

static rb_encoding *
str_enc_fast(VALUE str)
{
    const unsigned char *e = (const unsigned char *)str + RB_MONORUBY_STR_ENC_OFFSET;
    unsigned tag = e[0];
    unsigned key = tag < 32 && (RB_MONORUBY_ENC_PAYLOAD_TAGS >> tag & 1) ? tag | (unsigned)e[1] << 8 : tag;
    unsigned h = (key ^ key >> 6) & 63;
    if (enc_tag_cache[h].key == key + 1)
        return enc_tag_cache[h].enc;
    const char *name = api->str_enc_name(ctx(), M(str));
    if (!name)
        return NULL;
    rb_encoding *enc = enc_of_static_name(name);
    enc_tag_cache[h].key = key + 1;
    enc_tag_cache[h].enc = enc;
    return enc;
}

VALUE
rb_obj_encoding(VALUE obj)
{
    return rb_funcall(obj, rb_intern("encoding"), 0);
}

rb_encoding *
rb_enc_get(VALUE obj)
{
    if (RB_TYPE_P(obj, T_STRING)) {
        if (HAVE_API(str_enc_name)) {
            rb_encoding *enc = str_enc_fast(obj);
            if (enc)
                return enc;
        }
        return enc_of_name(check(api->str_encoding(ctx(), M(obj))));
    }
    if (RTEST(rb_obj_is_kind_of(obj, rb_cEncoding)))
        return rb_to_encoding(obj);
    if (!rb_respond_to(obj, rb_intern("encoding")))
        return NULL;
    VALUE e = rb_funcall(obj, rb_intern("encoding"), 0);
    return NIL_P(e) ? NULL : rb_to_encoding(e);
}

int
rb_enc_get_index(VALUE obj)
{
    rb_encoding *enc = rb_enc_get(obj);
    return enc ? enc->index : -1;
}

void
rb_enc_set_index(VALUE obj, int idx)
{
    rb_enc_associate_index(obj, idx);
}

static VALUE
enc_find_body(VALUE name)
{
    return rb_funcall(rb_cEncoding, rb_intern("find"), 1, name);
}

rb_encoding *
rb_enc_find(const char *name)
{
    int state;
    VALUE e = rb_protect(enc_find_body, rb_str_new_cstr(name), &state);
    if (state) {
        rb_set_errinfo(Qnil);
        return NULL;
    }
    return NIL_P(e) ? NULL : rb_to_encoding(e);
}

int
rb_enc_find_index(const char *name)
{
    rb_encoding *enc = rb_enc_find(name);
    return enc ? enc->index : -1;
}

rb_encoding *
rb_enc_from_index(int idx)
{
    enc_init();
    if (idx < 0 || idx >= n_encodings)
        return NULL;
    return encodings[idx];
}

VALUE
rb_enc_from_encoding(rb_encoding *enc)
{
    if (!enc)
        return Qnil;
    return enc_find_body(rb_str_new_cstr(enc->name));
}

rb_encoding *
rb_to_encoding(VALUE enc)
{
    if (!RTEST(rb_obj_is_kind_of(enc, rb_cEncoding)))
        enc = enc_find_body(rb_String(enc));
    return enc_of_name(rb_funcall(enc, rb_intern("name"), 0));
}

int
rb_to_encoding_index(VALUE enc)
{
    rb_encoding *e = rb_to_encoding(enc);
    return e ? e->index : -1;
}

VALUE
rb_enc_associate(VALUE obj, rb_encoding *enc)
{
    const char *name = enc ? enc->name : "ASCII-8BIT";
    if (HAVE_API(str_set_enc) && RB_TYPE_P(obj, T_STRING)) {
        int rc = api->str_set_enc(ctx(), M(obj), name);
        if (rc == 0)
            return obj;
        if (rc == -1)
            capi_unwind();
    }
    return force_encoding(obj, name);
}

VALUE
rb_enc_associate_index(VALUE obj, int idx)
{
    return rb_enc_associate(obj, rb_enc_from_index(idx));
}

void
rb_enc_copy(VALUE dst, VALUE src)
{
    rb_enc_associate(dst, rb_enc_get(src));
}

VALUE
rb_enc_str_new(const char *ptr, long len, rb_encoding *enc)
{
    return rb_enc_associate(rb_str_new(ptr, len), enc);
}

/* `ptr` taken as `enc` and appended with String#<<'s encoding rules. */
VALUE
rb_enc_str_buf_cat(VALUE str, const char *ptr, long len, rb_encoding *enc)
{
    return rb_str_buf_append(str, rb_enc_str_new(ptr, len, enc));
}

VALUE
rb_enc_str_new_cstr(const char *ptr, rb_encoding *enc)
{
    return rb_enc_str_new(ptr, (long)strlen(ptr), enc);
}

VALUE
rb_enc_str_new_static(const char *ptr, long len, rb_encoding *enc)
{
    return rb_enc_str_new(ptr, len, enc);
}

VALUE
rb_external_str_new_with_enc(const char *ptr, long len, rb_encoding *enc)
{
    return rb_enc_str_new(ptr, len, enc);
}

VALUE
rb_str_export_to_enc(VALUE str, rb_encoding *enc)
{
    return rb_funcall(str, rb_intern("encode"), 1, rb_str_new_cstr(enc->name));
}

struct conv_args {
    VALUE str, to, from;
};

static VALUE
conv_body(VALUE p)
{
    struct conv_args *a = (struct conv_args *)p;
    return rb_funcall(a->str, rb_intern("encode"), 2, a->to, a->from);
}

VALUE
rb_str_conv_enc(VALUE str, rb_encoding *from, rb_encoding *to)
{
    if (!to)
        return str;
    if (!from)
        from = rb_enc_get(str);
    if (from == to)
        return str;
    /* string.c's rb_str_conv_enc_opts: ASCII-only text needs no
     * conversion into an ASCII-compatible encoding (nor anything into
     * ASCII-8BIT), only a relabelled copy. */
    if ((rb_enc_asciicompat(to) && rb_enc_asciicompat(from) &&
         rb_enc_str_coderange(str) == ENC_CODERANGE_7BIT) ||
        to->index == 0) {
        if (rb_enc_get(str) != to) {
            str = rb_str_dup(str);
            rb_enc_associate(str, to);
        }
        return str;
    }
    struct conv_args a = {str, rb_str_new_cstr(to->name), rb_str_new_cstr(from->name)};
    int state;
    VALUE r = rb_protect(conv_body, (VALUE)&a, &state);
    if (state) {
        rb_set_errinfo(Qnil);
        return str;
    }
    return r;
}

VALUE
rb_str_encode(VALUE str, VALUE to, int ecflags, VALUE ecopts)
{
    (void)ecflags;
    if (NIL_P(ecopts))
        return rb_funcall(str, rb_intern("encode"), 1, to);
    return rb_funcall(str, rb_intern("encode"), 2, to, ecopts);
}

/* Per encoding index: 0 not asked yet, 1 ASCII-compatible, 2 not. */
static signed char enc_asciicompat_cache[256];

int
rb_enc_asciicompat(rb_encoding *enc)
{
    if (!enc)
        return 0;
    if (enc->index >= 0 && enc->index < 256 && enc_asciicompat_cache[enc->index])
        return enc_asciicompat_cache[enc->index] == 1;
    int r = RTEST(rb_funcall(rb_enc_from_encoding(enc), rb_intern("ascii_compatible?"), 0));
    if (enc->index >= 0 && enc->index < 256)
        enc_asciicompat_cache[enc->index] = r ? 1 : 2;
    return r;
}

int
rb_enc_str_asciionly_p(VALUE str)
{
    if (HAVE_API(str_coderange) && RB_TYPE_P(str, T_STRING))
        return rb_enc_str_coderange(str) == ENC_CODERANGE_7BIT;
    return RTEST(rb_funcall(str, rb_intern("ascii_only?"), 0));
}

VALUE
rb_enc_sprintf(rb_encoding *enc, const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    VALUE s = rb_vsprintf(fmt, ap);
    va_end(ap);
    return rb_enc_associate(s, enc);
}

/* ---------------------------------------------------------------------
 * Threads
 * --------------------------------------------------------------------- */

struct nogvl_call {
    void *(*func)(void *);
    void *data;
    void *result;
    int err;
};

/* Inside a blocking region's function on this kernel thread
 * (`rb_thread_call_with_gvl`). */
static __thread int capi_in_nogvl;

static int64_t
nogvl_trampoline(void *arg)
{
    struct nogvl_call *c = arg;
    capi_in_nogvl++;
    c->result = c->func(c->data);
    c->err = errno;
    capi_in_nogvl--;
    return 0;
}

void *
rb_nogvl(void *(*func)(void *), void *data1, rb_unblock_function_t *ubf, void *data2, int flags)
{
    (void)flags;
    struct capi_frame *saved = frame();
    struct nogvl_call c = {func, data1, NULL, 0};
    int64_t out;
    /* RUBY_UBF_IO / RUBY_UBF_PROCESS are (void *)-1, the ABI's own spelling of "no function,
       interrupt the syscall"; an older monoruby without the entry drops the ubf. */
    int rc = (ubf && HAVE_API(call_blocking_ubf))
        ? api->call_blocking_ubf(saved->ctx, nogvl_trampoline, &c, (void *)ubf, data2, &out)
        : api->call_blocking(saved->ctx, nogvl_trampoline, &c, &out);
    capi_cur = saved;
    if (rc != 0)
        capi_unwind();
    /* As CRuby's: the caller reads errno of a failed syscall in func. */
    errno = c.err;
    return c.result;
}

void *
rb_thread_call_without_gvl(void *(*func)(void *), void *data1, rb_unblock_function_t *ubf, void *data2)
{
    return rb_nogvl(func, data1, ubf, data2, 0);
}

void *
rb_thread_call_without_gvl2(void *(*func)(void *), void *data1, rb_unblock_function_t *ubf, void *data2)
{
    return rb_nogvl(func, data1, ubf, data2, 0);
}

VALUE
rb_thread_current(void)
{
    if (HAVE_API(thread_current))
        return check(api->thread_current(ctx()));
    return rb_funcall(rb_cThread, rb_intern("current"), 0);
}

/* `Thread#[]` / `#[]=`: fiber-local, as CRuby's are. The read skips the
 * method call (bigdecimal reads its modes on every operation). */
VALUE
rb_thread_local_aref(VALUE thread, ID key)
{
    if (HAVE_API(thread_local_aref)) {
        /* The fallback inside may run Ruby, as rb_funcall's does. */
        struct capi_frame *saved = frame();
        MrValue r = api->thread_local_aref(saved->ctx, M(thread), (MrSym)key);
        capi_cur = saved;
        return check(r);
    }
    return rb_funcall(thread, rb_intern("[]"), 1, ID2SYM(key));
}

VALUE
rb_thread_local_aset(VALUE thread, ID key, VALUE val)
{
    return rb_funcall(thread, rb_intern("[]="), 2, ID2SYM(key), val);
}

void
rb_thread_schedule(void)
{
    rb_funcall(rb_cThread, rb_intern("pass"), 0);
}

/* ---------------------------------------------------------------------
 * Interpreter
 * --------------------------------------------------------------------- */

const char *
rb_monoruby_ruby_version(void)
{
    return api->ruby_version();
}

void
ruby_init(void)
{
}

int
ruby_native_thread_p(void)
{
    return capi_cur != NULL;
}

/* ---------------------------------------------------------------------
 * Allocators, Data and TypedData
 * --------------------------------------------------------------------- */

/*
 * The payload of every Data / TypedData object; `struct RData` and
 * `struct RTypedData` in ruby.h are views of it. The object is a native
 * object of the interpreter's (`native_new`), of kind `data_ops`.
 */
struct capi_data {
    void *data;
    RUBY_DATA_FUNC dmark;
    RUBY_DATA_FUNC dfree;
    const rb_data_type_t *type;
    /* What `RBASIC(obj)` hands out: the object's FL_USER* bits, which
     * only this layer reads (`rb_monoruby_rbasic`). */
    struct RBasic basic;
};

static void
data_mark(void *p, MrMarker *marker)
{
    struct capi_data *d = p;
    if (!d->data)
        return;
    MrMarker *saved = capi_marker;
    capi_marker = marker;
    if (d->type) {
        if (d->type->function.dmark)
            d->type->function.dmark(d->data);
    }
    else if (d->dmark) {
        d->dmark(d->data);
    }
    capi_marker = saved;
}

static void
data_free(void *p)
{
    struct capi_data *d = p;
    RUBY_DATA_FUNC dfree = d->type ? d->type->function.dfree : d->dfree;
    if (d->data) {
        if (dfree == RUBY_DEFAULT_FREE)
            xfree(d->data);
        else if (dfree)
            dfree(d->data);
    }
    free(d);
}

static const MrNativeOps data_ops = { "Data", data_mark, data_free, NULL };

/*
 * The classes that allocate through this layer: those given an alloc
 * function (`rb_define_alloc_func`), those a Data object was wrapped
 * with, and subclasses of either once an instance of them was filled.
 * `func` is NULL after `rb_undef_alloc_func`.
 */
struct capi_alloc {
    VALUE klass;
    rb_alloc_func_t *func;
    int native;
};

static struct capi_alloc *alloc_table;
static size_t alloc_len, alloc_cap;

static struct capi_alloc *
alloc_entry(VALUE klass)
{
    for (size_t i = 0; i < alloc_len; i++)
        if (alloc_table[i].klass == klass)
            return &alloc_table[i];
    return NULL;
}

static struct capi_alloc *
alloc_entry_add(VALUE klass)
{
    struct capi_alloc *e = alloc_entry(klass);
    if (e)
        return e;
    if (alloc_len == alloc_cap) {
        alloc_cap = alloc_cap ? alloc_cap * 2 : 16;
        alloc_table = realloc(alloc_table, alloc_cap * sizeof(*alloc_table));
        if (!alloc_table)
            rb_memerror();
    }
    e = &alloc_table[alloc_len++];
    e->klass = klass;
    e->func = NULL;
    e->native = 0;
    api->gc_pin(ctx(), M(klass));
    return e;
}

/* Make `klass`'s instances native objects (once). */
static void
ensure_native(VALUE klass)
{
    struct capi_alloc *e = alloc_entry_add(klass);
    if (!e->native) {
        check_rc(api->class_set_alloc(ctx(), M(klass), MR_ALLOC_NATIVE));
        e->native = 1;
    }
}

void
rb_define_alloc_func(VALUE klass, rb_alloc_func_t *func)
{
    ensure_native(klass);
    alloc_entry(klass)->func = func;
}

void
rb_undef_alloc_func(VALUE klass)
{
    check_rc(api->class_set_alloc(ctx(), M(klass), MR_ALLOC_UNDEF));
    alloc_entry_add(klass)->func = NULL;
}

rb_alloc_func_t *
rb_get_alloc_func(VALUE klass)
{
    for (VALUE c = klass; c != Qnil; c = rb_class_superclass(c)) {
        struct capi_alloc *e = alloc_entry(c);
        if (e) {
            if (c != klass) {
                /* Remember the inherited one on the subclass itself. */
                struct capi_alloc *sub = alloc_entry_add(klass);
                sub->func = e->func;
            }
            return e->func;
        }
    }
    return NULL;
}

/* The class a Data object is made of: `0` is CRuby's hidden object. */
static VALUE
data_class(VALUE klass)
{
    static VALUE hidden;
    if (!klass) {
        if (!hidden) {
            hidden = rb_class_new(rb_cObject);
            api->gc_pin(ctx(), M(hidden));
        }
        klass = hidden;
    }
    ensure_native(klass);
    return klass;
}

/*
 * The instance whose payload an alloc function is making (data_of): its
 * first Data object of the instance's class is the instance itself, not a
 * fresh one the payload is then moved off. An alloc function that keeps a
 * reference to its object in the payload (liquid-c's `body->self = obj`,
 * which later reads the instance's frozen flag through it) must see the
 * object `allocate` returns. Keyed on the capi frame the alloc function
 * runs in, so a stale entry (the alloc function raised) is not taken by
 * another call.
 */
static struct {
    VALUE obj, klass;
    struct capi_frame *frame;
} pending_alloc;

static VALUE
data_wrap(VALUE klass, void *datap, RUBY_DATA_FUNC dmark, RUBY_DATA_FUNC dfree, const rb_data_type_t *type)
{
    klass = data_class(klass);
    if (pending_alloc.obj && klass == pending_alloc.klass && pending_alloc.frame == frame()) {
        VALUE obj = pending_alloc.obj;
        pending_alloc.obj = 0;
        struct capi_data *d = malloc(sizeof(*d));
        if (!d)
            rb_memerror();
        d->data = datap;
        d->dmark = dmark;
        d->dfree = dfree;
        d->type = type;
        d->basic.flags = 0;
        d->basic.klass = 0;
        if (api->native_set(ctx(), M(obj), d, &data_ops) != 0) {
            free(d);
            capi_unwind();
        }
        return obj;
    }
    struct capi_data *d = malloc(sizeof(*d));
    if (!d)
        rb_memerror();
    d->data = datap;
    d->dmark = dmark;
    d->dfree = dfree;
    d->type = type;
    d->basic.flags = 0;
    d->basic.klass = 0;
    MrValue obj = api->native_new(ctx(), M(klass), d, &data_ops);
    if (obj == MR_UNDEF) {
        free(d);
        capi_unwind();
    }
    return root(R(obj));
}

static const char *
displaying_class_of(VALUE obj)
{
    if (NIL_P(obj)) return "nil";
    if (obj == Qtrue) return "true";
    if (obj == Qfalse) return "false";
    return rb_obj_classname(obj);
}

static int
copy_ivar(ID id, VALUE val, st_data_t to)
{
    rb_ivar_set((VALUE)to, id, val);
    return ST_CONTINUE;
}

/*
 * The record behind `obj`, or NULL (`raise` = 0) / a TypeError when it
 * has none. An instance that `new` / `allocate` / `dup` made is
 * payload-less until something looks at its payload; then the alloc
 * function its class registered runs, as CRuby's `allocate` would have
 * run it, and the record it produced is moved onto the instance.
 */
static struct capi_data *
data_of(VALUE obj, int raise)
{
    struct capi_frame *f = frame(), *owner = f->owner;
    if (obj == owner->self_val && owner->self_data)
        return owner->self_data;
    struct capi_data *d = api->native_data(f->ctx, M(obj), &data_ops);
    if (d) {
        if (obj == owner->self_val)
            owner->self_data = d;
        return d;
    }
    if (api->type_of(ctx(), M(obj)) == MR_T_NATIVE) {
        VALUE klass = rb_obj_class(obj);
        rb_alloc_func_t *alloc = rb_get_alloc_func(klass);
        if (alloc) {
            pending_alloc.obj = obj;
            pending_alloc.klass = klass;
            pending_alloc.frame = frame();
            VALUE tmp = alloc(klass);
            pending_alloc.obj = 0;
            d = api->native_data(ctx(), M(tmp), &data_ops);
            if (d && tmp == obj)
                return d;
            if (!d)
                rb_raise(rb_eTypeError, "allocator of %s did not return a Data object", rb_class2name(klass));
            api->native_take(ctx(), M(tmp));
            check_rc(api->native_set(ctx(), M(obj), d, &data_ops));
            /* What else the alloc function did to its object (SWIG sets
             * the @__swigtype__ its type checks read) moves over too. */
            rb_ivar_foreach(tmp, copy_ivar, (st_data_t)obj);
            return d;
        }
    }
    if (raise)
        rb_raise(rb_eTypeError, "wrong argument type %s (expected Data)", displaying_class_of(obj));
    return NULL;
}

/*
 * A C method's receiver as CRuby's `allocate` would have left it: an
 * instance of a class with an alloc function that nothing has looked at
 * yet gets its payload, and the instance variables the alloc function
 * set, before the method runs, so a C `initialize` finds what the alloc
 * function prepared. Until a C method runs on it the instance stays as
 * `new` / `allocate` made it (doc/c_api.md §6).
 */
static void
materialize_payload(VALUE self)
{
    if (RB_SPECIAL_CONST_P(self))
        return;
    struct capi_frame *f = frame();
    struct capi_data *d = api->native_data(f->ctx, M(self), &data_ops);
    if (d) {
        /* What data_of would look up again for the method's own
         * GetTypedData / DATA_PTR. */
        if (self == f->owner->self_val)
            f->owner->self_data = d;
        return;
    }
    if (api->type_of(f->ctx, M(self)) == MR_T_NATIVE)
        data_of(self, 0);
}

VALUE
rb_data_object_wrap(VALUE klass, void *datap, RUBY_DATA_FUNC dmark, RUBY_DATA_FUNC dfree)
{
    return data_wrap(klass, datap, dmark, dfree, NULL);
}

VALUE
rb_data_object_zalloc(VALUE klass, size_t size, RUBY_DATA_FUNC dmark, RUBY_DATA_FUNC dfree)
{
    return data_wrap(klass, xcalloc(1, size), dmark, dfree, NULL);
}

VALUE
rb_data_object_make(VALUE klass, RUBY_DATA_FUNC dmark, RUBY_DATA_FUNC dfree, void **datap, size_t size)
{
    void *p = xcalloc(1, size);
    *datap = p;
    return data_wrap(klass, p, dmark, dfree, NULL);
}

void *
rb_data_object_get(VALUE obj)
{
    return data_of(obj, 1)->data;
}

VALUE
rb_data_typed_object_wrap(VALUE klass, void *datap, const rb_data_type_t *type)
{
    return data_wrap(klass, datap, NULL, NULL, type);
}

VALUE
rb_data_typed_object_zalloc(VALUE klass, size_t size, const rb_data_type_t *type)
{
    return data_wrap(klass, xcalloc(1, size), NULL, NULL, type);
}

VALUE
rb_data_typed_object_make(VALUE klass, const rb_data_type_t *type, void **datap, size_t size)
{
    void *p = xcalloc(1, size);
    *datap = p;
    return data_wrap(klass, p, NULL, NULL, type);
}

int
rb_typeddata_inherited_p(const rb_data_type_t *child, const rb_data_type_t *parent)
{
    for (; child; child = child->parent)
        if (child == parent)
            return 1;
    return 0;
}

int
rb_typeddata_is_kind_of(VALUE obj, const rb_data_type_t *type)
{
    struct capi_data *d = data_of(obj, 0);
    return d && d->type && rb_typeddata_inherited_p(d->type, type);
}

void *
rb_check_typeddata(VALUE obj, const rb_data_type_t *type)
{
    struct capi_data *d = data_of(obj, 0);
    const char *actual;
    if (!d || !d->type)
        actual = displaying_class_of(obj);
    else if (!rb_typeddata_inherited_p(d->type, type))
        actual = d->type->wrap_struct_name;
    else
        return d->data;
    rb_raise(rb_eTypeError, "wrong argument type %s (expected %s)", actual, type->wrap_struct_name);
}

void *
rb_monoruby_rdata(VALUE obj)
{
    return data_of(obj, 1);
}

/*
 * Object flags. monoruby keeps none of CRuby's flag word, so `RBASIC(obj)`
 * is a view the layer makes up: on a Data / TypedData object it is the
 * record's own `struct RBasic`, whose FL_USER* bits an extension may set
 * and read back (stringio keeps its open-for-read / -write state there);
 * on anything else it is a zeroed scratch copy whose writes go nowhere.
 * Neither carries FL_FREEZE (FL_TEST / OBJ_FROZEN read the frozen state),
 * and `klass` is 0: read the class with `rb_obj_class` / `RBASIC_CLASS`.
 * It is on every StringIO operation's path, so it is one payload lookup.
 */
struct RBasic *
rb_monoruby_rbasic(VALUE obj)
{
    static struct RBasic scratch;
    struct capi_data *d = SPECIAL_CONST_P(obj) ? NULL : data_of(obj, 0);
    if (d)
        return &d->basic;
    scratch.flags = 0;
    scratch.klass = 0;
    return &scratch;
}

VALUE
rb_monoruby_fl_test(VALUE obj, VALUE flags)
{
    if (SPECIAL_CONST_P(obj))
        return 0;
    VALUE f = 0;
    if (flags & ~(VALUE)RUBY_FL_FREEZE) {
        struct capi_data *d = data_of(obj, 0);
        if (d)
            f = d->basic.flags;
    }
    if ((flags & RUBY_FL_FREEZE) && OBJ_FROZEN(obj))
        f |= RUBY_FL_FREEZE;
    return f & flags;
}

void
rb_monoruby_fl_set(VALUE obj, VALUE flags)
{
    if (SPECIAL_CONST_P(obj))
        return;
    if (flags & ~(VALUE)RUBY_FL_FREEZE) {
        struct capi_data *d = data_of(obj, 0);
        if (d)
            d->basic.flags |= flags & ~(VALUE)RUBY_FL_FREEZE;
    }
    if (flags & RUBY_FL_FREEZE)
        rb_obj_freeze(obj);
}

void
rb_monoruby_fl_unset(VALUE obj, VALUE flags)
{
    if (SPECIAL_CONST_P(obj))
        return;
    struct capi_data *d = data_of(obj, 0);
    if (d)
        d->basic.flags &= ~flags;
}

int
rb_monoruby_typeddata_p(VALUE obj)
{
    struct capi_data *d = data_of(obj, 0);
    return d && d->type;
}

void
rb_obj_call_init(VALUE obj, int argc, const VALUE *argv)
{
    funcall_block(obj, rb_intern("initialize"), argc, argv, frame()->block);
}

void
rb_obj_call_init_kw(VALUE obj, int argc, const VALUE *argv, int kw_splat)
{
    funcall_block_kw(obj, rb_intern("initialize"), argc, argv, frame()->block, kw_splat);
}

VALUE
rb_class_new_instance_kw(int argc, const VALUE *argv, VALUE klass, int kw_splat)
{
    VALUE obj = rb_obj_alloc(klass);
    rb_obj_call_init_kw(obj, argc, argv, kw_splat);
    return obj;
}

VALUE
rb_class_new_instance_pass_kw(int argc, const VALUE *argv, VALUE klass)
{
    return rb_class_new_instance_kw(argc, argv, klass, rb_keyword_given_p());
}

VALUE
rb_class_new(VALUE super)
{
    return rb_funcall(rb_cClass, rb_intern("new"), 1, super);
}

VALUE
rb_module_new(void)
{
    return rb_funcall(rb_cModule, rb_intern("new"), 0);
}

VALUE
rb_call_super(int argc, const VALUE *argv)
{
    struct capi_frame *saved = frame();
    MrValue small[CAPI_SMALL_ARGS];
    MrValue r = api->call_super(saved->ctx, argc, mr_args(argc, argv, small), saved->block);
    capi_cur = saved;
    return check(r);
}

VALUE
rb_call_super_kw(int argc, const VALUE *argv, int kw_splat)
{
    (void)kw_splat;
    return rb_call_super(argc, argv);
}

/* ---------------------------------------------------------------------
 * Blocks from C
 * --------------------------------------------------------------------- */

/*
 * A C function as a block: an instance of a hidden class whose `call`
 * runs the function, turned into a Proc with `Method#to_proc`. The
 * function sees the block of the method that created it (`rb_yield`
 * inside an `rb_block_call` callback yields to the caller's block, as
 * in CRuby), kept as a Proc so the callback may outlive that call.
 */
struct capi_block {
    rb_block_call_func_t func;
    VALUE arg;
    VALUE block;
};

static void
block_mark(void *p, MrMarker *marker)
{
    struct capi_block *b = p;
    api->gc_mark(marker, M(b->arg));
    api->gc_mark(marker, M(b->block));
}

static void
block_free(void *p)
{
    free(p);
}

static const MrNativeOps block_ops = { "block", block_mark, block_free, NULL };

static VALUE block_class;
static VALUE break_class;
static __thread VALUE break_value;

static MrValue
block_dispatch(MrContext *c, MrValue self, int argc, const MrValue *argv, MrValue block, void *data)
{
    (void)data;
    struct capi_block *b = api->native_data(c, self, &block_ops);
    struct capi_frame fr;
    struct capi_frame *volatile fp = &fr;
    volatile MrValue result = MR_UNDEF;
    frame_init(&fr, c, NIL_P(b->block) ? MR_UNDEF : M(b->block), NULL, NULL);
    capi_cur = &fr;
    if (_setjmp(fr.jb) == 0) {
        VALUE blockarg = Qnil;
        if (block != MR_UNDEF)
            blockarg = check(api->block_to_proc(c, block));
        VALUE small[CAPI_SMALL_ARGS];
        VALUE *a = rb_args(argc, argv, small);
        VALUE yielded = argc > 0 ? a[0] : Qnil;
        result = M(b->func(yielded, b->arg, argc, a, blockarg));
    }
    free_bufs(fp);
    capi_cur = fp->prev;
    return result;
}

static VALUE
block_proc_new(rb_block_call_func_t func, VALUE arg, VALUE block)
{
    if (!block_class) {
        block_class = rb_class_new(rb_cObject);
        api->gc_pin(ctx(), M(block_class));
        check_rc(api->class_set_alloc(ctx(), M(block_class), MR_ALLOC_NATIVE));
        api->define_method_data(ctx(), M(block_class), "call", block_dispatch, NULL,
                                MR_ARGC_VARIADIC, 0);
        if (api->error_pending(ctx()))
            capi_unwind();
    }
    struct capi_block *b = malloc(sizeof(*b));
    if (!b)
        rb_memerror();
    b->func = func;
    b->arg = arg;
    b->block = block;
    VALUE obj = check(api->native_new(ctx(), M(block_class), b, &block_ops));
    VALUE m = rb_funcall(obj, rb_intern("method"), 1, ID2SYM(rb_intern("call")));
    return rb_funcall(m, rb_intern("to_proc"), 0);
}

/* The current block as a Proc, or nil. */
static VALUE
current_block_proc(void)
{
    return rb_block_given_p() ? rb_block_proc() : Qnil;
}

/*
 * `rb_iter_break` leaves the iteration through a hidden exception that
 * `rb_block_call` / `rb_iterate` catch; it is not a StandardError, so a
 * `rescue` in the iterating method's Ruby code does not see it.
 */
static VALUE
break_exception(void)
{
    if (!break_class) {
        break_class = rb_class_new(rb_eException);
        api->gc_pin(ctx(), M(break_class));
    }
    return break_class;
}

void
rb_iter_break_value(VALUE val)
{
    break_value = val;
    api->gc_pin(ctx(), M(val));
    rb_exc_raise(rb_exc_new_cstr(break_exception(), "break"));
}

void
rb_iter_break(void)
{
    rb_iter_break_value(Qnil);
}

/* A call that may end by `rb_iter_break`: its value then. */
static VALUE
iterate_call(MrValue r)
{
    if (r == MR_UNDEF && break_class) {
        MrValue exc = api->error_take(ctx());
        if (rb_obj_class(R(exc)) == break_class) {
            VALUE v = break_value;
            break_value = Qnil;
            api->gc_unpin(ctx(), M(v));
            return root(v);
        }
        api->raise_exception(ctx(), exc);
    }
    return check(r);
}

VALUE
rb_block_call(VALUE obj, ID mid, int argc, const VALUE *argv, rb_block_call_func_t bl_proc, VALUE data2)
{
    if (!bl_proc)
        return rb_funcall_passing_block(obj, mid, argc, argv);
    VALUE proc = block_proc_new(bl_proc, data2, current_block_proc());
    struct capi_frame *saved = frame();
    MrValue small[CAPI_SMALL_ARGS];
    MrValue r = api->funcall_sym(saved->ctx, M(obj), (MrSym)mid, argc,
                                 mr_args(argc, argv, small), M(proc));
    capi_cur = saved;
    return iterate_call(r);
}

VALUE
rb_block_call_kw(VALUE obj, ID mid, int argc, const VALUE *argv, rb_block_call_func_t bl_proc, VALUE data2, int kw_splat)
{
    (void)kw_splat;
    return rb_block_call(obj, mid, argc, argv, bl_proc, data2);
}

/*
 * catch / throw are Kernel's own: `throw` raises the interpreter's throw
 * error, which unwinds through the C frames like any exception, and
 * `catch` with a matching tag swallows it and returns the thrown value.
 */
VALUE
rb_catch_obj(VALUE tag, rb_block_call_func_t func, VALUE data)
{
    return rb_block_call(rb_mKernel, rb_intern("catch"), 1, &tag, func, data);
}

VALUE
rb_catch(const char *tag, rb_block_call_func_t func, VALUE data)
{
    return rb_catch_obj(ID2SYM(rb_intern(tag)), func, data);
}

void
rb_throw_obj(VALUE tag, VALUE val)
{
    rb_funcall(rb_mKernel, rb_intern("throw"), 2, tag, val);
    rb_bug("rb_throw_obj: throw returned");
}

void
rb_throw(const char *tag, VALUE val)
{
    rb_throw_obj(ID2SYM(rb_intern(tag)), val);
}

struct iterate_args {
    VALUE (*it_proc)(VALUE);
    VALUE data1;
    MrValue saved_block;
};

static VALUE
iterate_body(VALUE a)
{
    struct iterate_args *ia = (struct iterate_args *)a;
    return ia->it_proc(ia->data1);
}

VALUE
rb_iterate(VALUE (*it_proc)(VALUE), VALUE data1, rb_block_call_func_t bl_proc, VALUE data2)
{
    VALUE proc = block_proc_new(bl_proc, data2, current_block_proc());
    struct capi_frame *f = frame();
    struct iterate_args ia = { it_proc, data1, f->block };
    /* The block of the current frame, which `rb_each` /
     * `rb_funcall_passing_block` in `it_proc` pass on. */
    f->block = M(proc);
    int state;
    VALUE r = rb_protect(iterate_body, (VALUE)&ia, &state);
    f->block = ia.saved_block;
    if (state) {
        MrValue exc = M(rb_errinfo());
        rb_set_errinfo(Qnil);
        api->raise_exception(ctx(), exc);
        return iterate_call(MR_UNDEF);
    }
    return r;
}

VALUE
rb_each(VALUE obj)
{
    return rb_funcall_passing_block(obj, rb_intern("each"), 0, NULL);
}

VALUE
rb_proc_new(rb_block_call_func_t func, VALUE callback_arg)
{
    return block_proc_new(func, callback_arg, Qnil);
}

void
rb_gc_mark_locations(const VALUE *start, const VALUE *end)
{
    for (const VALUE *p = start; p < end; p++)
        rb_gc_mark(*p);
}

/* ---------------------------------------------------------------------
 * Struct
 * --------------------------------------------------------------------- */

static VALUE
struct_define(VALUE outer, const char *name, va_list ap)
{
    VALUE args = rb_ary_new();
    if (name && outer == Qundef)
        rb_ary_push(args, rb_str_new_cstr(name));
    for (const char *m; (m = va_arg(ap, const char *)) != NULL;)
        rb_ary_push(args, ID2SYM(rb_intern(m)));
    VALUE klass = rb_apply(rb_cStruct, rb_intern("new"), args);
    if (name && outer != Qundef)
        rb_define_const(outer, name, klass);
    return klass;
}

VALUE
rb_struct_define(const char *name, ...)
{
    va_list ap;
    va_start(ap, name);
    VALUE klass = struct_define(Qundef, name, ap);
    va_end(ap);
    return klass;
}

VALUE
rb_struct_define_under(VALUE outer, const char *name, ...)
{
    va_list ap;
    va_start(ap, name);
    VALUE klass = struct_define(outer, name, ap);
    va_end(ap);
    return klass;
}

VALUE
rb_struct_s_members(VALUE klass)
{
    return rb_funcall(klass, rb_intern("members"), 0);
}

VALUE
rb_struct_members(VALUE st)
{
    return rb_funcall(st, rb_intern("members"), 0);
}

VALUE
rb_struct_new(VALUE klass, ...)
{
    long n = RARRAY_LEN(rb_struct_s_members(klass));
    VALUE *argv = ALLOCA_N(VALUE, n > 0 ? n : 1);
    va_list ap;
    va_start(ap, klass);
    for (long i = 0; i < n; i++)
        argv[i] = va_arg(ap, VALUE);
    va_end(ap);
    return rb_class_new_instance((int)n, argv, klass);
}

VALUE
rb_struct_alloc(VALUE klass, VALUE values)
{
    return rb_apply(klass, rb_intern("new"), values);
}

VALUE
rb_struct_initialize(VALUE self, VALUE values)
{
    rb_apply(self, rb_intern("initialize"), values);
    return Qnil;
}

VALUE
rb_struct_aref(VALUE st, VALUE idx)
{
    return rb_funcall(st, rb_intern("[]"), 1, idx);
}

VALUE
rb_struct_aset(VALUE st, VALUE idx, VALUE val)
{
    return rb_funcall(st, rb_intern("[]="), 2, idx, val);
}

VALUE
rb_struct_getmember(VALUE st, ID id)
{
    return rb_struct_aref(st, ID2SYM(id));
}

VALUE
rb_struct_size(VALUE st)
{
    return rb_funcall(st, rb_intern("size"), 0);
}

/* ---- stage 4: what bcrypt, msgpack and json asked for ------------------ */

void
rb_ext_ractor_safe(bool flag)
{
    (void)flag;
}

/* ---- Bignum ----------------------------------------------------------- */

long long
rb_big2ll(VALUE big)
{
    return rb_num2ll(big);
}

unsigned long long
rb_big2ull(VALUE big)
{
    return rb_num2ull(big);
}

long
rb_big2long(VALUE big)
{
    return rb_num2long(big);
}

unsigned long
rb_big2ulong(VALUE big)
{
    return rb_num2ulong(big);
}

double
rb_big2dbl(VALUE big)
{
    return rb_num2dbl(big);
}

VALUE
rb_big2str(VALUE big, int base)
{
    return rb_funcall(big, rb_intern("to_s"), 1, INT2FIX(base));
}

VALUE
rb_dbl2big(double d)
{
    return rb_funcall(rb_float_new(d), rb_intern("to_i"), 0);
}

#define BIG_BINOP(name, op) \
    VALUE name(VALUE x, VALUE y) { return rb_funcall(x, rb_intern(op), 1, y); }
BIG_BINOP(rb_big_cmp, "<=>")
BIG_BINOP(rb_big_eq, "==")
BIG_BINOP(rb_big_plus, "+")
BIG_BINOP(rb_big_minus, "-")
BIG_BINOP(rb_big_mul, "*")
BIG_BINOP(rb_big_div, "/")
BIG_BINOP(rb_big_modulo, "%")
BIG_BINOP(rb_big_pow, "**")
BIG_BINOP(rb_big_and, "&")
BIG_BINOP(rb_big_or, "|")
BIG_BINOP(rb_big_xor, "^")
BIG_BINOP(rb_big_lshift, "<<")
BIG_BINOP(rb_big_rshift, ">>")
#undef BIG_BINOP

VALUE
rb_big_norm(VALUE x)
{
    return x;
}

int
rb_monoruby_integer_positive_p(VALUE i)
{
    if (FIXNUM_P(i))
        return FIX2LONG(i) >= 0;
    return !RTEST(rb_funcall(i, rb_intern("negative?"), 0));
}

size_t
rb_absint_size(VALUE val, int *nlz_bits_ret)
{
    VALUE abs = rb_funcall(val, rb_intern("abs"), 0);
    long bits = NUM2LONG(rb_funcall(abs, rb_intern("bit_length"), 0));
    size_t size = (size_t)((bits + 7) / 8);
    if (nlz_bits_ret)
        *nlz_bits_ret = (int)(size * 8 - (size_t)bits);
    return size;
}

/* bignum.c's rb_integer_pack: the low numwords * wordsize bytes of val
 * (two's complement with INTEGER_PACK_2COMP, the absolute value
 * otherwise) in the requested word and byte order. Returns val's sign,
 * doubled when its absolute value does not fit. */
int
rb_integer_pack(VALUE val, void *words, size_t numwords, size_t wordsize, size_t nails, int flags)
{
    if (nails != 0)
        rb_raise(rb_eNotImpError, "rb_integer_pack: nails are not supported");
    val = rb_to_int(val);
    size_t n = numwords * wordsize;
    int sign = FIXNUM_P(val) ? (FIX2LONG(val) > 0) - (FIX2LONG(val) < 0)
                             : NUM2INT(rb_funcall(val, rb_intern("<=>"), 1, INT2FIX(0)));
    VALUE abs = rb_funcall(val, rb_intern("abs"), 0);
    VALUE limit = rb_funcall(INT2FIX(1), rb_intern("<<"), 1, SIZET2NUM(n * 8));
    int cmp = NUM2INT(rb_funcall(abs, rb_intern("<=>"), 1, limit));
    int overflow = cmp > 0 || (cmp == 0 && !(sign < 0 && (flags & INTEGER_PACK_2COMP)));
    VALUE src = (flags & INTEGER_PACK_2COMP) ? val : abs;
    VALUE mask = rb_funcall(limit, rb_intern("-"), 1, INT2FIX(1));
    VALUE bytes = rb_funcall(rb_funcall(src, rb_intern("&"), 1, mask), rb_intern("digits"), 1, INT2FIX(256));
    long have = RARRAY_LEN(bytes);
    unsigned char *out = (unsigned char *)words;
    int msword = flags & INTEGER_PACK_MSWORD_FIRST;
    int msbyte = (flags & INTEGER_PACK_MSBYTE_FIRST)
        || ((flags & INTEGER_PACK_NATIVE_BYTE_ORDER) && __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__);
    for (size_t i = 0; i < n; i++) {
        size_t w = i / wordsize, b = i % wordsize;
        unsigned char byte = (long)i < have ? (unsigned char)NUM2INT(rb_ary_entry(bytes, (long)i)) : 0;
        size_t wi = msword ? numwords - 1 - w : w;
        size_t bi = msbyte ? wordsize - 1 - b : b;
        out[wi * wordsize + bi] = byte;
    }
    return overflow ? sign * 2 : sign;
}

int
rb_absint_singlebit_p(VALUE val)
{
    VALUE abs = rb_funcall(val, rb_intern("abs"), 0);
    if (!RTEST(rb_funcall(abs, rb_intern(">"), 1, INT2FIX(0))))
        return 0;
    VALUE below = rb_funcall(abs, rb_intern("-"), 1, INT2FIX(1));
    VALUE and = rb_funcall(abs, rb_intern("&"), 1, below);
    return RTEST(rb_funcall(and, rb_intern("=="), 1, INT2FIX(0)));
}

/* ---- Proc / IO -------------------------------------------------------- */

int
rb_proc_arity(VALUE proc)
{
    return NUM2INT(rb_funcall(proc, rb_intern("arity"), 0));
}

VALUE
rb_proc_lambda_p(VALUE proc)
{
    return rb_funcall(proc, rb_intern("lambda?"), 0);
}

VALUE
rb_io_write(VALUE io, VALUE str)
{
    return rb_funcall(io, rb_intern("write"), 1, str);
}

VALUE
rb_io_flush(VALUE io)
{
    return rb_funcall(io, rb_intern("flush"), 0);
}

/* io.c's rb_io_puts / rb_io_print / rb_io_printf: written over `write`
 * alone, so they serve any object that has one, as CRuby's do. An
 * extension's own `puts` (StringIO's, through IO::generic_writable) is
 * this function, so forwarding to `io.puts` would recurse. */
static VALUE
io_puts_ary(VALUE ary, VALUE out, int recur)
{
    if (recur) {
        VALUE s = rb_str_new_cstr("[...]");
        return rb_io_puts(1, &s, out);
    }
    for (long i = 0; i < RARRAY_LEN(ary); i++) {
        VALUE e = rb_ary_entry(ary, i);
        rb_io_puts(1, &e, out);
    }
    return Qtrue;
}

static VALUE
io_puts_ary_protected(VALUE argp)
{
    VALUE *args = (VALUE *)argp;
    return io_puts_ary(args[0], args[1], 0);
}

static int
io_puts_on_stack(VALUE ary, VALUE *stack, int depth)
{
    for (int i = 0; i < depth; i++)
        if (stack[i] == ary)
            return 1;
    return 0;
}

/* The arrays being printed, innermost last: a nested reference to one of
 * them prints as "[...]" (rb_exec_recursive's protocol for io_puts_ary). */
static VALUE io_puts_stack[64];
static int io_puts_depth;

VALUE
rb_io_puts(int argc, const VALUE *argv, VALUE out)
{
    if (argc == 0) {
        rb_io_write(out, rb_default_rs);
        return Qnil;
    }
    for (int i = 0; i < argc; i++) {
        VALUE line = rb_check_array_type(argv[i]);
        if (!NIL_P(line)) {
            if (io_puts_on_stack(line, io_puts_stack, io_puts_depth)) {
                io_puts_ary(line, out, 1);
            }
            else if (io_puts_depth < (int)(sizeof(io_puts_stack) / sizeof(io_puts_stack[0]))) {
                int state = 0;
                io_puts_stack[io_puts_depth++] = line;
                VALUE args[2] = {line, out};
                rb_protect(io_puts_ary_protected, (VALUE)args, &state);
                io_puts_depth--;
                if (state)
                    rb_jump_tag(state);
            }
            else {
                io_puts_ary(line, out, 0);
            }
            continue;
        }
        line = rb_obj_as_string(argv[i]);
        long len = RSTRING_LEN(line);
        if (len == 0 || RSTRING_PTR(line)[len - 1] != '\n') {
            VALUE args[2] = {line, rb_default_rs};
            rb_funcallv(out, rb_intern("write"), 2, args);
        }
        else {
            rb_io_write(out, line);
        }
    }
    return Qnil;
}

VALUE
rb_io_print(int argc, const VALUE *argv, VALUE out)
{
    VALUE line;
    if (argc == 0) {
        argc = 1;
        line = rb_lastline_get();
        argv = &line;
    }
    VALUE ofs = rb_gv_get("$,");
    VALUE ors = rb_output_rs;
    if (argc > 1 && !NIL_P(ofs))
        rb_warn("$, is set to non-nil value");
    for (int i = 0; i < argc; i++) {
        if (!NIL_P(ofs) && i > 0)
            rb_io_write(out, ofs);
        rb_io_write(out, argv[i]);
    }
    if (argc > 0 && !NIL_P(ors))
        rb_io_write(out, ors);
    return Qnil;
}

VALUE
rb_f_sprintf(int argc, const VALUE *argv)
{
    return rb_funcallv(rb_mKernel, rb_intern("format"), argc, argv);
}

VALUE
rb_io_printf(int argc, const VALUE *argv, VALUE out)
{
    rb_io_write(out, rb_f_sprintf(argc, argv));
    return Qnil;
}

VALUE
rb_io_gets(VALUE io)
{
    return rb_funcall(io, rb_intern("gets"), 0);
}

VALUE
rb_io_getbyte(VALUE io)
{
    return rb_funcall(io, rb_intern("getbyte"), 0);
}

VALUE
rb_io_close(VALUE io)
{
    return rb_funcall(io, rb_intern("close"), 0);
}

VALUE
rb_io_eof(VALUE io)
{
    return rb_funcall(io, rb_intern("eof?"), 0);
}

VALUE
rb_io_binmode(VALUE io)
{
    return rb_funcall(io, rb_intern("binmode"), 0);
}

VALUE
rb_io_addstr(VALUE io, VALUE str)
{
    rb_io_write(io, str);
    return io;
}

VALUE
rb_io_get_io(VALUE io)
{
    return rb_convert_type(io, T_FILE, "IO", "to_io");
}

VALUE
rb_io_check_io(VALUE io)
{
    return rb_check_convert_type(io, T_FILE, "IO", "to_io");
}

VALUE
rb_io_taint_check(VALUE io)
{
    rb_check_frozen(io);
    return io;
}

/* ---- io.c's mode and encoding parsing -----------------------------------
 * Ports of io.c's rb_io_modestr_fmode, rb_io_oflags_fmode,
 * rb_io_extract_encoding_option and rb_io_extract_modeenc (with their
 * helpers), so an extension that opens a stream of its own (stringio)
 * takes a mode string, `mode:` / `flags:` / `binmode:` / `textmode:` and
 * the encoding options exactly as IO.new does. */

static const char bom_prefix[] = "bom|";
static const char utf_prefix[] = "utf-";
enum { bom_prefix_len = (int)sizeof(bom_prefix) - 1 };
enum { utf_prefix_len = (int)sizeof(utf_prefix) - 1 };

static int
io_encname_bom_p(const char *name, long len)
{
    return len > bom_prefix_len && strncasecmp(name, bom_prefix, bom_prefix_len) == 0;
}

int
rb_io_modestr_fmode(const char *modestr)
{
    int fmode = 0;
    const char *m = modestr, *p = NULL;

    switch (*m++) {
      case 'r':
        fmode |= FMODE_READABLE;
        break;
      case 'w':
        fmode |= FMODE_WRITABLE | FMODE_TRUNC | FMODE_CREATE;
        break;
      case 'a':
        fmode |= FMODE_WRITABLE | FMODE_APPEND | FMODE_CREATE;
        break;
      default:
        goto error;
    }

    while (*m) {
        switch (*m++) {
          case 'b':
            fmode |= FMODE_BINMODE;
            break;
          case 't':
            fmode |= FMODE_TEXTMODE;
            break;
          case '+':
            fmode |= FMODE_READWRITE;
            break;
          case 'x':
            if (modestr[0] != 'w')
                goto error;
            fmode |= FMODE_EXCL;
            break;
          default:
            goto error;
          case ':':
            p = strchr(m, ':');
            if (io_encname_bom_p(m, p ? (long)(p - m) : (long)strlen(m)))
                fmode |= FMODE_SETENC_BY_BOM;
            goto finished;
        }
    }

  finished:
    if ((fmode & FMODE_BINMODE) && (fmode & FMODE_TEXTMODE))
        goto error;

    return fmode;

  error:
    rb_raise(rb_eArgError, "invalid access mode %s", modestr);
}

int
rb_io_oflags_fmode(int oflags)
{
    int fmode = 0;

    switch (oflags & O_ACCMODE) {
      case O_RDONLY:
        fmode = FMODE_READABLE;
        break;
      case O_WRONLY:
        fmode = FMODE_WRITABLE;
        break;
      case O_RDWR:
        fmode = FMODE_READWRITE;
        break;
    }
    if (oflags & O_APPEND)
        fmode |= FMODE_APPEND;
    if (oflags & O_TRUNC)
        fmode |= FMODE_TRUNC;
    if (oflags & O_CREAT)
        fmode |= FMODE_CREATE;
    if (oflags & O_EXCL)
        fmode |= FMODE_EXCL;
    return fmode;
}

static int
io_fmode_oflags(int fmode)
{
    int oflags = 0;

    switch (fmode & FMODE_READWRITE) {
      case FMODE_READABLE:
        oflags |= O_RDONLY;
        break;
      case FMODE_WRITABLE:
        oflags |= O_WRONLY;
        break;
      case FMODE_READWRITE:
        oflags |= O_RDWR;
        break;
    }
    if (fmode & FMODE_APPEND)
        oflags |= O_APPEND;
    if (fmode & FMODE_TRUNC)
        oflags |= O_TRUNC;
    if (fmode & FMODE_CREATE)
        oflags |= O_CREAT;
    if (fmode & FMODE_EXCL)
        oflags |= O_EXCL;
    return oflags;
}

int
rb_io_modestr_oflags(const char *modestr)
{
    return io_fmode_oflags(rb_io_modestr_fmode(modestr));
}

/* NULL => the default encoding; Qnil => none given (internal only). */
static void
io_ext_int_to_encs(rb_encoding *ext, rb_encoding *intern, rb_encoding **enc, rb_encoding **enc2, int fmode)
{
    int default_ext = 0;

    if (ext == NULL) {
        ext = rb_default_external_encoding();
        default_ext = 1;
    }
    if (rb_is_ascii8bit_enc(ext)) {
        intern = NULL;
    }
    else if (intern == NULL) {
        intern = rb_default_internal_encoding();
    }
    if (intern == NULL || intern == (rb_encoding *)Qnil ||
        (!(fmode & FMODE_SETENC_BY_BOM) && (intern == ext))) {
        *enc = (default_ext && intern != ext) ? NULL : ext;
        *enc2 = NULL;
    }
    else {
        *enc = intern;
        *enc2 = ext;
    }
}

static void
unsupported_encoding(const char *name, rb_encoding *enc)
{
    rb_enc_warn(enc, "Unsupported encoding %s ignored", name);
}

static void
parse_mode_enc(const char *estr, rb_encoding *estr_enc,
               rb_encoding **enc_p, rb_encoding **enc2_p, int *fmode_p)
{
    const char *p;
    char encname[ENCODING_MAXNAMELEN + 1];
    int idx, idx2;
    int fmode = fmode_p ? *fmode_p : 0;
    rb_encoding *ext_enc, *int_enc;
    long len;

    p = strrchr(estr, ':');
    len = p ? (p++ - estr) : (long)strlen(estr);
    if ((fmode & FMODE_SETENC_BY_BOM) || io_encname_bom_p(estr, len)) {
        estr += bom_prefix_len;
        len -= bom_prefix_len;
        if (!strncasecmp(estr, utf_prefix, utf_prefix_len)) {
            fmode |= FMODE_SETENC_BY_BOM;
        }
        else {
            rb_enc_warn(estr_enc, "BOM with non-UTF encoding %s is nonsense", estr);
            fmode &= ~FMODE_SETENC_BY_BOM;
        }
    }
    if (len == 0 || len > ENCODING_MAXNAMELEN) {
        idx = -1;
    }
    else {
        if (p) {
            memcpy(encname, estr, len);
            encname[len] = '\0';
            estr = encname;
        }
        idx = rb_enc_find_index(estr);
    }
    if (fmode_p)
        *fmode_p = fmode;

    if (idx >= 0) {
        ext_enc = rb_enc_from_index(idx);
    }
    else {
        if (idx != -2)
            unsupported_encoding(estr, estr_enc);
        ext_enc = NULL;
    }

    int_enc = NULL;
    if (p) {
        if (*p == '-' && *(p + 1) == '\0') {
            int_enc = (rb_encoding *)Qnil;
        }
        else {
            idx2 = rb_enc_find_index(p);
            if (idx2 < 0)
                unsupported_encoding(p, estr_enc);
            else if (!(fmode & FMODE_SETENC_BY_BOM) && (idx2 == idx))
                int_enc = (rb_encoding *)Qnil;
            else
                int_enc = rb_enc_from_index(idx2);
        }
    }

    io_ext_int_to_encs(ext_enc, int_enc, enc_p, enc2_p, fmode);
}

int
rb_io_extract_encoding_option(VALUE opt, rb_encoding **enc_p, rb_encoding **enc2_p, int *fmode_p)
{
    VALUE encoding = Qnil, extenc = Qundef, intenc = Qundef, tmp;
    int extracted = 0;
    rb_encoding *extencoding = NULL;
    rb_encoding *intencoding = NULL;

    if (!NIL_P(opt)) {
        VALUE v;
        v = rb_hash_lookup2(opt, ID2SYM(rb_intern("encoding")), Qnil);
        if (v != Qnil) encoding = v;
        v = rb_hash_lookup2(opt, ID2SYM(rb_intern("external_encoding")), Qundef);
        if (v != Qnil) extenc = v;
        v = rb_hash_lookup2(opt, ID2SYM(rb_intern("internal_encoding")), Qundef);
        if (!UNDEF_P(v)) intenc = v;
    }
    if ((!UNDEF_P(extenc) || !UNDEF_P(intenc)) && !NIL_P(encoding)) {
        if (!NIL_P(rb_gv_get("$VERBOSE"))) {
            int idx = rb_to_encoding_index(encoding);
            if (idx >= 0) encoding = rb_enc_from_encoding(rb_enc_from_index(idx));
            rb_warn("Ignoring encoding parameter '%"PRIsVALUE"': %s_encoding is used",
                    encoding, UNDEF_P(extenc) ? "internal" : "external");
        }
        encoding = Qnil;
    }
    if (!UNDEF_P(extenc) && !NIL_P(extenc)) {
        extencoding = rb_to_encoding(extenc);
    }
    if (!UNDEF_P(intenc)) {
        if (NIL_P(intenc)) {
            intencoding = (rb_encoding *)Qnil;
        }
        else if (!NIL_P(tmp = rb_check_string_type(intenc))) {
            char *p = StringValueCStr(tmp);
            if (*p == '-' && *(p + 1) == '\0')
                intencoding = (rb_encoding *)Qnil;
            else
                intencoding = rb_to_encoding(intenc);
        }
        else {
            intencoding = rb_to_encoding(intenc);
        }
        if (extencoding == intencoding)
            intencoding = (rb_encoding *)Qnil;
    }
    if (!NIL_P(encoding)) {
        extracted = 1;
        if (!NIL_P(tmp = rb_check_string_type(encoding)))
            parse_mode_enc(StringValueCStr(tmp), rb_enc_get(tmp), enc_p, enc2_p, fmode_p);
        else
            io_ext_int_to_encs(rb_to_encoding(encoding), NULL, enc_p, enc2_p, 0);
    }
    else if (!UNDEF_P(extenc) || !UNDEF_P(intenc)) {
        extracted = 1;
        io_ext_int_to_encs(extencoding, intencoding, enc_p, enc2_p, 0);
    }
    return extracted;
}

static void
validate_enc_binmode(int *fmode_p, int ecflags, rb_encoding *enc, rb_encoding *enc2)
{
    int fmode = *fmode_p;

    if ((fmode & FMODE_READABLE) && !enc2 && !(fmode & FMODE_BINMODE) &&
        !rb_enc_asciicompat(enc ? enc : rb_default_external_encoding()))
        rb_raise(rb_eArgError, "ASCII incompatible encoding needs binmode");

    if ((fmode & FMODE_BINMODE) && (ecflags & ECONV_NEWLINE_DECORATOR_MASK))
        rb_raise(rb_eArgError, "newline decorator with binary mode");
    if (!(fmode & FMODE_BINMODE) && (ecflags & ECONV_NEWLINE_DECORATOR_MASK)) {
        fmode |= FMODE_TEXTMODE;
        *fmode_p = fmode;
    }
    else if (!(ecflags & ECONV_NEWLINE_DECORATOR_MASK)) {
        fmode &= ~FMODE_TEXTMODE;
        *fmode_p = fmode;
    }
}

static void
extract_binmode(VALUE opthash, int *fmode)
{
    if (!NIL_P(opthash)) {
        VALUE v;
        v = rb_hash_aref(opthash, ID2SYM(rb_intern("textmode")));
        if (!NIL_P(v)) {
            if (*fmode & FMODE_TEXTMODE)
                rb_raise(rb_eArgError, "textmode specified twice");
            if (*fmode & FMODE_BINMODE)
                rb_raise(rb_eArgError, "both textmode and binmode specified");
            if (RTEST(v))
                *fmode |= FMODE_TEXTMODE;
        }
        v = rb_hash_aref(opthash, ID2SYM(rb_intern("binmode")));
        if (!NIL_P(v)) {
            if (*fmode & FMODE_BINMODE)
                rb_raise(rb_eArgError, "binmode specified twice");
            if (*fmode & FMODE_TEXTMODE)
                rb_raise(rb_eArgError, "both textmode and binmode specified");
            if (RTEST(v))
                *fmode |= FMODE_BINMODE;
        }
        if ((*fmode & FMODE_BINMODE) && (*fmode & FMODE_TEXTMODE))
            rb_raise(rb_eArgError, "both textmode and binmode specified");
    }
}

void
rb_io_extract_modeenc(VALUE *vmode_p, VALUE *vperm_p, VALUE opthash,
                      int *oflags_p, int *fmode_p, struct rb_io_encoding *convconfig_p)
{
    VALUE vmode;
    int oflags;
    int fmode;
    rb_encoding *enc, *enc2;
    int ecflags;
    VALUE ecopts;
    int has_enc = 0, has_vmode = 0;
    VALUE intmode;

    vmode = *vmode_p;

    io_ext_int_to_encs(NULL, NULL, &enc, &enc2, 0);

  vmode_handle:
    if (NIL_P(vmode)) {
        fmode = FMODE_READABLE;
        oflags = O_RDONLY;
    }
    else if (!NIL_P(intmode = rb_check_to_integer(vmode, "to_int"))) {
        vmode = intmode;
        oflags = NUM2INT(intmode);
        fmode = rb_io_oflags_fmode(oflags);
    }
    else {
        const char *p;

        StringValue(vmode);
        p = StringValueCStr(vmode);
        fmode = rb_io_modestr_fmode(p);
        oflags = io_fmode_oflags(fmode);
        p = strchr(p, ':');
        if (p) {
            has_enc = 1;
            parse_mode_enc(p + 1, rb_enc_get(vmode), &enc, &enc2, &fmode);
        }
        else {
            rb_encoding *e = (fmode & FMODE_BINMODE) ? rb_ascii8bit_encoding() : NULL;
            io_ext_int_to_encs(e, NULL, &enc, &enc2, fmode);
        }
    }

    if (NIL_P(opthash)) {
        ecflags = 0;
        if (enc2 && (fmode & FMODE_READABLE) && !(fmode & FMODE_BINMODE))
            ; /* SET_UNIVERSAL_NEWLINE_DECORATOR_IF_ENC2: Windows only */
        ecopts = Qnil;
        if (fmode & FMODE_BINMODE) {
            if (!has_enc)
                io_ext_int_to_encs(rb_ascii8bit_encoding(), NULL, &enc, &enc2, fmode);
        }
    }
    else {
        VALUE v;
        if (!has_vmode) {
            v = rb_hash_aref(opthash, ID2SYM(rb_intern("mode")));
            if (!NIL_P(v)) {
                if (!NIL_P(vmode))
                    rb_raise(rb_eArgError, "mode specified twice");
                has_vmode = 1;
                vmode = v;
                goto vmode_handle;
            }
        }
        v = rb_hash_aref(opthash, ID2SYM(rb_intern("flags")));
        if (!NIL_P(v)) {
            v = rb_to_int(v);
            oflags |= NUM2INT(v);
            vmode = INT2NUM(oflags);
            fmode = rb_io_oflags_fmode(oflags);
        }
        extract_binmode(opthash, &fmode);
        if (fmode & FMODE_BINMODE) {
            if (!has_enc)
                io_ext_int_to_encs(rb_ascii8bit_encoding(), NULL, &enc, &enc2, fmode);
        }
        v = rb_hash_aref(opthash, ID2SYM(rb_intern("perm")));
        if (!NIL_P(v)) {
            if (vperm_p) {
                if (!NIL_P(*vperm_p))
                    rb_raise(rb_eArgError, "perm specified twice");
                *vperm_p = v;
            }
        }
        ecflags = 0;
        if (rb_io_extract_encoding_option(opthash, &enc, &enc2, &fmode)) {
            if (has_enc)
                rb_raise(rb_eArgError, "encoding specified twice");
        }
        ecflags = rb_econv_prepare_options(opthash, &ecopts, ecflags);
    }

    validate_enc_binmode(&fmode, ecflags, enc, enc2);

    *vmode_p = vmode;
    *oflags_p = oflags;
    *fmode_p = fmode;
    convconfig_p->enc = enc;
    convconfig_p->enc2 = enc2;
    convconfig_p->ecflags = ecflags;
    convconfig_p->ecopts = ecopts;
}

/* transcode.c's econv_opts + rb_econv_prepare_options: the converter
 * option keys as flags, and the `replace:` / `fallback:` values as a
 * frozen hash (Qnil when there are none). */
static int
econv_opts(VALUE opt, int ecflags)
{
    VALUE v;
    int newlineflag = 0;
    VALUE sym_replace = ID2SYM(rb_intern("replace"));

    v = rb_hash_aref(opt, ID2SYM(rb_intern("invalid")));
    if (NIL_P(v))
        ;
    else if (v == sym_replace)
        ecflags |= ECONV_INVALID_REPLACE;
    else
        rb_raise(rb_eArgError, "unknown value for invalid character option");

    v = rb_hash_aref(opt, ID2SYM(rb_intern("undef")));
    if (NIL_P(v))
        ;
    else if (v == sym_replace)
        ecflags |= ECONV_UNDEF_REPLACE;
    else
        rb_raise(rb_eArgError, "unknown value for undefined character option");

    v = rb_hash_aref(opt, sym_replace);
    if (!NIL_P(v) && !(ecflags & ECONV_INVALID_REPLACE))
        ecflags |= ECONV_UNDEF_REPLACE;

    v = rb_hash_aref(opt, ID2SYM(rb_intern("xml")));
    if (!NIL_P(v)) {
        if (v == ID2SYM(rb_intern("text")))
            ecflags |= ECONV_XML_TEXT_DECORATOR | ECONV_UNDEF_HEX_CHARREF;
        else if (v == ID2SYM(rb_intern("attr")))
            ecflags |= ECONV_XML_ATTR_CONTENT_DECORATOR | ECONV_XML_ATTR_QUOTE_DECORATOR | ECONV_UNDEF_HEX_CHARREF;
        else if (SYMBOL_P(v))
            rb_raise(rb_eArgError, "unexpected value for xml option: %"PRIsVALUE, rb_sym2str(v));
        else
            rb_raise(rb_eArgError, "unexpected value for xml option");
    }

    v = rb_hash_aref(opt, ID2SYM(rb_intern("newline")));
    if (!NIL_P(v)) {
        newlineflag = 2;
        ecflags &= ~ECONV_NEWLINE_DECORATOR_MASK;
        if (v == ID2SYM(rb_intern("universal")))
            ecflags |= ECONV_UNIVERSAL_NEWLINE_DECORATOR;
        else if (v == ID2SYM(rb_intern("crlf")))
            ecflags |= ECONV_CRLF_NEWLINE_DECORATOR;
        else if (v == ID2SYM(rb_intern("cr")))
            ecflags |= ECONV_CR_NEWLINE_DECORATOR;
        else if (v == ID2SYM(rb_intern("lf")))
            ecflags |= ECONV_LF_NEWLINE_DECORATOR;
        else if (SYMBOL_P(v))
            rb_raise(rb_eArgError, "unexpected value for newline option: %"PRIsVALUE, rb_sym2str(v));
        else
            rb_raise(rb_eArgError, "unexpected value for newline option");
    }
    {
        int setflags = 0;
        static const struct { const char *key; int flag; } keys[] = {
            {"universal_newline", ECONV_UNIVERSAL_NEWLINE_DECORATOR},
            {"crlf_newline", ECONV_CRLF_NEWLINE_DECORATOR},
            {"cr_newline", ECONV_CR_NEWLINE_DECORATOR},
            {"lf_newline", ECONV_LF_NEWLINE_DECORATOR},
        };
        for (size_t i = 0; i < sizeof(keys) / sizeof(keys[0]); i++) {
            v = rb_hash_aref(opt, ID2SYM(rb_intern(keys[i].key)));
            if (RTEST(v))
                setflags |= keys[i].flag;
            newlineflag |= !NIL_P(v);
        }
        switch (newlineflag) {
          case 1:
            ecflags &= ~ECONV_NEWLINE_DECORATOR_MASK;
            ecflags |= setflags;
            break;
          case 3:
            rb_warning(":newline option precedes other newline options");
            break;
        }
    }
    return ecflags;
}

int
rb_econv_prepare_options(VALUE opthash, VALUE *opts, int ecflags)
{
    VALUE newhash = Qnil;
    VALUE v;

    if (NIL_P(opthash)) {
        *opts = Qnil;
        return ecflags;
    }
    ecflags = econv_opts(opthash, ecflags);

    v = rb_hash_aref(opthash, ID2SYM(rb_intern("replace")));
    if (!NIL_P(v)) {
        StringValue(v);
        if (!RTEST(rb_funcall(v, rb_intern("valid_encoding?"), 0))) {
            VALUE dumped = rb_funcall(v, rb_intern("dump"), 0);
            rb_raise(rb_eArgError, "replacement string is broken: %s as %s",
                     StringValueCStr(dumped), rb_enc_name(rb_enc_get(v)));
        }
        v = rb_str_new_frozen(v);
        newhash = rb_hash_new();
        rb_hash_aset(newhash, ID2SYM(rb_intern("replace")), v);
    }
    v = rb_hash_aref(opthash, ID2SYM(rb_intern("fallback")));
    if (!NIL_P(v)) {
        VALUE h = rb_check_hash_type(v);
        if (NIL_P(h) ? (RTEST(rb_obj_is_proc(v)) || RTEST(rb_obj_is_method(v)) || rb_respond_to(v, rb_intern("[]")))
                     : (v = h, 1)) {
            if (NIL_P(newhash))
                newhash = rb_hash_new();
            rb_hash_aset(newhash, ID2SYM(rb_intern("fallback")), v);
        }
    }
    if (!NIL_P(newhash))
        rb_hash_freeze(newhash);
    *opts = newhash;
    return ecflags;
}

int
rb_econv_prepare_opts(VALUE opthash, VALUE *opts)
{
    return rb_econv_prepare_options(opthash, opts, 0);
}

void
rb_enc_warn(rb_encoding *enc, const char *fmt, ...)
{
    (void)enc;
    va_list ap;
    va_start(ap, fmt);
    vwarn(fmt, ap);
    va_end(ap);
}

VALUE
rb_str_replace(VALUE str, VALUE str2)
{
    return rb_funcall(str, rb_intern("replace"), 1, str2);
}

/* CRuby's fstring table deduplicates; `String#-@` is monoruby's
 * equivalent (a frozen, deduplicated copy). */
VALUE
rb_str_to_interned_str(VALUE str)
{
    return rb_funcall(str, rb_intern("-@"), 0);
}

/* ---- encoding --------------------------------------------------------- */

int
rb_enc_str_coderange(VALUE str)
{
    if (HAVE_API(str_coderange) && RB_TYPE_P(str, T_STRING)) {
        int cr = api->str_coderange(ctx(), M(str));
        if (cr < 0)
            capi_unwind();
        return cr == 1 ? ENC_CODERANGE_7BIT : cr == 2 ? ENC_CODERANGE_VALID : ENC_CODERANGE_BROKEN;
    }
    if (!RTEST(rb_funcall(str, rb_intern("valid_encoding?"), 0)))
        return ENC_CODERANGE_BROKEN;
    if (RTEST(rb_funcall(str, rb_intern("ascii_only?"), 0)))
        return ENC_CODERANGE_7BIT;
    return ENC_CODERANGE_VALID;
}

ID
rb_intern3(const char *name, long len, rb_encoding *enc)
{
    (void)enc;
    return rb_intern2(name, len);
}

VALUE
rb_enc_interned_str(const char *ptr, long len, rb_encoding *enc)
{
    return rb_str_to_interned_str(rb_enc_str_new(ptr, len, enc));
}

VALUE
rb_enc_interned_str_cstr(const char *ptr, rb_encoding *enc)
{
    return rb_enc_interned_str(ptr, (long)strlen(ptr), enc);
}

rb_encoding *
rb_enc_compatible(VALUE str1, VALUE str2)
{
    /* Two Strings: encoding.c's enc_compatible_latter, on the encodings
     * and the cached code ranges, without a call into Ruby. strscan
     * checks its whole subject against every String pattern this way,
     * so this must not cost anything proportional to the subject. */
    if (RB_TYPE_P(str1, T_STRING) && RB_TYPE_P(str2, T_STRING)) {
        rb_encoding *enc1 = rb_enc_get(str1), *enc2 = rb_enc_get(str2);
        if (enc1 && enc2) {
            if (enc1 == enc2)
                return enc1;
            if (RSTRING_LEN(str2) == 0)
                return enc1;
            if (RSTRING_LEN(str1) == 0)
                return rb_enc_asciicompat(enc1) && rb_enc_str_coderange(str2) == ENC_CODERANGE_7BIT ? enc1 : enc2;
            if (!rb_enc_asciicompat(enc1) || !rb_enc_asciicompat(enc2))
                return NULL;
            int cr1 = rb_enc_str_coderange(str1), cr2 = rb_enc_str_coderange(str2);
            if (cr1 != cr2) {
                if (cr1 == ENC_CODERANGE_7BIT)
                    return enc2;
                if (cr2 == ENC_CODERANGE_7BIT)
                    return enc1;
            }
            if (cr2 == ENC_CODERANGE_7BIT)
                return enc1;
            return NULL;
        }
    }
    VALUE e = rb_funcall(rb_cEncoding, rb_intern("compatible?"), 2, str1, str2);
    return NIL_P(e) ? NULL : rb_to_encoding(e);
}

rb_encoding *
rb_enc_check(VALUE str1, VALUE str2)
{
    rb_encoding *enc = rb_enc_compatible(str1, str2);
    if (!enc) {
        rb_encoding *e1 = rb_enc_get(str1), *e2 = rb_enc_get(str2);
        rb_raise(rb_eEncCompatError, "incompatible character encodings: %s and %s",
                 e1 ? rb_enc_name(e1) : "?", e2 ? rb_enc_name(e2) : "?");
    }
    return enc;
}

/* The byte walks below know UTF-8 (index 1) and treat every other
 * encoding as single-byte, which is right for ASCII-8BIT / US-ASCII and
 * the ISO-8859 family; a multibyte legacy encoding (EUC-JP, Shift_JIS,
 * UTF-16) would need the interpreter's tables. */
static int
enc_is_utf8(rb_encoding *enc)
{
    return enc && enc->index == rb_utf8_encindex();
}

int
rb_enc_precise_mbclen(const char *p, const char *e, rb_encoding *enc)
{
    if (p >= e)
        return ONIGENC_CONSTRUCT_MBCLEN_NEEDMORE(1);
    /* monoruby's own walkers (UTF-8, the CJK multibyte sets, the wide
     * forms), answering in Onigmo's encoding; a byte per character for
     * everything else, as on CRuby. */
    return api->enc_precise_mbclen(ctx(), enc ? enc->name : "ASCII-8BIT",
                                   (const unsigned char *)p, (const unsigned char *)e);
}

int
rb_enc_mbclen(const char *p, const char *e, rb_encoding *enc)
{
    int n = rb_enc_precise_mbclen(p, e, enc);
    if (n > 0 && p + n <= e)
        return n;
    /* CRuby: an invalid or truncated character counts as one byte. */
    return 1;
}

long
rb_enc_strlen(const char *p, const char *e, rb_encoding *enc)
{
    long n = 0;
    while (p < e) {
        p += rb_enc_mbclen(p, e, enc);
        n++;
    }
    return n;
}

int
rb_enc_codelen(int code, rb_encoding *enc)
{
    if (!enc_is_utf8(enc))
        return 1;
    if (code < 0x80)
        return 1;
    if (code < 0x800)
        return 2;
    if (code < 0x10000)
        return 3;
    return 4;
}

unsigned int
rb_enc_codepoint_len(const char *p, const char *e, int *len_p, rb_encoding *enc)
{
    int len = 0;
    int64_t cp;
    if (p >= e)
        rb_raise(rb_eArgError, "empty string");
    /* monoruby's own decoders, for every encoding it walks. */
    cp = api->enc_codepoint(ctx(), enc ? enc->name : "ASCII-8BIT",
                            (const unsigned char *)p, (const unsigned char *)e, &len);
    if (cp < 0)
        rb_raise(rb_eArgError, "invalid byte sequence in %s", enc ? rb_enc_name(enc) : "ASCII-8BIT");
    if (len_p)
        *len_p = len;
    return (unsigned int)cp;
}

int
rb_enc_mbminlen(rb_encoding *enc)
{
    if (!enc)
        return 1;
    if (!strncasecmp(enc->name, "UTF-16", 6))
        return 2;
    if (!strncasecmp(enc->name, "UTF-32", 6))
        return 4;
    return 1;
}

int
rb_enc_mbmaxlen(rb_encoding *enc)
{
    /* Onigmo's max_enc_len of the multibyte encodings; 1 for the rest. */
    static const struct { const char *name; int len; } multibyte[] = {
        {"UTF-8", 4}, {"UTF8-MAC", 4}, {"UTF8-DoCoMo", 4}, {"UTF8-KDDI", 4},
        {"UTF8-SoftBank", 4}, {"CESU-8", 6}, {"UTF-16BE", 4}, {"UTF-16LE", 4},
        {"UTF-32BE", 4}, {"UTF-32LE", 4}, {"UTF-16", 4}, {"UTF-32", 4},
        {"EUC-JP", 3}, {"eucJP-ms", 3}, {"CP51932", 3}, {"EUC-JIS-2004", 3},
        {"stateless-ISO-2022-JP", 3}, {"stateless-ISO-2022-JP-KDDI", 3},
        {"Shift_JIS", 2}, {"Windows-31J", 2}, {"SJIS-DoCoMo", 2}, {"SJIS-KDDI", 2},
        {"SJIS-SoftBank", 2}, {"MacJapanese", 2}, {"EUC-KR", 2}, {"CP949", 2},
        {"Big5", 2}, {"Big5-HKSCS", 2}, {"Big5-UAO", 2}, {"CP950", 2}, {"CP951", 2},
        {"GBK", 2}, {"GB2312", 2}, {"GB12345", 2}, {"GB18030", 4}, {"EUC-TW", 4},
        {"Emacs-Mule", 4},
    };
    if (!enc)
        return 1;
    if (enc_is_utf8(enc))
        return 4;
    for (size_t i = 0; i < sizeof(multibyte) / sizeof(multibyte[0]); i++)
        if (strcasecmp(enc->name, multibyte[i].name) == 0)
            return multibyte[i].len;
    return 1;
}

int
rb_enc_unicode_p(rb_encoding *enc)
{
    return enc_is_utf8(enc);
}

int
rb_enc_dummy_p(rb_encoding *enc)
{
    return enc && RTEST(rb_funcall(rb_enc_from_encoding(enc), rb_intern("dummy?"), 0));
}

void
rb_must_asciicompat(VALUE str)
{
    rb_encoding *enc = rb_enc_get(str);
    if (enc && !rb_enc_asciicompat(enc))
        rb_raise(rb_eEncCompatError, "ASCII incompatible encoding: %s", rb_enc_name(enc));
}

void
rb_enc_raise(rb_encoding *enc, VALUE exc, const char *fmt, ...)
{
    (void)enc;
    va_list ap;
    va_start(ap, fmt);
    VALUE msg = rb_vsprintf(fmt, ap);
    va_end(ap);
    rb_exc_raise(rb_exc_new_str(exc, msg));
}

/* ---- ruby/util.h ------------------------------------------------------ */

char *
ruby_strdup(const char *str)
{
    size_t len = strlen(str) + 1;
    char *p = ruby_xmalloc(len);
    memcpy(p, str, len);
    return p;
}

/* ---- ruby/missing.h --------------------------------------------------- */

/*
 * CRuby's libruby carries these for the C libraries without them (glibc
 * before 2.38), and the extensions taken from CRuby call them (date's
 * strftime). Weak: a libc that has them keeps its own. Apple's has them
 * as fortification macros (ruby.h), so there they are not defined.
 */
#ifndef __APPLE__
__attribute__((weak)) size_t
strlcpy(char *dst, const char *src, size_t size)
{
    size_t len = strlen(src);
    if (size) {
        size_t n = len < size - 1 ? len : size - 1;
        memcpy(dst, src, n);
        dst[n] = '\0';
    }
    return len;
}

__attribute__((weak)) size_t
strlcat(char *dst, const char *src, size_t size)
{
    size_t dlen = strnlen(dst, size);
    if (dlen == size)
        return size + strlen(src);
    return dlen + strlcpy(dst + dlen, src, size - dlen);
}
#endif

unsigned long
ruby_strtoul(const char *str, char **endptr, int base)
{
    return strtoul(str, endptr, base);
}

double
ruby_strtod(const char *str, char **endptr)
{
    return strtod(str, endptr);
}

unsigned long
ruby_scan_digits(const char *str, ssize_t len, int base, size_t *retlen, int *overflow)
{
    const char *start = str;
    unsigned long ret = 0, mul_overflow = (~(unsigned long)0) / (unsigned long)base;
    *overflow = 0;
    if (!len)
        len = -1;
    while (len < 0 || len-- > 0) {
        int c = (unsigned char)*str;
        int d;
        if (rb_isdigit(c))
            d = c - '0';
        else if (rb_isalpha(c))
            d = rb_tolower(c) - 'a' + 10;
        else
            break;
        if (d >= base)
            break;
        if (mul_overflow < ret)
            *overflow = 1;
        ret *= (unsigned long)base;
        {
            unsigned long x = ret;
            ret += (unsigned long)d;
            if (ret < x)
                *overflow = 1;
        }
        str++;
    }
    *retlen = (size_t)(str - start);
    return ret;
}

unsigned long
ruby_scan_oct(const char *start, size_t len, size_t *retlen)
{
    const char *s = start;
    unsigned long v = 0;
    while (len-- && *s >= '0' && *s <= '7')
        v = (v << 3) | (unsigned long)(*s++ - '0');
    *retlen = (size_t)(s - start);
    return v;
}

unsigned long
ruby_scan_hex(const char *start, size_t len, size_t *retlen)
{
    const char *s = start;
    unsigned long v = 0;
    while (len-- && rb_isxdigit((unsigned char)*s)) {
        int c = (unsigned char)*s++;
        v = (v << 4) | (unsigned long)(rb_isdigit(c) ? c - '0' : rb_tolower(c) - 'a' + 10);
    }
    *retlen = (size_t)(s - start);
    return v;
}

/* A merge sort, so the comparator's third argument needs no thread-local
 * trampoline into qsort(3). */
static void
qsort_merge(char *base, char *tmp, size_t lo, size_t hi, size_t size,
            int (*cmp)(const void *, const void *, void *), void *d)
{
    if (hi - lo < 2)
        return;
    size_t mid = lo + (hi - lo) / 2;
    qsort_merge(base, tmp, lo, mid, size, cmp, d);
    qsort_merge(base, tmp, mid, hi, size, cmp, d);
    size_t i = lo, j = mid, k = lo;
    while (i < mid && j < hi) {
        if (cmp(base + i * size, base + j * size, d) <= 0)
            memcpy(tmp + (k++) * size, base + (i++) * size, size);
        else
            memcpy(tmp + (k++) * size, base + (j++) * size, size);
    }
    while (i < mid)
        memcpy(tmp + (k++) * size, base + (i++) * size, size);
    while (j < hi)
        memcpy(tmp + (k++) * size, base + (j++) * size, size);
    memcpy(base + lo * size, tmp + lo * size, (hi - lo) * size);
}

void
ruby_qsort(void *base, const size_t nel, const size_t size,
           int (*cmp)(const void *, const void *, void *), void *d)
{
    if (nel < 2)
        return;
    char *tmp = ruby_xmalloc(nel * size);
    qsort_merge(base, tmp, 0, nel, size, cmp, d);
    ruby_xfree(tmp);
}

char *
ruby_getcwd(void)
{
    VALUE cwd = rb_funcall(rb_path2class("Dir"), rb_intern("pwd"), 0);
    return ruby_strdup(StringValueCStr(cwd));
}

/* ---- ALLOCV ----------------------------------------------------------- */

void *
rb_alloc_tmp_buffer_with_count(volatile VALUE *store, size_t len, size_t count)
{
    (void)count;
    void *p = ruby_xmalloc(len);
    /* A pointer-sized integer the GC never looks at; ALLOCV_END reads
     * it back, and Qfalse means "nothing allocated". */
    *store = (VALUE)(uintptr_t)p;
    return p;
}

void *
rb_alloc_tmp_buffer(volatile VALUE *store, long len)
{
    if (len < 0)
        rb_raise(rb_eArgError, "negative buffer size (or size too big)");
    return rb_alloc_tmp_buffer_with_count(store, (size_t)len, (size_t)len);
}

void
rb_free_tmp_buffer(volatile VALUE *store)
{
    VALUE v = *store;
    *store = Qfalse;
    if (v != Qfalse && v != 0)
        ruby_xfree((void *)(uintptr_t)v);
}

/* ---------------------------------------------------------------------
 * ruby/re.h — Onigmo through the table, and the regexp helpers strscan
 * needs. The onig_* wrappers carry onigmo.h's signatures; the table's
 * are the same with regex_t / OnigRegion erased to void.
 * --------------------------------------------------------------------- */

#include "ruby/re.h"

static const MrOnigApi *
onig(void)
{
    static const MrOnigApi *t;
    if (!t)
        t = api->onig_api();
    return t;
}

OnigPosition
onig_search(OnigRegex reg, const OnigUChar *str, const OnigUChar *end,
            const OnigUChar *start, const OnigUChar *range, OnigRegion *region,
            OnigOptionType option)
{
    return onig()->search(reg, str, end, start, range, region, option);
}

OnigPosition
onig_match(OnigRegex reg, const OnigUChar *str, const OnigUChar *end,
           const OnigUChar *at, OnigRegion *region, OnigOptionType option)
{
    return onig()->match_at(reg, str, end, at, region, option);
}

OnigRegion *
onig_region_new(void)
{
    return onig()->region_new();
}

void
onig_region_init(OnigRegion *region)
{
    onig()->region_init(region);
}

/* CRuby's Onigmo clears a region it frees in place (`memset` to zero), so
 * a later onig_region_clear / onig_region_set on it starts from nothing.
 * The library monoruby links is older: it frees `beg` / `end` and zeroes
 * `allocated` but leaves `num_regs` and the dangling pointers, and the
 * next onig_region_clear writes through them (strscan's getch after a
 * failed scan, where rb_reg_onig_match freed the registers). */
void
onig_region_free(OnigRegion *region, int free_self)
{
    onig()->region_free(region, free_self);
    if (region && !free_self)
        memset(region, 0, sizeof(*region));
}

void
onig_region_copy(OnigRegion *to, const OnigRegion *from)
{
    onig()->region_copy(to, from);
}

void
onig_region_clear(OnigRegion *region)
{
    onig()->region_clear(region);
}

int
onig_region_resize(OnigRegion *region, int n)
{
    return onig()->region_resize(region, n);
}

int
onig_region_set(OnigRegion *region, int at, int beg, int end)
{
    return onig()->region_set(region, at, beg, end);
}

void
onig_free(OnigRegex reg)
{
    onig()->free(reg);
}

int
onig_name_to_backref_number(OnigRegex reg, const OnigUChar *name,
                            const OnigUChar *name_end, const OnigRegion *region)
{
    return onig()->name_to_backref_number(reg, name, name_end, region);
}

int
onig_foreach_name(OnigRegex reg,
                  int (*func)(const OnigUChar *, const OnigUChar *, int, int *, OnigRegex, void *),
                  void *arg)
{
    return onig()->foreach_name(reg, (int (*)(const uint8_t *, const uint8_t *, int, int *, void *, void *))func, arg);
}

int
onig_number_of_names(const OnigRegexType *reg)
{
    return onig()->number_of_names(reg);
}

int
onig_number_of_captures(const OnigRegexType *reg)
{
    return onig()->number_of_captures(reg);
}

int
onig_number_of_capture_histories(const OnigRegexType *reg)
{
    return onig()->number_of_capture_histories(reg);
}

/* The optional OnigErrorInfo is ignored: only onig_new produces one, and
 * an extension never compiles through this layer. */
int
onig_error_code_to_str(OnigUChar *s, OnigPosition err_code, ...)
{
    return onig()->error_code_to_str(s, err_code);
}

size_t
onig_region_memsize(const OnigRegion *regs)
{
    size_t size = sizeof(*regs);
    if (!regs->allocated)
        return size;
    size += regs->allocated * (sizeof(regs->beg[0]) + sizeof(regs->end[0]));
    return size;
}

/* ---------------------------------------------------------------------
 * Time (ruby/intern.h): each is a call into Time's own methods, so a
 * value an extension builds is exactly what `Time.at` would make.
 * --------------------------------------------------------------------- */

static VALUE
check_time(VALUE t)
{
    if (!RTEST(rb_obj_is_kind_of(t, rb_cTime)))
        rb_raise(rb_eTypeError, "wrong argument type %s (expected time)", displaying_class_of(t));
    return t;
}

VALUE
rb_time_new(time_t sec, long usec)
{
    return rb_funcall(rb_cTime, rb_intern("at"), 3, TIMET2NUM(sec), LONG2NUM(usec),
                      ID2SYM(rb_intern("microsecond")));
}

VALUE
rb_time_nano_new(time_t sec, long nsec)
{
    return rb_funcall(rb_cTime, rb_intern("at"), 3, TIMET2NUM(sec), LONG2NUM(nsec),
                      ID2SYM(rb_intern("nanosecond")));
}

/* `offset` as CRuby reads it: INT_MAX is the local zone, INT_MAX-1 UTC,
 * anything within a day a fixed offset in seconds. */
VALUE
rb_time_timespec_new(const struct timespec *ts, int offset)
{
    VALUE t = rb_time_nano_new(ts->tv_sec, ts->tv_nsec);
    if (offset == INT_MAX)
        return t;
    if (offset == INT_MAX - 1)
        return rb_funcall(t, rb_intern("utc"), 0);
    if (-86400 < offset && offset < 86400)
        return rb_funcall(t, rb_intern("localtime"), 1, INT2FIX(offset));
    rb_raise(rb_eArgError, "utc_offset out of range");
}

/* `off` is nil for the local zone, else what `Time#localtime` takes (an
 * offset in seconds, "+09:00", "UTC"). */
VALUE
rb_time_num_new(VALUE timev, VALUE off)
{
    VALUE t = rb_funcall(rb_cTime, rb_intern("at"), 1, timev);
    if (NIL_P(off) || off == Qundef)
        return t;
    return rb_funcall(t, rb_intern("localtime"), 1, off);
}

static struct timespec
num_to_timespec(VALUE num, int interval)
{
    const char *tstr = interval ? "time interval" : "time";
    struct timespec ts;
    VALUE t;
    if (RTEST(rb_obj_is_kind_of(num, rb_cTime))) {
        if (interval)
            rb_raise(rb_eTypeError, "can't convert Time into %s", tstr);
        t = num;
    }
    else if (RB_FLOAT_TYPE_P(num)) {
        /* CRuby rounds a Float to the nearest nanosecond rather than
         * keeping its exact binary value as `Time.at` does. */
        double f, d = modf(NUM2DBL(num), &f);
        long nsec;
        if (interval && (f < 0 || d < 0))
            rb_raise(rb_eArgError, "%s must not be negative", tstr);
        if (d >= 0) {
            nsec = (long)(d * 1e9 + 0.5);
            if (nsec >= 1000000000L) {
                nsec -= 1000000000L;
                f += 1;
            }
        }
        else if ((nsec = (long)(-d * 1e9 + 0.5)) > 0) {
            nsec = 1000000000L - nsec;
            f -= 1;
        }
        ts.tv_sec = NUM2TIMET(rb_funcall(rb_float_new(f), rb_intern("to_i"), 0));
        ts.tv_nsec = nsec;
        return ts;
    }
    else if (RB_INTEGER_TYPE_P(num) || RTEST(rb_obj_is_kind_of(num, rb_cRational))) {
        if (interval && RTEST(rb_funcall(num, rb_intern("<"), 1, INT2FIX(0))))
            rb_raise(rb_eArgError, "%s must not be negative", tstr);
        t = rb_funcall(rb_cTime, rb_intern("at"), 1, num);
    }
    else {
        rb_raise(rb_eTypeError, "can't convert %" PRIsVALUE " into %s", rb_obj_class(num), tstr);
    }
    ts.tv_sec = NUM2TIMET(rb_funcall(t, rb_intern("to_i"), 0));
    ts.tv_nsec = NUM2LONG(rb_funcall(t, rb_intern("nsec"), 0));
    return ts;
}

struct timespec
rb_time_timespec(VALUE time)
{
    return num_to_timespec(time, 0);
}

struct timespec
rb_time_timespec_interval(VALUE num)
{
    return num_to_timespec(num, 1);
}

static struct timeval
timespec_to_timeval(struct timespec ts)
{
    struct timeval tv;
    tv.tv_sec = ts.tv_sec;
    tv.tv_usec = (suseconds_t)(ts.tv_nsec / 1000);
    return tv;
}

struct timeval
rb_time_timeval(VALUE time)
{
    return timespec_to_timeval(num_to_timespec(time, 0));
}

struct timeval
rb_time_interval(VALUE num)
{
    return timespec_to_timeval(num_to_timespec(num, 1));
}

VALUE
rb_time_utc_offset(VALUE time)
{
    return rb_funcall(check_time(time), rb_intern("utc_offset"), 0);
}

/* Weak: strptime's ruby_time.c carries a definition of its own, which
 * links against the archive only when this one yields. */
__attribute__((weak)) void
rb_timespec_now(struct timespec *ts)
{
    if (clock_gettime(CLOCK_REALTIME, ts) == -1)
        rb_sys_fail("clock_gettime");
}

static regex_t *
check_reg(void *p)
{
    if (!p)
        capi_unwind();
    return (regex_t *)p;
}

regex_t *
rb_reg_ptr(VALUE re)
{
    return check_reg(api->regexp_ptr(ctx(), M(re)));
}

VALUE
rb_reg_source(VALUE re)
{
    return rb_funcall(re, rb_intern("source"), 0);
}

regex_t *
rb_reg_prepare_re(VALUE re, VALUE str)
{
    return check_reg(api->regexp_prepare(ctx(), M(re), M(str)));
}

OnigPosition
rb_reg_onig_match(VALUE re, VALUE str,
                  OnigPosition (*match)(regex_t *reg, VALUE str, struct re_registers *regs, void *args),
                  void *args, struct re_registers *regs)
{
    regex_t *reg = rb_reg_prepare_re(re, str);
    OnigPosition result = match(reg, str, regs, args);
    if (result < 0) {
        onig_region_free(regs, 0);
        switch (result) {
          case ONIG_MISMATCH:
            break;
          case ONIGERR_TIMEOUT:
            rb_raise(rb_const_get(rb_cRegexp, rb_intern("TimeoutError")), "regexp match timeout");
          default: {
            OnigUChar err[ONIG_MAX_ERROR_MESSAGE_LEN] = "";
            onig_error_code_to_str(err, result);
            rb_raise(rb_eRegexpError, "%s: %s", (const char *)err, RSTRING_PTR(rb_funcall(re, rb_intern("source"), 0)));
          }
        }
    }
    return result;
}

int
rb_reg_region_copy(struct re_registers *to, const struct re_registers *from)
{
    onig_region_copy(to, from);
    if (to->allocated)
        return 0;
    rb_gc();
    onig_region_copy(to, from);
    if (to->allocated)
        return 0;
    return ONIGERR_MEMORY;
}

long
rb_memsearch(const void *x0, long m, const void *y0, long n, struct rb_encoding *enc)
{
    const unsigned char *x = x0, *y = y0;
    (void)enc;
    if (m > n)
        return -1;
    if (m == n)
        return memcmp(x0, y0, m) == 0 ? 0 : -1;
    if (m < 1)
        return 0;
    if (m == 1) {
        const unsigned char *ys = memchr(y, *x, n);
        return ys ? ys - y : -1;
    }
    for (long i = 0; i + m <= n; i++) {
        if (y[i] == x[0] && memcmp(y + i, x, m) == 0)
            return i;
    }
    return -1;
}

VALUE
rb_reg_regcomp(VALUE str)
{
    return rb_funcall(rb_cRegexp, rb_intern("new"), 1, str);
}

VALUE
rb_reg_new_str(VALUE src, int options)
{
    return rb_funcall(rb_cRegexp, rb_intern("new"), 2, src, INT2FIX(options));
}

VALUE
rb_reg_new(const char *src, long len, int options)
{
    return rb_reg_new_str(rb_str_new(src, len), options);
}

VALUE
rb_reg_quote(VALUE str)
{
    return rb_funcall(rb_cRegexp, rb_intern("escape"), 1, str);
}

VALUE
rb_reg_match(VALUE re, VALUE str)
{
    return rb_funcall(re, rb_intern("=~"), 1, str);
}

VALUE
rb_reg_nth_match(int n, VALUE md)
{
    if (NIL_P(md))
        return Qnil;
    return rb_funcall(md, rb_intern("[]"), 1, INT2FIX(n));
}

VALUE
rb_reg_last_match(VALUE md)
{
    return rb_reg_nth_match(0, md);
}

VALUE
rb_reg_match_pre(VALUE md)
{
    if (NIL_P(md))
        return Qnil;
    return rb_funcall(md, rb_intern("pre_match"), 0);
}

VALUE
rb_reg_match_post(VALUE md)
{
    if (NIL_P(md))
        return Qnil;
    return rb_funcall(md, rb_intern("post_match"), 0);
}

int
rb_reg_options(VALUE re)
{
    return (int)NUM2INT(rb_funcall(re, rb_intern("options"), 0));
}

void
rb_match_busy(VALUE md)
{
    (void)md;
}

int
rb_long2int(long n)
{
    if ((long)(int)n != n)
        rb_raise(rb_eRangeError, "integer %ld too big to convert to 'int'", n);
    return (int)n;
}

void
rb_deprecate_constant(VALUE mod, const char *name)
{
    rb_funcall(mod, rb_intern("deprecate_constant"), 1, ID2SYM(rb_intern(name)));
}

/* ---- characters (encoding.h) ------------------------------------------ */

VALUE
rb_enc_uint_chr(unsigned int code, rb_encoding *enc)
{
    return rb_funcall(UINT2NUM(code), rb_intern("chr"), 1, rb_enc_from_encoding(enc));
}

int
rb_monoruby_enc_mbcput(unsigned int c, void *buf, rb_encoding *enc)
{
    VALUE s = rb_enc_uint_chr(c, enc);
    long len = RSTRING_LEN(s);
    memcpy(buf, RSTRING_PTR(s), len);
    return (int)len;
}

rb_encoding *
rb_find_encoding(VALUE enc)
{
    if (RTEST(rb_obj_is_kind_of(enc, rb_cEncoding)))
        return rb_to_encoding(enc);
    VALUE name = rb_check_string_type(enc);
    if (NIL_P(name))
        return rb_to_encoding(enc);
    int idx = rb_enc_find_index(StringValueCStr(name));
    return idx < 0 ? NULL : rb_enc_from_index(idx);
}

/* The head of the character `p` is in, for `s <= p < e`. A UTF-8 lead is
 * found by stepping back over continuation bytes; any other multibyte
 * encoding walks forward from `s`, as Onigmo's left_adjust_char_head does
 * for the encodings without a backward scan. */
char *
rb_enc_left_char_head(const char *s, const char *p, const char *e, rb_encoding *enc)
{
    if (p <= s)
        return (char *)p;
    if (enc_is_utf8(enc)) {
        const char *q = p;
        while (q > s && ((unsigned char)*q & 0xc0) == 0x80 && p - q < 3)
            q--;
        return (char *)q;
    }
    if (enc == NULL || rb_enc_mbmaxlen(enc) == 1)
        return (char *)p;
    const char *q = s;
    while (q < p) {
        int n = rb_enc_mbclen(q, e, enc);
        if (q + n > p)
            return (char *)q;
        q += n;
    }
    return (char *)q;
}

char *
rb_enc_right_char_head(const char *s, const char *p, const char *e, rb_encoding *enc)
{
    char *q = rb_enc_left_char_head(s, p, e, enc);
    if (q == p)
        return q;
    return q + rb_enc_mbclen(q, e, enc);
}

char *
rb_enc_prev_char(const char *s, const char *p, const char *e, rb_encoding *enc)
{
    if (p <= s)
        return NULL;
    return rb_enc_left_char_head(s, p - 1, e, enc);
}

/* ---- io helpers -------------------------------------------------------- */

void
rb_eof_error(void)
{
    rb_raise(rb_eEOFError, "end of file reached");
}

/* `$/`, `$\`, `$;` and the default record separator: CRuby exposes the
 * variables' storage (rb_rs, ...); here they are read from the globals
 * when used. */
VALUE
rb_monoruby_rs(void)
{
    return rb_gv_get("$/");
}

VALUE
rb_monoruby_output_rs(void)
{
    return rb_gv_get("$\\");
}

VALUE
rb_monoruby_fs(void)
{
    return rb_gv_get("$;");
}

VALUE
rb_monoruby_default_rs(void)
{
    static VALUE rs;
    if (!rs) {
        rs = rb_str_new_frozen(rb_str_new_cstr("\n"));
        api->gc_pin(ctx(), M(rs));
    }
    return rs;
}

/* `$_` of the Ruby frame the running extension method was called from,
 * as a C method shares its caller's in CRuby. */
VALUE
rb_lastline_get(void)
{
    if (HAVE_API(lastline_get))
        return R(api->lastline_get(ctx()));
    return Qnil;
}

void
rb_lastline_set(VALUE val)
{
    if (HAVE_API(lastline_set))
        api->lastline_set(ctx(), M(val));
}

/* ---------------------------------------------------------------------
 * What zlib.c needs beyond the rest: buffer capacity, Thread::Mutex,
 * File.open, and transcode.h's converter.
 * --------------------------------------------------------------------- */

size_t
rb_str_capacity(VALUE str)
{
    if (HAVE_API(str_capacity)) {
        int64_t c = api->str_capacity(ctx(), M(str));
        if (c < 0)
            capi_unwind();
        return (size_t)c;
    }
    return (size_t)RSTRING_LEN(str);
}

VALUE
rb_str_locktmp(VALUE str)
{
    StringValue(str);
    return str;
}

VALUE
rb_str_unlocktmp(VALUE str)
{
    StringValue(str);
    return str;
}

VALUE
rb_f_notimplement(int argc, const VALUE *argv, VALUE obj, VALUE marker)
{
    (void)argc;
    (void)argv;
    (void)obj;
    (void)marker;
    rb_notimplement();
}

static VALUE
mutex_class(void)
{
    static VALUE klass;
    if (!klass) {
        klass = rb_path2class("Thread::Mutex");
        rb_gc_register_address(&klass);
    }
    return klass;
}

VALUE
rb_mutex_new(void)
{
    return rb_funcall(mutex_class(), rb_intern("new"), 0);
}

VALUE
rb_mutex_locked_p(VALUE mutex)
{
    return rb_funcall(mutex, rb_intern("locked?"), 0);
}

VALUE
rb_mutex_trylock(VALUE mutex)
{
    return rb_funcall(mutex, rb_intern("try_lock"), 0);
}

VALUE
rb_mutex_lock(VALUE mutex)
{
    return rb_funcall(mutex, rb_intern("lock"), 0);
}

VALUE
rb_mutex_unlock(VALUE mutex)
{
    return rb_funcall(mutex, rb_intern("unlock"), 0);
}

VALUE
rb_mutex_sleep(VALUE mutex, VALUE timeout)
{
    return rb_funcall(mutex, rb_intern("sleep"), 1, timeout);
}

VALUE
rb_mutex_synchronize(VALUE mutex, VALUE (*func)(VALUE arg), VALUE arg)
{
    rb_mutex_lock(mutex);
    return rb_ensure(func, arg, rb_mutex_unlock, mutex);
}

VALUE
rb_file_open_str(VALUE fname, const char *modestr)
{
    FilePathValue(fname);
    return rb_funcall(rb_cFile, rb_intern("open"), 2, fname, rb_str_new_cstr(modestr));
}

VALUE
rb_file_open(const char *fname, const char *modestr)
{
    return rb_file_open_str(rb_str_new_cstr(fname), modestr);
}

/* The keyword arguments `String#encode` and `Encoding::Converter.new`
 * take for `ecflags` and the `ecopts` hash rb_econv_prepare_options made. */
static VALUE
econv_kwargs(int ecflags, VALUE ecopts)
{
    VALUE h = rb_hash_new();
    VALUE replace = ID2SYM(rb_intern("replace"));
    if ((ecflags & ECONV_INVALID_MASK) == ECONV_INVALID_REPLACE)
        rb_hash_aset(h, ID2SYM(rb_intern("invalid")), replace);
    switch (ecflags & ECONV_UNDEF_MASK) {
    case ECONV_UNDEF_REPLACE:
        rb_hash_aset(h, ID2SYM(rb_intern("undef")), replace);
        break;
    case ECONV_UNDEF_HEX_CHARREF:
        rb_hash_aset(h, ID2SYM(rb_intern("xml")), ID2SYM(rb_intern("text")));
        break;
    }
    static const struct { int flag; const char *key; } newlines[] = {
        {ECONV_UNIVERSAL_NEWLINE_DECORATOR, "universal_newline"},
        {ECONV_CRLF_NEWLINE_DECORATOR, "crlf_newline"},
        {ECONV_CR_NEWLINE_DECORATOR, "cr_newline"},
        {ECONV_LF_NEWLINE_DECORATOR, "lf_newline"},
    };
    for (size_t i = 0; i < sizeof(newlines) / sizeof(newlines[0]); i++) {
        if (ecflags & newlines[i].flag)
            rb_hash_aset(h, ID2SYM(rb_intern(newlines[i].key)), Qtrue);
    }
    if (ecflags & ECONV_XML_TEXT_DECORATOR)
        rb_hash_aset(h, ID2SYM(rb_intern("xml")), ID2SYM(rb_intern("text")));
    if (ecflags & ECONV_XML_ATTR_CONTENT_DECORATOR)
        rb_hash_aset(h, ID2SYM(rb_intern("xml")), ID2SYM(rb_intern("attr")));
    if (RB_TYPE_P(ecopts, T_HASH)) {
        VALUE v = rb_hash_aref(ecopts, replace);
        if (!NIL_P(v))
            rb_hash_aset(h, replace, v);
        v = rb_hash_aref(ecopts, ID2SYM(rb_intern("fallback")));
        if (!NIL_P(v))
            rb_hash_aset(h, ID2SYM(rb_intern("fallback")), v);
    }
    return h;
}

struct rb_econv_t {
    /* The Encoding::Converter, rooted while the struct lives. */
    VALUE conv;
    /* What the last rb_econv_convert ended with, for check_error. */
    rb_econv_result_t last;
};

static VALUE
econv_new_body(VALUE argp)
{
    VALUE *a = (VALUE *)argp;
    return rb_funcallv_kw(rb_path2class("Encoding::Converter"), rb_intern("new"), 3, a, RB_PASS_KEYWORDS);
}

rb_econv_t *
rb_econv_open_opts(const char *senc, const char *denc, int ecflags, VALUE ecopts)
{
    VALUE a[3] = {rb_str_new_cstr(senc), rb_str_new_cstr(denc), econv_kwargs(ecflags, ecopts)};
    int state;
    VALUE conv = rb_protect(econv_new_body, (VALUE)a, &state);
    if (state) {
        rb_set_errinfo(Qnil);
        return NULL;
    }
    rb_econv_t *ec = malloc(sizeof(*ec));
    if (!ec)
        rb_memerror();
    ec->conv = conv;
    ec->last = econv_source_buffer_empty;
    rb_gc_register_address(&ec->conv);
    return ec;
}

rb_econv_t *
rb_econv_open(const char *senc, const char *denc, int ecflags)
{
    return rb_econv_open_opts(senc, denc, ecflags, Qnil);
}

VALUE
rb_econv_open_exc(const char *senc, const char *denc, int ecflags)
{
    (void)ecflags;
    return rb_exc_new_str(rb_path2class("Encoding::ConverterNotFoundError"),
                          rb_sprintf("code converter not found (%s to %s)", senc, denc));
}

void
rb_econv_close(rb_econv_t *ec)
{
    if (!ec)
        return;
    rb_gc_unregister_address(&ec->conv);
    free(ec);
}

static const struct {
    const char *name;
    rb_econv_result_t res;
} econv_results[] = {
    {"invalid_byte_sequence", econv_invalid_byte_sequence},
    {"undefined_conversion", econv_undefined_conversion},
    {"destination_buffer_full", econv_destination_buffer_full},
    {"source_buffer_empty", econv_source_buffer_empty},
    {"finished", econv_finished},
    {"after_output", econv_after_output},
    {"incomplete_input", econv_incomplete_input},
};

rb_econv_result_t
rb_econv_convert(rb_econv_t *ec, const unsigned char **sp, const unsigned char *se,
                 unsigned char **dp, unsigned char *de, int flags)
{
    VALUE src = sp ? rb_str_new((const char *)*sp, se - *sp) : Qnil;
    VALUE dst = rb_str_new(0, 0);
    VALUE opts = rb_hash_new();
    if (flags & ECONV_PARTIAL_INPUT)
        rb_hash_aset(opts, ID2SYM(rb_intern("partial_input")), Qtrue);
    if (flags & ECONV_AFTER_OUTPUT)
        rb_hash_aset(opts, ID2SYM(rb_intern("after_output")), Qtrue);
    VALUE args[5] = {src, dst, Qnil, LONG2NUM(de - *dp), opts};
    VALUE r = rb_funcallv_kw(ec->conv, rb_intern("primitive_convert"), 5, args, RB_PASS_KEYWORDS);
    long out = RSTRING_LEN(dst);
    if (out > de - *dp)
        out = de - *dp;
    memcpy(*dp, RSTRING_PTR(dst), out);
    *dp += out;
    if (sp)
        *sp = se - RSTRING_LEN(src);
    ec->last = econv_source_buffer_empty;
    const char *name = rb_id2name(SYM2ID(r));
    for (size_t i = 0; i < sizeof(econv_results) / sizeof(econv_results[0]); i++) {
        if (strcmp(econv_results[i].name, name) == 0)
            ec->last = econv_results[i].res;
    }
    return ec->last;
}

VALUE
rb_econv_str_convert(rb_econv_t *ec, VALUE src, int flags)
{
    VALUE r = rb_funcall(ec->conv, rb_intern("convert"), 1, src);
    if (!(flags & ECONV_PARTIAL_INPUT))
        rb_str_buf_append(r, rb_funcall(ec->conv, rb_intern("finish"), 0));
    ec->last = econv_finished;
    return r;
}

void
rb_econv_check_error(rb_econv_t *ec)
{
    if (ec->last == econv_invalid_byte_sequence || ec->last == econv_undefined_conversion ||
        ec->last == econv_incomplete_input) {
        VALUE err = rb_funcall(ec->conv, rb_intern("last_error"), 0);
        if (!NIL_P(err))
            rb_exc_raise(err);
    }
}

struct conv_opts_args {
    VALUE str, to, from, kw;
};

static VALUE
conv_opts_body(VALUE argp)
{
    struct conv_opts_args *a = (struct conv_opts_args *)argp;
    VALUE args[3] = {a->to, a->from, a->kw};
    return rb_funcallv_kw(a->str, rb_intern("encode"), 3, args, RB_PASS_KEYWORDS);
}

VALUE
rb_str_conv_enc_opts(VALUE str, rb_encoding *from, rb_encoding *to, int ecflags, VALUE ecopts)
{
    if (!to)
        return str;
    if (!from)
        from = rb_enc_get(str);
    if (from == to)
        return str;
    if ((rb_enc_asciicompat(to) && rb_enc_asciicompat(from) &&
         rb_enc_str_coderange(str) == ENC_CODERANGE_7BIT) ||
        to->index == 0) {
        if (rb_enc_get(str) != to) {
            str = rb_str_dup(str);
            rb_enc_associate(str, to);
        }
        return str;
    }
    struct conv_opts_args a = {str, rb_str_new_cstr(to->name), rb_str_new_cstr(from->name),
                               econv_kwargs(ecflags, ecopts)};
    int state;
    VALUE r = rb_protect(conv_opts_body, (VALUE)&a, &state);
    if (state) {
        rb_set_errinfo(Qnil);
        return str;
    }
    return r;
}

/* ---------------------------------------------------------------------
 * ruby_snprintf / ruby_vsnprintf (ruby/subst.h)
 * --------------------------------------------------------------------- */

/* Whether every conversion in `fmt` is one the fast path formats: %%,
 * %c, %s and d/i/u with an optional l, ll or z, without flags, width or
 * precision. */
static int
subst_simple_format(const char *fmt)
{
    for (const char *p = fmt; *p; p++) {
        if (*p != '%')
            continue;
        p++;
        if (*p == '%' || *p == 'c' || *p == 's')
            continue;
        if (*p == 'z')
            p++;
        else if (*p == 'l') {
            p++;
            if (*p == 'l')
                p++;
        }
        if (*p != 'd' && *p != 'i' && *p != 'u')
            return 0;
    }
    return 1;
}

int
ruby_vsnprintf(char *str, size_t n, const char *fmt, va_list ap)
{
    if (!subst_simple_format(fmt))
        return vsnprintf(str, n, fmt, ap);
    size_t len = 0;
    /* `ch` is evaluated exactly once, whether or not it fits: the
       arguments below have side effects (va_arg, --nd). */
#define SUBST_PUT(ch) do { char subst_c_ = (ch); if (len + 1 < n) str[len] = subst_c_; len++; } while (0)
    for (const char *p = fmt; *p; p++) {
        if (*p != '%') {
            SUBST_PUT(*p);
            continue;
        }
        p++;
        if (*p == '%') {
            SUBST_PUT('%');
            continue;
        }
        if (*p == 'c') {
            SUBST_PUT((char)va_arg(ap, int));
            continue;
        }
        if (*p == 's') {
            const char *s = va_arg(ap, const char *);
            if (!s)
                s = "(null)";
            for (; *s; s++)
                SUBST_PUT(*s);
            continue;
        }
        int size = 0; /* 0 int, 1 long, 2 long long, 3 size_t */
        if (*p == 'z') {
            size = 3;
            p++;
        }
        else if (*p == 'l') {
            size = 1;
            p++;
            if (*p == 'l') {
                size = 2;
                p++;
            }
        }
        unsigned long long u;
        int neg = 0;
        if (*p == 'u') {
            switch (size) {
              case 0: u = va_arg(ap, unsigned int); break;
              case 1: u = va_arg(ap, unsigned long); break;
              case 2: u = va_arg(ap, unsigned long long); break;
              default: u = va_arg(ap, size_t); break;
            }
        }
        else {
            long long v;
            switch (size) {
              case 0: v = va_arg(ap, int); break;
              case 1: v = va_arg(ap, long); break;
              case 2: v = va_arg(ap, long long); break;
              default: v = (long long)va_arg(ap, ssize_t); break;
            }
            neg = v < 0;
            u = neg ? 0ULL - (unsigned long long)v : (unsigned long long)v;
        }
        char digits[24];
        int nd = 0;
        do {
            digits[nd++] = (char)('0' + u % 10);
            u /= 10;
        } while (u);
        if (neg)
            SUBST_PUT('-');
        while (nd)
            SUBST_PUT(digits[--nd]);
    }
#undef SUBST_PUT
    if (n)
        str[len < n ? len : n - 1] = '\0';
    return (int)len;
}

int
ruby_snprintf(char *str, size_t n, const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    int r = ruby_vsnprintf(str, n, fmt, ap);
    va_end(ap);
    return r;
}

/* ---------------------------------------------------------------------
 * rb_io_t
 *
 * monoruby's IO has no `struct rb_io` an extension could point into, so
 * `GetOpenFile` / `MakeOpenFile` hand out one the layer keeps for the
 * Ruby call that asked (its owner frame): filled from the object through
 * `io_state` (descriptor, the FMODE bits, the buffered input), and
 * written back through `io_update` -- a descriptor stored into a fresh
 * object's `fd`, the mode bits an extension changed (`close_read` clears
 * FMODE_READABLE; FMODE bits the interpreter has no use for are kept for
 * it), the buffered bytes it consumed. The write-back happens
 * before any call into Ruby and when the owner frame ends; after a call
 * into Ruby the struct is refreshed, so a close the Ruby code did shows
 * as `fd == -1`, as it does in CRuby.
 * --------------------------------------------------------------------- */

#include "ruby/fiber/scheduler.h"

struct capi_io {
    struct capi_io *next;
    struct capi_frame *owner;
    VALUE obj;
    rb_io_t fptr;
    /* What the object said at the last sync. */
    int fd0, mode0, rlen0;
};

static void
io_fill(struct capi_io *e, const MrIoState *st)
{
    e->fptr.fd = st->fd;
    e->fptr.mode = st->mode;
    e->fptr.rbuf.ptr = (char *)st->rbuf;
    e->fptr.rbuf.off = 0;
    e->fptr.rbuf.len = (int)st->rbuf_len;
    e->fptr.rbuf.capa = (int)st->rbuf_len;
    e->fd0 = st->fd;
    e->mode0 = st->mode;
    e->rlen0 = (int)st->rbuf_len;
}

static int
io_sync_in(struct capi_io *e)
{
    MrIoState st;
    if (api->io_state(ctx(), M(e->obj), &st) != 0)
        return -1;
    io_fill(e, &st);
    return 0;
}

/* Write `e` back if the extension changed it; nonzero on failure (the
 * error is pending). */
static int
io_sync_out(struct capi_io *e)
{
    int consumed = e->rlen0 - e->fptr.rbuf.len;
    if (consumed < 0)
        consumed = 0;
    if (e->fptr.fd == e->fd0 && e->fptr.mode == e->mode0 && consumed == 0)
        return 0;
    if (api->io_update(ctx(), M(e->obj), e->fptr.fd, e->fptr.mode, (size_t)consumed) != 0)
        return -1;
    return io_sync_in(e);
}

static void
io_out_all(void)
{
    for (struct capi_io *e = capi_ios; e; e = e->next)
        io_sync_out(e);
}

static void
io_in_all(void)
{
    for (struct capi_io *e = capi_ios; e; e = e->next)
        io_sync_in(e);
}

static void io_free_entry(struct capi_io *e);

/* The frame `f` ends: write back and drop the structs it owns. Inner
 * frames end first, so they are the newest entries. */
static void
io_release(struct capi_frame *f)
{
    struct capi_frame *saved = capi_cur;
    capi_cur = f;
    struct capi_io **pp = &capi_ios;
    while (*pp) {
        struct capi_io *e = *pp;
        if (e->owner == f) {
            io_sync_out(e);
            *pp = e->next;
            io_free_entry(e);
        } else {
            pp = &e->next;
        }
    }
    capi_cur = saved;
}

/* Freed entries are kept for the next GetOpenFile (one per C method call
 * on a socket): a few, per thread. */
#define CAPI_IO_SPARE 4
static __thread struct capi_io *io_spare[CAPI_IO_SPARE];
static __thread int io_nspare;

static void
io_free_entry(struct capi_io *e)
{
    if (io_nspare < CAPI_IO_SPARE)
        io_spare[io_nspare++] = e;
    else
        free(e);
}

static struct capi_io *
io_find(VALUE obj)
{
    for (struct capi_io *e = capi_ios; e; e = e->next)
        if (e->obj == obj)
            return e;
    return NULL;
}

static struct capi_io *
io_entry(VALUE obj)
{
    struct capi_io *e = io_find(obj);
    if (e)
        return e;
    if (!HAVE_API(io_update))
        rb_raise(rb_eNotImpError, "rb_io_t needs a newer monoruby");
    MrIoState st;
    if (api->io_state(ctx(), M(obj), &st) != 0)
        capi_unwind();
    if (io_nspare > 0) {
        e = io_spare[--io_nspare];
        memset(e, 0, sizeof(*e));
    } else if (!(e = calloc(1, sizeof(*e)))) {
        rb_memerror();
    }
    e->obj = obj;
    e->owner = frame()->owner;
    e->fptr.self = obj;
    e->fptr.pathv = Qnil;
    e->fptr.tied_io_for_writing = Qfalse;
    e->fptr.timeout = Qnil;
    e->fptr.encs.ecopts = Qnil;
    e->fptr.writeconv_asciicompat = Qnil;
    e->fptr.writeconv_pre_ecopts = Qnil;
    e->fptr.write_lock = Qnil;
    e->fptr.pid = 0;
    io_fill(e, &st);
    e->next = capi_ios;
    capi_ios = e;
    return e;
}

rb_io_t *
rb_io_get_fptr(VALUE io)
{
    return &io_entry(io)->fptr;
}

struct RFile *
rb_monoruby_rfile(VALUE io)
{
    /* Only `->fptr` is meant to be read; the struct lives with the
     * entry, which lives as long as the frame. */
    struct capi_io *e = io_entry(io);
    static __thread struct RFile rfile;
    rfile.basic.flags = T_FILE;
    rfile.basic.klass = rb_obj_class(io);
    rfile.fptr = &e->fptr;
    return &rfile;
}

rb_io_t *
rb_io_make_open_file(VALUE obj)
{
    struct capi_io *e = io_entry(obj);
    if (e->fptr.fd >= 0) {
        /* CRuby closes what the object had open first. */
        rb_io_close(obj);
        io_sync_in(e);
    }
    e->fptr.mode = 0;
    e->mode0 = 0;
    e->fptr.rbuf.len = e->rlen0 = 0;
    return &e->fptr;
}

void
rb_io_check_initialized(rb_io_t *fptr)
{
    if (!fptr)
        rb_raise(rb_eIOError, "uninitialized stream");
}

void
rb_io_check_closed(rb_io_t *fptr)
{
    rb_io_check_initialized(fptr);
    if (fptr->fd < 0)
        rb_raise(rb_eIOError, "closed stream");
}

void
rb_io_check_readable(rb_io_t *fptr)
{
    rb_io_check_closed(fptr);
    if (!(fptr->mode & FMODE_READABLE))
        rb_raise(rb_eIOError, "not opened for reading");
}

void
rb_io_check_char_readable(rb_io_t *fptr)
{
    rb_io_check_readable(fptr);
}

void
rb_io_check_byte_readable(rb_io_t *fptr)
{
    rb_io_check_readable(fptr);
}

void
rb_io_check_writable(rb_io_t *fptr)
{
    rb_io_check_closed(fptr);
    if (!(fptr->mode & FMODE_WRITABLE))
        rb_raise(rb_eIOError, "not opened for writing");
}

void
rb_io_synchronized(rb_io_t *fptr)
{
    fptr->mode |= FMODE_SYNC;
}

void
rb_io_set_nonblock(rb_io_t *fptr)
{
    int flags = fcntl(fptr->fd, F_GETFL);
    if (flags == -1)
        rb_sys_fail("fcntl(F_GETFL)");
    if ((flags & O_NONBLOCK) == 0 && fcntl(fptr->fd, F_SETFL, flags | O_NONBLOCK) == -1)
        rb_sys_fail("fcntl(F_SETFL)");
}

int
rb_io_read_pending(rb_io_t *fptr)
{
    return fptr->rbuf.len > 0;
}

void
rb_io_read_check(rb_io_t *fptr)
{
    if (!rb_io_read_pending(fptr))
        rb_thread_wait_fd(fptr->fd);
}

int
rb_io_descriptor(VALUE io)
{
    rb_io_t *fptr;
    GetOpenFile(rb_io_get_io(io), fptr);
    return fptr->fd;
}

int
rb_io_mode(VALUE io)
{
    rb_io_t *fptr;
    GetOpenFile(rb_io_get_io(io), fptr);
    return fptr->mode;
}

VALUE
rb_io_get_write_io(VALUE io)
{
    return io;
}

VALUE
rb_io_ascii8bit_binmode(VALUE io)
{
    rb_funcall(io, rb_intern("binmode"), 0);
    return io;
}

VALUE
rb_io_fdopen(int fd, int flags, const char *path)
{
    (void)path;
    return rb_funcall(rb_cIO, rb_intern("new"), 2, INT2FIX(fd), INT2FIX(flags));
}

/* io.c's: a fiber scheduler answers for itself (nil, false or the
 * events); otherwise IO#wait, bounded by the IO's own timeout when the
 * caller gives none, and false once that runs out. */
VALUE
rb_io_wait(VALUE io, VALUE events, VALUE timeout)
{
    VALUE scheduler = rb_fiber_scheduler_current();
    if (!NIL_P(scheduler))
        return rb_fiber_scheduler_io_wait(scheduler, io, events, timeout);
    if (NIL_P(timeout) && rb_obj_respond_to(io, rb_intern("timeout"), 0))
        timeout = rb_funcall(io, rb_intern("timeout"), 0);
    VALUE r = rb_funcall(io, rb_intern("wait"), 2, events, timeout);
    if (NIL_P(r) || r == Qfalse)
        return Qfalse;
    if (FIXNUM_P(r))
        return r;
    return events;
}

VALUE
rb_io_maybe_wait(int error, VALUE io, VALUE events, VALUE timeout)
{
    switch (error) {
    case EINTR:
#ifdef ERESTART
    case ERESTART:
#endif
        rb_thread_check_ints();
        return events;
    case EAGAIN:
#if defined(EWOULDBLOCK) && EWOULDBLOCK != EAGAIN
    case EWOULDBLOCK:
#endif
        return rb_io_wait(io, events, timeout);
    default:
        /* No event is ready. */
        return Qnil;
    }
}

int
rb_io_maybe_wait_readable(int error, VALUE io, VALUE timeout)
{
    VALUE r = rb_io_maybe_wait(error, io, INT2FIX(RUBY_IO_READABLE), timeout);
    if (RTEST(r))
        return NUM2INT(r);
    if (r == Qfalse)
        rb_raise(rb_eIOTimeoutError, "Timed out waiting for IO to become readable!");
    return 0;
}

int
rb_io_maybe_wait_writable(int error, VALUE io, VALUE timeout)
{
    VALUE r = rb_io_maybe_wait(error, io, INT2FIX(RUBY_IO_WRITABLE), timeout);
    if (RTEST(r))
        return NUM2INT(r);
    if (r == Qfalse)
        rb_raise(rb_eIOTimeoutError, "Timed out waiting for IO to become writable!");
    return 0;
}

#include <poll.h>

struct poll_call {
    struct pollfd pfd;
    int timeout_ms;
    int result;
    int err;
};

static void *
poll_nogvl(void *arg)
{
    struct poll_call *c = arg;
    c->result = poll(&c->pfd, 1, c->timeout_ms);
    c->err = errno;
    return NULL;
}

/* poll(2) on one descriptor with the interpreter released; EINTR is an
 * interrupt (delivered on the way back) or a retry. */
static int
poll_fd(int fd, short events, int timeout_ms)
{
    struct poll_call c;
    for (;;) {
        c.pfd.fd = fd;
        c.pfd.events = events;
        c.pfd.revents = 0;
        c.timeout_ms = timeout_ms;
        rb_thread_call_without_gvl(poll_nogvl, &c, RUBY_UBF_IO, NULL);
        if (c.result >= 0 || c.err != EINTR)
            break;
        rb_thread_check_ints();
    }
    if (c.result < 0) {
        errno = c.err;
        return -1;
    }
    return c.result == 0 ? 0 : c.pfd.revents;
}

int
rb_wait_for_single_fd(int fd, int events, struct timeval *tv)
{
    short pev = 0;
    if (events & RB_WAITFD_IN)
        pev |= POLLIN;
    if (events & RB_WAITFD_PRI)
        pev |= POLLPRI;
    if (events & RB_WAITFD_OUT)
        pev |= POLLOUT;
    int ms = tv ? (int)(tv->tv_sec * 1000 + tv->tv_usec / 1000) : -1;
    int r = poll_fd(fd, pev, ms);
    if (r <= 0)
        return r;
    int out = 0;
    if (r & (POLLIN | POLLHUP | POLLERR))
        out |= RB_WAITFD_IN & events;
    if (r & POLLPRI)
        out |= RB_WAITFD_PRI;
    if (r & (POLLOUT | POLLERR))
        out |= RB_WAITFD_OUT & events;
    return out;
}

int
rb_thread_wait_fd(int fd)
{
    return rb_wait_for_single_fd(fd, RB_WAITFD_IN, NULL);
}

int
rb_thread_fd_writable(int fd)
{
    return rb_wait_for_single_fd(fd, RB_WAITFD_OUT, NULL);
}

int
rb_io_wait_readable(int fd)
{
    rb_thread_check_ints();
    switch (errno) {
    case EINTR:
#ifdef ERESTART
    case ERESTART:
#endif
        return 1;
    case EAGAIN:
#if defined(EWOULDBLOCK) && EWOULDBLOCK != EAGAIN
    case EWOULDBLOCK:
#endif
        rb_thread_wait_fd(fd);
        return 1;
    default:
        return 0;
    }
}

int
rb_io_wait_writable(int fd)
{
    rb_thread_check_ints();
    switch (errno) {
    case EINTR:
#ifdef ERESTART
    case ERESTART:
#endif
        return 1;
    case EAGAIN:
#if defined(EWOULDBLOCK) && EWOULDBLOCK != EAGAIN
    case EWOULDBLOCK:
#endif
        rb_thread_fd_writable(fd);
        return 1;
    default:
        return 0;
    }
}

struct io_blocking_call {
    VALUE (*func)(void *);
    void *arg;
    VALUE result;
    int err;
};

static void *
io_blocking_trampoline(void *p)
{
    struct io_blocking_call *c = p;
    c->result = c->func(c->arg);
    c->err = errno;
    return NULL;
}

/* On a non-blocking descriptor (every socket the extension makes) the
 * call returns at once, EAGAIN sending the caller to rb_io_wait, so it
 * runs with the interpreter held: that saves the release and re-acquire
 * on every recv / send, and `func` may read a String (RSTRING_PTR, an
 * ABI call here) as CRuby lets it. A blocking descriptor's call runs
 * released. The caller reads errno after a failed syscall in `func`, so
 * it is carried across re-entering the interpreter. */
VALUE
rb_io_blocking_region(rb_io_t *fptr, VALUE (*func)(void *), void *arg)
{
    int fl = fptr ? fcntl(fptr->fd, F_GETFL) : -1;
    if (fl != -1 && (fl & O_NONBLOCK))
        return func(arg);
    struct io_blocking_call c = {func, arg, 0, 0};
    rb_thread_call_without_gvl(io_blocking_trampoline, &c, RUBY_UBF_IO, NULL);
    errno = c.err;
    return c.result;
}

static int64_t
check_ints_noop(void *arg)
{
    (void)arg;
    return 0;
}

/* Leaving and re-entering the interpreter is where it delivers what is
 * pending for the thread (Thread#raise / #kill, a trapped signal). */
void
rb_thread_check_ints(void)
{
    struct capi_frame *saved = frame();
    int64_t out;
    int e = errno;
    int rc = api->call_blocking(saved->ctx, check_ints_noop, NULL, &out);
    capi_cur = saved;
    errno = e;
    if (rc != 0)
        capi_unwind();
}

void *
rb_thread_prevent_fork(void *(*func)(void *), void *arg)
{
    return func(arg);
}

/* ---- rb_fdset_t ------------------------------------------------------ */

static size_t
fdset_bytes(int maxfd)
{
    size_t words = ((size_t)maxfd + NFDBITS) / NFDBITS;
    size_t n = words * sizeof(fd_mask);
    return n < sizeof(fd_set) ? sizeof(fd_set) : n;
}

void
rb_fd_init(rb_fdset_t *fds)
{
    fds->maxfd = 0;
    fds->fdset = calloc(1, sizeof(fd_set));
    if (!fds->fdset)
        rb_memerror();
}

void
rb_fd_term(rb_fdset_t *fds)
{
    free(fds->fdset);
    fds->fdset = NULL;
    fds->maxfd = 0;
}

void
rb_fd_zero(rb_fdset_t *fds)
{
    if (fds->fdset)
        memset(fds->fdset, 0, fdset_bytes(fds->maxfd));
}

static void
fdset_resize(int n, rb_fdset_t *fds)
{
    size_t m = fdset_bytes(n);
    size_t o = fdset_bytes(fds->maxfd);
    if (m > o) {
        fd_set *p = realloc(fds->fdset, m);
        if (!p)
            rb_memerror();
        memset((char *)p + o, 0, m - o);
        fds->fdset = p;
    }
    if (n >= fds->maxfd)
        fds->maxfd = n + 1;
}

void
rb_fd_set(int fd, rb_fdset_t *fds)
{
    fdset_resize(fd, fds);
    ((fd_mask *)fds->fdset)[fd / NFDBITS] |= (fd_mask)1 << (fd % NFDBITS);
}

void
rb_fd_clr(int fd, rb_fdset_t *fds)
{
    if (fd < fds->maxfd)
        ((fd_mask *)fds->fdset)[fd / NFDBITS] &= ~((fd_mask)1 << (fd % NFDBITS));
}

int
rb_fd_isset(int fd, const rb_fdset_t *fds)
{
    if (fd < 0 || fd >= fds->maxfd)
        return 0;
    return (((const fd_mask *)fds->fdset)[fd / NFDBITS] >> (fd % NFDBITS)) & 1;
}

void
rb_fd_copy(rb_fdset_t *dst, const fd_set *src, int max)
{
    size_t n = fdset_bytes(max);
    fd_set *p = realloc(dst->fdset, n);
    if (!p)
        rb_memerror();
    memset(p, 0, n);
    memcpy(p, src, n < sizeof(fd_set) ? n : sizeof(fd_set));
    dst->fdset = p;
    dst->maxfd = max;
}

void
rb_fd_dup(rb_fdset_t *dst, const rb_fdset_t *src)
{
    size_t n = fdset_bytes(src->maxfd);
    fd_set *p = realloc(dst->fdset, n);
    if (!p)
        rb_memerror();
    memcpy(p, src->fdset, n);
    dst->fdset = p;
    dst->maxfd = src->maxfd;
}

struct select_call {
    int n;
    fd_set *r, *w, *e;
    struct timeval *tv;
    int result;
    int err;
};

static void *
select_nogvl(void *arg)
{
    struct select_call *c = arg;
    c->result = select(c->n, c->r, c->w, c->e, c->tv);
    c->err = errno;
    return NULL;
}

int
rb_thread_fd_select(int max, rb_fdset_t *read, rb_fdset_t *write, rb_fdset_t *except, struct timeval *timeout)
{
    if (read)
        fdset_resize(max - 1, read);
    if (write)
        fdset_resize(max - 1, write);
    if (except)
        fdset_resize(max - 1, except);
    size_t n = fdset_bytes(max);
    fd_set *r0 = NULL, *w0 = NULL, *e0 = NULL;
    if (read && (r0 = malloc(n)))
        memcpy(r0, read->fdset, n);
    if (write && (w0 = malloc(n)))
        memcpy(w0, write->fdset, n);
    if (except && (e0 = malloc(n)))
        memcpy(e0, except->fdset, n);
    struct timeval tv, *tvp = NULL;
    if (timeout) {
        tv = *timeout;
        tvp = &tv;
    }
    struct select_call c;
    for (;;) {
        c.n = max;
        c.r = read ? read->fdset : NULL;
        c.w = write ? write->fdset : NULL;
        c.e = except ? except->fdset : NULL;
        c.tv = tvp;
        rb_thread_call_without_gvl(select_nogvl, &c, RUBY_UBF_IO, NULL);
        if (c.result >= 0 || c.err != EINTR)
            break;
        /* Linux leaves the time left in tv; elsewhere it is a retry with
         * the whole timeout, as a spurious wakeup would be. */
        if (r0)
            memcpy(read->fdset, r0, n);
        if (w0)
            memcpy(write->fdset, w0, n);
        if (e0)
            memcpy(except->fdset, e0, n);
        free(r0); free(w0); free(e0);
        r0 = w0 = e0 = NULL;
        rb_thread_check_ints();
        if (read && (r0 = malloc(n)))
            memcpy(r0, read->fdset, n);
        if (write && (w0 = malloc(n)))
            memcpy(w0, write->fdset, n);
        if (except && (e0 = malloc(n)))
            memcpy(e0, except->fdset, n);
    }
    free(r0); free(w0); free(e0);
    if (c.result < 0)
        errno = c.err;
    return c.result;
}

/* ---- descriptors and errors io.c / internal headers give ext/socket -- */

void
rb_update_max_fd(int fd)
{
    (void)fd;
}

void
rb_maygvl_fd_fix_cloexec(int fd)
{
    if (fd <= 2)
        return;
    int flags = fcntl(fd, F_GETFD);
    if (flags != -1 && !(flags & FD_CLOEXEC))
        fcntl(fd, F_SETFD, flags | FD_CLOEXEC);
}

void
rb_fd_fix_cloexec(int fd)
{
    rb_maygvl_fd_fix_cloexec(fd);
}

int
rb_reserved_fd_p(int fd)
{
    (void)fd;
    return 0;
}

/* Out of descriptors or memory: collect, which may close some. */
int
rb_gc_for_fd(int err)
{
    if (err == EMFILE || err == ENFILE || err == ENOMEM) {
        rb_gc();
        return 1;
    }
    return 0;
}

void
rb_syserr_fail_path(int err, VALUE path)
{
    if (NIL_P(path))
        rb_syserr_fail(err, NULL);
    rb_syserr_fail_str(err, path);
}

void
rb_readwrite_syserr_fail(enum rb_io_wait_readwrite waiting, int n, const char *mesg)
{
    const char *name = NULL;
    if (n == EAGAIN || n == EWOULDBLOCK)
        name = waiting == RB_IO_WAIT_READABLE ? "EAGAINWaitReadable" : "EAGAINWaitWritable";
    else if (n == EINPROGRESS)
        name = waiting == RB_IO_WAIT_READABLE ? "EINPROGRESSWaitReadable" : "EINPROGRESSWaitWritable";
    if (name && rb_const_defined(rb_cIO, rb_intern(name))) {
        VALUE klass = rb_const_get(rb_cIO, rb_intern(name));
        VALUE m = mesg ? rb_str_new_cstr(mesg) : Qnil;
        rb_exc_raise(rb_class_new_instance(1, &m, klass));
    }
    rb_syserr_fail(n, mesg);
}

void
rb_readwrite_sys_fail(enum rb_io_wait_readwrite waiting, const char *mesg)
{
    rb_readwrite_syserr_fail(waiting, errno, mesg);
}

VALUE
rb_str_tmp_frozen_acquire(VALUE str)
{
    return rb_str_new_frozen(str);
}

void
rb_str_tmp_frozen_release(VALUE orig, VALUE tmp)
{
    (void)orig;
    (void)tmp;
}

/* ---- the fiber scheduler -------------------------------------------- */

VALUE
rb_fiber_scheduler_get(void)
{
    VALUE fiber = rb_const_get(rb_cObject, rb_intern("Fiber"));
    return rb_funcall(fiber, rb_intern("scheduler"), 0);
}

VALUE
rb_fiber_scheduler_current(void)
{
    VALUE s = rb_fiber_scheduler_get();
    if (NIL_P(s))
        return Qnil;
    VALUE fiber = rb_const_get(rb_cObject, rb_intern("Fiber"));
    if (RTEST(rb_funcall(fiber, rb_intern("blocking?"), 0)))
        return Qnil;
    return s;
}

VALUE
rb_fiber_scheduler_address_resolve(VALUE scheduler, VALUE hostname)
{
    return rb_check_funcall(scheduler, rb_intern("address_resolve"), 1, &hostname);
}

VALUE
rb_fiber_scheduler_io_wait(VALUE scheduler, VALUE io, VALUE events, VALUE timeout)
{
    return rb_funcall(scheduler, rb_intern("io_wait"), 3, io, events, timeout);
}

VALUE
rb_fiber_scheduler_kernel_sleep(VALUE scheduler, VALUE duration)
{
    return rb_funcall(scheduler, rb_intern("kernel_sleep"), 1, duration);
}

/* ------------------------------------------------------------------ */
/* What CRuby's ext/openssl needs besides the above.                   */

void
rb_attr(VALUE klass, ID id, int need_reader, int need_writer, int honour_visibility)
{
    /* Called from an Init function, where CRuby's visibility in scope is
       public: both spellings define public accessors (rb_define_attr). */
    (void)honour_visibility;
    rb_define_attr(klass, rb_id2name(id), need_reader, need_writer);
}

VALUE
rb_str_drop_bytes(VALUE str, long len)
{
    rb_funcall(str, rb_intern("bytesplice"), 3, LONG2NUM(0), LONG2NUM(len), rb_str_new(NULL, 0));
    return str;
}

static __thread VALUE capi_verbose, capi_debug;

VALUE *
rb_ruby_verbose_ptr(void)
{
    capi_verbose = rb_gv_get("$VERBOSE");
    return &capi_verbose;
}

VALUE *
rb_ruby_debug_ptr(void)
{
    capi_debug = rb_gv_get("$DEBUG");
    return &capi_debug;
}

/* Re-entering Ruby from inside a blocking region is not supported: the
 * region's function runs with no Ruby call to return to (on a pool thread
 * under the green scheduler). openssl's key generation asks for pending
 * interrupts this way; skipping it delivers them once the region returns. */
void *
rb_thread_call_with_gvl(void *(*func)(void *), void *data1)
{
    if (capi_in_nogvl || !capi_cur)
        return NULL;
    return func(data1);
}

/* ---------------------------------------------------------------------
 * ruby/vm.h
 * --------------------------------------------------------------------- */

/* CRuby runs these when the VM is destructed, after the at_exit handlers;
   here, when the process exits (libc atexit, which runs after monoruby's
   exit sequence), last registered first. They get no VM: a NULL handle. */
static void (*vm_at_exit_fns[32])(ruby_vm_t *);
static int vm_at_exit_len;

static void
run_vm_at_exit(void)
{
    while (vm_at_exit_len > 0)
        vm_at_exit_fns[--vm_at_exit_len](NULL);
}

void
ruby_vm_at_exit(void (*func)(ruby_vm_t *))
{
    if (vm_at_exit_len == (int)(sizeof(vm_at_exit_fns) / sizeof(vm_at_exit_fns[0])))
        return;
    if (vm_at_exit_len == 0)
        atexit(run_vm_at_exit);
    vm_at_exit_fns[vm_at_exit_len++] = func;
}
