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
#include "ruby.h"
#include "ruby/encoding.h"
#include "ruby/thread.h"
#include "ruby/util.h"
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

struct capi_frame {
    MrContext *ctx;
    MrValue block;
    struct capi_frame *prev;
    /* The frame of the Ruby call this one belongs to (itself for a
     * dispatch frame); it owns the C strings and the Array copies
     * handed out during the call. */
    struct capi_frame *owner;
    struct capi_buf *bufs;
    struct capi_ary_slot ary[CAPI_ARY_SLOTS];
    unsigned ary_next;
    jmp_buf jb;
};

static __thread struct capi_frame *capi_cur;
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

/* Leave for the innermost frame; the error is pending in the interpreter. */
static CAPI_NORETURN void
capi_unwind(void)
{
    longjmp(frame()->jb, 1);
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
    for (unsigned i = 0; i < CAPI_ARY_SLOTS; i++) {
        free(f->ary[i].buf);
        f->ary[i].buf = NULL;
    }
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
    rb_eMathDomainError;
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

struct capi_method {
    VALUE (*func)(ANYARGS);
    int arity;
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

/* The MrMethodDataFn of every method defined through this layer. */
static MrValue
capi_dispatch(MrContext *c, MrValue self, int argc, const MrValue *argv, MrValue block, void *data)
{
    struct capi_frame fr;
    struct capi_frame *volatile fp = &fr;
    volatile MrValue result = MR_UNDEF;
    fr.ctx = c;
    fr.block = block;
    fr.prev = capi_cur;
    fr.owner = &fr;
    fr.bufs = NULL;
    memset(fr.ary, 0, sizeof(fr.ary));
    fr.ary_next = 0;
    capi_cur = &fr;
    if (setjmp(fr.jb) == 0) {
        VALUE small[CAPI_SMALL_ARGS];
        VALUE *a = rb_args(argc, argv, small);
        result = M(call_cfunc((const struct capi_method *)data, R(self), argc, a));
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
        c->api->size < offsetof(MrApi, call_super) + sizeof(void *))
        return -2;
    api = c->api;
    struct capi_frame fr;
    struct capi_frame *volatile fp = &fr;
    volatile int rc = -1;
    fr.ctx = c;
    fr.block = MR_UNDEF;
    fr.prev = capi_cur;
    fr.owner = &fr;
    fr.bufs = NULL;
    memset(fr.ary, 0, sizeof(fr.ary));
    fr.ary_next = 0;
    capi_cur = &fr;
    if (setjmp(fr.jb) == 0) {
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
            if (len == LEN_BIG_L)
                EMIT(long double);
            else
                EMIT(double);
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
    fr.ctx = outer->ctx;
    fr.block = outer->block;
    fr.prev = outer;
    fr.owner = outer->owner;
    fr.bufs = NULL;
    memset(fr.ary, 0, sizeof(fr.ary));
    fr.ary_next = 0;
    capi_cur = &fr;
    if (setjmp(fr.jb) == 0) {
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
            capi_errinfo = Qnil;
            return r_proc ? r_proc(data2, exc) : Qnil;
        }
    }
    rb_jump_tag(state);
}

VALUE
rb_rescue2(VALUE (*b_proc)(VALUE), VALUE data1, VALUE (*r_proc)(VALUE, VALUE), VALUE data2, ...)
{
    VALUE classes[16];
    int n = 0;
    va_list ap;
    va_start(ap, data2);
    for (;;) {
        VALUE k = va_arg(ap, VALUE);
        if (k == 0 || n == 16)
            break;
        classes[n++] = k;
    }
    va_end(ap);
    return vrescue(b_proc, data1, r_proc, data2, classes, n);
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
    MrValue r = api->funcall_sym(saved->ctx, M(recv), (MrSym)mid, argc,
                                 mr_args(argc, argv, small), block);
    capi_cur = saved;
    return check(r);
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
rb_intern(const char *name)
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

static void
define_cfunc(VALUE klass, const char *name, VALUE (*func)(ANYARGS), int argc, uint32_t flags)
{
    if (argc < -2 || argc > 15)
        rb_raise(rb_eArgError, "arity out of range: %d for -2..15", argc);
    struct capi_method *m = malloc(sizeof(*m));
    if (!m)
        rb_memerror();
    m->func = func;
    m->arity = argc;
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
    return rb_funcallv(klass, rb_intern("new"), argc, argv);
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
    static VALUE um;
    rb_funcall(object_method("freeze", &um), rb_intern("bind_call"), 1, obj);
    return obj;
}

VALUE
rb_obj_frozen_p(VALUE obj)
{
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
    return rb_sprintf("#<%" PRIsVALUE ">", rb_obj_class(obj));
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

VALUE
rb_iv_get(VALUE obj, const char *name)
{
    return check(api->ivar_get(ctx(), M(obj), name));
}

VALUE
rb_iv_set(VALUE obj, const char *name, VALUE val)
{
    check_rc(api->ivar_set(ctx(), M(obj), name, M(val)));
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
    return rb_eval_string(gvar_name(name, buf, sizeof(buf)));
}

VALUE
rb_gv_set(const char *name, VALUE val)
{
    char gv[256], code[300];
    snprintf(code, sizeof(code), "->(v) { %s = v }", gvar_name(name, gv, sizeof(gv)));
    return rb_funcall(rb_eval_string(code), rb_intern("call"), 1, val);
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
    uint8_t *p = api->str_cptr(ctx(), M(str), &len);
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
    return force_encoding(rb_str_new(ptr, len), "US-ASCII");
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
    char *p = RSTRING_PTR(s);
    long len = RSTRING_LEN(s);
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
    (void)RSTRING_PTR(str);
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
    for (unsigned i = 0; i < CAPI_ARY_SLOTS; i++) {
        if (owner->ary[i].buf && owner->ary[i].ary == a) {
            slot = &owner->ary[i];
            break;
        }
    }
    if (!slot) {
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

rb_encoding *
rb_enc_get(VALUE obj)
{
    if (RB_TYPE_P(obj, T_STRING))
        return enc_of_name(check(api->str_encoding(ctx(), M(obj))));
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
    return force_encoding(obj, enc ? enc->name : "ASCII-8BIT");
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

int
rb_enc_asciicompat(rb_encoding *enc)
{
    return RTEST(rb_funcall(rb_enc_from_encoding(enc), rb_intern("ascii_compatible?"), 0));
}

int
rb_enc_str_asciionly_p(VALUE str)
{
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
};

static int64_t
nogvl_trampoline(void *arg)
{
    struct nogvl_call *c = arg;
    c->result = c->func(c->data);
    return 0;
}

void *
rb_nogvl(void *(*func)(void *), void *data1, rb_unblock_function_t *ubf, void *data2, int flags)
{
    (void)ubf;
    (void)data2;
    (void)flags;
    struct capi_frame *saved = frame();
    struct nogvl_call c = {func, data1, NULL};
    int64_t out;
    int rc = api->call_blocking(saved->ctx, nogvl_trampoline, &c, &out);
    capi_cur = saved;
    if (rc != 0)
        capi_unwind();
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
    return rb_funcall(rb_cThread, rb_intern("current"), 0);
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

static VALUE
data_wrap(VALUE klass, void *datap, RUBY_DATA_FUNC dmark, RUBY_DATA_FUNC dfree, const rb_data_type_t *type)
{
    klass = data_class(klass);
    struct capi_data *d = malloc(sizeof(*d));
    if (!d)
        rb_memerror();
    d->data = datap;
    d->dmark = dmark;
    d->dfree = dfree;
    d->type = type;
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
    struct capi_data *d = api->native_data(ctx(), M(obj), &data_ops);
    if (d)
        return d;
    if (api->type_of(ctx(), M(obj)) == MR_T_NATIVE) {
        VALUE klass = rb_obj_class(obj);
        rb_alloc_func_t *alloc = rb_get_alloc_func(klass);
        if (alloc) {
            VALUE tmp = alloc(klass);
            d = api->native_data(ctx(), M(tmp), &data_ops);
            if (!d)
                rb_raise(rb_eTypeError, "allocator of %s did not return a Data object", rb_class2name(klass));
            api->native_take(ctx(), M(tmp));
            check_rc(api->native_set(ctx(), M(obj), d, &data_ops));
            return d;
        }
    }
    if (raise)
        rb_raise(rb_eTypeError, "wrong argument type %s (expected Data)", displaying_class_of(obj));
    return NULL;
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
    (void)kw_splat;
    rb_obj_call_init(obj, argc, argv);
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
    fr.ctx = c;
    fr.block = NIL_P(b->block) ? MR_UNDEF : M(b->block);
    fr.prev = capi_cur;
    fr.owner = &fr;
    fr.bufs = NULL;
    memset(fr.ary, 0, sizeof(fr.ary));
    fr.ary_next = 0;
    capi_cur = &fr;
    if (setjmp(fr.jb) == 0) {
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

VALUE
rb_io_puts(int argc, const VALUE *argv, VALUE io)
{
    return rb_funcallv(io, rb_intern("puts"), argc, argv);
}

VALUE
rb_io_print(int argc, const VALUE *argv, VALUE io)
{
    return rb_funcallv(io, rb_intern("print"), argc, argv);
}

VALUE
rb_io_printf(int argc, const VALUE *argv, VALUE io)
{
    return rb_funcallv(io, rb_intern("printf"), argc, argv);
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
    return io;
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
    (void)enc;
    return 1;
}

int
rb_enc_mbmaxlen(rb_encoding *enc)
{
    return enc_is_utf8(enc) ? 4 : 1;
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

void
onig_region_free(OnigRegion *region, int free_self)
{
    onig()->region_free(region, free_self);
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
