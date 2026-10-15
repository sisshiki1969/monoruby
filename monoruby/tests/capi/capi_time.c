/*
 * The Time API, rb_keyword_given_p, rb_enc_codepoint_len beyond UTF-8,
 * OBJ_INIT_COPY and the config.h / UNREACHABLE answers an extension
 * such as strptime builds on. Built against CRuby and against
 * monoruby's layer; tests/capi.rs compares what capi_time.rb prints.
 */
#include <ruby.h>
#include <ruby/encoding.h>
#include <limits.h>
#include <time.h>

#if !defined(HAVE_SYS_TIME_H) || !defined(HAVE_CLOCK_GETTIME) || !defined(HAVE_STRUCT_TM_TM_GMTOFF)
#error "config.h lacks the time answers"
#endif

static VALUE mT;

/* T.nano_new(sec, nsec), T.usec_new(sec, usec) */
static VALUE
t_nano_new(VALUE self, VALUE sec, VALUE nsec)
{
    (void)self;
    return rb_time_nano_new(NUM2TIMET(sec), NUM2LONG(nsec));
}

static VALUE
t_usec_new(VALUE self, VALUE sec, VALUE usec)
{
    (void)self;
    return rb_time_new(NUM2TIMET(sec), NUM2LONG(usec));
}

/* T.ts_new(sec, nsec, mode): mode 0 = local, 1 = UTC, else the offset */
static VALUE
t_ts_new(VALUE self, VALUE sec, VALUE nsec, VALUE mode)
{
    struct timespec ts;
    int m = NUM2INT(mode);
    (void)self;
    ts.tv_sec = NUM2TIMET(sec);
    ts.tv_nsec = NUM2LONG(nsec);
    return rb_time_timespec_new(&ts, m == 0 ? INT_MAX : m == 1 ? INT_MAX - 1 : m);
}

/* T.num_new(num, off) */
static VALUE
t_num_new(VALUE self, VALUE num, VALUE off)
{
    (void)self;
    return rb_time_num_new(num, off);
}

static VALUE
ts_to_ary(struct timespec ts)
{
    return rb_ary_new3(2, TIMET2NUM(ts.tv_sec), LONG2NUM(ts.tv_nsec));
}

static VALUE
tv_to_ary(struct timeval tv)
{
    return rb_ary_new3(2, TIMET2NUM(tv.tv_sec), LONG2NUM((long)tv.tv_usec));
}

/* T.timespec(v) -> [sec, nsec]; T.interval(v) likewise (rejects negatives
 * and Time); T.timeval(v) / T.timeval_interval(v) -> [sec, usec] */
static VALUE
t_timespec(VALUE self, VALUE v)
{
    (void)self;
    return ts_to_ary(rb_time_timespec(v));
}

static VALUE
t_interval(VALUE self, VALUE v)
{
    (void)self;
    return ts_to_ary(rb_time_timespec_interval(v));
}

static VALUE
t_timeval(VALUE self, VALUE v)
{
    (void)self;
    return tv_to_ary(rb_time_timeval(v));
}

static VALUE
t_timeval_interval(VALUE self, VALUE v)
{
    (void)self;
    return tv_to_ary(rb_time_interval(v));
}

/* T.utc_offset(t) */
static VALUE
t_utc_offset(VALUE self, VALUE t)
{
    (void)self;
    return rb_time_utc_offset(t);
}

/* T.now_plausible -> whether rb_timespec_now is within a minute of
 * Time.now, with a sane nsec */
static VALUE
t_now_plausible(VALUE self)
{
    struct timespec ts;
    time_t now = NUM2TIMET(rb_funcall(rb_funcall(rb_cTime, rb_intern("now"), 0), rb_intern("to_i"), 0));
    (void)self;
    rb_timespec_now(&ts);
    return (ts.tv_sec >= now - 60 && ts.tv_sec <= now + 60 && ts.tv_nsec >= 0 && ts.tv_nsec < 1000000000L)
               ? Qtrue : Qfalse;
}

/* T.kw(*args) -> [argc, rb_keyword_given_p] ; T.kw2(a, b) the same for
 * a fixed arity */
static VALUE
t_kw(int argc, VALUE *argv, VALUE self)
{
    (void)argv;
    (void)self;
    return rb_ary_new3(2, INT2FIX(argc), rb_keyword_given_p() ? Qtrue : Qfalse);
}

static VALUE
t_kw2(VALUE self, VALUE a, VALUE b)
{
    (void)self;
    (void)a;
    (void)b;
    return rb_keyword_given_p() ? Qtrue : Qfalse;
}

/* T.kw0 -> rb_keyword_given_p for a method that takes nothing */
static VALUE
t_kw0(VALUE self)
{
    (void)self;
    return rb_keyword_given_p() ? Qtrue : Qfalse;
}

/* T.kw_funcall(h) -> T.kw(h) through rb_funcall, which passes no
 * keywords */
static VALUE
t_kw_funcall(VALUE self, VALUE h)
{
    return rb_funcall(self, rb_intern("kw"), 1, h);
}

/* CapiTime::Base#kw(*args), the same as T.kw as an instance method, so
 * a Ruby subclass can reach it with super */
static VALUE
t_base_kw(int argc, VALUE *argv, VALUE self)
{
    return t_kw(argc, argv, self);
}

/* T.codepoint(str) -> [code point, byte length] of str's first character */
static VALUE
t_codepoint(VALUE self, VALUE str)
{
    int len = 0;
    unsigned int cp;
    (void)self;
    StringValue(str);
    cp = rb_enc_codepoint_len(RSTRING_PTR(str), RSTRING_END(str), &len, rb_enc_get(str));
    return rb_ary_new3(2, UINT2NUM(cp), INT2FIX(len));
}

/* T.init_copy(obj, orig) -> what OBJ_INIT_COPY answers */
static VALUE
t_init_copy(VALUE self, VALUE obj, VALUE orig)
{
    (void)self;
    return OBJ_INIT_COPY(obj, orig) ? Qtrue : Qfalse;
}

/* T.classify(v) -> "int" / "flt" / "other", through RB_INTEGER_TYPE_P
 * and a switch whose impossible arm is UNREACHABLE */
static VALUE
t_classify(VALUE self, VALUE v)
{
    int k = RB_INTEGER_TYPE_P(v) ? 0 : RB_FLOAT_TYPE_P(v) ? 1 : 2;
    (void)self;
    switch (k) {
      case 0: return rb_str_new_cstr("int");
      case 1: return rb_str_new_cstr("flt");
      case 2: return rb_str_new_cstr("other");
    }
    UNREACHABLE;
}

/* T.timet_roundtrip(n) -> TIMET2NUM(NUM2TIMET(n)) */
static VALUE
t_timet_roundtrip(VALUE self, VALUE n)
{
    (void)self;
    return TIMET2NUM(NUM2TIMET(n));
}

void
Init_capi_time(void)
{
    mT = rb_define_module("CapiTime");
    rb_define_singleton_method(mT, "nano_new", t_nano_new, 2);
    rb_define_singleton_method(mT, "usec_new", t_usec_new, 2);
    rb_define_singleton_method(mT, "ts_new", t_ts_new, 3);
    rb_define_singleton_method(mT, "num_new", t_num_new, 2);
    rb_define_singleton_method(mT, "timespec", t_timespec, 1);
    rb_define_singleton_method(mT, "interval", t_interval, 1);
    rb_define_singleton_method(mT, "timeval", t_timeval, 1);
    rb_define_singleton_method(mT, "timeval_interval", t_timeval_interval, 1);
    rb_define_singleton_method(mT, "utc_offset", t_utc_offset, 1);
    rb_define_singleton_method(mT, "now_plausible", t_now_plausible, 0);
    rb_define_singleton_method(mT, "kw", t_kw, -1);
    rb_define_singleton_method(mT, "kw2", t_kw2, 2);
    rb_define_singleton_method(mT, "kw0", t_kw0, 0);
    rb_define_singleton_method(mT, "kw_funcall", t_kw_funcall, 1);
    rb_define_method(rb_define_class_under(mT, "Base", rb_cObject), "kw", t_base_kw, -1);
    /* Outside any method call there are no keywords. */
    rb_define_const(mT, "KW_AT_INIT", rb_keyword_given_p() ? Qtrue : Qfalse);
    rb_define_singleton_method(mT, "codepoint", t_codepoint, 1);
    rb_define_singleton_method(mT, "init_copy", t_init_copy, 2);
    rb_define_singleton_method(mT, "classify", t_classify, 1);
    rb_define_singleton_method(mT, "timet_roundtrip", t_timet_roundtrip, 1);
}
