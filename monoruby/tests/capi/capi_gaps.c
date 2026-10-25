/*
 * API that real gems' extensions (psych, sqlite3, nokogiri, zlib, racc)
 * use and an earlier layer lacked: non-local exits through rb_protect,
 * rb_integer_pack, rb_range_beg_len, rb_utf8_str_new_static,
 * rb_enc_str_buf_cat, rb_io_extract_encoding_option, hidden instance
 * variables, rb_pid_t and STRINGIZE. Built against CRuby and monoruby by
 * tests/capi.rs; the output must be identical.
 */
#include <ruby.h>
#include <ruby/encoding.h>
#include <ruby/io.h>
#include <ruby/oniguruma.h>
#include <string.h>

static VALUE
g_yield(VALUE arg)
{
    return rb_yield(arg);
}

/* Runs the block under rb_protect and re-raises whatever left it, as
 * psych's parser does around its event handlers. */
static VALUE
g_protect_yield(VALUE self, VALUE arg)
{
    (void)self;
    int state = 0;
    VALUE r = rb_protect(g_yield, arg, &state);
    if (state)
        rb_jump_tag(state);
    return rb_ary_new_from_args(2, ID2SYM(rb_intern("normal")), r);
}

static VALUE
g_noop(VALUE arg)
{
    (void)arg;
    return Qnil;
}

static VALUE
g_ensure_yield(VALUE self, VALUE arg)
{
    (void)self;
    return rb_ensure(g_yield, arg, g_noop, Qnil);
}

/* [result, the bytes] of rb_integer_pack into numwords * wordsize bytes */
static VALUE
g_pack(VALUE self, VALUE v, VALUE numwords, VALUE wordsize, VALUE flags)
{
    (void)self;
    unsigned char buf[64];
    size_t n = NUM2SIZET(numwords), w = NUM2SIZET(wordsize);
    if (n * w > sizeof(buf))
        rb_raise(rb_eArgError, "too big");
    memset(buf, 0xaa, sizeof(buf));
    int r = rb_integer_pack(v, buf, n, w, 0, NUM2INT(flags));
    return rb_ary_new_from_args(2, INT2FIX(r), rb_str_new((const char *)buf, (long)(n * w)));
}

static VALUE
g_flags(VALUE self)
{
    (void)self;
    return rb_ary_new_from_args(4, INT2FIX(INTEGER_PACK_LITTLE_ENDIAN), INT2FIX(INTEGER_PACK_BIG_ENDIAN),
                                INT2FIX(INTEGER_PACK_2COMP), INT2FIX(INTEGER_PACK_NATIVE_BYTE_ORDER));
}

static VALUE
g_beg_len(VALUE self, VALUE range, VALUE len, VALUE err)
{
    (void)self;
    long beg = -1, rlen = -1;
    VALUE r = rb_range_beg_len(range, &beg, &rlen, NUM2LONG(len), NUM2INT(err));
    if (r == Qtrue)
        return rb_ary_new_from_args(3, Qtrue, LONG2NUM(beg), LONG2NUM(rlen));
    return rb_ary_new_from_args(1, r);
}

static VALUE
g_strings(VALUE self)
{
    (void)self;
    VALUE s = rb_utf8_str_new_static("caf\xc3\xa9", 5);
    VALUE b = rb_str_buf_new(0);
    rb_enc_str_buf_cat(b, "\xc3\xa9t\xc3\xa9", 6, rb_utf8_encoding());
    return rb_ary_new_from_args(4, s, rb_enc_from_encoding(rb_enc_get(s)), b, rb_enc_from_encoding(rb_enc_get(b)));
}

static VALUE
enc_or_nil(rb_encoding *e)
{
    return e ? rb_enc_from_encoding(e) : Qnil;
}

static VALUE
g_extract(VALUE self, VALUE opts)
{
    (void)self;
    rb_encoding *enc = NULL, *enc2 = NULL;
    int r = rb_io_extract_encoding_option(opts, &enc, &enc2, NULL);
    return rb_ary_new_from_args(3, INT2FIX(r), enc_or_nil(enc), enc_or_nil(enc2));
}

/* A name without '@' is invisible to Ruby, as sqlite3's "-aggregators". */
static VALUE
g_hidden(VALUE self, VALUE obj)
{
    (void)self;
    ID id = rb_intern("-hidden");
    VALUE before = rb_ivar_defined(obj, id);
    rb_iv_set(obj, "-hidden", INT2FIX(7));
    return rb_ary_new_from_args(4, before, rb_ivar_defined(obj, id), rb_iv_get(obj, "-hidden"),
                                rb_funcall(obj, rb_intern("instance_variables"), 0));
}

static VALUE
g_misc(VALUE self)
{
    (void)self;
    rb_pid_t pid = getpid();
    return rb_ary_new_from_args(2, rb_str_new_cstr(STRINGIZE(1.2.3)), pid > 0 ? Qtrue : Qfalse);
}

void
Init_capi_gaps(void)
{
    VALUE m = rb_define_module("CapiGaps");
    rb_define_singleton_method(m, "protect_yield", g_protect_yield, 1);
    rb_define_singleton_method(m, "ensure_yield", g_ensure_yield, 1);
    rb_define_singleton_method(m, "pack", g_pack, 4);
    rb_define_singleton_method(m, "flags", g_flags, 0);
    rb_define_singleton_method(m, "beg_len", g_beg_len, 3);
    rb_define_singleton_method(m, "strings", g_strings, 0);
    rb_define_singleton_method(m, "extract", g_extract, 1);
    rb_define_singleton_method(m, "hidden", g_hidden, 1);
    rb_define_singleton_method(m, "misc", g_misc, 0);
}
