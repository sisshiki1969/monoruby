/*
 * A C extension written against CRuby's C API, with nothing
 * monoruby-specific in it. tests/capi.rs builds it twice — against the
 * host CRuby's headers and against monoruby_capi/include (+ capi.c) —
 * and runs the same script on both interpreters.
 */
#include <errno.h>
#include <ruby.h>
#include <ruby/encoding.h>
#include <ruby/thread.h>

static VALUE mHello, cPoint, eHelloError;
static VALUE cached;
static ID id_x, id_y, id_call;

/* ---- arity variants ---- */

static VALUE
hello_add(VALUE self, VALUE a, VALUE b)
{
    return LONG2NUM(NUM2LONG(a) + NUM2LONG(b));
}

static VALUE
hello_zero(VALUE self)
{
    return INT2FIX(0);
}

static VALUE
hello_count(int argc, VALUE *argv, VALUE self)
{
    VALUE ary = rb_ary_new();
    for (int i = 0; i < argc; i++)
        rb_ary_push(ary, argv[i]);
    return rb_ary_new_from_args(2, INT2FIX(argc), ary);
}

static VALUE
hello_args_ary(VALUE self, VALUE args)
{
    return rb_ary_reverse(args);
}

static VALUE
hello_six(VALUE self, VALUE a, VALUE b, VALUE c, VALUE d, VALUE e, VALUE f)
{
    return rb_ary_new_from_args(6, f, e, d, c, b, a);
}

/* rb_scan_args("12*:&") */
static VALUE
hello_scan(int argc, VALUE *argv, VALUE self)
{
    VALUE req, opt1, opt2, rest, kw, blk;
    int n = rb_scan_args(argc, argv, "12*:&", &req, &opt1, &opt2, &rest, &kw, &blk);
    return rb_ary_new_from_args(7, INT2FIX(n), req, opt1, opt2, rest, kw,
                                NIL_P(blk) ? Qfalse : rb_funcall(blk, id_call, 1, req));
}

static VALUE
hello_kwargs(int argc, VALUE *argv, VALUE self)
{
    VALUE opts;
    ID table[3];
    VALUE values[3];
    table[0] = rb_intern("name");
    table[1] = rb_intern("size");
    table[2] = rb_intern("color");
    rb_scan_args(argc, argv, ":", &opts);
    rb_get_kwargs(opts, table, 1, 2, values);
    return rb_ary_new_from_args(3, values[0], values[1] == Qundef ? ID2SYM(rb_intern("none")) : values[1],
                                values[2] == Qundef ? ID2SYM(rb_intern("none")) : values[2]);
}

/* ---- values ---- */

static VALUE
hello_types(VALUE self, VALUE v)
{
    const char *t;
    switch (TYPE(v)) {
    case T_NIL: t = "nil"; break;
    case T_TRUE: t = "true"; break;
    case T_FALSE: t = "false"; break;
    case T_FIXNUM: t = "fixnum"; break;
    case T_BIGNUM: t = "bignum"; break;
    case T_FLOAT: t = "float"; break;
    case T_SYMBOL: t = "symbol"; break;
    case T_STRING: t = "string"; break;
    case T_ARRAY: t = "array"; break;
    case T_HASH: t = "hash"; break;
    case T_CLASS: t = "class"; break;
    case T_MODULE: t = "module"; break;
    case T_REGEXP: t = "regexp"; break;
    case T_STRUCT: t = "struct"; break;
    case T_OBJECT: t = "object"; break;
    case T_DATA: t = "data"; break;
    default: t = "other"; break;
    }
    return rb_str_new_cstr(t);
}

static VALUE
hello_truthy(VALUE self, VALUE v)
{
    return rb_ary_new_from_args(3, RTEST(v) ? Qtrue : Qfalse, NIL_P(v) ? Qtrue : Qfalse,
                                SPECIAL_CONST_P(v) ? Qtrue : Qfalse);
}

static VALUE
hello_nums(VALUE self, VALUE v)
{
    long l = NUM2LONG(v);
    double d = NUM2DBL(v);
    return rb_ary_new_from_args(5, LONG2NUM(l * 2), DBL2NUM(d / 4), INT2NUM((int)(l & 0xffff)),
                                ULONG2NUM((unsigned long)l), LL2NUM((long long)l - 1));
}

static VALUE
hello_int(VALUE self, VALUE v)
{
    return INT2NUM(NUM2INT(v));
}

static VALUE
hello_uint(VALUE self, VALUE v)
{
    return UINT2NUM(NUM2UINT(v));
}

static VALUE
hello_big(VALUE self)
{
    return rb_ary_new_from_args(4, ULL2NUM(18446744073709551615ULL), LL2NUM(-9223372036854775807LL - 1),
                                ULONG2NUM(9223372036854775808UL), LONG2NUM(4611686018427387904L));
}

static VALUE
hello_ull(VALUE self, VALUE v)
{
    unsigned long long u = NUM2ULL(v);
    return rb_sprintf("%llu", u);
}

static VALUE
hello_float(VALUE self, VALUE v)
{
    return DBL2NUM(RFLOAT_VALUE(rb_to_float(v)) * 1.5);
}

/* ---- strings ---- */

static VALUE
hello_greet(VALUE self, VALUE name)
{
    VALUE s = rb_str_new_cstr("Hello, ");
    rb_str_append(s, name);
    rb_str_cat(s, "!", 1);
    return s;
}

static VALUE
hello_upcase(VALUE self, VALUE str)
{
    StringValue(str);
    VALUE out = rb_str_new(NULL, RSTRING_LEN(str));
    char *dst = RSTRING_PTR(out);
    const char *src = RSTRING_PTR(str);
    for (long i = 0; i < RSTRING_LEN(str); i++)
        dst[i] = (src[i] >= 'a' && src[i] <= 'z') ? src[i] - 32 : src[i];
    return out;
}

static VALUE
hello_cstr(VALUE self, VALUE str)
{
    const char *p = StringValueCStr(str);
    return LONG2NUM((long)strlen(p));
}

static VALUE
hello_strings(VALUE self)
{
    VALUE b = rb_str_new("a\0b", 3);
    VALUE u = rb_utf8_str_new_cstr("\xe3\x81\x82");
    VALUE a = rb_usascii_str_new_cstr("ascii");
    VALUE e = rb_enc_str_new("xyz", 3, rb_utf8_encoding());
    VALUE r = rb_str_resize(rb_str_new_cstr("truncate me"), 8);
    VALUE f = rb_sprintf("%d-%s-%05.1f-%" PRIsVALUE "-%+" PRIsVALUE "-%ld%%", 42, "str", 3.14159,
                         ID2SYM(rb_intern("sym")), rb_str_new_cstr("q"), 123456789L);
    VALUE buf = rb_str_buf_new(10);
    rb_str_catf(buf, "<%x>", 255);
    rb_str_cat2(buf, "tail");
    return rb_ary_new_from_args(9, b, rb_str_length(u), rb_enc_associate(rb_str_dup(u), rb_ascii8bit_encoding()),
                                rb_str_new_cstr(rb_enc_get(a)->name), e, r, f, buf,
                                rb_str_new_cstr(rb_enc_name(rb_enc_get(u))));
}

static VALUE
hello_frozen_cat(VALUE self, VALUE str)
{
    rb_str_cat2(str, "x");
    return str;
}

/* ---- symbols ---- */

static VALUE
hello_syms(VALUE self, VALUE name)
{
    ID id = rb_to_id(name);
    VALUE sym = ID2SYM(id);
    return rb_ary_new_from_args(4, sym, rb_str_new_cstr(rb_id2name(id)), rb_sym2str(sym),
                                SYM2ID(sym) == rb_intern(rb_id2name(id)) ? Qtrue : Qfalse);
}

/* ---- arrays ---- */

static VALUE
hello_sum(VALUE self, VALUE ary)
{
    Check_Type(ary, T_ARRAY);
    long sum = 0;
    for (long i = 0; i < RARRAY_LEN(ary); i++)
        sum += NUM2LONG(RARRAY_AREF(ary, i));
    const VALUE *p = RARRAY_CONST_PTR(ary);
    long sum2 = 0;
    for (long i = 0; i < RARRAY_LEN(ary); i++)
        sum2 += NUM2LONG(p[i]);
    return rb_ary_new_from_args(2, LONG2NUM(sum), LONG2NUM(sum2));
}

static VALUE
hello_arrays(VALUE self)
{
    VALUE a = rb_ary_new_capa(4);
    for (int i = 0; i < 5; i++)
        rb_ary_push(a, INT2FIX(i * i));
    rb_ary_store(a, 7, INT2FIX(-1));
    VALUE popped = rb_ary_pop(a);
    VALUE shifted = rb_ary_shift(a);
    rb_ary_unshift(a, ID2SYM(rb_intern("first")));
    VALUE vals[] = {INT2FIX(1), INT2FIX(2)};
    VALUE b = rb_ary_new_from_values(2, vals);
    rb_ary_concat(b, rb_ary_new_from_args(1, INT2FIX(3)));
    return rb_ary_new_from_args(7, a, popped, shifted, rb_ary_entry(a, -1), rb_ary_entry(a, 100),
                                rb_ary_join(b, rb_str_new_cstr("+")), rb_ary_includes(b, INT2FIX(2)));
}

/* ---- hashes ---- */

static int
collect_pair(VALUE key, VALUE val, VALUE acc)
{
    rb_ary_push(acc, rb_ary_new_from_args(2, key, val));
    return ST_CONTINUE;
}

static int
stop_at_b(VALUE key, VALUE val, VALUE acc)
{
    rb_ary_push(acc, key);
    return key == ID2SYM(rb_intern("b")) ? ST_STOP : ST_CONTINUE;
}

static VALUE
hello_hashes(VALUE self, VALUE h)
{
    VALUE acc = rb_ary_new();
    rb_hash_foreach(h, collect_pair, acc);
    VALUE stopped = rb_ary_new();
    rb_hash_foreach(h, stop_at_b, stopped);
    VALUE n = rb_hash_new();
    VALUE key = rb_str_new_cstr("key");
    rb_hash_aset(n, key, INT2FIX(1));
    rb_hash_aset(n, ID2SYM(rb_intern("sym")), INT2FIX(2));
    rb_str_cat2(key, "-mutated");
    return rb_ary_new_from_args(7, acc, stopped, n, rb_hash_aref(n, rb_str_new_cstr("key")),
                                rb_hash_lookup2(n, INT2FIX(99), ID2SYM(rb_intern("missing"))),
                                LONG2NUM((long)RHASH_SIZE(h)), rb_hash_delete(n, ID2SYM(rb_intern("sym"))));
}

/* ---- calling Ruby, blocks ---- */

static VALUE
hello_call(VALUE self, VALUE recv, VALUE name, VALUE arg)
{
    return rb_funcall(recv, rb_to_id(name), 1, arg);
}

static VALUE
hello_each_pair(VALUE self, VALUE ary)
{
    if (!rb_block_given_p())
        return ID2SYM(rb_intern("no_block"));
    VALUE out = rb_ary_new();
    for (long i = 0; i + 1 < RARRAY_LEN(ary); i += 2)
        rb_ary_push(out, rb_yield_values(2, RARRAY_AREF(ary, i), RARRAY_AREF(ary, i + 1)));
    return out;
}

static VALUE
hello_to_proc(VALUE self)
{
    VALUE p = rb_block_proc();
    return rb_proc_call(p, rb_ary_new_from_args(1, INT2FIX(20)));
}

/* ---- exceptions ---- */

static VALUE
hello_raise(VALUE self, VALUE kind)
{
    switch (NUM2INT(kind)) {
    case 0: rb_raise(rb_eArgError, "bad argument: %d", 7);
    case 1: rb_raise(eHelloError, "custom %s", "error");
    case 2: rb_exc_raise(rb_exc_new_cstr(rb_eKeyError, "key!"));
    case 3: rb_raise(rb_eTypeError, "type %" PRIsVALUE, rb_str_new_cstr("mismatch"));
    case 4: errno = ENOENT; rb_sys_fail("open");
    default: rb_raise(rb_eRuntimeError, "unexpected");
    }
    return Qnil;
}

static VALUE
risky(VALUE arg)
{
    if (RTEST(arg))
        rb_raise(rb_eRuntimeError, "risky failed");
    return ID2SYM(rb_intern("ok"));
}

static VALUE
hello_protect(VALUE self, VALUE fail)
{
    int state = 0;
    VALUE r = rb_protect(risky, fail, &state);
    if (state) {
        VALUE err = rb_errinfo();
        rb_set_errinfo(Qnil);
        return rb_ary_new_from_args(3, INT2FIX(state != 0), rb_obj_class(err),
                                    rb_funcall(err, rb_intern("message"), 0));
    }
    return rb_ary_new_from_args(2, INT2FIX(state), r);
}

static VALUE
hello_reraise(VALUE self)
{
    int state = 0;
    rb_protect(risky, Qtrue, &state);
    if (state)
        rb_jump_tag(state);
    return Qnil;
}

static VALUE ensure_log;

static VALUE
log_ensure(VALUE arg)
{
    rb_ary_push(ensure_log, arg);
    return Qnil;
}

static VALUE
hello_ensure(VALUE self, VALUE fail)
{
    ensure_log = rb_ary_new();
    rb_gc_register_address(&ensure_log);
    return rb_ensure(risky, fail, log_ensure, ID2SYM(rb_intern("ensured")));
}

static VALUE
hello_ensure_log(VALUE self)
{
    return ensure_log;
}

static VALUE
on_rescue(VALUE data, VALUE exc)
{
    return rb_ary_new_from_args(2, data, rb_funcall(exc, rb_intern("message"), 0));
}

static VALUE
call_block_body(VALUE arg)
{
    return rb_funcall(arg, id_call, 0);
}

static VALUE
hello_rescue(VALUE self, VALUE blk)
{
    return rb_rescue2(call_block_body, blk, on_rescue, ID2SYM(rb_intern("rescued")), rb_eArgError,
                      rb_eKeyError, (VALUE)0);
}

static VALUE
hello_check_frozen(VALUE self, VALUE obj)
{
    rb_check_frozen(obj);
    return Qtrue;
}

/* ---- objects, ivars, constants ---- */

static VALUE
point_initialize(VALUE self, VALUE x, VALUE y)
{
    rb_ivar_set(self, id_x, x);
    rb_ivar_set(self, id_y, y);
    return self;
}

static VALUE
point_dist2(VALUE self)
{
    long x = NUM2LONG(rb_ivar_get(self, id_x));
    long y = NUM2LONG(rb_ivar_get(self, id_y));
    return LONG2NUM(x * x + y * y);
}

static VALUE
point_to_s(VALUE self)
{
    return rb_sprintf("(%" PRIsVALUE ", %" PRIsVALUE ")", rb_iv_get(self, "@x"), rb_iv_get(self, "@y"));
}

static VALUE
point_secret(VALUE self)
{
    return ID2SYM(rb_intern("secret"));
}

static VALUE
hello_objects(VALUE self)
{
    VALUE args[] = {INT2FIX(3), INT2FIX(4)};
    VALUE p = rb_class_new_instance(2, args, cPoint);
    return rb_ary_new_from_args(7, p, rb_obj_class(p), rb_obj_is_kind_of(p, rb_cObject),
                                rb_obj_is_instance_of(p, rb_cObject), rb_str_new_cstr(rb_obj_classname(p)),
                                rb_respond_to(p, rb_intern("dist2")) ? Qtrue : Qfalse,
                                rb_const_get(mHello, rb_intern("VERSION")));
}

static VALUE
hello_cached(VALUE self)
{
    return cached;
}

static VALUE
hello_gc_churn(VALUE self, VALUE n)
{
    VALUE keep = rb_str_new_cstr("kept across GC");
    for (long i = 0; i < NUM2LONG(n); i++)
        rb_str_new_cstr("garbage garbage garbage garbage garbage garbage");
    rb_gc_start();
    return rb_ary_new_from_args(2, keep, cached);
}

static VALUE
hello_eval(VALUE self, VALUE code)
{
    return rb_eval_string(StringValueCStr(code));
}

static VALUE
hello_range(VALUE self, VALUE r)
{
    VALUE b, e;
    int excl;
    if (!rb_range_values(r, &b, &e, &excl))
        return Qfalse;
    return rb_ary_new_from_args(3, b, e, excl ? Qtrue : Qfalse);
}

struct sum_args {
    long n;
    long result;
};

static void *
sum_nogvl(void *p)
{
    struct sum_args *a = p;
    long s = 0;
    for (long i = 1; i <= a->n; i++)
        s += i;
    a->result = s;
    return a;
}

static VALUE
hello_nogvl(VALUE self, VALUE n)
{
    struct sum_args a = {NUM2LONG(n), 0};
    void *r = rb_thread_call_without_gvl(sum_nogvl, &a, RUBY_UBF_IO, NULL);
    return rb_ary_new_from_args(2, LONG2NUM(a.result), r == &a ? Qtrue : Qfalse);
}

void
Init_capi_hello(void)
{
    id_x = rb_intern("@x");
    id_y = rb_intern("@y");
    id_call = rb_intern("call");

    mHello = rb_define_module("CapiHello");
    rb_define_const(mHello, "VERSION", rb_str_freeze(rb_str_new_cstr("1.0")));
    eHelloError = rb_define_class_under(mHello, "Error", rb_eStandardError);

    cached = rb_ary_new_from_args(2, rb_str_new_cstr("cached"), rb_float_new(2.5));
    rb_gc_register_address(&cached);

    rb_define_module_function(mHello, "add", hello_add, 2);
    rb_define_module_function(mHello, "zero", hello_zero, 0);
    rb_define_module_function(mHello, "count", hello_count, -1);
    rb_define_module_function(mHello, "args_ary", hello_args_ary, -2);
    rb_define_module_function(mHello, "six", hello_six, 6);
    rb_define_module_function(mHello, "scan", hello_scan, -1);
    rb_define_module_function(mHello, "kwargs", hello_kwargs, -1);
    rb_define_module_function(mHello, "types", hello_types, 1);
    rb_define_module_function(mHello, "truthy", hello_truthy, 1);
    rb_define_module_function(mHello, "nums", hello_nums, 1);
    rb_define_module_function(mHello, "int", hello_int, 1);
    rb_define_module_function(mHello, "uint", hello_uint, 1);
    rb_define_module_function(mHello, "big", hello_big, 0);
    rb_define_module_function(mHello, "ull", hello_ull, 1);
    rb_define_module_function(mHello, "float", hello_float, 1);
    rb_define_module_function(mHello, "greet", hello_greet, 1);
    rb_define_module_function(mHello, "upcase", hello_upcase, 1);
    rb_define_module_function(mHello, "cstr", hello_cstr, 1);
    rb_define_module_function(mHello, "strings", hello_strings, 0);
    rb_define_module_function(mHello, "frozen_cat", hello_frozen_cat, 1);
    rb_define_module_function(mHello, "syms", hello_syms, 1);
    rb_define_module_function(mHello, "sum", hello_sum, 1);
    rb_define_module_function(mHello, "arrays", hello_arrays, 0);
    rb_define_module_function(mHello, "hashes", hello_hashes, 1);
    rb_define_module_function(mHello, "call", hello_call, 3);
    rb_define_module_function(mHello, "each_pair", hello_each_pair, 1);
    rb_define_module_function(mHello, "to_proc", hello_to_proc, 0);
    rb_define_module_function(mHello, "raise", hello_raise, 1);
    rb_define_module_function(mHello, "protect", hello_protect, 1);
    rb_define_module_function(mHello, "reraise", hello_reraise, 0);
    rb_define_module_function(mHello, "ensure", hello_ensure, 1);
    rb_define_module_function(mHello, "ensure_log", hello_ensure_log, 0);
    rb_define_module_function(mHello, "rescue", hello_rescue, 1);
    rb_define_module_function(mHello, "check_frozen", hello_check_frozen, 1);
    rb_define_module_function(mHello, "objects", hello_objects, 0);
    rb_define_module_function(mHello, "cached", hello_cached, 0);
    rb_define_module_function(mHello, "gc_churn", hello_gc_churn, 1);
    rb_define_module_function(mHello, "eval", hello_eval, 1);
    rb_define_module_function(mHello, "range", hello_range, 1);
    rb_define_module_function(mHello, "nogvl", hello_nogvl, 1);

    cPoint = rb_define_class_under(mHello, "Point", rb_cObject);
    rb_define_method(cPoint, "initialize", point_initialize, 2);
    rb_define_method(cPoint, "dist2", point_dist2, 0);
    rb_define_method(cPoint, "to_s", point_to_s, 0);
    rb_define_private_method(cPoint, "secret", point_secret, 0);
    rb_define_attr(cPoint, "x", 1, 0);
    rb_define_attr(cPoint, "y", 1, 1);
    rb_define_alias(cPoint, "inspect", "to_s");
}
