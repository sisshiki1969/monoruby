/*
 * The VALUE layout an extension can see: CRuby's special constants and
 * their bit tests, static Symbols, flonums, Qundef, and the paths that
 * carry such values across the layer (arguments and results, C globals,
 * RARRAY_CONST_PTR, blocks, rb_protect, mark functions). Built against
 * CRuby and monoruby by tests/capi.rs; the output must be identical.
 */
#include <ruby.h>
#include <string.h>
#include <ruby/encoding.h>

static VALUE mV;
static VALUE g_val;

/* ---- constants and bit tests ---- */

static VALUE
v_constants(VALUE self)
{
    (void)self;
    return rb_ary_new_from_args(4, LONG2NUM((long)Qfalse), LONG2NUM((long)Qnil),
                                LONG2NUM((long)Qtrue), LONG2NUM((long)Qundef));
}

/* [RTEST, NIL_P, SPECIAL_CONST_P, IMMEDIATE_P, FIXNUM_P, FLONUM_P,
 *  SYMBOL_P, UNDEF_P, C truth] of v */
static VALUE
v_tests(VALUE self, VALUE v)
{
    (void)self;
    return rb_ary_new_from_args(9, RTEST(v) ? Qtrue : Qfalse, NIL_P(v) ? Qtrue : Qfalse,
                                SPECIAL_CONST_P(v) ? Qtrue : Qfalse, IMMEDIATE_P(v) ? Qtrue : Qfalse,
                                FIXNUM_P(v) ? Qtrue : Qfalse, FLONUM_P(v) ? Qtrue : Qfalse,
                                SYMBOL_P(v) ? Qtrue : Qfalse, RB_UNDEF_P(v) ? Qtrue : Qfalse,
                                v ? Qtrue : Qfalse);
}

static VALUE
v_type(VALUE self, VALUE v)
{
    (void)self;
    return INT2FIX(TYPE(v));
}

/* ---- Symbols ---- */

static VALUE
v_sym(VALUE self, VALUE sym)
{
    (void)self;
    ID id = SYM2ID(sym);
    return rb_ary_new_from_args(5, ID2SYM(id), rb_str_new_cstr(rb_id2name(id)),
                                id == rb_intern(rb_id2name(id)) ? Qtrue : Qfalse,
                                rb_id2sym(rb_sym2id(sym)) == sym ? Qtrue : Qfalse,
                                ID2SYM(rb_intern("made_in_c")));
}

/* ---- numbers ---- */

static VALUE
v_num(VALUE self, VALUE v)
{
    (void)self;
    if (FIXNUM_P(v))
        return rb_ary_new_from_args(2, rb_str_new_cstr("fixnum"), LONG2NUM(FIX2LONG(v)));
    if (FLONUM_P(v))
        return rb_ary_new_from_args(2, rb_str_new_cstr("flonum"), rb_float_new(RFLOAT_VALUE(v)));
    if (RB_FLOAT_TYPE_P(v))
        return rb_ary_new_from_args(2, rb_str_new_cstr("heap float"), rb_float_new(RFLOAT_VALUE(v)));
    if (RB_INTEGER_TYPE_P(v))
        return rb_ary_new_from_args(2, rb_str_new_cstr("bignum"), rb_funcall(v, rb_intern("to_s"), 0));
    return Qnil;
}

/* ---- a C global, registered ---- */

static VALUE
v_set_global(VALUE self, VALUE v)
{
    (void)self;
    g_val = v;
    return Qnil;
}

static VALUE
v_global(VALUE self)
{
    (void)self;
    return g_val;
}

/* ---- RARRAY_CONST_PTR ---- */

static VALUE
v_ptr_sum(VALUE self, VALUE ary)
{
    (void)self;
    long n = RARRAY_LEN(ary), sum = 0;
    const VALUE *p = RARRAY_CONST_PTR(ary);
    for (long i = 0; i < n; i++)
        sum += NIL_P(p[i]) ? 0 : FIX2LONG(p[i]);
    return LONG2NUM(sum);
}

/* Two Arrays' pointers live at once; the elements are passed back. */
static VALUE
v_ptr_zip(VALUE self, VALUE a, VALUE b)
{
    (void)self;
    const VALUE *pa = RARRAY_CONST_PTR(a);
    const VALUE *pb = RARRAY_CONST_PTR(b);
    long n = RARRAY_LEN(a) < RARRAY_LEN(b) ? RARRAY_LEN(a) : RARRAY_LEN(b);
    VALUE out = rb_ary_new();
    for (long i = 0; i < n; i++)
        rb_ary_push(out, rb_ary_new_from_args(3, pa[i], pb[i], RARRAY_AREF(a, i) == pa[i] ? Qtrue : Qfalse));
    return out;
}

static VALUE
v_from_values(VALUE self)
{
    (void)self;
    VALUE buf[5] = { Qnil, Qfalse, Qtrue, INT2FIX(7), ID2SYM(rb_intern("s")) };
    return rb_ary_new_from_values(5, buf);
}

/* ---- Qundef out of the API ---- */

static VALUE
v_lookup(VALUE self, VALUE hash, VALUE key)
{
    (void)self;
    VALUE v = rb_hash_lookup2(hash, key, Qundef);
    if (RB_UNDEF_P(v))
        return ID2SYM(rb_intern("undef"));
    return v;
}

static VALUE
v_scan(int argc, VALUE *argv, VALUE self)
{
    (void)self;
    VALUE a, b = Qundef, opts = Qundef;
    rb_scan_args(argc, argv, "11:", &a, &b, &opts);
    return rb_ary_new_from_args(3, a, b, opts);
}

/* ---- blocks, protect, ensure ---- */

static VALUE
v_yield_all(VALUE self)
{
    (void)self;
    VALUE out = rb_ary_new();
    rb_ary_push(out, rb_yield(Qnil));
    rb_ary_push(out, rb_yield(Qfalse));
    rb_ary_push(out, rb_yield(Qtrue));
    rb_ary_push(out, rb_yield(ID2SYM(rb_intern("sym"))));
    rb_ary_push(out, rb_yield(rb_float_new(1.5)));
    rb_ary_push(out, rb_yield_values(2, Qnil, Qfalse));
    return out;
}

static VALUE
v_block_cb(VALUE yielded, VALUE arg, int argc, const VALUE *argv, VALUE blockarg)
{
    (void)argv;
    return rb_ary_new_from_args(4, yielded, arg, INT2FIX(argc), NIL_P(blockarg) ? Qtrue : Qfalse);
}

static VALUE
v_block_call(VALUE self, VALUE ary, VALUE arg)
{
    (void)self;
    return rb_block_call(ary, rb_intern("map"), 0, NULL, v_block_cb, arg);
}

static VALUE
v_return_arg(VALUE arg)
{
    return arg;
}

static VALUE
v_raise_arg(VALUE arg)
{
    rb_raise(rb_eArgError, "raised %s", NIL_P(arg) ? "nil" : "false");
    return Qnil;
}

static VALUE
v_protect(VALUE self, VALUE arg, VALUE raise)
{
    (void)self;
    int state = 0;
    VALUE r = rb_protect(RTEST(raise) ? v_raise_arg : v_return_arg, arg, &state);
    VALUE err = state ? rb_errinfo() : Qnil;
    rb_set_errinfo(Qnil);
    return rb_ary_new_from_args(3, r, INT2FIX(state), NIL_P(err) ? Qnil : rb_funcall(err, rb_intern("message"), 0));
}

static VALUE
v_ensure(VALUE self, VALUE arg)
{
    (void)self;
    return rb_ensure(v_return_arg, arg, v_return_arg, Qtrue);
}

static int
v_each_pair(VALUE key, VALUE val, VALUE out)
{
    rb_ary_push(out, rb_ary_new_from_args(2, key, val));
    return ST_CONTINUE;
}

static VALUE
v_pairs(VALUE self, VALUE hash)
{
    (void)self;
    VALUE out = rb_ary_new();
    rb_hash_foreach(hash, v_each_pair, out);
    return out;
}

static VALUE
v_call_back(VALUE self, VALUE obj, VALUE v)
{
    (void)self;
    return rb_funcall(obj, rb_intern("echo"), 1, v);
}

/* ---- a buffer filled through RSTRING_PTR, then rb_str_set_len ---- */

static VALUE
v_buf_build(VALUE self, VALUE src)
{
    (void)self;
    long n = RSTRING_LEN(src);
    VALUE s = rb_str_buf_new(n + 8);
    char *p = RSTRING_PTR(s);
    for (long i = 0; i < n; i++)
        p[i] = RSTRING_PTR(src)[n - 1 - i];
    rb_str_set_len(s, n);
    rb_enc_associate(s, rb_utf8_encoding());
    rb_str_cat(s, "!", 1);
    return rb_ary_new_from_args(3, s, LONG2NUM(RSTRING_LEN(s)), rb_str_buf_new(0));
}

/* ---- freeze from C is the primitive, whatever `freeze` the class defines ---- */

static VALUE
v_freeze(VALUE self, VALUE obj)
{
    (void)self;
    rb_obj_freeze(obj);
    return rb_obj_frozen_p(obj);
}

/* ---- a TypedData holding a VALUE, marked ---- */

struct holder {
    VALUE v;
};

static void
holder_mark(void *p)
{
    rb_gc_mark(((struct holder *)p)->v);
}

static void
holder_free(void *p)
{
    xfree(p);
}

static const rb_data_type_t holder_type = {
    "CapiValue::Holder",
    { holder_mark, holder_free, NULL, },
    NULL, NULL, RUBY_TYPED_FREE_IMMEDIATELY
};

static VALUE
holder_alloc(VALUE klass)
{
    struct holder *h;
    VALUE obj = TypedData_Make_Struct(klass, struct holder, &holder_type, h);
    h->v = Qnil;
    return obj;
}

static VALUE
holder_set(VALUE self, VALUE v)
{
    struct holder *h;
    TypedData_Get_Struct(self, struct holder, &holder_type, h);
    h->v = v;
    return self;
}

static VALUE
holder_get(VALUE self)
{
    struct holder *h;
    TypedData_Get_Struct(self, struct holder, &holder_type, h);
    return h->v;
}

/* rb_scan_args_kw: the flag decides whether a trailing Hash is keywords. */
static VALUE
v_scan_kw(int argc, VALUE *argv, VALUE self)
{
    VALUE flag = argv[0];
    VALUE a = Qundef, opts = Qundef;
    int n = rb_scan_args_kw(NUM2INT(flag), argc - 1, argv + 1, "01:", &a, &opts);
    return rb_ary_new_from_args(3, INT2NUM(n), a == Qundef ? ID2SYM(rb_intern("undef")) : a,
                                opts == Qundef ? ID2SYM(rb_intern("undef")) : opts);
}

/* rb_check_funcall: the call, or Qundef when the object does not respond. */
static VALUE
v_check_call(VALUE self, VALUE obj, VALUE name)
{
    VALUE r = rb_check_funcall(obj, rb_to_id(name), 0, NULL);
    return r == Qundef ? ID2SYM(rb_intern("undef")) : r;
}

/* rb_str_tmp_new: a String of `len` writable bytes. */
static VALUE
v_tmp_fill(VALUE self, VALUE len, VALUE ch)
{
    long n = NUM2LONG(len);
    VALUE s = rb_str_tmp_new(n);
    memset(RSTRING_PTR(s), NUM2INT(ch), (size_t)n);
    /* The tmp String is hidden on CRuby: hand back a copy of its bytes. */
    return rb_ary_new_from_args(2, LONG2NUM(RSTRING_LEN(s)), rb_str_new(RSTRING_PTR(s), n));
}

/* rb_catch_obj / rb_throw_obj: a throw from C, caught by C or by Ruby. */
static VALUE
v_throw_cb(VALUE tag, VALUE data, int argc, const VALUE *argv, VALUE blockarg)
{
    if (RTEST(data))
        rb_throw_obj(tag, data);
    return ID2SYM(rb_intern("ran"));
}

static VALUE
v_catch_(VALUE self, VALUE tag, VALUE thrown)
{
    return rb_catch_obj(tag, v_throw_cb, thrown);
}

static VALUE
v_throw_(VALUE self, VALUE tag, VALUE val)
{
    rb_throw_obj(tag, val);
    return Qnil; /* not reached */
}

void
Init_capi_value(void)
{
    mV = rb_define_module("CapiValue");
    g_val = Qnil;
    rb_global_variable(&g_val);
    rb_define_singleton_method(mV, "constants_", v_constants, 0);
    rb_define_singleton_method(mV, "tests", v_tests, 1);
    rb_define_singleton_method(mV, "type", v_type, 1);
    rb_define_singleton_method(mV, "sym", v_sym, 1);
    rb_define_singleton_method(mV, "num", v_num, 1);
    rb_define_singleton_method(mV, "set_global", v_set_global, 1);
    rb_define_singleton_method(mV, "global", v_global, 0);
    rb_define_singleton_method(mV, "ptr_sum", v_ptr_sum, 1);
    rb_define_singleton_method(mV, "ptr_zip", v_ptr_zip, 2);
    rb_define_singleton_method(mV, "from_values", v_from_values, 0);
    rb_define_singleton_method(mV, "lookup", v_lookup, 2);
    rb_define_singleton_method(mV, "scan", v_scan, -1);
    rb_define_singleton_method(mV, "yield_all", v_yield_all, 0);
    rb_define_singleton_method(mV, "block_call", v_block_call, 2);
    rb_define_singleton_method(mV, "protect", v_protect, 2);
    rb_define_singleton_method(mV, "scan_kw", v_scan_kw, -1);
    rb_define_singleton_method(mV, "check_call", v_check_call, 2);
    rb_define_singleton_method(mV, "tmp_fill", v_tmp_fill, 2);
    rb_define_singleton_method(mV, "catch_", v_catch_, 2);
    rb_define_singleton_method(mV, "throw_", v_throw_, 2);
    rb_define_singleton_method(mV, "ensure", v_ensure, 1);
    rb_define_singleton_method(mV, "pairs", v_pairs, 1);
    rb_define_singleton_method(mV, "call_back", v_call_back, 2);
    rb_define_singleton_method(mV, "buf_build", v_buf_build, 1);
    rb_define_singleton_method(mV, "freeze_", v_freeze, 1);
    VALUE cHolder = rb_define_class_under(mV, "Holder", rb_cObject);
    rb_define_alloc_func(cHolder, holder_alloc);
    rb_define_method(cHolder, "set", holder_set, 1);
    rb_define_method(cHolder, "get", holder_get, 0);
    rb_define_const(mV, "FALSE_CONST", Qfalse);
    rb_define_const(mV, "NIL_CONST", Qnil);
    rb_define_const(mV, "SYM_CONST", ID2SYM(rb_intern("const_sym")));
}
