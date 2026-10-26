/*
 * What CRuby code takes for granted about IDs and methods and what the
 * date extension exercised: one-character operator IDs
 * (rb_funcall(x, '+', 1, y), rb_intern("+") == '+'), RETURN_ENUMERATOR
 * over rb_frame_this_func, one C function under two names being one
 * method (an alias, so its name stays the first one), the coerce helpers, rb_rational_* / rb_Complex, Marshal,
 * rb_backref_get / rb_backref_set, rb_copy_generic_ivar,
 * rb_cstr_to_inum and rb_int_positive_pow. Built against CRuby and
 * monoruby by tests/capi.rs; the output must be identical.
 */
#include <ruby.h>
#include <ruby/encoding.h>
#include <ruby/re.h>
#include <string.h>

static VALUE
i_ops(VALUE self, VALUE x, VALUE y)
{
    (void)self;
    VALUE r = rb_ary_new();
    rb_ary_push(r, rb_funcall(x, '+', 1, y));
    rb_ary_push(r, rb_funcall(x, '-', 1, y));
    rb_ary_push(r, rb_funcall(x, '*', 1, y));
    rb_ary_push(r, rb_funcall(x, '/', 1, y));
    rb_ary_push(r, rb_funcall(x, '%', 1, y));
    rb_ary_push(r, rb_funcall(x, '<', 1, y));
    rb_ary_push(r, rb_funcall(x, '>', 1, y));
    rb_ary_push(r, rb_funcall(x, '&', 1, y));
    rb_ary_push(r, rb_funcall(x, '|', 1, y));
    rb_ary_push(r, rb_funcall(x, '^', 1, y));
    rb_ary_push(r, rb_funcall(x, '~', 0));
    rb_ary_push(r, rb_funcall(x, '!', 0));
    rb_ary_push(r, rb_intern("+") == '+' ? Qtrue : Qfalse);
    rb_ary_push(r, rb_intern("<") == '<' ? Qtrue : Qfalse);
    rb_ary_push(r, ID2SYM(rb_intern("-")));
    rb_ary_push(r, ID2SYM('*'));
    rb_ary_push(r, rb_str_new_cstr(rb_id2name('%')));
    rb_ary_push(r, rb_funcall(x, rb_intern("**"), 1, y));
    return r;
}

/* Date#step-style: an Enumerator for itself without a block. */
static VALUE
i_each_twice(int argc, VALUE *argv, VALUE self)
{
    RETURN_ENUMERATOR(self, argc, argv);
    for (int i = 0; i < 2; i++)
        rb_yield(argc > 0 ? argv[0] : Qnil);
    return self;
}

static VALUE
i_this_func(VALUE self)
{
    (void)self;
    return ID2SYM(rb_frame_this_func());
}

/* The same function under two names, instance and singleton. */
static VALUE
i_same(VALUE self)
{
    (void)self;
    return INT2FIX(42);
}

static VALUE
i_coerce_cmp(VALUE self, VALUE x, VALUE y)
{
    (void)self;
    return rb_num_coerce_cmp(x, y, rb_intern("<=>"));
}

static VALUE
i_coerce_bin(VALUE self, VALUE x, VALUE y)
{
    (void)self;
    return rb_num_coerce_bin(x, y, '+');
}

static VALUE
i_coerce_relop(VALUE self, VALUE x, VALUE y)
{
    (void)self;
    return rb_num_coerce_relop(x, y, '<');
}

static VALUE
i_numbers(VALUE self)
{
    (void)self;
    VALUE r = rb_ary_new();
    VALUE q = rb_rational_new(INT2FIX(6), INT2FIX(4));
    rb_ary_push(r, q);
    rb_ary_push(r, rb_rational_num(q));
    rb_ary_push(r, rb_rational_den(q));
    rb_ary_push(r, rb_rational_new1(INT2FIX(7)));
    rb_ary_push(r, rb_Rational(INT2FIX(1), INT2FIX(3)));
    rb_ary_push(r, rb_Complex(INT2FIX(1), INT2FIX(2)));
    rb_ary_push(r, rb_complex_new(INT2FIX(3), INT2FIX(0)));
    rb_ary_push(r, rb_cstr_to_inum("1f", 16, 1));
    rb_ary_push(r, rb_cstr_to_inum("123abc", 10, 0));
    rb_ary_push(r, rb_int_positive_pow(3, 40));
    rb_ary_push(r, rb_int_positive_pow(-2, 5));
    return r;
}

static VALUE
i_marshal(VALUE self, VALUE obj)
{
    (void)self;
    VALUE s = rb_marshal_dump(obj, Qnil);
    return rb_ary_new_from_args(2, rb_obj_class(s), rb_marshal_load(s));
}

/* Match inside, as the date parsers do, and put the caller's $~ back. */
static VALUE
i_match_restoring(VALUE self, VALUE str)
{
    (void)self;
    VALUE saved = rb_backref_get();
    rb_match_busy(saved);
    VALUE re = rb_reg_new_str(rb_str_new_cstr("(\\d+)"), 0);
    VALUE m = rb_funcall(re, rb_intern("match"), 1, str);
    VALUE found = NIL_P(m) ? Qnil : rb_reg_nth_match(1, m);
    rb_backref_set(saved);
    return found;
}

static VALUE
i_match_clobbering(VALUE self, VALUE str)
{
    (void)self;
    VALUE re = rb_reg_new_str(rb_str_new_cstr("(\\d+)"), 0);
    rb_funcall(re, rb_intern("match"), 1, str);
    return rb_backref_get();
}

static VALUE
i_copy_ivars(VALUE self, VALUE from, VALUE to)
{
    (void)self;
    rb_copy_generic_ivar(to, from);
    return to;
}

void
Init_capi_ids(void)
{
    VALUE m = rb_define_module("CapiIds");
    rb_define_singleton_method(m, "ops", i_ops, 2);
    rb_define_singleton_method(m, "coerce_cmp", i_coerce_cmp, 2);
    rb_define_singleton_method(m, "coerce_bin", i_coerce_bin, 2);
    rb_define_singleton_method(m, "coerce_relop", i_coerce_relop, 2);
    rb_define_singleton_method(m, "numbers", i_numbers, 0);
    rb_define_singleton_method(m, "marshal", i_marshal, 1);
    rb_define_singleton_method(m, "match_restoring", i_match_restoring, 1);
    rb_define_singleton_method(m, "match_clobbering", i_match_clobbering, 1);
    rb_define_singleton_method(m, "copy_ivars", i_copy_ivars, 2);
    rb_define_singleton_method(m, "same", i_same, 0);
    rb_define_singleton_method(m, "same_too", i_same, 0);
    VALUE c = rb_define_class_under(m, "Thing", rb_cObject);
    rb_define_method(c, "each_twice", i_each_twice, -1);
    rb_define_method(c, "this_func", i_this_func, 0);
    rb_define_method(c, "same", i_same, 0);
    rb_define_method(c, "same_too", i_same, 0);
    rb_define_private_method(c, "same_private", i_same, 0);
}
