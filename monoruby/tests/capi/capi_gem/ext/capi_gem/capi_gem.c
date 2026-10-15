/*
 * The C half of the fixture gem: written against CRuby's ruby.h with
 * nothing monoruby-specific in it, built by extconf.rb + make.
 */
#include "ruby.h"
#include "ruby/config.h"

#ifndef CAPI_GEM_FROM_EXTCONF
#error "extconf.rb's $defs did not reach the compiler"
#endif
#ifndef HAVE_RB_STR_NEW_CSTR
#error "have_func(rb_str_new_cstr) did not define HAVE_RB_STR_NEW_CSTR"
#endif
#ifdef HAVE_RB_NO_SUCH_FUNCTION_XYZZY
#error "have_func found a function the library does not have"
#endif
#ifndef HAVE_TYPE_RB_DATA_TYPE_T
#error "have_type(rb_data_type_t) failed"
#endif

struct counter {
    long n;
};

static void
counter_free(void *p)
{
    xfree(p);
}

static size_t
counter_size(const void *p)
{
    return sizeof(struct counter);
}

static const rb_data_type_t counter_type = {
    "CapiGem::Counter",
    {0, counter_free, counter_size},
    0, 0, RUBY_TYPED_FREE_IMMEDIATELY
};

static VALUE
counter_alloc(VALUE klass)
{
    struct counter *c;
    VALUE obj = TypedData_Make_Struct(klass, struct counter, &counter_type, c);
    c->n = 0;
    return obj;
}

static VALUE
counter_incr(VALUE self)
{
    struct counter *c;
    TypedData_Get_Struct(self, struct counter, &counter_type, c);
    return LONG2NUM(++c->n);
}

static VALUE
gem_greet(VALUE self, VALUE name)
{
    Check_Type(name, T_STRING);
    return rb_sprintf("hello, %"PRIsVALUE" (sizeof(long) = %d)", name, (int)SIZEOF_LONG);
}

static VALUE
gem_sum(int argc, VALUE *argv, VALUE self)
{
    long sum = 0;
    int i;
    for (i = 0; i < argc; i++) sum += NUM2LONG(argv[i]);
    if (rb_block_given_p()) return rb_yield(LONG2NUM(sum));
    return LONG2NUM(sum);
}

void
Init_capi_gem(void)
{
    VALUE m = rb_define_module("CapiGem");
    VALUE c = rb_define_class_under(m, "Counter", rb_cObject);
    rb_define_alloc_func(c, counter_alloc);
    rb_define_method(c, "incr", counter_incr, 0);
    rb_define_singleton_method(m, "greet", gem_greet, 1);
    rb_define_singleton_method(m, "sum", gem_sum, -1);
    rb_define_const(m, "BUILT_WITH", rb_str_new_cstr(
#ifdef MONORUBY_CAPI
        "monoruby"
#else
        "cruby"
#endif
    ));
}
