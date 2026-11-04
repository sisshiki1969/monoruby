/*
 * Global variables backed by C (rb_define_variable and its relatives),
 * instance_eval / instance_exec / module_eval / module_exec called from
 * C, and what an alloc function does to its object besides wrapping a
 * payload. Written against CRuby's ruby.h only; built and run on both
 * interpreters by tests/capi.rs, which compares the output.
 */
#include <ruby.h>

static VALUE mVars, cTagged;

/* ---- rb_define_variable / rb_define_readonly_variable ---- */

static VALUE plain = Qnil;
static VALUE fixed = Qnil;

static VALUE
vars_plain(VALUE self)
{
    (void)self;
    return plain;
}

static VALUE
vars_set_plain(VALUE self, VALUE v)
{
    (void)self;
    plain = v;
    return v;
}

static VALUE
vars_set_fixed(VALUE self, VALUE v)
{
    (void)self;
    fixed = v;
    return v;
}

/* ---- rb_define_virtual_variable ---- */

static long counter;

static VALUE
counter_get(ID id, VALUE *data)
{
    (void)data;
    return rb_ary_new_from_args(2, ID2SYM(id), LONG2NUM(++counter));
}

static void
counter_set(VALUE val, ID id, VALUE *data)
{
    (void)id;
    (void)data;
    counter = NUM2LONG(val);
}

static VALUE
vars_counter(VALUE self)
{
    (void)self;
    return LONG2NUM(counter);
}

/* ---- rb_define_hooked_variable: the hooks see the storage ---- */

static VALUE hooked_store = Qnil;

static VALUE
hooked_get(ID id, VALUE *var)
{
    (void)id;
    return rb_ary_new_from_args(2, ID2SYM(rb_intern("hooked")), *var);
}

static void
hooked_set(VALUE val, ID id, VALUE *var)
{
    (void)id;
    if (NIL_P(val))
        rb_raise(rb_eArgError, "nil is not allowed");
    *var = rb_funcall(val, rb_intern("to_s"), 0);
}

static VALUE
vars_hooked_store(VALUE self)
{
    (void)self;
    return hooked_store;
}

static VALUE
vars_gv_get(VALUE self, VALUE name)
{
    (void)self;
    return rb_gv_get(StringValueCStr(name));
}

static VALUE
vars_gv_set(VALUE self, VALUE name, VALUE val)
{
    (void)self;
    return rb_gv_set(StringValueCStr(name), val);
}

/* ---- instance_eval and friends ---- */

static VALUE
vars_ieval_str(VALUE self, VALUE obj, VALUE code)
{
    (void)self;
    return rb_obj_instance_eval(1, &code, obj);
}

static VALUE
vars_ieval_block(VALUE self, VALUE obj)
{
    (void)self;
    return rb_obj_instance_eval(0, NULL, obj);
}

static VALUE
vars_iexec(int argc, VALUE *argv, VALUE self)
{
    (void)self;
    return rb_obj_instance_exec(argc - 1, argv + 1, argv[0]);
}

static VALUE
vars_meval_str(VALUE self, VALUE mod, VALUE code)
{
    (void)self;
    return rb_mod_module_eval(1, &code, mod);
}

static VALUE
vars_mexec(int argc, VALUE *argv, VALUE self)
{
    (void)self;
    return rb_mod_module_exec(argc - 1, argv + 1, argv[0]);
}

/* ---- an alloc function that sets an instance variable ---- */

struct tagged {
    long x;
};

static void
tagged_free(void *p)
{
    xfree(p);
}

static VALUE
tagged_alloc(VALUE klass)
{
    struct tagged *t;
    VALUE obj = Data_Make_Struct(klass, struct tagged, NULL, tagged_free, t);
    t->x = -1;
    rb_iv_set(obj, "@from_alloc", rb_str_new_cstr("alloc"));
    return obj;
}

static VALUE
tagged_initialize(VALUE self, VALUE x)
{
    struct tagged *t;
    Data_Get_Struct(self, struct tagged, t);
    t->x = NUM2LONG(x);
    return self;
}

static VALUE
tagged_x(VALUE self)
{
    struct tagged *t;
    Data_Get_Struct(self, struct tagged, t);
    return LONG2NUM(t->x);
}

static VALUE
tagged_tag(VALUE self)
{
    return rb_iv_get(self, "@from_alloc");
}

void
Init_capi_vars(void)
{
    mVars = rb_define_module("CapiVars");
    rb_define_variable("$v_plain", &plain);
    rb_define_readonly_variable("$v_fixed", &fixed);
    rb_define_virtual_variable("$v_counter", counter_get, counter_set);
    rb_define_virtual_variable("$v_readonly_virtual", counter_get, NULL);
    rb_define_hooked_variable("$v_hooked", &hooked_store, hooked_get, hooked_set);
    fixed = rb_str_new_cstr("fixed");

    rb_define_singleton_method(mVars, "plain", vars_plain, 0);
    rb_define_singleton_method(mVars, "plain=", vars_set_plain, 1);
    rb_define_singleton_method(mVars, "set_fixed", vars_set_fixed, 1);
    rb_define_singleton_method(mVars, "counter", vars_counter, 0);
    rb_define_singleton_method(mVars, "hooked_store", vars_hooked_store, 0);
    rb_define_singleton_method(mVars, "gv_get", vars_gv_get, 1);
    rb_define_singleton_method(mVars, "gv_set", vars_gv_set, 2);
    rb_define_singleton_method(mVars, "ieval_str", vars_ieval_str, 2);
    rb_define_singleton_method(mVars, "ieval_block", vars_ieval_block, 1);
    rb_define_singleton_method(mVars, "iexec", vars_iexec, -1);
    rb_define_singleton_method(mVars, "meval_str", vars_meval_str, 2);
    rb_define_singleton_method(mVars, "mexec", vars_mexec, -1);

    cTagged = rb_define_class_under(mVars, "Tagged", rb_cObject);
    rb_define_alloc_func(cTagged, tagged_alloc);
    rb_define_method(cTagged, "initialize", tagged_initialize, 1);
    rb_define_method(cTagged, "x", tagged_x, 0);
    rb_define_method(cTagged, "tag", tagged_tag, 0);
}
