/*
 * A C++ extension in the shape SWIG generates (gosu's wrapper is one):
 * every function pointer cast to CRuby's ANYARGS types before the API
 * gets it, `new` left to rb_define_alloc_func when the headers announce
 * it, a type tag set by the alloc function, a read-only global for the
 * wrapper's runtime data and a virtual one for a counter. Written
 * against CRuby's ruby.h only; built and run on both interpreters by
 * tests/capi.rs, which compares the output.
 */
#include <ruby.h>
#include <ruby/st.h>

#define VALUEFUNC(f) ((VALUE (*)(ANYARGS)) f)
#define VOIDFUNC(f) ((RUBY_DATA_FUNC) f)
#define SWIG_RUBY_VOID_ANYARGS_FUNC(f) ((void (*)(ANYARGS))(f))
#define SWIG_RUBY_INT_ANYARGS_FUNC(f) ((int (*)(ANYARGS))(f))

namespace {

struct Thing {
    long a;
    long b;
};

VALUE cThing;
VALUE runtime_data;
long trackings;

void
thing_free(void *p)
{
    delete static_cast<Thing *>(p);
}

VALUE
thing_allocate(VALUE klass)
{
    VALUE obj = Data_Wrap_Struct(klass, VOIDFUNC(0), VOIDFUNC(thing_free), 0);
    rb_iv_set(obj, "@__swigtype__", rb_str_new2("_p_Thing"));
    return obj;
}

VALUE
thing_initialize(int argc, VALUE *argv, VALUE self)
{
    if (NIL_P(rb_iv_get(self, "@__swigtype__")))
        rb_raise(rb_eTypeError, "Expected argument 0 of type Thing *");
    Thing *t = new Thing();
    t->a = argc > 0 ? NUM2LONG(argv[0]) : 0;
    t->b = argc > 1 ? NUM2LONG(argv[1]) : 0;
    DATA_PTR(self) = t;
    rb_iv_set(self, "@args", rb_ary_new_from_values(argc, argv));
    return self;
}

VALUE
thing_sum(VALUE self)
{
    Thing *t;
    Data_Get_Struct(self, Thing, t);
    return LONG2NUM(t->a + t->b);
}

VALUE
thing_args(VALUE self)
{
    return rb_iv_get(self, "@args");
}

VALUE
thing_tag(VALUE self)
{
    return rb_iv_get(self, "@__swigtype__");
}

VALUE
rescue_body(VALUE arg)
{
    if (RTEST(arg))
        rb_raise(rb_eArgError, "boom");
    return INT2FIX(1);
}

VALUE
rescue_handler(VALUE data, VALUE exc)
{
    return rb_ary_new_from_args(3, ID2SYM(rb_intern("rescued")), data, rb_class_name(rb_obj_class(exc)));
}

VALUE
type_body(VALUE arg)
{
    if (arg == Qtrue)
        rb_raise(rb_eTypeError, "type boom");
    if (SYMBOL_P(arg))
        rb_raise(rb_eArgError, "arg boom");
    return INT2FIX(2);
}

VALUE
ensure_body(VALUE ary)
{
    return rb_ary_push(ary, ID2SYM(rb_intern("body")));
}

VALUE
ensure_after(VALUE ary)
{
    rb_ary_push(ary, ID2SYM(rb_intern("ensured")));
    return Qnil;
}

VALUE
m_guard(VALUE, VALUE raise_p)
{
    return rb_rescue(VALUEFUNC(rescue_body), raise_p, VALUEFUNC(rescue_handler), INT2FIX(7));
}

VALUE
m_guard2(VALUE, VALUE arg)
{
    return rb_rescue2(VALUEFUNC(type_body), arg, VALUEFUNC(rescue_handler), INT2FIX(8), rb_eTypeError, (VALUE)0);
}

VALUE
m_ensure(VALUE, VALUE ary)
{
    rb_ensure(VALUEFUNC(ensure_body), ary, VALUEFUNC(ensure_after), ary);
    return ary;
}

int
table_cb(st_data_t key, st_data_t value, st_data_t arg)
{
    rb_ary_push((VALUE)arg, rb_ary_new_from_args(2, LONG2NUM((long)key), LONG2NUM((long)value)));
    return ST_CONTINUE;
}

VALUE
m_table(VALUE)
{
    st_table *tbl = st_init_numtable();
    for (long i = 1; i <= 3; i++)
        st_insert(tbl, (st_data_t)i, (st_data_t)(i * i));
    VALUE out = rb_ary_new();
    st_foreach(tbl, SWIG_RUBY_INT_ANYARGS_FUNC(table_cb), (st_data_t)out);
    st_free_table(tbl);
    return rb_funcall(out, rb_intern("sort"), 0);
}

VALUE
times_cb(VALUE yielded, VALUE factor, int, const VALUE *, VALUE)
{
    return LONG2NUM(NUM2LONG(yielded) * NUM2LONG(factor));
}

VALUE
m_block_call(VALUE, VALUE ary, VALUE factor)
{
    return rb_block_call(ary, rb_intern("map"), 0, 0, VALUEFUNC(times_cb), factor);
}

VALUE
m_proc(VALUE, VALUE factor)
{
    return rb_proc_new(VALUEFUNC(times_cb), factor);
}

VALUE
catch_body(VALUE tag, VALUE arg, int, const VALUE *, VALUE)
{
    rb_throw_obj(tag, arg);
}

VALUE
m_catch(VALUE, VALUE arg)
{
    return rb_catch("cxx_tag", VALUEFUNC(catch_body), arg);
}

VALUE
m_ieval(VALUE, VALUE obj)
{
    VALUE code = rb_str_new2("@secret * 2");
    return rb_obj_instance_eval(1, &code, obj);
}

int
ivar_cb(ID key, VALUE val, st_data_t arg)
{
    rb_ary_push((VALUE)arg, rb_ary_new_from_args(2, ID2SYM(key), val));
    return ST_CONTINUE;
}

VALUE
m_ivars(VALUE, VALUE obj)
{
    VALUE out = rb_ary_new();
    rb_ivar_foreach(obj, SWIG_RUBY_INT_ANYARGS_FUNC(ivar_cb), (st_data_t)out);
    return out;
}

VALUE
m_alloc_api(VALUE)
{
#ifdef HAVE_RB_DEFINE_ALLOC_FUNC
    return Qtrue;
#else
    return Qfalse;
#endif
}

VALUE
trackings_count(ID, VALUE *)
{
    return LONG2NUM(++trackings);
}

}  // namespace

extern "C" void
Init_capi_cxx(void)
{
    VALUE m = rb_define_module("CapiCxx");
    cThing = rb_define_class_under(m, "Thing", rb_cObject);
#ifdef HAVE_RB_DEFINE_ALLOC_FUNC
    rb_define_alloc_func(cThing, thing_allocate);
#endif
    rb_define_method(cThing, "initialize", VALUEFUNC(thing_initialize), -1);
    rb_define_method(cThing, "sum", VALUEFUNC(thing_sum), 0);
    rb_define_method(cThing, "args", VALUEFUNC(thing_args), 0);
    rb_define_method(cThing, "tag", VALUEFUNC(thing_tag), 0);
    rb_define_module_function(m, "alloc_api?", VALUEFUNC(m_alloc_api), 0);
    rb_define_module_function(m, "guard", VALUEFUNC(m_guard), 1);
    rb_define_module_function(m, "guard2", VALUEFUNC(m_guard2), 1);
    rb_define_module_function(m, "ensure", VALUEFUNC(m_ensure), 1);
    rb_define_module_function(m, "table", VALUEFUNC(m_table), 0);
    rb_define_module_function(m, "block_call", VALUEFUNC(m_block_call), 2);
    rb_define_module_function(m, "proc", VALUEFUNC(m_proc), 1);
    rb_define_module_function(m, "catch", VALUEFUNC(m_catch), 1);
    rb_define_module_function(m, "ieval", VALUEFUNC(m_ieval), 1);
    rb_define_module_function(m, "ivars", VALUEFUNC(m_ivars), 1);

    runtime_data = rb_str_new2("runtime data");
    rb_define_readonly_variable("$capi_cxx_runtime", &runtime_data);
    rb_define_virtual_variable("$capi_cxx_trackings", VALUEFUNC(trackings_count),
                               SWIG_RUBY_VOID_ANYARGS_FUNC((rb_gvar_setter_t *)NULL));
}
