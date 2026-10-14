/*
 * The error paths of the C API layer that the CRuby comparison
 * (capi_hello.c) cannot reach because CRuby leaves them undefined
 * (RSTRING_PTR of a non-String, ...), and one-off loader cases selected
 * by -D at build time. monoruby only.
 */
#include <ruby.h>

#if defined(CAPI_RC)
/* No layer at all: an entry point that reports the given status. */
int monoruby_capi_init(void *ctx, void (*init)(void)) { (void)ctx; (void)init; return CAPI_RC; }
void CAPI_INIT(void) {}
#elif defined(CAPI_INIT_RAISES)
void
CAPI_INIT(void)
{
    rb_raise(rb_eRuntimeError, "Init failed");
}
#elif defined(CAPI_NO_INIT)
void Init_something_else(void) {}
#elif defined(CAPI_INIT_SUPER)
void
CAPI_INIT(void)
{
    rb_call_super(0, NULL);
}
#else

static VALUE slot;

static VALUE e_rstring_ptr(VALUE self, VALUE v) { return rb_str_new_cstr(RSTRING_PTR(v)); }
static VALUE e_resize(VALUE self, VALUE v) { return rb_str_resize(v, 1); }
static VALUE e_cat(VALUE self, VALUE v) { return rb_str_cat(v, "x", 1); }
static VALUE e_ary_len(VALUE self, VALUE v) { return LONG2NUM(RARRAY_LEN(v)); }
static VALUE e_ary_push(VALUE self, VALUE v) { return rb_ary_push(v, INT2FIX(1)); }
static VALUE e_hash_aset(VALUE self, VALUE v) { return rb_hash_aset(v, INT2FIX(1), INT2FIX(2)); }
static VALUE e_noop(VALUE self) { return Qnil; }
static VALUE e_define_on(VALUE self, VALUE v) { rb_define_method(v, "x", e_noop, 0); return Qnil; }
static VALUE e_alloc(VALUE klass) { return Qnil; }
static VALUE e_alloc_on(VALUE self, VALUE v) { rb_define_alloc_func(v, e_alloc); return Qnil; }
static VALUE e_undef_alloc_on(VALUE self, VALUE v) { rb_undef_alloc_func(v); return Qnil; }
static VALUE e_data_ptr(VALUE self, VALUE v) { return LONG2NUM((long)(DATA_PTR(v) != NULL)); }
static VALUE e_typed_p(VALUE self, VALUE v) { return RTYPEDDATA_P(v) ? Qtrue : Qfalse; }

static VALUE
e_register(VALUE self)
{
    slot = rb_str_new_cstr("registered");
    rb_gc_register_address(&slot);
    rb_gc_start();
    VALUE seen = rb_str_dup(slot);
    rb_gc_unregister_address(&slot);
    /* Unregistering an address that is not registered is a no-op. */
    rb_gc_unregister_address(&slot);
    slot = Qnil;
    return seen;
}

void
Init_capi_errors(void)
{
    VALUE m = rb_define_module("CapiErrors");
    rb_define_module_function(m, "rstring_ptr", e_rstring_ptr, 1);
    rb_define_module_function(m, "resize", e_resize, 1);
    rb_define_module_function(m, "cat", e_cat, 1);
    rb_define_module_function(m, "ary_len", e_ary_len, 1);
    rb_define_module_function(m, "ary_push", e_ary_push, 1);
    rb_define_module_function(m, "hash_aset", e_hash_aset, 1);
    rb_define_module_function(m, "define_on", e_define_on, 1);
    rb_define_module_function(m, "register", e_register, 0);
    rb_define_module_function(m, "alloc_on", e_alloc_on, 1);
    rb_define_module_function(m, "undef_alloc_on", e_undef_alloc_on, 1);
    rb_define_module_function(m, "data_ptr", e_data_ptr, 1);
    rb_define_module_function(m, "typed?", e_typed_p, 1);
    /* A class whose alloc function does not make a Data object. */
    VALUE bad = rb_define_class_under(m, "Bad", rb_cObject);
    rb_define_alloc_func(bad, e_alloc);
}
#endif
