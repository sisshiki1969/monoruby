/*
 * VALUEs the layer hands to C stay alive while C holds them, as CRuby's
 * conservative stack scan keeps them: each case takes a value, drops
 * the only other reference to it, calls back into Ruby (which collects
 * and allocates over the freed cells) and then reads the value.
 */
#include <ruby.h>

static ID id_churn;

static VALUE
churn(VALUE self)
{
    return rb_funcall(self, id_churn, 0);
}

/* rb_ary_entry (and RARRAY_AREF on top of it). */
static VALUE
t_entry(VALUE self, VALUE ary)
{
    VALUE e = RARRAY_AREF(ary, 0);
    rb_ary_store(ary, 0, Qnil);
    churn(self);
    return rb_str_dup(e);
}

/* The elements behind RARRAY_CONST_PTR, the first time and after the
 * pointer is taken again for the same Array with an element replaced. */
static VALUE
t_cptr(VALUE self, VALUE ary)
{
    VALUE a = RARRAY_CONST_PTR(ary)[0];
    rb_ary_store(ary, 1, rb_str_new_cstr("replaced"));
    VALUE b = RARRAY_CONST_PTR(ary)[1];
    VALUE c = RARRAY_CONST_PTR(ary)[2];
    rb_ary_clear(ary);
    churn(self);
    return rb_ary_new_from_args(3, rb_str_dup(a), rb_str_dup(b), rb_str_dup(c));
}

/* rb_gv_get. */
static VALUE
t_gv(VALUE self)
{
    VALUE e = rb_gv_get("$capi_roots");
    rb_gv_set("$capi_roots", Qnil);
    churn(self);
    return rb_str_dup(e);
}

/* rb_backref_get, $~ of the caller. */
static VALUE
t_backref(VALUE self)
{
    VALUE md = rb_backref_get();
    rb_backref_set(Qnil);
    churn(self);
    return rb_funcall(md, rb_intern("to_a"), 0);
}

/* rb_lastline_get, $_ of the caller. */
static VALUE
t_lastline(VALUE self)
{
    VALUE line = rb_lastline_get();
    rb_lastline_set(Qnil);
    churn(self);
    return rb_str_dup(line);
}

static VALUE
raise_fresh(VALUE arg)
{
    (void)arg;
    rb_exc_raise(rb_exc_new_str(rb_eRuntimeError,
                                rb_str_plus(rb_str_new_cstr("raised in "),
                                            rb_str_new_cstr("an earlier call"))));
    return Qnil;
}

/* rb_errinfo is set by rb_protect in one call ... */
static VALUE
t_protect(VALUE self)
{
    int state;
    rb_protect(raise_fresh, Qnil, &state);
    return INT2FIX(state != 0);
}

/* ... and read in a later one. */
static VALUE
t_errinfo(VALUE self)
{
    VALUE err = rb_errinfo();
    rb_set_errinfo(Qnil);
    return NIL_P(err) ? Qnil : rb_funcall(err, rb_intern("message"), 0);
}

void
Init_capi_roots(void)
{
    id_churn = rb_intern("churn");
    VALUE c = rb_define_class("CapiRoots", rb_cObject);
    rb_define_method(c, "entry", t_entry, 1);
    rb_define_method(c, "cptr", t_cptr, 1);
    rb_define_method(c, "gv", t_gv, 0);
    rb_define_method(c, "backref", t_backref, 0);
    rb_define_method(c, "lastline", t_lastline, 0);
    rb_define_method(c, "protect", t_protect, 0);
    rb_define_method(c, "errinfo", t_errinfo, 0);
}
