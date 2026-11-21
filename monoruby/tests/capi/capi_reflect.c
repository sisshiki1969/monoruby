/*
 * The reflection side of the API (doc/c_api.md §3.26): Module / Class /
 * Object queries, constants and class variables reached from C,
 * rb_exec_recursive, category warnings, end procs, arithmetic
 * sequences, rb_path2class's errors, the public-only calls, keyword
 * coercion, and the CRuby globals ruby/spec's capi files take for
 * granted. Built against CRuby and monoruby by tests/capi.rs; the output
 * must be identical.
 */
#include <ruby.h>
#include <string.h>

static VALUE
r_ancestors(VALUE self, VALUE mod)
{
    (void)self;
    return rb_mod_ancestors(mod);
}

static VALUE
r_methods(VALUE self, VALUE mod, VALUE obj)
{
    (void)self;
    VALUE f = Qfalse;
    VALUE r = rb_ary_new();
    rb_ary_push(r, rb_funcall(rb_class_instance_methods(1, &f, mod), rb_intern("sort"), 0));
    rb_ary_push(r, rb_funcall(rb_class_public_instance_methods(1, &f, mod), rb_intern("sort"), 0));
    rb_ary_push(r, rb_funcall(rb_class_protected_instance_methods(1, &f, mod), rb_intern("sort"), 0));
    rb_ary_push(r, rb_funcall(rb_class_private_instance_methods(1, &f, mod), rb_intern("sort"), 0));
    rb_ary_push(r, rb_funcall(rb_obj_singleton_methods(0, NULL, obj), rb_intern("sort"), 0));
    return r;
}

static VALUE
r_modules(VALUE self, VALUE klass, VALUE mod)
{
    (void)self;
    VALUE f = Qfalse;
    VALUE r = rb_ary_new();
    rb_ary_push(r, rb_mod_include_p(klass, mod));
    rb_ary_push(r, rb_mod_include_p(klass, rb_mComparable));
    rb_ary_push(r, rb_mod_included_modules(klass));
    rb_ary_push(r, rb_funcall(rb_mod_constants(1, &f, klass), rb_intern("sort"), 0));
    rb_ary_push(r, rb_funcall(rb_mod_class_variables(0, NULL, klass), rb_intern("sort"), 0));
    return r;
}

static VALUE
r_remove(VALUE self, VALUE klass, VALUE obj)
{
    (void)self;
    VALUE r = rb_ary_new();
    rb_ary_push(r, rb_mod_remove_const(klass, ID2SYM(rb_intern("A"))));
    rb_ary_push(r, rb_const_remove(klass, rb_intern("B")));
    rb_ary_push(r, rb_mod_remove_cvar(klass, ID2SYM(rb_intern("@@cv"))));
    rb_ary_push(r, rb_obj_remove_instance_variable(obj, ID2SYM(rb_intern("@a"))));
    rb_ary_push(r, rb_funcall(klass, rb_intern("constants"), 1, Qfalse));
    rb_ary_push(r, rb_funcall(klass, rb_intern("class_variables"), 0));
    rb_ary_push(r, rb_obj_instance_variables(obj));
    return r;
}

static VALUE
r_const_from(VALUE self, VALUE space, VALUE name)
{
    (void)self;
    ID id = rb_to_id(name);
    VALUE r = rb_ary_new();
    rb_ary_push(r, rb_const_defined_from(space, id) ? Qtrue : Qfalse);
    rb_ary_push(r, rb_const_get_from(space, id));
    return r;
}

static VALUE
r_ivars(VALUE self, VALUE obj)
{
    (void)self;
    VALUE r = rb_ary_new();
    rb_ary_push(r, ULONG2NUM(rb_ivar_count(obj)));
    rb_free_generic_ivar(obj);
    rb_ary_push(r, rb_obj_instance_variables(obj));
    return r;
}

static int
r_foreach_i(ID name, VALUE val, st_data_t arg)
{
    rb_ary_push((VALUE)arg, ID2SYM(name));
    rb_ary_push((VALUE)arg, val);
    return ST_CONTINUE;
}

static VALUE
r_ivar_foreach(VALUE self, VALUE obj)
{
    (void)self;
    VALUE r = rb_ary_new();
    rb_ivar_foreach(obj, r_foreach_i, (st_data_t)r);
    return r;
}

static VALUE
r_boundp(VALUE self, VALUE klass, VALUE name)
{
    (void)self;
    ID id = rb_to_id(name);
    VALUE r = rb_ary_new();
    for (int ex = 0; ex < 4; ex++)
        rb_ary_push(r, INT2FIX(rb_method_boundp(klass, id, ex)));
    return r;
}

static VALUE
r_method(VALUE self, VALUE obj, VALUE klass, VALUE name)
{
    (void)self;
    VALUE r = rb_ary_new();
    rb_ary_push(r, rb_funcall(rb_obj_method(obj, name), rb_intern("name"), 0));
    rb_ary_push(r, INT2FIX(rb_obj_method_arity(obj, rb_to_id(name))));
    rb_ary_push(r, INT2FIX(rb_mod_method_arity(klass, rb_to_id(name))));
    return r;
}

static VALUE
r_cvar(VALUE self, VALUE klass)
{
    (void)self;
    VALUE r = rb_ary_new();
    rb_ary_push(r, rb_cvar_defined(klass, rb_intern("@@made")));
    rb_define_class_variable(klass, "@@made", INT2FIX(7));
    rb_ary_push(r, rb_cvar_defined(klass, rb_intern("@@made")));
    rb_ary_push(r, rb_cvar_get(klass, rb_intern("@@made")));
    rb_ary_push(r, rb_funcall(rb_f_global_variables(), rb_intern("include?"), 1, ID2SYM(rb_intern("$stdout"))));
    return r;
}

static VALUE
r_define_ids(VALUE self, VALUE outer, VALUE super)
{
    (void)self;
    VALUE r = rb_ary_new();
    VALUE anon = rb_define_class_id(rb_intern("Anon"), super);
    rb_ary_push(r, rb_mod_name(anon));
    rb_ary_push(r, rb_class_superclass(anon));
    rb_ary_push(r, rb_class_superclass(rb_define_class_id(rb_intern("Anon"), 0)));
    rb_ary_push(r, rb_mod_name(rb_define_module_id(rb_intern("AnonM"))));
    rb_ary_push(r, rb_define_class_id_under(outer, rb_intern("Named"), super));
    rb_ary_push(r, rb_define_class_id_under(outer, rb_intern("Named"), super));
    rb_ary_push(r, rb_define_module_id_under(outer, rb_intern("NamedM")));
    return r;
}

static VALUE
r_class_info(VALUE self, VALUE klass, VALUE obj)
{
    (void)self;
    VALUE r = rb_ary_new();
    rb_ary_push(r, rb_class_get_superclass(klass));
    rb_ary_push(r, rb_class_get_superclass(rb_cBasicObject));
    rb_ary_push(r, rb_class_get_superclass(rb_mKernel));
    rb_ary_push(r, rb_class_attached_object(rb_singleton_class(obj)) == obj ? Qtrue : Qfalse);
    rb_ary_push(r, rb_class_subclasses(klass));
    rb_ary_push(r, rb_class_path_cached(klass));
    rb_ary_push(r, rb_class_path_cached(rb_class_new(rb_cObject)));
    rb_ary_push(r, rb_class_real(rb_singleton_class(obj)));
    rb_ary_push(r, rb_class_inherited_p(klass, rb_cObject));
    return r;
}

static VALUE
r_inherited_p_error(VALUE self, VALUE klass, VALUE arg)
{
    (void)self;
    return rb_class_inherited_p(klass, arg);
}

/* rb_exec_recursive: an inspect that sees itself in a cycle. */
static VALUE r_inspect_rec(VALUE self, VALUE obj);

static VALUE
inspect_i(VALUE obj, VALUE arg, int recursive)
{
    (void)arg;
    if (recursive)
        return rb_str_new_cstr("[...]");
    VALUE s = rb_str_new_cstr("[");
    for (long i = 0; i < RARRAY_LEN(obj); i++) {
        VALUE e = rb_ary_entry(obj, i);
        if (i)
            rb_str_cat_cstr(s, ", ");
        rb_str_append(s, RB_TYPE_P(e, T_ARRAY) ? r_inspect_rec(Qnil, e) : rb_inspect(e));
    }
    return rb_str_cat_cstr(s, "]");
}

static VALUE
r_inspect_rec(VALUE self, VALUE obj)
{
    (void)self;
    return rb_exec_recursive(inspect_i, obj, 0);
}

static VALUE
pointer_i(VALUE obj, VALUE arg, int recursive)
{
    (void)obj;
    (void)recursive;
    return strcmp((const char *)arg, "a C pointer") == 0 ? Qtrue : Qfalse;
}

static VALUE
r_rec_pointer(VALUE self)
{
    (void)self;
    static const char text[] = "a C pointer";
    return rb_exec_recursive(pointer_i, rb_str_new_cstr("x"), (VALUE)text);
}

/* The paired form: recursion only for the same (obj, paired) pair. */
static VALUE r_eq_rec(VALUE self, VALUE a, VALUE b);

static VALUE
eq_i(VALUE a, VALUE b, int recursive)
{
    if (recursive)
        return Qtrue;
    if (RARRAY_LEN(a) != RARRAY_LEN(b))
        return Qfalse;
    for (long i = 0; i < RARRAY_LEN(a); i++) {
        VALUE x = rb_ary_entry(a, i), y = rb_ary_entry(b, i);
        VALUE same = (RB_TYPE_P(x, T_ARRAY) && RB_TYPE_P(y, T_ARRAY)) ? r_eq_rec(Qnil, x, y)
                                                                        : rb_equal(x, y);
        if (!RTEST(same))
            return Qfalse;
    }
    return Qtrue;
}

static VALUE
r_eq_rec(VALUE self, VALUE a, VALUE b)
{
    (void)self;
    return rb_exec_recursive_paired(eq_i, a, b, b);
}

/* The outer form: an inner recursion unwinds to the outermost call,
 * which then runs once more with `recursive` set. */
static VALUE r_hash_rec(VALUE self, VALUE obj);

static VALUE
hash_i(VALUE obj, VALUE arg, int recursive)
{
    (void)arg;
    if (recursive)
        return rb_str_new_cstr("outer saw recursion");
    VALUE s = rb_str_new_cstr("");
    for (long i = 0; i < RARRAY_LEN(obj); i++) {
        VALUE e = rb_ary_entry(obj, i);
        rb_str_append(s, RB_TYPE_P(e, T_ARRAY) ? r_hash_rec(Qnil, e) : rb_obj_as_string(e));
    }
    return s;
}

static VALUE
r_hash_rec(VALUE self, VALUE obj)
{
    (void)self;
    return rb_exec_recursive_outer(hash_i, obj, 0);
}

static VALUE
r_warn(VALUE self, VALUE n)
{
    (void)self;
    rb_category_warn(RB_WARN_CATEGORY_DEPRECATED, "deprecated %d", NUM2INT(n));
    rb_category_warning(RB_WARN_CATEGORY_EXPERIMENTAL, "experimental %d", NUM2INT(n));
    rb_category_warn(RB_WARN_CATEGORY_NONE, "plain %d", NUM2INT(n));
    rb_compile_warn("file.rb", 12, "compile %s", "warn");
    rb_compile_warning("file.rb", 13, "compile %s", "warning");
    return Qnil;
}

static VALUE
r_backtrace(VALUE self)
{
    (void)self;
    VALUE bt = rb_make_backtrace();
    return RB_TYPE_P(bt, T_ARRAY) && RARRAY_LEN(bt) > 0 ? rb_ary_entry(bt, 0) : Qnil;
}

static void
end_proc(VALUE io)
{
    rb_io_write(io, rb_str_new_cstr("end proc ran\n"));
}

static VALUE
r_end_proc(VALUE self, VALUE io)
{
    (void)self;
    rb_set_end_proc(end_proc, io);
    return Qnil;
}

static VALUE
r_aseq(VALUE self, VALUE as, VALUE len, VALUE err)
{
    (void)self;
    rb_arithmetic_sequence_components_t c;
    VALUE r = rb_ary_new();
    if (rb_arithmetic_sequence_extract(as, &c)) {
        rb_ary_push(r, rb_ary_new_from_args(4, c.begin, c.end, c.step, c.exclude_end ? Qtrue : Qfalse));
    } else {
        rb_ary_push(r, Qnil);
    }
    long beg = -1, n = -1, step = -1;
    VALUE ok = rb_arithmetic_sequence_beg_len_step(as, &beg, &n, &step, NUM2LONG(len), NUM2INT(err));
    rb_ary_push(r, ok);
    if (ok == Qtrue)
        rb_ary_push(r, rb_ary_new_from_args(3, LONG2NUM(beg), LONG2NUM(n), LONG2NUM(step)));
    return r;
}

static VALUE
r_range_beg_len(VALUE self, VALUE range, VALUE len, VALUE err)
{
    (void)self;
    long beg = -1, n = -1;
    VALUE ok = rb_range_beg_len(range, &beg, &n, NUM2LONG(len), NUM2INT(err));
    return ok == Qtrue ? rb_ary_new_from_args(2, LONG2NUM(beg), LONG2NUM(n)) : ok;
}

static VALUE
r_path2class(VALUE self, VALUE path)
{
    (void)self;
    return rb_path_to_class(path);
}

static VALUE
r_path2class_cstr(VALUE self, VALUE path)
{
    (void)self;
    return rb_path2class(StringValueCStr(path));
}

static VALUE
r_public_call(VALUE self, VALUE obj, VALUE name)
{
    (void)self;
    return rb_funcallv_public(obj, rb_to_id(name), 0, NULL);
}

static VALUE
r_with_block(VALUE self, VALUE obj, VALUE name)
{
    (void)self;
    return rb_funcall_with_block(obj, rb_to_id(name), 0, NULL, rb_block_proc());
}

static VALUE
r_kw_call(VALUE self, VALUE obj, VALUE name, VALUE arg)
{
    (void)self;
    return rb_funcallv_kw(obj, rb_to_id(name), 1, &arg, RB_PASS_KEYWORDS);
}

static VALUE
raise_i(VALUE arg)
{
    (void)arg;
    rb_raise(rb_eRuntimeError, "boom");
    return Qnil;
}

static VALUE
rescue_i(VALUE arg, VALUE exc)
{
    (void)arg;
    return exc;
}

static VALUE
r_rescue_bad_class(VALUE self)
{
    (void)self;
    return rb_rescue2(raise_i, Qnil, rescue_i, Qnil, INT2FIX(1), (VALUE)0);
}

static VALUE
r_stdout(VALUE self)
{
    (void)self;
    VALUE r = rb_ary_new();
    rb_ary_push(r, rb_stdout == rb_gv_get("$stdout") ? Qtrue : Qfalse);
    rb_ary_push(r, rb_stderr == rb_gv_get("$stderr") ? Qtrue : Qfalse);
    rb_ary_push(r, rb_stdin == rb_gv_get("$stdin") ? Qtrue : Qfalse);
    rb_ary_push(r, rb_defout == rb_stdout ? Qtrue : Qfalse);
    rb_ary_push(r, rb_output_fs);
    rb_ary_push(r, rb_argv);
    rb_ary_push(r, rb_mFileTest);
    rb_ary_push(r, rb_mWaitReadable);
    rb_ary_push(r, rb_cRefinement);
    return r;
}

static VALUE
r_types(VALUE self, VALUE time)
{
    (void)self;
    VALUE r = rb_ary_new();
    rb_ary_push(r, rb_type_p(time, T_DATA) ? Qtrue : Qfalse);
    rb_ary_push(r, rb_type_p(Qnil, T_NIL) ? Qtrue : Qfalse);
    return r;
}

static VALUE
r_alloc_funcs(VALUE self, VALUE plain, VALUE str_sub)
{
    (void)self;
    VALUE r = rb_ary_new();
    rb_alloc_func_t f = rb_get_alloc_func(plain);
    rb_ary_push(r, f ? Qtrue : Qfalse);
    VALUE obj = f(plain);
    rb_ary_push(r, rb_obj_class(obj) == plain ? Qtrue : Qfalse);
    rb_ary_push(r, rb_get_alloc_func(str_sub) ? Qtrue : Qfalse);
    rb_undef_alloc_func(plain);
    rb_ary_push(r, rb_get_alloc_func(plain) ? Qtrue : Qfalse);
    return r;
}

static VALUE
r_block_data(RB_BLOCK_CALL_FUNC_ARGLIST(yielded, data))
{
    (void)yielded;
    return data;
}

static VALUE
r_instance_exec(VALUE self, VALUE obj)
{
    (void)self;
    return rb_block_call(obj, rb_intern("instance_exec"), 0, NULL, r_block_data, rb_str_new_cstr("data"));
}

void
Init_capi_reflect(void)
{
    VALUE m = rb_define_module("CapiReflect");
    rb_define_module_function(m, "ancestors", r_ancestors, 1);
    rb_define_module_function(m, "methods", r_methods, 2);
    rb_define_module_function(m, "modules", r_modules, 2);
    rb_define_module_function(m, "remove", r_remove, 2);
    rb_define_module_function(m, "const_from", r_const_from, 2);
    rb_define_module_function(m, "ivars", r_ivars, 1);
    rb_define_module_function(m, "ivar_foreach", r_ivar_foreach, 1);
    rb_define_module_function(m, "boundp", r_boundp, 2);
    rb_define_module_function(m, "method_info", r_method, 3);
    rb_define_module_function(m, "cvar", r_cvar, 1);
    rb_define_module_function(m, "define_ids", r_define_ids, 2);
    rb_define_module_function(m, "class_info", r_class_info, 2);
    rb_define_module_function(m, "inherited_p", r_inherited_p_error, 2);
    rb_define_module_function(m, "inspect_rec", r_inspect_rec, 1);
    rb_define_module_function(m, "rec_pointer", r_rec_pointer, 0);
    rb_define_module_function(m, "eq_rec", r_eq_rec, 2);
    rb_define_module_function(m, "hash_rec", r_hash_rec, 1);
    rb_define_module_function(m, "warn", r_warn, 1);
    rb_define_module_function(m, "backtrace", r_backtrace, 0);
    rb_define_module_function(m, "end_proc", r_end_proc, 1);
    rb_define_module_function(m, "aseq", r_aseq, 3);
    rb_define_module_function(m, "range_beg_len", r_range_beg_len, 3);
    rb_define_module_function(m, "path2class", r_path2class, 1);
    rb_define_module_function(m, "path2class_cstr", r_path2class_cstr, 1);
    rb_define_module_function(m, "public_call", r_public_call, 2);
    rb_define_module_function(m, "with_block", r_with_block, 2);
    rb_define_module_function(m, "kw_call", r_kw_call, 3);
    rb_define_module_function(m, "rescue_bad_class", r_rescue_bad_class, 0);
    rb_define_module_function(m, "streams", r_stdout, 0);
    rb_define_module_function(m, "types", r_types, 1);
    rb_define_module_function(m, "alloc_funcs", r_alloc_funcs, 2);
    rb_define_module_function(m, "instance_exec", r_instance_exec, 1);
}
