/*
 * Stage 2 of the CRuby-compatible C API: TypedData / Data objects with
 * alloc functions, C functions as blocks, `rb_call_super` and the
 * Struct helpers. Written against CRuby's ruby.h only; built and run on
 * both interpreters by tests/capi.rs, which compares the output.
 */
#include <ruby.h>
#include <string.h>

static VALUE mData, cPoint, cPoint3, cCounter, cNope, cDerived;

/* ---- TypedData ---- */

struct point {
    double x, y;
    VALUE label;
};

static int points_freed;

static void
point_mark(void *p)
{
    struct point *pt = p;
    rb_gc_mark(pt->label);
}

static void
point_free(void *p)
{
    points_freed++;
    xfree(p);
}

static size_t
point_size(const void *p)
{
    (void)p;
    return sizeof(struct point);
}

static const rb_data_type_t point_type = {
    "Point",
    { point_mark, point_free, point_size, },
    0, 0, RUBY_TYPED_FREE_IMMEDIATELY
};

/* A type derived from Point: rb_check_typeddata(obj, &point_type) accepts it. */
static const rb_data_type_t point3_type = {
    "Point3",
    { point_mark, point_free, point_size, },
    &point_type, 0, RUBY_TYPED_FREE_IMMEDIATELY
};

static VALUE
point_alloc(VALUE klass)
{
    struct point *pt;
    VALUE obj = TypedData_Make_Struct(klass, struct point, &point_type, pt);
    pt->label = Qnil;
    return obj;
}

static VALUE
point3_alloc(VALUE klass)
{
    struct point *pt;
    VALUE obj = TypedData_Make_Struct(klass, struct point, &point3_type, pt);
    pt->label = Qnil;
    return obj;
}

static VALUE
point_initialize(int argc, VALUE *argv, VALUE self)
{
    struct point *pt;
    VALUE x, y, label;
    rb_scan_args(argc, argv, "21", &x, &y, &label);
    TypedData_Get_Struct(self, struct point, &point_type, pt);
    pt->x = NUM2DBL(x);
    pt->y = NUM2DBL(y);
    pt->label = label;
    return self;
}

static VALUE
point_init_copy(VALUE self, VALUE orig)
{
    struct point *a, *b;
    TypedData_Get_Struct(self, struct point, &point_type, a);
    TypedData_Get_Struct(orig, struct point, &point_type, b);
    *a = *b;
    return self;
}

static VALUE
point_x(VALUE self)
{
    struct point *pt;
    TypedData_Get_Struct(self, struct point, &point_type, pt);
    return DBL2NUM(pt->x);
}

static VALUE
point_y(VALUE self)
{
    struct point *pt;
    TypedData_Get_Struct(self, struct point, &point_type, pt);
    return DBL2NUM(pt->y);
}

static VALUE
point_label(VALUE self)
{
    struct point *pt;
    TypedData_Get_Struct(self, struct point, &point_type, pt);
    return pt->label;
}

static VALUE
point_set_label(VALUE self, VALUE label)
{
    struct point *pt;
    TypedData_Get_Struct(self, struct point, &point_type, pt);
    pt->label = label;
    return label;
}

/* The checks and accessors an extension applies to an arbitrary VALUE. */
static VALUE
data_x_of(VALUE self, VALUE obj)
{
    return point_x(obj);
}

static VALUE
data_point_p(VALUE self, VALUE obj)
{
    return rb_typeddata_is_kind_of(obj, &point_type) ? Qtrue : Qfalse;
}

static VALUE
data_type_name(VALUE self, VALUE obj)
{
    if (!RB_TYPE_P(obj, T_DATA))
        return Qnil;
    if (!RTYPEDDATA_P(obj))
        return rb_str_new_cstr("untyped");
    return rb_str_new_cstr(RTYPEDDATA_TYPE(obj)->wrap_struct_name);
}

static VALUE
data_same_ptr_p(VALUE self, VALUE obj)
{
    struct point *pt;
    TypedData_Get_Struct(obj, struct point, &point_type, pt);
    return (void *)pt == RTYPEDDATA_DATA(obj) && (void *)pt == DATA_PTR(obj) ? Qtrue : Qfalse;
}

static VALUE
data_has_alloc_p(VALUE self, VALUE klass)
{
    return rb_get_alloc_func(klass) ? Qtrue : Qfalse;
}

static VALUE
data_points_freed(VALUE self)
{
    return INT2NUM(points_freed);
}

/* ---- untyped Data ---- */

struct counter {
    long n;
};

static VALUE
counter_alloc(VALUE klass)
{
    struct counter *c;
    return Data_Make_Struct(klass, struct counter, NULL, RUBY_DEFAULT_FREE, c);
}

static VALUE
counter_incr(VALUE self)
{
    struct counter *c;
    Data_Get_Struct(self, struct counter, c);
    c->n++;
    return LONG2NUM(c->n);
}

static VALUE
counter_reset(VALUE self)
{
    /* DATA_PTR as an lvalue: swap in a fresh record. */
    struct counter *old = DATA_PTR(self);
    struct counter *c = ZALLOC(struct counter);
    c->n = -old->n;
    DATA_PTR(self) = c;
    xfree(old);
    return LONG2NUM(c->n);
}

static VALUE
counter_wrap(VALUE self, VALUE n)
{
    /* A Data object made without an alloc function, by a factory. */
    struct counter *c = ZALLOC(struct counter);
    c->n = NUM2LONG(n);
    return Data_Wrap_Struct(cCounter, NULL, RUBY_DEFAULT_FREE, c);
}

/* ---- blocks from C ---- */

static VALUE
sum_i(RB_BLOCK_CALL_FUNC_ARGLIST(elt, acc))
{
    rb_ary_push(acc, rb_yield(INT2NUM(NUM2INT(elt) * 2)));
    return Qnil;
}

static VALUE
data_map2(VALUE self, VALUE ary)
{
    VALUE acc = rb_ary_new();
    rb_block_call(ary, rb_intern("each"), 0, NULL, sum_i, acc);
    return acc;
}

static VALUE
with_index_i(RB_BLOCK_CALL_FUNC_ARGLIST(elt, acc))
{
    VALUE pair = rb_ary_new_from_args(3, INT2NUM(argc), elt, argv[1]);
    rb_ary_push(acc, pair);
    return Qnil;
}

static VALUE
data_with_index(VALUE self, VALUE ary)
{
    VALUE acc = rb_ary_new();
    rb_block_call(ary, rb_intern("each_with_index"), 0, NULL, with_index_i, acc);
    return acc;
}

static VALUE
first_over_i(RB_BLOCK_CALL_FUNC_ARGLIST(elt, limit))
{
    if (NUM2LONG(elt) > NUM2LONG(limit))
        rb_iter_break_value(elt);
    return Qnil;
}

static VALUE
data_first_over(VALUE self, VALUE obj, VALUE limit)
{
    return rb_block_call(obj, rb_intern("each"), 0, NULL, first_over_i, limit);
}

static VALUE
break_i(RB_BLOCK_CALL_FUNC_ARGLIST(elt, limit))
{
    rb_iter_break();
    return Qnil;
}

static VALUE
data_break_first(VALUE self, VALUE obj)
{
    return rb_block_call(obj, rb_intern("each"), 0, NULL, break_i, Qnil);
}

static VALUE
raise_i(RB_BLOCK_CALL_FUNC_ARGLIST(elt, limit))
{
    rb_raise(rb_eRangeError, "from the block: %d", NUM2INT(elt));
}

static VALUE
data_raise_in_block(VALUE self, VALUE obj)
{
    return rb_block_call(obj, rb_intern("each"), 0, NULL, raise_i, Qnil);
}

static VALUE
adder_i(RB_BLOCK_CALL_FUNC_ARGLIST(elt, n))
{
    return INT2NUM(NUM2INT(elt) + NUM2INT(n));
}

static VALUE
data_adder(VALUE self, VALUE n)
{
    return rb_proc_new(adder_i, n);
}

static VALUE
collect_i(RB_BLOCK_CALL_FUNC_ARGLIST(elt, acc))
{
    rb_ary_push(acc, elt);
    return Qnil;
}

static VALUE
iterate_each(VALUE obj)
{
    return rb_each(obj);
}

static VALUE
data_iterate(VALUE self, VALUE obj)
{
    VALUE acc = rb_ary_new();
    rb_iterate(iterate_each, obj, collect_i, acc);
    return acc;
}

/* Keeps the proc, so the callback outlives the call that made it. */
static VALUE
data_pass_block(VALUE self, VALUE ary)
{
    /* NULL callback: the method's own block is passed on. */
    return rb_block_call(ary, rb_intern("map"), 0, NULL, NULL, Qnil);
}

/* ---- rb_call_super ---- */

static VALUE
derived_greet(int argc, VALUE *argv, VALUE self)
{
    VALUE from_base = rb_call_super(argc, argv);
    return rb_sprintf("derived(%" PRIsVALUE ")", from_base);
}

static VALUE
derived_each_twice(VALUE self)
{
    /* The block the method received goes to the super method. */
    return rb_call_super(0, NULL);
}

static VALUE
derived_initialize(VALUE self, VALUE a, VALUE b)
{
    rb_call_super(1, &a);
    rb_iv_set(self, "@b", b);
    return self;
}

static VALUE
data_super_outside(VALUE self)
{
    return rb_call_super(0, NULL);
}

/* ---- Struct, allocation ---- */

static VALUE
data_structs(VALUE self)
{
    VALUE pair = rb_struct_define("CapiPair", "a", "b", NULL);
    VALUE under = rb_struct_define_under(mData, "Pair", "left", "right", NULL);
    VALUE anon = rb_struct_define(NULL, "k", NULL);
    VALUE s = rb_struct_new(pair, INT2NUM(1), INT2NUM(2));
    VALUE u = rb_struct_alloc(under, rb_ary_new_from_args(2, rb_str_new_cstr("l"), Qnil));
    rb_struct_aset(u, INT2NUM(1), rb_str_new_cstr("r"));
    VALUE a = rb_struct_new(anon, Qtrue);
    return rb_ary_new_from_args(10,
        rb_class_name(pair), rb_class_name(under), rb_mod_name(anon),
        rb_ary_dup(rb_struct_members(s)), rb_ary_dup(rb_struct_s_members(under)),
        rb_struct_aref(s, INT2NUM(0)), rb_struct_getmember(s, rb_intern("b")),
        rb_struct_size(u), rb_struct_aref(u, ID2SYM(rb_intern("right"))),
        rb_struct_getmember(a, rb_intern("k")));
}

static VALUE
data_build(int argc, VALUE *argv, VALUE self)
{
    VALUE obj = rb_obj_alloc(argv[0]);
    rb_obj_call_init(obj, argc - 1, argv + 1);
    return obj;
}

static VALUE
data_anonymous(VALUE self)
{
    VALUE k = rb_class_new(cPoint);
    VALUE m = rb_module_new();
    return rb_ary_new_from_args(4, rb_class_name(k) == Qnil ? Qnil : Qfalse, rb_class_superclass(k),
                                rb_obj_is_kind_of(m, rb_cModule), rb_obj_is_kind_of(m, rb_cClass));
}

void
Init_capi_data(void)
{
    mData = rb_define_module("CapiData");

    cPoint = rb_define_class_under(mData, "Point", rb_cObject);
    rb_define_alloc_func(cPoint, point_alloc);
    rb_define_method(cPoint, "initialize", point_initialize, -1);
    rb_define_method(cPoint, "initialize_copy", point_init_copy, 1);
    rb_define_method(cPoint, "x", point_x, 0);
    rb_define_method(cPoint, "y", point_y, 0);
    rb_define_method(cPoint, "label", point_label, 0);
    rb_define_method(cPoint, "label=", point_set_label, 1);

    cPoint3 = rb_define_class_under(mData, "Point3", cPoint);
    rb_define_alloc_func(cPoint3, point3_alloc);

    cCounter = rb_define_class_under(mData, "Counter", rb_cObject);
    rb_define_alloc_func(cCounter, counter_alloc);
    rb_define_method(cCounter, "incr", counter_incr, 0);
    rb_define_method(cCounter, "reset", counter_reset, 0);
    rb_define_singleton_method(cCounter, "wrap", counter_wrap, 1);

    cNope = rb_define_class_under(mData, "Nope", rb_cObject);
    rb_undef_alloc_func(cNope);

    VALUE cBase = rb_const_get(mData, rb_intern("Base"));
    cDerived = rb_define_class_under(mData, "Derived", cBase);
    rb_define_method(cDerived, "initialize", derived_initialize, 2);
    rb_define_method(cDerived, "greet", derived_greet, -1);
    rb_define_method(cDerived, "each_twice", derived_each_twice, 0);

    rb_define_module_function(mData, "x_of", data_x_of, 1);
    rb_define_module_function(mData, "point?", data_point_p, 1);
    rb_define_module_function(mData, "type_name", data_type_name, 1);
    rb_define_module_function(mData, "same_ptr?", data_same_ptr_p, 1);
    rb_define_module_function(mData, "has_alloc?", data_has_alloc_p, 1);
    rb_define_module_function(mData, "points_freed", data_points_freed, 0);
    rb_define_module_function(mData, "map2", data_map2, 1);
    rb_define_module_function(mData, "with_index", data_with_index, 1);
    rb_define_module_function(mData, "first_over", data_first_over, 2);
    rb_define_module_function(mData, "break_first", data_break_first, 1);
    rb_define_module_function(mData, "raise_in_block", data_raise_in_block, 1);
    rb_define_module_function(mData, "adder", data_adder, 1);
    rb_define_module_function(mData, "iterate", data_iterate, 1);
    rb_define_module_function(mData, "pass_block", data_pass_block, 1);
    rb_define_module_function(mData, "super_outside", data_super_outside, 0);
    rb_define_module_function(mData, "structs", data_structs, 0);
    rb_define_module_function(mData, "build", data_build, -1);
    rb_define_module_function(mData, "anonymous", data_anonymous, 0);
}
