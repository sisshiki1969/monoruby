/*
 * Stage 5 of the CRuby-compatible C API: st_table. Written against
 * CRuby's ruby.h / ruby/st.h only; built and run on both interpreters by
 * tests/capi.rs, which compares the output. The table implementation is
 * CRuby's st.c on both sides, so iteration order, the hash functions and
 * even st_memsize agree.
 */
#include <ruby.h>
#include <ruby/st.h>
#include <string.h>

static VALUE mSt, cTable;

static VALUE
pair(st_data_t k, st_data_t v)
{
    return rb_assoc_new(LONG2NUM((long)k), LONG2NUM((long)v));
}

/* ---- numtable: the whole API on integer keys ---- */

static int
collect_i(st_data_t k, st_data_t v, st_data_t arg)
{
    VALUE ary = (VALUE)arg;
    if (RARRAY_LEN(ary) >= 5) return ST_STOP;
    rb_ary_push(ary, pair(k, v));
    return ST_CONTINUE;
}

static int
delete_even_below_50_i(st_data_t k, st_data_t v, st_data_t arg)
{
    (void)v;
    (void)arg;
    if (k == 60) return ST_STOP;
    if (k < 50 && k % 2 == 0) return ST_DELETE;
    return ST_CONTINUE;
}

static int
bump_i(st_data_t *key, st_data_t *value, st_data_t arg, int existing)
{
    (void)key;
    if (existing) {
        *value += arg;
        return ST_CONTINUE;
    }
    *value = arg * 100;
    return ST_CONTINUE;
}

static int
drop_i(st_data_t *key, st_data_t *value, st_data_t arg, int existing)
{
    (void)key;
    (void)value;
    (void)arg;
    return existing ? ST_DELETE : ST_STOP;
}

static st_data_t
twice(st_data_t k)
{
    return k * 2;
}

static int
sum_i(st_data_t k, st_data_t v, st_data_t arg)
{
    (void)k;
    *(long *)arg += (long)v;
    return ST_CONTINUE;
}

static VALUE
st_numtable_m(VALUE self)
{
    VALUE out = rb_ary_new();
    st_table *tab = st_init_numtable();
    st_table *copy;
    st_data_t k, v;
    st_data_t buf[5];
    st_index_t n, i;
    long sum = 0;
    (void)self;

    for (i = 1; i <= 1000; i++) {
        rb_ary_push(out, INT2FIX(st_insert(tab, i, i * 2)));
    }
    /* [0 x 1000] is dull to print; keep the count and the re-insert. */
    rb_ary_store(out, 0, LONG2NUM(RARRAY_LEN(out)));
    rb_ary_resize(out, 1);
    rb_ary_push(out, INT2FIX(st_insert(tab, 5, 55)));
    rb_ary_push(out, LONG2NUM((long)st_table_size(tab)));
    rb_ary_push(out, st_lookup(tab, 5, &v) ? LONG2NUM((long)v) : Qnil);
    rb_ary_push(out, st_lookup(tab, 777, &v) ? LONG2NUM((long)v) : Qnil);
    rb_ary_push(out, st_lookup(tab, 5000, &v) ? LONG2NUM((long)v) : Qnil);
    rb_ary_push(out, INT2FIX(st_is_member(tab, 1000)));
    rb_ary_push(out, INT2FIX(st_is_member(tab, 1001)));

    k = 10;
    rb_ary_push(out, st_delete(tab, &k, &v) ? pair(k, v) : Qnil);
    k = 10;
    rb_ary_push(out, INT2FIX(st_delete(tab, &k, &v)));
    rb_ary_push(out, st_shift(tab, &k, &v) ? pair(k, v) : Qnil);
    rb_ary_push(out, LONG2NUM((long)st_table_size(tab)));

    {
        VALUE first = rb_ary_new();
        st_foreach(tab, collect_i, (st_data_t)first);
        rb_ary_push(out, first);
    }
    st_foreach(tab, delete_even_below_50_i, 0);
    rb_ary_push(out, LONG2NUM((long)st_table_size(tab)));
    {
        VALUE first = rb_ary_new();
        st_foreach(tab, collect_i, (st_data_t)first);
        rb_ary_push(out, first);
    }
    n = st_keys(tab, buf, 5);
    {
        VALUE keys = rb_ary_new();
        for (i = 0; i < n; i++) rb_ary_push(keys, LONG2NUM((long)buf[i]));
        rb_ary_push(out, keys);
    }
    n = st_values(tab, buf, 5);
    {
        VALUE vals = rb_ary_new();
        for (i = 0; i < n; i++) rb_ary_push(vals, LONG2NUM((long)buf[i]));
        rb_ary_push(out, vals);
    }

    /* update: existing, new, delete-through-update */
    st_update(tab, 3, bump_i, 1000);
    st_update(tab, 2000, bump_i, 7);
    rb_ary_push(out, st_lookup(tab, 3, &v) ? LONG2NUM((long)v) : Qnil);
    rb_ary_push(out, st_lookup(tab, 2000, &v) ? LONG2NUM((long)v) : Qnil);
    rb_ary_push(out, INT2FIX(st_update(tab, 2000, drop_i, 0)));
    rb_ary_push(out, INT2FIX(st_update(tab, 2001, drop_i, 0)));
    rb_ary_push(out, INT2FIX(st_is_member(tab, 2000)));

    /* insert2 copies the key through the function only when new */
    rb_ary_push(out, INT2FIX(st_insert2(tab, 3000, 1, twice)));
    rb_ary_push(out, INT2FIX(st_insert2(tab, 6000, 2, twice)));
    k = 6000;
    rb_ary_push(out, st_get_key(tab, 6000, &k) ? LONG2NUM((long)k) : Qnil);
    rb_ary_push(out, st_lookup(tab, 6000, &v) ? LONG2NUM((long)v) : Qnil);
    rb_ary_push(out, INT2FIX(st_is_member(tab, 3000)));

    st_add_direct(tab, 4000, 44);
    rb_ary_push(out, st_lookup(tab, 4000, &v) ? LONG2NUM((long)v) : Qnil);

    /* copy is independent of the original */
    copy = st_copy(tab);
    rb_ary_push(out, LONG2NUM((long)st_table_size(copy)));
    st_insert(copy, 9999, 1);
    k = 3;
    st_delete(copy, &k, 0);
    rb_ary_push(out, INT2FIX(st_is_member(tab, 9999)));
    rb_ary_push(out, INT2FIX(st_is_member(tab, 3)));
    rb_ary_push(out, INT2FIX(st_is_member(copy, 3)));
    rb_ary_push(out, LONG2NUM((long)(st_memsize(copy) == st_memsize(tab))));
    st_clear(copy);
    rb_ary_push(out, LONG2NUM((long)st_table_size(copy)));
    rb_ary_push(out, INT2FIX(st_shift(copy, &k, &v)));
    st_free_table(copy);

    st_foreach(tab, sum_i, (st_data_t)&sum);
    rb_ary_push(out, LONG2NUM(sum));
    rb_ary_push(out, LONG2NUM((long)st_memsize(tab)));
    st_free_table(tab);

    /* small tables have no bins: everything above again, in miniature */
    tab = st_init_numtable_with_size(2);
    st_insert(tab, 1, 10);
    st_insert(tab, 2, 20);
    st_insert(tab, 3, 30);
    k = 2;
    st_delete(tab, &k, &v);
    st_insert(tab, 4, 40);
    {
        VALUE first = rb_ary_new();
        st_foreach(tab, collect_i, (st_data_t)first);
        rb_ary_push(out, first);
    }
    rb_ary_push(out, LONG2NUM((long)st_memsize(tab)));
    st_free_table(tab);
    return out;
}

/* ---- string tables ---- */

static int
collect_str_i(st_data_t k, st_data_t v, st_data_t arg)
{
    rb_ary_push((VALUE)arg, rb_assoc_new(rb_str_new_cstr((const char *)k), LONG2NUM((long)v)));
    return ST_CONTINUE;
}

static VALUE
st_strtable_m(VALUE self)
{
    VALUE out = rb_ary_new();
    static const char *const words[] = {"foo", "bar", "Foo", "baz", "FOO", "qux", "bar"};
    char probe[8];
    st_table *tab = st_init_strtable();
    st_table *ci = st_init_strcasetable();
    st_data_t v;
    size_t i;
    (void)self;

    for (i = 0; i < sizeof(words) / sizeof(words[0]); i++) {
        rb_ary_push(out, INT2FIX(st_insert(tab, (st_data_t)words[i], (st_data_t)i)));
        rb_ary_push(out, INT2FIX(st_insert(ci, (st_data_t)words[i], (st_data_t)i)));
    }
    rb_ary_push(out, LONG2NUM((long)st_table_size(tab)));
    rb_ary_push(out, LONG2NUM((long)st_table_size(ci)));
    /* a different pointer with the same bytes finds the entry */
    strcpy(probe, "baz");
    rb_ary_push(out, st_lookup(tab, (st_data_t)probe, &v) ? LONG2NUM((long)v) : Qnil);
    strcpy(probe, "BAZ");
    rb_ary_push(out, st_lookup(tab, (st_data_t)probe, &v) ? LONG2NUM((long)v) : Qnil);
    rb_ary_push(out, st_lookup(ci, (st_data_t)probe, &v) ? LONG2NUM((long)v) : Qnil);
    strcpy(probe, "fOo");
    rb_ary_push(out, st_lookup(ci, (st_data_t)probe, &v) ? LONG2NUM((long)v) : Qnil);
    {
        VALUE all = rb_ary_new();
        st_foreach(tab, collect_str_i, (st_data_t)all);
        rb_ary_push(out, all);
        all = rb_ary_new();
        st_foreach(ci, collect_str_i, (st_data_t)all);
        rb_ary_push(out, all);
    }
    rb_ary_push(out, INT2FIX(st_strcasecmp("Hello", "hELLO")));
    rb_ary_push(out, INT2FIX(st_strcasecmp("abc", "abd") < 0));
    rb_ary_push(out, INT2FIX(st_strncasecmp("abcX", "ABCy", 3)));
    rb_ary_push(out, INT2FIX(st_numcmp(3, 3)));
    rb_ary_push(out, INT2FIX(st_numcmp(3, 4) != 0));
    st_free_table(tab);
    st_free_table(ci);
    return out;
}

/* ---- a hash type of our own, with heavy collisions ---- */

static int
mod_cmp(st_data_t a, st_data_t b)
{
    return a != b;
}

static st_index_t
mod_hash(st_data_t a)
{
    return a % 7;
}

static const struct st_hash_type mod_type = {mod_cmp, mod_hash};

static int
check_i(st_data_t k, st_data_t v, st_data_t arg, int error)
{
    (void)v;
    if (error) {
        rb_ary_push((VALUE)arg, ID2SYM(rb_intern("error")));
        return ST_STOP;
    }
    if (k % 3 == 0) return ST_DELETE;
    rb_ary_push((VALUE)arg, LONG2NUM((long)k));
    return k > 40 ? ST_STOP : ST_CONTINUE;
}

static int
replace_val_i(st_data_t *key, st_data_t *value, st_data_t arg, int existing)
{
    (void)key;
    (void)existing;
    *value = *value * 10 + arg;
    return ST_CONTINUE;
}

static int
replace_sel_i(st_data_t k, st_data_t v, st_data_t arg, int error)
{
    (void)v;
    (void)arg;
    (void)error;
    return k % 2 ? ST_REPLACE : ST_CONTINUE;
}

static VALUE
st_custom_m(VALUE self)
{
    VALUE out = rb_ary_new();
    st_table *tab = st_init_table_with_size(&mod_type, 0);
    st_data_t k, v;
    st_index_t i;
    (void)self;

    for (i = 1; i <= 60; i++) st_insert(tab, i, i + 100);
    rb_ary_push(out, LONG2NUM((long)st_table_size(tab)));
    rb_ary_push(out, st_lookup(tab, 43, &v) ? LONG2NUM((long)v) : Qnil);
    rb_ary_push(out, st_lookup(tab, 61, &v) ? LONG2NUM((long)v) : Qnil);
    for (i = 1; i <= 60; i += 4) {
        k = i;
        st_delete(tab, &k, 0);
    }
    rb_ary_push(out, LONG2NUM((long)st_table_size(tab)));
    rb_ary_push(out, st_lookup(tab, 5, &v) ? LONG2NUM((long)v) : Qnil);
    rb_ary_push(out, st_lookup(tab, 6, &v) ? LONG2NUM((long)v) : Qnil);
    {
        VALUE seen = rb_ary_new();
        st_foreach_check(tab, check_i, (st_data_t)seen, 0);
        rb_ary_push(out, seen);
    }
    rb_ary_push(out, LONG2NUM((long)st_table_size(tab)));
    st_foreach_with_replace(tab, replace_sel_i, replace_val_i, 1);
    rb_ary_push(out, st_lookup(tab, 7, &v) ? LONG2NUM((long)v) : Qnil);
    rb_ary_push(out, st_lookup(tab, 8, &v) ? LONG2NUM((long)v) : Qnil);
    {
        st_data_t keys[8];
        st_index_t n = st_keys_check(tab, keys, 8, 0);
        VALUE ks = rb_ary_new();
        for (i = 0; i < n; i++) rb_ary_push(ks, LONG2NUM((long)keys[i]));
        rb_ary_push(out, ks);
        n = st_values_check(tab, keys, 8, 0);
        ks = rb_ary_new();
        for (i = 0; i < n; i++) rb_ary_push(ks, LONG2NUM((long)keys[i]));
        rb_ary_push(out, ks);
    }
    /* delete_safe leaves a tombstone the caller cleans up */
    k = 2;
    rb_ary_push(out, INT2FIX(st_delete_safe(tab, &k, &v, 0)));
    st_cleanup_safe(tab, 0);
    rb_ary_push(out, INT2FIX(st_is_member(tab, 2)));
    rb_ary_push(out, LONG2NUM((long)st_table_size(tab)));
    st_free_table(tab);
    return out;
}

/* ---- the hash functions themselves ---- */

static VALUE
st_hashes_m(VALUE self, VALUE str)
{
    VALUE out = rb_ary_new();
    st_index_t h;
    (void)self;
    StringValue(str);
    h = st_hash(RSTRING_PTR(str), RSTRING_LEN(str), 0);
    rb_ary_push(out, ULL2NUM(h));
    rb_ary_push(out, ULL2NUM(st_hash(RSTRING_PTR(str), RSTRING_LEN(str), 12345)));
    rb_ary_push(out, ULL2NUM(st_hash_uint(h, 7)));
    rb_ary_push(out, ULL2NUM(st_hash_uint32(h, 9)));
    rb_ary_push(out, ULL2NUM(st_hash_end(h)));
    rb_ary_push(out, ULL2NUM(st_hash_start(5)));
    rb_ary_push(out, ULL2NUM(st_numhash(12345)));
    rb_ary_push(out, ULL2NUM(st_numhash(0)));
    rb_ary_push(out, LONG2NUM(rb_memhash(RSTRING_PTR(str), RSTRING_LEN(str)) ==
                              rb_memhash(RSTRING_PTR(str), RSTRING_LEN(str))));
    return out;
}

/* ---- an st_table holding VALUEs, behind TypedData ---- */

struct table {
    st_table *tab;
};

static int
mark_i(st_data_t k, st_data_t v, st_data_t arg)
{
    (void)arg;
    rb_gc_mark((VALUE)k);
    rb_gc_mark((VALUE)v);
    return ST_CONTINUE;
}

static void
table_mark(void *p)
{
    struct table *t = p;
    if (t->tab) st_foreach(t->tab, mark_i, 0);
}

static int tables_freed;

static void
table_free(void *p)
{
    struct table *t = p;
    if (t->tab) st_free_table(t->tab);
    tables_freed++;
    xfree(t);
}

static size_t
table_size(const void *p)
{
    const struct table *t = p;
    return sizeof(*t) + (t->tab ? st_memsize(t->tab) : 0);
}

static const rb_data_type_t table_type = {
    "CapiSt::Table",
    {table_mark, table_free, table_size,},
    0, 0, RUBY_TYPED_FREE_IMMEDIATELY
};

static VALUE
table_alloc(VALUE klass)
{
    struct table *t;
    VALUE obj = TypedData_Make_Struct(klass, struct table, &table_type, t);
    t->tab = st_init_numtable();
    return obj;
}

static struct table *
get_table(VALUE self)
{
    struct table *t;
    TypedData_Get_Struct(self, struct table, &table_type, t);
    return t;
}

static VALUE
table_aset(VALUE self, VALUE key, VALUE val)
{
    st_insert(get_table(self)->tab, (st_data_t)key, (st_data_t)val);
    return val;
}

static VALUE
table_aref(VALUE self, VALUE key)
{
    st_data_t v;
    if (st_lookup(get_table(self)->tab, (st_data_t)key, &v)) return (VALUE)v;
    return Qnil;
}

static VALUE
table_delete(VALUE self, VALUE key)
{
    st_data_t k = (st_data_t)key, v;
    if (st_delete(get_table(self)->tab, &k, &v)) return (VALUE)v;
    return Qnil;
}

static VALUE
table_len(VALUE self)
{
    return LONG2NUM((long)st_table_size(get_table(self)->tab));
}

static int
yield_i(st_data_t k, st_data_t v, st_data_t arg)
{
    (void)arg;
    rb_yield(rb_assoc_new((VALUE)k, (VALUE)v));
    return ST_CONTINUE;
}

static VALUE
table_each(VALUE self)
{
    st_foreach(get_table(self)->tab, yield_i, 0);
    return self;
}

static VALUE
table_freed_count(VALUE self)
{
    (void)self;
    return INT2FIX(tables_freed);
}

void
Init_capi_st(void)
{
    mSt = rb_define_module("CapiSt");
    rb_define_module_function(mSt, "numtable", st_numtable_m, 0);
    rb_define_module_function(mSt, "strtable", st_strtable_m, 0);
    rb_define_module_function(mSt, "custom", st_custom_m, 0);
    rb_define_module_function(mSt, "hashes", st_hashes_m, 1);
    rb_define_module_function(mSt, "tables_freed", table_freed_count, 0);

    cTable = rb_define_class_under(mSt, "Table", rb_cObject);
    rb_define_alloc_func(cTable, table_alloc);
    rb_define_method(cTable, "[]=", table_aset, 2);
    rb_define_method(cTable, "[]", table_aref, 1);
    rb_define_method(cTable, "delete", table_delete, 1);
    rb_define_method(cTable, "size", table_len, 0);
    rb_define_method(cTable, "each", table_each, 0);
}
