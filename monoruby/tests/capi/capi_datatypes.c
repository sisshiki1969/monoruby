/* The data-type functions of doc/c_api.md §3.27, driven by
 * capi_datatypes.rb on CRuby and on monoruby. */
#include "ruby.h"
#include "ruby/encoding.h"
#include <string.h>

static VALUE m;

/* String */

static VALUE
subpos(VALUE self, VALUE str, VALUE beg, VALUE len)
{
    long l = NUM2LONG(len);
    char *p = rb_str_subpos(str, NUM2LONG(beg), &l);
    if (!p)
        return Qnil;
    return rb_ary_new_from_args(2, LONG2NUM(p - RSTRING_PTR(str)), LONG2NUM(l));
}

static VALUE
sublen(VALUE self, VALUE str, VALUE pos)
{
    return LONG2NUM(rb_str_sublen(str, NUM2LONG(pos)));
}

static VALUE
offset(VALUE self, VALUE str, VALUE pos)
{
    return LONG2NUM(rb_str_offset(str, NUM2LONG(pos)));
}

static VALUE
update(VALUE self, VALUE str, VALUE beg, VALUE len, VALUE val)
{
    rb_str_update(str, NUM2LONG(beg), NUM2LONG(len), val);
    return str;
}

static VALUE
str_misc(VALUE self, VALUE a, VALUE b)
{
    VALUE r = rb_ary_new();
    rb_ary_push(r, INT2FIX(rb_str_comparable(a, b)));
    rb_ary_push(r, INT2FIX(rb_str_hash_cmp(a, b)));
    rb_ary_push(r, rb_str_succ(a));
    rb_ary_push(r, rb_str_export_locale(rb_str_new_cstr("abc")));
    rb_ary_push(r, rb_str_encode_ospath(a));
    VALUE i1 = rb_interned_str_cstr("interned"), i2 = rb_interned_str("interned", 8);
    rb_ary_push(r, rb_ary_new_from_args(4, i1, rb_obj_frozen_p(i1), rb_obj_encoding(i1),
                                        i1 == i2 ? Qtrue : Qfalse));
    rb_ary_push(r, rb_obj_encoding(rb_interned_str("\xff", 1)));
    rb_ary_push(r, rb_obj_encoding(rb_usascii_str_new_static("st", 2)));
    return r;
}

static VALUE
ellipsize(VALUE self, VALUE str, VALUE len)
{
    return rb_str_ellipsize(str, NUM2LONG(len));
}

static VALUE
scrub(VALUE self, VALUE str, VALUE repl)
{
    return rb_str_scrub(str, repl);
}

static VALUE
uv_to_utf8(VALUE self, VALUE uv)
{
    char buf[6];
    int n = rb_uv_to_utf8(buf, NUM2ULONG(uv));
    return rb_enc_str_new(buf, n, rb_utf8_encoding());
}

static VALUE
memcicmp(VALUE self, VALUE a, VALUE b)
{
    long n = RSTRING_LEN(a) < RSTRING_LEN(b) ? RSTRING_LEN(a) : RSTRING_LEN(b);
    int c = rb_memcicmp(RSTRING_PTR(a), RSTRING_PTR(b), n);
    return INT2FIX(c < 0 ? -1 : c > 0);
}

static VALUE
invalid_str(VALUE self, VALUE s)
{
    rb_invalid_str(StringValueCStr(s), "Integer()");
    return Qnil;
}

static VALUE
encode(VALUE self, VALUE str, VALUE to, VALUE flags, VALUE opts)
{
    return rb_str_encode(str, to, NUM2INT(flags), opts);
}

static VALUE
new_with_class(VALUE self, VALUE tmpl)
{
    return rb_str_new_with_class(tmpl, "made", 4);
}

static VALUE
cstr2inum(VALUE self, VALUE s, VALUE base)
{
    return rb_cstr2inum(StringValueCStr(s), NUM2INT(base));
}

static VALUE
external_str(VALUE self, VALUE s, VALUE enc)
{
    return rb_external_str_new_with_enc(RSTRING_PTR(s), RSTRING_LEN(s), rb_to_encoding(enc));
}

static VALUE
export_to_enc(VALUE self, VALUE s, VALUE enc)
{
    VALUE r = rb_str_export_to_enc(s, rb_to_encoding(enc));
    return rb_ary_new_from_args(2, r, r == s ? Qtrue : Qfalse);
}

static VALUE
sprintf_value(VALUE self, VALUE width, VALUE prec, VALUE v)
{
    return rb_sprintf("[%*.*" PRIsVALUE "][%-6" PRIsVALUE "][%+" PRIsVALUE "]", FIX2INT(width),
                      FIX2INT(prec), v, v, v);
}

static VALUE
value_cstr(VALUE self, VALUE s)
{
    return rb_str_new_cstr(rb_string_value_cstr(&s) ? "ok" : "null");
}

/* Array */

static VALUE
ary_misc(VALUE self, VALUE a, VALUE b)
{
    VALUE r = rb_ary_new();
    rb_ary_push(r, rb_ary_cmp(a, b));
    rb_ary_push(r, rb_ary_rassoc(rb_ary_new_from_args(2, rb_ary_new_from_args(2, INT2FIX(1), INT2FIX(2)),
                                                      rb_ary_new_from_args(2, INT2FIX(3), INT2FIX(4))),
                                 INT2FIX(4)));
    VALUE c = rb_ary_new();
    rb_ary_replace(c, a);
    rb_ary_push(r, c);
    rb_ary_push(r, rb_ary_resurrect(a));
    VALUE buf[3] = {INT2FIX(1), INT2FIX(2), INT2FIX(3)};
    rb_mem_clear(buf, 2);
    rb_ary_push(r, rb_ary_new_from_values(3, buf));
    return r;
}

static VALUE
rotate(VALUE self, VALUE a, VALUE n)
{
    return rb_ary_rotate(a, NUM2LONG(n));
}

static VALUE
modify(VALUE self, VALUE a)
{
    rb_ary_modify(a);
    return Qtrue;
}

static VALUE
ary_each(VALUE self, VALUE a)
{
    return rb_ary_each(a);
}

static VALUE
new_capa(VALUE self, VALUE n)
{
    return rb_ary_new_capa(NUM2LONG(n));
}

/* Hash */

static VALUE
bulk_insert(VALUE self, VALUE h, VALUE pairs)
{
    rb_hash_bulk_insert(RARRAY_LEN(pairs), RARRAY_CONST_PTR(pairs), h);
    return h;
}

static VALUE
delete_if(VALUE self, VALUE h)
{
    return rb_hash_delete_if(h);
}

static VALUE
sum_values(VALUE key, VALUE oldv, VALUE newv)
{
    return rb_ary_new_from_args(3, key, oldv, newv);
}

static VALUE
update_by(VALUE self, VALUE h1, VALUE h2, VALUE with)
{
    return rb_hash_update_by(h1, h2, RTEST(with) ? sum_values : NULL);
}

static VALUE
hash_of(VALUE self, VALUE obj)
{
    VALUE h = rb_hash(obj);
    return FIXNUM_P(h) ? Qtrue : h;
}

static VALUE
hash_capa(VALUE self, VALUE n)
{
    return rb_hash_new_capa(NUM2LONG(n));
}

/* Set */

static int
set_collect(VALUE elem, VALUE acc)
{
    rb_ary_push(acc, elem);
    return RARRAY_LEN(acc) >= 2 ? ST_STOP : ST_CONTINUE;
}

static VALUE
set_api(VALUE self)
{
    VALUE r = rb_ary_new();
    VALUE s = rb_set_new_capa(4);
    rb_ary_push(r, rb_set_add(s, INT2FIX(1)) ? Qtrue : Qfalse);
    rb_ary_push(r, rb_set_add(s, INT2FIX(1)) ? Qtrue : Qfalse);
    rb_set_add(s, INT2FIX(2));
    rb_set_add(s, INT2FIX(3));
    rb_ary_push(r, rb_set_lookup(s, INT2FIX(2)) ? Qtrue : Qfalse);
    rb_ary_push(r, rb_set_lookup(s, INT2FIX(9)) ? Qtrue : Qfalse);
    rb_ary_push(r, SIZET2NUM(rb_set_size(s)));
    VALUE acc = rb_ary_new();
    rb_set_foreach(s, set_collect, acc);
    rb_ary_push(r, acc);
    rb_ary_push(r, rb_set_delete(s, INT2FIX(2)) ? Qtrue : Qfalse);
    rb_ary_push(r, rb_set_delete(s, INT2FIX(2)) ? Qtrue : Qfalse);
    rb_ary_push(r, rb_funcall(s, rb_intern("to_a"), 0));
    rb_ary_push(r, rb_set_clear(s) == s ? Qtrue : Qfalse);
    rb_ary_push(r, rb_set_new());
    return r;
}

/* Struct / Data */

static VALUE
data_define(VALUE self, VALUE super)
{
    return rb_data_define(NIL_P(super) ? 0 : super, "x", "y", NULL);
}

static VALUE
data_define_dup(VALUE self)
{
    return rb_data_define(0, "x", "x", NULL);
}

static VALUE
struct_noaccessor(VALUE self)
{
    VALUE k = rb_struct_define_without_accessor(NULL, rb_cStruct, NULL, "p", "q", NULL);
    VALUE o = rb_struct_alloc_noinit(k);
    return rb_ary_new_from_args(4, rb_ary_dup(rb_struct_s_members(k)),
                                rb_obj_respond_to(o, rb_intern("p"), 0) ? Qtrue : Qfalse,
                                rb_struct_aref(rb_struct_new(k, INT2FIX(1), INT2FIX(2)), INT2FIX(1)),
                                rb_obj_is_kind_of(o, k));
}

/* Numbers */

static VALUE
num_misc(VALUE self, VALUE big)
{
    VALUE r = rb_ary_new();
    rb_ary_push(r, rb_num2fix(INT2FIX(5)));
    rb_ary_push(r, rb_fix2str(INT2FIX(255), 16));
    rb_ary_push(r, INT2FIX(rb_fix2short(INT2FIX(-7))));
    rb_ary_push(r, INT2FIX(rb_fix2ushort(INT2FIX(7))));
    rb_ary_push(r, rb_dbl_cmp(1.0, 2.0));
    rb_ary_push(r, rb_dbl_cmp(2.0, 2.0));
    rb_ary_push(r, rb_dbl_cmp(0.0 / 0.0, 2.0));
    rb_ary_push(r, rb_check_to_int(rb_float_new(1.5)));
    rb_ary_push(r, rb_check_to_int(rb_str_new_cstr("1")));
    rb_ary_push(r, rb_check_to_float(INT2FIX(3)));
    rb_ary_push(r, rb_check_to_float(rb_str_new_cstr("3")));
    rb_ary_push(r, INT2FIX(rb_big_sign(big)));
    rb_ary_push(r, INT2FIX(rb_big_sign(rb_funcall(big, rb_intern("-@"), 0))));
    rb_ary_push(r, rb_big_eql(big, big));
    rb_ary_push(r, rb_big_idiv(big, INT2FIX(7)));
    rb_ary_push(r, rb_big_divmod(big, INT2FIX(7)));
    rb_ary_push(r, INT2FIX(rb_bigzero_p(big)));
    rb_ary_push(r, rb_num_coerce_bit(INT2FIX(6), INT2FIX(3), '&'));
    size_t nlz;
    size_t words = rb_absint_numwords(big, 32, &nlz);
    rb_ary_push(r, rb_ary_new_from_args(2, SIZET2NUM(words), SIZET2NUM(nlz)));
    unsigned long buf[3];
    rb_big_pack(rb_funcall(big, rb_intern("-@"), 0), buf, 3);
    rb_ary_push(r, rb_big_unpack(buf, 3));
    unsigned char bytes[4] = {0x12, 0x34, 0x56, 0x78};
    rb_ary_push(r, rb_integer_unpack(bytes, 4, 1, 0, INTEGER_PACK_BIG_ENDIAN));
    rb_ary_push(r, rb_integer_unpack(bytes, 2, 2, 0, INTEGER_PACK_LITTLE_ENDIAN));
    unsigned char neg[2] = {0xff, 0xfe};
    rb_ary_push(r, rb_integer_unpack(neg, 2, 1, 0, INTEGER_PACK_BIG_ENDIAN | INTEGER_PACK_2COMP));
    rb_ary_push(r, rb_integer_unpack(neg, 2, 1, 0, INTEGER_PACK_BIG_ENDIAN | INTEGER_PACK_NEGATIVE));
    rb_ary_push(r, rb_flt_rationalize(rb_float_new(0.333)));
    rb_ary_push(r, rb_flt_rationalize_with_prec(rb_float_new(0.333), rb_float_new(0.01)));
    return r;
}

static VALUE
num2fix(VALUE self, VALUE v)
{
    return rb_num2fix(v);
}

static VALUE
num2long(VALUE self, VALUE v)
{
    return LONG2NUM(NUM2LONG(v));
}

static VALUE
num2ulong(VALUE self, VALUE v)
{
    return ULONG2NUM(NUM2ULONG(v));
}

static VALUE
num2dbl(VALUE self, VALUE v)
{
    return DBL2NUM(NUM2DBL(v));
}

static VALUE
zerodiv(VALUE self)
{
    rb_num_zerodiv();
    return Qnil;
}

static VALUE
out_of_int(VALUE self, VALUE v)
{
    rb_out_of_int(NUM2LONG(v));
    return Qnil;
}

static VALUE
cmperr(VALUE self, VALUE a, VALUE b)
{
    rb_cmperr(a, b);
    return Qnil;
}

static VALUE
complex_api(VALUE self)
{
    VALUE a = rb_complex_raw(INT2FIX(1), INT2FIX(2)), b = rb_dbl_complex_new(0.5, -1.5);
    VALUE r = rb_ary_new();
    rb_ary_push(r, a);
    rb_ary_push(r, b);
    rb_ary_push(r, rb_complex_real(a));
    rb_ary_push(r, rb_complex_imag(a));
    rb_ary_push(r, rb_complex_plus(a, b));
    rb_ary_push(r, rb_complex_minus(a, b));
    rb_ary_push(r, rb_complex_mul(a, b));
    rb_ary_push(r, rb_complex_div(a, INT2FIX(2)));
    rb_ary_push(r, rb_complex_uminus(a));
    rb_ary_push(r, rb_complex_conjugate(a));
    rb_ary_push(r, rb_complex_abs(rb_complex_raw(INT2FIX(3), INT2FIX(4))));
    rb_ary_push(r, rb_complex_arg(rb_complex_raw(INT2FIX(0), INT2FIX(1))));
    rb_ary_push(r, rb_complex_pow(a, INT2FIX(2)));
    rb_ary_push(r, rb_complex_new_polar(INT2FIX(2), INT2FIX(0)));
    rb_ary_push(r, rb_complex_polar(INT2FIX(1), INT2FIX(0)));
    return r;
}

/* Symbols and IDs */

static VALUE
id_kinds(VALUE self, VALUE sym)
{
    ID id = SYM2ID(sym);
    VALUE r = rb_ary_new();
    if (rb_is_const_id(id))
        rb_ary_push(r, ID2SYM(rb_intern("const")));
    if (rb_is_class_id(id))
        rb_ary_push(r, ID2SYM(rb_intern("class")));
    if (rb_is_global_id(id))
        rb_ary_push(r, ID2SYM(rb_intern("global")));
    if (rb_is_instance_id(id))
        rb_ary_push(r, ID2SYM(rb_intern("instance")));
    if (rb_is_attrset_id(id))
        rb_ary_push(r, ID2SYM(rb_intern("attrset")));
    if (rb_is_local_id(id))
        rb_ary_push(r, ID2SYM(rb_intern("local")));
    if (rb_is_junk_id(id))
        rb_ary_push(r, ID2SYM(rb_intern("junk")));
    return r;
}

static VALUE
attrset(VALUE self, VALUE sym)
{
    return ID2SYM(rb_id_attrset(SYM2ID(sym)));
}

static VALUE
symname_p(VALUE self, VALUE s)
{
    return rb_symname_p(StringValueCStr(s)) ? Qtrue : Qfalse;
}

static VALUE
enc_symname_p(VALUE self, VALUE s)
{
    return rb_enc_symname2_p(RSTRING_PTR(s), RSTRING_LEN(s), rb_enc_get(s)) ? Qtrue : Qfalse;
}

static VALUE
all_symbols_has(VALUE self, VALUE sym)
{
    return rb_ary_includes(rb_sym_all_symbols(), sym);
}

static VALUE
name_error(VALUE self, VALUE sym)
{
    rb_name_error(SYM2ID(sym), "bad name %" PRIsVALUE, sym);
    return Qnil;
}

void
Init_capi_datatypes(void)
{
    m = rb_define_module("CapiDatatypes");
    rb_define_module_function(m, "subpos", subpos, 3);
    rb_define_module_function(m, "sublen", sublen, 2);
    rb_define_module_function(m, "offset", offset, 2);
    rb_define_module_function(m, "update", update, 4);
    rb_define_module_function(m, "str_misc", str_misc, 2);
    rb_define_module_function(m, "ellipsize", ellipsize, 2);
    rb_define_module_function(m, "scrub", scrub, 2);
    rb_define_module_function(m, "uv_to_utf8", uv_to_utf8, 1);
    rb_define_module_function(m, "memcicmp", memcicmp, 2);
    rb_define_module_function(m, "invalid_str", invalid_str, 1);
    rb_define_module_function(m, "encode", encode, 4);
    rb_define_module_function(m, "new_with_class", new_with_class, 1);
    rb_define_module_function(m, "cstr2inum", cstr2inum, 2);
    rb_define_module_function(m, "external_str", external_str, 2);
    rb_define_module_function(m, "export_to_enc", export_to_enc, 2);
    rb_define_module_function(m, "sprintf_value", sprintf_value, 3);
    rb_define_module_function(m, "value_cstr", value_cstr, 1);
    rb_define_module_function(m, "ary_misc", ary_misc, 2);
    rb_define_module_function(m, "rotate", rotate, 2);
    rb_define_module_function(m, "modify", modify, 1);
    rb_define_module_function(m, "ary_each", ary_each, 1);
    rb_define_module_function(m, "new_capa", new_capa, 1);
    rb_define_module_function(m, "bulk_insert", bulk_insert, 2);
    rb_define_module_function(m, "delete_if", delete_if, 1);
    rb_define_module_function(m, "update_by", update_by, 3);
    rb_define_module_function(m, "hash_of", hash_of, 1);
    rb_define_module_function(m, "hash_capa", hash_capa, 1);
    rb_define_module_function(m, "set_api", set_api, 0);
    rb_define_module_function(m, "data_define", data_define, 1);
    rb_define_module_function(m, "data_define_dup", data_define_dup, 0);
    rb_define_module_function(m, "struct_noaccessor", struct_noaccessor, 0);
    rb_define_module_function(m, "num_misc", num_misc, 1);
    rb_define_module_function(m, "num2fix", num2fix, 1);
    rb_define_module_function(m, "num2long", num2long, 1);
    rb_define_module_function(m, "num2ulong", num2ulong, 1);
    rb_define_module_function(m, "num2dbl", num2dbl, 1);
    rb_define_module_function(m, "zerodiv", zerodiv, 0);
    rb_define_module_function(m, "out_of_int", out_of_int, 1);
    rb_define_module_function(m, "cmperr", cmperr, 2);
    rb_define_module_function(m, "complex_api", complex_api, 0);
    rb_define_module_function(m, "id_kinds", id_kinds, 1);
    rb_define_module_function(m, "attrset", attrset, 1);
    rb_define_module_function(m, "symname_p", symname_p, 1);
    rb_define_module_function(m, "enc_symname_p", enc_symname_p, 1);
    rb_define_module_function(m, "all_symbols_has", all_symbols_has, 1);
    rb_define_module_function(m, "name_error", name_error, 1);
}
