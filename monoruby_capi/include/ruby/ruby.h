/*
 * ruby/ruby.h — monoruby's CRuby-compatible C API.
 *
 * A C extension written for CRuby builds against this header instead of
 * CRuby's and links `monoruby_capi/src/capi.c` (the layer that implements
 * the `rb_*` functions over monoruby's extension ABI, monoruby_ext.h).
 * The result loads with a plain `require` from $LOAD_PATH.
 *
 * This is *API* compatibility, not ABI compatibility: a VALUE carries
 * monoruby's bit layout (Qnil is 0x04, Qfalse 0x14, Qundef 0), so a
 * library compiled against CRuby's headers cannot be loaded and has to
 * be rebuilt. The object layout macros (RSTRING_PTR, RARRAY_LEN, ...)
 * are functions here. What is not provided yet is listed in
 * doc/c_api.md; using it is a compile or link error, never a silent
 * misbehaviour.
 *
 * Differences an extension can observe:
 *   - Qfalse is not 0. Test with RTEST / NIL_P, never `if (v)`.
 *   - Every VALUE the extension receives during a method call stays
 *     alive until that call returns (the collector is precise, so this
 *     stands in for CRuby's conservative stack scan). A VALUE kept in a
 *     C global beyond the call needs rb_gc_register_address /
 *     rb_global_variable / rb_gc_register_mark_object, as in CRuby.
 *   - rb_raise and friends longjmp, as in CRuby, but only across the
 *     extension's own C frames: the layer catches the jump where Ruby
 *     called the extension and hands the exception back to the
 *     interpreter.
 */
#ifndef MONORUBY_CAPI_RUBY_RUBY_H
#define MONORUBY_CAPI_RUBY_RUBY_H 1

#include <limits.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
/* CRuby's ruby.h brings in <stdio.h> and <inttypes.h>; extension sources
 * and mkmf's own conftests (which call printf without including it) rely
 * on that. */
#include <stdio.h>
#include <inttypes.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>

#include "ruby/version.h"

#ifdef __cplusplus
extern "C" {
#endif

#define MONORUBY_CAPI 1

#include "ruby/config.h"

/* ---- VALUE and ID ----------------------------------------------------- */

typedef uintptr_t VALUE;
typedef uintptr_t ID;
typedef intptr_t SIGNED_VALUE;
typedef uintptr_t st_data_t;
typedef st_data_t st_index_t;

#define PRI_VALUE_PREFIX "l"
#define RUBY_PRI_VALUE_MARK "\v"
#define PRIdVALUE PRI_VALUE_PREFIX "d"
#define PRIiVALUE PRI_VALUE_PREFIX "i"
#define PRIuVALUE PRI_VALUE_PREFIX "u"
#define PRIxVALUE PRI_VALUE_PREFIX "x"
#define PRIsVALUE PRI_VALUE_PREFIX "i" RUBY_PRI_VALUE_MARK

/* monoruby's immediates (value.rs). */
#define RUBY_Qfalse ((VALUE)0x14)
#define RUBY_Qtrue ((VALUE)0x1c)
#define RUBY_Qnil ((VALUE)0x04)
#define RUBY_Qundef ((VALUE)0)
#define Qfalse RUBY_Qfalse
#define Qtrue RUBY_Qtrue
#define Qnil RUBY_Qnil
#define Qundef RUBY_Qundef

#define RUBY_FIXNUM_FLAG 0x01
#define FIXNUM_FLAG RUBY_FIXNUM_FLAG
#define RUBY_FLONUM_MASK 0x03
#define RUBY_FLONUM_FLAG 0x02
#define RUBY_SYMBOL_FLAG 0x0c

#define RB_TEST(v) (((VALUE)(v) != Qfalse) && ((VALUE)(v) != Qnil))
#define RTEST(v) RB_TEST(v)
#define RB_NIL_P(v) ((VALUE)(v) == Qnil)
#define NIL_P(v) RB_NIL_P(v)
#define RB_UNDEF_P(v) ((VALUE)(v) == Qundef)
#define UNDEF_P(v) RB_UNDEF_P(v)
#define RB_NIL_OR_UNDEF_P(v) (NIL_P(v) || UNDEF_P(v))

#define RB_FIXNUM_P(v) ((((SIGNED_VALUE)(v)) & RUBY_FIXNUM_FLAG) != 0)
#define FIXNUM_P(v) RB_FIXNUM_P(v)
#define RB_FLONUM_P(v) ((((VALUE)(v)) & RUBY_FLONUM_MASK) == RUBY_FLONUM_FLAG)
#define FLONUM_P(v) RB_FLONUM_P(v)
#define RB_STATIC_SYM_P(v) ((((VALUE)(v)) & 0xff) == RUBY_SYMBOL_FLAG)
#define STATIC_SYM_P(v) RB_STATIC_SYM_P(v)
#define RB_DYNAMIC_SYM_P(v) 0
#define DYNAMIC_SYM_P(v) 0
#define RB_SYMBOL_P(v) RB_STATIC_SYM_P(v)
#define SYMBOL_P(v) RB_SYMBOL_P(v)
/* Not a pointer to an object: an immediate, or Qundef. */
#define RB_SPECIAL_CONST_P(v) (((((VALUE)(v)) & 7) != 0) || ((VALUE)(v) == 0))
#define SPECIAL_CONST_P(v) RB_SPECIAL_CONST_P(v)
/* CRuby's IMMEDIATE_P is false for nil and false. */
#define RB_IMMEDIATE_P(v) \
    (((((VALUE)(v)) & 7) != 0) && ((VALUE)(v) != Qnil) && ((VALUE)(v) != Qfalse))
#define IMMEDIATE_P(v) RB_IMMEDIATE_P(v)

#define RUBY_FIXNUM_MAX (LONG_MAX >> 1)
#define RUBY_FIXNUM_MIN (LONG_MIN >> 1)
#define FIXNUM_MAX RUBY_FIXNUM_MAX
#define FIXNUM_MIN RUBY_FIXNUM_MIN
#define RB_POSFIXABLE(f) ((f) < FIXNUM_MAX + 1)
#define RB_NEGFIXABLE(f) ((f) >= FIXNUM_MIN)
#define RB_FIXABLE(f) (RB_POSFIXABLE(f) && RB_NEGFIXABLE(f))
#define POSFIXABLE(f) RB_POSFIXABLE(f)
#define NEGFIXABLE(f) RB_NEGFIXABLE(f)
#define FIXABLE(f) RB_FIXABLE(f)

#define RB_INT2FIX(i) ((VALUE)((((VALUE)(long)(i)) << 1) | RUBY_FIXNUM_FLAG))
#define INT2FIX(i) RB_INT2FIX(i)
#define LONG2FIX(i) RB_INT2FIX(i)
#define rb_fix_new(i) RB_INT2FIX(i)
#define RB_FIX2LONG(x) ((long)(((SIGNED_VALUE)(x)) >> 1))
#define FIX2LONG(x) RB_FIX2LONG(x)
#define FIX2ULONG(x) ((unsigned long)RB_FIX2LONG(x))

#define RB_ID2SYM(id) ((VALUE)((((VALUE)(id)) << 32) | RUBY_SYMBOL_FLAG))
#define RB_SYM2ID(s) ((ID)(((VALUE)(s)) >> 32))
#define ID2SYM(id) RB_ID2SYM(id)
#define SYM2ID(s) RB_SYM2ID(s)

/* ---- types ------------------------------------------------------------ */

enum ruby_value_type {
    RUBY_T_NONE = 0x00,
    RUBY_T_OBJECT = 0x01,
    RUBY_T_CLASS = 0x02,
    RUBY_T_MODULE = 0x03,
    RUBY_T_FLOAT = 0x04,
    RUBY_T_STRING = 0x05,
    RUBY_T_REGEXP = 0x06,
    RUBY_T_ARRAY = 0x07,
    RUBY_T_HASH = 0x08,
    RUBY_T_STRUCT = 0x09,
    RUBY_T_BIGNUM = 0x0a,
    RUBY_T_FILE = 0x0b,
    RUBY_T_DATA = 0x0c,
    RUBY_T_MATCH = 0x0d,
    RUBY_T_COMPLEX = 0x0e,
    RUBY_T_RATIONAL = 0x0f,
    RUBY_T_NIL = 0x11,
    RUBY_T_TRUE = 0x12,
    RUBY_T_FALSE = 0x13,
    RUBY_T_SYMBOL = 0x14,
    RUBY_T_FIXNUM = 0x15,
    RUBY_T_UNDEF = 0x16,
    RUBY_T_IMEMO = 0x1a,
    RUBY_T_NODE = 0x1b,
    RUBY_T_ICLASS = 0x1c,
    RUBY_T_ZOMBIE = 0x1d,
    RUBY_T_MOVED = 0x1e,
    RUBY_T_MASK = 0x1f
};

#define T_NONE RUBY_T_NONE
#define T_OBJECT RUBY_T_OBJECT
#define T_CLASS RUBY_T_CLASS
#define T_MODULE RUBY_T_MODULE
#define T_FLOAT RUBY_T_FLOAT
#define T_STRING RUBY_T_STRING
#define T_REGEXP RUBY_T_REGEXP
#define T_ARRAY RUBY_T_ARRAY
#define T_HASH RUBY_T_HASH
#define T_STRUCT RUBY_T_STRUCT
#define T_BIGNUM RUBY_T_BIGNUM
#define T_FILE RUBY_T_FILE
#define T_DATA RUBY_T_DATA
#define T_MATCH RUBY_T_MATCH
#define T_COMPLEX RUBY_T_COMPLEX
#define T_RATIONAL RUBY_T_RATIONAL
#define T_NIL RUBY_T_NIL
#define T_TRUE RUBY_T_TRUE
#define T_FALSE RUBY_T_FALSE
#define T_SYMBOL RUBY_T_SYMBOL
#define T_FIXNUM RUBY_T_FIXNUM
#define T_UNDEF RUBY_T_UNDEF
#define T_MASK RUBY_T_MASK

/* The type of a heap object (a VALUE that is not SPECIAL_CONST_P). */
int rb_monoruby_heap_type(VALUE obj);

static inline enum ruby_value_type
rb_type(VALUE obj)
{
    if (obj == Qnil) return RUBY_T_NIL;
    if (obj == Qtrue) return RUBY_T_TRUE;
    if (obj == Qfalse) return RUBY_T_FALSE;
    if (obj == Qundef) return RUBY_T_UNDEF;
    if (RB_FIXNUM_P(obj)) return RUBY_T_FIXNUM;
    if (RB_FLONUM_P(obj)) return RUBY_T_FLOAT;
    if (RB_STATIC_SYM_P(obj)) return RUBY_T_SYMBOL;
    return (enum ruby_value_type)rb_monoruby_heap_type(obj);
}

#define TYPE(x) ((int)rb_type((VALUE)(x)))
#define RB_TYPE_P(obj, t) (rb_type((VALUE)(obj)) == (t))
#define BUILTIN_TYPE(x) TYPE(x)
#define RB_BUILTIN_TYPE(x) TYPE(x)
#define RB_FLOAT_TYPE_P(obj) (RB_FLONUM_P(obj) || RB_TYPE_P(obj, T_FLOAT))
#define RB_INTEGER_TYPE_P(obj) (RB_FIXNUM_P(obj) || RB_TYPE_P(obj, T_BIGNUM))
#define RB_BIGNUM_TYPE_P(obj) RB_TYPE_P(obj, T_BIGNUM)
#define RB_SYMBOL_TYPE_P(obj) RB_SYMBOL_P(obj)

void rb_check_type(VALUE v, int t);
#define Check_Type(v, t) rb_check_type((VALUE)(v), (t))

/* ---- memory ----------------------------------------------------------- */

void *ruby_xmalloc(size_t size);
void *ruby_xmalloc2(size_t n, size_t size);
void *ruby_xcalloc(size_t n, size_t size);
void *ruby_xrealloc(void *ptr, size_t size);
void *ruby_xrealloc2(void *ptr, size_t n, size_t size);
void ruby_xfree(void *ptr);
#define xmalloc ruby_xmalloc
#define xmalloc2 ruby_xmalloc2
#define xcalloc ruby_xcalloc
#define xrealloc ruby_xrealloc
#define xrealloc2 ruby_xrealloc2
#define xfree ruby_xfree

#define RB_ALLOC_N(type, n) ((type *)ruby_xmalloc2((n), sizeof(type)))
#define RB_ALLOC(type) ((type *)ruby_xmalloc(sizeof(type)))
#define RB_ZALLOC_N(type, n) ((type *)ruby_xcalloc((n), sizeof(type)))
#define RB_ZALLOC(type) (RB_ZALLOC_N(type, 1))
#define RB_REALLOC_N(var, type, n) ((var) = (type *)ruby_xrealloc2((void *)(var), (n), sizeof(type)))
#define ALLOC_N(type, n) RB_ALLOC_N(type, n)
#define ALLOC(type) RB_ALLOC(type)
#define ZALLOC_N(type, n) RB_ZALLOC_N(type, n)
#define ZALLOC(type) RB_ZALLOC(type)
#define REALLOC_N(var, type, n) RB_REALLOC_N(var, type, n)
#define ALLOCA_N(type, n) ((type *)__builtin_alloca(sizeof(type) * (n)))
#define MEMZERO(p, type, n) memset((p), 0, sizeof(type) * (size_t)(n))
#define MEMCPY(p1, p2, type, n) memcpy((p1), (p2), sizeof(type) * (size_t)(n))
#define MEMMOVE(p1, p2, type, n) memmove((p1), (p2), sizeof(type) * (size_t)(n))
#define MEMCMP(p1, p2, type, n) memcmp((p1), (p2), sizeof(type) * (size_t)(n))
NORETURN(void rb_memerror(void));

/* ---- GC --------------------------------------------------------------- */

void rb_gc_register_address(VALUE *addr);
void rb_gc_unregister_address(VALUE *addr);
void rb_global_variable(VALUE *addr);
void rb_gc_register_mark_object(VALUE obj);
void rb_gc_mark(VALUE obj);
void rb_gc_mark_maybe(VALUE obj);
void rb_gc_mark_movable(VALUE obj);
VALUE rb_gc_location(VALUE obj);
void rb_gc(void);
VALUE rb_gc_start(void);
VALUE rb_gc_enable(void);
VALUE rb_gc_disable(void);
void rb_gc_writebarrier(VALUE a, VALUE b);
void rb_gc_writebarrier_unprotect(VALUE obj);
/* No generational collector: writes need no barrier. */
#define RB_OBJ_WRITE(a, slot, b) (*(slot) = (b))
#define RB_OBJ_WRITTEN(a, oldv, b) ((void)0)
#define RB_GC_GUARD(v) (*(volatile VALUE *)&(v))

/* ---- classes and methods ---------------------------------------------- */

#ifdef __cplusplus
#define ANYARGS ...
#else
#define ANYARGS
#endif
#define RUBY_METHOD_FUNC(func) ((VALUE (*)(ANYARGS))(func))
typedef VALUE rb_alloc_func_t(VALUE klass);

VALUE rb_define_class(const char *name, VALUE super);
VALUE rb_define_class_under(VALUE outer, const char *name, VALUE super);
VALUE rb_define_module(const char *name);
VALUE rb_define_module_under(VALUE outer, const char *name);
void rb_define_method(VALUE klass, const char *name, VALUE (*func)(ANYARGS), int argc);
void rb_define_private_method(VALUE klass, const char *name, VALUE (*func)(ANYARGS), int argc);
void rb_define_protected_method(VALUE klass, const char *name, VALUE (*func)(ANYARGS), int argc);
void rb_define_singleton_method(VALUE obj, const char *name, VALUE (*func)(ANYARGS), int argc);
void rb_define_module_function(VALUE mod, const char *name, VALUE (*func)(ANYARGS), int argc);
void rb_define_global_function(const char *name, VALUE (*func)(ANYARGS), int argc);
void rb_define_method_id(VALUE klass, ID mid, VALUE (*func)(ANYARGS), int argc);
/* Accept any function pointer, as CRuby's own macros do. */
#define rb_define_method(klass, name, func, argc) \
    rb_define_method((klass), (name), RUBY_METHOD_FUNC(func), (argc))
#define rb_define_private_method(klass, name, func, argc) \
    rb_define_private_method((klass), (name), RUBY_METHOD_FUNC(func), (argc))
#define rb_define_protected_method(klass, name, func, argc) \
    rb_define_protected_method((klass), (name), RUBY_METHOD_FUNC(func), (argc))
#define rb_define_singleton_method(obj, name, func, argc) \
    rb_define_singleton_method((obj), (name), RUBY_METHOD_FUNC(func), (argc))
#define rb_define_module_function(mod, name, func, argc) \
    rb_define_module_function((mod), (name), RUBY_METHOD_FUNC(func), (argc))
#define rb_define_global_function(name, func, argc) \
    rb_define_global_function((name), RUBY_METHOD_FUNC(func), (argc))
#define rb_define_method_id(klass, mid, func, argc) \
    rb_define_method_id((klass), (mid), RUBY_METHOD_FUNC(func), (argc))

void rb_define_alias(VALUE klass, const char *name1, const char *name2);
void rb_alias(VALUE klass, ID name1, ID name2);
void rb_undef_method(VALUE klass, const char *name);
void rb_undef(VALUE klass, ID id);
void rb_define_attr(VALUE klass, const char *name, int read, int write);
void rb_define_const(VALUE klass, const char *name, VALUE val);
void rb_define_global_const(const char *name, VALUE val);
VALUE rb_const_get(VALUE klass, ID id);
VALUE rb_const_get_at(VALUE klass, ID id);
int rb_const_defined(VALUE klass, ID id);
int rb_const_defined_at(VALUE klass, ID id);
void rb_const_set(VALUE klass, ID id, VALUE val);
VALUE rb_path2class(const char *path);
VALUE rb_path_to_class(VALUE path);
void rb_include_module(VALUE klass, VALUE module);
void rb_extend_object(VALUE obj, VALUE module);
void rb_prepend_module(VALUE klass, VALUE module);
VALUE rb_class_new_instance(int argc, const VALUE *argv, VALUE klass);
VALUE rb_obj_alloc(VALUE klass);
VALUE rb_class_name(VALUE klass);
VALUE rb_class_path(VALUE klass);
VALUE rb_mod_name(VALUE mod);
const char *rb_class2name(VALUE klass);
const char *rb_obj_classname(VALUE obj);
VALUE rb_class_superclass(VALUE klass);
VALUE rb_class_real(VALUE klass);
VALUE rb_singleton_class(VALUE obj);
VALUE rb_obj_class(VALUE obj);
VALUE rb_class_of(VALUE obj);
#define CLASS_OF(v) rb_class_of((VALUE)(v))
#define RBASIC_CLASS(v) rb_class_of((VALUE)(v))
VALUE rb_obj_is_kind_of(VALUE obj, VALUE klass);
VALUE rb_obj_is_instance_of(VALUE obj, VALUE klass);
VALUE rb_class_inherited_p(VALUE mod, VALUE arg);
int rb_respond_to(VALUE obj, ID id);
int rb_obj_respond_to(VALUE obj, ID id, int priv);

/* ---- objects ---------------------------------------------------------- */

VALUE rb_obj_freeze(VALUE obj);
VALUE rb_obj_frozen_p(VALUE obj);
void rb_check_frozen(VALUE obj);
#define rb_check_frozen_inline(obj) rb_check_frozen(obj)
#define OBJ_FROZEN(obj) RTEST(rb_obj_frozen_p((VALUE)(obj)))
#define RB_OBJ_FROZEN(obj) OBJ_FROZEN(obj)
#define OBJ_FREEZE(obj) ((void)rb_obj_freeze((VALUE)(obj)))
#define RB_OBJ_FREEZE(obj) OBJ_FREEZE(obj)
VALUE rb_obj_dup(VALUE obj);
VALUE rb_obj_clone(VALUE obj);
VALUE rb_obj_id(VALUE obj);
VALUE rb_obj_hide(VALUE obj);
VALUE rb_obj_reveal(VALUE obj, VALUE klass);
VALUE rb_equal(VALUE a, VALUE b);
int rb_eql(VALUE a, VALUE b);
VALUE rb_hash(VALUE obj);
VALUE rb_inspect(VALUE obj);
VALUE rb_obj_as_string(VALUE obj);
VALUE rb_any_to_s(VALUE obj);
void rb_p(VALUE obj);
VALUE rb_eval_string(const char *str);
VALUE rb_eval_string_protect(const char *str, int *state);
VALUE rb_require(const char *feature);
VALUE rb_f_require(VALUE self, VALUE feature);
VALUE rb_obj_instance_variables(VALUE obj);
VALUE rb_ivar_get(VALUE obj, ID id);
VALUE rb_ivar_set(VALUE obj, ID id, VALUE val);
VALUE rb_ivar_defined(VALUE obj, ID id);
VALUE rb_iv_get(VALUE obj, const char *name);
VALUE rb_iv_set(VALUE obj, const char *name, VALUE val);
VALUE rb_attr_get(VALUE obj, ID id);
VALUE rb_cvar_get(VALUE klass, ID id);
void rb_cvar_set(VALUE klass, ID id, VALUE val);
VALUE rb_cv_get(VALUE klass, const char *name);
void rb_cv_set(VALUE klass, const char *name, VALUE val);
VALUE rb_gv_get(const char *name);
VALUE rb_gv_set(const char *name, VALUE val);
VALUE rb_Integer(VALUE v);
VALUE rb_Float(VALUE v);
VALUE rb_String(VALUE v);
VALUE rb_Array(VALUE v);
VALUE rb_Hash(VALUE v);
VALUE rb_to_int(VALUE v);
VALUE rb_to_float(VALUE v);
VALUE rb_check_to_integer(VALUE v, const char *method);
VALUE rb_convert_type(VALUE v, int type, const char *tname, const char *method);
VALUE rb_check_convert_type(VALUE v, int type, const char *tname, const char *method);
int rb_cmpint(VALUE val, VALUE a, VALUE b);
VALUE rb_obj_is_proc(VALUE obj);
VALUE rb_obj_is_method(VALUE obj);

/* ---- calling Ruby ----------------------------------------------------- */

VALUE rb_funcall(VALUE recv, ID mid, int n, ...);
VALUE rb_funcallv(VALUE recv, ID mid, int argc, const VALUE *argv);
VALUE rb_funcallv_public(VALUE recv, ID mid, int argc, const VALUE *argv);
VALUE rb_funcall2(VALUE recv, ID mid, int argc, const VALUE *argv);
VALUE rb_funcall3(VALUE recv, ID mid, int argc, const VALUE *argv);
VALUE rb_funcall_passing_block(VALUE recv, ID mid, int argc, const VALUE *argv);
VALUE rb_funcall_with_block(VALUE recv, ID mid, int argc, const VALUE *argv, VALUE procval);
VALUE rb_apply(VALUE recv, ID mid, VALUE args);
VALUE rb_yield(VALUE val);
VALUE rb_yield_values(int n, ...);
VALUE rb_yield_values2(int argc, const VALUE *argv);
VALUE rb_yield_splat(VALUE args);
int rb_block_given_p(void);
void rb_need_block(void);
VALUE rb_block_proc(void);
VALUE rb_block_lambda(void);
VALUE rb_proc_call(VALUE proc, VALUE args);
VALUE rb_proc_call_with_block(VALUE proc, int argc, const VALUE *argv, VALUE passed_proc);
int rb_keyword_given_p(void);
#define RB_BLOCK_CALL_FUNC_ARGLIST(yielded_arg, callback_arg) \
    VALUE yielded_arg, VALUE callback_arg, int argc, const VALUE *argv, VALUE blockarg

/* ---- exceptions ------------------------------------------------------- */

#define TAG_RETURN 0x1
#define TAG_BREAK 0x2
#define TAG_NEXT 0x3
#define TAG_RETRY 0x4
#define TAG_REDO 0x5
#define TAG_RAISE 0x6
#define TAG_THROW 0x7
#define TAG_FATAL 0x8

NORETURN(void rb_raise(VALUE exc, const char *fmt, ...));
NORETURN(void rb_exc_raise(VALUE exc));
NORETURN(void rb_fatal(const char *fmt, ...));
NORETURN(void rb_bug(const char *fmt, ...));
NORETURN(void rb_sys_fail(const char *msg));
NORETURN(void rb_sys_fail_str(VALUE msg));
NORETURN(void rb_syserr_fail(int err, const char *msg));
NORETURN(void rb_syserr_fail_str(int err, VALUE msg));
NORETURN(void rb_notimplement(void));
NORETURN(void rb_error_arity(int argc, int min, int max));
NORETURN(void rb_jump_tag(int state));
NORETURN(void rb_iter_break(void));
NORETURN(void rb_iter_break_value(VALUE val));
VALUE rb_exc_new(VALUE klass, const char *ptr, long len);
VALUE rb_exc_new_cstr(VALUE klass, const char *ptr);
VALUE rb_exc_new_str(VALUE klass, VALUE str);
#define rb_exc_new2 rb_exc_new_cstr
#define rb_exc_new3 rb_exc_new_str
VALUE rb_syserr_new(int err, const char *msg);
void rb_warn(const char *fmt, ...);
void rb_warning(const char *fmt, ...);
VALUE rb_protect(VALUE (*func)(VALUE), VALUE arg, int *state);
VALUE rb_ensure(VALUE (*b_proc)(VALUE), VALUE data1, VALUE (*e_proc)(VALUE), VALUE data2);
VALUE rb_rescue(VALUE (*b_proc)(VALUE), VALUE data1, VALUE (*r_proc)(VALUE, VALUE), VALUE data2);
VALUE rb_rescue2(VALUE (*b_proc)(VALUE), VALUE data1, VALUE (*r_proc)(VALUE, VALUE), VALUE data2, ...);
VALUE rb_errinfo(void);
void rb_set_errinfo(VALUE err);

#define UNLIMITED_ARGUMENTS (-1)
static inline int
rb_check_arity(int argc, int min, int max)
{
    if (argc < min || (max != UNLIMITED_ARGUMENTS && argc > max))
        rb_error_arity(argc, min, max);
    return argc;
}
#define rb_check_arity rb_check_arity

int rb_scan_args(int argc, const VALUE *argv, const char *fmt, ...);
int rb_get_kwargs(VALUE keyword_hash, const ID *table, int required, int optional, VALUE *values);
VALUE rb_extract_keywords(VALUE *orighash);

/* ---- numbers ---------------------------------------------------------- */

VALUE rb_int2inum(intptr_t n);
VALUE rb_uint2inum(uintptr_t n);
VALUE rb_ll2inum(long long n);
VALUE rb_ull2inum(unsigned long long n);
VALUE rb_int2big(intptr_t n);
VALUE rb_uint2big(uintptr_t n);
long rb_num2long(VALUE v);
unsigned long rb_num2ulong(VALUE v);
long rb_num2int(VALUE v);
unsigned long rb_num2uint(VALUE v);
long rb_fix2int(VALUE v);
unsigned long rb_fix2uint(VALUE v);
short rb_num2short(VALUE v);
unsigned short rb_num2ushort(VALUE v);
long long rb_num2ll(VALUE v);
unsigned long long rb_num2ull(VALUE v);
double rb_num2dbl(VALUE v);
VALUE rb_float_new(double d);
VALUE rb_float_new_in_heap(double d);
double rb_float_value(VALUE v);
char rb_num2chr(VALUE v);

static inline VALUE
rb_long2num_inline(long v)
{
    return RB_FIXABLE(v) ? RB_INT2FIX(v) : rb_int2inum(v);
}
static inline VALUE
rb_ulong2num_inline(unsigned long v)
{
    return RB_POSFIXABLE(v) ? RB_INT2FIX(v) : rb_uint2inum(v);
}
static inline long
rb_num2long_inline(VALUE v)
{
    return RB_FIXNUM_P(v) ? RB_FIX2LONG(v) : rb_num2long(v);
}

#define LONG2NUM(v) rb_long2num_inline((long)(v))
#define ULONG2NUM(v) rb_ulong2num_inline((unsigned long)(v))
#define INT2NUM(v) RB_INT2FIX((int)(v))
#define UINT2NUM(v) RB_INT2FIX((unsigned int)(v))
#define LL2NUM(v) rb_ll2inum(v)
#define ULL2NUM(v) rb_ull2inum(v)
#define SIZET2NUM(v) ULONG2NUM(v)
#define SSIZET2NUM(v) LONG2NUM(v)
#define OFFT2NUM(v) LL2NUM(v)
#define CHR2FIX(x) RB_INT2FIX((long)((x) & 0xff))
#define NUM2LONG(x) rb_num2long_inline((VALUE)(x))
#define NUM2ULONG(x) rb_num2ulong((VALUE)(x))
#define NUM2INT(x) ((int)rb_num2int((VALUE)(x)))
#define NUM2UINT(x) ((unsigned int)rb_num2uint((VALUE)(x)))
#define FIX2INT(x) ((int)rb_fix2int((VALUE)(x)))
#define FIX2UINT(x) ((unsigned int)rb_fix2uint((VALUE)(x)))
#define NUM2SHORT(x) rb_num2short((VALUE)(x))
#define NUM2USHORT(x) rb_num2ushort((VALUE)(x))
#define NUM2LL(x) rb_num2ll((VALUE)(x))
#define NUM2ULL(x) rb_num2ull((VALUE)(x))
#define NUM2SIZET(x) ((size_t)NUM2ULONG(x))
#define NUM2SSIZET(x) ((ssize_t)NUM2LONG(x))
#define NUM2OFFT(x) ((off_t)NUM2LL(x))
#define NUM2DBL(x) rb_num2dbl((VALUE)(x))
#define NUM2CHR(x) rb_num2chr((VALUE)(x))
#define DBL2NUM(d) rb_float_new(d)
#define RFLOAT_VALUE(v) rb_float_value((VALUE)(v))
#define rb_int_new(v) rb_int2inum(v)
#define rb_uint_new(v) rb_uint2inum(v)

/* ---- strings ---------------------------------------------------------- */

VALUE rb_str_new(const char *ptr, long len);
VALUE rb_str_new_cstr(const char *ptr);
VALUE rb_str_new_static(const char *ptr, long len);
VALUE rb_str_new_frozen(VALUE str);
VALUE rb_str_buf_new(long capa);
VALUE rb_usascii_str_new(const char *ptr, long len);
VALUE rb_usascii_str_new_cstr(const char *ptr);
VALUE rb_utf8_str_new(const char *ptr, long len);
VALUE rb_utf8_str_new_cstr(const char *ptr);
VALUE rb_external_str_new(const char *ptr, long len);
VALUE rb_external_str_new_cstr(const char *ptr);
VALUE rb_locale_str_new(const char *ptr, long len);
VALUE rb_locale_str_new_cstr(const char *ptr);
#define rb_str_new2 rb_str_new_cstr
#define rb_str_new3 rb_str_new_shared
#define rb_str_new4 rb_str_new_frozen
#define rb_str_new5 rb_str_new_with_class
#define rb_str_buf_new2 rb_str_buf_new_cstr
#define rb_usascii_str_new2 rb_usascii_str_new_cstr
#define rb_str_new_lit(s) rb_str_new_static((s), (long)(sizeof(s) - 1))
#define rb_str_new_literal(s) rb_str_new_lit(s)
#define rb_usascii_str_new_lit(s) rb_usascii_str_new((s), (long)(sizeof(s) - 1))
#define rb_utf8_str_new_lit(s) rb_utf8_str_new((s), (long)(sizeof(s) - 1))
VALUE rb_str_new_shared(VALUE str);
VALUE rb_str_new_with_class(VALUE obj, const char *ptr, long len);
VALUE rb_str_buf_new_cstr(const char *ptr);
VALUE rb_str_cat(VALUE str, const char *ptr, long len);
VALUE rb_str_cat_cstr(VALUE str, const char *ptr);
#define rb_str_cat2 rb_str_cat_cstr
#define rb_str_buf_cat rb_str_cat
#define rb_str_buf_cat2 rb_str_cat_cstr
#define rb_str_buf_cat_ascii rb_str_cat_cstr
#define rb_str_cat_lit(str, s) rb_str_cat((str), (s), (long)(sizeof(s) - 1))
VALUE rb_str_buf_append(VALUE str, VALUE str2);
VALUE rb_str_append(VALUE str, VALUE str2);
VALUE rb_str_concat(VALUE str, VALUE obj);
VALUE rb_str_plus(VALUE a, VALUE b);
VALUE rb_str_times(VALUE str, VALUE times);
VALUE rb_str_dup(VALUE str);
VALUE rb_str_resurrect(VALUE str);
VALUE rb_str_freeze(VALUE str);
VALUE rb_str_length(VALUE str);
long rb_str_strlen(VALUE str);
VALUE rb_str_substr(VALUE str, long beg, long len);
VALUE rb_str_subseq(VALUE str, long beg, long len);
VALUE rb_str_equal(VALUE a, VALUE b);
int rb_str_cmp(VALUE a, VALUE b);
st_index_t rb_str_hash(VALUE str);
VALUE rb_str_inspect(VALUE str);
VALUE rb_str_dump(VALUE str);
VALUE rb_str_split(VALUE str, const char *sep);
VALUE rb_str_intern(VALUE str);
VALUE rb_str_to_str(VALUE obj);
VALUE rb_string_value(volatile VALUE *ptr);
char *rb_string_value_ptr(volatile VALUE *ptr);
char *rb_string_value_cstr(volatile VALUE *ptr);
VALUE rb_check_string_type(VALUE obj);
VALUE rb_str_resize(VALUE str, long len);
void rb_str_set_len(VALUE str, long len);
void rb_str_modify(VALUE str);
void rb_str_modify_expand(VALUE str, long expand);
VALUE rb_str_to_inum(VALUE str, int base, int badcheck);
VALUE rb_cstr2inum(const char *str, int base);
VALUE rb_str2inum(VALUE str, int base);
double rb_str_to_dbl(VALUE str, int badcheck);
double rb_cstr_to_dbl(const char *p, int badcheck);
VALUE rb_sprintf(const char *fmt, ...);
VALUE rb_vsprintf(const char *fmt, va_list ap);
VALUE rb_str_catf(VALUE str, const char *fmt, ...);
VALUE rb_str_vcatf(VALUE str, const char *fmt, va_list ap);
VALUE rb_str_format(int argc, const VALUE *argv, VALUE fmt);

#define StringValue(v) rb_string_value(&(v))
#define StringValuePtr(v) rb_string_value_ptr(&(v))
#define StringValueCStr(v) rb_string_value_cstr(&(v))
#define SafeStringValue(v) StringValue(v)
#define ExportStringValue(v) StringValue(v)

char *rb_monoruby_rstring_ptr(VALUE str);
long rb_monoruby_rstring_len(VALUE str);
#define RSTRING_PTR(str) rb_monoruby_rstring_ptr((VALUE)(str))
#define RSTRING_LEN(str) rb_monoruby_rstring_len((VALUE)(str))
#define RSTRING_END(str) (RSTRING_PTR(str) + RSTRING_LEN(str))
#define RSTRING_LENINT(str) ((int)RSTRING_LEN(str))
#define RSTRING_GETMEM(str, ptrvar, lenvar) \
    ((ptrvar) = RSTRING_PTR(str), (lenvar) = RSTRING_LEN(str))

/* ---- symbols and IDs -------------------------------------------------- */

ID rb_intern(const char *name);
ID rb_intern2(const char *name, long len);
ID rb_intern_str(VALUE str);
const char *rb_id2name(ID id);
VALUE rb_id2sym(ID id);
ID rb_sym2id(VALUE sym);
VALUE rb_id2str(ID id);
VALUE rb_sym2str(VALUE sym);
ID rb_to_id(VALUE name);
VALUE rb_to_symbol(VALUE name);
ID rb_check_id(volatile VALUE *namep);
VALUE rb_sym_to_s(VALUE sym);
#define rb_intern_const(str) rb_intern(str)
#define CONST_ID(var, str) \
    do { static ID rbimpl_id; if (!rbimpl_id) rbimpl_id = rb_intern(str); (var) = rbimpl_id; } while (0)

/* ---- arrays ----------------------------------------------------------- */

VALUE rb_ary_new(void);
VALUE rb_ary_new_capa(long capa);
VALUE rb_ary_new_from_args(long n, ...);
VALUE rb_ary_new_from_values(long n, const VALUE *elts);
VALUE rb_ary_tmp_new(long capa);
#define rb_ary_new2 rb_ary_new_capa
#define rb_ary_new3 rb_ary_new_from_args
#define rb_ary_new4 rb_ary_new_from_values
#define rb_assoc_new(car, cdr) rb_ary_new_from_args(2, (car), (cdr))
VALUE rb_ary_push(VALUE ary, VALUE item);
VALUE rb_ary_pop(VALUE ary);
VALUE rb_ary_shift(VALUE ary);
VALUE rb_ary_unshift(VALUE ary, VALUE item);
VALUE rb_ary_cat(VALUE ary, const VALUE *train, long len);
void rb_ary_store(VALUE ary, long idx, VALUE val);
VALUE rb_ary_entry(VALUE ary, long offset);
VALUE rb_ary_aref(int argc, const VALUE *argv, VALUE ary);
VALUE rb_ary_dup(VALUE ary);
VALUE rb_ary_clear(VALUE ary);
VALUE rb_ary_concat(VALUE x, VALUE y);
VALUE rb_ary_plus(VALUE x, VALUE y);
VALUE rb_ary_join(VALUE ary, VALUE sep);
VALUE rb_ary_reverse(VALUE ary);
VALUE rb_ary_sort(VALUE ary);
VALUE rb_ary_sort_bang(VALUE ary);
VALUE rb_ary_includes(VALUE ary, VALUE item);
VALUE rb_ary_delete(VALUE ary, VALUE item);
VALUE rb_ary_delete_at(VALUE ary, long pos);
VALUE rb_ary_subseq(VALUE ary, long beg, long len);
VALUE rb_ary_to_ary(VALUE obj);
VALUE rb_ary_to_s(VALUE ary);
VALUE rb_ary_freeze(VALUE ary);
VALUE rb_ary_resize(VALUE ary, long len);
VALUE rb_ary_assoc(VALUE ary, VALUE key);
VALUE rb_check_array_type(VALUE obj);
long rb_array_len(VALUE ary);
const VALUE *rb_array_const_ptr(VALUE ary);
#define RARRAY_LEN(a) rb_array_len((VALUE)(a))
#define RARRAY_LENINT(a) ((int)RARRAY_LEN(a))
#define RARRAY_CONST_PTR(a) rb_array_const_ptr((VALUE)(a))
/* Read-only in monoruby: write elements with rb_ary_store / RARRAY_ASET. */
#define RARRAY_PTR(a) ((VALUE *)RARRAY_CONST_PTR(a))
#define RARRAY_AREF(a, i) rb_ary_entry((VALUE)(a), (long)(i))
#define RARRAY_ASET(a, i, v) rb_ary_store((VALUE)(a), (long)(i), (v))
#define RARRAY_PTR_USE(ary, ptr_name, expr) \
    do { const VALUE *ptr_name = RARRAY_CONST_PTR(ary); expr; } while (0)

/* ---- hashes ----------------------------------------------------------- */

enum st_retval { ST_CONTINUE, ST_STOP, ST_DELETE, ST_CHECK, ST_REPLACE };
VALUE rb_hash_new(void);
VALUE rb_hash_new_capa(long capa);
VALUE rb_hash_aset(VALUE hash, VALUE key, VALUE val);
VALUE rb_hash_aref(VALUE hash, VALUE key);
VALUE rb_hash_lookup(VALUE hash, VALUE key);
VALUE rb_hash_lookup2(VALUE hash, VALUE key, VALUE def);
VALUE rb_hash_fetch(VALUE hash, VALUE key);
VALUE rb_hash_delete(VALUE hash, VALUE key);
VALUE rb_hash_clear(VALUE hash);
VALUE rb_hash_size(VALUE hash);
size_t rb_hash_size_num(VALUE hash);
VALUE rb_hash_dup(VALUE hash);
VALUE rb_hash_freeze(VALUE hash);
VALUE rb_hash_keys(VALUE hash);
VALUE rb_hash_values(VALUE hash);
VALUE rb_hash_has_key(VALUE hash, VALUE key);
VALUE rb_hash_set_ifnone(VALUE hash, VALUE ifnone);
VALUE rb_check_hash_type(VALUE obj);
void rb_hash_foreach(VALUE hash, int (*func)(VALUE key, VALUE val, VALUE arg), VALUE arg);
#define rb_hash_foreach(hash, func, arg) \
    rb_hash_foreach((hash), (int (*)(VALUE, VALUE, VALUE))(func), (arg))
#define RHASH_SIZE(h) rb_hash_size_num((VALUE)(h))
#define RHASH_EMPTY_P(h) (RHASH_SIZE(h) == 0)

/* ---- ranges, procs ---------------------------------------------------- */

VALUE rb_range_new(VALUE beg, VALUE end, int exclude_end);
int rb_range_values(VALUE range, VALUE *begp, VALUE *endp, int *exclp);

/* ---- global classes, filled in when the extension is loaded ------------ */

RUBY_EXTERN VALUE rb_mKernel, rb_mComparable, rb_mEnumerable, rb_mErrno,
    rb_mMath, rb_mProcess, rb_mGC;
RUBY_EXTERN VALUE rb_cBasicObject, rb_cObject, rb_cArray, rb_cClass,
    rb_cComplex, rb_cDir, rb_cEncoding, rb_cEnumerator, rb_cFalseClass,
    rb_cFile, rb_cFloat, rb_cHash, rb_cIO, rb_cInteger, rb_cMatch,
    rb_cMethod, rb_cModule, rb_cNilClass, rb_cNumeric, rb_cProc,
    rb_cRandom, rb_cRange, rb_cRational, rb_cRegexp, rb_cSet, rb_cStat,
    rb_cString, rb_cStruct, rb_cSymbol, rb_cThread, rb_cTime,
    rb_cTrueClass, rb_cUnboundMethod, rb_cBinding;
RUBY_EXTERN VALUE rb_eException, rb_eStandardError, rb_eSystemExit,
    rb_eInterrupt, rb_eSignal, rb_eFatal, rb_eArgError, rb_eEOFError,
    rb_eIndexError, rb_eStopIteration, rb_eKeyError, rb_eRangeError,
    rb_eIOError, rb_eRuntimeError, rb_eFrozenError, rb_eSecurityError,
    rb_eSystemCallError, rb_eThreadError, rb_eTypeError, rb_eZeroDivError,
    rb_eNotImpError, rb_eNoMemError, rb_eNoMethodError, rb_eFloatDomainError,
    rb_eLocalJumpError, rb_eSysStackError, rb_eRegexpError, rb_eEncodingError,
    rb_eEncCompatError, rb_eNoMatchingPatternError,
    rb_eNoMatchingPatternKeyError, rb_eScriptError, rb_eNameError,
    rb_eSyntaxError, rb_eLoadError, rb_eMathDomainError;
RUBY_EXTERN VALUE rb_stdin, rb_stdout, rb_stderr;

/* ---- allocators, Data and TypedData ----------------------------------- */

void rb_define_alloc_func(VALUE klass, rb_alloc_func_t *func);
void rb_undef_alloc_func(VALUE klass);
rb_alloc_func_t *rb_get_alloc_func(VALUE klass);
void rb_obj_call_init(VALUE obj, int argc, const VALUE *argv);
void rb_obj_call_init_kw(VALUE obj, int argc, const VALUE *argv, int kw_splat);
VALUE rb_class_new(VALUE super);
VALUE rb_module_new(void);
VALUE rb_call_super(int argc, const VALUE *argv);
VALUE rb_call_super_kw(int argc, const VALUE *argv, int kw_splat);

typedef void (*RUBY_DATA_FUNC)(void *);
#define RUBY_DEFAULT_FREE ((RUBY_DATA_FUNC)-1)
#define RUBY_NEVER_FREE ((RUBY_DATA_FUNC)0)
#define RUBY_UNTYPED_DATA_FUNC(func) func
#define RUBY_UNTYPED_DATA_WARNING 0

struct rb_data_type_struct {
    const char *wrap_struct_name;
    struct {
        RUBY_DATA_FUNC dmark;
        RUBY_DATA_FUNC dfree;
        size_t (*dsize)(const void *);
        RUBY_DATA_FUNC dcompact;
        void *reserved[1];
    } function;
    const struct rb_data_type_struct *parent;
    void *data;
    VALUE flags;
};
typedef struct rb_data_type_struct rb_data_type_t;

#define RUBY_TYPED_DEFAULT_FREE RUBY_DEFAULT_FREE
#define RUBY_TYPED_NEVER_FREE RUBY_NEVER_FREE
/* All of these are accepted and none changes anything: the collector is
 * neither generational nor moving, and every payload is freed at sweep. */
#define RUBY_TYPED_FREE_IMMEDIATELY 1
#define RUBY_TYPED_WB_PROTECTED 0
#define RUBY_TYPED_FROZEN_SHAREABLE 0
#define RUBY_TYPED_EMBEDDABLE 0
#define RUBY_TYPED_PROMOTED1 0

/* The Data / TypedData payload as the extension sees it: `RDATA(obj)->data`
 * and `RTYPEDDATA(obj)->type` are the fields CRuby code reads. The
 * objects themselves have no struct; both are views of the layer's own
 * record behind the native payload. */
struct RData {
    void *data;
    RUBY_DATA_FUNC dmark;
    RUBY_DATA_FUNC dfree;
    const rb_data_type_t *type;
};
struct RTypedData {
    void *data;
    RUBY_DATA_FUNC dmark;
    RUBY_DATA_FUNC dfree;
    const rb_data_type_t *type;
};

VALUE rb_data_object_wrap(VALUE klass, void *datap, RUBY_DATA_FUNC dmark, RUBY_DATA_FUNC dfree);
VALUE rb_data_object_zalloc(VALUE klass, size_t size, RUBY_DATA_FUNC dmark, RUBY_DATA_FUNC dfree);
VALUE rb_data_object_make(VALUE klass, RUBY_DATA_FUNC dmark, RUBY_DATA_FUNC dfree, void **datap, size_t size);
void *rb_data_object_get(VALUE obj);
VALUE rb_data_typed_object_wrap(VALUE klass, void *datap, const rb_data_type_t *type);
VALUE rb_data_typed_object_zalloc(VALUE klass, size_t size, const rb_data_type_t *type);
VALUE rb_data_typed_object_make(VALUE klass, const rb_data_type_t *type, void **datap, size_t size);
void *rb_check_typeddata(VALUE obj, const rb_data_type_t *type);
int rb_typeddata_is_kind_of(VALUE obj, const rb_data_type_t *type);
int rb_typeddata_inherited_p(const rb_data_type_t *child, const rb_data_type_t *parent);
void *rb_monoruby_rdata(VALUE obj);
int rb_monoruby_typeddata_p(VALUE obj);
#define RDATA(obj) ((struct RData *)rb_monoruby_rdata((VALUE)(obj)))
#define RTYPEDDATA(obj) ((struct RTypedData *)rb_monoruby_rdata((VALUE)(obj)))
#define DATA_PTR(obj) (RDATA(obj)->data)
#define RTYPEDDATA_DATA(obj) (RTYPEDDATA(obj)->data)
#define RTYPEDDATA_GET_DATA(obj) RTYPEDDATA_DATA(obj)
#define RTYPEDDATA_TYPE(obj) (RTYPEDDATA(obj)->type)
#define RTYPEDDATA_P(obj) rb_monoruby_typeddata_p((VALUE)(obj))
#define RTYPEDDATA_EMBEDDED_P(obj) 0
#define Data_Wrap_Struct(klass, mark, free, sval) \
    rb_data_object_wrap((klass), (sval), (RUBY_DATA_FUNC)(mark), (RUBY_DATA_FUNC)(free))
#define Data_Make_Struct(klass, type, mark, free, sval) \
    rb_data_object_make((klass), (RUBY_DATA_FUNC)(mark), (RUBY_DATA_FUNC)(free), (void **)&(sval), sizeof(type))
#define Data_Get_Struct(obj, type, sval) ((sval) = (type *)rb_data_object_get((VALUE)(obj)))
#define TypedData_Wrap_Struct(klass, data_type, sval) rb_data_typed_object_wrap((klass), (sval), (data_type))
#define TypedData_Make_Struct(klass, type, data_type, sval) \
    rb_data_typed_object_make((klass), (data_type), (void **)&(sval), sizeof(type))
#define TypedData_Get_Struct(obj, type, data_type, sval) \
    ((sval) = (type *)rb_check_typeddata((VALUE)(obj), (data_type)))

/* ---- blocks from C ----------------------------------------------------- */

typedef VALUE rb_block_call_func(VALUE yielded_arg, VALUE callback_arg, int argc, const VALUE *argv, VALUE blockarg);
typedef rb_block_call_func *rb_block_call_func_t;
VALUE rb_block_call(VALUE obj, ID mid, int argc, const VALUE *argv, rb_block_call_func_t bl_proc, VALUE data2);
VALUE rb_block_call_kw(VALUE obj, ID mid, int argc, const VALUE *argv, rb_block_call_func_t bl_proc, VALUE data2, int kw_splat);
VALUE rb_iterate(VALUE (*it_proc)(VALUE), VALUE data1, rb_block_call_func_t bl_proc, VALUE data2);
VALUE rb_each(VALUE obj);
VALUE rb_proc_new(rb_block_call_func_t func, VALUE callback_arg);
void rb_gc_mark_locations(const VALUE *start, const VALUE *end);

/* ---- Struct ----------------------------------------------------------- */

VALUE rb_struct_define(const char *name, ...);
VALUE rb_struct_define_under(VALUE outer, const char *name, ...);
VALUE rb_struct_new(VALUE klass, ...);
VALUE rb_struct_alloc(VALUE klass, VALUE values);
VALUE rb_struct_initialize(VALUE self, VALUE values);
VALUE rb_struct_aref(VALUE st, VALUE idx);
VALUE rb_struct_aset(VALUE st, VALUE idx, VALUE val);
VALUE rb_struct_getmember(VALUE st, ID id);
VALUE rb_struct_members(VALUE st);
VALUE rb_struct_s_members(VALUE klass);
VALUE rb_struct_size(VALUE st);
#define RSTRUCT_LEN(st) NUM2LONG(rb_struct_size((VALUE)(st)))
#define RSTRUCT_GET(st, i) rb_struct_aref((VALUE)(st), LONG2NUM(i))
#define RSTRUCT_SET(st, i, v) rb_struct_aset((VALUE)(st), LONG2NUM(i), (v))

/* ---- interpreter ------------------------------------------------------ */

const char *rb_monoruby_ruby_version(void);
void ruby_init(void);
int ruby_native_thread_p(void);
int rb_during_gc(void);
VALUE rb_thread_current(void);
void rb_thread_schedule(void);

#ifdef __cplusplus
}
#endif

#endif /* MONORUBY_CAPI_RUBY_RUBY_H */
