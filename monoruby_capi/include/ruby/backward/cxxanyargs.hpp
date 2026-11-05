/*
 * ruby/backward/cxxanyargs.hpp — overloads for C++ extensions that pass
 * function pointers cast to CRuby's ANYARGS types (`VALUE (*)(...)`,
 * `void (*)(...)`, `int (*)(...)`), the way SWIG-generated wrappers do.
 * In C, ANYARGS is an unprototyped parameter that takes any function
 * pointer; in C++ it is a variadic prototype no ordinary function
 * matches, so CRuby's header of this name provides overloads taking
 * that form and casting back, and so does this one. ruby/ruby.h
 * includes it at its end when compiled as C++.
 *
 * The method definers (rb_define_method and the others) need no
 * overload: ruby.h makes them macros that cast the function.
 */
#ifndef MONORUBY_CAPI_RUBY_BACKWARD_CXXANYARGS_HPP
#define MONORUBY_CAPI_RUBY_BACKWARD_CXXANYARGS_HPP 1
#ifdef __cplusplus

#include "ruby/ruby.h"
#include <cstdarg>
#include <cstddef>

extern "C++" {
namespace ruby {
namespace backward {
namespace cxxanyargs {

typedef VALUE type(ANYARGS);
typedef void void_type(ANYARGS);
typedef int int_type(ANYARGS);
typedef VALUE onearg_type(VALUE);

/* ---- hooked global variables ------------------------------------------ */

inline void
rb_define_virtual_variable(const char *q, type *w, void_type *e)
{
    ::rb_define_virtual_variable(q, reinterpret_cast<rb_gvar_getter_t *>(w),
                                 reinterpret_cast<rb_gvar_setter_t *>(e));
}

inline void
rb_define_virtual_variable(const char *q, rb_gvar_getter_t *w, void_type *e)
{
    ::rb_define_virtual_variable(q, w, reinterpret_cast<rb_gvar_setter_t *>(e));
}

inline void
rb_define_virtual_variable(const char *q, type *w, rb_gvar_setter_t *e)
{
    ::rb_define_virtual_variable(q, reinterpret_cast<rb_gvar_getter_t *>(w), e);
}

inline void
rb_define_hooked_variable(const char *q, VALUE *w, type *e, void_type *r)
{
    ::rb_define_hooked_variable(q, w, reinterpret_cast<rb_gvar_getter_t *>(e),
                                reinterpret_cast<rb_gvar_setter_t *>(r));
}

inline void
rb_define_hooked_variable(const char *q, VALUE *w, rb_gvar_getter_t *e, void_type *r)
{
    ::rb_define_hooked_variable(q, w, e, reinterpret_cast<rb_gvar_setter_t *>(r));
}

inline void
rb_define_hooked_variable(const char *q, VALUE *w, type *e, rb_gvar_setter_t *r)
{
    ::rb_define_hooked_variable(q, w, reinterpret_cast<rb_gvar_getter_t *>(e), r);
}

#if __cplusplus >= 201103L
inline void
rb_define_virtual_variable(const char *q, rb_gvar_getter_t *w, std::nullptr_t e)
{
    ::rb_define_virtual_variable(q, w, e);
}

inline void
rb_define_virtual_variable(const char *q, type *w, std::nullptr_t e)
{
    ::rb_define_virtual_variable(q, reinterpret_cast<rb_gvar_getter_t *>(w), e);
}

inline void
rb_define_virtual_variable(const char *q, std::nullptr_t w, rb_gvar_setter_t *e)
{
    ::rb_define_virtual_variable(q, w, e);
}

inline void
rb_define_virtual_variable(const char *q, std::nullptr_t w, void_type *e)
{
    ::rb_define_virtual_variable(q, w, reinterpret_cast<rb_gvar_setter_t *>(e));
}

inline void
rb_define_hooked_variable(const char *q, VALUE *w, rb_gvar_getter_t *e, std::nullptr_t r)
{
    ::rb_define_hooked_variable(q, w, e, r);
}

inline void
rb_define_hooked_variable(const char *q, VALUE *w, type *e, std::nullptr_t r)
{
    ::rb_define_hooked_variable(q, w, reinterpret_cast<rb_gvar_getter_t *>(e), r);
}

inline void
rb_define_hooked_variable(const char *q, VALUE *w, std::nullptr_t e, rb_gvar_setter_t *r)
{
    ::rb_define_hooked_variable(q, w, e, r);
}

inline void
rb_define_hooked_variable(const char *q, VALUE *w, std::nullptr_t e, void_type *r)
{
    ::rb_define_hooked_variable(q, w, e, reinterpret_cast<rb_gvar_setter_t *>(r));
}
#endif

/* ---- exceptions and tag jumps ----------------------------------------- */

inline VALUE
rb_iterate(onearg_type *q, VALUE w, type *e, VALUE r)
{
    return ::rb_iterate(q, w, reinterpret_cast<rb_block_call_func_t>(e), r);
}

inline VALUE
rb_block_call(VALUE q, ID w, int e, const VALUE *r, type *t, VALUE y)
{
    return ::rb_block_call(q, w, e, r, reinterpret_cast<rb_block_call_func_t>(t), y);
}

inline VALUE
rb_rescue(type *q, VALUE w, type *e, VALUE r)
{
    typedef VALUE func1_t(VALUE);
    typedef VALUE func2_t(VALUE, VALUE);
    return ::rb_rescue(reinterpret_cast<func1_t *>(q), w, reinterpret_cast<func2_t *>(e), r);
}

inline VALUE
rb_rescue2(type *q, VALUE w, type *e, VALUE r, ...)
{
    typedef VALUE func1_t(VALUE);
    typedef VALUE func2_t(VALUE, VALUE);
    va_list ap;
    va_start(ap, r);
    VALUE ret = ::rb_vrescue2(reinterpret_cast<func1_t *>(q), w, reinterpret_cast<func2_t *>(e), r, ap);
    va_end(ap);
    return ret;
}

inline VALUE
rb_ensure(type *q, VALUE w, type *e, VALUE r)
{
    typedef VALUE func1_t(VALUE);
    return ::rb_ensure(reinterpret_cast<func1_t *>(q), w, reinterpret_cast<func1_t *>(e), r);
}

inline VALUE
rb_catch(const char *q, type *w, VALUE e)
{
    return ::rb_catch(q, reinterpret_cast<rb_block_call_func_t>(w), e);
}

inline VALUE
rb_catch_obj(VALUE q, type *w, VALUE e)
{
    return ::rb_catch_obj(q, reinterpret_cast<rb_block_call_func_t>(w), e);
}

#if __cplusplus >= 201103L
inline VALUE
rb_iterate(onearg_type *q, VALUE w, std::nullptr_t e, VALUE r)
{
    return ::rb_iterate(q, w, e, r);
}

inline VALUE
rb_block_call(VALUE q, ID w, int e, const VALUE *r, std::nullptr_t t, VALUE y)
{
    return ::rb_block_call(q, w, e, r, t, y);
}

inline VALUE
rb_catch(const char *q, std::nullptr_t w, VALUE e)
{
    return ::rb_catch(q, w, e);
}
#endif

/* ---- procs ------------------------------------------------------------ */

inline VALUE
rb_proc_new(type *q, VALUE w)
{
    return ::rb_proc_new(reinterpret_cast<rb_block_call_func_t>(q), w);
}

/* ---- st_table and instance variables ---------------------------------- */

/* st_foreach and st_foreach_check are macros over these (ruby/st.h). */
inline int
rb_st_foreach(st_table *q, int_type *w, st_data_t e)
{
    return ::rb_st_foreach(q, reinterpret_cast<st_foreach_callback_func *>(w), e);
}

inline int
rb_st_foreach_check(st_table *q, int_type *w, st_data_t e, st_data_t)
{
    return ::rb_st_foreach_check(q, reinterpret_cast<st_foreach_check_callback_func *>(w), e, 0);
}

inline void
rb_ivar_foreach(VALUE q, int_type *w, st_data_t e)
{
    typedef int func_t(ID, VALUE, st_data_t);
    ::rb_ivar_foreach(q, reinterpret_cast<func_t *>(w), e);
}

}  // namespace cxxanyargs
}  // namespace backward
}  // namespace ruby
}  // extern "C++"

using namespace ruby::backward::cxxanyargs;

#endif /* __cplusplus */
#endif /* MONORUBY_CAPI_RUBY_BACKWARD_CXXANYARGS_HPP */
