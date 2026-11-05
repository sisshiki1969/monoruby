/*
 * ruby/subst.h — as CRuby's: snprintf / vsnprintf become ruby_snprintf /
 * ruby_vsnprintf (capi.c), which format the plain integer and string
 * conversions without libc's stdio machinery and hand everything else to
 * libc. bigdecimal formats its digits one snprintf("%lu") at a time.
 * C++ is left alone (`std::snprintf` would not survive the rename).
 */
#ifndef MONORUBY_CAPI_RUBY_SUBST_H
#define MONORUBY_CAPI_RUBY_SUBST_H 1
#include "ruby/ruby.h"
#endif

#if !defined(RUBY_DONT_SUBST) && !defined(__cplusplus) && !defined(MONORUBY_CAPI_SUBST_DONE)
#define MONORUBY_CAPI_SUBST_DONE 1
#include <stdarg.h>
#include <stdio.h>
int ruby_snprintf(char *str, size_t n, const char *fmt, ...) __attribute__((format(printf, 3, 4)));
int ruby_vsnprintf(char *str, size_t n, const char *fmt, va_list ap);
#undef snprintf
#undef vsnprintf
#define snprintf ruby_snprintf
#define vsnprintf ruby_vsnprintf
#endif
