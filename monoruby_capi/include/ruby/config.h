/*
 * ruby/config.h — what CRuby's generated ruby/config.h answers, for the
 * host monoruby runs on. ruby/ruby.h includes it; an extension that
 * includes it directly (for the SIZEOF_* / HAVE_* answers) gets the same
 * values. mkmf lists it ($(arch_hdrdir)/ruby/config.h) as a prerequisite
 * of every object, so it has to exist even where nothing reads it.
 */
#ifndef MONORUBY_CAPI_RUBY_CONFIG_H
#define MONORUBY_CAPI_RUBY_CONFIG_H 1


#define SIZEOF_INT 4
#define SIZEOF_LONG 8
#define SIZEOF_LONG_LONG 8
#define SIZEOF_VOIDP 8
#define SIZEOF_SIZE_T 8
#define SIZEOF_VALUE 8
#define HAVE_LONG_LONG 1
#define RUBY_INTEGER_UNIFICATION 1
#define HAVE_STDARG_PROTOTYPES 1
#define RUBY_EXTERN extern
#define RUBY_FUNC_EXPORTED __attribute__((__visibility__("default"))) extern
#define RUBY_SYMBOL_EXPORT_BEGIN _Pragma("GCC visibility push(default)")
#define RUBY_SYMBOL_EXPORT_END _Pragma("GCC visibility pop")
#define RUBY_FUNC_NONNULL(n, x) x
#define NORETURN(x) __attribute__((__noreturn__)) x
#ifndef UNREACHABLE
#define UNREACHABLE __builtin_unreachable()
#endif
#ifndef UNREACHABLE_RETURN
#define UNREACHABLE_RETURN(_) __builtin_unreachable()
#endif
#define DEPRECATED(x) x
#define PRINTF_ARGS(decl, a, b) decl
#define RUBY_ATTR_NORETURN __attribute__((__noreturn__))
#define RB_LIKELY(x) (__builtin_expect(!!(x), 1))
#define RB_UNLIKELY(x) (__builtin_expect(!!(x), 0))
#define LIKELY(x) RB_LIKELY(x)
#define UNLIKELY(x) RB_UNLIKELY(x)
#ifndef RUBY_UNUSED_VAR
#define RUBY_UNUSED_VAR(x) x __attribute__((__unused__))
#endif

#define HAVE_STDINT_H 1
#define HAVE_STDBOOL_H 1
#define HAVE_INTTYPES_H 1
#define HAVE_UNISTD_H 1
#define HAVE_SYS_TYPES_H 1
#define HAVE_STRING_H 1
#define HAVE_STDLIB_H 1
#define HAVE_TIME_H 1
#define HAVE_SYS_TIME_H 1
#define HAVE_CLOCK_GETTIME 1
#define HAVE_GMTIME_R 1
#define HAVE_LOCALTIME_R 1
#define HAVE_MKTIME 1
#define HAVE_TIMEGM 1
#define HAVE_STRUCT_TM_TM_GMTOFF 1
#define HAVE_STRUCT_TM_TM_ZONE 1
#define HAVE_STRUCT_TIMESPEC 1
#define HAVE_STRUCT_TIMEVAL 1
#define SIZEOF_SHORT 2
#define SIZEOF_FLOAT 4
#define SIZEOF_DOUBLE 8
#define SIZEOF_OFF_T 8
#define SIZEOF_TIME_T 8
#define SIZEOF_INTPTR_T 8
#define SIZEOF_UINTPTR_T 8
#define SIZEOF_PTRDIFF_T 8
#define SIZEOF_INT64_T 8
#define SIZEOF_UINT64_T 8
#define HAVE_INT64_T 1
#define HAVE_UINT64_T 1
#define RUBY_PLATFORM_CPU_64BIT 1

#endif /* MONORUBY_CAPI_RUBY_CONFIG_H */
