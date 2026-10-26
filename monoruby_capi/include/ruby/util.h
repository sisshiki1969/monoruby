/*
 * ruby/util.h — the helpers CRuby exports from util.c that extensions
 * use (bcrypt's wrapper.c takes `ruby_strdup` for `strdup`).
 */
#ifndef MONORUBY_CAPI_RUBY_UTIL_H
#define MONORUBY_CAPI_RUBY_UTIL_H 1

#include "ruby/ruby.h"

#ifdef __cplusplus
extern "C" {
#endif

char *ruby_strdup(const char *str);
unsigned long ruby_strtoul(const char *str, char **endptr, int base);
double ruby_strtod(const char *str, char **endptr);
unsigned long ruby_scan_oct(const char *start, size_t len, size_t *retlen);
unsigned long ruby_scan_hex(const char *start, size_t len, size_t *retlen);
unsigned long ruby_scan_digits(const char *str, ssize_t len, int base, size_t *retlen, int *overflow);
void ruby_qsort(void *base, const size_t nel, const size_t size,
                int (*cmp)(const void *, const void *, void *), void *d);
char *ruby_getcwd(void);

#ifdef __cplusplus
}
#endif

#endif /* MONORUBY_CAPI_RUBY_UTIL_H */
