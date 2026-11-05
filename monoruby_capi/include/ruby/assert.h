#ifndef RUBY_ASSERT_H
#define RUBY_ASSERT_H 1
/* CRuby's assertion macros, as they are without RUBY_DEBUG: the checked
 * forms evaluate nothing, RUBY_ASSERT_ALWAYS uses the C library's. */
#include <assert.h>

#define RUBY_ASSERT_ALWAYS(expr, ...) assert(expr)
#define RUBY_ASSERT_MESG(expr, ...) ((void)0)
#define RUBY_ASSERT(expr, ...) ((void)0)
#define RUBY_ASSERT_WHEN(cond, expr, ...) ((void)0)
#define RUBY_ASSERT_MESG_WHEN(cond, expr, ...) ((void)0)
#define RUBY_ASSERT_NDEBUG 1
#endif
