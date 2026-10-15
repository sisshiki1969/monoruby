/*
 * ruby.h — monoruby's CRuby-compatible C API (see ruby/ruby.h).
 */
#ifndef RUBY_H
#define RUBY_H 1
/* The headers under ruby/, as CRuby's ruby.h announces them (yajl-ruby
 * includes ruby/encoding.h only under HAVE_RUBY_ENCODING_H). */
#define HAVE_RUBY_DEFINES_H  1
#define HAVE_RUBY_ENCODING_H 1
#define HAVE_RUBY_INTERN_H   1
#define HAVE_RUBY_MISSING_H  1
#define HAVE_RUBY_ONIGMO_H   1
#define HAVE_RUBY_RE_H       1
#define HAVE_RUBY_RUBY_H     1
#define HAVE_RUBY_ST_H       1
#define HAVE_RUBY_THREAD_H   1
#define HAVE_RUBY_UTIL_H     1
#define HAVE_RUBY_VERSION_H  1
#include "ruby/ruby.h"
#endif
