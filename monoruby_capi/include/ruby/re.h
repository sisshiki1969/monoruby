/* ruby/re.h — monoruby's C API layer.
 *
 * CRuby's regexp surface for extensions that run Onigmo themselves
 * (strscan): `rb_reg_prepare_re` hands out the `regex_t` monoruby would
 * match a given string with, and the `onig_*` functions declared by
 * `ruby/onigmo.h` (Onigmo's own header, the version linked into
 * monoruby) are wrappers in capi.c over the interpreter's copy of
 * Onigmo, since an extension's `.so` cannot resolve them against the
 * binary. `OnigRegion` / `struct re_registers` is the real struct, so
 * `regs->beg[i]` reads are what CRuby's are.
 *
 * Differences from CRuby:
 *  - There is no `struct RRegexp`: a monoruby Regexp is not laid out as
 *    CRuby's, so `RREGEXP(re)->ptr` is not addressable. `RREGEXP_PTR(re)`
 *    is a call (`rb_reg_ptr`) and read-only; `RREGEXP_SRC(re)` likewise.
 *  - Every `regex_t *` from `rb_reg_prepare_re` / `RREGEXP_PTR` belongs
 *    to monoruby: never `onig_free` it. `rb_reg_prepare_re` returns the
 *    buffer compiled for the string's codec from a cache, so the pointer
 *    may differ from `RREGEXP_PTR(re)` and stays valid either way.
 *  - `onig_new` is not provided (nor Onigmo's encoding / syntax tables,
 *    which are data symbols an extension could not reach); a pattern is
 *    compiled by making a Regexp (`rb_reg_regcomp`).
 */
#ifndef MONORUBY_CAPI_RE_H
#define MONORUBY_CAPI_RE_H

#include "ruby/ruby.h"
#include "ruby/encoding.h"
#include "ruby/onigmo.h"

#ifdef __cplusplus
extern "C" {
#endif

struct re_registers;            /* OnigRegion, in onigmo.h */

struct rmatch_offset {
    long beg;
    long end;
};

/* The pattern buffer of `re` under the regexp's own codec. */
regex_t *rb_reg_ptr(VALUE re);
#define RREGEXP_PTR(re) rb_reg_ptr(re)
/* `Regexp#source`. */
VALUE rb_reg_source(VALUE re);
#define RREGEXP_SRC(re)     rb_reg_source(re)
#define RREGEXP_SRC_PTR(re) RSTRING_PTR(rb_reg_source(re))
#define RREGEXP_SRC_LEN(re) RSTRING_LEN(rb_reg_source(re))
#define RREGEXP_SRC_END(re) RSTRING_END(rb_reg_source(re))

VALUE rb_reg_regcomp(VALUE str);
VALUE rb_reg_new_str(VALUE src, int options);
VALUE rb_reg_new(const char *src, long len, int options);
VALUE rb_reg_quote(VALUE str);
VALUE rb_reg_match(VALUE re, VALUE str);
VALUE rb_reg_nth_match(int n, VALUE md);
VALUE rb_reg_last_match(VALUE md);
VALUE rb_reg_match_pre(VALUE md);
VALUE rb_reg_match_post(VALUE md);
int rb_reg_options(VALUE re);
void rb_match_busy(VALUE md);

/* The `regex_t` to run on `str`'s bytes: compiled for `str`'s codec, and
 * checked for encoding compatibility as `Regexp#match` is (raises
 * Encoding::CompatibilityError / ArgumentError). Owned by monoruby. */
regex_t *rb_reg_prepare_re(VALUE re, VALUE str);
/* CRuby 3.3's: `match(reg, str, regs, args)` on the prepared buffer;
 * ONIG_MISMATCH when it did not match, the match position otherwise, and
 * a RegexpError / Regexp::TimeoutError for Onigmo's other failures. */
OnigPosition rb_reg_onig_match(VALUE re, VALUE str,
                               OnigPosition (*match)(regex_t *reg, VALUE str, struct re_registers *regs, void *args),
                               void *args, struct re_registers *regs);
/* `onig_region_copy`, retrying after a GC when it ran out of memory. */
int rb_reg_region_copy(struct re_registers *dst, const struct re_registers *src);
/* Bytes an OnigRegion owns, the struct included (CRuby's addition). */
size_t onig_region_memsize(const struct re_registers *regs);

#ifdef __cplusplus
}
#endif

#endif /* MONORUBY_CAPI_RE_H */
