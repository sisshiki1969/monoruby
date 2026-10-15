/*
 * ruby/encoding.h — monoruby's CRuby-compatible encoding API.
 *
 * An rb_encoding is a record the C API layer keeps per encoding name;
 * rb_enc_name reads its name as CRuby's macro does. The index of an
 * encoding is the layer's own: 0, 1 and 2 are ASCII-8BIT, UTF-8 and
 * US-ASCII, as in CRuby.
 */
#ifndef MONORUBY_CAPI_RUBY_ENCODING_H
#define MONORUBY_CAPI_RUBY_ENCODING_H 1

#include "ruby/ruby.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct rb_encoding {
    const char *name;
    int index;
} rb_encoding;

#define rb_enc_name(enc) ((enc)->name)
#define rb_enc_to_index(enc) ((enc) ? (enc)->index : 0)
#define ENCODING_GET(obj) rb_enc_get_index((VALUE)(obj))
#define ENCODING_SET(obj, i) rb_enc_set_index((VALUE)(obj), (i))
/* CRuby reads the index out of the object header; here every form asks
 * the interpreter. */
#define ENCODING_GET_INLINED(obj) ENCODING_GET(obj)
#define RB_ENCODING_GET(obj) ENCODING_GET(obj)
#define RB_ENCODING_GET_INLINED(obj) ENCODING_GET(obj)
#define RB_ENCODING_SET(obj, i) ENCODING_SET(obj, i)
#define RB_ENCODING_SET_INLINED(obj, i) ENCODING_SET(obj, i)
#define ENCODING_SET_INLINED(obj, i) ENCODING_SET(obj, i)
#define ENCODING_INLINE_MAX 127
#define ENCODING_MAXNAMELEN 42

/* Coderange: CRuby caches it in the object header; `rb_enc_str_coderange`
 * computes it here each time (`valid_encoding?` / `ascii_only?`). The
 * values are CRuby's. */
#define RUBY_ENC_CODERANGE_UNKNOWN 0
#define RUBY_ENC_CODERANGE_7BIT ((int)(1 << 20))
#define RUBY_ENC_CODERANGE_VALID ((int)(2 << 20))
#define RUBY_ENC_CODERANGE_BROKEN ((int)(3 << 20))
#define RUBY_ENC_CODERANGE_MASK ((int)(3 << 20))
#define ENC_CODERANGE_UNKNOWN RUBY_ENC_CODERANGE_UNKNOWN
#define ENC_CODERANGE_7BIT RUBY_ENC_CODERANGE_7BIT
#define ENC_CODERANGE_VALID RUBY_ENC_CODERANGE_VALID
#define ENC_CODERANGE_BROKEN RUBY_ENC_CODERANGE_BROKEN
#define ENC_CODERANGE_MASK RUBY_ENC_CODERANGE_MASK
#define RB_ENC_CODERANGE_UNKNOWN RUBY_ENC_CODERANGE_UNKNOWN
#define RB_ENC_CODERANGE_7BIT RUBY_ENC_CODERANGE_7BIT
#define RB_ENC_CODERANGE_VALID RUBY_ENC_CODERANGE_VALID
#define RB_ENC_CODERANGE_BROKEN RUBY_ENC_CODERANGE_BROKEN
int rb_enc_str_coderange(VALUE str);
#define ENC_CODERANGE(obj) rb_enc_str_coderange((VALUE)(obj))
#define RB_ENC_CODERANGE(obj) rb_enc_str_coderange((VALUE)(obj))
#define ENC_CODERANGE_ASCIIONLY(obj) (rb_enc_str_coderange((VALUE)(obj)) == ENC_CODERANGE_7BIT)
#define RB_ENC_CODERANGE_ASCIIONLY(obj) ENC_CODERANGE_ASCIIONLY(obj)
#define ENC_CODERANGE_CLEAN_P(cr) ((cr) == ENC_CODERANGE_7BIT || (cr) == ENC_CODERANGE_VALID)
#define RB_ENC_CODERANGE_CLEAN_P(cr) ENC_CODERANGE_CLEAN_P(cr)
/* Nothing to cache, so these are no-ops. */
#define ENC_CODERANGE_SET(obj, cr) ((void)(obj), (void)(cr))
#define ENC_CODERANGE_CLEAR(obj) ((void)(obj))
#define RB_ENC_CODERANGE_SET(obj, cr) ENC_CODERANGE_SET(obj, cr)
#define RB_ENC_CODERANGE_CLEAR(obj) ENC_CODERANGE_CLEAR(obj)

rb_encoding *rb_ascii8bit_encoding(void);
rb_encoding *rb_utf8_encoding(void);
rb_encoding *rb_usascii_encoding(void);
rb_encoding *rb_locale_encoding(void);
rb_encoding *rb_filesystem_encoding(void);
rb_encoding *rb_default_external_encoding(void);
rb_encoding *rb_default_internal_encoding(void);
int rb_ascii8bit_encindex(void);
int rb_utf8_encindex(void);
int rb_usascii_encindex(void);
rb_encoding *rb_enc_get(VALUE obj);
int rb_enc_get_index(VALUE obj);
void rb_enc_set_index(VALUE obj, int idx);
rb_encoding *rb_enc_find(const char *name);
int rb_enc_find_index(const char *name);
rb_encoding *rb_enc_from_index(int idx);
VALUE rb_enc_from_encoding(rb_encoding *enc);
rb_encoding *rb_to_encoding(VALUE enc);
int rb_to_encoding_index(VALUE enc);
VALUE rb_enc_associate(VALUE obj, rb_encoding *enc);
VALUE rb_enc_associate_index(VALUE obj, int idx);
void rb_enc_copy(VALUE dst, VALUE src);
VALUE rb_enc_str_new(const char *ptr, long len, rb_encoding *enc);
VALUE rb_enc_str_new_cstr(const char *ptr, rb_encoding *enc);
VALUE rb_enc_str_new_static(const char *ptr, long len, rb_encoding *enc);
VALUE rb_external_str_new_with_enc(const char *ptr, long len, rb_encoding *enc);
VALUE rb_str_export_to_enc(VALUE str, rb_encoding *enc);
VALUE rb_str_conv_enc(VALUE str, rb_encoding *from, rb_encoding *to);
VALUE rb_str_encode(VALUE str, VALUE to, int ecflags, VALUE ecopts);
int rb_enc_asciicompat(rb_encoding *enc);
int rb_enc_str_asciionly_p(VALUE str);
VALUE rb_enc_sprintf(rb_encoding *enc, const char *fmt, ...);
ID rb_intern3(const char *name, long len, rb_encoding *enc);
VALUE rb_enc_interned_str(const char *ptr, long len, rb_encoding *enc);
VALUE rb_enc_interned_str_cstr(const char *ptr, rb_encoding *enc);
/* The common encoding of two strings (the first's when both are ASCII
 * only), raising Encoding::CompatibilityError when there is none. */
rb_encoding *rb_enc_check(VALUE str1, VALUE str2);
rb_encoding *rb_enc_compatible(VALUE str1, VALUE str2);
long rb_enc_strlen(const char *p, const char *e, rb_encoding *enc);
int rb_enc_mbclen(const char *p, const char *e, rb_encoding *enc);
int rb_enc_precise_mbclen(const char *p, const char *e, rb_encoding *enc);
int rb_enc_codelen(int code, rb_encoding *enc);
unsigned int rb_enc_codepoint_len(const char *p, const char *e, int *len_p, rb_encoding *enc);
int rb_enc_mbminlen(rb_encoding *enc);
int rb_enc_mbmaxlen(rb_encoding *enc);
int rb_enc_unicode_p(rb_encoding *enc);
int rb_enc_dummy_p(rb_encoding *enc);
void rb_must_asciicompat(VALUE str);
NORETURN(void rb_enc_raise(rb_encoding *enc, VALUE exc, const char *fmt, ...));
#define rb_enc_mbc_to_codepoint(p, e, enc) rb_enc_codepoint_len((p), (e), NULL, (enc))
#define rb_enc_codepoint(p, e, enc) rb_enc_codepoint_len((p), (e), NULL, (enc))
#define MBCLEN_CHARFOUND_P(ret) ((ret) > 0)
#define MBCLEN_CHARFOUND_LEN(ret) (ret)
#define MBCLEN_INVALID_P(ret) ((ret) < 0)
#define MBCLEN_NEEDMORE_P(ret) ((ret) == 0)
#define ONIGENC_MBCLEN_CHARFOUND_P MBCLEN_CHARFOUND_P
#define ONIGENC_MBCLEN_CHARFOUND_LEN MBCLEN_CHARFOUND_LEN
#define ONIGENC_MBCLEN_INVALID_P MBCLEN_INVALID_P
#define ONIGENC_MBCLEN_NEEDMORE_P MBCLEN_NEEDMORE_P
#define rb_enc_str_new_lit(s, enc) rb_enc_str_new((s), (long)(sizeof(s) - 1), (enc))

#ifdef __cplusplus
}
#endif

#endif /* MONORUBY_CAPI_RUBY_ENCODING_H */
