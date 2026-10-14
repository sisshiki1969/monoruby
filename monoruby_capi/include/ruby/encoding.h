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
#define rb_enc_str_new_lit(s, enc) rb_enc_str_new((s), (long)(sizeof(s) - 1), (enc))

#ifdef __cplusplus
}
#endif

#endif /* MONORUBY_CAPI_RUBY_ENCODING_H */
