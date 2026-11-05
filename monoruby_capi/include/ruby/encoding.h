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
VALUE rb_enc_str_buf_cat(VALUE str, const char *ptr, long len, rb_encoding *enc);
VALUE rb_external_str_new_with_enc(const char *ptr, long len, rb_encoding *enc);
VALUE rb_str_export_to_enc(VALUE str, rb_encoding *enc);
VALUE rb_str_conv_enc(VALUE str, rb_encoding *from, rb_encoding *to);
VALUE rb_str_encode(VALUE str, VALUE to, int ecflags, VALUE ecopts);
int rb_enc_asciicompat(rb_encoding *enc);
int rb_enc_str_asciionly_p(VALUE str);
#define rb_enc_str_asciicompat_p(str) rb_enc_asciicompat(rb_enc_get(str))
VALUE rb_enc_sprintf(rb_encoding *enc, const char *fmt, ...);
ID rb_intern3(const char *name, long len, rb_encoding *enc);
ID rb_check_id_cstr(const char *ptr, long len, rb_encoding *enc);
VALUE rb_check_symbol_cstr(const char *ptr, long len, rb_encoding *enc);
VALUE rb_obj_encoding(VALUE obj);
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
/* `rb_enc_precise_mbclen` answers in Onigmo's encoding, as CRuby's does:
 * the length when a character is found, -1 for an invalid sequence, and
 * -1-n when n more bytes are needed. These are `ruby/onigmo.h`'s
 * definitions, token for token, so including both in either order is
 * fine. */
#ifndef ONIGENC_MBCLEN_CHARFOUND_P
#define ONIGENC_CONSTRUCT_MBCLEN_CHARFOUND(n)   (n)
#define ONIGENC_MBCLEN_CHARFOUND_P(r)           (0 < (r))
#define ONIGENC_MBCLEN_CHARFOUND_LEN(r)         (r)

#define ONIGENC_CONSTRUCT_MBCLEN_INVALID()      (-1)
#define ONIGENC_MBCLEN_INVALID_P(r)             ((r) == -1)

#define ONIGENC_CONSTRUCT_MBCLEN_NEEDMORE(n)    (-1-(n))
#define ONIGENC_MBCLEN_NEEDMORE_P(r)            ((r) < -1)
#define ONIGENC_MBCLEN_NEEDMORE_LEN(r)          (-1-(r))
#endif
#define MBCLEN_CHARFOUND_P(ret) ONIGENC_MBCLEN_CHARFOUND_P(ret)
#define MBCLEN_CHARFOUND_LEN(ret) ONIGENC_MBCLEN_CHARFOUND_LEN(ret)
#define MBCLEN_INVALID_P(ret) ONIGENC_MBCLEN_INVALID_P(ret)
#define MBCLEN_NEEDMORE_P(ret) ONIGENC_MBCLEN_NEEDMORE_P(ret)
#define MBCLEN_NEEDMORE_LEN(ret) ONIGENC_MBCLEN_NEEDMORE_LEN(ret)
/* Encoding::Converter's option bits (transcode.c), CRuby's values. Only
 * `rb_econv_prepare_options` and `rb_str_encode` read them here. */
#define ECONV_ERROR_HANDLER_MASK                0x000000ff
#define ECONV_INVALID_MASK                      0x0000000f
#define ECONV_INVALID_REPLACE                   0x00000002
#define ECONV_UNDEF_MASK                        0x000000f0
#define ECONV_UNDEF_REPLACE                     0x00000020
#define ECONV_UNDEF_HEX_CHARREF                 0x00000030
#define ECONV_DECORATOR_MASK                    0x0001ff00
#define ECONV_NEWLINE_DECORATOR_MASK            0x00007f00
#define ECONV_NEWLINE_DECORATOR_READ_MASK       0x00000f00
#define ECONV_NEWLINE_DECORATOR_WRITE_MASK      0x00007000
#define ECONV_UNIVERSAL_NEWLINE_DECORATOR       0x00000100
#define ECONV_CRLF_NEWLINE_DECORATOR            0x00001000
#define ECONV_CR_NEWLINE_DECORATOR              0x00002000
#define ECONV_LF_NEWLINE_DECORATOR              0x00004000
#define ECONV_XML_TEXT_DECORATOR                0x00008000
#define ECONV_XML_ATTR_CONTENT_DECORATOR        0x00010000
#define ECONV_STATEFUL_DECORATOR_MASK           0x00f00000
#define ECONV_XML_ATTR_QUOTE_DECORATOR          0x00100000
#define ECONV_DEFAULT_NEWLINE_DECORATOR         0
#define ECONV_PARTIAL_INPUT                     0x00020000
#define ECONV_AFTER_OUTPUT                      0x00040000
int rb_econv_prepare_options(VALUE opthash, VALUE *ecopts, int ecflags);
int rb_econv_prepare_opts(VALUE opthash, VALUE *ecopts);

/* transcode.h's converter, over an `Encoding::Converter`. */
typedef enum {
    econv_invalid_byte_sequence,
    econv_undefined_conversion,
    econv_destination_buffer_full,
    econv_source_buffer_empty,
    econv_finished,
    econv_after_output,
    econv_incomplete_input
} rb_econv_result_t;
typedef struct rb_econv_t rb_econv_t;
rb_econv_t *rb_econv_open(const char *source_encoding, const char *destination_encoding, int ecflags);
rb_econv_t *rb_econv_open_opts(const char *source_encoding, const char *destination_encoding, int ecflags, VALUE ecopts);
void rb_econv_close(rb_econv_t *ec);
rb_econv_result_t rb_econv_convert(rb_econv_t *ec,
                                   const unsigned char **source_buffer_ptr, const unsigned char *source_buffer_end,
                                   unsigned char **destination_buffer_ptr, unsigned char *destination_buffer_end,
                                   int flags);
VALUE rb_econv_str_convert(rb_econv_t *ec, VALUE src, int flags);
void rb_econv_check_error(rb_econv_t *ec);
VALUE rb_econv_open_exc(const char *senc, const char *denc, int ecflags);
VALUE rb_str_conv_enc_opts(VALUE str, rb_encoding *from, rb_encoding *to, int ecflags, VALUE ecopts);
#define rb_is_ascii8bit_enc(enc) ((enc) == rb_ascii8bit_encoding())
void rb_enc_warn(rb_encoding *enc, const char *fmt, ...);
VALUE rb_enc_uint_chr(unsigned int code, rb_encoding *enc);
int rb_monoruby_enc_mbcput(unsigned int c, void *buf, rb_encoding *enc);
#define rb_enc_mbcput(c, buf, enc) rb_monoruby_enc_mbcput((unsigned int)(c), (buf), (enc))
rb_encoding *rb_find_encoding(VALUE enc);
char *rb_enc_left_char_head(const char *s, const char *p, const char *e, rb_encoding *enc);
char *rb_enc_right_char_head(const char *s, const char *p, const char *e, rb_encoding *enc);
char *rb_enc_prev_char(const char *s, const char *p, const char *e, rb_encoding *enc);
#define rb_enc_str_new_lit(s, enc) rb_enc_str_new((s), (long)(sizeof(s) - 1), (enc))

#ifdef __cplusplus
}
#endif

#endif /* MONORUBY_CAPI_RUBY_ENCODING_H */
