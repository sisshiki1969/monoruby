/*
 * libxslt helpers for monoruby's Nokogiri::XSLT (`xslt_stylesheet.c`):
 * the generic-error capture (`xsltSetGenericErrorFunc` /
 * `xmlSetGenericErrorFunc` take a variadic callback, so the formatting
 * happens here), and the `_private` slot of a stylesheet, reached from a
 * transform context too, so no libxslt struct is mirrored in Rust.
 */
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <libxml/xmlerror.h>
#include <libxslt/xslt.h>
#include <libxslt/xsltInternals.h>
#include <libxslt/xsltutils.h>
#include <libxslt/transform.h>

/* The text collected by one capture. */
typedef struct {
    char *buf;
    size_t len;
    size_t cap;
} mrb_xslt_errbuf;

static void
mrb_xslt_error_append(void *ctx, const char *msg, ...)
{
    mrb_xslt_errbuf *b = (mrb_xslt_errbuf *)ctx;
    va_list ap, ap2;
    int n;

    if (b == NULL)
        return;
    va_start(ap, msg);
    va_copy(ap2, ap);
    n = vsnprintf(NULL, 0, msg, ap);
    va_end(ap);
    if (n < 0) {
        va_end(ap2);
        return;
    }
    if (b->len + (size_t)n + 1 > b->cap) {
        size_t cap = b->cap ? b->cap * 2 : 256;
        char *grown;
        while (cap < b->len + (size_t)n + 1)
            cap *= 2;
        grown = realloc(b->buf, cap);
        if (grown == NULL) {
            va_end(ap2);
            return;
        }
        b->buf = grown;
        b->cap = cap;
    }
    vsnprintf(b->buf + b->len, (size_t)n + 1, msg, ap2);
    va_end(ap2);
    b->len += (size_t)n;
}

/*
 * Route libxslt's generic errors (and libxml2's too when `with_xml`) into
 * a fresh buffer until `mrb_xslt_error_capture_end`. NULL when out of
 * memory (nothing is captured then).
 */
void *
mrb_xslt_error_capture_begin(int with_xml)
{
    mrb_xslt_errbuf *b = calloc(1, sizeof(*b));

    if (b == NULL)
        return NULL;
    xsltSetGenericErrorFunc(b, mrb_xslt_error_append);
    if (with_xml)
        xmlSetGenericErrorFunc(b, mrb_xslt_error_append);
    return b;
}

/*
 * Stop capturing (both handlers go back to their defaults, as nokogiri
 * leaves them): the text so far, `*len` bytes, NULL when nothing was
 * reported. Freed with `mrb_xslt_error_capture_free`.
 */
char *
mrb_xslt_error_capture_end(void *handle, size_t *len)
{
    mrb_xslt_errbuf *b = (mrb_xslt_errbuf *)handle;
    char *text;

    xsltSetGenericErrorFunc(NULL, NULL);
    xmlSetGenericErrorFunc(NULL, NULL);
    if (b == NULL) {
        *len = 0;
        return NULL;
    }
    text = b->buf;
    *len = b->len;
    free(b);
    return text;
}

void
mrb_xslt_error_capture_free(char *text)
{
    free(text);
}

void *
mrb_xslt_stylesheet_get_private(xsltStylesheetPtr style)
{
    return style->_private;
}

void
mrb_xslt_stylesheet_set_private(xsltStylesheetPtr style, void *p)
{
    style->_private = p;
}

xsltStylesheetPtr
mrb_xslt_transform_ctxt_style(xsltTransformContextPtr ctxt)
{
    return ctxt->style;
}
