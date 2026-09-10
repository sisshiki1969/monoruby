/*
 * Small C helpers for monoruby's Nokogiri over the bundled libxml2:
 *
 * - accessors for the `xmlParserCtxt` fields the SAX layer reads and
 *   writes (the struct is large and version-specific, so it is not
 *   mirrored on the Rust side);
 * - the SAX `warning` / `error` callbacks, which are variadic (printf
 *   style): they format the message here and hand the text to a Rust
 *   function registered at start-up.
 */

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

#include <libxml/parser.h>
#include <libxml/HTMLparser.h>

void *
mrb_xml_ctxt_get_private(xmlParserCtxtPtr ctxt)
{
    return ctxt->_private;
}

void
mrb_xml_ctxt_set_private(xmlParserCtxtPtr ctxt, void *p)
{
    ctxt->_private = p;
}

xmlSAXHandlerPtr
mrb_xml_ctxt_get_sax(xmlParserCtxtPtr ctxt)
{
    return ctxt->sax;
}

void
mrb_xml_ctxt_set_sax(xmlParserCtxtPtr ctxt, xmlSAXHandlerPtr sax)
{
    ctxt->sax = sax;
}

void
mrb_xml_ctxt_set_user_data(xmlParserCtxtPtr ctxt, void *data)
{
    ctxt->userData = data;
}

xmlDocPtr
mrb_xml_ctxt_get_my_doc(xmlParserCtxtPtr ctxt)
{
    return ctxt->myDoc;
}

int
mrb_xml_ctxt_get_standalone(xmlParserCtxtPtr ctxt)
{
    return ctxt->standalone;
}

const xmlChar *
mrb_xml_ctxt_get_encoding(xmlParserCtxtPtr ctxt)
{
    if (ctxt->encoding != NULL)
        return ctxt->encoding;
    if (ctxt->input != NULL)
        return ctxt->input->encoding;
    return NULL;
}

const xmlChar *
mrb_xml_ctxt_get_version(xmlParserCtxtPtr ctxt)
{
    return ctxt->version;
}

int
mrb_xml_ctxt_get_options(xmlParserCtxtPtr ctxt)
{
    return ctxt->options;
}

/* -1 when there is no input */
int
mrb_xml_ctxt_get_line(xmlParserCtxtPtr ctxt)
{
    return ctxt->input != NULL ? ctxt->input->line : -1;
}

int
mrb_xml_ctxt_get_column(xmlParserCtxtPtr ctxt)
{
    return ctxt->input != NULL ? ctxt->input->col : -1;
}

/*
 * The variadic SAX message callbacks. `is_error` is 0 for `warning`,
 * 1 for `error`.
 */
typedef void (*mrb_sax_message_fn)(void *ctx, int is_error, const char *text);

static mrb_sax_message_fn mrb_sax_message_handler = NULL;

void
mrb_xml_sax_set_message_handler(mrb_sax_message_fn fn)
{
    mrb_sax_message_handler = fn;
}

static void
mrb_xml_sax_message(void *ctx, int is_error, const char *msg, va_list ap)
{
    char stack_buf[512];
    char *buf = stack_buf;
    va_list ap2;
    int n;

    if (mrb_sax_message_handler == NULL)
        return;
    va_copy(ap2, ap);
    n = vsnprintf(stack_buf, sizeof(stack_buf), msg, ap);
    if (n < 0) {
        va_end(ap2);
        return;
    }
    if ((size_t)n >= sizeof(stack_buf)) {
        buf = malloc((size_t)n + 1);
        if (buf == NULL) {
            va_end(ap2);
            return;
        }
        vsnprintf(buf, (size_t)n + 1, msg, ap2);
    }
    va_end(ap2);
    mrb_sax_message_handler(ctx, is_error, buf);
    if (buf != stack_buf)
        free(buf);
}

void
mrb_xml_sax_warning(void *ctx, const char *msg, ...)
{
    va_list ap;
    va_start(ap, msg);
    mrb_xml_sax_message(ctx, 0, msg, ap);
    va_end(ap);
}

void
mrb_xml_sax_error(void *ctx, const char *msg, ...)
{
    va_list ap;
    va_start(ap, msg);
    mrb_xml_sax_message(ctx, 1, msg, ap);
    va_end(ap);
}

/*
 * `xmlXPathContext` accessors for the custom-function handler: the
 * function being called (set by the evaluator before the call) and the
 * function-lookup data (the running call).
 */
#include <libxml/xpath.h>

const xmlChar *
mrb_xpath_ctx_get_function(xmlXPathContextPtr ctx)
{
    return ctx->function;
}

const xmlChar *
mrb_xpath_ctx_get_function_uri(xmlXPathContextPtr ctx)
{
    return ctx->functionURI;
}

/*
 * The data registered with `xmlXPathRegisterFuncLookup` (`userData` is
 * not usable for this: `xmlXPathSetErrorHandler` stores its own data
 * pointer there).
 */
void *
mrb_xpath_ctx_get_func_lookup_data(xmlXPathContextPtr ctx)
{
    return ctx->funcLookupData;
}
