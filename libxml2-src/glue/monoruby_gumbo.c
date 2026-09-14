/*
 * The gumbo side of monoruby's Nokogiri HTML5 support: a parse entry
 * point taking plain option values, accessors over the (opaque to Rust)
 * `GumboOutput`, and the walk that builds the libxml2 tree from the
 * gumbo tree. The walk is nokogiri's `build_tree` (ext/nokogiri/gumbo.c,
 * MIT licensed, Copyright 2013-2021 Sam Ruby, Stephen Checkoway and the
 * Nokogiri contributors), kept in C so that the gumbo structs are never
 * mirrored on the Rust side.
 */

#include <limits.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#include <libxml/tree.h>
#include <libxml/HTMLtree.h>

#include "nokogiri_gumbo.h"

GumboOutput *
mrb_gumbo_parse(
    const char *input, size_t length,
    int max_attributes, int max_errors, int max_tree_depth, int noscript_as_text,
    int is_fragment, const char *fragment_context, int fragment_namespace,
    const char *fragment_encoding, int quirks_mode, int has_form_ancestor)
{
    GumboOptions options = kGumboDefaultOptions;
    options.max_attributes = max_attributes;
    options.max_errors = max_errors;
    options.max_tree_depth = max_tree_depth < 0 ? UINT_MAX : (unsigned int)max_tree_depth;
    options.parse_noscript_content_as_text = noscript_as_text != 0;
    if (is_fragment) {
        options.fragment_context = fragment_context;
        options.fragment_namespace = (GumboNamespaceEnum)fragment_namespace;
        options.fragment_encoding = fragment_encoding;
        options.quirks_mode = (GumboQuirksModeEnum)quirks_mode;
        options.fragment_context_has_form_ancestor = has_form_ancestor != 0;
        /* one more for the html element the fragment parse adds */
        if (options.max_tree_depth < UINT_MAX)
            options.max_tree_depth++;
    }
    return gumbo_parse_with_options(&options, input, length);
}

int
mrb_gumbo_output_status(const GumboOutput *output)
{
    return (int)output->status;
}

const char *
mrb_gumbo_status_string(int status)
{
    return gumbo_status_to_string((GumboOutputStatus)status);
}

void
mrb_gumbo_destroy_output(GumboOutput *output)
{
    gumbo_destroy_output(output);
}

int
mrb_gumbo_has_doctype(const GumboOutput *output)
{
    return output->document->v.document.has_doctype ? 1 : 0;
}

/* NULL for an empty identifier, as nokogiri passes them to libxml2 */
const char *
mrb_gumbo_doctype_name(const GumboOutput *output)
{
    return output->document->v.document.name;
}

const char *
mrb_gumbo_doctype_public(const GumboOutput *output)
{
    const char *s = output->document->v.document.public_identifier;
    return (s && s[0]) ? s : NULL;
}

const char *
mrb_gumbo_doctype_system(const GumboOutput *output)
{
    const char *s = output->document->v.document.system_identifier;
    return (s && s[0]) ? s : NULL;
}

int
mrb_gumbo_quirks_mode(const GumboOutput *output)
{
    return (int)output->document->v.document.doc_type_quirks_mode;
}

int
mrb_gumbo_compute_quirks_mode(const char *name, const char *pubid, const char *sysid)
{
    return (int)gumbo_compute_quirks_mode(name, pubid, sysid);
}

/* URI = system id, external id = public id */
xmlDocPtr
mrb_gumbo_new_html_doc(const char *dtd_name, const char *system, const char *public)
{
    /* These two libxml2 functions take the public and system ids in
     * opposite orders. */
    htmlDocPtr doc = htmlNewDocNoDtD(/* URI */ NULL, /* ExternalID */ NULL);
    if (doc == NULL)
        return NULL;
    if (dtd_name) {
        xmlCreateIntSubset(doc, (const xmlChar *)dtd_name, (const xmlChar *)public, (const xmlChar *)system);
    }
    return doc;
}

static xmlNsPtr
lookup_or_add_ns(xmlDocPtr doc, xmlNodePtr root, const char *href, const char *prefix)
{
    xmlNsPtr ns = xmlSearchNs(doc, root, (const xmlChar *)prefix);
    if (ns) {
        return ns;
    }
    return xmlNewNs(root, (const xmlChar *)href, (const xmlChar *)prefix);
}

static void
set_line(xmlNodePtr node, size_t line)
{
    /* libxml2 uses 65535 to mean look elsewhere for the line number on
     * some nodes. */
    if (line < 65535) {
        node->line = (unsigned short)line;
    }
}

/* Construct an XML tree rooted at xml_output_node from the Gumbo tree
 * rooted at gumbo_node. */
static void
build_tree(xmlDocPtr doc, xmlNodePtr xml_output_node, const GumboNode *gumbo_node)
{
    xmlNodePtr xml_root = NULL;
    xmlNodePtr xml_node = xml_output_node;
    size_t child_index = 0;

    while (true) {
        const GumboVector *children = gumbo_node->type == GUMBO_NODE_DOCUMENT ?
                                      &gumbo_node->v.document.children : &gumbo_node->v.element.children;
        if (child_index >= children->length) {
            /* Move up the tree and to the next child. */
            if (xml_node == xml_output_node) {
                /* We've built as much of the tree as we can. */
                return;
            }
            child_index = gumbo_node->index_within_parent + 1;
            gumbo_node = gumbo_node->parent;
            xml_node = xml_node->parent;
            /* Children of fragments don't share the same root, so reset it
             * and it'll be set below. In the non-fragment case, this will
             * only happen after the html element has been finished at which
             * point there are no further elements. */
            if (xml_node == xml_output_node) {
                xml_root = NULL;
            }
            continue;
        }
        const GumboNode *gumbo_child = children->data[child_index++];
        xmlNodePtr xml_child;

        switch (gumbo_child->type) {
        case GUMBO_NODE_DOCUMENT:
            abort(); /* Bug in Gumbo. */

        case GUMBO_NODE_TEXT:
        case GUMBO_NODE_WHITESPACE:
            xml_child = xmlNewDocText(doc, (const xmlChar *)gumbo_child->v.text.text);
            set_line(xml_child, gumbo_child->v.text.start_pos.line);
            xmlAddChild(xml_node, xml_child);
            break;

        case GUMBO_NODE_CDATA:
            xml_child = xmlNewCDataBlock(doc, (const xmlChar *)gumbo_child->v.text.text,
                                         (int)strlen(gumbo_child->v.text.text));
            set_line(xml_child, gumbo_child->v.text.start_pos.line);
            xmlAddChild(xml_node, xml_child);
            break;

        case GUMBO_NODE_COMMENT:
            xml_child = xmlNewDocComment(doc, (const xmlChar *)gumbo_child->v.text.text);
            set_line(xml_child, gumbo_child->v.text.start_pos.line);
            xmlAddChild(xml_node, xml_child);
            break;

        case GUMBO_NODE_TEMPLATE:
        /* XXX: Should create a template element and a new DocumentFragment */
        case GUMBO_NODE_ELEMENT: {
            xml_child = xmlNewDocNode(doc, NULL, (const xmlChar *)gumbo_child->v.element.name, NULL);
            set_line(xml_child, gumbo_child->v.element.start_pos.line);
            if (xml_root == NULL) {
                xml_root = xml_child;
            }
            xmlNsPtr ns = NULL;
            switch (gumbo_child->v.element.tag_namespace) {
            case GUMBO_NAMESPACE_HTML:
                break;
            case GUMBO_NAMESPACE_SVG:
                ns = lookup_or_add_ns(doc, xml_root, "http://www.w3.org/2000/svg", "svg");
                break;
            case GUMBO_NAMESPACE_MATHML:
                ns = lookup_or_add_ns(doc, xml_root, "http://www.w3.org/1998/Math/MathML", "math");
                break;
            }
            if (ns != NULL) {
                xmlSetNs(xml_child, ns);
            }
            xmlAddChild(xml_node, xml_child);

            /* Add the attributes. */
            const GumboVector *attrs = &gumbo_child->v.element.attributes;
            for (size_t i = 0; i < attrs->length; i++) {
                const GumboAttribute *attr = attrs->data[i];

                switch (attr->attr_namespace) {
                case GUMBO_ATTR_NAMESPACE_XLINK:
                    ns = lookup_or_add_ns(doc, xml_root, "http://www.w3.org/1999/xlink", "xlink");
                    break;

                case GUMBO_ATTR_NAMESPACE_XML:
                    ns = lookup_or_add_ns(doc, xml_root, "http://www.w3.org/XML/1998/namespace", "xml");
                    break;

                case GUMBO_ATTR_NAMESPACE_XMLNS:
                    ns = lookup_or_add_ns(doc, xml_root, "http://www.w3.org/2000/xmlns/", "xmlns");
                    break;

                default:
                    ns = NULL;
                }
                xmlNewNsProp(xml_child, ns, (const xmlChar *)attr->name, (const xmlChar *)attr->value);
            }

            /* Add children for this element. */
            child_index = 0;
            gumbo_node = gumbo_child;
            xml_node = xml_child;
        }
        }
    }
}

void
mrb_gumbo_build_document(xmlDocPtr doc, const GumboOutput *output)
{
    build_tree(doc, (xmlNodePtr)doc, output->document);
}

void
mrb_gumbo_build_fragment(xmlDocPtr doc, xmlNodePtr fragment, const GumboOutput *output)
{
    build_tree(doc, fragment, output->root);
}

size_t
mrb_gumbo_error_count(const GumboOutput *output)
{
    return output->errors.length;
}

/* The caret diagnostic of error `i` (malloc'd, `mrb_gumbo_free` it),
 * with its length, code, line and column. */
char *
mrb_gumbo_error(
    const GumboOutput *output, size_t i, const char *input, size_t length,
    size_t *size, const char **code, size_t *line, size_t *column)
{
    const GumboError *err = output->errors.data[i];
    GumboSourcePosition position = gumbo_error_position(err);
    char *msg = NULL;
    *size = gumbo_caret_diagnostic_to_string(err, input, length, &msg);
    *code = gumbo_error_code(err);
    *line = position.line;
    *column = position.column;
    return msg;
}

void
mrb_gumbo_free(void *p)
{
    free(p);
}
