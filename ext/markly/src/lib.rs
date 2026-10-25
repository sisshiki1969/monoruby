//! The native half of the `markly` gem (`markly.so`): cmark-gfm's parser
//! and its HTML / CommonMark / plain-text renderers, on the `comrak`
//! crate. The gem's Ruby half (`Markly.parse`, `Node#each` / `walk` /
//! `to_html`, the `Renderer` classes, the flag constants) is the installed
//! gem's own; `gem/markly/markly.rb` stands in for the extension: it keeps
//! the node tree as Ruby objects (`Markly::Node`, a doubly linked tree the
//! gem edits in place) and hands the tree to these entry points as a
//! nested Array, one row per node:
//!
//! ```text
//! [type, string_content, url, title, header_level, list_type, list_start,
//!  list_tight, list_delim, fence_info, fenced, table_alignments,
//!  table_header?, tasklist_checked, extra, source_position, children]
//! ```
//!
//! `String.__markly_parse(text, flags, extensions)` answers such a row for
//! the document; the three `String.__markly_render_*` entry points take
//! one back, rebuild the comrak tree and format it. Node types are the
//! gem's symbols (`:header`, `:hrule`, `:html`, `:inline_html`, …, with
//! cmark-gfm's extension type strings `:table`, `:table_header`,
//! `:table_row`, `:table_cell`, `:strikethrough`); a comrak node kind the
//! gem has no name for comes through as `:custom_block` /
//! `:custom_inline`, which cmark renders as its children.
//!
//! The plain-text renderer is a port of cmark-gfm's `plaintext.c` over its
//! `renderer.c` line discipline (prefix, `CR`, `BLANKLINE`, tight lists)
//! without the width-based wrapping, which the gem does not enable by
//! default (`to_plaintext(width: 0)`).
//!
//! This is `src/builtins/markly.rs` moved out of the interpreter into a
//! dynamically loaded extension (`markly_native.so`, required by
//! `gem/markly/markly.rb`; doc/native_extension_loading.md, step 3).
use comrak::arena_tree::Node as ArenaNode;
use comrak::nodes::{
    Ast, AstNode, LineColumn, ListDelimType, ListType, NodeCode, NodeCodeBlock,
    NodeFootnoteDefinition, NodeFootnoteReference, NodeHeading, NodeHtmlBlock, NodeLink, NodeList,
    NodeTable, NodeTaskItem, NodeValue, Sourcepos, TableAlignment,
};
use comrak::{Arena, Options};
use monoruby_ext::*;
use std::borrow::Cow;
use std::cell::RefCell;
use std::ffi::c_int;
use std::fmt::Write as _;

#[unsafe(no_mangle)]
pub unsafe extern "C" fn Init_markly_native(ctx: *mut MrContext) -> c_int {
    // SAFETY: the interpreter's contract for `Init_`.
    unsafe { init(ctx, init_markly) }
}

fn init_markly(ctx: &mut Ctx) -> Result<()> {
    let string = ctx.const_get(Value::UNDEF, "String").ok_or(Error)?;
    let s = MR_METHOD_SINGLETON;
    ctx.define_method(string, "__markly_parse", method!(markly_parse), 3, s);
    ctx.define_method(string, "__markly_render_html", method!(render_html), 3, s);
    ctx.define_method(
        string,
        "__markly_render_commonmark",
        method!(render_commonmark),
        3,
        s,
    );
    ctx.define_method(
        string,
        "__markly_render_plaintext",
        method!(render_plaintext),
        3,
        s,
    );
    ctx.define_method(string, "__markly_escape_href", method!(escape_href), 1, s);
    ctx.define_method(string, "__markly_escape_html", method!(escape_html), 1, s);
    Ok(())
}

// The gem's flag bits (`markly/flags.rb`, cmark-gfm's `CMARK_OPT_*`).
const OPT_SOURCEPOS: i64 = 1 << 1;
const OPT_HARDBREAKS: i64 = 1 << 2;
const OPT_NOBREAKS: i64 = 1 << 4;
const OPT_SMART: i64 = 1 << 10;
const OPT_GITHUB_PRE_LANG: i64 = 1 << 11;
const OPT_FOOTNOTES: i64 = 1 << 13;
const OPT_FULL_INFO_STRING: i64 = 1 << 16;
const OPT_UNSAFE: i64 = 1 << 17;
// markly 0.19; `INLINE_CODE_INFO` (1 << 19) and `HTML_BLOCK_BLANK_LINES`
// (1 << 20) have no comrak counterpart.
const OPT_FRONT_MATTER: i64 = 1 << 18;

/// The tuple's slots.
const T_TYPE: usize = 0;
const T_CONTENT: usize = 1;
const T_URL: usize = 2;
const T_TITLE: usize = 3;
const T_LEVEL: usize = 4;
const T_LIST_TYPE: usize = 5;
const T_LIST_START: usize = 6;
const T_LIST_TIGHT: usize = 7;
const T_LIST_DELIM: usize = 8;
const T_FENCE_INFO: usize = 9;
const T_FENCED: usize = 10;
const T_ALIGNMENTS: usize = 11;
const T_TABLE_HEADER: usize = 12;
const T_CHECKED: usize = 13;
const T_EXTRA: usize = 14;
const T_SOURCEPOS: usize = 15;
const T_CHILDREN: usize = 16;
const T_LEN: usize = 17;

/// The elements of `v` if it is an Array.
fn try_ary(ctx: &Ctx, v: Value) -> Option<Vec<Value>> {
    if ctx.type_of(v) != MrType::Array {
        return None;
    }
    Some((0..ctx.ary_len(v)).filter_map(|i| ctx.ary_get(v, i)).collect())
}

/// The elements of `v`; a TypeError if it is not an Array.
fn expect_ary(ctx: &mut Ctx, v: Value) -> Result<Vec<Value>> {
    match try_ary(ctx, v) {
        Some(a) => Ok(a),
        None => {
            let class = ctx.class_name(v);
            Err(ctx.type_error(format!("no implicit conversion of {class} into Array")))
        }
    }
}

/// The name of `v` if it is a Symbol.
fn try_sym(ctx: &mut Ctx, v: Value) -> Option<String> {
    if ctx.type_of(v) != MrType::Symbol {
        return None;
    }
    ctx.sym_name(v).ok()
}

/// The bytes of a String, as UTF-8 (lossily).
fn lossy(ctx: &Ctx, v: Value) -> Result<String> {
    ctx.str_string(v)
}

/// The comrak options a flag word and an extension list select.
fn options(flags: i64, extensions: &[String], width: usize) -> Options<'static> {
    let mut o = Options::default();
    // cmark-gfm's parser (the gem's only one).
    o.render.gfm_quirks = true;
    o.parse.smart = flags & OPT_SMART != 0;
    o.extension.footnotes = flags & OPT_FOOTNOTES != 0;
    o.render.sourcepos = flags & OPT_SOURCEPOS != 0;
    o.render.hardbreaks = flags & OPT_HARDBREAKS != 0;
    o.render.github_pre_lang = flags & OPT_GITHUB_PRE_LANG != 0;
    o.render.full_info_string = flags & OPT_FULL_INFO_STRING != 0;
    o.render.r#unsafe = flags & OPT_UNSAFE != 0;
    if flags & OPT_FRONT_MATTER != 0 {
        o.extension.front_matter_delimiter = Some("---".to_string());
    }
    o.render.width = width;
    for ext in extensions {
        match ext.as_str() {
            "table" => o.extension.table = true,
            "strikethrough" => o.extension.strikethrough = true,
            "autolink" => o.extension.autolink = true,
            #[allow(deprecated)]
            "tagfilter" => o.extension.tagfilter = true,
            "tasklist" => o.extension.tasklist = true,
            _ => {}
        }
    }
    o
}

fn extension_names(ctx: &mut Ctx, v: Value) -> Result<Vec<String>> {
    let ary = expect_ary(ctx, v)?;
    ary.into_iter()
        .map(|e| match try_sym(ctx, e) {
            Some(name) => Ok(name),
            None => {
                let class = ctx.class_name(e);
                Err(ctx.type_error(format!(
                    "extension names should be Symbols; got a {class}"
                )))
            }
        })
        .collect()
}

///
/// ### String.__markly_parse(text, flags, extensions)
///
/// Parses `text` (any bytes; invalid UTF-8 is replaced, as the gem's
/// `VALIDATE_UTF8` does) and answers the document's tuple.
///
fn markly_parse(ctx: &mut Ctx, _: Value, args: &[Value], _: Block) -> Result<Value> {
    let text = lossy(ctx, args[0])?;
    let flags = ctx.int(args[1])?;
    let extensions = extension_names(ctx, args[2])?;
    let opts = options(flags, &extensions, 0);
    let arena = Arena::new();
    let root = comrak::parse_document(&arena, &text, &opts);
    Ok(to_tuple(ctx, root))
}

fn render_with(
    ctx: &mut Ctx,
    args: &[Value],
    width: usize,
    f: impl for<'a> FnOnce(&'a AstNode<'a>, &Options<'_>) -> String,
) -> Result<Value> {
    let flags = ctx.int(args[1])?;
    let extensions = if ctx.type_of(args[2]) == MrType::Array {
        extension_names(ctx, args[2])?
    } else {
        vec![]
    };
    let mut opts = options(flags, &extensions, width);
    // The rendered node kinds come from the tree, not from the option
    // set: a table parsed with the extension renders as a table whatever
    // list `to_html` was given (cmark's render list only drives tagfilter).
    opts.extension.table = true;
    opts.extension.strikethrough = true;
    opts.extension.tasklist = true;
    opts.extension.autolink = true;
    let arena = Arena::new();
    let (root, unwrap_paragraph) = from_tuple(ctx, &arena, args[0])?;
    if let Err(e) = root.validate() {
        return Err(ctx.runtime_error(format!(
            "could not render: ill-formed node tree: {e:?}"
        )));
    }
    let mut out = f(root, &opts);
    if unwrap_paragraph && out.starts_with("<p>") && out.ends_with("</p>\n") {
        // The fragment's inline children were rendered inside a paragraph
        // that is not theirs (see `from_tuple`).
        out = out[3..out.len() - 5].to_string();
    }
    Ok(ctx.str(out))
}

fn width_arg(ctx: &mut Ctx, v: Value) -> Result<usize> {
    if v.is_nil() {
        Ok(120)
    } else {
        Ok(ctx.int(v)?.max(0) as usize)
    }
}

/// Render a fenced or indented code block the way cmark-gfm does.
///
/// comrak's own renderer differs from it in three ways that show in the
/// gem comparison, all of them here rather than in a hook, because the
/// only hook it offers (`codefence_syntax_highlighter`) is handed the
/// attributes already computed:
///
/// 1. It collects the attributes in a `HashMap` and writes them in its
///    iteration order, so a tag carrying two of them came out in an
///    order that followed the per-process hash seed.
/// 2. It trims the whole meta string, where cmark-gfm drops exactly the
///    one space that ended the language and keeps the rest — so
///    ` ```rb  x y ` is `data-meta=" x y"` there and `"x y"` here.
/// 3. It renders a ```` ```math ```` block as its own math markup
///    (`data-math-style`), which cmark-gfm, having no math extension,
///    does not.
///
/// Everything else follows comrak's `render_code_block`: the leading
/// `cr`, the escaped literal, the closing tags, the trailing `lf`. The
/// codefence plugins it consults there are not consulted here, since
/// monoruby installs none.
fn render_code_block<T>(
    context: &mut comrak::html::Context<T>,
    node: &comrak::nodes::AstNode<'_>,
    ncb: &NodeCodeBlock,
) -> std::fmt::Result {
    context.cr()?;

    // cmark-gfm's split: the language is up to the first whitespace, and
    // the meta string is everything after that one byte — not trimmed.
    // The parser has already stripped the info string's trailing
    // whitespace, so no trailing run survives to matter.
    let info = ncb.info.as_str();
    let first_tag = info.find(|c: char| c.is_whitespace()).unwrap_or(info.len());
    let lang = &info[..first_tag];
    let meta = if first_tag < info.len() {
        Some(&info[first_tag + 1..])
    } else {
        None
    };

    // In cmark-gfm's order: on `<pre>` the source position, the
    // language and the meta string; on `<code>` the class and the meta
    // string. `Vec`, so the order is the order.
    let mut pre_attributes: Vec<(&str, Cow<'_, str>)> = Vec::new();
    let mut code_attributes: Vec<(&str, Cow<'_, str>)> = Vec::new();
    if context.options.render.sourcepos {
        pre_attributes.push(("data-sourcepos", node.data().sourcepos.to_string().into()));
    }
    if !info.is_empty() {
        let full_info = context.options.render.full_info_string;
        if context.options.render.github_pre_lang {
            pre_attributes.push(("lang", lang.into()));
            if let Some(meta) = meta
                && full_info
            {
                pre_attributes.push(("data-meta", meta.into()));
            }
        } else {
            code_attributes.push(("class", format!("language-{lang}").into()));
            if let Some(meta) = meta
                && full_info
            {
                code_attributes.push(("data-meta", meta.into()));
            }
        }
    }

    comrak::html::write_opening_tag(context, "pre", pre_attributes)?;
    comrak::html::write_opening_tag(context, "code", code_attributes)?;
    context.escape(&ncb.literal)?;
    context.write_str("</code></pre>")?;
    context.lf()
}

comrak::create_formatter!(MarklyHtmlFormatter, {
    NodeValue::CodeBlock(ref ncb) => |context, node, entering| {
        if entering {
            render_code_block(context, node, ncb)?;
        }
    },
});

///
/// ### String.__markly_render_html(tuple, flags, extensions)
///
fn render_html(ctx: &mut Ctx, _: Value, args: &[Value], _: Block) -> Result<Value> {
    render_with(ctx, args, 0, |root, opts| {
        let mut out = String::new();
        let _ = MarklyHtmlFormatter::format_document(root, opts, &mut out);
        out
    })
}

///
/// ### String.__markly_render_commonmark(tuple, flags, width)
///
fn render_commonmark(ctx: &mut Ctx, _: Value, args: &[Value], _: Block) -> Result<Value> {
    let width = width_arg(ctx, args[2])?;
    render_with(ctx, args, width, |root, opts| {
        let mut out = String::new();
        let _ = comrak::format_commonmark(root, opts, &mut out);
        out
    })
}

///
/// ### String.__markly_render_plaintext(tuple, flags, width)
///
fn render_plaintext(ctx: &mut Ctx, _: Value, args: &[Value], _: Block) -> Result<Value> {
    let width = width_arg(ctx, args[2])?;
    let flags = ctx.int(args[1])?;
    render_with(ctx, args, width, |root, _| {
        let mut r = Plain::new(flags, width);
        r.render(root);
        r.finish()
    })
}

///
/// ### String.__markly_escape_href(str)
///
fn escape_href(ctx: &mut Ctx, _: Value, args: &[Value], _: Block) -> Result<Value> {
    let s = ctx.str_bytes(args[0])?;
    let Ok(s) = std::str::from_utf8(s) else {
        return Err(ctx.argument_error("invalid byte sequence in UTF-8"));
    };
    let mut out = String::new();
    let _ = comrak::html::escape_href(&mut out, s, false);
    Ok(ctx.str(out))
}

///
/// ### String.__markly_escape_html(str)
///
fn escape_html(ctx: &mut Ctx, _: Value, args: &[Value], _: Block) -> Result<Value> {
    let s = ctx.str_bytes(args[0])?;
    let Ok(s) = std::str::from_utf8(s) else {
        return Err(ctx.argument_error("invalid byte sequence in UTF-8"));
    };
    let mut out = String::new();
    let _ = comrak::html::escape(&mut out, s);
    Ok(ctx.str(out))
}

// ---------------------------------------------------------------------------
// comrak tree → tuple
// ---------------------------------------------------------------------------

fn to_tuple<'a>(ctx: &Ctx, node: &'a AstNode<'a>) -> Value {
    let ast = node.data();
    let mut t = vec![Value::nil(); T_LEN];
    // cmark leaves the two break kinds unpositioned.
    let sp = match ast.value {
        NodeValue::SoftBreak | NodeValue::LineBreak => Sourcepos::from((0, 0, 0, 0)),
        _ => ast.sourcepos,
    };
    t[T_SOURCEPOS] = ctx.ary_from(&vec![
        Value::int(sp.start.line as i64),
        Value::int(sp.start.column as i64),
        Value::int(sp.end.line as i64),
        Value::int(sp.end.column as i64),
    ]);
    let ty = match &ast.value {
        NodeValue::Document => "document",
        NodeValue::BlockQuote | NodeValue::MultilineBlockQuote(_) => "blockquote",
        NodeValue::List(l) => {
            list_fields(ctx, &mut t, l);
            "list"
        }
        NodeValue::Item(_) => "list_item",
        NodeValue::TaskItem(ti) => {
            t[T_CHECKED] = Value::bool(ti.symbol.is_some());
            "list_item"
        }
        NodeValue::CodeBlock(cb) => {
            t[T_CONTENT] = ctx.str(&cb.literal);
            t[T_FENCE_INFO] = ctx.str(&cb.info);
            t[T_FENCED] = Value::bool(cb.fenced);
            t[T_EXTRA] = ctx.ary_from(&vec![
                ctx.str((cb.fence_char as char).to_string()),
                Value::int(cb.fence_length as i64),
                Value::int(cb.fence_offset as i64),
            ]);
            "code_block"
        }
        NodeValue::FrontMatter(s) => {
            // comrak keeps the delimiter lines; cmark-gfm's node holds only
            // what lies between them.
            let mut lines: Vec<&str> = s.lines().collect();
            if lines.first().is_some_and(|l| l.trim_end() == "---") {
                lines.remove(0);
            }
            if let Some(end) = lines.iter().position(|l| l.trim_end() == "---") {
                lines.truncate(end);
            }
            let mut inner = lines.join("\n");
            if !lines.is_empty() {
                inner.push('\n');
            }
            t[T_CONTENT] = ctx.str(inner);
            "front_matter"
        }
        NodeValue::HtmlBlock(hb) => {
            t[T_CONTENT] = ctx.str(&hb.literal);
            "html"
        }
        NodeValue::Paragraph => "paragraph",
        NodeValue::Heading(h) => {
            t[T_LEVEL] = Value::int(h.level as i64);
            "header"
        }
        NodeValue::ThematicBreak => "hrule",
        NodeValue::FootnoteDefinition(fd) => {
            t[T_CONTENT] = ctx.str(&fd.name);
            t[T_EXTRA] = Value::int(fd.total_references as i64);
            "footnote_definition"
        }
        NodeValue::Table(tb) => {
            t[T_ALIGNMENTS] = ctx.ary_from(&
                tb.alignments
                    .iter()
                    .map(|a| match a {
                        TableAlignment::None => Value::nil(),
                        TableAlignment::Left => ctx.sym("left"),
                        TableAlignment::Center => ctx.sym("center"),
                        TableAlignment::Right => ctx.sym("right"),
                    })
                    .collect::<Vec<_>>(),
            );
            "table"
        }
        NodeValue::TableRow(header) => {
            t[T_TABLE_HEADER] = Value::bool(*header);
            if *header { "table_header" } else { "table_row" }
        }
        NodeValue::TableCell => "table_cell",
        NodeValue::Text(s) => {
            t[T_CONTENT] = ctx.str(s.as_bytes());
            "text"
        }
        NodeValue::SoftBreak => "softbreak",
        NodeValue::LineBreak => "linebreak",
        NodeValue::Code(c) => {
            t[T_CONTENT] = ctx.str(&c.literal);
            "code"
        }
        NodeValue::HtmlInline(s) => {
            t[T_CONTENT] = ctx.str(s);
            "inline_html"
        }
        NodeValue::Raw(s) => {
            t[T_CONTENT] = ctx.str(s);
            "inline_html"
        }
        NodeValue::Emph => "emph",
        NodeValue::Strong => "strong",
        NodeValue::Strikethrough => "strikethrough",
        NodeValue::Link(l) => {
            t[T_URL] = ctx.str(&l.url);
            t[T_TITLE] = ctx.str(&l.title);
            "link"
        }
        NodeValue::Image(l) => {
            t[T_URL] = ctx.str(&l.url);
            t[T_TITLE] = ctx.str(&l.title);
            "image"
        }
        NodeValue::FootnoteReference(fr) => {
            // cmark rewrites a reference's literal to the footnote's
            // index once the definitions are resolved; the label lives
            // on the definition (and in `extra` here, so the Ruby side
            // can find the definition for `parent_footnote_def`).
            t[T_CONTENT] = ctx.str(fr.ix.to_string());
            t[T_EXTRA] = ctx.ary_from(&vec![
                Value::int(fr.ref_num as i64),
                Value::int(fr.ix as i64),
                ctx.str(&fr.name),
            ]);
            "footnote_reference"
        }
        other => {
            if other.block() {
                "custom_block"
            } else {
                "custom_inline"
            }
        }
    };
    t[T_TYPE] = ctx.sym(ty);
    t[T_CHILDREN] = ctx.ary_from(&node.children().map(|c| to_tuple(ctx, c)).collect::<Vec<_>>());
    ctx.ary_from(&t)
}

fn list_fields(ctx: &Ctx, t: &mut [Value], l: &NodeList) {
    t[T_LIST_TYPE] = ctx.sym(match l.list_type {
        ListType::Bullet => "bullet_list",
        ListType::Ordered => "ordered_list",
    });
    t[T_LIST_START] = Value::int(l.start as i64);
    t[T_LIST_TIGHT] = Value::bool(l.tight);
    t[T_LIST_DELIM] = ctx.sym(match l.delimiter {
        ListDelimType::Period => "period",
        ListDelimType::Paren => "paren",
    });
}

// ---------------------------------------------------------------------------
// tuple → comrak tree
// ---------------------------------------------------------------------------

struct Row {
    ty: String,
    t: Vec<Value>,
}

impl Row {
    fn new(ctx: &mut Ctx, v: Value) -> Result<Row> {
        let t = expect_ary(ctx, v)?;
        if t.len() != T_LEN {
            return Err(ctx.argument_error("malformed Markly node tuple"));
        }
        let Some(ty) = try_sym(ctx, t[T_TYPE]) else {
            let class = ctx.class_name(t[T_TYPE]);
            return Err(ctx.type_error(format!("no implicit conversion of {class} into Symbol")));
        };
        Ok(Row { ty, t })
    }

    fn str(&self, ctx: &mut Ctx, i: usize) -> Result<String> {
        let v = self.t[i];
        if v.is_nil() {
            Ok(String::new())
        } else {
            lossy(ctx, v)
        }
    }

    fn int(&self, ctx: &mut Ctx, i: usize, default: i64) -> Result<i64> {
        let v = self.t[i];
        if v.is_nil() { Ok(default) } else { ctx.int(v) }
    }

    fn truthy(&self, i: usize) -> bool {
        self.t[i].truthy()
    }

    fn sym_is(&self, ctx: &mut Ctx, i: usize, name: &str) -> bool {
        try_sym(ctx, self.t[i]).is_some_and(|s| s == name)
    }

    fn sourcepos(&self, ctx: &mut Ctx) -> Result<Sourcepos> {
        if let Some(a) = try_ary(ctx, self.t[T_SOURCEPOS])
            && a.len() == 4
        {
            let mut n = |i: usize| -> Result<usize> { Ok(ctx.int(a[i])?.max(0) as usize) };
            return Ok(Sourcepos {
                start: LineColumn {
                    line: n(0)?,
                    column: n(1)?,
                },
                end: LineColumn {
                    line: n(2)?,
                    column: n(3)?,
                },
            });
        }
        Ok((0, 0, 0, 0).into())
    }
}

fn list_data(ctx: &mut Ctx, row: &Row, inherited: Option<NodeList>) -> Result<NodeList> {
    let mut l = inherited.unwrap_or_default();
    if row.ty == "list" {
        l.list_type = if row.sym_is(ctx, T_LIST_TYPE, "ordered_list") {
            ListType::Ordered
        } else {
            ListType::Bullet
        };
        l.start = row.int(ctx, T_LIST_START, 1)?.max(0) as usize;
        l.tight = row.truthy(T_LIST_TIGHT);
        l.delimiter = if row.sym_is(ctx, T_LIST_DELIM, "paren") {
            ListDelimType::Paren
        } else {
            ListDelimType::Period
        };
        l.bullet_char = b'-';
        l.marker_offset = 0;
        l.padding = 2;
    }
    Ok(l)
}

const INLINE_TYPES: &[&str] = &[
    "text",
    "softbreak",
    "linebreak",
    "code",
    "inline_html",
    "emph",
    "strong",
    "strikethrough",
    "link",
    "image",
    "footnote_reference",
    "custom_inline",
];

/// The comrak root for a tuple. A custom node at the root (the gem's
/// `extract_children` fragment) renders as its children, which need a
/// parent to hang from: a document for blocks, a paragraph for inlines
/// (comrak refuses inlines directly under a document) — the second
/// answer says the paragraph's own tags are to be stripped.
fn from_tuple<'a>(
    ctx: &mut Ctx,
    arena: &'a Arena<'a>,
    v: Value,
) -> Result<(&'a AstNode<'a>, bool)> {
    let row = Row::new(ctx, v)?;
    if row.ty == "custom_block" || row.ty == "custom_inline" {
        let inline = match try_ary(ctx, row.t[T_CHILDREN]) {
            Some(children) => children.into_iter().all(|c| {
                try_ary(ctx, c)
                    .and_then(|r| r.first().and_then(|&t| try_sym(ctx, t)))
                    .is_some_and(|ty| INLINE_TYPES.contains(&ty.as_str()))
            }),
            None => false,
        };
        let value = if inline {
            NodeValue::Paragraph
        } else {
            NodeValue::Document
        };
        let holder = arena.alloc(ArenaNode::new(RefCell::new(Ast::new_with_sourcepos(
            value,
            row.sourcepos(ctx)?,
        ))));
        append_children(ctx, arena, holder, &row, None)?;
        return Ok((holder, inline));
    }
    let root = build(ctx, arena, &row, None)?;
    // comrak's renderers read a list item's or a table cell's ancestors;
    // a detached one (the gem's `dup` of an item) gets a synthetic parent
    // to render under, as cmark tolerates the missing one.
    let holder = |value: NodeValue| {
        arena.alloc(ArenaNode::new(RefCell::new(Ast::new_with_sourcepos(
            value,
            (0, 0, 0, 0).into(),
        ))))
    };
    match row.ty.as_str() {
        "list_item" | "item" => {
            // Loose: cmark renders a parentless item's paragraphs as `<p>`.
            let list = holder(NodeValue::List(NodeList {
                tight: false,
                bullet_char: b'-',
                padding: 2,
                ..NodeList::default()
            }));
            list.append(root);
        }
        "table_header" | "table_row" => {
            let table = holder(NodeValue::Table(Box::new(NodeTable::default())));
            table.append(root);
        }
        "table_cell" => {
            let table = holder(NodeValue::Table(Box::new(NodeTable::default())));
            let tr = holder(NodeValue::TableRow(false));
            table.append(tr);
            tr.append(root);
        }
        _ => {}
    }
    Ok((root, false))
}

fn build<'a>(
    ctx: &mut Ctx,
    arena: &'a Arena<'a>,
    row: &Row,
    parent_list: Option<NodeList>,
) -> Result<&'a AstNode<'a>> {
    let mut list = parent_list;
    let value = match row.ty.as_str() {
        "document" => NodeValue::Document,
        "blockquote" | "block_quote" => NodeValue::BlockQuote,
        "list" => {
            let l = list_data(ctx, row, None)?;
            list = Some(l);
            NodeValue::List(l)
        }
        "list_item" | "item" => {
            let checked = row.t[T_CHECKED];
            if checked.is_nil() {
                NodeValue::Item(list_data(ctx, row, parent_list)?)
            } else {
                NodeValue::TaskItem(NodeTaskItem {
                    symbol: if checked.truthy() { Some('x') } else { None },
                    symbol_sourcepos: (0, 0, 0, 0).into(),
                })
            }
        }
        "code_block" => {
            let fenced = row.t[T_FENCED].is_nil() || row.truthy(T_FENCED);
            let (fence_char, fence_length, fence_offset) = match try_ary(ctx, row.t[T_EXTRA]) {
                Some(a) if a.len() == 3 => (
                    ctx.str_bytes(a[0])?.first().copied().unwrap_or(b'`'),
                    ctx.int(a[1])?.max(0) as usize,
                    ctx.int(a[2])?.max(0) as usize,
                ),
                _ => (b'`', 3, 0),
            };
            NodeValue::CodeBlock(Box::new(NodeCodeBlock {
                fenced,
                fence_char,
                fence_length,
                fence_offset,
                info: row.str(ctx, T_FENCE_INFO)?,
                literal: row.str(ctx, T_CONTENT)?,
                closed: true,
            }))
        }
        "front_matter" => NodeValue::FrontMatter(row.str(ctx, T_CONTENT)?),
        "html" | "html_block" => NodeValue::HtmlBlock(NodeHtmlBlock {
            block_type: 6,
            literal: row.str(ctx, T_CONTENT)?,
        }),
        "paragraph" => NodeValue::Paragraph,
        "header" | "heading" => NodeValue::Heading(NodeHeading {
            level: row.int(ctx, T_LEVEL, 1)?.clamp(1, 6) as u8,
            setext: false,
            closed: true,
        }),
        "hrule" | "thematic_break" => NodeValue::ThematicBreak,
        "footnote_definition" => NodeValue::FootnoteDefinition(NodeFootnoteDefinition {
            name: row.str(ctx, T_CONTENT)?,
            total_references: row.int(ctx, T_EXTRA, 1)?.max(0) as u32,
        }),
        "table" => {
            let alignments: Vec<TableAlignment> = match try_ary(ctx, row.t[T_ALIGNMENTS]) {
                Some(a) => a
                    .into_iter()
                    .map(|v| match try_sym(ctx, v).as_deref() {
                        Some("left") => TableAlignment::Left,
                        Some("center") => TableAlignment::Center,
                        Some("right") => TableAlignment::Right,
                        _ => TableAlignment::None,
                    })
                    .collect(),
                None => vec![],
            };
            NodeValue::Table(Box::new(NodeTable {
                num_columns: alignments.len(),
                alignments,
                num_rows: 0,
                num_nonempty_cells: 0,
            }))
        }
        "table_header" => NodeValue::TableRow(true),
        "table_row" => NodeValue::TableRow(row.truthy(T_TABLE_HEADER)),
        "table_cell" => NodeValue::TableCell,
        "text" => NodeValue::Text(row.str(ctx, T_CONTENT)?.into()),
        "softbreak" => NodeValue::SoftBreak,
        "linebreak" => NodeValue::LineBreak,
        "code" => NodeValue::Code(NodeCode {
            num_backticks: 1,
            literal: row.str(ctx, T_CONTENT)?,
        }),
        "inline_html" | "html_inline" => NodeValue::HtmlInline(row.str(ctx, T_CONTENT)?),
        "emph" => NodeValue::Emph,
        "strong" => NodeValue::Strong,
        "strikethrough" => NodeValue::Strikethrough,
        "link" => NodeValue::Link(Box::new(NodeLink {
            url: row.str(ctx, T_URL)?,
            title: row.str(ctx, T_TITLE)?,
        })),
        "image" => NodeValue::Image(Box::new(NodeLink {
            url: row.str(ctx, T_URL)?,
            title: row.str(ctx, T_TITLE)?,
        })),
        "footnote_reference" => {
            let (ref_num, ix, name) = match try_ary(ctx, row.t[T_EXTRA]) {
                Some(a) if a.len() == 3 => (
                    ctx.int(a[0])?.max(0) as u32,
                    ctx.int(a[1])?.max(0) as u32,
                    lossy(ctx, a[2])?,
                ),
                _ => (1, 1, row.str(ctx, T_CONTENT)?),
            };
            NodeValue::FootnoteReference(Box::new(NodeFootnoteReference {
                name,
                texts: vec![],
                ref_num,
                ix,
            }))
        }
        _ => {
            // custom_block / custom_inline: cmark renders nothing of its
            // own for these, only the children; splice them into the
            // parent instead of allocating a node.
            let holder = arena.alloc(ArenaNode::new(RefCell::new(Ast::new_with_sourcepos(
                NodeValue::Document,
                row.sourcepos(ctx)?,
            ))));
            append_children(ctx, arena, holder, row, parent_list)?;
            return Ok(holder);
        }
    };
    let node = arena.alloc(ArenaNode::new(RefCell::new(Ast::new_with_sourcepos(
        value,
        row.sourcepos(ctx)?,
    ))));
    append_children(ctx, arena, node, row, list)?;
    Ok(node)
}

fn append_children<'a>(
    ctx: &mut Ctx,
    arena: &'a Arena<'a>,
    node: &'a AstNode<'a>,
    row: &Row,
    list: Option<NodeList>,
) -> Result<()> {
    let Some(children) = try_ary(ctx, row.t[T_CHILDREN]) else {
        return Ok(());
    };
    for c in children {
        let child_row = Row::new(ctx, c)?;
        if child_row.ty == "custom_block" || child_row.ty == "custom_inline" {
            // Transparent: its children become ours.
            let holder = build(ctx, arena, &child_row, list)?;
            while let Some(grandchild) = holder.first_child() {
                grandchild.detach();
                node.append(grandchild);
            }
        } else {
            node.append(build(ctx, arena, &child_row, list)?);
        }
    }
    Ok(())
}

// ---------------------------------------------------------------------------
// plain text (cmark-gfm plaintext.c over renderer.c)
// ---------------------------------------------------------------------------

struct Plain {
    flags: i64,
    width: usize,
    buf: String,
    prefix: String,
    need_cr: u8,
    begin_line: bool,
    in_tight_list_item: bool,
    no_linebreaks: bool,
    footnote_ix: u32,
}

impl Plain {
    fn new(flags: i64, width: usize) -> Self {
        Plain {
            flags,
            width,
            buf: String::new(),
            prefix: String::new(),
            need_cr: 0,
            begin_line: true,
            in_tight_list_item: false,
            no_linebreaks: false,
            footnote_ix: 0,
        }
    }

    fn cr(&mut self) {
        if self.need_cr < 1 {
            self.need_cr = 1;
        }
    }

    fn blankline(&mut self) {
        if self.need_cr < 2 {
            self.need_cr = 2;
        }
    }

    /// renderer.c `S_out` for a literal string (no wrapping, no escaping).
    fn out(&mut self, s: &str) {
        if self.in_tight_list_item && self.need_cr > 1 {
            self.need_cr = 1;
        }
        let mut k = self.buf.len() as isize - 1;
        while self.need_cr > 0 {
            if k < 0 || self.buf.as_bytes()[k as usize] == b'\n' {
                k -= 1;
            } else {
                self.buf.push('\n');
                if self.need_cr > 1 {
                    self.buf.push_str(&self.prefix);
                }
            }
            self.begin_line = true;
            self.need_cr -= 1;
        }
        for ch in s.chars() {
            if self.begin_line {
                self.buf.push_str(&self.prefix);
            }
            if ch == '\n' {
                self.buf.push('\n');
                self.begin_line = true;
            } else {
                self.buf.push(ch);
                self.begin_line = false;
            }
        }
    }

    fn finish(mut self) -> String {
        if !self.buf.is_empty() && !self.buf.ends_with('\n') {
            self.buf.push('\n');
        }
        self.buf
    }

    fn render<'a>(&mut self, node: &'a AstNode<'a>) {
        self.node(node, true);
        for child in node.children() {
            self.render(child);
        }
        self.node(node, false);
    }

    fn parent_list<'a>(node: &'a AstNode<'a>) -> Option<NodeList> {
        let p = node.parent()?;
        match &p.data().value {
            NodeValue::List(l) => Some(*l),
            _ => None,
        }
    }

    fn item_index<'a>(node: &'a AstNode<'a>) -> usize {
        let mut i = 0;
        let mut n = Some(node);
        while let Some(x) = n {
            i += 1;
            n = x.previous_sibling();
        }
        i
    }

    fn is_item(v: &NodeValue) -> bool {
        matches!(v, NodeValue::Item(_) | NodeValue::TaskItem(_))
    }

    fn node<'a>(&mut self, node: &'a AstNode<'a>, entering: bool) {
        let hardbreaks = self.flags & OPT_HARDBREAKS != 0;
        let nobreaks = self.flags & OPT_NOBREAKS != 0;
        // Tight-list bookkeeping (see plaintext.c).
        if entering {
            if let Some(p) = node.parent()
                && Self::is_item(&p.data().value)
                && let Some(l) = Self::parent_list(p)
            {
                self.in_tight_list_item = l.tight;
            }
        } else if matches!(node.data().value, NodeValue::List(_)) {
            self.in_tight_list_item = node
                .parent()
                .filter(|p| Self::is_item(&p.data().value))
                .and_then(Self::parent_list)
                .is_some_and(|l| l.tight);
        }
        let value = node.data().value.clone();
        match value {
            NodeValue::List(_) => {
                if !entering
                    && let Some(next) = node.next_sibling()
                    && matches!(
                        next.data().value,
                        NodeValue::CodeBlock(_) | NodeValue::List(_)
                    )
                {
                    self.cr();
                }
            }
            NodeValue::Strikethrough => self.out("~"),
            NodeValue::TaskItem(ti) => {
                if entering {
                    self.cr();
                    self.out(if ti.symbol.is_some() { "- [x] " } else { "- [ ] " });
                    self.prefix.push_str("  ");
                } else {
                    let len = self.prefix.len().saturating_sub(2);
                    self.prefix.truncate(len);
                    self.cr();
                }
            }
            NodeValue::Item(_) => {
                let list = Self::parent_list(node).unwrap_or_default();
                let (marker, marker_width) = if list.list_type == ListType::Bullet {
                    ("  - ".to_string(), 4)
                } else {
                    let n = Self::item_index(node) + list.start.saturating_sub(1);
                    let m = format!(
                        "{}{}{}",
                        n,
                        if list.delimiter == ListDelimType::Paren { ")" } else { "." },
                        if n < 10 { "  " } else { " " }
                    );
                    let w = m.len();
                    (m, w)
                };
                if entering {
                    self.out(&marker);
                    for _ in 0..marker_width {
                        self.prefix.push(' ');
                    }
                } else {
                    let len = self.prefix.len().saturating_sub(marker_width);
                    self.prefix.truncate(len);
                    self.cr();
                }
            }
            NodeValue::Heading(_) => {
                if entering {
                    self.no_linebreaks = true;
                } else {
                    self.no_linebreaks = false;
                    self.blankline();
                }
            }
            NodeValue::CodeBlock(cb) => {
                if entering {
                    let first_in_item = node.previous_sibling().is_none()
                        && node.parent().is_some_and(|p| Self::is_item(&p.data().value));
                    if !first_in_item {
                        self.blankline();
                    }
                    self.out(&cb.literal);
                    self.blankline();
                }
            }
            NodeValue::ThematicBreak => {
                if entering {
                    self.blankline();
                }
            }
            NodeValue::Paragraph => {
                if !entering {
                    self.blankline();
                }
            }
            NodeValue::Text(s) => {
                if entering {
                    self.out(&s);
                }
            }
            NodeValue::LineBreak => {
                if entering {
                    self.cr();
                }
            }
            NodeValue::SoftBreak => {
                if entering {
                    if hardbreaks || (!self.no_linebreaks && self.width == 0 && !nobreaks) {
                        self.cr();
                    } else {
                        self.out(" ");
                    }
                }
            }
            NodeValue::Code(c) => {
                if entering {
                    self.out(&c.literal);
                }
            }
            NodeValue::FootnoteReference(fr) => {
                if entering {
                    self.out("[^");
                    self.out(&fr.name);
                    self.out("]");
                }
            }
            NodeValue::FootnoteDefinition(_) => {
                if entering {
                    self.footnote_ix += 1;
                    self.out("[^");
                    self.out(&self.footnote_ix.to_string());
                    self.out("]: ");
                    self.prefix.push_str("    ");
                } else {
                    let len = self.prefix.len().saturating_sub(4);
                    self.prefix.truncate(len);
                }
            }
            NodeValue::Table(_) => self.blankline(),
            NodeValue::TableRow(_) => {
                if entering {
                    self.cr();
                    self.out("|");
                }
            }
            NodeValue::TableCell => {
                if entering {
                    self.out(" ");
                } else {
                    self.out(" |");
                    // The header row closes with the alignment line.
                    let Some(row) = node.parent() else { return };
                    let is_header = matches!(row.data().value, NodeValue::TableRow(true));
                    if is_header && node.next_sibling().is_none() {
                        let alignments = row.parent().map_or(vec![], |t| match &t.data().value {
                            NodeValue::Table(tb) => tb.alignments.clone(),
                            _ => vec![],
                        });
                        self.cr();
                        self.out("|");
                        for a in alignments {
                            self.out(match a {
                                TableAlignment::None => " --- |",
                                TableAlignment::Left => " :-- |",
                                TableAlignment::Center => " :-: |",
                                TableAlignment::Right => " --: |",
                            });
                        }
                        self.cr();
                    }
                }
            }
            _ => {}
        }
    }
}
