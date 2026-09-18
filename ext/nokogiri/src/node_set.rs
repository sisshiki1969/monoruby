//! `Nokogiri::XML::NodeSet`: an `xmlNodeSet` owned by the Ruby object.

use crate::*;

/// The payload of a `NodeSet`.
pub(crate) struct XmlNodeSet {
    pub set: *mut xml::xmlNodeSet,
}

impl XmlNodeSet {
    pub fn empty() -> Self {
        // SAFETY: plain allocation.
        let set = unsafe { xml::xmlXPathNodeSetCreate(std::ptr::null_mut()) };
        XmlNodeSet { set }
    }

    pub fn len(&self) -> usize {
        if self.set.is_null() {
            0
        } else {
            // SAFETY: a live set.
            unsafe { (*self.set).nodeNr.max(0) as usize }
        }
    }

    pub fn node(&self, i: usize) -> *mut xml::xmlNode {
        // SAFETY: `i` is below `len()`.
        unsafe { *(*self.set).nodeTab.add(i) }
    }
}

native!(
    XmlNodeSet,
    "Nokogiri::XML::NodeSet",
    |set, m| {
        // The Ruby objects of the member nodes (`xml_node_set_mark`).
        for i in 0..set.len() {
            let node = set.node(i);
            // SAFETY: members are live nodes of live documents.
            unsafe {
                if let Some(v) = ruby_object_of(node) {
                    m.mark(v);
                }
            }
        }
    },
    // `Object#dup` / `#clone`: an empty set that `initialize_copy`
    // fills (nokogiri allocates the copy with `xml_node_set_allocate`).
    |_set| Some(XmlNodeSet::empty())
);

impl Drop for XmlNodeSet {
    fn drop(&mut self) {
        if self.set.is_null() {
            return;
        }
        // The set's array and header only: member namespace copies are
        // owned by their `Namespace` objects (`xml_node_set_deallocate`).
        // SAFETY: allocated by libxml2, ours to free.
        unsafe {
            let free = xml::xml_free();
            if !(*self.set).nodeTab.is_null() {
                free((*self.set).nodeTab as *mut c_void);
            }
            free(self.set as *mut c_void);
        }
    }
}

/// The Ruby object already wrapping `node`, if any (`ruby_object_get`).
pub(crate) unsafe fn ruby_object_of(node: *mut xml::xmlNode) -> Option<Value> {
    // SAFETY: a live node; `_private` holds Value bits or NULL.
    unsafe {
        let p = match (*node).type_ {
            xml::XML_NAMESPACE_DECL => (*(node as *mut xml::xmlNs))._private,
            xml::XML_DOCUMENT_NODE | xml::XML_HTML_DOCUMENT_NODE => {
                (*(node as *mut xml::xmlDoc))._private
            }
            _ => (*node)._private,
        };
        if p.is_null() {
            None
        } else {
            Some(Value(p as u64))
        }
    }
}

pub(crate) fn init(ctx: &mut Ctx, c: &Classes) {
    let s = c.node_set;
    ctx.define_method(s, "&", method!(intersection), 1, 0);
    ctx.define_method(s, "-", method!(minus), 1, 0);
    ctx.define_method(s, "|", method!(union), 1, 0);
    ctx.define_method(s, "[]", method!(slice), MR_ARGC_VARIADIC, 0) /* arity 1..2 */;
    ctx.define_method(s, "slice", method!(slice), MR_ARGC_VARIADIC, 0) /* arity 1..2 */;
    ctx.define_method(s, "delete", method!(delete), 1, 0);
    ctx.define_method(s, "include?", method!(include), 1, 0);
    ctx.define_method(s, "length", method!(length), 0, 0);
    ctx.define_method(s, "push", method!(push), 1, 0);
    ctx.define_method(s, "to_a", method!(to_a), 0, 0);
    ctx.define_method(s, "unlink", method!(unlink), 0, 0);
    ctx.define_method(s, "initialize_copy", method!(initialize_copy), 1, 0);
}

/// The set of a `NodeSet`. An instance the interpreter allocated but
/// nothing filled yet (`NodeSet.new` runs the gem's `initialize`, which
/// pushes into it) gets an empty set here, as nokogiri's allocator gave
/// it one up front.
pub(crate) fn set_ptr(ctx: &mut Ctx, v: Value) -> Result<*mut xml::xmlNodeSet> {
    if let Some(s) = ctx.native::<XmlNodeSet>(v) {
        return Ok(s.set);
    }
    if ctx.is_kind_of(v, classes().node_set) {
        ctx.native_set(v, XmlNodeSet::empty())?;
        return Ok(ctx.native::<XmlNodeSet>(v).unwrap().set);
    }
    Err(ctx.argument_error("node_set must be a Nokogiri::XML::NodeSet"))
}

fn recv(ctx: &mut Ctx, this: Value) -> Result<*mut xml::xmlNodeSet> {
    set_ptr(ctx, this)
}

/// A member node as a Ruby object: nodes wrap as usual, an XPath
/// namespace copy as an owning `Namespace`
/// (`noko_xml_node_wrap_node_set_result`).
fn wrap_member(ctx: &mut Ctx, node: *mut xml::xmlNode) -> Result<Value> {
    // SAFETY: a live member.
    if unsafe { (*node).type_ } == xml::XML_NAMESPACE_DECL {
        wrap_namespace(ctx, node as *mut xml::xmlNs, std::ptr::null_mut())
    } else {
        wrap_node(ctx, node)
    }
}

/// `noko_xml_node_set_wrap`: a `NodeSet` owning `set` (NULL: an empty
/// one), bound to `document` (nil: none) and decorated by it; every
/// member gets its Ruby object.
pub(crate) fn wrap_node_set(
    ctx: &mut Ctx,
    set: *mut xml::xmlNodeSet,
    document: Value,
) -> Result<Value> {
    let inner = if set.is_null() {
        XmlNodeSet::empty()
    } else {
        XmlNodeSet { set }
    };
    let n = inner.len();
    let rb = ctx.native_new(classes().node_set, inner)?;
    // `decorate` and the member wrapping run Ruby: root the new set.
    let len = ctx.temp_len();
    ctx.temp_push(rb);
    let mut fill = || -> Result<()> {
        if !document.is_nil() {
            ctx.ivar_set(rb, "@document", document)?;
            ctx.funcall(document, "decorate", &[rb], None)?;
        }
        for i in 0..n {
            let node = ctx.native::<XmlNodeSet>(rb).unwrap().node(i);
            wrap_member(ctx, node)?;
        }
        Ok(())
    };
    let r = fill();
    ctx.temp_truncate(len);
    r?;
    Ok(rb)
}

fn document_of(ctx: &Ctx, set: Value) -> Value {
    ctx.ivar_get(set, "@document")
}

fn member_node(ctx: &mut Ctx, v: Value) -> Result<*mut xml::xmlNode> {
    if let Some(n) = ctx.native::<XmlNamespace>(v) {
        return Ok(n.ns as *mut xml::xmlNode);
    }
    node_ptr(ctx, v).map_err(|_| {
        ctx.argument_error("node must be a Nokogiri::XML::Node or Nokogiri::XML::Namespace")
    })
}

/// Remove `val` from `cur` keeping order (`xpath_node_set_del`).
unsafe fn node_set_del(cur: *mut xml::xmlNodeSet, val: *mut xml::xmlNode) {
    // SAFETY: a live set.
    unsafe {
        if cur.is_null() || val.is_null() {
            return;
        }
        let n = (*cur).nodeNr as usize;
        let tab = (*cur).nodeTab;
        let Some(i) = (0..n).find(|&i| *tab.add(i) == val) else {
            return;
        };
        for j in i..n - 1 {
            *tab.add(j) = *tab.add(j + 1);
        }
        *tab.add(n - 1) = std::ptr::null_mut();
        (*cur).nodeNr -= 1;
    }
}

/// NodeSet#length -> Integer
fn length(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let set = recv(ctx, this)?;
    // SAFETY: a live set.
    Ok(Value::int(unsafe { (*set).nodeNr } as i64))
}

/// NodeSet#push(node) -> self
fn push(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let set = recv(ctx, this)?;
    let node = member_node(ctx, args[0])?;
    // SAFETY: live set and node.
    unsafe { xml::xmlXPathNodeSetAdd(set, node) };
    Ok(this)
}

/// NodeSet#delete(node) -> node | nil
fn delete(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let set = recv(ctx, this)?;
    let node = member_node(ctx, args[0])?;
    // SAFETY: live set and node.
    unsafe {
        if xml::xmlXPathNodeSetContains(set, node) != 0 {
            node_set_del(set, node);
            return Ok(args[0]);
        }
    }
    Ok(Value::nil())
}

/// NodeSet#include?(node) -> bool
fn include(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let set = recv(ctx, this)?;
    let node = member_node(ctx, args[0])?;
    // SAFETY: live set and node.
    Ok(Value::bool(
        unsafe { xml::xmlXPathNodeSetContains(set, node) } != 0,
    ))
}

/// NodeSet#&(other) -> NodeSet
fn intersection(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let set = recv(ctx, this)?;
    let other = set_ptr(ctx, args[0])?;
    // SAFETY: live sets; the result is a new set we own.
    let result = unsafe { xml::xmlXPathIntersection(set, other) };
    let doc = document_of(ctx, this);
    wrap_node_set(ctx, result, doc)
}

/// NodeSet#|(other) -> NodeSet
fn union(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let set = recv(ctx, this)?;
    let other = set_ptr(ctx, args[0])?;
    // SAFETY: live sets; the result is a new set we own.
    let result = unsafe {
        let new = xml::xmlXPathNodeSetMerge(std::ptr::null_mut(), set);
        xml::xmlXPathNodeSetMerge(new, other)
    };
    let doc = document_of(ctx, this);
    wrap_node_set(ctx, result, doc)
}

/// NodeSet#-(other) -> NodeSet
fn minus(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let set = recv(ctx, this)?;
    let other = set_ptr(ctx, args[0])?;
    // SAFETY: live sets; the result is a new set we own.
    let result = unsafe {
        let new = xml::xmlXPathNodeSetMerge(std::ptr::null_mut(), set);
        for j in 0..(*other).nodeNr as usize {
            node_set_del(new, *(*other).nodeTab.add(j));
        }
        new
    };
    let doc = document_of(ctx, this);
    wrap_node_set(ctx, result, doc)
}

fn index_at(ctx: &mut Ctx, this: Value, offset: i64) -> Result<Value> {
    let set = recv(ctx, this)?;
    // SAFETY: a live set.
    let n = unsafe { (*set).nodeNr } as i64;
    if offset >= n || offset.abs() > n {
        return Ok(Value::nil());
    }
    let offset = if offset < 0 { offset + n } else { offset };
    // SAFETY: `offset` is within the set.
    let node = unsafe { *(*set).nodeTab.add(offset as usize) };
    wrap_member(ctx, node)
}

fn subseq(ctx: &mut Ctx, this: Value, beg: i64, len: i64) -> Result<Value> {
    let set = recv(ctx, this)?;
    // SAFETY: a live set; the new set is ours.
    let n = unsafe { (*set).nodeNr } as i64;
    if beg > n || beg < 0 || len < 0 {
        return Ok(Value::nil());
    }
    let len = len.min(n - beg);
    let new = unsafe {
        let new = xml::xmlXPathNodeSetCreate(std::ptr::null_mut());
        for j in beg..beg + len {
            xml::xmlXPathNodeSetAddUnique(new, *(*set).nodeTab.add(j as usize));
        }
        new
    };
    let doc = document_of(ctx, this);
    wrap_node_set(ctx, new, doc)
}

/// NodeSet#[](index) / [](start, length) / [](range) -> Node | NodeSet | nil
fn slice(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    check_arity(ctx, args, 1, 2)?;
    let set = recv(ctx, this)?;
    // SAFETY: a live set.
    let n = unsafe { (*set).nodeNr } as i64;
    if let Some(len) = args.get(1).copied() {
        let mut beg = ctx.int(args[0])?;
        let len = ctx.int(len)?;
        if beg < 0 {
            beg += n;
        }
        return subseq(ctx, this, beg, len);
    }
    let arg = args[0];
    if let Some(i) = try_fixnum(ctx, arg) {
        return index_at(ctx, this, i);
    }
    let range_class = ctx.const_get(Value::UNDEF, "Range").unwrap_or_default();
    if ctx.is_kind_of(arg, range_class) {
        let start = ctx.funcall(arg, "begin", &[], None)?;
        let end = ctx.funcall(arg, "end", &[], None)?;
        let mut beg = if start.is_nil() { 0 } else { ctx.int(start)? };
        let mut fin = if end.is_nil() { n } else { ctx.int(end)? };
        if beg < 0 {
            beg += n;
            if beg < 0 {
                return Ok(Value::nil());
            }
        }
        if beg > n {
            return Ok(Value::nil());
        }
        if fin < 0 {
            fin += n;
        }
        if !end.is_nil() && !ctx.funcall(arg, "exclude_end?", &[], None)?.truthy() {
            fin += 1;
        }
        let len = (fin - beg).max(0);
        return subseq(ctx, this, beg, len);
    }
    let i = ctx.int(arg)?;
    index_at(ctx, this, i)
}

/// NodeSet#to_a -> Array
fn to_a(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let set = recv(ctx, this)?;
    // SAFETY: a live set.
    let n = unsafe { (*set).nodeNr } as usize;
    // Wrapping may run Ruby (`decorate`): root the members made so far.
    let len = ctx.temp_len();
    let mut build = || -> Result<Vec<Value>> {
        let mut out = Vec::with_capacity(n);
        for i in 0..n {
            // SAFETY: `i` is within the set.
            let node = unsafe { *(*set).nodeTab.add(i) };
            let v = wrap_member(ctx, node)?;
            ctx.temp_push(v);
            out.push(v);
        }
        Ok(out)
    };
    let r = build();
    ctx.temp_truncate(len);
    Ok(ctx.ary_from_vec(r?))
}

/// NodeSet#unlink -> self
fn unlink(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let set = recv(ctx, this)?;
    // SAFETY: a live set of live nodes.
    let n = unsafe { (*set).nodeNr } as usize;
    let unlink = "unlink";
    for j in 0..n {
        // SAFETY: as above.
        unsafe {
            let node = *(*set).nodeTab.add(j);
            if (*node).type_ == xml::XML_NAMESPACE_DECL {
                continue;
            }
            let rb = wrap_node(ctx, node)?;
            ctx.funcall(rb, unlink, &[], None)?;
            // `unlink` may have re-pointed the object (a copied node).
            *(*set).nodeTab.add(j) = node_ptr(ctx, rb)?;
        }
    }
    Ok(this)
}

/// NodeSet#initialize_copy(other) -> self
fn initialize_copy(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    let set = recv(ctx, this)?;
    let other = set_ptr(ctx, args[0])?;
    // SAFETY: live sets.
    unsafe { xml::xmlXPathNodeSetMerge(set, other) };
    let doc = document_of(ctx, args[0]);
    if !doc.is_nil() {
        ctx.ivar_set(this, "@document", doc)?;
        ctx.funcall(doc, "decorate", &[this], None)?;
    }
    Ok(this)
}
