//! `Nokogiri::XML::NodeSet`: an `xmlNodeSet` owned by the Ruby object.

use super::*;

/// The payload of a `NodeSet`.
pub(super) struct XmlNodeSet {
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

impl NativeData for XmlNodeSet {
    fn mark(&self, alloc: &mut alloc::Allocator<RValue>) {
        // The Ruby objects of the member nodes (`xml_node_set_mark`).
        for i in 0..self.len() {
            let node = self.node(i);
            // SAFETY: members are live nodes of live documents.
            unsafe {
                if let Some(v) = ruby_object_of(node) {
                    v.mark(alloc);
                }
            }
        }
    }
    /// `Object#dup` / `#clone`: an empty set that `initialize_copy`
    /// fills (nokogiri allocates the copy with `xml_node_set_allocate`).
    fn dup(&self) -> Option<Box<dyn NativeData>> {
        Some(Box::new(XmlNodeSet::empty()))
    }
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
}

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
pub(super) unsafe fn ruby_object_of(node: *mut xml::xmlNode) -> Option<Value> {
    // SAFETY: a live node; `_private` holds Value bits or NULL.
    unsafe {
        let p = match (*node).type_ {
            xml::XML_NAMESPACE_DECL => (*(node as *mut xml::xmlNs))._private,
            xml::XML_DOCUMENT_NODE | xml::XML_HTML_DOCUMENT_NODE => (*(node as *mut xml::xmlDoc))._private,
            _ => (*node)._private,
        };
        if p.is_null() { None } else { Some(Value::from_u64(p as u64)) }
    }
}

pub(super) extern "C" fn node_set_alloc(class_id: ClassId, _globals: &mut Globals) -> Value {
    Value::new_native(class_id, Box::new(XmlNodeSet::empty()))
}

pub(super) fn init(globals: &mut Globals, c: &Classes) {
    let s = c.node_set;
    globals.store[s].set_alloc_func(node_set_alloc);
    globals.define_builtin_func(s, "&", intersection, 1);
    globals.define_builtin_func(s, "-", minus, 1);
    globals.define_builtin_func(s, "|", union, 1);
    globals.define_builtin_func_with(s, "[]", slice, 1, 2, false);
    globals.define_builtin_func_with(s, "slice", slice, 1, 2, false);
    globals.define_builtin_func(s, "delete", delete, 1);
    globals.define_builtin_func(s, "include?", include, 1);
    globals.define_builtin_func(s, "length", length, 0);
    globals.define_builtin_func(s, "push", push, 1);
    globals.define_builtin_func(s, "to_a", to_a, 0);
    globals.define_builtin_func(s, "unlink", unlink, 0);
    globals.define_builtin_func(s, "initialize_copy", initialize_copy, 1);
}

pub(super) fn set_ptr(v: Value) -> Result<*mut xml::xmlNodeSet> {
    match v.try_native::<XmlNodeSet>() {
        Some(s) => Ok(s.set),
        None => Err(MonorubyErr::argumenterr("node_set must be a Nokogiri::XML::NodeSet")),
    }
}

fn this(lfp: Lfp) -> Result<*mut xml::xmlNodeSet> {
    set_ptr(lfp.self_val())
}

/// A member node as a Ruby object: nodes wrap as usual, an XPath
/// namespace copy as an owning `Namespace`
/// (`noko_xml_node_wrap_node_set_result`).
fn wrap_member(vm: &mut Executor, globals: &mut Globals, node: *mut xml::xmlNode) -> Result<Value> {
    // SAFETY: a live member.
    if unsafe { (*node).type_ } == xml::XML_NAMESPACE_DECL {
        wrap_namespace(globals, node as *mut xml::xmlNs, std::ptr::null_mut())
    } else {
        wrap_node(vm, globals, node)
    }
}

/// `noko_xml_node_set_wrap`: a `NodeSet` owning `set` (NULL: an empty
/// one), bound to `document` (nil: none) and decorated by it; every
/// member gets its Ruby object.
pub(super) fn wrap_node_set(
    vm: &mut Executor,
    globals: &mut Globals,
    set: *mut xml::xmlNodeSet,
    document: Value,
) -> Result<Value> {
    let inner = if set.is_null() { XmlNodeSet::empty() } else { XmlNodeSet { set } };
    let n = inner.len();
    let rb = Value::new_native(classes().node_set, Box::new(inner));
    // `decorate` and the member wrapping run Ruby: root the new set.
    let len = vm.temp_len();
    vm.temp_push(rb);
    let mut fill = || -> Result<()> {
        if !document.is_nil() {
            globals.store.set_ivar(rb, IdentId::get_id("@document"), document)?;
            vm.invoke_method_inner(globals, IdentId::get_id("decorate"), document, &[rb], None, None)?;
        }
        for i in 0..n {
            let node = rb.try_native::<XmlNodeSet>().unwrap().node(i);
            wrap_member(vm, globals, node)?;
        }
        Ok(())
    };
    let r = fill();
    vm.temp_clear(len);
    r?;
    Ok(rb)
}

fn document_of(globals: &Globals, set: Value) -> Value {
    globals
        .store
        .get_ivar(set, IdentId::get_id("@document"))
        .unwrap_or_default()
}

fn member_node(v: Value) -> Result<*mut xml::xmlNode> {
    if let Some(n) = v.try_native::<XmlNamespace>() {
        return Ok(n.ns as *mut xml::xmlNode);
    }
    node_ptr(v).map_err(|_| MonorubyErr::argumenterr("node must be a Nokogiri::XML::Node or Nokogiri::XML::Namespace"))
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
#[monoruby_builtin]
fn length(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let set = this(lfp)?;
    // SAFETY: a live set.
    Ok(Value::integer(unsafe { (*set).nodeNr } as i64))
}

/// NodeSet#push(node) -> self
#[monoruby_builtin]
fn push(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let set = this(lfp)?;
    let node = member_node(lfp.arg(0))?;
    // SAFETY: live set and node.
    unsafe { xml::xmlXPathNodeSetAdd(set, node) };
    Ok(lfp.self_val())
}

/// NodeSet#delete(node) -> node | nil
#[monoruby_builtin]
fn delete(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let set = this(lfp)?;
    let node = member_node(lfp.arg(0))?;
    // SAFETY: live set and node.
    unsafe {
        if xml::xmlXPathNodeSetContains(set, node) != 0 {
            node_set_del(set, node);
            return Ok(lfp.arg(0));
        }
    }
    Ok(Value::nil())
}

/// NodeSet#include?(node) -> bool
#[monoruby_builtin]
fn include(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let set = this(lfp)?;
    let node = member_node(lfp.arg(0))?;
    // SAFETY: live set and node.
    Ok(Value::bool(unsafe { xml::xmlXPathNodeSetContains(set, node) } != 0))
}

/// NodeSet#&(other) -> NodeSet
#[monoruby_builtin]
fn intersection(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let set = this(lfp)?;
    let other = set_ptr(lfp.arg(0))?;
    // SAFETY: live sets; the result is a new set we own.
    let result = unsafe { xml::xmlXPathIntersection(set, other) };
    let doc = document_of(globals, lfp.self_val());
    wrap_node_set(vm, globals, result, doc)
}

/// NodeSet#|(other) -> NodeSet
#[monoruby_builtin]
fn union(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let set = this(lfp)?;
    let other = set_ptr(lfp.arg(0))?;
    // SAFETY: live sets; the result is a new set we own.
    let result = unsafe {
        let new = xml::xmlXPathNodeSetMerge(std::ptr::null_mut(), set);
        xml::xmlXPathNodeSetMerge(new, other)
    };
    let doc = document_of(globals, lfp.self_val());
    wrap_node_set(vm, globals, result, doc)
}

/// NodeSet#-(other) -> NodeSet
#[monoruby_builtin]
fn minus(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let set = this(lfp)?;
    let other = set_ptr(lfp.arg(0))?;
    // SAFETY: live sets; the result is a new set we own.
    let result = unsafe {
        let new = xml::xmlXPathNodeSetMerge(std::ptr::null_mut(), set);
        for j in 0..(*other).nodeNr as usize {
            node_set_del(new, *(*other).nodeTab.add(j));
        }
        new
    };
    let doc = document_of(globals, lfp.self_val());
    wrap_node_set(vm, globals, result, doc)
}

fn index_at(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, offset: i64) -> Result<Value> {
    let set = this(lfp)?;
    // SAFETY: a live set.
    let n = unsafe { (*set).nodeNr } as i64;
    if offset >= n || offset.abs() > n {
        return Ok(Value::nil());
    }
    let offset = if offset < 0 { offset + n } else { offset };
    // SAFETY: `offset` is within the set.
    let node = unsafe { *(*set).nodeTab.add(offset as usize) };
    wrap_member(vm, globals, node)
}

fn subseq(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, beg: i64, len: i64) -> Result<Value> {
    let set = this(lfp)?;
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
    let doc = document_of(globals, lfp.self_val());
    wrap_node_set(vm, globals, new, doc)
}

/// NodeSet#[](index) / [](start, length) / [](range) -> Node | NodeSet | nil
#[monoruby_builtin]
fn slice(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let set = this(lfp)?;
    // SAFETY: a live set.
    let n = unsafe { (*set).nodeNr } as i64;
    if let Some(len) = lfp.try_arg(1) {
        let mut beg = lfp.arg(0).expect_integer(&globals.store)?;
        let len = len.expect_integer(&globals.store)?;
        if beg < 0 {
            beg += n;
        }
        return subseq(vm, globals, lfp, beg, len);
    }
    let arg = lfp.arg(0);
    if let Some(i) = arg.try_fixnum() {
        return index_at(vm, globals, lfp, i);
    }
    if let Some(range) = arg.is_range() {
        let start = range.start();
        let end = range.end();
        let mut beg = if start.is_nil() { 0 } else { start.expect_integer(&globals.store)? };
        let mut fin = if end.is_nil() { n } else { end.expect_integer(&globals.store)? };
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
        if !end.is_nil() && !range.exclude_end() {
            fin += 1;
        }
        let len = (fin - beg).max(0);
        return subseq(vm, globals, lfp, beg, len);
    }
    index_at(vm, globals, lfp, arg.expect_integer(&globals.store)?)
}

/// NodeSet#to_a -> Array
#[monoruby_builtin]
fn to_a(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let set = this(lfp)?;
    // SAFETY: a live set.
    let n = unsafe { (*set).nodeNr } as usize;
    // Wrapping may run Ruby (`decorate`): root the members made so far.
    let len = vm.temp_len();
    let mut build = || -> Result<Vec<Value>> {
        for i in 0..n {
            // SAFETY: `i` is within the set.
            let node = unsafe { *(*set).nodeTab.add(i) };
            let v = wrap_member(vm, globals, node)?;
            vm.temp_push(v);
        }
        Ok((len..vm.temp_len()).map(|i| vm.temp_at(i)).collect())
    };
    let r = build();
    vm.temp_clear(len);
    Ok(Value::array_from_vec(r?))
}

/// NodeSet#unlink -> self
#[monoruby_builtin]
fn unlink(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let set = this(lfp)?;
    // SAFETY: a live set of live nodes.
    let n = unsafe { (*set).nodeNr } as usize;
    let unlink = IdentId::get_id("unlink");
    for j in 0..n {
        // SAFETY: as above.
        unsafe {
            let node = *(*set).nodeTab.add(j);
            if (*node).type_ == xml::XML_NAMESPACE_DECL {
                continue;
            }
            let rb = wrap_node(vm, globals, node)?;
            vm.invoke_method_inner(globals, unlink, rb, &[], None, None)?;
            // `unlink` may have re-pointed the object (a copied node).
            *(*set).nodeTab.add(j) = node_ptr(rb)?;
        }
    }
    Ok(lfp.self_val())
}

/// NodeSet#initialize_copy(other) -> self
#[monoruby_builtin]
fn initialize_copy(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let set = this(lfp)?;
    let other = set_ptr(lfp.arg(0))?;
    // SAFETY: live sets.
    unsafe { xml::xmlXPathNodeSetMerge(set, other) };
    let doc = document_of(globals, lfp.arg(0));
    if !doc.is_nil() {
        globals.store.set_ivar(lfp.self_val(), IdentId::get_id("@document"), doc)?;
        vm.invoke_method_inner(globals, IdentId::get_id("decorate"), doc, &[lfp.self_val()], None, None)?;
    }
    Ok(lfp.self_val())
}
