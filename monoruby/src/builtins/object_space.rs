//! `ObjectSpace::WeakMap` — a map that holds neither its keys nor its
//! values.
//!
//! The storage is `ObjTy::WEAKMAP` (`value/rvalue/weakmap.rs`), whose
//! `mark` traces nothing; the collector breaks pairs whose halves did
//! not survive, between the mark and the sweep. Everything here is the
//! Ruby surface over that.
//!
//! Keys are compared by identity (`equal?`), as CRuby's WeakMap does —
//! never by `hash` / `eql?`, which would have to re-enter the
//! interpreter from inside a collection.

use super::*;

pub(super) fn init(globals: &mut Globals) {
    let name = IdentId::get_id("ObjectSpace");
    let object_space = match globals.store.get_constant_noautoload(OBJECT_CLASS, name) {
        Some(v) => v.as_class_id(),
        None => globals
            .store
            .define_module_with_identid(name, OBJECT_CLASS)
            .id(),
    };
    let object = globals.store.get_module(OBJECT_CLASS);
    let weakmap = globals
        .store
        .define_class_with_instance_ty("WeakMap", object, object_space, ObjTy::WEAKMAP)
        .id();
    globals.store[weakmap].set_alloc_func(weakmap_alloc);

    globals.define_builtin_func(weakmap, "[]", weakmap_index, 1);
    globals.define_builtin_func(weakmap, "[]=", weakmap_index_assign, 2);
    globals.define_builtin_func(weakmap, "delete", weakmap_delete, 1);
    globals.define_builtin_func(weakmap, "key?", weakmap_key_p, 1);
    globals.define_builtin_func(weakmap, "size", weakmap_size, 0);
    globals.define_builtin_func(weakmap, "keys", weakmap_keys, 0);
    globals.define_builtin_func(weakmap, "values", weakmap_values, 0);
    // The pairs as a flat Array, which the Ruby half turns into each /
    // each_key / each_value. Taking a snapshot keeps a block free to
    // allocate — and so to collect — without walking a map that is
    // being mutated underneath it.
    globals.define_builtin_func(weakmap, "__entries", weakmap_entries, 0);

    // The heap walk behind `ObjectSpace.each_object`. It answers a
    // snapshot Array, which the Ruby half yields from — see there for
    // why iteration cannot walk the heap directly.
    globals.define_builtin_module_func_with(
        object_space,
        "__live_objects",
        live_objects,
        0,
        1,
        false,
    );
}

///
/// The objects currently on the heap, as an Array, optionally only
/// those that are `kind_of?` `klass`.
///
/// `alloc.rs` already knows which cells hold an object — a free cell's
/// header is a `next` pointer — so this is that walk, filtered.
///
/// Two things it has to be careful about. The walk itself must not
/// collect, because a collection would free cells it is part way
/// through reading, so the whole of it runs with GC disabled. And the
/// class test is a plain ancestor check rather than a `#kind_of?` call:
/// dispatching into Ruby for every cell on the heap would allocate, and
/// a redefined `#kind_of?` would be running while the heap is being
/// walked.
///
#[monoruby_builtin]
fn live_objects(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let filter = lfp.try_arg(0).filter(|v| !v.is_nil());
    let class = match filter {
        Some(v) => Some(v.expect_class_or_module(&globals.store)?.id()),
        None => None,
    };
    let gc_was_on = Globals::gc_enable(false);
    let mut found: Vec<Value> = vec![];
    crate::alloc::ALLOC.with(|alloc| {
        alloc.borrow().for_each_live(|rv| {
            // A frame is the interpreter's own bookkeeping, not a Ruby
            // object; CRuby hides its equivalents from this walk too.
            if rv.ty() == ObjTy::FRAME {
                return;
            }
            let v = Value::from_rvalue_ref(rv);
            if let Some(module) = v.is_class_or_module() {
                // An iclass is the proxy a module becomes when it is
                // included; it is not a Ruby object and CRuby skips it.
                if module.is_iclass() {
                    return;
                }
                // Nor is a *hidden metaclass* — the singleton class of a
                // singleton class, which exists only to carry the chain.
                // CRuby tells them apart by what the singleton is
                // attached to, and skips the ones attached to another
                // singleton.
                if let Some(attached) = module.is_singleton()
                    && attached
                        .is_class_or_module()
                        .is_some_and(|a| a.is_singleton().is_some())
                {
                    return;
                }
            }
            if let Some(class) = class
                && !v.is_kind_of(&globals.store, class)
            {
                return;
            }
            found.push(v);
        });
    });
    // The Array allocates, so it is built only once the walk is over —
    // and before GC comes back on, since until the Array exists nothing
    // is holding what the walk found.
    let result = Value::array_from_vec(found);
    Globals::gc_enable(gc_was_on);
    Ok(result)
}

extern "C" fn weakmap_alloc(class_id: ClassId, _globals: &mut Globals) -> Value {
    Value::new_weakmap(class_id)
}

/// The receiver's pairs, or a TypeError for a bare `allocate`-alike of
/// another class.
fn map_of<'a>(globals: &Globals, v: &'a Value) -> Result<&'a WeakMapInner> {
    match v.try_weakmap_inner() {
        Some(m) => Ok(m),
        None => Err(MonorubyErr::typeerr(format!(
            "wrong argument type {} (expected ObjectSpace::WeakMap)",
            v.builtin_class_name(&globals.store)
        ))),
    }
}

/// WeakMap#[](key) -> value or nil
#[monoruby_builtin]
fn weakmap_index(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let this = lfp.self_val();
    let m = map_of(globals, &this)?;
    Ok(m.get(lfp.arg(0)).unwrap_or_default())
}

/// WeakMap#[]=(key, value) -> nil
///
/// An immediate key is accepted and reads back, but has no cell to
/// watch, so the collector drops the pair on its next pass — CRuby
/// behaves the same way. The assignment answers nil, not the value.
#[monoruby_builtin]
fn weakmap_index_assign(
    _: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    map_of(globals, &lfp.self_val())?;
    lfp.self_val()
        .as_weakmap_inner_mut()
        .insert(lfp.arg(0), lfp.arg(1));
    Ok(Value::nil())
}

/// WeakMap#delete(key) -> the value, or nil
#[monoruby_builtin]
fn weakmap_delete(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    map_of(globals, &lfp.self_val())?;
    Ok(lfp
        .self_val()
        .as_weakmap_inner_mut()
        .remove(lfp.arg(0))
        .unwrap_or_default())
}

/// WeakMap#key?(key) -> bool
#[monoruby_builtin]
fn weakmap_key_p(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let this = lfp.self_val();
    let m = map_of(globals, &this)?;
    Ok(Value::bool(m.get(lfp.arg(0)).is_some()))
}

/// WeakMap#size -> Integer
#[monoruby_builtin]
fn weakmap_size(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let this = lfp.self_val();
    let m = map_of(globals, &this)?;
    Ok(Value::integer(m.len() as i64))
}

/// WeakMap#keys -> Array
#[monoruby_builtin]
fn weakmap_keys(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let this = lfp.self_val();
    let m = map_of(globals, &this)?;
    Ok(Value::array_from_iter(m.iter().map(|(k, _)| k)))
}

/// WeakMap#values -> Array
#[monoruby_builtin]
fn weakmap_values(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let this = lfp.self_val();
    let m = map_of(globals, &this)?;
    Ok(Value::array_from_iter(m.iter().map(|(_, v)| v)))
}

/// WeakMap#__entries -> [k1, v1, k2, v2, …] (private)
#[monoruby_builtin]
fn weakmap_entries(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let this = lfp.self_val();
    let m = map_of(globals, &this)?;
    let mut out = Vec::with_capacity(m.len() * 2);
    for (k, v) in m.iter() {
        out.push(k);
        out.push(v);
    }
    Ok(Value::array_from_vec(out))
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    /// `ObjectSpace.each_object` (#1422). It answered nothing at all —
    /// `builtins/object_space.rb` said so in as many words — where
    /// CRuby walks the heap.
    #[test]
    fn each_object_walks_the_heap() {
        // Each expression builds its own class so the counts do not
        // depend on what else the process happens to be holding.
        run_tests(&[
            // The instances of a fresh class, and the count `each_object`
            // answers.
            r#"
            k = Class.new
            a, b = k.new, k.new
            n = ObjectSpace.each_object(k) { |o| }
            found = []
            ObjectSpace.each_object(k) { |o| found << o.equal?(a) || o.equal?(b) }
            [n, found.size, found.all?, a.class == k, b.class == k]
            "#,
            // No block answers an Enumerator, and `each` on it counts
            // the same.
            r#"
            k = Class.new
            o = k.new
            e = ObjectSpace.each_object(k)
            [e.class.to_s, e.each {}, e.to_a.size, e.to_a.first.equal?(o)]
            "#,
            // An object reachable only from a constant, a global, an
            // Array, a Hash key and an ivar is still on the heap.
            r#"
            k = Class.new
            K = k.new
            $each_object_global = k.new
            arr = [k.new]
            h = {k.new => 1}
            holder = Object.new
            holder.instance_variable_set(:@x, k.new)
            n = ObjectSpace.each_object(k) { |o| }
            [n, arr.size, h.size, holder.instance_variable_get(:@x).class == k]
            "#,
            // A class is found by `each_object(Class)`, a module by
            // `each_object(Module)` — and a class is a Module too.
            r#"
            k = Class.new
            m = Module.new
            [ObjectSpace.each_object(Class).include?(k),
             ObjectSpace.each_object(Module).include?(m),
             ObjectSpace.each_object(Module).include?(k),
             ObjectSpace.each_object(Class).include?(m)]
            "#,
            // A singleton class is walked; the *hidden* metaclass above
            // one is not, which is the only thing CRuby's walk hides
            // among classes.
            r#"
            k = Class.new
            sclass = k.new.singleton_class
            meta = k.singleton_class
            hidden = Class.new.singleton_class
            ancestors = ObjectSpace.each_object(Class).select { |c| hidden.is_a?(c) }
            [ObjectSpace.each_object(meta).to_a.include?(sclass),
             ancestors.find { |h| h.inspect.include?(hidden.inspect) }.nil?]
            "#,
            // The block is free to allocate: the walk hands back a
            // snapshot rather than iterating the live heap.
            r#"
            k = Class.new
            3.times { k.new }
            seen = 0
            ObjectSpace.each_object(k) { |o| seen += 1; 200.times { Object.new } }
            seen >= 3
            "#,
            // Every object of a class with no instances at all.
            r#"
            k = Class.new
            [ObjectSpace.each_object(k) { |o| }, ObjectSpace.each_object(k).to_a]
            "#,
        ]);
    }
}
