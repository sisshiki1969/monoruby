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
            v.get_real_class_name(&globals.store)
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
