use super::struct_class::{
    eq, eql, get_members, hash, members, ne, qualified_real_class_name, struct_members,
};
use super::*;

/// `Data` (Ruby 3.2+ value objects). The class itself is defined here in
/// Rust so that `Data.define` can produce *real* `Data` subclasses (CRuby:
/// `Measure.ancestors` includes `Data`, and `Data.instance_method(:initialize)`
/// can be bound to any defined-class instance). Storage reuses the Struct
/// slot machinery (`ObjTy::STRUCT` + `define_struct_class`), so member
/// readers JIT-compile to the same direct slot loads as Struct readers.
///
/// The split with `builtins/data.rb`:
/// * Rust (here): the class factory (`__define_class`), slot install +
///   freeze (`__data_init`), and the identity-sensitive primitives
///   (`inspect`, `==`, `eql?`, `!=`, `hash`, `members`, `deconstruct`) —
///   these need slot access and the recursion guards.
/// * Ruby (`data.rb`): `define`'s member validation, `new`/`[]`
///   positional→keyword coercion, keyword validation (`initialize`),
///   `with`, `to_h`, `deconstruct_keys` — pure protocol logic.
pub(super) fn init(globals: &mut Globals) {
    let klass = globals.define_class_under_obj("Data");
    let cid = klass.id();
    // Like Struct, `Data` itself is not allocatable; only classes produced
    // by `Data.define` get the slot allocator (via `define_struct_class`).
    globals.store[cid].clear_alloc_func();
    globals.define_builtin_class_func(cid, "__define_class", data_define_class, 2);
    globals.define_builtin_func(cid, "__data_init", data_init, 1);
    globals.define_builtin_func(cid, "inspect", data_inspect, 0);
    globals.define_builtin_func(cid, "==", eq, 1);
    globals.define_builtin_func(cid, "eql?", eql, 1);
    globals.define_builtin_func(cid, "!=", ne, 1);
    globals.define_builtin_func(cid, "hash", hash, 0);
    globals.define_builtin_func(cid, "members", members, 0);
    globals.define_builtin_func(cid, "deconstruct", deconstruct, 0);
}

/// Whether `class_id` is a `Data` subclass — i.e. its instances are the
/// frozen value objects `Data.define` produces. `Marshal` needs this:
/// CRuby freezes a `Data` it loads, but not a `Struct`.
pub(crate) fn is_data_subclass(store: &Store, class_id: ClassId) -> bool {
    match data_class_id(store) {
        Some(data) => derives_from(store, class_id, data),
        None => false,
    }
}

/// The `ClassId` of `::Data`, looked up via the constant so no reserved
/// builtin id is needed.
fn data_class_id(store: &Store) -> Option<ClassId> {
    store
        .get_constant_noautoload(OBJECT_CLASS, IdentId::get_id("Data"))
        .and_then(|v| v.is_class_or_module())
        .map(|m| m.id())
}

/// Whether `class_id`'s superclass chain reaches `target`.
///
/// The walk follows the ancestor *objects*. Re-looking each step's id up
/// in `store` instead would spin forever on a chain that passes through
/// an iclass: an iclass reports the included module's id, and that
/// module's own entry leads back into the same chain — a `Struct`
/// subclass (`Struct` includes `Enumerable`) hung `Data#inspect` and
/// `Marshal.load` that way.
fn derives_from(store: &Store, class_id: ClassId, target: ClassId) -> bool {
    let mut module = store[class_id].get_module();
    loop {
        if !module.is_iclass() && module.id() == target {
            return true;
        }
        match module.superclass() {
            Some(s) => module = s,
            None => return false,
        }
    }
}

///
/// Data.__define_class(superclass, members) -> Class
///
/// The low-level factory behind `Data.define`: creates an anonymous
/// slot-storage class inheriting `superclass` (normally `Data` itself, or
/// the receiver when `define` is called on an already-defined class),
/// stores `/members`, installs the per-member slot readers (firing
/// `method_added` like `attr_reader` would), and defines the class-level
/// `members`. `new`/`[]`/validation are layered in Ruby (`data.rb`).
#[monoruby_builtin]
fn data_define_class(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let superclass = lfp.arg(0).as_class();
    let members_arg = lfp.arg(1).as_array();
    let m = globals.store.define_struct_class(None, superclass);
    let class_id = m.id();
    let new_class = m.as_val();

    let members = ArrayInner::from_iter(members_arg.iter().cloned());
    let inline = members.len() <= crate::value::STRUCT_INLINE_SLOTS;
    globals
        .store
        .set_ivar(new_class, IdentId::_MEMBERS, Value::array(members))?;

    // Readers only — Data is immutable, so no writers are installed;
    // `__data_init` stores through the slots directly.
    for (i, arg) in members_arg.iter().enumerate() {
        let name = arg.expect_symbol_or_string(globals)?;
        globals.define_struct_reader(class_id, name, i as u16, inline, Visibility::Public);
        vm.invoke_method_added(globals, class_id, name, None)?;
    }

    // Class-level `members` goes on the defined class's metaclass (NOT on
    // `Data`): a plain `class X < Data` subclass must not respond to it.
    globals.define_builtin_class_func(class_id, "members", struct_members, 0);

    Ok(new_class)
}

///
/// Data#__data_init(values) -> nil
///
/// Store `values` (already validated/ordered by the Ruby layer) into the
/// member slots and freeze the receiver. This is the tail of
/// `Data#initialize`, split out so slot access stays in Rust.
#[monoruby_builtin]
fn data_init(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let mut self_val = lfp.self_val();
    let values = lfp.arg(0).as_array();
    for (i, v) in values.iter().enumerate() {
        self_val.set_struct_slot(i, *v);
    }
    self_val.set_frozen();
    Ok(Value::nil())
}

///
/// Data#deconstruct -> Array
///
/// Member values in declaration order, read straight from the slots
/// (reader overrides do not affect it, matching CRuby).
#[monoruby_builtin]
fn deconstruct(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let self_val = lfp.self_val();
    let mut v = Vec::new();
    if let Some(s) = self_val.try_struct() {
        for i in 0..s.len() {
            v.push(s.get(i));
        }
    }
    Ok(Value::array_from_vec(v))
}

///
/// Data#inspect / Data#to_s (aliased in data.rb)
///
/// `#<data Name amount=42, unit="km">`. The class label follows Struct's
/// rules (fully-qualified real path; dropped entirely when any path
/// segment is anonymous; a user-defined `#name` is never consulted).
/// Recursive structures render the repeated member as
/// `#<data Name:...>` — with `#<Class:0x...>` in place of `Name` for an
/// anonymous class — matching CRuby.
#[monoruby_builtin]
fn data_inspect(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let self_val = lfp.self_val();
    let store = &globals.store;
    let mut set = std::collections::HashSet::new();
    set.insert(self_val.id());
    let s = render_data(store, self_val, &mut set)?;
    Ok(Value::string(s))
}

/// The label a self-referential `STRUCT`-typed value renders as:
/// `#<data D:...>` for a `Data` (with `#<Class:0x...>` in place of the
/// name when the class is anonymous), `#<struct S:...>` for a Struct.
pub(crate) fn recursive_struct_label(store: &Store, val: Value) -> String {
    match data_class_id(store) {
        Some(data_cid) if derives_from(store, val.class(), data_cid) => {
            let name = data_class_label(store, val.real_class(store).id()).unwrap_or_else(|| {
                format!(
                    "#<Class:0x{:016x}>",
                    store[val.class()].get_module().as_val().id()
                )
            });
            format!("#<data {name}:...>")
        }
        _ => super::struct_class::recursive_struct_label(store, val),
    }
}

/// Render a `STRUCT`-typed value — a `Data` value object or a plain
/// `Struct` instance — the way its `#inspect` does. This is what the
/// Rust-level inspect uses, so one nested inside another struct, a
/// Data, an Array or a Hash no longer falls back to `#<S:0x…>`.
pub(crate) fn render_struct_or_data(
    store: &Store,
    val: Value,
    set: &mut std::collections::HashSet<u64>,
) -> Option<String> {
    match data_class_id(store) {
        Some(data_cid) if derives_from(store, val.class(), data_cid) => {
            render_data(store, val, set).ok()
        }
        _ => super::struct_class::render_struct(store, val, set).ok(),
    }
}

/// The class label for `#<data ...>` rendering: the fully-qualified real
/// path, or `None` when the class (or any segment of its path) is
/// anonymous. Mirrors Struct#inspect's guard — `get_parents` cannot be
/// called on an anonymous class.
fn data_class_label(store: &Store, class_id: ClassId) -> Option<String> {
    if store[class_id].get_name().is_some() {
        qualified_real_class_name(store, class_id)
    } else {
        None
    }
}

pub(crate) fn render_data(
    store: &Store,
    val: Value,
    set: &mut std::collections::HashSet<u64>,
) -> Result<String> {
    let mut out = String::from("#<data");
    if let Some(name) = data_class_label(store, val.real_class(store).id()) {
        out.push(' ');
        out.push_str(&name);
    }
    let members = get_members(store, store[val.real_class(store).id()].get_module())?;
    let mut first = true;
    for (i, m) in members.iter().enumerate() {
        let name = m.try_symbol().unwrap();
        let slot = val.try_struct().and_then(|s| s.try_get(i));
        // A nested `Data` or `Struct` renders through the shared
        // Rust-level inspect, which knows both.
        let rendered = match slot {
            Some(v) => v.inspect_inner(store, set),
            None => "nil".to_string(),
        };
        out.push_str(if first { " " } else { ", " });
        first = false;
        out.push_str(&format!("{name:?}={rendered}"));
    }
    out.push('>');
    Ok(out)
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn data_is_real_data_subclass() {
        let prelude = r#"M = Data.define(:amount, :unit)"#;
        run_test_with_prelude(
            r#"[M.superclass, M.new(1, "m").is_a?(Data), Data.superclass]"#,
            prelude,
        );
        // A plain `class X < Data` subclass must not respond to `members`;
        // defined classes (and their subclasses) must.
        run_test_with_prelude(
            r#"
            sub = Class.new(Data)
            [sub.respond_to?(:members), M.respond_to?(:members), Class.new(M).members]
            "#,
            prelude,
        );
    }

    #[test]
    fn data_initialize_bind_call() {
        // The psych deserialization pattern: Data#initialize bound onto an
        // allocated instance of a defined class.
        run_test_with_prelude(
            r#"
            d1 = M.new(42, "km")
            d2 = M.allocate
            Data.instance_method(:initialize).bind_call(d2, **d1.to_h)
            [d2 == d1, d2.frozen?]
            "#,
            r#"M = Data.define(:amount, :unit)"#,
        );
    }

    #[test]
    fn data_recursive_inspect() {
        // Recursive member renders as `#<data Name:...>`; nested
        // (non-recursive) data renders in full.
        run_test_once(
            r#"
            M2 = Data.define(:amount, :unit)
            a = M2.allocate
            a.send(:initialize, amount: 42, unit: a)
            nested = M2.new(1, M2.new(2, "m"))
            [a.to_s, nested.inspect]
            "#,
        );
    }

    #[test]
    fn data_equality_and_hash_recursion() {
        let prelude = r#"M = Data.define(:amount, :unit)"#;
        run_test_with_prelude(
            r#"
            a = M.allocate
            a.send(:initialize, amount: 42, unit: a)
            b = M.allocate
            b.send(:initialize, amount: 42, unit: b)
            [a == b, a.eql?(b), a.hash == b.hash]
            "#,
            prelude,
        );
        run_test_with_prelude(
            r#"
            other = Data.define(:amount, :unit)
            [M.new(1, "m") == other.new(1, "m"), M.new(1, "m") != M.new(1, "m")]
            "#,
            prelude,
        );
    }

    #[test]
    fn data_deconstruct_and_members() {
        let prelude = r#"M = Data.define(:amount, :unit)"#;
        run_test_with_prelude(r#"M.new(1, "m").deconstruct"#, prelude);
        run_test_with_prelude(r#"[M.members, M.new(1, "m").members]"#, prelude);
        run_test_with_prelude(r#"M.new(1, "m").to_h { |k, v| [v, k] }"#, prelude);
    }

    #[test]
    fn data_define_errors() {
        run_test_error(r#"Data.define(:a, :a)"#);
        run_test_error(r#"Data.define(1)"#);
    }
}
