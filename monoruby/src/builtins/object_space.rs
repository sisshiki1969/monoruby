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
    globals.define_builtin_func_with(weakmap, "delete", weakmap_delete, 1, 1, false);
    globals.define_builtin_func(weakmap, "inspect", weakmap_inspect, 0);
    globals.define_builtin_func(weakmap, "to_s", weakmap_inspect, 0);

    // `WeakKeyMap` shares `WeakMap`'s cell: it is the same pairs, with
    // the value half held strongly instead of weakly (see
    // `WeakMapInner::weak_values`). What differs is the lookup — CRuby
    // compares a `WeakKeyMap`'s keys with `#hash` / `#eql?` rather than
    // by identity — so it gets its own methods rather than sharing
    // `WeakMap`'s.
    let weakkeymap = globals
        .store
        .define_class_with_instance_ty("WeakKeyMap", object, object_space, ObjTy::WEAKMAP)
        .id();
    globals.store[weakkeymap].set_alloc_func(weakkeymap_alloc);
    globals.define_builtin_func(weakkeymap, "[]", weakkeymap_index, 1);
    globals.define_builtin_func(weakkeymap, "[]=", weakkeymap_index_assign, 2);
    globals.define_builtin_func(weakkeymap, "getkey", weakkeymap_getkey, 1);
    globals.define_builtin_func(weakkeymap, "key?", weakkeymap_key_p, 1);
    globals.define_builtin_func_with(weakkeymap, "delete", weakkeymap_delete, 1, 1, false);
    globals.define_builtin_func(weakkeymap, "clear", weakkeymap_clear, 0);
    globals.define_builtin_func(weakkeymap, "inspect", weakkeymap_inspect, 0);
    // No `#size` and no `#to_s`: CRuby's WeakKeyMap has exactly `[]`,
    // `[]=`, `clear`, `delete`, `getkey`, `inspect` and `key?`. The
    // size shows up inside `#inspect` and nowhere else.

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
            //
            // Handing out the objects that *do* reach a frame — a
            // `Binding`, or a `Proc` and its `#binding` — does not
            // weaken I5 (`doc/jit_invariants.md` §3.5). The JIT reads
            // locals as rbp-relative slots only while a frame is
            // uncaptured, and the very act of taking a binding or
            // capturing a proc promotes the frame to the heap,
            // tombstones the stack slots and makes `GuardCapture`
            // deopt. So a `Binding` cannot exist for a frame that is
            // still being speculated on, and this walk creates no such
            // capability — it only finds the ones already made. A
            // method that never captures has no `Binding` on the heap
            // at all, so its locals stay unreachable from here; both
            // halves are pinned by `each_object_cannot_reach_an_
            // uncaptured_frame`.
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

/// WeakMap#delete(key) { |key| } -> the value, the block's answer, or nil
#[monoruby_builtin]
fn weakmap_delete(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    map_of(globals, &lfp.self_val())?;
    if let Some(v) = lfp.self_val().as_weakmap_inner_mut().remove(lfp.arg(0)) {
        return Ok(v);
    }
    // A miss runs the block on the key, if one was given.
    match lfp.block() {
        Some(bh) => {
            let data = vm.get_block_data(globals, bh)?;
            vm.invoke_block(globals, &data, &[lfp.arg(0)])
        }
        None => Ok(Value::nil()),
    }
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

    /// `each_object` hands out every object on the heap, including the
    /// `Binding`s and `Proc`s that can write another frame's locals.
    /// That is safe, and it is worth a test rather than an argument.
    ///
    /// The JIT reads locals as rbp-relative stack slots only while a
    /// frame is *uncaptured* (`doc/jit_invariants.md` §3.5, I5).
    /// Taking a binding or capturing a proc promotes the frame to the
    /// heap and tombstones those slots, so a `Binding` can only ever
    /// name a frame the JIT has already stopped speculating about.
    /// This walk finds such capabilities; it never manufactures one.
    #[test]
    fn each_object_cannot_reach_an_uncaptured_frame() {
        run_tests(&[
            // A method that never captures: its locals are unreachable,
            // and it keeps computing the right answer.
            r#"
            def untouched
              secret = 7
              t = 0
              i = 0
              while i < 400
                t += secret
                i += 1
              end
              t
            end
            50.times { untouched }
            reachable = 0
            ObjectSpace.each_object(Binding) do |b|
              reachable += 1 if b.local_variables.include?(:secret)
            end
            [reachable, untouched]
            "#,
            // A local rewritten from outside, through `each_object`,
            // while the frame is still running its JIT-compiled loop —
            // the loop must see the new value from that iteration on.
            r#"
            def rewrite_from_outside
              ObjectSpace.each_object(Binding) do |b|
                b.local_variable_set(:x, 100) if b.local_variables.include?(:x)
              end
            end
            def live_frame
              x = 1
              b = binding
              total = 0
              i = 0
              while i < 400
                total += x
                i += 1
                rewrite_from_outside if i == 200
              end
              total
            end
            live_frame
            "#,
            // The same through a `Proc`'s binding rather than one taken
            // directly.
            r#"
            def via_proc
              ObjectSpace.each_object(Proc) do |pr|
                b = (pr.binding rescue nil)
                next unless b && b.local_variables.include?(:y)
                b.local_variable_set(:y, 100)
              end
            end
            def live_proc_frame
              y = 1
              keep = proc { y }
              total = 0
              i = 0
              while i < 400
                total += y
                i += 1
                via_proc if i == 200
              end
              total
            end
            live_proc_frame
            "#,
            // Walking everything, repeatedly, from inside hot code and
            // across collections, leaves the hot code's own arithmetic
            // alone.
            r#"
            def churn(n)
              acc = 0
              i = 0
              while i < n
                acc += i
                a = [i, i.to_s, {i => i}]
                acc += a.size
                i += 1
              end
              acc
            end
            swept = 0
            5.times do
              churn(200)
              ObjectSpace.each_object { |o| swept += 1 }
              GC.start
            end
            [swept > 0, churn(200) == (0...200).sum + 200 * 3]
            "#,
        ]);
    }

    /// `ObjectSpace::WeakKeyMap` (#1422), which did not exist.
    ///
    /// It is weak-key and *strong*-value, and unlike `WeakMap` it
    /// compares keys with `#hash` / `#eql?` rather than by identity.
    #[test]
    fn weak_key_map() {
        run_tests(&[
            // Equality semantics, and the first key is the one kept.
            r#"
            m = ObjectSpace::WeakKeyMap.new
            k1, k2 = %w[a a].map(&:upcase)
            m[k1] = 1
            [m[k2], m.key?(k2), m.getkey(k2).equal?(k1), m.getkey("X"), k1, k2]
            "#,
            // An equal key replaces the value, not the key, so the size
            // does not grow.
            r#"
            m = ObjectSpace::WeakKeyMap.new
            a, b, c = "foo", "bar", "bar"
            m[a] = 1; m[b] = 2; m[c] = 3
            [m[b], m[c], m.getkey(b).equal?(c), m.getkey(b).equal?(b), a, b, c]
            "#,
            // A key with no cell can never be collected, so it is
            // refused — and never found.
            r#"
            m = ObjectSpace::WeakKeyMap.new
            r = [1, 1.0, :a, true, false, nil].map do |k|
              begin
                m[k] = "x"
                :stored
              rescue ArgumentError => e
                e.message
              end
            end
            r << [m.getkey(1), m.getkey(:a), m.delete(1), m.key?(nil)]
            "#,
            // `#delete` answers the value, and runs the block on a miss.
            r#"
            m = ObjectSpace::WeakKeyMap.new
            k = "K"
            m[k] = 42
            got = []
            r1 = m.delete("K")
            r2 = m.delete(Object.new) { |key| got << key.class.to_s; 5 }
            r3 = m.delete(Object.new)
            [r1, r2, r3, got, m.key?(k), k]
            "#,
            // `#clear` and `#size`.
            r#"
            m = ObjectSpace::WeakKeyMap.new
            a, b = "a", "b"
            m[a] = 1; m[b] = 2
            [m.key?(a), m.clear.equal?(m), m.key?(a), m[a], a, b]
            "#,
            // `#inspect` shows the size and nothing else — the pairs are
            // weak, so rendering them would be a promise it cannot keep.
            r#"
            m = ObjectSpace::WeakKeyMap.new
            a = "foo"
            empty = m.inspect
            m[a] = 1
            one = m.inspect
            [!!(empty =~ /\A#<ObjectSpace::WeakKeyMap:0x[0-9a-f]+ size=0>\z/),
             !!(one =~ /\A#<ObjectSpace::WeakKeyMap:0x[0-9a-f]+ size=1>\z/),
             a]
            "#,
            // A key is compared by `#hash` first: a different hash never
            // reaches `#eql?`.
            r#"
            calls = []
            k = Object.new
            k.define_singleton_method(:hash) { calls << :hash; 42 }
            k.define_singleton_method(:eql?) { |o| calls << :eql; false }
            other = Object.new
            other.define_singleton_method(:hash) { 7 }
            m = ObjectSpace::WeakKeyMap.new
            m[other] = 1
            [m[k], calls, other.class.to_s]
            "#,
            // An identical key finds itself even when `#eql?` says no.
            r#"
            k = Object.new
            k.define_singleton_method(:eql?) { |o| false }
            m = ObjectSpace::WeakKeyMap.new
            m[k] = 7
            [m[k], m.key?(k), m.getkey(k).equal?(k)]
            "#,
            // A value reachable only from the map survives a collection:
            // that is the whole difference from `WeakMap`.
            r#"
            m = ObjectSpace::WeakKeyMap.new
            key = "held"
            m[key] = "value that only the map holds"
            GC.start
            [m[key], m.key?(key), key]
            "#,
        ]);
        // A key with no `#hash` cannot be stored.
        run_test_error(
            r#"ObjectSpace::WeakKeyMap.new[BasicObject.new] = 1"#,
        );
    }

    /// The `WeakMap` corners its own specs cover, which were failing
    /// alongside the missing `WeakKeyMap`.
    #[test]
    fn weak_map_each_delete_and_inspect() {
        run_tests(&[
            // `#delete` runs the block on a miss, as `WeakKeyMap`'s does.
            r#"
            m = ObjectSpace::WeakMap.new
            k, v = Object.new, Object.new
            m[k] = v
            [m.delete(k).equal?(v), m.delete(Object.new) { |key| 5 },
             m.delete(Object.new), m.key?(k)]
            "#,
            // `#each` is not an Enumerator without a block: an empty map
            // answers itself, a non-empty one raises on the first pair.
            r#"
            m = ObjectSpace::WeakMap.new
            r = [m.each.equal?(m), m.each_key.equal?(m), m.each_value.equal?(m)]
            k, v = "k", "v"
            m[k] = v
            r << [(m.each rescue $!.class.to_s),
                  (m.each_key rescue $!.class.to_s),
                  (m.each_value rescue $!.class.to_s)]
            r << k << v
            "#,
            // It is Enumerable, which needs the include to happen after
            // the module exists.
            r#"ObjectSpace::WeakMap.include?(Enumerable)"#,
            // `#inspect` shows the pairs, and an empty map shows none.
            r#"
            m = ObjectSpace::WeakMap.new
            empty = m.inspect
            k, v = Object.new, Object.new
            m[k] = v
            one = m.inspect
            [!!(empty =~ /\A#<ObjectSpace::WeakMap:0x[0-9a-f]+>\z/),
             !!(one =~ /\A#<ObjectSpace::WeakMap:0x[0-9a-f]+: #<Object:0x[0-9a-f]+> => #<Object:0x[0-9a-f]+>>\z/),
             k.class.to_s, v.class.to_s]
            "#,
        ]);
    }

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

extern "C" fn weakkeymap_alloc(class_id: ClassId, _globals: &mut Globals) -> Value {
    Value::new_weakkeymap(class_id)
}

///
/// The index of the pair whose key is `eql?` to `key`, by CRuby's rule
/// for a `WeakKeyMap`: `#hash` first, then `#eql?`.
///
/// An immediate — Integer, Float, Symbol, `true`, `false`, `nil` — is
/// never a key, because it has no cell and so can never be collected;
/// CRuby refuses to store under one and answers `nil` for every lookup.
/// That check is the caller's, since only `#[]=` raises for it.
///
/// The scan is linear. A weak map is a registry of a few entries, which
/// is the same reasoning `WeakMap` records for its own identity scan,
/// and it keeps the comparison in one place — `#eql?` can run arbitrary
/// Ruby, which a hash table would have to do while rehashing too.
///
fn weakkeymap_find(
    vm: &mut Executor,
    globals: &mut Globals,
    self_val: Value,
    key: Value,
) -> Result<Option<usize>> {
    if key.try_rvalue().is_none() {
        return Ok(None);
    }
    let entries: Vec<Value> = map_of(globals, &self_val)?
        .entries()
        .iter()
        .map(|(k, _)| *k)
        .collect();
    // Identity first, so a key finds itself however its `#eql?`
    // behaves — CRuby has an example for exactly that.
    if let Some(i) = entries.iter().position(|k| k.id() == key.id()) {
        return Ok(Some(i));
    }
    // Then `#hash`, once, on the key being looked up. A key with no
    // `#hash` fails here, which is what makes `map[BasicObject.new] = 1`
    // a NoMethodError rather than a stored pair.
    let hash = IdentId::get_id("hash");
    let key_hash = vm.invoke_method_inner(globals, hash, key, &[], None, None)?;
    for (i, k) in entries.into_iter().enumerate() {
        let k_hash = vm.invoke_method_inner(globals, hash, k, &[], None, None)?;
        if !vm
            .invoke_method_inner(globals, IdentId::_EQ, k_hash, &[key_hash], None, None)?
            .as_bool()
        {
            continue;
        }
        // `#eql?` goes to the key being looked up, not the one held:
        // CRuby asks the argument, and the specs check that the stored
        // key is never asked.
        let eq = vm.invoke_method_inner(globals, IdentId::get_id("eql?"), key, &[k], None, None)?;
        if eq.as_bool() {
            return Ok(Some(i));
        }
    }
    Ok(None)
}

/// WeakKeyMap#[](key) -> the value, or nil
#[monoruby_builtin]
fn weakkeymap_index(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let self_val = lfp.self_val();
    map_of(globals, &self_val)?;
    Ok(match weakkeymap_find(vm, globals, self_val, lfp.arg(0))? {
        Some(i) => map_of(globals, &self_val)?.entries()[i].1,
        None => Value::nil(),
    })
}

/// WeakKeyMap#[]=(key, value) -> value
#[monoruby_builtin]
fn weakkeymap_index_assign(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let mut self_val = lfp.self_val();
    map_of(globals, &self_val)?;
    let (key, value) = (lfp.arg(0), lfp.arg(1));
    // A key with no cell can never be collected, so the pair could
    // never be cleared and the map would leak. CRuby refuses it.
    if key.try_rvalue().is_none() {
        return Err(MonorubyErr::argumenterr(
            "WeakKeyMap keys must be garbage collectable",
        ));
    }
    // The lookup calls `#hash` on the key, so one that has none fails
    // there — `map[BasicObject.new] = 1` is a NoMethodError.
    match weakkeymap_find(vm, globals, self_val, key)? {
        // An equal key replaces the one already held, as well as its
        // value: CRuby's newest key wins, so `#getkey` afterwards
        // answers the one stored most recently rather than the first.
        Some(i) => self_val.as_weakmap_inner_mut().set_at(i, key, value),
        None => self_val.as_weakmap_inner_mut().push(key, value),
    }
    // The values are the strong half here, so an old map taking a young
    // one has to be remembered for the next minor collection.
    self_val.write_barrier(value);
    Ok(value)
}

/// WeakKeyMap#getkey(key) -> the key the map holds, or nil
#[monoruby_builtin]
fn weakkeymap_getkey(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let self_val = lfp.self_val();
    map_of(globals, &self_val)?;
    Ok(match weakkeymap_find(vm, globals, self_val, lfp.arg(0))? {
        Some(i) => map_of(globals, &self_val)?.entries()[i].0,
        None => Value::nil(),
    })
}

/// WeakKeyMap#key?(key) -> bool
#[monoruby_builtin]
fn weakkeymap_key_p(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let self_val = lfp.self_val();
    map_of(globals, &self_val)?;
    let found = weakkeymap_find(vm, globals, self_val, lfp.arg(0))?;
    Ok(Value::bool(found.is_some()))
}

/// WeakKeyMap#delete(key) { |key| } -> the value, the block's answer, or nil
#[monoruby_builtin]
fn weakkeymap_delete(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let mut self_val = lfp.self_val();
    map_of(globals, &self_val)?;
    let key = lfp.arg(0);
    if let Some(i) = weakkeymap_find(vm, globals, self_val, key)? {
        return Ok(self_val.as_weakmap_inner_mut().remove_at(i));
    }
    // A miss runs the block on the key, if one was given.
    match lfp.block() {
        Some(bh) => {
            let data = vm.get_block_data(globals, bh)?;
            vm.invoke_block(globals, &data, &[key])
        }
        None => Ok(Value::nil()),
    }
}

/// WeakKeyMap#clear -> self
#[monoruby_builtin]
fn weakkeymap_clear(
    _: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let mut self_val = lfp.self_val();
    map_of(globals, &self_val)?;
    self_val.as_weakmap_inner_mut().clear();
    Ok(self_val)
}

/// WeakKeyMap#inspect -> String
///
/// CRuby shows the size and nothing else — the pairs are weak, so
/// rendering them would be a promise it cannot keep.
#[monoruby_builtin]
fn weakkeymap_inspect(
    _: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let self_val = lfp.self_val();
    map_of(globals, &self_val)?;
    let name = globals
        .store
        .get_class_name(self_val.real_class(&globals.store).id());
    Ok(Value::string(format!(
        "#<{name}:0x{:016x} size={}>",
        self_val.id(),
        map_of(globals, &self_val)?.len()
    )))
}

/// `#<ClassName:0xaddr>` for an object, without dispatching.
///
/// A weak map's halves may be `BasicObject`s, which have no `#inspect`
/// — and calling one would run Ruby while rendering a map whose pairs
/// the collector may break. CRuby renders them the same way, with
/// `rb_any_to_s`.
fn plain_inspect(globals: &Globals, v: Value) -> String {
    match v.try_rvalue() {
        Some(_) => format!(
            "#<{}:0x{:016x}>",
            globals.store.get_class_name(v.real_class(&globals.store).id()),
            v.id()
        ),
        // An immediate has no address to show, so it renders itself.
        None => v.inspect(&globals.store),
    }
}

/// WeakMap#inspect -> String
///
/// CRuby shows the pairs: `#<ObjectSpace::WeakMap:0xaddr: k => v, …>`,
/// and just `#<ObjectSpace::WeakMap:0xaddr>` when there are none.
#[monoruby_builtin]
fn weakmap_inspect(
    _: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let self_val = lfp.self_val();
    let pairs: Vec<(Value, Value)> = map_of(globals, &self_val)?.iter().collect();
    let name = globals
        .store
        .get_class_name(self_val.real_class(&globals.store).id());
    let head = format!("#<{name}:0x{:016x}", self_val.id());
    if pairs.is_empty() {
        return Ok(Value::string(format!("{head}>")));
    }
    let body = pairs
        .iter()
        .map(|(k, v)| format!("{} => {}", plain_inspect(globals, *k), plain_inspect(globals, *v)))
        .collect::<Vec<_>>()
        .join(", ");
    Ok(Value::string(format!("{head}: {body}>")))
}
