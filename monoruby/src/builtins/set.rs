use super::*;

//
// Set class
//
// Internally, a Set is a Hash where keys are the set elements and values are `true`.
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_under_obj("Set", SET_CLASS, ObjTy::HASH);
    globals.define_builtin_class_func_rest(SET_CLASS, "[]", set_index);
    globals.define_builtin_class_func_rest(SET_CLASS, "new", new);
    globals.store[SET_CLASS].set_alloc_func(set_alloc_func);

    globals.define_builtin_func(SET_CLASS, "<<", add, 1);
    globals.define_builtin_func(SET_CLASS, "add", add, 1);
    globals.define_builtin_func(SET_CLASS, "add?", add_q, 1);
    // `include?` is the method; `member?` and `===` are its aliases
    // (`rb_define_alias(rb_cSet, "===", "include?")`), which is what
    // `Method#original_name` and `#inspect` report — builtins/set.rb
    // re-declares the alias groups of this table as true aliases.
    globals.define_builtin_funcs(SET_CLASS, "include?", &["member?", "==="], teq, 1);
    globals.define_builtin_funcs(SET_CLASS, "size", &["length"], size, 0);
    globals.define_builtin_func(SET_CLASS, "empty?", empty_, 0);
    globals.define_builtin_func(SET_CLASS, "clear", clear, 0);
    globals.define_builtin_func(SET_CLASS, "delete", delete, 1);
    globals.define_builtin_func(SET_CLASS, "delete?", delete_q, 1);
    globals.define_builtin_func(SET_CLASS, "each", each, 0);
    globals.define_builtin_func(SET_CLASS, "to_a", to_a, 0);
    // One FuncId, so that `Set#to_s` and `Set#inspect` compare equal as
    // Method objects.
    globals.define_builtin_funcs(SET_CLASS, "inspect", &["to_s"], set_inspect, 0);
    // No `dup` / `clone` of its own, as in CRuby: `Kernel#dup` copies the
    // table (and its comparison mode) and then runs this.
    globals.define_private_builtin_func(SET_CLASS, "initialize_copy", initialize_copy, 1);
    globals.define_builtin_func_rest(SET_CLASS, "merge", merge);
    globals.define_builtin_func(SET_CLASS, "subtract", subtract, 1);
    globals.define_builtin_func(SET_CLASS, "replace", replace, 1);
    globals.define_builtin_funcs(SET_CLASS, "&", &["intersection"], intersection, 1);
    globals.define_builtin_funcs(SET_CLASS, "|", &["+", "union"], union_, 1);
    globals.define_builtin_funcs(SET_CLASS, "-", &["difference"], difference, 1);
    globals.define_builtin_func(SET_CLASS, "^", symmetric_difference, 1);
    globals.define_builtin_func(SET_CLASS, "==", eq, 1);
    globals.define_builtin_func(SET_CLASS, "eql?", eq, 1);
    globals.define_builtin_funcs(SET_CLASS, "subset?", &["<="], subset_, 1);
    globals.define_builtin_funcs(SET_CLASS, "superset?", &[">="], superset_, 1);
    globals.define_builtin_func(SET_CLASS, "proper_subset?", proper_subset_, 1);
    globals.define_builtin_func(SET_CLASS, "proper_superset?", proper_superset_, 1);
    globals.define_builtin_func(SET_CLASS, "<", proper_subset_, 1);
    globals.define_builtin_func(SET_CLASS, ">", proper_superset_, 1);
    globals.define_builtin_func(SET_CLASS, "<=>", spaceship, 1);
    globals.define_builtin_func(SET_CLASS, "disjoint?", disjoint_, 1);
    globals.define_builtin_func(SET_CLASS, "intersect?", intersect_, 1);
    globals.define_builtin_func(SET_CLASS, "delete_if", delete_if, 0);
    globals.define_builtin_func(SET_CLASS, "keep_if", keep_if, 0);
    globals.define_builtin_funcs(SET_CLASS, "select!", &["filter!"], select_, 0);
    globals.define_builtin_func(SET_CLASS, "reject!", reject_, 0);
    globals.define_builtin_funcs(SET_CLASS, "collect!", &["map!"], collect_, 0);
    globals.define_builtin_func(SET_CLASS, "flatten", flatten, 0);
    globals.define_builtin_func(SET_CLASS, "flatten!", flatten_, 0);
    globals.define_builtin_func_with(SET_CLASS, "join", join, 0, 1, false);
    globals.define_builtin_func(SET_CLASS, "classify", classify, 0);
    globals.define_builtin_func(SET_CLASS, "divide", divide, 0);
    globals.define_builtin_func(SET_CLASS, "reset", reset, 0);
    globals.define_builtin_func(SET_CLASS, "hash", set_hash, 0);
    globals.define_builtin_func(SET_CLASS, "compare_by_identity", compare_by_identity, 0);
    globals.define_builtin_func(SET_CLASS, "compare_by_identity?", compare_by_identity_q, 0);
}

///
/// ### Set#compare_by_identity
///
/// Makes the set compare its members by identity. A `Set` is backed by
/// a `Hash`, so this delegates to the hash's identity machinery.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Set/i/compare_by_identity.html]
#[monoruby_builtin]
fn compare_by_identity(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    lfp.self_val().ensure_not_frozen(&globals.store)?;
    lfp.expect_no_block()?;
    lfp.self_val().as_hash().compare_by_identity(vm, globals)?;
    Ok(lfp.self_val())
}

///
/// ### Set#compare_by_identity?
///
/// [https://docs.ruby-lang.org/ja/latest/method/Set/i/compare_by_identity=3f.html]
#[monoruby_builtin]
fn compare_by_identity_q(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    lfp.expect_no_block()?;
    Ok(Value::bool(lfp.self_val().as_hash().is_compare_by_identity()))
}

/// Create a new empty Set (a Hash with class SET_CLASS).
fn new_empty_set() -> Value {
    Value::hash_with_class_and_default(SET_CLASS, Value::nil())
}

/// Create a new empty Set of `model`'s class that compares its members the
/// same way `model` does — what CRuby's `set_s_alloc(rb_obj_class(set))`
/// followed by `set_i_compare_by_identity` builds for every derived set
/// (`&`, `|`, `-`, `^`, `map!`, `flatten`, the subsets of `classify` /
/// `divide`), since 4.0.9 for the identity half.
///
/// The mode is set while the set is still empty, so every subsequent insert
/// already uses the right comparison — flipping it afterwards would rehash
/// (and merge) elements that were meant to stay distinct.
fn new_empty_set_like(model: Value, store: &Store) -> Result<Value> {
    let set = Value::hash_with_class_and_default(model.real_class(store).id(), Value::nil());
    if model.as_hash().is_compare_by_identity() {
        set.as_hash().set_compare_by_identity_empty(true)?;
    }
    Ok(set)
}

/// Create a Set like `model` ([`new_empty_set_like`]) from an iterator
/// of values.
fn set_from_iter_like(
    model: Value,
    iter: impl Iterator<Item = Value>,
    vm: &mut Executor,
    globals: &mut Globals,
) -> Result<Value> {
    let set = new_empty_set_like(model, &globals.store)?;
    // The new Set is held only here while each member's `#hash` / `#eql?`
    // run: root it, or a collection there frees it.
    vm.with_temp_scope(|vm| {
        vm.temp_push(set);
        let mut h = set.as_hash();
        for v in iter {
            h.aset(v, Value::bool(true), vm, globals)?;
        }
        Ok(set)
    })
}

/// Get the keys of a Set's internal hash as a Vec.
fn set_keys(val: Value) -> Vec<Value> {
    val.as_hashmap_inner().keys()
}

/// [`set_keys`] for a walk that runs Ruby (`#hash` / `#eql?`) between one
/// member and the next, which must not hold a live iterator over a table
/// that Ruby may change: each member is also pushed onto the temp stack,
/// since one that Ruby removes from `set` meanwhile is held by nothing
/// else. Call it inside `with_temp_scope`.
fn set_keys_rooted(vm: &mut Executor, set: Value) -> Vec<Value> {
    let keys = set_keys(set);
    for k in &keys {
        vm.temp_push(*k);
    }
    keys
}

/// Hand `set`'s members to `f` in order, the way CRuby's `set_iter` walks
/// its table: by position, under the iteration guard, each member read
/// from the live table as it is reached. A member added meanwhile raises,
/// and one deleted meanwhile is tombstoned in place and never reached.
/// A walk of a snapshot instead held the members only in a Rust `Vec`,
/// and one the block deleted was freed by a collection and then yielded.
fn set_walk(
    vm: &mut Executor,
    globals: &mut Globals,
    set: Value,
    mut f: impl FnMut(&mut Executor, &mut Globals, Value) -> Result<()>,
) -> Result<()> {
    // `hash` is a view of `set` itself, so each turn reads the live table.
    let hash = set.as_hash();
    let _iter_guard = hash.iter_guard();
    let mut i = 0;
    while i < hash.entry_count() {
        if let Some((k, _)) = hash.entry_at(i) {
            f(vm, globals, k)?;
        }
        i += 1;
    }
    Ok(())
}

/// Whether `f` answers `true` for a member of `set`, walked as
/// [`set_walk`] walks it up to the first such member (a walk whose `f`
/// always answers `false` reaches every member). Each member stays rooted
/// while `f` runs: Ruby code there may delete it from `set` while `f`
/// still uses it, and hashing an Array member, for one, runs its
/// elements' `#hash` with nothing else holding the Array.
fn set_any(
    vm: &mut Executor,
    globals: &mut Globals,
    set: Value,
    mut f: impl FnMut(&mut Executor, &mut Globals, Value) -> Result<bool>,
) -> Result<bool> {
    let hash = set.as_hash();
    let _iter_guard = hash.iter_guard();
    vm.with_temp_scope(|vm| {
        // One slot, kept for the walk, holds the member `f` is using. An
        // immediate (an Integer, a Symbol) is never collected and is not
        // stored: on the M1 that store, made on every turn of a walk over
        // Integers, cost Set#== about 40% more cycles.
        let slot = vm.temp_len();
        vm.temp_push(Value::nil());
        let mut i = 0;
        while i < hash.entry_count() {
            if let Some((k, _)) = hash.entry_at(i) {
                if !k.is_packed_value() {
                    vm.temp_set(slot, k);
                }
                if f(vm, globals, k)? {
                    return Ok(true);
                }
            }
            i += 1;
        }
        Ok(false)
    })
}

/// `delete_if` / `keep_if` / `reject!` / `select!`: delete, as they are
/// reached, the members for which the block answers `delete_when`.
/// Whether the set shrank, by these deletions or by the block's own.
fn set_delete_where(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    bh: BlockHandler,
    delete_when: bool,
) -> Result<bool> {
    let self_val = lfp.self_val();
    self_val.ensure_not_frozen(&globals.store)?;
    let data = vm.get_block_data(globals, bh)?;
    let size = self_val.as_hash().len();
    // `set_any` keeps the member rooted until `remove` is done with it:
    // the block may have deleted it already, and `remove` hashes it again.
    set_any(vm, globals, self_val, |vm, globals, k| {
        if vm.invoke_block(globals, &data, &[k])?.as_bool() == delete_when {
            self_val.as_hash().remove(k, vm, globals)?;
        }
        Ok(false)
    })?;
    Ok(self_val.as_hash().len() != size)
}

/// Check if a value is a Set.
fn is_set(val: Value, store: &Store) -> bool {
    val.is_kind_of(store, SET_CLASS)
}

/// Convert an enumerable Value to a Vec of elements, each of which is
/// also pushed onto the temp stack: the callers run Ruby (`#hash` /
/// `#eql?`) between one element and the next, while the elements of a
/// `#to_a` answer, or ones that Ruby removes from `val` meanwhile, are
/// held by nothing else. Call it inside `with_temp_scope`.
fn enum_to_vec(vm: &mut Executor, globals: &mut Globals, val: Value) -> Result<Vec<Value>> {
    let elems: Vec<Value> = if is_set(val, &globals.store) {
        set_keys(val)
    } else if let Some(ary) = val.try_array_ty() {
        ary.iter().copied().collect()
    } else {
        // Check if it responds to each (is enumerable)
        let each_id = IdentId::EACH;
        if globals.check_method(val, each_id).is_none() {
            return Err(MonorubyErr::argumenterr(format!(
                "value must be enumerable"
            )));
        }
        let result = vm.invoke_method_inner(globals, IdentId::TO_A, val, &[], None, None)?;
        let ary = result.expect_array_ty(globals)?;
        ary.iter().copied().collect()
    };
    for v in &elems {
        vm.temp_push(*v);
    }
    Ok(elems)
}

///
/// ### Set.new
///
/// - new(enum = nil) -> Set
/// - new(enum) {|o| ... } -> Set
///
/// [https://docs.ruby-lang.org/ja/latest/method/Set/s/new.html]
///
/// Explicit class method so that `Set.new` goes through `Set.allocate`
/// (producing a Hash-backed instance) and then calls the Ruby-level
/// `initialize`. Without this override, the default `Class#new` inline
/// fast path allocates a plain Object instead of a Hash-backed Set.
///
/// Takes any number of arguments and hands them all to `initialize`: a
/// subclass may widen the constructor (chunky_png's `Palette < Set` has
/// `initialize(enum, decoding_map = nil)`), and it is `initialize`, not
/// this trampoline, that checks the arity.
#[monoruby_builtin]
fn new(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _pc: BytecodePtr) -> Result<Value> {
    let obj =
        vm.invoke_method_inner(globals, IdentId::ALLOCATE, lfp.self_val(), &[], None, None)?;
    let args: Vec<Value> = lfp.arg(0).as_array().iter().copied().collect();
    vm.invoke_method_inner(
        globals,
        IdentId::INITIALIZE,
        obj,
        &args,
        lfp.block(),
        None,
    )?;
    Ok(obj)
}

/// Allocator for `Set` and its subclasses. Set uses the same underlying hash
/// storage as `Hash`, so the allocator hands back an empty hash typed with
/// the given class id.
pub(crate) extern "C" fn set_alloc_func(class_id: ClassId, _: &mut Globals) -> Value {
    Value::hash_with_class_and_default(class_id, Value::nil())
}

///
/// ### Set.[]
///
/// - Set[*ary] -> Set
///
/// [https://docs.ruby-lang.org/ja/latest/method/Set/s/=5b=5d.html]
#[monoruby_builtin]
fn set_index(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let args = lfp.arg(0).as_array();
    // Of the receiver's class: `MySet[1, 2]` is a `MySet`.
    let set = Value::hash_with_class_and_default(lfp.self_val().as_class().id(), Value::nil());
    // The new Set is held only here while each member's `#hash` / `#eql?`
    // run: root it, or a collection there frees it.
    vm.with_temp_scope(|vm| {
        vm.temp_push(set);
        let mut h = set.as_hash();
        for v in args.iter().copied() {
            h.aset(v, Value::bool(true), vm, globals)?;
        }
        Ok(set)
    })
}

///
/// ### Set#<<
///
/// - add(o) -> self
/// - self << o -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Set/i/=3c=3c.html]
#[monoruby_builtin]
fn add(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let val = lfp.arg(0);
    let self_val = lfp.self_val();
    self_val
        .as_hash_mut(&globals.store)?
        .aset(val, Value::bool(true), vm, globals)?;
    Ok(self_val)
}

///
/// ### Set#add?
///
/// - add?(o) -> self | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Set/i/add=3f.html]
#[monoruby_builtin]
fn add_q(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let val = lfp.arg(0);
    let self_val = lfp.self_val();
    self_val.ensure_not_frozen(&globals.store)?;
    let already = self_val.as_hashmap_inner().contains_key(val, vm, globals)?;
    if already {
        Ok(Value::nil())
    } else {
        self_val
            .as_hash().aset(val, Value::bool(true), vm, globals)?;
        Ok(self_val)
    }
}

///
/// ### Set#===
///
/// - include?(o) -> bool
/// - member?(o) -> bool
/// - self === o -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Set/i/=3d=3d=3d.html]
#[monoruby_builtin]
fn teq(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let val = lfp.arg(0);
    let self_val = lfp.self_val();
    let result = self_val.as_hashmap_inner().contains_key(val, vm, globals)?;
    Ok(Value::bool(result))
}

///
/// ### Set#size
///
/// - size -> Integer
/// - length -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/Set/i/length.html]
#[monoruby_builtin]
fn size(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    Ok(Value::integer(self_val.as_hashmap_inner().len() as i64))
}

///
/// ### Set#empty?
///
/// - empty? -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Set/i/empty=3f.html]
#[monoruby_builtin]
fn empty_(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    Ok(Value::bool(self_val.as_hashmap_inner().is_empty()))
}

///
/// ### Set#clear
///
/// - clear -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Set/i/clear.html]
#[monoruby_builtin]
fn clear(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    self_val.ensure_not_frozen(&globals.store)?;
    if self_val.as_hash().iter_active() {
        // A walk of this set is under way (`set_walk`): delete the members
        // where they stand, as CRuby's `set_i_clear` does then, which
        // leaves their positions to the walk as tombstones instead of
        // dropping the table under it. Deleting them by key would run
        // each one's `#hash`, and one that deleted the others would leave
        // them to a snapshot nothing roots, for a collection to free.
        self_val.as_hash().clear_during_iteration(vm, globals)?;
    } else {
        self_val.as_hash().clear()?;
    }
    Ok(self_val)
}

///
/// ### Set#delete
///
/// - delete(o) -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Set/i/delete.html]
#[monoruby_builtin]
fn delete(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let val = lfp.arg(0);
    let self_val = lfp.self_val();
    self_val.ensure_not_frozen(&globals.store)?;
    self_val.as_hash().remove(val, vm, globals)?;
    Ok(self_val)
}

///
/// ### Set#delete?
///
/// - delete?(o) -> self | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Set/i/delete.html]
#[monoruby_builtin]
fn delete_q(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let val = lfp.arg(0);
    let self_val = lfp.self_val();
    self_val.ensure_not_frozen(&globals.store)?;
    let removed = self_val.as_hash().remove(val, vm, globals)?;
    if removed.is_some() {
        Ok(self_val)
    } else {
        Ok(Value::nil())
    }
}

///
/// ### Set#each
///
/// - each {|o| ... } -> self
/// - each -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/Set/i/each.html]
#[monoruby_builtin]
fn each(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, pc: BytecodePtr) -> Result<Value> {
    let bh = match lfp.block() {
        None => {
            let id = IdentId::EACH;
            return vm.generate_enumerator(id, lfp.self_val(), lfp.iter().collect(), pc);
        }
        Some(block) => block,
    };
    let self_val = lfp.self_val();
    let data = vm.get_block_data(globals, bh)?;
    set_walk(vm, globals, self_val, |vm, globals, val| {
        vm.invoke_block(globals, &data, &[val])?;
        Ok(())
    })?;
    Ok(lfp.self_val())
}

///
/// ### Set#to_a
///
/// - to_a -> Array
///
/// [https://docs.ruby-lang.org/ja/latest/method/Set/i/to_a.html]
#[monoruby_builtin]
fn to_a(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let keys = set_keys(lfp.self_val());
    Ok(Value::array_from_vec(keys))
}

///
/// ### Set#inspect
///
/// - inspect -> String
/// - to_s -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/Set/i/inspect.html]
///
/// CRuby's `set_inspect`: every element through `rb_inspect` — its own
/// `#inspect`, dispatched — under `rb_exec_recursive`, so a set reached
/// again from one of its own elements is `Set[...]`. A direct subclass
/// of Set never gets here: `Set::SubclassCompatible#inspect`, which
/// `Set.inherited` mixes into it, answers in the older `#<MySet: {…}>`
/// shape.
///
/// Registered as an alias pair so the two method names share a single
/// `FuncId` and compare equal as `Method` objects.
#[monoruby_builtin]
fn set_inspect(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    use crate::builtins::encoding::{InspectBuf, inspect_resenc, rb_inspect_in};
    let self_val = lfp.self_val();
    let class = self_val.real_class(&globals.store).id();
    let name = globals.store.get_class_name(class);
    // `Set[...]`-style for every class that reaches here: a direct
    // subclass of Set has `Set::SubclassCompatible#inspect` in front
    // of this (the old `#<Name: {...}>` form), so only Set itself and
    // `Set::CoreSet` and its descendants get here (#2044).
    let (open, close) = (format!("{name}["), "]");
    // The class name may be anything; only the punctuation is ASCII.
    let start = |store: &Store| -> Result<InspectBuf> {
        let mut buf = InspectBuf::new("");
        buf.append_str(store, &open)?;
        Ok(buf)
    };
    let mut recursive = start(&globals.store)?;
    recursive.cat("...");
    recursive.cat(close);
    let recursive = recursive.into_value();
    // The elements are the ones the set held when `inspect` began,
    // whatever an `#inspect` along the way removes — so they are rooted
    // here rather than through the set.
    let keys = set_keys(self_val);
    vm.with_temp_scope(|vm| {
        for k in &keys {
            vm.temp_push(*k);
        }
        crate::value::exec_recursive(
            self_val.id(),
            || {
                let resenc = inspect_resenc(globals);
                let mut buf = start(&globals.store)?;
                for (i, k) in keys.iter().enumerate() {
                    if i > 0 {
                        buf.cat(", ");
                    }
                    let s = rb_inspect_in(vm, globals, *k, resenc)?;
                    buf.append(&globals.store, s.as_rstring_inner())?;
                }
                buf.cat(close);
                Ok(buf.into_value())
            },
            recursive,
        )
    })
}

///
/// ### Set#initialize_copy
///
/// - initialize_copy(other) -> self
///
/// Make `self` a copy of the Set *other*: its elements and its comparison
/// mode. What `dup` and `clone` run on the copy (CRuby's
/// `set_i_initialize_copy`).
#[monoruby_builtin]
fn initialize_copy(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let self_val = lfp.self_val();
    let other = lfp.arg(0);
    if self_val.id() == other.id() {
        return Ok(self_val);
    }
    // As in CRuby, before `other` is looked at. The copied table carries
    // no iteration count, so the walk under way would release its guard
    // on a table that never took it.
    if self_val.as_hash().iter_active() {
        return Err(MonorubyErr::runtimeerr(
            "cannot replace set during iteration",
        ));
    }
    if !is_set(other, &globals.store) {
        return Err(MonorubyErr::wrong_argument_type(
            &globals.store,
            other,
            "set",
        ));
    }
    // The table as it is — members and the `compare_by_identity` mode —
    // with no rehashing.
    let inner = other.as_hash().inner().clone_inner();
    self_val.as_hash_mut(&globals.store)?.replace_inner(inner);
    Ok(self_val)
}

///
/// ### Set#merge
///
/// - merge(*enums) -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Set/i/merge.html]
#[monoruby_builtin]
fn merge(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    self_val.ensure_not_frozen(&globals.store)?;
    let args = lfp.arg(0).as_array();
    for arg in args.iter().copied() {
        vm.with_temp_scope(|vm| {
            let elems = enum_to_vec(vm, globals, arg)?;
            for elem in elems {
                self_val
                    .as_hash()
                    .aset(elem, Value::bool(true), vm, globals)?;
            }
            Ok(())
        })?;
    }
    Ok(self_val)
}

///
/// ### Set#subtract
///
/// - subtract(enum) -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Set/i/subtract.html]
#[monoruby_builtin]
fn subtract(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    self_val.ensure_not_frozen(&globals.store)?;
    vm.with_temp_scope(|vm| {
        let elems = enum_to_vec(vm, globals, lfp.arg(0))?;
        for elem in elems {
            self_val.as_hash().remove(elem, vm, globals)?;
        }
        Ok(self_val)
    })
}

///
/// ### Set#replace
///
/// - replace(enum) -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Set/i/replace.html]
#[monoruby_builtin]
fn replace(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    self_val.ensure_not_frozen(&globals.store)?;
    let arg = lfp.arg(0);
    // Replacing with a Set adopts *its* comparison mode, in both directions
    // — CRuby implements this as `rb_hash_replace`, which copies the source
    // hash wholesale. Any other Enumerable leaves `self`'s mode alone.
    let ident = if is_set(arg, &globals.store) {
        Some(arg.as_hash().is_compare_by_identity())
    } else {
        None
    };
    vm.with_temp_scope(|vm| {
        let elems = enum_to_vec(vm, globals, arg)?;
        self_val.as_hash().clear()?;
        if let Some(ident) = ident {
            self_val.as_hash().set_compare_by_identity_empty(ident)?;
        }
        for elem in elems {
            self_val
                .as_hash().aset(elem, Value::bool(true), vm, globals)?;
        }
        Ok(self_val)
    })
}

///
/// ### Set#&
///
/// - self & enum -> Set
/// - intersection(enum) -> Set
///
/// [https://docs.ruby-lang.org/ja/latest/method/Set/i/=26.html]
#[monoruby_builtin]
fn intersection(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let self_val = lfp.self_val();
    // `other` and `result` are fresh Sets held only here while members'
    // `#hash` / `#eql?` run: they sit on the temp stack, with the members
    // walked, or a collection there frees them.
    vm.with_temp_scope(|vm| {
        let other_elems = enum_to_vec(vm, globals, lfp.arg(0))?;
        // Build a temp hash for the other side. It compares like `self` does, so
        // the membership tests below follow `self`'s rule — CRuby probes the
        // enum's elements against `self`'s own hash.
        let other = new_empty_set_like(self_val, &globals.store)?;
        vm.temp_push(other);
        for elem in other_elems {
            other.as_hash().aset(elem, Value::bool(true), vm, globals)?;
        }
        let other_inner = other.as_hashmap_inner();
        // Since 4.0.9 the result compares like `self` too (CRuby 4.0.6 built a
        // plain Set here, the one derived set that dropped the flag).
        let result = new_empty_set_like(self_val, &globals.store)?;
        vm.temp_push(result);
        for k in set_keys_rooted(vm, self_val) {
            if other_inner.contains_key(k, vm, globals)? {
                result.as_hash().aset(k, Value::bool(true), vm, globals)?;
            }
        }
        Ok(result)
    })
}

///
/// ### Set#|
///
/// - self | enum -> Set
/// - self + enum -> Set
/// - union(enum) -> Set
///
/// [https://docs.ruby-lang.org/ja/latest/method/Set/i/=7c.html]
#[monoruby_builtin]
fn union_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    // `result` is a fresh Set held only here while members' `#hash` /
    // `#eql?` run: it sits on the temp stack, with the members to add, or
    // a collection there frees it.
    vm.with_temp_scope(|vm| {
        let self_keys = set_keys_rooted(vm, lfp.self_val());
        let other_elems = enum_to_vec(vm, globals, lfp.arg(0))?;
        // CRuby builds `|` by duplicating `self` and merging the enum into it, so
        // the result keeps `self`'s comparison mode (and the merge follows it).
        let result = new_empty_set_like(lfp.self_val(), &globals.store)?;
        vm.temp_push(result);
        for k in self_keys {
            result
                .as_hash().aset(k, Value::bool(true), vm, globals)?;
        }
        for k in other_elems {
            result
                .as_hash().aset(k, Value::bool(true), vm, globals)?;
        }
        Ok(result)
    })
}

///
/// ### Set#-
///
/// - self - enum -> Set
/// - difference(enum) -> Set
///
/// [https://docs.ruby-lang.org/ja/latest/method/Set/i/=2d.html]
#[monoruby_builtin]
fn difference(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    // `other` and `result` are rooted as in `intersection`.
    vm.with_temp_scope(|vm| {
        let other_elems = enum_to_vec(vm, globals, lfp.arg(0))?;
        // CRuby builds `-` by duplicating `self` and deleting the enum's elements
        // from it: the result keeps `self`'s comparison mode, and so do the
        // deletions (hence the `_like` temp probed below).
        let other = new_empty_set_like(self_val, &globals.store)?;
        vm.temp_push(other);
        for elem in other_elems {
            other.as_hash().aset(elem, Value::bool(true), vm, globals)?;
        }
        let other_inner = other.as_hashmap_inner();
        let result = new_empty_set_like(self_val, &globals.store)?;
        vm.temp_push(result);
        for k in set_keys_rooted(vm, self_val) {
            if !other_inner.contains_key(k, vm, globals)? {
                result.as_hash().aset(k, Value::bool(true), vm, globals)?;
            }
        }
        Ok(result)
    })
}

///
/// ### Set#^
///
/// - self ^ enum -> Set
///
/// [https://docs.ruby-lang.org/ja/latest/method/Set/i/=5e.html]
#[monoruby_builtin]
fn symmetric_difference(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let self_val = lfp.self_val();
    let self_inner = self_val.as_hashmap_inner();
    // `other` and `result` are rooted as in `intersection`.
    vm.with_temp_scope(|vm| {
        let other_elems = enum_to_vec(vm, globals, lfp.arg(0))?;
        // Both halves of the symmetric difference are probed with `self`'s
        // comparison mode, and the result keeps it (as CRuby's `^` does).
        let other = new_empty_set_like(self_val, &globals.store)?;
        vm.temp_push(other);
        for elem in other_elems {
            other.as_hash().aset(elem, Value::bool(true), vm, globals)?;
        }
        let other_inner = other.as_hashmap_inner();
        let result = new_empty_set_like(self_val, &globals.store)?;
        vm.temp_push(result);
        for k in set_keys_rooted(vm, self_val) {
            if !other_inner.contains_key(k, vm, globals)? {
                result.as_hash().aset(k, Value::bool(true), vm, globals)?;
            }
        }
        // `other` is this method's own, which no Ruby code can reach.
        for (k, _) in other_inner.iter() {
            if !self_inner.contains_key(k, vm, globals)? {
                result.as_hash().aset(k, Value::bool(true), vm, globals)?;
            }
        }
        Ok(result)
    })
}

///
/// ### Set#==
///
/// - self == set -> bool
/// - eql?(set) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Set/i/=3d=3d.html]
#[monoruby_builtin]
fn eq(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let other = lfp.arg(0);
    // A Set equals itself without a membership probe per element, which is
    // where `Set#==` starts in CRuby too.
    if lfp.self_val().id() == other.id() {
        return Ok(Value::bool(true));
    }
    if !is_set(other, &globals.store) {
        return Ok(Value::bool(false));
    }
    let self_val = lfp.self_val();
    // A set comparing by identity is never equal to one that does not.
    if self_val.as_hash().is_compare_by_identity() != other.as_hash().is_compare_by_identity() {
        return Ok(Value::bool(false));
    }
    let self_inner = self_val.as_hashmap_inner();
    let other_inner = other.as_hashmap_inner();
    if self_inner.len() != other_inner.len() {
        return Ok(Value::bool(false));
    }
    crate::value::exec_recursive_paired(
        self_val.id(),
        other.id(),
        || {
            // Walked by position (`set_any`): a live table iterator would go on
            // to read a member that a `#hash` here deleted, which nothing holds
            // once it is gone, so a collection frees it first.
            let missing = set_any(vm, globals, self_val, |vm, globals, k| {
                Ok(!other_inner.contains_key(k, vm, globals)?)
            })?;
            Ok(Value::bool(!missing))
        },
        Value::bool(true),
    )
}

///
/// ### Set#subset?
///
/// - subset?(set) -> bool
/// - self <= set -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Set/i/=3c=3d.html]
#[monoruby_builtin]
fn subset_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let self_inner = self_val.as_hashmap_inner();
    let other = lfp.arg(0);
    if !is_set(other, &globals.store) {
        return Err(MonorubyErr::argumenterr("value must be a set"));
    }
    let other_inner = other.as_hashmap_inner();
    if self_inner.len() > other_inner.len() {
        return Ok(Value::bool(false));
    }
    // Walked by position, as in `eq`.
    let missing = set_any(vm, globals, self_val, |vm, globals, k| {
        Ok(!other_inner.contains_key(k, vm, globals)?)
    })?;
    Ok(Value::bool(!missing))
}

///
/// ### Set#superset?
///
/// - superset?(set) -> bool
/// - self >= set -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Set/i/=3e=3d.html]
#[monoruby_builtin]
fn superset_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let self_inner = self_val.as_hashmap_inner();
    let other = lfp.arg(0);
    if !is_set(other, &globals.store) {
        return Err(MonorubyErr::argumenterr("value must be a set"));
    }
    let other_inner = other.as_hashmap_inner();
    if self_inner.len() < other_inner.len() {
        return Ok(Value::bool(false));
    }
    // Walked by position, as in `eq`.
    let missing = set_any(vm, globals, other, |vm, globals, k| {
        Ok(!self_inner.contains_key(k, vm, globals)?)
    })?;
    Ok(Value::bool(!missing))
}

///
/// ### Set#proper_subset?
///
/// - proper_subset?(set) -> bool
/// - self < set -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Set/i/proper_subset=3f.html]
#[monoruby_builtin]
fn proper_subset_(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let self_val = lfp.self_val();
    let self_inner = self_val.as_hashmap_inner();
    let other = lfp.arg(0);
    if !is_set(other, &globals.store) {
        return Err(MonorubyErr::argumenterr("value must be a set"));
    }
    let other_inner = other.as_hashmap_inner();
    if self_inner.len() >= other_inner.len() {
        return Ok(Value::bool(false));
    }
    // Walked by position, as in `eq`.
    let missing = set_any(vm, globals, self_val, |vm, globals, k| {
        Ok(!other_inner.contains_key(k, vm, globals)?)
    })?;
    Ok(Value::bool(!missing))
}

///
/// ### Set#proper_superset?
///
/// - proper_superset?(set) -> bool
/// - self > set -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Set/i/proper_superset=3f.html]
#[monoruby_builtin]
fn proper_superset_(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let self_val = lfp.self_val();
    let self_inner = self_val.as_hashmap_inner();
    let other = lfp.arg(0);
    if !is_set(other, &globals.store) {
        return Err(MonorubyErr::argumenterr("value must be a set"));
    }
    let other_inner = other.as_hashmap_inner();
    if self_inner.len() <= other_inner.len() {
        return Ok(Value::bool(false));
    }
    // Walked by position, as in `eq`.
    let missing = set_any(vm, globals, other, |vm, globals, k| {
        Ok(!self_inner.contains_key(k, vm, globals)?)
    })?;
    Ok(Value::bool(!missing))
}

///
/// ### Set#<=>
///
/// - self <=> set -> -1 | 0 | 1 | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Set/i/=3c=3d=3e.html]
#[monoruby_builtin]
fn spaceship(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let other = lfp.arg(0);
    if !is_set(other, &globals.store) {
        return Ok(Value::nil());
    }
    let self_val = lfp.self_val();
    let self_inner = self_val.as_hashmap_inner();
    let other_inner = other.as_hashmap_inner();

    // Both sets are walked by position, as in `eq`.
    let self_sub_other = !set_any(vm, globals, self_val, |vm, globals, k| {
        Ok(!other_inner.contains_key(k, vm, globals)?)
    })?;
    let other_sub_self = !set_any(vm, globals, other, |vm, globals, k| {
        Ok(!self_inner.contains_key(k, vm, globals)?)
    })?;

    if self_sub_other && other_sub_self {
        Ok(Value::integer(0))
    } else if self_sub_other {
        Ok(Value::integer(-1))
    } else if other_sub_self {
        Ok(Value::integer(1))
    } else {
        Ok(Value::nil())
    }
}

///
/// ### Set#disjoint?
///
/// - disjoint?(enum) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Set/i/disjoint=3f.html]
#[monoruby_builtin]
fn disjoint_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let other = lfp.arg(0);

    if is_set(other, &globals.store) {
        Ok(Value::bool(!sets_intersect(vm, globals, self_val, other)?))
    } else {
        vm.with_temp_scope(|vm| {
            let elems = match enum_to_vec(vm, globals, other) {
                Ok(v) => v,
                Err(_) => return Err(MonorubyErr::argumenterr("value must be enumerable")),
            };
            let self_inner = self_val.as_hashmap_inner();
            for elem in elems {
                if self_inner.contains_key(elem, vm, globals)? {
                    return Ok(Value::bool(false));
                }
            }
            Ok(Value::bool(true))
        })
    }
}

///
/// ### Set#intersect?
///
/// - intersect?(enum) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Set/i/intersect=3f.html]
#[monoruby_builtin]
fn intersect_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let other = lfp.arg(0);

    if is_set(other, &globals.store) {
        Ok(Value::bool(sets_intersect(vm, globals, self_val, other)?))
    } else {
        vm.with_temp_scope(|vm| {
            let elems = match enum_to_vec(vm, globals, other) {
                Ok(v) => v,
                Err(_) => return Err(MonorubyErr::argumenterr("value must be enumerable")),
            };
            let self_inner = self_val.as_hashmap_inner();
            for elem in elems {
                if self_inner.contains_key(elem, vm, globals)? {
                    return Ok(Value::bool(true));
                }
            }
            Ok(Value::bool(false))
        })
    }
}

/// `disjoint?` / `intersect?` with a Set: whether `set` and `other` share
/// a member. The smaller one is walked by position, as in `eq`, and each
/// of its members looked up in the other.
fn sets_intersect(
    vm: &mut Executor,
    globals: &mut Globals,
    set: Value,
    other: Value,
) -> Result<bool> {
    let (walked, probed) = if set.as_hashmap_inner().len() <= other.as_hashmap_inner().len() {
        (set, other)
    } else {
        (other, set)
    };
    let probed_inner = probed.as_hashmap_inner();
    set_any(vm, globals, walked, |vm, globals, k| {
        probed_inner.contains_key(k, vm, globals)
    })
}

///
/// ### Set#delete_if
///
/// - delete_if {|o| ... } -> self
/// - delete_if -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/Set/i/delete_if.html]
#[monoruby_builtin]
fn delete_if(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, pc: BytecodePtr) -> Result<Value> {
    let bh = match lfp.block() {
        None => {
            let id = IdentId::get_id("delete_if");
            return vm.generate_enumerator(id, lfp.self_val(), lfp.iter().collect(), pc);
        }
        Some(block) => block,
    };
    set_delete_where(vm, globals, lfp, bh, true)?;
    Ok(lfp.self_val())
}

///
/// ### Set#keep_if
///
/// - keep_if {|o| ... } -> self
/// - keep_if -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/Set/i/keep_if.html]
#[monoruby_builtin]
fn keep_if(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, pc: BytecodePtr) -> Result<Value> {
    let bh = match lfp.block() {
        None => {
            let id = IdentId::get_id("keep_if");
            return vm.generate_enumerator(id, lfp.self_val(), lfp.iter().collect(), pc);
        }
        Some(block) => block,
    };
    set_delete_where(vm, globals, lfp, bh, false)?;
    Ok(lfp.self_val())
}

///
/// ### Set#select!
///
/// - select! {|o| ... } -> self | nil
/// - filter! {|o| ... } -> self | nil
/// - select! -> Enumerator
/// - filter! -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/Set/i/select=21.html]
#[monoruby_builtin]
fn select_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, pc: BytecodePtr) -> Result<Value> {
    let bh = match lfp.block() {
        None => {
            let id = IdentId::get_id("select!");
            return vm.generate_enumerator(id, lfp.self_val(), lfp.iter().collect(), pc);
        }
        Some(block) => block,
    };
    if set_delete_where(vm, globals, lfp, bh, false)? {
        Ok(lfp.self_val())
    } else {
        Ok(Value::nil())
    }
}

///
/// ### Set#reject!
///
/// - reject! {|o| ... } -> self | nil
/// - reject! -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/Set/i/reject=21.html]
#[monoruby_builtin]
fn reject_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, pc: BytecodePtr) -> Result<Value> {
    let bh = match lfp.block() {
        None => {
            let id = IdentId::get_id("reject!");
            return vm.generate_enumerator(id, lfp.self_val(), lfp.iter().collect(), pc);
        }
        Some(block) => block,
    };
    if set_delete_where(vm, globals, lfp, bh, true)? {
        Ok(lfp.self_val())
    } else {
        Ok(Value::nil())
    }
}

///
/// ### Set#collect!
///
/// - collect! {|o| ... } -> self
/// - map! {|o| ... } -> self
/// - collect! -> Enumerator
/// - map! -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/Set/i/collect=21.html]
#[monoruby_builtin]
fn collect_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, pc: BytecodePtr) -> Result<Value> {
    let bh = match lfp.block() {
        None => {
            let id = IdentId::get_id("collect!");
            return vm.generate_enumerator(id, lfp.self_val(), lfp.iter().collect(), pc);
        }
        Some(block) => block,
    };
    lfp.self_val().ensure_not_frozen(&globals.store)?;
    let data = vm.get_block_data(globals, bh)?;
    vm.with_temp_scope(|vm| {
        // Block return values are freshly allocated; stash them in a GC-rooted
        // Array on the temp_stack so they survive both the collection loop
        // and the subsequent insert loop (which calls Ruby-level .hash/.eql?).
        vm.temp_array_new(Some(lfp.self_val().as_hash().len()));
        let new_elems_idx = vm.temp_len() - 1;
        // Walked, not snapshotted: see `set_walk`.
        set_walk(vm, globals, lfp.self_val(), |vm, globals, val| {
            let mapped = vm.invoke_block(globals, &data, &[val])?;
            vm.temp_at(new_elems_idx).as_array().push(mapped);
            Ok(())
        })?;
        let self_val = lfp.self_val();
        // CRuby rebuilds `self` from a freshly allocated Set of the same
        // class — and, since 4.0.9, the same comparison mode — so the mapped
        // elements are compared the way `self` compares and the flag stays.
        self_val.as_hash().clear()?;
        let n = vm.temp_at(new_elems_idx).as_array().len();
        for i in 0..n {
            let elem = vm.temp_at(new_elems_idx).as_array()[i];
            self_val
                .as_hash().aset(elem, Value::bool(true), vm, globals)?;
        }
        Ok(self_val)
    })
}

///
/// ### Set#flatten
///
/// - flatten -> Set
///
/// [https://docs.ruby-lang.org/ja/latest/method/Set/i/flatten.html]
#[monoruby_builtin]
fn flatten(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    // Of `self`'s class and (since 4.0.9) comparison mode.
    let mut result = new_empty_set_like(self_val, &globals.store)?;
    let mut seen = std::collections::HashSet::new();
    seen.insert(self_val.id());
    // `result` is held only here while members' `#hash` / `#eql?` run:
    // root it, or a collection there frees it.
    vm.with_temp_scope(|vm| {
        vm.temp_push(result);
        flatten_set_into(&mut result, self_val, vm, globals, &mut seen)?;
        Ok(result)
    })
}

fn flatten_set_into(
    result: &mut Value,
    set: Value,
    vm: &mut Executor,
    globals: &mut Globals,
    seen: &mut std::collections::HashSet<u64>,
) -> Result<()> {
    // Walked by position (`set_any`), as CRuby's `set_flatten_merge`
    // walks it: from a snapshot, the members a `#hash` deleted would be
    // held by the snapshot alone, and a collection would free them before
    // their turn. A nested set stays rooted while it is flattened.
    set_any(vm, globals, set, |vm, globals, val| {
        if is_set(val, &globals.store) {
            if !seen.insert(val.id()) {
                return Err(MonorubyErr::argumenterr(
                    "tried to flatten recursive Set".to_string(),
                ));
            }
            flatten_set_into(result, val, vm, globals, seen)?;
            seen.remove(&val.id());
        } else {
            result
                .as_hash().aset(val, Value::bool(true), vm, globals)?;
        }
        Ok(false)
    })?;
    Ok(())
}

///
/// ### Set#flatten!
///
/// - flatten! -> self | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Set/i/flatten=21.html]
#[monoruby_builtin]
fn flatten_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let keys = set_keys(self_val);
    let has_nested = keys.iter().any(|v| is_set(*v, &globals.store));
    if !has_nested {
        return Ok(Value::nil());
    }
    self_val.ensure_not_frozen(&globals.store)?;
    let mut result = new_empty_set_like(self_val, &globals.store)?;
    let mut seen = std::collections::HashSet::new();
    seen.insert(self_val.id());
    // `result` is held only here while members' `#hash` / `#eql?` run,
    // both while it is built and while `self` is refilled from it: root
    // it, or a collection there frees it.
    vm.with_temp_scope(|vm| {
        vm.temp_push(result);
        flatten_set_into(&mut result, self_val, vm, globals, &mut seen)?;
        // `replace(self, flatten(self))`: the flattened set compares like
        // `self` (since 4.0.9), so the flag survives the swap.
        let self_val = lfp.self_val();
        self_val.as_hash().clear()?;
        let new_keys = set_keys(result);
        for k in new_keys {
            self_val.as_hash().aset(k, Value::bool(true), vm, globals)?;
        }
        Ok(self_val)
    })
}

///
/// ### Set#join
///
/// - join(sep = "") -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/Set/i/join.html]
#[monoruby_builtin]
fn join(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let sep = if let Some(s) = lfp.try_arg(0) {
        if s.is_nil() {
            String::new()
        } else {
            s.coerce_to_string(vm, globals)?
        }
    } else {
        String::new()
    };
    // The members are the ones the set held when `join` began, as CRuby
    // joins `to_a`, and they are rooted with it: one that a `#to_s`
    // deletes is held by nothing else, and a collection frees it before
    // its turn.
    vm.with_temp_scope(|vm| {
        let keys = set_keys_rooted(vm, lfp.self_val());
        let mut result = String::new();
        for (i, k) in keys.iter().enumerate() {
            if i > 0 {
                result.push_str(&sep);
            }
            let s_val = vm.invoke_method_inner(globals, IdentId::TO_S, *k, &[], None, None)?;
            if let Some(s) = s_val.is_str() {
                result.push_str(s);
            } else {
                result.push_str(&s_val.to_s(&globals.store));
            }
        }
        Ok(Value::string(result))
    })
}

///
/// ### Set#classify
///
/// - classify {|o| ... } -> Hash
/// - classify -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/Set/i/classify.html]
#[monoruby_builtin]
fn classify(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, pc: BytecodePtr) -> Result<Value> {
    let bh = match lfp.block() {
        None => {
            let id = IdentId::get_id("classify");
            return vm.generate_enumerator(id, lfp.self_val(), vec![], pc);
        }
        Some(block) => block,
    };
    let data = vm.get_block_data(globals, bh)?;
    // Result is a Hash mapping classification → Set
    let result_hash = Value::hash_from_inner(Default::default());
    // `result_hash`, the per-iteration block result, and any freshly-built
    // subset must stay reachable across the GC that `invoke_block` /
    // `new_empty_set` / `insert` can trigger (e.g. under gc-stress). They live
    // only in Rust locals, so without rooting them on the temp stack the
    // unrooted `result_hash` becomes a use-after-free — it manifested as a
    // `RValue::ty()` unwrap-on-`None` panic (a swept, dead RVALUE).
    vm.with_temp_scope(|vm| {
        vm.temp_push(result_hash);
        // Walked, not snapshotted: see `set_walk`. The block may delete the
        // member it is given, so the member is rooted with its
        // classification until it is added to a subset below.
        set_walk(vm, globals, lfp.self_val(), |vm, globals, k| {
            let classification = vm.invoke_block(globals, &data, &[k])?;
            let depth = vm.temp_len();
            vm.temp_push(classification);
            vm.temp_push(k);
            let existing = result_hash.as_hashmap_inner().get(classification, vm, globals)?;
            if let Some(set) = existing {
                set.as_hash().aset(k, Value::bool(true), vm, globals)?;
            } else {
                // Each subset is of `self`'s class and (since 4.0.9)
                // comparison mode.
                let new_set = new_empty_set_like(lfp.self_val(), &globals.store)?;
                vm.temp_push(new_set);
                new_set.as_hash().aset(k, Value::bool(true), vm, globals)?;
                result_hash.as_hash().aset(classification, new_set, vm, globals)?;
            }
            vm.temp_clear(depth);
            Ok(())
        })?;
        Ok(result_hash)
    })
}

///
/// ### Set#divide
///
/// - divide {|o| ... } -> Set
/// - divide {|o1, o2| ... } -> Set
/// - divide -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/Set/i/divide.html]
#[monoruby_builtin]
fn divide(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, pc: BytecodePtr) -> Result<Value> {
    let bh = match lfp.block() {
        None => {
            let id = IdentId::get_id("divide");
            return vm.generate_enumerator(id, lfp.self_val(), vec![], pc);
        }
        Some(block) => block,
    };
    let data = vm.get_block_data(globals, bh)?;

    // Check block arity to determine mode.
    //
    // When `divide` is driven through its own no-block Enumerator
    // (`set.divide` then `.each { |a, b| ... }`), the block it receives
    // is the internal enumerator yielder proc, not the user's block, so
    // its declared arity is meaningless here. `Enumerator#each` records
    // the real user-block arity on `Globals`; use that instead.
    let arity = if data.func_id() == Some(crate::globals::YIELDER_FUNCID) {
        globals.current_enum_block_arity().unwrap_or(1)
    } else if let Some(fid) = data.func_id() {
        globals[fid].arity()
    } else {
        1
    };
    if arity == 2 {
        // Arity-2 mode: group elements by strongly-connected components
        // of the directed graph with edge i->j iff block(keys[i], keys[j])
        // returns truthy. Block is called for every ordered pair (i, j)
        // where i != j.
        // The pairs are taken from a snapshot of the members, which is
        // rooted: the block may delete members from the set, leaving
        // `keys` their only holder.
        vm.with_temp_scope(|vm| {
            let keys = set_keys_rooted(vm, lfp.self_val());
            let n = keys.len();
            let mut adj: Vec<Vec<usize>> = vec![Vec::new(); n];
            for i in 0..n {
                for j in 0..n {
                    if i == j {
                        continue;
                    }
                    let result = vm.invoke_block(globals, &data, &[keys[i], keys[j]])?;
                    if result.as_bool() {
                        adj[i].push(j);
                    }
                }
            }
            // Tarjan's SCC
            let mut index_counter: usize = 0;
            let mut stack: Vec<usize> = Vec::new();
            let mut on_stack: Vec<bool> = vec![false; n];
            let mut idx: Vec<Option<usize>> = vec![None; n];
            let mut low: Vec<usize> = vec![0; n];
            let mut sccs: Vec<Vec<usize>> = Vec::new();
            // Iterative Tarjan to avoid stack overflow on large sets.
            enum Step {
                Enter(usize),
                Resume(usize, usize),
            }
            for start in 0..n {
                if idx[start].is_some() {
                    continue;
                }
                let mut work: Vec<Step> = vec![Step::Enter(start)];
                while let Some(step) = work.pop() {
                    match step {
                        Step::Enter(v) => {
                            idx[v] = Some(index_counter);
                            low[v] = index_counter;
                            index_counter += 1;
                            stack.push(v);
                            on_stack[v] = true;
                            work.push(Step::Resume(v, 0));
                        }
                        Step::Resume(v, i) => {
                            if i < adj[v].len() {
                                let w = adj[v][i];
                                work.push(Step::Resume(v, i + 1));
                                if idx[w].is_none() {
                                    work.push(Step::Enter(w));
                                } else if on_stack[w] {
                                    if let Some(wi) = idx[w] {
                                        if wi < low[v] {
                                            low[v] = wi;
                                        }
                                    }
                                }
                            } else {
                                // Propagate low from children that have returned.
                                for &w in &adj[v] {
                                    if on_stack[w] && low[w] < low[v] {
                                        low[v] = low[w];
                                    }
                                }
                                if Some(low[v]) == idx[v] {
                                    let mut comp = Vec::new();
                                    while let Some(w) = stack.pop() {
                                        on_stack[w] = false;
                                        comp.push(w);
                                        if w == v {
                                            break;
                                        }
                                    }
                                    sccs.push(comp);
                                }
                            }
                        }
                    }
                }
            }
            let result_set = new_empty_set();
            // Root `result_set` and each freshly-built `subset` across the GC that
            // `set_from_iter` / `insert` can trigger (see `classify`).
            vm.with_temp_scope(|vm| {
                vm.temp_push(result_set);
                for comp in sccs {
                    let elems: Vec<Value> = comp.into_iter().map(|i| keys[i]).collect();
                    let subset = set_from_iter_like(lfp.self_val(), elems.into_iter(), vm, globals)?;
                    let depth = vm.temp_len();
                    vm.temp_push(subset);
                    result_set.as_hash().aset(subset, Value::bool(true), vm, globals)?;
                    vm.temp_clear(depth);
                }
                Ok(result_set)
            })
        })
    } else {
        // Arity-1 mode: classify and return set of sets
        let mut groups: std::collections::HashMap<u64, Vec<Value>> = std::collections::HashMap::new();
        let mut group_order: Vec<u64> = Vec::new();
        // The whole grouping runs inside one temp scope: each
        // classification value is kept rooted so its identity (`id()`,
        // the group key) can't be recycled by a GC for a later,
        // different classification — under per-safepoint stress a freed
        // result's address is reused almost immediately.
        vm.with_temp_scope(|vm| {
            // Walked, not snapshotted (see `set_walk`); a member is rooted
            // as it is grouped, since the block may delete it from the set
            // later, leaving `groups` its only holder.
            set_walk(vm, globals, lfp.self_val(), |vm, globals, k| {
                let classification = vm.invoke_block(globals, &data, &[k])?;
                vm.temp_push(classification);
                vm.temp_push(k);
                let key = classification.id() as u64;
                if !groups.contains_key(&key) {
                    group_order.push(key);
                }
                groups.entry(key).or_default().push(k);
                Ok(())
            })?;
            let result_set = new_empty_set();
            // Root `result_set` and each freshly-built `subset` across the GC
            // that `set_from_iter` / `insert` can trigger (see `classify`).
            vm.temp_push(result_set);
            for key in group_order {
                if let Some(elems) = groups.remove(&key) {
                    let subset = set_from_iter_like(lfp.self_val(), elems.into_iter(), vm, globals)?;
                    let depth = vm.temp_len();
                    vm.temp_push(subset);
                    result_set.as_hash().aset(subset, Value::bool(true), vm, globals)?;
                    vm.temp_clear(depth);
                }
            }
            Ok(result_set)
        })
    }
}

///
/// ### Set#reset
///
/// - reset -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Set/i/reset.html]
#[monoruby_builtin]
fn reset(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    // In CRuby, reset rebuilds the internal hash. For us, it's a no-op since
    // our hash is always consistent.
    Ok(lfp.self_val())
}

///
/// ### Set#hash
///
/// - hash -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/Set/i/hash.html]
#[monoruby_builtin]
fn set_hash(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    use crate::RubyHash;
    use std::hash::Hasher;
    // Hash every element through `Value::ruby_hash`, which is content-based
    // (e.g. strings hash by bytes, not by object identity). Combine with
    // XOR so the result is order-independent — two Sets with the same
    // members always hash equal regardless of insertion order.
    //
    // The per-process seeded hasher keeps element digests consistent
    // within a process while randomizing them across processes
    // (CVE-2011-4815).
    // Constant mixed in to distinguish an empty Set from other empty
    // aggregates that XOR-combine to 0.
    let mut combined: u64 = 0x5e7_5e7_5e7_5e7;
    // Walked by position (`set_any`), as CRuby's `set_i_hash` walks it:
    // from a snapshot, the members a `#hash` deleted would be held by the
    // snapshot alone, and a collection would free them before their turn.
    set_any(vm, globals, lfp.self_val(), |vm, globals, k| {
        let mut hasher = crate::value::seeded_hasher();
        k.ruby_hash(&mut hasher, vm, globals)?;
        combined ^= hasher.finish();
        Ok(false)
    })?;
    Ok(Value::from_hash_digest(combined))
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn set_basic() {
        run_tests(&[
            "Set.new.class.to_s",
            "Set[1, 2, 3].size",
            "Set[1, 2, 3].include?(2)",
            "Set[1, 2, 3].include?(4)",
            "Set.new.empty?",
            "Set[1].empty?",
            "s = Set.new; s << 1; s << 2; s << 1; s.size",
            "s = Set[1, 2]; s.add?(3).equal?(s)",
            "s = Set[1, 2]; s.add?(1).nil?",
            "s = Set[1, 2, 3]; s.delete(2); s.include?(2)",
            "s = Set[1, 2]; s.delete?(2).equal?(s)",
            "s = Set[1, 2]; s.delete?(3).nil?",
            "(Set[1, 2, 3] & Set[2, 3, 4]).to_a.sort",
            "(Set[1, 2, 3] | Set[2, 3, 4]).to_a.sort",
            "(Set[1, 2, 3] - Set[2, 3, 4]).to_a.sort",
            "(Set[1, 2, 3] ^ Set[2, 3, 4]).to_a.sort",
            "Set[1, 2] == Set[2, 1]",
            "Set[1, 2] == Set[1, 2, 3]",
            "Set[1, 2].subset?(Set[1, 2, 3])",
            "Set[1, 2, 3].superset?(Set[1, 2])",
            "Set[1, 2].proper_subset?(Set[1, 2, 3])",
            "Set[1, 2].proper_subset?(Set[1, 2])",
            "Set[1, 2].disjoint?(Set[3, 4])",
            "Set[1, 2].disjoint?(Set[2, 3])",
            "Set[1, 2].intersect?(Set[2, 3])",
            "Set[1, 2].intersect?(Set[3, 4])",
            "a = []; Set[3, 1, 2].each {|x| a << x}; a.sort",
            "Set[3, 1, 2].to_a.sort",
            "Set[].to_s",
        ]);
    }

    #[test]
    fn set_inspect_recursive() {
        run_test_no_result_check(
            r#"
            s1 = Set[]
            s2 = Set[s1]
            s1 << s2
            s1.inspect.include?("Set[...]")
            "#,
        );
    }

    #[test]
    fn set_merge() {
        run_tests(&[
            "s = Set[1]; s.merge([2, 3]); s.to_a.sort",
            "Set.new([1, 2, 3]).to_a.sort",
            "Set.new([1, 2, 3]) {|x| x * 2}.to_a.sort",
            "Set[1, 2] <=> Set[1, 2]",
            "Set[1, 2] <=> Set[1, 2, 3]",
            "Set[1, 2, 3] <=> Set[1, 2]",
            "(Set[1, 2] <=> Set[3, 4]).nil?",
            "s = Set[1, 2]; t = s.dup; t << 3; [s.size, t.size]",
            "s = Set[1, 2, 3]; s.clear; s.size",
            "s = Set[1, 2, 3]; s.clear; s.empty?",
            "s = Set[1, 2, 3]; s.clear.equal?(s)",
            "s = Set[1, 2]; s.to_set.equal?(s)",
            "Set[1, 2, 3].to_set.to_a.sort",
            "s = Set[1, 2, 3, 4]; s.subtract([2, 4]); s.to_a.sort",
            "s = Set[1, 2, 3]; s.subtract(Set[3, 4]); s.to_a.sort",
            "s = Set[1, 2]; s.subtract([]).to_a.sort",
            "s = Set[1, 2]; s.subtract([1, 2]); s.size",
            "s = Set[1, 2, 3]; s.replace([4, 5]); s.to_a.sort",
            "s = Set[1, 2]; s.replace(Set[3, 4, 5]); s.to_a.sort",
            "s = Set[1, 2]; s.replace([]).size",
            "s = Set[1, 2]; s.replace([1, 2]).equal?(s)",
            "Set[1, 2, 3].proper_superset?(Set[1, 2])",
            "Set[1, 2].proper_superset?(Set[1, 2])",
            "Set[1, 2].proper_superset?(Set[1, 2, 3])",
            "Set[1, 2, 3] > Set[1, 2]",
            "Set[1, 2] > Set[1, 2]",
            "s = Set[1, 2, 3, 4, 5]; s.delete_if {|x| x % 2 == 0}; s.to_a.sort",
            "s = Set[1, 2, 3]; s.delete_if {|x| false}; s.to_a.sort",
            "s = Set[1, 2, 3]; s.delete_if {|x| true}; s.size",
            "s = Set[1, 2, 3]; s.delete_if {|x| x > 1}.equal?(s)",
            "s = Set[1, 2, 3, 4, 5]; s.keep_if {|x| x.odd?}; s.to_a.sort",
            "s = Set[1, 2, 3]; s.keep_if {|x| true}; s.to_a.sort",
            "s = Set[1, 2, 3]; s.keep_if {|x| false}; s.size",
            "s = Set[1, 2, 3]; s.keep_if {|x| x < 3}.equal?(s)",
            "s = Set[1, 2, 3, 4]; s.select! {|x| x.even?}; s.to_a.sort",
            "s = Set[1, 2, 3, 4]; s.reject! {|x| x.even?}; s.to_a.sort",
            "s = Set[2, 4]; s.select! {|x| x.even?}.nil?",
            "s = Set[1, 2, 3]; s.collect! {|x| x * 2}; s.to_a.sort",
            "Set[Set[1, 2], Set[3, 4], 5].flatten.to_a.sort",
            "s = Set[Set[1, 2], Set[3, 4], 5]; s.flatten!; s.to_a.sort",
            "s = Set[Set[1, 2], Set[3, 4], 5]; s.flatten!.equal?(s)",
            "s = Set[1, 2, 3]; s.flatten!.nil?",
            "Set[Set[Set[1]], 2].flatten.to_a.sort",
        ]);
    }

    #[test]
    fn set_flatten_recursive() {
        run_test_error("s = Set.new; s << s; s.flatten");
    }

    #[test]
    fn set_flatten_bang_recursive() {
        run_test_error("s = Set.new; s << s; s.flatten!");
    }

    #[test]
    fn set_case_equality() {
        run_tests(&[
            "Set[1, 2, 3] === 2",
            "Set[1, 2, 3] === 4",
            "Set[1, 2, 3].join(', ')",
            "Set[1,2,3,4,5].classify {|x| x % 2}.map {|k,v| [k, v.to_a.sort]}.sort",
            "Set[1,2,3,4,5].divide {|x| x % 2}.map {|s| s.to_a.sort}.sort",
            "s = Set[1, 2, 3]; s.reset.to_a.sort",
            // Two sets with same elements
            "Set[1, 2] == Set[1, 2]",
            "Set[1, 2] == Set[2, 1]",
            "Set[1, 2] == Set[1, 3]",
        ]);
    }

    #[test]
    fn set_frozen_mutations_raise() {
        for op in [
            "s << 9",
            "s.add(9)",
            "s.add?(9)",
            "s.add?(1)",
            "s.delete(9)",
            "s.delete(1)",
            "s.delete?(9)",
            "s.delete?(1)",
            "s.clear",
            "s.merge([9])",
            "s.merge([])",
            "s.subtract([9])",
            "s.subtract([])",
            "s.replace([9])",
            "s.replace([])",
            "s.delete_if {|x| false}",
            "s.keep_if {|x| true}",
            "s.select! {|x| true}",
            "s.reject! {|x| false}",
            "s.collect! {|x| x}",
        ] {
            run_test_error(&format!("s = Set[1, 2, 3].freeze; {op}"));
        }
    }

    #[test]
    fn set_frozen_flatten_bang() {
        // Flat frozen set: flatten! returns nil without raising.
        run_test("Set[1, 2, 3].freeze.flatten!.inspect");
        // Nested frozen set: flatten! raises FrozenError.
        run_test_error("Set[Set[1, 2]].freeze.flatten!");
    }

    #[test]
    fn set_frozen_block_no_block_returns_enumerator() {
        run_tests(&[
            // Without a block, these return an enumerator even on frozen sets.
            "Set[1, 2, 3].freeze.delete_if.class.to_s",
            "Set[1, 2, 3].freeze.keep_if.class.to_s",
            "Set[1, 2, 3].freeze.select!.class.to_s",
            "Set[1, 2, 3].freeze.reject!.class.to_s",
            "Set[1, 2, 3].freeze.collect!.class.to_s",
            // Symmetric relation: elements within abs-difference of 1 group together.
            "Set[1, 3, 4, 6, 9, 10, 11].divide {|x, y| (x - y).abs == 1 }\
             .map {|s| s.to_a.sort }.sort",
            // SCC with a symmetric relation: (a+b).even? on {1,2,3,4} groups
            // odd {1,3} and even {2,4}.
            "Set[1, 2, 3, 4].divide {|a, b| (a + b).even? }\
             .map {|s| s.to_a.sort }.sort",
            "Set.private_instance_methods(false).include?(:initialize)",
        ]);
    }

    #[test]
    fn set_initialize_uses_each_entry_when_available() {
        // If the argument responds to #each_entry, Set.new uses it
        // (rather than #each or #to_a).
        run_test_with_prelude(
            "$trace = []; s = Set.new(E.new([10, 20, 30])); [$trace.sort, s.to_a.sort]",
            r#"
            class E
              def initialize(a); @a = a; end
              def each_entry; @a.each { |x| $trace << x; yield x }; end
              def each; raise "should not be called"; end
            end
            "#,
        );
    }

    #[test]
    fn set_initialize_falls_back_to_each() {
        run_test_with_prelude(
            "s = Set.new(E.new([1, 2, 2, 3])); s.to_a.sort",
            r#"
            class E
              def initialize(a); @a = a; end
              def each; @a.each { |x| yield x }; end
            end
            "#,
        );
    }

    #[test]
    fn set_initialize_non_enumerable_raises() {
        run_test_error("Set.new(42)");
    }

    #[test]
    fn set_inspect_to_s_alias_equal_method() {
        run_tests(&[
            // Both names resolve to the same underlying Method.
            "Set.instance_method(:to_s) == Set.instance_method(:inspect)",
            "Set[].method(:to_s) == Set[].method(:inspect)",
            // Set#=== is an alias for #include? / #member?.
            "Set.instance_method(:===) == Set.instance_method(:include?)",
            "Set.instance_method(:===) == Set.instance_method(:member?)",
        ]);
    }

    #[test]
    fn set_iteration_mutation_raises() {
        // Adding a new element during iteration should raise RuntimeError.
        run_test_error(
            "s = Set[:a, :b, :c]; s.each { |x| s << :new if x == :a }",
        );
        // merge during iteration raises.
        run_test_error(
            "s = Set[1]; s.each { s.merge([99]) }",
        );
        // replace during iteration raises (clear inside).
        run_test_error(
            "s = Set[1, 2]; s.each { s.replace([9]) }",
        );
    }

    #[test]
    fn set_misc() {
        run_tests(&[
            // Removing an element during iteration is permitted (CRuby-compatible).
            "s = Set[1, 2, 3]; s.each { |x| s.delete(x) if x == 2 }; s.to_a.sort",
            // Same content → same hash, regardless of insertion order.
            "Set[].hash == Set[].hash",
            "Set[1, 2, 3].hash == Set[1, 2, 3].hash",
            "Set[:a, \"b\", \"c\"].hash == Set[\"c\", \"b\", :a].hash",
            "Set[].hash != Set[1, 2, 3].hash",
            // select!/reject! without a block return an Enumerator whose #each
            // drives the backing method; the user block's truthiness must reach
            // the filter logic.
            "s = Set[\"one\", \"two\", \"three\"]; \
             s.select!.each { |x| x.size != 3 }; s.to_a",
            "s = Set[\"one\", \"two\", \"three\"]; \
             s.reject!.each { |x| x.size == 3 }; s.to_a",
            r#"Set.new.compare_by_identity?"#,
            r#"(s=Set.new; s.compare_by_identity.equal?(s))"#,
            r#"(s=Set.new; s.compare_by_identity; s.compare_by_identity?)"#,
            // distinct equal-value String objects are kept apart.
            r#"(a="x"; b="x"; s=Set.new; s.compare_by_identity; s.merge([a,a,b]); s.size)"#,
            // member? becomes identity-based.
            r#"(e=[1]; s=Set.new; s<<e; before=s.member?(e.dup); s.compare_by_identity; [before, s.member?(e.dup), s.member?(e)])"#,
            // a compare_by_identity set is never == a normal one.
            r#"(Set[1,2].compare_by_identity == Set[1,2])"#,
            r#"(a=Set[1,2]; a.compare_by_identity; b=Set[1,2]; b.compare_by_identity; a==b)"#,
            // persists across dup / clone.
            r#"(s=Set.new; s.compare_by_identity; [s.dup.compare_by_identity?, s.clone.compare_by_identity?])"#,
            r#"(s=Set.new; s.compare_by_identity?)"#,
        ]);
    }

    /// Which Set operations carry the `compare_by_identity` flag over to
    /// their result. Since CRuby 4.0.9 (the pin) every set derived from
    /// `self` compares the way `self` does: `|` / `-` / `^` duplicate
    /// `self`, and `&`, `flatten`, `map!`, the subsets of `classify` /
    /// `divide` allocate a fresh Set of `self`'s class and switch it to
    /// identity comparison first (4.0.6 built those four plain, so they
    /// dropped the flag). The in-place `map!` / `flatten!` / `replace`
    /// swap in a freshly built hash and take *its* flag — which is how
    /// `replace` can turn identity comparison off.
    #[test]
    fn compare_by_identity_propagation() {
        run_tests(&[
            // Binary operators: all four retain.
            r#"(s=Set[1,2,3,4].compare_by_identity; [(s|Set[3,4,5]).compare_by_identity?,
                (s-Set[3,4]).compare_by_identity?, (s^Set[3,4,5]).compare_by_identity?,
                (s&Set[3,4]).compare_by_identity?])"#,
            // ... and their element comparison follows `self`, so two equal
            // but distinct Strings stay distinct.
            r#"(a="x".dup; b="x".dup; s=Set.new.compare_by_identity; s<<a;
                [(s-[b]).size, (s-[a]).size, (s|[b]).size, (s^[b]).size, (s&[b]).size])"#,
            // A plain Set keeps comparing by value through the same ops.
            r#"(a="x".dup; b="x".dup; s=Set.new; s<<a;
                [(s-[b]).size, (s|[b]).size, (s^[b]).size, (s&[b]).size])"#,
            // flatten keeps it (so flatten!, `replace(self, flatten)`, does
            // too, flattened or not).
            r#"Set[1,2,Set[3,4]].compare_by_identity.flatten.compare_by_identity?"#,
            r#"(s=Set[1,2,Set[3,4]].compare_by_identity; s.flatten!; s.compare_by_identity?)"#,
            r#"(s=Set[1,2].compare_by_identity; s.flatten!; s.compare_by_identity?)"#,
            // map! / merge / subtract keep it.
            r#"(s=Set[1,2,3].compare_by_identity; s.map!{|x| x*2}; [s.compare_by_identity?, s.to_a.sort])"#,
            r#"(s=Set[1,2].compare_by_identity; s.merge([3]); s.compare_by_identity?)"#,
            r#"(s=Set[1,2].compare_by_identity; s.subtract([2]); s.compare_by_identity?)"#,
            // replace adopts the argument's flag when it is a Set, in both
            // directions, and leaves it alone for any other Enumerable.
            r#"(s=Set[:a].compare_by_identity; s.replace(Set[1,2]); s.compare_by_identity?)"#,
            r#"(s=Set[:a]; s.replace(Set[1,2].compare_by_identity); s.compare_by_identity?)"#,
            r#"(s=Set[:a].compare_by_identity; s.replace([1,2]); [s.compare_by_identity?, s.to_a])"#,
            // classify / divide: the subsets compare like `self`, the
            // container of divide is a plain Set.
            r#"(s=Set[1,2,3,4].compare_by_identity; s.classify(&:odd?).values.map(&:compare_by_identity?))"#,
            r#"(s=Set[1,2,3,4].compare_by_identity; d=s.divide(&:odd?); [d.compare_by_identity?, d.map(&:compare_by_identity?)])"#,
            r#"(s=Set[1,2,3,4].compare_by_identity; d=s.divide{|a,b| (a-b).abs==1}; [d.compare_by_identity?, d.map(&:compare_by_identity?)])"#,
            // ... and are of `self`'s class, as every derived set is.
            r#"(class MySet < Set; end; s=MySet[1,2,3]; [(s&[1]).class, (s|[9]).class, (s-[1]).class, (s^[9]).class, s.flatten.class, s.classify(&:odd?).values.map(&:class), s.divide(&:odd?).class, s.divide(&:odd?).map(&:class), s.dup.map!{|x| x}.class])"#,
        ]);
    }

    #[test]
    fn divide_enumerator_arity() {
        run_tests(&[
            // Arity-2 via the no-block Enumerator: graph/SCC grouping.
            r#"Set[1,2,3,4].divide.each { |a, b| (a + b).even? } == Set[Set[1, 3], Set[2, 4]]"#,
            // Arity-1 via the no-block Enumerator: classify grouping.
            r#"Set[1,2,3,4].divide.each { |x| x % 2 }.map { |s| s.to_a.sort }.sort"#,
            // No block -> Enumerator.
            r#"Set[1,2,3,4].divide.is_a?(Enumerator)"#,
            // Direct (non-enumerator) arity-2 / arity-1 still correct.
            r#"Set[1,3,4,6,9,10,11].divide { |x, y| (x - y).abs == 1 }.map { |s| s.to_a.sort }.sort"#,
            r#"Set[1,2,3,4].divide { |x| x % 2 }.map { |s| s.to_a.sort }.sort"#,
        ]);
    }

    #[test]
    fn object_keys_use_ruby_hash_eql() {
        // Set#hash is order-independent and a Set works as a
        // content-based Hash/Set key (object keys dispatch to Ruby
        // #hash / #eql?).
        run_tests(&[
            r#"Set[1,3].hash == Set[3,1].hash"#,
            r#"Set[Set[3,1],Set[4,2]] == Set[Set[1,3],Set[2,4]]"#,
            r#"{Set[1,3] => :x}[Set[3,1]]"#,
            r#"Set[Set[1,2]].include?(Set[2,1])"#,
            r#"{[1,2] => :a}[[1,2]]"#,
            r#"({}[Object.new]).nil?"#,
        ]);
        // A user object's #hash / #eql? are consulted on Hash lookup.
        run_test(
            r#"
            class K
              def initialize(n); @n = n; end
              def hash; @n.hash; end
              def eql?(o); o.is_a?(K) && o.instance_variable_get(:@n) == @n; end
            end
            h = {}
            h[K.new(1)] = :a
            h[K.new(2)] = :b
            [h[K.new(1)], h[K.new(2)], h[K.new(3)]]
            "#,
        );
    }

    /// `Set.[]`, `|`, `&`, `-`, `^`, `flatten`, `flatten!` and `divide`
    /// keep the Sets they build rooted while the members' `#hash` runs,
    /// here with a collection in it.
    #[test]
    fn derived_sets_survive_gc_in_member_hash() {
        run_test_once(
            r##"
            class GcKey
              def initialize(n) = @n = n
              attr_reader :n
              def hash = (GC.start if $gc_in_hash; @n.hash)
              def eql?(o) = o.is_a?(GcKey) && o.n == @n
              def inspect = "k#{@n}"
            end
            def ks(*a) = a.map { GcKey.new(_1) }
            def mk(*a)
              $gc_in_hash = false
              s = Set.new(a)
              $gc_in_hash = true
              s
            end
            $gc_in_hash = true
            r = nil
            20.times do
              r = [
                Set[*ks(1, 2, 3)],
                Set[*ks(1, 2)] | ks(3, 4),
                mk(*ks(1, 2)) & ks(1, 2),
                mk(*ks(1, 2, 3)) - ks(2),
                mk(*ks(1, 2)) ^ ks(2, 3),
                mk(mk(*ks(1, 2)), *ks(3)).flatten,
                mk(mk(*ks(1, 2)), *ks(3)).flatten!,
                mk(*ks(1, 2, 3, 4)).divide { |x| x.n % 2 }.map { _1.to_a.map(&:n).sort }.sort,
              ].map { |x| x.is_a?(Set) ? x.to_a.map(&:n).sort : x }
            end
            $gc_in_hash = false
            r
            "##,
        );
    }

    /// `merge`, `subtract`, `replace`, `disjoint?`, `intersect?` and `|`
    /// given an Enumerable that is neither an Array nor a Set keep the
    /// members its `#to_a` answered rooted while the members' `#hash`
    /// runs, here with a collection in it.
    #[test]
    fn enum_arguments_survive_gc_in_member_hash() {
        run_test_once(
            r##"
            class GcKey
              def initialize(n) = @n = n
              attr_reader :n
              def hash = (GC.start if $gc_in_hash; @n.hash)
              def eql?(o) = o.is_a?(GcKey) && o.n == @n
            end
            class GcEnum
              include Enumerable
              def each = (1..4).each { |i| yield GcKey.new(i) }
            end
            def mk
              $gc_in_hash = false
              s = Set.new([GcKey.new(0), GcKey.new(2)])
              $gc_in_hash = true
              s
            end
            r = nil
            20.times do
              r = [
                mk.merge(GcEnum.new).size,
                mk.subtract(GcEnum.new).size,
                mk.replace(GcEnum.new).size,
                mk.disjoint?(GcEnum.new),
                mk.intersect?(GcEnum.new),
                (mk | GcEnum.new).size,
              ]
            end
            $gc_in_hash = false
            r
            "##,
        );
    }

    /// `each`, `delete_if`, `keep_if`, `select!`, `reject!`, `map!`,
    /// `classify` and `divide` walk the live table, as CRuby does: a
    /// member the block deletes is not yielded, and a collection inside
    /// the block frees nothing they still hold.
    #[test]
    fn walks_skip_members_deleted_by_the_block() {
        run_test_once(
            r##"
            r = nil
            30.times do
              r = %i[each delete_if select! keep_if reject! map! classify divide].map do |m|
                s = Set.new((1..6).map { |i| "n#{i}" * 3 })
                seen = []
                blk = proc do |x|
                  seen << x
                  s.delete(s.to_a.last) if s.size > 1
                  GC.start
                end
                case m
                when :each then s.each(&blk)
                when :delete_if, :reject! then s.send(m) { |x| blk.(x); false }
                when :select!, :keep_if then s.send(m) { |x| blk.(x); true }
                when :map! then s.map! { |x| blk.(x); x }
                when :classify, :divide then s.send(m) { |x| blk.(x); 1 }
                end
                [m, seen]
              end
            end
            r
            "##,
        );
    }

    /// The members are deleted as they are reached, as CRuby deletes them,
    /// so the block sees the earlier deletions; `select!` / `reject!`
    /// answer the set when the block itself removed members; adding a
    /// member during the walk raises; and `clear` inside the walk ends it.
    #[test]
    fn deleting_walks_follow_cruby() {
        run_test_once(
            r##"
            def t
              yield
            rescue => e
              e.class
            end
            r = []
            s = Set[1, 2, 3, 4]
            r << t { seen = []; s.delete_if { |x| seen << [x, s.include?(1)]; x == 1 }; [seen, s.to_a] }
            s = Set[1, 2, 3, 4]
            r << t { seen = []; v = s.reject! { |x| seen << x; s.delete(4); false }; [seen, s.to_a, v.equal?(s)] }
            s = Set[1, 2, 3, 4]
            r << t { seen = []; v = s.select! { |x| seen << x; s.delete(4); true }; [seen, s.to_a, v.equal?(s)] }
            s = Set[1, 2, 3, 4]
            r << t { s.delete_if { |x| s << 9; false } }
            s = Set[1, 2, 3, 4]
            r << t { seen = []; s.delete_if { |x| seen << x; s.clear; false }; [seen, s.to_a] }
            s = Set[1, 2, 3, 4]
            r << t { seen = []; s.each { |x| seen << x; s.clear }; [seen, s.to_a] }
            s = Set[1, 2, 3, 4]
            r << t { s.keep_if { |x| x.even? }.to_a } << s.to_a
            r
            "##,
        );
    }

    /// `clear` inside a walk deletes the members where they stand, as
    /// CRuby's does, without running their `#hash`: deleting them by key
    /// ran it, and one that deleted the rest (here, with a collection)
    /// left them to a snapshot nothing rooted. Inline sets, identity ones
    /// among them, are cleared the same way.
    #[test]
    fn clear_inside_a_walk_hashes_nothing() {
        run_test_once(
            r##"
            def drop_last = ($s.delete($s.to_a.last); nil)
            class ClearKey
              def initialize(n) = @n = n
              attr_reader :n
              def hash
                if $in_clear
                  $in_clear = false
                  $hash_calls += 1
                  drop_last
                  GC.start
                end
                @n.hash
              end
              def eql?(o) = o.is_a?(ClearKey) && o.n == @n
            end
            def fill = ($s = Set.new((1..6).map { ClearKey.new("k#{_1}" * 3) }); nil)
            r = nil
            10.times do
              $hash_calls = 0
              fill
              seen = []
              $s.each { |x| seen << x.n; $in_clear = true; $s.clear; $in_clear = false }
              r = [seen, $s.size, $s.to_a, $hash_calls]
            end
            s = Set[1, 2]
            seen = []
            s.each { |x| seen << x; s.clear }
            r << [seen, s.size, (s << 3).to_a]
            s = Set["a".dup, "b".dup].compare_by_identity
            seen = []
            s.each { |x| seen << x; s.clear }
            r << [seen, s.size, s.compare_by_identity?, (s << "c").to_a]
            "##,
        );
    }

    /// `flatten`, `flatten!`, `disjoint?` / `intersect?` with a Set,
    /// `join`, `hash`, `==`, `subset?`, `superset?`, `<`, `>` and `<=>`
    /// keep every member they have yet to reach alive while a member's
    /// `#hash` / `#to_s` deletes another one (from a method of its own, so
    /// no frame holds it) and collects: they walk the live table, which
    /// skips the deleted member as CRuby does, or, for `join`, a rooted
    /// snapshot, as CRuby joins `to_a`.
    #[test]
    fn walks_root_members_deleted_by_member_hash() {
        run_test_once(
            r##"
            def kill_last = ($s.delete($s.to_a.last); nil)
            class WalkKey
              def initialize(v) = @v = v
              attr_reader :v
              def hash = ($evil && ($evil = false; kill_last; GC.start); @v.hash)
              def to_s = ($evil && ($evil = false; kill_last; GC.start); @v)
              def eql?(o) = o.is_a?(WalkKey) && o.v == @v
            end
            def keys(tag, n) = (1..n).map { WalkKey.new("#{tag}#{_1}" * 3) }
            def fill = ($s = Set.new(keys("n", 6)); nil)
            r = nil
            3.times do
              r = %i[flatten flatten! disjoint? intersect? join hash == subset? superset?
                     proper_subset? proper_superset? <=>].map do |m|
                $evil = false
                fill
                same = Set.new(keys("n", 6))
                more = Set.new(keys("n", 6) + keys("z", 1))
                far = Set.new(keys("m", 9))
                nested = Set[$s, WalkKey.new("z")]
                $evil = true
                res = case m
                      when :flatten then $s.flatten.size
                      when :flatten! then nested.flatten!.size
                      when :disjoint? then $s.disjoint?(far)
                      when :intersect? then $s.intersect?(far)
                      when :join then $s.join(",")
                      when :hash then $s.hash.class
                      when :== then $s == same
                      when :subset? then $s.subset?(more)
                      when :superset? then more.superset?($s)
                      when :proper_subset? then $s < more
                      when :proper_superset? then more > $s
                      when :<=> then $s <=> more
                      end
                $evil = false
                [m, res, $s.size]
              end
            end
            r
            "##,
        );
    }

    /// `delete_if`, `reject!`, `select!` and `keep_if` keep the member
    /// they hand the block alive until they have deleted it: the block
    /// may delete it first, and the deletion then hashes it once more,
    /// here an Array whose elements' `#hash` collects with nothing else
    /// holding the Array.
    #[test]
    fn deleting_walks_root_the_member_they_delete() {
        run_test_once(
            r##"
            class GcElem
              def initialize(n) = @n = n
              attr_reader :n
              def hash = (GC.start if $gc_in_hash; @n.hash)
              def eql?(o) = o.is_a?(GcElem) && o.n == @n
            end
            def fill = ($s = Set.new((1..4).map { |i| [GcElem.new(i), GcElem.new(i * 10)] }); nil)
            def drop(a) = ($s.delete(a); nil)
            r = nil
            10.times do
              r = %i[delete_if reject! select! keep_if].map do |m|
                $gc_in_hash = false
                fill
                $gc_in_hash = true
                v = case m
                    when :delete_if, :reject! then $s.send(m) { |a| drop(a) if a[0].n.odd?; a[0].n.odd? }
                    else $s.send(m) { |a| drop(a) if a[0].n.odd?; a[0].n.even? }
                    end
                $gc_in_hash = false
                [m, v.equal?($s), $s.to_a.map { |a| a.map(&:n) }]
              end
            end
            r
            "##,
        );
    }

    /// `initialize_copy` on a set that is being walked raises, as CRuby's
    /// does, from a block or from a member's `#hash`, for an inline and a
    /// boxed set: the copied table carries no iteration count, so the walk
    /// released its guard on a table that never took it (an overflow
    /// panic, or a set that refused every later insert).
    #[test]
    fn initialize_copy_inside_a_walk_raises() {
        run_test_once(
            r##"
            def t
              yield
            rescue => e
              [e.class, e.message]
            end
            class CopyKey
              def initialize(v) = @v = v
              attr_reader :v
              def hash = ($copy && ($copy = false; $s.send(:initialize_copy, $other)); @v.hash)
              def eql?(o) = o.is_a?(CopyKey) && o.v == @v
            end
            def keys(*a) = a.map { CopyKey.new(_1) }
            def copy! = $s.send(:initialize_copy, $other)
            def members = $s.to_a.map { _1.is_a?(CopyKey) ? _1.v : _1 }
            $other = Set[7, 8, 9, 10, 11]
            r = []
            [[1, 2], [1, 2, 3, 4, 5]].each do |init|
              %i[each delete_if keep_if select! reject! map! classify divide].each do |m|
                $s = Set.new(init)
                res = t do
                  case m
                  when :map! then $s.map! { |x| copy!; x }
                  when :classify, :divide then $s.send(m) { copy!; 1 }
                  else $s.send(m) { copy!; m == :keep_if || m == :select! }
                  end
                  :no_error
                end
                r << [m, res, members, t { ($s << 6).size }]
              end
            end
            %i[== subset? superset? proper_subset? proper_superset? <=> disjoint? intersect?
               hash flatten flatten!].each do |m|
              $s = Set.new(keys(1, 2, 3, 4, 5))
              same = Set.new(keys(1, 2, 3, 4, 5))
              more = Set.new(keys(1, 2, 3, 4, 5, 6, 7))
              far = Set.new(keys(*20..30))
              nested = Set[$s, CopyKey.new(0)]
              $copy = true
              res = t do
                case m
                when :== then $s == same
                when :subset? then $s.subset?(more)
                when :superset? then more.superset?($s)
                when :proper_subset? then $s < more
                when :proper_superset? then more > $s
                when :<=> then $s <=> more
                when :disjoint? then $s.disjoint?(far)
                when :intersect? then $s.intersect?(far)
                when :hash then $s.hash
                when :flatten then $s.flatten
                when :flatten! then nested.flatten!
                end
                :no_error
              end
              $copy = false
              r << [m, res, members, t { ($s << CopyKey.new(6)).size }]
            end
            r
            "##,
        );
    }
}
