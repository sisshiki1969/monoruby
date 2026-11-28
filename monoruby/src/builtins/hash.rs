use super::*;
#[cfg(target_arch = "aarch64")]
use jitgen::{AbstractState, JitContext};

//
// Hash class
//

/// CRuby's `RHASH_AR_TABLE_MAX_SIZE`: up to this many pairs a Hash is an
/// "ar table", a linear array of pairs, and past it an st table. The
/// boundary is observable where the two tables answer differently —
/// `rb_hash_assoc` looks a key up by its hash only in an st table — and
/// monoruby's own inline / boxed boundary is not the same one (it is
/// about what the *keys* are), so the pair count stands in for it.
const AR_TABLE_MAX_SIZE: usize = 8;

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_under_obj("Hash", HASH_CLASS, ObjTy::HASH);
    // `Hash.new(ifnone = nil, capacity: nil)` / `Hash.new { … }`. The
    // Ruby 3.4+ `capacity:` keyword is accepted (and ignored — monoruby's
    // Hash has no pre-sizing); `kw_rest = false` makes any *other* keyword
    // an ArgumentError. Positional args stay a `rest` so they are
    // forwarded verbatim to `#initialize` — a `Hash` subclass may override
    // `initialize` to take any arity, and the plain-`Hash` arity check
    // (0..1) lives there. (The previous `Effect::CAPTURE` flag was never
    // consumed anywhere, so dropping it changes no behavior.)
    // No class-level `new` here: `Hash.new` resolves to the generic
    // `Class#new` (builtins/class.rb) — allocate via `hash_alloc_func`,
    // then dispatch `#initialize`. Routing through the one true dispatch
    // path keeps subclass-overridden `initialize` (positional *and*
    // keyword arguments) working.
    globals.store[HASH_CLASS].set_alloc_func(hash_alloc_func);
    globals.define_builtin_class_func_rest(HASH_CLASS, "[]", hash_bracket);
    globals.define_builtin_class_func(HASH_CLASS, "try_convert", try_convert, 1);
    globals.define_builtin_class_func(HASH_CLASS, "ruby2_keywords_hash", ruby2_keywords_hash, 1);
    globals.define_builtin_class_func(HASH_CLASS, "ruby2_keywords_hash?", ruby2_keywords_hash_p, 1);

    // `capacity:` (kw slot `try_arg(1)`) is a pure allocation hint: it is
    // converted as CRuby converts it, and otherwise ignored.
    globals.define_private_builtin_func_with_kw(
        HASH_CLASS,
        "initialize",
        initialize,
        0,
        1,
        false,
        &["capacity"],
        false,
    );
    globals.define_builtin_inline_func_with(
        HASH_CLASS,
        "default",
        default,
        inline_gen2!(hash_default_value),
        0,
        1,
        false,
    );
    globals.define_builtin_inline_func(
        HASH_CLASS,
        "default_proc",
        default_proc,
        inline_gen2!(hash_default_proc),
        0,
    );
    globals.define_builtin_func(HASH_CLASS, "default_proc=", default_proc_assign, 1);
    globals.define_builtin_inline_func(
        HASH_CLASS,
        "default=",
        default_assign,
        inline_gen2!(hash_default_assign),
        1,
    );
    globals.define_builtin_func(HASH_CLASS, "==", eq, 1);
    globals.define_builtin_func(HASH_CLASS, "eql?", eql, 1);
    let hash_hash = globals.define_builtin_func(HASH_CLASS, "hash", hash, 0);
    globals.store.set_hash_hash_fid(hash_hash);
    globals.define_builtin_func(HASH_CLASS, "<", lt, 1);
    globals.define_builtin_func(HASH_CLASS, "<=", le, 1);
    globals.define_builtin_func(HASH_CLASS, ">", gt, 1);
    globals.define_builtin_func(HASH_CLASS, ">=", ge, 1);
    globals.define_builtin_inline_func(HASH_CLASS, "[]", index, inline_gen2!(hash_index), 1);
    // Positional entry access, used by Ruby-level iteration written as a
    // `while` loop over indices instead of an `each` block. Internal: the
    // index is a position in the hash's own entry order, which is not part of
    // the public Ruby interface.
    globals.define_builtin_inline_func(
        HASH_CLASS,
        "__key_at",
        key_at,
        inline_gen2!(hash_key_at),
        1,
    );
    globals.define_builtin_inline_func(
        HASH_CLASS,
        "__value_at",
        value_at,
        inline_gen2!(hash_value_at),
        1,
    );
    // The raw entry-vector length (tombstones included) and the per-position
    // liveness test — with `__key_at`/`__value_at`, the parts of the
    // Ruby-level position-indexed traversal (`Hash#each`, builtins/hash.rb)
    // that Ruby cannot express. A delete during iteration tombstones the
    // entry in place (positions must stay stable under the walk), so the
    // walk bounds itself with `__entry_count` and skips dead positions via
    // `__live_at`.
    globals.define_builtin_inline_func(
        HASH_CLASS,
        "__entry_count",
        entry_count,
        inline_gen2!(hash_entry_count),
        0,
    );
    globals.define_builtin_inline_func(
        HASH_CLASS,
        "__live_at",
        live_at,
        inline_gen2!(hash_live_at),
        1,
    );
    // Internals of the Ruby-level `Hash#each` (builtins/hash.rb). `each`
    // lives in Ruby so that a `h.each { .. }` call site can inline both the
    // method and the block; these three are the parts that cannot.
    globals.define_builtin_func(HASH_CLASS, "__pairs", pairs, 0);
    // The block-shape question `Hash#map` (builtins/hash.rb) asks about its
    // own block, answered without capturing it into a Proc.
    globals.define_builtin_inline_func(
        HASH_CLASS,
        "__block_splits_pair?",
        block_splits_pair,
        inline_gen2!(hash_block_splits_pair),
        0,
    );
    // A fresh, empty Hash pre-sized for n entries — the result-hash
    // allocation for the Ruby-side bulk builders (to_h / transform_*),
    // skipping the inline→boxed→indexed growth ladder.
    globals.define_builtin_func(HASH_CLASS, "__new_hash_with_capacity", new_hash_with_capacity, 1);
    // The table copy + positional value store behind `Hash#transform_values(!)`
    // (builtins/hash.rb): the keys never change, so the result is a dup of the
    // table whose value slots are overwritten in place — no per-pair digest,
    // probe, or insert.
    globals.define_builtin_func(HASH_CLASS, "__dup_table", dup_table, 0);
    // The one-probe `map.key?(k) ? map[k] : k` behind the block-less
    // `Hash#transform_keys(map)` walk. Inlined like `Hash#[]` (a direct
    // call skipping the method frame), so the mapping step is a single
    // probe with no builtin dispatch.
    globals.define_builtin_inline_func(
        HASH_CLASS,
        "__get_or_key",
        get_or_key,
        inline_gen2!(hash_get_or_key),
        1,
    );
    globals.define_builtin_func(HASH_CLASS, "__set_value_at", set_value_at, 2);
    globals.define_builtin_inline_func(
        HASH_CLASS,
        "__iter_begin",
        iter_begin,
        inline_gen2!(hash_iter_begin_gen),
        0,
    );
    globals.define_builtin_inline_func(
        HASH_CLASS,
        "__iter_end",
        iter_end,
        inline_gen2!(hash_iter_end_gen),
        1,
    );
    globals.define_builtin_inline_func(
        HASH_CLASS,
        "[]=",
        index_assign,
        inline_gen2!(hash_index_assign),
        2,
    );
    globals.define_builtin_func(HASH_CLASS, "clear", clear, 0);
    globals.define_builtin_func(HASH_CLASS, "replace", replace, 1);
    globals.define_builtin_func(HASH_CLASS, "compare_by_identity", compare_by_identity, 0);
    globals.define_builtin_func(HASH_CLASS, "rehash", rehash, 0);
    globals.define_builtin_func(HASH_CLASS, "delete", delete, 1);
    // collect/map: implemented in Ruby (builtins/hash.rb) for arity adaptation and subclass support
    globals.define_builtin_funcs(HASH_CLASS, "each", &["each_pair"], each, 0);
    // each_key / each_value: implemented in Ruby (builtins/hash.rb) on the
    // live positional walk, so a hot call site can inline both the method
    // and the block (same reasoning as `each`).
    globals.define_builtin_funcs(HASH_CLASS, "select", &["filter"], select, 0);
    globals.define_builtin_funcs(HASH_CLASS, "select!", &["filter!"], select_, 0);
    globals.define_builtin_inline_func(HASH_CLASS, "empty?", empty_, inline_gen2!(hash_empty), 0);
    globals.define_builtin_func_with(HASH_CLASS, "fetch", fetch, 1, 2, false);
    globals.define_builtin_inline_funcs(
        HASH_CLASS,
        "include?",
        &["has_key?", "key?", "member?"],
        include,
        inline_gen2!(hash_include_gen),
        1,
    );
    globals.define_builtin_funcs(HASH_CLASS, "inspect", &["to_s"], inspect, 0);
    globals.define_builtin_func(HASH_CLASS, "assoc", assoc, 1);
    globals.define_builtin_func(HASH_CLASS, "rassoc", rassoc, 1);
    globals.define_builtin_func(HASH_CLASS, "invert", invert, 0);
    globals.define_builtin_func(HASH_CLASS, "keys", keys, 0);
    globals.define_builtin_func_rest(HASH_CLASS, "merge", merge);
    globals.define_builtin_funcs_rest(HASH_CLASS, "merge!", &["update"], merge_);
    globals.define_builtin_inline_funcs(
        HASH_CLASS,
        "size",
        &["length"],
        size,
        inline_gen2!(hash_size),
        0,
    );
    globals.define_builtin_func(HASH_CLASS, "delete_if", delete_if, 0);
    globals.define_builtin_func(HASH_CLASS, "reject", reject, 0);
    globals.define_builtin_func(HASH_CLASS, "shift", shift, 0);
    globals.define_builtin_func(HASH_CLASS, "reject!", reject_, 0);
    globals.define_builtin_func(HASH_CLASS, "sort", sort, 0);
    globals.define_builtin_func(HASH_CLASS, "store", index_assign, 2);
    globals.define_builtin_func(HASH_CLASS, "key", key, 1);
    globals.define_builtin_funcs(HASH_CLASS, "has_value?", &["value?"], has_value, 1);
    globals.define_builtin_func(HASH_CLASS, "keep_if", keep_if, 0);
    globals.define_builtin_func(HASH_CLASS, "values", values, 0);
    globals.define_builtin_funcs_with_kw(
        HASH_CLASS,
        "clone",
        &["dup"],
        clone,
        0,
        1,
        false,
        &[],
        false,
    );
    globals.define_builtin_inline_func(
        HASH_CLASS,
        "compare_by_identity?",
        compare_by_identity_,
        inline_gen2!(hash_compare_by_identity),
        0,
    );
    globals.define_builtin_func_rest(HASH_CLASS, "values_at", values_at);
    globals.define_builtin_func(HASH_CLASS, "to_h", to_h, 0);
    init_recv_escape(globals);

    // ENV is a plain Object (extended with Enumerable in
    // builtins/env.rb) that keeps no state: its methods read and write
    // the process environment itself (`crate::environ`).
    let env = Value::object(OBJECT_CLASS);
    globals.set_constant_by_str(OBJECT_CLASS, "ENV", env);
    // CRuby's ENV is a T_DATA with no `_dump_data`: Marshal refuses it
    // (#2013). The hidden variable is how `marshal.rs` knows it.
    globals
        .store
        .set_ivar(env, IdentId::get_id("/__env"), Value::bool(true))
        .unwrap();
    globals.define_builtin_singleton_func(env, "size", env_size, 0);
    globals.define_builtin_singleton_func(env, "length", env_size, 0);
    globals.define_builtin_singleton_func(env, "empty?", env_empty, 0);
    globals.define_builtin_singleton_func_with(env, "fetch", env_fetch, 1, 2, false);
    globals.define_builtin_singleton_func(env, "[]", env_index, 1);
    globals.define_builtin_singleton_func(env, "[]=", env_index_assign, 2);
    globals.define_builtin_singleton_func(env, "store", env_index_assign, 2);
    globals.define_builtin_singleton_func(env, "delete", env_delete, 1);
    globals.define_builtin_singleton_func(env, "to_hash", env_to_hash, 0);
    globals.define_builtin_singleton_func(env, "to_h", env_to_h, 0);
    globals.define_builtin_singleton_func(env, "to_s", env_to_s, 0);
    globals.define_builtin_singleton_func(env, "rehash", env_rehash, 0);
    globals.define_builtin_singleton_func(env, "assoc", env_assoc, 1);
    globals.define_builtin_singleton_func(env, "rassoc", env_rassoc, 1);
    globals.define_builtin_singleton_func(env, "key", env_key, 1);
    for name in ["has_key?", "include?", "key?", "member?"] {
        globals.define_builtin_singleton_func(env, name, env_has_key, 1);
    }
    for name in ["has_value?", "value?"] {
        globals.define_builtin_singleton_func(env, name, env_has_value, 1);
    }
    globals.define_builtin_singleton_func_with(env, "merge!", env_merge_bang, 0, 0, true);
    globals.define_builtin_singleton_func_with(env, "update", env_merge_bang, 0, 0, true);
    globals.define_builtin_singleton_func(env, "replace", env_replace, 1);
    // The destructive methods. Each removal is an `unsetenv`, so the
    // variable also leaves the environment a child process inherits.
    globals.define_builtin_singleton_func(env, "clear", env_clear, 0);
    globals.define_builtin_singleton_func(env, "shift", env_shift, 0);
    globals.define_builtin_singleton_func(env, "delete_if", env_delete_if, 0);
    globals.define_builtin_singleton_func(env, "reject!", env_reject_bang, 0);
    globals.define_builtin_singleton_func(env, "keep_if", env_keep_if, 0);
    globals.define_builtin_singleton_funcs(env, "select!", &["filter!"], env_select_bang, 0);
    // The readers. They all hand out fresh frozen copies, transcoded to
    // `Encoding.default_internal` when one is set (the environment's own
    // bytes are taken to be in the locale encoding).
    globals.define_builtin_singleton_funcs(env, "each_pair", &["each"], env_each_pair, 0);
    globals.define_builtin_singleton_func(env, "each_key", env_each_key, 0);
    globals.define_builtin_singleton_func(env, "each_value", env_each_value, 0);
    globals.define_builtin_singleton_func(env, "keys", env_keys, 0);
    globals.define_builtin_singleton_func(env, "values", env_values, 0);
    globals.define_builtin_singleton_func(env, "to_a", env_to_a, 0);
    globals.define_builtin_singleton_func(env, "invert", env_invert, 0);
    globals.define_builtin_singleton_funcs(env, "select", &["filter"], env_select, 0);
    globals.define_builtin_singleton_func(env, "reject", env_reject, 0);
    globals.define_builtin_singleton_func_with(env, "values_at", env_values_at, 0, 0, true);
    globals.define_builtin_singleton_func_with(env, "slice", env_slice, 0, 0, true);
}

///
/// The receiver escape summaries (`RecvEscape`) of the builtins above,
/// for the JIT's elision of Hash literals used as a receiver
/// (`doc/escape_analysis.md`). A method listed here must never mutate,
/// store or return its receiver — nor return an Enumerator over it when
/// called without a block — in any of its argument forms. The ones
/// written in Ruby (builtins/hash.rb) are listed by
/// [`init_recv_escape_ruby`] once that file has run.
///
fn init_recv_escape(globals: &mut Globals) {
    use crate::executor::inline::{ElementsOp, RecvEscape};
    let table: &[(&str, RecvEscape)] = &[
        ("size", RecvEscape::elements(ElementsOp::Size)),
        ("empty?", RecvEscape::elements(ElementsOp::Empty)),
        ("[]", RecvEscape::elements(ElementsOp::HashIndex)),
        ("fetch", RecvEscape::elements(ElementsOp::HashFetch)),
        ("include?", RecvEscape::elements(ElementsOp::HashHasKey)),
        ("keys", RecvEscape::transparent()),
        ("values", RecvEscape::transparent()),
        ("values_at", RecvEscape::transparent()),
        ("key", RecvEscape::transparent()),
        ("has_value?", RecvEscape::transparent()),
        ("assoc", RecvEscape::transparent()),
        ("rassoc", RecvEscape::transparent()),
        ("invert", RecvEscape::transparent()),
        ("merge", RecvEscape::transparent()),
        ("sort", RecvEscape::transparent()),
        ("inspect", RecvEscape::transparent()),
        ("hash", RecvEscape::transparent()),
        ("==", RecvEscape::transparent()),
        ("eql?", RecvEscape::transparent()),
        ("<", RecvEscape::transparent()),
        ("<=", RecvEscape::transparent()),
        (">", RecvEscape::transparent()),
        (">=", RecvEscape::transparent()),
    ];
    set_recv_escape(globals, table);
}

///
/// The summaries of the `Hash` methods builtins/hash.rb defines: each
/// reads its receiver (through `each`, `[]`, `key?`, `dup`) and returns
/// something else. Run once startup.rb has loaded that file.
///
pub(crate) fn init_recv_escape_ruby(globals: &mut Globals) {
    use crate::executor::inline::RecvEscape;
    let table: &[(&str, RecvEscape)] = &[
        ("dig", RecvEscape::transparent()),
        ("slice", RecvEscape::transparent()),
        ("except", RecvEscape::transparent()),
        ("fetch_values", RecvEscape::transparent()),
        ("any?", RecvEscape::transparent()),
        ("all?", RecvEscape::transparent()),
        ("count", RecvEscape::transparent()),
        ("compact", RecvEscape::transparent()),
        ("flatten", RecvEscape::transparent()),
    ];
    set_recv_escape(globals, table);
}

fn set_recv_escape(globals: &mut Globals, table: &[(&str, crate::executor::inline::RecvEscape)]) {
    for (name, info) in table {
        let fid = globals
            .store
            .check_method_for_class(HASH_CLASS, IdentId::get_id(name))
            .and_then(|e| e.func_id())
            .unwrap_or_else(|| panic!("Hash#{name} is not defined"));
        globals.store.set_recv_escape(fid, *info);
    }
}

///
/// ### Hash#initialize
///
/// - initialize(ifnone = nil) -> self
/// - initialize {|hash, key| ... } -> self
///
/// Private hook called by `Hash.new`. Mirrors the previous Ruby
/// implementation in `builtins/hash.rb`:
///   - frozen receivers raise `FrozenError` before any mutation;
///   - giving both a positional `ifnone` *and* a block is
///     `ArgumentError("wrong number of arguments (given 1, expected 0)")`;
///   - with a block, the block becomes the hash's `default_proc`;
///   - with no block, the hash's default value is set to the
///     argument (or `nil` if none was given — explicitly resetting
///     any prior default).
/// Returns `self`.
#[monoruby_builtin]
fn initialize(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    pc: BytecodePtr,
) -> Result<Value> {
    let mut hash = lfp.self_val().as_hash_mut(&globals.store)?;
    // `capacity:` sizes nothing here, but `rb_hash_init` reads whatever
    // is given with NUM2LONG, nil included.
    if let Some(capa) = lfp.try_arg(1) {
        capa.num2long(vm, globals)?;
    }
    if let Some(bh) = lfp.block() {
        if lfp.try_arg(0).is_some() {
            return Err(MonorubyErr::argumenterr(
                "wrong number of arguments (given 1, expected 0)",
            ));
        }
        hash.set_defalut_proc(vm.generate_proc(globals, bh, pc)?, vm, globals)?;
    } else {
        hash.set_defalut_value(lfp.try_arg(0).unwrap_or_default(), vm, globals)?;
    }
    Ok(lfp.self_val())
}

/// Allocator for `Hash` and its subclasses.
pub(crate) extern "C" fn hash_alloc_func(class_id: ClassId, _: &mut Globals) -> Value {
    Value::hash_with_class_and_default(class_id, Value::nil())
}

/// Build an Enumerator whose `size` returns the receiver hash's current size.
/// Used by methods like `each`, `select`, `transform_values` etc. when called
/// without a block.
fn hash_to_sized_enum(
    vm: &mut Executor,
    globals: &Globals,
    method: IdentId,
    lfp: Lfp,
    pc: BytecodePtr,
) -> Result<Value> {
    // The Enumerator records the name the *call site* used, so
    // `{a: 1}.filter` reads `…:filter` rather than `…:select` (CRuby's
    // `rb_frame_this_func()`); `method` is the definition name, and the
    // fallback wherever the call site cannot be trusted to name it.
    let method = super::kernel::invoked_name(vm, globals, lfp.self_val(), method);
    let size = Value::integer(lfp.self_val().as_hash().len() as i64);
    vm.generate_enumerator_with_size(method, lfp.self_val(), lfp.iter().collect(), pc, Some(size))
}

///
/// ### Hash.[]
///
/// - Hash[] -> {}
/// - Hash[key, value, ...] -> {key => value, ...}
/// - Hash[hash] -> new_hash (copy)
/// - Hash[object] -> attempts conversion
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/s/=5b=5d.html]
#[monoruby_builtin]
fn hash_bracket(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let class_id = lfp.self_val().as_class_id();
    let args = lfp.arg(0).as_array();
    let len = args.len();
    // The result is built in a Hash on the temp stack, not in a bare
    // `HashmapInner`: `aset` stores the frozen copy of an unfrozen String
    // key, which nothing else references, and the next key's `hash` /
    // `eql?` or the next element's `to_ary` may run a collection.
    vm.with_temp_scope(|vm| {
        let mut result = Value::hash_from_inner(HashmapInner::default());
        vm.temp_push(result);
        match len {
            0 => {}
            1 => {
                let arg = args[0];
                if arg.try_hash_ty().is_some() {
                    // Single hash argument: return a fresh hash with copied entries
                    // (do NOT carry over default value/proc or compare_by_identity).
                    result = copy_pairs(result, arg.as_hash(), vm, globals)?;
                } else if let Some(ary) = arg.try_array_ty() {
                    // Single array argument: try to convert [[k,v], ...] to hash
                    hash_from_array_pairs(result.as_hash(), ary, vm, globals)?;
                } else {
                    // Try to_hash first. What `to_hash` / `to_ary` answers is
                    // referenced from nowhere else, so it goes on the temp
                    // stack while its entries are copied.
                    let to_hash_id = IdentId::get_id("to_hash");
                    if let Some(coerced) =
                        crate::coerce::check_funcall(vm, globals, arg, to_hash_id)?
                        && coerced.try_hash_ty().is_some()
                    {
                        vm.temp_push(coerced);
                        result = copy_pairs(result, coerced.as_hash(), vm, globals)?;
                    } else if let Some(coerced) =
                        crate::coerce::check_funcall(vm, globals, arg, IdentId::TO_ARY)?
                        && let Some(ary) = coerced.try_array_ty()
                    {
                        vm.temp_push(coerced);
                        hash_from_array_pairs(result.as_hash(), ary, vm, globals)?;
                    } else {
                        return Err(MonorubyErr::argumenterr(
                            "odd number of arguments for Hash".to_string(),
                        ));
                    }
                }
            }
            _ => {
                if len % 2 != 0 {
                    return Err(MonorubyErr::argumenterr(
                        "odd number of arguments for Hash".to_string(),
                    ));
                }
                let mut map = result.as_hash();
                for i in (0..len).step_by(2) {
                    map.aset(args[i], args[i + 1], vm, globals)?;
                }
            }
        }
        // Tag the result with the calling class so `MyHash[...]` returns a
        // `MyHash`. Do NOT call `#initialize` on the subclass (matches CRuby).
        if class_id != HASH_CLASS {
            result.change_class_fresh(class_id);
        }
        Ok(result)
    })
}

/// `Hash[hash]`: the Hash that takes the place of the empty *dst* as a
/// copy of *src*'s pairs, without its default or its compare_by_identity
/// mode. CRuby's `rb_hash_s_create` copies the table as it stands
/// (`hash_copy`), so no key is hashed again and no Ruby code runs; only a
/// Hash that compares by identity is stored pair by pair, since the copy
/// keys on `hash` / `eql?`. Both Hashes must be rooted.
fn copy_pairs(dst: Value, src: Hashmap, vm: &mut Executor, globals: &mut Globals) -> Result<Value> {
    if src.is_empty() {
        return Ok(dst);
    }
    if src.is_compare_by_identity() {
        aset_pairs(dst.as_hash(), src, vm, globals)?;
        return Ok(dst);
    }
    let mut inner = src.inner().clone_inner();
    // `clone_inner` copies the default (`Hash#dup` semantics); drop it.
    inner
        .as_mut()
        .set_defalut_value(Value::nil(), vm, globals)?;
    Ok(Value::hash_from_inner(inner))
}

/// Store every pair of `src` into `dst` with `aset`, in order. Both must
/// be rooted. `src` is walked as CRuby's `rb_hash_foreach` walks it
/// (`Hashmap::walk`): the keys' `hash` / `eql?` may change it meanwhile.
fn aset_pairs(
    mut dst: Hashmap,
    src: Hashmap,
    vm: &mut Executor,
    globals: &mut Globals,
) -> Result<()> {
    src.walk(vm, globals, |vm, globals, _, k, v| {
        dst.aset(k, v, vm, globals)?;
        Ok(false)
    })?;
    Ok(())
}

/// Store the elements of `ary` (expected to be [k,v] pairs) into `map`.
///
/// `[k, v]` pairs become entries; `[k]` becomes `k => nil` (matching CRuby).
/// Anything else raises `ArgumentError` with the index of the offending element.
/// `map` and `ary` must be rooted: the elements' `to_ary` and the keys'
/// `hash` / `eql?` run Ruby.
fn hash_from_array_pairs(
    mut map: Hashmap,
    ary: Array,
    vm: &mut Executor,
    globals: &mut Globals,
) -> Result<()> {
    // By index, with the length read again each time (as CRuby's
    // `rb_hash_s_create` walks it): a `to_ary` may change the Array.
    vm.with_temp_scope(|vm| {
        // Two slots, kept for the walk, hold the pair being stored.
        let slot = vm.temp_len();
        vm.temp_push(Value::nil());
        vm.temp_push(Value::nil());
        let mut idx = 0;
        while idx < ary.len() {
            let elem = ary[idx];
            let Some(pair) = elem.check_array_type(vm, globals)? else {
                // CRuby formats `nil`/`true`/`false` literally and uses
                // class name for everything else.
                let label = if elem.is_nil() {
                    "nil".to_string()
                } else if elem.id() == crate::value::TRUE_VALUE {
                    "true".to_string()
                } else if elem.id() == crate::value::FALSE_VALUE {
                    "false".to_string()
                } else {
                    elem.get_real_class_name(globals)
                };
                return Err(MonorubyErr::argumenterr(format!(
                    "wrong element type {} at {} (expected array)",
                    label, idx
                )));
            };
            let (k, v) = match pair.len() {
                1 => (pair[0], Value::nil()),
                2 => (pair[0], pair[1]),
                n => {
                    return Err(MonorubyErr::argumenterr(format!(
                        "invalid number of elements ({} for 1..2)",
                        n
                    )));
                }
            };
            // The key and the value wait on the temp stack while `aset`
            // runs the key's `hash` / `eql?`: the pair may be one that
            // `to_ary` made, referenced from nowhere else, or that Ruby
            // code may empty, and a collection would then free what is
            // about to be stored.
            vm.temp_set(slot, k);
            vm.temp_set(slot + 1, v);
            map.aset(k, v, vm, globals)?;
            idx += 1;
        }
        Ok(())
    })
}

///
/// ### Hash.try_convert
///
/// - try_convert(obj) -> Hash | nil
///
/// Tries to convert obj into a Hash, using to_hash method.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/s/try_convert.html]
///
/// ### Hash.ruby2_keywords_hash
/// - ruby2_keywords_hash(hash) -> Hash
///
/// Returns a duplicate of `hash` carrying the ruby2_keywords flag
/// (a `*rest` splat whose final element carries the flag turns it
/// back into keywords at dispatch).
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/s/ruby2_keywords_hash.html]
#[monoruby_builtin]
fn ruby2_keywords_hash(
    _: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let h = lfp.arg(0);
    if h.try_hash_ty().is_none() {
        return Err(MonorubyErr::typeerr(format!(
            "no implicit conversion of {} into Hash",
            h.builtin_class_name(&globals.store)
        )));
    }
    // A full dup: preserves the receiver's class (Hash subclass) and
    // instance variables, exactly like the generic Object#dup path.
    let dup = h.dup();
    dup.try_hash_ty().unwrap().set_ruby2_keywords_flag();
    Ok(dup)
}

///
/// ### Hash.ruby2_keywords_hash?
/// - ruby2_keywords_hash?(hash) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/s/ruby2_keywords_hash=3f.html]
#[monoruby_builtin]
fn ruby2_keywords_hash_p(
    _: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let h = lfp.arg(0);
    if h.try_hash_ty().is_none() {
        return Err(MonorubyErr::typeerr(format!(
            "no implicit conversion of {} into Hash",
            h.builtin_class_name(&globals.store)
        )));
    }
    Ok(Value::bool(h.as_hashmap_inner().ruby2_keywords_flag()))
}

#[monoruby_builtin]
fn try_convert(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    Ok(match lfp.arg(0).check_hash_type(vm, globals)? {
        Some(h) => h.as_val(),
        None => Value::nil(),
    })
}

///
/// ### Hash#default
///
/// - default -> object | nil
/// - default(key) -> object | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/default.html]
#[monoruby_builtin]
fn default(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let hash = lfp.self_val().as_hash();
    match lfp.try_arg(0) {
        Some(key) => builtin_default_for(vm, globals, lfp.self_val(), key),
        None => {
            let default = hash.defalut_value().unwrap_or_default();
            Ok(default)
        }
    }
}

///
/// ### Hash#default_proc
///
/// - default_proc -> Proc | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/default_proc.html]
#[monoruby_builtin]
fn default_proc(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let hash = lfp.self_val().as_hash();
    Ok(hash.defalut_proc().map(Proc::as_val).unwrap_or_default())
}

///
/// ### Hash#default_proc=
///
/// - default_proc=(proc_or_nil)
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/default_proc=3d.html]
#[monoruby_builtin]
fn default_proc_assign(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    lfp.self_val().ensure_not_frozen(&globals.store)?;
    let arg = lfp.arg(0);
    if arg.is_nil() {
        let mut hash = lfp.self_val().as_hash();
        hash.set_defalut_value(Value::nil(), vm, globals)?;
        return Ok(Value::nil());
    }
    // Coerce via :to_proc when arg is not already a Proc, mirroring CRuby.
    let proc_val = if arg.is_proc().is_some() {
        arg
    } else {
        let to_proc_id = IdentId::TO_PROC;
        let coerced =
            vm.invoke_method_if_exists(globals, to_proc_id, arg, &[], None, None)?;
        match coerced {
            Some(v) if v.is_proc().is_some() => v,
            _ => {
                return Err(MonorubyErr::typeerr(format!(
                    "wrong default_proc type {} (expected Proc)",
                    arg.get_real_class_name(globals)
                )));
            }
        }
    };
    let proc = proc_val.is_proc().unwrap();
    // For lambdas, the arity must be exactly 2 (matches CRuby's hash.c).
    let func_id = proc.func_id();
    let is_lambda = !globals[func_id].is_block_style();
    if is_lambda {
        let arity = globals[func_id].arity();
        if arity != 2 {
            return Err(MonorubyErr::typeerr(format!(
                "default_proc takes two arguments (2 for {arity})"
            )));
        }
    }
    let mut hash = lfp.self_val().as_hash();
    hash.set_defalut_proc(proc, vm, globals)?;
    Ok(proc_val)
}

///
/// ### Hash#default=
///
/// - default=(value)
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/default=3d.html]
#[monoruby_builtin]
fn default_assign(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    lfp.self_val().ensure_not_frozen(&globals.store)?;
    let default = lfp.arg(0);
    lfp.self_val()
        .as_hash()
        .set_defalut_value(default, vm, globals)?;
    Ok(default)
}

///
/// ### Hash#length
///
/// - length -> Integer
/// - size -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/length.html]
#[monoruby_builtin]
fn size(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let len = lfp.self_val().as_hash().len();
    Ok(Value::integer(len as i64))
}

///
/// ### Hash#==
///
/// - self == other -> bool
/// - self === other -> bool
/// - self.eql?(other) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/length.html]
#[monoruby_builtin]
fn eq(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let rhs_v = lfp.arg(0);
    // A Hash equals itself, whatever it holds — `rb_hash_equal` answers
    // that before looking at a single entry, and it is not only a
    // shortcut: `h = { x: Float::NAN }; h == h` is true in CRuby, and was
    // false here.
    if self_val.id() == rhs_v.id() {
        return Ok(Value::bool(true));
    }
    let lhs = self_val.as_hash();
    let rhs = if let Some(rhs) = rhs_v.try_hash_ty() {
        rhs
    } else {
        return Ok(Value::bool(false));
    };
    if lhs.len() != rhs.len() {
        return Ok(Value::bool(false));
    }
    // Two *non-empty* hashes that differ only in `compare_by_identity` are
    // not equal (their key-equality semantics differ). Two empty hashes
    // are equal regardless of the flag, matching CRuby.
    if lhs.len() != 0 && lhs.is_compare_by_identity() != rhs.is_compare_by_identity() {
        return Ok(Value::bool(false));
    }
    crate::value::exec_recursive_paired(self_val.id(), rhs_v.id(), || {
        // Walked as `rb_hash_foreach(hash1, eql_i)` walks it: the keys'
        // `hash` / `eql?` and the values' `==` may change the receiver.
        let differ = lhs.walk(vm, globals, |vm, globals, _, k, lhs_value| {
            // `rb_equal` per value, identity included: that is what makes
            // `{ x: Float::NAN } == { x: Float::NAN }` true, since both
            // hold the same NaN.
            Ok(match rhs.get(k, vm, globals)? {
                Some(rhs_value) => !vm.rb_equal(globals, lhs_value, rhs_value)?,
                None => true,
            })
        })?;
        Ok(Value::bool(!differ))
    }, Value::bool(true))
}

///
/// ### Hash#eql?
///
/// - eql?(other) -> bool
///
/// Like `==`, but compares values via `#eql?` rather than `#==`.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/eql=3f.html]
#[monoruby_builtin]
fn eql(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let rhs_v = lfp.arg(0);
    // Identity first, as in `==` above.
    if self_val.id() == rhs_v.id() {
        return Ok(Value::bool(true));
    }
    let lhs = self_val.as_hash();
    let rhs = if let Some(rhs) = rhs_v.try_hash_ty() {
        rhs
    } else {
        return Ok(Value::bool(false));
    };
    if lhs.len() != rhs.len() {
        return Ok(Value::bool(false));
    }
    // Two *non-empty* hashes that differ only in `compare_by_identity` are
    // not equal (their key-equality semantics differ). Two empty hashes
    // are equal regardless of the flag, matching CRuby.
    if lhs.len() != 0 && lhs.is_compare_by_identity() != rhs.is_compare_by_identity() {
        return Ok(Value::bool(false));
    }
    let eql_id = IdentId::EQL_;
    crate::value::exec_recursive_paired(self_val.id(), rhs_v.id(), || {
        // Walked, as in `==` above.
        let differ = lhs.walk(vm, globals, |vm, globals, _, k, lhs_value| {
            let Some(rhs_value) = rhs.get(k, vm, globals)? else {
                return Ok(true);
            };
            // `rb_eql` checks identity before dispatching, so a value that
            // is not `eql?` to itself (a NaN) still matches itself.
            if lhs_value.id() == rhs_value.id() {
                return Ok(false);
            }
            let result = vm.invoke_method_inner(
                globals,
                eql_id,
                lhs_value,
                &[rhs_value],
                None,
                None,
            )?;
            Ok(!result.as_bool())
        })?;
        Ok(Value::bool(!differ))
    }, Value::bool(true))
}

///
/// ### Hash#hash
///
/// - hash -> Integer
///
/// Returns an order-independent hash code derived from each key/value
/// pair. Self-referential hashes use a per-thread recursion guard so the
/// recursive node contributes a stable sentinel rather than recursing.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/hash.html]
#[monoruby_builtin]
fn hash(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::integer(hash_hash_value(
        vm,
        globals,
        lfp.self_val(),
    )?))
}

/// `rb_hash_hash`: what `Hash#hash` answers, and the hash value a Hash
/// files a Hash key under ([`Value::hash_value`]) while that method is
/// the builtin one.
pub(crate) fn hash_hash_value(
    vm: &mut Executor,
    globals: &mut Globals,
    self_val: Value,
) -> Result<i64> {
    // No guard of its own: `rb_hash_hash` has none, and the
    // outer-recursion collapse (CRuby's `rb_exec_recursive_outer`) is
    // around each key and value it hashes (`Value::hash_value`), so a
    // cycle at any depth collapses the outermost of those to the
    // sentinel and `h.hash == {x: h}.hash` when h[:x] = h.
    let h = self_val.as_hash();
    // Use a non-commutative pair mix so that swapping values between
    // keys yields different hashes (otherwise `{a:2,b:7}` and
    // `{a:7,b:2}` would collide). Per-pair hashes are summed (rather
    // than XOR'd) so identical values across pairs don't cancel.
    // Size is folded in to distinguish hashes of different cardinality.
    const KEY_MIX: i64 = 0x100000001b3u64 as i64;
    const VAL_MIX: i64 = 0xc6bc279692b5c323u64 as i64;
    const KEY_OFFSET: i64 = 0x9e3779b97f4a7c15u64 as i64;
    let mut acc: i64 = (h.len() as i64).wrapping_mul(0x9ddfea08eb382d69u64 as i64);
    // Walked as `rb_hash_foreach(hash, hash_i)` walks it: a key's or a
    // value's `hash` may change the receiver.
    h.walk(vm, globals, |vm, globals, _, k, v| {
        let kh = k.hash_value(vm, globals)?;
        let vh = v.hash_value(vm, globals)?;
        let kpart = kh.wrapping_add(KEY_OFFSET).wrapping_mul(KEY_MIX);
        let vpart = vh.wrapping_mul(VAL_MIX).rotate_left(13);
        acc = acc.wrapping_add(kpart ^ vpart);
        Ok(false)
    })?;
    // Fold into Fixnum range.
    Ok(acc >> 1)
}

/// Check if all key-value pairs in `sub` exist in `sup`.
fn hash_subset(
    sub: Hashmap,
    sup: Hashmap,
    vm: &mut Executor,
    globals: &mut Globals,
) -> Result<bool> {
    // One of the two may be what the operand's `to_hash` answered, which
    // nothing else references: keep both on the temp stack, or a
    // collection in a key's `hash` / `eql?` or a value's `==` frees the
    // Hash being walked.
    vm.with_temp_scope(|vm| {
        vm.temp_push(sub.as_val());
        vm.temp_push(sup.as_val());
        // Walked as `rb_hash_foreach(hash, hash_le_i)` walks it: the key's
        // `hash` / `eql?` may take the pair out of `sub`, and `sub_value`
        // is compared after.
        let differ = sub.walk(vm, globals, |vm, globals, _, k, sub_value| {
            Ok(match sup.get(k, vm, globals)? {
                Some(sup_value) => !vm.eq_values_bool(globals, sub_value, sup_value)?,
                None => true,
            })
        })?;
        Ok(!differ)
    })
}

///
/// ### Hash#<
///
/// - self < other -> bool
///
/// Returns true if self is a proper subset of other.
///
#[monoruby_builtin]
fn lt(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let lhs = lfp.self_val().as_hash();
    let rhs_val = lfp.arg(0).coerce_to_hash(vm, globals)?;
    let rhs = rhs_val;
    let result = lhs.len() < rhs.len() && hash_subset(lhs, rhs, vm, globals)?;
    Ok(Value::bool(result))
}

///
/// ### Hash#<=
///
/// - self <= other -> bool
///
/// Returns true if self is a subset of other.
///
#[monoruby_builtin]
fn le(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let lhs = lfp.self_val().as_hash();
    let rhs_val = lfp.arg(0).coerce_to_hash(vm, globals)?;
    let rhs = rhs_val;
    let result = lhs.len() <= rhs.len() && hash_subset(lhs, rhs, vm, globals)?;
    Ok(Value::bool(result))
}

///
/// ### Hash#>
///
/// - self > other -> bool
///
/// Returns true if self is a proper superset of other.
///
#[monoruby_builtin]
fn gt(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let lhs = lfp.self_val().as_hash();
    let rhs_val = lfp.arg(0).coerce_to_hash(vm, globals)?;
    let rhs = rhs_val;
    let result = lhs.len() > rhs.len() && hash_subset(rhs, lhs, vm, globals)?;
    Ok(Value::bool(result))
}

///
/// ### Hash#>=
///
/// - self >= other -> bool
///
/// Returns true if self is a superset of other.
///
#[monoruby_builtin]
fn ge(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let lhs = lfp.self_val().as_hash();
    let rhs_val = lfp.arg(0).coerce_to_hash(vm, globals)?;
    let rhs = rhs_val;
    let result = lhs.len() >= rhs.len() && hash_subset(rhs, lhs, vm, globals)?;
    Ok(Value::bool(result))
}

///
/// ### Hash#[]=
///
/// - self[key] = value
/// - store(key, value) -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/=5b=5d=3d.html]
#[monoruby_builtin]
fn index_assign(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let key = lfp.arg(0);
    let val = lfp.arg(1);
    // CRuby: when storing into a Hash with a fresh String key, the key
    // is dup'd and frozen so later mutation of the caller's String can't
    // corrupt the hash — only when the key is new: an existing key is
    // kept, and nothing is copied (`Hashmap::aset`). Frozen strings are
    // stored as-is. A compare_by_identity hash keys on object identity,
    // so it stores the caller's String as-is (no copy, no freeze).
    lfp.self_val()
        .as_hash_mut(&globals.store)?
        .aset(key, val, vm, globals)?;
    Ok(val)
}

///
/// ### Hash#[]
///
/// - self[key] -> object | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/=5b=5d.html]
#[monoruby_builtin]
fn index(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    hash_aref(vm, globals, lfp.self_val(), lfp.arg(0))
}

/// `Hash#[]` on the Hash *hash*, as CRuby's `rb_hash_aref`: the stored
/// value, or on a miss the `default` *method* (`rb_funcall id_default`),
/// so that a Hash subclass overriding `#default` is honoured — unless
/// `default` is still the builtin (`rb_method_basic_definition_p`), whose
/// answer is read here without a dispatch.
pub(crate) fn hash_aref(
    vm: &mut Executor,
    globals: &mut Globals,
    hash: Value,
    key: Value,
) -> Result<Value> {
    if let Some(v) = Hashmap::new(hash).get(key, vm, globals)? {
        return Ok(v);
    }
    if hash_default_is_builtin(&globals.store, hash.class()) {
        builtin_default_for(vm, globals, hash, key)
    } else {
        vm.invoke_method_inner(globals, IdentId::DEFAULT, hash, &[key], None, None)
    }
}

/// The builtin `Hash#default(key)`: the default proc's answer for
/// (*hash*, *key*), or the default value.
fn builtin_default_for(
    vm: &mut Executor,
    globals: &mut Globals,
    hash: Value,
    key: Value,
) -> Result<Value> {
    let h = hash.as_hash();
    if let Some(default_proc) = h.defalut_proc() {
        vm.invoke_proc(globals, &default_proc, &[hash, key])
    } else {
        Ok(h.defalut_value().unwrap_or_default())
    }
}

/// Whether `default` on a Hash of class `class_id` (a subclass or a
/// singleton class included) is still the builtin `Hash#default` — the
/// stored default value, or the default proc — rather than a user
/// redefinition: CRuby's `rb_method_basic_definition_p(CLASS_OF(hash),
/// id_default)`, the test `rb_hash_aref` makes on a miss before it
/// reads the default itself instead of dispatching.
pub(crate) fn hash_default_is_builtin(store: &Store, class_id: ClassId) -> bool {
    match store
        .check_method_for_class(class_id, IdentId::DEFAULT)
        .and_then(|entry| entry.func_id())
    {
        Some(fid) => matches!(
            store[fid].kind,
            FuncKind::Builtin { abs_address }
                if abs_address == default as BuiltinFn as *const u8 as u64
        ),
        None => false,
    }
}

/// `Hash#default=` with the frozen check already deopt-guarded by JIT code:
/// runs the promotion / box-allocation cases the inline fast path cannot,
/// then answers the assigned value (what `Hash#default=` returns).
extern "C" fn hash_default_assign_extern(
    vm: &mut Executor,
    globals: &mut Globals,
    recv: Value,
    val: Value,
) -> Option<Value> {
    match recv.as_hash().set_defalut_value(val, vm, globals) {
        Ok(()) => Some(val),
        Err(err) => {
            vm.set_error(err);
            None
        }
    }
}

///
/// Inline `Hash#default=` as machine code.
///
/// The common shapes store in place: a boxed hash that already carries a
/// default box gets its discriminant/payload overwritten (replacing a
/// default proc exactly as `Hash#default=` does), and a nil assignment
/// with no default box is a no-op. First-time non-nil defaults (box
/// allocation, possibly inline→boxed promotion) go through the runtime
/// call. The in-place store bypasses the builtin's frozen check, so a
/// frozen receiver deopts to the interpreter to raise `FrozenError`.
///
fn hash_default_assign(
    state: &mut AbstractState,
    ir: &mut AsmIr,
    _: &JitContext,
    store: &Store,
    callid: CallSiteId,
    _: Option<CachedClass>,
    _: Option<CachedClass>,
) -> bool {
    let callsite = &store[callid];
    if !callsite.is_simple() || callsite.pos_num != 1 {
        return false;
    }
    let CallSiteInfo {
        dst, args, recv, ..
    } = *callsite;
    state.load(ir, recv, GP::Rdi);
    let deopt = ir.new_deopt(state);
    ir.guard_frozen(deopt);
    state.load(ir, args, GP::Rsi);
    let using_fpr = state.get_using_fpr(ir);
    ir.fpr_save(using_fpr);
    ir.inline(move |r#gen, _, _, _| {
        r#gen.emit_hash_default_assign(hash_default_assign_extern as *const () as u64)
    });
    ir.fpr_restore(using_fpr);
    let error = ir.new_error(state);
    ir.handle_error(error);
    state.def_rax2acc(ir, dst);
    true
}

///
/// Inline `Hash#[]` as a direct call to `hashindex_pure`, skipping the Ruby method
/// frame.
///
/// Only a receiver whose class is exactly `Hash` inlines. `Hash#[]` consults
/// the `#default` *method* on a miss (CRuby's `rb_funcall(id_default)`), which
/// a subclass — or a singleton — may override; `hashindex_pure` reads the stored
/// default directly, so for those receivers the generic path must stay in
/// charge. A redefinition of `Hash#default` itself is covered by the class
/// version guard emitted ahead of the inliner.
///
/// The helper behind the call (and behind the probe's slow path) is the
/// pure one, [`hashindex_pure`]: a helper called from JIT code has no
/// frame, so it must not run Ruby (see [`inline_helper_call`]), and a
/// key's `#hash` / `#eql?` or the Hash's default proc may be Ruby code.
/// Where it declines, the JIT code deopts and the interpreter calls the
/// builtin in a frame of its own; a site that keeps declining is stamped
/// (`RecompileReason::ImpureHelper`) and takes the ordinary call from its
/// recompile on (#2156).
///
fn hash_index(
    state: &mut AbstractState,
    ir: &mut AsmIr,
    ctx: &JitContext,
    store: &Store,
    callid: CallSiteId,
    recv_class: Option<CachedClass>,
    idx_class: Option<CachedClass>,
) -> bool {
    let Some(recv_class) = recv_class else {
        // The call site could not prove the receiver's class (a multi-class
        // dispatch arm / the class-set guard); this generator needs it.
        return false;
    };
    let callsite = &store[callid];
    if !callsite.is_simple() {
        return false;
    }
    if callsite.pos_num != 1 {
        return false;
    }
    if !recv_class.is(HASH_CLASS) || state.pc().impure_helper() {
        return false;
    }
    let helper = hashindex_pure as *const () as u64;
    state.load(ir, callsite.args, GP::Rcx);
    state.load(ir, callsite.recv, GP::Rdx);
    // A compile-time key (`LinkMode::C`: a frozen String literal, a folded
    // String constant, an immediate literal) carries an identity promise
    // stronger than any class guard, so its digest is computed *now* — by
    // the same leaves the runtime probe would call, under the same
    // per-process seed — and baked into the probe as an immediate. The
    // key-class guard and the key-polymorphism question disappear with
    // the runtime hashing: whatever other keys this site has seen, *this*
    // compile indexes with exactly this key.
    if let Some(layout) = hash_entries_layout()
        && let Some(v) = state.concrete_value(callsite.args)
    {
        use crate::codegen::jitgen::asmir::ProbeDigest;
        let const_probe = if v.is_packed_value() && v.id() != FLOAT_ZERO {
            // Identity (`key_eq: None`) is exact for every packed key but
            // `0.0`, which a heap `-0.0` key answers for too.
            Some((ProbeDigest::Const(packed_digest_c(v.id())), None))
        } else if v.is_plain_rstring_inner().is_some() {
            Some((
                ProbeDigest::Const(string_digest_c(v.id())),
                Some(string_key_eq_c as *const () as u64),
            ))
        } else {
            None
        };
        if let Some((digest, key_eq)) = const_probe {
            let using_fpr = state.get_using_fpr(ir);
            ir.fpr_save(using_fpr);
            ir.hash_probe(layout, helper, digest, key_eq);
            ir.fpr_restore(using_fpr);
            ctx.pure_helper_exit(state, ir);
            state.def_rax2acc(ir, callsite.dst);
            return true;
        }
    }
    // The probe in line, when the key is a Symbol, `nil`, `true` or `false`
    // (an immediate's digest is its bits through the mixer —
    // `Value::packed_hash_value`; a fixnum's is too since #2204, and could
    // join them), or a String of class `String` itself (a subclass's `eql?` is
    // dispatched — see `string_key_eq`). The key's class comes from this
    // site's inline cache, so it is guarded; the receiver's shape is checked
    // inside the probe, and a shape it does not handle is answered by the
    // builtin call, not by an exit.
    //
    // The inline cache holds the *last* key class. A site the VM has seen
    // index a Hash with more than one key class (Rails' `tag_options`
    // reads its options Hash with a Symbol and then with a String) takes
    // the builtin call, which keys on nothing: the probe's guard would miss
    // on every other call. The PMC is what says so — the POLY byte alone
    // does not, since it is stamped for a receiver-class change as well,
    // and a receiver-polymorphic site's Hash arm may still be
    // key-monomorphic.
    let key_polymorphic = {
        let pmc = &callsite.pmc;
        let mut seen: Option<Option<CachedClass>> = None;
        pmc.overflow() != 0
            || pmc
                .entries()
                .iter()
                .filter(|e| e.recv.is(HASH_CLASS))
                .any(|e| match seen {
                    None => {
                        seen = Some(e.arg);
                        false
                    }
                    Some(first) => first != e.arg,
                })
    };
    let probe = idx_class.filter(|_| !key_polymorphic).and_then(|kc| {
        use crate::codegen::jitgen::asmir::ProbeDigest;
        match kc.id() {
            SYMBOL_CLASS | NIL_CLASS | TRUE_CLASS | FALSE_CLASS => Some((
                kc,
                ProbeDigest::Call(packed_digest_c as *const () as u64),
                None,
            )),
            // String keys go through the site-local last-key memo: the
            // run-time-variable-but-frozen key (an attribute name, an
            // interned column) is the shape where the full byte hash per
            // lookup actually hurts.
            STRING_CLASS => Some((
                kc,
                ProbeDigest::CallMemoized(string_digest_c as *const () as u64),
                Some(string_key_eq_c as *const () as u64),
            )),
            _ => None,
        }
    });
    if let (Some(layout), Some((kc, digest, key_eq))) = (hash_entries_layout(), probe) {
        // A key-monomorphic site's guard must not deopt forever once the
        // program does start feeding it a second key class: its miss is a
        // counter-gated recompile, after which the PMC shows the variance
        // and the site takes the builtin call above.
        let deopt = ctx.arg_miss_deopt(state, ir);
        state.guard_class(ir, callsite.args, GP::Rcx, kc, deopt);
        let using_fpr = state.get_using_fpr(ir);
        ir.fpr_save(using_fpr);
        ir.hash_probe(layout, helper, digest, key_eq);
        ir.fpr_restore(using_fpr);
        ctx.pure_helper_exit(state, ir);
        state.def_rax2acc(ir, callsite.dst);
        return true;
    }
    let using_fpr = state.get_using_fpr(ir);
    ir.fpr_save(using_fpr);
    ir.inline(move |r#gen, _, _, _| r#gen.emit_call_2args(helper));
    ir.fpr_restore(using_fpr);
    ctx.pure_helper_exit(state, ir);
    state.def_rax2acc(ir, callsite.dst);
    true
}

///
/// Inline `Hash#__get_or_key` as a direct call to `hashgetorkey_pure`,
/// skipping the Ruby method frame — the same shape (and emitter) as
/// `Hash#[]`, pure helper and deopt included.
///
fn hash_get_or_key(
    state: &mut AbstractState,
    ir: &mut AsmIr,
    ctx: &JitContext,
    store: &Store,
    callid: CallSiteId,
    recv_class: Option<CachedClass>,
    _: Option<CachedClass>,
) -> bool {
    let Some(recv_class) = recv_class else {
        // The call site could not prove the receiver's class (a multi-class
        // dispatch arm / the class-set guard); this generator needs it.
        return false;
    };
    let callsite = &store[callid];
    if !callsite.is_simple() || callsite.pos_num != 1 || state.pc().impure_helper() {
        return false;
    }
    if !recv_class.is(HASH_CLASS) {
        return false;
    }
    state.load(ir, callsite.args, GP::Rcx);
    state.load(ir, callsite.recv, GP::Rdx);
    let using_fpr = state.get_using_fpr(ir);
    ir.fpr_save(using_fpr);
    ir.inline(|r#gen, _, _, _| r#gen.emit_call_2args(hashgetorkey_pure as *const () as u64));
    ir.fpr_restore(using_fpr);
    ctx.pure_helper_exit(state, ir);
    state.def_rax2acc(ir, callsite.dst);
    true
}

///
/// A builtin with at most one argument, compiled as a direct call to a
/// runtime helper `f(vm, globals, recv, arg)` of the [`hashindex_pure`] ABI,
/// skipping the Ruby method frame (the builtin's native wrapper and its
/// frame). `may_fail` puts the error exit behind the call: the helper
/// returns 0 with the error set.
///
/// The helper must not run Ruby code. Without a frame of its own, the
/// code after the call keeps every assumption compiled before it (class
/// and constant versions, ivar types, class proofs), and a basic-operator
/// redefinition made in the middle of it cannot convert the caller (chain
/// deopt finds no return address to rewrite). A builtin that may dispatch
/// to Ruby takes a pure helper and a deopt instead (`hash_include_gen`).
///
pub(super) fn inline_helper_call(
    state: &mut AbstractState,
    ir: &mut AsmIr,
    store: &Store,
    callid: CallSiteId,
    pos_num: usize,
    f: u64,
    may_fail: bool,
) -> bool {
    let callsite = &store[callid];
    if !callsite.is_simple() || callsite.pos_num != pos_num {
        return false;
    }
    debug_assert!(pos_num <= 1);
    if pos_num == 1 {
        state.load(ir, callsite.args, GP::Rcx);
    }
    state.load(ir, callsite.recv, GP::Rdx);
    let using_fpr = state.get_using_fpr(ir);
    ir.fpr_save(using_fpr);
    ir.inline(move |r#gen, _, _, _| r#gen.emit_call_2args(f));
    ir.fpr_restore(using_fpr);
    if may_fail {
        let error = ir.new_error(state);
        ir.handle_error(error);
    }
    state.def_rax2acc(ir, callsite.dst);
    true
}

/// `Hash#include?` / `key?` / `has_key?` / `member?` as a direct call to
/// [`hashinclude_pure`].
///
/// A helper called from JIT code must not run Ruby (see
/// [`inline_helper_call`]), and a key's `#hash` / `#eql?` may be Ruby
/// code. So the helper answers only the keys it can look up without
/// dispatching and declines the rest: the JIT code deopts, and the
/// interpreter calls the builtin in a frame of its own. A site that keeps
/// declining is stamped (`RecompileReason::ImpureHelper`) and takes the
/// ordinary call from its recompile on.
fn hash_include_gen(
    state: &mut AbstractState,
    ir: &mut AsmIr,
    ctx: &JitContext,
    store: &Store,
    callid: CallSiteId,
    _: Option<CachedClass>,
    _: Option<CachedClass>,
) -> bool {
    let callsite = &store[callid];
    if !callsite.is_simple() || callsite.pos_num != 1 || state.pc().impure_helper() {
        return false;
    }
    state.load(ir, callsite.args, GP::Rcx);
    state.load(ir, callsite.recv, GP::Rdx);
    let using_fpr = state.get_using_fpr(ir);
    ir.fpr_save(using_fpr);
    ir.inline(|r#gen, _, _, _| r#gen.emit_call_2args(hashinclude_pure as *const () as u64));
    ir.fpr_restore(using_fpr);
    ctx.pure_helper_exit(state, ir);
    state.def_rax2acc(ir, callsite.dst);
    true
}

fn hash_iter_begin_gen(
    state: &mut AbstractState,
    ir: &mut AsmIr,
    _: &JitContext,
    store: &Store,
    callid: CallSiteId,
    _: Option<CachedClass>,
    _: Option<CachedClass>,
) -> bool {
    inline_helper_call(
        state,
        ir,
        store,
        callid,
        0,
        hash_iter_begin_c as *const () as u64,
        false,
    )
}

fn hash_iter_end_gen(
    state: &mut AbstractState,
    ir: &mut AsmIr,
    _: &JitContext,
    store: &Store,
    callid: CallSiteId,
    _: Option<CachedClass>,
    _: Option<CachedClass>,
) -> bool {
    inline_helper_call(
        state,
        ir,
        store,
        callid,
        1,
        hash_iter_end_c as *const () as u64,
        false,
    )
}

/// [`include`] for the JIT, in the [`hashindex_pure`] ABI: the answer wherever
/// finding it runs no Ruby code, and 0 — with no error set — where it
/// would. As for [`hashindex_pure`], that is a packed key, a String of
/// class `String` itself, or any key of an identity-keyed Hash: those are
/// digested and compared without dispatching `#hash` / `#eql?` (see
/// `HashRef::get`).
extern "C" fn hashinclude_pure(
    vm: &mut Executor,
    globals: &mut Globals,
    base: Value,
    key: Value,
) -> Option<Value> {
    let h = base.as_hash();
    if !(key.is_packed_value()
        || key.is_plain_rstring_inner().is_some()
        || h.is_compare_by_identity())
    {
        return None;
    }
    // `contains_key` cannot fail for such a key; should it, decline and
    // let the interpreter raise it in a frame.
    h.contains_key(key, vm, globals).ok().map(Value::bool)
}

/// [`iter_begin`] for the JIT, in the [`hashindex_pure`] ABI.
extern "C" fn hash_iter_begin_c(
    _: &mut Executor,
    _: &mut Globals,
    base: Value,
    _: Value,
) -> Option<Value> {
    Some(Value::bool(base.as_hash().iter_incr()))
}

/// [`iter_end`] for the JIT, in the [`hashindex_pure`] ABI.
extern "C" fn hash_iter_end_c(
    _: &mut Executor,
    _: &mut Globals,
    base: Value,
    recorded: Value,
) -> Option<Value> {
    base.as_hash().iter_decr(recorded.as_bool());
    Some(Value::nil())
}

///
/// Inline `Hash#[]=` as a direct call to `hashindex_assign_pure`, skipping the Ruby
/// method frame.
///
/// Unlike `Hash#[]` this needs no `Hash`-class restriction: the builtin
/// dispatches nothing a subclass could override — a subclass that overrides
/// `#[]=` itself resolves to its own method, never here.
///
fn hash_index_assign(
    state: &mut AbstractState,
    ir: &mut AsmIr,
    ctx: &JitContext,
    store: &Store,
    callid: CallSiteId,
    _: Option<CachedClass>,
    _: Option<CachedClass>,
) -> bool {
    let callsite = &store[callid];
    if !callsite.is_simple() || callsite.pos_num != 2 || state.pc().impure_helper() {
        return false;
    }
    let CallSiteInfo {
        recv, args, dst, ..
    } = *callsite;
    // The pure helper, as for `Hash#[]` (`hash_index`): the store runs a
    // new key's `#hash` / `#eql?`.
    let helper = hashindex_assign_pure as *const () as u64;
    state.load(ir, args, GP::Rsi);
    state.load(ir, args + 1usize, GP::Rdx);
    state.load(ir, recv, GP::Rdi);
    let using_fpr = state.get_using_fpr(ir);
    ir.fpr_save(using_fpr);
    ir.inline(move |r#gen, _, _, _| r#gen.emit_hash_index_assign(helper));
    ir.fpr_restore(using_fpr);
    ctx.pure_helper_exit(state, ir);
    state.def_rax2acc(ir, dst);
    true
}

///
/// ### Hash#__block_splits_pair? (internal)
///
/// Whether the *caller's* block must receive a `[key, value]` pair split in
/// two rather than whole — CRuby's `rb_block_pair_yield_optimizable`, reduced
/// to the one case where the two differ in meaning: a lambda binds strictly,
/// so one that requires at least two positional arguments cannot take the
/// pair whole, while one that does not (`->(a, *b)`, a Symbol proc) must. A
/// proc auto-splats the pair across its own parameters, which makes the split
/// unobservable there — see `Hash#map` (builtins/hash.rb).
///
/// The block is read where `Kernel#block_given?` reads it, off the caller's
/// yield home, so `map` needs no `&blk` parameter. That matters beyond the
/// Proc allocation a capture costs: reading a block parameter compiles to a
/// `BlockArg` bytecode, which bars the whole method from JIT specialization
/// (`Iseq::has_block_arg`) and moves its frame to the heap — exactly what
/// writing `each` and `map` in Ruby was meant to buy back.
///
#[monoruby_builtin]
fn block_splits_pair(
    vm: &mut Executor,
    globals: &mut Globals,
    _lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let Some(caller) = vm.cfp().prev() else {
        return Ok(Value::bool(false));
    };
    let Some(bh) = caller.lfp().yield_home().block() else {
        return Ok(Value::bool(false));
    };
    // The handler is not always a Proc — monoruby carries most block shapes
    // unmaterialized — so each is asked in its own terms, and none of them is
    // converted here.
    let split = if let Some(proc) = bh.try_proc() {
        // A materialized Proc answers as `Proc#lambda?` / `#arity` do, curry
        // and `Method#to_proc` indirections included.
        super::proc::proc_is_lambda(globals, &proc) && {
            let ar = super::proc::proc_arity_value(globals, &proc);
            ar >= 2 || ar <= -3
        }
    } else if let Some((fid, _)) = bh.try_proxy() {
        // Still a proxy: a literal block, or a lambda literal written straight
        // at the call site. It names its func, where block-style is exactly
        // the not-a-lambda flag.
        !globals[fid].is_block_style() && {
            let ar = globals[fid].arity();
            ar >= 2 || ar <= -3
        }
    } else if let Some(m) = bh.get().is_method() {
        // `&method(:m)`: `Method#to_proc` is applied lazily at the yield and
        // produces a lambda reporting the *method's* arity (`Proc#arity`).
        let ar = if m.method_missing_name().is_some() {
            -1
        } else {
            globals[m.func_id()].arity()
        };
        ar >= 2 || ar <= -3
    } else {
        // `&:sym` — a lambda of arity -2, so never split — or an object whose
        // `#to_proc` must run to say. Resolve it the way the yield will;
        // that resolution already happens on every yield to such a handler.
        match vm.get_block_data(globals, bh)?.func_id() {
            Some(fid) => {
                !globals[fid].is_block_style() && {
                    let ar = globals[fid].arity();
                    ar >= 2 || ar <= -3
                }
            }
            None => false,
        }
    };
    Ok(Value::bool(split))
}

/// The block of the method frame being compiled, when the JIT can name it
/// statically: `Some(Some(fid))` for a literal block (or a lambda literal)
/// written at the call site that entered the method, `Some(None)` when no
/// block was passed, `None` when only the runtime knows (an unspecialized
/// frame, or an `&blk` argument).
fn literal_block(jitctx: &JitContext) -> Option<Option<FuncId>> {
    jitctx
        .resolve_given_block()
        .map(|b| b.map(|info| info.block_fid))
}

/// Fold an inline-generated block-shape predicate to the constant *b*.
fn def_bool(state: &mut AbstractState, store: &Store, callid: CallSiteId, b: bool) {
    if let Some(dst) = store[callid].dst {
        state.def_C(dst, Immediate::bool(b));
    }
}

/// `Hash#__block_splits_pair?` at a call site whose block the JIT knows:
/// the answer depends on nothing but that block's func (the proxy arm of
/// [`block_splits_pair`]), so it is a compile-time constant and `Hash#map`'s
/// choice of loop folds away with it.
fn hash_block_splits_pair(
    state: &mut AbstractState,
    _: &mut AsmIr,
    jitctx: &JitContext,
    store: &Store,
    callid: CallSiteId,
    _: Option<CachedClass>,
    _: Option<CachedClass>,
) -> bool {
    if !store[callid].is_simple() {
        return false;
    }
    let Some(block) = literal_block(jitctx) else {
        return false;
    };
    let b = block.is_some_and(|fid| {
        let info = &store[fid];
        !info.is_block_style() && {
            let ar = info.arity();
            ar >= 2 || ar <= -3
        }
    });
    def_bool(state, store, callid, b);
    true
}

/// ### Hash#__key_at (internal)
///
/// The key of the `index`-th entry in insertion order, or `nil` when out of
/// range. See `entry_at`.
#[monoruby_builtin]
fn key_at(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let idx = lfp.arg(0).coerce_to_i64(globals)?;
    Ok(entry_component(lfp.self_val(), idx, true))
}

/// ### Hash#__value_at (internal)
#[monoruby_builtin]
fn value_at(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let idx = lfp.arg(0).coerce_to_i64(globals)?;
    Ok(entry_component(lfp.self_val(), idx, false))
}

///
/// ### Hash#__entry_count (internal)
///
/// The raw entry-vector length, tombstones included — the exclusive bound
/// for a position-indexed walk. Equals `size` whenever no delete happened
/// during a live traversal.
///
#[monoruby_builtin]
fn entry_count(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let n = lfp.self_val().as_hash().entry_count();
    Ok(Value::integer(n as i64))
}

///
/// ### Hash#__live_at (internal)
///
/// `true` iff the `index`-th entry position exists and is not a tombstone
/// (an entry deleted while a traversal was live). See `__entry_count`.
///
#[monoruby_builtin]
fn live_at(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let idx = lfp.arg(0).coerce_to_i64(globals)?;
    let live = idx >= 0 && lfp.self_val().as_hash().live_at(idx as usize);
    Ok(Value::bool(live))
}

///
/// ### Hash#__pairs (internal)
///
/// A snapshot of the entries as `[[k, v], ...]`.
///
/// `each` yields from this rather than indexing the live hash: deleting
/// during iteration is explicitly allowed, and it shifts the entry vector,
/// so an index-based traversal would skip the entry after each deletion.
/// Snapshotting also means a key whose `#hash` no longer matches its stored
/// slot (`Hash#rehash`, mutable keys) is still yielded.
///
#[monoruby_builtin]
fn pairs(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let hash = lfp.self_val().as_hash();
    Ok(Value::array_from_iter(
        hash.iter().map(|(k, v)| Value::array2(k, v)),
    ))
}

///
/// ### Hash#__iter_begin (internal)
///
/// Take an iteration reference, so that adding a new key while `each` is
/// running raises the way CRuby does. Returns whether the reference was
/// actually recorded — the inline representation saturates its two depth
/// bits — and that answer must be handed back to `__iter_end`.
///
#[monoruby_builtin]
fn new_hash_with_capacity(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let n = lfp.arg(0).coerce_to_int_i64(vm, globals)?.max(0) as usize;
    Ok(Value::hash_with_capacity(n))
}

///
/// ### Hash#__get_or_key (internal)
///
/// `self.key?(k) ? self[k] : k` in a single probe — the mapping step of
/// `Hash#transform_keys(map)`, CRuby's `rb_hash_lookup2(map, k, k)`.
/// Falling back to the key itself needs no sentinel, and — unlike
/// `Hash#[]` — the receiver's default value/proc is never consulted.
///
#[monoruby_builtin]
fn get_or_key(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let k = lfp.arg(0);
    Ok(lfp.self_val().as_hash().get(k, vm, globals)?.unwrap_or(k))
}

///
/// ### Hash#__dup_table (internal)
///
/// The table copy behind `Hash#transform_values`: the entries and the
/// compare_by_identity mode are duplicated, but — matching CRuby's
/// `hash_dup_with_compare_by_id` — the default value/proc is not, and the
/// copy is a plain `Hash` regardless of the receiver's class. The caller
/// then overwrites the value slots in place via `__set_value_at`, so no
/// key is ever re-hashed.
///
#[monoruby_builtin]
fn dup_table(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let mut inner = lfp.self_val().as_hashmap_inner().clone_inner();
    // clone_inner copies the default (`Hash#dup` semantics); drop it.
    inner
        .as_mut()
        .set_defalut_value(Value::nil(), vm, globals)?;
    Ok(Value::hash_from_inner(inner))
}

///
/// ### Hash#__set_value_at (internal)
///
/// Overwrite the value of the `index`-th entry in place — the positional
/// store behind `Hash#transform_values(!)`. The key set is untouched, so
/// no digest or probe runs; out-of-range and tombstoned positions are
/// ignored (a delete during the walk tombstones in place). Returns the
/// stored value.
///
#[monoruby_builtin]
fn set_value_at(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let idx = lfp.arg(0).coerce_to_i64(globals)?;
    let v = lfp.arg(1);
    if idx >= 0 {
        lfp.self_val().as_hash().set_value_at(idx as usize, v);
    }
    Ok(v)
}

#[monoruby_builtin]
fn iter_begin(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::bool(lfp.self_val().as_hash().iter_incr()))
}

///
/// ### Hash#__iter_end (internal)
///
/// Release the reference taken by `__iter_begin`, whose result must be
/// passed back here. Called from an `ensure`, so a block that raises or
/// breaks still balances the count.
///
#[monoruby_builtin]
fn iter_end(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    lfp.self_val().as_hash().iter_decr(lfp.arg(0).as_bool());
    Ok(Value::nil())
}

/// Shared by the builtin and the inlined C-ABI helper: a negative or
/// out-of-range index is `nil` rather than an error, so a Ruby `while` loop
/// can bound itself with `size` and never pay an error edge.
fn entry_component(recv: Value, idx: i64, want_key: bool) -> Value {
    if idx < 0 {
        return Value::nil();
    }
    match recv.as_hash().entry_at(idx as usize) {
        Some((k, v)) => {
            if want_key {
                k
            } else {
                v
            }
        }
        None => Value::nil(),
    }
}

///
/// Shared shape of the three zero-argument accessor inliners.
///
/// A block is rejected for all three: `compare_by_identity?` raises on one,
/// and for the other two a block is so unusual that keeping the generic path
/// is not worth a separate rule. `Hash#default` also takes an optional key —
/// that form invokes the default proc, so only the zero-argument call inlines.
///
fn hash_accessor_callsite(store: &Store, callid: CallSiteId) -> Option<&CallSiteInfo> {
    let callsite = &store[callid];
    if !callsite.is_simple() || callsite.pos_num != 0 || callsite.block_fid.is_some() {
        return None;
    }
    Some(callsite)
}

///
/// Inline `Hash#compare_by_identity?` as machine code.
///
fn hash_compare_by_identity(
    state: &mut AbstractState,
    ir: &mut AsmIr,
    _: &JitContext,
    store: &Store,
    callid: CallSiteId,
    _: Option<CachedClass>,
    _: Option<CachedClass>,
) -> bool {
    let Some(callsite) = hash_accessor_callsite(store, callid) else {
        return false;
    };
    let (recv, dst) = (callsite.recv, callsite.dst);
    state.load(ir, recv, GP::Rdi);
    ir.hash_compare_by_identity(GP::Rax, GP::Rdi);
    state.def_rax2acc(ir, dst);
    true
}

///
/// Inline `Hash#default` (the zero-argument form) as machine code.
///
fn hash_default_value(
    state: &mut AbstractState,
    ir: &mut AsmIr,
    _: &JitContext,
    store: &Store,
    callid: CallSiteId,
    _: Option<CachedClass>,
    _: Option<CachedClass>,
) -> bool {
    hash_default_inline(state, ir, store, callid, false)
}

///
/// Inline `Hash#default_proc` as machine code.
///
fn hash_default_proc(
    state: &mut AbstractState,
    ir: &mut AsmIr,
    _: &JitContext,
    store: &Store,
    callid: CallSiteId,
    _: Option<CachedClass>,
    _: Option<CachedClass>,
) -> bool {
    hash_default_inline(state, ir, store, callid, true)
}

fn hash_default_inline(
    state: &mut AbstractState,
    ir: &mut AsmIr,
    store: &Store,
    callid: CallSiteId,
    want_proc: bool,
) -> bool {
    let Some(callsite) = hash_accessor_callsite(store, callid) else {
        return false;
    };
    let (recv, dst) = (callsite.recv, callsite.dst);
    state.load(ir, recv, GP::Rdi);
    ir.hash_default(GP::Rax, GP::Rdi, want_proc);
    state.def_rax2acc(ir, dst);
    true
}

///
/// Inline `Hash#size` as machine code.
///
fn hash_size(
    state: &mut AbstractState,
    ir: &mut AsmIr,
    _: &JitContext,
    store: &Store,
    callid: CallSiteId,
    _: Option<CachedClass>,
    _: Option<CachedClass>,
) -> bool {
    let callsite = &store[callid];
    if !callsite.is_simple() {
        return false;
    }
    // Without a recognised entry layout the boxed length is unreachable from
    // generated code, so the generic path stays in charge.
    let Some(layout) = hash_entries_layout() else {
        return false;
    };
    let dst = callsite.dst;
    state.load(ir, callsite.recv, GP::Rdi);
    ir.hash_len_fixnum(GP::Rax, GP::Rdi, layout, true);
    state.def_reg2acc_fixnum(ir, GP::Rax, dst);
    true
}

///
/// Inline `Hash#empty?` as machine code: `Hash#size` compared with 0.
///
fn hash_empty(
    state: &mut AbstractState,
    ir: &mut AsmIr,
    _: &JitContext,
    store: &Store,
    callid: CallSiteId,
    _: Option<CachedClass>,
    _: Option<CachedClass>,
) -> bool {
    let callsite = &store[callid];
    if !callsite.is_simple() {
        return false;
    }
    let Some(layout) = hash_entries_layout() else {
        return false;
    };
    let dst = callsite.dst;
    state.load(ir, callsite.recv, GP::Rdi);
    ir.hash_len_fixnum(GP::Rax, GP::Rdi, layout, true);
    ir.fixnum_zero_to_bool(GP::Rax);
    state.def_rax2acc(ir, dst);
    true
}

///
/// Inline `Hash#__entry_count` as machine code: the raw entry-vector length,
/// tombstones included — the exclusive bound for a position-indexed walk.
/// Identical to `Hash#size` except that the tombstone count is not
/// subtracted.
///
fn hash_entry_count(
    state: &mut AbstractState,
    ir: &mut AsmIr,
    _: &JitContext,
    store: &Store,
    callid: CallSiteId,
    _: Option<CachedClass>,
    _: Option<CachedClass>,
) -> bool {
    let callsite = &store[callid];
    if !callsite.is_simple() {
        return false;
    }
    let Some(layout) = hash_entries_layout() else {
        return false;
    };
    let dst = callsite.dst;
    state.load(ir, callsite.recv, GP::Rdi);
    ir.hash_len_fixnum(GP::Rax, GP::Rdi, layout, false);
    state.def_reg2acc_fixnum(ir, GP::Rax, dst);
    true
}

///
/// Inline `Hash#__live_at` as machine code: Ruby `true` iff the position is
/// in range and the entry is not a tombstone. Total by construction, like
/// `__key_at` — no call, no error edge.
///
fn hash_live_at(
    state: &mut AbstractState,
    ir: &mut AsmIr,
    _: &JitContext,
    store: &Store,
    callid: CallSiteId,
    _: Option<CachedClass>,
    idx_class: Option<CachedClass>,
) -> bool {
    let callsite = &store[callid];
    if !callsite.is_simple() || callsite.pos_num != 1 || idx_class != Some(CachedClass::INTEGER) {
        return false;
    }
    let Some(layout) = hash_entries_layout() else {
        return false;
    };
    state.load_fixnum(ir, callsite.args, GP::Rcx);
    state.load(ir, callsite.recv, GP::Rdx);
    ir.hash_live_at(layout);
    state.def_rax2acc(ir, callsite.dst);
    true
}

///
/// Inline `Hash#__key_at` / `#__value_at` as machine code: the receiver's
/// representation is decoded, bounds-checked and indexed in line. No call, no
/// error edge — a negative or out-of-range index answers `nil` exactly as the
/// builtin does, which is what lets the Ruby-level `while` loops these exist
/// for bound themselves with `size` alone.
///
fn hash_entry_at_inline(
    state: &mut AbstractState,
    ir: &mut AsmIr,
    store: &Store,
    callid: CallSiteId,
    idx_class: Option<CachedClass>,
    want_key: bool,
) -> bool {
    let callsite = &store[callid];
    if !callsite.is_simple() || callsite.pos_num != 1 || idx_class != Some(CachedClass::INTEGER) {
        return false;
    }
    let Some(layout) = hash_entries_layout() else {
        return false;
    };
    // `load_fixnum` guards the tag, so a Bignum index deopts rather than being
    // untagged into a nonsense position.
    state.load_fixnum(ir, callsite.args, GP::Rcx);
    state.load(ir, callsite.recv, GP::Rdx);
    ir.hash_entry_at(want_key, layout);
    state.def_rax2acc(ir, callsite.dst);
    true
}

fn hash_key_at(
    state: &mut AbstractState,
    ir: &mut AsmIr,
    _: &JitContext,
    store: &Store,
    callid: CallSiteId,
    _: Option<CachedClass>,
    idx_class: Option<CachedClass>,
) -> bool {
    hash_entry_at_inline(state, ir, store, callid, idx_class, true)
}

fn hash_value_at(
    state: &mut AbstractState,
    ir: &mut AsmIr,
    _: &JitContext,
    store: &Store,
    callid: CallSiteId,
    _: Option<CachedClass>,
    idx_class: Option<CachedClass>,
) -> bool {
    hash_entry_at_inline(state, ir, store, callid, idx_class, false)
}

///
/// `Hash#[]` for the JIT: the answer wherever finding it runs no Ruby
/// code, and 0 — with no error set — where it would.
///
/// The lookup itself runs none for a packed key, a String of class
/// `String` itself, or any key of an identity-keyed Hash: those are
/// digested and compared without dispatching `#hash` / `#eql?` (see
/// `HashRef::get`). A miss runs none unless the Hash has a default proc
/// (the `#default` *method* is not consulted: the inliner admits only
/// receivers of class `Hash` itself, whose `#default` is the builtin
/// under the class-version guard). Everything else declines, and the JIT
/// code deopts so that the interpreter performs the lookup in a frame.
///
extern "C" fn hashindex_pure(
    vm: &mut Executor,
    globals: &mut Globals,
    base: Value,
    key: Value,
) -> Option<Value> {
    let h = base.as_hash();
    if !(key.is_packed_value()
        || key.is_plain_rstring_inner().is_some()
        || h.is_compare_by_identity())
    {
        return None;
    }
    // `get` cannot fail for such a key; should it, decline and let the
    // interpreter raise it in a frame.
    match h.get(key, vm, globals) {
        Ok(Some(v)) => Some(v),
        Ok(None) if h.defalut_proc().is_none() => {
            Some(h.defalut_value().unwrap_or_default())
        }
        _ => None,
    }
}

/// `Hash#__get_or_key` for the JIT: same ABI as [`hashindex_pure`], so
/// the inliner reuses `emit_call_2args` with this address. One probe, the
/// key itself on a miss, the default never consulted; the keys
/// [`hashindex_pure`] declines are declined here too.
extern "C" fn hashgetorkey_pure(
    vm: &mut Executor,
    globals: &mut Globals,
    base: Value,
    key: Value,
) -> Option<Value> {
    let h = base.as_hash();
    if !(key.is_packed_value()
        || key.is_plain_rstring_inner().is_some()
        || h.is_compare_by_identity())
    {
        return None;
    }
    h.get(key, vm, globals).ok().map(|v| v.unwrap_or(key))
}

///
/// `Hash#[]=` for the JIT, for the shapes its inline fast path leaves
/// alone (insertion, which may promote the representation, and the boxed
/// map): the store wherever it runs no Ruby code, and 0 — with no error
/// set and the Hash untouched — where it would run some or would raise.
/// Mirrors the `index_assign` builtin, including the fresh-String-key
/// dup/freeze, and answers the assigned value.
///
/// The probe runs none for the keys [`hashindex_pure`] admits (and
/// neither does promoting an inline Hash, whose stored keys are all of
/// that kind). The raising cases — a frozen receiver, a new key during
/// an iteration — are declined rather than raised, since both are
/// detected before anything is stored: the interpreter then performs
/// the store again in a frame, and raises from there.
///
extern "C" fn hashindex_assign_pure(
    vm: &mut Executor,
    globals: &mut Globals,
    base: Value,
    key: Value,
    val: Value,
) -> Option<Value> {
    if !(key.is_packed_value()
        || key.is_plain_rstring_inner().is_some()
        || base.as_hash().is_compare_by_identity())
    {
        return None;
    }
    let mut h = base.as_hash_mut(&globals.store).ok()?;
    h.aset(key, val, vm, globals).ok()?;
    Some(val)
}

///
/// ### Hash#clear
///
/// - clear -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/clear.html]
#[monoruby_builtin]
fn clear(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let mut h = lfp.self_val().as_hash_mut(&globals.store)?;
    if h.iter_active() {
        // A walk of this Hash is under way (`Hashmap::walk`, or `each`):
        // delete the pairs where they stand, as CRuby's `rb_hash_clear`
        // does then (`rb_hash_foreach(hash, clear_i, 0)`), which leaves
        // their positions to the walk as tombstones instead of dropping
        // the table under it.
        h.clear_during_iteration(vm, globals)?;
    } else {
        h.clear()?;
    }
    Ok(lfp.self_val())
}

///
/// ### Hash#replace
///
/// - replace(other) -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/replace.html]
#[monoruby_builtin]
fn replace(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let mut self_ = lfp.self_val().as_hash_mut(&globals.store)?;
    if lfp.arg(0).id() == lfp.self_val().id() {
        return Ok(lfp.self_val());
    }
    // The table is swapped whole, which a walk of it could not survive
    // (unlike `clear`, which empties it in place then).
    if self_.iter_active() {
        return Err(MonorubyErr::runtimeerr(
            "can't replace hash during iteration",
        ));
    }
    let arg = lfp.arg(0).coerce_to_hash(vm, globals)?;
    self_.replace_inner(arg.inner().clone_inner());

    Ok(lfp.self_val())
}

///
/// ### Hash#rehash
///
/// - rehash -> self
///
/// The pairs are entered again into a table of their own, which replaces
/// the receiver's once every key has been hashed — so a `#hash` that
/// raises leaves the receiver as it was, as in CRuby.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/rehash.html]
#[monoruby_builtin]
fn rehash(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let hash = self_val.as_hash();
    if hash.iter_active() {
        return Err(MonorubyErr::runtimeerr("rehash during iteration"));
    }
    self_val.ensure_not_frozen(&globals.store)?;
    // A copy for its default, default proc and comparison mode.
    let mut inner = hash.inner().clone_inner();
    inner.as_mut().clear()?;
    let tmp = Value::hash_from_inner(inner);
    vm.with_temp_scope(|vm| {
        vm.temp_push(tmp);
        hash.walk(vm, globals, |vm, globals, _, k, v| {
            tmp.as_hash().insert(k, v, vm, globals)?;
            Ok(false)
        })?;
        self_val
            .as_hash_mut(&globals.store)?
            .replace_inner(tmp.as_hash().inner().clone_inner());
        Ok(self_val)
    })
}

///
/// ### Hash#keys
///
/// - keys -> [object]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/keys.html]
#[monoruby_builtin]
fn keys(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let keys = lfp.self_val().as_hash().keys();
    Ok(Value::array_from_vec(keys))
}

///
/// ### Hash#values
///
/// - values -> [object]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/values.html]
#[monoruby_builtin]
fn values(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let keys = lfp.self_val().as_hash().values();
    Ok(Value::array_from_vec(keys))
}

///
/// ### Hash#clone
///
/// - clone -> Hash
/// - dup -> Hash
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/clone.html]
#[monoruby_builtin]
fn clone(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    // Use the real class (skipping singleton/iclass) so that singleton
    // methods on the receiver do NOT show up on the clone.
    let class_id = lfp.self_val().real_class(&globals.store).id();
    let inner = lfp.self_val().as_hashmap_inner().clone_inner();
    let mut v = Value::hash_from_inner(inner);
    if class_id != HASH_CLASS {
        v.change_class_fresh(class_id);
    }
    Ok(v)
}

///
/// ### Hash#delete
///
/// - delete(key) -> object | nil
/// - delete(key) {|key| ... } -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/delete.html]
#[monoruby_builtin]
fn delete(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let mut h = lfp.self_val().as_hash_mut(&globals.store)?;
    let key = lfp.arg(0);
    let removed_value = h.remove(key, vm, globals)?;
    if removed_value.is_none()
        && let Some(bh) = lfp.block()
    {
        return vm.invoke_block_once(globals, bh, &[key]);
    }

    Ok(removed_value.unwrap_or_default())
}

///
///
/// ### Hash#each
///
/// - each {|key, value| ... } -> self
/// - each_pair {|key, value| ... } -> self
/// - each -> Enumerator
/// - each_pair -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/each.html]
#[monoruby_builtin]
fn each(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, pc: BytecodePtr) -> Result<Value> {
    let bh = match lfp.block() {
        None => {
            let id = IdentId::EACH;
            return hash_to_sized_enum(vm, globals, id, lfp, pc);
        }
        Some(block) => block,
    };
    let hash = lfp.self_val().as_hash();
    let data = vm.get_block_data(globals, bh)?;
    let _iter_guard = hash.iter_guard();
    // Snapshot the stored (key, value) pairs before yielding so the traversal
    // tolerates deletion during iteration (CRuby allows `h.each { h.delete(k) }`
    // / `h.shift`). Yielding the snapshotted pairs directly — rather than
    // re-looking-up each key — is essential: a key's `#hash` may legitimately
    // differ from its stored slot (e.g. `Hash#rehash`, mutable keys), and a
    // re-lookup would then miss it. Adding a *new* key still raises via the
    // iteration guard on `insert`.
    let pairs: Vec<(Value, Value)> = hash.iter().collect();
    for (k, v) in pairs {
        vm.invoke_block(globals, &data, &[Value::array2(k, v)])?;
    }
    Ok(lfp.self_val())
}

///
/// ### Hash#select
///
/// - select -> Enumerator
/// - select {|key, value| ... } -> Hash
/// - filter -> Enumerator
/// - filter {|key, value| ... } -> Hash
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/filter.html]
#[monoruby_builtin]
fn select(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, pc: BytecodePtr) -> Result<Value> {
    let bh = match lfp.block() {
        None => {
            let id = IdentId::get_id("select");
            return hash_to_sized_enum(vm, globals, id, lfp, pc);
        }
        Some(block) => block,
    };
    let src = lfp.self_val().as_hash();
    let mut inner = HashmapInner::default();
    if src.is_compare_by_identity() {
        inner.compare_by_identity(vm, globals)?;
    }
    // CRuby walks a copy of the receiver (`hash_dup_with_compare_by_id`),
    // so what the block does to the receiver changes neither what is
    // yielded nor the walk itself. The copy here is the receiver's pairs,
    // taken up front and kept on the temp stack: once the block replaces or
    // deletes the receiver's entry, they are referenced from nowhere else,
    // and a collection in a later block call would free them. The result
    // can stay a bare `HashmapInner` until the walk ends: `insert` stores
    // the pairs as they are, so all it holds is on the temp stack already.
    vm.with_temp_scope(|vm| {
        let data = vm.get_block_data(globals, bh)?;
        let base = vm.temp_len();
        let n = vm.temp_push_pairs(src);
        for i in 0..n {
            let (k, v) = vm.temp_pair(base, i);
            if vm.invoke_block(globals, &data, &[k, v])?.as_bool() {
                inner.insert(k, v, vm, globals)?;
            }
        }
        Ok(Value::hash_from_inner(inner))
    })
}

///
/// ### Hash#select
///
/// select! -> Enumerator
/// select! {|key, value| ... } -> self | nil
/// filter! -> Enumerator
/// filter! {|key, value| ... } -> self | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/select=21.html]
#[monoruby_builtin]
fn select_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, pc: BytecodePtr) -> Result<Value> {
    let bh = match lfp.block() {
        None => {
            let id = IdentId::get_id("select!");
            return hash_to_sized_enum(vm, globals, id, lfp, pc);
        }
        Some(block) => block,
    };
    let changed = hash_delete_where(vm, globals, lfp, bh, false)?;
    Ok(if changed {
        lfp.self_val()
    } else {
        Value::nil()
    })
}

/// `delete_if` / `keep_if` / `reject!` / `select!`: delete, as they are
/// reached, the pairs for which the block answers `delete_when`. Whether
/// the Hash shrank, by these deletions or by the block's own.
///
/// As `rb_hash_foreach(hash, delete_if_i / keep_if_i, hash)`: the walk is
/// `Hashmap::walk`, and each deletion is made when its pair is reached
/// (the block sees the Hash without the pairs deleted so far, and a
/// `break` or an exception keeps them deleted), by position, after the
/// `rb_hash_modify` check the block may have made fail by freezing the
/// receiver.
#[inline(always)]
fn hash_delete_where(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    bh: BlockHandler,
    delete_when: bool,
) -> Result<bool> {
    let self_val = lfp.self_val();
    self_val.ensure_not_frozen(&globals.store)?;
    let data = vm.get_block_data(globals, bh)?;
    let hash = self_val.as_hash();
    let size = hash.len();
    hash.walk(vm, globals, |vm, globals, i, k, v| {
        if vm.invoke_block(globals, &data, &[k, v])?.as_bool() == delete_when {
            self_val.ensure_not_frozen(&globals.store)?;
            self_val.as_hash().delete_at(i, vm, globals)?;
        }
        Ok(false)
    })?;
    Ok(hash.len() != size)
}

///
/// ### Hash#empty?
///
/// - empty? -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/empty=3f.html]
#[monoruby_builtin]
fn empty_(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    lfp.expect_no_block()?;
    let self_ = lfp.self_val();
    let b = self_.as_hashmap_inner().is_empty();
    Ok(Value::bool(b))
}

///
/// ### Hash#has_key?
///
/// - has_key?(key) -> bool
/// - include?(key) -> bool
/// - key?(key) -> bool
/// - member?(key) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/has_key=3f.html]
#[monoruby_builtin]
fn include(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let b = lfp
        .self_val()
        .as_hash()
        .contains_key(lfp.arg(0), vm, globals)?;
    Ok(Value::bool(b))
}

///
/// ### Hash#inspect
///
/// - to_s -> String
/// - inspect -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/inspect.html]
#[monoruby_builtin]
fn inspect(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    use crate::builtins::encoding::{InspectBuf, inspect_resenc, rb_inspect_in};
    let self_val = lfp.self_val();
    if self_val.as_hash().len() == 0 {
        return Ok(Value::string_from_inner(RStringInner::from_encoding(
            b"{}",
            Encoding::UsAscii,
        )));
    }
    crate::value::exec_recursive(
        self_val.id(),
        || {
            // CRuby's `inspect_hash` / `inspect_i`: keys and values
            // through `rb_inspect`, a Symbol key as its name, the result
            // taking the first key's encoding and then that of any
            // non-ASCII piece (#1841).
            let resenc = inspect_resenc(globals);
            let escape = globals.store.inspect_escape();
            let mut buf = InspectBuf::new("{");
            let mut first = true;
            // Walked as `rb_hash_foreach(hash, inspect_i)` walks it: a
            // key's or a value's `inspect` may change the receiver.
            let hash = self_val.as_hash();
            hash.walk(vm, globals, |vm, globals, _, k, v| {
                if let Some(sym) = k.try_symbol() {
                    // `rb_sym2str(key)` gives the encoding; the label is
                    // the bare name, or `"name"` when it needs quoting
                    // (stricter than `Symbol#inspect`: operators,
                    // `=`-setters, `@` / `$` names are quoted).
                    if first {
                        buf.copy_encoding(&crate::builtins::symbol::symbol_string_inner(sym));
                    } else {
                        buf.cat(", ");
                    }
                    if let Some(label) =
                        crate::value::symbol_hash_label_in_resenc(sym, &globals.store)
                    {
                        buf.append(&globals.store, &label)?;
                    } else {
                        let label = crate::value::symbol_hash_label(sym, escape);
                        let label = if escape {
                            crate::value::escape_nonascii_to_u(&label)
                        } else {
                            label
                        };
                        buf.append_str(&globals.store, &label)?;
                    }
                    buf.cat(": ");
                } else {
                    let ks = rb_inspect_in(vm, globals, k, resenc)?;
                    let ks = ks.as_rstring_inner();
                    if first {
                        buf.copy_encoding(ks);
                    } else {
                        buf.cat(", ");
                    }
                    buf.append(&globals.store, ks)?;
                    buf.cat(" => ");
                }
                first = false;
                let vs = rb_inspect_in(vm, globals, v, resenc)?;
                buf.append(&globals.store, vs.as_rstring_inner())?;
                Ok(false)
            })?;
            buf.cat("}");
            Ok(buf.into_value())
        },
        Value::string_from_inner(RStringInner::from_encoding(b"{...}", Encoding::UsAscii)),
    )
}

///
/// ### Hash#reject
///
/// - reject {|key, value| ... } -> Hash
/// - reject -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/reject.html]
#[monoruby_builtin]
fn reject(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, pc: BytecodePtr) -> Result<Value> {
    let bh = match lfp.block() {
        None => {
            let id = IdentId::get_id("reject");
            return hash_to_sized_enum(vm, globals, id, lfp, pc);
        }
        Some(block) => block,
    };
    // `reject` returns a fresh Hash (NOT the receiver's class, NOT carrying
    // singleton methods, default value, or default_proc).
    let mut inner = HashmapInner::default();
    if lfp.self_val().as_hash().is_compare_by_identity() {
        inner.compare_by_identity(vm, globals)?;
    }
    // A rooted copy of the pairs is walked into a bare `HashmapInner`, as
    // in `select`.
    vm.with_temp_scope(|vm| {
        let p = vm.get_block_data(globals, bh)?;
        let base = vm.temp_len();
        let n = vm.temp_push_pairs(lfp.self_val().as_hash());
        for i in 0..n {
            let (k, v) = vm.temp_pair(base, i);
            if !vm.invoke_block(globals, &p, &[k, v])?.as_bool() {
                inner.insert(k, v, vm, globals)?;
            }
        }
        Ok(Value::hash_from_inner(inner))
    })
}

///
/// ### Hash#delete_if
///
/// - delete_if -> Enumerator
/// - delete_if {|key, value| ... } -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/delete_if.html]
#[monoruby_builtin]
fn delete_if(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, pc: BytecodePtr) -> Result<Value> {
    let bh = match lfp.block() {
        None => {
            let id = IdentId::get_id("delete_if");
            return hash_to_sized_enum(vm, globals, id, lfp, pc);
        }
        Some(block) => block,
    };
    hash_delete_where(vm, globals, lfp, bh, true)?;
    Ok(lfp.self_val())
}

///
/// ### Hash#reject!
///
/// - reject! -> Enumerator
/// - reject! {|key, value| ... } -> self | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/reject=21.html]
#[monoruby_builtin]
fn reject_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, pc: BytecodePtr) -> Result<Value> {
    let bh = match lfp.block() {
        None => {
            let id = IdentId::get_id("reject!");
            return hash_to_sized_enum(vm, globals, id, lfp, pc);
        }
        Some(block) => block,
    };
    let changed = hash_delete_where(vm, globals, lfp, bh, true)?;
    Ok(if changed {
        lfp.self_val()
    } else {
        Value::nil()
    })
}

///
/// ### Enumerable#sort
///
/// - sort -> [object]
/// - sort {|a, b| ... } -> [object]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/sort.html]
#[monoruby_builtin]
fn sort(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let hash = lfp.self_val().as_hash();
    // Keys that are all Fixnums, or all Strings, order the pairs by
    // themselves: two distinct keys of those classes never compare equal,
    // so the values are never consulted, and the keys sort without running
    // any Ruby.
    if lfp.block().is_none() {
        let mut keys = hash.keys();
        if matches!(
            executor::op::homogeneous_ord(globals, &keys),
            Some(executor::op::HomogeneousOrd::Fixnum | executor::op::HomogeneousOrd::String)
        ) {
            vm.sort(globals, &mut keys)?;
            let pairs = keys
                .into_iter()
                .map(|k| {
                    // The keys are of a class whose `hash` / `eql?` is
                    // answered natively, so the lookup runs no Ruby.
                    let v = hash.get(k, vm, globals)?.unwrap_or_default();
                    Ok(Value::array2(k, v))
                })
                .collect::<Result<Vec<_>>>()?;
            return Ok(Value::array_from_vec(pairs));
        }
    }
    // Otherwise this is CRuby's `Enumerable#sort`, `to_a.sort`: the pairs
    // themselves are compared — with `Array#<=>`, which goes on to the
    // values when two keys compare equal (`{1 => "b", 1.0 => "a"}`), or
    // with the block — and a failed comparison names the pairs
    // ("comparison of Array with Array failed").
    let pairs: Vec<Value> = hash.iter().map(|(k, v)| Value::array2(k, v)).collect();
    let pairs = Array::new_from_vec(pairs);
    vm.with_temp_scope(|vm| {
        vm.temp_push(pairs.into());
        super::array::sort_inner(vm, globals, lfp, pairs)
    })
}

///
/// ### Hash#assoc
///
/// - assoc(key) -> [key, value] | nil
///
/// [https://docs.ruby-lang.org/ja/3.2/method/Hash/i/assoc.html]
#[monoruby_builtin]
fn assoc(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let key = lfp.arg(0);
    let hash = lfp.self_val().as_hash();
    if hash.len() == 0 {
        return Ok(Value::nil());
    }
    // `rb_hash_assoc`: a Hash whose table is an st one — more pairs than
    // an ar table holds — and not an identity table looks the key up
    // first: one `#hash` on the argument, and the pairs filed under it
    // compared with the argument's `==`. Found that way, the pair is
    // `[argument, stored value]`, as `rb_assoc_new(key, value)` makes it.
    // Only a miss scans (#2206).
    if hash.len() > AR_TABLE_MAX_SIZE && !hash.is_compare_by_identity() {
        if let Some(value) = hash.assoc_value(key, vm, globals)? {
            return Ok(Value::array2(key, value));
        }
    }
    // Walked (`Hashmap::walk`): a key's `==` may change the receiver.
    let mut found = Value::nil();
    hash.walk(vm, globals, |vm, globals, _, k, v| {
        // `rb_equal(argument, stored key)` (`assoc_i`): the argument is
        // the receiver of the `==`, and a key that *is* the argument is
        // found without a call.
        let hit = vm.rb_equal(globals, key, k)?;
        if hit {
            found = Value::array2(k, v);
        }
        Ok(hit)
    })?;
    Ok(found)
}

///
/// ### Hash#rassoc
///
/// - rassoc(value) -> [key, value] | nil
///
/// [https://docs.ruby-lang.org/ja/3.2/method/Hash/i/rassoc.html]
#[monoruby_builtin]
fn rassoc(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let value = lfp.arg(0);
    let hash = lfp.self_val().as_hash();
    // Walked (`Hashmap::walk`): a value's `==` may change the receiver.
    let mut found = Value::nil();
    hash.walk(vm, globals, |vm, globals, _, k, v| {
        // `rb_equal(argument, stored value)` (`rassoc_i`).
        let hit = vm.rb_equal(globals, value, v)?;
        if hit {
            found = Value::array2(k, v);
        }
        Ok(hit)
    })?;
    Ok(found)
}

///
/// ### Hash#invert
///
/// - invert -> Hash
///
/// [https://docs.ruby-lang.org/ja/3.2/method/Hash/i/invert.html]
#[monoruby_builtin]
fn invert(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    lfp.expect_no_block()?;
    let hash = lfp.self_val().as_hash();
    // Built the way `runtime::gen_hash_inner` builds a Hash literal: in a
    // bare `HashmapInner`, sized for the receiver up front as CRuby's
    // `rb_hash_new_with_size` sizes it, for as long as no Ruby code can
    // run. The map lives on the Rust stack, which the collector does not
    // scan, and it alone holds the frozen copy of an unfrozen String value
    // turned key; that is sound because `aset_native_key` stores a packed
    // key or a String of class String without a call, so nothing here
    // reaches a safepoint. The first value whose `#hash` / `#eql?` may
    // dispatch is declined, and from it on the result is built rooted
    // (`invert_rooted`). Building every result rooted cost ~45% on a
    // 1000-pair Hash (#2173): a call through `Hashmap::aset` and two
    // root-slot stores per pair.
    let mut map = HashmapInner::with_capacity(hash.len());
    for (stored, (k, v)) in hash.iter().enumerate() {
        if !map.aset_native_key(v, k, vm, globals)? {
            return invert_rooted(vm, globals, hash, map, stored);
        }
    }
    Ok(Value::hash_from_inner(map))
}

/// The rest of [`invert`], from the first pair whose new key may run Ruby
/// code: `map`, holding the `stored` pairs before it, becomes the result
/// Hash right away and stays on the temp stack while the others go in, so
/// that a collection inside a key's `#hash` / `#eql?` finds every pair
/// stored so far — the frozen copies of String keys included, which
/// nothing else holds.
///
/// From here the receiver is walked as `rb_hash_foreach(hash,
/// rb_hash_invert_i)` walks it (`Hashmap::walk`, which also keeps the
/// pair being stored rooted): the new key's `hash` / `eql?` may take
/// pairs out of the receiver (#2155). Nothing has run since the native
/// part read the first `stored` pairs, so the walk starts behind them.
#[inline(never)]
fn invert_rooted(
    vm: &mut Executor,
    globals: &mut Globals,
    hash: Hashmap,
    map: HashmapInner,
    stored: usize,
) -> Result<Value> {
    // Where the pair after the `stored` live ones stands: a position
    // counts the tombstones before it too.
    let start = if hash.entry_count() == hash.len() {
        stored
    } else {
        let mut live = 0;
        let mut pos = 0;
        while live < stored {
            live += hash.live_at(pos) as usize;
            pos += 1;
        }
        pos
    };
    vm.with_temp_scope(|vm| {
        let res = Value::hash_from_inner(map);
        vm.temp_push(res);
        let mut map = res.as_hash();
        hash.walk_from(start, vm, globals, |vm, globals, _, k, v| {
            map.aset(v, k, vm, globals)?;
            Ok(false)
        })?;
        Ok(res)
    })
}

///
/// ### Kernel#__tally_up (monoruby intrinsic)
///
/// - __tally_up(hash, key) -> Integer
///
/// One step of `Enumerable#tally` (CRuby's `tally_up`, an `st_update`):
/// count *key* once more in *hash*. The key object is stored as it is — a String
/// is not copied and frozen, which is `rb_hash_aset`'s doing — the
/// Hash's default is not consulted, and a count that is not an Integer
/// is a TypeError.
///
#[monoruby_builtin]
pub(super) fn tally_up(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    // `FIXNUM_MAX`: a count that has reached it fails `tally_up`'s
    // `Check_Type(tally, T_BIGNUM)`, as it does in CRuby.
    const FIXNUM_MAX: i64 = (1 << 62) - 1;
    let key = lfp.arg(1);
    let mut h = lfp.arg(0).as_hash_mut(&globals.store)?;
    // One probe, so that the key's `#hash` is sent once (#2265).
    let count = h.update(
        key,
        |v, globals| {
            Ok(match v {
                None => Value::integer(1),
                Some(v) => match v.unpack() {
                    RV::Fixnum(i) if i < FIXNUM_MAX => Value::integer(i + 1),
                    RV::BigInt(b) => Value::bigint(b + 1),
                    _ => {
                        return Err(MonorubyErr::wrong_argument_type(
                            &globals.store,
                            v,
                            "Integer",
                        ));
                    }
                },
            })
        },
        vm,
        globals,
    )?;
    Ok(count)
}

///
/// ### Hash#merge
///
/// - merge(*others) -> Hash
/// - merge(*others) {|key, self_val, other_val| ... } -> Hash
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/merge.html]
#[monoruby_builtin]
fn merge(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let h = lfp.self_val().dup().as_hash();
    // The dup is a fresh Hash referenced only by this Rust local; root it
    // across `#to_hash`, the block calls, and the Ruby `hash`/`eql?`
    // dispatches inside get/insert (contrast `merge!`, where `h` is the
    // frame-rooted receiver).
    vm.with_temp_scope(|vm| {
        vm.temp_push(h.into());
        let data = match lfp.block() {
            Some(block) => Some(vm.get_block_data(globals, block)?),
            None => None,
        };
        for arg in lfp.arg(0).as_array().iter() {
            let other_val = arg.coerce_to_hash(vm, globals)?;
            // What a non-Hash operand's `to_hash` answered is referenced
            // from nowhere else: root it while it is walked.
            vm.temp_push(other_val.as_val());
            update_pairs(h, other_val, data.as_ref(), vm, globals)?;
        }
        Ok(h.into())
    })
}

///
/// ### Hash#merge!
///
/// - merge!(*others) -> self
/// - merge!(*others) {|key, self_val, other_val| ... } -> self
/// - update(*others) -> self
/// - update(*others) {|key, self_val, other_val| ... } -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/merge=21.html]
#[monoruby_builtin]
fn merge_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let h = lfp.self_val().as_hash_mut(&globals.store)?;
    // What a non-Hash operand's `to_hash` answers is referenced from
    // nowhere else: it goes on the temp stack, or a collection in the
    // block or in a key's `hash` / `eql?` frees it.
    vm.with_temp_scope(|vm| {
        let data = match lfp.block() {
            Some(block) => Some(vm.get_block_data(globals, block)?),
            None => None,
        };
        for arg in lfp.arg(0).as_array().iter() {
            let other = arg.coerce_to_hash(vm, globals)?;
            vm.temp_push(other.as_val());
            update_pairs(h, other, data.as_ref(), vm, globals)?;
        }
        Ok(lfp.self_val())
    })
}

/// `rb_hash_update` for one operand: store the pairs of `other` into
/// `dst`, a key both have going in with what `block` answers for it (the
/// key, `dst`'s value, `other`'s value). Both must be rooted.
///
/// `other` is walked as `rb_hash_foreach(other, rb_hash_update_i /
/// rb_hash_update_block_i, hash)` walks it (`Hashmap::walk`): the block
/// and the keys' `hash` / `eql?` may delete from it or clear it, and a
/// new key in it raises.
fn update_pairs(
    mut dst: Hashmap,
    other: Hashmap,
    block: Option<&ProcData>,
    vm: &mut Executor,
    globals: &mut Globals,
) -> Result<()> {
    let Some(data) = block else {
        return aset_pairs(dst, other, vm, globals);
    };
    other.walk(vm, globals, |vm, globals, _, k, other_v| {
        let Some(self_v) = dst.get(k, vm, globals)? else {
            dst.aset(k, other_v, vm, globals)?;
            return Ok(false);
        };
        let v = vm.invoke_block(globals, data, &[k, self_v, other_v])?;
        // The block's answer is referenced from nowhere else until `aset`
        // (which runs the key's `hash` / `eql?`) has stored it.
        vm.with_temp_scope(|vm| {
            vm.temp_push(v);
            dst.aset(k, v, vm, globals)
        })?;
        Ok(false)
    })?;
    Ok(())
}

///
/// ### Hash#compare_by_identity
///
/// - compare_by_identity -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/compare_by_identity.html]
#[monoruby_builtin]
fn compare_by_identity(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    lfp.expect_no_block()?;
    let mut hash = lfp.self_val().as_hash();
    if hash.is_compare_by_identity() {
        return Ok(lfp.self_val());
    }
    lfp.self_val().ensure_not_frozen(&globals.store)?;
    if hash.iter_active() {
        return Err(MonorubyErr::runtimeerr(
            "compare_by_identity during iteration",
        ));
    }
    hash.compare_by_identity(vm, globals)?;
    Ok(lfp.self_val())
}

/// ### Hash#compare_by_identity?
#[monoruby_builtin]
fn compare_by_identity_(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    lfp.expect_no_block()?;
    Ok(Value::bool(
        lfp.self_val().as_hash().is_compare_by_identity(),
    ))
}

/// ### Hash#values_at
#[monoruby_builtin]
fn values_at(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let hash = lfp.self_val().as_hash();
    let keys = lfp.arg(0).as_array();
    // The values accumulate in a rooted Array: one the default proc made
    // without storing it is referenced from nowhere else, and the next
    // key's default proc may run a collection.
    vm.with_temp_scope(|vm| {
        let res = Value::array_with_capacity(keys.len());
        vm.temp_push(res);
        let mut res = res.as_array();
        for k in keys.iter() {
            let v = hash.index(vm, globals, *k)?;
            res.push(v);
        }
        Ok(res.as_val())
    })
}

/// ### Hash#to_h
#[monoruby_builtin]
fn to_h(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    if let Some(bh) = lfp.block() {
        let data = vm.get_block_data(globals, bh)?;
        let hash = lfp.self_val().as_hash();
        let pairs: Vec<(Value, Value)> = hash.iter().collect();
        // Accumulate into a rooted Ruby Hash (not a bare Rust RubyMap):
        // the entries come from the block's fresh return Arrays and must
        // survive the following block calls.
        vm.with_temp_scope(|vm| {
            vm.temp_push(Value::hash(RubyMap::default()));
            let map_idx = vm.temp_len() - 1;
            for (k, v) in pairs {
                let result = vm.invoke_block(globals, &data, &[k, v])?;
                let arr = result.expect_array_ty(globals)?;
                if arr.len() != 2 {
                    return Err(MonorubyErr::typeerr(
                        "wrong element type (expected array with 2 elements)",
                    ));
                }
                vm.temp_at(map_idx).as_hash().insert(arr[0], arr[1], vm, globals)?;
            }
            Ok(vm.temp_at(map_idx))
        })
    } else {
        Ok(lfp.self_val())
    }
}

// ENV object

/// Bumped every time monoruby writes to the process environment.
/// `Time`'s local-zone lookup watches it so it can skip re-reading `TZ`
/// (a `getenv` scan plus `tzset`, which re-opens the zone file) on every
/// conversion while nothing has changed.
static ENV_GENERATION: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);

/// Record a `setenv` / `unsetenv`.
fn env_did_change() {
    ENV_GENERATION.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
}

/// The current environment generation, for a cache that wants to know
/// whether a variable it read is still current.
pub(crate) fn env_generation() -> u64 {
    ENV_GENERATION.load(std::sync::atomic::Ordering::Relaxed)
}

/// `env_str_new` (`rb_external_str_new_with_enc`): an environment string
/// carries the *locale* encoding — also when its bytes are not valid in
/// it — except under a US-ASCII (or binary) locale, where one with a
/// non-ASCII byte is ASCII-8BIT (a UTF-8 value under the `C` locale is
/// the common case).
fn env_string_bytes(bytes: &[u8]) -> Value {
    let enc = super::encoding::locale_encoding();
    if env_is_binary(enc, bytes) {
        Value::bytes(bytes.to_vec())
    } else {
        Value::string_from_inner(RStringInner::from_encoding(bytes, enc))
    }
}

/// Whether [`env_string_bytes`] hands `bytes` out as ASCII-8BIT under
/// the locale encoding `enc`.
fn env_is_binary(enc: crate::value::Encoding, bytes: &[u8]) -> bool {
    use crate::value::Encoding;
    enc == Encoding::Ascii8 || (enc == Encoding::UsAscii && !bytes.is_ascii())
}

/// [`env_string_bytes`] for bytes already copied into a string of the
/// locale encoding.
fn env_string_from_inner(inner: RStringInner) -> Value {
    if env_is_binary(inner.encoding(), inner.as_bytes()) {
        Value::bytes(inner.as_bytes().to_vec())
    } else {
        Value::string_from_inner(inner)
    }
}

/// The bytes of a String `env_name` / `env_value` / `env_str` answered.
fn env_bytes(s: &Value) -> &[u8] {
    s.is_rstring_inner()
        .expect("an ENV name or value is a String")
        .as_bytes()
}

/// `StringValue`: `v` itself when it is a String, else what its `to_str`
/// answers (`TypeError` when it has none).
fn env_str(vm: &mut Executor, globals: &mut Globals, v: Value) -> Result<Value> {
    if v.is_rstring().is_some() {
        Ok(v)
    } else {
        Ok(v.coerce_to_rstring(vm, globals)?.into())
    }
}

/// CRuby's `get_env_cstr`: what makes a String unusable as the `what`
/// ("name" or "value") of an environment variable. The bytes go to
/// libc as they are, so the encoding has to be one where a NUL byte is
/// a NUL character, and there must be none.
fn env_check(s: Value, what: &str) -> Result<()> {
    let inner = s
        .is_rstring_inner()
        .expect("an ENV name or value is a String");
    let enc = inner.encoding();
    if !enc.is_ascii_compatible() {
        return Err(MonorubyErr::argumenterr(format!(
            "bad environment variable {what}: ASCII incompatible encoding: {}",
            enc.name()
        )));
    }
    if inner.as_bytes().contains(&0) {
        return Err(MonorubyErr::argumenterr(format!(
            "bad environment variable {what}: contains null byte"
        )));
    }
    Ok(())
}

/// CRuby's `env_name`: `v` as the String naming a variable. The caller
/// keeps it rooted if it goes on to run Ruby code.
fn env_name(vm: &mut Executor, globals: &mut Globals, v: Value) -> Result<Value> {
    let s = env_str(vm, globals, v)?;
    env_check(s, "name")?;
    Ok(s)
}

/// `getenv`: the variable `name` as the process environment has it
/// *now*, as the fresh frozen String `env_name_new` hands out —
/// transcoded to `Encoding.default_internal` when one is set.
///
/// ENV is not a cache: native code (a C extension, a library it calls)
/// changes the environment with `setenv(3)` / `unsetenv(3)` behind
/// monoruby's back, and CRuby's ENV asks libc on every access (#2109).
fn env_getenv(globals: &mut Globals, name: &[u8]) -> Option<Value> {
    let enc = super::encoding::locale_encoding();
    // Only the copy is made under the environment lock.
    let inner = crate::environ::get(name, |value| RStringInner::from_encoding(value, enc))?;
    Some(env_fresh_string(globals, env_string_from_inner(inner)))
}

/// `Errno::EINVAL` for a name `setenv(3)` / `unsetenv(3)` (`func`)
/// refuses: an empty one, or one with a `=` in it.
fn env_invalid_name(store: &Store, func: &str, name: &[u8]) -> MonorubyErr {
    let err = std::io::Error::from_raw_os_error(libc::EINVAL);
    MonorubyErr::from_io_err(
        store,
        &err,
        format!(
            "Invalid argument - {func}({})",
            String::from_utf8_lossy(name)
        ),
    )
}

/// CRuby's `env_aset`: set the variable `key` to `val`, or delete it
/// when `val` is nil. Answers the value stored, as a String.
///
/// Both arguments are converted first, then the name is checked, then
/// the value, and only then does `setenv` itself refuse an empty name
/// or one with a `=`.
fn env_aset(vm: &mut Executor, globals: &mut Globals, key: Value, val: Value) -> Result<Value> {
    if val.is_nil() {
        env_delete_name(vm, globals, key)?;
        return Ok(Value::nil());
    }
    vm.with_temp_scope(|vm| {
        // Either conversion may run Ruby code.
        let name = env_str(vm, globals, key)?;
        vm.temp_push(name);
        let value = env_str(vm, globals, val)?;
        vm.temp_push(value);
        env_check(name, "name")?;
        env_check(value, "value")?;
        let (name, bytes) = (env_bytes(&name), env_bytes(&value));
        if !crate::environ::is_settable_name(name) {
            return Err(env_invalid_name(&globals.store, "setenv", name));
        }
        crate::environ::set(name, bytes);
        env_did_change();
        Ok(value)
    })
}

/// CRuby's `env_delete`: unset the variable `key` names and answer the
/// value it had, `None` when it was not set.
///
/// Like CRuby, this asks `getenv(3)` and calls `unsetenv(3)` only when
/// it found something — and `getenv` can find a variable for a name
/// `unsetenv` rejects: Darwin's compares only up to a `=` (`"HOME=r"`
/// finds HOME), glibc's finds `"HOME=r"` in the entry `HOME=r=x`. Then
/// `unsetenv` fails, and CRuby raises its `EINVAL`.
fn env_delete_name(vm: &mut Executor, globals: &mut Globals, key: Value) -> Result<Option<Value>> {
    let name = env_name(vm, globals, key)?;
    let name = env_bytes(&name);
    let removed = env_getenv(globals, name);
    if removed.is_some() {
        if !crate::environ::is_settable_name(name) {
            return Err(env_invalid_name(&globals.store, "unsetenv", name));
        }
        crate::environ::unset(name);
        env_did_change();
    }
    Ok(removed)
}

/// [`env_delete_name`] for a name read out of the environment itself.
fn env_delete_bytes(name: &[u8]) {
    crate::environ::unset(name);
    env_did_change();
}

/// An Enumerator over ENV for a method called without its block, sized
/// by the number of variables there are now (`rb_env_size`).
fn env_to_sized_enum(
    vm: &mut Executor,
    globals: &Globals,
    method: &str,
    lfp: Lfp,
    pc: BytecodePtr,
) -> Result<Value> {
    let method = super::kernel::invoked_name(vm, globals, lfp.self_val(), IdentId::get_id(method));
    let size = Value::integer(crate::environ::count() as i64);
    vm.generate_enumerator_with_size(method, lfp.self_val(), lfp.iter().collect(), pc, Some(size))
}

/// `ENV.size` / `ENV.length`: the number of variables in the process
/// environment now.
#[monoruby_builtin]
fn env_size(_: &mut Executor, _: &mut Globals, _: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::integer(crate::environ::count() as i64))
}

/// `ENV.empty?`
#[monoruby_builtin]
fn env_empty(_: &mut Executor, _: &mut Globals, _: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::bool(crate::environ::is_empty()))
}

/// A String just built from the process environment, as ENV hands it
/// out: frozen (so a caller can never take it for the live variable),
/// and transcoded to `Encoding.default_internal` when one is set, as
/// `env_enc_str_new` does.
fn env_fresh_string(globals: &mut Globals, v: Value) -> Value {
    let target = env_internal_encoding(globals);
    env_fresh_string_in(globals, v, target)
}

/// The encoding ENV's strings are handed out in when it is not the
/// locale's: `Encoding.default_internal`, if one is set.
fn env_internal_encoding(globals: &mut Globals) -> Option<crate::value::Encoding> {
    let int = super::io::enc_default_internal_obj(globals)?;
    super::io::enc_obj_to_enum(globals, int)
}

/// [`env_fresh_string`] with [`env_internal_encoding`] already looked
/// up, for the readers that hand out every variable at once.
fn env_fresh_string_in(
    globals: &Globals,
    v: Value,
    target: Option<crate::value::Encoding>,
) -> Value {
    let mut out = v;
    if let Some(target) = target
        && let Some(inner) = v.is_rstring_inner()
        && target != inner.encoding()
    {
        // `rb_str_conv_enc`: a string that does not convert is handed
        // out unchanged, in its locale encoding, rather than raising.
        if let Ok(bytes) = crate::value::transcode::transcode_for_env(
            &globals.store,
            inner.as_bytes(),
            inner.encoding(),
            target,
        ) {
            out = Value::string_from_inner(RStringInner::from_encoding_scanned(&bytes, target));
        }
    }
    out.set_frozen();
    out
}

/// The whole environment, for the readers that hand out every variable:
/// the bytes copied out of `environ` in one go, turned into Strings one
/// variable at a time as the reader gets to it. ENV keeps no state of
/// its own: this copy, and `getenv` for what changed since it was taken
/// (`live_value`), are all a reader consults.
struct EnvReader {
    live: crate::environ::Snapshot,
    target: Option<crate::value::Encoding>,
    /// [`env_generation`] when the snapshot was taken.
    generation: u64,
}

impl EnvReader {
    fn new(globals: &mut Globals) -> Self {
        Self {
            generation: env_generation(),
            live: crate::environ::Snapshot::take(),
            target: env_internal_encoding(globals),
        }
    }

    fn len(&self) -> usize {
        self.live.len()
    }

    /// The bytes of the `i`th variable's name.
    fn name_bytes(&self, i: usize) -> &[u8] {
        self.live.get(i).0
    }

    /// The `i`th variable's name as ENV hands it out.
    fn name(&self, globals: &Globals, i: usize) -> Value {
        env_fresh_string_in(globals, env_string_bytes(self.live.get(i).0), self.target)
    }

    /// The `i`th variable's value as ENV hands it out.
    fn value(&self, globals: &Globals, i: usize) -> Value {
        env_fresh_string_in(globals, env_string_bytes(self.live.get(i).1), self.target)
    }

    /// The `i`th variable's value as the environment has it *now* —
    /// `None` when it has been unset since the snapshot. The readers
    /// CRuby writes as "the names, then `getenv` of each" (`select`,
    /// `delete_if` and their kin) see what the block did meanwhile.
    ///
    /// `getenv` is a scan of `environ`, so one per variable makes the
    /// walk quadratic; it is only made once ENV has been written to
    /// since the snapshot. (What native code changed during the walk
    /// without going through ENV is therefore not seen until the next
    /// call — the snapshot `each` works on has the same limit.)
    fn live_value(&self, globals: &Globals, i: usize) -> Option<Value> {
        if env_generation() == self.generation {
            return Some(self.value(globals, i));
        }
        let enc = super::encoding::locale_encoding();
        let inner = crate::environ::get(self.live.get(i).0, |value| {
            RStringInner::from_encoding(value, enc)
        })?;
        Some(env_fresh_string_in(
            globals,
            env_string_from_inner(inner),
            self.target,
        ))
    }
}

/// Every variable as a fresh `Hash` of fresh frozen Strings
/// (`env_to_hash`), built straight from the environment.
fn env_snapshot_hash(vm: &mut Executor, globals: &mut Globals) -> Result<Value> {
    let env = EnvReader::new(globals);
    vm.with_temp_scope(|vm| {
        vm.temp_push(Value::hash_with_capacity(env.len()));
        let idx = vm.temp_len() - 1;
        for i in 0..env.len() {
            let (k, v) = (env.name(globals, i), env.value(globals, i));
            vm.temp_at(idx).as_hash().insert(k, v, vm, globals)?;
        }
        Ok(vm.temp_at(idx))
    })
}

/// ###ENV.[]
/// - self[key] -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/ENV/s/=5b=5d.html]
#[monoruby_builtin]
fn env_index(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let name = env_name(vm, globals, lfp.arg(0))?;
    Ok(env_getenv(globals, env_bytes(&name)).unwrap_or_default())
}

///
/// ### ENV.fetch
///
/// - fetch(key) -> String
/// - fetch(key, default) -> String
/// - fetch(key) {|key| ... } -> String
///
/// The key is a name like any other given to ENV (`env_name`: `to_str`,
/// then an ArgumentError for a NUL or an ASCII-incompatible encoding).
/// The block and the `KeyError` get the name as the String it was
/// converted to.
///
/// [https://docs.ruby-lang.org/ja/latest/method/ENV/s/fetch.html]
#[monoruby_builtin]
fn env_fetch(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    if lfp.block().is_some() && lfp.try_arg(1).is_some() {
        // CRuby's rb_warn: caller-location prefix, straight to $stderr
        // (not the overridable Kernel#warn) — and before the name is
        // looked at.
        vm.ruby_warn_caller(globals, "warning: block supersedes default value argument")?;
    }
    let name = env_name(vm, globals, lfp.arg(0))?;
    if let Some(v) = env_getenv(globals, env_bytes(&name)) {
        return Ok(v);
    }
    if let Some(bh) = lfp.block() {
        // A converted name is a fresh String only this local holds.
        return vm.with_temp_scope(|vm| {
            vm.temp_push(name);
            vm.invoke_block_once(globals, bh, &[name])
        });
    }
    if let Some(default) = lfp.try_arg(1) {
        return Ok(default);
    }
    Err(MonorubyErr::keyerr_with(
        format!("key not found: {}", name.inspect(&globals.store)),
        lfp.self_val(),
        name,
    ))
}

///
/// ### ENV.to_hash
///
/// - to_hash -> Hash
///
/// A fresh `Hash` of the environment as it is now; changing it does not
/// change the environment.
///
/// [https://docs.ruby-lang.org/ja/latest/method/ENV/s/to_hash.html]
#[monoruby_builtin]
fn env_to_hash(vm: &mut Executor, globals: &mut Globals, _: Lfp, _: BytecodePtr) -> Result<Value> {
    env_snapshot_hash(vm, globals)
}

///
/// ### ENV.to_h
///
/// - to_h -> Hash
/// - to_h {|name, value| block } -> Hash
///
/// Like [`env_to_hash`]; with a block, each pair is replaced by the
/// 2-element Array the block answers for it, as `Hash#to_h` does —
/// otherwise an `ArgumentError` (wrong size) or `TypeError` is raised.
///
/// [https://docs.ruby-lang.org/ja/latest/method/ENV/s/to_h.html]
#[monoruby_builtin]
fn env_to_h(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let Some(bh) = lfp.block() else {
        return env_snapshot_hash(vm, globals);
    };
    let data = vm.get_block_data(globals, bh)?;
    let env = EnvReader::new(globals);
    // Accumulate into a rooted Ruby Hash (not a bare Rust RubyMap): the
    // entries come from the block's fresh return Arrays and must
    // survive the following block calls.
    vm.with_temp_scope(|vm| {
        vm.temp_push(Value::hash(RubyMap::default()));
        let map_idx = vm.temp_len() - 1;
        for i in 0..env.len() {
            let (k, v) = (env.name(globals, i), env.value(globals, i));
            let result = vm.invoke_block(globals, &data, &[k, v])?;
            // CRuby coerces the block's return value to an Array *only* via
            // `#to_ary` (never `#to_a`); anything else raises
            // `TypeError("wrong element type <Class> (expected array)")`.
            let Some(arr) = result.check_array_type(vm, globals)? else {
                return Err(MonorubyErr::typeerr(format!(
                    "wrong element type {} (expected array)",
                    result.get_real_class_name(&globals.store),
                )));
            };
            if arr.len() != 2 {
                return Err(MonorubyErr::argumenterr(format!(
                    "element has wrong array length (expected 2, was {})",
                    arr.len(),
                )));
            }
            // `insert` runs the key's `#hash` / `#eql?`, and nothing a
            // collection looks at holds the pair: the Array the block
            // answered is referenced from nowhere, and Ruby code there can
            // take the key and the value out of it anyway. Root the two.
            let (k, v) = (arr[0], arr[1]);
            vm.temp_push(k);
            vm.temp_push(v);
            vm.temp_at(map_idx).as_hash().insert(k, v, vm, globals)?;
            vm.temp_clear(map_idx + 1);
        }
        Ok(vm.temp_at(map_idx))
    })
}

///
/// ### ENV.[]=
/// ### ENV.store
///
/// - self[name] = value -> value
/// - store(name, value) -> value
///
/// Sets the environment variable named *name* to *value* with
/// `setenv(3)`, so that native code and child processes see it. If
/// *value* is `nil`, the variable is deleted. Raises `TypeError` when
/// *name* or *value* is not a String and does not respond to `#to_str`,
/// `ArgumentError` when either holds a NUL or is in an
/// ASCII-incompatible encoding, and `Errno::EINVAL` when *name* is
/// empty or contains `'='` — in that order (see [`env_aset`]). With a
/// `nil` *value* such a name is deleted as [`env_delete`] deletes it:
/// nothing happens unless `getenv(3)` finds a variable for it, and then
/// `unsetenv(3)`'s `Errno::EINVAL` is raised.
///
/// [https://docs.ruby-lang.org/ja/latest/method/ENV/s/=5b=5d=3d.html]
/// [https://docs.ruby-lang.org/ja/latest/method/ENV/s/store.html]
#[monoruby_builtin]
fn env_index_assign(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    env_aset(vm, globals, lfp.arg(0), lfp.arg(1))
}

///
/// ### ENV.delete
///
/// - delete(name) -> String | nil
/// - delete(name) {|name| block } -> object
///
/// Removes the environment variable named *name* (`unsetenv(3)`).
/// Returns the previous value, or the block's return value (if a
/// block is given and the variable was not set), or `nil`. Raises
/// `Errno::EINVAL` when `getenv(3)` finds a variable for a name
/// `unsetenv` rejects (see [`env_delete_name`]).
///
/// [https://docs.ruby-lang.org/ja/latest/method/ENV/s/delete.html]
#[monoruby_builtin]
fn env_delete(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let key = lfp.arg(0);
    match env_delete_name(vm, globals, key)? {
        Some(removed) => Ok(removed),
        // The block is given the name as the caller wrote it.
        None => match lfp.block() {
            Some(bh) => vm.invoke_block_once(globals, bh, &[key]),
            None => Ok(Value::nil()),
        },
    }
}

///
/// ### ENV.to_s
///
/// - to_s -> "ENV"
///
/// Returns the literal String `"ENV"`.
///
/// [https://docs.ruby-lang.org/ja/latest/method/ENV/s/to_s.html]
#[monoruby_builtin]
fn env_to_s(_: &mut Executor, _: &mut Globals, _: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::string("ENV".to_string()))
}

///
/// ### ENV.rehash
///
/// - rehash -> nil
///
/// Provided for compatibility with `Hash#rehash`. monoruby keeps ENV
/// in libc's `environ`, so there is no Ruby-side bucket to rebuild;
/// always returns `nil`.
///
/// [https://docs.ruby-lang.org/ja/latest/method/ENV/s/rehash.html]
#[monoruby_builtin]
fn env_rehash(_: &mut Executor, _: &mut Globals, _: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::nil())
}

///
/// ### ENV.assoc
///
/// - assoc(name) -> [name, value] | nil
///
/// Returns `[name, value]` if the variable is set, otherwise `nil`. The
/// pair holds the name it was *given* (as a String), next to a fresh
/// copy of the value.
///
/// [https://docs.ruby-lang.org/ja/latest/method/ENV/s/assoc.html]
#[monoruby_builtin]
fn env_assoc(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let name = env_name(vm, globals, lfp.arg(0))?;
    Ok(match env_getenv(globals, env_bytes(&name)) {
        Some(v) => Value::array2(name, v),
        None => Value::nil(),
    })
}

///
/// ### ENV.rassoc
///
/// - rassoc(value) -> [name, value] | nil
///
/// Returns `[name, value]` for the first variable whose value is byte
/// for byte *value*'s, or `nil` if none is — also when *value* does not
/// respond to `#to_str` (no `TypeError` is raised, per spec). A value no
/// variable can have (one with a NUL in it) simply matches nothing.
///
/// [https://docs.ruby-lang.org/ja/latest/method/ENV/s/rassoc.html]
#[monoruby_builtin]
fn env_rassoc(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let Some(value) = lfp.arg(0).check_string_type(vm, globals)? else {
        return Ok(Value::nil());
    };
    // `env_rassoc` is the odd one out: it builds the name with a plain
    // `rb_str_new` (so ASCII-8BIT, never transcoded) and pairs it with
    // the *searched-for* value, not a fresh copy.
    Ok(match crate::environ::name_of_value(env_bytes(&value)) {
        Some(name) => Value::array2(Value::bytes(name), value),
        None => Value::nil(),
    })
}

///
/// ### ENV.key
///
/// - key(value) -> String | nil
///
/// Coerces *value* with `#to_str` (raising `TypeError` if not
/// coercible) and returns the name of the first variable whose value
/// is byte for byte the same, or `nil` if none is.
///
/// [https://docs.ruby-lang.org/ja/latest/method/ENV/s/key.html]
#[monoruby_builtin]
fn env_key(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let value = env_str(vm, globals, lfp.arg(0))?;
    Ok(match crate::environ::name_of_value(env_bytes(&value)) {
        Some(name) => env_fresh_string(globals, env_string_bytes(&name)),
        None => Value::nil(),
    })
}

///
/// ### ENV.has_key?
/// ### ENV.include?
/// ### ENV.key?
/// ### ENV.member?
///
/// - has_key?(name) -> bool
/// - include?(name) -> bool
/// - key?(name) -> bool
/// - member?(name) -> bool
///
/// Returns whether the named variable is set.
///
/// [https://docs.ruby-lang.org/ja/latest/method/ENV/s/has_key=3f.html]
/// [https://docs.ruby-lang.org/ja/latest/method/ENV/s/include=3f.html]
/// [https://docs.ruby-lang.org/ja/latest/method/ENV/s/key=3f.html]
/// [https://docs.ruby-lang.org/ja/latest/method/ENV/s/member=3f.html]
#[monoruby_builtin]
fn env_has_key(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let name = env_name(vm, globals, lfp.arg(0))?;
    Ok(Value::bool(crate::environ::is_set(env_bytes(&name))))
}

///
/// ### ENV.has_value?
/// ### ENV.value?
///
/// - has_value?(value) -> bool | nil
/// - value?(value) -> bool | nil
///
/// Returns whether some variable's value is byte for byte *value*'s.
/// If *value* does not respond to `#to_str`, returns `nil` (no
/// `TypeError` is raised, per spec).
///
/// [https://docs.ruby-lang.org/ja/latest/method/ENV/s/has_value=3f.html]
/// [https://docs.ruby-lang.org/ja/latest/method/ENV/s/value=3f.html]
#[monoruby_builtin]
fn env_has_value(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let Some(value) = lfp.arg(0).check_string_type(vm, globals)? else {
        return Ok(Value::nil());
    };
    Ok(Value::bool(
        crate::environ::name_of_value(env_bytes(&value)).is_some(),
    ))
}

/// Unset every variable for which the block's answer is `remove_when`
/// (`delete_if` / `reject!` / `keep_if` / `select!`). As in CRuby, the
/// names are taken once and each one's value is read when its turn
/// comes, so a variable the block removed meanwhile is passed over.
///
/// Returns the number of variables removed.
fn env_remove_where(
    vm: &mut Executor,
    globals: &mut Globals,
    bh: BlockHandler,
    remove_when: bool,
) -> Result<usize> {
    let data = vm.get_block_data(globals, bh)?;
    let env = EnvReader::new(globals);
    let mut removed = 0;
    for i in 0..env.len() {
        let Some(v) = env.live_value(globals, i) else {
            continue;
        };
        let k = env.name(globals, i);
        if vm.invoke_block(globals, &data, &[k, v])?.as_bool() == remove_when {
            env_delete_bytes(env.name_bytes(i));
            removed += 1;
        }
    }
    Ok(removed)
}

///
/// ### ENV.delete_if
/// ### ENV.reject!
///
/// - delete_if {|name, value| ... } -> ENV
/// - reject! {|name, value| ... } -> ENV | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/ENV/s/delete_if.html]
#[monoruby_builtin]
fn env_delete_if(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    pc: BytecodePtr,
) -> Result<Value> {
    let Some(bh) = lfp.block() else {
        return env_to_sized_enum(vm, globals, "delete_if", lfp, pc);
    };
    env_remove_where(vm, globals, bh, true)?;
    Ok(lfp.self_val())
}

#[monoruby_builtin]
fn env_reject_bang(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    pc: BytecodePtr,
) -> Result<Value> {
    let Some(bh) = lfp.block() else {
        return env_to_sized_enum(vm, globals, "reject!", lfp, pc);
    };
    let removed = env_remove_where(vm, globals, bh, true)?;
    // `reject!` reports "nothing changed" with nil; `delete_if` always
    // answers ENV.
    Ok(if removed == 0 {
        Value::nil()
    } else {
        lfp.self_val()
    })
}

///
/// ### ENV.keep_if
/// ### ENV.select! / ENV.filter!
///
/// - keep_if {|name, value| ... } -> ENV
/// - select! {|name, value| ... } -> ENV | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/ENV/s/keep_if.html]
#[monoruby_builtin]
fn env_keep_if(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    pc: BytecodePtr,
) -> Result<Value> {
    let Some(bh) = lfp.block() else {
        return env_to_sized_enum(vm, globals, "keep_if", lfp, pc);
    };
    env_remove_where(vm, globals, bh, false)?;
    Ok(lfp.self_val())
}

#[monoruby_builtin]
fn env_select_bang(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    pc: BytecodePtr,
) -> Result<Value> {
    let Some(bh) = lfp.block() else {
        return env_to_sized_enum(vm, globals, "select!", lfp, pc);
    };
    let removed = env_remove_where(vm, globals, bh, false)?;
    Ok(if removed == 0 {
        Value::nil()
    } else {
        lfp.self_val()
    })
}

///
/// ### ENV.each / ENV.each_pair
///
/// - each {|name, value| ... } -> ENV
/// - each -> Enumerator
///
/// Yields each variable as one `[name, value]` Array of the frozen
/// copies `env_enc_str_new` builds, as `Hash#each` yields a pair. The
/// walk is over a snapshot — the block may change ENV as it goes.
///
/// [https://docs.ruby-lang.org/ja/latest/method/ENV/s/each.html]
#[monoruby_builtin]
fn env_each_pair(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    pc: BytecodePtr,
) -> Result<Value> {
    let Some(bh) = lfp.block() else {
        return env_to_sized_enum(vm, globals, "each_pair", lfp, pc);
    };
    let data = vm.get_block_data(globals, bh)?;
    // The snapshot is the copied-out bytes: a pair's Strings are made
    // as the block is about to see them.
    let env = EnvReader::new(globals);
    for i in 0..env.len() {
        let pair = Value::array2(env.name(globals, i), env.value(globals, i));
        vm.invoke_block(globals, &data, &[pair])?;
    }
    Ok(lfp.self_val())
}

///
/// ### ENV.each_key
/// ### ENV.each_value
///
/// - each_key {|name| ... } -> ENV
/// - each_value {|value| ... } -> ENV
///
/// [https://docs.ruby-lang.org/ja/latest/method/ENV/s/each_key.html]
#[monoruby_builtin]
fn env_each_key(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    pc: BytecodePtr,
) -> Result<Value> {
    env_each_half(vm, globals, lfp, pc, false, "each_key")
}

#[monoruby_builtin]
fn env_each_value(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    pc: BytecodePtr,
) -> Result<Value> {
    env_each_half(vm, globals, lfp, pc, true, "each_value")
}

fn env_each_half(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    pc: BytecodePtr,
    values: bool,
    name: &str,
) -> Result<Value> {
    let Some(bh) = lfp.block() else {
        return env_to_sized_enum(vm, globals, name, lfp, pc);
    };
    let data = vm.get_block_data(globals, bh)?;
    let env = EnvReader::new(globals);
    for i in 0..env.len() {
        let s = if values {
            env.value(globals, i)
        } else {
            env.name(globals, i)
        };
        vm.invoke_block(globals, &data, &[s])?;
    }
    Ok(lfp.self_val())
}

///
/// ### ENV.keys
/// ### ENV.values
///
/// - keys -> [String]
/// - values -> [String]
///
/// [https://docs.ruby-lang.org/ja/latest/method/ENV/s/keys.html]
#[monoruby_builtin]
fn env_keys(_: &mut Executor, globals: &mut Globals, _: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(env_half(globals, false))
}

#[monoruby_builtin]
fn env_values(_: &mut Executor, globals: &mut Globals, _: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(env_half(globals, true))
}

/// Every name, or every value: only the Strings asked for are built.
fn env_half(globals: &mut Globals, values: bool) -> Value {
    let env = EnvReader::new(globals);
    let out: Vec<Value> = (0..env.len())
        .map(|i| {
            if values {
                env.value(globals, i)
            } else {
                env.name(globals, i)
            }
        })
        .collect();
    Value::array_from_vec(out)
}

///
/// ### ENV.to_a
///
/// - to_a -> [[String, String]]
///
/// [https://docs.ruby-lang.org/ja/latest/method/ENV/s/to_a.html]
#[monoruby_builtin]
fn env_to_a(_: &mut Executor, globals: &mut Globals, _: Lfp, _: BytecodePtr) -> Result<Value> {
    let env = EnvReader::new(globals);
    let out: Vec<Value> = (0..env.len())
        .map(|i| Value::array2(env.name(globals, i), env.value(globals, i)))
        .collect();
    Ok(Value::array_from_vec(out))
}

///
/// ### ENV.invert
///
/// - invert -> Hash
///
/// [https://docs.ruby-lang.org/ja/latest/method/ENV/s/invert.html]
#[monoruby_builtin]
fn env_invert(vm: &mut Executor, globals: &mut Globals, _: Lfp, _: BytecodePtr) -> Result<Value> {
    let env = EnvReader::new(globals);
    vm.with_temp_scope(|vm| {
        vm.temp_push(Value::hash_with_capacity(env.len()));
        let idx = vm.temp_len() - 1;
        for i in 0..env.len() {
            let (k, v) = (env.name(globals, i), env.value(globals, i));
            vm.temp_at(idx).as_hash().insert(v, k, vm, globals)?;
        }
        Ok(vm.temp_at(idx))
    })
}

///
/// ### ENV.select / ENV.filter
///
/// - select {|name, value| ... } -> Hash
///
/// Unlike `select!` this leaves the environment alone and answers a
/// plain `Hash`. CRuby walks the names and reads each one's value when
/// its turn comes, so a value the block changed is the one collected.
///
/// [https://docs.ruby-lang.org/ja/latest/method/ENV/s/select.html]
#[monoruby_builtin]
fn env_select(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    pc: BytecodePtr,
) -> Result<Value> {
    let Some(bh) = lfp.block() else {
        return env_to_sized_enum(vm, globals, "select", lfp, pc);
    };
    let data = vm.get_block_data(globals, bh)?;
    let env = EnvReader::new(globals);
    vm.with_temp_scope(|vm| {
        vm.temp_push(Value::hash(RubyMap::default()));
        let idx = vm.temp_len() - 1;
        for i in 0..env.len() {
            let Some(v) = env.live_value(globals, i) else {
                continue;
            };
            let k = env.name(globals, i);
            // The pair outlives the block call only on the temp stack.
            vm.temp_push(k);
            vm.temp_push(v);
            if vm.invoke_block(globals, &data, &[k, v])?.as_bool() {
                vm.temp_at(idx).as_hash().insert(k, v, vm, globals)?;
            }
            vm.temp_clear(idx + 1);
        }
        Ok(vm.temp_at(idx))
    })
}

///
/// ### ENV.reject
///
/// - reject {|name, value| ... } -> Hash
///
/// `ENV.to_h` without the pairs the block is true for.
///
/// [https://docs.ruby-lang.org/ja/latest/method/ENV/s/reject.html]
#[monoruby_builtin]
fn env_reject(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    pc: BytecodePtr,
) -> Result<Value> {
    let Some(bh) = lfp.block() else {
        return env_to_sized_enum(vm, globals, "reject", lfp, pc);
    };
    let data = vm.get_block_data(globals, bh)?;
    let env = EnvReader::new(globals);
    vm.with_temp_scope(|vm| {
        vm.temp_push(Value::hash(RubyMap::default()));
        let idx = vm.temp_len() - 1;
        for i in 0..env.len() {
            let (k, v) = (env.name(globals, i), env.value(globals, i));
            vm.temp_push(k);
            vm.temp_push(v);
            if !vm.invoke_block(globals, &data, &[k, v])?.as_bool() {
                vm.temp_at(idx).as_hash().insert(k, v, vm, globals)?;
            }
            vm.temp_clear(idx + 1);
        }
        Ok(vm.temp_at(idx))
    })
}

///
/// ### ENV.clear
///
/// - clear -> ENV
///
/// [https://docs.ruby-lang.org/ja/latest/method/ENV/s/clear.html]
#[monoruby_builtin]
fn env_clear(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let live = crate::environ::Snapshot::take();
    for (name, _) in live.iter() {
        env_delete_bytes(name);
    }
    Ok(lfp.self_val())
}

///
/// ### ENV.shift
///
/// - shift -> [String, String] | nil
///
/// Removes the first variable of the environment and returns it.
///
/// [https://docs.ruby-lang.org/ja/latest/method/ENV/s/shift.html]
#[monoruby_builtin]
fn env_shift(_: &mut Executor, globals: &mut Globals, _: Lfp, _: BytecodePtr) -> Result<Value> {
    let mut first = None;
    crate::environ::walk(|name, value| {
        first = Some((name.to_vec(), value.to_vec()));
        false
    });
    let Some((name, value)) = first else {
        return Ok(Value::nil());
    };
    let target = env_internal_encoding(globals);
    let pair = Value::array2(
        env_fresh_string_in(globals, env_string_bytes(&name), target),
        env_fresh_string_in(globals, env_string_bytes(&value), target),
    );
    env_delete_bytes(&name);
    Ok(pair)
}

///
/// ### ENV.merge!
/// ### ENV.update
///
/// - merge!(*others) -> ENV
/// - merge!(*others) {|name, old_value, new_value| block } -> ENV
/// - update(*others) -> ENV
/// - update(*others) {|name, old_value, new_value| block } -> ENV
///
/// Stores each pair of each hash argument as `ENV[name] = value` does
/// ([`env_aset`]): a `nil` value deletes the variable. When a block is
/// given and the variable already exists, the block is invoked with
/// `(name, old_value, new_value)` and what it answers is stored
/// instead. Returns ENV.
///
/// [https://docs.ruby-lang.org/ja/latest/method/ENV/s/merge=21.html]
/// [https://docs.ruby-lang.org/ja/latest/method/ENV/s/update.html]
#[monoruby_builtin]
fn env_merge_bang(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let block_data = match lfp.block() {
        Some(bh) => Some(vm.get_block_data(globals, bh)?),
        None => None,
    };
    let env = lfp.self_val();
    for arg in lfp.arg(0).as_array().iter() {
        // `env_update`: ENV itself is skipped (#2157); its pairs are
        // already there, and a block would otherwise run for each one.
        if arg.id() == env.id() {
            continue;
        }
        let other = arg.coerce_to_hash(vm, globals)?;
        let pairs: Vec<(Value, Value)> = other.iter().collect();
        // The pairs wait on the temp stack until they are stored: what
        // a non-Hash's `to_hash` answered is referenced from nowhere
        // else, and the names' and values' `to_str` and the block run
        // Ruby that may empty a Hash the caller holds. A collection
        // there would free a pair this walk has yet to store or yield.
        vm.with_temp_scope(|vm| {
            for &(k, v) in &pairs {
                vm.temp_push(k);
                vm.temp_push(v);
            }
            for (k, mut v) in pairs {
                if let Some(ref data) = block_data {
                    // `env_update_block_i`: the current value is looked
                    // up as `ENV[name]` would, so the name is checked
                    // first — whatever the new value is.
                    let name = env_name(vm, globals, k)?;
                    if let Some(old) = env_getenv(globals, env_bytes(&name)) {
                        v = vm.with_temp_scope(|vm| {
                            vm.temp_push(name);
                            vm.temp_push(old);
                            vm.invoke_block(globals, data, &[k, old, v])
                        })?;
                    }
                }
                // The block's answer is only on the Rust stack while
                // the name is converted.
                vm.with_temp_scope(|vm| {
                    vm.temp_push(v);
                    env_aset(vm, globals, k, v)
                })?;
            }
            Ok(())
        })?;
    }
    Ok(lfp.self_val())
}

///
/// ### ENV.replace
///
/// - replace(other_hash) -> ENV
///
/// Replaces the contents of ENV with *other_hash*. Pairs are stored in
/// order as `ENV[name] = value` does ([`env_aset`]), except that each
/// name is converted and checked before its value is; an invalid one
/// stops the walk with the earlier pairs applied and nothing removed
/// yet, as in CRuby. Only after every pair is in are the variables the
/// Hash did not name removed. Returns ENV.
///
/// [https://docs.ruby-lang.org/ja/latest/method/ENV/s/replace.html]
#[monoruby_builtin]
fn env_replace(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    // `env_replace`: the names are taken before the argument is even
    // converted.
    let live = crate::environ::Snapshot::take();
    let mut stale: Vec<&[u8]> = live.iter().map(|(name, _)| name).collect();
    // `if (env == hash) return env;` (#2157).
    if lfp.arg(0).id() == lfp.self_val().id() {
        return Ok(lfp.self_val());
    }
    let other = lfp.arg(0).coerce_to_hash(vm, globals)?;
    let pairs: Vec<(Value, Value)> = other.iter().collect();
    // The pairs wait on the temp stack until they are stored, as in
    // `env_merge_bang`: the Hash may be a `to_hash` answer nothing else
    // references, or one the names' and values' `to_str` empty, and a
    // collection there would free a pair not stored yet.
    vm.with_temp_scope(|vm| {
        for &(k, v) in &pairs {
            vm.temp_push(k);
            vm.temp_push(v);
        }
        for (k, v) in pairs {
            // `env_replace_i`: the name is converted and checked before
            // the value is looked at, and it is the converted name that
            // `keylist_delete` takes off the stale list — so a key that
            // answers `to_str` keeps the variable it names. `env_aset`
            // roots the name while it converts the value.
            let name = env_name(vm, globals, k)?;
            env_aset(vm, globals, name, v)?;
            let name = env_bytes(&name);
            stale.retain(|s| *s != name);
        }
        Ok(())
    })?;
    for name in stale {
        env_delete_bytes(name);
    }
    Ok(lfp.self_val())
}

///
/// ### ENV.values_at
///
/// - values_at(*names) -> [String | nil]
///
/// Returns an Array of `ENV[name]` for the given *names*: a name that
/// is not set produces `nil` in the resulting array.
///
/// [https://docs.ruby-lang.org/ja/latest/method/ENV/s/values_at.html]
#[monoruby_builtin]
fn env_values_at(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let args = lfp.arg(0).as_array();
    // The fresh copies accumulate in a rooted Array: converting a later
    // name may run Ruby code, so a bare Rust Vec would not keep the
    // earlier ones alive.
    vm.with_temp_scope(|vm| {
        vm.temp_array_new(args.len());
        for k in args.iter() {
            let name = env_name(vm, globals, *k)?;
            let v = env_getenv(globals, env_bytes(&name)).unwrap_or_default();
            vm.temp_array_push(v);
        }
        Ok(vm.temp_pop())
    })
}

///
/// ### ENV.slice
///
/// - slice(*names) -> Hash
///
/// Returns a Hash containing the names that exist in ENV mapped to
/// their values. Each name argument is converted exactly once; the
/// resulting hash uses the *original* argument objects as keys (so a
/// mock that responds to `#to_str` is preserved as a key in the
/// result).
///
/// [https://docs.ruby-lang.org/ja/latest/method/ENV/s/slice.html]
#[monoruby_builtin]
fn env_slice(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let args = lfp.arg(0).as_array();
    vm.with_temp_scope(|vm| {
        vm.temp_push(Value::hash(RubyMap::default()));
        let idx = vm.temp_len() - 1;
        for k in args.iter() {
            let name = env_name(vm, globals, *k)?;
            if let Some(v) = env_getenv(globals, env_bytes(&name)) {
                // The result is keyed by the *given* name, which — as any
                // `Hash#[]=` does — is stored as a frozen copy. The fresh
                // value waits on the temp stack meanwhile: a key that is
                // not a String runs its own `hash` / `eql?` there.
                vm.temp_push(v);
                vm.temp_at(idx).as_hash().aset(*k, v, vm, globals)?;
                vm.temp_clear(idx + 1);
            }
        }
        Ok(vm.temp_at(idx))
    })
}

///
/// ### Hash#fetch
///
/// - fetch(key) -> object
/// - fetch(key, default) -> object
/// - fetch(key) {|key| ... } -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/ENV/s/fetch.html]
#[monoruby_builtin]
fn fetch(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let hash = lfp.self_val().as_hash();
    let arg0 = lfp.arg(0);
    let s = if let Some(bh) = lfp.block() {
        if lfp.try_arg(1).is_some() {
            let warn_id = IdentId::get_id("warn");
            let msg = Value::string_from_str("warning: block supersedes default value argument");
            vm.invoke_method_inner(globals, warn_id, lfp.self_val(), &[msg], None, None)?;
        }
        match hash.get(arg0, vm, globals)? {
            Some(v) => v,
            None => vm.invoke_block_once(globals, bh, &[arg0])?,
        }
    } else if let Some(arg1) = lfp.try_arg(1) {
        match hash.get(arg0, vm, globals)? {
            Some(v) => v,
            None => arg1,
        }
    } else {
        match hash.get(arg0, vm, globals)? {
            Some(v) => v,
            None => {
                return Err(MonorubyErr::keyerr_with(
                    format!("key not found: {}", arg0.inspect(&globals.store)),
                    lfp.self_val(),
                    arg0,
                ));
            }
        }
    };
    Ok(s)
}

///
/// ### Hash#shift
///
/// - shift -> [key, value] | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/shift.html]
#[monoruby_builtin]
fn shift(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let mut h = lfp.self_val().as_hash_mut(&globals.store)?;
    match h.shift(vm, globals)? {
        Some((k, v)) => Ok(Value::array2(k, v)),
        None => Ok(Value::nil()),
    }
}

///
/// ### Hash#key
///
/// - key(value) -> key | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/key.html]
#[monoruby_builtin]
fn key(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let hash = lfp.self_val().as_hash();
    let target = lfp.arg(0);
    // Walked (`Hashmap::walk`): a value's `==` may change the receiver.
    let mut found = Value::nil();
    hash.walk(vm, globals, |vm, globals, _, k, v| {
        // `rb_equal(stored value, argument)` (`key_i`).
        let hit = vm.rb_equal(globals, v, target)?;
        if hit {
            found = k;
        }
        Ok(hit)
    })?;
    Ok(found)
}

///
/// ### Hash#has_value?
///
/// - has_value?(value) -> bool
/// - value?(value) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/has_value=3f.html]
#[monoruby_builtin]
fn has_value(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let hash = lfp.self_val().as_hash();
    let target = lfp.arg(0);
    // Walked (`Hashmap::walk`): a value's `==` may change the receiver.
    let mut found = false;
    hash.walk(vm, globals, |vm, globals, _, _, v| {
        // `rb_equal(stored value, argument)` (`rb_hash_search_value`): the
        // stored value is the receiver of the `==`, and one that *is* the
        // argument answers true without it.
        found = vm.rb_equal(globals, v, target)?;
        Ok(found)
    })?;
    Ok(Value::bool(found))
}

///
/// ### Hash#keep_if
///
/// - keep_if {|key, value| ... } -> self
/// - keep_if -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/keep_if.html]
#[monoruby_builtin]
fn keep_if(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, pc: BytecodePtr) -> Result<Value> {
    let bh = match lfp.block() {
        None => {
            let id = IdentId::get_id("keep_if");
            return hash_to_sized_enum(vm, globals, id, lfp, pc);
        }
        Some(block) => block,
    };
    hash_delete_where(vm, globals, lfp, bh, false)?;
    Ok(lfp.self_val())
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    /// The probe's site-local last-key memo: run-time-built *frozen*
    /// keys hit it (same object per site), a rotating key set thrashes
    /// it harmlessly, a *mutable* key is never memoized — its in-place
    /// mutation must be re-digested (the staleness this design must
    /// never exhibit) — and an explicit GC between lookups exercises
    /// the key-word zeroing.
    #[test]
    fn hash_probe_key_memo() {
        run_test(
            r#"
        h = {}
        32.times { |i| h["key#{i}"] = i }
        frozen_keys = (0...32).map { |i| "key#{i}".freeze }
        acc = 0
        64.times do |r|
          acc += h[frozen_keys[r % 32]]
          acc += h[frozen_keys[5]]
        end
        mk = "key7".dup
        64.times { acc += h[mk].to_i }
        mk.sub!("7", "9")
        64.times { acc += h[mk].to_i }
        GC.start
        64.times { acc += h[frozen_keys[5]] }
        [acc, h[frozen_keys[31]], h[mk]]
        "#,
        );
    }

    /// `Hash#[]` with a compile-time key (`LinkMode::C`): a frozen
    /// String literal (`"..".freeze`), a folded String constant, and
    /// immediate literals bake their digest into the inline probe at
    /// JIT compile time. Pins hits, misses, the default fallback, and
    /// both map regimes (the hash grows across the linear→indexed
    /// boundary mid-test, so the baked digest must keep finding its
    /// key after rehashes).
    #[test]
    fn hash_index_baked_digest() {
        run_test_with_prelude(
            r#"
        h = {}
        acc = 0
        64.times do |i|
          h["k#{i}"] = i
          h[:sym] = i if i == 0
          h[7] = i + 100 if i == 0
          acc += h["k5".freeze].to_i
          acc += h[KEY].to_i
          acc += h[:sym] + h[7]
          acc += h["absent".freeze].to_i
          acc += h.fetch("k9".freeze, 0).to_i
        end
        d = Hash.new { |_, k| k.size }
        64.times { acc += d["dflt".freeze] }
        [acc, h["k5".freeze], h[KEY], h[:sym], h[7]]
        "#,
            r#"
        KEY = "k63"
        "#,
        );
    }

    /// A key that inherits the builtin identity `hash` skips the `#hash`
    /// dispatch and digests `id()` inline. The digest has to be the one the
    /// dispatch would have produced, and the fast path has to stop applying
    /// the moment the class gains a `hash` of its own — including after the
    /// hash already holds keys of that class.
    #[test]
    fn identity_hash_fast_path_agrees_with_dispatch() {
        run_test_once(
            r#"
            a = Object.new; b = Object.new
            h = { a => :a, b => :b }
            plain = [h[a], h[b], h[Object.new]]

            # A class with its own hash / eql? still keys by value.
            class IdHashK
              def initialize(v); @v = v; end
              def hash; @v.hash; end
              def eql?(o); o.is_a?(IdHashK) && o.instance_variable_get(:@v) == @v; end
            end
            by_value = [{ IdHashK.new(1) => :one }[IdHashK.new(1)],
                        { IdHashK.new(1) => :one }[IdHashK.new(2)]]

            # Defining `hash` later must retire the fast path for that class.
            class IdHashLate; end
            l = IdHashLate.new
            before = { l => :before }[l]
            class IdHashLate
              def hash; 12345; end
              def eql?(o); o.is_a?(IdHashLate); end
            end
            after = { IdHashLate.new => :after }[IdHashLate.new]

            # Set membership and Array#hash mix the same digest.
            require 'set'
            s = Set.new([a, b])
            mixed = [s.include?(a), s.include?(Object.new),
                     [a].hash == [a].hash, [a].hash == [b].hash]

            [plain, by_value, before, after, mixed]
            "#,
        );
    }

    #[test]
    fn small_hash_linear_scan() {
        // The small-hash fast paths (rubymap's AR mode and the inline
        // 2-pair representation in HashmapInner): immediate keys on small
        // tables answer by identity scan. Every rule they must preserve
        // is pinned against CRuby here.
        run_test_once(
            r#"
            r = []
            h = { 1 => :a, 5 => :b, 9 => :c }
            r << [h[1], h[9], h[7], h.key?(5), h.key?(7)]
            h2 = { a: 1, nil => 3, true => 4, false => 5, 1.5 => 6 }
            r << [h2[:a], h2[nil], h2[true], h2[false], h2[1.5], h2[:zz]]
            # Fixnum and Float keys never alias (eql? distinguishes)
            h3 = { 1 => :int, 1.0 => :float }
            r << [h3[1], h3[1.0]]
            # default value / default proc fire on a fast-path miss
            h5 = Hash.new(:dflt); h5[3] = :x
            r << [h5[3], h5[4]]
            h6 = Hash.new { |_, k| "miss#{k}" }; h6[1] = :hit
            r << [h6[1], h6[2]]
            # compare_by_identity: same-content distinct strings differ
            s1 = +"key"; s2 = +"key"
            h7 = {}.compare_by_identity
            h7[s1] = :one; h7[42] = :int
            r << [h7[s1], h7[s2], h7[42]]
            # growth across the boundary keeps hits and order; deletion
            # back under it re-enables the scan
            h8 = {}
            (1..12).each { |i| h8[i] = i * 10 }
            r << [h8[3], h8[11], h8.keys.first(3)]
            (6..12).each { |i| h8.delete(i) }
            r << [h8[3], h8[7], h8.size]
            # NaN key: identity semantics (same flonum bits hit)
            n = 0.0 / 0.0
            h9 = { n => :nan }
            r << [h9[n], h9[0.0 / 0.0]]
            r
            "#,
        );
        // Object keys keep the hashed path: #hash must still be
        // consulted (a custom hash/eql? pair keeps working).
        run_test_once(
            r#"
            class HKey
              attr_reader :h
              def initialize(h) = @h = h
              def hash = @h
              def eql?(o) = o.is_a?(HKey) && o.h == @h
            end
            k1 = HKey.new(42); k2 = HKey.new(42)
            { k1 => :obj }[k2]
            "#,
        );
    }

    /// Serializes ENV-mutating tests in this module. `setenv(3)` and
    /// `unsetenv(3)` are not thread-safe (they manipulate the process-
    /// wide `environ` array), so running ENV tests in parallel can race
    /// and SIGSEGV. Tests that touch ENV take this lock for their full
    /// body — including the `run_test_once` invocation, since CRuby
    /// reads `environ` from the same process via `run_ruby` -> exec
    /// before we mutate it back.
    static ENV_TEST_LOCK: std::sync::Mutex<()> = std::sync::Mutex::new(());

    /// Acquire the ENV test lock, recovering through poisoning so a
    /// failing test never wedges every other ENV test that follows it.
    fn env_lock() -> std::sync::MutexGuard<'static, ()> {
        ENV_TEST_LOCK
            .lock()
            .unwrap_or_else(|poison| poison.into_inner())
    }

    #[test]
    fn test_hash() {
        run_tests(&[
            r##"
        a = []
        h = Hash.new
        a << h.length
        h["Ruby"] = :ruby
        h[5] = 4.2
        h[:u] = "Ruby"
        a << h.length
        a << h[5]
        a << h[5.0]
        a << h["Ruby"]
        a << h.size
        a << h.keys
        a << h.values
        a << h.has_key?("ruby")
        a << h.include?(5.0)
        a << h.key?(5)
        a << h.member?(:u)
        a << h.inspect
        a << h.to_s
        a
        "##,
            "{}",
            r#"{1=>:ass, 4.5=>"Ruby", [1,2,3]=>{:f=>6}}"#,
            "{}.empty?",
            "{a:1}.empty?",
            // hash_splat
            r##"h = {a: 1}; {**h}"##,
            r##"h = {a: 1, b: 2}; {c: 3, **h}"##,
            r##"h1 = {a: 1}; h2 = {b: 2}; {**h1, **h2}"##,
            r##"h = {a: 1}; {a: 0, **h}"##,
            // clear
            r##"a = {a:1,b:2}; a.clear; a[:c] = 100; a"##,
            // transform_keys
            r##"{a: 1, b: 2}.transform_keys {|k| k.to_s}"##,
            r##"{a: 1, b: 2}.transform_keys {|k| k.to_s.upcase}"##,
            // transform_values
            r##"{a: 1, b: 2}.transform_values {|v| v * 10}"##,
            r##"{a: "x", b: "y"}.transform_values {|v| v.upcase}"##,
            // replace
            r##"
        a1 = {a:1,b:2}
        a2 = {c:3,d:4}
        z = a1.replace(a2)
        a1[:z] = 100
        [a1, a2, z]
        "##,
            // eq
            r##"{} == {}"##,
            r##"{a:4} == {a:4}"##,
            r##"{a:4} == {a:4.0}"##,
            r##"{a:4} == {a:5}"##,
            r##"{a:4} == {a:5, b:7}"##,
            r##"{a:4} == :a"##,
            // eq_recursive
            // Self-referencing hash: h == h should return true, not stack overflow
            "h = {}; h[:a] = h; h == h",
            // Two distinct recursive hashes with same structure
            "a = {}; a[:x] = a; b = {}; b[:x] = b; a == b",
            // Cross-recursive hashes: a contains b, b contains a
            "a = {}; b = {}; a[:x] = b; b[:x] = a; a == b",
            // Recursive hash with non-matching values
            "a = {x: 1}; a[:y] = a; b = {x: 2}; b[:y] = b; a == b",
            // Nested: array inside hash, hash inside array
            "h = {}; a = [h]; h[:a] = a; h == h",
        ]);
    }

    #[test]
    fn fetch() {
        run_test(
            r##"
        h = { one: nil }
        [h.fetch(:one), h.fetch(:two, "error")]
        "##,
        );
        run_test_error(
            r##"
        h = { one: nil }
        h.fetch(:two)
        "##,
        );
    }

    #[test]
    fn delete() {
        run_tests(&[
            r##"
        a = []
        h = {:ab => "some" , :cd => "all"}
        a << h.delete(:ab) #=> "some"
        a << h.delete(:ef) #=> nil
        a << h.delete(:ef){|key|"#{key} Nothing"} #=> "ef Nothing"
        a
        "##,
            // each
            r##"
        a = []
        {:a=>1, :b=>2, :c=>3}.each {|k, v|
            a << k
            a << v
        }
        a
        "##,
            r##"
        a = []
        {:a=>1, :b=>2, :c=>3}.each {|kv|
            a << kv
        }
        a
        "##,
            // map
            r##"
        {:a=>1, :b=>2, :c=>3}.collect {|k, v|
            k.to_s + v.to_s
        }
        "##,
            // each_value
            r##"
        a = []
        {:a=>1, :b=>2, :c=>3}.each_value {|v|
            a << v
        }
        a
        "##,
            r##"
        a = []
        {:a=>1, :b=>2, :c=>3}.each_key {|k|
            a << k
        }
        a
        "##,
            // select
            r##"
        res = []
        h = { "a" => 100, "b" => 200, "c" => 300 }
        res << h.select {|k,v| k > "a"}  #=> {"b" => 200, "c" => 300}
        res << h.select {|k,v| v < 200}  #=> {"a" => 100}
        res
        "##,
            r##"
        res = []
        h = { "a" => 100, "b" => 200, "c" => 300 }
        res << h.select! {|k,v| k > "a"}  #=> {"b" => 200, "c" => 300}
        res << h
        res
        "##,
            // assoc
            r##"{a: 1, b: 2, c: 3}.assoc(:a)"##,
            r##"{a: 1, b: 2, c: 3}.assoc(:b)"##,
            r##"{a: 1, b: 2, c: 3}.assoc(:z)"##,
            r##"{"a" => 1, "b" => 2}.assoc("b")"##,
            r##"{1 => :a, 1.0 => :b}.assoc(1)"##,
            // rassoc
            r##"{a: 1, b: 2, c: 3}.rassoc(1)"##,
            r##"{a: 1, b: 2, c: 3}.rassoc(2)"##,
            r##"{a: 1, b: 2, c: 3}.rassoc(9)"##,
            r##"{"a" => 1, "b" => 2}.rassoc(2)"##,
            // invert
            r##"
        {5 => "5", 1 => "1", 2 => "2", 3 => "3"}.invert
        "##,
            // sort
            r##"
        {5 => "5", 1 => "1", 2 => "2", 3 => "3"}.sort
        "##,
            // reject
            r##"
        h = { 2 =>"8", 4 =>"6", 6 =>"4", 8 =>"2" }
        h2 = h.reject{|key, value| key.to_i < value.to_i} #=> {6=>"4", 8=>"2"}
        [h, h2]
        "##,
        ]);
    }

    #[test]
    fn merge() {
        run_test_with_prelude(
            r##"
            [h1.merge, h1.merge(h2), h1.merge(h2, h3)]
        "##,
            r#"
            h1 = { "a" => 100, "b" => 200 }
            h2 = { "b" => 246, "c" => 300 }
            h3 = { "b" => 357, "d" => 400 }
            "#,
        );
        run_test(
            r#"
            foo = {1 => 'a', 2 => 'b', 3 => 'c'}
            bar = {2 => 'B', 3 => 'C', 4 => 'D'}
            res = []
            res << foo.update(bar)
            res << foo.update(bar) {|key, foo_val, bar_val| foo_val + bar_val }
            res << foo
            res
            "#,
        );
    }

    #[test]
    fn merge_() {
        run_test_with_prelude(
            r##"
        [h1.merge!, h1.update(h2), h1.merge!(h2, h3)]
        "##,
            r#"
            h1 = { "a" => 100, "b" => 200 }
            h2 = { "b" => 246, "c" => 300 }
            h3 = { "b" => 357, "d" => 400 }
            "#,
        );
    }

    #[test]
    fn compare_by_identity() {
        run_test_with_prelude(
            r##"
            [h1["a"], h1[75], h1[:c]]
        "##,
            r#"
            h1 = { "a" => 100, 75 => 200, :c => "c" }
            h1.compare_by_identity
            "#,
        );
    }

    #[test]
    fn to_h() {
        run_test(
            r##"
        hash = { "a" => 97, "b" => 98 }
        hash.to_h {|key, value| [key.upcase, value - 32] } # => {"A"=>65, "B"=>66}
        "##,
        );
    }

    #[test]
    fn env_fetch() {
        let _g = env_lock();
        //run_test(r##"ENV["PWD"]"##);
        //run_test(r##"ENV.fetch("PWD")"##);
        run_tests(&[
            r##"ENV.fetch("XZCDEWS", "ABC")"##,
            r##"ENV.fetch("XZCDEWS") {|key| key + "先生"}"##,
        ]);
        run_test_error(r##"ENV[100]"##);
    }

    #[test]
    fn env_fetch_coverage() {
        let _g = env_lock();
        // key coercion (String), default arg, block, and the TypeError /
        // KeyError branches of `env_fetch`.
        run_test_once(
            r##"(a=ENV.fetch("PATH").class; b=ENV.fetch("NO_SUCH_VAR_ZZ","def"); c=ENV.fetch("NO_SUCH_VAR_ZZ"){|k| "blk:#{k}"}; d=(begin; ENV.fetch(Object.new); rescue => e; e.class; end); f=(begin; ENV.fetch("NO_SUCH_VAR_ZZ"); rescue => e; e.class; end); [a,b,c,d,f])"##,
        );
    }

    #[test]
    fn env_to_h_block_coverage() {
        let _g = env_lock();
        // `env_to_hash` block form: #to_ary coercion is fine, a non-array
        // result raises TypeError, and a wrong-length array raises
        // ArgumentError.
        run_test_once(
            r##"(h=ENV.to_h{|k,v| [k.to_sym, v.length]}; a=h.keys.all?{|x| x.is_a?(Symbol)}; b=(begin; ENV.to_h{|k,v| "x"}; rescue => e; e.class; end); c=(begin; ENV.to_h{|k,v| [k]}; rescue => e; e.class; end); [a,b,c])"##,
        );
    }

    #[test]
    fn env_alias_and_copy_coverage() {
        let _g = env_lock();
        // `ENV[]` returns a frozen String, the alias identities hold, and
        // clone/dup/except take their dedicated paths.
        run_test_once(
            r##"(ENV["MONO_T_ZZ"]="hi"; a=ENV["MONO_T_ZZ"].frozen?; b=ENV.method(:has_key?)==ENV.method(:include?); c=ENV.method(:key?)==ENV.method(:include?); d=ENV.method(:member?)==ENV.method(:include?); e2=ENV.method(:has_value?)==ENV.method(:value?); f=(begin; ENV.clone(freeze:1); rescue => x; x.class; end); g=(begin; ENV.clone(foo:1); rescue => x; x.class; end); h=(begin; ENV.clone; rescue => x; x.class; end); i=(begin; ENV.dup; rescue => x; x.class; end); j=ENV.except("MONO_T_ZZ").class; k=ENV.except("MONO_T_ZZ").key?("MONO_T_ZZ"); ENV.delete("MONO_T_ZZ"); [a,b,c,d,e2,f,g,h,i,j,k])"##,
        );
    }

    #[test]
    fn env_index_assign_updates_hash() {
        let _g = env_lock();
        // Assignment is visible via ENV[]
        run_test_once(
            r##"
            ENV["MONORUBY_ENV_TEST_BASIC"] = "hello"
            v = ENV["MONORUBY_ENV_TEST_BASIC"]
            ENV["MONORUBY_ENV_TEST_BASIC"] = nil
            [v, ENV["MONORUBY_ENV_TEST_BASIC"]]
            "##,
        );
    }

    #[test]
    fn env_index_assign_delete_via_nil() {
        let _g = env_lock();
        run_test_once(
            r##"
            ENV["MONORUBY_ENV_TEST_DEL"] = "x"
            before = ENV["MONORUBY_ENV_TEST_DEL"]
            ENV["MONORUBY_ENV_TEST_DEL"] = nil
            [before, ENV["MONORUBY_ENV_TEST_DEL"]]
            "##,
        );
    }

    #[test]
    fn env_delete_method() {
        let _g = env_lock();
        run_test_once(
            r##"
            ENV["MONORUBY_ENV_TEST_DEL2"] = "y"
            a = ENV.delete("MONORUBY_ENV_TEST_DEL2")
            b = ENV.delete("MONORUBY_ENV_TEST_DEL2")
            c = ENV.delete("MONORUBY_ENV_TEST_DEL2") {|k| k + "!missing"}
            [a, b, c]
            "##,
        );
    }

    #[test]
    fn env_index_assign_type_errors() {
        let _g = env_lock();
        run_test_error(r##"ENV[100] = "x""##);
        run_test_error(r##"ENV["MONORUBY_ENV_TEST_BAD"] = 100"##);
    }

    #[test]
    fn env_index_assign_embedded_nul() {
        let _g = env_lock();
        run_test_error(r##"ENV["A\0B"] = "x""##);
        run_test_error(r##"ENV["MONORUBY_ENV_TEST_NUL"] = "a\0b""##);
    }

    /// What the process environment holds for `name`, read the way the
    /// interpreter reads it (and under the same lock, so that this
    /// module's tests do not race the other tests' interpreters).
    fn environ_get(name: &str) -> Option<Vec<u8>> {
        crate::environ::get(name.as_bytes(), |value| value.to_vec())
    }

    /// Verify that `ENV[]=` propagates to libc's `environ`, so that FFI
    /// callers of `getenv(3)` observe the value. This is what was
    /// previously broken.
    #[test]
    fn env_assign_propagates_to_libc_setenv() {
        let _g = env_lock();
        let key = "MONORUBY_ENV_TEST_LIBC_PROP";
        // Make sure the variable is not present before the test runs.
        crate::environ::unset(key.as_bytes());

        // Run a short Ruby script that sets the variable via `ENV[]=`.
        let mut globals = crate::Globals::new_test();
        let src = format!(r#"ENV["{key}"] = "hello""#);
        let _ = globals.run(src, std::path::Path::new("(test)"));

        // `getenv` should now see "hello".
        assert_eq!(environ_get(key).as_deref(), Some(&b"hello"[..]));

        // ENV[] = nil should remove it from libc as well.
        let src = format!(r#"ENV["{key}"] = nil"#);
        let _ = globals.run(src, std::path::Path::new("(test)"));
        assert_eq!(environ_get(key), None, "set after ENV[]=nil");
    }

    #[test]
    fn env_delete_propagates_to_libc_unsetenv() {
        let _g = env_lock();
        let key = "MONORUBY_ENV_TEST_LIBC_DEL";
        crate::environ::unset(key.as_bytes());

        let mut globals = crate::Globals::new_test();
        let src = format!(r#"ENV["{key}"] = "bye"; ENV.delete("{key}")"#);
        let _ = globals.run(src, std::path::Path::new("(test)"));
        assert_eq!(environ_get(key), None, "set after ENV.delete");
    }

    /// `delete_if` / `reject!` / `keep_if` / `select!` / `shift` must
    /// reach `environ` too: before these had ENV-specific bodies they ran
    /// `Hash`'s, so the variable left `ENV` while a child process still
    /// inherited it.
    #[test]
    fn env_destructive_methods_propagate_to_libc_unsetenv() {
        let _g = env_lock();
        let mut globals = crate::Globals::new_test();
        for (i, expr) in [
            r#"ENV.delete_if { |k, _| k == "{KEY}" }"#,
            r#"ENV.reject! { |k, _| k == "{KEY}" }"#,
            r#"ENV.keep_if { |k, _| k != "{KEY}" }"#,
            r#"ENV.select! { |k, _| k != "{KEY}" }"#,
            r#"ENV.filter! { |k, _| k != "{KEY}" }"#,
        ]
        .iter()
        .enumerate()
        {
            let key = format!("MONORUBY_ENV_TEST_DESTRUCTIVE_{i}");
            crate::environ::unset(key.as_bytes());
            let src = format!(r#"ENV["{key}"] = "v"; {}"#, expr.replace("{KEY}", &key));
            let _ = globals.run(src, std::path::Path::new("(test)"));
            assert_eq!(
                environ_get(&key),
                None,
                "{expr} left the variable in environ"
            );
        }
    }

    /// `ENV` is the process environment, not a copy of it taken at
    /// startup: a variable native code sets or removes behind Ruby's
    /// back is what every reader answers with (#2109). `crate::environ`
    /// stands in for the extension here; it serializes with the other
    /// tests' interpreters, which a bare `setenv(3)` from a test thread
    /// would not.
    #[test]
    fn env_reads_the_live_environment() {
        let _g = env_lock();
        const KEY: &str = "MONORUBY_ENV_TEST_NATIVE";
        let probe = format!(
            r##"
            k = "{KEY}"
            seen = []
            ENV.each_pair {{ |a, b| seen << b if a == k }}
            [
              ENV[k], ENV.fetch(k, :none), ENV.key?(k), ENV.assoc(k),
              ENV.values_at(k), ENV.slice(k), ENV.to_h[k], ENV.to_a.assoc(k),
              ENV.keys.include?(k), ENV.key("native 2109"),
              ENV.value?("native 2109"), ENV.rassoc("native 2109"),
              ENV.size == ENV.keys.size, ENV.empty?,
              ENV.inspect.include?(k), ENV.any? {{ |a, _| a == k }},
              ENV.count {{ |a, _| a == k }}, ENV.select {{ |a, _| a == k }},
              ENV.filter_map {{ |a, b| b if a == k }}, seen,
            ].inspect
            "##
        );
        let mut globals = crate::Globals::new_test();
        let mut run = |globals: &mut crate::Globals| {
            globals
                .run(probe.clone(), std::path::Path::new("(test)"))
                .unwrap()
                .as_str()
                .to_string()
        };
        crate::environ::unset(KEY.as_bytes());
        let absent = format!(
            "[nil, :none, false, nil, [nil], {{}}, nil, nil, false, nil, \
             false, nil, true, false, false, false, 0, {{}}, [], []]"
        );
        assert_eq!(run(&mut globals), absent);
        crate::environ::set(KEY.as_bytes(), b"native 2109");
        let v = r#""native 2109""#;
        let pair = format!(r#"["{KEY}", {v}]"#);
        let hash = format!(r#"{{"{KEY}" => {v}}}"#);
        assert_eq!(
            run(&mut globals),
            format!(
                "[{v}, {v}, true, {pair}, [{v}], {hash}, {v}, {pair}, true, \
                 \"{KEY}\", true, {pair}, true, false, true, true, 1, {hash}, \
                 [{v}], [{v}]]"
            )
        );
        // A value changed in place, then the variable removed again.
        crate::environ::set(KEY.as_bytes(), b"changed");
        let got = globals
            .run(
                format!(r#"[ENV["{KEY}"], ENV.to_h["{KEY}"], ENV.delete("{KEY}")].inspect"#),
                std::path::Path::new("(test)"),
            )
            .unwrap();
        assert_eq!(got.as_str(), r#"["changed", "changed", "changed"]"#);
        assert_eq!(environ_get(KEY), None);
        crate::environ::set(KEY.as_bytes(), b"native 2109");
        crate::environ::unset(KEY.as_bytes());
        assert_eq!(run(&mut globals), absent);
    }

    /// Methods `ENV` used to inherit from `Hash` answer for the live
    /// environment too, and the ones a Hash has but `ENV` must not act
    /// on (`freeze`) say so.
    #[test]
    fn env_hash_like_surface() {
        let _g = env_lock();
        run_test_once(
            r##"
            k = "MONORUBY_ENV_TEST_SURFACE"
            ENV[k] = "s"
            r = []
            r << (ENV.size == ENV.to_a.size) << (ENV.length == ENV.size) << ENV.empty?
            r << ENV.inspect.include?(%Q{"#{k}" => "s"})
            r << (ENV.map { |a, _| a }.include?(k))
            r << ENV.count { |a, b| a == k && b == "s" }
            r << ENV.min_by { |a, _| a == k ? 0 : 1 }
            r << ENV.each_with_object([]) { |(a, _), acc| acc << a if a == k }
            r << ENV.sort.assoc(k)
            r << ENV.all? { |a, b| a.is_a?(String) && b.is_a?(String) }
            r << (begin; ENV.freeze; rescue TypeError => e; e.message; end)
            r << ENV.frozen?
            ENV.delete(k)
            r << ENV.key?(k) << ENV.inspect.include?(k)
            r
            "##,
        );
    }

    /// Every ENV reader hands out a fresh *frozen* copy, so nothing a
    /// caller does to the result can reach the live environment.
    #[test]
    fn env_readers_return_frozen_copies() {
        let _g = env_lock();
        run_test_once(
            r##"
            k = "MONORUBY_ENV_TEST_FROZEN"
            ENV[k] = "uniq_value_4242"
            r = []
            r << ENV[k].frozen?
            r << ENV.fetch(k).frozen?
            r << ENV.keys.all?(&:frozen?)
            r << ENV.values.all?(&:frozen?)
            r << ENV.to_h.keys.all?(&:frozen?)
            r << ENV.to_a.flatten.all?(&:frozen?)
            r << ENV.select { true }.values.all?(&:frozen?)
            r << ENV.reject { false }.keys.all?(&:frozen?)
            r << ENV.invert.values.all?(&:frozen?)
            r << ENV.values_at(k).all?(&:frozen?)
            r << ENV.slice(k).values.all?(&:frozen?)
            r << ENV.key("uniq_value_4242").frozen?
            r << ENV.assoc(k)[1].frozen?
            ok = true
            ENV.each_pair { |a, b| ok &&= (a.frozen? && b.frozen?) }
            ENV.each_key { |a| ok &&= a.frozen? }
            ENV.each_value { |b| ok &&= b.frozen? }
            r << ok
            r << (ENV.select { |a, _| a == k } == { k => "uniq_value_4242" })
            r << (ENV.reject { |a, _| a != k } == { k => "uniq_value_4242" })
            r << ENV.select { true }.class
            r << ENV.each_pair {}.equal?(ENV)
            r << ENV.each_key {}.equal?(ENV)
            ENV.delete(k)
            r
            "##,
        );
    }

    /// `filter!` is the *same* method as `select!` (and `filter` as
    /// `select`), and the blockless forms answer an Enumerator whose
    /// `size` is ENV's size — CRuby's `enum_size`.
    #[test]
    fn env_aliases_and_enumerator_sizes() {
        let _g = env_lock();
        run_test_once(
            r##"
            n = ENV.size
            [
              ENV.method(:filter!) == ENV.method(:select!),
              ENV.method(:filter) == ENV.method(:select),
              [ENV.each_pair.size, ENV.each.size, ENV.each_key.size,
               ENV.each_value.size, ENV.select.size, ENV.reject.size,
               ENV.delete_if.size, ENV.keep_if.size, ENV.select!.size,
               ENV.reject!.size].uniq == [n],
              [ENV.each_pair.class, ENV.select.class, ENV.reject!.class].uniq,
            ]
            "##,
        );
    }

    /// Environment strings carry the *locale* encoding, and every reader
    /// transcodes to `Encoding.default_internal` when one is set
    /// (`env_str_new` / `env_enc_str_new`). Host-dependent: the locale is
    /// the host's, so this one always asks a live CRuby.
    #[test]
    fn env_strings_use_the_locale_encoding() {
        let _g = env_lock();
        run_test_once_live(
            r##"
            k = "MONORUBY_ENV_TEST_ENCODING"
            ENV[k] = "uniq_value_9876"
            e = ENV[k].encoding
            r = []
            r << [Encoding.find("locale"), Encoding::BINARY].include?(e)
            r << ([ENV.fetch(k).encoding, ENV.values_at(k)[0].encoding,
                   ENV.to_h[k].encoding, ENV.key("uniq_value_9876").encoding,
                   ENV.slice(k)[k].encoding, ENV.assoc(k)[1].encoding].uniq == [e])
            Encoding.default_internal = Encoding::IBM437
            # Only the test variable is touched here: transcoding *every*
            # variable would depend on what the host's environment holds.
            r << [ENV[k].encoding.to_s, ENV.fetch(k).encoding.to_s,
                  ENV.values_at(k)[0].encoding.to_s, ENV.slice(k)[k].encoding.to_s,
                  ENV.key("uniq_value_9876").encoding.to_s,
                  ENV.assoc(k)[1].encoding.to_s]
            Encoding.default_internal = nil
            ENV.delete(k)
            r
            "##,
        );
    }

    // -- ENV.[]= validation ------------------------------------------------

    /// Assigning a String value returns the *same* String object — the
    /// `ENV.send(:[]=, key, value).should equal(value)` ruby/spec.
    #[test]
    fn env_index_assign_returns_value_identity() {
        let _g = env_lock();
        run_test_once(
            r##"
            v = "MONORUBY_TEST_VAL"
            r = ENV.send(:[]=, "MONORUBY_ENV_TEST_ID", v)
            ENV.send(:[]=, "MONORUBY_ENV_TEST_ID", nil)
            r.equal?(v)
            "##,
        );
    }

    /// Empty key or a key containing '=' must raise Errno::EINVAL when
    /// the value is non-nil.
    #[test]
    fn env_index_assign_einval_for_invalid_keys() {
        let _g = env_lock();
        run_test_error(r##"ENV[""] = "x""##);
        run_test_error(r##"ENV["foo=bar"] = "x""##);
    }

    /// `ENV[invalid_key] = nil` is a silent no-op when `getenv` finds
    /// nothing for the name, as here (`a` is not set): it raises
    /// `unsetenv`'s EINVAL only when it does (`env_delete_name_with_equals`).
    #[test]
    fn env_index_assign_invalid_key_with_nil_is_noop() {
        let _g = env_lock();
        run_test_once(
            r##"
            ENV[""]      = nil
            ENV["a=b"]   = nil
            ENV.key?("") || ENV.key?("a=b")
            "##,
        );
    }

    // -- ENV.to_s / ENV.rehash --------------------------------------------

    #[test]
    fn env_to_s_returns_literal_env() {
        let _g = env_lock();
        run_test(r##"ENV.to_s"##);
    }

    #[test]
    fn env_rehash_returns_nil() {
        let _g = env_lock();
        run_test(r##"ENV.rehash"##);
    }

    // -- ENV.to_hash / ENV.to_h -------------------------------------------

    /// `ENV.to_h` and `ENV.to_hash` return a dup'd Hash, not ENV itself.
    #[test]
    fn env_to_h_returns_fresh_hash() {
        let _g = env_lock();
        run_tests(&[
            r##"ENV.to_h.equal?(ENV)"##,
            r##"ENV.to_hash.equal?(ENV)"##,
            r##"ENV.to_h.is_a?(Hash) && !ENV.to_h.equal?(ENV)"##,
        ]);
    }

    /// Block form transforms each pair into [k', v'].
    #[test]
    fn env_to_h_with_block() {
        let _g = env_lock();
        run_test_once(
            r##"
            ENV["MONORUBY_ENV_TEST_TOH"] = "1"
            h = ENV.to_h { |k, v| [k.downcase, v + "!"] }
            v = h["monoruby_env_test_toh"]
            ENV.delete("MONORUBY_ENV_TEST_TOH")
            v
            "##,
        );
    }

    /// Block must return a 2-element Array.
    #[test]
    fn env_to_h_block_size_error() {
        let _g = env_lock();
        run_test_error(r##"ENV.to_h { |k, v| [k] }"##);
    }

    // -- ENV.assoc / ENV.rassoc / ENV.key ---------------------------------

    #[test]
    fn env_assoc_basic() {
        let _g = env_lock();
        run_test_once(
            r##"
            ENV["MONORUBY_ENV_TEST_ASSOC"] = "yes"
            r = ENV.assoc("MONORUBY_ENV_TEST_ASSOC")
            ENV.delete("MONORUBY_ENV_TEST_ASSOC")
            [r, ENV.assoc("MONORUBY_ENV_TEST_ASSOC_NONE")]
            "##,
        );
    }

    /// `ENV.assoc` raises TypeError for a non-coercible argument.
    #[test]
    fn env_assoc_typeerror() {
        let _g = env_lock();
        run_test_error(r##"ENV.assoc(Object.new)"##);
    }

    /// `ENV.rassoc` returns nil (no TypeError) for a non-coercible value.
    #[test]
    fn env_rassoc_basic() {
        let _g = env_lock();
        run_test_once(
            r##"
            ENV["MONORUBY_ENV_TEST_RASSOC"] = "uniq_value_1234"
            r = ENV.rassoc("uniq_value_1234")
            n = ENV.rassoc("__no_such_value__")
            o = ENV.rassoc(Object.new)
            ENV.delete("MONORUBY_ENV_TEST_RASSOC")
            [r, n, o]
            "##,
        );
    }

    #[test]
    fn env_key_basic() {
        let _g = env_lock();
        run_test_once(
            r##"
            ENV["MONORUBY_ENV_TEST_KEY"] = "uniq_value_5678"
            r = ENV.key("uniq_value_5678")
            n = ENV.key("__no_such_value__")
            ENV.delete("MONORUBY_ENV_TEST_KEY")
            [r, n]
            "##,
        );
    }

    #[test]
    fn env_key_typeerror() {
        let _g = env_lock();
        run_test_error(r##"ENV.key(Object.new)"##);
    }

    // -- ENV.has_key? / include? / key? / member? -------------------------

    #[test]
    fn env_has_key_aliases() {
        let _g = env_lock();
        run_test_once(
            r##"
            ENV["MONORUBY_ENV_TEST_HK"] = "1"
            r = [
              ENV.has_key?("MONORUBY_ENV_TEST_HK"),
              ENV.include?("MONORUBY_ENV_TEST_HK"),
              ENV.key?("MONORUBY_ENV_TEST_HK"),
              ENV.member?("MONORUBY_ENV_TEST_HK"),
              ENV.has_key?("MONORUBY_ENV_TEST_HK_NONE"),
            ]
            ENV.delete("MONORUBY_ENV_TEST_HK")
            r
            "##,
        );
    }

    #[test]
    fn env_has_key_typeerror() {
        let _g = env_lock();
        run_test_error(r##"ENV.has_key?(Object.new)"##);
        run_test_error(r##"ENV.include?(Object.new)"##);
    }

    // -- ENV.has_value? / value? ------------------------------------------

    /// `ENV.has_value?` returns false for a missing String value, but
    /// returns nil (not TypeError) for a non-coercible argument.
    #[test]
    fn env_has_value_basic() {
        let _g = env_lock();
        run_test_once(
            r##"
            ENV["MONORUBY_ENV_TEST_HV"] = "uniq_v_9999"
            r = [
              ENV.has_value?("uniq_v_9999"),
              ENV.value?("uniq_v_9999"),
              ENV.has_value?("__no_such_value__"),
              ENV.has_value?(Object.new),
            ]
            ENV.delete("MONORUBY_ENV_TEST_HV")
            r
            "##,
        );
    }

    // -- ENV.merge! / ENV.update ------------------------------------------

    #[test]
    fn env_merge_bang_basic() {
        let _g = env_lock();
        run_test_once(
            r##"
            r = ENV.merge!("MONORUBY_ENV_TEST_M1" => "1",
                           "MONORUBY_ENV_TEST_M2" => "2")
            same = r.equal?(ENV)
            v = [ENV["MONORUBY_ENV_TEST_M1"], ENV["MONORUBY_ENV_TEST_M2"]]
            ENV.delete("MONORUBY_ENV_TEST_M1")
            ENV.delete("MONORUBY_ENV_TEST_M2")
            [same, v]
            "##,
        );
    }

    /// Block-form `merge!` is invoked only on collisions.
    #[test]
    fn env_merge_bang_block() {
        let _g = env_lock();
        run_test_once(
            r##"
            ENV["MONORUBY_ENV_TEST_BLK"] = "old"
            ENV.merge!("MONORUBY_ENV_TEST_BLK" => "new",
                       "MONORUBY_ENV_TEST_BLK_NEW" => "fresh") do
              |k, old, new| "#{old}+#{new}"
            end
            r = [ENV["MONORUBY_ENV_TEST_BLK"], ENV["MONORUBY_ENV_TEST_BLK_NEW"]]
            ENV.delete("MONORUBY_ENV_TEST_BLK")
            ENV.delete("MONORUBY_ENV_TEST_BLK_NEW")
            r
            "##,
        );
    }

    /// A `nil` value in the merged hash deletes the key from ENV and
    /// libc's `environ` (matching CRuby and `ENV[k] = nil`). yjit-bench
    /// relies on `ENV.merge!("GEM_HOME" => nil, "GEM_PATH" => nil)`
    /// at every benchmark's harness top — without this branch every
    /// monoruby benchmark aborted at load with TypeError.
    #[test]
    fn env_merge_bang_nil_deletes() {
        let _g = env_lock();
        run_test_once(
            r##"
            ENV["MONORUBY_ENV_TEST_MN1"] = "v1"
            ENV["MONORUBY_ENV_TEST_MN2"] = "v2"
            before = [ENV["MONORUBY_ENV_TEST_MN1"], ENV["MONORUBY_ENV_TEST_MN2"]]
            ENV.merge!("MONORUBY_ENV_TEST_MN1" => nil,
                       "MONORUBY_ENV_TEST_MN2" => nil,
                       "MONORUBY_ENV_TEST_MN3" => "new")
            after = [ENV["MONORUBY_ENV_TEST_MN1"],
                     ENV["MONORUBY_ENV_TEST_MN2"],
                     ENV["MONORUBY_ENV_TEST_MN3"]]
            ENV.delete("MONORUBY_ENV_TEST_MN3")
            [before, after]
            "##,
        );
    }

    /// `update` is an alias for `merge!`.
    #[test]
    fn env_update_alias() {
        let _g = env_lock();
        run_test_once(
            r##"
            ENV.update("MONORUBY_ENV_TEST_UP" => "u")
            v = ENV["MONORUBY_ENV_TEST_UP"]
            ENV.delete("MONORUBY_ENV_TEST_UP")
            v
            "##,
        );
    }

    /// A bad pair makes `merge!` raise without applying later good pairs.
    #[test]
    fn env_merge_bang_fails_fast() {
        let _g = env_lock();
        run_test_once(
            r##"
            ENV["MONORUBY_ENV_TEST_FF"] = "0"
            begin
              ENV.merge!({Object.new => "1", "MONORUBY_ENV_TEST_FF" => "2"})
            rescue TypeError
            end
            v = ENV["MONORUBY_ENV_TEST_FF"]
            ENV.delete("MONORUBY_ENV_TEST_FF")
            v
            "##,
        );
    }

    #[test]
    fn env_merge_bang_einval() {
        let _g = env_lock();
        run_test_error(r##"ENV.merge!("foo=" => "bar")"##);
        run_test_error(r##"ENV.merge!("" => "bar")"##);
    }

    // -- ENV.replace ------------------------------------------------------

    /// Successful `replace` removes the original-only keys and adopts
    /// the input pairs (the "replaces ENV with a Hash" spec).
    #[test]
    fn env_replace_clears_originals() {
        let _g = env_lock();
        run_test_once(
            r##"
            ENV["MONORUBY_ENV_TEST_REPL_OLD"] = "old"
            saved = ENV.to_hash
            r = ENV.replace({"MONORUBY_ENV_TEST_REPL_NEW" => "fresh"})
            same = r.equal?(ENV)
            v = [same,
                 ENV.key?("MONORUBY_ENV_TEST_REPL_OLD"),
                 ENV["MONORUBY_ENV_TEST_REPL_NEW"]]
            # Restore so the test does not pollute the harness env.
            ENV.replace(saved)
            ENV.delete("MONORUBY_ENV_TEST_REPL_OLD")
            ENV.delete("MONORUBY_ENV_TEST_REPL_NEW")
            v
            "##,
        );
    }

    /// When the bad pair comes first, `replace` raises before applying
    /// any pair — covering the "does not accept good data following an
    /// error" spec. We assert per-key (not whole-hash equality) because
    /// other parallel tests may legitimately mutate sibling ENV keys.
    #[test]
    fn env_replace_aborts_when_bad_pair_first() {
        let _g = env_lock();
        run_test_once(
            r##"
            ENV["MONORUBY_ENV_TEST_REPL2"] = "before"
            ENV.delete("MONORUBY_ENV_TEST_REPL2_NEW")
            begin
              ENV.replace({Object.new => Object.new,
                           "MONORUBY_ENV_TEST_REPL2_NEW" => "x",
                           "MONORUBY_ENV_TEST_REPL2"     => "after"})
            rescue TypeError
            end
            v = [ENV["MONORUBY_ENV_TEST_REPL2"],
                 ENV.key?("MONORUBY_ENV_TEST_REPL2_NEW")]
            ENV.delete("MONORUBY_ENV_TEST_REPL2")
            ENV.delete("MONORUBY_ENV_TEST_REPL2_NEW")
            v
            "##,
        );
    }

    #[test]
    fn env_replace_einval() {
        let _g = env_lock();
        run_test_error(r##"ENV.replace("=" => "bar")"##);
        run_test_error(r##"ENV.replace("" => "bar")"##);
    }

    #[test]
    fn env_replace_typeerror_argument() {
        let _g = env_lock();
        run_test_error(r##"ENV.replace(Object.new)"##);
    }

    // -- ENV.values_at / ENV.slice ----------------------------------------

    #[test]
    fn env_values_at_basic() {
        let _g = env_lock();
        run_test_once(
            r##"
            ENV["MONORUBY_ENV_TEST_VA1"] = "a"
            ENV["MONORUBY_ENV_TEST_VA2"] = "b"
            r = ENV.values_at("MONORUBY_ENV_TEST_VA1",
                              "MONORUBY_ENV_TEST_VA_NONE",
                              "MONORUBY_ENV_TEST_VA2")
            ENV.delete("MONORUBY_ENV_TEST_VA1")
            ENV.delete("MONORUBY_ENV_TEST_VA2")
            r
            "##,
        );
    }

    #[test]
    fn env_values_at_typeerror() {
        let _g = env_lock();
        run_test_error(r##"ENV.values_at("PWD", Object.new)"##);
    }

    #[test]
    fn env_slice_basic() {
        let _g = env_lock();
        run_test_once(
            r##"
            ENV["MONORUBY_ENV_TEST_SL1"] = "x"
            ENV["MONORUBY_ENV_TEST_SL2"] = "y"
            r = ENV.slice("MONORUBY_ENV_TEST_SL1",
                          "MONORUBY_ENV_TEST_SL_NONE",
                          "MONORUBY_ENV_TEST_SL2")
            ENV.delete("MONORUBY_ENV_TEST_SL1")
            ENV.delete("MONORUBY_ENV_TEST_SL2")
            r
            "##,
        );
    }

    #[test]
    fn env_slice_typeerror() {
        let _g = env_lock();
        run_test_error(r##"ENV.slice(Object.new)"##);
    }

    #[test]
    fn hash_inspect_recursive() {
        run_tests(&[
            r##"
        h = {}
        h[:self] = h
        h.inspect
        "##,
            // hash_replace_type_check
            r##"
        begin
          {}.replace(42)
          false
        rescue TypeError
          true
        end
        "##,
        ]);
    }

    #[test]
    fn hash_literal_chunked() {
        // A Hash literal longer than LITERAL_CHUNK_LEN (256 entries) is
        // built in chunks (issue #706). Cover insertion order, duplicate
        // keys across chunk boundaries (last wins), and a `**` splat
        // following the chunked part.
        let mut entries = String::new();
        for i in 0..600 {
            entries += &format!("{} => {}, ", i % 300, i);
        }
        run_test(&format!(
            "h = {{ {entries} }}; [h.size, h[0], h[299], h.keys[0], h.keys[-1]]"
        ));
        let mut entries = String::new();
        for i in 0..300 {
            entries += &format!("{i} => {i}, ");
        }
        run_test_once(&format!(
            "s = {{ 9999 => 1 }}; h = {{ {entries} **s }}; [h.size, h[9999], h.keys[-1]]"
        ));
    }

    #[test]
    fn hash_literal_huge() {
        // issue #706: a Hash literal with thousands of entries used to
        // allocate ~2 temporary registers per entry in a single frame and
        // overflow the native stack (frames live on the machine stack).
        let mut entries = String::new();
        for i in 0..4000 {
            entries += &format!("{i} => [0, 0, nil, nil, nil, {i}, nil], ");
        }
        run_test_once(&format!("h = {{ {entries} }}; [h.size, h[3999]]"));
    }

    #[test]
    fn hash_literal_error_propagation() {
        run_test_error(
            r##"
        class Foo
          def hash
            raise "boom"
          end
        end
        h = {Foo.new => 1}
        "##,
        );
    }

    #[test]
    fn hash_literal() {
        run_tests(&[
            r#"{1 => "a", 2 => "b", 3 => "c"}"#,
            // hash_tos_recursive
            // Same object appearing multiple times (not recursive)
            r#"a = [1]; {a:a, b:a}.to_s"#,
            r#"a = {a:1}; {a:a, b:a}.to_s"#,
            r#"a = {a:1}; {a:[a], b:a}.to_s"#,
            // Self-containing hash
            r##"
        h = {a: 1}
        h[:self] = h
        h.to_s
        "##,
            // hash_inspect_user_defined
            // User-defined inspect on custom objects inside hash values
            r##"
        class Bar
          def inspect
            "custom_bar"
          end
        end
        {a: Bar.new, b: 1}.inspect
        "##,
            // hash_inspect
            // Empty hash
            r#"{}.inspect"#,
            r#"{}.to_s"#,
            // Symbol keys
            r#"{a: 1, b: 2, c: 3}.inspect"#,
            // String keys
            r#"{"a" => 1, "b" => 2}.inspect"#,
            // Integer keys
            r#"{1 => "one", 2 => "two"}.inspect"#,
            // Mixed key types
            r#"{a: 1, "b" => 2, 3 => :three}.inspect"#,
            // Nested hash
            r#"{a: {b: {c: 1}}}.inspect"#,
            // Hash containing array
            r#"{a: [1, 2, 3], b: [4, 5]}.inspect"#,
            // Various value types
            r#"{a: nil, b: true, c: false, d: 1, e: 2.5, f: "str", g: :sym}.inspect"#,
            // Hash with Range values
            r#"{a: 1..5, b: 1...5}.inspect"#,
            // to_s is aliased to inspect
            r#"{a: 1}.to_s"#,
            // User-defined inspect in nested values
            r##"
        class MyVal
          def inspect
            "<val>"
          end
        end
        {a: MyVal.new, b: [MyVal.new]}.inspect
        "##,
            // User-defined inspect as keys
            r##"
        class MyKey
          def inspect
            "<key>"
          end
          def hash
            42
          end
          def eql?(other)
            true
          end
        end
        {MyKey.new => "value"}.inspect
        "##,
        ]);
    }

    #[test]
    fn hash_new() {
        run_tests(&[
            r##"
        h = Hash.new do |hash, key|
            hash[key] = "foo"
            "bar"
        end

        [h[:a], h[:a], h[:a]]
        "##,
            r##"
        res = []
        h = Hash.new("default")
        res << h.default
        res << h.default(:some)
        res << h
        h.default = "another default"
        res << h.default
        res
        "##,
            r##"
        res = []
        h = Hash.new{|hash, key| hash[key] ="default"}
        res << h.default
        res << h.default(:some)
        res << h
        res
        "##,
            r##"
        res = []
        h = Hash.new
        res << h.default
        res << h.default(:some)
        res << h
        res
        "##,
            r##"
        res = []
        h = Hash.new {|hash, key| "The #{key} not exist in #{hash.inspect}"}
        res << h.default
        res << h.default_proc.call({}, :foo)
        res << h
        res
        "##,
        ]);
    }

    #[test]
    fn shift() {
        run_tests(&[
            r##"
        h = {a: 1, b: 2, c: 3}
        res = []
        res << h.shift
        res << h
        res
        "##,
            r##"
        h = {}
        h.shift
        "##,
            r##"
        [Hash.new("default").shift, Hash.new.shift]
        "##,
        ]);
    }

    #[test]
    fn mutate_during_iteration() {
        run_tests(&[
            // Deleting the current key while iterating visits every original
            // entry and empties the hash (CRuby allows this).
            r##"
        h = { a: 1, b: 2, c: 3, d: 4, e: 5, f: 6, g: 7, h: 8, i: 9, j: 10,
              k: 11, l: 12, m: 13, n: 14, o: 15, p: 16, q: 17, r: 18, s: 19, t: 20 }
        visited = []
        h.each_pair { |k, v| visited << k; h.delete(k) }
        [visited, h]
        "##,
            // Shifting entries while iterating is likewise allowed.
            r##"
        h = { a: 1, b: 2, c: 3 }
        visited = []
        shifted = []
        h.each_pair { |k, v| visited << k; shifted << h.shift }
        [visited, shifted, h]
        "##,
            // Adding a new key mid-iteration still raises.
            r##"
        h = { a: 1 }
        begin
          h.each { |k, v| h[:b] = 2 }
        rescue RuntimeError
          :raised
        end
        "##,
        ]);
    }

    #[test]
    fn hash_compare() {
        run_tests(&[
            // <
            r#"{a: 1} < {a: 1, b: 2}"#,
            r#"{a: 1, b: 2} < {a: 1, b: 2}"#,
            r#"{a: 1, b: 2} < {a: 1}"#,
            r#"{} < {a: 1}"#,
            r#"{} < {}"#,
            // <=
            r#"{a: 1} <= {a: 1, b: 2}"#,
            r#"{a: 1, b: 2} <= {a: 1, b: 2}"#,
            r#"{a: 1, b: 2} <= {a: 1}"#,
            r#"{} <= {}"#,
            // >
            r#"{a: 1, b: 2} > {a: 1}"#,
            r#"{a: 1, b: 2} > {a: 1, b: 2}"#,
            r#"{a: 1} > {a: 1, b: 2}"#,
            r#"{a: 1} > {}"#,
            r#"{} > {}"#,
            // >=
            r#"{a: 1, b: 2} >= {a: 1}"#,
            r#"{a: 1, b: 2} >= {a: 1, b: 2}"#,
            r#"{a: 1} >= {a: 1, b: 2}"#,
            r#"{} >= {}"#,
            // different values
            r#"{a: 1} < {a: 2, b: 2}"#,
            r#"{a: 1} <= {a: 2}"#,
            // hash_delete_if
            r##"
        h = {a: 1, b: 2, c: 3}
        res = h.delete_if {|k, v| v > 1}
        [h, res.equal?(h)]
        "##,
            // key
            r##"
        h = {a: 1, b: 2, c: 3}
        [h.key(2), h.key(4)]
        "##,
            // hash_reject_bang
            r##"
        h = {a: 1, b: 2, c: 3}
        res1 = h.reject! {|k, v| v > 1}
        h2 = {a: 1}
        res2 = h2.reject! {|k, v| v > 10}
        [h, res1.equal?(h), res2]
        "##,
            // keep_if
            r##"
        h = {a: 1, b: 2, c: 3}
        res = h.keep_if {|k, v| v > 1}
        [res, h, res.equal?(h)]
        "##,
            // hash_bracket
            r#"Hash[]"#,
            r#"Hash["a", 1, "b", 2]"#,
            r#"Hash[{a: 1, b: 2}]"#,
            r#"Hash[["a", 1], ["b", 2]]"#,
            r##"
        h = Hash["a", 1, "b", 2, "c", 3]
        [h["a"], h["b"], h["c"]]
        "##,
            // index_splat
            r#"a = ["a", 1, "b", 2]; Hash[*a]"#,
            r##"
        args = ["a", 1, "b", 2, "c", 3]
        h = Hash[*args]
        [h["a"], h["b"], h["c"]]
        "##,
            r#"a = [1, 2, 3]; [*a]"#,
            // compare_by_identity_q
            "h = {}; h.compare_by_identity?",
            "h = {}; h.compare_by_identity; h.compare_by_identity?",
            // values_at
            r#"h = {a: 1, b: 2, c: 3}; h.values_at(:a, :c)"#,
            r#"h = {a: 1, b: 2}; h.values_at(:a, :x, :b)"#,
            // dig
            r#"h = {a: {b: {c: 1}}}; h.dig(:a, :b, :c)"#,
            r#"h = {a: {b: 1}}; h.dig(:a, :x)"#,
            r#"h = {a: 1}; h.dig(:a)"#,
        ]);
    }

    #[test]
    fn dig_errors() {
        // no arguments
        run_test_error("h = {a: 1}; h.dig");
    }

    #[test]
    fn to_h_and_try_convert() {
        run_tests(&[
            // to_h2
            "h = {a: 1, b: 2}; h.to_h == h",
            // to_h_with_block
            "{a: 1, b: 2}.to_h {|k, v| [k, v.to_s] }",
            // try_convert
            "Hash.try_convert({a: 1})",
            "Hash.try_convert(1)",
            "Hash.try_convert(nil)",
        ]);
    }

    #[test]
    fn hash_implicit_conversions() {
        // Hash#merge with to_hash
        run_test_with_prelude(
            "{a: 1}.merge(o)",
            "class C; def to_hash; {b: 2}; end; end; o = C.new",
        );
        // Hash#< with to_hash
        run_test_with_prelude(
            "{a: 1} < o",
            "class C; def to_hash; {a: 1, b: 2}; end; end; o = C.new",
        );
    }

    #[test]
    fn hash_iter_guard_new_key_raises() {
        // Adding a brand-new key during iteration must raise RuntimeError.
        run_test_error("h = {a: 1, b: 2}; h.each { h[:c] = 3 }");
        run_test_error("h = {a: 1}; h.each_key { h[:new] = 0 }");
        run_test_error("h = {a: 1}; h.each_value { h[:new] = 0 }");
    }

    #[test]
    fn hash_iter_guard_allowed() {
        run_tests(&[
            // Updating an already-present key during iteration is allowed,
            // matching CRuby semantics.
            "h = {a: 1, b: 2}; h.each { |k, v| h[k] = v * 10 }; h.to_a.sort",
            // Hash#delete during iteration does NOT raise (CRuby-compatible).
            // Exact visitation order is implementation-defined, so just check
            // that the call succeeds and returns the pre-delete value.
            "h = {a: 1}; \
             seen = nil; \
             h.each { |k, v| seen = h.delete(k) }; \
             [seen, h.empty?]",
        ]);
    }

    #[test]
    fn hash_iter_guard_clear() {
        // `clear` during a walk is a deletion of every pair, which CRuby
        // allows as it allows `delete`: the walk ends there.
        run_test("h = {a: 1, b: 2}; r = []; h.each { |k, _| r << k; h.clear }; [r, h]");
    }

    #[test]
    fn hash_iter_guard_lev() {
        run_tests(&[
            // Nested iteration increments iter_lev twice and decrements back to 0;
            // after all iterations complete, mutation is allowed again.
            "h = {a: 1, b: 2}; \
             h.each { |k1, _| h.each { |k2, _| _ = [k1, k2] } }; \
             h[:c] = 3; h.keys.sort",
            // If the each block raises, the iter_lev guard is still decremented
            // (RAII Drop) so subsequent mutations succeed.
            "h = {a: 1}; \
             begin; h.each { raise 'stop' }; rescue; end; \
             h[:b] = 2; h.keys.sort",
            // Deep nesting on a small (inline) hash: the depth counter
            // saturates and un-saturates soundly across many levels.
            "h = {a: 1}; r = 0; \
             h.each { h.each { h.each { h.each { h.each { r += 1 } } } } }; \
             h[:b] = 2; [r, h.keys.sort]",
        ]);
    }

    /// An explicitly passed mapping goes through implicit to_hash
    /// conversion (TypeError for nil / non-hash); an absent one returns
    /// the Enumerator.
    #[test]
    fn transform_keys_argument_validation() {
        run_tests(&[
            "{a: 1}.transform_keys.class.to_s",
            "{a: 1, b: 2}.transform_keys({a: :A}) { |k| k.to_s }",
            r##"
            conv = Object.new
            def conv.to_hash = { a: :z }
            {a: 1}.transform_keys(conv)
            "##,
            "h = {a: 1, b: 2}; h.transform_keys!({a: :A}); h",
        ]);
        run_test_error("{a: 1}.transform_keys(nil)");
        run_test_error("{a: 1}.transform_keys!(nil)");
        run_test_error("{a: 1}.transform_keys(42)");
    }

    /// A singleton / redefined #hash on an Array or Hash key is
    /// dispatched (exactly once per probe) instead of the native
    /// structural digest; plain container keys stay native.
    #[test]
    fn container_key_hash_dispatch() {
        run_tests(&[
            r##"
            calls = 0
            k = ["x"]
            k.define_singleton_method(:hash) { calls += 1; 0 }
            h = {}
            h[k] = 1
            [h[k], calls >= 2, h.size]
            "##,
            // a plain Hash subclass inherits Hash#hash and digests
            // structurally, so it works as a key interchangeably
            r##"
            sub = Class.new(Hash)
            k = sub[[[:a, 1]]]
            [k.hash == {a: 1}.hash, { {a: 1} => :x }[k]]
            "##,
        ]);
    }

    /// `Hash#replace` transfers the compare_by_identity mode in both
    /// directions — including for small (inline-representation) hashes,
    /// whose mode bit lives in the header flags byte and must travel
    /// with the replacement (ruby/spec core/hash/replace_spec.rb).
    #[test]
    fn hash_replace_compare_by_identity() {
        run_tests(&[
            "h = { a: 1, c: 3 }; \
             h.replace({ b: 2, d: 4 }.compare_by_identity); \
             h.compare_by_identity?",
            "h = { a: 1, c: 3 }.compare_by_identity; \
             h.replace(b: 2, d: 4); \
             h.compare_by_identity?",
            // identity lookups keep working through the transfer
            "s = +'k'; src = {}.compare_by_identity; src[s] = 1; \
             h = {}; h.replace(src); [h.compare_by_identity?, h[s], h[+'k']]",
        ]);
    }

    /// `h.default = x` inside `each` is legal (it does not change the key
    /// set) but forces a small hash out of its inline representation
    /// mid-iteration; the live iteration count must survive the move, so
    /// a new-key insert still raises inside the block and mutation is
    /// allowed again afterwards.
    #[test]
    fn hash_default_set_during_iteration() {
        run_tests(&[
            "h = {a: 1}; \
             h.each { h.default = 5 }; \
             h[:b] = 2; [h[:zz], h.keys.sort]",
            r##"
            h = {a: 1}
            err = nil
            h.each do
              h.default = 5
              begin
                h[:new_key] = 1
              rescue RuntimeError => e
                err = e.message
              end
            end
            [err, h[:miss], h.size]
            "##,
        ]);
    }

    // ----- Tests for ruby/spec sweep (PR #361) -----

    #[test]
    fn hash_new_validation() {
        run_tests(&[
            // 0..1 positional args ok
            "Hash.new.default",
            "Hash.new(5).default",
            // Block-form ok
            "Hash.new { |h, k| k }.default_proc.is_a?(Proc)",
        ]);
        // Both default value and block: ArgumentError
        run_test_error("Hash.new(5) { 0 }");
        // More than one positional: ArgumentError
        run_test_error("Hash.new(5, 6)");
    }

    #[test]
    fn hash_new_capacity_keyword() {
        run_tests(&[
            // `capacity:` (Ruby 3.4+) is accepted and ignored.
            "Hash.new(capacity: 42).default.inspect",
            "Hash.new(5, capacity: 42).default",
            "(Hash.new(capacity: 42) { 1 }).default_proc.is_a?(Proc)",
            "Hash.new(capacity: -42).default.inspect",
            // A braced Hash stays a positional default, not keywords.
            "Hash.new({ foo: 1 }).default",
        ]);
        // Any other keyword is an ArgumentError.
        run_test_error("Hash.new(unknown: true)");
        run_test_error("Hash.new(1, unknown: true)");
        run_test_error("Hash.new(unknown: true) { 0 }");
    }

    #[test]
    fn hash_initialize_private() {
        run_tests(&[
            r#"Hash.private_instance_methods.include?(:initialize)"#,
            // Reset default value
            r#"h = {}; h.default = 42; h.send(:initialize, 1); h.default"#,
            r#"h = {}; h.default = 42; h.send(:initialize); h.default.nil?"#,
            // Reset default_proc
            r#"h = {}; h.send(:initialize) { |_, k| k * 2 }; h["a"]"#,
            // Returns self
            r#"h = Hash.new; h.send(:initialize).equal?(h)"#,
        ]);
        // FrozenError on a frozen hash
        run_test_error(r#"{}.freeze.send(:initialize)"#);
        run_test_error(r#"{}.freeze.send(:initialize, 5)"#);
        run_test_error(r#"{}.freeze.send(:initialize) { 5 }"#);
    }

    #[test]
    fn hash_initialize_subclass_args() {
        // Hash.new must forward *args + block to the subclass's #initialize.
        run_test(
            r#"
            klass = Class.new(Hash) do
              def initialize(*args)
                args.each_with_index { |v, i| self[i] = v }
              end
            end
            h = klass.new(:one, :two)
            [h[0], h[1], h.class.superclass]
            "#,
        );
    }

    #[test]
    fn hash_bracket_subclass() {
        // `MyHash[...]` returns a MyHash, both for Array form, kvs, and copy form.
        run_test(
            r#"
            klass = Class.new(Hash)
            [
              klass[].instance_of?(klass),
              klass[1, 2, 3, 4].instance_of?(klass),
              klass[1 => 2].instance_of?(klass),
              # Hash[subclass-instance] returns plain Hash.
              Hash[klass[1, 2]].class,
            ]
            "#,
        );
    }

    #[test]
    fn hash_bracket_kwarg_form() {
        // `Recv[k => v]` / `Recv[**h]` — the trailing keywords form an
        // implicit positional Hash; previously dropped.
        run_tests(&[
            r#"Hash[5 => 6]"#,
            r#"Hash["a" => 1, "b" => 2]"#,
            r#"h = {9 => 9}; Hash[**h]"#,
            r#"
            klass = Class.new(Hash) { def to_hash; {trap: 1}; end }
            a = klass[5 => 6]
            [a.class.ancestors.include?(Hash), {5 => 6} == a,
             {3 => 4}.merge(klass[1 => 2])]
            "#,
            // Proc#[] with kwargs forwards to the block (like #call).
            r#"->(*a){ a }[1, k: 2]"#,
            r#"->(a, k:){ [a, k] }[1, k: 9]"#,
        ]);
    }

    #[test]
    fn hash_bracket_array_form() {
        run_tests(&[
            // 1-element arrays become `key => nil`.
            "Hash[[[:a]]]",
            // [[k, v], ...] form.
            "Hash[[[:a, 1], [:b, 2]]]",
        ]);
        // Wrong element type: ArgumentError carries CRuby-shaped message.
        run_test_error("Hash[[:a]]");
        run_test_error("Hash[[nil]]");
        // Pair too long: ArgumentError.
        run_test_error("Hash[[[:a, :b, :c]]]");
    }

    #[test]
    fn hash_fetch_keyerror_fields() {
        run_tests(&[
            // KeyError carries `receiver` (the hash) and `key`, and the message
            // formats the key with `inspect` (so a string key shows quoted).
            r#"
            h = {}
            begin
              h.fetch("foo")
            rescue KeyError => e
              [e.receiver.equal?(h), e.key, e.message]
            end
            "#,
            // Symbol keys: receiver is preserved, key is the missing symbol.
            r#"
            h = { a: 1 }
            begin
              h.fetch(:z)
            rescue KeyError => e
              [e.receiver.equal?(h), e.key]
            end
            "#,
        ]);
    }

    #[test]
    fn hash_fetch_block_supersedes_warning() {
        // Calling fetch with a default arg AND a block warns via Kernel#warn
        // (so the message hits $stderr properly, captureable from Ruby).
        run_test(
            r#"
            require "stringio"
            old = $stderr
            begin
              $stderr = StringIO.new
              r = {}.fetch(9, :foo) { |i| i * i }
              [r, $stderr.string.include?("block supersedes")]
            ensure
              $stderr = old
            end
            "#,
        );
    }

    #[test]
    fn hash_fetch_values_basic() {
        run_tests(&[
            r#"{a: 1, b: 2, c: 3}.fetch_values(:a)"#,
            r#"{a: 1, b: 2, c: 3}.fetch_values(:c, :a)"#,
            r#"{a: 1, b: 2, c: 3}.fetch_values"#,
            // Block form supplies values for missing keys.
            r#"{a: 1}.fetch_values(:a, :z) { |k| "missing #{k}" }"#,
        ]);
    }

    #[test]
    fn hash_flatten() {
        run_tests(&[
            r#"{}.flatten.class"#,
            r#"{}.flatten"#,
            r#"{a: 1, b: [2, 3]}.flatten"#,
            r#"{a: 1, b: [2, 3]}.flatten(2)"#,
            r#"{a: [[1, 2]]}.flatten(2)"#,
        ]);
        // Non-Integer level raises TypeError.
        run_test_error(r#"{a: 1}.flatten(Object.new)"#);
    }

    #[test]
    fn hash_to_proc() {
        run_tests(&[
            r#"{a: 1}.to_proc.is_a?(Proc)"#,
            r#"{a: 1}.to_proc.lambda?"#,
            r#"{a: 1}.to_proc.arity"#,
            r#"{a: 1, b: 2}.to_proc.call(:a)"#,
            r#"{a: 1}.to_proc.call(:nope)"#,
            // &proc form via map
            r#"[:a, :b].map(&{a: 1, b: 2}.to_proc)"#,
            // Default value visible through the lambda.
            r#"h = Hash.new(:dflt); h.to_proc.call(:nope)"#,
        ]);
        run_test_error(r#"{a: 1}.to_proc.call"#);
        run_test_error(r#"{a: 1}.to_proc.call(1, 2)"#);
    }

    #[test]
    fn hash_rehash_basic() {
        run_test("h = {a: 1, b: 2}; h.rehash.equal?(h)");
        run_test_error(r#"{a: 1}.freeze.rehash"#);
    }

    #[test]
    fn hash_compact() {
        run_tests(&[
            r#"{a: 1, b: nil, c: 3}.compact"#,
            r#"{a: nil, b: nil}.compact"#,
            // Originals are untouched.
            r#"h = {a: 1, b: nil}; h.compact; h"#,
            // compact! returns self when something changed, nil otherwise.
            r#"h = {a: 1, b: nil}; r = h.compact!; [h, r.equal?(h)]"#,
            r#"h = {a: 1, b: 2}; h.compact!"#,
        ]);
        // compact retains default value / proc.
        run_test(
            r#"
            h = Hash.new(42)
            h[:a] = 1
            h[:b] = nil
            r = h.compact
            [r, r.default]
            "#,
        );
        run_test_error(r#"{a: 1, b: nil}.freeze.compact!"#);
    }

    #[test]
    fn hash_sort_with_block() {
        run_tests(&[
            r#"{1 => 2, 2 => 9, 3 => 4}.sort { |a, b| b <=> a }"#,
            r#"{1 => 2, 2 => 9, 3 => 4}.sort"#,
        ]);
    }

    #[test]
    fn hash_merge_with_block() {
        // Block is invoked for keys present in both hashes.
        run_tests(&[
            r#"{a: 1, b: 2}.merge({b: 3, c: 4}) { |_, l, r| l + r }"#,
            // Multiple `others` work with the block too.
            r#"{a: 1}.merge({a: 2}, {a: 3}) { |_, l, r| l * 10 + r }"#,
        ]);
    }

    #[test]
    fn hash_default_proc_assign() {
        run_tests(&[
            // Returns the assigned proc.
            r#"h = {}; pr = Proc.new {}; (h.default_proc = pr).equal?(pr)"#,
            // nil clears default_proc.
            r#"h = Hash.new { 42 }; h.default_proc = nil; h.default_proc.nil?"#,
            // 2-arity lambdas are accepted.
            r#"h = {}; h.default_proc = ->(a, b) { a }; h.default_proc.is_a?(Proc)"#,
            // :to_proc coercion: an Object whose #to_proc returns a Proc.
            r#"
            obj = Object.new
            def obj.to_proc; Proc.new { 42 }; end
            h = Hash.new
            h.default_proc = obj
            h[:any]
            "#,
        ]);
        // Non-2-arity lambda raises TypeError.
        run_test_error(r#"{}.default_proc = ->(a) { }"#);
        run_test_error(r#"{}.default_proc = ->(a, b, c) { }"#);
        // Non-Proc, non-coercible: TypeError.
        run_test_error(r#"{}.default_proc = 42"#);
    }

    #[test]
    fn hash_index_assign_string_key() {
        run_tests(&[
            // A non-frozen String key is dup'd and frozen on store; later
            // mutation of the original String doesn't affect the stored key.
            r#"
            key = +"foo"
            h = {}
            h[key] = 0
            key << "bar"
            [h.keys[0], h.keys[0].frozen?]
            "#,
            // Singleton methods on the original key do NOT bleed into the stored
            // copy (the stored key uses the real String class).
            r#"
            key = +"foo"
            def key.reverse; "bar"; end
            h = {}
            h[key] = 0
            h.keys[0].reverse
            "#,
            // A frozen String key is stored as-is.
            r#"
            key = "foo".freeze
            h = {}
            h[key] = 0
            h.keys[0].equal?(key)
            "#,
        ]);
    }

    /// `Hash#[]` / `#[]=` reached through their JIT inliners — a direct call
    /// that skips the Ruby method frame — rather than through the builtin.
    /// `drive` warms the call site past the compile threshold; the cases walk
    /// both representations (the inline one and the boxed map a fourth key
    /// promotes to) and the defaults only the boxed one can carry.
    #[test]
    fn hash_index_jit() {
        run_test(
            r#"
            def drive(n)
              r = nil
              n.times { r = yield }
              r
            end
            res = []
            # every immediate key kind, plus hit / miss / past-the-end
            h = {}
            h[:a] = 1; h[1] = 2; h[nil] = 3
            res << drive(30) { [h[:a], h[1], h[nil], h[:zz], h[2], h[true], h["s"]] }
            # update in place keeps size and insertion order
            res << drive(30) { h[:a] = h[:a].to_i + 1 }
            res << [h.size, h.to_a, h[:a]]
            # a 4th key promotes to the boxed map; both paths still answer
            h[:d] = 4
            res << drive(30) { [h.size, h[:d], h[:a], h[:zz]] }
            # an empty inline hash misses everything
            e = {}
            res << drive(30) { [e[:a], e[0], e.size] }
            # a default is only reachable once boxed: it must still be honoured
            d = Hash.new(7)
            res << drive(30) { [d[:missing], (d[:x] = 1), d[:x]] }
            dp = Hash.new { |hh, k| hh[k] = "gen" }
            res << drive(30) { [dp[:k], dp.size] }
            res
            "#,
        );
    }

    /// Heap keys through the same inliners: a user-defined `#hash` / `#eql?`
    /// pair must still be honoured, a fresh String key dup'd and frozen on
    /// store and looked up by value, and an identity-keyed hash must keep
    /// equal-but-distinct keys apart.
    #[test]
    fn hash_index_jit_heap_keys() {
        run_test(
            r#"
            def drive(n)
              r = nil
              n.times { r = yield }
              r
            end
            class K
              attr_reader :n
              def initialize(n) = @n = n
              def hash = @n.hash
              def eql?(o) = o.is_a?(K) && o.n == @n
            end
            res = []
            c = {}
            c[K.new(1)] = "one"
            res << drive(30) { [c[K.new(1)], c[K.new(2)], c.size] }
            res << drive(30) { c[K.new(1)] = "uno" }
            res << [c.size, c[K.new(1)]]
            # a fresh String key is dup'd and frozen on store, and looked up
            # by value rather than by identity
            s = {}
            k = +"key"
            drive(30) { s[k] = 1 }
            k << "!"
            res << [s.size, s["key"], s[k], s.keys.map { |x| x.frozen? }]
            # compare_by_identity: two equal-but-distinct String keys stay apart
            ci = {}.compare_by_identity
            k1 = +"k"; k2 = +"k"
            ci[k1] = 1; ci[k2] = 2
            res << drive(30) { [ci.size, ci[k1], ci[k2], ci["k"]] }
            res << drive(30) { ci[k1] = 10 }
            res << [ci.size, ci[k1], ci[k2]]
            res
            "#,
        );
    }

    /// The raising cases still raise through the inliner: a frozen receiver,
    /// and adding a key during iteration — while updating an existing one is
    /// allowed (CRuby's rule).
    #[test]
    fn hash_index_assign_jit_raises() {
        run_test(
            r#"
            def drive(n)
              r = nil
              n.times { r = yield }
              r
            end
            res = []
            f = {a: 1}.freeze
            res << drive(30) { (f[:a] = 2) rescue $!.class.to_s }
            res << f
            it = {a: 1, b: 2}
            res << drive(30) { it.each { |k, v| it[k] = v }; it.to_a }
            res << drive(30) { (it.each { it[:new] = 1 }) rescue $!.class.to_s }
            res << it.size
            # the assigned value is what `[]=` answers, alias included
            res << drive(30) { ({}.send(:[]=, :k, 99)) }
            res << drive(30) { ({a: 1}.store(:b, 2)) }
            res
            "#,
        );
    }

    /// `Hash#[]` consults the `#default` *method* on a miss, so a subclass (or
    /// a singleton) overriding it must not be answered by the inliner, whose
    /// `hashindex_pure` reads the stored default directly.
    #[test]
    fn hash_index_subclass_default() {
        run_test(
            r#"
            def drive(n)
              r = nil
              n.times { r = yield }
              r
            end
            class H < Hash
              def default(k) = "D(#{k})"
            end
            res = []
            h = H.new
            res << drive(30) { h[:a] }
            h[:a] = 1
            res << drive(30) { [h[:a], h[:b]] }
            g = {}
            def g.default(k) = :singleton
            res << drive(30) { g[:zz] }
            res
            "#,
        );
    }

    #[test]
    fn hash_index_no_dup_default() {
        // Hash#[] returns the stored default value WITHOUT dup'ing it.
        run_test(
            r#"
            d = +"foo"
            h = Hash.new(d)
            h[:any].equal?(d)
            "#,
        );
    }

    #[test]
    fn hash_hash_method() {
        run_tests(&[
            // Order-independent.
            r#"{0 => 2, 11 => 1}.hash == {11 => 1, 0 => 2}.hash"#,
            // Same values across pairs do not cancel out.
            r#"{a: 2, b: 2}.hash == {a: 7, b: 7}.hash"#,
            // Different key/value pairings give different hashes.
            r#"{a: 2, b: 7}.hash == {a: 7, b: 2}.hash"#,
            // Stable across calls.
            r#"h = {a: 1, b: 2}; h.hash == h.hash"#,
        ]);
    }

    #[test]
    fn hash_eql_separate_from_eq() {
        // `==` compares values via `==`; `eql?` compares via `#eql?`.
        // 1 == 1.0 but 1.eql?(1.0) is false.
        run_tests(&[
            r#"{a: 1} == {a: 1.0}"#,
            r#"{a: 1}.eql?({a: 1.0})"#,
            r#"{1.0 => "x"}.eql?({1.0 => "x"})"#,
            // Equal values via `eql?` semantics.
            r#"{1 => "a"}.eql?({1 => "a"})"#,
        ]);
    }

    #[test]
    fn hash_eq_compare_by_identity_flag() {
        run_tests(&[
            // Non-empty hashes differing only in compare_by_identity are
            // not equal; two empty hashes are equal regardless.
            r#"{1 => 2} == {1 => 2}.compare_by_identity"#,
            r#"{1 => 2}.compare_by_identity == {1 => 2}"#,
            r#"{1 => 2}.eql?({1 => 2}.compare_by_identity)"#,
            r#"{}.compare_by_identity == {}"#,
            r#"({} == {}.compare_by_identity)"#,
            r#"{1 => 2}.compare_by_identity == {1 => 2}.compare_by_identity"#,
        ]);
    }

    #[test]
    fn hash_transform_keys_bang_conflicts_and_break() {
        run_tests(&[
            // New keys that collide with not-yet-processed original keys
            // must not corrupt them, and the produced keys aren't deleted.
            r#"{a: 1, b: 2, c: 3, d: 4}.transform_keys!(&:succ)"#,
            r#"{a: 1, b: 2}.transform_keys!({a: :x})"#,
            r#"{a: 1, b: 2}.transform_keys!({a: :b})"#,
            // A break leaves the partial in-place result.
            r#"h = {a: 1, b: 2, c: 3, d: 4}; h.transform_keys! { |k| break if k == :c; k.succ }; h"#,
            // Enumerator when neither a block nor a hash is given.
            r#"{a: 1}.transform_keys!.class.name"#,
        ]);
    }

    #[test]
    fn hash_transform_keys_map_lookup() {
        // The block-less map walk maps each key with the one-probe
        // `__get_or_key` (CRuby's `rb_hash_lookup2(map, k, k)`).
        run_tests(&[
            // A nil value in the map is a hit — nil becomes the new key —
            // so the lookup must not read it as a miss.
            r#"{a: 1}.transform_keys({a: nil})"#,
            // The map's default value / default proc is never consulted.
            r#"m = Hash.new(:dd); m[:a] = :A; {a: 1, b: 2}.transform_keys(m)"#,
            r#"
            m = Hash.new { |mm, k| mm[k] = :gen }
            m[:a] = :A
            g = {a: 1, b: 2}.transform_keys(m)
            [g, m.size]
            "#,
            // Warm the call site into the JIT inliner (hit, miss, and a
            // heap key with a redefined #hash / #eql?).
            r#"
            class TKK
              attr_reader :n
              def initialize(n) = @n = n
              def hash = @n.hash
              def eql?(o) = o.is_a?(TKK) && o.n == @n
            end
            m = { :a => :A, TKK.new(1) => :one }
            h = { :a => 1, :b => 2, TKK.new(1) => 3 }
            r = nil
            30.times { r = h.transform_keys(m) }
            [r[:A], r[:b], r[:one]]
            "#,
        ]);
    }

    #[test]
    fn hash_get_or_key_fallback_paths() {
        // A Hash-subclass map declines the `__get_or_key` inliner (the
        // receiver-class guard) and answers through the plain builtin.
        run_test(
            r#"
            def drive(n)
              r = nil
              n.times { r = yield }
              r
            end
            klass = Class.new(Hash)
            m = klass.new
            m[:a] = :A
            h = { a: 1, b: 2 }
            drive(30) { h.transform_keys(m) }
            "#,
        );
        // A heap probe key whose #hash raises surfaces the error through
        // the warmed inline path (and through the builtin before warmup).
        run_test_error(
            r#"
            class TKBadKey
              def hash
                raise "boom"
              end
            end
            m = {}
            9.times { |i| m[i] = i }
            h = { TKBadKey.new => 1 }
            r = nil
            30.times { r = h.transform_keys(m) }
            r
            "#,
        );
    }

    #[test]
    fn hash_get_or_key_splat_call_site() {
        // A splat call site fails the inliner's `is_simple` gate, so the
        // warmed call answers through the plain builtin (internal method,
        // no CRuby comparison).
        let v = run_test_no_result_check(
            r#"
            m = {a: :A}
            r = nil
            30.times { r = m.__get_or_key(*[:a]) }
            r.inspect
            "#,
        );
        assert_eq!(":A", v.as_str());
    }

    #[test]
    fn hash_get_or_key_inlined_error_arm() {
        // The error arm of the *inlined* helper (`hashgetorkey`): warm the
        // call site with good keys first — an error on the very first
        // iteration would surface through the interpreter-tier builtin
        // instead — then probe a boxed map (an inline receiver never
        // digests, so it could not raise) with a key whose #hash raises.
        run_test_error(
            r#"
            def gok_probe(m, k) = m.__get_or_key(k)
            class GOKRaisingHash
              def hash = raise("bad hash")
            end
            m = {}
            9.times { |i| m[:"k#{i}"] = i }
            r = nil
            30.times { r = gok_probe(m, :k0) }
            gok_probe(m, GOKRaisingHash.new)
            "#,
        );
    }

    #[test]
    fn hash_transform_values_dup_semantics() {
        // transform_values copies the table only (CRuby's
        // hash_dup_with_compare_by_id): no default, plain Hash class,
        // compare_by_identity preserved, and the walk runs over the
        // detached copy, so mutating self from the block is allowed.
        run_tests(&[
            // The default value / default proc is not carried over.
            r#"h = Hash.new(99); h[:x] = 1; g = h.transform_values { |v| v + 1 }; [g, g.default, g[:missing]]"#,
            r#"h = Hash.new { |hh, k| k.to_s }; h[:x] = 1; g = h.transform_values { |v| v }; [g.default_proc.nil?, g[:zzz]]"#,
            // A subclass receiver yields a plain Hash.
            r#"
            klass = Class.new(Hash)
            h = klass[a: 1]
            g = h.transform_values { |v| v * 2 }
            [g.class == Hash, g]
            "#,
            // compare_by_identity mode is preserved.
            r#"
            h = {}.compare_by_identity
            a = "s"; b = "s"
            h[a] = 1; h[b] = 2
            g = h.transform_values { |v| v * 3 }
            [g.compare_by_identity?, g.size, g.values.sort]
            "#,
            // Boxed identity-mode receiver — the IdentMap arm of the
            // positional store (the 2-entry case above stays inline).
            r#"
            h = {}.compare_by_identity
            6.times { |i| h["k#{i}"] = i }
            g = h.transform_values { |v| v * 2 }
            [g.compare_by_identity?, g.size, g.values]
            "#,
            // A frozen receiver is fine — the result is a fresh hash.
            r#"{x: 1}.freeze.transform_values { |v| v + 1 }"#,
            // The block mutating self doesn't disturb the walk (it runs
            // over the copy), and a break discards the partial result.
            r#"h = {a: 1, b: 2, c: 3}; g = h.transform_values { |v| h.delete(:c); v * 2 }; [g, h]"#,
            r#"h = {a: 1, b: 2}; g = h.transform_values { |v| h[:new] ||= 100; v }; [g, h]"#,
            r#"h = {a: 1, b: 2, c: 3}; out = h.transform_values { |v| break :stopped if v == 2; v }; [out, h]"#,
            // Boxed-and-indexed receiver.
            r#"h = {}; 30.times { |i| h[i] = i }; g = h.transform_values { |v| v + 1 }; [g.size, g[0], g[29], g.keys == h.keys]"#,
        ]);
    }

    #[test]
    fn hash_transform_values_bang_in_place() {
        run_tests(&[
            // Returns self; keeps the default, the class, and the
            // compare_by_identity mode (in-place overwrite).
            r#"h = {a: 1, b: 2}; ret = h.transform_values! { |v| v * 10 }; [ret.equal?(h), h]"#,
            r#"h = Hash.new(7); h[:x] = 1; h.transform_values! { |v| v + 1 }; [h.default, h[:x]]"#,
            r#"
            klass = Class.new(Hash)
            h = klass[k: 5]
            [h.transform_values! { |v| v * 2 }.class == klass, h[:k]]
            "#,
            // Deleting a not-yet-visited key from the block is allowed:
            // the position is tombstoned and never visited.
            r#"h = {a: 1, b: 2, c: 3}; h.transform_values! { |v| h.delete(:c) if v == 1; v * 2 }; h"#,
            // Boxed receiver, deleting the *current* key: at this size the
            // delete sticks in CRuby too (st_table), and the store no-ops
            // on the tombstone.
            r#"h = {}; 10.times { |i| h[i] = i }; h.transform_values! { |v| h.delete(0) if v == 0; v + 1 }; [h.size, h.key?(0), h[1]]"#,
            // Boxed identity-mode receiver overwritten in place.
            r#"h = {}.compare_by_identity; 6.times { |i| h["k#{i}"] = i }; h.transform_values! { |v| v + 1 }; [h.compare_by_identity?, h.values]"#,
            // A break leaves the partial in-place result.
            r#"h = {a: 1, b: 2, c: 3}; h.transform_values! { |v| break if v == 2; v * 10 }; h"#,
            // Enumerator when no block is given (frozen receiver too).
            r#"{a: 1}.transform_values!.class.name"#,
            r#"{}.freeze.transform_values!.is_a?(Enumerator)"#,
        ]);
        // Adding a new key from the block raises (iteration guard), and
        // a frozen receiver with a block raises FrozenError.
        run_test_error(r#"h = {a: 1, b: 2}; h.transform_values! { |v| h[:zz] = 9; v }"#);
        run_test_error(r#"{a: 1}.freeze.transform_values! { |v| v }"#);
        // Deleting the *current* key from the block: CRuby's outcome is
        // representation-dependent (an ar_table resurrects the entry
        // because the replace writes through the slot pointer; an
        // st_table keeps it deleted), so don't compare — pin monoruby's
        // uniform behavior: the delete wins, the store no-ops on the
        // tombstone.
        let v = run_test_no_result_check(
            r#"h = {a: 1, b: 2}; h.transform_values! { |v| h.delete(:a); v * 10 }; h.inspect"#,
        );
        assert_eq!("{b: 20}", v.as_str());
    }

    #[test]
    fn hash_set_value_at_out_of_range() {
        // Direct `__set_value_at` edges the Ruby-level walks never
        // produce (they bound themselves with `__entry_count`):
        // out-of-range and negative indices are ignored for both
        // representations. Internal method, so no CRuby comparison.
        let v = run_test_no_result_check(
            r#"
            h = {a: 1}
            h.__set_value_at(5, 99)
            h.__set_value_at(-1, 99)
            big = {}
            10.times { |i| big[i] = i }
            big.__set_value_at(99, :bogus)
            big.__set_value_at(-1, :bogus)
            ident = {}.compare_by_identity
            6.times { |i| ident["k#{i}"] = i }
            ident.__set_value_at(99, :bogus)
            [h, big.values == (0..9).to_a, ident.values == (0..5).to_a].inspect
            "#,
        );
        assert_eq!("[{a: 1}, true, true]", v.as_str());
    }

    #[test]
    fn hash_inspect_calls_to_s_when_inspect_returns_non_string() {
        // Verify the *behavior* of inspect: when #inspect returns a non-
        // String, #to_s is invoked on it; when it returns a String, #to_s
        // is NOT invoked. We avoid asserting the exact rendered form so the
        // test doesn't depend on Ruby's `=>` vs `: ` formatting era.
        run_tests(&[
            r#"
            obj = Object.new
            def obj.inspect; self; end
            def obj.to_s; "X-X"; end
            {1 => obj}.inspect.include?("X-X")
            "#,
            // #to_s is NOT called when #inspect returns a String.
            r#"
            obj = Object.new
            def obj.inspect; "ok"; end
            def obj.to_s; raise "should not be called"; end
            {1 => obj}.inspect.include?("ok")
            "#,
        ]);
        // Exceptions raised by #to_s propagate (not swallowed).
        run_test(
            r#"
            obj = Object.new
            def obj.inspect; self; end
            def obj.to_s; raise "boom"; end
            begin
              {1 => obj}.inspect
              :no_error
            rescue RuntimeError
              :ok
            end
            "#,
        );
    }

    #[test]
    fn hash_inspect_symbol_key_label_quoting() {
        // Hash short form: bare `name:` only for plain-identifier
        // symbols (optional single trailing ?/!); operators,
        // `=`-setters, `@`/`$`-prefixed, digit-leading, embedded
        // spaces/dashes, and the empty symbol use `"name":`.
        // (Non-ASCII-identifier keys like `:"あ"` are bare on CI but
        // would false-fail here against the sandbox's broken-locale
        // CRuby — covered by `core/hash/inspect_spec.rb` instead.)
        run_tests(&[
            r#"
            h = {}
            [:a, :A, :_x, :foo?, :foo!, :"foo=", :"0", :"!", :"+",
             :"a b", :"a-b", :"", :x1, :CONST, :"1abc",
             :"@iv", :"$g", :"==", :"[]", :"[]="].each { |s| h[s] = 1 }
            h.inspect
            "#,
            r#"{ a: 1, "b c": 2, "+": 3 }.to_s"#,
            r#"{ foo?: 1, bar!: 2, "baz=": 3 }.inspect"#,
            // Symbol key whose name is invalid UTF-8 (interned as
            // bytes): quoted, per-byte `\xNN` form.
            r#"{ "\xff".b.to_sym => 1 }.inspect"#,
            r#"{ "\xe3\x81".b.to_sym => 2, ok: 3 }.to_s"#,
        ]);
    }

    #[test]
    fn hash_sized_enumerator_no_block() {
        // Block-less calls return an Enumerator whose .size is the hash size.
        run_tests(&[
            r#"h = {a:1, b:2, c:3}; [h.each.size, h.each_key.size, h.each_value.size, h.each_pair.size]"#,
            r#"h = {a:1, b:2}; [h.select.size, h.reject.size, h.delete_if.size, h.keep_if.size]"#,
            r#"h = {a:1}; [h.transform_keys.size, h.transform_values.size]"#,
        ]);
    }

    #[test]
    fn hash_frozen_no_block_returns_enumerator() {
        // For mutating methods, `frozen.send(method)` (no block) returns an
        // Enumerator instead of immediately raising FrozenError.
        run_tests(&[
            r#"{}.freeze.delete_if.is_a?(Enumerator)"#,
            r#"{}.freeze.keep_if.is_a?(Enumerator)"#,
            r#"{}.freeze.select!.is_a?(Enumerator)"#,
            r#"{}.freeze.reject!.is_a?(Enumerator)"#,
            r#"{}.freeze.filter!.is_a?(Enumerator)"#,
        ]);
        // ...but with a block we still get FrozenError.
        run_test_error(r#"{a:1}.freeze.delete_if { true }"#);
    }

    #[test]
    fn hash_select_filter_preserves_compare_by_identity() {
        // select / filter copy the receiver's compare_by_identity flag.
        run_test(
            r#"
            h = { a: 1, b: 2 }.compare_by_identity
            h.select { true }.compare_by_identity?
            "#,
        );
    }

    #[test]
    fn hash_dup_preserves_class_not_singleton() {
        run_tests(&[
            // dup keeps the class but does NOT carry singleton methods.
            r#"
            klass = Class.new(Hash)
            h = klass[a: 1]
            h.dup.class == klass
            "#,
            r#"
            h = { 1 => 2 }
            def h.to_a; nil; end
            h.dup.to_a
            "#,
        ]);
    }

    #[test]
    fn hash_reject_no_default_carry() {
        run_tests(&[
            // reject returns a fresh hash without the receiver's default.
            r#"
            h = Hash.new(99)
            h[:a] = 1
            r = h.reject { false }
            [r.default, r[:a]]
            "#,
            // Singleton method on the receiver does not bleed into the result.
            r#"
            h = { 1 => 2 }
            def h.to_a; nil; end
            h.reject { false }.to_a
            "#,
        ]);
    }

    #[test]
    fn hash_except_clears_default() {
        run_test(
            r#"
            h = Hash.new(99)
            h[:a] = 1
            r = h.except(:a)
            [r.default, r.size]
            "#,
        );
    }

    #[test]
    fn hash_to_h_block_coercion_and_errors() {
        run_tests(&[
            r#"{a: 1, b: 2}.to_h { |k, v| [k.to_s, v * v] }"#,
            // Coerce via #to_ary.
            r#"
            obj = Object.new
            def obj.to_ary; [:b, "b"]; end
            { a: 1 }.to_h { |_| obj }
            "#,
        ]);
        // Block returns wrong-shaped Array → ArgumentError with the
        // specific wording the spec expects.
        run_test_error(r#"{a: 1}.to_h { |k, v| [k, v, 1] }"#);
        run_test_error(r#"{a: 1}.to_h { |k, v| [k] }"#);
        // Block returns non-Array → TypeError.
        run_test_error(r#"{a: 1}.to_h { |_| 42 }"#);
    }

    #[test]
    fn hash_to_h_subclass_returns_plain_hash() {
        // Hash subclass#to_h (no block) returns a plain Hash that retains
        // default value/proc and compare_by_identity flag.
        run_tests(&[
            r#"
            klass = Class.new(Hash)
            h = klass.new
            h[:foo] = :bar
            r = h.to_h
            [r.class, r[:foo]]
            "#,
            r#"
            klass = Class.new(Hash)
            h = klass.new
            h.default = 42
            r = h.to_h
            r.default
            "#,
        ]);
    }

    #[test]
    fn hash_ruby2_keywords_hash_stub() {
        run_tests(&[
            // Boolean predicate.
            r#"Hash.ruby2_keywords_hash?({})"#,
            // Mark returns a Hash (we can at least round-trip).
            r#"Hash.ruby2_keywords_hash({a: 1}) == {a: 1}"#,
        ]);
        run_test_error(r#"Hash.ruby2_keywords_hash?(nil)"#);
        run_test_error(r#"Hash.ruby2_keywords_hash([])"#);
    }

    #[test]
    fn hash_literal_reserved_word_keys() {
        // Parser shorthand for nil:/false:/true: (PR #361).
        run_tests(&[
            "{nil: 1, false: 2, true: 3}",
            "{nil: 1}.keys",
        ]);
    }

    #[test]
    fn constant_hash_literal_is_copied_per_evaluation() {
        // A constant literal is built once as a template and copied on
        // each evaluation (`HashmapInner::from_literal_pairs` +
        // `RValue::deep_copy`): every evaluation must still yield a
        // fresh, unfrozen, independent Hash with a nil default.
        run_test(
            r##"
            def f; {"name" => 1, "version" => 2, sym: 3, 4 => 5, 1.5 => 6, nil => 7, true => 8, false => 9}; end
            a = f
            b = f
            a["name"] = 100
            a[:new] = 200
            b.delete("version")
            r = [a, b, f, a.equal?(b), a.frozen?, a.default, a.compare_by_identity?, a.size, f.size]
            r << f.keys.map(&:frozen?)
            r << f.keys.map(&:class)
            # A String key is the same frozen object on every evaluation.
            r << f.keys[0].equal?(f.keys[0])
            r << (f["name"] + f[:sym] + f[4] + f[1.5] + f[nil] + f[true] + f[false])
            r
            "##,
        );
        // Inline (all-immediate, up to three pairs), boxed, and past the
        // chunking limit.
        run_test(
            r##"
            def g; {1 => 2, :a => 3}; end
            def h; {"a" => 1, "b" => 2}; end
            x = g; y = g; x[:z] = 9
            p = h; q = h; p["c"] = 3
            [x, y, p, q, g.size, h.size, g.frozen?, h.frozen?, h.keys.all?(&:frozen?)]
            "##,
        );
        run_test(
            r##"
            def big; {"k0"=>0,"k1"=>1,"k2"=>2,"k3"=>3,"k4"=>4,"k5"=>5,"k6"=>6,"k7"=>7,"k8"=>8,"k9"=>9,"k10"=>10,"k11"=>11,"k12"=>12,"k13"=>13,"k14"=>14,"k15"=>15,"k16"=>16,"k17"=>17,"k18"=>18,"k19"=>19,"k20"=>20,"k21"=>21,"k22"=>22,"k23"=>23,"k24"=>24,"k25"=>25,"k26"=>26,"k27"=>27,"k28"=>28,"k29"=>29,"k30"=>30,"k31"=>31,"k32"=>32,"k33"=>33,"k34"=>34,"k35"=>35,"k36"=>36,"k37"=>37,"k38"=>38,"k39"=>39}; end
            a = big; b = big; a["k0"] = -1
            [a.size, b.size, a["k0"], b["k0"], a["k39"], big.keys.first(3), big.values.sum]
            "##,
        );
        // Duplicate keys: the later pair wins, once, and String keys
        // compare by content.
        run_test(
            r##"
            def d; {a: 1, "s" => 2, a: 3, "s" => 4, 1 => 5, 1 => 6}; end
            [d, d.size, d.keys]
            "##,
        );
        // Mutable String values keep the per-evaluation copy: each call
        // gets its own value object, and mutating one leaves the next
        // alone. Under `frozen_string_literal` the value is one shared
        // frozen String, as it would be anywhere else in the file.
        run_test(
            r##"
            def m; {"k" => "v"}; end
            a = m; a["k"] << "!"
            b = m
            r = [a, b, a["k"].equal?(b["k"]), b["k"].frozen?]
            src = "# frozen_string_literal: true\n{'k' => 'v', s: 'w'}"
            h1 = eval(src); h2 = eval(src)
            r << [h1, h1["k"].frozen?, h1[:s].frozen?, h1.frozen?, h1.equal?(h2), h1["k"].equal?(h2["k"])]
            r
            "##,
        );
        // A heap key or a computed element still takes the general path
        // and behaves the same.
        run_test(
            r##"
            k = "dyn"
            def n(k); {k => 1, 2**70 => 2, 1e300 * 1e10 => 3, "lit" => 4}; end
            a = n(k); b = n(k)
            [a, a.equal?(b), a.keys[0].equal?(k), a.keys[3].frozen?, a.size]
            "##,
        );
    }

    #[test]
    fn hash_literal_duplicated_key_warning() {
        // A hash literal that repeats a *literal* key warns "key ... is
        // duplicated and overwritten" through `$stderr` (so a redirected
        // `$stderr` captures it, as CRuby's compile-time warning does).
        // Only literal keys are checked; a runtime key (`{k => 1, k => 2}`)
        // does not warn. The message carries an `(eval at ...)` path prefix
        // that differs from CRuby's, so match the stable part by regex.
        run_test(
            r#"
            require 'stringio'
            def cap; $stderr = StringIO.new; yield; s = $stderr.string; $stderr = STDERR; s; end
            [
              !!(cap { eval("{foo: :bar, foo: :foo}") } =~ /key :foo is duplicated/),
              !!(cap { eval(%q[{"a" => 1, "a" => 2}]) } =~ /key "a" is duplicated/),
              !!(cap { eval("{1000 => :a, 1000 => :b}") } =~ /key 1000 is duplicated/),
              !!(cap { eval("{1.0 => :a, 1.0 => :b}") } =~ /key 1.0 is duplicated/),
              !!(cap { eval("{true => 1, true => 2}") } =~ /key true is duplicated/),
              !!(cap { eval("{nil => 1, nil => 2}") } =~ /key nil is duplicated/),
              !!(cap { eval("{100000000000000000000 => 1, 100000000000000000000 => 2}") } =~ /is duplicated/),
              # A runtime (non-literal) key never warns.
              cap { k = 1; eval("{k => 1, k => 2}", binding) }.empty?,
              # A duplicate spanning a literal `**{...}` splat is detected
              # (a compile-time check, so the splat operand's keys count).
              !!(cap { eval("{a: 1, **{a: 2, b: 3, c: 1}, c: 3}") } =~ /key :a is duplicated/),
              !!(cap { eval("{a: 1, **{a: 2, b: 3, c: 1}, c: 3}") } =~ /key :c is duplicated/),
              # Nested literal splats are flattened recursively.
              !!(cap { eval("{a: 1, **{a: 2, **{d: 5}}, **{d: 6}}") } =~ /key :d is duplicated/),
              # A runtime `**h` splat is opaque: no compile-time warning.
              cap { h = {a: 2}; eval("{a: 1, **h}", binding) }.empty?,
              # No duplicate: no warning.
              cap { eval("{a: 1, b: 2}") }.empty?,
              # The overwrite still happens (last value wins).
              eval("{a: 1, a: 2}"),
              eval("{a: 1, **{a: 2, b: 3, c: 1}, c: 3}"),
            ]
            "#,
        );
    }

    #[test]
    fn hash_literal_freezes_string_keys() {
        // A String key in a hash *literal* is stored as a frozen dup (like
        // `Hash#[]=`), so later mutation of the source String can't corrupt
        // the hash. Frozen keys and non-String keys are stored as-is.
        run_tests(&[
            r#"s = "x"; h = { s => 1 }; [h.keys.first.frozen?, h.keys.first.equal?(s)]"#,
            // The classic mutate-the-source case: the stored key is unaffected.
            r#"key = +"foo"; h = { key => "bar" }; key.reverse!; [h["foo"], h.keys.first, key]"#,
            // An already-frozen String key is stored without copying.
            r#"s = "x".freeze; h = { s => 1 }; h.keys.first.equal?(s)"#,
            // Non-String keys are never dup'd.
            r#"k = [1]; h = { k => 1 }; h.keys.first.equal?(k)"#,
            // Duplicate string keys in a literal still collapse (last wins).
            r#"{ "a" => 1, "a" => 2 }"#,
        ]);
    }

    #[test]
    fn hash_literal_double_splat_nil() {
        // `**nil` in a hash literal contributes nothing (CRuby treats a
        // nil operand as empty), whether the operand is a literal nil or a
        // variable that holds nil at runtime.
        run_tests(&[
            "{**nil}",
            "x = nil; {**x}",
            "{a: 1, **nil, b: 2}",
            "h = {a: 1}; {**h, **nil}",
            "{**nil}.empty?",
            // A non-nil, non-Hash operand still raises TypeError.
            "begin; {**1}; rescue TypeError; :te; end",
            "begin; {**false}; rescue TypeError; :te; end",
            // A `#to_hash` object still expands normally alongside `**nil`.
            "o = Object.new; def o.to_hash; {z: 9}; end; {**o, **nil}",
        ]);
    }

    #[test]
    fn hash_literal_double_splat_order() {
        // A hash literal is built strictly left-to-right: every element
        // (ordinary `k: v` pair or `**` splat) is applied in source order,
        // and a later element overwrites an earlier key while preserving
        // the key's first-seen insertion position. An explicit pair after a
        // `**` splat must overwrite a key the splat contributed.
        run_tests(&[
            r#"h = {b: 2, c: 3}; ({a: 1, **h, c: 4}).to_a"#,
            r#"h = {b: 2, c: 3}; ({**h, a: 1}).to_a"#,
            r#"h = {b: 2, c: 3}; ({a: 1, **h}).to_a"#,
            r#"({x: 0, **{a: 1}, a: 9, y: 5}).to_a"#,
            // Two interleaved splats with pairs before, between, and after.
            r#"({**{a: 1}, b: 2, **{a: 3, c: 4}, b: 5}).to_a"#,
            // `**nil` interleaved between pairs contributes nothing but must
            // not disturb ordering.
            r#"({a: 1, **nil, b: 2}).to_a"#,
            // A `#to_hash` operand overwritten by a trailing explicit pair.
            r#"o = Object.new; def o.to_hash; {k: 1}; end; ({**o, k: 2}).to_a"#,
        ]);
    }

    #[test]
    fn hash_each_block_trailing_comma() {
        // PR #361 follow-up: a trailing comma in a block parameter list
        // (`|k,|`) injects a synthetic anonymous post param, bumping the
        // total positional arity to >1 so a single Array argument from
        // Hash#each (`[k, v]`) auto-splats. The block then sees only the
        // key, matching CRuby's `core/hash/shared/each.rb`.
        run_tests(&[
            r#"
            ary = []
            { "a" => 1, "b" => 2, "c" => 3 }.each { |k,| ary << k }
            ary.sort
            "#,
            // Same for Array#each on a list of pairs.
            r#"[[1, 2], [3, 4]].map { |k,| k }"#,
            // |a, b,| (already 2 params) also auto-splats; trailing comma is
            // a no-op for the destructuring count.
            r#"[[1, 2, 3]].map { |a, b,| [a, b] }"#,
            // No trailing comma → single param sees the whole array.
            r#"[[1, 2]].map { |k| k }"#,
        ]);
    }

    /// Whether Hash#map passes the [k, v] pair whole or split follows
    /// CRuby's rb_block_pair_yield_optimizable, and procs and lambdas
    /// differ: a proc splits whenever it can take more than one positional
    /// (`{ |a, *b| }` splits, bare `{ |*a| }` gets the pair whole); a
    /// lambda — Symbol procs included — only when it requires at least two
    /// (`->(a, b, *c)` splits, `->(a, *b)` gets the pair whole). With an
    /// overridden `each`, values pass through unrepacked: a proc auto-splats
    /// a single-array yield, a strict lambda raises on it.
    /// Inline `Hash#default=`: every shape the machine code splits on, hot
    /// enough to compile. In-place overwrite of an existing default box
    /// (including replacing a default *proc*, which `default=` clears),
    /// nil assignments with and without a box (the no-box nil is an inline
    /// no-op) on both representations, first-time defaults through the
    /// runtime call (box allocation / inline→boxed promotion), the method's
    /// return value, and a frozen receiver deopting to raise.
    #[test]
    fn hash_default_assign_inline() {
        run_test(
            r#"
            def drive(n)
              r = nil
              n.times { r = yield }
              r
            end
            res = []
            h1 = Hash.new(0)
            drive(30) { h1.default = 5 }
            res << [h1.default, h1[:missing]]
            h2 = Hash.new { |hh, k| :proc }
            drive(30) { h2.default = 9 }
            res << [h2.default, h2.default_proc, h2[:missing]]
            h3 = Hash.new(3)
            drive(30) { h3.default = nil }
            res << [h3.default, h3[:missing]]
            h4 = {a: 1}
            drive(30) { h4.default = nil }
            res << h4.default
            h5 = {a: 1}
            drive(30) { h5.default = 7 }
            res << [h5.default, h5[:a], h5[:missing]]
            h6 = {}
            res << drive(30) { h6.send(:default=, 42) }
            h7 = Hash.new(1)
            h7.freeze
            res << (drive(30) { h7.default = 2 } rescue $!.class.to_s)
            res
            "#,
        );
    }

    /// each_key / each_value (Ruby, builtins/hash.rb): the same live
    /// positional walk as `each`, yielding bare keys/values. Pinned to
    /// CRuby including the enumerator forms and deletes mid-iteration.
    #[test]
    fn hash_each_key_value() {
        run_tests(&[
            r#"acc = []; r = {a: 1, b: 2, c: 3}.each_key { |k| acc << k }; [acc, r.class]"#,
            r#"acc = []; r = {a: 1, b: 2, c: 3}.each_value { |v| acc << v }; [acc, r.class]"#,
            r#"e = {a: 1, b: 2}.each_key; [e.class, e.size, e.to_a]"#,
            r#"e = {a: 1, b: 2}.each_value; [e.class, e.size, e.to_a]"#,
            // A multi-param block sees the bare key (no pair, no splat).
            r#"acc = []; {a: 1}.each_key { |k, v| acc << [k, v] }; acc"#,
            // Deleting a not-yet-visited key mid-walk skips it (#1095 discipline).
            r#"h = {a:1, b:2, c:3, d:4, e:5, f:6, g:7}; acc = []; h.each_key { |k| h.delete(:e); acc << k }; [acc, h.keys]"#,
            r#"h = {a:1, b:2, c:3, d:4, e:5, f:6, g:7}; acc = []; h.each_value { |v| h.delete(:e); acc << v }; [acc, h.keys]"#,
            // Adding raises; empty hash yields nothing.
            r#"h = {a: 1}; (h.each_key { h[:new] = 1 } rescue $!.class.to_s)"#,
            r#"n = 0; {}.each_key { n += 1 }; n"#,
        ]);
    }

    /// Deleting during iteration follows CRuby (#1095): while a traversal
    /// is live a delete tombstones its entry in place — the index table
    /// drops it, the walk's positions stay stable — so a deleted
    /// not-yet-visited entry is not yielded, deleting the current or a
    /// visited entry skips nothing, and `size`/`keys`/`inspect`/`dup`
    /// observed mid-walk never see the dead entry. Compaction is lazy: the
    /// next mutation outside an iteration (or a clone) sweeps the
    /// tombstones. Exercised on both representations — an inline hash
    /// promotes to boxed on the first mid-iteration delete — plus identity
    /// hashes and `shift`.
    #[test]
    fn hash_delete_during_iteration() {
        run_tests(&[
            // Not-yet-visited: skipped (boxed).
            r#"h = {a:1, b:2, c:3, d:4, e:5, f:6, g:7}; acc = []; h.each { |k, v| h.delete(:e) if k == :b; acc << k }; [acc, h.keys, h.size]"#,
            // Current key: nothing skipped.
            r#"h = {a:1, b:2, c:3, d:4, e:5, f:6, g:7}; acc = []; h.each { |k, v| h.delete(k); acc << k }; [acc, h.keys, h.empty?]"#,
            // Already-visited key: nothing skipped.
            r#"h = {a:1, b:2, c:3, d:4, e:5, f:6, g:7}; acc = []; h.each { |k, v| h.delete(:a) if k == :c; acc << k }; [acc, h.keys]"#,
            // Everything deleted up front: only the current entry yields.
            r#"h = {a:1, b:2, c:3, d:4, e:5, f:6, g:7}; acc = []; h.each { |k, v| h.keys.each { |x| h.delete(x) }; acc << k }; [acc, h.keys, h.size]"#,
            // Inline representation: promotes to boxed mid-iteration.
            r#"h = {a:1, b:2, c:3}; acc = []; h.each { |k, v| h.delete(:b); acc << k }; [acc, h.keys, h.size]"#,
            // Live size mid-walk, and update of an existing key stays legal.
            r#"h = {a:1, b:2, c:3, d:4, e:5, f:6, g:7}; acc = []; h.each { |k, v| h.delete(:e) if k == :a; acc << h.size; h[:b] = 99 }; [acc.first, h[:b]]"#,
            // Re-adding a deleted key is adding a new key: raises.
            r#"h = {a:1, b:2, c:3, d:4, e:5, f:6, g:7}; (h.each { |k, v| h.delete(:e); h[:e] = 5 } rescue $!.class.to_s)"#,
            // shift during iteration tombstones the first live entry.
            r#"h = {a:1, b:2, c:3}; acc = []; h.each { |k, v| h.shift; acc << k }; [acc, h.keys]"#,
            // Observers mid-window: keys/inspect/dup never see the dead entry,
            // and the dup is independently mutable (compacted copy).
            r#"h = {a:1, b:2, c:3, d:4, e:5, f:6, g:7}; r = nil; h.each { |k, v| h.delete(:e) if k == :a; r = [h.keys, h.inspect, h.dup.size] if k == :b }; r"#,
            // Identity hash: same discipline through the IdentMap path.
            r#"h = {a:1, b:2, c:3, d:4, e:5, f:6}.compare_by_identity; acc = []; h.each { |k, v| h.delete(:e); acc << k }; [acc, h.keys]"#,
            // Break leaves tombstones; the next mutation compacts and the
            // hash is fully usable.
            r#"h = {a:1, b:2, c:3, d:4, e:5, f:6, g:7}; h.each { |k, v| h.delete(:e); break }; h[:new] = 1; h.delete(:a); [h.keys, h.size]"#,
            // to_h's block form shares the walk.
            r#"h = {a:1, b:2, c:3, d:4, e:5, f:6, g:7}; r = h.to_h { |k, v| h.delete(:e); [k, v] }; [r.keys, h.keys]"#,
        ]);
    }

    #[test]
    fn hash_flat_map_pair_split_rules() {
        run_tests(&[
            r#"h = {"a" => [1, 2], "b" => [3]}; h.flat_map(&:last)"#,
            r#"h = {"a" => [1, 2], "b" => [3]}; h.flat_map { |pair| pair }"#,
            r#"h = {"a" => [1, 2], "b" => [3]}; h.flat_map { |k, v| [k] + v }"#,
            r#"h = {"a" => [1, 2], "b" => [3]}; h.flat_map { |*a| a }"#,
            r#"h = {"a" => [1, 2], "b" => [3]}; h.flat_map(&->(a, *b) { [a, b] })"#,
            r#"h = {"a" => [1, 2], "b" => [3]}; h.flat_map { |k, v| v.size }"#,
        ]);
    }

    #[test]
    fn hash_map_pair_split_rules() {
        run_tests(&[
            r#"h = {a: 1, b: 2}; h.map { |*a| a }"#,
            r#"h = {a: 1, b: 2}; h.map(&->(*a) { a })"#,
            r#"h = {a: 1, b: 2}; h.map { |a, *b| [a, b] }"#,
            r#"h = {a: 1, b: 2}; h.map(&->(a, *b) { [a, b] })"#,
            r#"h = {a: 1, b: 2}; h.map(&->(a, b, *c) { [a, b, c] })"#,
            r#"h = {a: 1, b: 2}; h.map(&->(a, b = :d) { [a, b] })"#,
            r#"h = {a: 1, b: 2}; h.map { |a, b = :d| [a, b] }"#,
            r#"h = {a: 1, b: 2}; h.map(&:to_s)"#,
            r#"h = {a: 1, b: 2}; h.map { |a, b, *c| [a, b, c] }"#,
        ]);
        // A singleton `each` on a plain Hash instance must take the
        // pass-through path — `instance_of?(Hash)` cannot see it, which is
        // why the dispatch checks `method(:each).owner`.
        run_test_once(
            r#"
            hs = {a: 1, b: 2}
            def hs.each; yield :s, :t; end
            [hs.map { |k, v| [k, v] }, hs.map { |pair| pair }, hs.map { |*a| a }]
            "#,
        );
        // Pass-through on an overridden each that yields a single array:
        // the proc auto-splats it, the strict lambda gets it as ONE argument
        // and raises.
        run_test(
            r#"
            cls = Class.new(Hash) { def each; yield [:x, 9]; end }
            h = cls.new
            r1 = h.map { |k, v| [k, v] }
            r2 = h.map { |*a| a }
            r3 = (h.map(&->(k, v) { [k, v] }) rescue $!.class.to_s)
            zero = Class.new(Hash) { def each; yield; end }.new.map { |*a| a }
            [r1, r2, r3, zero]
            "#,
        );
        // Subclass WITHOUT an each override still gets the fast path.
        run_test(
            r#"
            cls = Class.new(Hash)
            h = cls.new
            h[:p] = 1
            h[:q] = 2
            [h.map { |k, v| [k, v] }, h.map { |pair| pair }, h.map { |*a| a }]
            "#,
        );
    }

    /// `Hash#map` asks `__block_splits_pair?` about a block it never captures,
    /// so every shape a block handler can still be in when it arrives —
    /// literal, lambda literal, Proc object, `&:sym`, `&method(:m)`, curried,
    /// and an object that only answers `#to_proc` — has to be classified
    /// without materializing it. A wrong answer is visible either as a wrongly
    /// split pair or as an ArgumentError from a strict lambda.
    #[test]
    fn hash_map_block_handler_shapes() {
        run_test(
            r#"
            h = {a: 1, b: 2}
            def two(k, v) = [k, v]
            def one(x) = x
            pr2 = proc { |k, v| [k, v] }
            la2 = ->(k, v) { [k, v] }
            la1 = ->(a) { a }
            res = []
            res << h.map(&pr2)
            res << h.map(&la2)
            res << h.map(&la1)
            res << h.map(&method(:two))
            res << h.map(&method(:one))
            res << h.map(&:itself)
            res << h.map(&->(a, b, c = 3) { [a, b, c] })
            res << h.map(&->((a, b)) { [a, b] })
            res << h.map(&Class.new { def to_proc = ->(k, v) { [k, v] } }.new)
            res << h.map(&Class.new { def to_proc = proc { |x| x } }.new)
            # a curried lambda answers from its own stored flag; its results
            # are Procs, whose #inspect carries an address — compare shapes.
            res << h.map(&la2.curry[]).map { |p| [p.class.to_s, p.lambda?] }
            res
            "#,
        );
    }

    #[test]
    fn hash_map_arity_and_subclass() {
        run_tests(&[
            // Plain Hash#map: a two-param block sees k, v; a one-param block
            // sees the [k, v] pair.
            r#"{a: 1, b: 2}.map { |k, v| [k, v] }"#,
            r#"{a: 1, b: 2}.map { |x| x }"#,
            // A strict arity-2 Method (no auto-splat) still receives k and v.
            r#"
            c = Class.new { def register(a, b); [a, b]; end }
            m = c.new.method(:register)
            {1 => "a", 2 => "b"}.map(&m)
            "#,
        ]);
        // A subclass overriding #each to `yield k, v` (two values) maps
        // correctly for both block arities.
        run_test(
            r#"
            cls = Class.new(Hash) do
              def each
                super { |k, v| yield k, v }
              end
            end
            o = cls.new
            o["x"] = "y"
            [o.map { |k, v| [k, v] }, o.map { |z| z }]
            "#,
        );
        // A subclass overriding #each to `yield [k, v]` (one array) too.
        run_test(
            r#"
            cls = Class.new(Hash) do
              def each
                super { |k, v| yield [k, v] }
              end
            end
            o = cls.new
            o["x"] = "y"
            o.map { |k, v| [k, v] }
            "#,
        );
    }

    #[test]
    fn hash_sort_with_comparator_block() {
        // `Hash#sort` with a block sorts the freshly built [k, v] pair
        // Arrays with a Ruby comparator — the pairs live only in a Rust
        // Vec while that block runs, which is what the rooting covers.
        // All three comparator outcomes (<0, >0 and 0) are exercised.
        run_test(
            r#"
            h = { "b" => 2, "a" => 1, "c" => 3 }
            [
              h.sort { |x, y| x[0] <=> y[0] },
              h.sort { |x, y| y[1] <=> x[1] },
              h.sort { |x, y| 0 },
            ]
            "#,
        );
        // A comparator that raises propagates the error out of the sort.
        // Enough pairs that the sort keeps asking after the first raise, so
        // the "an error is already pending" short-circuit is taken too.
        run_test(
            r#"
            h = { "d" => 4, "b" => 2, "a" => 1, "c" => 3, "e" => 5 }
            begin
              h.sort { |x, y| raise ArgumentError, "boom" }
            rescue ArgumentError => e
              [e.class, e.message]
            end
            "#,
        );
    }

    #[test]
    fn hash_sort_non_integer_comparator() {
        // Regression for #1076: `Hash#sort` used to honour only Fixnum
        // comparator results and silently treat everything else as "equal",
        // so a Float or Bignum comparator returned an UNSORTED array with no
        // error and a `nil` comparator swallowed CRuby's ArgumentError. It
        // now follows `rb_cmpint` like `Array#sort`: an Integer's sign, any
        // other object via its own `<=>` against 0, and `nil` as an error.
        run_test(
            r#"
            h = { "b" => 2, "a" => 1, "c" => 3 }
            [
              h.sort { |x, y| (x[1] <=> y[1]) * 1.0 },            # Float
              h.sort { |x, y| (x[1] <=> y[1]) * (10 ** 20) },     # Bignum
              h.sort { |x, y| Rational(x[1] <=> y[1], 3) },       # Rational
            ]
            "#,
        );
        // `nil` names the two elements; a non-numeric result names itself
        // against the 0 it was compared with — both exactly as CRuby does.
        run_test(
            r#"
            h = { "b" => 2, "a" => 1, "c" => 3 }
            [
              (begin; h.sort { |x, y| nil }; rescue => e; [e.class, e.message]; end),
              (begin; h.sort { |x, y| "z" }; rescue => e; [e.class, e.message]; end),
            ]
            "#,
        );
    }

    #[test]
    fn hash_to_h_with_block() {
        // NOTE: this exercises the *Ruby-level* `Hash#to_h` in
        // `monoruby/builtins/hash.rb`, which reopens `class Hash` at startup
        // and shadows the Rust builtin registered here. (`Hash.instance_method
        // (:to_h).source_location` is `["<internal:hash>", 11]`.) The Rust
        // `to_h` block branch is therefore not reachable from Ruby.
        // A block return that is not a 2-element Array is a TypeError /
        // ArgumentError, matching CRuby.
        run_test(
            r#"
            h = { a: 1, b: 2 }
            [
              h.to_h { |k, v| [k.to_s, v * 10] },
              h.to_h { |k, v| [v, k] },
              {}.to_h { |k, v| [k, v] },
              h.to_h,
            ]
            "#,
        );
        run_test(
            r#"
            h = { a: 1 }
            [
              (begin; h.to_h { |k, v| [1, 2, 3] }; rescue => e; e.class; end),
              (begin; h.to_h { |k, v| 5 }; rescue => e; e.class; end),
            ]
            "#,
        );
    }

    #[test]
    fn env_fetch_default_block_and_missing() {
        // `ENV.fetch` coerces the key to a fresh String and keeps it alive
        // across the `warn` call and the Ruby dispatches inside `get`.
        // Covers all four outcomes: hit, default argument, block, and the
        // block-supersedes-default warning path.
        let _g = env_lock();
        run_test(
            r#"
            ENV["MONORUBY_COV_FETCH"] = "hit"
            res = [
              ENV.fetch("MONORUBY_COV_FETCH"),
              ENV.fetch("MONORUBY_COV_ABSENT", "dflt"),
              ENV.fetch("MONORUBY_COV_FETCH", "dflt"),
              ENV.fetch("MONORUBY_COV_ABSENT") { |k| "blk:#{k}" },
              ENV.fetch("MONORUBY_COV_FETCH") { |k| "blk:#{k}" },
              ENV.fetch("MONORUBY_COV_ABSENT", "dflt") { |k| "block wins" },
              (begin; ENV.fetch("MONORUBY_COV_ABSENT"); rescue KeyError => e; e.class; end),
            ]
            ENV.delete("MONORUBY_COV_FETCH")
            res
            "#,
        );
    }

    #[test]
    fn hash_to_h_semantics() {
        // Pins every behaviour of `Hash#to_h` against CRuby. Written before
        // moving the implementation from `builtins/hash.rb` into Rust, so any
        // divergence introduced by the port shows up here.

        // No block: a plain Hash returns *itself* (identity, not a copy).
        run_test(
            r#"
            h = { a: 1, b: 2 }
            [h.to_h.equal?(h), h.to_h, {}.to_h]
            "#,
        );
        // No block on a subclass: a new *plain* Hash, carrying the receiver's
        // default / default_proc / compare_by_identity.
        run_test(
            r#"
            sub = Class.new(Hash)
            o = sub.new
            o[:x] = 1
            r = o.to_h
            [r.class, r, r.equal?(o)]
            "#,
        );
        run_test(
            r#"
            sub = Class.new(Hash)
            o = sub.new(99)
            o[:x] = 1
            r = o.to_h
            [r.class, r[:missing], r.default]
            "#,
        );
        run_test(
            r#"
            sub = Class.new(Hash)
            o = sub.new { |h, k| "gen:#{k}" }
            o[:x] = 1
            r = o.to_h
            [r.class, r[:missing], r.default_proc.nil?]
            "#,
        );
        run_test(
            r#"
            sub = Class.new(Hash)
            o = sub.new
            o.compare_by_identity
            a = "k"
            b = "k"
            o[a] = 1
            o[b] = 2
            r = o.to_h
            [r.class, r.compare_by_identity?, r.size]
            "#,
        );
        // Block form.
        run_test(
            r#"
            h = { a: 1, b: 2 }
            [h.to_h { |k, v| [k.to_s, v * 10] }, h.to_h { |k, v| [v, k] }, {}.to_h { |k, v| [k, v] }]
            "#,
        );
        // A non-Array block result is coerced through `to_ary` when it has one.
        run_test(
            r#"
            pairish = Class.new do
              def initialize(a, b); @a = a; @b = b; end
              def to_ary; [@a, @b]; end
            end
            { a: 1 }.to_h { |k, v| pairish.new(k.to_s, v + 1) }
            "#,
        );
        // ...and otherwise raises, with CRuby's exception class and message.
        run_test(
            r#"
            h = { a: 1 }
            [
              (begin; h.to_h { |k, v| 5 }; rescue => e; [e.class, e.message]; end),
              (begin; h.to_h { |k, v| [1, 2, 3] }; rescue => e; [e.class, e.message]; end),
              (begin; h.to_h { |k, v| [1] }; rescue => e; [e.class, e.message]; end),
            ]
            "#,
        );
    }

    #[test]
    fn hash_entry_at_intrinsics() {
        // `__key_at` / `__value_at` are monoruby-only, so the script computes
        // the same value the other way round when they are absent — CRuby
        // takes the `to_a` branch and the results must agree, which pins the
        // intrinsics to CRuby's insertion order rather than to our own idea
        // of it.
        run_test(
            r##"
            def entries(h)
              if h.respond_to?(:__key_at)
                (0...h.size).map { |i| [h.__key_at(i), h.__value_at(i)] }
              else
                h.to_a
              end
            end
            small = { a: 1, "b" => 2, 3 => :c }
            big = {}
            i = 0
            while i < 100; big["k#{i}"] = i; i += 1; end
            ident = {}
            ident.compare_by_identity
            x = "k"
            y = "k"
            ident[x] = 1
            ident[y] = 2
            [entries(small), entries(big).last, entries({}), entries(ident)]
            "##,
        );
        // Out of range (and a negative index) is nil, so a `while` loop bounded
        // by `size` never needs an error edge.
        run_test(
            r##"
            h = { a: 1 }
            if h.respond_to?(:__key_at)
              [h.__key_at(1), h.__value_at(1), h.__key_at(-1), h.__value_at(-1)]
            else
              [nil, nil, nil, nil]
            end
            "##,
        );
    }

    /// Drive the intrinsics through the JIT the way they are meant to be
    /// used — a hot `while` loop bounded by `size` — and pin the result to
    /// CRuby. The loop is inside a method called many times, so the method
    /// JIT compiles it and the machine-code path (not the interpreter's
    /// builtin) produces these answers.
    #[test]
    fn hash_entry_at_intrinsics_jit() {
        run_test(
            r##"
            def entries(h)
              return h.to_a unless h.respond_to?(:__key_at)
              r = []
              i = 0
              while i < h.size
                r << [h.__key_at(i), h.__value_at(i)]
                i += 1
              end
              r
            end

            small = { a: 1, b: 2 }          # inline representation
            big = {}
            i = 0
            while i < 40; big[i] = i * 3; i += 1; end   # boxed representation
            ident = {}
            ident.compare_by_identity
            ident[:x] = 1
            ident[:y] = 2

            out = []
            n = 0
            while n < 30
              # One call site sees both representations, so the compiled code
              # has to handle the inline/boxed split without deopting.
              out << entries(small)
              out << entries(big).last
              out << entries(ident)
              out << entries({})
              n += 1
            end
            [out.uniq, small.size, big.size, ident.size, {}.size]
            "##,
        );
    }

    /// A hash that outgrows the inline representation while a compiled call
    /// site is already hot: the same code must keep answering correctly
    /// across the promotion.
    #[test]
    fn hash_entry_at_intrinsics_promotion() {
        run_test(
            r##"
            def probe(h)
              return [h.size, h.to_a.first, h.to_a.last] unless h.respond_to?(:__key_at)
              [h.size, [h.__key_at(0), h.__value_at(0)],
                       [h.__key_at(h.size - 1), h.__value_at(h.size - 1)]]
            end

            h = {}
            out = []
            i = 0
            while i < 30
              h[i] = i * 2
              out << probe(h)
              i += 1
            end
            out
            "##,
        );
    }

    /// The zero-argument accessors are emitted as machine code, so drive
    /// each through a hot call site and pin every combination to CRuby.
    /// `default` and `default_proc` read the same slot and differ only in
    /// which discriminant they accept — the crossed cases (a value default
    /// asked for its proc, and vice versa) are the ones that would break if
    /// the discriminants were transposed.
    #[test]
    fn hash_default_accessors_jit() {
        run_test(
            r##"
            def probe(h)
              [h.default, h.default_proc.class, h.compare_by_identity?]
            end

            plain = {}
            valued = Hash.new(7)
            proced = Hash.new { |hash, key| hash[key] = key.to_s }
            ident = {}
            ident.compare_by_identity
            big_ident = {}
            big_ident.compare_by_identity
            i = 0
            while i < 10; big_ident[i.to_s] = i; i += 1; end
            valued_big = Hash.new(:d)
            i = 0
            while i < 10; valued_big[i] = i; i += 1; end

            out = []
            n = 0
            while n < 30
              out << probe(plain)
              out << probe(valued)
              out << probe(proced)
              out << probe(ident)
              out << probe(big_ident)
              out << probe(valued_big)
              n += 1
            end
            out.uniq
            "##,
        );
        // The one-argument form invokes the default proc, so it must keep
        // using the generic path rather than the inlined reader.
        run_test(
            r##"
            h = Hash.new { |hash, key| "made:#{key}" }
            v = Hash.new(3)
            r = []
            i = 0
            while i < 30
              r = [h.default(:k), v.default(:k), h.default, v.default]
              i += 1
            end
            r
            "##,
        );
        // A block makes `compare_by_identity?` raise (a monoruby-specific
        // restriction, so this is not compared against CRuby). The call site
        // is compiled hot first, which is exactly when the inliner could
        // wrongly swallow the block.
        run_test_error(
            r##"
            h = {}
            i = 0
            while i < 300
              h.compare_by_identity?
              i += 1
            end
            h.compare_by_identity? { 1 }
            "##,
        );
    }

    /// Index types the machine-code path must not swallow. Both of these
    /// are the builtin's own behaviour, and a hot call site has to keep it:
    /// a Bignum index guards out of the compiled path (rather than being
    /// untagged into a truncated position) and a non-Integer index still
    /// raises TypeError.
    #[test]
    fn hash_entry_at_intrinsics_index_types() {
        run_test(
            r##"
            h = { a: 1, b: 2 }
            if h.respond_to?(:__key_at)
              r = []
              i = 0
              while i < 30
                r = [
                  (begin; h.__key_at(2 ** 70); rescue RangeError; :range; end),
                  (begin; h.__value_at(-(2 ** 70)); rescue RangeError; :range; end),
                  (begin; h.__key_at("x"); rescue TypeError; :type; end),
                ]
                i += 1
              end
              r
            else
              [:range, :range, :type]
            end
            "##,
        );
    }

    #[test]
    fn env_replace_applies_pairs_before_an_error() {
        let _g = env_lock();
        run_test_once(
            r##"
            saved = ENV.to_hash
            ENV["MONORUBY_ENV_REPL_KEEP"] = "keep"
            e = (ENV.replace("MONORUBY_ENV_REPL_A" => "1", Object.new => Object.new) rescue $!.class)
            r = [e, ENV["MONORUBY_ENV_REPL_A"], ENV["MONORUBY_ENV_REPL_KEEP"]]
            ENV.replace(saved)
            r << ENV.key?("MONORUBY_ENV_REPL_A")
            "##,
        );
    }

    // -- ENV on the bytes of `environ` (#2131, #2135–#2138, #2140, #2141) --

    /// A name or a value with a NUL in it, or in an encoding that is not
    /// ASCII-compatible, is refused with CRuby's `env_name` / `env_value`
    /// messages — by every method that takes a name.
    #[test]
    fn env_name_and_value_checks() {
        let _g = env_lock();
        run_test_once(
            r##"
            def t; yield; rescue Exception => e; [e.class, e.message]; end
            k = "a\0b"
            n = "MONORUBY_ENV_CHK"
            u16 = n.encode("UTF-16LE")
            [
              t { ENV[k] }, t { ENV.fetch(k, 1) }, t { ENV.fetch(k) { 2 } },
              t { ENV.key?(k) }, t { ENV.include?(k) }, t { ENV.has_key?(k) },
              t { ENV.member?(k) }, t { ENV.delete(k) }, t { ENV.delete(k) { 3 } },
              t { ENV[n] = "a\0b" }, t { ENV["A\0B"] = "x" }, t { ENV["A\0B"] = nil },
              t { ENV.store("A\0B", "x") }, t { ENV.assoc(k) }, t { ENV.values_at(k) },
              t { ENV.values_at("PATH", k) }, t { ENV.slice(k) }, t { ENV.slice("a\0", 1) },
              t { ENV.except(k).size == ENV.size }, t { ENV.merge!(k => "x") },
              t { ENV.merge!(n => "x\0y") }, t { ENV.update(k => nil) },
              t { ENV.key?(n) },
              t { ENV[u16] }, t { ENV.fetch(u16, 1) }, t { ENV.key?(n.encode("UTF-32BE")) },
              t { ENV[u16] = "x" }, t { ENV[n] = "x".encode("UTF-16LE") },
              t { ENV[n] = "あ".encode("UTF-16LE") }, t { ENV.delete(u16) },
              t { ENV.assoc(u16) }, t { ENV.values_at(u16) }, t { ENV.slice(u16) },
              t { ENV.merge!(u16 => "x") }, t { ENV.key?(n) },
              t { ENV[1] }, t { ENV[nil] }, t { ENV.fetch(:a, 1) }, t { ENV[n] = 1 },
              t { ENV.key?(1) }, t { ENV.delete(1) }, t { ENV.except(1).size == ENV.size },
            ]
            "##,
        );
    }

    /// `key`, `value?` and `rassoc` compare bytes and check nothing: a
    /// value no variable can hold is simply not found.
    #[test]
    fn env_value_lookups_check_nothing() {
        let _g = env_lock();
        run_test_once(
            r##"
            def t; yield; rescue Exception => e; [e.class, e.message]; end
            o = Object.new; def o.to_str = "monoruby env value 2138"
            ENV["MONORUBY_ENV_VAL"] = "monoruby env value 2138"
            r = [
              t { ENV.key("a\0b") }, t { ENV.value?("a\0b") }, t { ENV.has_value?("a\0b") },
              t { ENV.rassoc("a\0b") }, t { ENV.key("x".encode("UTF-16LE")) },
              t { ENV.value?("x".encode("UTF-16LE")) }, t { ENV.rassoc("x".encode("UTF-16LE")) },
              t { ENV.key(1) }, t { ENV.value?(1) }, t { ENV.rassoc(1) },
              t { ENV.key(o) }, t { ENV.value?(o) }, t { ENV.rassoc(o) },
              t { ENV.key("monoruby env value 2138").frozen? },
              t { ENV.rassoc("monoruby env value 2138").map(&:frozen?) },
              t { ENV.rassoc("monoruby env value 2138")[0].encoding.to_s },
            ]
            ENV.delete("MONORUBY_ENV_VAL")
            r
            "##,
        );
    }

    /// `[]=` converts both operands, then checks the name, then the
    /// value, and only then asks `setenv` — so a bad value is reported
    /// ahead of a name `setenv` rejects (#2140).
    #[test]
    fn env_aset_check_order() {
        let _g = env_lock();
        run_test_once(
            r##"
            def t; yield; rescue Exception => e; [e.class, e.message]; end
            [
              t { ENV[""] = 1 }, t { ENV["A=B"] = 1 }, t { ENV["a\0b"] = 1 },
              t { ENV[1] = "a\0" }, t { ENV["A=B"] = "a\0" }, t { ENV[""] = "a\0" },
              t { ENV.merge!("" => 1) }, t { ENV.merge!("A=B" => "a\0") },
              t { ENV.merge!("" => "x") }, t { ENV.merge!("A=B" => "x") },
              t { ENV[""] = "x" }, t { ENV["A=B"] = "x" }, t { ENV[""] = nil },
              t { ENV["A=B"] = nil }, t { ENV[""] }, t { ENV["A=B"] },
              t { ENV.delete("") }, t { ENV.delete("A=B") }, t { ENV.delete("A=B") { 5 } },
              t { ENV.key?("") }, t { ENV.key?("A=B") }, t { ENV.fetch("A=B", 9) },
              t { ENV.fetch("a\0b") { 1 } }, t { ENV.assoc("") },
            ]
            "##,
        );
    }

    /// Bytes go into the environment and come back as they are, whatever
    /// the encoding the String carries, valid or not (#2136, #2138) —
    /// and a child process inherits them (#2135).
    #[test]
    fn env_bytes_round_trip() {
        let _g = env_lock();
        run_test_once(
            r##"
            def t; yield; rescue Exception => e; [e.class, e.message]; end
            b = "MONORUBY_ENV_\xFFK".b
            r = []
            r << t { (ENV[b] = "v\xFE".b).bytes }
            r << t { [ENV[b].bytes, ENV.fetch(b).bytes, ENV.key?(b), ENV.values_at(b)[0].bytes] }
            r << t { ENV.keys.select { |x| x.b.start_with?("MONORUBY_ENV_\xFF".b) }.map(&:bytes) }
            r << t { ENV.to_h.count { |k, v| k.b == b && v.b == "v\xFE".b } }
            r << t { ENV.to_a.count { |k, v| k.b == b && v.b == "v\xFE".b } }
            r << t { ENV.count { |k, v| k.b == b && v.b == "v\xFE".b } }
            r << t { IO.popen("env", &:read).b.lines.select { |l| l.start_with?(b) }.map(&:bytes) }
            r << t { [ENV.key("v\xFE".b).bytes, ENV.key("v\xFE").bytes, ENV.value?("v\xFE".b)] }
            r << t { ENV.rassoc("v\xFE".b).map(&:bytes) }
            r << t { ENV.assoc(b).map(&:bytes) }
            r << t { ENV.slice(b).to_a.flatten.map(&:bytes) }
            r << t { [ENV.delete(b).bytes, ENV.key?(b)] }
            u = "MONORUBY_ENV_\xFFK"
            r << t { ENV[u] = "v\xFE"; [ENV[u].bytes, ENV.delete(u).bytes, ENV.key?(u)] }
            r << t { ENV.merge!(b => "v\xFE".b); ENV.delete(b).bytes }
            k = "MONORUBY_ENV_BYTES"
            r << t { ENV[k] = "\xC6\xFC".dup.force_encoding("EUC-JP"); ENV[k].bytes }
            r << t { ENV[k] = "日本"; [ENV[k].bytes, ENV.key("日本") == k] }
            r << t { [ENV.value?("日本"), ENV.rassoc("日本")[0] == k] }
            r << t { [ENV.key("日本".b) == k, ENV.value?("日本".b)] }
            r << t { ENV.delete(k).bytes }
            r
            "##,
        );
    }

    /// The encoding a String read from the environment carries: the
    /// locale's, kept even over bytes that are not valid in it, and
    /// BINARY only where the locale itself cannot hold them (CRuby's
    /// `env_str_new`).
    #[test]
    fn env_string_encoding_rule() {
        use super::env_is_binary;
        use crate::Encoding;
        assert!(env_is_binary(Encoding::Ascii8, b"abc"));
        assert!(env_is_binary(Encoding::UsAscii, b"ab\xff"));
        assert!(!env_is_binary(Encoding::UsAscii, b"abc"));
        assert!(!env_is_binary(Encoding::Utf8(0), b"ab\xff"));
        assert!(!env_is_binary(Encoding::Utf8(0), b"abc"));
    }

    /// A variable native code set with bytes that are not UTF-8 in its
    /// *name* is an ordinary variable: read, enumerated, inherited by a
    /// child and deleted (#2135, #2136).
    #[test]
    fn env_native_non_utf8_variable() {
        let _g = env_lock();
        const NAME: &[u8] = b"MONORUBY_ENV_NATIVE\xff";
        crate::environ::set(NAME, b"n\xfe");
        let mut globals = crate::Globals::new_test();
        let got = globals
            .run(
                r##"
                k = "MONORUBY_ENV_NATIVE\xFF".b
                seen = []
                ENV.each { |a, b| seen << b.bytes if a.b == k }
                [
                  ENV[k].bytes, ENV.key?(k), ENV.fetch(k).bytes,
                  ENV.keys.count { |x| x.b == k }, ENV.to_h.count { |a, _| a.b == k },
                  seen, ENV.key("n\xFE".b).bytes == k.bytes,
                  system("true"), IO.popen("env", &:read).b.include?(k + "=n\xFE\n".b),
                  ENV.delete(k).bytes, ENV.key?(k),
                ].inspect
                "##
                .to_string(),
                std::path::Path::new("(test)"),
            )
            .unwrap();
        crate::environ::unset(NAME);
        assert_eq!(
            got.as_str(),
            "[[110, 254], true, [110, 254], 1, 1, [[110, 254]], true, true, true, \
             [110, 254], false]"
        );
    }

    /// `each` / `each_pair` hand the block one `[name, value]` pair (so a
    /// lambda of two parameters is an arity error), the filters two
    /// arguments (#2141).
    #[test]
    fn env_block_arguments() {
        let _g = env_lock();
        run_test_once(
            r##"
            def t; yield; rescue Exception => e; [e.class, e.message]; end
            ENV["MONORUBY_ENV_BLK"] = "1"
            r = [
              t { ENV.each { |x| break x.class } },
              t { ENV.each { |x, y| break [x.class, y.class] } },
              t { ENV.each { |*x| break x.size } },
              t { ENV.each(&->(k, v) { }); 1 },
              t { ENV.each(&->(k) { }); 1 },
              t { ENV.each_pair { |x| break x.class } },
              t { ENV.each_pair { |*x| break x.size } },
              t { ENV.select { |x| break x.class } },
              t { ENV.select { |*x| break x.size } },
              t { ENV.filter { |*x| break x.size } },
              t { ENV.reject { |x| break x.class } },
              t { ENV.reject { |*x| break x.size } },
              t { ENV.delete_if { |x| break x.class } },
              t { ENV.delete_if { |*x| break x.size } },
              t { ENV.keep_if { |*x| break x.size } },
              t { ENV.select! { |*x| break x.size } },
              t { ENV.filter! { |*x| break x.size } },
              t { ENV.reject! { |*x| break x.size } },
              t { ENV.to_h { |*x| break x.size } },
              t { ENV.to_h { |x| break x.class } },
              t { ENV.each_key { |*x| break x.size } },
              t { ENV.each_value { |*x| break x.size } },
              t { ENV.any? { |x| break x.class } },
              t { ENV.map { |x| break x.class } },
              t { ENV.find { |*x| break x.size } },
              t { ENV.to_hash { |k, v| [v, k] }["MONORUBY_ENV_BLK"] },
              t { ENV.to_h { |k, v| [v, k] }["MONORUBY_ENV_BLK"] },
            ]
            ENV.delete("MONORUBY_ENV_BLK")
            r
            "##,
        );
    }

    /// Which String a method answers or yields: the caller's own, its
    /// `to_str` conversion, or a fresh frozen copy (#2141).
    #[test]
    fn env_names_and_values_identity() {
        let _g = env_lock();
        run_test_once(
            r##"
            def t; yield; rescue Exception => e; [e.class, e.message]; end
            n = "MONORUBY_ENV_ID"
            nope = "MONORUBY_ENV_ID_NOPE"
            ENV[n] = "1"
            o = Object.new; def o.to_str = "MONORUBY_ENV_ID"
            q = Object.new; def q.to_str = "MONORUBY_ENV_ID_NOPE"; def q.inspect = "QQ"
            r = [
              t { ENV.fetch(o) }, t { ENV.fetch(q) { |k| k.class } },
              t { ENV.fetch(nope) rescue [$!.class, $!.message, $!.key, $!.receiver.equal?(ENV)] },
              t { ENV.fetch(q) rescue [$!.class, $!.message, $!.key.class] },
              t { ENV.fetch(1, 2) { 3 } },
              t { k = n.dup; ENV.assoc(k)[0].equal?(k) }, t { ENV.assoc(o)[0].class },
              t { ENV.assoc(n).map(&:frozen?) },
              t { k = nope.dup; ENV.delete(k) { |x| x.equal?(k) } },
              t { ENV.delete(q) { |x| x.class } }, t { ENV.delete(o) },
              t { ENV[n] = "1"; v = "2"; (ENV[n] = v).equal?(v) },
              t { ENV.store(n, o).class }, t { ENV[n] },
              t { ENV.to_a.assoc(n).map(&:frozen?) }, t { ENV.invert[n].class },
              t { ENV.values_at(n, nope, o) },
              t { ENV.slice(n, o, nope).keys.map(&:class) },
              t { ENV.slice(n).keys[0].frozen? },
              t { ENV.each_value.to_a.include?(n) },
              t { ENV.each_key.to_a.include?(n) },
            ]
            ENV.delete(n)
            r
            "##,
        );
    }

    /// `merge!` / `update` / `replace` are `[]=` per pair: a `nil` value
    /// deletes, and the block of `merge!` is given the name, the value
    /// the environment holds and the new one (#2141).
    #[test]
    fn env_merge_and_replace_are_aset() {
        let _g = env_lock();
        run_test_once(
            r##"
            def t; yield; rescue Exception => e; [e.class, e.message]; end
            n = "MONORUBY_ENV_MRG"
            ENV[n] = "1"
            r = []
            r << t { ENV.replace(ENV.to_h.merge(n => nil)); ENV.key?(n) }
            r << t { ENV[n] = "1"; ENV.merge!(n => nil); ENV.key?(n) }
            r << t { ENV[n] = "1"; ENV.merge!(1 => nil) { } }
            r << t { ENV.merge!(1 => nil) }
            r << t { ENV.merge!(n => "2") { |k, a, b| [k, a, b].map(&:frozen?).inspect }; ENV[n] }
            r << t { ENV.merge!(n => "2") { |k, a, b| [k, a, b].inspect }; ENV[n] }
            r << t { ENV.merge!(n => "2") { |k, a, b| nil }; ENV[n] }
            r << t { ENV.merge!(n => "3") { |k, a, b| raise "not called" }; ENV[n] }
            r << t { ENV.update({ n => "4" }, { n => "5" }) { |k, a, b| a + b }; ENV[n] }
            ENV.delete(n)
            r
            "##,
        );
    }

    /// The filters look each variable up when its turn comes, so a
    /// variable the block removed or changed meanwhile is skipped or
    /// seen with its new value; `each` and `reject` walk what the
    /// environment held when they started.
    #[test]
    fn env_iteration_while_changing() {
        let _g = env_lock();
        run_test_once(
            r##"
            a = "MONORUBY_ENV_IT_A"
            b = "MONORUBY_ENV_IT_B"
            r = []
            ENV[a] = "1"; ENV[b] = "1"
            seen = []
            ENV.delete_if { |k, v| ENV.delete(k == a ? b : a) if k == a || k == b; seen << k if k == a || k == b; false }
            r << seen.size
            ENV.delete(a); ENV.delete(b)
            ENV[a] = "1"
            seen = []
            ENV.each { |k, v| ENV[b] = "2"; seen << k if k == b }
            r << seen << ENV.delete(b)
            ENV[a] = "1"; ENV[b] = "1"
            r << ENV.select { |k, v| ENV[a] = "changed"; ENV[b] = "changed"; k == a || k == b }.values.sort
            ENV[a] = "1"; ENV[b] = "1"
            r << ENV.filter_map { |k, v| ENV[a] = "changed"; ENV[b] = "changed"; v if k == a || k == b }.sort
            ENV[a] = "1"; ENV[b] = "1"
            r << ENV.reject { |k, v| ENV[a] = "changed"; ENV[b] = "changed"; k != a && k != b }.values.sort
            ENV[a] = "1"; ENV[b] = "1"
            r << ENV.keep_if { |k, v| ENV[a] = "changed"; true }.equal?(ENV)
            r << [ENV.delete(a), ENV.delete(b)]
            r
            "##,
        );
    }

    /// `getenv` can find a variable for a name `unsetenv` rejects —
    /// Darwin's compares up to the `=` (`"NAME=x"` finds NAME), glibc's
    /// finds `"NAME=x"` in the entry `NAME=x=y` — and then `ENV.delete`
    /// and `ENV[name] = nil` raise `unsetenv`'s EINVAL, as on CRuby.
    /// Live: which of these find something is the platform libc's answer.
    #[test]
    fn env_delete_name_with_equals() {
        let _g = env_lock();
        run_test_once_live(
            r##"
            def t; yield; rescue Exception => e; [e.class, e.message]; end
            n = "MONORUBY_ENV_EQ"
            ENV[n] = "1"
            r = [
              t { ENV.delete("#{n}=x") },
              t { ENV["#{n}=x"] = nil },
              t { ENV.delete("#{n}=x") { :blk } },
              ENV[n],
            ]
            ENV[n] = "x=y"
            r << t { ENV.delete("#{n}=x") } << t { ENV.store("#{n}=x", nil) } << ENV[n]
            ENV.delete(n)
            r
            "##,
        );
    }

    /// `ENV.to_h { }` keeps the key and the value the block answered
    /// alive while the key's `#hash` runs — here a collection, then enough
    /// allocation to reuse whatever it freed. Both when the pair Array is
    /// fresh and when the `#hash` empties it, so the value is held by
    /// nothing else.
    #[test]
    fn env_to_h_block_pair_survives_gc_in_hash() {
        let _g = env_lock();
        run_test_once(
            r##"
            class EnvToHKey
              attr_reader :s
              def initialize(s) = @s = s
              def hash
                if @s == "MONORUBY_ENV_TOH"
                  $pair&.clear
                  GC.start
                  $junk = Array.new(3000) { |i| "junk#{i}" }
                end
                @s.hash
              end
              def eql?(o) = o.is_a?(EnvToHKey) && @s == o.s
            end
            check = ->(h) do
              GC.start
              [
                h.find { |k, _| k.s == "MONORUBY_ENV_TOH" }&.last,
                h.count { |_, v| !(v.is_a?(String) && v.start_with?("fresh-")) },
              ]
            end
            ENV["MONORUBY_ENV_TOH"] = "1"
            fresh = ENV.to_h { |k, v| [EnvToHKey.new(k), "fresh-" + v] }
            emptied = ENV.to_h { |k, v| $pair = [EnvToHKey.new(k), "fresh-" + v] }
            ENV.delete("MONORUBY_ENV_TOH")
            [check.(fresh), check.(emptied)]
            "##,
        );
    }

    /// `size` and `empty?` are answered without building anything, and
    /// the enumerators size themselves from the environment.
    #[test]
    fn env_size_and_enumerator_sizes() {
        let _g = env_lock();
        run_test_once(
            r##"
            n = ENV.size
            [
              n == ENV.keys.size, n == ENV.length, n > 0, ENV.empty?,
              ENV.each.size == n, ENV.each_pair.size == n, ENV.each_key.size == n,
              ENV.each_value.size == n, ENV.select.size == n, ENV.reject.size == n,
              ENV.delete_if.size == n, ENV.keep_if.size == n,
              ENV.select!.size == n, ENV.reject!.size == n,
            ]
            "##,
        );
    }

    // -- ENV is a plain Object extended with Enumerable (#2077) ----------

    /// ENV is no Hash: it is an Object whose singleton class has ENV's
    /// own methods and includes Enumerable, and nothing of `Hash`'s.
    #[test]
    fn env_is_an_object_extended_with_enumerable() {
        let _g = env_lock();
        run_test_once(
            r##"
            def t; yield; rescue Exception => e; [e.class, e.message.sub(/0x\h+/, "0x")]; end
            [
              ENV.class, ENV.is_a?(Hash), Hash === ENV, ENV.is_a?(Enumerable),
              ENV.instance_of?(Object), ENV.singleton_class.include?(Enumerable),
              ENV.singleton_class.ancestors.include?(Hash),
              ENV.singleton_methods.sort,
              (ENV.methods - Object.new.methods - ENV.singleton_methods - Enumerable.instance_methods).sort,
              (Enumerable.instance_methods - ENV.methods).sort,
              %i[compare_by_identity default default= transform_values! dig merge to_proc
                 fetch_values flatten deconstruct_keys].map { |m| ENV.respond_to?(m) },
              ENV.method(:map).owner, ENV.method(:each).owner == ENV.singleton_class,
              ENV.method(:first).owner, ENV.method(:sort).owner,
              (case ENV when Hash then :hash when Enumerable then :enum end),
              ENV.frozen?, ENV.instance_variables, ENV == ENV.to_h, ENV == ENV,
              ENV.eql?(ENV), ENV.equal?(ENV), ENV.hash == ENV.hash,
              t { ENV.merge({}) }, t { ENV.dig("PATH") }, t { ENV.default },
              t { ENV.compare_by_identity },
              t { Hash.instance_method(:size).bind_call(ENV) },
              t { Marshal.dump(ENV) }, t { Marshal.dump([ENV]) },
              t { ENV.dup }, t { ENV.clone }, t { ENV.freeze },
              ENV.to_s, ENV.inspect == ENV.to_h.inspect,
              Object.instance_method(:inspect).bind_call(ENV).sub(/0x\h+/, "0x"),
              ObjectSpace.each_object(Hash).any? { |h| h.equal?(ENV) },
              Ractor.shareable?(ENV),
              Enumerable.instance_method(:map).bind_call(ENV) { 1 }.sum == ENV.size,
            ]
            "##,
        );
    }

    /// Enumerable's methods run over `ENV.each`: the `[name, value]`
    /// pairs of the moment.
    #[test]
    fn env_enumerable_methods() {
        let _g = env_lock();
        run_test_once(
            r##"
            k = "MONORUBY_ENV_ENUM"
            ENV[k] = "1"
            n = ENV.size
            r = [
              ENV.first.class, ENV.first(1).size, ENV.entries.size == n,
              ENV.find { |a, b| a == k }, ENV.detect { |a, b| a == k }.class,
              ENV.each_slice(2).first.size, ENV.lazy.map { |a, b| a }.first(1).size,
              ENV.each.class, ENV.each.size == n, ENV.map.size == n,
              ENV.each_with_index.size == n, ENV.sort.size == n, ENV.sort.first.class,
              ENV.min.class, ENV.max(1).class, ENV.zip([1]).first.size,
              ENV.sum([]).size == n * 2, ENV.filter_map { |a, b| b if a == k },
              ENV.to_set.size == n, ENV.count { |a, b| a == k }, ENV.count == n,
              ENV.include?(k), ENV.member?(k), ENV.group_by { |a, b| a == k }[true],
              ENV.partition { |a, b| a == k }[0],
              ENV.each_with_object([]) { |(a, b), acc| acc << b if a == k },
              ENV.inject(0) { |acc, (a, b)| acc + 1 } == n,
              ENV.min_by { |a, b| a == k ? 0 : 1 }, ENV.max_by { |a, b| a == k ? 1 : 0 },
              ENV.each_entry.first.class, ENV.reduce { |a, b| a }.size,
              ENV.to_enum.size == n, ENV.each_pair.with_index.first.flatten.size,
              ENV.any? { |a, b| a == k }, ENV.none? { |a, b| a == k },
              ENV.all? { |a, b| a.is_a?(String) }, ENV.one? { |a, b| a == k },
              ENV.any?, ENV.none?, ENV.tally.class, ENV.uniq.size == n,
              ENV.find_index { |a, b| a == k } == ENV.keys.index(k),
              ENV.find_all { |a, b| a == k }, ENV.flat_map { |a, b| a == k ? [b] : [] },
              ENV.collect { |a, b| a }.size == n, ENV.take_while { true }.size == n,
              ENV.drop(n - 1).size, ENV.each_cons(n).to_a.size,
              ENV.reverse_each.first == ENV.to_a.last, ENV.cycle(2).count == 2 * n,
              ENV.chunk_while { |a, b| true }.to_a.size, ENV.grep(Array).size == n,
              ENV.compact.class, ENV.minmax.size, ENV.to_a.first.map(&:frozen?),
              ENV.sort_by { |a, b| a }.size == n,
            ]
            ENV.delete(k)
            r
            "##,
        );
    }

    /// What takes a Hash takes ENV through `to_hash`, and what takes a
    /// list takes it through `each` (it has no `to_ary`).
    #[test]
    fn env_converts_with_to_hash() {
        let _g = env_lock();
        run_test_once(
            r##"
            def kw(**k) = k.size
            n = ENV.size
            [
              {}.merge(ENV).size == n, {}.update(ENV).size == n, {}.replace(ENV).size == n,
              Hash[ENV].size == n, Hash(ENV).size == n, Hash.try_convert(ENV).class,
              kw(**ENV) == n, {**ENV}.size == n, ({} == ENV), (ENV.to_h == ENV),
              ENV.respond_to?(:to_hash), ENV.respond_to?(:to_ary),
              [ENV].flatten.size, Array(ENV).size == n, [*ENV].size == n,
              system(ENV, "true"), IO.popen(ENV, "echo hi", &:read),
              ("%{PATH}" % ENV rescue $!.class),
            ]
            "##,
        );
    }

    /// `Hash#merge` / `#merge!` / `#update` keep the block's answer and a
    /// non-Hash operand's `to_hash` answer rooted. The block's answer
    /// waited in a Rust local while `aset` ran the key's `hash`, and the
    /// converted Hash was walked while the block and the keys' `hash` ran;
    /// a collection there freed them ("Dead object", or a merge that
    /// silently stopped early).
    #[test]
    fn hash_merge_survives_gc_in_callbacks() {
        run_test_once(
            r##"
            class K
              attr_reader :i
              def initialize(i) = @i = i
              def hash = (GC.start; @i.hash)
              def eql?(o) = o.is_a?(K) && o.i == @i
              def inspect = "K#{@i}"
            end
            class O
              def to_hash = {"a#{1}" => "x" * 30, "b#{2}" => "y" * 30, "c#{3}" => "z" * 30}
            end
            class OK
              def to_hash
                h = {}
                3.times { |i| h[K.new(i)] = "o#{i}" * 10 }
                h
              end
            end
            a = {}
            b = {}
            3.times { |i| a[K.new(i)] = "a#{i}"; b[K.new(i)] = "b#{i}" }
            r = []
            5.times do
              r << a.merge(b) { |_, x, y| x + y * 10 }
              h = a.dup
              h.update(b) { |_, x, y| x + y * 10 }
              r << h
              r << {"a1" => 1, "b2" => 2, "c3" => 3}.merge(O.new) { |_, _, y| GC.start; y.size }
              h = {"a1" => 1, "b2" => 2, "c3" => 3}
              h.merge!(O.new) { |_, _, y| GC.start; y.size }
              r << h
              r << {}.merge(OK.new)
              h = {}
              h.update(OK.new)
              r << h
            end
            r.map { |h| h.map { |k, v| [k.inspect, v] } }.uniq
            "##,
        );
    }

    /// `Hash#values_at` gathers the values in a rooted Array: a value the
    /// default proc made without storing it was held only in a Rust Vec,
    /// and the next key's default proc collected it (its cell then held a
    /// later value).
    #[test]
    fn hash_values_at_survives_gc_in_default_proc() {
        run_test_once(
            r##"
            h = Hash.new { |_, k| GC.start; "v#{k}" * 10 }
            r = nil
            5.times { r = h.values_at(1, 2, 3, 4) }
            [r, h.size]
            "##,
        );
    }

    /// `Hash.[]` builds its result in a rooted Hash. It used a bare
    /// `HashmapInner`, where the frozen copy of an unfrozen String key is
    /// referenced from nowhere else, and it walked what `to_hash` /
    /// `to_ary` answered without rooting it, while the keys' `hash` and the
    /// elements' `to_ary` ran a collection.
    #[test]
    fn hash_bracket_survives_gc_in_callbacks() {
        run_test_once(
            r##"
            class K
              attr_reader :i
              def initialize(i) = @i = i
              def hash = (GC.start; @i.hash)
              def eql?(o) = o.is_a?(K) && o.i == @i
              def inspect = "K#{@i}"
            end
            class TH
              def initialize(i) = @i = i
              def to_hash
                h = {}
                h["key-#{@i}"] = "a" * 10
                h[K.new(1)] = "b" * 10
                h["other-#{@i}"] = "c" * 10
                h[K.new(2)] = "d" * 10
                h
              end
            end
            class TA
              def initialize(i) = @i = i
              def to_ary = [["key-#{@i}", "a" * 10], [K.new(1), "b" * 10], ["other-#{@i}", "c" * 10], [K.new(2), "d" * 10]]
            end
            class P
              def initialize(i) = @i = i
              def to_ary = (GC.start; ["k#{@i}", "v#{@i}" * 10])
            end
            class MyHash < Hash; end
            r = []
            5.times do |i|
              r << Hash["k#{i}", "a", K.new(1), "b", "o#{i}", "c", K.new(2), "d"]
              r << Hash[TH.new(i)]
              r << Hash[TA.new(i)]
              r << Hash[[P.new(1), P.new(2), P.new(3)]]
              ci = {}.compare_by_identity
              ci[+"x#{i}"] = 1
              ci[K.new(5)] = 2
              ci[+"y#{i}"] = 3
              ci[K.new(6)] = 4
              r << Hash[ci]
              m = MyHash[TA.new(i)]
              r << [m.class.name, m.size]
            end
            r.map { |h| h.is_a?(Hash) ? h.map { |k, v| [k.inspect, v] } : h }
            "##,
        );
    }

    /// `Hash#invert` builds its result in a rooted Hash: the frozen copy
    /// of an unfrozen String value turned key was referenced only from a
    /// bare `HashmapInner` while the next key's `hash` ran a collection.
    #[test]
    fn hash_invert_survives_gc_in_key_hash() {
        run_test_once(
            r##"
            class K
              attr_reader :i
              def initialize(i) = @i = i
              def hash = (GC.start; @i.hash)
              def eql?(o) = o.is_a?(K) && o.i == @i
              def inspect = "K#{@i}"
            end
            r = nil
            5.times { r = {a: "x#{1}", b: "y#{2}", c: K.new(3), d: K.new(4)}.invert }
            [r.map { |k, v| [k.inspect, v] }, r.keys.map(&:frozen?)]
            "##,
        );
    }

    /// `Hash#invert` builds its result in a bare map while each new key is
    /// stored without a call, and roots it at the first key that may run
    /// Ruby (#2173). The String keys stored before that are frozen copies
    /// the bare map alone held, so a collection in the declined key's
    /// `hash`, and the allocations after it that would reuse their cells,
    /// must find them in the rooted result. Each round's Strings have
    /// contents of their own: an fstring an earlier result still holds
    /// would keep its copy alive and hide a lost root.
    #[test]
    fn hash_invert_roots_the_native_part_at_the_first_dispatching_key() {
        run_test_once(
            r##"
            class K
              attr_reader :i
              def initialize(i) = @i = i
              def hash = (GC.start; $junk = Array.new(2000) { "junk#{_1}" }; @i.hash)
              def eql?(o) = o.is_a?(K) && o.i == @i
              def inspect = "K#{@i}"
            end
            [0, 1, 3, 12, 39].map do |at|
              h = (0...40).to_h { |i| [i, i == at ? K.new(i) : "s#{at}-#{i}"] }
              r = h.invert
              [r.keys.map(&:inspect).join(","), r.keys.count(&:frozen?), r.values.sum]
            end
            "##,
        );
    }

    /// What the native part of `Hash#invert` stores is what `Hash#[]=`
    /// stores: the fstring of an unfrozen String key, the first of equal
    /// keys with the last value, `0.0` and `-0.0` as one key; and the
    /// result never compares by identity.
    #[test]
    fn hash_invert_keys() {
        run_test_once(
            r##"
            s = "str"
            h = {a: s, b: "str", c: 1, d: 1, e: :sym, f: 1.5, g: nil, h: true,
                 i: "x".encode("UTF-16LE"), j: "y".b, k: 0.0, l: -0.0, m: 2**70, n: 2**70}
            r = h.invert
            ci = {}.compare_by_identity.tap { |x| x["a"] = 1; x["a".dup] = 2 }
            [r.to_a.inspect, r.keys.map(&:frozen?), r.keys[0].equal?(s), r.keys[0].equal?(-"str"),
             r.keys.map { _1.encoding.to_s rescue nil }, r.compare_by_identity?,
             ci.invert.to_a, ci.invert.compare_by_identity?,
             {}.invert, {a: 1}.invert, (0...10).to_h { [_1, _1 % 3] }.invert]
            "##,
        );
    }

    /// `Hash#select` / `#filter` / `#reject` walk a rooted copy of the
    /// receiver's pairs into a rooted result, as CRuby walks a copy of the
    /// receiver. They walked the receiver itself and built the result in a
    /// bare `HashmapInner`: a pair the block replaced or deleted was
    /// referenced from nowhere else, and a collection in a later block call
    /// freed it; a deletion also skipped the next pair, and growth past the
    /// inline form yielded garbage.
    #[test]
    fn hash_select_reject_survive_gc_in_block() {
        run_test_once(
            r##"
            r = []
            5.times do
              h = {}
              6.times { |i| h["k#{i}"] = "v#{i}" * 20 }
              r << h.select { |k, v| h[k] = v.upcase; GC.start; true }
              h = {}
              6.times { |i| h["k#{i}"] = "v#{i}" * 20 }
              r << h.reject { |k, v| h[k] = v.upcase; GC.start; false }
              h = {}
              6.times { |i| h["k#{i}"] = "v#{i}" * 20 }
              r << h.select { |k, v| h.delete(k); GC.start; true }
              h = {}
              6.times { |i| h["k#{i}"] = "v#{i}" * 20 }
              r << h.reject { |k, v| h.clear; GC.start; false }
              h = {}.compare_by_identity
              6.times { |i| h["k#{i}"] = "v#{i}" * 20 }
              s = h.filter { |k, v| h[k] = v.upcase; GC.start; k != "k3" }
              r << [s.values, s.compare_by_identity?]
            end
            h = {a: 1, b: 2}
            r << h.select { |k, v| 20.times { |i| h[:"x#{i}"] = i } if k == :a; true }
            r.uniq
            "##,
        );
    }

    /// `Hash#<` / `#<=` / `#>` / `#>=` keep what the operand's `to_hash`
    /// answered rooted while the values' `==` runs: a collection there
    /// freed the Hash being walked.
    #[test]
    fn hash_subset_survives_gc_in_value_eq() {
        run_test_once(
            r##"
            class V
              attr_reader :s
              def initialize(s) = @s = s
              def ==(o) = (GC.start; o.is_a?(V) && o.s == @s)
            end
            class O2
              def to_hash = {"a#{1}" => V.new("x" * 30), "b#{2}" => V.new("y" * 30), "c#{3}" => V.new("z"), "e" => 1}
            end
            class O3
              def to_hash = {"a#{1}" => V.new("x" * 30), "b#{2}" => V.new("y" * 30)}
            end
            h = {"a1" => V.new("x" * 30), "b2" => V.new("y" * 30), "c3" => V.new("z")}
            5.times.map { [h < O2.new, h <= O2.new, h > O3.new, h >= O3.new] }.uniq
            "##,
        );
    }

    /// `ENV.update` / `ENV.replace` keep the pairs of what the operand's
    /// `to_hash` answered rooted while the block and the names' `to_str`
    /// run: a collection there freed them.
    #[test]
    fn env_update_replace_survive_gc_in_callbacks() {
        let _g = env_lock();
        run_test_once(
            r##"
            names = %w[MONORUBY_GC_A1 MONORUBY_GC_B2 MONORUBY_GC_C3]
            class O
              def to_hash = {"MONORUBY_GC_A#{1}" => "x" * 30, "MONORUBY_GC_B#{2}" => "y" * 30, "MONORUBY_GC_C#{3}" => "z" * 30}
            end
            class S
              def initialize(s) = @s = s
              def to_str = (GC.start; @s)
            end
            class O4
              def initialize(saved) = @saved = saved
              def to_hash = @saved.merge(S.new("MONORUBY_GC_A1") => "p" * 10 + "1", S.new("MONORUBY_GC_B2") => "q" * 10 + "2")
            end
            names.each { |n| ENV[n] = "0" }
            5.times { ENV.update(O.new) { |k, old, new| GC.start; new } }
            r = names.map { |n| ENV[n] }
            saved = ENV.to_h
            5.times { ENV.replace(O4.new(saved)) }
            r << ENV["MONORUBY_GC_A1"] << ENV["MONORUBY_GC_B2"]
            ENV.replace(saved)
            names.each { |n| ENV.delete(n) }
            r
            "##,
        );
    }

    /// `ENV.slice` keeps the value it read on the temp stack while it
    /// stores it under the given name: a name that is not a String runs
    /// its own `hash` there, and a collection freed the fresh value.
    #[test]
    fn env_slice_survives_gc_in_key_hash() {
        let _g = env_lock();
        run_test_once(
            r##"
            class SK
              def initialize(s) = @s = s
              def to_str = @s
              def hash = (GC.start; $junk = Array.new(3000) { |i| "junk#{i}" }; @s.hash)
              def eql?(o) = o.is_a?(SK) && o.to_str == @s
            end
            ENV["MONORUBY_GCE_S"] = "slice-" + "value"
            r = []
            5.times do
              s = ENV.slice(SK.new("MONORUBY_GCE_S"), "MONORUBY_GCE_NONE")
              r << [s.size, s.values, s.values.map(&:frozen?)]
            end
            ENV.delete("MONORUBY_GCE_S")
            r.uniq
            "##,
        );
    }

    /// `ENV.update` / `ENV.replace` keep every pair of the operand on the
    /// temp stack until it is stored. The pairs were copied into a Rust
    /// Vec, and a collection in a name's `to_str` or in the block freed
    /// those that nothing else held: all of a `to_hash` answer's, and
    /// those of a Hash that Ruby code emptied meanwhile.
    #[test]
    fn env_update_replace_survive_gc_when_operand_is_emptied() {
        let _g = env_lock();
        run_test_once(
            r##"
            def junk = ($junk = Array.new(3000) { |i| "junk#{i}" })
            # A name whose `to_str` collects.
            class G
              def initialize(s) = @s = s
              def to_str = (GC.start; junk; @s)
            end
            # A name whose `to_str` empties the Hash it is in (once), and
            # collects.
            class EK
              attr_accessor :h
              def initialize(s) = @s = s
              def to_str = (@h&.clear; @h = nil; GC.start; junk; @s)
            end
            class TH
              def initialize(&b) = @b = b
              def to_hash = @b.call
            end
            # Made in a frame that is gone: the Hash alone holds its values.
            def mk3(k) = {"MONORUBY_GCE_M" + "4" => "new-" + "4", k => "new-" + "3"}.tap { k.h = _1 }
            def mk7(saved, k) = saved.merge("MONORUBY_GCE_" + "R2" => "w" + "2", k => "v" + "1").tap { k.h = _1 }
            r = []
            # A `to_hash` answer while the names' `to_str` collect.
            ENV.update(TH.new { {G.new("MONORUBY_GCE_U1") => "v" + "1", "MONORUBY_GCE_" + "U2" => "w" + "2"} })
            r << [ENV["MONORUBY_GCE_U1"], ENV["MONORUBY_GCE_U2"]]
            ENV.update(TH.new { {G.new("MONORUBY_GCE_U1") => "x" + "1", "MONORUBY_GCE_" + "U2" => "y" + "2"} }) { |_, o, n| o + n }
            r << [ENV["MONORUBY_GCE_U1"], ENV["MONORUBY_GCE_U2"]]
            # The last name's `to_str` empties the Hash before the block
            # runs, or before ENV.replace converts the value.
            ENV["MONORUBY_GCE_M3"] = "old3"
            ENV.update(mk3(EK.new("MONORUBY_GCE_M3"))) { |_, o, n| o + n }
            r << [ENV["MONORUBY_GCE_M3"], ENV["MONORUBY_GCE_M4"]]
            saved = ENV.to_h
            ENV.replace(mk7(saved, EK.new("MONORUBY_GCE_R1")))
            r << [ENV["MONORUBY_GCE_R1"], ENV["MONORUBY_GCE_R2"], ENV.size == saved.size + 2]
            ENV.replace(saved)
            # The block empties the Hash at its first pair. CRuby walks the
            # Hash itself and never reaches the second pair; monoruby walks
            # a copy of the pairs and stores it. Either way, nothing freed
            # is read (no variable is set from a reused cell).
            ENV["MONORUBY_GCE_M1"] = "old1"
            ENV["MONORUBY_GCE_M2"] = "old2"
            h = {"MONORUBY_GCE_M" + "1" => "new-" + "1", "MONORUBY_GCE_M" + "2" => "new-" + "2"}
            ENV.update(h) { |_, o, n| h.clear; GC.start; junk; n }
            r << [ENV["MONORUBY_GCE_M1"], ["old2", "new-2"].include?(ENV["MONORUBY_GCE_M2"])]
            r << ENV.keys.grep(/junk/).size
            %w[U1 U2 M1 M2 M3 M4 R1 R2].each { ENV.delete("MONORUBY_GCE_#{_1}") }
            r
            "##,
        );
    }

    /// `ENV.replace` converts and checks each name before its value, as
    /// `env_replace_i` does, and takes the converted name off the list of
    /// variables to remove: a name that answers `to_str` kept the variable
    /// set only to see it removed at the end, and a bad name was reported
    /// after the value's `to_str` had run.
    #[test]
    fn env_replace_converts_each_name_first() {
        let _g = env_lock();
        run_test_once(
            r##"
            def t; [:ok, yield]; rescue Exception => e; [e.class, e.message]; end
            saved = ENV.to_h
            log = []
            o = Object.new
            o.define_singleton_method(:to_str) { log << :key; "MONORUBY_ENV_RPX" }
            v = Object.new
            v.define_singleton_method(:to_str) { log << :val; "new" }
            r = []
            ENV["MONORUBY_ENV_RPX"] = "old"
            r << t { ENV.replace(saved.merge(o => "new")); ENV["MONORUBY_ENV_RPX"] }
            ENV["MONORUBY_ENV_RPX"] = "old"
            r << t { ENV.replace(saved.merge(o => v)); ENV["MONORUBY_ENV_RPX"] }
            r << log.dup
            [["A\0B", v], ["", v], ["A=B", v], ["MONORUBY_ENV_RPY", "a\0b"], [o, "a\0b"], [o, nil]].each do |k, x|
              log.clear
              r << [t { ENV.replace(saved.merge(k => x)).equal?(ENV) }, log.dup, ENV["MONORUBY_ENV_RPX"]]
            end
            ENV.replace(saved)
            r << ENV.key?("MONORUBY_ENV_RPX") << ENV.key?("MONORUBY_ENV_RPY")
            "##,
        );
    }

    /// `Hash#merge` / `#merge!` / `#update` keep the pair being merged on
    /// the temp stack until it is stored. A key's `hash` (or the `to_int`
    /// of what it answers) that empties the operand left the pair
    /// referenced only from Rust locals, and a collection there freed the
    /// value about to be stored or yielded, or the key itself.
    #[test]
    fn hash_merge_survives_gc_when_key_hash_empties_operand() {
        run_test_once(
            r##"
            # `hash` empties the Hash its key was taken from, once, and
            # collects.
            class K
              attr_accessor :o
              def hash = (o&.clear; self.o = nil; GC.start; $junk = Array.new(200) { [_1] }; 1)
              def eql?(x) = equal?(x)
              def inspect = "K"
            end
            # `hash` answers an object whose `to_int` does that.
            class HV
              def initialize(o) = @o = o
              def to_int = (@o&.clear; @o = nil; GC.start; $junk = Array.new(200) { [_1] }; 1)
            end
            class K2
              attr_accessor :o
              def hash = HV.new(o).tap { self.o = nil }
              def eql?(x) = equal?(x)
              def inspect = "K2"
            end
            class TH
              def initialize(h) = @h = h
              def to_hash = @h
            end
            # The value is referenced only from the Hash made here.
            def mk(k) = {k => "v" * 20}.tap { k.o = _1 }
            # The key is, too: it is made in a frame that is gone.
            $v = "v" * 20
            def mk2 = (k = K2.new; {k => $v}.tap { k.o = _1 })
            def mka = (k = K.new; {[k] => $v}.tap { k.o = _1 })
            5.times.map do
              k1 = K.new
              k2 = K.new
              [
                {}.merge(mk(K.new)), {}.merge!(mk(K.new)),
                {}.merge(mk(K.new)) { |_, x, y| [x, y] },
                {}.update(mk(K.new)) { |_, x, y| [x, y] },
                {k1 => 0}.merge(mk(k1)) { |_, x, y| [x, y] },
                {k2 => 0}.update(mk(k2)) { |_, x, y| [x, y] },
                {}.merge(TH.new(mk(K.new))), {}.update(TH.new(mk(K.new))),
                {}.merge(mk2), {}.update(mka), {}.merge(mka) { |_, x, y| [x, y] },
              ].map(&:inspect)
            end.uniq
            "##,
        );
    }

    /// `Hash#invert` keeps the pair being stored on the temp stack: the
    /// new key's `hash` (or the `to_int` of what it answers) that empties
    /// the receiver left it referenced only from Rust locals, and a
    /// collection there freed the old key or the new one.
    #[test]
    fn hash_invert_survives_gc_when_key_hash_empties_receiver() {
        run_test_once(
            r##"
            class K
              attr_accessor :o
              def hash = (o&.clear; self.o = nil; GC.start; $junk = Array.new(200) { [_1] }; 1)
              def eql?(x) = equal?(x)
              def inspect = "K"
            end
            class HV
              def initialize(o) = @o = o
              def to_int = (@o&.clear; @o = nil; GC.start; $junk = Array.new(200) { [_1] }; 1)
            end
            class K2
              attr_accessor :o
              def hash = HV.new(o).tap { self.o = nil }
              def eql?(x) = equal?(x)
              def inspect = "K2"
            end
            def mk(k) = {"key" * 10 => k}.tap { k.o = _1 }
            $v = "v" * 20
            def mk2 = (k = K2.new; {$v => k}.tap { k.o = _1 })
            def mka = (k = K.new; {$v => [k]}.tap { k.o = _1 })
            5.times.map { [mk(K.new).invert, mk2.invert, mka.invert].map(&:inspect) }.uniq
            "##,
        );
    }

    /// `Hash.[]` keeps the pair being stored on the temp stack, whether it
    /// comes from a Hash, a `to_hash` answer, an Array element or a
    /// `to_ary` answer: a key's `hash` (or the `to_int` of what it
    /// answers) that empties that container left the pair referenced
    /// only from Rust locals, and a collection there freed it.
    #[test]
    fn hash_bracket_survives_gc_when_key_hash_empties_source() {
        run_test_once(
            r##"
            class K
              attr_accessor :o
              def hash = (o&.clear; self.o = nil; GC.start; $junk = Array.new(200) { [_1] }; 1)
              def eql?(x) = equal?(x)
              def inspect = "K"
            end
            class HV
              def initialize(o) = @o = o
              def to_int = (@o&.clear; @o = nil; GC.start; $junk = Array.new(200) { [_1] }; 1)
            end
            class K2
              attr_accessor :o
              def hash = HV.new(o).tap { self.o = nil }
              def eql?(x) = equal?(x)
              def inspect = "K2"
            end
            class TH
              def initialize(h) = @h = h
              def to_hash = @h
            end
            class TA
              def initialize(a) = @a = a
              def to_ary = @a
            end
            def mk(k) = {k => "v" * 20}.tap { k.o = _1 }
            def mkp(k) = [k, "v" * 20].tap { k.o = _1 }
            $v = "v" * 20
            def mk2 = (k = K2.new; {k => $v}.tap { k.o = _1 })
            def mka = (k = K.new; {[k] => $v}.tap { k.o = _1 })
            def mkp2 = (k = K2.new; [k, $v].tap { k.o = _1 })
            5.times.map do
              [
                Hash[mk(K.new)], Hash[TH.new(mk(K.new))], Hash[[mkp(K.new)]],
                Hash[TA.new([mkp(K.new)])], Hash[[TA.new(mkp(K.new))]],
                Hash[mk2], Hash[TH.new(mka)], Hash[[mkp2]],
              ].map(&:inspect)
            end.uniq
            "##,
        );
    }

    /// `Hash#<` / `#<=` / `#>` / `#>=` keep the pair being looked up on
    /// the temp stack: a key's `hash` that empties the Hash being walked
    /// left its value referenced only from a Rust local, and a collection
    /// there freed it before it was compared.
    #[test]
    fn hash_subset_survives_gc_when_key_hash_empties_operand() {
        run_test_once(
            r##"
            class K
              attr_accessor :o
              def hash = (o&.clear; self.o = nil; GC.start; $junk = Array.new(200) { [_1] }; 1)
              def eql?(x) = equal?(x)
              def inspect = "K"
            end
            class TH
              def initialize(h) = @h = h
              def to_hash = @h
            end
            def mk(k) = {k => "v" * 20}.tap { k.o = _1 }
            5.times.map do
              k = K.new
              sup = {k => "v" * 20, x: 1}
              [mk(k) <= sup, mk(k) < sup, sup >= mk(k), sup > mk(k), mk(k) <= TH.new(sup)]
            end.uniq
            "##,
        );
    }

    /// `Hash#merge` / `#merge!` / `#update`, `#select`, `#reject` and
    /// `ENV.update` keep the Proc that a `&obj` block's `to_proc` answered
    /// on the temp stack. It was referenced only from a Rust `ProcData`,
    /// so a collection in an operand's `to_hash` or a key's `hash`, before
    /// or between the yields, freed it and the frame its block closes
    /// over, and the next yield ran on freed memory.
    #[test]
    fn to_proc_block_survives_gc_in_merge_select_reject() {
        let _g = env_lock();
        run_test_once(
            r##"
            class K
              attr_reader :i
              def initialize(i) = @i = i
              def hash = (GC.start; $junk = Array.new(3000) { |j| "junk#{j}" }; @i.hash)
              def eql?(o) = o.is_a?(K) && o.i == @i
            end
            class TP
              def initialize(t) = @t = t
              def to_proc
                tag = @t * 3
                proc { |*a| "#{tag}/#{a.size}" unless tag.start_with?("r") }
              end
            end
            class TH
              def initialize(h) = @h = h
              def to_hash = (GC.start; $junk = Array.new(3000) { |j| "junk#{j}" }; @h)
            end
            a = {K.new(1) => 1, K.new(2) => 2}
            b = {K.new(1) => 10, K.new(2) => 20}
            r = []
            3.times do |i|
              r << a.merge(b, &TP.new("m#{i}")).values
              r << a.dup.merge!(b, &TP.new("b#{i}")).values
              r << {"k" => 1}.update(TH.new({"k" => 2}), &TP.new("u#{i}"))
              r << a.select(&TP.new("s#{i}")).values
              r << a.reject(&TP.new("r#{i}")).values
              ENV["MONORUBY_GC_TO_PROC"] = "old"
              ENV.update(TH.new({"MONORUBY_GC_TO_PROC" => "new"}), &TP.new("e#{i}"))
              r << ENV.delete("MONORUBY_GC_TO_PROC")
            end
            r
            "##,
        );
    }
}
