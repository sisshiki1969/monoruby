use super::*;
#[cfg(target_arch = "aarch64")]
use jitgen::{AbstractState, JitContext};

//
// Hash class
//

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
    globals.define_builtin_class_func_with_kw(
        HASH_CLASS,
        "new",
        new,
        0,
        0,
        true,
        &["capacity"],
        false,
    );
    globals.store[HASH_CLASS].set_alloc_func(hash_alloc_func);
    globals.define_builtin_class_func_rest(HASH_CLASS, "[]", hash_bracket);
    globals.define_builtin_class_func(HASH_CLASS, "try_convert", try_convert, 1);
    globals.define_builtin_class_func(HASH_CLASS, "ruby2_keywords_hash", ruby2_keywords_hash, 1);
    globals.define_builtin_class_func(HASH_CLASS, "ruby2_keywords_hash?", ruby2_keywords_hash_p, 1);

    globals.define_private_builtin_func_with(HASH_CLASS, "initialize", initialize, 0, 1, false);
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
    globals.define_builtin_funcs(HASH_CLASS, "==", &["==="], eq, 1);
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
    globals.define_builtin_func(HASH_CLASS, "__block_splits_pair?", block_splits_pair, 0);
    globals.define_builtin_func(HASH_CLASS, "__iter_begin", iter_begin, 0);
    globals.define_builtin_func(HASH_CLASS, "__iter_end", iter_end, 1);
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
    globals.define_builtin_func(HASH_CLASS, "delete", delete, 1);
    // collect/map: implemented in Ruby (builtins/hash.rb) for arity adaptation and subclass support
    globals.define_builtin_funcs(HASH_CLASS, "each", &["each_pair"], each, 0);
    // each_key / each_value: implemented in Ruby (builtins/hash.rb) on the
    // live positional walk, so a hot call site can inline both the method
    // and the block (same reasoning as `each`).
    globals.define_builtin_funcs(HASH_CLASS, "select", &["filter"], select, 0);
    globals.define_builtin_funcs(HASH_CLASS, "select!", &["filter!"], select_, 0);
    globals.define_builtin_func(HASH_CLASS, "empty?", empty_, 0);
    globals.define_builtin_func_with(HASH_CLASS, "fetch", fetch, 1, 2, false);
    globals.define_builtin_funcs(
        HASH_CLASS,
        "include?",
        &["has_key?", "key?", "member?"],
        include,
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
    globals.define_builtin_func_rest(HASH_CLASS, "dig", dig);
    globals.define_builtin_func(HASH_CLASS, "to_h", to_h, 0);

    let mut env_map = RubyMap::default();
    let mut vm = Executor::default();
    std::env::vars().for_each(|(var, val)| {
        env_map
            .insert(Value::string(var), Value::string(val), &mut vm, globals)
            .unwrap();
    });
    #[cfg(windows)]
    if let None = env_map.get(&Value::string("HOME")) {
        let home_drive = env_map.get(&Value::string("HOMEDRIVE"));
        let home_path = env_map.get(&Value::string("HOMEPATH"));
        let user_profile = env_map.get(&Value::string("USERPROFILE"));
        let home = if home_drive.is_some() && home_drive.is_some() {
            home_drive.unwrap().as_string().unwrap().to_string()
                + home_path.unwrap().as_string().unwrap()
        } else if let Some(up) = user_profile {
            up.as_string().unwrap().to_string()
        } else {
            "".to_string()
        };
        env_map.insert(
            Value::string("HOME"),
            Value::string(home.replace('\\', "/")),
        );
    };

    let env = Value::hash(env_map);
    globals.set_constant_by_str(OBJECT_CLASS, "ENV", env);
    globals.define_builtin_singleton_func_with(env, "fetch", env_fetch, 1, 2, false);
    globals.define_builtin_singleton_func(env, "[]", env_index, 1);
    globals.define_builtin_singleton_func(env, "[]=", env_index_assign, 2);
    globals.define_builtin_singleton_func(env, "store", env_index_assign, 2);
    globals.define_builtin_singleton_func(env, "delete", env_delete, 1);
    globals.define_builtin_singleton_func(env, "to_hash", env_to_hash, 0);
    globals.define_builtin_singleton_func(env, "to_h", env_to_hash, 0);
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
    globals.define_builtin_singleton_func_with(env, "values_at", env_values_at, 0, 0, true);
    globals.define_builtin_singleton_func_with(env, "slice", env_slice, 0, 0, true);
}

///
/// ### Hash.new
///
/// - new(ifnone = nil) -> Hash
/// - new {|hash, key| ... } -> Hash
///
/// Allocates a fresh Hash and forwards `*args` and `&block` to
/// `#initialize`, matching `Class#new` semantics. The default value /
/// default proc / arg validation all live in `Hash#initialize` (Ruby).
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/s/new.html]
#[monoruby_builtin]
fn new(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let class = lfp.self_val().as_class_id();
    let obj = Value::hash_with_class_and_default(class, Value::nil());
    // Forward the positional args verbatim to `#initialize` (arity is
    // validated there, so a subclass can override it). The `capacity:`
    // keyword is bound by the registration and ignored; unknown keywords
    // are already rejected before we get here.
    vm.invoke_method_inner(
        globals,
        IdentId::INITIALIZE,
        obj,
        &lfp.arg(0).as_array(),
        lfp.block(),
        None,
    )?;
    Ok(obj)
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
    method: IdentId,
    lfp: Lfp,
    pc: BytecodePtr,
) -> Result<Value> {
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
    let mut result = match len {
        0 => Value::hash(RubyMap::default()),
        1 => {
            let arg = args[0];
            if arg.try_hash_ty().is_some() {
                // Single hash argument: return a fresh hash with copied entries
                // (do NOT carry over default value/proc or compare_by_identity).
                let mut map = RubyMap::default();
                for (k, v) in arg.as_hash().iter() {
                    map.insert(k, v, vm, globals)?;
                }
                Value::hash(map)
            } else if let Some(ary) = arg.try_array_ty() {
                // Single array argument: try to convert [[k,v], ...] to hash
                hash_from_array_pairs(ary.iter().copied(), vm, globals)?
            } else {
                // Try to_hash first
                let to_hash_id = IdentId::get_id("to_hash");
                if let Some(coerced) =
                    vm.invoke_method_if_exists(globals, to_hash_id, arg, &[], None, None)?
                    && coerced.try_hash_ty().is_some()
                {
                    let mut map = RubyMap::default();
                    for (k, v) in coerced.as_hash().iter() {
                        map.insert(k, v, vm, globals)?;
                    }
                    Value::hash(map)
                } else if let Some(coerced) =
                    vm.invoke_method_if_exists(globals, IdentId::TO_ARY, arg, &[], None, None)?
                    && let Some(ary) = coerced.try_array_ty()
                {
                    hash_from_array_pairs(ary.iter().copied(), vm, globals)?
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
            let mut map = RubyMap::default();
            for i in (0..len).step_by(2) {
                map.insert(args[i], args[i + 1], vm, globals)?;
            }
            Value::hash(map)
        }
    };
    // Tag the result with the calling class so `MyHash[...]` returns a
    // `MyHash`. Do NOT call `#initialize` on the subclass (matches CRuby).
    if class_id != HASH_CLASS {
        result.change_class(class_id);
    }
    Ok(result)
}

/// Helper to convert an iterator of values (expected to be [k,v] pairs) into a Hash.
///
/// `[k, v]` pairs become entries; `[k]` becomes `k => nil` (matching CRuby).
/// Anything else raises `ArgumentError` with the index of the offending element.
fn hash_from_array_pairs(
    iter: impl Iterator<Item = Value>,
    vm: &mut Executor,
    globals: &mut Globals,
) -> Result<Value> {
    let mut map = RubyMap::default();
    for (idx, elem) in iter.enumerate() {
        let Some(pair) = elem.try_array_ty() else {
            // CRuby formats `nil`/`true`/`false` literally and uses class
            // name for everything else.
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
        match pair.len() {
            1 => {
                map.insert(pair[0], Value::nil(), vm, globals)?;
            }
            2 => {
                map.insert(pair[0], pair[1], vm, globals)?;
            }
            n => {
                return Err(MonorubyErr::argumenterr(format!(
                    "invalid number of elements ({} for 1..2)",
                    n
                )));
            }
        }
    }
    Ok(Value::hash(map))
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
            h.get_real_class_name(&globals.store)
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
            h.get_real_class_name(&globals.store)
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
    let arg = lfp.arg(0);
    if arg.try_hash_ty().is_some() {
        return Ok(arg);
    }
    let method = IdentId::get_id("to_hash");
    if let Some(result) = vm.invoke_method_if_exists(globals, method, arg, &[], None, None)? {
        if result.is_nil() {
            return Ok(Value::nil());
        }
        if result.try_hash_ty().is_some() {
            return Ok(result);
        }
        return Err(MonorubyErr::typeerr(format!(
            "can't convert {} into Hash ({}#to_hash gives {})",
            arg.get_real_class_name(globals),
            arg.get_real_class_name(globals),
            result.get_real_class_name(globals),
        )));
    }
    Ok(Value::nil())
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
        Some(key) => {
            if let Some(default_proc) = hash.defalut_proc() {
                vm.invoke_proc(globals, &default_proc, &[lfp.self_val(), key])
            } else {
                Ok(hash.defalut_value().unwrap_or_default())
            }
        }
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
        for (k, lhs_value) in lhs.iter() {
            if let Some(rhs_value) = rhs.get(k, vm, globals)?
                && vm.eq_values_bool(globals, lhs_value, rhs_value)?
            {
                continue;
            } else {
                return Ok(Value::bool(false));
            }
        }
        Ok(Value::bool(true))
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
        for (k, lhs_value) in lhs.iter() {
            let Some(rhs_value) = rhs.get(k, vm, globals)? else {
                return Ok(Value::bool(false));
            };
            let result = vm.invoke_method_inner(
                globals,
                eql_id,
                lhs_value,
                &[rhs_value],
                None,
                None,
            )?;
            if !result.as_bool() {
                return Ok(Value::bool(false));
            }
        }
        Ok(Value::bool(true))
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
    let self_val = lfp.self_val();
    let recursive_marker = Value::integer(0);
    // Outer-recursion collapse (CRuby rb_exec_recursive_outer): a cycle
    // detected at any depth collapses the *outermost* #hash to the
    // sentinel, so `h.hash == {x: h}.hash` when h[:x] = h (h.eql?(x: h)).
    crate::value::exec_recursive_outer(
        self_val.id(),
        || {
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
            let hash_id = IdentId::HASH;
            for (k, v) in h.iter() {
                let kh = vm
                    .invoke_method_inner(globals, hash_id, k, &[], None, None)?
                    .try_fixnum()
                    .unwrap_or(0);
                let vh = vm
                    .invoke_method_inner(globals, hash_id, v, &[], None, None)?
                    .try_fixnum()
                    .unwrap_or(0);
                let kpart = kh.wrapping_add(KEY_OFFSET).wrapping_mul(KEY_MIX);
                let vpart = vh.wrapping_mul(VAL_MIX).rotate_left(13);
                acc = acc.wrapping_add(kpart ^ vpart);
            }
            // Fold into Fixnum range: a Bignum digest would degrade to 0
            // when this hash is itself an element of an outer Hash.
            Ok(Value::from_hash_digest(acc as u64))
        },
        recursive_marker,
    )
}

/// Check if all key-value pairs in `sub` exist in `sup`.
fn hash_subset(
    sub: Hashmap,
    sup: Hashmap,
    vm: &mut Executor,
    globals: &mut Globals,
) -> Result<bool> {
    for (k, sub_value) in sub.iter() {
        if let Some(sup_value) = sup.get(k, vm, globals)?
            && vm.eq_values_bool(globals, sub_value, sup_value)?
        {
            continue;
        } else {
            return Ok(false);
        }
    }
    Ok(true)
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
    let mut key = lfp.arg(0);
    let val = lfp.arg(1);
    // CRuby: when storing into a Hash with a fresh String key, the key
    // is dup'd and frozen so later mutation of the caller's String can't
    // corrupt the hash. Frozen strings are stored as-is; existing-key
    // preservation is handled by the underlying RubyMap. A
    // compare_by_identity hash keys on object identity, so it stores
    // the caller's String as-is (no copy, no freeze).
    if !lfp.self_val().as_hash().is_compare_by_identity() {
        key = key.frozen_hash_key();
    }
    lfp.self_val()
        .as_hash_mut(&globals.store)?
        .insert(key, val, vm, globals)?;
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
    let key = lfp.arg(0);
    let self_val = lfp.self_val();
    let h = Hashmap::new(self_val);
    if let Some(v) = h.get(key, vm, globals)? {
        Ok(v)
    } else {
        // CRuby's `Hash#[]` invokes the `default` *method* on a miss
        // (rb_funcall id_default), so Hash subclasses overriding
        // `#default` are honoured. The default `Hash#default` builtin
        // returns the stored value / runs the default proc, so plain
        // hashes behave exactly as before.
        vm.invoke_method_inner(
            globals,
            IdentId::get_id("default"),
            self_val,
            &[key],
            None,
            None,
        )
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
    _: ClassId,
    _: Option<ClassId>,
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
/// Inline `Hash#[]` as a direct call to `hashindex`, skipping the Ruby method
/// frame.
///
/// Only a receiver whose class is exactly `Hash` inlines. `Hash#[]` consults
/// the `#default` *method* on a miss (CRuby's `rb_funcall(id_default)`), which
/// a subclass — or a singleton — may override; `hashindex` reads the stored
/// default directly, so for those receivers the generic path must stay in
/// charge. A redefinition of `Hash#default` itself is covered by the class
/// version guard emitted ahead of the inliner.
///
fn hash_index(
    state: &mut AbstractState,
    ir: &mut AsmIr,
    _: &JitContext,
    store: &Store,
    callid: CallSiteId,
    recv_class: ClassId,
    _: Option<ClassId>,
) -> bool {
    let callsite = &store[callid];
    if !callsite.is_simple() {
        return false;
    }
    if callsite.pos_num != 1 {
        return false;
    }
    if recv_class != HASH_CLASS {
        return false;
    }
    state.load(ir, callsite.args, GP::Rcx);
    state.load(ir, callsite.recv, GP::Rdx);
    let using_fpr = state.get_using_fpr(ir);
    ir.fpr_save(using_fpr);
    ir.inline(|r#gen, _, _, _| r#gen.emit_call_2args(hashindex as *const () as u64));
    ir.fpr_restore(using_fpr);
    let error = ir.new_error(state);
    ir.handle_error(error);
    state.def_rax2acc(ir, callsite.dst);
    true
}

///
/// Inline `Hash#[]=` as a direct call to `hashindex_assign`, skipping the Ruby
/// method frame.
///
/// Unlike `Hash#[]` this needs no `Hash`-class restriction: the builtin
/// dispatches nothing a subclass could override — a subclass that overrides
/// `#[]=` itself resolves to its own method, never here.
///
fn hash_index_assign(
    state: &mut AbstractState,
    ir: &mut AsmIr,
    _: &JitContext,
    store: &Store,
    callid: CallSiteId,
    _: ClassId,
    _: Option<ClassId>,
) -> bool {
    let callsite = &store[callid];
    if !callsite.is_simple() || callsite.pos_num != 2 {
        return false;
    }
    let CallSiteInfo {
        recv, args, dst, ..
    } = *callsite;
    state.load(ir, args, GP::Rsi);
    state.load(ir, args + 1usize, GP::Rdx);
    state.load(ir, recv, GP::Rdi);
    let using_fpr = state.get_using_fpr(ir);
    ir.fpr_save(using_fpr);
    ir.inline(|r#gen, _, _, _| {
        r#gen.emit_hash_index_assign(hashindex_assign as *const () as u64)
    });
    ir.fpr_restore(using_fpr);
    let error = ir.new_error(state);
    ir.handle_error(error);
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
    _: ClassId,
    _: Option<ClassId>,
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
    _: ClassId,
    _: Option<ClassId>,
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
    _: ClassId,
    _: Option<ClassId>,
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
    _: ClassId,
    _: Option<ClassId>,
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
    _: ClassId,
    _: Option<ClassId>,
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
    _: ClassId,
    idx_class: Option<ClassId>,
) -> bool {
    let callsite = &store[callid];
    if !callsite.is_simple() || callsite.pos_num != 1 || idx_class != Some(INTEGER_CLASS) {
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
    idx_class: Option<ClassId>,
    want_key: bool,
) -> bool {
    let callsite = &store[callid];
    if !callsite.is_simple() || callsite.pos_num != 1 || idx_class != Some(INTEGER_CLASS) {
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
    _: ClassId,
    idx_class: Option<ClassId>,
) -> bool {
    hash_entry_at_inline(state, ir, store, callid, idx_class, true)
}

fn hash_value_at(
    state: &mut AbstractState,
    ir: &mut AsmIr,
    _: &JitContext,
    store: &Store,
    callid: CallSiteId,
    _: ClassId,
    idx_class: Option<ClassId>,
) -> bool {
    hash_entry_at_inline(state, ir, store, callid, idx_class, false)
}

extern "C" fn hashindex(
    vm: &mut Executor,
    globals: &mut Globals,
    base: Value,
    key: Value,
) -> Option<Value> {
    let h = base.as_hash();
    match h.index(vm, globals, key) {
        Ok(v) => Some(v),
        Err(err) => {
            vm.set_error(err);
            None
        }
    }
}

/// `Hash#[]=` for the shapes its inline fast path leaves alone: insertion
/// (which may promote the representation), the boxed map, an eql?-keyed heap
/// key, and the raising cases (frozen receiver, new key during iteration).
/// Mirrors the `index_assign` builtin, including the fresh-String-key
/// dup/freeze, and answers the assigned value.
extern "C" fn hashindex_assign(
    vm: &mut Executor,
    globals: &mut Globals,
    base: Value,
    key: Value,
    val: Value,
) -> Option<Value> {
    let key = if base.as_hash().is_compare_by_identity() {
        key
    } else {
        key.frozen_hash_key()
    };
    let res = base
        .as_hash_mut(&globals.store)
        .and_then(|mut h| h.insert(key, val, vm, globals));
    match res {
        Ok(()) => Some(val),
        Err(err) => {
            vm.set_error(err);
            None
        }
    }
}

///
/// ### Hash#clear
///
/// - clear -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/clear.html]
#[monoruby_builtin]
fn clear(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    lfp.self_val().as_hash_mut(&globals.store)?.clear()?;
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
    let arg = lfp.arg(0).coerce_to_hash(vm, globals)?;
    self_.replace_inner(arg.inner().clone_inner());

    Ok(lfp.self_val())
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
        v.change_class(class_id);
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
            return hash_to_sized_enum(vm, id, lfp, pc);
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
            return hash_to_sized_enum(vm, id, lfp, pc);
        }
        Some(block) => block,
    };
    let data = vm.get_block_data(globals, bh)?;
    let src = lfp.self_val().as_hash();
    let mut inner = HashmapInner::default();
    if src.is_compare_by_identity() {
        inner.compare_by_identity(vm, globals)?;
    }
    for (k, v) in src.iter() {
        if vm.invoke_block(globals, &data, &[k, v])?.as_bool() {
            inner.insert(k, v, vm, globals)?;
        }
    }
    Ok(Value::hash_from_inner(inner))
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
            return hash_to_sized_enum(vm, id, lfp, pc);
        }
        Some(block) => block,
    };
    lfp.self_val().ensure_not_frozen(&globals.store)?;
    let data = vm.get_block_data(globals, bh)?;
    let mut remove = vec![];
    let self_val = lfp.self_val();
    let hash = self_val.as_hash();
    {
        let _iter_guard = hash.iter_guard();
        for (k, v) in hash.iter() {
            if !vm.invoke_block(globals, &data, &[k, v])?.as_bool() {
                remove.push(k);
            }
        }
    }
    let changed = !remove.is_empty();
    let mut h = lfp.self_val().as_hash();
    for k in remove {
        h.remove(k, vm, globals)?;
    }
    Ok(if changed {
        lfp.self_val()
    } else {
        Value::nil()
    })
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
    let self_val = lfp.self_val();
    crate::value::exec_recursive(
        self_val.id(),
        || {
            let escape = crate::builtins::encoding::inspect_escape_nonascii(globals);
            let mk = |s: String| {
                if escape {
                    Value::string_from_inner(RStringInner::from_encoding(
                        crate::value::escape_nonascii_to_u(&s).as_bytes(),
                        Encoding::UsAscii,
                    ))
                } else {
                    Value::string(s)
                }
            };
            let hash = self_val.as_hash();
            if hash.len() == 0 {
                return Ok(mk("{}".to_string()));
            }
            let mut s = String::from("{");
            let mut first = true;
            for (k, v) in hash.iter() {
                if !first {
                    s.push_str(", ");
                }
                first = false;
                let v_str = inspect_value_for_hash(vm, globals, v)?;
                if let Some(sym) = k.try_symbol() {
                    // Hash short form: bare `name:` only for plain-
                    // identifier symbols; operators / `=`-setters /
                    // `@`/`$` / quoted names use `"name":` (stricter
                    // than `Symbol#inspect`).
                    let key_str = crate::value::symbol_hash_label(sym, escape);
                    s.push_str(&format!("{key_str}: {v_str}"));
                } else {
                    let k_str = inspect_value_for_hash(vm, globals, k)?;
                    s.push_str(&format!("{k_str} => {v_str}"));
                }
            }
            s.push('}');
            Ok(mk(s))
        },
        Value::string("{...}".to_string()),
    )
}

/// Return the string to embed for a hash key/value when rendering
/// `Hash#inspect`. Calls Ruby `#inspect`; if the result is not a String,
/// calls Ruby `#to_s` on it, but does not coerce further (no `#to_str`).
/// Per CRuby `rb_hash_inspect`, exceptions raised by `#to_s` propagate.
fn inspect_value_for_hash(
    vm: &mut Executor,
    globals: &mut Globals,
    v: Value,
) -> Result<String> {
    let inspected = vm.invoke_method_inner(globals, IdentId::INSPECT, v, &[], None, None)?;
    if let Some(inner) = inspected.is_rstring_inner() {
        if let Some(esc) = crate::value::escape_unicode_noncompat_component(inner) {
            return Ok(esc);
        }
    }
    if let Some(s) = inspected.is_str() {
        return Ok(s.to_string());
    }
    let to_s_result =
        vm.invoke_method_inner(globals, IdentId::TO_S, inspected, &[], None, None)?;
    if let Some(inner) = to_s_result.is_rstring_inner() {
        if let Some(esc) = crate::value::escape_unicode_noncompat_component(inner) {
            return Ok(esc);
        }
    }
    if let Some(s) = to_s_result.is_str() {
        Ok(s.to_string())
    } else {
        // #to_s returned a non-String; fall back to the default object
        // representation rather than coercing via #to_str.
        Ok(to_s_result.to_s(&globals.store))
    }
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
            return hash_to_sized_enum(vm, id, lfp, pc);
        }
        Some(block) => block,
    };
    // `reject` returns a fresh Hash (NOT the receiver's class, NOT carrying
    // singleton methods, default value, or default_proc).
    let mut inner = HashmapInner::default();
    if lfp.self_val().as_hash().is_compare_by_identity() {
        inner.compare_by_identity(vm, globals)?;
    }
    let p = vm.get_block_data(globals, bh)?;
    for (k, v) in lfp.self_val().as_hash().iter() {
        if !vm.invoke_block(globals, &p, &[k, v])?.as_bool() {
            inner.insert(k, v, vm, globals)?;
        }
    }
    Ok(Value::hash_from_inner(inner))
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
            return hash_to_sized_enum(vm, id, lfp, pc);
        }
        Some(block) => block,
    };
    lfp.self_val().ensure_not_frozen(&globals.store)?;
    let data = vm.get_block_data(globals, bh)?;
    let mut remove = vec![];
    let self_val = lfp.self_val();
    let hash = self_val.as_hash();
    {
        let _iter_guard = hash.iter_guard();
        for (k, v) in hash.iter() {
            if vm.invoke_block(globals, &data, &[k, v])?.as_bool() {
                remove.push(k);
            }
        }
    }
    let mut h = lfp.self_val().as_hash();
    for k in remove {
        h.remove(k, vm, globals)?;
    }
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
            return hash_to_sized_enum(vm, id, lfp, pc);
        }
        Some(block) => block,
    };
    lfp.self_val().ensure_not_frozen(&globals.store)?;
    let data = vm.get_block_data(globals, bh)?;
    let mut remove = vec![];
    let self_val = lfp.self_val();
    let hash = self_val.as_hash();
    {
        let _iter_guard = hash.iter_guard();
        for (k, v) in hash.iter() {
            if vm.invoke_block(globals, &data, &[k, v])?.as_bool() {
                remove.push(k);
            }
        }
    }
    let changed = !remove.is_empty();
    let mut h = lfp.self_val().as_hash();
    for k in remove {
        h.remove(k, vm, globals)?;
    }
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
    if let Some(bh) = lfp.block() {
        let mut pairs: Vec<Value> = hash
            .iter()
            .map(|(k, v)| Value::array2(k, v))
            .collect();
        let data = vm.get_block_data(globals, bh)?;
        // Root the pending pair Arrays: the comparator block runs
        // arbitrary Ruby while they live only in this Rust Vec.
        vm.with_temp_scope(|vm| {
            vm.temp_array_new(pairs.len());
            vm.temp_array_extend_from_slice(&pairs);
            // Stable sort with a comparator block.
            let mut err: Option<MonorubyErr> = None;
            pairs.sort_by(|a, b| {
                if err.is_some() {
                    return std::cmp::Ordering::Equal;
                }
                // Interpret the block's result exactly as `Array#sort` does:
                // an Integer's sign, a non-Integer via its own `<=>` against
                // 0, and `nil` as "not comparable" (ArgumentError).
                match vm
                    .invoke_block(globals, &data, &[*a, *b])
                    .and_then(|r| vm.cmpint(globals, r, *a, *b))
                {
                    Ok(ord) => ord,
                    Err(e) => {
                        err = Some(e);
                        std::cmp::Ordering::Equal
                    }
                }
            });
            if let Some(e) = err {
                return Err(e);
            }
            Ok(Value::array_from_vec(pairs))
        })
    } else {
        let mut keys = hash.keys();
        // Root the merge buffer (see `Array::sort_inner`): the hash keeps
        // the keys alive, but a user-defined `<=>` may mutate the hash
        // mid-sort, and the buffer would then be their only reference.
        let scratch_len = executor::op::merge_scratch_len(keys.len());
        vm.with_temp_scope(|vm| {
            let buf = if scratch_len == 0 {
                std::ptr::null_mut()
            } else {
                let mut scratch = Array::new_from_vec(vec![Value::nil(); scratch_len]);
                vm.temp_push(scratch.into());
                scratch.as_mut_ptr()
            };
            vm.sort(globals, &mut keys, buf)
        })?;
        // Each fresh pair must survive the Ruby `hash`/`eql?` dispatch
        // inside `hash.get` for the following keys — accumulate into a
        // rooted Array instead of a bare Rust Vec.
        vm.with_temp_scope(|vm| {
            vm.temp_array_new(keys.len());
            let idx = vm.temp_len() - 1;
            for &k in keys.iter() {
                let v = hash.get(k, vm, globals)?.unwrap();
                vm.temp_array_push(Value::array2(k, v));
            }
            Ok(vm.temp_at(idx))
        })
    }
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
    for (k, v) in hash.iter() {
        if vm.eq_values_bool(globals, key, k)? {
            return Ok(Value::array_from_vec(vec![k, v]));
        }
    }
    Ok(Value::nil())
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
    for (k, v) in hash.iter() {
        if vm.eq_values_bool(globals, value, v)? {
            return Ok(Value::array_from_vec(vec![k, v]));
        }
    }
    Ok(Value::nil())
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
    let mut map = RubyMap::default();
    for (k, v) in hash.iter() {
        map.insert(v, k, vm, globals)?;
    }
    Ok(Value::hash(map))
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
    let mut h = lfp.self_val().dup().as_hash();
    // The dup is a fresh Hash referenced only by this Rust local; root it
    // across `#to_hash`, the block calls, and the Ruby `hash`/`eql?`
    // dispatches inside get/insert (contrast `merge!`, where `h` is the
    // frame-rooted receiver).
    vm.with_temp_scope(|vm| {
    vm.temp_push(h.into());
    if let Some(block) = lfp.block() {
        let data = vm.get_block_data(globals, block)?;
        for arg in lfp.arg(0).as_array().iter() {
            let other_val = arg.coerce_to_hash(vm, globals)?;
            for (k, other_v) in other_val.iter() {
                if let Some(self_v) = h.get(k, vm, globals)? {
                    let v = vm.invoke_block(globals, &data, &[k, self_v, other_v])?;
                    h.insert(k, v, vm, globals)?;
                } else {
                    h.insert(k, other_v, vm, globals)?;
                }
            }
        }
    } else {
        for arg in lfp.arg(0).as_array().iter() {
            let other_val = arg.coerce_to_hash(vm, globals)?;
            for (k, v) in other_val.iter() {
                h.insert(k, v, vm, globals)?;
            }
        }
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
    let mut h = lfp.self_val().as_hash_mut(&globals.store)?;
    if let Some(block) = lfp.block() {
        let data = vm.get_block_data(globals, block)?;
        for arg in lfp.arg(0).as_array().iter() {
            let other_val = arg.coerce_to_hash(vm, globals)?;
            let other = other_val;
            for (k, other_v) in other.iter() {
                if let Some(self_v) = h.get(k, vm, globals)? {
                    let v = vm.invoke_block(globals, &data, &[k, self_v, other_v])?;
                    h.insert(k, v, vm, globals)?;
                } else {
                    h.insert(k, other_v, vm, globals)?;
                }
            }
        }
    } else {
        for arg in lfp.arg(0).as_array().iter() {
            let other_val = arg.coerce_to_hash(vm, globals)?;
            let other = other_val;
            for (k, v) in other.iter() {
                h.insert(k, v, vm, globals)?;
            }
        }
    }
    Ok(lfp.self_val())
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
    lfp.self_val().ensure_not_frozen(&globals.store)?;
    lfp.expect_no_block()?;
    lfp.self_val().as_hash().compare_by_identity(vm, globals)?;
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
    let mut res = vec![];
    for k in keys.iter() {
        let v = hash.index(vm, globals, *k)?;
        res.push(v);
    }
    Ok(Value::array_from_vec(res))
}

/// ### Hash#dig
#[monoruby_builtin]
fn dig(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let args = lfp.arg(0).as_array();
    if args.is_empty() {
        return Err(MonorubyErr::argumenterr(
            "wrong number of arguments (given 0, expected 1+)",
        ));
    }
    let hash = lfp.self_val().as_hash();
    let first_key = args[0];
    let mut val = if let Some(v) = hash.get(first_key, vm, globals)? {
        v
    } else {
        return Ok(Value::nil());
    };
    for i in 1..args.len() {
        if val.is_nil() {
            return Ok(Value::nil());
        }
        val =
            vm.invoke_method_inner(globals, IdentId::get_id("dig"), val, &[args[i]], None, None)?;
    }
    Ok(val)
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

/// ###ENV.[]
/// - self[key] -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/ENV/s/=5b=5d.html]
#[monoruby_builtin]
fn env_index(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let key = lfp.arg(0);
    let key = if key.is_str().is_some() {
        key
    } else {
        let s = key.coerce_to_str(vm, globals)?;
        Value::string(s)
    };
    match lfp.self_val().as_hash().get(key, vm, globals)? {
        // ENV values are always Strings; CRuby returns a fresh *frozen* copy
        // so a caller can never mutate the live environment through it.
        Some(v) if v.is_str().is_some() => {
            let s = v.expect_string(&globals.store)?;
            let mut frozen = Value::string(s);
            frozen.set_frozen();
            Ok(frozen)
        }
        Some(v) => Ok(v),
        None => Ok(Value::nil()),
    }
}

///
/// ### ENV.fetch
///
/// - fetch(key) -> String
/// - fetch(key, default) -> String
/// - fetch(key) {|key| ... } -> String
///
/// Like `Hash#fetch`, but the key is first coerced to a `String` (via
/// `#to_str`). A key that is not a String and does not respond to `#to_str`
/// raises `TypeError("no implicit conversion of <Class> into String")`,
/// matching CRuby's `ENV.fetch`.
///
/// [https://docs.ruby-lang.org/ja/latest/method/ENV/s/fetch.html]
#[monoruby_builtin]
fn env_fetch(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let arg0 = lfp.arg(0);
    let key = if arg0.is_str().is_some() {
        arg0
    } else {
        let s = arg0.coerce_to_str(vm, globals)?;
        Value::string(s)
    };
    // A coerced key is a fresh String referenced only by this Rust local;
    // root it across the `warn` call and the Ruby dispatches in `get`.
    vm.with_temp_scope(|vm| {
        vm.temp_push(key);
        let hash = lfp.self_val().as_hash();
        let s = if let Some(bh) = lfp.block() {
            if lfp.try_arg(1).is_some() {
                // CRuby's rb_warn: caller-location prefix, straight to
                // $stderr (not the overridable Kernel#warn).
                vm.ruby_warn_caller(globals, "warning: block supersedes default value argument")?;
            }
            match hash.get(key, vm, globals)? {
                Some(v) => v,
                None => vm.invoke_block_once(globals, bh, &[key])?,
            }
        } else if let Some(arg1) = lfp.try_arg(1) {
            match hash.get(key, vm, globals)? {
                Some(v) => v,
                None => arg1,
            }
        } else {
            match hash.get(key, vm, globals)? {
                Some(v) => v,
                None => {
                    return Err(MonorubyErr::keyerr_with(
                        format!("key not found: {}", key.inspect(&globals.store)),
                        lfp.self_val(),
                        key,
                    ));
                }
            }
        };
        Ok(s)
    })
}

///
/// ### ENV.to_hash
/// ### ENV.to_h
///
/// - to_hash -> Hash
/// - to_h -> Hash
/// - to_h {|name, value| block } -> Hash
///
/// Returns a fresh `Hash` snapshot of the environment so callers can
/// mutate it without affecting the live environment. When a block is
/// given, the block must return a 2-element Array `[new_name, new_value]`;
/// otherwise an `ArgumentError` (wrong size) or `TypeError` is raised.
///
/// [https://docs.ruby-lang.org/ja/latest/method/ENV/s/to_h.html]
/// [https://docs.ruby-lang.org/ja/latest/method/ENV/s/to_hash.html]
#[monoruby_builtin]
fn env_to_hash(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    if let Some(bh) = lfp.block() {
        let data = vm.get_block_data(globals, bh)?;
        let hash = lfp.self_val().as_hash();
        let pairs: Vec<(Value, Value)> = hash.iter().collect();
        // Accumulate into a rooted Ruby Hash (not a bare Rust RubyMap):
        // the entries come from the block's fresh return Arrays and must
        // survive the following block calls.
        return vm.with_temp_scope(|vm| {
        vm.temp_push(Value::hash(RubyMap::default()));
        let map_idx = vm.temp_len() - 1;
        for (k, v) in pairs {
            let result = vm.invoke_block(globals, &data, &[k, v])?;
            // CRuby coerces the block's return value to an Array *only* via
            // `#to_ary` (never `#to_a`); anything else raises
            // `TypeError("wrong element type <Class> (expected array)")`.
            let arr = if result.is_array_ty() {
                result.as_array()
            } else {
                let converted = if let Some(func_id) =
                    globals.check_method(result, IdentId::TO_ARY)
                {
                    vm.invoke_func_inner(globals, func_id, result, &[], None, None)?
                } else {
                    Value::nil()
                };
                if converted.is_array_ty() {
                    converted.as_array()
                } else {
                    return Err(MonorubyErr::typeerr(format!(
                        "wrong element type {} (expected array)",
                        result.get_real_class_name(&globals.store),
                    )));
                }
            };
            if arr.len() != 2 {
                return Err(MonorubyErr::argumenterr(format!(
                    "element has wrong array length (expected 2, was {})",
                    arr.len(),
                )));
            }
            vm.temp_at(map_idx).as_hash().insert(arr[0], arr[1], vm, globals)?;
        }
        Ok(vm.temp_at(map_idx))
        });
    }
    let inner = lfp.self_val().as_hashmap_inner().clone_inner();
    Ok(Value::hash_from_inner(inner))
}

/// Coerce a `Value` into an owned `String` for use as an environment variable
/// name or value. Non-String values that do not respond to `to_str` raise a
/// `TypeError`; strings that contain an embedded NUL byte raise an
/// `ArgumentError`, matching CRuby's `ENV` semantics.
fn coerce_env_string(
    v: Value,
    vm: &mut Executor,
    globals: &mut Globals,
) -> Result<String> {
    let s = if v.is_str().is_some() {
        v.expect_string(&globals.store)?
    } else {
        v.coerce_to_str(vm, globals)?
    };
    if s.as_bytes().contains(&0) {
        return Err(MonorubyErr::argumenterr("bare \\0 in env"));
    }
    Ok(s)
}

///
/// ### ENV.[]=
/// ### ENV.store
///
/// - self[name] = value -> value
/// - store(name, value) -> value
///
/// Sets the environment variable named *name* to *value*. If *value*
/// is `nil`, the variable is deleted. Updates both the Ruby-visible
/// hash and libc's `environ` via `setenv(3)` / `unsetenv(3)` so that
/// FFI callers (e.g. `getenv(3)`) observe the change. Raises
/// `Errno::EINVAL` when *name* is empty or contains `'='`, and
/// `TypeError` when *name* or *value* is not a String and does not
/// respond to `#to_str`.
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
    lfp.self_val().ensure_not_frozen(&globals.store)?;
    let key_val = lfp.arg(0);
    let value_val = lfp.arg(1);

    let key = coerce_env_string(key_val, vm, globals)?;

    if value_val.is_nil() {
        // Delete the variable from both the hash and libc's environ.
        // Per CRuby, ENV[invalid_key] = nil is a silent no-op (it does
        // *not* raise EINVAL), matching the spec
        // "does nothing when the key is not a valid environment variable
        //  key and the value is nil".
        let key_v = Value::string(key.clone());
        lfp.self_val().as_hash().remove(key_v, vm, globals)?;
        let c_key = std::ffi::CString::new(key.as_bytes())
            .map_err(|_| MonorubyErr::argumenterr("bare \\0 in env"))?;
        // SAFETY: `c_key` is a NUL-terminated C string whose storage
        // outlives this call. `unsetenv` is thread-safe on Linux.
        unsafe {
            libc::unsetenv(c_key.as_ptr());
        }
        return Ok(Value::nil());
    }

    // Validate the key for setenv: empty string or '=' is rejected with
    // Errno::EINVAL, matching CRuby and `setenv(3)`.
    if let Err(e) = check_env_key_for_set(&key, &globals.store) {
        return Err(e);
    }

    let value = coerce_env_string(value_val, vm, globals)?;

    let c_key = std::ffi::CString::new(key.as_bytes())
        .map_err(|_| MonorubyErr::argumenterr("bare \\0 in env"))?;
    let c_val = std::ffi::CString::new(value.as_bytes())
        .map_err(|_| MonorubyErr::argumenterr("bare \\0 in env"))?;
    // SAFETY: both pointers reference NUL-terminated C strings whose
    // storage outlives this call. `setenv` copies its arguments.
    unsafe {
        libc::setenv(c_key.as_ptr(), c_val.as_ptr(), 1);
    }

    let key_v = Value::string(key);
    let val_v = Value::string(value);
    lfp.self_val()
        .as_hash()
        .insert(key_v, val_v, vm, globals)?;
    // Per spec, ENV.[]= / ENV.store should return the *original* value
    // argument (so that `equal?` on a String literal matches).
    if value_val.is_str().is_some() {
        Ok(value_val)
    } else {
        Ok(val_v)
    }
}

/// Reject environment variable names that `setenv(3)` would refuse:
/// empty strings, and names that contain a literal '=' character.
fn check_env_key_for_set(key: &str, store: &Store) -> Result<()> {
    if key.is_empty() || key.contains('=') {
        let err = std::io::Error::from_raw_os_error(libc::EINVAL);
        return Err(MonorubyErr::from_io_err(
            store,
            &err,
            format!("Invalid argument - setenv({})", key),
        ));
    }
    Ok(())
}

///
/// ### ENV.delete
///
/// - delete(name) -> String | nil
/// - delete(name) {|name| block } -> object
///
/// Removes the environment variable named *name* from both the
/// Ruby-visible hash and libc's `environ` (via `unsetenv(3)`).
/// Returns the previous value, or the block's return value (if a
/// block is given and the variable was not set), or `nil`.
///
/// [https://docs.ruby-lang.org/ja/latest/method/ENV/s/delete.html]
#[monoruby_builtin]
fn env_delete(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    lfp.self_val().ensure_not_frozen(&globals.store)?;
    let key_val = lfp.arg(0);
    let key = coerce_env_string(key_val, vm, globals)?;
    let key_v = Value::string(key.clone());
    // `key_v` is a fresh String referenced only by this Rust local; root
    // it across the Ruby dispatches in `remove` (it is passed to the
    // block afterwards).
    vm.with_temp_scope(|vm| {
        vm.temp_push(key_v);
        let removed = lfp.self_val().as_hash().remove(key_v, vm, globals)?;

        let c_key = std::ffi::CString::new(key.as_bytes())
            .map_err(|_| MonorubyErr::argumenterr("bare \\0 in env"))?;
        // SAFETY: `c_key` is a NUL-terminated C string whose storage outlives
        // this call. `unsetenv` is a no-op if the variable is not set.
        unsafe {
            libc::unsetenv(c_key.as_ptr());
        }

        if removed.is_none()
            && let Some(bh) = lfp.block()
        {
            return vm.invoke_block_once(globals, bh, &[key_v]);
        }
        Ok(removed.unwrap_or_default())
    })
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
/// Coerces *name* with `#to_str` (raising `TypeError` if not coercible)
/// and returns `[name, value]` if the variable is set, otherwise `nil`.
///
/// [https://docs.ruby-lang.org/ja/latest/method/ENV/s/assoc.html]
#[monoruby_builtin]
fn env_assoc(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let key = coerce_env_string(lfp.arg(0), vm, globals)?;
    let key_v = Value::string(key);
    let hash = lfp.self_val().as_hash();
    if let Some(v) = hash.get(key_v, vm, globals)? {
        Ok(Value::array_from_vec(vec![key_v, v]))
    } else {
        Ok(Value::nil())
    }
}

///
/// ### ENV.rassoc
///
/// - rassoc(value) -> [name, value] | nil
///
/// Coerces *value* with `#to_str` and returns `[name, value]` for the
/// first variable whose value equals it, or `nil` if none does. If
/// *value* does not respond to `#to_str`, returns `nil` (no
/// `TypeError` is raised, per spec).
///
/// [https://docs.ruby-lang.org/ja/latest/method/ENV/s/rassoc.html]
#[monoruby_builtin]
fn env_rassoc(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let target = match try_coerce_env_string(lfp.arg(0), vm, globals)? {
        Some(s) => Value::string(s),
        None => return Ok(Value::nil()),
    };
    let hash = lfp.self_val().as_hash();
    for (k, v) in hash.iter() {
        if vm.eq_values_bool(globals, target, v)? {
            return Ok(Value::array_from_vec(vec![k, v]));
        }
    }
    Ok(Value::nil())
}

///
/// ### ENV.key
///
/// - key(value) -> String | nil
///
/// Coerces *value* with `#to_str` (raising `TypeError` if not
/// coercible) and returns the name of the first variable whose value
/// equals it, or `nil` if none does.
///
/// [https://docs.ruby-lang.org/ja/latest/method/ENV/s/key.html]
#[monoruby_builtin]
fn env_key(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let target = Value::string(coerce_env_string(lfp.arg(0), vm, globals)?);
    let hash = lfp.self_val().as_hash();
    for (k, v) in hash.iter() {
        if vm.eq_values_bool(globals, target, v)? {
            return Ok(k);
        }
    }
    Ok(Value::nil())
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
/// Coerces *name* with `#to_str` (raising `TypeError` if not coercible)
/// and returns whether the named variable is set in ENV.
///
/// [https://docs.ruby-lang.org/ja/latest/method/ENV/s/has_key=3f.html]
/// [https://docs.ruby-lang.org/ja/latest/method/ENV/s/include=3f.html]
/// [https://docs.ruby-lang.org/ja/latest/method/ENV/s/key=3f.html]
/// [https://docs.ruby-lang.org/ja/latest/method/ENV/s/member=3f.html]
#[monoruby_builtin]
fn env_has_key(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let key_v = Value::string(coerce_env_string(lfp.arg(0), vm, globals)?);
    let hash = lfp.self_val().as_hash();
    Ok(Value::bool(hash.get(key_v, vm, globals)?.is_some()))
}

///
/// ### ENV.has_value?
/// ### ENV.value?
///
/// - has_value?(value) -> bool | nil
/// - value?(value) -> bool | nil
///
/// Coerces *value* with `#to_str` and returns whether some variable in
/// ENV has it as its value. If *value* does not respond to `#to_str`,
/// returns `nil` (no `TypeError` is raised, per spec).
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
    let target = match try_coerce_env_string(lfp.arg(0), vm, globals)? {
        Some(s) => Value::string(s),
        None => return Ok(Value::nil()),
    };
    let hash = lfp.self_val().as_hash();
    for (_, v) in hash.iter() {
        if vm.eq_values_bool(globals, target, v)? {
            return Ok(Value::bool(true));
        }
    }
    Ok(Value::bool(false))
}

/// Set or unset a single environment variable, propagating to libc's
/// `environ` and updating the Ruby-visible hash. Used as a building
/// block for `ENV.[]=`, `ENV.merge!`, `ENV.update`, `ENV.replace`.
/// `key` must already have passed `check_env_key_for_set`.
fn env_set_one(
    self_val: Value,
    key: &str,
    value: &str,
    vm: &mut Executor,
    globals: &mut Globals,
) -> Result<Value> {
    let c_key = std::ffi::CString::new(key.as_bytes())
        .map_err(|_| MonorubyErr::argumenterr("bare \\0 in env"))?;
    let c_val = std::ffi::CString::new(value.as_bytes())
        .map_err(|_| MonorubyErr::argumenterr("bare \\0 in env"))?;
    // SAFETY: both pointers reference NUL-terminated C strings whose
    // storage outlives this call. `setenv` copies its arguments.
    unsafe {
        libc::setenv(c_key.as_ptr(), c_val.as_ptr(), 1);
    }
    let key_v = Value::string(key.to_string());
    let val_v = Value::string(value.to_string());
    self_val.as_hash().insert(key_v, val_v, vm, globals)?;
    Ok(val_v)
}

fn env_unset_one(
    self_val: Value,
    key: &str,
    vm: &mut Executor,
    globals: &mut Globals,
) -> Result<()> {
    let key_v = Value::string(key.to_string());
    self_val.as_hash().remove(key_v, vm, globals)?;
    let c_key = std::ffi::CString::new(key.as_bytes())
        .map_err(|_| MonorubyErr::argumenterr("bare \\0 in env"))?;
    // SAFETY: `c_key` is a NUL-terminated C string whose storage outlives
    // this call. `unsetenv` is a no-op if the variable is not set.
    unsafe {
        libc::unsetenv(c_key.as_ptr());
    }
    Ok(())
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
/// Iterates each hash argument; for each pair, *name* and *value* are
/// coerced with `#to_str` (`TypeError` if not coercible), *name* is
/// validated (`Errno::EINVAL` if empty or contains `'='`), then the
/// variable is set. When a block is given and the variable already
/// exists, the block is invoked with `(name, old_value, new_value)`
/// and its return value is used as the new value. Returns ENV.
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
    lfp.self_val().ensure_not_frozen(&globals.store)?;
    let block_data = if let Some(bh) = lfp.block() {
        Some(vm.get_block_data(globals, bh)?)
    } else {
        None
    };
    for arg in lfp.arg(0).as_array().iter() {
        let other = arg.coerce_to_hash(vm, globals)?;
        let pairs: Vec<(Value, Value)> = other.iter().collect();
        for (k, v) in pairs {
            let key = coerce_env_string(k, vm, globals)?;
            // CRuby: a `nil` value in the merged hash deletes the
            // variable instead of coercing-then-setting. yjit-bench's
            // `harness-common.rb` relies on this exact form:
            // `ENV.merge!("GEM_HOME" => nil, "GEM_PATH" => nil)`.
            // Without this branch monoruby raises
            // `TypeError: no implicit conversion of NilClass into String`
            // in `coerce_env_string`, breaking every benchmark.
            if v.is_nil() {
                let key_v = Value::string(key.clone());
                lfp.self_val().as_hash().remove(key_v, vm, globals)?;
                if let Ok(c_key) = std::ffi::CString::new(key.as_bytes()) {
                    // SAFETY: NUL-terminated C string; `unsetenv` is
                    // thread-safe on Linux.
                    unsafe {
                        libc::unsetenv(c_key.as_ptr());
                    }
                }
                continue;
            }
            check_env_key_for_set(&key, &globals.store)?;
            let mut value = coerce_env_string(v, vm, globals)?;

            if let Some(ref data) = block_data {
                let key_v = Value::string(key.clone());
                // `key_v` must survive the Ruby dispatches inside `get`
                // — it is passed to the block afterwards.
                let result = vm.with_temp_scope(|vm| {
                    vm.temp_push(key_v);
                    if let Some(old_v) = lfp.self_val().as_hash().get(key_v, vm, globals)? {
                        let new_v = Value::string(value.clone());
                        Ok(Some(vm.invoke_block(globals, data, &[key_v, old_v, new_v])?))
                    } else {
                        Ok(None)
                    }
                })?;
                if let Some(result) = result {
                    value = coerce_env_string(result, vm, globals)?;
                }
            }

            env_set_one(lfp.self_val(), &key, &value, vm, globals)?;
        }
    }
    Ok(lfp.self_val())
}

///
/// ### ENV.replace
///
/// - replace(other_hash) -> ENV
///
/// Replaces the contents of ENV with *other_hash*. Every name / value
/// pair is validated up front (`TypeError` for non-coercible objects,
/// `Errno::EINVAL` for empty names or names containing `'='`), so an
/// invalid pair leaves ENV untouched. Returns ENV.
///
/// [https://docs.ruby-lang.org/ja/latest/method/ENV/s/replace.html]
#[monoruby_builtin]
fn env_replace(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    lfp.self_val().ensure_not_frozen(&globals.store)?;
    let other = lfp.arg(0).coerce_to_hash(vm, globals)?;

    // Phase 1: validate everything up front.
    let mut new_pairs: Vec<(String, String)> = Vec::new();
    for (k, v) in other.iter() {
        let key = coerce_env_string(k, vm, globals)?;
        check_env_key_for_set(&key, &globals.store)?;
        let value = coerce_env_string(v, vm, globals)?;
        new_pairs.push((key, value));
    }

    // Phase 2: drop every current variable from libc + the hash.
    let cur_keys: Vec<String> = lfp
        .self_val()
        .as_hash()
        .iter()
        .filter_map(|(k, _)| k.is_str().map(|s| s.to_string()))
        .collect();
    for k in &cur_keys {
        env_unset_one(lfp.self_val(), k, vm, globals)?;
    }
    // The `iter()` snapshot above is keyed by string content; a hash
    // entry whose key happens not to be a String (in theory unreachable
    // for ENV, but defensive) would be left behind without this clear.
    lfp.self_val().as_hash().clear()?;

    // Phase 3: insert the validated pairs.
    for (key, value) in new_pairs {
        env_set_one(lfp.self_val(), &key, &value, vm, globals)?;
    }
    Ok(lfp.self_val())
}

///
/// ### ENV.values_at
///
/// - values_at(*names) -> [String | nil]
///
/// Returns an Array of values for the given *names*. Each name is
/// coerced via `#to_str` (`TypeError` if not coercible); a name that
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
    let hash = lfp.self_val().as_hash();
    let mut out: Vec<Value> = Vec::with_capacity(args.len());
    for k in args.iter() {
        let key = coerce_env_string(*k, vm, globals)?;
        let key_v = Value::string(key);
        out.push(hash.get(key_v, vm, globals)?.unwrap_or_default());
    }
    Ok(Value::array_from_vec(out))
}

///
/// ### ENV.slice
///
/// - slice(*names) -> Hash
///
/// Returns a Hash containing the names that exist in ENV mapped to
/// their values. Each name argument is coerced via `#to_str` exactly
/// once (`TypeError` if not coercible); the resulting hash uses the
/// *original* argument objects as keys (so a mock that responds to
/// `#to_str` is preserved as a key in the result).
///
/// [https://docs.ruby-lang.org/ja/latest/method/ENV/s/slice.html]
#[monoruby_builtin]
fn env_slice(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let args = lfp.arg(0).as_array();
    let hash = lfp.self_val().as_hash();
    let mut map = RubyMap::default();
    for k in args.iter() {
        let s = coerce_env_string(*k, vm, globals)?;
        let s_v = Value::string(s);
        if let Some(v) = hash.get(s_v, vm, globals)? {
            map.insert(*k, v, vm, globals)?;
        }
    }
    Ok(Value::hash(map))
}

/// Like `coerce_env_string`, but returns `Ok(None)` instead of raising
/// `TypeError` when the value is neither a String nor responds to
/// `#to_str`. Used by `ENV.rassoc` / `ENV.has_value?` per their specs.
fn try_coerce_env_string(
    v: Value,
    vm: &mut Executor,
    globals: &mut Globals,
) -> Result<Option<String>> {
    if v.is_str().is_some() {
        let s = v.expect_string(&globals.store)?;
        if s.as_bytes().contains(&0) {
            return Err(MonorubyErr::argumenterr("bare \\0 in env"));
        }
        return Ok(Some(s));
    }
    if globals.check_method(v, IdentId::TO_STR).is_some() {
        let s = v.coerce_to_str(vm, globals)?;
        if s.as_bytes().contains(&0) {
            return Err(MonorubyErr::argumenterr("bare \\0 in env"));
        }
        return Ok(Some(s));
    }
    Ok(None)
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
    for (k, v) in hash.iter() {
        if vm.eq_values_bool(globals, v, target)? {
            return Ok(k);
        }
    }
    Ok(Value::nil())
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
            return hash_to_sized_enum(vm, id, lfp, pc);
        }
        Some(block) => block,
    };
    lfp.self_val().ensure_not_frozen(&globals.store)?;
    let data = vm.get_block_data(globals, bh)?;
    let mut remove = vec![];
    let self_val = lfp.self_val();
    let hash = self_val.as_hash();
    {
        let _iter_guard = hash.iter_guard();
        for (k, v) in hash.iter() {
            if !vm.invoke_block(globals, &data, &[k, v])?.as_bool() {
                remove.push(k);
            }
        }
    }
    let mut h = lfp.self_val().as_hash();
    for k in remove {
        h.remove(k, vm, globals)?;
    }
    Ok(lfp.self_val())
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

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

    /// Verify that `ENV[]=` propagates to libc's `environ`, so that FFI
    /// callers of `getenv(3)` observe the value. This is what was
    /// previously broken.
    #[test]
    fn env_assign_propagates_to_libc_setenv() {
        let _g = env_lock();
        use std::ffi::{CStr, CString};
        let key = "MONORUBY_ENV_TEST_LIBC_PROP";
        let c_key = CString::new(key).unwrap();
        // Make sure the variable is not present before the test runs.
        unsafe { libc::unsetenv(c_key.as_ptr()) };

        // Run a short Ruby script that sets the variable via `ENV[]=`.
        let mut globals = crate::Globals::new_test();
        let src = format!(r#"ENV["{key}"] = "hello""#);
        let _ = globals.run(src, std::path::Path::new("(test)"));

        // libc `getenv` should now see "hello".
        let got = unsafe { libc::getenv(c_key.as_ptr()) };
        assert!(!got.is_null(), "getenv returned NULL after ENV[]=");
        let s = unsafe { CStr::from_ptr(got) }.to_str().unwrap();
        assert_eq!(s, "hello");

        // ENV[] = nil should remove it from libc as well.
        let src = format!(r#"ENV["{key}"] = nil"#);
        let _ = globals.run(src, std::path::Path::new("(test)"));
        let got = unsafe { libc::getenv(c_key.as_ptr()) };
        assert!(got.is_null(), "getenv should return NULL after ENV[]=nil");
    }

    #[test]
    fn env_delete_propagates_to_libc_unsetenv() {
        let _g = env_lock();
        use std::ffi::CString;
        let key = "MONORUBY_ENV_TEST_LIBC_DEL";
        let c_key = CString::new(key).unwrap();
        unsafe { libc::unsetenv(c_key.as_ptr()) };

        let mut globals = crate::Globals::new_test();
        let src = format!(
            r#"ENV["{key}"] = "bye"; ENV.delete("{key}")"#
        );
        let _ = globals.run(src, std::path::Path::new("(test)"));
        let got = unsafe { libc::getenv(c_key.as_ptr()) };
        assert!(got.is_null(), "getenv should return NULL after ENV.delete");
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

    /// Per spec: `ENV[invalid_key] = nil` is a silent no-op (does *not*
    /// raise EINVAL) so library code can clear keys defensively.
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
    fn hash_iter_guard_clear_raises() {
        run_test_error("h = {a: 1, b: 2}; h.each { h.clear }");
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
    /// `hashindex` reads the stored default directly.
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
}
