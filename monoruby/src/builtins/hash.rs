use super::*;

//
// Hash class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_under_obj("Hash", HASH_CLASS, ObjTy::HASH);
    globals.define_builtin_class_func_with_effect(HASH_CLASS, "new", new, 0, 2, Effect::CAPTURE);
    globals.store[HASH_CLASS].set_alloc_func(hash_alloc_func);
    globals.define_builtin_class_func_rest(HASH_CLASS, "[]", hash_bracket);
    globals.define_builtin_class_func(HASH_CLASS, "try_convert", try_convert, 1);

    globals.define_builtin_func_with(HASH_CLASS, "default", default, 0, 1, false);
    globals.define_builtin_func(HASH_CLASS, "default_proc", default_proc, 0);
    globals.define_builtin_func(HASH_CLASS, "default_proc=", default_proc_assign, 1);
    globals.define_builtin_func(HASH_CLASS, "default=", default_assign, 1);
    globals.define_builtin_funcs(HASH_CLASS, "==", &["===", "eql?"], eq, 1);
    globals.define_builtin_func(HASH_CLASS, "<", lt, 1);
    globals.define_builtin_func(HASH_CLASS, "<=", le, 1);
    globals.define_builtin_func(HASH_CLASS, ">", gt, 1);
    globals.define_builtin_func(HASH_CLASS, ">=", ge, 1);
    globals.define_builtin_inline_func(HASH_CLASS, "[]", index, Box::new(hash_index), 1);
    globals.define_builtin_func(HASH_CLASS, "[]=", index_assign, 2);
    globals.define_builtin_func(HASH_CLASS, "clear", clear, 0);
    globals.define_builtin_func(HASH_CLASS, "replace", replace, 1);
    globals.define_builtin_func(HASH_CLASS, "compare_by_identity", compare_by_identity, 0);
    globals.define_builtin_func(HASH_CLASS, "delete", delete, 1);
    globals.define_builtin_funcs(HASH_CLASS, "collect", &["map"], map, 0);
    globals.define_builtin_funcs(HASH_CLASS, "each", &["each_pair"], each, 0);
    globals.define_builtin_func(HASH_CLASS, "each_key", each_key, 0);
    globals.define_builtin_func(HASH_CLASS, "each_value", each_value, 0);
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
    globals.define_builtin_funcs(HASH_CLASS, "size", &["length"], size, 0);
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
    globals.define_builtin_func(HASH_CLASS, "compare_by_identity?", compare_by_identity_, 0);
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
    globals.define_builtin_singleton_func_with(env, "fetch", fetch, 1, 2, false);
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
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/s/new.html]
#[monoruby_builtin]
fn new(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, pc: BytecodePtr) -> Result<Value> {
    let class = lfp.self_val().as_class_id();
    let obj = if let Some(bh) = lfp.block() {
        let default_proc = vm.generate_proc(globals, bh, pc)?;
        Value::hash_with_class_and_default_proc(class, default_proc)
    } else {
        let default = lfp.try_arg(0).unwrap_or_default();
        Value::hash_with_class_and_default(class, default)
    };
    Ok(obj)
}

/// Allocator for `Hash` and its subclasses.
pub(crate) extern "C" fn hash_alloc_func(class_id: ClassId, _: &mut Globals) -> Value {
    Value::hash_with_class_and_default(class_id, Value::nil())
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
    let args = lfp.arg(0).as_array();
    let len = args.len();
    match len {
        0 => Ok(Value::hash(RubyMap::default())),
        1 => {
            let arg = args[0];
            if arg.try_hash_ty().is_some() {
                // Single hash argument: return a copy
                let inner = arg.as_hashmap_inner().clone();
                Ok(Value::hash_from_inner(inner))
            } else if let Some(ary) = arg.try_array_ty() {
                // Single array argument: try to convert [[k,v], ...] to hash
                hash_from_array_pairs(ary.iter().copied(), vm, globals)
            } else {
                // Try to_hash first
                let to_hash_id = IdentId::get_id("to_hash");
                if let Some(result) =
                    vm.invoke_method_if_exists(globals, to_hash_id, arg, &[], None, None)?
                {
                    if result.try_hash_ty().is_some() {
                        let inner = result.as_hashmap_inner().clone();
                        return Ok(Value::hash_from_inner(inner));
                    }
                }
                // Try to_ary
                if let Some(result) =
                    vm.invoke_method_if_exists(globals, IdentId::TO_ARY, arg, &[], None, None)?
                {
                    if let Some(ary) = result.try_array_ty() {
                        return hash_from_array_pairs(ary.iter().copied(), vm, globals);
                    }
                }
                Err(MonorubyErr::argumenterr(
                    "odd number of arguments for Hash".to_string(),
                ))
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
            Ok(Value::hash(map))
        }
    }
}

/// Helper to convert an iterator of values (expected to be [k,v] pairs) into a Hash.
fn hash_from_array_pairs(
    iter: impl Iterator<Item = Value>,
    vm: &mut Executor,
    globals: &mut Globals,
) -> Result<Value> {
    let mut map = RubyMap::default();
    for elem in iter {
        if let Some(pair) = elem.try_array_ty() {
            if pair.len() == 2 {
                map.insert(pair[0], pair[1], vm, globals)?;
            } else {
                return Err(MonorubyErr::argumenterr(format!(
                    "invalid number of elements ({} for 1..2)",
                    pair.len()
                )));
            }
        } else {
            return Err(MonorubyErr::argumenterr(
                "wrong number of arguments (odd number of arguments for Hash)".to_string(),
            ));
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
    _: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    lfp.self_val().ensure_not_frozen(&globals.store)?;
    let arg = lfp.arg(0);
    let mut hash = lfp.self_val().as_hash();
    if arg.is_nil() {
        hash.set_defalut_value(Value::nil());
        Ok(Value::nil())
    } else if let Some(proc) = arg.is_proc() {
        hash.set_defalut_proc(proc);
        Ok(arg)
    } else {
        Err(MonorubyErr::typeerr(format!(
            "wrong default_proc type {} (expected Proc)",
            arg.get_real_class_name(globals)
        )))
    }
}

///
/// ### Hash#default=
///
/// - default=(value)
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/default=3d.html]
#[monoruby_builtin]
fn default_assign(
    _: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    lfp.self_val().ensure_not_frozen(&globals.store)?;
    let default = lfp.arg(0);
    lfp.self_val().as_hash().set_defalut_value(default);
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
    lfp.self_val().ensure_not_frozen(&globals.store)?;
    let key = lfp.arg(0);
    let val = lfp.arg(1);
    lfp.self_val().as_hash().insert(key, val, vm, globals)?;
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
    let h = Hashmap::new(lfp.self_val());
    h.index(vm, globals, key)
}

fn hash_index(
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
    if callsite.pos_num != 1 {
        return false;
    }
    state.load(ir, callsite.args, GP::Rcx);
    state.load(ir, callsite.recv, GP::Rdx);
    let using_xmm = state.get_using_xmm();
    ir.xmm_save(using_xmm);
    ir.inline(|r#gen, _, _| {
        monoasm! {&mut r#gen.jit,
            movq rdi, rbx;
            movq rsi, r12;
            movq rax, (hashindex);
            call rax;
        }
    });
    ir.xmm_restore(using_xmm);
    let error = ir.new_error(state);
    ir.handle_error(error);
    state.def_rax2acc(ir, callsite.dst);
    true
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

///
/// ### Hash#clear
///
/// - clear -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/clear.html]
#[monoruby_builtin]
fn clear(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    lfp.self_val().ensure_not_frozen(&globals.store)?;
    lfp.self_val().as_hash().clear()?;
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
    lfp.self_val().ensure_not_frozen(&globals.store)?;
    let mut self_ = lfp.self_val();
    let arg = lfp.arg(0).coerce_to_hash(vm, globals)?;
    let h = self_.as_hashmap_inner_mut();
    *h = arg.inner().clone();

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
fn clone(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let h = lfp.self_val().as_hashmap_inner().clone();
    Ok(Value::hash_from_inner(h))
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
    lfp.self_val().ensure_not_frozen(&globals.store)?;
    let mut h = lfp.self_val().as_hash();
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
/// ### Enumerable#collect
///
/// - collect {|key, value| ... } -> self
/// - map {|key, value| ... } -> self
/// - collect -> Enumerator
/// - map -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/Enumerable/i/collect.html]
#[monoruby_builtin]
fn map(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, pc: BytecodePtr) -> Result<Value> {
    let bh = match lfp.block() {
        None => {
            let id = IdentId::get_id("collect");
            return vm.generate_enumerator(id, lfp.self_val(), lfp.iter().collect(), pc);
        }
        Some(block) => block,
    };
    let hash = lfp.self_val().as_hash();
    vm.invoke_block_map1(
        globals,
        bh,
        hash.iter().map(|(k, v)| Value::array2(k, v)),
        hash.len(),
    )
}

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
            let id = IdentId::get_id("each");
            return vm.generate_enumerator(id, lfp.self_val(), lfp.iter().collect(), pc);
        }
        Some(block) => block,
    };
    let hash = lfp.self_val().as_hash();
    let data = vm.get_block_data(globals, bh)?;
    let _iter_guard = hash.iter_guard();
    for (k, v) in hash.iter() {
        vm.invoke_block(globals, &data, &[Value::array2(k, v)])?;
    }
    Ok(lfp.self_val())
}

///
/// ### Hash#each_value
///
/// - each_value {|value| ... } -> self
/// - each_value -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/each_value.html]
#[monoruby_builtin]
fn each_value(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    pc: BytecodePtr,
) -> Result<Value> {
    let bh = match lfp.block() {
        None => {
            let id = IdentId::get_id("each_value");
            return vm.generate_enumerator(id, lfp.self_val(), lfp.iter().collect(), pc);
        }
        Some(block) => block,
    };
    let hash = lfp.self_val().as_hash();
    let _iter_guard = hash.iter_guard();
    let iter = hash.iter().map(|(_, v)| v);
    vm.invoke_block_iter1(globals, bh, iter)?;
    Ok(lfp.self_val())
}

///
/// ### Hash#each_key
///
/// - each_key {|value| ... } -> self
/// - each_key -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/each_key.html]
#[monoruby_builtin]
fn each_key(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, pc: BytecodePtr) -> Result<Value> {
    let bh = match lfp.block() {
        None => {
            let id = IdentId::get_id("each_key");
            return vm.generate_enumerator(id, lfp.self_val(), lfp.iter().collect(), pc);
        }
        Some(block) => block,
    };
    let hash = lfp.self_val().as_hash();
    let _iter_guard = hash.iter_guard();
    let iter = hash.iter().map(|(k, _)| k);
    vm.invoke_block_iter1(globals, bh, iter)?;
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
            return vm.generate_enumerator(id, lfp.self_val(), lfp.iter().collect(), pc);
        }
        Some(block) => block,
    };
    let data = vm.get_block_data(globals, bh)?;
    let mut inner = HashmapInner::default();
    for (k, v) in lfp.self_val().as_hash().iter() {
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
    lfp.self_val().ensure_not_frozen(&globals.store)?;
    let bh = match lfp.block() {
        None => {
            let id = IdentId::get_id("select!");
            return vm.generate_enumerator(id, lfp.self_val(), lfp.iter().collect(), pc);
        }
        Some(block) => block,
    };
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
            let hash = self_val.as_hash();
            if hash.len() == 0 {
                return Ok(Value::string("{}".to_string()));
            }
            let mut s = String::from("{");
            let mut first = true;
            for (k, v) in hash.iter() {
                if !first {
                    s.push_str(", ");
                }
                first = false;
                let v_inspect =
                    vm.invoke_method_inner(globals, IdentId::INSPECT, v, &[], None, None)?;
                if let Some(sym) = k.try_symbol() {
                    s.push_str(&format!("{sym}: {}", v_inspect.to_s(&globals.store)));
                } else {
                    let k_inspect =
                        vm.invoke_method_inner(globals, IdentId::INSPECT, k, &[], None, None)?;
                    s.push_str(&format!(
                        "{} => {}",
                        k_inspect.to_s(&globals.store),
                        v_inspect.to_s(&globals.store)
                    ));
                }
            }
            s.push('}');
            Ok(Value::string(s))
        },
        Value::string("{...}".to_string()),
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
            return vm.generate_enumerator(id, lfp.self_val(), lfp.iter().collect(), pc);
        }
        Some(block) => block,
    };
    let h = lfp.self_val().dup();
    let p = vm.get_block_data(globals, bh)?;
    vm.temp_push(h);
    let mut res = Hashmap::new(h);
    for (k, v) in lfp.self_val().expect_hash_ty(globals)?.iter() {
        if vm.invoke_block(globals, &p, &[k, v])?.as_bool() {
            res.remove(k, vm, globals)?;
        }
    }
    let h = vm.temp_pop();
    Ok(h)
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
    lfp.self_val().ensure_not_frozen(&globals.store)?;
    let bh = match lfp.block() {
        None => {
            let id = IdentId::get_id("delete_if");
            return vm.generate_enumerator(id, lfp.self_val(), lfp.iter().collect(), pc);
        }
        Some(block) => block,
    };
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
    lfp.self_val().ensure_not_frozen(&globals.store)?;
    let bh = match lfp.block() {
        None => {
            let id = IdentId::get_id("reject!");
            return vm.generate_enumerator(id, lfp.self_val(), lfp.iter().collect(), pc);
        }
        Some(block) => block,
    };
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
/// - [NOT SUPPORTED] sort {|a, b| ... } -> [object]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/sort.html]
#[monoruby_builtin]
fn sort(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    lfp.expect_no_block()?;
    let hash = lfp.self_val().as_hash();
    let mut ary = hash.keys();
    vm.sort(globals, &mut ary)?;
    let mut res = vec![];
    for k in ary {
        res.push(Value::array2(k, hash.get(k, vm, globals)?.unwrap()))
    }
    Ok(Value::array_from_vec(res))
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
/// - [NOT SUPPORTED]merge(*others) {|key, self_val, other_val| ... } -> Hash
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/merge.html]
#[monoruby_builtin]
fn merge(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    lfp.expect_no_block()?;
    let mut h = lfp.self_val().dup().as_hash();
    for arg in lfp.arg(0).as_array().iter() {
        let other_val = arg.coerce_to_hash(vm, globals)?;
        let other = other_val;
        for (k, v) in other.iter() {
            h.insert(k, v, vm, globals)?;
        }
    }

    Ok(h.into())
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
    lfp.self_val().ensure_not_frozen(&globals.store)?;
    let mut h = lfp.self_val().as_hash();
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
        let mut new_map = RubyMap::default();
        for (k, v) in pairs {
            let result = vm.invoke_block(globals, &data, &[k, v])?;
            let arr = result.expect_array_ty(globals)?;
            if arr.len() != 2 {
                return Err(MonorubyErr::typeerr(
                    "wrong element type (expected array with 2 elements)",
                ));
            }
            new_map.insert(arr[0], arr[1], vm, globals)?;
        }
        Ok(Value::hash(new_map))
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
    let val = lfp
        .self_val()
        .as_hash()
        .get(key, vm, globals)?
        .unwrap_or_default();
    Ok(val)
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
        let mut new_map = RubyMap::default();
        for (k, v) in pairs {
            let result = vm.invoke_block(globals, &data, &[k, v])?;
            let arr = result.expect_array_ty(globals)?;
            if arr.len() != 2 {
                return Err(MonorubyErr::argumenterr(format!(
                    "element has wrong array length (expected 2, was {})",
                    arr.len(),
                )));
            }
            new_map.insert(arr[0], arr[1], vm, globals)?;
        }
        return Ok(Value::hash(new_map));
    }
    let inner = lfp.self_val().as_hashmap_inner().clone();
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
            check_env_key_for_set(&key, &globals.store)?;
            let mut value = coerce_env_string(v, vm, globals)?;

            if let Some(ref data) = block_data {
                let key_v = Value::string(key.clone());
                if let Some(old_v) = lfp.self_val().as_hash().get(key_v, vm, globals)? {
                    let new_v = Value::string(value.clone());
                    let result = vm.invoke_block(globals, data, &[key_v, old_v, new_v])?;
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
            eprintln!("warning: block supersedes default value argument");
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
                return Err(MonorubyErr::keyerr(format!(
                    "key not found: {}",
                    arg0.to_s(&globals.store)
                )));
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
    lfp.self_val().ensure_not_frozen(&globals.store)?;
    let mut h = lfp.self_val().as_hash();
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
    lfp.self_val().ensure_not_frozen(&globals.store)?;
    let bh = match lfp.block() {
        None => {
            let id = IdentId::get_id("keep_if");
            return vm.generate_enumerator(id, lfp.self_val(), lfp.iter().collect(), pc);
        }
        Some(block) => block,
    };
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
        run_test(
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
        );
        run_test("{}");
        run_test(r#"{1=>:ass, 4.5=>"Ruby", [1,2,3]=>{:f=>6}}"#);
        run_test("{}.empty?");
        run_test("{a:1}.empty?");
    }

    #[test]
    fn hash_splat() {
        run_test(r##"h = {a: 1}; {**h}"##);
        run_test(r##"h = {a: 1, b: 2}; {c: 3, **h}"##);
        run_test(r##"h1 = {a: 1}; h2 = {b: 2}; {**h1, **h2}"##);
        run_test(r##"h = {a: 1}; {a: 0, **h}"##);
    }

    #[test]
    fn clear() {
        run_test(r##"a = {a:1,b:2}; a.clear; a[:c] = 100; a"##);
    }

    #[test]
    fn transform_keys() {
        run_test(r##"{a: 1, b: 2}.transform_keys {|k| k.to_s}"##);
        run_test(r##"{a: 1, b: 2}.transform_keys {|k| k.to_s.upcase}"##);
    }

    #[test]
    fn transform_values() {
        run_test(r##"{a: 1, b: 2}.transform_values {|v| v * 10}"##);
        run_test(r##"{a: "x", b: "y"}.transform_values {|v| v.upcase}"##);
    }

    #[test]
    fn replace() {
        run_test(
            r##"
        a1 = {a:1,b:2}
        a2 = {c:3,d:4}
        z = a1.replace(a2)
        a1[:z] = 100
        [a1, a2, z]
        "##,
        );
    }

    #[test]
    fn eq() {
        run_test(r##"{} == {}"##);
        run_test(r##"{a:4} == {a:4}"##);
        run_test(r##"{a:4} == {a:4.0}"##);
        run_test(r##"{a:4} == {a:5}"##);
        run_test(r##"{a:4} == {a:5, b:7}"##);
        run_test(r##"{a:4} == :a"##);
    }

    #[test]
    fn eq_recursive() {
        // Self-referencing hash: h == h should return true, not stack overflow
        run_test("h = {}; h[:a] = h; h == h");
        // Two distinct recursive hashes with same structure
        run_test("a = {}; a[:x] = a; b = {}; b[:x] = b; a == b");
        // Cross-recursive hashes: a contains b, b contains a
        run_test("a = {}; b = {}; a[:x] = b; b[:x] = a; a == b");
        // Recursive hash with non-matching values
        run_test("a = {x: 1}; a[:y] = a; b = {x: 2}; b[:y] = b; a == b");
        // Nested: array inside hash, hash inside array
        run_test("h = {}; a = [h]; h[:a] = a; h == h");
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
        run_test(
            r##"
        a = []
        h = {:ab => "some" , :cd => "all"}
        a << h.delete(:ab) #=> "some"
        a << h.delete(:ef) #=> nil
        a << h.delete(:ef){|key|"#{key} Nothing"} #=> "ef Nothing"
        a
        "##,
        );
    }

    #[test]
    fn each() {
        run_test(
            r##"
        a = []
        {:a=>1, :b=>2, :c=>3}.each {|k, v|
            a << k
            a << v
        }
        a
        "##,
        );
        run_test(
            r##"
        a = []
        {:a=>1, :b=>2, :c=>3}.each {|kv|
            a << kv
        }
        a
        "##,
        );
    }

    #[test]
    fn map() {
        run_test(
            r##"
        {:a=>1, :b=>2, :c=>3}.collect {|k, v|
            k.to_s + v.to_s
        }
        "##,
        );
    }

    #[test]
    fn each_value() {
        run_test(
            r##"
        a = []
        {:a=>1, :b=>2, :c=>3}.each_value {|v|
            a << v
        }
        a
        "##,
        );
        run_test(
            r##"
        a = []
        {:a=>1, :b=>2, :c=>3}.each_key {|k|
            a << k
        }
        a
        "##,
        );
    }

    #[test]
    fn select() {
        run_test(
            r##"
        res = []
        h = { "a" => 100, "b" => 200, "c" => 300 }
        res << h.select {|k,v| k > "a"}  #=> {"b" => 200, "c" => 300}
        res << h.select {|k,v| v < 200}  #=> {"a" => 100}
        res
        "##,
        );
        run_test(
            r##"
        res = []
        h = { "a" => 100, "b" => 200, "c" => 300 }
        res << h.select! {|k,v| k > "a"}  #=> {"b" => 200, "c" => 300}
        res << h
        res
        "##,
        );
    }

    #[test]
    fn assoc() {
        run_test(r##"{a: 1, b: 2, c: 3}.assoc(:a)"##);
        run_test(r##"{a: 1, b: 2, c: 3}.assoc(:b)"##);
        run_test(r##"{a: 1, b: 2, c: 3}.assoc(:z)"##);
        run_test(r##"{"a" => 1, "b" => 2}.assoc("b")"##);
        run_test(r##"{1 => :a, 1.0 => :b}.assoc(1)"##);
    }

    #[test]
    fn rassoc() {
        run_test(r##"{a: 1, b: 2, c: 3}.rassoc(1)"##);
        run_test(r##"{a: 1, b: 2, c: 3}.rassoc(2)"##);
        run_test(r##"{a: 1, b: 2, c: 3}.rassoc(9)"##);
        run_test(r##"{"a" => 1, "b" => 2}.rassoc(2)"##);
    }

    #[test]
    fn invert() {
        run_test(
            r##"
        {5 => "5", 1 => "1", 2 => "2", 3 => "3"}.invert
        "##,
        );
    }

    #[test]
    fn sort() {
        run_test(
            r##"
        {5 => "5", 1 => "1", 2 => "2", 3 => "3"}.sort
        "##,
        );
    }

    #[test]
    fn reject() {
        run_test(
            r##"
        h = { 2 =>"8", 4 =>"6", 6 =>"4", 8 =>"2" }
        h2 = h.reject{|key, value| key.to_i < value.to_i} #=> {6=>"4", 8=>"2"}
        [h, h2]
        "##,
        );
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
        run_test(r##"ENV.fetch("XZCDEWS", "ABC")"##);
        run_test(r##"ENV.fetch("XZCDEWS") {|key| key + "先生"}"##);
        run_test_error(r##"ENV[100]"##);
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
        run_test(r##"ENV.to_h.equal?(ENV)"##);
        run_test(r##"ENV.to_hash.equal?(ENV)"##);
        run_test(r##"ENV.to_h.is_a?(Hash) && !ENV.to_h.equal?(ENV)"##);
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
        run_test(
            r##"
        h = {}
        h[:self] = h
        h.inspect
        "##,
        );
    }

    #[test]
    fn hash_replace_type_check() {
        run_test(
            r##"
        begin
          {}.replace(42)
          false
        rescue TypeError
          true
        end
        "##,
        );
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
        run_test(r#"{1 => "a", 2 => "b", 3 => "c"}"#);
    }

    #[test]
    fn hash_tos_recursive() {
        // Same object appearing multiple times (not recursive)
        run_test(r#"a = [1]; {a:a, b:a}.to_s"#);
        run_test(r#"a = {a:1}; {a:a, b:a}.to_s"#);
        run_test(r#"a = {a:1}; {a:[a], b:a}.to_s"#);
        // Self-containing hash
        run_test(
            r##"
        h = {a: 1}
        h[:self] = h
        h.to_s
        "##,
        );
    }

    #[test]
    fn hash_inspect_user_defined() {
        // User-defined inspect on custom objects inside hash values
        run_test(
            r##"
        class Bar
          def inspect
            "custom_bar"
          end
        end
        {a: Bar.new, b: 1}.inspect
        "##,
        );
    }

    #[test]
    fn hash_inspect() {
        // Empty hash
        run_test(r#"{}.inspect"#);
        run_test(r#"{}.to_s"#);
        // Symbol keys
        run_test(r#"{a: 1, b: 2, c: 3}.inspect"#);
        // String keys
        run_test(r#"{"a" => 1, "b" => 2}.inspect"#);
        // Integer keys
        run_test(r#"{1 => "one", 2 => "two"}.inspect"#);
        // Mixed key types
        run_test(r#"{a: 1, "b" => 2, 3 => :three}.inspect"#);
        // Nested hash
        run_test(r#"{a: {b: {c: 1}}}.inspect"#);
        // Hash containing array
        run_test(r#"{a: [1, 2, 3], b: [4, 5]}.inspect"#);
        // Various value types
        run_test(r#"{a: nil, b: true, c: false, d: 1, e: 2.5, f: "str", g: :sym}.inspect"#);
        // Hash with Range values
        run_test(r#"{a: 1..5, b: 1...5}.inspect"#);
        // to_s is aliased to inspect
        run_test(r#"{a: 1}.to_s"#);
        // User-defined inspect in nested values
        run_test(
            r##"
        class MyVal
          def inspect
            "<val>"
          end
        end
        {a: MyVal.new, b: [MyVal.new]}.inspect
        "##,
        );
        // User-defined inspect as keys
        run_test(
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
        );
    }

    #[test]
    fn hash_new() {
        run_test(
            r##"
        h = Hash.new do |hash, key|
            hash[key] = "foo"
            "bar"
        end

        [h[:a], h[:a], h[:a]]
        "##,
        );
        run_test(
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
        );
        run_test(
            r##"
        res = []
        h = Hash.new{|hash, key| hash[key] ="default"}
        res << h.default
        res << h.default(:some)
        res << h
        res
        "##,
        );
        run_test(
            r##"
        res = []
        h = Hash.new
        res << h.default
        res << h.default(:some)
        res << h
        res
        "##,
        );
        run_test(
            r##"
        res = []
        h = Hash.new {|hash, key| "The #{key} not exist in #{hash.inspect}"}
        res << h.default
        res << h.default_proc.call({}, :foo)
        res << h
        res
        "##,
        );
    }

    #[test]
    fn shift() {
        run_test(
            r##"
        h = {a: 1, b: 2, c: 3}
        res = []
        res << h.shift
        res << h
        res
        "##,
        );
        run_test(
            r##"
        h = {}
        h.shift
        "##,
        );
        run_test(
            r##"
        [Hash.new("default").shift, Hash.new.shift]
        "##,
        );
    }

    #[test]
    fn hash_compare() {
        // <
        run_test(r#"{a: 1} < {a: 1, b: 2}"#);
        run_test(r#"{a: 1, b: 2} < {a: 1, b: 2}"#);
        run_test(r#"{a: 1, b: 2} < {a: 1}"#);
        run_test(r#"{} < {a: 1}"#);
        run_test(r#"{} < {}"#);
        // <=
        run_test(r#"{a: 1} <= {a: 1, b: 2}"#);
        run_test(r#"{a: 1, b: 2} <= {a: 1, b: 2}"#);
        run_test(r#"{a: 1, b: 2} <= {a: 1}"#);
        run_test(r#"{} <= {}"#);
        // >
        run_test(r#"{a: 1, b: 2} > {a: 1}"#);
        run_test(r#"{a: 1, b: 2} > {a: 1, b: 2}"#);
        run_test(r#"{a: 1} > {a: 1, b: 2}"#);
        run_test(r#"{a: 1} > {}"#);
        run_test(r#"{} > {}"#);
        // >=
        run_test(r#"{a: 1, b: 2} >= {a: 1}"#);
        run_test(r#"{a: 1, b: 2} >= {a: 1, b: 2}"#);
        run_test(r#"{a: 1} >= {a: 1, b: 2}"#);
        run_test(r#"{} >= {}"#);
        // different values
        run_test(r#"{a: 1} < {a: 2, b: 2}"#);
        run_test(r#"{a: 1} <= {a: 2}"#);
    }

    #[test]
    fn hash_delete_if() {
        run_test(
            r##"
        h = {a: 1, b: 2, c: 3}
        res = h.delete_if {|k, v| v > 1}
        [h, res.equal?(h)]
        "##,
        );
    }

    #[test]
    fn key() {
        run_test(
            r##"
        h = {a: 1, b: 2, c: 3}
        [h.key(2), h.key(4)]
        "##,
        );
    }

    #[test]
    fn hash_reject_bang() {
        run_test(
            r##"
        h = {a: 1, b: 2, c: 3}
        res1 = h.reject! {|k, v| v > 1}
        h2 = {a: 1}
        res2 = h2.reject! {|k, v| v > 10}
        [h, res1.equal?(h), res2]
        "##,
        );
    }

    #[test]
    fn keep_if() {
        run_test(
            r##"
        h = {a: 1, b: 2, c: 3}
        res = h.keep_if {|k, v| v > 1}
        [res, h, res.equal?(h)]
        "##,
        );
    }

    #[test]
    fn hash_bracket() {
        run_test(r#"Hash[]"#);
        run_test(r#"Hash["a", 1, "b", 2]"#);
        run_test(r#"Hash[{a: 1, b: 2}]"#);
        run_test(r#"Hash[["a", 1], ["b", 2]]"#);
        run_test(
            r##"
        h = Hash["a", 1, "b", 2, "c", 3]
        [h["a"], h["b"], h["c"]]
        "##,
        );
    }

    #[test]
    fn index_splat() {
        run_test(r#"a = ["a", 1, "b", 2]; Hash[*a]"#);
        run_test(
            r##"
        args = ["a", 1, "b", 2, "c", 3]
        h = Hash[*args]
        [h["a"], h["b"], h["c"]]
        "##,
        );
        run_test(r#"a = [1, 2, 3]; [*a]"#);
    }

    #[test]
    fn compare_by_identity_q() {
        run_test("h = {}; h.compare_by_identity?");
        run_test("h = {}; h.compare_by_identity; h.compare_by_identity?");
    }

    #[test]
    fn values_at() {
        run_test(r#"h = {a: 1, b: 2, c: 3}; h.values_at(:a, :c)"#);
        run_test(r#"h = {a: 1, b: 2}; h.values_at(:a, :x, :b)"#);
    }

    #[test]
    fn dig() {
        run_test(r#"h = {a: {b: {c: 1}}}; h.dig(:a, :b, :c)"#);
        run_test(r#"h = {a: {b: 1}}; h.dig(:a, :x)"#);
        run_test(r#"h = {a: 1}; h.dig(:a)"#);
    }

    #[test]
    fn dig_errors() {
        // no arguments
        run_test_error("h = {a: 1}; h.dig");
    }

    #[test]
    fn to_h2() {
        run_test("h = {a: 1, b: 2}; h.to_h == h");
    }

    #[test]
    fn to_h_with_block() {
        run_test("{a: 1, b: 2}.to_h {|k, v| [k, v.to_s] }");
    }

    #[test]
    fn try_convert() {
        run_test("Hash.try_convert({a: 1})");
        run_test("Hash.try_convert(1)");
        run_test("Hash.try_convert(nil)");
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
    fn hash_iter_guard_existing_key_allowed() {
        // Updating an already-present key during iteration is allowed,
        // matching CRuby semantics.
        run_test("h = {a: 1, b: 2}; h.each { |k, v| h[k] = v * 10 }; h.to_a.sort");
    }

    #[test]
    fn hash_iter_guard_delete_allowed() {
        // Hash#delete during iteration does NOT raise (CRuby-compatible).
        // Exact visitation order is implementation-defined, so just check
        // that the call succeeds and returns the pre-delete value.
        run_test(
            "h = {a: 1}; \
             seen = nil; \
             h.each { |k, v| seen = h.delete(k) }; \
             [seen, h.empty?]",
        );
    }

    #[test]
    fn hash_iter_guard_clear_raises() {
        run_test_error("h = {a: 1, b: 2}; h.each { h.clear }");
    }

    #[test]
    fn hash_iter_guard_nested_iteration() {
        // Nested iteration increments iter_lev twice and decrements back to 0;
        // after all iterations complete, mutation is allowed again.
        run_test(
            "h = {a: 1, b: 2}; \
             h.each { |k1, _| h.each { |k2, _| _ = [k1, k2] } }; \
             h[:c] = 3; h.keys.sort",
        );
    }

    #[test]
    fn hash_iter_guard_released_after_block_exception() {
        // If the each block raises, the iter_lev guard is still decremented
        // (RAII Drop) so subsequent mutations succeed.
        run_test(
            "h = {a: 1}; \
             begin; h.each { raise 'stop' }; rescue; end; \
             h[:b] = 2; h.keys.sort",
        );
    }
}
