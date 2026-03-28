use super::*;

//
// Hash class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_under_obj("Hash", HASH_CLASS, ObjTy::HASH);
    globals.define_builtin_class_func_with_effect(HASH_CLASS, "new", new, 0, 1, Effect::CAPTURE);
    globals.define_builtin_class_func(HASH_CLASS, "allocate", allocate, 0);
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
    globals.define_builtin_funcs(HASH_CLASS, "clone", &["dup"], clone, 0);
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
    globals.define_builtin_singleton_func(env, "to_hash", env_to_hash, 0);
}

///
/// ### Hash.new
///
/// - new(ifnone = nil) -> Hash
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/s/new.html]
#[monoruby_builtin]
fn new(vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, pc: BytecodePtr) -> Result<Value> {
    let class = lfp.self_val().as_class_id();
    let obj = if let Some(bh) = lfp.block() {
        let default_proc = vm.generate_proc(bh, pc)?;
        Value::hash_with_class_and_default_proc(class, default_proc)
    } else {
        let default = lfp.try_arg(0).unwrap_or_default();
        Value::hash_with_class_and_default(class, default)
    };
    Ok(obj)
}

/// ### Hash.allocate
/// - allocate -> Hash
///
/// [https://docs.ruby-lang.org/ja/latest/method/Class/i/allocate.html]
#[monoruby_builtin]
fn allocate(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let class_id = lfp.self_val().as_class_id();
    Ok(Value::hash_with_class_and_default(class_id, Value::nil()))
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

/// Try to use a value as a Hash, calling `to_hash` if it doesn't have hash type.
fn coerce_to_hash(vm: &mut Executor, globals: &mut Globals, arg: Value) -> Result<Value> {
    if arg.try_hash_ty().is_some() {
        return Ok(arg);
    }
    let to_hash_id = IdentId::get_id("to_hash");
    if let Some(result) = vm.invoke_method_if_exists(globals, to_hash_id, arg, &[], None, None)? {
        if result.try_hash_ty().is_some() {
            return Ok(result);
        }
        return Err(MonorubyErr::typeerr(format!(
            "can't convert {} to Hash ({}#to_hash gives {})",
            arg.get_real_class_name(globals),
            arg.get_real_class_name(globals),
            result.get_real_class_name(globals),
        )));
    }
    Err(MonorubyErr::no_implicit_conversion(
        &globals.store,
        arg,
        HASH_CLASS,
    ))
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
            "can't convert {} to Hash ({}#to_hash gives {})",
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
fn default_assign(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
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
    let rhs_v = lfp.arg(0);
    let lhs = lfp.self_val().as_hash();
    let rhs = if let Some(rhs) = rhs_v.try_hash_ty() {
        rhs
    } else {
        return Ok(Value::bool(false));
    };
    if lhs.len() != rhs.len() {
        return Ok(Value::bool(false));
    }
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
    let rhs_val = coerce_to_hash(vm, globals, lfp.arg(0))?;
    let rhs = rhs_val.as_hash();
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
    let rhs_val = coerce_to_hash(vm, globals, lfp.arg(0))?;
    let rhs = rhs_val.as_hash();
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
    let rhs_val = coerce_to_hash(vm, globals, lfp.arg(0))?;
    let rhs = rhs_val.as_hash();
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
    let rhs_val = coerce_to_hash(vm, globals, lfp.arg(0))?;
    let rhs = rhs_val.as_hash();
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
    ir.inline(|r#gen, _, _| {
        monoasm! {&mut r#gen.jit,
            movq rdi, rbx;
            movq rsi, r12;
            movq rax, (hashindex);
            call rax;
        }
    });
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
    lfp.self_val().as_hash().clear();
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
    let arg = coerce_to_hash(vm, globals, lfp.arg(0))?;
    let h = self_.as_hashmap_inner_mut();
    *h = arg.as_hashmap_inner().clone();

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
    for (k, v) in lfp.self_val().as_hash().iter() {
        if !vm.invoke_block(globals, &data, &[k, v])?.as_bool() {
            remove.push(k);
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
/// - [NOT SUPPORTED] reject -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/reject.html]
#[monoruby_builtin]
fn reject(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let bh = lfp.expect_block()?;
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
    for (k, v) in lfp.self_val().as_hash().iter() {
        if vm.invoke_block(globals, &data, &[k, v])?.as_bool() {
            remove.push(k);
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
    for (k, v) in lfp.self_val().as_hash().iter() {
        if vm.invoke_block(globals, &data, &[k, v])?.as_bool() {
            remove.push(k);
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
        let other_val = coerce_to_hash(vm, globals, *arg)?;
        let other = other_val.as_hash();
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
            let other_val = coerce_to_hash(vm, globals, *arg)?;
            let other = other_val.as_hash();
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
            let other_val = coerce_to_hash(vm, globals, *arg)?;
            let other = other_val.as_hash();
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
fn compare_by_identity_(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    lfp.expect_no_block()?;
    Ok(Value::bool(lfp.self_val().as_hash().is_compare_by_identity()))
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
        return Err(MonorubyErr::argumenterr("wrong number of arguments (given 0, expected 1+)"));
    }
    let hash = lfp.self_val().as_hash();
    let first_key = args[0];
    let mut val = if let Some(v) = hash.get(first_key, vm, globals)? { v } else { return Ok(Value::nil()); };
    for i in 1..args.len() {
        if val.is_nil() { return Ok(Value::nil()); }
        val = vm.invoke_method_inner(globals, IdentId::get_id("dig"), val, &[args[i]], None, None)?;
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
                return Err(MonorubyErr::typeerr("wrong element type (expected array with 2 elements)"));
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
    if key.is_str().is_none() {
        return Err(MonorubyErr::no_implicit_conversion(
            globals,
            key,
            STRING_CLASS,
        ));
    }
    let val = lfp
        .self_val()
        .as_hash()
        .get(key, vm, globals)?
        .unwrap_or_default();
    Ok(val)
}

#[monoruby_builtin]
fn env_to_hash(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    Ok(lfp.self_val())
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
    let s = if let Some(bh) = lfp.block() {
        if lfp.try_arg(1).is_some() {
            eprintln!("warning: block supersedes default value argument");
        }
        match hash.get(lfp.arg(0), vm, globals)? {
            Some(v) => v,
            None => vm.invoke_block_once(globals, bh, &[lfp.arg(0)])?,
        }
    } else if lfp.try_arg(1).is_none() {
        match hash.get(lfp.arg(0), vm, globals)? {
            Some(v) => v,
            None => {
                return Err(MonorubyErr::keyerr(format!(
                    "key not found: {}",
                    lfp.arg(0).to_s(&globals.store)
                )));
            }
        }
    } else {
        match hash.get(lfp.arg(0), vm, globals)? {
            Some(v) => v,
            None => lfp.arg(1),
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
    for (k, v) in lfp.self_val().as_hash().iter() {
        if !vm.invoke_block(globals, &data, &[k, v])?.as_bool() {
            remove.push(k);
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
        //run_test(r##"ENV["PWD"]"##);
        //run_test(r##"ENV.fetch("PWD")"##);
        run_test(r##"ENV.fetch("XZCDEWS", "ABC")"##);
        run_test(r##"ENV.fetch("XZCDEWS") {|key| key + "先生"}"##);
        run_test_error(r##"ENV[100]"##);
    }

    #[test]
    fn hash_inspect_recursive() {
        run_test_once(
            r##"
        h = {}
        h[:self] = h
        h.inspect
        "##,
        );
    }

    #[test]
    fn hash_replace_type_check() {
        run_test_once(
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
        run_test_once(
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
}
