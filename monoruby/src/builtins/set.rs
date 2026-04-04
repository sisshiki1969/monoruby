use super::*;

//
// Set class
//
// Internally, a Set is a Hash where keys are the set elements and values are `true`.
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_under_obj("Set", SET_CLASS, ObjTy::HASH);
    globals.define_builtin_class_func_rest(SET_CLASS, "[]", set_index);
    globals.define_builtin_class_func_with(SET_CLASS, "new", new, 0, 1, false);
    globals.define_builtin_class_func(SET_CLASS, "allocate", allocate, 0);

    globals.define_builtin_func(SET_CLASS, "<<", add, 1);
    globals.define_builtin_func(SET_CLASS, "add", add, 1);
    globals.define_builtin_func(SET_CLASS, "add?", add_q, 1);
    globals.define_builtin_funcs(SET_CLASS, "===", &["include?", "member?"], teq, 1);
    globals.define_builtin_funcs(SET_CLASS, "size", &["length"], size, 0);
    globals.define_builtin_func(SET_CLASS, "empty?", empty_, 0);
    globals.define_builtin_func(SET_CLASS, "clear", clear, 0);
    globals.define_builtin_func(SET_CLASS, "delete", delete, 1);
    globals.define_builtin_func(SET_CLASS, "delete?", delete_q, 1);
    globals.define_builtin_func(SET_CLASS, "each", each, 0);
    globals.define_builtin_func(SET_CLASS, "to_a", to_a, 0);
    // inspect/to_s defined in Ruby (builtins/builtins.rb) for cycle detection
    globals.define_builtin_func(SET_CLASS, "to_set", to_set, 0);
    globals.define_builtin_funcs_with_kw(SET_CLASS, "dup", &["clone"], dup, 0, 1, false, &[], false);
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
}

/// Create a new empty Set (a Hash with class SET_CLASS).
fn new_empty_set() -> Value {
    Value::hash_with_class_and_default(SET_CLASS, Value::nil())
}

/// Create a Set from an iterator of values.
fn set_from_iter(
    iter: impl Iterator<Item = Value>,
    vm: &mut Executor,
    globals: &mut Globals,
) -> Result<Value> {
    let mut set = new_empty_set();
    let inner = set.as_hashmap_inner_mut();
    for v in iter {
        inner.insert(v, Value::bool(true), vm, globals)?;
    }
    Ok(set)
}

/// Get the keys of a Set's internal hash as a Vec.
fn set_keys(val: Value) -> Vec<Value> {
    val.as_hashmap_inner().keys()
}

/// Check if a value is a Set.
fn is_set(val: Value, store: &Store) -> bool {
    val.is_kind_of(store, SET_CLASS)
}

/// Convert an enumerable Value to a Vec of elements.
fn enum_to_vec(vm: &mut Executor, globals: &mut Globals, val: Value) -> Result<Vec<Value>> {
    if is_set(val, &globals.store) {
        return Ok(set_keys(val));
    }
    if let Some(ary) = val.try_array_ty() {
        return Ok(ary.iter().copied().collect());
    }
    // Check if it responds to each (is enumerable)
    let each_id = IdentId::get_id("each");
    if globals.check_method(val, each_id).is_none() {
        return Err(MonorubyErr::argumenterr(format!(
            "value must be enumerable"
        )));
    }
    let result = vm.invoke_method_inner(globals, IdentId::TO_A, val, &[], None, None)?;
    let ary = result.expect_array_ty(globals)?;
    Ok(ary.iter().copied().collect())
}

/// ### Set.allocate
#[monoruby_builtin]
fn allocate(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let class_id = lfp.self_val().as_class_id();
    Ok(Value::hash_with_class_and_default(class_id, Value::nil()))
}

///
/// ### Set.[]
///
/// - Set[*ary -> Set
///
/// [https://docs.ruby-lang.org/ja/latest/method/Set/s/=5b=5d.html]
#[monoruby_builtin]
fn set_index(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let args = lfp.arg(0).as_array();
    set_from_iter(args.iter().copied(), vm, globals)
}

///
/// ### Set.new
///
/// - new(enum = nil) -> Set
/// - new(enum) {|o| block } -> Set
///
/// [https://docs.ruby-lang.org/ja/latest/method/Set/s/new.html]
#[monoruby_builtin]
fn new(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _pc: BytecodePtr) -> Result<Value> {
    let enum_val = lfp.try_arg(0);
    match enum_val {
        Some(val) if !val.is_nil() => {
            let elems = enum_to_vec(vm, globals, val)?;
            if let Some(bh) = lfp.block() {
                let data = vm.get_block_data(globals, bh)?;
                let mut set = new_empty_set();
                for elem in elems {
                    let mapped = vm.invoke_block(globals, &data, &[elem])?;
                    set.as_hashmap_inner_mut()
                        .insert(mapped, Value::bool(true), vm, globals)?;
                }
                Ok(set)
            } else {
                set_from_iter(elems.into_iter(), vm, globals)
            }
        }
        _ => Ok(new_empty_set()),
    }
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
    let mut self_val = lfp.self_val();
    self_val
        .as_hashmap_inner_mut()
        .insert(val, Value::bool(true), vm, globals)?;
    Ok(self_val)
}

///
/// ### Set#<<
///
/// - add?(o) -> self | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Set/i/=3c=3c.html]
#[monoruby_builtin]
fn add_q(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let val = lfp.arg(0);
    let mut self_val = lfp.self_val();
    let already = self_val.as_hashmap_inner().contains_key(val, vm, globals)?;
    if already {
        Ok(Value::nil())
    } else {
        self_val
            .as_hashmap_inner_mut()
            .insert(val, Value::bool(true), vm, globals)?;
        Ok(self_val)
    }
}

///
/// ### Set#===
///
/// - include?(o) -> bool[permalink][rdoc][edit]
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
fn clear(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let mut self_val = lfp.self_val();
    self_val.as_hashmap_inner_mut().clear();
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
    let mut self_val = lfp.self_val();
    self_val.as_hashmap_inner_mut().remove(val, vm, globals)?;
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
    let mut self_val = lfp.self_val();
    let removed = self_val.as_hashmap_inner_mut().remove(val, vm, globals)?;
    if removed.is_some() {
        Ok(self_val)
    } else {
        Ok(Value::nil())
    }
}

///
/// ### Set#each
///
#[monoruby_builtin]
fn each(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, pc: BytecodePtr) -> Result<Value> {
    let bh = match lfp.block() {
        None => {
            let id = IdentId::get_id("each");
            return vm.generate_enumerator(id, lfp.self_val(), lfp.iter().collect(), pc);
        }
        Some(block) => block,
    };
    let self_val = lfp.self_val();
    let keys = self_val.as_hashmap_inner().keys();
    let data = vm.get_block_data(globals, bh)?;
    for val in keys {
        vm.invoke_block(globals, &data, &[val])?;
    }
    Ok(lfp.self_val())
}

///
/// ### Set#to_a
///
#[monoruby_builtin]
fn to_a(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let keys = set_keys(lfp.self_val());
    Ok(Value::array_from_vec(keys))
}

///
/// ### Set#inspect / Set#to_s
///
/// Handled by RValue::hash_inspect (in value/rvalue.rs) which detects
/// whether the HASH-typed object is a Set or Hash by checking the class ID,
/// and uses the shared HashSet-based cycle detection from Value::inspect_inner.

///
/// ### Set#to_set
///
#[monoruby_builtin]
fn to_set(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(lfp.self_val())
}

///
/// ### Set#dup / Set#clone
///
#[monoruby_builtin]
fn dup(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let keys = set_keys(lfp.self_val());
    set_from_iter(keys.into_iter(), vm, globals)
}

///
/// ### Set#merge
///
#[monoruby_builtin]
fn merge(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let mut self_val = lfp.self_val();
    let args = lfp.arg(0).as_array();
    for arg in args.iter().copied() {
        let elems = enum_to_vec(vm, globals, arg)?;
        for elem in elems {
            self_val
                .as_hashmap_inner_mut()
                .insert(elem, Value::bool(true), vm, globals)?;
        }
    }
    Ok(self_val)
}

///
/// ### Set#subtract
///
#[monoruby_builtin]
fn subtract(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let elems = enum_to_vec(vm, globals, lfp.arg(0))?;
    let mut self_val = lfp.self_val();
    for elem in elems {
        self_val.as_hashmap_inner_mut().remove(elem, vm, globals)?;
    }
    Ok(self_val)
}

///
/// ### Set#replace
///
#[monoruby_builtin]
fn replace(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let elems = enum_to_vec(vm, globals, lfp.arg(0))?;
    let mut self_val = lfp.self_val();
    self_val.as_hashmap_inner_mut().clear();
    for elem in elems {
        self_val
            .as_hashmap_inner_mut()
            .insert(elem, Value::bool(true), vm, globals)?;
    }
    Ok(self_val)
}

///
/// ### Set#& / Set#intersection
///
#[monoruby_builtin]
fn intersection(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let self_val = lfp.self_val();
    let self_inner = self_val.as_hashmap_inner();
    let other_elems = enum_to_vec(vm, globals, lfp.arg(0))?;
    // Build a temp hash for the other side
    let mut other = new_empty_set();
    for elem in other_elems {
        other
            .as_hashmap_inner_mut()
            .insert(elem, Value::bool(true), vm, globals)?;
    }
    let other_inner = other.as_hashmap_inner();
    let mut result = new_empty_set();
    for (k, _) in self_inner.iter() {
        if other_inner.contains_key(k, vm, globals)? {
            result
                .as_hashmap_inner_mut()
                .insert(k, Value::bool(true), vm, globals)?;
        }
    }
    Ok(result)
}

///
/// ### Set#| / Set#+ / Set#union
///
#[monoruby_builtin]
fn union_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_keys = set_keys(lfp.self_val());
    let other_elems = enum_to_vec(vm, globals, lfp.arg(0))?;
    let mut result = new_empty_set();
    for k in self_keys {
        result
            .as_hashmap_inner_mut()
            .insert(k, Value::bool(true), vm, globals)?;
    }
    for k in other_elems {
        result
            .as_hashmap_inner_mut()
            .insert(k, Value::bool(true), vm, globals)?;
    }
    Ok(result)
}

///
/// ### Set#- / Set#difference
///
#[monoruby_builtin]
fn difference(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let self_inner = self_val.as_hashmap_inner();
    let other_elems = enum_to_vec(vm, globals, lfp.arg(0))?;
    let mut other = new_empty_set();
    for elem in other_elems {
        other
            .as_hashmap_inner_mut()
            .insert(elem, Value::bool(true), vm, globals)?;
    }
    let other_inner = other.as_hashmap_inner();
    let mut result = new_empty_set();
    for (k, _) in self_inner.iter() {
        if !other_inner.contains_key(k, vm, globals)? {
            result
                .as_hashmap_inner_mut()
                .insert(k, Value::bool(true), vm, globals)?;
        }
    }
    Ok(result)
}

///
/// ### Set#^
///
#[monoruby_builtin]
fn symmetric_difference(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let self_val = lfp.self_val();
    let self_inner = self_val.as_hashmap_inner();
    let other_elems = enum_to_vec(vm, globals, lfp.arg(0))?;
    let mut other = new_empty_set();
    for elem in other_elems {
        other
            .as_hashmap_inner_mut()
            .insert(elem, Value::bool(true), vm, globals)?;
    }
    let other_inner = other.as_hashmap_inner();
    let mut result = new_empty_set();
    for (k, _) in self_inner.iter() {
        if !other_inner.contains_key(k, vm, globals)? {
            result
                .as_hashmap_inner_mut()
                .insert(k, Value::bool(true), vm, globals)?;
        }
    }
    for (k, _) in other_inner.iter() {
        if !self_inner.contains_key(k, vm, globals)? {
            result
                .as_hashmap_inner_mut()
                .insert(k, Value::bool(true), vm, globals)?;
        }
    }
    Ok(result)
}

///
/// ### Set#==
///
#[monoruby_builtin]
fn eq(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let other = lfp.arg(0);
    if !is_set(other, &globals.store) {
        return Ok(Value::bool(false));
    }
    let self_val = lfp.self_val();
    let self_inner = self_val.as_hashmap_inner();
    let other_inner = other.as_hashmap_inner();
    if self_inner.len() != other_inner.len() {
        return Ok(Value::bool(false));
    }
    for (k, _) in self_inner.iter() {
        if !other_inner.contains_key(k, vm, globals)? {
            return Ok(Value::bool(false));
        }
    }
    Ok(Value::bool(true))
}

///
/// ### Set#subset? / Set#<=
///
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
    for (k, _) in self_inner.iter() {
        if !other_inner.contains_key(k, vm, globals)? {
            return Ok(Value::bool(false));
        }
    }
    Ok(Value::bool(true))
}

///
/// ### Set#superset? / Set#>=
///
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
    for (k, _) in other_inner.iter() {
        if !self_inner.contains_key(k, vm, globals)? {
            return Ok(Value::bool(false));
        }
    }
    Ok(Value::bool(true))
}

///
/// ### Set#proper_subset? / Set#<
///
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
    for (k, _) in self_inner.iter() {
        if !other_inner.contains_key(k, vm, globals)? {
            return Ok(Value::bool(false));
        }
    }
    Ok(Value::bool(true))
}

///
/// ### Set#proper_superset? / Set#>
///
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
    for (k, _) in other_inner.iter() {
        if !self_inner.contains_key(k, vm, globals)? {
            return Ok(Value::bool(false));
        }
    }
    Ok(Value::bool(true))
}

///
/// ### Set#<=>
///
#[monoruby_builtin]
fn spaceship(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let other = lfp.arg(0);
    if !is_set(other, &globals.store) {
        return Ok(Value::nil());
    }
    let self_val = lfp.self_val();
    let self_inner = self_val.as_hashmap_inner();
    let other_inner = other.as_hashmap_inner();

    let mut self_sub_other = true;
    for (k, _) in self_inner.iter() {
        if !other_inner.contains_key(k, vm, globals)? {
            self_sub_other = false;
            break;
        }
    }

    let mut other_sub_self = true;
    for (k, _) in other_inner.iter() {
        if !self_inner.contains_key(k, vm, globals)? {
            other_sub_self = false;
            break;
        }
    }

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
#[monoruby_builtin]
fn disjoint_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let self_inner = self_val.as_hashmap_inner();
    let other = lfp.arg(0);

    if is_set(other, &globals.store) {
        let other_inner = other.as_hashmap_inner();
        if self_inner.len() <= other_inner.len() {
            for (k, _) in self_inner.iter() {
                if other_inner.contains_key(k, vm, globals)? {
                    return Ok(Value::bool(false));
                }
            }
        } else {
            for (k, _) in other_inner.iter() {
                if self_inner.contains_key(k, vm, globals)? {
                    return Ok(Value::bool(false));
                }
            }
        }
    } else {
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
    }
    Ok(Value::bool(true))
}

///
/// ### Set#intersect?
///
#[monoruby_builtin]
fn intersect_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let self_inner = self_val.as_hashmap_inner();
    let other = lfp.arg(0);

    if is_set(other, &globals.store) {
        let other_inner = other.as_hashmap_inner();
        if self_inner.len() <= other_inner.len() {
            for (k, _) in self_inner.iter() {
                if other_inner.contains_key(k, vm, globals)? {
                    return Ok(Value::bool(true));
                }
            }
        } else {
            for (k, _) in other_inner.iter() {
                if self_inner.contains_key(k, vm, globals)? {
                    return Ok(Value::bool(true));
                }
            }
        }
    } else {
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
    }
    Ok(Value::bool(false))
}

///
/// ### Set#delete_if
///
#[monoruby_builtin]
fn delete_if(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, pc: BytecodePtr) -> Result<Value> {
    let bh = match lfp.block() {
        None => {
            let id = IdentId::get_id("delete_if");
            return vm.generate_enumerator(id, lfp.self_val(), lfp.iter().collect(), pc);
        }
        Some(block) => block,
    };
    let keys = set_keys(lfp.self_val());
    let data = vm.get_block_data(globals, bh)?;
    let mut to_delete = vec![];
    for val in keys {
        let result = vm.invoke_block(globals, &data, &[val])?;
        if result.as_bool() {
            to_delete.push(val);
        }
    }
    let mut self_val = lfp.self_val();
    for val in to_delete {
        self_val.as_hashmap_inner_mut().remove(val, vm, globals)?;
    }
    Ok(self_val)
}

///
/// ### Set#keep_if
///
#[monoruby_builtin]
fn keep_if(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, pc: BytecodePtr) -> Result<Value> {
    let bh = match lfp.block() {
        None => {
            let id = IdentId::get_id("keep_if");
            return vm.generate_enumerator(id, lfp.self_val(), lfp.iter().collect(), pc);
        }
        Some(block) => block,
    };
    let keys = set_keys(lfp.self_val());
    let data = vm.get_block_data(globals, bh)?;
    let mut to_delete = vec![];
    for val in keys {
        let result = vm.invoke_block(globals, &data, &[val])?;
        if !result.as_bool() {
            to_delete.push(val);
        }
    }
    let mut self_val = lfp.self_val();
    for val in to_delete {
        self_val.as_hashmap_inner_mut().remove(val, vm, globals)?;
    }
    Ok(self_val)
}

///
/// ### Set#select! / Set#filter!
///
#[monoruby_builtin]
fn select_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, pc: BytecodePtr) -> Result<Value> {
    let bh = match lfp.block() {
        None => {
            let id = IdentId::get_id("select!");
            return vm.generate_enumerator(id, lfp.self_val(), lfp.iter().collect(), pc);
        }
        Some(block) => block,
    };
    let keys = set_keys(lfp.self_val());
    let data = vm.get_block_data(globals, bh)?;
    let mut to_delete = vec![];
    for val in keys {
        let result = vm.invoke_block(globals, &data, &[val])?;
        if !result.as_bool() {
            to_delete.push(val);
        }
    }
    if to_delete.is_empty() {
        return Ok(Value::nil());
    }
    let mut self_val = lfp.self_val();
    for val in to_delete {
        self_val.as_hashmap_inner_mut().remove(val, vm, globals)?;
    }
    Ok(self_val)
}

///
/// ### Set#reject!
///
#[monoruby_builtin]
fn reject_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, pc: BytecodePtr) -> Result<Value> {
    let bh = match lfp.block() {
        None => {
            let id = IdentId::get_id("reject!");
            return vm.generate_enumerator(id, lfp.self_val(), lfp.iter().collect(), pc);
        }
        Some(block) => block,
    };
    let keys = set_keys(lfp.self_val());
    let data = vm.get_block_data(globals, bh)?;
    let mut to_delete = vec![];
    for val in keys {
        let result = vm.invoke_block(globals, &data, &[val])?;
        if result.as_bool() {
            to_delete.push(val);
        }
    }
    if to_delete.is_empty() {
        return Ok(Value::nil());
    }
    let mut self_val = lfp.self_val();
    for val in to_delete {
        self_val.as_hashmap_inner_mut().remove(val, vm, globals)?;
    }
    Ok(self_val)
}

///
/// ### Set#collect! / Set#map!
///
#[monoruby_builtin]
fn collect_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, pc: BytecodePtr) -> Result<Value> {
    let bh = match lfp.block() {
        None => {
            let id = IdentId::get_id("collect!");
            return vm.generate_enumerator(id, lfp.self_val(), lfp.iter().collect(), pc);
        }
        Some(block) => block,
    };
    let keys = set_keys(lfp.self_val());
    let data = vm.get_block_data(globals, bh)?;
    let mut new_elems = vec![];
    for val in keys {
        let mapped = vm.invoke_block(globals, &data, &[val])?;
        new_elems.push(mapped);
    }
    let mut self_val = lfp.self_val();
    self_val.as_hashmap_inner_mut().clear();
    for elem in new_elems {
        self_val
            .as_hashmap_inner_mut()
            .insert(elem, Value::bool(true), vm, globals)?;
    }
    Ok(self_val)
}

///
/// ### Set#flatten
///
#[monoruby_builtin]
fn flatten(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let mut result = new_empty_set();
    let mut seen = std::collections::HashSet::new();
    seen.insert(self_val.id());
    flatten_set_into(&mut result, self_val, vm, globals, &mut seen)?;
    Ok(result)
}

fn flatten_set_into(
    result: &mut Value,
    set: Value,
    vm: &mut Executor,
    globals: &mut Globals,
    seen: &mut std::collections::HashSet<u64>,
) -> Result<()> {
    let keys = set_keys(set);
    for val in keys {
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
                .as_hashmap_inner_mut()
                .insert(val, Value::bool(true), vm, globals)?;
        }
    }
    Ok(())
}

///
/// ### Set#flatten!
///
#[monoruby_builtin]
fn flatten_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let keys = set_keys(self_val);
    let has_nested = keys.iter().any(|v| is_set(*v, &globals.store));
    if !has_nested {
        return Ok(Value::nil());
    }
    let mut result = new_empty_set();
    let mut seen = std::collections::HashSet::new();
    seen.insert(self_val.id());
    flatten_set_into(&mut result, self_val, vm, globals, &mut seen)?;
    // Replace self's contents
    let mut self_val = lfp.self_val();
    self_val.as_hashmap_inner_mut().clear();
    let new_keys = set_keys(result);
    for k in new_keys {
        self_val
            .as_hashmap_inner_mut()
            .insert(k, Value::bool(true), vm, globals)?;
    }
    Ok(self_val)
}

/// ### Set#join
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
    let keys = set_keys(lfp.self_val());
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
}

/// ### Set#classify
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
    let keys = set_keys(lfp.self_val());
    // Result is a Hash mapping classification → Set
    let mut result_hash = Value::hash_from_inner(Default::default());
    for k in keys {
        let classification = vm.invoke_block(globals, &data, &[k])?;
        let existing = result_hash.as_hashmap_inner().get(classification, vm, globals)?;
        if let Some(mut set) = existing {
            set.as_hashmap_inner_mut().insert(k, Value::bool(true), vm, globals)?;
        } else {
            let mut new_set = new_empty_set();
            new_set.as_hashmap_inner_mut().insert(k, Value::bool(true), vm, globals)?;
            result_hash.as_hashmap_inner_mut().insert(classification, new_set, vm, globals)?;
        }
    }
    Ok(result_hash)
}

/// ### Set#divide
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
    let keys = set_keys(lfp.self_val());

    // Check block arity to determine mode
    let arity = if let Some(fid) = data.func_id() {
        globals[fid].arity()
    } else {
        1
    };
    if arity == 2 || arity == -1 || arity < -2 || arity > 2 {
        // Arity-2 mode: union-find based divide
        // Group elements where block(a, b) returns true
        let n = keys.len();
        let mut parent: Vec<usize> = (0..n).collect();
        fn find(parent: &mut Vec<usize>, i: usize) -> usize {
            if parent[i] != i {
                parent[i] = find(parent, parent[i]);
            }
            parent[i]
        }
        fn union(parent: &mut Vec<usize>, i: usize, j: usize) {
            let ri = find(parent, i);
            let rj = find(parent, j);
            if ri != rj {
                parent[ri] = rj;
            }
        }
        for i in 0..n {
            for j in (i + 1)..n {
                let result = vm.invoke_block(globals, &data, &[keys[i], keys[j]])?;
                if result.as_bool() {
                    union(&mut parent, i, j);
                }
            }
        }
        // Group by root
        let mut groups: std::collections::HashMap<usize, Vec<Value>> = std::collections::HashMap::new();
        for i in 0..n {
            let root = find(&mut parent, i);
            groups.entry(root).or_default().push(keys[i]);
        }
        let mut result_set = new_empty_set();
        for (_root, elems) in groups {
            let subset = set_from_iter(elems.into_iter(), vm, globals)?;
            result_set.as_hashmap_inner_mut().insert(subset, Value::bool(true), vm, globals)?;
        }
        Ok(result_set)
    } else {
        // Arity-1 mode: classify and return set of sets
        let mut groups: std::collections::HashMap<u64, Vec<Value>> = std::collections::HashMap::new();
        let mut group_order: Vec<u64> = Vec::new();
        for k in &keys {
            let classification = vm.invoke_block(globals, &data, &[*k])?;
            let key = classification.id() as u64;
            if !groups.contains_key(&key) {
                group_order.push(key);
            }
            groups.entry(key).or_default().push(*k);
        }
        let mut result_set = new_empty_set();
        for key in group_order {
            if let Some(elems) = groups.remove(&key) {
                let subset = set_from_iter(elems.into_iter(), vm, globals)?;
                result_set.as_hashmap_inner_mut().insert(subset, Value::bool(true), vm, globals)?;
            }
        }
        Ok(result_set)
    }
}

/// ### Set#reset
#[monoruby_builtin]
fn reset(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    // In CRuby, reset rebuilds the internal hash. For us, it's a no-op since
    // our hash is always consistent.
    Ok(lfp.self_val())
}

/// ### Set#hash
#[monoruby_builtin]
fn set_hash(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};
    let keys = set_keys(lfp.self_val());
    // XOR all element hashes for order-independence
    let mut combined: u64 = 0;
    let hash_id = IdentId::get_id("hash");
    for k in keys {
        let h = vm.invoke_method_inner(globals, hash_id, k, &[], None, None)?;
        if let Some(i) = h.try_fixnum() {
            combined ^= i as u64;
        }
    }
    // Mix with Set class identity
    let mut hasher = DefaultHasher::new();
    "Set".hash(&mut hasher);
    combined ^= hasher.finish();
    Ok(Value::integer(combined as i64))
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn set_basic() {
        run_test("Set.new.class.to_s");
        run_test("Set[1, 2, 3].size");
        run_test("Set[1, 2, 3].include?(2)");
        run_test("Set[1, 2, 3].include?(4)");
        run_test("Set.new.empty?");
        run_test("Set[1].empty?");
    }

    #[test]
    fn set_add_delete() {
        run_test("s = Set.new; s << 1; s << 2; s << 1; s.size");
        run_test("s = Set[1, 2]; s.add?(3).equal?(s)");
        run_test("s = Set[1, 2]; s.add?(1).nil?");
        run_test("s = Set[1, 2, 3]; s.delete(2); s.include?(2)");
        run_test("s = Set[1, 2]; s.delete?(2).equal?(s)");
        run_test("s = Set[1, 2]; s.delete?(3).nil?");
    }

    #[test]
    fn set_operations() {
        run_test("(Set[1, 2, 3] & Set[2, 3, 4]).to_a.sort");
        run_test("(Set[1, 2, 3] | Set[2, 3, 4]).to_a.sort");
        run_test("(Set[1, 2, 3] - Set[2, 3, 4]).to_a.sort");
        run_test("(Set[1, 2, 3] ^ Set[2, 3, 4]).to_a.sort");
    }

    #[test]
    fn set_comparison() {
        run_test("Set[1, 2] == Set[2, 1]");
        run_test("Set[1, 2] == Set[1, 2, 3]");
        run_test("Set[1, 2].subset?(Set[1, 2, 3])");
        run_test("Set[1, 2, 3].superset?(Set[1, 2])");
        run_test("Set[1, 2].proper_subset?(Set[1, 2, 3])");
        run_test("Set[1, 2].proper_subset?(Set[1, 2])");
        run_test("Set[1, 2].disjoint?(Set[3, 4])");
        run_test("Set[1, 2].disjoint?(Set[2, 3])");
        run_test("Set[1, 2].intersect?(Set[2, 3])");
        run_test("Set[1, 2].intersect?(Set[3, 4])");
    }

    #[test]
    fn set_each() {
        run_test("a = []; Set[3, 1, 2].each {|x| a << x}; a.sort");
    }

    #[test]
    fn set_to_a() {
        run_test("Set[3, 1, 2].to_a.sort");
    }

    #[test]
    fn set_inspect() {
        run_test("Set[].to_s");
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
        run_test("s = Set[1]; s.merge([2, 3]); s.to_a.sort");
    }

    #[test]
    fn set_new_with_enum() {
        run_test("Set.new([1, 2, 3]).to_a.sort");
        run_test("Set.new([1, 2, 3]) {|x| x * 2}.to_a.sort");
    }

    #[test]
    fn set_spaceship() {
        run_test("Set[1, 2] <=> Set[1, 2]");
        run_test("Set[1, 2] <=> Set[1, 2, 3]");
        run_test("Set[1, 2, 3] <=> Set[1, 2]");
        run_test("(Set[1, 2] <=> Set[3, 4]).nil?");
    }

    #[test]
    fn set_dup() {
        run_test("s = Set[1, 2]; t = s.dup; t << 3; [s.size, t.size]");
    }

    #[test]
    fn set_clear() {
        run_test("s = Set[1, 2, 3]; s.clear; s.size");
        run_test("s = Set[1, 2, 3]; s.clear; s.empty?");
        run_test("s = Set[1, 2, 3]; s.clear.equal?(s)");
    }

    #[test]
    fn set_to_set() {
        run_test("s = Set[1, 2]; s.to_set.equal?(s)");
        run_test("Set[1, 2, 3].to_set.to_a.sort");
    }

    #[test]
    fn set_subtract() {
        run_test("s = Set[1, 2, 3, 4]; s.subtract([2, 4]); s.to_a.sort");
        run_test("s = Set[1, 2, 3]; s.subtract(Set[3, 4]); s.to_a.sort");
        run_test("s = Set[1, 2]; s.subtract([]).to_a.sort");
        run_test("s = Set[1, 2]; s.subtract([1, 2]); s.size");
    }

    #[test]
    fn set_replace() {
        run_test("s = Set[1, 2, 3]; s.replace([4, 5]); s.to_a.sort");
        run_test("s = Set[1, 2]; s.replace(Set[3, 4, 5]); s.to_a.sort");
        run_test("s = Set[1, 2]; s.replace([]).size");
        run_test("s = Set[1, 2]; s.replace([1, 2]).equal?(s)");
    }

    #[test]
    fn set_proper_superset() {
        run_test("Set[1, 2, 3].proper_superset?(Set[1, 2])");
        run_test("Set[1, 2].proper_superset?(Set[1, 2])");
        run_test("Set[1, 2].proper_superset?(Set[1, 2, 3])");
        run_test("Set[1, 2, 3] > Set[1, 2]");
        run_test("Set[1, 2] > Set[1, 2]");
    }

    #[test]
    fn set_delete_if() {
        run_test("s = Set[1, 2, 3, 4, 5]; s.delete_if {|x| x % 2 == 0}; s.to_a.sort");
        run_test("s = Set[1, 2, 3]; s.delete_if {|x| false}; s.to_a.sort");
        run_test("s = Set[1, 2, 3]; s.delete_if {|x| true}; s.size");
        run_test("s = Set[1, 2, 3]; s.delete_if {|x| x > 1}.equal?(s)");
    }

    #[test]
    fn set_keep_if() {
        run_test("s = Set[1, 2, 3, 4, 5]; s.keep_if {|x| x.odd?}; s.to_a.sort");
        run_test("s = Set[1, 2, 3]; s.keep_if {|x| true}; s.to_a.sort");
        run_test("s = Set[1, 2, 3]; s.keep_if {|x| false}; s.size");
        run_test("s = Set[1, 2, 3]; s.keep_if {|x| x < 3}.equal?(s)");
    }

    #[test]
    fn set_filter_reject() {
        run_test("s = Set[1, 2, 3, 4]; s.select! {|x| x.even?}; s.to_a.sort");
        run_test("s = Set[1, 2, 3, 4]; s.reject! {|x| x.even?}; s.to_a.sort");
        run_test("s = Set[2, 4]; s.select! {|x| x.even?}.nil?");
    }

    #[test]
    fn set_map() {
        run_test("s = Set[1, 2, 3]; s.collect! {|x| x * 2}; s.to_a.sort");
    }

    #[test]
    fn set_flatten() {
        run_test("Set[Set[1, 2], Set[3, 4], 5].flatten.to_a.sort");
    }

    #[test]
    fn set_flatten_bang() {
        run_test("s = Set[Set[1, 2], Set[3, 4], 5]; s.flatten!; s.to_a.sort");
        run_test("s = Set[Set[1, 2], Set[3, 4], 5]; s.flatten!.equal?(s)");
        run_test("s = Set[1, 2, 3]; s.flatten!.nil?");
        run_test("Set[Set[Set[1]], 2].flatten.to_a.sort");
    }

    #[test]
    fn set_flatten_recursive() {
        run_test_error("s = Set.new; s << s; s.flatten");
    }

    #[test]
    fn set_flatten_bang_recursive() {
        run_test_error("s = Set.new; s << s; s.flatten!");
    }
}
