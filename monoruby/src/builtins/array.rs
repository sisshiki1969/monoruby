use super::*;
use jitgen::JitContext;
use rubymap::RubyEql;
use smallvec::smallvec;
use std::cmp::Ordering;

//
// Array class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_under_obj("Array", ARRAY_CLASS, ObjTy::ARRAY);
    globals.define_builtin_class_func_with(ARRAY_CLASS, "new", new, 0, 0, true);
    globals.define_builtin_class_func_rest(ARRAY_CLASS, "[]", array_class_bracket);
    globals.define_builtin_class_func(ARRAY_CLASS, "try_convert", array_try_convert, 1);
    //globals.define_builtin_class_inline_func_with(
    //    ARRAY_CLASS,
    //    "new",
    //    new,
    //    Box::new(super::class::gen_class_new(allocate_array)),
    //    0,
    //    0,
    //    true,
    //);
    globals.define_builtin_class_inline_func(
        ARRAY_CLASS,
        "allocate",
        allocate,
        Box::new(array_allocate),
        0,
    );
    globals.define_private_builtin_func_with(ARRAY_CLASS, "initialize", initialize, 0, 2, false);
    globals.define_builtin_inline_funcs(
        ARRAY_CLASS,
        "size",
        &["length"],
        size,
        Box::new(array_size),
        0,
    );
    globals.define_builtin_inline_funcs(
        ARRAY_CLASS,
        "clone",
        &["dup"],
        clone,
        Box::new(array_clone),
        0,
    );
    globals.define_builtin_func_with(ARRAY_CLASS, "count", count, 0, 1, false);
    globals.define_builtin_func(ARRAY_CLASS, "empty?", empty, 0);
    globals.define_builtin_funcs(ARRAY_CLASS, "inspect", &["to_s"], inspect, 0);
    globals.define_builtin_func(ARRAY_CLASS, "to_a", to_a, 0);
    globals.define_builtin_func(ARRAY_CLASS, "to_h", to_h, 0);
    globals.define_builtin_func(ARRAY_CLASS, "hash", hash, 0);
    globals.define_builtin_func(ARRAY_CLASS, "+", add, 1);
    globals.define_builtin_func(ARRAY_CLASS, "-", sub, 1);
    globals.define_builtin_func(ARRAY_CLASS, "*", mul, 1);
    globals.define_builtin_func(ARRAY_CLASS, "&", and, 1);
    globals.define_builtin_func(ARRAY_CLASS, "|", or, 1);
    globals.define_builtin_func_with(ARRAY_CLASS, "shift", shift, 0, 1, false);
    globals.define_builtin_funcs_rest(ARRAY_CLASS, "unshift", &["prepend"], unshift);
    globals.define_builtin_func_rest(ARRAY_CLASS, "concat", concat);
    globals.define_builtin_inline_func(ARRAY_CLASS, "<<", shl, Box::new(array_shl), 1);
    globals.define_builtin_funcs_with(ARRAY_CLASS, "push", &["append"], push, 0, 0, true);
    globals.define_builtin_func_with(ARRAY_CLASS, "pop", pop, 0, 1, false);
    globals.define_builtin_funcs(ARRAY_CLASS, "==", &["==="], eq, 1);
    globals.define_builtin_func(ARRAY_CLASS, "eql?", eql, 1);
    globals.define_builtin_func(ARRAY_CLASS, "<=>", cmp, 1);
    globals.define_builtin_inline_funcs_with(
        ARRAY_CLASS,
        "[]",
        &["slice"],
        index,
        Box::new(array_index),
        1,
        2,
        false,
    );
    globals.define_builtin_inline_func_with(
        ARRAY_CLASS,
        "[]=",
        index_assign,
        Box::new(array_index_assign),
        2,
        3,
        false,
    );
    globals.define_builtin_func(ARRAY_CLASS, "clear", clear, 0);
    globals.define_builtin_func_with(ARRAY_CLASS, "fill", fill, 0, 3, false);
    globals.define_builtin_func(ARRAY_CLASS, "drop", drop, 1);
    globals.define_builtin_func_rest(ARRAY_CLASS, "zip", zip);
    globals.define_builtin_funcs_with(ARRAY_CLASS, "inject", &["reduce"], inject, 0, 2, false);
    globals.define_builtin_func_with(ARRAY_CLASS, "join", join, 0, 1, false);
    globals.define_builtin_func_with(ARRAY_CLASS, "first", first, 0, 1, false);
    globals.define_builtin_func_with(ARRAY_CLASS, "last", last, 0, 1, false);
    globals.define_builtin_func_with(ARRAY_CLASS, "fetch", fetch, 1, 2, false);
    globals.define_builtin_func(ARRAY_CLASS, "take", take, 1);
    globals.define_builtin_func_with(ARRAY_CLASS, "sum", sum, 0, 1, false);
    globals.define_builtin_func(ARRAY_CLASS, "min", min, 0);
    globals.define_builtin_func(ARRAY_CLASS, "max", max, 0);
    globals.define_builtin_func(ARRAY_CLASS, "partition", partition, 0);
    globals.define_builtin_funcs(ARRAY_CLASS, "filter", &["select", "find_all"], filter, 0);
    globals.define_builtin_funcs(ARRAY_CLASS, "filter!", &["select!"], filter_, 0);
    globals.define_builtin_func(ARRAY_CLASS, "keep_if", keep_if, 0);
    globals.define_builtin_func(ARRAY_CLASS, "reject!", reject_, 0);
    globals.define_builtin_func(ARRAY_CLASS, "delete_if", delete_if, 0);
    globals.define_builtin_func(ARRAY_CLASS, "reject", reject, 0);
    globals.define_builtin_func(ARRAY_CLASS, "sort", sort, 0);
    globals.define_builtin_func(ARRAY_CLASS, "sort!", sort_, 0);
    globals.define_builtin_func(ARRAY_CLASS, "sort_by!", sort_by_, 0);
    globals.define_builtin_func(ARRAY_CLASS, "sort_by", sort_by, 0);
    globals.define_builtin_func(ARRAY_CLASS, "group_by", group_by, 0);
    //globals.define_builtin_func(ARRAY_CLASS, "each", each, 0);
    //globals.define_builtin_func(ARRAY_CLASS, "each_with_index", each_with_index, 0);
    //globals.define_builtin_funcs(ARRAY_CLASS, "map", &["collect"], map, 0);
    //globals.define_builtin_funcs(ARRAY_CLASS, "map!", &["collect!"], map_, 0);
    globals.define_builtin_funcs(ARRAY_CLASS, "flat_map", &["collect_concat"], flat_map, 0);
    globals.define_builtin_func_with(ARRAY_CLASS, "all?", all_, 0, 1, false);
    globals.define_builtin_func_with(ARRAY_CLASS, "any?", any_, 0, 1, false);
    globals.define_builtin_funcs(ARRAY_CLASS, "detect", &["find"], detect, 0);
    globals.define_builtin_func(ARRAY_CLASS, "grep", grep, 1);
    globals.define_builtin_func(ARRAY_CLASS, "include?", include_, 1);
    globals.define_builtin_func(ARRAY_CLASS, "reverse", reverse, 0);
    globals.define_builtin_func(ARRAY_CLASS, "reverse!", reverse_, 0);
    globals.define_builtin_func(ARRAY_CLASS, "transpose", transpose, 0);
    globals.define_builtin_func_with(ARRAY_CLASS, "rotate!", rotate_, 0, 1, false);
    globals.define_builtin_func_with(ARRAY_CLASS, "rotate", rotate, 0, 1, false);
    globals.define_builtin_func_rest(ARRAY_CLASS, "product", product);

    globals.define_builtin_func_rest(ARRAY_CLASS, "union", union);
    globals.define_builtin_func_rest(ARRAY_CLASS, "difference", difference);
    globals.define_builtin_func_rest(ARRAY_CLASS, "intersection", intersection);
    globals.define_builtin_func(ARRAY_CLASS, "intersect?", intersect_, 1);
    globals.define_builtin_func(ARRAY_CLASS, "uniq", uniq, 0);
    globals.define_builtin_func(ARRAY_CLASS, "uniq!", uniq_, 0);
    globals.define_builtin_func_with(ARRAY_CLASS, "slice!", slice_, 1, 2, false);
    globals.define_builtin_func_with_kw(ARRAY_CLASS, "pack", pack, 1, 1, false, &["buffer"], false);
    globals.define_builtin_func_with(ARRAY_CLASS, "flatten", flatten, 0, 1, false);
    globals.define_builtin_func_with(ARRAY_CLASS, "flatten!", flatten_, 0, 1, false);
    globals.define_builtin_func(ARRAY_CLASS, "compact", compact, 0);
    globals.define_builtin_func(ARRAY_CLASS, "compact!", compact_, 0);
    globals.define_builtin_func_with_kw(
        ARRAY_CLASS,
        "shuffle!",
        shuffle_,
        0,
        0,
        false,
        &["random"],
        false,
    );
    globals.define_builtin_func(ARRAY_CLASS, "delete", delete, 1);
    globals.define_builtin_func(ARRAY_CLASS, "delete_at", delete_at, 1);
    globals.define_builtin_funcs_with(
        ARRAY_CLASS,
        "find_index",
        &["index"],
        find_index,
        0,
        1,
        false,
    );
    globals.define_builtin_func_with(ARRAY_CLASS, "insert", insert, 1, 1, true);
    globals.define_builtin_funcs(ARRAY_CLASS, "replace", &["initialize_copy"], replace, 1);
}

///
/// ### Array.new
///
/// - new(size = 0, val = nil) -> Array
/// - new(ary) -> Array
/// - new(size) {|index| ... } -> Array
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/s/new.html]
#[monoruby_builtin]
fn new(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let class = lfp.self_val().as_class_id();
    let obj = Value::array_empty_with_class(class);
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
/// ### Array#allocate
/// - allocate -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Class/i/allocate.html]
#[monoruby_builtin]
fn allocate(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let class = lfp.self_val().as_class_id();
    let obj = Value::array_empty_with_class(class);
    Ok(obj)
}

fn array_allocate(
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
    let dst = callsite.dst;
    state.load(ir, callsite.recv, GP::Rdi);
    let using_xmm = state.get_using_xmm();
    ir.xmm_save(using_xmm);
    ir.inline(move |r#gen, _, _| {
        monoasm! { &mut r#gen.jit,
            movq rax, (allocate_array);
            call rax;
        }
    });
    ir.xmm_restore(using_xmm);

    state.def_reg2acc(ir, GP::Rax, dst);
    true
}

extern "C" fn allocate_array(class_val: Value) -> Value {
    let class_id = class_val.as_class_id();
    Value::array_empty_with_class(class_id)
}

///
/// ### Array.[]
///
/// Array[*args] -> Array
///
#[monoruby_builtin]
fn array_class_bracket(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let class = lfp.self_val().as_class_id();
    let args = lfp.arg(0).as_array();
    let ary = ArrayInner::from(args.to_vec().into());
    Ok(Value::array_with_class(ary, class))
}

///
/// ### Array.try_convert
///
/// - try_convert(obj) -> Array | nil
///
#[monoruby_builtin]
fn array_try_convert(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let obj = lfp.arg(0);
    if obj.is_array_ty() {
        return Ok(obj);
    }
    if obj.is_nil() {
        return Ok(Value::nil());
    }
    let to_ary = IdentId::get_id("to_ary");
    if let Some(func_id) = globals.check_method(obj, to_ary) {
        let result = vm.invoke_func_inner(globals, func_id, obj, &[], None, None)?;
        if result.is_array_ty() {
            return Ok(result);
        }
        if result.is_nil() {
            return Ok(Value::nil());
        }
        return Err(MonorubyErr::typeerr(format!(
            "can't convert {} into Array ({}#to_ary gives {})",
            obj.get_real_class_name(&globals.store),
            obj.get_real_class_name(&globals.store),
            result.get_real_class_name(&globals.store),
        )));
    }
    Ok(Value::nil())
}

///
/// ### Array#initialize
///
/// - new(size = 0, val = nil) -> Array
/// - new(ary) -> Array
/// - new(size) {|index| ... } -> Array
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/s/new.html]
#[monoruby_builtin]
fn initialize(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    lfp.self_val().ensure_not_frozen(&globals.store)?;
    let mut self_val = lfp.self_val().as_array();
    if lfp.try_arg(0).is_none() {
        self_val.clear();
        return Ok(self_val.into());
    }
    if lfp.try_arg(1).is_none() && lfp.block().is_none() {
        // Try to_ary conversion first
        let arg = lfp.arg(0);
        if arg.is_array_ty() {
            *self_val = (*arg.as_array()).clone();
            return Ok(self_val.into());
        }
        let to_ary = IdentId::get_id("to_ary");
        if let Some(func_id) = globals.check_method(arg, to_ary) {
            let result = vm.invoke_func_inner(globals, func_id, arg, &[], None, None)?;
            if result.is_array_ty() {
                *self_val = (*result.as_array()).clone();
                return Ok(self_val.into());
            }
            if !result.is_nil() {
                return Err(MonorubyErr::typeerr(format!(
                    "can't convert {} into Array ({}#to_ary gives {})",
                    arg.get_real_class_name(&globals.store),
                    arg.get_real_class_name(&globals.store),
                    result.get_real_class_name(&globals.store),
                )));
            }
        }
    }
    // Use coerce_to_int to call to_int on the size argument
    let size = lfp.arg(0).coerce_to_int_i64(vm, globals)?;
    if size < 0 {
        return Err(MonorubyErr::negative_array_size());
    }
    let size = size as usize;
    // Guard against unreasonably large sizes that would cause capacity overflow.
    // CRuby also limits array size; we use a practical limit here.
    const MAX_ARRAY_SIZE: usize = 1 << 30; // ~1 billion elements
    if size > MAX_ARRAY_SIZE {
        return Err(MonorubyErr::argumenterr(format!("array size too big")));
    }
    if let Some(bh) = lfp.block() {
        if lfp.try_arg(1).is_some() {
            eprintln!("warning: block supersedes default value argument");
        }
        let iter = (0..size).map(|i| Value::integer(i as i64));
        let mut res = vm.invoke_block_map1(globals, bh, iter, size)?;
        RValue::swap_kind(lfp.self_val().rvalue_mut(), res.rvalue_mut());
    } else {
        let val = lfp.try_arg(1).unwrap_or_default();
        *self_val = ArrayInner::from(smallvec![val; size]);
    }
    Ok(self_val.into())
}

///
/// ### Array#length
///
/// - length -> Integer
/// - size -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/length.html]
#[monoruby_builtin]
fn size(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let len = lfp.self_val().as_array().len();
    Ok(Value::integer(len as i64))
}

fn array_size(
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
    let dst = callsite.dst;
    state.load(ir, callsite.recv, GP::Rdi);
    ir.inline(move |r#gen, _, _| {
        r#gen.get_array_length();
        monoasm! { &mut r#gen.jit,
            salq  rax, 1;
            orq   rax, 1;
        }
    });

    state.def_reg2acc_fixnum(ir, GP::Rax, dst);
    true
}

///
/// ### Array#clone
///
/// - clone -> Array
/// - dup -> Array
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/clone.html]
#[monoruby_builtin]
fn clone(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let class_id = lfp.self_val().class();
    Ok(Value::array_with_class(
        lfp.self_val().as_array_inner().clone(),
        class_id,
    ))
}

fn array_clone(
    state: &mut AbstractState,
    ir: &mut AsmIr,
    _: &JitContext,
    store: &Store,
    callid: CallSiteId,
    class_id: ClassId,
    _: Option<ClassId>,
) -> bool {
    let callsite = &store[callid];
    if !callsite.is_simple() {
        return false;
    }
    let dst = callsite.dst;
    state.load(ir, callsite.recv, GP::Rdi);
    let using_xmm = state.get_using_xmm();
    ir.xmm_save(using_xmm);
    ir.inline(move |r#gen, _, _| {
        monoasm! { &mut r#gen.jit,
            movq rax, (array_dup);
            call rax;
        }
    });
    ir.xmm_restore(using_xmm);
    state.def_reg2acc_class(ir, GP::Rax, dst, class_id);
    true
}

extern "C" fn array_dup(val: Value) -> Value {
    let class_id = val.class();
    Value::array_with_class(val.as_array_inner().clone(), class_id)
}

///
/// ### Array#count
///
/// - count -> Integer
/// - count(item) -> Integer
/// - count {|item| ... } -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/count.html]
#[monoruby_builtin]
fn count(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    if let Some(arg0) = lfp.try_arg(0) {
        if lfp.block().is_some() {
            vm.ruby_warn(globals, "warning: given block not used")?;
        }
        let mut count = 0;
        let ary = lfp.self_val();
        let mut i = 0;
        while i < ary.as_array().len() {
            let elem = ary.as_array()[i];
            if vm.invoke_eq(globals, arg0, elem)? {
                count += 1;
            }
            i += 1;
        }
        Ok(Value::integer(count))
    } else if let Some(bh) = lfp.block() {
        let mut count = 0;
        let bh = vm.get_block_data(globals, bh)?;
        let ary = lfp.self_val();
        let mut i = 0;
        while i < ary.as_array().len() {
            let elem = ary.as_array()[i];
            if vm.invoke_block(globals, &bh, &[elem])?.as_bool() {
                count += 1;
            }
            i += 1;
        }
        Ok(Value::integer(count))
    } else {
        let len = lfp.self_val().as_array().len();
        Ok(Value::integer(len as i64))
    }
}

///
/// ### Array#empty?
///
/// - empty? -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/empty=3f.html]
#[monoruby_builtin]
fn empty(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let b = lfp.self_val().as_array().is_empty();
    Ok(Value::bool(b))
}

///
/// ### Array#inspect
///
/// - inspect -> String
/// - to_s -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/inspect.html]
#[monoruby_builtin]
fn inspect(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    crate::value::exec_recursive(
        self_val.id(),
        || {
            let ary = self_val.as_array();
            if ary.len() == 0 {
                return Ok(Value::string_from_inner(RStringInner::from_encoding(
                    b"[]",
                    Encoding::UsAscii,
                )));
            }
            let mut s = String::from("[");
            for (i, elem) in ary.iter().enumerate() {
                if i > 0 {
                    s.push_str(", ");
                }
                let inspected =
                    vm.invoke_method_inner(globals, IdentId::INSPECT, *elem, &[], None, None)?;
                // If #inspect returned a String, use it directly.
                // Otherwise, call Ruby #to_s on the result.
                if let Some(str_inner) = inspected.is_rstring_inner() {
                    let bytes = str_inner.as_bytes();
                    match std::str::from_utf8(bytes) {
                        Ok(utf8) => s.push_str(utf8),
                        Err(_) => s.push_str(&inspected.to_s(&globals.store)),
                    }
                } else {
                    let to_s_result = vm.invoke_method_inner(
                        globals,
                        IdentId::TO_S,
                        inspected,
                        &[],
                        None,
                        None,
                    )?;
                    if let Some(str_inner) = to_s_result.is_rstring_inner() {
                        let bytes = str_inner.as_bytes();
                        match std::str::from_utf8(bytes) {
                            Ok(utf8) => s.push_str(utf8),
                            Err(_) => s.push_str(&to_s_result.to_s(&globals.store)),
                        }
                    } else {
                        // If #to_s also doesn't return a String, use default representation
                        s.push_str(&to_s_result.to_s(&globals.store));
                    }
                }
            }
            s.push(']');
            Ok(Value::string(s))
        },
        Value::string("[...]".to_string()),
    )
}

///
/// ### Array#to_a
///
/// - to_a -> Array
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/to_a.html]
#[monoruby_builtin]
fn to_a(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    if self_val.class() != ARRAY_CLASS {
        let ary = self_val.as_array();
        Ok(Value::array_from_vec(ary.to_vec()))
    } else {
        Ok(self_val)
    }
}

///
/// ### Array#to_h
///
/// - to_h -> Hash
/// - to_h {|elem| .. } -> Hash
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/to_h.html]
#[monoruby_builtin]
fn to_h(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    fn inner(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<()> {
        let self_val = lfp.self_val();
        let block = match lfp.block() {
            Some(bh) => Some(vm.get_block_data(globals, bh)?),
            None => None,
        };
        let mut i = 0;
        while i < self_val.as_array().len() {
            let orig_elem = self_val.as_array()[i];
            let elem = if let Some(p) = &block {
                vm.invoke_block(globals, p, &[orig_elem])?
            } else {
                orig_elem
            };
            let elem = match elem.try_array_ty() {
                Some(a) => a,
                None => {
                    return Err(MonorubyErr::typeerr(format!(
                        "wrong element type {} at {i} (expected array)",
                        elem.get_real_class_name(&globals.store)
                    )));
                }
            };
            if elem.len() != 2 {
                return Err(MonorubyErr::argumenterr(format!(
                    "wrong array length at {i} (expected 2, was {})",
                    elem.len()
                )));
            }
            vm.temp_hash_insert(globals, elem[0], elem[1])?;
            i += 1;
        }
        Ok(())
    }

    vm.temp_push(Value::hash(RubyMap::default()));
    let err = inner(vm, globals, lfp);
    let res = vm.temp_pop();
    err?;
    Ok(res)
}

///
/// ### Array#hash
///
/// - hash -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/hash.html]
#[monoruby_builtin]
fn hash(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let h = lfp.self_val().calculate_hash(vm, globals)?;
    Ok(Value::integer_from_u64(h))
}

///
/// ### Array#+
///
/// - self + other -> Array
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/=2b.html]
#[monoruby_builtin]
fn add(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let lhs = lfp.self_val().as_array();
    let rhs = lfp.arg(0).coerce_to_array(vm, globals)?;
    let mut v = lhs.to_vec();
    v.extend_from_slice(&rhs);
    Ok(Value::array_from_vec(v))
}

///
/// ### Array#-
///
/// - self - other -> Array
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/=2d.html]
#[monoruby_builtin]
fn sub(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let lhs_v = lfp.self_val();
    let rhs = lfp.arg(0).coerce_to_array(vm, globals)?;
    let mut v = vec![];
    for lhs in lhs_v.as_array().iter() {
        let mut flag = true;
        for rhs in rhs.iter() {
            if vm.eq_values_bool(globals, *lhs, *rhs)? {
                flag = false;
                break;
            }
        }
        if flag {
            v.push(*lhs)
        }
    }
    Ok(Value::array_from_vec(v))
}

///
/// ### Array#*
///
/// - self * times -> Array
/// - self * sep -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/=2a.html]
#[monoruby_builtin]
fn mul(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let lhs = lfp.self_val().as_array();
    let arg = lfp.arg(0);
    if let Some(v) = arg.try_fixnum() {
        if v < 0 {
            return Err(MonorubyErr::negative_argument());
        }
        let rhs = v as usize;
        let vec = lhs.repeat(rhs);
        Ok(Value::array_from_vec(vec))
    } else if let Some(sep) = arg.is_str() {
        let mut visited: HashSet<u64> = HashSet::default();
        visited.insert(lhs.id());
        let res = array_join(vm, globals, lhs, sep, &mut visited)?;
        Ok(Value::string(res))
    } else if let Ok(s) = arg.coerce_to_str(vm, globals) {
        // Try to_str coercion for join
        let mut visited: HashSet<u64> = HashSet::default();
        visited.insert(lhs.id());
        let res = array_join(vm, globals, lhs, &s, &mut visited)?;
        Ok(Value::string(res))
    } else {
        // Try to_int coercion for repeat
        let v = arg.coerce_to_int_i64(vm, globals)?;
        if v < 0 {
            return Err(MonorubyErr::negative_argument());
        }
        let rhs = v as usize;
        let vec = lhs.repeat(rhs);
        Ok(Value::array_from_vec(vec))
    }
}

///
/// ### Array#&
///
/// - self & other -> Array
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/=26.html]
#[monoruby_builtin]
fn and(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let mut lhs = Array::new_from_vec(lfp.self_val().as_array().to_vec());
    let rhs = lfp.arg(0).coerce_to_array(vm, globals)?;
    lhs.uniq(vm, globals)?;
    lhs.retain(|v| Ok(rhs.contains(v)))?;
    Ok(lhs.as_val())
}

///
/// ### Array#|
///
/// - self | other -> Array
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/=7c.html]
#[monoruby_builtin]
fn or(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let mut lhs = Array::new_from_vec(lfp.self_val().as_array().to_vec());
    let rhs = lfp.arg(0).coerce_to_array(vm, globals)?;
    lhs.extend(rhs.iter().cloned());
    lhs.uniq(vm, globals)?;
    Ok(lhs.as_val())
}

///
/// ### Array#shift
/// - shift -> object | nil
/// - shift(n) -> Array
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/shift.html]
#[monoruby_builtin]
fn shift(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    lfp.self_val().ensure_not_frozen(&globals.store)?;
    let mut ary = lfp.self_val().as_array();
    if lfp.try_arg(0).is_none() {
        if ary.len() == 0 {
            return Ok(Value::nil());
        }
        let res = ary[0];
        ary.drain(0..1);
        Ok(res)
    } else {
        let i = lfp.arg(0).coerce_to_int_i64(vm, globals)?;
        if i < 0 {
            return Err(MonorubyErr::negative_array_size());
        }
        let num = std::cmp::min(i as usize, ary.len());
        let iter = ary.drain(0..num);
        Ok(Value::array_from_iter(iter))
    }
}

///
/// ### Array#unshift
///
/// - unshift(*obj) -> self
/// - prepend(*obj) -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/prepend.html]
#[monoruby_builtin]
fn unshift(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    lfp.self_val().ensure_not_frozen(&globals.store)?;
    let mut ary = lfp.self_val().as_array();
    ary.insert_many(0, lfp.arg(0).as_array().iter().cloned());
    Ok(ary.into())
}

///
/// ### Array#concat
///
/// - concat(other) -> self
/// - concat(*other_arrays) -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/concat.html]
#[monoruby_builtin]
fn concat(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    lfp.self_val().ensure_not_frozen(&globals.store)?;
    let mut self_ary = lfp.self_val().as_array();
    let mut ary: Array = Array::new_empty();
    for a in lfp.arg(0).as_array().iter().cloned() {
        let converted = a.coerce_to_array(vm, globals)?;
        ary.extend_from_slice(&converted);
    }
    self_ary.extend_from_slice(&ary);
    Ok(self_ary.into())
}

///
/// ### Array#<<
///
/// - self << obj -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/=3c=3c.html]
#[monoruby_builtin]
fn shl(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    lfp.self_val().ensure_not_frozen(&globals.store)?;
    let mut ary = lfp.self_val().as_array();
    ary.push(lfp.arg(0));
    Ok(ary.into())
}

extern "C" fn ary_shl(mut ary: Array, arg: Value) -> Value {
    ary.push(arg);
    ary.into()
}

fn array_shl(
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
    let CallSiteInfo {
        dst, args, recv, ..
    } = *callsite;
    state.load(ir, recv, GP::Rdi);
    state.load(ir, args, GP::Rsi);
    let using_xmm = state.get_using_xmm();
    ir.xmm_save(using_xmm);
    ir.inline(move |r#gen, _, _| {
        monoasm!( &mut r#gen.jit,
            movq rax, (ary_shl);
            call rax;
        );
    });
    ir.xmm_restore(using_xmm);
    state.def_reg2acc_class(ir, GP::Rax, dst, recv_class);
    true
}

///
/// ### Array#append
///
/// - push(*obj) -> self
/// - append(*obj) -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/append.html]
#[monoruby_builtin]
fn push(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    lfp.self_val().ensure_not_frozen(&globals.store)?;
    let mut ary = lfp.self_val().as_array();
    ary.extend(lfp.arg(0).as_array().iter().cloned());
    Ok(ary.into())
}

///
/// ### Array#pop
///
/// - pop -> object | nil
/// - pop(n) -> Array
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/pop.html]
#[monoruby_builtin]
fn pop(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    lfp.self_val().ensure_not_frozen(&globals.store)?;
    let mut ary = lfp.self_val().as_array();
    if let Some(n) = lfp.try_arg(0) {
        let n = n.coerce_to_int_i64(vm, globals)?;
        if n < 0 {
            return Err(MonorubyErr::argumenterr("negative array size"));
        }
        let n = n as usize;
        let len = ary.len();
        let start = if n >= len { 0 } else { len - n };
        let result = Value::array_from_iter(ary[start..].iter().cloned());
        ary.truncate(start);
        Ok(result)
    } else {
        let res = ary.pop().unwrap_or_default();
        Ok(res)
    }
}

///
/// ### Array#eql?
///
/// - self.eql?(other) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/eql=3f.html]
#[monoruby_builtin]
fn eql(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let arg = lfp.arg(0);
    let lhs = self_val.as_array();
    let rhs = if let Some(rhs) = arg.try_array_ty() {
        rhs
    } else {
        return Ok(Value::bool(false));
    };
    if lhs.len() != rhs.len() {
        return Ok(Value::bool(false));
    }
    crate::value::exec_recursive_paired(self_val.id(), arg.id(), || {
        for i in 0..lhs.len() {
            if !lhs[i].eql(&rhs[i], vm, globals)? {
                return Ok(Value::bool(false));
            }
        }
        Ok(Value::bool(true))
    }, Value::bool(true))
}

///
/// ### Array#==
///
/// - self == other -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/=3d=3d.html]
#[monoruby_builtin]
fn eq(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let arg = lfp.arg(0);
    let lhs = self_val.as_array();
    let rhs = if let Some(rhs) = arg.try_array_ty() {
        rhs
    } else {
        return Ok(Value::bool(false));
    };
    if lhs.len() != rhs.len() {
        return Ok(Value::bool(false));
    }
    crate::value::exec_recursive_paired(self_val.id(), arg.id(), || {
        for i in 0..lhs.len() {
            if vm.ne_values_bool(globals, lhs[i], rhs[i])? {
                return Ok(Value::bool(false));
            }
        }
        Ok(Value::bool(true))
    }, Value::bool(true))
}

///
/// ### Array#<=>
///
/// - self <=> other -> -1 | 0 | 1 | nil
///
/// [https://docs.ruby-lang.org/ja/3.2/method/Array/i/=3c=3d=3e.html]
#[monoruby_builtin]
fn cmp(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let arg = lfp.arg(0);
    let lhs = self_val.as_array();
    let rhs = if let Some(rhs) = arg.try_array_ty() {
        rhs
    } else if let Some(fid) = globals.check_method(arg, IdentId::TO_ARY) {
        let v = vm.invoke_func_inner(globals, fid, arg, &[], None, None)?;
        if let Some(ary) = v.try_array_ty() {
            ary
        } else {
            return Ok(Value::nil());
        }
    } else {
        return Ok(Value::nil());
    };
    crate::value::exec_recursive_paired(
        self_val.id(),
        arg.id(),
        || {
            for (i, lhs) in lhs.iter().enumerate() {
                if let Some(rhs) = rhs.get(i) {
                    match vm.compare_values_inner(globals, *lhs, *rhs)? {
                        Some(res) if res != Ordering::Equal => {
                            return Ok(Value::integer(res as i64));
                        }
                        Some(_) => {} // Equal, continue
                        None => return Ok(Value::nil()),
                    }
                } else {
                    return Ok(Value::integer(Ordering::Greater as i64));
                }
            }
            if rhs.len() > lhs.len() {
                return Ok(Value::integer(Ordering::Less as i64));
            }
            Ok(Value::integer(Ordering::Equal as i64))
        },
        Value::integer(Ordering::Equal as i64),
    )
}

///
/// ### Array#[]
///
/// - self[nth] -> object | nil
/// - self[range] -> Array | nil
/// - self[start, length] -> Array | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/=5b=5d.html]
#[monoruby_builtin]
fn index(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let ary = lfp.self_val().as_array();
    if let Some(arg1) = lfp.try_arg(1) {
        let arg0 = lfp.arg(0);
        // Coerce indices via to_int if needed
        let arg0 = if arg0.try_fixnum().is_none() {
            Value::integer(arg0.coerce_to_int_i64(vm, globals)?)
        } else {
            arg0
        };
        let arg1 = if arg1.try_fixnum().is_none() {
            Value::integer(arg1.coerce_to_int_i64(vm, globals)?)
        } else {
            arg1
        };
        ary.get_elem2(vm, globals, arg0, arg1)
    } else {
        let idx = lfp.arg(0);
        // If the index is not a fixnum or range, try to_int coercion
        if idx.try_fixnum().is_none() && idx.is_range().is_none() {
            let i = idx.coerce_to_int_i64(vm, globals)?;
            return ary.get_elem1(vm, globals, Value::integer(i));
        }
        ary.get_elem1(vm, globals, idx)
    }
}

fn array_index(
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
    let CallSiteInfo {
        recv, args, dst, ..
    } = *callsite;
    let dst = if let Some(dst) = dst {
        dst
    } else {
        return true;
    };
    state.array_integer_index(ir, store, dst, recv, args);
    true
}

///
/// ### Array#[]=
///
/// - self[nth] = val
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/=5b=5d=3d.html]
#[monoruby_builtin]
fn index_assign(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    lfp.self_val().ensure_not_frozen(&globals.store)?;
    let mut ary = lfp.self_val().as_array();
    if lfp.try_arg(2).is_none() {
        let i = lfp.arg(0);
        let val = lfp.arg(1);
        if let Some(idx) = i.try_fixnum() {
            ary.set_index(idx, val)
        } else if let Some(range) = i.is_range() {
            let start = ary.get_array_index_nil_or(vm, globals, range.start(), 0)?;
            let len = if range.end().is_nil() {
                // endless range: length is from start to end of array
                ary.len().checked_sub(start)
            } else {
                let end = ary.get_array_index_nil_or(vm, globals, range.end(), ary.len())?;
                if range.exclude_end() {
                    end.checked_sub(start)
                } else {
                    (end + 1).checked_sub(start)
                }
            };
            if let Some(len) = len {
                ary.set_index2(start as usize, len as usize, val)
            } else {
                // end < start: treat as zero-length replacement at start
                ary.set_index2(start as usize, 0, val)
            }
        } else {
            let idx = i.coerce_to_int_i64(vm, globals)?;
            ary.set_index(idx, val)
        }
    } else {
        let mut i = lfp.arg(0).coerce_to_int_i64(vm, globals)?;
        let l = lfp.arg(1).coerce_to_int_i64(vm, globals)?;
        if l < 0 {
            return Err(MonorubyErr::indexerr(format!("negative length ({})", l)));
        }
        if i < 0 {
            i += ary.len() as i64;
            if i < 0 {
                return Err(MonorubyErr::index_too_small(
                    lfp.arg(0).coerce_to_int_i64(vm, globals)?,
                    0,
                ));
            }
        }
        let val = lfp.arg(2);
        ary.set_index2(i as usize, l as usize, val)
    }
}

fn array_index_assign(
    state: &mut AbstractState,
    ir: &mut AsmIr,
    _: &JitContext,
    store: &Store,
    callid: CallSiteId,
    _: ClassId,
    idx_class: Option<ClassId>,
) -> bool {
    let callsite = &store[callid];
    if !callsite.is_simple() || callsite.pos_num != 2 || idx_class != Some(INTEGER_CLASS) {
        return false;
    }
    let CallSiteInfo {
        recv, args, dst, ..
    } = *callsite;
    assert!(dst.is_none());
    state.array_integer_index_assign(ir, store, args + 1usize, recv, args);
    true
}

///
/// ### Array#clear
///
/// - clear -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/clear.html]
#[monoruby_builtin]
fn clear(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    lfp.expect_no_block()?;
    lfp.self_val().ensure_not_frozen(&globals.store)?;
    let mut ary = lfp.self_val().as_array();
    ary.clear();
    Ok(ary.into())
}

///
/// ### Array#replace
///
/// - replace(other) -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/replace.html]
#[monoruby_builtin]
fn replace(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    lfp.self_val().ensure_not_frozen(&globals.store)?;
    let mut self_ary = lfp.self_val().as_array();
    let other = lfp.arg(0).coerce_to_array(vm, globals)?;
    // Copy elements first to handle the case where self and other are the same array.
    let elems: Vec<Value> = other.iter().cloned().collect();
    self_ary.clear();
    self_ary.extend_from_slice(&elems);
    Ok(self_ary.into())
}

///
/// ### Array#fill
///
/// - fill(val) -> self
/// - fill(val, start, length = nil) -> self
/// - fill(val, range) -> self
/// - fill {|index| ... } -> self
/// - fill(start, length = nil) {|index| ... } -> self
/// - fill(range) {|index| ... } -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/fill.html]
#[monoruby_builtin]
fn fill(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    lfp.self_val().ensure_not_frozen(&globals.store)?;
    let mut ary = lfp.self_val().as_array();

    if let Some(bh) = lfp.block() {
        // Block form: fill {}, fill(start) {}, fill(start, length) {}, fill(range) {}
        let data = vm.get_block_data(globals, bh)?;
        let (start, end_idx) = if let Some(arg0) = lfp.try_arg(0) {
            if let Some(range) = arg0.is_range() {
                // fill(range) {|index| ... } -> self
                fill_range_indices(vm, globals, &ary, range)?
            } else {
                // fill(start, length = nil) {|index| ... } -> self
                let start = arg0.coerce_to_int_i64(vm, globals)?;
                let start = if start < 0 {
                    let s = ary.len() as i64 + start;
                    if s < 0 { 0i64 } else { s }
                } else {
                    start
                } as usize;
                if let Some(len_val) = lfp.try_arg(1) {
                    if len_val.is_nil() {
                        // nil length means fill to end
                        (start, ary.len())
                    } else {
                        let len = len_val.coerce_to_int_i64(vm, globals)?;
                        if len <= 0 {
                            return Ok(ary.into());
                        }
                        let end_idx = start
                            .checked_add(len as usize)
                            .ok_or_else(|| MonorubyErr::argumenterr("argument too big"))?;
                        (start, end_idx)
                    }
                } else {
                    // fill {|index| ... } -> self
                    (start, ary.len())
                }
            }
        } else {
            (0, ary.len())
        };
        if end_idx > ary.len() {
            fill_resize(&mut ary, end_idx)?;
        }
        for i in start..end_idx {
            let val = vm.invoke_block(globals, &data, &[Value::integer(i as i64)])?;
            ary[i] = val;
        }
    } else {
        // Non-block form: fill(val), fill(val, start), fill(val, start, length), fill(val, range)
        let val = if let Some(v) = lfp.try_arg(0) {
            v
        } else {
            return Err(MonorubyErr::wrong_number_of_arg_range(0, 1..=3));
        };
        if let Some(arg1) = lfp.try_arg(1) {
            if let Some(range) = arg1.is_range() {
                // fill(val, range) -> self
                let (start, end_idx) = fill_range_indices(vm, globals, &ary, range)?;
                if end_idx > ary.len() {
                    fill_resize(&mut ary, end_idx)?;
                }
                for i in start..end_idx {
                    ary[i] = val;
                }
            } else {
                // fill(val, start, length = nil) -> self
                let start = arg1.coerce_to_int_i64(vm, globals)?;
                let start = if start < 0 {
                    let s = ary.len() as i64 + start;
                    if s < 0 { 0i64 } else { s }
                } else {
                    start
                } as usize;
                if let Some(len_val) = lfp.try_arg(2) {
                    if len_val.is_nil() {
                        // nil length means fill to end
                        if start > ary.len() {
                            fill_resize(&mut ary, start)?;
                        }
                        let len = ary.len();
                        for i in start..len {
                            ary[i] = val;
                        }
                    } else {
                        let len = len_val.coerce_to_int_i64(vm, globals)?;
                        if len <= 0 {
                            return Ok(ary.into());
                        }
                        let end_idx = start
                            .checked_add(len as usize)
                            .ok_or_else(|| MonorubyErr::argumenterr("argument too big"))?;
                        if end_idx > ary.len() {
                            fill_resize(&mut ary, end_idx)?;
                        }
                        for i in start..end_idx {
                            ary[i] = val;
                        }
                    }
                } else {
                    if start > ary.len() {
                        fill_resize(&mut ary, start)?;
                    }
                    let len = ary.len();
                    for i in start..len {
                        ary[i] = val;
                    }
                }
            }
        } else {
            // fill(val) -> self
            ary.fill(val);
        }
    }
    Ok(ary.into())
}

/// Helper to resize array for fill, raising ArgumentError if too large.
fn fill_resize(ary: &mut Array, new_len: usize) -> Result<()> {
    // Limit to prevent capacity overflow panics. CRuby also refuses huge sizes.
    const MAX_FILL_SIZE: usize = 1 << 30; // ~1 billion elements
    if new_len > MAX_FILL_SIZE {
        return Err(MonorubyErr::argumenterr("argument too big"));
    }
    ary.resize(new_len, Value::nil());
    Ok(())
}

/// Helper to compute (start, end) indices from a Range for Array#fill.
fn fill_range_indices(
    vm: &mut Executor,
    globals: &mut Globals,
    ary: &Array,
    range: &RangeInner,
) -> Result<(usize, usize)> {
    let len = ary.len() as i64;
    // nil begin means 0 (beginless range)
    let mut start = if range.start().is_nil() {
        0i64
    } else {
        range.start().coerce_to_int_i64(vm, globals)?
    };
    if start < 0 {
        start += len;
        if start < 0 {
            return Err(MonorubyErr::rangeerr(format!(
                "{}..{} out of range",
                range.start().coerce_to_int_i64(vm, globals)?,
                if range.end().is_nil() {
                    String::new()
                } else {
                    range.end().coerce_to_int_i64(vm, globals)?.to_string()
                }
            )));
        }
    }
    // nil end means array length (endless range)
    let end_idx = if range.end().is_nil() {
        len
    } else {
        let mut end_val = range.end().coerce_to_int_i64(vm, globals)?;
        if end_val < 0 {
            end_val += len;
        }
        if range.exclude_end() {
            end_val
        } else {
            end_val + 1
        }
    };
    if end_idx < start {
        return Ok((start as usize, start as usize));
    }
    Ok((start as usize, end_idx as usize))
}

///
/// ### Array#drop
///
/// - drop(n) -> Array
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/drop.html]
#[monoruby_builtin]
fn drop(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let ary = lfp.self_val().as_array();
    let num = lfp.arg(0).coerce_to_int_i64(vm, globals)?;
    if num < 0 {
        return Err(MonorubyErr::argumenterr(format!(
            "Attempt to drop negative size. {}",
            num
        )));
    };
    let num = num as usize;
    if num >= ary.len() {
        return Ok(Value::array_empty());
    };
    let ary = &ary[num..];
    Ok(Value::array_from_iter(ary.iter().cloned()))
}

///
/// ### Array#zip
///
/// - zip(*lists) -> [[object]]
/// - zip(*lists) {|v1, v2, ...| ...} -> nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/zip.html]
#[monoruby_builtin]
fn zip(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ary = lfp.self_val().as_array();
    let self_len = self_ary.len();
    let each_id = IdentId::get_id("each");

    // Phase 1: Convert each argument to an Array or collect elements via #each.
    // For arguments responding to #each but not #to_ary, we collect only up to
    // self.len() elements to handle infinite enumerators correctly.
    let mut args_list: Vec<Vec<Value>> = vec![];

    for a in lfp.arg(0).as_array().iter() {
        if let Ok(ary) = a.coerce_to_array(vm, globals) {
            args_list.push(ary.to_vec());
        } else if globals.check_method(*a, each_id).is_some() {
            let to_enum_id = IdentId::get_id("to_enum");
            let enum_val = vm.invoke_method_inner(globals, to_enum_id, *a, &[], None, None)?;
            let next_id = IdentId::get_id("next");
            let mut collected = Vec::with_capacity(self_len);
            for _ in 0..self_len {
                match vm.invoke_method_inner(globals, next_id, enum_val, &[], None, None) {
                    Ok(val) => collected.push(val),
                    Err(_) => break, // StopIteration — enumerator exhausted
                }
            }
            args_list.push(collected);
        } else {
            return Err(MonorubyErr::typeerr(format!(
                "wrong argument type {} (must respond to :each)",
                a.get_real_class_name(&globals.store),
            )));
        }
    }

    // Phase 2: Build result
    // Helper to build a single row for index i
    let build_row = |i: usize, val: Value, args: &[Vec<Value>]| -> Array {
        let mut row = Array::new_empty();
        row.push(val);
        for arg in args {
            row.push(if i < arg.len() {
                arg[i]
            } else {
                Value::nil()
            });
        }
        row
    };

    if let Some(bh) = lfp.block() {
        let data = vm.get_block_data(globals, bh)?;
        for (i, val) in self_ary.iter().enumerate() {
            let row = build_row(i, *val, &args_list);
            vm.invoke_block(globals, &data, &[row.as_val()])?;
        }
        Ok(Value::nil())
    } else {
        let mut result_ary = Array::new_empty();
        for (i, val) in self_ary.iter().enumerate() {
            let row = build_row(i, *val, &args_list);
            result_ary.push(row.as_val());
        }
        Ok(result_ary.as_val())
    }
}

///
/// ### Array#inject
///
/// - inject(init = self.first) {|result, item| ... } -> object
/// - inject(sym) -> object
/// - inject(init, sym) -> object
/// - reduce(init = self.first) {|result, item| ... } -> object
/// - reduce(sym) -> object
/// - reduce(init, sym) -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Enumerable/i/inject.html]
#[monoruby_builtin]
fn inject(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val().as_array();
    let mut iter = self_.iter().cloned();
    if let Some(bh) = lfp.block() {
        let res = if lfp.try_arg(0).is_none() {
            iter.next().unwrap_or_default()
        } else {
            lfp.arg(0)
        };
        vm.invoke_block_fold1(globals, bh, iter, res)
    } else {
        let (sym, mut res) = if let Some(arg0) = lfp.try_arg(0) {
            if let Some(arg1) = lfp.try_arg(1) {
                (arg1.expect_symbol_or_string(globals)?, arg0)
            } else {
                let sym = arg0.expect_symbol_or_string(globals)?;
                let res = iter.next().unwrap_or_default();
                (sym, res)
            }
        } else {
            return Err(MonorubyErr::argumenterr("wrong number of arguments"));
        };
        for v in iter {
            res = vm.invoke_method_inner(globals, sym, res, &[v], None, None)?;
        }
        Ok(res)
    }
}

///
/// ### Array#join
///
/// - join(sep = $,) -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/join.html]
#[monoruby_builtin]
fn join(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let arg0 = lfp.try_arg(0);
    let sep = if let Some(sep) = &arg0 {
        if sep.is_nil() {
            // When nil is passed explicitly, use $, as default separator
            get_output_field_separator(globals)
        } else {
            // Coerce separator via to_str
            Some(sep.coerce_to_string(vm, globals)?)
        }
    } else {
        // No argument: use $, as default separator
        get_output_field_separator(globals)
    };
    let sep_str = sep.as_deref().unwrap_or("");
    let ary = lfp.self_val().as_array();
    let mut visited: HashSet<u64> = HashSet::default();
    visited.insert(ary.id());
    let res = array_join(vm, globals, ary, sep_str, &mut visited)?;
    Ok(Value::string(res))
}

/// Get the value of $, (output field separator) as a String.
fn get_output_field_separator(globals: &mut Globals) -> Option<String> {
    let gvar_id = IdentId::get_id("$,");
    match globals.get_gvar(gvar_id) {
        Some(v) if !v.is_nil() => Some(v.to_s(&globals.store)),
        _ => None,
    }
}

fn array_join(
    vm: &mut Executor,
    globals: &mut Globals,
    ary: Array,
    sep: &str,
    visited: &mut HashSet<u64>,
) -> Result<String> {
    let mut result = String::new();
    for (i, v) in ary.iter().enumerate() {
        if i > 0 {
            result.push_str(sep);
        }
        // If element is already a string, use it directly
        if let Some(s) = v.is_str() {
            result.push_str(s);
            continue;
        }
        // If element is an array, recursively join
        if let Some(inner_ary) = v.try_array_ty() {
            if !visited.insert(inner_ary.id()) {
                return Err(MonorubyErr::argumenterr(
                    "recursive array join",
                ));
            }
            let s = array_join(vm, globals, inner_ary, sep, visited)?;
            visited.remove(&inner_ary.id());
            result.push_str(&s);
            continue;
        }
        // Conversion order: to_str -> to_ary -> to_s
        // Try to_str first
        if let Some(fid) = globals.check_method(*v, IdentId::TO_STR) {
            let ret = vm.invoke_func_inner(globals, fid, *v, &[], None, None)?;
            if let Some(s) = ret.is_str() {
                result.push_str(s);
                continue;
            }
            // to_str returned non-nil non-string: fall through
            if !ret.is_nil() {
                result.push_str(&ret.to_s(&globals.store));
                continue;
            }
        }
        // Try to_ary second
        if let Some(fid) = globals.check_method(*v, IdentId::TO_ARY) {
            let ret = vm.invoke_func_inner(globals, fid, *v, &[], None, None)?;
            if let Some(inner_ary) = ret.try_array_ty() {
                if !visited.insert(inner_ary.id()) {
                    return Err(MonorubyErr::argumenterr(
                        "recursive array join",
                    ));
                }
                let s = array_join(vm, globals, inner_ary, sep, visited)?;
                visited.remove(&inner_ary.id());
                result.push_str(&s);
                continue;
            }
            if !ret.is_nil() {
                result.push_str(&ret.to_s(&globals.store));
                continue;
            }
        }
        // Try to_s third
        if let Some(fid) = globals.check_method(*v, IdentId::TO_S) {
            let ret = vm.invoke_func_inner(globals, fid, *v, &[], None, None)?;
            if let Some(s) = ret.is_str() {
                result.push_str(s);
                continue;
            }
        }
        // Fallback: use Rust-level to_s
        result.push_str(&v.to_s(&globals.store));
    }
    Ok(result)
}

///
/// ### Array#first
///
/// - first -> object | nil
/// - first(n) -> Array
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/first.html]
#[monoruby_builtin]
fn first(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let ary = lfp.self_val().as_array();
    if lfp.try_arg(0).is_none() {
        Ok(ary.first().cloned().unwrap_or_default())
    } else {
        let n = lfp.arg(0).coerce_to_int_i64(vm, globals)?;
        if n < 0 {
            return Err(MonorubyErr::argumenterr("must be positive."));
        }
        let n = if n as usize > ary.len() {
            ary.len()
        } else {
            n as usize
        };
        Ok(Value::array_from_iter(ary[0..n].iter().cloned()))
    }
}

///
/// ### Array#last
///
/// - last -> object | nil
/// - last(n) -> Array
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/last.html]
#[monoruby_builtin]
fn last(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let ary = lfp.self_val().as_array();
    if lfp.try_arg(0).is_none() {
        Ok(ary.last().cloned().unwrap_or_default())
    } else {
        let n = lfp.arg(0).coerce_to_int_i64(vm, globals)?;
        if n < 0 {
            return Err(MonorubyErr::argumenterr("must be positive."));
        }
        let n = if n as usize > ary.len() {
            0
        } else {
            ary.len() - n as usize
        };
        Ok(Value::array_from_iter(ary[n..].iter().cloned()))
    }
}

///
/// ### Array#fetch
///
/// - fetch(nth) -> object
/// - fetch(nth, ifnone) -> object
/// - fetch(nth) {|nth| ... } -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/fetch.html]
#[monoruby_builtin]
fn fetch(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let ary = lfp.self_val().as_array();
    let index = lfp.arg(0).coerce_to_int_i64(vm, globals)?;
    let resolved = ary.get_array_index(index);
    if let Some(idx) = resolved {
        if let Some(val) = ary.get(idx) {
            return Ok(*val);
        }
    }
    // Index out of bounds
    if let Some(bh) = lfp.block() {
        let data = vm.get_block_data(globals, bh)?;
        let val = vm.invoke_block(globals, &data, &[lfp.arg(0)])?;
        Ok(val)
    } else if let Some(default) = lfp.try_arg(1) {
        Ok(default)
    } else {
        Err(MonorubyErr::indexerr(format!(
            "index {} outside of array bounds: {}...{}",
            index,
            -(ary.len() as i64),
            ary.len()
        )))
    }
}

///
/// ### Array#take
///
/// - take(n) -> Array
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/take.html]
#[monoruby_builtin]
fn take(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let ary = lfp.self_val().as_array();
    let n = lfp.arg(0).coerce_to_int_i64(vm, globals)?;
    if n < 0 {
        return Err(MonorubyErr::argumenterr("must be positive."));
    }
    let n = n as usize;
    if n > ary.len() {
        Ok(ary.as_val().dup())
    } else {
        Ok(Value::array_from_iter(ary[0..n].iter().cloned()))
    }
}

///
/// ### Array#sum
///
/// - sum(init=0) -> object
/// - sum(init=0) {|e| expr } -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/sum.html]
#[monoruby_builtin]
fn sum(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let mut sum = if lfp.try_arg(0).is_none() {
        Value::i32(0)
    } else {
        lfp.arg(0)
    };
    let self_ = lfp.self_val().as_array();
    let iter = self_.iter().cloned();
    match lfp.block() {
        None => {
            for v in iter {
                sum =
                    executor::op::add_values(vm, globals, sum, v).ok_or_else(|| vm.take_error())?;
            }
        }
        Some(bh) => {
            let data = vm.get_block_data(globals, bh)?;
            for v in iter {
                let rhs = vm.invoke_block(globals, &data, &[v])?;
                sum = executor::op::add_values(vm, globals, sum, rhs)
                    .ok_or_else(|| vm.take_error())?;
            }
        }
    }
    Ok(sum)
}

///
/// ### Array#min
///
/// - min -> object | nil
/// - min {|a, b| ... } -> object | nil
/// - [NOT SUPPORTED] min(n) -> Array
/// - [NOT SUPPORTED] min(n) {|a, b| ... } -> Array
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/min.html]
#[monoruby_builtin]
fn min(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let ary = lfp.self_val().as_array();
    if ary.len() == 0 {
        return Ok(Value::nil());
    }
    if let Some(bh) = lfp.block() {
        let data = vm.get_block_data(globals, bh)?;
        let mut min = ary[0];
        for v in &ary[1..] {
            let block_res = vm.invoke_block(globals, &data, &[*v, min])?;
            if block_res.is_nil() {
                return Err(MonorubyErr::argumenterr("comparison of elements failed"));
            }
            let res = block_res.coerce_to_int_i64(vm, globals)?;
            if res < 0 {
                min = *v;
            }
        }
        Ok(min)
    } else {
        let mut min = ary[0];
        for v in &ary[1..] {
            if vm.compare_values(globals, min, *v)? == std::cmp::Ordering::Greater {
                min = *v;
            }
        }
        Ok(min)
    }
}

///
/// ### Array#max
///
/// - max -> object | nil
/// - max {|a, b| ... } -> object | nil
/// - [NOT SUPPORTED] max(n) -> Array
/// - [NOT SUPPORTED] max(n) {|a, b| ... } -> Array
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/max.html]
#[monoruby_builtin]
fn max(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let ary = lfp.self_val().as_array();
    if ary.len() == 0 {
        return Ok(Value::nil());
    }
    if let Some(bh) = lfp.block() {
        let data = vm.get_block_data(globals, bh)?;
        let mut max = ary[0];
        for v in &ary[1..] {
            let block_res = vm.invoke_block(globals, &data, &[*v, max])?;
            if block_res.is_nil() {
                return Err(MonorubyErr::argumenterr("comparison of elements failed"));
            }
            let res = block_res.coerce_to_int_i64(vm, globals)?;
            if res > 0 {
                max = *v;
            }
        }
        Ok(max)
    } else {
        let mut max = ary[0];
        for v in &ary[1..] {
            if vm.compare_values(globals, max, *v)? == std::cmp::Ordering::Less {
                max = *v;
            }
        }
        Ok(max)
    }
}

///
/// ### Enumerable#partition
///
/// - [NOT SUPPORTED] partition -> Enumerator
/// - partition {|item| ... } -> [[object], [object]]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/max.html]
#[monoruby_builtin]
fn partition(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let bh = lfp.expect_block()?;
    let self_val = lfp.self_val();
    self_val.expect_array_ty(globals)?;
    let mut res_true = vec![];
    let mut res_false = vec![];
    let p = vm.get_block_data(globals, bh)?;
    let mut i = 0;
    while i < self_val.as_array().len() {
        let elem = self_val.as_array()[i];
        if vm.invoke_block(globals, &p, &[elem])?.as_bool() {
            res_true.push(elem);
        } else {
            res_false.push(elem);
        };
        i += 1;
    }
    Ok(Value::array2(
        Value::array_from_vec(res_true),
        Value::array_from_vec(res_false),
    ))
}

fn sort_inner(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, mut ary: Array) -> Result<Value> {
    if let Some(bh) = lfp.block() {
        let data = vm.get_block_data(globals, bh)?;
        let f = |lhs: Value, rhs: Value| -> Result<std::cmp::Ordering> {
            let block_res = vm.invoke_block(globals, &data, &[lhs, rhs])?;
            if block_res.is_nil() {
                return Err(MonorubyErr::argumenterr("comparison of elements failed"));
            }
            // Try to use <=> with 0 for non-Integer results
            let res = match block_res.try_fixnum() {
                Some(i) => i,
                None => {
                    // Try coercing via <=> with 0
                    let cmp = vm.invoke_method_inner(
                        globals,
                        IdentId::_CMP,
                        block_res,
                        &[Value::integer(0)],
                        None,
                        None,
                    )?;
                    if cmp.is_nil() {
                        return Err(MonorubyErr::argumenterr(
                            "comparison of elements failed",
                        ));
                    }
                    cmp.coerce_to_int_i64(vm, globals)?
                }
            };
            Ok(match res {
                0 => std::cmp::Ordering::Equal,
                res if res < 0 => std::cmp::Ordering::Less,
                _ => std::cmp::Ordering::Greater,
            })
        };
        executor::op::sort_by(&mut ary, f)?;
    } else {
        vm.sort(globals, &mut ary)?;
    }
    Ok(ary.into())
}

///
/// ### Array#sort!
///
/// - sort! -> self
/// - sort! {|a, b| ... } -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/sort.html]
#[monoruby_builtin]
fn sort_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    lfp.self_val().ensure_not_frozen(&globals.store)?;
    let ary = lfp.self_val().as_array();
    let gc_enabled = Globals::gc_enable(false);
    let res = sort_inner(vm, globals, lfp, ary);
    Globals::gc_enable(gc_enabled);
    res
}

///
/// ### Array#sort
///
/// - sort -> Array
/// - sort {|a, b| ... } -> Array
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/sort.html]
#[monoruby_builtin]
fn sort(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let ary = Array::new_from_vec(lfp.self_val().as_array().to_vec());
    let gc_enabled = Globals::gc_enable(false);
    let res = sort_inner(vm, globals, lfp, ary);
    Globals::gc_enable(gc_enabled);
    res
}

///
/// ### Array#sort_by!
///
/// - sort_by! {|item| ... } -> self
/// - sort_by! -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/sort_by=21.html]
#[monoruby_builtin]
fn sort_by_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, pc: BytecodePtr) -> Result<Value> {
    if let Some(bh) = lfp.block() {
        lfp.self_val().ensure_not_frozen(&globals.store)?;
        let data = vm.get_block_data(globals, bh)?;
        let f = |lhs: Value, rhs: Value| -> Result<std::cmp::Ordering> {
            let lhs = vm.invoke_block(globals, &data, &[lhs])?;
            let rhs = vm.invoke_block(globals, &data, &[rhs])?;
            Executor::compare_values(vm, globals, lhs, rhs)
        };
        let mut ary = lfp.self_val().as_array();
        let gc_enabled = Globals::gc_enable(false);
        let res = executor::op::sort_by(&mut ary, f);
        Globals::gc_enable(gc_enabled);
        res?;
        Ok(ary.into())
    } else {
        vm.generate_enumerator(IdentId::get_id("sort_by!"), lfp.self_val(), vec![], pc)
    }
}

///
/// ### Enumerable#sort_by
///
/// - sort_by {|item| ... } -> [object]
/// - [NOT SUPPORTED] sort_by -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/Enumerable/i/sort_by.html]
#[monoruby_builtin]
fn sort_by(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, pc: BytecodePtr) -> Result<Value> {
    if let Some(bh) = lfp.block() {
        let data = vm.get_block_data(globals, bh)?;
        let f = |lhs: Value, rhs: Value| -> Result<std::cmp::Ordering> {
            let lhs = vm.invoke_block(globals, &data, &[lhs])?;
            let rhs = vm.invoke_block(globals, &data, &[rhs])?;
            Executor::compare_values(vm, globals, lhs, rhs)
        };
        let mut ary = Array::new_from_vec(lfp.self_val().as_array().to_vec());
        let gc_enabled = Globals::gc_enable(false);
        let res = executor::op::sort_by(&mut ary, f);
        Globals::gc_enable(gc_enabled);
        res?;
        Ok(ary.into())
    } else {
        vm.generate_enumerator(IdentId::get_id("sort_by"), lfp.self_val(), vec![], pc)
    }
}

///
/// ### Array#filter
///
/// - select -> Enumerator
/// - filter -> Enumerator
/// - select {|item| ... } -> [object]
/// - filter {|item| ... } -> [object]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/filter.html]
#[monoruby_builtin]
fn filter(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, pc: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    if let Some(bh) = lfp.block() {
        let data = vm.get_block_data(globals, bh)?;
        let mut res = vec![];
        let mut i = 0;
        while i < self_val.as_array().len() {
            let elem = self_val.as_array()[i];
            if vm.invoke_block(globals, &data, &[elem])?.as_bool() {
                res.push(elem);
            };
            i += 1;
        }
        Ok(Value::array_from_vec(res))
    } else {
        vm.generate_enumerator(IdentId::get_id("filter"), lfp.self_val(), vec![], pc)
    }
}

///
/// ### Array#filter!
///
/// - select! {|item| block } -> self | nil
/// - select! -> Enumerator
/// - filter! {|item| block } -> self | nil
/// - filter! -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/filter=21.html]
#[monoruby_builtin]
fn filter_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, pc: BytecodePtr) -> Result<Value> {
    if let Some(bh) = lfp.block() {
        lfp.self_val().ensure_not_frozen(&globals.store)?;
        let mut ary = lfp.self_val().as_array();
        let data = vm.get_block_data(globals, bh)?;
        let changed = ary
            .retain(|v| {
                vm.invoke_block(globals, &data, &[*v])
                    .map(|res| res.as_bool())
            })?
            .is_some();
        Ok(if changed {
            lfp.self_val()
        } else {
            Value::nil()
        })
    } else {
        vm.generate_enumerator(IdentId::get_id("filter!"), lfp.self_val(), vec![], pc)
    }
}

///
/// ### Array#keep_if
///
/// - keep_if {|item| block } -> self
/// - keep_if -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/keep_if.html]
#[monoruby_builtin]
fn keep_if(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, pc: BytecodePtr) -> Result<Value> {
    if let Some(bh) = lfp.block() {
        lfp.self_val().ensure_not_frozen(&globals.store)?;
        let mut ary = lfp.self_val().as_array();
        let data = vm.get_block_data(globals, bh)?;
        ary.retain(|v| {
            vm.invoke_block(globals, &data, &[*v])
                .map(|res| res.as_bool())
        })?;
        Ok(lfp.self_val())
    } else {
        vm.generate_enumerator(IdentId::get_id("keep_if"), lfp.self_val(), vec![], pc)
    }
}

///
/// ### Array#reject
///
/// - reject {|x| ... } -> self | nil
/// - reject -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/reject.html]
#[monoruby_builtin]
fn reject(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, pc: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    if let Some(bh) = lfp.block() {
        let data = vm.get_block_data(globals, bh)?;
        let mut res = vec![];
        let mut i = 0;
        while i < self_val.as_array().len() {
            let elem = self_val.as_array()[i];
            if !vm.invoke_block(globals, &data, &[elem])?.as_bool() {
                res.push(elem);
            };
            i += 1;
        }
        Ok(Value::array_from_vec(res))
    } else {
        vm.generate_enumerator(IdentId::get_id("reject"), lfp.self_val(), vec![], pc)
    }
}

///
/// ### Array#reject!
///
/// - reject! {|x| ... } -> self | nil
/// - reject! -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/delete_if.html]
#[monoruby_builtin]
fn reject_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, pc: BytecodePtr) -> Result<Value> {
    if let Some(bh) = lfp.block() {
        lfp.self_val().ensure_not_frozen(&globals.store)?;
        let mut ary = lfp.self_val().as_array();
        let data = vm.get_block_data(globals, bh)?;
        let changed = ary
            .retain(|v| {
                vm.invoke_block(globals, &data, &[*v])
                    .map(|res| !res.as_bool())
            })?
            .is_some();
        Ok(if changed {
            lfp.self_val()
        } else {
            Value::nil()
        })
    } else {
        vm.generate_enumerator(IdentId::get_id("reject!"), lfp.self_val(), vec![], pc)
    }
}

///
/// ### Array#delete_if
///
/// - delete_if {|x| ... } -> self
/// - delete_if -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/delete_if.html]
#[monoruby_builtin]
fn delete_if(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, pc: BytecodePtr) -> Result<Value> {
    if let Some(bh) = lfp.block() {
        lfp.self_val().ensure_not_frozen(&globals.store)?;
        let mut ary = lfp.self_val().as_array();
        let data = vm.get_block_data(globals, bh)?;
        ary.retain(|v| {
            vm.invoke_block(globals, &data, &[*v])
                .map(|res| !res.as_bool())
        })?;
        Ok(lfp.self_val())
    } else {
        vm.generate_enumerator(IdentId::get_id("delete_if"), lfp.self_val(), vec![], pc)
    }
}

///
/// ### Enumerable#group_by
///
/// - group_by -> Enumerator
/// - group_by {|obj| ... } -> Hash
///
/// [https://docs.ruby-lang.org/ja/latest/method/Enumerable/i/group_by.html]
#[monoruby_builtin]
fn group_by(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, pc: BytecodePtr) -> Result<Value> {
    fn inner(
        vm: &mut Executor,
        globals: &mut Globals,
        data: &ProcData,
        self_val: Value,
        map: &mut RubyMap<Value, Value>,
    ) -> Result<()> {
        let mut i = 0;
        while i < self_val.as_array().len() {
            let elem = self_val.as_array()[i];
            let key = vm.invoke_block(globals, &data, &[elem])?;
            map.entry(key, vm, globals)?
                .and_modify(|v: &mut Value| v.as_array().push(elem))
                .or_insert(Value::array1(elem));
            i += 1;
        }
        Ok(())
    }
    let self_val = lfp.self_val();
    if let Some(bh) = lfp.block() {
        let data = vm.get_block_data(globals, bh)?;
        let mut map = RubyMap::default();
        let gc_enabled = Globals::gc_enable(false);
        let res = inner(vm, globals, &data, self_val, &mut map);
        Globals::gc_enable(gc_enabled);
        res?;
        /*for elem in ary.iter() {
            let key = vm.invoke_block(globals, &data, &[*elem]);
            map.entry(HashKey(key?))
                .and_modify(|v: &mut Value| v.as_array().push(*elem))
                .or_insert(Value::array1(*elem));
        }*/
        Ok(Value::hash(map))
    } else {
        vm.generate_enumerator(IdentId::get_id("group_by"), lfp.self_val(), vec![], pc)
    }
}

/*///
/// ### Array#each
///
/// - each -> Enumerator
/// - each {|item| .... } -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/each.html]
#[monoruby_builtin]
fn each(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let ary = lfp.self_val().as_array();
    if let Some(bh) = lfp.block() {
        vm.invoke_block_iter1(globals, bh, ary.iter().cloned())?;
        Ok(ary.into())
    } else {
        vm.generate_enumerator(IdentId::EACH, lfp.self_val(), vec![])
    }
}*/

/*///
/// ### Enumerable#each_with_index
///
/// - each_with_index([NOT SUPPORTED]*args) -> Enumerator
/// - each_with_index([NOT SUPPORTED]*args) {|item, index| ... } -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Enumerable/i/each_with_index.html]
#[monoruby_builtin]
fn each_with_index(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let ary = lfp.self_val().as_array();
    if let Some(bh) = lfp.block() {
        vm.invoke_block_iter_with_index1(globals, bh, ary.iter().cloned())?;
        Ok(ary.into())
    } else {
        vm.generate_enumerator(IdentId::get_id("each_with_index"), lfp.self_val(), vec![])
    }
}*/

/*/// ### Array#map
///
/// - map {|item| ... } -> [object]
/// - map -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/collect.html]
#[monoruby_builtin]
fn map(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let ary = lfp.self_val().as_array();
    let size_hint = ary.len();
    let iter = ary.iter().cloned();
    if let Some(bh) = lfp.block() {
        vm.invoke_block_map1(globals, bh, iter, size_hint)
    } else {
        let id = IdentId::get_id("map");
        vm.generate_enumerator(id, lfp.self_val(), vec![])
    }
}*/

/*/// ### Array#map!
///
/// - collect! {|item| ..} -> self
/// - map! {|item| ..} -> self
/// - collect! -> Enumerator
/// - map! -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/collect=21.html]
#[monoruby_builtin]
fn map_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let ary = lfp.self_val().as_array();
    let size_hint = ary.len();
    let iter = ary.iter().cloned();
    if let Some(bh) = lfp.block() {
        let mut res = vm.invoke_block_map1(globals, bh, iter, size_hint)?;
        RValue::swap_kind(lfp.self_val().rvalue_mut(), res.rvalue_mut());
        Ok(lfp.self_val())
    } else {
        let id = IdentId::get_id("map!");
        vm.generate_enumerator(id, lfp.self_val(), vec![])
    }
}*/

/// ### Enumerable#collect_concat
///
/// - flat_map {| obj | block } -> Array
/// - collect_concat {| obj | block } -> Array
/// - [NOT SUPPORTED] flat_map -> Enumerator
/// - [NOT SUPPORTED] collect_concat -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/Enumerable/i/collect_concat.html]
#[monoruby_builtin]
fn flat_map(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let bh = lfp.expect_block()?;
    let ary = lfp.self_val().as_array();
    let size_hint = ary.len();
    vm.invoke_block_flat_map1(globals, bh, ary.iter().cloned(), size_hint)
}

fn all_any_inner(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    is_all: bool,
) -> Result<Value> {
    let self_val = lfp.self_val();
    if let Some(pattern) = lfp.try_arg(0) {
        // Pattern form: all?(pattern) / any?(pattern) — block is ignored
        if lfp.block().is_some() {
            vm.ruby_warn(globals, "warning: given block not used")?;
        }
        let mut i = 0;
        while i < self_val.as_array().len() {
            let elem = self_val.as_array()[i];
            if op::cmp_teq_values_bool(vm, globals, pattern, elem)? != is_all {
                return Ok(Value::bool(!is_all));
            };
            i += 1;
        }
    } else if let Some(bh) = lfp.block() {
        let data = vm.get_block_data(globals, bh)?;
        let mut i = 0;
        while i < self_val.as_array().len() {
            let elem = self_val.as_array()[i];
            if vm.invoke_block(globals, &data, &[elem])?.as_bool() != is_all {
                return Ok(Value::bool(!is_all));
            };
            i += 1;
        }
    } else {
        let mut i = 0;
        while i < self_val.as_array().len() {
            let elem = self_val.as_array()[i];
            if elem.as_bool() != is_all {
                return Ok(Value::bool(!is_all));
            };
            i += 1;
        }
    }
    Ok(Value::bool(is_all))
}

///
/// #### Array#all?
///
/// - all? -> bool
/// - all? {|item| ... } -> bool
/// - all?(pattern) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/all=3f.html]
#[monoruby_builtin]
fn all_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    all_any_inner(vm, globals, lfp, true)
}

///
/// #### Array#any?
///
/// - any? -> bool
/// - any? {|item| ... } -> bool
/// - any?(pattern) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/any=3f.html]
#[monoruby_builtin]
fn any_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    all_any_inner(vm, globals, lfp, false)
}

///
/// #### Enumerable#detect
///
/// - find([NOT SUPPORTED]ifnone = nil) {|item| ... } -> object
/// - detect([NOT SUPPORTED]ifnone = nil) {|item| ... } -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Enumerable/i/detect.html]
#[monoruby_builtin]
fn detect(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let bh = lfp.expect_block()?;
    let data = vm.get_block_data(globals, bh)?;
    let mut i = 0;
    while i < self_val.as_array().len() {
        let elem = self_val.as_array()[i];
        if vm.invoke_block(globals, &data, &[elem])?.as_bool() {
            return Ok(elem);
        };
        i += 1;
    }
    Ok(Value::nil())
}

///
/// #### Enumerable#grep
///
/// - grep(pattern) -> [object]
/// - grep(pattern) {|item| ... } -> [object]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Enumerable/i/grep.html]
#[monoruby_builtin]
fn grep(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let ary: Vec<_> = match lfp.block() {
        None => {
            let mut res = vec![];
            let mut i = 0;
            while i < self_val.as_array().len() {
                let v = self_val.as_array()[i];
                if cmp_teq_values_bool(vm, globals, lfp.arg(0), v)? {
                    res.push(v)
                }
                i += 1;
            }
            res
        }
        Some(bh) => {
            let bh = vm.get_block_data(globals, bh)?;
            let mut res = vec![];
            let mut i = 0;
            while i < self_val.as_array().len() {
                let v = self_val.as_array()[i];
                if cmp_teq_values_bool(vm, globals, lfp.arg(0), v)? {
                    let mapped = vm.invoke_block(globals, &bh, &[v])?;
                    res.push(mapped);
                }
                i += 1;
            }
            res
        }
    };
    Ok(Value::array_from_vec(ary))
}

///
/// #### Array#include?
///
/// - include?(val) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/include=3f.html]
#[monoruby_builtin]
fn include_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let rhs = lfp.arg(0);
    let mut i = 0;
    while i < self_val.as_array().len() {
        let lhs = self_val.as_array()[i];
        if vm.eq_values_bool(globals, lhs, rhs)? {
            return Ok(Value::bool(true));
        };
        i += 1;
    }
    Ok(Value::bool(false))
}

///
/// #### Array#reverse
///
/// - reverse -> Array
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/reverse.html]
#[monoruby_builtin]
fn reverse(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let ary = lfp.self_val().as_array();
    let iter = ary.iter().rev().cloned();
    Ok(Value::array_from_iter(iter))
}

///
/// #### Array#reverse!
///
/// - reverse! -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/reverse.html]
#[monoruby_builtin]
fn reverse_(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    lfp.self_val().ensure_not_frozen(&globals.store)?;
    let mut ary = lfp.self_val().as_array();
    ary.reverse();
    Ok(ary.into())
}

///
/// #### Array#reverse!
///
/// - reverse! -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/reverse.html]
#[monoruby_builtin]
fn transpose(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let ary = lfp.self_val().as_array();
    if ary.len() == 0 {
        return Ok(Value::array_empty());
    }
    let first = coerce_to_array_for_transpose(vm, globals, ary[0])?;
    let len = first.len();
    let mut trans = Array::new_empty();
    for i in 0..len {
        let mut temp = Array::new_empty();
        for v in ary.iter() {
            let a = coerce_to_array_for_transpose(vm, globals, *v)?;
            if a.len() != len {
                return Err(MonorubyErr::indexerr("Element size differs."));
            }
            temp.push(a[i]);
        }
        trans.push(temp.into());
    }
    Ok(trans.into())
}

fn coerce_to_array_for_transpose(
    vm: &mut Executor,
    globals: &mut Globals,
    val: Value,
) -> Result<Array> {
    if let Some(ary) = val.try_array_ty() {
        return Ok(ary);
    }
    // Try to_ary
    if let Some(fid) = globals.check_method(val, IdentId::TO_ARY) {
        let ret = vm.invoke_func_inner(globals, fid, val, &[], None, None)?;
        if let Some(ary) = ret.try_array_ty() {
            return Ok(ary);
        }
    }
    Err(MonorubyErr::typeerr(format!(
        "no implicit conversion of {} into Array",
        val.get_real_class_name(globals)
    )))
}

///
/// ### Array#rotate!
///
/// - rotate!(cnt = 1) -> Array
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/rotate=21.html]
#[monoruby_builtin]
fn rotate_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    lfp.self_val().ensure_not_frozen(&globals.store)?;
    let i = if let Some(arg0) = lfp.try_arg(0) {
        arg0.coerce_to_int_i64(vm, globals)?
    } else {
        1
    };
    let mut ary = lfp.self_val().as_array();
    let ary_len = ary.len() as i64;
    if ary_len == 0 {
    } else if i > 0 {
        let i = i % ary_len;
        ary.rotate_left(i as usize);
    } else {
        let i = (-i) % ary_len;
        ary.rotate_right(i as usize);
    };
    Ok(lfp.self_val())
}

///
/// ### Array#rotate
///
/// - rotate(cnt = 1) -> Array
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/rotate.html]
#[monoruby_builtin]
fn rotate(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let i = if let Some(arg0) = lfp.try_arg(0) {
        arg0.coerce_to_int_i64(vm, globals)?
    } else {
        1
    };
    let mut ary: Array = Array::new_from_vec(lfp.self_val().as_array().to_vec());
    let ary_len = ary.len() as i64;
    if ary_len == 0 {
    } else if i > 0 {
        let i = i % ary_len;
        ary.rotate_left(i as usize);
    } else {
        let i = (-i) % ary_len;
        ary.rotate_right(i as usize);
    };
    Ok(ary.into())
}

///
/// ### Array#product
///
/// - product(*lists) -> Array
/// - product(*lists) { |e| ... } -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/product.html]
#[monoruby_builtin]
fn product(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let lhs = lfp.self_val().as_array();
    let mut lists = vec![];
    for rhs in lfp.arg(0).as_array().into_iter() {
        lists.push(rhs.coerce_to_array(vm, globals)?);
    }
    // Check total product size to avoid memory exhaustion.
    let mut total: usize = lhs.len();
    for l in &lists {
        total = total
            .checked_mul(l.len())
            .ok_or_else(|| MonorubyErr::rangeerr("too big to product"))?;
        if total > 1_000_000 {
            return Err(MonorubyErr::rangeerr("too big to product"));
        }
    }
    let v: Vec<Value> = product_inner(lhs, lists)
        .into_iter()
        .map(|a| Value::array_from_iter(a.into_iter().cloned()))
        .collect();
    if let Some(bh) = lfp.block() {
        let ary = Array::new_from_vec(v);
        vm.temp_push(ary.into());
        vm.invoke_block_iter1(globals, bh, ary.into_iter().cloned())?;
        vm.temp_pop();
        Ok(lfp.self_val())
    } else {
        Ok(Value::array_from_vec(v))
    }
}

fn product_inner(lhs: Array, rhs: Vec<Array>) -> Vec<Array> {
    let mut res = vec![];
    for e1 in lhs.into_iter() {
        if rhs.is_empty() {
            res.push(Array::new1(*e1));
        } else {
            let mut rhs = rhs.clone();
            let l = rhs.remove(0);
            for e2 in product_inner(l, rhs).into_iter() {
                let mut v = vec![*e1];
                v.extend(e2.into_iter());
                res.push(Array::new_from_vec(v));
            }
        }
    }
    res
}

///
/// ### Array#union
///
/// - union(*other_arrays) -> Array
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/union.html]
#[monoruby_builtin]
fn union(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let mut ary = Array::new_from_vec(lfp.self_val().as_array().to_vec());
    for rhs in lfp.arg(0).as_array().into_iter() {
        let rhs = rhs.coerce_to_array(vm, globals)?;
        ary.extend(rhs.into_iter().cloned());
    }
    ary.uniq(vm, globals)?;
    Ok(ary.into())
}

///
/// ### Array#difference
///
/// - difference(*other_arrays) -> Array
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/difference.html]
#[monoruby_builtin]
fn difference(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let lhs_v = lfp.self_val();
    let others: Vec<_> = lfp
        .arg(0)
        .as_array()
        .iter()
        .map(|v| v.coerce_to_array(vm, globals))
        .collect::<Result<Vec<_>>>()?;
    let mut v = vec![];
    'outer: for lhs in lhs_v.as_array().iter() {
        for other in &others {
            for rhs in other.iter() {
                if lhs.eql(rhs, vm, globals)? {
                    continue 'outer;
                }
            }
        }
        v.push(*lhs);
    }
    Ok(Value::array_from_vec(v))
}

///
/// ### Array#intersection
///
/// - intersection(*other_arrays) -> Array
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/intersection.html]
#[monoruby_builtin]
fn intersection(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let src = lfp.self_val().as_array();
    // Build a unique copy as a plain Array
    let mut ary = Value::array_from_vec(src.to_vec()).as_array();
    ary.uniq(vm, globals)?;
    let others: Vec<_> = lfp
        .arg(0)
        .as_array()
        .iter()
        .map(|v| v.coerce_to_array(vm, globals))
        .collect::<Result<Vec<_>>>()?;
    ary.retain(|lhs| {
        for other in &others {
            let mut found = false;
            for rhs in other.iter() {
                if lhs.eql(rhs, vm, globals)? {
                    found = true;
                    break;
                }
            }
            if !found {
                return Ok(false);
            }
        }
        Ok(true)
    })?;
    Ok(ary.as_val())
}

///
/// ### Array#intersect?
///
/// - intersect?(other) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/intersect=3f.html]
#[monoruby_builtin]
fn intersect_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let lhs = lfp.self_val().as_array();
    for rhs in lfp.arg(0).coerce_to_array(vm, globals)?.iter().cloned() {
        for lhs in lhs.iter().cloned() {
            if lhs.eql(&rhs, vm, globals)? {
                return Ok(Value::bool(true));
            }
        }
    }
    Ok(Value::bool(false))
}

///
/// ### Array#uniq
///
/// - uniq -> Array
/// - uniq! -> self | nil
/// - uniq {|item| ... } -> Array
/// - uniq! {|item| ... } -> self | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/uniq.html]
#[monoruby_builtin]
fn uniq(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let mut ary = Array::new_from_vec(lfp.self_val().as_array().to_vec());
    match lfp.block() {
        None => ary.uniq(vm, globals)?,
        Some(bh) => uniq_block(vm, globals, ary, bh)?,
    };
    Ok(ary.into())
}

#[monoruby_builtin]
fn uniq_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    lfp.self_val().ensure_not_frozen(&globals.store)?;
    let mut ary = lfp.self_val().as_array();
    let deleted = match lfp.block() {
        None => ary.uniq(vm, globals)?,
        Some(bh) => uniq_block(vm, globals, ary, bh)?,
    };
    if deleted {
        Ok(lfp.self_val())
    } else {
        Ok(Value::nil())
    }
}

fn uniq_block(
    vm: &mut Executor,
    globals: &mut Globals,
    ary: Array,
    bh: BlockHandler,
) -> Result<bool> {
    vm.temp_push(ary.into());
    let res = uniq_inner(vm, globals, ary, bh);
    vm.temp_pop();
    res
}

fn uniq_inner(
    vm: &mut Executor,
    globals: &mut Globals,
    mut ary: Array,
    bh: BlockHandler,
) -> Result<bool> {
    let mut h = RubySet::default();
    let data = vm.get_block_data(globals, bh)?;
    vm.temp_array_new(Some(ary.len()));
    let res = ary.retain(|x| {
        let res = vm.invoke_block(globals, &data, &[*x])?;
        vm.temp_array_push(res);
        Ok(h.insert(res, vm, globals)?)
    });
    vm.temp_pop();
    res.map(|removed| removed.is_some())
}

///
/// ### Array#slice!
///
/// - slice!(nth) -> object | nil
/// - slice!(start, len) -> Array | nil
/// - [NOT SUPPORTED]slice!(range) -> Array | nil        
///
/// https://docs.ruby-lang.org/ja/latest/method/Array/i/slice=21.html
#[monoruby_builtin]
fn slice_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    lfp.self_val().ensure_not_frozen(&globals.store)?;
    let ary = lfp.self_val().as_array();
    if let Some(arg1) = lfp.try_arg(1) {
        let start = match ary.get_array_index(lfp.arg(0).coerce_to_int_i64(vm, globals)?) {
            Some(i) => i,
            None => return Ok(Value::nil()),
        };
        let len = arg1.coerce_to_int_i64(vm, globals)?;
        if len < 0 {
            return Ok(Value::nil());
        };
        let len = len as usize;
        Ok(slice_inner(ary, start, len))
    } else if let Some(range) = lfp.arg(0).is_range() {
        // nil begin means 0 (beginless range)
        let start = if range.start().is_nil() {
            0
        } else {
            match ary.get_array_index(range.start().coerce_to_int_i64(vm, globals)?) {
                Some(i) => i,
                None => return Ok(Value::nil()),
            }
        };
        // nil end means array length (endless range)
        let end = if range.end().is_nil() {
            ary.len().saturating_sub(1)
        } else {
            match ary.get_array_index(range.end().coerce_to_int_i64(vm, globals)?) {
                Some(i) => i,
                None => return Ok(Value::array_empty()),
            }
        };
        if end < start {
            return Ok(Value::array_empty());
        }
        let len = if range.end().is_nil() {
            ary.len() - start
        } else {
            end - start + if range.exclude_end() { 0 } else { 1 }
        };
        Ok(slice_inner(ary, start, len))
    } else {
        let index = lfp.arg(0).coerce_to_int_i64(vm, globals)?;
        let index = match ary.get_array_index(index) {
            Some(i) if i < ary.len() => i,
            _ => return Ok(Value::nil()),
        };
        let mut ary = ary;
        let val = ary.remove(index);
        Ok(val)
    }
}

fn slice_inner(mut aref: Array, start: usize, len: usize) -> Value {
    let ary_len = aref.len();
    if ary_len < start {
        return Value::nil();
    }
    if ary_len <= start || len == 0 {
        return Value::array_empty();
    }
    let end = if ary_len < start + len {
        ary_len
    } else {
        start + len
    };
    let iter = aref.drain(start..end);
    Value::array_from_iter(iter)
}

///
/// ### Array#pack
///
/// - pack(template) -> String
/// - [NOT SUPPORTED] pack(template, buffer: String.new) -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/pack.html]
#[monoruby_builtin]
fn pack(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let template = lfp.arg(0).coerce_to_string(vm, globals)?;
    let buffer = lfp.try_arg(1);
    let ary = lfp.self_val().as_array();
    rvalue::pack(vm, globals, &ary, &template, buffer)
}

fn try_convert_to_array(
    v: &Value,
    vm: &mut Executor,
    globals: &mut Globals,
) -> Result<Option<Array>> {
    if let Some(ary) = v.try_array_ty() {
        return Ok(Some(ary));
    }
    // Check respond_to?(:to_ary, true) to respect respond_to_missing? protocol.
    let respond_to = IdentId::get_id("respond_to?");
    let responds = match vm.invoke_method_inner(
        globals,
        respond_to,
        *v,
        &[Value::symbol(IdentId::TO_ARY), Value::bool(true)],
        None,
        None,
    ) {
        Ok(val) => val.as_bool(),
        // BasicObject without respond_to? and without method_missing
        Err(_) => return Ok(None),
    };
    if !responds {
        return Ok(None);
    }
    // Call to_ary via invoke_method_inner to support method_missing.
    let result = vm.invoke_method_inner(globals, IdentId::TO_ARY, *v, &[], None, None)?;
    if let Some(ary) = result.try_array_ty() {
        return Ok(Some(ary));
    }
    if result.is_nil() {
        return Ok(None);
    }
    Err(MonorubyErr::typeerr(format!(
        "can't convert {} into Array ({}#to_ary gives {})",
        v.get_real_class_name(&globals.store),
        v.get_real_class_name(&globals.store),
        result.get_real_class_name(&globals.store),
    )))
}

fn flatten_inner(
    vm: &mut Executor,
    globals: &mut Globals,
    ary: &Array,
    res: &mut Vec<Value>,
    lv: Option<usize>,
    changed: &mut bool,
    seen: &mut Vec<u64>,
) -> Result<()> {
    let id = ary.id();
    if seen.contains(&id) {
        return Err(MonorubyErr::argumenterr("tried to flatten recursive array"));
    }
    seen.push(id);
    for v in ary.iter() {
        // At level 0, no flattening occurs — do not call to_ary.
        if let Some(0) = lv {
            res.push(*v);
            continue;
        }
        if let Some(inner) = try_convert_to_array(v, vm, globals)? {
            *changed = true;
            if let Some(lv) = lv {
                flatten_inner(vm, globals, &inner, res, Some(lv - 1), changed, seen)?;
            } else {
                flatten_inner(vm, globals, &inner, res, None, changed, seen)?;
            }
        } else {
            res.push(*v);
        }
    }
    seen.pop();
    Ok(())
}

///
/// ### Array#flatten
///
/// - flatten(lv = nil) -> Array
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/flatten.html]
#[monoruby_builtin]
fn flatten(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let ary = lfp.self_val().as_array();
    let lv = if let Some(arg0) = lfp.try_arg(0) {
        if arg0.is_nil() {
            None
        } else {
            match arg0.coerce_to_int_i64(vm, globals)? {
                i if i >= 0 => Some(i as usize),
                _ => None,
            }
        }
    } else {
        None
    };
    let mut res = vec![];
    let mut changed = false;
    let mut seen = vec![];
    flatten_inner(vm, globals, &ary, &mut res, lv, &mut changed, &mut seen)?;
    Ok(Value::array_from_vec(res))
}

///
/// ### Array#flatten!
///
/// - flatten!(lv = nil) -> self | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/flatten.html]
#[monoruby_builtin]
fn flatten_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    lfp.self_val().ensure_not_frozen(&globals.store)?;
    let mut ary = lfp.self_val().as_array();
    let lv = if let Some(arg0) = lfp.try_arg(0) {
        if arg0.is_nil() {
            None
        } else {
            match arg0.coerce_to_int_i64(vm, globals)? {
                i if i >= 0 => Some(i as usize),
                _ => None,
            }
        }
    } else {
        None
    };
    let mut res = vec![];
    let mut changed = false;
    let mut seen = vec![];
    flatten_inner(vm, globals, &ary, &mut res, lv, &mut changed, &mut seen)?;
    ary.replace(res);
    Ok(if changed { ary.into() } else { Value::nil() })
}

///
/// ### Array#compact
///
/// - compact -> Array
/// - compact! -> self | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/compact.html]
#[monoruby_builtin]
fn compact(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let mut ary = Array::new_from_vec(lfp.self_val().as_array().to_vec());
    ary.retain(|v| Ok(!v.is_nil()))?;
    Ok(ary.into())
}

#[monoruby_builtin]
fn compact_(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    lfp.self_val().ensure_not_frozen(&globals.store)?;
    let mut ary = lfp.self_val().as_array();
    Ok(if ary.retain(|v| Ok(!v.is_nil()))?.is_some() {
        lfp.self_val()
    } else {
        Value::nil()
    })
}

///
/// ### Array#shuffle!
///
/// - shuffle! -> self
/// - shuffle!(random: Random) -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/shuffle=21.html]
#[monoruby_builtin]
fn shuffle_(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    lfp.self_val().ensure_not_frozen(&globals.store)?;
    let mut ary = lfp.self_val().as_array();
    ary.shuffle(&mut rand::rng());
    Ok(lfp.self_val())
}

///
/// ### Array#delete
///
/// - delete(val) -> object | nil
/// - delete(val) {...} -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/delete.html]
#[monoruby_builtin]
fn delete(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    lfp.self_val().ensure_not_frozen(&globals.store)?;
    let mut ary = lfp.self_val().as_array();
    let arg0 = lfp.arg(0);
    let f = |v: &Value| Ok(!vm.eq_values_bool(globals, *v, arg0)?);
    if let Some(last) = ary.retain(f)? {
        Ok(last)
    } else if let Some(bh) = lfp.block() {
        vm.invoke_block_once(globals, bh, &[])
    } else {
        Ok(Value::nil())
    }
}

///
/// ### Array#delete_at
///
/// - delete_at(pos) -> object | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/delete_at.html]
#[monoruby_builtin]
fn delete_at(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    lfp.self_val().ensure_not_frozen(&globals.store)?;
    let mut ary = lfp.self_val().as_array();
    let pos = lfp.arg(0).coerce_to_int_i64(vm, globals)?;
    let pos = if pos < 0 {
        let pos = pos + ary.len() as i64;
        if pos < 0 {
            return Ok(Value::nil());
        }
        pos as usize
    } else if pos >= ary.len() as i64 {
        return Ok(Value::nil());
    } else {
        pos as usize
    };
    Ok(ary.remove(pos))
}

///
/// ### Array#find_index
///
/// - find_index(val) -> Integer | nil
/// - index(val) -> Integer | nil
/// - find_index {|item| ...} -> Integer | nil
/// - index {|item| ...} -> Integer | nil
/// - find_index -> Enumerator
/// - index -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/find_index.html]
#[monoruby_builtin]
fn find_index(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    pc: BytecodePtr,
) -> Result<Value> {
    let self_val = lfp.self_val();
    if let Some(arg0) = lfp.try_arg(0) {
        if lfp.block().is_some() {
            vm.ruby_warn(globals, "warning: given block not used")?;
        }
        let func_id = vm.find_method(globals, arg0, IdentId::_EQ, false)?;
        let mut i = 0;
        while i < self_val.as_array().len() {
            let v = self_val.as_array()[i];
            if vm
                .invoke_func_inner(globals, func_id, arg0, &[v], None, None)?
                .as_bool()
            {
                return Ok(Value::integer(i as i64));
            }
            i += 1;
        }
        Ok(Value::nil())
    } else if let Some(bh) = lfp.block() {
        let data = vm.get_block_data(globals, bh)?;
        let mut i = 0;
        while i < self_val.as_array().len() {
            let v = self_val.as_array()[i];
            if vm.invoke_block(globals, &data, &[v])?.as_bool() {
                return Ok(Value::integer(i as i64));
            }
            i += 1;
        }
        Ok(Value::nil())
    } else {
        let id = IdentId::get_id("find_index");
        vm.generate_enumerator(id, lfp.self_val(), vec![], pc)
    }
}

///
/// ### Array#insert
///
/// - insert(nth, *val) -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/insert.html]
#[monoruby_builtin]
fn insert(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    lfp.self_val().ensure_not_frozen(&globals.store)?;
    let val = lfp.arg(1).as_array();
    if val.len() == 0 {
        return Ok(lfp.self_val());
    }
    let mut ary = lfp.self_val().as_array();
    let nth = lfp.arg(0).coerce_to_int_i64(vm, globals)?;
    let nth = if nth < 0 {
        let nth = nth + ary.len() as i64 + 1;
        if nth < 0 {
            return Err(MonorubyErr::index_too_small(nth, -(ary.len() as i64) - 1));
        }
        nth as usize
    } else {
        nth as usize
    };
    if nth >= ary.len() {
        ary.resize(nth, Value::nil());
    }
    ary.insert_many(nth, val.iter().cloned());
    Ok(lfp.self_val())
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn array_new() {
        run_test_with_prelude(
            r##"
        a = A.new(4, 42)
        a << 4
        a[2] = 5
        [a, a.class]
        "##,
            r##"
        class A < Array
        end
        "##,
        );
        run_test_with_prelude(
            r##"
        a = A.new(10)
        a << 4
        a[2] = 5
        [a, a.class]
        "##,
            r##"
        class A < Array
        end
        "##,
        );
        run_test_with_prelude(
            r##"
        a = A.new([1,2,3])
        a << 4
        a[2] = 5
        [a, a.class]
        "##,
            r##"
        class A < Array
        end
        "##,
        );
        run_test_with_prelude(
            r##"
        mul = 3
        a = A.new(5) {|i| i * mul }
        a << 4
        a[2] = 5
        [a, a.class]
        "##,
            r##"
        class A < Array
        end
        "##,
        );
        run_test_no_result_check("[].hash");
        run_test_error("Array.new(-5)");
        run_test_error("Array.new(:r, 42)");
    }

    #[test]
    fn array_new_to_int_coercion() {
        // Array.new should call to_int on size argument
        run_test_with_prelude(
            r#"Array.new(o)"#,
            r#"class C; def to_int; 3; end; end; o = C.new"#,
        );
        run_test_with_prelude(
            r#"Array.new(o, "x")"#,
            r#"class C; def to_int; 2; end; end; o = C.new"#,
        );
    }

    #[test]
    fn to_a() {
        run_test(r##"[].to_a"##);
        run_test(r##"[1,2,3].to_a"##);
    }

    #[test]
    fn to_h() {
        run_test(r##"[[:foo, :bar], [1, 2]].to_h"##);
        run_test(r##"["foo", "bar"].to_h {|s| [s.ord, s]}"##);
        run_test_error(r##"[[:foo, :bar], 3].to_h"##);
        run_test_error(r##"[[:foo, :bar], [1,2,3]].to_h"##);
    }

    #[test]
    fn size() {
        run_test(r##"[].size"##);
        run_test(r##"[].length"##);
        run_test(r##"[1,2,3].size"##);
        run_test_with_prelude(
            r##"
        a = A.new
        a << 100
        a << 42
        a << 2
        a.size
        "##,
            r##"
        class A < Array
        end
        "##,
        );
    }

    #[test]
    fn count() {
        run_test(r##"ary = [1, 2, 4, 2.0]; ary.count"##);
        run_test(r##"ary = [1, 2, 4, 2.0]; ary.count(2)"##);
        run_test(r##"ary = [1, 2, 4, 2.0]; ary.count{|x| x % 2 == 0 }"##);
        run_test(r##"ary = [1, 2, 4, 5]; ary.count(&:even?)"##);
    }

    #[test]
    fn empty() {
        run_test(r##"[].empty?"##);
        run_test(r##"[1,2,3].empty?"##);
    }

    #[test]
    fn shift() {
        run_test(
            r##"
            a = [1,2,3,4]
            [a.shift, a]
        "##,
        );
        run_test(
            r##"
            a = [1,2,3,4]
            [a.shift(3), a]
        "##,
        );
        run_test(
            r##"
            a = [1,2,3,4]
            [a.shift(10), a]
        "##,
        );
        run_test(r##"[].shift"##);
        run_test(r##"[].shift(2)"##);
        run_test_error(r##"[1,2,3].shift(:e)"##);
        run_test_error(r##"[1,2,3].shift(-2)"##);
    }

    #[test]
    fn unshift() {
        run_test(
            r##"
            res = []
            arr = [1,2,3]
            arr.unshift 0
            res << arr
            arr.unshift [0]
            res << arr
            arr.unshift 1,2
            res << arr
            res
        "##,
        );
    }

    #[test]
    fn concat() {
        run_test(r##"["a", "b"].concat ["c", "d"]"##);
        run_test(r##"["a"].concat(["b"], ["c", "d"])"##);
        run_test(r##"a = [1,2]; a.concat(a, a)"##);
    }

    #[test]
    fn add() {
        run_test(r##"[1,2,3] + [4]"##);
        run_test(
            r##"
        a = [1,2,3]
        res = a + [4,5,6,7,8]
        [a, res]
        "##,
        );
    }

    #[test]
    fn sub() {
        run_test(r##"[1,2,3] - [2,5]"##);
        run_test(
            r##"
        a = [:a,:b,:c]
        res = a - [:b,:d]
        [a, res]
        "##,
        );
    }

    #[test]
    fn mul() {
        run_test(r##"[1,2,3] * 4"##);
        run_test(
            r##"
        a = [1,2,3]
        res = a * 4
        [a, res]
        "##,
        );
        run_test(r##"[] * "|""##);
        run_test(r##"[1] * "|""##);
        run_test(r##"[1,2,3,4] * "|""##);
    }

    #[test]
    fn and() {
        run_test(r##"[] & []"##);
        run_test_error(r##"[] & 100"##);
        run_test(r##"[1,1,2,3,5,7,2,1,7] & []"##);
        run_test(r##"[1,1,2,3,5,7,2,1,7] & [1]"##);
        run_test(r##"[1,1,2,3,5,7,2,1,7] & [1,2]"##);
        run_test(r##"[1,1,2,3,5,7,2,1,7] & [1,3,7]"##);
        run_test(r##"[1,1,2,3,5,7,2,1,7] & [1,3,7,2]"##);
    }

    #[test]
    fn or() {
        run_test(r##"[] | []"##);
        run_test_error(r##"[] | 100"##);
        run_test(r##"[1,1,2,3,5,7,2,1,7] | []"##);
        run_test(r##"[1,1,2,3,5,7,2,1,7] | [1]"##);
        run_test(r##"[1,1,2,3,5,7,2,1,7] | [1,2]"##);
        run_test(r##"[1,1,2,3,5,7,2,1,7] | [1,3,7]"##);
        run_test(r##"[1,1,2,3,5,7,2,1,7] | [1,3,7,2]"##);
    }

    #[test]
    fn shl() {
        run_test(r##"a = [1,2,3]; a << 10; a"##);
        run_test(r##"a = [1,2,3]; a.<<(10); a"##);
    }

    #[test]
    fn push_pop() {
        run_test(
            r##"
        res = []
        array = [1, [2, 3], 4]
        res << array.pop      # => 4
        res << array.pop      # => [2, 3]
        res << array          # => [1]
        res << array.pop      # => 1
        res << array.pop      # => nil
        res << array          # => []
        res
        "##,
        );
        run_test(
            r##"
        array = [1, 2, 3]
        array.push 4
        array.push [5, 6]
        array.push 7, 8
        array 
        "##,
        );
    }

    #[test]
    fn eq() {
        run_test(r##"["a","c"] == ["a","c",7]"##);
        run_test(r##"["a","c"] === ["a","c",7]"##);
        run_test(r##"["a","c",7] == ["a","c",7]"##);
        run_test(r##"["a","c",7] === ["a","c",7]"##);
        run_test(r##"["a","c",7] == ["a","c","7"]"##);
        run_test(r##"["a","c",7] === ["a","c","7"]"##);
        run_test(r##"["a","c",7] == "a""##);
        run_test(r##"["a","c",7] === "a""##);
        run_test(r##"[ 1, 2, 3 ] <=> [ 1, 3, 2 ] "##);
        run_test(r##"[ 1, 2, 3 ] <=> [ 1, 2, 3 ] "##);
        run_test(r##"[ 1, 2, 3 ] <=> [ 1, 2 ] "##);
        run_test(r##"[ 1, 2 ] <=> [ 1, 2, 3 ] "##);
    }

    #[test]
    fn eq_recursive() {
        // Self-referencing array: a == a should return true, not stack overflow
        run_test("a = []; a << a; a == a");
        run_test("a = [1]; a << a; a == a");
        // Two distinct recursive arrays with same structure
        run_test("a = [1]; a << a; b = [1]; b << b; a == b");
        // Cross-recursive: a contains b, b contains a
        run_test("a = [1]; b = [1]; a << b; b << a; a == b");
        // Different lengths: recursive but not equal
        run_test("a = [1]; a << a; b = [1, 2]; b << b; a == b");
        // Non-recursive element differs
        run_test("a = [1]; a << a; b = [2]; b << b; a == b");
        // Empty self-referencing
        run_test("a = []; a << a; b = []; b << b; a == b");
    }

    #[test]
    fn cmp_recursive() {
        // Self-referencing array: a <=> a should return 0, not stack overflow
        run_test("a = []; a << a; (a <=> a)");
        run_test("a = [1]; a << a; (a <=> a)");
        // Cross-recursive same structure
        run_test("a = [1]; b = [1]; a << b; b << a; (a <=> b)");
        // Different lengths with recursive element — returns nil, not ArgumentError
        run_test("a = [1]; a << a; b = [1, 2]; b << b; (a <=> b)");
        run_test("a = [1, 2]; a << a; b = [1]; b << b; (b <=> a)");
    }

    #[test]
    fn cmp_incomparable_elements() {
        // Incomparable elements return nil instead of ArgumentError
        run_test("[1, :a] <=> [1, 2]");
        run_test("[1, 2] <=> [1, :a]");
    }

    #[test]
    fn recursive_guard_cleanup() {
        // Guard must be cleaned up after use: non-recursive comparison after recursive
        run_test(
            r#"
            a = [1]; a << a
            r1 = a == a
            r2 = [1, 2] == [1, 2]
            [r1, r2]
            "#,
        );
        // Multiple recursive operations in sequence
        run_test(
            r#"
            a = []; a << a
            r1 = a == a
            r2 = (a <=> a)
            r3 = a == a
            [r1, r2, r3]
            "#,
        );
    }

    #[test]
    fn eql() {
        run_test(r##"["a", "b", "c"].eql? ["a", "b", "c"]"##);
        run_test(r##"["a", "b", "c"].eql? ["a", "c", "b"]"##);
        run_test(r##"["a", "b", 1].eql? ["a", "b", 1.0]"##);
    }

    #[test]
    fn eql_recursive() {
        // Self-referencing array eql?
        run_test("a = []; a << a; a.eql?(a)");
        run_test("a = [1]; a << a; b = [1]; b << b; a.eql?(b)");
        // Cross-recursive via hash
        run_test(
            r#"
            x, y, z = [], [], []
            a = {foo: x, bar: 42}; b = {foo: y, bar: 42}; c = {foo: z, bar: 42}
            x << a; y << c; z << b
            y.eql?(z)
            "#,
        );
    }

    #[test]
    fn index() {
        run_test(
            r##"
        a = [1,2,3];
        a[2] = 42;
        a[4] = 99;
        a[-1] = 14;
        a
        "##,
        );
        run_test(
            r##"
        ary = [0, 1, 2, 3]
        ary[1, 2] = ["a", "b", "c", "d"]
        ary
        "##,
        );
        run_test(
            r##"
        ary = [0, 1, 2]
        ary[5, 1] = "Z"
        ary
        "##,
        );
        run_test(
            r##"
        ary = [0, 1, 2, 3]
        ary[0, 10] = ["a"]
        ary
        "##,
        );
        run_test(
            r##"
        ary = [0, 1, 2, 3]
        ary[-1, 10] = ["a"]
        ary
        "##,
        );
        run_test_error(
            r##"
        ary = [0, 1, 2, 3]
        ary[0, -1] = ["a"]
        ary
        "##,
        );
        run_test(
            r##"
        a = [1,2,3];
        a.[]=(2, 42);
        a.[]=(4,99);
        a.[]=(-2, 14);
        a
        "##,
        );
        run_test(
            r##"
        a = ["a","b","c","d","e"];
        [a[0..1], a[0...1], a[0..-1], a[-2..-1], a[-2..4], a[0..10], a[10..11], a[2..1], a[-1..-2], a[5..10]]
        "##,
        );
        run_test(
            r##"
        a = [ "a", "b", "c", "d", "e" ];
        [a.[](0), a.[](1), a.[](-1), a.[](-2), a.[](10)]
        "##,
        );
        run_test(
            r##"
        a = [ "a", "b", "c", "d", "e" ];
        [a.[](0..1), a.[](0...1), a.[](0..-1), a.[](-2..-1), a.[](-2..4), a.[](0..10), a.[](10..11), a.[](2..1), a.[](-1..-2), a.[](5..10)]
        "##,
        );
        run_test(
            r##"
        a = [ "a", "b", "c", "d", "e" ];
        [a[0, 1], a[-1, 1], a[0, 10], a[0, 0], a[0, -1], a[10, 1], a[5], a[5, 1], a[5..10]]
        "##,
        );
        run_test(
            r##"
        a = [*(0..10)];
        x = (a[5..8] = [6,7,8,9,10,11])
        y = (a[2...-2] = 6)
        [a, x, y]
        "##,
        );
        run_test(
            r##"
        a = [*(0..10)];
        x = (a[8..6] = 7)
        [a, x]
        "##,
        );
        run_test(
            r##"
        a = [*(0..10)];
        x = (a[8..-6] = [:a,:b,:c])
        [a, x]
        "##,
        );
    }

    #[test]
    fn index_nil_range() {
        // Array#[] with beginless range (nil..n)
        run_test(r##"[1,2,3,4,5][nil..2]"##);
        run_test(r##"[1,2,3,4,5][nil...2]"##);
        // Array#[] with endless range (n..nil)
        run_test(r##"[1,2,3,4,5][2..nil]"##);
        run_test(r##"[1,2,3,4,5][2...nil]"##);
        // Array#[] with nil..nil
        run_test(r##"[1,2,3,4,5][nil..nil]"##);
        // Array#[] with beginless/endless using .. syntax
        run_test(r##"[1,2,3,4,5][..2]"##);
        run_test(r##"[1,2,3,4,5][...2]"##);
        run_test(r##"[1,2,3,4,5][2..]"##);
        // Array#[]= with beginless range
        run_test(
            r##"
        a = [1,2,3,4,5]
        a[nil..2] = [10,20]
        a
        "##,
        );
        // Array#[]= with endless range
        run_test(
            r##"
        a = [1,2,3,4,5]
        a[2..nil] = [10,20]
        a
        "##,
        );
        // Array#[]= with nil..nil
        run_test(
            r##"
        a = [1,2,3,4,5]
        a[nil..nil] = [10,20]
        a
        "##,
        );
        // values_at with beginless/endless ranges
        run_test(r##"[1,2,3,4,5].values_at(..2)"##);
        run_test(r##"[1,2,3,4,5].values_at(2..)"##);
        run_test(r##"[1,2,3,4,5].values_at(nil..nil)"##);
    }

    #[test]
    fn index_negative_count_nil() {
        // Negative index beyond array size with count returns nil
        run_test("[1, 2, 3][-10, 2]");
        // Negative index within range works normally
        run_test("[1, 2, 3][-2, 2]");
        // Positive index beyond array size with count returns nil
        run_test("[1, 2, 3][4, 2]");
        // At end returns empty
        run_test("[1, 2, 3][3, 2]");
    }

    #[test]
    fn fill() {
        run_test(
            r##"
            a = [2, 3, 4, 5]
            a.fill(100)
            a"##,
        );
        run_test(
            r##"
            a = [1, 2, 3, 4]
            a.fill("a", 0..3)
            a"##,
        );
        run_test(
            r##"
            a = [1, 2, 3, 4]
            a.fill("x", 1, 2)
            a"##,
        );
        run_test(
            r##"
            a = [1, 2, 3, 4]
            a.fill { |i| i * 10 }
            a"##,
        );
        run_test(
            r##"
            a = [1, 2, 3, 4]
            a.fill(-2) { |i| i * 10 }
            a"##,
        );
        // fill with no arguments fills with nil
        run_test_error(
            r#"
            a = [1, 2, 3]
            a.fill
            a
            "#,
        );
        // fill with range (non-block form)
        run_test(
            r##"
            a = [1, 2, 3, 4, 5]
            a.fill("z", 1..3)
            a"##,
        );
        // fill with block and range
        run_test(
            r##"
            a = [1, 2, 3, 4]
            a.fill(0..2) { |i| i * 100 }
            a"##,
        );
        // fill extending the array
        run_test(
            r##"
            a = [1, 2]
            a.fill("x", 0, 5)
            a"##,
        );
        // fill with negative start
        run_test(
            r##"
            a = [1, 2, 3, 4]
            a.fill("y", -3, 2)
            a"##,
        );
        // fill with beginless range
        run_test(
            r##"
            a = [1, 2, 3, 4, 5]
            a.fill("x", ..2)
            a"##,
        );
        // fill with endless range
        run_test(
            r##"
            a = [1, 2, 3, 4, 5]
            a.fill("x", 2..)
            a"##,
        );
        // fill with nil..nil range
        run_test(
            r##"
            a = [1, 2, 3, 4, 5]
            a.fill("x", nil..nil)
            a"##,
        );
        // fill with block and beginless range
        run_test(
            r##"
            a = [1, 2, 3, 4, 5]
            a.fill(..2) { |i| i * 10 }
            a"##,
        );
        // fill(val, start, nil) means fill to end
        run_test(
            r##"
            a = [0, 1, 2, 3, 4]
            a.fill(:a, 1, nil)
            a"##,
        );
        // fill with block(start, nil) means fill to end
        run_test(
            r##"
            a = [0, 1, 2, 3, 4]
            a.fill(1, nil) { |i| i * 10 }
            a"##,
        );
    }

    #[test]
    fn drop() {
        run_test(
            r##"
            a = [2, 3, 4, 5]
            [a.drop(2), a.drop(100), a]
            a"##,
        );
    }

    #[test]
    fn zip() {
        run_test(r##"[1,2,3].zip([4,5,6], [7,8,9])"##);
        run_test(r##"[1,2].zip([:a,:b,:c], [:A,:B,:C,:D])"##);
        run_test(r##"[1,2,3,4,5].zip([:a,:b,:c], [:A,:B,:C,:D])"##);
        run_test(
            r##"
            a = []
            [1,2,3].zip([4,5,6], [7,8,9]) { |ary| a << ary }
            a
        "##,
        );
        // zip with enumerator / non-array enumerable (uses #each fallback)
        run_test("[1, 2, 3].zip((4..6))");
        // zip with infinite enumerator (must not hang)
        run_test("[1, 2].zip(10.upto(Float::INFINITY))");
        // zip with shorter #each-based enumerable fills nil
        run_test(r##"
            o = Object.new
            def o.each; yield 10; end
            [1, 2].zip(o)
        "##);
        // zip raises TypeError for non-enumerable
        run_test_error(r##"[1, 2].zip(42)"##);
    }

    #[test]
    fn clear() {
        run_test(
            r##"
            a = [2, 3, 4, 5]
            a.clear
            a"##,
        );
    }

    #[test]
    fn inject() {
        run_test(r##"[2, 3, 4, 5].inject(0) {|result, item| result + item }"##);
        run_test(r##"[2, 3, 4, 5].inject {|result, item| result + item }"##);
        run_test(r##"[2, 3, 4, 5].inject(5) {|result, item| result + item**2 }"##);
        run_test(r##"[1, 2, 3, 4, 5].inject(:+)"##);
        run_test(r##"[1, 2, 3, 4, 5].inject(10, :+)"##);
    }

    #[test]
    fn join() {
        run_test(r##"[2, 3, 4, 5].join"##);
        run_test(r##"[2, 3, 4, 5].join("-")"##);
    }

    #[test]
    fn first() {
        run_test(r##"[[0,1,2,3].first, [].first]"##);
        run_test(
            r##"
        a = [0,1,2]
        [a.first(0), a.first(1), a.first(2), a.first(3), a.first(4)]
        "##,
        );
        run_test(r##"[[0,1,2,3].last, [].last]"##);
        run_test(
            r##"
        a = [0,1,2]
        [a.last(0), a.last(1), a.last(2), a.last(3), a.last(4)]
        "##,
        );
    }

    #[test]
    fn fetch() {
        run_test(r##"[0,1,2,3].fetch(1)"##);
        run_test(r##"[0,1,2,3].fetch(-1)"##);
        run_test(r##"[0,1,2,3].fetch(-4)"##);
        run_test(r##"[0,1,2,3].fetch(4, 999)"##);
        run_test(r##"[0,1,2,3].fetch(-5, 999)"##);
        run_test(r##"[0,1,2,3].fetch(4) { |i| i * 10 }"##);
        run_test_error(r##"[0,1,2,3].fetch(4)"##);
        run_test_error(r##"[0,1,2,3].fetch(-5)"##);
    }

    #[test]
    fn take() {
        run_test_error(r##"[0,1,2,3].take(-5)"##);
        run_test_error(r##"[0,1,2,3].take(:a)"##);
        run_test(r##"[0,1,2,3,4,5].take(3)"##);
        run_test(r##"[0,1,2,3,4,5].take(100)"##);
        run_test(r##"[0,1,2,3,4,5].take(0)"##);
        run_test(
            r##"
            a = [0,1,2,3,4,5];
            [a.take(3) << 5, a]
            "##,
        );
    }

    #[test]
    fn sum() {
        run_test(
            r##"[[].sum, [].sum(0.0), [1, 2, 3].sum, [3, 5.5].sum, [2.5, 3.0].sum(0.0) {|e| e * e }, ["a", "b", "c"].sum("")]"##,
        );
        run_test_error("[Object.new].sum");
    }

    #[test]
    fn min() {
        run_test(
            r##"
            [[].min, [1,2,-42].min, [-42.4242, 100, 9999999999999999999999999999999].min]
            "##,
        );
        run_test_error("[1,:hh].min");
        run_test_error("[Float::NAN, Float::NAN].min");
    }

    #[test]
    fn max() {
        run_test(
            r##"
            [[].max, [1,2,-42].max, [-42.4242, 100, 9999999999999999999999999999999].max]
            "##,
        );
        run_test_error("[1,:hh].max");
        run_test_error("[Float::NAN, Float::NAN].max");
    }

    #[test]
    fn partition() {
        run_test(r##"[10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0].partition {|i| i % 3 == 0 }"##);
    }

    #[test]
    fn sort() {
        run_test(
            r##"
            res = []
            a = []
            a.sort!
            res << a
            a = [1,2,-42]
            a.sort!
            res << a
            a = [999999999999999999999999999999, -42.4242, 100, 100.001, -555, 0.0, 100, 76543, 100.0, -12, 25.24, 777777777, 0.0, 29.2, -1, 13, 5790, -17.58, -852, 0.1, 6.66, -100, -7890123]
            a.sort!
            res << a
            res
            "##,
        );
        run_test(
            r##"
            a = [999999999999999999999999999999, -42.4242, 100, 100.001, -555, 0.0, 100, 76543, 100.0]
            res = a.sort
            [res, a]
            "##,
        );
        run_test_error("[1,:hh].sort!");
        run_test_error("[1,:hh].sort");
        run_test_error("[Float::NAN, Float::NAN].sort!");
        run_test_error("[Float::NAN, Float::NAN].sort");
        run_test(
            r#"
        fruits = %w{apple pear fig}
        fruits.sort_by! { |word| word.length }
        fruits
        "#,
        );
        run_test(
            r#"
        fruits = %w{apple pear fig}
        new_fruits = fruits.sort_by { |word| word.length }
        [fruits, new_fruits]
        "#,
        );
        run_test(
            r##"
        ary2 = ["9", "7", "10", "11", "8"]
        [ary2.sort{|a, b| a.to_i <=> b.to_i }, ary2]
        "##,
        );
        run_test(
            r##"
        ary2 = ["9", "7", "10", "11", "8"]
        [ary2.sort, ary2]
        "##,
        );
        run_test(
            r##"
        ary2 = ["9", "7", "10", "11", "8"]
        ary2.sort!{|a, b| a.to_i <=> b.to_i }
        ary2
        "##,
        );
        run_test(
            r##"
        ary2 = ["9", "7", "10", "11", "8"]
        ary2.sort!
        ary2
        "##,
        );
        // Enumerable#sort (delegates to to_a.sort)
        run_test("[1,2,3].permutation(2).to_a.sort");
        run_test("[1,2].repeated_combination(2).to_a.sort");
        run_test("[1,2].repeated_permutation(2).to_a.sort");
        run_test("[3,1,2].each.sort { |a,b| b <=> a }");
    }

    #[test]
    fn group_by() {
        run_test(
            r##"
            [*(1..6)].group_by {|i| i%3}
        "##,
        );
    }

    #[test]
    fn each() {
        run_test(
            r##"
        x = 100
        [2, 3, 4, 5].each do |y|
          x += y
        end
        x
        "##,
        );
        run_test(
            r##"
        x = []
        [2, 3, 4, 5].reverse_each do |y|
          x << y
        end
        x
        "##,
        );
    }

    #[test]
    fn each_with_index() {
        run_test(
            r##"
        x = 100
        [2, 3, 4, 5].each_with_index do |item, index|
          x += item * 7 + index
        end
        x
        "##,
        );
    }

    #[test]
    fn select() {
        run_test(r##"[1,2,3,4,5].select { |num| num.even? }"##);
        run_test(r##"[1,2,3,4,5].select(&:even?)"##);
        run_test(r##"[1,2,3,4,5].reject { |num| num.even? }"##);
        run_test(r##"[1,2,3,4,5].reject { true }"##);
        run_test(r##"[1,2,3,4,5].reject(&:even?)"##);
        run_test(r##"a=[1,2,3,4,5]; a.reject! { |num| num.even? }; a"##);
        run_test(r##"a=[1,2,3,4,5]; a.reject! { true }; a"##);
        run_test(r##"a=[1,2,3,4,5]; a.reject! { false }; a"##);
        run_test(r##"a=[1,2,3,4,5]; a.reject!(&:even?); a"##);
        run_test(r##"a=[1,2,3,4,5]; a.delete_if { |num| num.even? }; a"##);
        run_test(r##"a=[1,2,3,4,5]; a.delete_if { true }; a"##);
        run_test(r##"a=[1,2,3,4,5]; a.delete_if { false }; a"##);
        run_test(r##"a=[1,2,3,4,5]; a.delete_if(&:even?); a"##);
        run_test(
            r##"
        a = %w{ a b c d e f }
        b = a.select! {|v| v =~ /[a-z]/ }   # => nil
        [a, b] # => [["a", "b", "c", "d", "e", "f"], nil]
        "##,
        );
        run_test(
            r##"
        a = %w{ a b c d e f }
        b = a.select! {|v| v =~ /[c-e]/ }
        [a, b]
        "##,
        );
    }

    #[test]
    fn map() {
        run_test(
            r##"
        x = 10
        a = [2, 3, 4, 5, 6, 7, 8]
        res = a.map do |y|
          x + y
        end
        [res, a]
        "##,
        );
        run_test(
            r##"
        x = 10
        a = [2, 3, 4, 5, 6, 7, 8]
        res = a.map! do |y|
          x + y
        end
        [res, a]
        "##,
        );
    }

    #[test]
    fn all_any() {
        run_test(r#"[5,  6, 7].all? {|v| v > 0 }"#);
        run_test(r#"[5, -1, 7].all? {|v| v > 0 }"#);
        run_test(r#"[5, -1, 7].all?"#);
        run_test(r#"[5, nil, 7].all?"#);
        run_test(r#"[5, -1, false].all?"#);
        run_test(r#"[nil, false].all?"#);
        run_test(r#"[1, 2, 3].all?(Integer)"#);
        run_test(r#"[1, "a", 3].all?(Integer)"#);
        run_test(r#"[].all?(Integer)"#);

        run_test(r#"[5,  6, 7].any? {|v| v > 0 }"#);
        run_test(r#"[5, -1, 7].any? {|v| v > 0 }"#);
        run_test(r#"[5, -1, 7].any?"#);
        run_test(r#"[5, nil, 7].any?"#);
        run_test(r#"[5, -1, false].any?"#);
        run_test(r#"[nil, false].any?"#);
        run_test(r#"[1, "a", 3].any?(String)"#);
        run_test(r#"[1, 2, 3].any?(String)"#);
    }

    #[test]
    fn pop_with_count() {
        run_test(
            r##"
            a = [1, 2, 3, 4, 5]
            [a.pop(2), a]"##,
        );
        run_test(
            r##"
            a = [1, 2, 3]
            [a.pop(0), a]"##,
        );
        run_test(
            r##"
            a = [1, 2, 3]
            [a.pop(10), a]"##,
        );
    }

    #[test]
    fn detect() {
        run_test(r#"[1, 2, 3, 4, 5].find {|i| i % 3 == 0 }"#);
        run_test(r#"[2, 2, 2, 2, 2].find {|i| i % 3 == 0 }"#);
    }

    #[test]
    fn grep() {
        run_test(r#"['aa', 'bb', 'cc', 'dd', 'ee'].grep(/[bc]/)"#);
        //run_test(r#"Array.instance_methods.grep(/gr/)"#);
        run_test(r#"['aa', 'bb', 'cc', 'dd', 'ee'].grep(/[bc]/) {|s| s.upcase }"#);
        run_test(r#"[1, 2, 3, 4, 5].grep(Integer) {|n| n * 2 }"#);
        run_test(r#"[1, 'a', 2, 'b'].grep(String)"#);
    }

    #[test]
    fn include() {
        run_test_with_prelude(
            r#"
            [a.include?("b"), a.include?("z")]
        "#,
            r#"
            a = ["a","b","c"]
        "#,
        );
    }

    #[test]
    fn reverse() {
        run_test(
            r#"
            a = [1, 2, 3, 4, 5]
            [a.reverse, a]
        "#,
        );
        run_test(
            r#"
            a = [1, 2, 3, 4, 5]
            a.reverse!
            a
        "#,
        );
    }

    #[test]
    fn transpose() {
        run_test(r#"[[1,2],[3,4],[5,6]].transpose"#);
        run_test(r#"[].transpose"#);
        run_test_error(r#"[1,2,3].transpose"#);
        run_test_error(r#"[[1,2],[3,4,5],[6,7]].transpose"#);
    }

    #[test]
    fn rotate() {
        run_test(
            r#"
        a = ["a","b","c","d"]
        [a.rotate, a]
        "#,
        );
        run_test(
            r#"
        a = ["a","b","c","d"]
        [a.rotate(2), a]
        "#,
        );
        run_test(
            r#"
        a = ["a","b","c","d"]
        [a.rotate(-3), a]
        "#,
        );
        run_test(
            r#"
        a = []
        b = a.rotate
        [a, b, a.object_id == b.object_id]
        "#,
        );
        run_test(
            r#"
        a = ["a","b","c","d"]
        [a.rotate!, a]
        "#,
        );
        run_test(
            r#"
        a = ["a","b","c","d"]
        [a.rotate!(2), a]
        "#,
        );
        run_test(
            r#"
        a = ["a","b","c","d"]
        [a.rotate!(-3), a]
        "#,
        );
        run_test(
            r#"
        a = []
        b = a.rotate!
        [a, b, a.object_id == b.object_id]
        "#,
        );
    }

    #[test]
    fn product() {
        run_test(r#"[1,2,3].product([4,5])"#);
        run_test(r#"[1,2].product([1,2])"#);
        run_test(r#"[1,2].product([3,4],[5,6])"#);
        run_test(r#"[1,2].product()"#);
        run_test(r#"[1,2].product([])"#);
        run_test(
            r#"
            a = []
            [1,2,3].product([4,5]) {|e| a << e} # => [1,2,3]
            a # => [[1,4],[1,5],[2,4],[2,5],[3,4],[3,5]]
            "#,
        );
        run_test_error(r#"[1,2].product(1,2,3)"#);
    }

    #[test]
    fn intersect() {
        run_test(
            r#"
            a = [ "1", "2", "3" ]
            b = [ "3", "4", "5" ]
            c = [ "5", "6", "7" ]
            [
                a.intersect?(b),  # => true
                a.intersect?(c)   # => false
            ]
        "#,
        );
        run_test(r#"["a", "b", "c"].intersect?([ "c", "d", "e" ])"#);
        run_test(r#"["a", "b", "c"].intersect?([ "d", "e", "f" ])"#);
    }

    #[test]
    fn union() {
        run_test(r#"["a", "b", "c"].union([ "c", "d", "a" ])"#);
        run_test(r#"["a"].union(["e", "b"], ["a", "c", "b"])"#);
        run_test(r#"["a"].union"#);
        run_test_error(r#"["a"].union([], 100)"#);
    }

    #[test]
    fn uniq() {
        run_test(
            r#"
        a = [1, 3, 2, 2.0, "2", "3", 3]
        b = a.uniq
        [a, b]
        "#,
        );
        run_test(
            r#"
        a = [1, 3, 2, 2.0, "2", "3", 3]
        b = a.uniq {|n| n.to_s }
        [a, b]
        "#,
        );
        run_test(
            r#"
        a = [1, 3, 2, 2.0, "2", "3", 3]
        a.uniq!
        a
        "#,
        );
        run_test(
            r#"
        a = [1, 3, 2, 2.0, "2", "3", 3]
        a.uniq! {|n| n.to_s }
        a
        "#,
        );
    }

    #[test]
    fn shuffle() {
        run_test(r#"[*(0..20)].shuffle!.shuffle!().sum"#);
    }

    #[test]
    fn slice() {
        run_test(
            r#"
        a = [ "a", "b", "c", "d", "e", "f", "g", "h" ]
        b = a.slice!(2, 3)
        [a, b]
        "#,
        );
        run_test(
            r#"
        a = [ "a", "b", "c", "d", "e", "f", "g", "h" ]
        b = a.slice!(-5, 3)
        [a, b]
        "#,
        );
        run_test(
            r#"
        a = [ "a", "b", "c", "d", "e", "f", "g", "h" ]
        b = a.slice!(-200, 3)
        [a, b]
        "#,
        );
        run_test(
            r#"
        a = [ "a", "b", "c", "d", "e", "f", "g", "h" ]
        b = a.slice!(2, -200)
        [a, b]
        "#,
        );
        run_test(
            r#"
        a = [ "a", "b", "c", "d", "e", "f", "g", "h" ]
        b = a.slice!(2..3)
        [a, b]
        "#,
        );
        run_test(
            r#"
        a = [ "a", "b", "c", "d", "e", "f", "g", "h" ]
        b = a.slice!(2...5)
        [a, b]
        "#,
        );
        run_test(
            r#"
        a = [ "a", "b", "c", "d", "e", "f", "g", "h" ]
        b = a.slice!(-100..3)
        [a, b]
        "#,
        );
        run_test(
            r#"
        a = [ "a", "b", "c", "d", "e", "f", "g", "h" ]
        b = a.slice!(2..-100)
        [a, b]
        "#,
        );
        run_test(
            r#"
        a = [ "a", "b", "c", "d", "e", "f", "g", "h" ]
        b = a.slice!(2..1)
        [a, b]
        "#,
        );
        run_test(
            r#"
        a = [ "a", "b", "c", "d", "e", "f", "g", "h" ]
        b = a.slice!(-100..-30)
        [a, b]
        "#,
        );
        // slice! with beginless range
        run_test(
            r#"
        a = [ "a", "b", "c", "d", "e" ]
        b = a.slice!(..2)
        [a, b]
        "#,
        );
        // slice! with endless range
        run_test(
            r#"
        a = [ "a", "b", "c", "d", "e" ]
        b = a.slice!(2..)
        [a, b]
        "#,
        );
        // slice! with nil..nil
        run_test(
            r#"
        a = [ "a", "b", "c", "d", "e" ]
        b = a.slice!(nil..nil)
        [a, b]
        "#,
        );
    }

    #[test]
    fn pack() {
        run_test(r#"[*(0..100)].pack("C*")"#);
        run_test(r#"[0x12345678].pack("I")"#);
        run_test(r#"[0x12345678].pack("N")"#);
    }

    #[test]
    fn pack_count() {
        run_test(r#"[1,2,3].pack("L3").unpack("L3")"#);
        run_test(r#"[1,2,3,4,5].pack("L*").unpack("L*")"#);
        run_test(r#"[1,2,3].pack("S3").unpack("S3")"#);
        run_test(r#"[1,2,3,4].pack("C4").unpack("C4")"#);
        run_test(r#"[1,2,3].pack("Q2").unpack("Q2")"#);
        run_test(r#"[0x1234, 0x5678].pack("n2").unpack("n2")"#);
        run_test(r#"[0x12345678, 0xdeadbeef].pack("V2").unpack("V2")"#);
    }

    #[test]
    fn pack_endian() {
        run_test(r#"[0x1234].pack("s<").unpack("s<")"#);
        run_test(r#"[0x1234].pack("s>").unpack("s>")"#);
        run_test(r#"[0x12345678].pack("l<").unpack("l<")"#);
        run_test(r#"[0x12345678].pack("l>").unpack("l>")"#);
        run_test(r#"[0x123456789ABCDEF0].pack("q<").unpack("q<")"#);
        run_test(r#"[0x123456789ABCDEF0].pack("q>").unpack("q>")"#);
        run_test(r#"[0x1234].pack("S<").unpack("S<")"#);
        run_test(r#"[0x1234].pack("S>").unpack("S>")"#);
        run_test(r#"[0x12345678].pack("L<").unpack("L<")"#);
        run_test(r#"[0x12345678].pack("L>").unpack("L>")"#);
    }

    #[test]
    fn pack_native_size() {
        // l!/L! — native long is 64-bit on x86-64
        run_test(r#"[0x123456789ABCDEF0].pack("l!").unpack("l!")"#);
        run_test(r#"[0x123456789ABCDEF0].pack("L!").unpack("L!")"#);
        // s!/S! — native short is still 16-bit
        run_test(r#"[0x1234].pack("s!").unpack("s!")"#);
        run_test(r#"[0x1234].pack("S!").unpack("S!")"#);
        // i!/I! — native int is still 32-bit
        run_test(r#"[0x12345678].pack("i!").unpack("i!")"#);
        run_test(r#"[0x12345678].pack("I!").unpack("I!")"#);
        // _ modifier is equivalent to !
        run_test(r#"[0x123456789ABCDEF0].pack("l_").unpack("l_")"#);
        run_test(r#"[0x123456789ABCDEF0].pack("L_").unpack("L_")"#);
    }

    #[test]
    fn pack_pointer_size() {
        // j/J — pointer-sized integer (64-bit on x86-64)
        run_test(r#"[42].pack("j").unpack("j")"#);
        run_test(r#"[42].pack("J").unpack("J")"#);
        run_test(r#"[-1].pack("j").unpack("j")"#);
        run_test(r#"[123456789].pack("J").unpack("J")"#);
    }

    #[test]
    fn pack_whitespace_and_comments() {
        // Whitespace between directives should be ignored
        run_test(r#"[1, 2].pack("C C").unpack("C C")"#);
        run_test(r#"[1, 2, 3].pack("C  C\tC").unpack("CCC")"#);
        // Comments should be ignored (# to end of line)
        run_test(r#"[1, 2].pack("C #comment\nC").unpack("CC")"#);
    }

    #[test]
    fn pack_bits() {
        run_test(r#"["10110001"].pack("b8").unpack("b8")"#);
        run_test(r#"["10110001"].pack("B8").unpack("B8")"#);
        run_test(r#"["1011"].pack("b4").unpack("b8")"#);
        run_test(r#"["1011"].pack("B4").unpack("B8")"#);
        run_test(r#"["10110001010"].pack("b*").unpack("b*")"#);
        run_test(r#"["10110001010"].pack("B*").unpack("B*")"#);
    }

    #[test]
    fn pack_utf8() {
        run_test(r#"[65, 66, 67].pack("U3")"#);
        run_test(r#"[0x3042, 0x3044].pack("U*")"#);
        run_test(r#"[65, 0x3042, 0x1F600].pack("U*").unpack("U*")"#);
        run_test(r#""ABC".unpack("U3")"#);
    }

    #[test]
    fn flatten() {
        run_test(r##"a = [1,2,[3,4,[5,6],7],8]; [a.flatten, a]"##);
        run_test(r##"[1,2,[3,4,[5,6],7],8].flatten(nil)"##);
        run_test(r##"[1,2,[3,4,[5,6],7],8].flatten(-1)"##);
        run_test(r##"[1,2,[3,4,[5,6],7],8].flatten(0)"##);
        run_test(r##"[1,2,[3,4,[5,6],7],8].flatten(1)"##);

        run_test(r##"a = [1,2,[3,4,[5,6],7],8]; [a.flatten!, a]"##);
        run_test(r##"a = [1,2,[3,4,[5,6],7],8]; [a.flatten!(nil), a]"##);
        run_test(r##"a = [1,2,[3,4,[5,6],7],8]; [a.flatten!(-1), a]"##);
        run_test(r##"a = [1,2,[3,4,[5,6],7],8]; [a.flatten!(0), a]"##);
        run_test(r##"a = [1,2,[3,4,[5,6],7],8]; [a.flatten!(1), a]"##);
    }

    #[test]
    fn compact() {
        run_test(
            r##"
        ary = [1, nil, 2, nil, 3, nil]
        [ary.compact, ary]
        "##,
        );
        run_test(
            r##"
        ary = [1, nil, 2, nil, 3, nil]
        [ary.compact!, ary]
        "##,
        );
        run_test(
            r##"
        ary = [1, 2, 3]
        [ary.compact!, ary]
        "##,
        );
    }

    #[test]
    fn delete() {
        run_test(
            r##"
        res = []
        a = [1, 2, 3, 2.0, 1]
        res << a.delete(2)
        res << a
        res << a.delete(1) {"wow"}
        res << a.delete(1) {"wow"}
        res << a
        res
        "##,
        );
    }

    #[test]
    fn delete_at() {
        run_test(
            r##"
        res = []
        a = [1, 2, 3, 2.0, 1]
        res << a.delete_at(7)
        res << a.delete_at(-10)
        res << a.delete_at(3)
        res << a.delete_at(-1)
        res << a
        res
        "##,
        );
    }

    #[test]
    fn find_index() {
        run_test(r##"[1, 0, 0, 1, 0].index(1) "##);
        run_test(r##"[1, 0, 0, 0, 0].index(1) "##);
        run_test(r##"[0, 0, 0, 0, 0].index(1)"##);
        run_test(r##"[0, 1, 0, 1, 0].index {|v| v > 0}"##);
    }

    #[test]
    fn insert() {
        run_test(
            r##"
        res = []
        ary = [1, 2, 3]
        res << ary.insert(2, "a", "b")
        res << ary.insert(-2, "X")
        res << ary.insert(30, 5, 9)
        res
        "##,
        );
    }

    #[test]
    fn bsearch() {
        run_test(
            r##"
        res = []
        ary = [0, 4, 7, 10, 12]
        res << ary.bsearch {|x| x >=   4 } # => 4
        res << ary.bsearch {|x| x >=   6 } # => 7
        res << ary.bsearch {|x| x >=  -1 } # => 0
        res << ary.bsearch {|x| x >= 100 } # => nil

        ary = [0, 4, 7, 10, 12]
        # 4 <= v < 8 になる要素を検索
        res << ary.bsearch {|x| 1 - x / 4 } # => 4 or 7
        # 8 <= v < 10 になる要素を検索
        res << ary.bsearch {|x| 4 - x / 2 } # => nil
        
        res
        "##,
        );
    }

    #[test]
    fn flatten_recursive() {
        run_test_once(
            r##"
        a = [1, 2]
        a << a
        begin
          a.flatten
          false
        rescue ArgumentError
          true
        end
        "##,
        );
    }

    #[test]
    fn array_tos_recursive() {
        // Self-containing array
        run_test_once(
            r##"
        a = [1, 2]
        a << a
        a.to_s
        "##,
        );
        // Same object appearing multiple times (not recursive)
        run_test(r#"a = [1]; b = [a, a]; b.to_s"#);
        run_test(r#"h = {x: 1}; [h, h].to_s"#);
    }

    #[test]
    fn array_inspect_user_defined() {
        // User-defined inspect on custom objects inside arrays
        run_test(
            r##"
        class Foo
          def inspect
            "custom_foo"
          end
        end
        [Foo.new, 1, "hello"].inspect
        "##,
        );
    }

    #[test]
    fn array_inspect() {
        // Empty array
        run_test(r#"[].inspect"#);
        run_test(r#"[].to_s"#);
        // Single element
        run_test(r#"[1].inspect"#);
        // Various types
        run_test(r#"[1, 2.5, "str", :sym, nil, true, false].inspect"#);
        // Nested arrays
        run_test(r#"[[1, 2], [3, [4, 5]]].inspect"#);
        // Array containing hash
        run_test(r#"[{a: 1, b: 2}, {c: 3}].inspect"#);
        // Mixed nesting
        run_test(r#"[1, [2, {a: 3}], "hello", :world].inspect"#);
        // Array with string containing special characters
        run_test(r#"["hello\nworld", "tab\there"].inspect"#);
        // Array with Range
        run_test(r#"[1..5, 1...5].inspect"#);
        // to_s is aliased to inspect
        run_test(r#"[1, 2, 3].to_s"#);
        // User-defined to_s should NOT affect inspect output
        // (inspect uses inspect, not to_s, for each element)
        run_test(
            r##"
        class Baz
          def to_s
            "baz_to_s"
          end
          def inspect
            "baz_inspect"
          end
        end
        [Baz.new].inspect
        "##,
        );
        // User-defined inspect inside nested structures
        run_test(
            r##"
        class MyObj
          def inspect
            "<my>"
          end
        end
        [[MyObj.new], {k: MyObj.new}].inspect
        "##,
        );
    }

    #[test]
    fn product_size_check() {
        run_test_no_result_check(
            r##"
        begin
          ([0] * 1000).product([0] * 1000, [0] * 1000)
          false
        rescue RangeError
          true
        end
        "##,
        );
    }

    #[test]
    fn slice_bang_integer() {
        run_test(
            r##"
        a = [1, 2, 3, 4, 5]
        [a.slice!(2), a]
        "##,
        );
    }

    #[test]
    fn index_assign_negative() {
        run_test(r##"a = [1,2,3,4,5]; a[-2, 2] = []; a"##);
        run_test(r##"a = [1,2,3,4,5]; a[-3, 1] = [99]; a"##);
        run_test(r##"a = [1,2,3,4,5]; a[-5, 3] = [:a, :b]; a"##);
        run_test(r##"a = [1,2,3,4,5]; a[-1, 1] = [:x, :y, :z]; a"##);
        run_test(r##"a = [1,2,3]; a[-3, 0] = [:a]; a"##);
        run_test_error(r##"a = [1,2,3]; a[-4, 1] = []"##);
    }

    #[test]
    fn index_assign_self() {
        // Self-assignment: b[start, length] = b
        run_test(r##"b = [1, 2, 3, 4, 5]; b[1, 0] = b; b"##);
        run_test(r##"b = [1, 2, 3, 4, 5]; b[0, 5] = b; b"##);
        run_test(r##"b = [1, 2, 3, 4, 5]; b[2, 2] = b; b"##);
        run_test(r##"b = [1, 2, 3]; b[0, 0] = b; b"##);
        run_test(r##"b = [1]; b[0, 1] = b; b"##);
        // Self-assignment via range
        run_test(r##"b = [1, 2, 3, 4, 5]; b[1..2] = b; b"##);
        run_test(r##"b = [1, 2, 3]; b[0..0] = b; b"##);
    }

    #[test]
    fn index_assign_range_end_less_than_start() {
        // end < start with range: treated as zero-length insert at start
        run_test(r##"a = [1, 2, 3, 4, 5]; a[3..1] = [:a, :b]; a"##);
        // start beyond array size: fills with nil then inserts
        run_test(r##"a = [1, 2, 3]; a[5..2] = [99]; a"##);
        run_test(r##"a = [1, 2, 3]; a[5..2] = 42; a"##);
    }

    #[test]
    fn replace() {
        run_test(r##"a = [1,2,3]; b = [4,5]; a.replace(b); [a, b]"##);
        run_test(r##"a = [1,2,3]; a.replace([]); a"##);
        run_test(r##"a = []; a.replace([1,2,3]); a"##);
        run_test(r##"a = [1,2,3]; a.replace(a); a"##);
    }

    #[test]
    fn cycle() {
        run_test(
            r##"
            res = []
            [1,2,3].cycle(2) { |x| res << x }
            res
            "##,
        );
        run_test(
            r##"
            res = []
            [1,2].cycle(0) { |x| res << x }
            res
            "##,
        );
        run_test("[].cycle(3) { |x| x }");
    }

    #[test]
    fn combination() {
        run_test(
            r##"
            res = []
            [1,2,3,4].combination(2) { |c| res << c }
            res
            "##,
        );
        run_test(
            r##"
            res = []
            [1,2,3].combination(0) { |c| res << c }
            res
            "##,
        );
        run_test(
            r##"
            res = []
            [1,2,3].combination(1) { |c| res << c }
            res
            "##,
        );
    }

    #[test]
    fn bsearch_index() {
        run_test("[1,2,3,4,5].bsearch_index { |x| x >= 3 }");
        run_test("[1,2,3,4,5].bsearch_index { |x| x >= 6 }");
        run_test("[1,3,5,7,9].bsearch_index { |x| x <=> 5 }");
    }

    #[test]
    fn implicit_type_conversions() {
        // to_int conversions
        run_test_with_prelude(
            "[1,2,3,4,5].first(n)",
            "class MyNum; def to_int; 3; end; end\nn = MyNum.new",
        );
        run_test_with_prelude(
            "[1,2,3,4,5].drop(n)",
            "class MyNum; def to_int; 2; end; end\nn = MyNum.new",
        );
        run_test_with_prelude(
            "[1,2,3,4,5].fetch(n)",
            "class MyNum; def to_int; 2; end; end\nn = MyNum.new",
        );
        run_test_with_prelude(
            "[1,2,3,4,5].delete_at(n)",
            "class MyNum; def to_int; 2; end; end\nn = MyNum.new",
        );
        run_test_with_prelude(
            "[[1,[2]],3].flatten(n)",
            "class MyNum; def to_int; 1; end; end\nn = MyNum.new",
        );
        run_test_with_prelude(
            "a = [1,2,3]; a.insert(n, 99); a",
            "class MyNum; def to_int; 1; end; end\nn = MyNum.new",
        );
        // to_ary conversions
        run_test_with_prelude(
            "([1,2,3] <=> obj)",
            "class MyAry; def to_ary; [1,2,3]; end; end\nobj = MyAry.new",
        );
        run_test_with_prelude(
            "a = [1]; a.concat(obj); a",
            "class MyAry; def to_ary; [4,5]; end; end\nobj = MyAry.new",
        );
        // to_int in []=
        run_test_with_prelude(
            "a = [1,2,3,4,5]; a[n] = 99; a",
            "class MyNum; def to_int; 2; end; end\nn = MyNum.new",
        );
        // cycle with to_int
        run_test_with_prelude(
            "res = []; [1,2].cycle(n) { |x| res << x }; res",
            "class MyNum; def to_int; 2; end; end\nn = MyNum.new",
        );
        // combination with to_int
        run_test_with_prelude(
            "res = []; [1,2,3].combination(n) { |c| res << c }; res",
            "class MyNum; def to_int; 2; end; end\nn = MyNum.new",
        );
    }

    #[test]
    fn permutation() {
        run_test(
            r##"
            res = []
            [1,2,3].permutation(2) { |p| res << p }
            res
            "##,
        );
        run_test(
            r##"
            res = []
            [1,2,3].permutation { |p| res << p }
            res
            "##,
        );
        run_test(
            r##"
            res = []
            [1,2,3].permutation(0) { |p| res << p }
            res
            "##,
        );
        run_test(
            r##"
            res = []
            [1,2,3].permutation(1) { |p| res << p }
            res
            "##,
        );
        run_test("[1,2,3].permutation(4).to_a");
        run_test("[1,2,3].permutation(-1).to_a");
    }

    #[test]
    fn repeated_combination() {
        run_test(
            r##"
            res = []
            [1,2,3].repeated_combination(2) { |c| res << c }
            res
            "##,
        );
        run_test(
            r##"
            res = []
            [1,2].repeated_combination(3) { |c| res << c }
            res
            "##,
        );
        run_test("[1,2,3].repeated_combination(0).to_a");
        run_test("[1,2,3].repeated_combination(1).to_a");
    }

    #[test]
    fn array_implicit_conversions() {
        // Array#+ calls to_ary
        run_test_with_prelude(
            "[1, 2] + o",
            "class C; def to_ary; [3, 4]; end; end; o = C.new",
        );
        // Array#- calls to_ary
        run_test_with_prelude(
            "[1, 2, 3] - o",
            "class C; def to_ary; [2]; end; end; o = C.new",
        );
        // Array#* with to_str joins
        run_test_with_prelude(
            "[1, 2, 3] * o",
            r#"class C; def to_str; ","; end; end; o = C.new"#,
        );
        // Array#* with to_int repeats
        run_test_with_prelude("[1, 2] * o", "class C; def to_int; 3; end; end; o = C.new");
        // Array#[] with to_int
        run_test_with_prelude(
            "[10, 20, 30][o]",
            "class C; def to_int; 1; end; end; o = C.new",
        );
        // Array#last with to_int
        run_test_with_prelude(
            "[1,2,3,4,5].last(o)",
            "class C; def to_int; 2; end; end; o = C.new",
        );
        // Array#take with to_int
        run_test_with_prelude(
            "[1,2,3,4,5].take(o)",
            "class C; def to_int; 3; end; end; o = C.new",
        );
    }

    #[test]
    fn array_new_huge_size() {
        run_test_error(r##"Array.new(2**62)"##);
        run_test_error(r##"Array.new(1 << 31)"##);
    }

    #[test]
    fn pack_hex() {
        run_test(r#"["6162"].pack("h4")"#);
        run_test(r#"["6162"].pack("H4")"#);
        run_test(r#"["6162636465"].pack("h*")"#);
        run_test(r#"["6162636465"].pack("H*")"#);
        run_test(r#""\x16\x26".unpack("h4")"#);
        run_test(r#""\x61\x62".unpack("H4")"#);
    }

    // ===== Tests for new methods =====

    #[test]
    fn array_class_bracket() {
        run_test("Array[1, 2, 3]");
        run_test("Array[]");
        run_test("Array[42]");
        // Subclass support
        run_test_with_prelude(
            "A[1, 2, 3]",
            "class A < Array; end",
        );
        run_test_with_prelude(
            "A[1, 2, 3].class",
            "class A < Array; end",
        );
    }

    #[test]
    fn array_try_convert() {
        run_test("Array.try_convert([1, 2])");
        run_test("Array.try_convert(nil)");
        run_test(r#"Array.try_convert("string")"#);
        // Object with to_ary
        run_test_with_prelude(
            "Array.try_convert(C.new)",
            "class C; def to_ary; [1, 2]; end; end",
        );
        // Object without to_ary returns nil
        run_test_with_prelude(
            "Array.try_convert(C.new)",
            "class C; end",
        );
        // to_ary returns non-Array raises TypeError
        run_test_error(
            "class C; def to_ary; 'not array'; end; end; Array.try_convert(C.new)",
        );
    }

    #[test]
    fn array_at() {
        run_test("[1, 2, 3].at(0)");
        run_test("[1, 2, 3].at(1)");
        run_test("[1, 2, 3].at(-1)");
        run_test("[1, 2, 3].at(5)");
    }

    #[test]
    fn array_rindex() {
        // With value argument
        run_test("[1, 2, 3, 2, 1].rindex(2)");
        run_test("[1, 2, 3].rindex(4)");
        // With block
        run_test("[1, 2, 3, 4].rindex {|x| x > 2}");
        run_test("[1, 2, 3].rindex {|x| x > 10}");
    }

    #[test]
    fn array_each_index() {
        run_test("res = []; [10, 20, 30].each_index {|i| res << i}; res");
        run_test("[].each_index {|i| i}");
    }

    #[test]
    fn array_difference() {
        run_test("[1, 2, 3, 4, 5].difference([2, 4])");
        run_test("[1, 2, 3].difference([1], [3])");
        run_test("[1, 2, 3].difference([])");
        run_test("[1, 2, 3].difference()");
        // Duplicates preserved from self
        run_test("[1, 1, 2, 2, 3].difference([1])");
    }

    #[test]
    fn array_intersection() {
        run_test("[1, 2, 3, 4].intersection([2, 3, 5])");
        run_test("[1, 2, 3].intersection([2, 3], [3, 4])");
        run_test("[1, 2, 3].intersection([])");
        // Duplicates removed
        run_test("[1, 1, 2, 2].intersection([1, 2])");
    }

    #[test]
    fn array_repeated_permutation() {
        run_test("[1, 2].repeated_permutation(2).to_a.sort");
        run_test("[1, 2, 3].repeated_permutation(0).to_a");
        run_test("[1, 2].repeated_permutation(1).to_a.sort");
        run_test("[].repeated_permutation(2).to_a");
    }

    #[test]
    fn array_join_nil_separator() {
        run_test("[1, 2, 3].join(nil)");
        run_test("[1, 2, 3].join");
        run_test(r#"[1, 2, 3].join("-")"#);
        run_test("[].join(nil)");
        // nested arrays use same separator
        run_test(r#"[[1, [2]], 3].join("-")"#);
        // * with string acts like join
        run_test(r#"[1, 2, 3] * ",""#);
        run_test(r#"[[1, 2], [3, 4]] * "-""#);
    }

    #[test]
    fn join_conversion_order() {
        // to_str is tried first
        run_test_with_prelude(
            r#"[C.new].join"#,
            r#"class C; def to_s; "s"; end; def to_str; "str"; end; end"#,
        );
        // to_ary causes recursive join
        run_test_with_prelude(
            r#"[C.new].join("-")"#,
            r#"class C; def to_ary; [1, 2]; end; end"#,
        );
    }

    #[test]
    fn bsearch_nil_result() {
        // nil block result treated as find-minimum (false)
        run_test("[1, 2, 3].bsearch { nil }");
        run_test("[1, 2, 3].bsearch_index { nil }");
        // empty block => nil
        run_test("[1, 2, 3, 4].bsearch { |x| nil }");
    }

    #[test]
    fn sort_block_nil_and_cmp() {
        // block returning nil raises ArgumentError
        run_test_error("[1, 2].sort {}");
        // block with <=> comparison
        run_test("[1, 2, 5, 10, 7, -4, 12].sort { |n, m| n - m }");
    }

    #[test]
    fn transpose_to_ary() {
        run_test("[[1, 2], [3, 4]].transpose");
        run_test_with_prelude(
            "[[1, 2], C.new].transpose",
            "class C; def to_ary; [3, 4]; end; end",
        );
    }

    #[test]
    fn index_ignores_block_with_arg() {
        run_test("[1, 2, 3].index(2) { |x| false }");
        run_test("[1, 2, 3].rindex(2) { |x| false }");
    }

    #[test]
    fn map_bang_frozen() {
        run_test_error("[1, 2, 3].freeze.map! { |x| x }");
        run_test_error("[].freeze.map! { |x| x }");
    }

    #[test]
    fn values_at_range_padding() {
        run_test("[1, 2, 3].values_at(1..5)");
        run_test("[].values_at(0..2)");
        run_test("[1, 2, 3, 4, 5].values_at(1..3)");
        // endless range
        run_test("[1, 2, 3, 4].values_at(1..)");
        run_test("[1, 2, 3, 4].values_at(3...)");
        run_test("[1, 2, 3].values_at(0...5)");
    }

    #[test]
    fn cycle_to_int_and_size() {
        run_test("[1, 2, 3].cycle(2).to_a");
        run_test("[1, 2, 3].cycle(0).to_a");
        run_test("[].cycle(5).to_a");
        // Enumerator size
        run_test("[1, 2, 3].cycle(2).size");
        run_test("[1, 2, 3].cycle(0).size");
        run_test("[].cycle(2).size");
        run_test_no_result_check("[1, 2].cycle.size");
    }

    #[test]
    fn subclass_return_types() {
        run_test_with_prelude("C[1,2,3].sort.class", "class C < Array; end");
        run_test_with_prelude("(C[1,2] + [3]).class", "class C < Array; end");
        run_test_with_prelude("(C[1,2,2] & [2,3]).class", "class C < Array; end");
        run_test_with_prelude("(C[1,2] | [3]).class", "class C < Array; end");
        run_test_with_prelude("C[1,2,2].uniq.class", "class C < Array; end");
        run_test_with_prelude("C[1,nil,2].compact.class", "class C < Array; end");
        run_test_with_prelude("C[3,1,2].rotate.class", "class C < Array; end");
        run_test_with_prelude("C[1,2].union([3]).class", "class C < Array; end");
        run_test_with_prelude("C[1,2,3].to_a.class", "class C < Array; end");
        run_test_with_prelude("C[1,2,3].map { |x| x }.class", "class C < Array; end");
    }

    #[test]
    fn pattern_arg_ignores_block() {
        run_test("[1, 2, 3].all?(Integer) { |x| false }");
        run_test("[1, 2, 3].any?(String) { |x| true }");
        run_test("[1, 2, 3].count(1) { |x| true }");
        run_test("[1, 2, 3].none?(String) { |x| true }");
        run_test("[1, 2, 3].one?(1) { |x| false }");
    }

    #[test]
    fn minmax_with_block() {
        run_test("[1, 3, 2].minmax");
        run_test("[1, 3, 2].minmax { |a, b| b <=> a }");
        run_test("[].minmax");
        run_test("[5].minmax");
    }

    #[test]
    fn sample() {
        // sample without arguments returns a single element
        run_test("[1].sample");
        run_test("[].sample");
        // sample(n) returns an array of n elements
        run_test("[1, 2, 3, 4, 5].sample(0)");
        run_test("[1, 2, 3].sample(1).size");
        run_test("[1, 2, 3].sample(5).sort");
        run_test_no_result_check("[1, 2, 3, 4, 5].sample(3).size");
        // sample with negative n raises error
        run_test_error("[1, 2, 3].sample(-1)");
    }

    #[test]
    fn shuffle_nondestructive() {
        // shuffle returns a new array with the same elements
        run_test_no_result_check("[1, 2, 3].shuffle.sort");
        run_test(
            r#"
            a = [1, 2, 3]
            a.shuffle
            a
            "#,
        );
        // shuffle returns Array, not subclass
        run_test_with_prelude(
            r#"
            a = C.new([1, 2, 3])
            a.shuffle.class
            "#,
            "class C < Array; end",
        );
    }

    #[test]
    fn fetch_values() {
        run_test("[10, 20, 30].fetch_values(0, 2)");
        run_test("[10, 20, 30].fetch_values(0, -1)");
        run_test("[10, 20, 30].fetch_values");
        // with block for missing indices
        run_test("[10, 20, 30].fetch_values(0, 5) { |i| i * 100 }");
        // error for out-of-range without block
        run_test_error("[10, 20, 30].fetch_values(0, 5)");
    }

    #[test]
    fn min_max_with_block() {
        // min with block
        run_test("[2, 33, 4, 11].min {|a, b| a <=> b}");
        run_test(r#"["2","33","4","11"].min {|a,b| a.length <=> b.length}"#);
        run_test("[2, 33, 4, 11].min {|a, b| b <=> a}");
        // max with block
        run_test("[2, 33, 4, 11].max {|a, b| a <=> b}");
        run_test(r#"["2","33","4","11"].max {|a,b| a.length <=> b.length}"#);
        run_test("[2, 33, 4, 11].max {|a, b| b <=> a}");
        // block returning constant
        run_test("[1, 2, 3, 4].min {|a,b| 15}");
        run_test("[1, 2, 3, 4].max {|a,b| 15}");
        // nil block result raises ArgumentError
        run_test_error("[11, 12, 22, 33].min {|a, b| nil}");
        run_test_error("[11, 12, 22, 33].max {|a, b| nil}");
        // empty array
        run_test("[].min {|a,b| a <=> b}");
        run_test("[].max {|a,b| a <=> b}");
    }

    #[test]
    fn assoc() {
        run_test(
            r#"
            s1 = [1, 2]
            s2 = [2, 3]
            a = [s1, s2]
            [a.assoc(1), a.assoc(2), a.assoc(42)]
            "#,
        );
        // ignores non-array elements
        run_test(r#"["foo", [1, 2], [3, 4]].assoc(3)"#);
        run_test(r#"["foo", [1, 2], [3, 4]].assoc("bar")"#);
    }

    #[test]
    fn rassoc() {
        run_test(
            r#"
            a = [[1, "one"], [2, "two"], [3, "three"]]
            [a.rassoc("two"), a.rassoc("four")]
            "#,
        );
        // ignores non-array elements
        run_test(r#"["foo", [1, 2], [3, 4]].rassoc(4)"#);
        run_test(r#"["foo", [1, 2], [3, 4]].rassoc(99)"#);
    }

    #[test]
    fn keep_if() {
        run_test(
            r#"
            a = [1, 2, 3, 4, 5]
            a.keep_if { |x| x > 2 }
            a
            "#,
        );
        // returns self even when no changes
        run_test(
            r#"
            a = [1, 2, 3]
            a.keep_if { |x| true }.equal?(a)
            "#,
        );
        run_test("[1, 2, 3].keep_if { |x| x.odd? }");
    }

    #[test]
    fn append_method() {
        run_test("[1, 2].append(3, 4)");
        run_test("[].append(1)");
        run_test(
            r#"
            a = [1]
            a.append(2, 3)
            a
            "#,
        );
    }

    #[test]
    fn drop_while() {
        run_test("[1, 2, 3, 4, 5].drop_while { |x| x < 3 }");
        run_test("[1, 2, 3].drop_while { |x| true }");
        run_test("[1, 2, 3].drop_while { |x| false }");
        run_test("[].drop_while { |x| true }");
    }

    #[test]
    fn to_ary() {
        run_test("[1, 2, 3].to_ary");
        run_test(
            r#"
            a = [1, 2]
            a.to_ary.equal?(a)
            "#,
        );
    }

    #[test]
    fn deconstruct() {
        run_test("[1, 2, 3].deconstruct");
        run_test(
            r#"
            a = [1, 2]
            a.deconstruct.equal?(a)
            "#,
        );
    }

    #[test]
    fn frozen_returns_enumerator_without_block() {
        // filter!/select! on frozen array without block should return Enumerator
        run_test(r#"[1,2,3].freeze.select!.class"#);
        run_test(r#"[1,2,3].freeze.filter!.class"#);
        // keep_if on frozen array without block should return Enumerator
        run_test(r#"[1,2,3].freeze.keep_if.class"#);
        // reject! on frozen array without block should return Enumerator
        run_test(r#"[1,2,3].freeze.reject!.class"#);
        // delete_if on frozen array without block should return Enumerator
        run_test(r#"[1,2,3].freeze.delete_if.class"#);
        // sort_by! on frozen array without block should return Enumerator
        run_test(r#"[1,2,3].freeze.sort_by!.class"#);
    }

    #[test]
    fn all_any_ignore_block_with_pattern() {
        // When pattern argument is given, block should be ignored
        run_test(r#"[1, 2, 3].all?(Integer) { |x| false }"#);
        run_test(r#"[1, 2, 3].any?(String) { |x| true }"#);
        run_test(r#"[1, 2, 3].none?(String) { |x| true }"#);
        run_test(r#"[1, 2, 3].one?(Integer) { |x| false }"#);
        run_test(r#"[1, 2, 3].count(2) { |x| true }"#);
    }
}
