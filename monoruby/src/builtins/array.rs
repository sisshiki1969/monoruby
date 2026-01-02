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
    globals.define_builtin_func_with(ARRAY_CLASS, "initialize", initialize, 0, 2, false);
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
    globals.define_builtin_func_with(ARRAY_CLASS, "push", push, 0, 0, true);
    globals.define_builtin_func(ARRAY_CLASS, "pop", pop, 0);
    globals.define_builtin_funcs(ARRAY_CLASS, "==", &["==="], eq, 1);
    globals.define_builtin_func(ARRAY_CLASS, "eql?", eql, 1);
    globals.define_builtin_func(ARRAY_CLASS, "<=>", cmp, 1);
    globals.define_builtin_funcs_with(ARRAY_CLASS, "[]", &["slice"], index, 1, 2, false);
    globals.define_builtin_func_with(ARRAY_CLASS, "[]=", index_assign, 2, 3, false);
    globals.define_builtin_func(ARRAY_CLASS, "clear", clear, 0);
    globals.define_builtin_func(ARRAY_CLASS, "fill", fill, 1);
    globals.define_builtin_func(ARRAY_CLASS, "drop", drop, 1);
    globals.define_builtin_func_rest(ARRAY_CLASS, "zip", zip);
    globals.define_builtin_funcs_with(ARRAY_CLASS, "inject", &["reduce"], inject, 0, 2, false);
    globals.define_builtin_func_with(ARRAY_CLASS, "join", join, 0, 1, false);
    globals.define_builtin_func_with(ARRAY_CLASS, "first", first, 0, 1, false);
    globals.define_builtin_func_with(ARRAY_CLASS, "last", last, 0, 1, false);
    globals.define_builtin_func(ARRAY_CLASS, "take", take, 1);
    globals.define_builtin_func_with(ARRAY_CLASS, "sum", sum, 0, 1, false);
    globals.define_builtin_func(ARRAY_CLASS, "min", min, 0);
    globals.define_builtin_func(ARRAY_CLASS, "max", max, 0);
    globals.define_builtin_func(ARRAY_CLASS, "partition", partition, 0);
    globals.define_builtin_funcs(ARRAY_CLASS, "filter", &["select", "find_all"], filter, 0);
    globals.define_builtin_funcs(ARRAY_CLASS, "filter!", &["select!"], filter_, 0);
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
    globals.define_builtin_func(ARRAY_CLASS, "all?", all_, 0);
    globals.define_builtin_func(ARRAY_CLASS, "any?", any_, 0);
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
fn new(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let class = lfp.self_val().as_class_id();
    let obj = Value::array_empty_with_class(class);
    vm.invoke_method_if_exists(
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
fn allocate(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let class = lfp.self_val().as_class_id();
    let obj = Value::array_empty_with_class(class);
    Ok(obj)
}

fn array_allocate(
    bb: &mut AbstractContext,
    ir: &mut AsmIr,
    _: &JitContext,
    _: &Store,
    callsite: &CallSiteInfo,
    _: ClassId,
    _: BytecodePtr,
) -> bool {
    if !callsite.is_simple() {
        return false;
    }
    let dst = callsite.dst;
    bb.load(ir, callsite.recv, GP::Rdi);
    ir.inline(move |r#gen, _, _| {
        monoasm! { &mut r#gen.jit,
            movq rax, (allocate_array);
            call rax;
        }
    });

    bb.def_reg2acc(ir, GP::Rax, dst);
    true
}

extern "C" fn allocate_array(class_val: Value) -> Value {
    let class_id = class_val.as_class_id();
    Value::array_empty_with_class(class_id)
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
fn initialize(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let mut self_val = lfp.self_val().as_array();
    if lfp.try_arg(0).is_none() {
        return Ok(self_val.into());
    }
    if lfp.try_arg(1).is_none() {
        if let Some(ary) = lfp.arg(0).try_array_ty() {
            *self_val = (*ary).clone();
            return Ok(self_val.into());
        }
    }
    if let Some(size) = lfp.arg(0).try_fixnum() {
        if size < 0 {
            return Err(MonorubyErr::negative_array_size());
        }
        let size = size as usize;
        if let Some(bh) = lfp.block() {
            if lfp.try_arg(1).is_some() {
                eprintln!("warning: block supersedes default value argument");
            }
            let iter = (0..size).map(|i| Value::integer(i as i64));
            let mut res = vm.invoke_block_map1(globals, bh, iter, size)?;
            RValue::swap_kind(lfp.self_val().rvalue_mut(), res.rvalue_mut());
        } else {
            let val = if lfp.try_arg(1).is_none() {
                Value::nil()
            } else {
                lfp.arg(1)
            };
            *self_val = ArrayInner::from(smallvec![val; size]);
        }
        Ok(self_val.into())
    } else {
        Err(MonorubyErr::no_implicit_conversion(
            globals,
            lfp.arg(0),
            INTEGER_CLASS,
        ))
    }
}

///
/// ### Array#length
///
/// - length -> Integer
/// - size -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/length.html]
#[monoruby_builtin]
fn size(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let len = lfp.self_val().as_array().len();
    Ok(Value::integer(len as i64))
}

fn array_size(
    bb: &mut AbstractContext,
    ir: &mut AsmIr,
    _: &JitContext,
    _: &Store,
    callsite: &CallSiteInfo,
    _: ClassId,
    _: BytecodePtr,
) -> bool {
    if !callsite.is_simple() {
        return false;
    }
    let dst = callsite.dst;
    bb.load(ir, callsite.recv, GP::Rdi);
    ir.inline(move |r#gen, _, _| {
        r#gen.get_array_length();
        monoasm! { &mut r#gen.jit,
            salq  rax, 1;
            orq   rax, 1;
        }
    });

    bb.def_reg2acc_fixnum(ir, GP::Rax, dst);
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
fn clone(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let class_id = lfp.self_val().class();
    Ok(Value::array_with_class(
        lfp.self_val().as_array_inner().clone(),
        class_id,
    ))
}

fn array_clone(
    bb: &mut AbstractContext,
    ir: &mut AsmIr,
    _: &JitContext,
    _: &Store,
    callsite: &CallSiteInfo,
    class_id: ClassId,
    _: BytecodePtr,
) -> bool {
    if !callsite.is_simple() {
        return false;
    }
    let dst = callsite.dst;
    bb.load(ir, callsite.recv, GP::Rdi);
    let using_xmm = bb.get_using_xmm();
    ir.xmm_save(using_xmm);
    ir.inline(move |r#gen, _, _| {
        monoasm! { &mut r#gen.jit,
            movq rax, (array_dup);
            call rax;
        }
    });
    ir.xmm_restore(using_xmm);
    bb.def_reg2acc_class(ir, GP::Rax, dst, class_id);
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
fn count(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    if let Some(arg0) = lfp.try_arg(0) {
        if lfp.block().is_some() {
            eprintln!("warning: given block not used");
        }
        let mut count = 0;
        for elem in lfp.self_val().as_array().iter() {
            if vm.invoke_eq(globals, arg0, *elem)? {
                count += 1;
            }
        }
        Ok(Value::integer(count))
    } else if let Some(bh) = lfp.block() {
        let mut count = 0;
        let bh = vm.get_block_data(globals, bh)?;
        for elem in lfp.self_val().as_array().iter() {
            if vm.invoke_block(globals, &bh, &[*elem])?.as_bool() {
                count += 1;
            }
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
fn empty(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let b = lfp.self_val().as_array().is_empty();
    Ok(Value::bool(b))
}

///
/// ### Array#to_a
///
/// - to_a -> Array
///
/// [https://docs.ruby-lang.org/ja/latest/method/Object/i/to_a.html]
#[monoruby_builtin]
fn to_a(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    Ok(lfp.self_val())
}

///
/// ### Array#to_h
///
/// - to_h -> Hash
/// - to_h {|elem| .. } -> Hash
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/to_h.html]
#[monoruby_builtin]
fn to_h(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    fn inner(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<()> {
        let self_ = lfp.self_val().as_array();
        let block = match lfp.block() {
            Some(bh) => Some(vm.get_block_data(globals, bh)?),
            None => None,
        };
        for (i, elem) in self_.iter().enumerate() {
            let elem = if let Some(p) = &block {
                vm.invoke_block(globals, p, &[*elem])?
            } else {
                *elem
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
fn hash(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
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
fn add(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let mut lhs = lfp.self_val().dup().as_array();
    let rhs = match lfp.arg(0).try_array_ty() {
        Some(v) => v,
        None => {
            return Err(MonorubyErr::no_implicit_conversion(
                globals,
                lfp.arg(0),
                ARRAY_CLASS,
            ));
        }
    };
    lhs.extend_from_slice(&rhs);
    Ok(lhs.into())
}

///
/// ### Array#-
///
/// - self - other -> Array
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/=2d.html]
#[monoruby_builtin]
fn sub(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let lhs_v = lfp.self_val();
    let rhs = match lfp.arg(0).try_array_ty() {
        Some(ary) => ary,
        None => {
            return Err(MonorubyErr::no_implicit_conversion(
                globals,
                lfp.arg(0),
                ARRAY_CLASS,
            ));
        }
    };
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
fn mul(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let lhs = lfp.self_val().as_array();
    if let Some(v) = lfp.arg(0).try_fixnum() {
        if v < 0 {
            return Err(MonorubyErr::negative_argument());
        }
        let rhs = v as usize;
        let vec = lhs.repeat(rhs);
        Ok(Value::array_from_vec(vec))
    } else if let Some(sep) = lfp.arg(0).is_str() {
        let res = array_join(&globals.store, lhs, sep);
        Ok(Value::string(res))
    } else {
        Err(MonorubyErr::no_implicit_conversion(
            globals,
            lfp.arg(0),
            INTEGER_CLASS,
        ))
    }
}

///
/// ### Array#&
///
/// - self & other -> Array
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/=26.html]
#[monoruby_builtin]
fn and(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let mut lhs = lfp.self_val().dup().as_array();
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
fn or(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let mut lhs = lfp.self_val().dup().as_array();
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
fn shift(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let mut ary = lfp.self_val().as_array();
    if lfp.try_arg(0).is_none() {
        if ary.len() == 0 {
            return Ok(Value::nil());
        }
        let res = ary[0];
        ary.drain(0..1);
        Ok(res)
    } else {
        let i = lfp.arg(0).coerce_to_i64(globals)?;
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
fn unshift(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
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
fn concat(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let mut self_ary = lfp.self_val().as_array();
    let mut ary: Array = Array::new_empty();
    for a in lfp.arg(0).as_array().iter().cloned() {
        if let Some(a) = a.try_array_ty() {
            ary.extend_from_slice(&a);
        } else {
            return Err(MonorubyErr::no_implicit_conversion(globals, a, ARRAY_CLASS));
        }
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
fn shl(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let mut ary = lfp.self_val().as_array();
    ary.push(lfp.arg(0));
    Ok(ary.into())
}

extern "C" fn ary_shl(mut ary: Array, arg: Value) -> Value {
    ary.push(arg);
    ary.into()
}

fn array_shl(
    bb: &mut AbstractContext,
    ir: &mut AsmIr,
    _: &JitContext,
    _: &Store,
    callsite: &CallSiteInfo,
    recv_class: ClassId,
    _: BytecodePtr,
) -> bool {
    if !callsite.is_simple() {
        return false;
    }
    let CallSiteInfo {
        dst, args, recv, ..
    } = *callsite;
    bb.load(ir, recv, GP::Rdi);
    bb.load(ir, args, GP::Rsi);
    let using_xmm = bb.get_using_xmm();
    ir.xmm_save(using_xmm);
    ir.inline(move |r#gen, _, _| {
        monoasm!( &mut r#gen.jit,
            movq rax, (ary_shl);
            call rax;
        );
    });
    ir.xmm_restore(using_xmm);
    bb.def_reg2acc_class(ir, GP::Rax, dst, recv_class);
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
fn push(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let mut ary = lfp.self_val().as_array();
    ary.extend(lfp.arg(0).as_array().iter().cloned());
    Ok(ary.into())
}

///
/// ### Array#pop
///
/// - pop -> object | nil
/// - [NOT SUPPORTED] pop(n) -> Array
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/pop.html]
#[monoruby_builtin]
fn pop(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let mut ary = lfp.self_val().as_array();
    let res = ary.pop().unwrap_or_default();
    Ok(res)
}

///
/// ### Array#eql?
///
/// - self.eql?(other) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/eql=3f.html]
#[monoruby_builtin]
fn eql(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let lhs = lfp.self_val().as_array();
    let rhs = if let Some(rhs) = lfp.arg(0).try_array_ty() {
        rhs
    } else {
        return Ok(Value::bool(false));
    };
    if lhs.len() != rhs.len() {
        return Ok(Value::bool(false));
    }
    for i in 0..lhs.len() {
        if !lhs[i].eql(&rhs[i], vm, globals)? {
            return Ok(Value::bool(false));
        }
    }
    Ok(Value::bool(true))
}

///
/// ### Array#==
///
/// - self == other -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/=3d=3d.html]
#[monoruby_builtin]
fn eq(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let lhs = lfp.self_val().as_array();
    let rhs = if let Some(rhs) = lfp.arg(0).try_array_ty() {
        rhs
    } else {
        return Ok(Value::bool(false));
    };
    if lhs.len() != rhs.len() {
        return Ok(Value::bool(false));
    }
    for i in 0..lhs.len() {
        if vm.ne_values_bool(globals, lhs[i], rhs[i])? {
            return Ok(Value::bool(false));
        }
    }
    Ok(Value::bool(true))
}

///
/// ### Array#<=>
///
/// - self <=> other -> -1 | 0 | 1 | nil
///
/// [https://docs.ruby-lang.org/ja/3.2/method/Array/i/=3c=3d=3e.html]
#[monoruby_builtin]
fn cmp(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let lhs = lfp.self_val().as_array();
    let rhs = if let Some(rhs) = lfp.arg(0).try_array_ty() {
        rhs
    } else {
        return Ok(Value::nil());
    };
    for (i, lhs) in lhs.iter().enumerate() {
        if let Some(rhs) = rhs.get(i) {
            let res = vm.compare_values(globals, *lhs, *rhs)?;
            if res != Ordering::Equal {
                return Ok(Value::integer(res as i64));
            }
        } else {
            return Ok(Value::integer(Ordering::Greater as i64));
        }
    }
    if rhs.len() > lhs.len() {
        return Ok(Value::integer(Ordering::Less as i64));
    }
    Ok(Value::integer(Ordering::Equal as i64))
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
fn index(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let ary = lfp.self_val().as_array();
    if lfp.try_arg(1).is_none() {
        let idx = lfp.arg(0);
        ary.get_elem1(globals, idx)
    } else {
        ary.get_elem2(globals, lfp.arg(0), lfp.arg(1))
    }
}

///
/// ### Array#[]=
///
/// - self[nth] = val
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/=5b=5d=3d.html]
#[monoruby_builtin]
fn index_assign(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let mut ary = lfp.self_val().as_array();
    if lfp.try_arg(2).is_none() {
        let i = lfp.arg(0);
        let val = lfp.arg(1);
        if let Some(idx) = i.try_fixnum() {
            ary.set_index(idx, val)
        } else if let Some(range) = i.is_range() {
            let start = ary.get_array_index_checked(globals, range.start())?;
            let end = ary.get_array_index_checked(globals, range.end())?;
            let len = if range.exclude_end() {
                end.checked_sub(start)
            } else {
                (end + 1).checked_sub(start)
            };
            if let Some(len) = len {
                ary.set_index2(start as usize, len as usize, val)
            } else if let Some(a) = val.try_array_ty() {
                ary.insert_many(start, a.iter().cloned());
                Ok(val)
            } else {
                ary.insert(start, val);
                Ok(val)
            }
        } else {
            Err(MonorubyErr::runtimeerr(format!(
                "index {:?} is not supported yet.",
                i.inspect(&globals.store)
            )))
        }
    } else {
        let i = lfp.arg(0).coerce_to_i64(globals)?;
        let l = lfp.arg(1).coerce_to_i64(globals)?;
        if l < 0 {
            return Err(MonorubyErr::indexerr(format!("negative length ({})", l)));
        }
        if i < 0 {
            return Err(MonorubyErr::index_too_small(i, 0));
        }
        let val = lfp.arg(2);
        ary.set_index2(i as usize, l as usize, val)
    }
}

///
/// ### Array#clear
///
/// - clear -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/fill.html]
#[monoruby_builtin]
fn clear(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    lfp.expect_no_block()?;
    let mut ary = lfp.self_val().as_array();
    ary.clear();
    Ok(ary.into())
}

///
/// ### Array#fill
///
/// - fill(val) -> self
/// - [NOT SUPPORTED] fill {|index| ... } -> self
/// - [NOT SUPPORTED] fill(val, start, length = nil) -> self
/// - [NOT SUPPORTED] fill(val, range) -> self
/// - [NOT SUPPORTED] fill(start, length = nil) {|index| ... } -> self
/// - [NOT SUPPORTED] fill(range) {|index| ... } -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/fill.html]
#[monoruby_builtin]
fn fill(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    lfp.expect_no_block()?;
    let mut ary = lfp.self_val().as_array();
    ary.fill(lfp.arg(0));
    Ok(ary.into())
}

///
/// ### Array#drop
///
/// - drop(n) -> Array
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/drop.html]
#[monoruby_builtin]
fn drop(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let ary = lfp.self_val().as_array();
    let num = lfp.arg(0).coerce_to_i64(globals)?;
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
fn zip(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let self_ary = lfp.self_val().as_array();
    let mut args_ary = vec![];
    for a in lfp.arg(0).as_array().iter() {
        args_ary.push(a.expect_array_ty(globals)?.to_vec());
    }
    let mut ary = Array::new_empty();
    for (i, val) in self_ary.iter().enumerate() {
        let mut vec = Array::new_empty();
        vec.push(*val);
        for args in &args_ary {
            if i < args.len() {
                vec.push(args[i]);
            } else {
                vec.push(Value::nil());
            }
        }
        ary.push(vec.as_val());
    }
    match lfp.block() {
        None => Ok(ary.as_val()),
        Some(block) => {
            let size_hint = ary.len();
            vm.temp_push(ary.as_val());
            let res = vm.invoke_block_map1(globals, block, ary.iter().cloned(), size_hint);
            vm.temp_pop();
            res?;
            Ok(Value::nil())
        }
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
fn inject(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
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
/// TODO: support recursive join for Array class arguments.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/join.html]
#[monoruby_builtin]
fn join(_: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let arg0 = lfp.try_arg(0);
    let sep = if let Some(sep) = &arg0 {
        sep.expect_string(globals)?
    } else {
        "".to_string()
    };
    let ary = lfp.self_val().as_array();
    let res = array_join(&globals.store, ary, &sep);
    Ok(Value::string(res))
}

fn array_join(store: &Store, ary: Array, sep: &str) -> String {
    ary.iter()
        .map(|v| v.to_s(store))
        .collect::<Vec<_>>()
        .join(sep)
}

///
/// ### Array#first
///
/// - first -> object | nil
/// - first(n) -> Array
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/first.html]
#[monoruby_builtin]
fn first(_: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let ary = lfp.self_val().as_array();
    if lfp.try_arg(0).is_none() {
        Ok(ary.first().cloned().unwrap_or_default())
    } else {
        let n = lfp.arg(0).coerce_to_i64(globals)?;
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
fn last(_: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let ary = lfp.self_val().as_array();
    if lfp.try_arg(0).is_none() {
        Ok(ary.last().cloned().unwrap_or_default())
    } else {
        let n = lfp.arg(0).coerce_to_i64(globals)?;
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
/// ### Array#take
///
/// - take(n) -> Array
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/take.html]
#[monoruby_builtin]
fn take(_: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let ary = lfp.self_val().as_array();
    let n = lfp.arg(0).coerce_to_i64(globals)?;
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
fn sum(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
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
/// - [NOT SUPPORTED] min(n) -> Array
/// - [NOT SUPPORTED] min {|a, b| ... } -> object | nil
/// - [NOT SUPPORTED] min(n) {|a, b| ... } -> Array
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/min.html]
#[monoruby_builtin]
fn min(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    lfp.expect_no_block()?;
    let ary = lfp.self_val().as_array();
    if ary.len() == 0 {
        return Ok(Value::nil());
    }
    let mut min = ary[0];
    for v in &ary[1..] {
        if vm.compare_values(globals, min, *v)? == std::cmp::Ordering::Greater {
            min = *v;
        }
    }
    Ok(min)
}

///
/// ### Array#max
///
/// - max -> object | nil
/// - [NOT SUPPORTED] max(n) -> Array
/// - [NOT SUPPORTED] max {|a, b| ... } -> object | nil
/// - [NOT SUPPORTED] max(n) {|a, b| ... } -> Array
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/max.html]
#[monoruby_builtin]
fn max(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    lfp.expect_no_block()?;
    let ary = lfp.self_val().as_array();
    if ary.len() == 0 {
        return Ok(Value::nil());
    }
    let mut max = ary[0];
    for v in &ary[1..] {
        if vm.compare_values(globals, max, *v)? == std::cmp::Ordering::Less {
            max = *v;
        }
    }
    Ok(max)
}

///
/// ### Enumerable#partition
///
/// - [NOT SUPPORTED] partition -> Enumerator
/// - partition {|item| ... } -> [[object], [object]]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/max.html]
#[monoruby_builtin]
fn partition(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let bh = lfp.expect_block()?;
    let aref = lfp.self_val().expect_array_ty(globals)?;
    let mut res_true = vec![];
    let mut res_false = vec![];
    let p = vm.get_block_data(globals, bh)?;
    for elem in aref.iter().cloned() {
        if vm.invoke_block(globals, &p, &[elem])?.as_bool() {
            res_true.push(elem);
        } else {
            res_false.push(elem);
        };
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
            let res = vm
                .invoke_block(globals, &data, &[lhs, rhs])?
                .expect_integer(globals)?;
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
fn sort_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
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
fn sort(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let ary = lfp.self_val().dup().as_array();
    let gc_enabled = Globals::gc_enable(false);
    let res = sort_inner(vm, globals, lfp, ary);
    Globals::gc_enable(gc_enabled);
    res
}

///
/// ### Array#sort_by!
///
/// - sort_by! {|item| ... } -> self
/// - [NOT SUPPORTED] sort_by! -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/sort_by=21.html]
#[monoruby_builtin]
fn sort_by_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let bh = lfp.expect_block()?;
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
}

///
/// ### Enumerable#sort_by
///
/// - sort_by {|item| ... } -> [object]
/// - [NOT SUPPORTED] sort_by -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/Enumerable/i/sort_by.html]
#[monoruby_builtin]
fn sort_by(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    if let Some(bh) = lfp.block() {
        let data = vm.get_block_data(globals, bh)?;
        let f = |lhs: Value, rhs: Value| -> Result<std::cmp::Ordering> {
            let lhs = vm.invoke_block(globals, &data, &[lhs])?;
            let rhs = vm.invoke_block(globals, &data, &[rhs])?;
            Executor::compare_values(vm, globals, lhs, rhs)
        };
        let mut ary = lfp.self_val().dup().as_array();
        let gc_enabled = Globals::gc_enable(false);
        let res = executor::op::sort_by(&mut ary, f);
        Globals::gc_enable(gc_enabled);
        res?;
        Ok(ary.into())
    } else {
        vm.generate_enumerator(IdentId::get_id("sort_by"), lfp.self_val(), vec![])
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
fn filter(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let ary = lfp.self_val().as_array();
    if let Some(bh) = lfp.block() {
        let data = vm.get_block_data(globals, bh)?;
        let mut res = vec![];
        for elem in ary.iter() {
            if vm.invoke_block(globals, &data, &[*elem])?.as_bool() {
                res.push(*elem);
            };
        }
        Ok(Value::array_from_vec(res))
    } else {
        vm.generate_enumerator(IdentId::get_id("filter"), lfp.self_val(), vec![])
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
fn filter_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let mut ary = lfp.self_val().as_array();
    if let Some(bh) = lfp.block() {
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
        vm.generate_enumerator(IdentId::get_id("filter!"), lfp.self_val(), vec![])
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
fn reject(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let ary = lfp.self_val().as_array();
    if let Some(bh) = lfp.block() {
        let data = vm.get_block_data(globals, bh)?;
        let mut res = vec![];
        for elem in ary.iter() {
            if !vm.invoke_block(globals, &data, &[*elem])?.as_bool() {
                res.push(*elem);
            };
        }
        Ok(Value::array_from_vec(res))
    } else {
        vm.generate_enumerator(IdentId::get_id("reject"), lfp.self_val(), vec![])
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
fn reject_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let mut ary = lfp.self_val().as_array();
    if let Some(bh) = lfp.block() {
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
        vm.generate_enumerator(IdentId::get_id("reject!"), lfp.self_val(), vec![])
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
fn delete_if(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let mut ary = lfp.self_val().as_array();
    if let Some(bh) = lfp.block() {
        let data = vm.get_block_data(globals, bh)?;
        ary.retain(|v| {
            vm.invoke_block(globals, &data, &[*v])
                .map(|res| !res.as_bool())
        })?;
        Ok(lfp.self_val())
    } else {
        vm.generate_enumerator(IdentId::get_id("delete_if"), lfp.self_val(), vec![])
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
fn group_by(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    fn inner(
        vm: &mut Executor,
        globals: &mut Globals,
        data: &ProcData,
        ary: Array,
        map: &mut RubyMap<Value, Value>,
    ) -> Result<()> {
        for elem in ary.iter() {
            let key = vm.invoke_block(globals, &data, &[*elem])?;
            map.entry(key, vm, globals)?
                .and_modify(|v: &mut Value| v.as_array().push(*elem))
                .or_insert(Value::array1(*elem));
        }
        Ok(())
    }
    let ary = lfp.self_val().as_array();
    if let Some(bh) = lfp.block() {
        let data = vm.get_block_data(globals, bh)?;
        let mut map = RubyMap::default();
        let gc_enabled = Globals::gc_enable(false);
        let res = inner(vm, globals, &data, ary, &mut map);
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
        vm.generate_enumerator(IdentId::get_id("group_by"), lfp.self_val(), vec![])
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
fn each(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
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
fn each_with_index(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
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
fn map(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
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
fn map_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
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
fn flat_map(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
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
    let ary = lfp.self_val().as_array();
    if let Some(bh) = lfp.block() {
        let data = vm.get_block_data(globals, bh)?;
        for elem in ary.iter() {
            if vm.invoke_block(globals, &data, &[*elem])?.as_bool() != is_all {
                return Ok(Value::bool(!is_all));
            };
        }
    } else {
        for elem in ary.iter() {
            if elem.as_bool() != is_all {
                return Ok(Value::bool(!is_all));
            };
        }
    }
    Ok(Value::bool(is_all))
}

///
/// #### Array#all?
///
/// - all? -> bool
/// - all? {|item| ... } -> bool
/// - [NOT SUPPORTED] all?(pattern) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/all=3f.html]
#[monoruby_builtin]
fn all_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    all_any_inner(vm, globals, lfp, true)
}

///
/// #### Array#any?
///
/// - any? -> bool
/// - any? {|item| ... } -> bool
/// - [NOT SUPPORTED] any?(pattern) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/any=3f.html]
#[monoruby_builtin]
fn any_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
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
fn detect(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let ary = lfp.self_val().as_array();
    let bh = lfp.expect_block()?;
    let data = vm.get_block_data(globals, bh)?;
    for elem in ary.iter() {
        if vm.invoke_block(globals, &data, &[*elem])?.as_bool() {
            return Ok(*elem);
        };
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
fn grep(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let ary = lfp.self_val().as_array();
    let ary: Vec<_> = match lfp.block() {
        None => {
            let mut res = vec![];
            for v in ary.iter() {
                if cmp_teq_values_bool(vm, globals, lfp.arg(0), *v)? {
                    res.push(*v)
                }
            }
            res
        }
        _ => unimplemented!(),
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
fn include_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let ary = lfp.self_val().as_array();
    let rhs = lfp.arg(0);
    for lhs in ary.iter().cloned() {
        if vm.eq_values_bool(globals, lhs, rhs)? {
            return Ok(Value::bool(true));
        };
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
fn reverse(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
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
fn reverse_(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
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
fn transpose(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let ary = lfp.self_val().as_array();
    if ary.len() == 0 {
        return Ok(Value::array_empty());
    }
    let len = ary[0]
        .try_array_ty()
        .ok_or_else(|| MonorubyErr::argumenterr("Each element of receiver must be an array."))?
        .len();
    let mut trans = Array::new_empty();
    for i in 0..len {
        let mut temp = Array::new_empty();
        for v in ary.iter() {
            let a = v.try_array_ty().ok_or_else(|| {
                MonorubyErr::argumenterr("Each element of receiver must be an array.")
            })?;
            if a.len() != len {
                return Err(MonorubyErr::indexerr("Element size differs."));
            }
            temp.push(a[i]);
        }
        trans.push(temp.into());
    }
    Ok(trans.into())
}

///
/// ### Array#rotate!
///
/// - rotate!(cnt = 1) -> Array
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/rotate=21.html]
#[monoruby_builtin]
fn rotate_(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let i = if let Some(arg0) = lfp.try_arg(0) {
        arg0.coerce_to_i64(globals)?
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
fn rotate(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let i = if let Some(arg0) = lfp.try_arg(0) {
        arg0.coerce_to_i64(globals)?
    } else {
        1
    };
    let mut ary: Array = lfp.self_val().dup().as_array();
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
fn product(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let lhs = lfp.self_val().as_array();
    let mut lists = vec![];
    for rhs in lfp.arg(0).as_array().into_iter() {
        lists.push(rhs.expect_array_ty(globals)?);
    }
    let v: Vec<Value> = product_inner(lhs, lists)
        .into_iter()
        .map(|a| Value::array_from_iter(a.into_iter().cloned()))
        .collect();
    if let Some(bh) = lfp.block() {
        let len = v.len();
        let ary = Array::new_from_vec(v);
        vm.temp_push(ary.into());
        let res = vm.invoke_block_map1(globals, bh, ary.into_iter().cloned(), len);
        vm.temp_pop();
        res
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
fn union(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let mut ary = lfp.self_val().dup().as_array();
    for rhs in lfp.arg(0).as_array().into_iter() {
        let rhs = rhs.expect_array_ty(globals)?;
        ary.extend(rhs.into_iter().cloned());
    }
    ary.uniq(vm, globals)?;
    Ok(ary.into())
}

///
/// ### Array#intersect?
///
/// - intersect?(other) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/intersect=3f.html]
#[monoruby_builtin]
fn intersect_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let lhs = lfp.self_val().as_array();
    for rhs in lfp.arg(0).as_array().iter().cloned() {
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
fn uniq(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let mut ary = lfp.self_val().dup().as_array();
    match lfp.block() {
        None => ary.uniq(vm, globals)?,
        Some(bh) => uniq_block(vm, globals, ary, bh)?,
    };
    Ok(ary.into())
}

#[monoruby_builtin]
fn uniq_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
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
fn slice_(_: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let ary = lfp.self_val().as_array();
    if let Some(arg1) = lfp.try_arg(1) {
        let start = match ary.get_array_index(lfp.arg(0).coerce_to_i64(globals)?) {
            Some(i) => i,
            None => return Ok(Value::nil()),
        };
        let len = arg1.coerce_to_i64(globals)?;
        if len < 0 {
            return Ok(Value::nil());
        };
        let len = len as usize;
        Ok(slice_inner(ary, start, len))
    } else if let Some(range) = lfp.arg(0).is_range() {
        let start = match ary.get_array_index(range.start().coerce_to_i64(globals)?) {
            Some(i) => i,
            None => return Ok(Value::nil()),
        };
        let end = match ary.get_array_index(range.end().coerce_to_i64(globals)?) {
            Some(i) => i,
            None => return Ok(Value::array_empty()),
        };
        if end < start {
            return Ok(Value::array_empty());
        }
        let len = end - start + if range.exclude_end() { 0 } else { 1 };
        Ok(slice_inner(ary, start, len))
    } else {
        unimplemented!()
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
fn pack(_: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let ary = lfp.self_val().as_array();
    rvalue::pack(globals, &ary, lfp.arg(0).expect_str(globals)?)
}

fn flatten_inner(ary: &Array, res: &mut Vec<Value>, lv: Option<usize>, changed: &mut bool) {
    for v in ary.iter() {
        if let Some(ary) = v.try_array_ty() {
            if let Some(lv) = lv {
                if lv == 0 {
                    res.push(*v);
                } else {
                    *changed = true;
                    flatten_inner(&ary, res, Some(lv - 1), changed);
                }
            } else {
                *changed = true;
                flatten_inner(&ary, res, None, changed);
            }
        } else {
            res.push(*v);
        }
    }
}

///
/// ### Array#flatten
///
/// - flatten(lv = nil) -> Array
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/flatten.html]
#[monoruby_builtin]
fn flatten(_: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let ary = lfp.self_val().as_array();
    let lv = if let Some(arg0) = lfp.try_arg(0) {
        if arg0.is_nil() {
            None
        } else {
            match arg0.expect_integer(globals)? {
                i if i >= 0 => Some(i as usize),
                _ => None,
            }
        }
    } else {
        None
    };
    let mut res = vec![];
    let mut changed = false;
    flatten_inner(&ary, &mut res, lv, &mut changed);
    Ok(Value::array_from_vec(res))
}

///
/// ### Array#flatten!
///
/// - flatten!(lv = nil) -> self | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/flatten.html]
#[monoruby_builtin]
fn flatten_(_: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let mut ary = lfp.self_val().as_array();
    let lv = if let Some(arg0) = lfp.try_arg(0) {
        if arg0.is_nil() {
            None
        } else {
            match arg0.expect_integer(globals)? {
                i if i >= 0 => Some(i as usize),
                _ => None,
            }
        }
    } else {
        None
    };
    let mut res = vec![];
    let mut changed = false;
    flatten_inner(&ary, &mut res, lv, &mut changed);
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
fn compact(_: &mut Executor, _: &mut Globals, lfp: Lfp) -> Result<Value> {
    let mut ary = lfp.self_val().dup().as_array();
    ary.retain(|v| Ok(!v.is_nil()))?;
    Ok(ary.into())
}

#[monoruby_builtin]
fn compact_(_: &mut Executor, _: &mut Globals, lfp: Lfp) -> Result<Value> {
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
fn shuffle_(_: &mut Executor, _: &mut Globals, lfp: Lfp) -> Result<Value> {
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
fn delete(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
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
fn delete_at(_: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let mut ary = lfp.self_val().as_array();
    let pos = lfp.arg(0).coerce_to_i64(globals)?;
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
fn find_index(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let ary = lfp.self_val().as_array();
    if let Some(bh) = lfp.block() {
        let data = vm.get_block_data(globals, bh)?;
        for (i, v) in ary.iter().enumerate() {
            if vm.invoke_block(globals, &data, &[*v])?.as_bool() {
                return Ok(Value::integer(i as i64));
            }
        }
        Ok(Value::nil())
    } else if let Some(arg0) = lfp.try_arg(0) {
        let func_id = vm.find_method(globals, arg0, IdentId::_EQ, false)?;
        for (i, v) in ary.iter().enumerate() {
            if vm
                .invoke_func_inner(globals, func_id, arg0, &[*v], None, None)?
                .as_bool()
            {
                return Ok(Value::integer(i as i64));
            }
        }
        Ok(Value::nil())
    } else {
        let id = IdentId::get_id("find_index");
        vm.generate_enumerator(id, lfp.self_val(), vec![])
    }
}

///
/// ### Array#insert
///
/// - insert(nth, *val) -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/insert.html]
#[monoruby_builtin]
fn insert(_: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let val = lfp.arg(1).as_array();
    if val.len() == 0 {
        return Ok(lfp.self_val());
    }
    let mut ary = lfp.self_val().as_array();
    let nth = lfp.arg(0).expect_integer(globals)?;
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
    fn eql() {
        run_test(r##"["a", "b", "c"].eql? ["a", "b", "c"]"##);
        run_test(r##"["a", "b", "c"].eql? ["a", "c", "b"]"##);
        run_test(r##"["a", "b", 1].eql? ["a", "b", 1.0]"##);
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
        run_test_error(
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
    fn fill() {
        run_test(
            r##"
            a = [2, 3, 4, 5]
            a.fill(100)
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

        run_test(r#"[5,  6, 7].any? {|v| v > 0 }"#);
        run_test(r#"[5, -1, 7].any? {|v| v > 0 }"#);
        run_test(r#"[5, -1, 7].any?"#);
        run_test(r#"[5, nil, 7].any?"#);
        run_test(r#"[5, -1, false].any?"#);
        run_test(r#"[nil, false].any?"#);
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
    }

    #[test]
    fn pack() {
        run_test(r#"[*(0..100)].pack("C*")"#);
        run_test(r#"[0x12345678].pack("I")"#);
        run_test(r#"[0x12345678].pack("N")"#);
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
}
