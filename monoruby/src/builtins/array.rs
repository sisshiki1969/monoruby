use super::*;
use crate::builtins::slot::Guarded;
use smallvec::smallvec;
use std::cmp::Ordering;

//
// Array class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_under_obj("Array", ARRAY_CLASS);
    globals.define_builtin_class_inline_func_with(
        ARRAY_CLASS,
        "new",
        new,
        Box::new(super::class::gen_class_new(allocate_array)),
        analysis::v_v_vv,
        0,
        0,
        true,
    );
    globals.define_builtin_func(ARRAY_CLASS, "allocate", allocate, 0);
    globals.define_builtin_func_with(ARRAY_CLASS, "initialize", initialize, 0, 2, false);
    globals.define_builtin_func(ARRAY_CLASS, "size", size, 0);
    globals.define_builtin_func(ARRAY_CLASS, "length", size, 0);
    globals.define_builtin_func_with(ARRAY_CLASS, "count", count, 0, 1, false);
    globals.define_builtin_func(ARRAY_CLASS, "empty?", empty, 0);
    globals.define_builtin_func(ARRAY_CLASS, "to_a", to_a, 0);
    globals.define_builtin_func(ARRAY_CLASS, "+", add, 1);
    globals.define_builtin_func(ARRAY_CLASS, "-", sub, 1);
    globals.define_builtin_func(ARRAY_CLASS, "*", mul, 1);
    globals.define_builtin_func_with(ARRAY_CLASS, "shift", shift, 0, 1, false);
    globals.define_builtin_func_rest(ARRAY_CLASS, "unshift", unshift);
    globals.define_builtin_func_rest(ARRAY_CLASS, "prepend", unshift);
    globals.define_builtin_func_rest(ARRAY_CLASS, "concat", concat);
    globals.define_builtin_inline_func(
        ARRAY_CLASS,
        "<<",
        shl,
        Box::new(array_shl),
        analysis::v_v_v,
        1,
    );
    globals.define_builtin_func(ARRAY_CLASS, "==", eq, 1);
    globals.define_builtin_func(ARRAY_CLASS, "<=>", cmp, 1);
    globals.define_builtin_func_with(ARRAY_CLASS, "[]", index, 1, 2, false);
    globals.define_builtin_func_with(ARRAY_CLASS, "[]=", index_assign, 2, 3, false);
    globals.define_builtin_func(ARRAY_CLASS, "clear", clear, 0);
    globals.define_builtin_func(ARRAY_CLASS, "fill", fill, 1);
    globals.define_builtin_func(ARRAY_CLASS, "drop", drop, 1);
    globals.define_builtin_func_rest(ARRAY_CLASS, "zip", zip);
    globals.define_builtin_func_with(ARRAY_CLASS, "inject", inject, 0, 1, false);
    globals.define_builtin_func_with(ARRAY_CLASS, "reduce", inject, 0, 1, false);
    globals.define_builtin_func_with(ARRAY_CLASS, "join", join, 0, 1, false);
    globals.define_builtin_func_with(ARRAY_CLASS, "first", first, 0, 1, false);
    globals.define_builtin_func_with(ARRAY_CLASS, "last", last, 0, 1, false);
    globals.define_builtin_func_with(ARRAY_CLASS, "sum", sum, 0, 1, false);
    globals.define_builtin_func(ARRAY_CLASS, "min", min, 0);
    globals.define_builtin_func(ARRAY_CLASS, "max", max, 0);
    globals.define_builtin_func(ARRAY_CLASS, "sort", sort, 0);
    globals.define_builtin_func(ARRAY_CLASS, "sort!", sort_, 0);
    globals.define_builtin_func(ARRAY_CLASS, "sort_by!", sort_by_, 0);
    globals.define_builtin_func(ARRAY_CLASS, "each", each, 0);
    globals.define_builtin_func(ARRAY_CLASS, "each_with_index", each_with_index, 0);
    globals.define_builtin_func(ARRAY_CLASS, "map", map, 0);
    globals.define_builtin_func(ARRAY_CLASS, "flat_map", flat_map, 0);
    globals.define_builtin_func(ARRAY_CLASS, "collect_concat", flat_map, 0);
    globals.define_builtin_func(ARRAY_CLASS, "all?", all_, 0);
    globals.define_builtin_func(ARRAY_CLASS, "detect", detect, 0);
    globals.define_builtin_func(ARRAY_CLASS, "find", detect, 0);
    globals.define_builtin_func(ARRAY_CLASS, "grep", grep, 1);
    globals.define_builtin_func(ARRAY_CLASS, "include?", include_, 1);
    globals.define_builtin_func(ARRAY_CLASS, "reverse", reverse, 0);
    globals.define_builtin_func(ARRAY_CLASS, "reverse!", reverse_, 0);
    globals.define_builtin_func(ARRAY_CLASS, "transpose", transpose, 0);
    globals.define_builtin_func_with(ARRAY_CLASS, "rotate!", rotate_, 0, 1, false);
    globals.define_builtin_func_with(ARRAY_CLASS, "rotate", rotate, 0, 1, false);
    globals.define_builtin_func(ARRAY_CLASS, "uniq", uniq, 0);
    globals.define_builtin_func(ARRAY_CLASS, "uniq!", uniq_, 0);
    globals.define_builtin_func_with(ARRAY_CLASS, "slice!", slice_, 1, 2, false);
    globals.define_builtin_func_with(ARRAY_CLASS, "pack", pack, 0, 1, false);
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
    let obj = Value::array_with_class(vec![], class);
    vm.invoke_method_if_exists(
        globals,
        IdentId::INITIALIZE,
        obj,
        &lfp.arg(0).as_array(),
        lfp.block(),
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
    let obj = Value::array_with_class(vec![], class);
    Ok(obj)
}

extern "C" fn allocate_array(class_val: Value) -> Value {
    let class_id = class_val.as_class_id();
    Value::array_with_class(vec![], class_id)
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
    let mut self_val = Array::new(lfp.self_val());
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
            *self_val = vm
                .invoke_block_map1(globals, bh, iter, size)?
                .as_array()
                .clone();
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
            if vm
                .invoke_method_inner(globals, IdentId::_EQ, arg0, &[*elem], None)?
                .as_bool()
            {
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
/// ### Array#+
///
/// - self + other -> Array
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/=2b.html]
#[monoruby_builtin]
fn add(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let mut lhs = Array::dup(lfp.self_val().as_array());
    let rhs = match lfp.arg(0).try_array_ty() {
        Some(v) => v,
        None => {
            return Err(MonorubyErr::no_implicit_conversion(lfp.arg(0), ARRAY_CLASS));
        }
    };
    lhs.extend_from_slice(&*rhs);
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
        None => return Err(MonorubyErr::no_implicit_conversion(lfp.arg(0), ARRAY_CLASS)),
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
    let lhs = Array::new(lfp.self_val());
    if let Some(v) = lfp.arg(0).try_fixnum() {
        if v < 0 {
            return Err(MonorubyErr::negative_argument());
        }
        let rhs = v as usize;
        let vec = lhs.repeat(rhs);
        Ok(Value::array_from_vec(vec))
    } else if let Some(sep) = lfp.arg(0).is_str() {
        let res = array_join(globals, lhs, &sep);
        Ok(Value::string(res))
    } else {
        Err(MonorubyErr::no_implicit_conversion(
            lfp.arg(0),
            INTEGER_CLASS,
        ))
    }
}

///
/// ### Array#shift
/// - shift -> object | nil
/// - shift(n) -> Array
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/shift.html]
#[monoruby_builtin]
fn shift(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let mut ary = Array::new(lfp.self_val());
    if lfp.try_arg(0).is_none() {
        if ary.len() == 0 {
            return Ok(Value::nil());
        }
        let res = ary[0];
        ary.drain(0..1);
        Ok(res)
    } else {
        let i = lfp.arg(0).coerce_to_i64()?;
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
    let mut ary = Array::new(lfp.self_val());
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
fn concat(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let mut self_ary = Array::new(lfp.self_val());
    let mut ary: Array = Array::new_empty();
    for a in lfp.arg(0).as_array().iter().cloned() {
        if let Some(a) = a.try_array_ty() {
            ary.extend_from_slice(&a);
        } else {
            return Err(MonorubyErr::no_implicit_conversion(a, ARRAY_CLASS));
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
    let mut ary = Array::new(lfp.self_val());
    ary.push(lfp.arg(0));
    Ok(ary.into())
}

extern "C" fn ary_shl(mut ary: Array, arg: Value) -> Value {
    ary.push(arg);
    ary.into()
}

fn array_shl(
    ir: &mut AsmIr,
    _store: &Store,
    bb: &mut BBContext,
    callsite: &CallSiteInfo,
    pc: BcPc,
) {
    let CallSiteInfo {
        recv, dst, args, ..
    } = *callsite;
    ir.fetch_to_reg(bb, args, GP::Rsi);
    let deopt = ir.new_deopt(bb, pc);
    let using_xmm = bb.get_using_xmm();
    ir.guard_class(bb, recv, GP::Rdi, ARRAY_CLASS, deopt);
    ir.inline(move |gen, _| {
        gen.xmm_save(using_xmm);
        monoasm!( &mut gen.jit,
            movq rax, (ary_shl);
            call rax;
        );
        gen.xmm_restore(using_xmm);
    });
    ir.reg2acc_guarded(bb, GP::Rax, dst, Guarded::ArrayTy);
}

///
/// ### Array#==
///
/// - self == other -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/=3d=3d.html]
#[monoruby_builtin]
fn eq(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let lhs = Array::new(lfp.self_val());
    let rhs = Array::new(lfp.arg(0));
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
    let lhs = Array::new(lfp.self_val());
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
fn index(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let ary = Array::new(lfp.self_val());
    if lfp.try_arg(1).is_none() {
        let idx = lfp.arg(0);
        ary.get_elem1(idx)
    } else {
        ary.get_elem2(lfp.arg(0), lfp.arg(1))
    }
}

///
/// ### Array#[]=
///
/// - self[nth] = val
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/=5b=5d=3d.html]
#[monoruby_builtin]
fn index_assign(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let mut ary = Array::new(lfp.self_val());
    if lfp.try_arg(2).is_none() {
        let i = lfp.arg(0);
        let val = lfp.arg(1);
        if let Some(idx) = i.try_fixnum() {
            ary.set_index(idx, val)
        } else {
            unimplemented!()
        }
    } else {
        let i = lfp.arg(0).coerce_to_i64()?;
        let l = lfp.arg(1).coerce_to_i64()?;
        if l < 0 {
            return Err(MonorubyErr::indexerr(format!("negative length ({})", l)));
        }
        if i < 0 {
            return Err(MonorubyErr::index_too_small(i, 0));
        }
        let val = lfp.arg(2);
        return ary.set_index2(i as usize, l as usize, val);
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
    let mut ary = Array::new(lfp.self_val());
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
    let mut ary = Array::new(lfp.self_val());
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
fn drop(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let ary = Array::new(lfp.self_val());
    let num = lfp.arg(0).coerce_to_i64()?;
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
    let self_ary = Array::new(lfp.self_val());
    let mut args_ary = vec![];
    for a in lfp.arg(0).as_array().iter() {
        args_ary.push(a.expect_array()?.to_vec());
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
/// - reduce(init = self.first) {|result, item| ... } -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Enumerable/i/inject.html]
#[monoruby_builtin]
fn inject(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let bh = lfp.expect_block()?;
    let self_ = lfp.self_val();
    let mut iter = self_.as_array().iter().cloned();
    let res = if lfp.try_arg(0).is_none() {
        iter.next().unwrap_or_default()
    } else {
        lfp.arg(0)
    };
    vm.invoke_block_fold1(globals, bh, iter, res)
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
    let sep = if lfp.try_arg(0).is_none() {
        "".to_string()
    } else {
        lfp.arg(0).expect_string()?
    };
    let ary = Array::new(lfp.self_val());
    let res = array_join(globals, ary, &sep);
    Ok(Value::string(res))
}

fn array_join(globals: &Globals, ary: Array, sep: &str) -> String {
    ary.iter()
        .map(|v| globals.to_s(*v))
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
fn first(_: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let ary = Array::new(lfp.self_val());
    if lfp.try_arg(0).is_none() {
        Ok(ary.first().cloned().unwrap_or_default())
    } else {
        let n = lfp.arg(0).coerce_to_i64()?;
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
fn last(_: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let ary = Array::new(lfp.self_val());
    if lfp.try_arg(0).is_none() {
        Ok(ary.last().cloned().unwrap_or_default())
    } else {
        let n = lfp.arg(0).coerce_to_i64()?;
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
    let self_ = lfp.self_val();
    let iter = self_.as_array().iter().cloned();
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
    let ary = Array::new(lfp.self_val());
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
    let ary = Array::new(lfp.self_val());
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
/// ### Array#sort!
///
/// - sort! -> self
/// - [NOT SUPPORTED] sort! {|a, b| ... } -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/sort.html]
#[monoruby_builtin]
fn sort_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    lfp.expect_no_block()?;
    let mut ary = Array::new(lfp.self_val());
    vm.sort_by(globals, &mut ary, Executor::compare_values)?;
    Ok(ary.into())
}

///
/// ### Array#sort
///
/// - sort -> Array
/// - [NOT SUPPORTED] sort {|a, b| ... } -> Array
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/sort.html]
#[monoruby_builtin]
fn sort(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    lfp.expect_no_block()?;
    let mut ary = Array::new(lfp.self_val().dup());
    vm.sort_by(globals, &mut ary, Executor::compare_values)?;
    Ok(ary.into())
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
    let f = |vm: &mut Executor,
             globals: &mut Globals,
             lhs: Value,
             rhs: Value|
     -> Result<std::cmp::Ordering> {
        let lhs = vm.invoke_block(globals, &data, &[lhs])?;
        let rhs = vm.invoke_block(globals, &data, &[rhs])?;
        let res = Executor::compare_values(vm, globals, lhs, rhs);
        res
    };
    let mut ary = Array::new(lfp.self_val());
    vm.sort_by(globals, &mut ary, f)?;
    Ok(ary.into())
}

///
/// ### Array#each
///
/// - each -> Enumerator
/// - each {|item| .... } -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/each.html]
#[monoruby_builtin]
fn each(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let ary = Array::new(lfp.self_val());
    if let Some(bh) = lfp.block() {
        vm.invoke_block_iter1(globals, bh, ary.iter().cloned())?;
        Ok(ary.into())
    } else {
        vm.generate_enumerator(IdentId::EACH, lfp.self_val(), vec![])
    }
}

///
/// ### Enumerable#each_with_index
///
/// - each_with_index([NOT SUPPORTED]*args) -> Enumerator
/// - each_with_index([NOT SUPPORTED]*args) {|item, index| ... } -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Enumerable/i/each_with_index.html]
#[monoruby_builtin]
fn each_with_index(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let ary = Array::new(lfp.self_val());
    if let Some(bh) = lfp.block() {
        vm.invoke_block_iter_with_index1(globals, bh, ary.iter().cloned())?;
        Ok(ary.into())
    } else {
        vm.generate_enumerator(IdentId::EACH, lfp.self_val(), vec![])
    }
}

/// ### Array#map
///
/// - map {|item| ... } -> [object]
/// - map -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/collect.html]
#[monoruby_builtin]
fn map(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let ary = Array::new(lfp.self_val());
    let size_hint = ary.len();
    let iter = ary.iter().cloned();
    if let Some(bh) = lfp.block() {
        vm.invoke_block_map1(globals, bh, iter, size_hint)
    } else {
        let id = IdentId::get_id("map");
        vm.generate_enumerator(id, lfp.self_val(), vec![])
    }
}

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
    let ary = Array::new(lfp.self_val());
    let size_hint = ary.len();
    vm.invoke_block_flat_map1(globals, bh, ary.iter().cloned(), size_hint)
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
    let ary = Array::new(lfp.self_val());
    if let Some(bh) = lfp.block() {
        let data = vm.get_block_data(globals, bh)?;
        for elem in ary.iter() {
            if !vm.invoke_block(globals, &data, &[*elem])?.as_bool() {
                return Ok(Value::bool(false));
            };
        }
    } else {
        for elem in ary.iter() {
            if !elem.as_bool() {
                return Ok(Value::bool(false));
            };
        }
    }
    Ok(Value::bool(true))
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
    let ary = Array::new(lfp.self_val());
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
    let ary = Array::new(lfp.self_val());
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
    let ary = Array::new(lfp.self_val());
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
    let ary = Array::new(lfp.self_val());
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
    let mut ary = Array::new(lfp.self_val());
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
    let ary = Array::new(lfp.self_val());
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
fn rotate_(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let i = if let Some(arg0) = lfp.try_arg(0) {
        arg0.coerce_to_i64()?
    } else {
        1
    };
    let mut ary = Array::new(lfp.self_val());
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
fn rotate(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let i = if let Some(arg0) = lfp.try_arg(0) {
        arg0.coerce_to_i64()?
    } else {
        1
    };
    let mut ary: Array = Array::dup(lfp.self_val().as_array());
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
/// ### Array#uniq
///
/// - uniq -> Array
/// - uniq! -> self | nil
/// - uniq {|item| ... } -> Array
/// - uniq! {|item| ... } -> self | nil
///
/// https://docs.ruby-lang.org/ja/latest/method/Array/i/uniq.html
#[monoruby_builtin]
fn uniq(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let ary = Array::dup(lfp.self_val().as_array());
    match lfp.block() {
        None => uniq_noblock(ary)?,
        Some(bh) => uniq_block(vm, globals, ary, bh)?,
    };
    Ok(ary.into())
}

#[monoruby_builtin]
fn uniq_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let ary = Array::new(lfp.self_val());
    let deleted = match lfp.block() {
        None => uniq_noblock(ary)?,
        Some(bh) => uniq_block(vm, globals, ary, bh)?,
    };
    if deleted {
        Ok(lfp.self_val())
    } else {
        Ok(Value::nil())
    }
}

fn uniq_noblock(mut ary: Array) -> Result<bool> {
    let mut h = HashSet::default();
    let mut recursive = false;
    let self_id = ary.id();
    ary.retain(|x| {
        if self_id == x.id() {
            if !recursive {
                recursive = true;
                Ok(true)
            } else {
                Ok(false)
            }
        } else {
            Ok(h.insert(HashKey(*x)))
        }
    })
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
    let mut h = HashSet::default();
    let data = vm.get_block_data(globals, bh)?;
    vm.temp_array_new(Some(ary.len()));
    let res = ary.retain(|x| {
        let res = vm.invoke_block(globals, &data, &[*x])?;
        vm.temp_array_push(res);
        Ok(h.insert(HashKey(res)))
    });
    vm.temp_pop();
    res
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
fn slice_(_: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let aref = Array::new(lfp.self_val());
    if let Some(arg1) = lfp.try_arg(1) {
        let start = match aref.get_array_index(lfp.arg(0).coerce_to_i64()?) {
            Some(i) => i,
            None => return Ok(Value::nil()),
        };
        let len = arg1.coerce_to_i64()?;
        if len < 0 {
            return Ok(Value::nil());
        };
        let len = len as usize;
        Ok(slice_inner(aref, start, len))
    } else if let Some(range) = lfp.arg(0).is_range() {
        let start = match aref.get_array_index(range.start.coerce_to_i64()?) {
            Some(i) => i,
            None => return Ok(Value::nil()),
        };
        let end = match aref.get_array_index(range.end.coerce_to_i64()?) {
            Some(i) => i,
            None => return Ok(Value::array_empty()),
        };
        if end < start {
            return Ok(Value::array_empty());
        }
        let len = end - start + if range.exclude_end() { 0 } else { 1 };
        Ok(slice_inner(aref, start, len))
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
/// - [NOT DESCRIBED] pack -> String
/// - pack(template) -> String
/// - [NOT SUPPORTED] pack(template, buffer: String.new) -> String
///
#[monoruby_builtin]
fn pack(_: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let ary = Array::new(lfp.self_val());
    if let Some(arg0) = lfp.try_arg(0)
        && arg0.expect_string()? != "C*"
    {
        unimplemented!()
    }
    let mut v = vec![];
    for elem in ary.iter() {
        let i = elem.coerce_to_i64()? as i8 as u8;
        v.push(i);
    }
    Ok(Value::bytes(v))
}

#[cfg(test)]
mod test {
    use super::tests::*;

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
        run_test_error("Array.new(-5)");
        run_test_error("Array.new(:r, 42)");
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
    fn shl() {
        run_test(r##"a = [1,2,3]; a << 10; a"##);
        run_test(r##"a = [1,2,3]; a.<<(10); a"##);
    }

    #[test]
    fn eq() {
        run_test(r##"["a","c"] == ["a","c",7]"##);
        run_test(r##"["a","c",7] == ["a","c",7]"##);
        run_test(r##"["a","c",7] == ["a","c","7"]"##);
        run_test(r##"[ 1, 2, 3 ] <=> [ 1, 3, 2 ] "##);
        run_test(r##"[ 1, 2, 3 ] <=> [ 1, 2, 3 ] "##);
        run_test(r##"[ 1, 2, 3 ] <=> [ 1, 2 ] "##);
        run_test(r##"[ 1, 2 ] <=> [ 1, 2, 3 ] "##);
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
    fn map() {
        run_test(
            r##"
        x = 10
        [2, 3, 4, 5, 6, 7, 8].map do |y|
          x + y
        end
        "##,
        );
    }

    #[test]
    fn all_() {
        run_test(r#"[5,  6, 7].all? {|v| v > 0 }"#);
        run_test(r#"[5, -1, 7].all? {|v| v > 0 }"#);
        run_test(r#"[5, -1, 7].all?"#);
        run_test(r#"[5, nil, 7].all?"#);
        run_test(r#"[5, -1, false].all?"#);
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
    }
}
