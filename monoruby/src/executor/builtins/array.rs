use smallvec::smallvec;

use crate::*;

//
// Array class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_under_obj("Array", ARRAY_CLASS);
    globals.define_builtin_class_func(ARRAY_CLASS, "new", new, -1);
    globals.define_builtin_func(ARRAY_CLASS, "initialize", initialize, -1);
    globals.define_builtin_func(ARRAY_CLASS, "size", size, 0);
    globals.define_builtin_func(ARRAY_CLASS, "length", size, 0);
    globals.define_builtin_func(ARRAY_CLASS, "empty?", empty, 0);
    globals.define_builtin_func(ARRAY_CLASS, "+", add, 1);
    globals.define_builtin_func(ARRAY_CLASS, "*", mul, 1);
    globals.define_builtin_func(ARRAY_CLASS, "shift", shift, -1);
    globals.define_builtin_func(ARRAY_CLASS, "unshift", unshift, -1);
    globals.define_builtin_func(ARRAY_CLASS, "prepend", unshift, -1);
    globals.define_builtin_func(ARRAY_CLASS, "<<", shl, 1);
    globals.define_builtin_func(ARRAY_CLASS, "[]", index, -1);
    globals.define_builtin_func(ARRAY_CLASS, "[]=", index_assign, -1);
    globals.define_builtin_func(ARRAY_CLASS, "clear", clear, 0);
    globals.define_builtin_func(ARRAY_CLASS, "fill", fill, 1);
    globals.define_builtin_func(ARRAY_CLASS, "inject", inject, -1);
    globals.define_builtin_func(ARRAY_CLASS, "reduce", inject, -1);
    globals.define_builtin_func(ARRAY_CLASS, "join", join, -1);
    globals.define_builtin_func(ARRAY_CLASS, "sum", sum, -1);
    globals.define_builtin_func(ARRAY_CLASS, "min", min, 0);
    globals.define_builtin_func(ARRAY_CLASS, "max", max, 0);
    globals.define_builtin_func(ARRAY_CLASS, "sort!", sort_, 0);
    globals.define_builtin_func(ARRAY_CLASS, "each", each, 0);
    globals.define_builtin_func(ARRAY_CLASS, "map", map, 0);
    globals.define_builtin_func(ARRAY_CLASS, "flat_map", flat_map, 0);
    globals.define_builtin_func(ARRAY_CLASS, "collect_concat", flat_map, 0);
    globals.define_builtin_func(ARRAY_CLASS, "detect", detect, 0);
    globals.define_builtin_func(ARRAY_CLASS, "find", detect, 0);
    globals.define_builtin_func(ARRAY_CLASS, "include?", include_, 1);
    globals.define_builtin_func(ARRAY_CLASS, "reverse", reverse, 0);
    globals.define_builtin_func(ARRAY_CLASS, "reverse!", reverse_, 0);
    globals.define_builtin_func(ARRAY_CLASS, "transpose", transpose, 0);
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
fn new(vm: &mut Executor, globals: &mut Globals, lfp: LFP, arg: Arg, len: usize) -> Result<Value> {
    let class = lfp.self_val().as_class_id();
    let obj = Value::array_with_class(vec![], class);
    vm.invoke_method_if_exists(globals, IdentId::INITIALIZE, obj, arg, len, lfp.block())?;
    Ok(obj)
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
fn initialize(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: LFP,
    arg: Arg,
    len: usize,
) -> Result<Value> {
    MonorubyErr::check_number_of_arguments_range(len, 0..=2)?;
    let mut self_val = lfp.self_val();
    if len == 0 {
        return Ok(self_val);
    }
    if len == 1 {
        if let Some(ary) = arg[0].is_array() {
            *self_val.as_array_mut() = ary.clone();
            return Ok(self_val);
        }
    }
    if let Some(size) = arg[0].try_fixnum() {
        if size < 0 {
            return Err(MonorubyErr::negative_array_size());
        }
        let size = size as usize;
        if lfp.block().is_some() {
            if len == 2 {
                eprintln!("warning: block supersedes default value argument");
            }
            let iter = (0..size).map(|i| Value::integer(i as i64)).into_iter();
            let vec = vm.invoke_block_map1(globals, iter)?;
            *self_val.as_array_mut() = ArrayInner::from_vec(vec);
        } else {
            let val = if len == 1 { Value::nil() } else { arg[1] };
            *self_val.as_array_mut() = ArrayInner::from(smallvec![val; size]);
        }
        Ok(self_val)
    } else {
        Err(MonorubyErr::no_implicit_conversion(
            globals,
            arg[0],
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
fn size(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: LFP,
    _arg: Arg,
    len: usize,
) -> Result<Value> {
    MonorubyErr::check_number_of_arguments(len, 0)?;
    let len = lfp.self_val().as_array().len();
    Ok(Value::integer(len as i64))
}

///
/// ### Array#empty?
///
/// - empty? -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/empty=3f.html]
#[monoruby_builtin]
fn empty(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: LFP,
    _arg: Arg,
    len: usize,
) -> Result<Value> {
    MonorubyErr::check_number_of_arguments(len, 0)?;
    let b = lfp.self_val().as_array().is_empty();
    Ok(Value::bool(b))
}

///
/// ### Array#+
///
/// - self + other -> Array
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/=2b.html]
#[monoruby_builtin]
fn add(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: LFP,
    arg: Arg,
    _len: usize,
) -> Result<Value> {
    let mut lhs = lfp.self_val().as_array().clone();
    let rhs = match arg[0].is_array() {
        Some(v) => v,
        None => {
            return Err(MonorubyErr::no_implicit_conversion(
                globals,
                arg[0],
                ARRAY_CLASS,
            ));
        }
    };
    lhs.extend_from_slice(rhs);
    Ok(Value::array(lhs))
}

///
/// ### Array#*
///
/// - self * times -> Array
/// - [NOT SUPPORTED]self * sep -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/=2a.html]
#[monoruby_builtin]
fn mul(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: LFP,
    arg: Arg,
    _len: usize,
) -> Result<Value> {
    let self_val = lfp.self_val();
    let lhs = self_val.as_array();
    let rhs = match arg[0].try_fixnum() {
        Some(v) => {
            if v < 0 {
                return Err(MonorubyErr::negative_argument());
            } else {
                v as usize
            }
        }
        None => {
            return Err(MonorubyErr::no_implicit_conversion(
                globals,
                arg[0],
                INTEGER_CLASS,
            ));
        }
    };
    let vec = lhs.repeat(rhs);
    Ok(Value::array_from_vec(vec))
}

///
/// ### Array#shift
/// - shift -> object | nil
/// - shift(n) -> Array
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/shift.html]
#[monoruby_builtin]
fn shift(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: LFP,
    arg: Arg,
    len: usize,
) -> Result<Value> {
    MonorubyErr::check_number_of_arguments_range(len, 0..=1)?;
    let mut self_val = lfp.self_val();
    let ary = self_val.as_array_mut();
    if len == 0 {
        if ary.len() == 0 {
            return Ok(Value::nil());
        }
        let res = ary[0];
        ary.drain(0..1);
        Ok(res)
    } else {
        let i = arg[0].coerce_to_i64(globals)?;
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
/// - unshift(*obj) -> self
/// - prepend(*obj) -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/prepend.html]
#[monoruby_builtin]
fn unshift(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: LFP,
    arg: Arg,
    len: usize,
) -> Result<Value> {
    let mut self_val = lfp.self_val();
    let ary = self_val.as_array_mut();
    let iter = arg.iter(len);
    ary.insert_many(0, iter);
    Ok(self_val)
}

///
/// ### Array#<<
///
/// - self << obj -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/=3c=3c.html]
#[monoruby_builtin]
fn shl(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: LFP,
    arg: Arg,
    _len: usize,
) -> Result<Value> {
    let mut self_val = lfp.self_val();
    self_val.as_array_mut().push(arg[0]);
    Ok(self_val)
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
fn index(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: LFP,
    arg: Arg,
    len: usize,
) -> Result<Value> {
    MonorubyErr::check_number_of_arguments_range(len, 1..=2)?;
    if len == 1 {
        let idx = arg[0];
        lfp.self_val().as_array().get_elem1(globals, idx)
    } else {
        lfp.self_val().as_array().get_elem2(globals, arg[0], arg[1])
    }
}

///
/// ### Array#[]=
///
/// - self[nth] = val
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/=5b=5d=3d.html]
#[monoruby_builtin]
fn index_assign(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: LFP,
    arg: Arg,
    len: usize,
) -> Result<Value> {
    MonorubyErr::check_number_of_arguments_range(len, 2..=3)?;
    if len == 2 {
        let i = arg[0];
        let val = arg[1];
        if let Some(idx) = i.try_fixnum() {
            return lfp.self_val().as_array_mut().set_index(idx, val);
        } else {
            unimplemented!()
        }
    } else if len == 3 {
        let i = arg[0].coerce_to_i64(globals)?;
        let l = arg[1].coerce_to_i64(globals)?;
        if l < 0 {
            return Err(MonorubyErr::indexerr(format!("negative length ({})", l)));
        }
        if i < 0 {
            return Err(MonorubyErr::index_too_small(i, 0));
        }
        let val = arg[2];
        return lfp
            .self_val()
            .as_array_mut()
            .set_index2(i as usize, l as usize, val);
    } else {
        unreachable!()
    }
}

///
/// ### Array#clear
///
/// - clear -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/fill.html]
#[monoruby_builtin]
fn clear(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: LFP,
    _arg: Arg,
    len: usize,
) -> Result<Value> {
    MonorubyErr::check_number_of_arguments(len, 0)?;
    lfp.expect_no_block()?;
    let mut self_val = lfp.self_val();
    self_val.as_array_mut().clear();
    Ok(self_val)
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
fn fill(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: LFP,
    arg: Arg,
    len: usize,
) -> Result<Value> {
    MonorubyErr::check_number_of_arguments(len, 1)?;
    lfp.expect_no_block()?;
    let mut self_val = lfp.self_val();
    self_val.as_array_mut().fill(arg[0]);
    Ok(self_val)
}

///
/// ### Array#inject
///
/// - inject(init = self.first) {|result, item| ... } -> object
/// - reduce(init = self.first) {|result, item| ... } -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Enumerable/i/inject.html]
#[monoruby_builtin]
fn inject(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: LFP,
    arg: Arg,
    len: usize,
) -> Result<Value> {
    MonorubyErr::check_number_of_arguments_range(len, 0..=1)?;
    lfp.expect_block()?;
    let self_ = lfp.self_val();
    let mut iter = self_.as_array().iter().cloned();
    let res = if len == 0 {
        iter.next().unwrap_or_default()
    } else {
        arg[0]
    };
    vm.invoke_block_fold1(globals, iter, res)
}

///
/// ### Array#join
///
/// - join(sep = $,) -> String
/// TODO: support recursive join for Array class arguments.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/join.html]
#[monoruby_builtin]
fn join(_: &mut Executor, globals: &mut Globals, lfp: LFP, arg: Arg, len: usize) -> Result<Value> {
    MonorubyErr::check_number_of_arguments_range(len, 0..=1)?;
    let sep = if len == 0 {
        "".to_string()
    } else {
        arg[0].expect_string(globals)?
    };
    let self_ = lfp.self_val();
    let mut res = String::new();
    array_join(globals, &mut res, self_.as_array(), &sep);
    Ok(Value::string(res))
}

fn array_join(globals: &Globals, res: &mut String, aref: &ArrayInner, sep: &str) {
    for elem in &**aref {
        let s = globals.to_s(*elem);
        if res.is_empty() {
            *res = s;
        } else {
            *res += sep;
            *res += &s;
        }
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
fn sum(vm: &mut Executor, globals: &mut Globals, lfp: LFP, arg: Arg, len: usize) -> Result<Value> {
    MonorubyErr::check_number_of_arguments_range(len, 0..=1)?;
    let mut sum = if len == 0 { Value::i32(0) } else { arg[0] };
    let self_ = lfp.self_val();
    let iter = self_.as_array().iter().cloned();
    match lfp.block() {
        None => {
            for v in iter {
                sum =
                    executor::op::add_values(vm, globals, sum, v).ok_or_else(|| vm.take_error())?;
            }
        }
        Some(_) => {
            let data = globals.get_block_data(vm.cfp());
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
fn min(vm: &mut Executor, globals: &mut Globals, lfp: LFP, _: Arg, len: usize) -> Result<Value> {
    MonorubyErr::check_number_of_arguments(len, 0)?;
    lfp.expect_no_block()?;
    let self_val = lfp.self_val();
    let ary = self_val.as_array();
    if ary.len() == 0 {
        return Ok(Value::nil());
    }
    let mut min = ary[0];
    for v in &ary[1..] {
        if vm.cmp_cmp_values_inner(globals, min, *v)?
            == Value::from_ord(std::cmp::Ordering::Greater)
        {
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
fn max(vm: &mut Executor, globals: &mut Globals, lfp: LFP, _: Arg, len: usize) -> Result<Value> {
    MonorubyErr::check_number_of_arguments(len, 0)?;
    lfp.expect_no_block()?;
    let self_val = lfp.self_val();
    let ary = self_val.as_array();
    if ary.len() == 0 {
        return Ok(Value::nil());
    }
    let mut max = ary[0];
    for v in &ary[1..] {
        if vm.cmp_cmp_values_inner(globals, max, *v)? == Value::from_ord(std::cmp::Ordering::Less) {
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
fn sort_(vm: &mut Executor, globals: &mut Globals, lfp: LFP, _: Arg, len: usize) -> Result<Value> {
    MonorubyErr::check_number_of_arguments(len, 0)?;
    lfp.expect_no_block()?;
    let mut self_val = lfp.self_val();
    let ary = self_val.as_array_mut();
    //let mut vec = ary.to_vec();
    vm.sort_by(globals, ary, Executor::compare_values)?;
    //self_val.as_array_mut().clone_from_slice(&vec);
    Ok(self_val)
}

///
/// ### Array#each
///
/// - each {|item| .... } -> self
/// - [NOT SUPPORTED] each -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/each.html]
#[monoruby_builtin]
fn each(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: LFP,
    _arg: Arg,
    _len: usize,
) -> Result<Value> {
    let ary = lfp.self_val();
    lfp.expect_block()?;
    vm.invoke_block_iter1(globals, ary.as_array().iter().cloned())?;
    Ok(lfp.self_val())
}

/// ### Array#map
///
/// - map {|item| ... } -> [object]
/// - [NOT SUPPORTED] map -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/collect.html]
#[monoruby_builtin]
fn map(vm: &mut Executor, globals: &mut Globals, lfp: LFP, _: Arg, len: usize) -> Result<Value> {
    MonorubyErr::check_number_of_arguments(len, 0)?;
    let ary = lfp.self_val();
    let iter = ary.as_array().iter().cloned();
    lfp.expect_block()?;
    let vec = vm.invoke_block_map1(globals, iter)?;
    let res = Value::array_from_vec(vec);
    Ok(res)
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
fn flat_map(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: LFP,
    _: Arg,
    len: usize,
) -> Result<Value> {
    MonorubyErr::check_number_of_arguments(len, 0)?;
    let ary = lfp.self_val();
    let iter = ary.as_array().iter().cloned();
    lfp.expect_block()?;
    let mut v = vec![];
    for elem in vm.invoke_block_map1(globals, iter)? {
        match elem.is_array() {
            Some(ary) => v.extend(ary.iter()),
            None => v.push(elem),
        }
    }
    let res = Value::array_from_vec(v);
    Ok(res)
}

///
/// #### Enumerable#detect
///
/// - find([NOT SUPPORTED]ifnone = nil) {|item| ... } -> object
/// - detect([NOT SUPPORTED]ifnone = nil) {|item| ... } -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Enumerable/i/detect.html]
#[monoruby_builtin]
fn detect(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: LFP,
    _arg: Arg,
    len: usize,
) -> Result<Value> {
    MonorubyErr::check_number_of_arguments(len, 0)?;
    let ary = lfp.self_val();
    lfp.expect_block()?;
    let data = globals.get_block_data(vm.cfp());
    for elem in ary.as_array().iter() {
        if vm.invoke_block(globals, &data, &[*elem])?.as_bool() {
            return Ok(*elem);
        };
    }
    Ok(Value::nil())
}

///
/// #### Array#include?
///
/// - include?(val) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/include=3f.html]
#[monoruby_builtin]
fn include_(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: LFP,
    arg: Arg,
    len: usize,
) -> Result<Value> {
    MonorubyErr::check_number_of_arguments(len, 1)?;
    let ary = lfp.self_val();
    let rhs = arg[0];
    for lhs in ary.as_array().iter().cloned() {
        if vm.cmp_eq_values_bool(globals, lhs, rhs)? {
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
fn reverse(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: LFP,
    _arg: Arg,
    len: usize,
) -> Result<Value> {
    MonorubyErr::check_number_of_arguments(len, 0)?;
    let self_val = lfp.self_val();
    let iter = self_val.as_array().iter().rev().cloned();
    Ok(Value::array_from_iter(iter))
}

///
/// #### Array#reverse!
///
/// - reverse! -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/reverse.html]
#[monoruby_builtin]
fn reverse_(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: LFP,
    _arg: Arg,
    len: usize,
) -> Result<Value> {
    MonorubyErr::check_number_of_arguments(len, 0)?;
    let mut self_val = lfp.self_val();
    self_val.as_array_mut().reverse();
    Ok(self_val)
}

///
/// #### Array#reverse!
///
/// - reverse! -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/reverse.html]
#[monoruby_builtin]
fn transpose(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: LFP,
    _arg: Arg,
    len: usize,
) -> Result<Value> {
    MonorubyErr::check_number_of_arguments(len, 0)?;
    let self_val = lfp.self_val();
    let ary = self_val.as_array();
    if ary.len() == 0 {
        return Ok(Value::array_empty());
    }
    let len = ary[0]
        .is_array()
        .ok_or_else(|| MonorubyErr::argumenterr("Each element of receiver must be an array."))?
        .len();
    let mut trans = vec![];
    for i in 0..len {
        let mut temp = vec![];
        for v in ary.iter().cloned() {
            let a = v.is_array().ok_or_else(|| {
                MonorubyErr::argumenterr("Each element of receiver must be an array.")
            })?;
            if a.len() != len {
                return Err(MonorubyErr::indexerr("Element size differs."));
            }
            temp.push(a[i]);
        }
        let ary = Value::array_from_vec(temp);
        trans.push(ary);
    }
    let res = Value::array_from_vec(trans);
    Ok(res)
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
    fn mul() {
        run_test(r##"[1,2,3] * 4"##);
        run_test(
            r##"
        a = [1,2,3]
        res = a * 4
        [a, res]
        "##,
        );
    }

    #[test]
    fn shl() {
        run_test(r##"a = [1,2,3]; a << 10; a"##);
        run_test(r##"a = [1,2,3]; a.<<(10); a"##);
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
            a = [999999999999999999999999999999, -42.4242, 100, 100.001, -555, 0.0, 100, 76543, 100.0]
            a.sort!
            res << a
            res
            "##,
        );
        run_test_error("[1,:hh].sort!");
        run_test_error("[Float::NAN, Float::NAN].sort!");
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
    fn detect() {
        run_test(r#"[1, 2, 3, 4, 5].find {|i| i % 3 == 0 }"#);
        run_test(r#"[2, 2, 2, 2, 2].find {|i| i % 3 == 0 }"#);
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
}
