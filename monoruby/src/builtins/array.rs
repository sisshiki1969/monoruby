use super::*;
#[cfg(target_arch = "x86_64")]
use jitgen::JitContext;
#[cfg(target_arch = "aarch64")]
use jitgen::{AbstractState, JitContext};
use rubymap::RubyEql;
use smallvec::smallvec;
use std::cmp::Ordering;

//
// Array class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_under_obj("Array", ARRAY_CLASS, ObjTy::ARRAY);
    // NOTE: no native `Array.new` — it inherits the Ruby `Class#new`
    // trampoline (startup.rb: `__builtin_allocate__` + forwarded
    // `__builtin_initialize__`), which reaches `Array#initialize` through
    // the specialized inline path. The old native override invoked
    // `initialize` through the generic `invoke_method_inner`, paying a
    // global method-cache lookup per construction.
    globals.define_builtin_class_func_rest(ARRAY_CLASS, "[]", array_class_bracket);
    globals.define_builtin_class_func(ARRAY_CLASS, "try_convert", array_try_convert, 1);
    globals.store[ARRAY_CLASS].set_alloc_func(array_alloc_func);
    // NOTE: no native `Array#initialize` either — the definition lives in
    // builtins/array.rb (JIT-specialized argument binding). Nothing in the
    // startup sequence constructs an Array with arguments before array.rb
    // loads (verified empirically: an instrumented native fallback was
    // never reached across startup, rubygems loading, and the full test
    // suite), and a future pre-load `Array.new(n)` fails loudly through
    // `Object#initialize`'s arity check rather than silently diverging.
    // Native legs of the Ruby `initialize` (private, `__`-prefixed).
    globals.define_private_builtin_func(ARRAY_CLASS, "__init_fill", init_fill, 2);
    globals.define_private_builtin_func(ARRAY_CLASS, "__init_from", init_from, 1);
    globals.define_private_builtin_func(ARRAY_CLASS, "__size_to_int", size_to_int, 1);
    // `flat_map` in builtins/array.rb: the block's Array, appended in one
    // native call (`Array#concat` takes a rest argument and converts each).
    globals.define_private_builtin_func(ARRAY_CLASS, "__concat_to", concat_to, 2);
    let size_fid = globals.define_builtin_inline_funcs(
        ARRAY_CLASS,
        "size",
        &["length"],
        size,
        inline_gen2!(array_size),
        0,
    );
    globals.define_builtin_inline_func(
        ARRAY_CLASS,
        "clone",
        clone,
        inline_gen2!(array_clone),
        0,
    );
    globals.define_builtin_inline_func(
        ARRAY_CLASS,
        "dup",
        dup,
        inline_gen2!(array_dup_inline),
        0,
    );
    globals.define_builtin_func_with(ARRAY_CLASS, "count", count, 0, 1, false);
    globals.define_builtin_inline_func(
        ARRAY_CLASS,
        "empty?",
        empty,
        inline_gen2!(array_empty),
        0,
    );
    globals.define_builtin_funcs(ARRAY_CLASS, "inspect", &["to_s"], inspect, 0);
    globals.define_builtin_func(ARRAY_CLASS, "to_a", to_a, 0);
    globals.define_builtin_func(ARRAY_CLASS, "to_h", to_h, 0);
    let array_hash = globals.define_builtin_func(ARRAY_CLASS, "hash", hash, 0);
    globals.store.set_array_hash_fid(array_hash);
    globals.define_builtin_func(ARRAY_CLASS, "+", add, 1);
    globals.define_builtin_func(ARRAY_CLASS, "-", sub, 1);
    globals.define_builtin_func(ARRAY_CLASS, "*", mul, 1);
    globals.define_builtin_func(ARRAY_CLASS, "&", and, 1);
    globals.define_builtin_func(ARRAY_CLASS, "|", or, 1);
    globals.define_builtin_func_with(ARRAY_CLASS, "shift", shift, 0, 1, false);
    globals.define_builtin_funcs_rest(ARRAY_CLASS, "unshift", &["prepend"], unshift);
    globals.define_builtin_func_rest(ARRAY_CLASS, "concat", concat);
    globals.define_builtin_inline_func(ARRAY_CLASS, "<<", shl, inline_gen2!(array_shl), 1);
    globals.define_builtin_funcs_with(ARRAY_CLASS, "push", &["append"], push, 0, 0, true);
    globals.define_builtin_func_with(ARRAY_CLASS, "pop", pop, 0, 1, false);
    globals.define_builtin_func(ARRAY_CLASS, "==", eq, 1);
    globals.define_builtin_func(ARRAY_CLASS, "eql?", eql, 1);
    globals.define_builtin_func(ARRAY_CLASS, "<=>", cmp, 1);
    globals.define_builtin_inline_funcs_with(
        ARRAY_CLASS,
        "[]",
        &["slice"],
        index,
        inline_gen2!(array_index),
        1,
        2,
        false,
    );
    let aset_fid = globals.define_builtin_inline_func_with(
        ARRAY_CLASS,
        "[]=",
        index_assign,
        inline_gen2!(array_index_assign),
        2,
        3,
        false,
    );
    // What the methods of builtins/array.rb read and write the receiver
    // with, so that an Array subclass (or a program) redefining `size`,
    // `[]` or `[]=` does not change what `each`, `map`, ... walk: CRuby
    // reads `RARRAY_LEN` / `RARRAY_AREF` there and calls no method.
    // `__size` and `__aset` are `size` and `[]=` themselves under a
    // private name (the same function, so the same inline code);
    // `__aref` is `rb_ary_entry`, which takes an Integer and nothing
    // else.
    for (name, fid) in [("__size", size_fid), ("__aset", aset_fid)] {
        globals.add_method(ARRAY_CLASS, IdentId::get_id(name), fid, Visibility::Private);
    }
    let aref_fid = globals.define_builtin_inline_func_with(
        ARRAY_CLASS,
        "__aref",
        aref,
        inline_gen2!(array_aref),
        1,
        2,
        false,
    );
    globals.add_method(
        ARRAY_CLASS,
        IdentId::get_id("__aref"),
        aref_fid,
        Visibility::Private,
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
    globals.define_builtin_func_with(ARRAY_CLASS, "min", min, 0, 1, false);
    globals.define_builtin_func_with(ARRAY_CLASS, "max", max, 0, 1, false);
    globals.define_builtin_func(ARRAY_CLASS, "minmax", minmax, 0);
    globals.define_builtin_funcs(ARRAY_CLASS, "filter!", &["select!"], filter_, 0);
    globals.define_builtin_func(ARRAY_CLASS, "keep_if", keep_if, 0);
    globals.define_builtin_func(ARRAY_CLASS, "reject!", reject_, 0);
    globals.define_builtin_func(ARRAY_CLASS, "delete_if", delete_if, 0);
    globals.define_builtin_func(ARRAY_CLASS, "sort", sort, 0);
    globals.define_builtin_func(ARRAY_CLASS, "sort!", sort_, 0);
    globals.define_builtin_func(ARRAY_CLASS, "sort_by!", sort_by_, 0);
    globals.define_builtin_func(ARRAY_CLASS, "sort_by", sort_by, 0);
    globals.define_builtin_func(ARRAY_CLASS, "group_by", group_by, 0);
    //globals.define_builtin_func(ARRAY_CLASS, "each", each, 0);
    //globals.define_builtin_func(ARRAY_CLASS, "each_with_index", each_with_index, 0);
    //globals.define_builtin_funcs(ARRAY_CLASS, "map", &["collect"], map, 0);
    //globals.define_builtin_funcs(ARRAY_CLASS, "map!", &["collect!"], map_, 0);
    globals.define_builtin_func_with(ARRAY_CLASS, "all?", all_, 0, 1, false);
    globals.define_builtin_func_with(ARRAY_CLASS, "any?", any_, 0, 1, false);
    globals.define_builtin_funcs_with(ARRAY_CLASS, "detect", &["find"], detect, 0, 1, false);
    globals.define_builtin_func_with(ARRAY_CLASS, "rfind", rfind, 0, 1, false);
    globals.define_builtin_func(ARRAY_CLASS, "grep", grep, 1);
    globals.define_builtin_func(ARRAY_CLASS, "include?", include_, 1);
    globals.define_builtin_func(ARRAY_CLASS, "reverse", reverse, 0);
    globals.define_builtin_func(ARRAY_CLASS, "reverse!", reverse_, 0);
    globals.define_builtin_func(ARRAY_CLASS, "transpose", transpose, 0);
    globals.define_builtin_inline_func_with(
        ARRAY_CLASS,
        "rotate!",
        rotate_,
        inline_gen2!(array_rotate_),
        0,
        1,
        false,
    );
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
        "sample",
        sample,
        0,
        1,
        false,
        &["random"],
        false,
    );
    globals.define_builtin_func_with_kw(
        ARRAY_CLASS,
        "shuffle",
        shuffle,
        0,
        0,
        false,
        &["random"],
        false,
    );
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
    globals.define_builtin_func(ARRAY_CLASS, "replace", replace, 1);
    // A method of its own, as in CRuby, and private as every
    // `initialize_copy` is (an alias would carry `replace`'s visibility).
    globals.define_private_builtin_func(ARRAY_CLASS, "initialize_copy", replace, 1);
    init_recv_escape(globals);
}

///
/// The receiver escape summaries (`RecvEscape`) of the builtins above,
/// for the JIT's elision of Array literals used as a receiver
/// (`doc/escape_analysis.md`). A method listed here must never mutate,
/// store or return its receiver — nor return an Enumerator over it when
/// called without a block — in any of its argument forms.
///
fn init_recv_escape(globals: &mut Globals) {
    use crate::executor::inline::{ElementsOp, RecvEscape};
    let table: &[(&str, RecvEscape)] = &[
        ("min", RecvEscape::elements(ElementsOp::Min)),
        ("max", RecvEscape::elements(ElementsOp::Max)),
        ("size", RecvEscape::elements(ElementsOp::Size)),
        ("empty?", RecvEscape::elements(ElementsOp::Empty)),
        ("first", RecvEscape::elements(ElementsOp::First)),
        ("last", RecvEscape::elements(ElementsOp::Last)),
        ("[]", RecvEscape::elements(ElementsOp::ArrayIndex)),
        ("include?", RecvEscape::transparent()),
        ("minmax", RecvEscape::transparent()),
        ("sum", RecvEscape::transparent()),
        ("count", RecvEscape::transparent()),
        ("join", RecvEscape::transparent()),
        ("pack", RecvEscape::transparent()),
        ("hash", RecvEscape::transparent()),
        ("any?", RecvEscape::transparent()),
        ("all?", RecvEscape::transparent()),
        ("sort", RecvEscape::transparent()),
        ("take", RecvEscape::transparent()),
        ("drop", RecvEscape::transparent()),
        ("reverse", RecvEscape::transparent()),
        ("uniq", RecvEscape::transparent()),
        ("compact", RecvEscape::transparent()),
        ("+", RecvEscape::transparent()),
        ("-", RecvEscape::transparent()),
        ("&", RecvEscape::transparent()),
        ("|", RecvEscape::transparent()),
        ("inspect", RecvEscape::transparent()),
    ];
    for (name, info) in table {
        let fid = globals
            .store
            .check_method_for_class(ARRAY_CLASS, IdentId::get_id(name))
            .and_then(|e| e.func_id())
            .unwrap();
        globals.store.set_recv_escape(fid, *info);
    }
}

/// Allocator for `Array` and its subclasses.
pub(crate) extern "C" fn array_alloc_func(class_id: ClassId, _: &mut Globals) -> Value {
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
    if let Some(result) = crate::coerce::check_funcall(vm, globals, obj, IdentId::TO_ARY)? {
        if result.is_array_ty() {
            return Ok(result);
        }
        if result.is_nil() {
            return Ok(Value::nil());
        }
        return Err(MonorubyErr::cant_convert_error_ary(globals, obj, result));
    }
    Ok(Value::nil())
}

///
/// ### Array#__init_fill (private)
///
/// Native leg of the Ruby `Array#initialize` (builtins/array.rb): resets
/// the receiver to `n` copies of `val`. `n` is already an Integer (the
/// Ruby side coerces before calling); negative and oversized sizes raise
/// here so the checks live in one place.
#[monoruby_builtin]
fn init_fill(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let mut self_val = lfp.self_val().as_array_mut(&globals.store)?;
    // A Bignum size must reach the size checks, not raise TypeError:
    // CRuby gives fixnum_max+1 (fits in `long`) ArgumentError "array size
    // too big" and only raises RangeError past the `long` range.
    let size = lfp.arg(0).num2long(vm, globals)?;
    if size < 0 {
        return Err(MonorubyErr::negative_array_size());
    }
    let size = size as usize;
    // `rb_ary_initialize`: a size past `ARY_MAX_SIZE` is an ArgumentError,
    // and one the allocator refuses a NoMemoryError (`Array.new(2**34)`),
    // which the fill below would abort the process on. Only a size that
    // could plausibly fail is tried first.
    if size > ARY_MAX_SIZE {
        return Err(MonorubyErr::argumenterr("array size too big"));
    }
    if size > ARY_CAPA_PROBE && !crate::builtins::kernel::array_capa_available(size) {
        return Err(crate::builtins::string::no_memory_error(vm, globals));
    }
    let val = lfp.arg(1);
    *self_val = ArrayInner::from(smallvec![val; size]);
    Ok(self_val.into())
}

///
/// ### Array#__init_from (private)
///
/// Native leg of the Ruby `Array#initialize`: the single-argument
/// contents path. An Array (or an object whose `#to_ary` yields one)
/// replaces the receiver's contents and returns self; a missing or
/// nil-returning `#to_ary` returns nil so the caller falls through to
/// the size path; any other `#to_ary` result raises TypeError.
#[monoruby_builtin]
fn init_from(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let arg = lfp.arg(0);
    let from_array: ArrayInner = if arg.is_array_ty() {
        (*arg.as_array()).clone()
    } else if let Some(result) = crate::coerce::check_funcall(vm, globals, arg, IdentId::TO_ARY)? {
        if result.is_array_ty() {
            (*result.as_array()).clone()
        } else if result.is_nil() {
            return Ok(Value::nil());
        } else {
            return Err(MonorubyErr::cant_convert_error_ary(globals, arg, result));
        }
    } else {
        return Ok(Value::nil());
    };
    let mut self_val = lfp.self_val().as_array_mut(&globals.store)?;
    *self_val = from_array;
    Ok(self_val.into())
}

///
/// ### Array#__size_to_int (private)
///
/// Native leg of the Ruby `Array#initialize`: implicit `#to_int`
/// coercion of a non-Integer size argument, with CRuby's error messages
/// ("no implicit conversion of X into Integer").
#[monoruby_builtin]
fn size_to_int(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let n = lfp.arg(0).num2long(vm, globals)?;
    Ok(Value::integer(n))
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
    _: Option<CachedClass>,
    _: Option<CachedClass>,
) -> bool {
    let callsite = &store[callid];
    if !callsite.is_simple() {
        return false;
    }
    let dst = callsite.dst;
    state.load(ir, callsite.recv, GP::Rdi);
    // Pure-LIR container length (no arch-specific closure): inline-or-heap length
    // of the array receiver in Rdi, fixnum-tagged into Rax.
    ir.array_len_fixnum(GP::Rax, GP::Rdi);
    state.def_reg2acc_fixnum(ir, GP::Rax, dst);
    true
}

fn array_empty(
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
    let dst = callsite.dst;
    state.load(ir, callsite.recv, GP::Rdi);
    ir.array_len_fixnum(GP::Rsi, GP::Rdi);
    ir.fixnum_zero_to_bool(GP::Rsi);
    state.def_rax2acc(ir, dst);
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
    let recv = lfp.self_val();
    let mut cloned = Value::array_with_class(recv.as_array_inner().clone(), recv.class());
    if recv.is_frozen() {
        cloned.set_frozen();
    }
    Ok(cloned)
}

fn array_clone(
    state: &mut AbstractState,
    ir: &mut AsmIr,
    _: &JitContext,
    store: &Store,
    callid: CallSiteId,
    class_id: Option<CachedClass>,
    _: Option<CachedClass>,
) -> bool {
    let Some(class_id) = class_id else {
        // The call site could not prove the receiver's class (a multi-class
        // dispatch arm / the class-set guard); this generator needs it.
        return false;
    };
    let callsite = &store[callid];
    if !callsite.is_simple() {
        return false;
    }
    let dst = callsite.dst;
    state.load(ir, callsite.recv, GP::Rdi);
    let using_fpr = state.get_using_fpr(ir);
    ir.fpr_save(using_fpr);
    ir.inline(move |r#gen, _, _, _| {
        r#gen.emit_array_clone(array_clone_extern as *const () as u64)
    });
    ir.fpr_restore(using_fpr);
    state.def_reg2acc_class(ir, GP::Rax, dst, class_id.id());
    true
}

extern "C" fn array_clone_extern(val: Value) -> Value {
    let mut cloned = Value::array_with_class(val.as_array_inner().clone(), val.class());
    if val.is_frozen() {
        cloned.set_frozen();
    }
    cloned
}

#[monoruby_builtin]
fn dup(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let recv = lfp.self_val();
    let real_class_id = recv.real_class(&globals.store).id();
    Ok(Value::array_with_class(
        recv.as_array_inner().clone(),
        real_class_id,
    ))
}

fn array_dup_inline(
    state: &mut AbstractState,
    ir: &mut AsmIr,
    _: &JitContext,
    store: &Store,
    callid: CallSiteId,
    class_id: Option<CachedClass>,
    _: Option<CachedClass>,
) -> bool {
    let Some(class_id) = class_id else {
        // The call site could not prove the receiver's class (a multi-class
        // dispatch arm / the class-set guard); this generator needs it.
        return false;
    };
    let callsite = &store[callid];
    if !callsite.is_simple() {
        return false;
    }
    let dst = callsite.dst;
    state.load(ir, callsite.recv, GP::Rdi);
    let using_fpr = state.get_using_fpr(ir);
    ir.fpr_save(using_fpr);
    ir.inline(move |r#gen, _, _, _| {
        r#gen.emit_array_dup(array_dup_extern as *const () as u64)
    });
    ir.fpr_restore(using_fpr);
    state.def_reg2acc_class(ir, GP::Rax, dst, class_id.id());
    true
}

extern "C" fn array_dup_extern(val: Value, globals: &Globals) -> Value {
    let real_class_id = val.real_class(&globals.store).id();
    Value::array_with_class(val.as_array_inner().clone(), real_class_id)
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
            vm.ruby_warn_caller(globals, "warning: given block not used")?;
        }
        let mut count = 0;
        let ary = lfp.self_val();
        // `rb_equal(element, argument)`, like `include?` — element as the
        // receiver, and an element that *is* the argument counted whatever
        // its `==` says.
        let mut search = executor::op::EqSearch::new(globals, arg0);
        let mut i = 0;
        while i < ary.as_array().len() {
            let elem = ary.as_array()[i];
            if vm.rb_equal_search(globals, &mut search, elem)? {
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
    use crate::builtins::encoding::{InspectBuf, inspect_resenc, rb_inspect_in};
    let self_val = lfp.self_val();
    if self_val.as_array().len() == 0 {
        return Ok(Value::string_from_inner(RStringInner::from_encoding(
            b"[]",
            Encoding::UsAscii,
        )));
    }
    crate::value::exec_recursive(
        self_val.id(),
        || {
            // CRuby's `inspect_ary`: each element through `rb_inspect`,
            // the result taking the first element's encoding and then
            // that of any non-ASCII one (#1841).
            let resenc = inspect_resenc(globals);
            let mut buf = InspectBuf::new("[");
            let mut i = 0;
            // The array may change under a user-defined `#inspect`.
            while let Some(elem) = self_val.as_array().get(i).copied() {
                let s = rb_inspect_in(vm, globals, elem, resenc)?;
                let s = s.as_rstring_inner();
                if i > 0 {
                    buf.cat(", ");
                } else {
                    buf.copy_encoding(s);
                }
                buf.append(&globals.store, s)?;
                i += 1;
            }
            buf.cat("]");
            Ok(buf.into_value())
        },
        Value::string_from_inner(RStringInner::from_encoding(b"[...]", Encoding::UsAscii)),
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
    fn inner(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, hash_idx: usize) -> Result<()> {
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
            // `rb_check_array_type`
            let elem = if let Some(a) = elem.check_array_type(vm, globals)? {
                a
            } else {
                return Err(MonorubyErr::typeerr(format!(
                    "wrong element type {} at {i} (expected array)",
                    elem.get_real_class_name(&globals.store)
                )));
            };
            if elem.len() != 2 {
                return Err(MonorubyErr::argumenterr(format!(
                    "wrong array length at {i} (expected 2, was {})",
                    elem.len()
                )));
            }
            // The pair is the block's answer or what an element's
            // `#to_ary` made, held only here while the key's `#hash` /
            // `#eql?` run: root the key and the value, or a collection
            // there frees the value about to be stored.
            let (k, v) = (elem[0], elem[1]);
            vm.temp_push(k);
            vm.temp_push(v);
            let res = vm.temp_at(hash_idx).as_hash().aset(k, v, vm, globals);
            vm.temp_clear(hash_idx + 1);
            res?;
            i += 1;
        }
        Ok(())
    }

    vm.with_temp_scope(|vm| {
        vm.temp_push(Value::hash(RubyMap::default()));
        let hash_idx = vm.temp_len() - 1;
        inner(vm, globals, lfp, hash_idx)?;
        Ok(vm.temp_at(hash_idx))
    })
}

///
/// ### Array#hash
///
/// - hash -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/hash.html]
#[monoruby_builtin]
fn hash(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    // The number a Hash files an Array key under
    // (`Value::array_hash_value`).
    Ok(Value::integer(
        lfp.self_val().array_hash_value(vm, globals)?,
    ))
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
    let lhs = lfp.self_val().as_array();
    vm.with_temp_scope(|vm| {
        let rhs = lfp.arg(0).coerce_to_array(vm, globals)?;
        vm.temp_push(rhs.into());
        let mut res = Array::new_empty();
        vm.temp_push(res.into());
        // CRuby (`rb_ary_diff`): when either side is small, a linear
        // `eql?` scan of `other` — no `#hash` is sent. Otherwise hash
        // `other` once and look each element up.
        if lhs.len() <= SMALL_ARRAY_LEN || rhs.len() <= SMALL_ARRAY_LEN {
            let mut i = 0;
            while i < lhs.len() {
                let elt = lhs[i];
                i += 1;
                if !ary_includes_by_eql(rhs, elt, vm, globals)? {
                    res.push(elt);
                }
            }
        } else {
            let rhs_set = ary_make_set(rhs, vm, globals)?;
            let mut i = 0;
            while i < lhs.len() {
                let elt = lhs[i];
                i += 1;
                if !rhs_set.contains(&elt, vm, globals)? {
                    res.push(elt);
                }
            }
        }
        Ok(res.into())
    })
}

/// CRuby's `SMALL_ARRAY_LEN`: up to this many elements the set
/// operations (`&`, `|`, `-` and their multi-argument forms) compare by
/// a linear `eql?` scan and never send `#hash`.
const SMALL_ARRAY_LEN: usize = 16;

/// `rb_ary_includes_by_eql`: whether `item.eql?(e)` (identity first) for
/// some element `e` of `ary`. The length is re-read on every step, as
/// `eql?` may mutate `ary`.
fn ary_includes_by_eql(
    ary: Array,
    item: Value,
    vm: &mut Executor,
    globals: &mut Globals,
) -> Result<bool> {
    let mut i = 0;
    while i < ary.len() {
        let e = ary[i];
        i += 1;
        if item.eql(&e, vm, globals)? {
            return Ok(true);
        }
    }
    Ok(false)
}

/// `rb_ary_union`: append each element of `ary` to `union` unless an
/// `eql?` one is already there.
fn ary_union_linear(
    mut union: Array,
    ary: Array,
    vm: &mut Executor,
    globals: &mut Globals,
) -> Result<()> {
    let mut i = 0;
    while i < ary.len() {
        let elt = ary[i];
        i += 1;
        if !ary_includes_by_eql(union, elt, vm, globals)? {
            union.push(elt);
        }
    }
    Ok(())
}

/// Hash every element of `ary` into a fresh set (`ary_make_hash`).
fn ary_make_set(ary: Array, vm: &mut Executor, globals: &mut Globals) -> Result<RubySet<Value>> {
    let mut set = RubySet::default();
    let mut i = 0;
    while i < ary.len() {
        let e = ary[i];
        i += 1;
        set.insert(e, vm, globals)?;
    }
    Ok(set)
}

/// The leading elements of `ary` whose keys ask nothing
/// ([`crate::value::rvalue::is_observed_key`] is false: immediates, plain
/// Strings, Bignums, non-zero Floats, objects that are keys by identity).
/// No Ruby runs while they are hashed, so `ary` cannot change under the
/// walk and nothing can be collected: the ones to keep are gathered in
/// Rust memory, and neither they nor `seen`'s keys need rooting.
///
/// Answers where it stopped — the position of the first element whose key
/// has to be asked, or the end.
fn ary_add_new_unasked(
    vm: &mut Executor,
    globals: &mut Globals,
    seen: &mut RubySet<Value>,
    ary: Array,
    kept: &mut Vec<Value>,
) -> Result<usize> {
    let mut i = 0;
    while i < ary.len() {
        let elem = ary[i];
        if crate::value::rvalue::is_observed_key(elem, globals) {
            break;
        }
        if seen.insert(elem, vm, globals)? {
            kept.push(elem);
        }
        i += 1;
    }
    Ok(i)
}

/// `ary_add_hash` / `ary_add_hash_by` from the position `start` on:
/// append to `res` each element of `ary` whose key no earlier element
/// had. The key is the element, or what the block answers for it; keys
/// are compared as Hash keys.
///
/// A key's `#hash` / `eql?` and the block may resize `ary`, so its length
/// and the element are read afresh each round: an element that was popped
/// in the meantime is not looked at (#2213).
///
/// The caller holds a temp scope and has rooted `res` and `ary`. `seen`
/// is not marked, so each key it keeps is rooted here: the element of a
/// pair it accepted is in `res` by then, and a block's answer is pushed.
fn ary_add_new_asked(
    vm: &mut Executor,
    globals: &mut Globals,
    seen: &mut RubySet<Value>,
    mut res: Array,
    ary: Value,
    block: Option<&ProcData>,
    start: usize,
) -> Result<()> {
    // Two slots, kept for the walk: the element being asked, and its key
    // for the duration of the probe (as `HashRef::walk_from` keeps its).
    let elem_slot = vm.temp_len();
    vm.temp_push(Value::nil());
    vm.temp_push(Value::nil());
    let mut i = start;
    while let Some(elem) = ary.as_array().get(i).copied() {
        i += 1;
        if !elem.is_packed_value() {
            vm.temp_set(elem_slot, elem);
        }
        let key = match block {
            Some(data) => vm.invoke_block(globals, data, &[elem])?,
            None => elem,
        };
        // The key for the duration of the probe, whatever it answers.
        if !key.is_packed_value() {
            vm.temp_set(elem_slot + 1, key);
        }
        if seen.insert(key, vm, globals)? {
            res.push(elem);
            if block.is_some() && !key.is_packed_value() {
                // Kept by `seen`, and asked `eql?` by later rounds.
                vm.temp_push(key);
            }
        }
    }
    Ok(())
}

/// `ary_add_hash` over the whole of `ary`, appending to `res`
/// ([`ary_add_new_unasked`] while it can, then [`ary_add_new_asked`]).
fn ary_add_new(
    vm: &mut Executor,
    globals: &mut Globals,
    seen: &mut RubySet<Value>,
    mut res: Array,
    ary: Array,
) -> Result<()> {
    let mut kept = Vec::with_capacity(ary.len());
    let start = ary_add_new_unasked(vm, globals, seen, ary, &mut kept)?;
    res.extend(kept);
    ary_add_new_asked(vm, globals, seen, res, ary.into(), None, start)
}

/// `ary_make_hash` + `rb_hash_values`: the elements of `ary` without
/// those whose key an earlier one had, as a fresh Array. The caller holds
/// a temp scope (the result is rooted in it).
fn ary_uniq_values(
    vm: &mut Executor,
    globals: &mut Globals,
    ary: Value,
    block: Option<BlockHandler>,
) -> Result<Array> {
    let mut seen = RubySet::default();
    // Room for the elements the no-call pass keeps; with a block there is
    // no such pass, since the block is Ruby.
    let mut kept = match block {
        Some(_) => vec![],
        None => Vec::with_capacity(ary.as_array().len()),
    };
    let start = match block {
        Some(_) => 0,
        None => ary_add_new_unasked(vm, globals, &mut seen, ary.as_array(), &mut kept)?,
    };
    let res = Array::new_from_vec(kept);
    vm.temp_push(res.into());
    if start < ary.as_array().len() {
        let data = match block {
            Some(bh) => Some(vm.get_block_data(globals, bh)?),
            None => None,
        };
        ary_add_new_asked(vm, globals, &mut seen, res, ary, data.as_ref(), start)?;
    }
    Ok(res)
}

/// `rb_ary_and`: the elements of `lhs` that are also in `rhs`, without
/// duplicates, in `lhs` order. The caller roots both arrays.
fn ary_and(lhs: Array, rhs: Array, vm: &mut Executor, globals: &mut Globals) -> Result<Array> {
    let mut res = Array::new_empty();
    if lhs.len() == 0 || rhs.len() == 0 {
        return Ok(res);
    }
    vm.with_temp_scope(|vm| {
        vm.temp_push(res.into());
        if lhs.len() <= SMALL_ARRAY_LEN && rhs.len() <= SMALL_ARRAY_LEN {
            let mut i = 0;
            while i < lhs.len() {
                let v = lhs[i];
                i += 1;
                if !ary_includes_by_eql(rhs, v, vm, globals)? {
                    continue;
                }
                if ary_includes_by_eql(res, v, vm, globals)? {
                    continue;
                }
                res.push(v);
            }
        } else {
            let mut rhs_set = ary_make_set(rhs, vm, globals)?;
            let mut i = 0;
            while i < lhs.len() {
                let v = lhs[i];
                i += 1;
                if rhs_set.swap_remove(&v, vm, globals)? {
                    res.push(v);
                }
            }
        }
        Ok(res)
    })
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
        // `rb_ary_times`: a product past the Array size limit is an
        // ArgumentError, and one the allocator refuses a NoMemoryError
        // (`[1] * 2**40`) — `repeat` would abort the process on it.
        let Some(len) = lhs
            .len()
            .checked_mul(rhs)
            .filter(|len| *len <= ARY_MAX_SIZE)
        else {
            return Err(MonorubyErr::argumenterr("argument too big"));
        };
        if !crate::builtins::kernel::array_capa_available(len) {
            return Err(crate::builtins::string::no_memory_error(vm, globals));
        }
        let vec = lhs.repeat(rhs);
        Ok(Value::array_from_vec(vec))
    } else if let Some(inner) = arg.is_rstring_inner() {
        let sep_bytes = inner.as_bytes().to_vec();
        let sep_enc = inner.encoding();
        let sep_cr = inner.code_range();
        let mut visited: HashSet<u64> = HashSet::default();
        visited.insert(lhs.id());
        let mut out: Vec<u8> = Vec::new();
        let mut state: Option<(Encoding, CodeRange)> = None;
        array_join(
            vm,
            globals,
            lhs,
            &sep_bytes,
            sep_enc,
            sep_cr,
            &mut visited,
            &mut out,
            &mut state,
        )?;
        let enc = state.map(|(e, _)| e).unwrap_or(Encoding::UsAscii);
        Ok(Value::string_from_inner(RStringInner::from_encoding_scanned(
            &out, enc,
        )))
    } else if let Ok(s) = arg.coerce_to_str(vm, globals) {
        // Try to_str coercion for join
        let sep_inner = RStringInner::from_str_scanned(s.as_str());
        let sep_bytes = sep_inner.as_bytes().to_vec();
        let sep_enc = sep_inner.encoding();
        let sep_cr = sep_inner.code_range();
        let mut visited: HashSet<u64> = HashSet::default();
        visited.insert(lhs.id());
        let mut out: Vec<u8> = Vec::new();
        let mut state: Option<(Encoding, CodeRange)> = None;
        array_join(
            vm,
            globals,
            lhs,
            &sep_bytes,
            sep_enc,
            sep_cr,
            &mut visited,
            &mut out,
            &mut state,
        )?;
        let enc = state.map(|(e, _)| e).unwrap_or(Encoding::UsAscii);
        Ok(Value::string_from_inner(RStringInner::from_encoding_scanned(
            &out, enc,
        )))
    } else {
        // Try to_int coercion for repeat
        let v = arg.num2long(vm, globals)?;
        if v < 0 {
            return Err(MonorubyErr::negative_argument());
        }
        let rhs = v as usize;
        // `rb_ary_times`: a product past the Array size limit is an
        // ArgumentError, and one the allocator refuses a NoMemoryError
        // (`[1] * 2**40`) — `repeat` would abort the process on it.
        let Some(len) = lhs
            .len()
            .checked_mul(rhs)
            .filter(|len| *len <= ARY_MAX_SIZE)
        else {
            return Err(MonorubyErr::argumenterr("argument too big"));
        };
        if !crate::builtins::kernel::array_capa_available(len) {
            return Err(crate::builtins::string::no_memory_error(vm, globals));
        }
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
    let lhs = lfp.self_val().as_array();
    vm.with_temp_scope(|vm| {
        let rhs = lfp.arg(0).coerce_to_array(vm, globals)?;
        vm.temp_push(rhs.into());
        Ok(ary_and(lhs, rhs, vm, globals)?.into())
    })
}

///
/// ### Array#|
///
/// - self | other -> Array
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/=7c.html]
#[monoruby_builtin]
fn or(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let lhs = lfp.self_val().as_array();
    vm.with_temp_scope(|vm| {
        let rhs = lfp.arg(0).coerce_to_array(vm, globals)?;
        vm.temp_push(rhs.into());
        ary_union_multi(lhs, &[rhs], vm, globals)
    })
}

/// `rb_ary_union_multi`: a linear `eql?` merge while the combined size
/// is small, otherwise a hash-based `uniq` of the concatenation. The
/// caller roots `others`.
fn ary_union_multi(
    lhs: Array,
    others: &[Array],
    vm: &mut Executor,
    globals: &mut Globals,
) -> Result<Value> {
    let sum = lhs.len() + others.iter().map(|a| a.len()).sum::<usize>();
    let res = Array::new_empty();
    vm.with_temp_scope(|vm| {
        vm.temp_push(res.into());
        if sum <= SMALL_ARRAY_LEN {
            ary_union_linear(res, lhs, vm, globals)?;
            for other in others {
                ary_union_linear(res, *other, vm, globals)?;
            }
        } else {
            let mut seen = RubySet::default();
            ary_add_new(vm, globals, &mut seen, res, lhs)?;
            for other in others {
                ary_add_new(vm, globals, &mut seen, res, *other)?;
            }
        }
        Ok(res.into())
    })
}

///
/// ### Array#shift
/// - shift -> object | nil
/// - shift(n) -> Array
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/shift.html]
#[monoruby_builtin]
fn shift(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let mut ary = lfp.self_val().as_array_mut(&globals.store)?;
    if lfp.try_arg(0).is_none() {
        if ary.len() == 0 {
            return Ok(Value::nil());
        }
        let res = ary[0];
        ary.drain(0..1);
        Ok(res)
    } else {
        let i = lfp.arg(0).num2long(vm, globals)?;
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
    let mut ary = lfp.self_val().as_array_mut(&globals.store)?;
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
    let mut self_ary = lfp.self_val().as_array_mut(&globals.store)?;
    let mut ary: Array = Array::new_empty();
    let args: Vec<Value> = lfp.arg(0).as_array().iter().cloned().collect();
    vm.with_temp_scope(|vm| {
        // `coerce_to_array` dispatches `#to_ary` — arbitrary Ruby, which
        // may collect. The accumulator is reachable only from this Rust
        // local, so it has to be rooted for the duration or the sweep
        // takes it and `extend_from_slice` reads a dead RValue.
        vm.temp_push(ary.into());
        for a in args {
            let converted = a.coerce_to_array(vm, globals)?;
            ary.extend_from_slice(&converted);
        }
        Ok::<(), MonorubyErr>(())
    })?;
    self_ary.extend_from_slice(&ary);
    Ok(self_ary.into())
}

/// `Array#__concat_to(dst, src)` (private): the elements of the Array
/// *src* appended to the Array *dst*, which is returned. `flat_map` in
/// builtins/array.rb passes Arrays it made or checked, so nothing is
/// converted and no Ruby runs; anything else is a TypeError rather than
/// a panic, since `send` reaches a private method too.
#[monoruby_builtin]
fn concat_to(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let (dst, src) = (lfp.arg(0), lfp.arg(1));
    match (dst.try_rvalue(), src.try_rvalue()) {
        (Some(d), Some(s)) if d.ty() == ObjTy::ARRAY && s.ty() == ObjTy::ARRAY => {
            if d.is_frozen() {
                return Err(MonorubyErr::cant_modify_frozen(&globals.store, dst));
            }
        }
        _ => return Err(MonorubyErr::typeerr("__concat_to: Array expected")),
    }
    let mut dst = dst.as_array();
    if dst.id() == src.id() {
        // The slice would borrow the buffer being grown.
        let elems = dst.to_vec();
        dst.extend_from_slice(&elems);
    } else {
        dst.extend_from_slice(&src.as_array());
    }
    Ok(dst.into())
}

///
/// ### Array#<<
///
/// - self << obj -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/=3c=3c.html]
#[monoruby_builtin]
fn shl(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let mut ary = lfp.self_val().as_array_mut(&globals.store)?;
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
    recv_class: Option<CachedClass>,
    _: Option<CachedClass>,
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
    let CallSiteInfo {
        dst, args, recv, ..
    } = *callsite;
    state.load(ir, recv, GP::Rdi);
    // `emit_array_shl`'s inline store bypasses `Array::push`, which never
    // checked frozen-ness itself — a frozen receiver has to leave JIT code
    // so the interpreter can raise `FrozenError`.
    let deopt = ir.new_deopt(state);
    ir.guard_frozen(deopt);
    state.load(ir, args, GP::Rsi);
    let using_fpr = state.get_using_fpr(ir);
    ir.fpr_save(using_fpr);
    ir.inline(move |r#gen, _, _, _| r#gen.emit_array_shl(ary_shl as *const () as u64));
    ir.fpr_restore(using_fpr);
    state.def_reg2acc_class(ir, GP::Rax, dst, recv_class.id());
    true
}

///
/// `Array#rotate!` with the count already narrowed to an `i64` — the body
/// `rotate_` runs once its argument handling is done. Called directly from
/// JIT code, which has proven the receiver is an unfrozen `Array` and the
/// count a Fixnum, so none of `rotate_`'s coercion / frozen / arity work is
/// needed.
///
extern "C" fn ary_rotate_(mut ary: Array, count: i64) -> Value {
    let ary_len = ary.len() as i64;
    if ary_len != 0 {
        if count > 0 {
            ary.rotate_left(wrap_rotate_count(count, ary_len));
        } else {
            ary.rotate_right(wrap_rotate_count(count, ary_len));
        }
    }
    ary.into()
}

fn array_rotate_(
    state: &mut AbstractState,
    ir: &mut AsmIr,
    _: &JitContext,
    store: &Store,
    callid: CallSiteId,
    recv_class: Option<CachedClass>,
    arg_class: Option<CachedClass>,
) -> bool {
    let Some(recv_class) = recv_class else {
        // The call site could not prove the receiver's class (a multi-class
        // dispatch arm / the class-set guard); this generator needs it.
        return false;
    };
    let callsite = &store[callid];
    if !callsite.is_simple() || callsite.pos_num > 1 {
        return false;
    }
    // `arg_class` is only fed for 1-positional-argument sites, which is
    // exactly the form we narrow; the 0-argument form rotates by 1.
    let has_arg = callsite.pos_num == 1;
    if has_arg && arg_class != Some(CachedClass::INTEGER) {
        return false;
    }
    let CallSiteInfo {
        dst, args, recv, ..
    } = *callsite;
    state.load(ir, recv, GP::Rdi);
    let deopt = ir.new_deopt(state);
    ir.guard_frozen(deopt);
    if has_arg {
        state.load_fixnum(ir, args, GP::Rsi);
    }
    let using_fpr = state.get_using_fpr(ir);
    ir.fpr_save(using_fpr);
    ir.inline(move |r#gen, _, _, _| {
        r#gen.emit_array_rotate_(ary_rotate_ as *const () as u64, has_arg)
    });
    ir.fpr_restore(using_fpr);
    state.def_reg2acc_class(ir, GP::Rax, dst, recv_class.id());
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
    let mut ary = lfp.self_val().as_array_mut(&globals.store)?;
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
    let mut ary = lfp.self_val().as_array_mut(&globals.store)?;
    if let Some(n) = lfp.try_arg(0) {
        let n = n.num2long(vm, globals)?;
        if n < 0 {
            return Err(MonorubyErr::argumenterr("negative array size"));
        }
        let n = n as usize;
        let len = ary.len();
        let start = if n >= len { 0 } else { len - n };
        let result = Value::array_from_slice(&ary[start..]);
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
    // Identity first, as in `==` below.
    if self_val.id() == arg.id() {
        return Ok(Value::bool(true));
    }
    let lhs = self_val.as_array();
    let rhs = if let Some(rhs) = arg.try_array_ty() {
        rhs
    } else {
        return Ok(Value::bool(false));
    };
    if lhs.len() != rhs.len() {
        return Ok(Value::bool(false));
    }
    crate::value::exec_recursive_paired(
        self_val.id(),
        arg.id(),
        || {
            Ok(Value::bool(self_val.array_elements_eql(
                rhs.into(),
                vm,
                globals,
            )?))
        },
        Value::bool(true),
    )
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
    // An Array equals itself without any element being looked at, which is
    // where `rb_ary_equal` starts too.
    if self_val.id() == arg.id() {
        return Ok(Value::bool(true));
    }
    let lhs = self_val.as_array();
    let rhs = if let Some(rhs) = arg.try_array_ty() {
        rhs
    } else {
        // If the other object responds to #to_ary, delegate to other == self.
        let respond_to = IdentId::get_id("respond_to?");
        let respond = vm.invoke_method_inner(
            globals,
            respond_to,
            arg,
            &[Value::symbol(IdentId::TO_ARY)],
            None,
            None,
        );
        if respond.is_ok_and(|v| v.as_bool()) {
            return vm
                .eq_values_funcall(globals, arg, self_val)
                .map(|v| Value::bool(v.as_bool()));
        }
        return Ok(Value::bool(false));
    };
    if lhs.len() != rhs.len() {
        return Ok(Value::bool(false));
    }
    crate::value::exec_recursive_paired(
        self_val.id(),
        arg.id(),
        || {
            // `recursive_equal`: an element's `==` is Ruby and may
            // resize either Array, so the lengths are read again after
            // each call; Arrays that no longer match in length differ.
            let mut len = lhs.len();
            let mut i = 0;
            while i < len {
                let (l, r) = (lhs[i], rhs[i]);
                // CRuby first checks #equal? (identity), which makes
                // [NaN] == [NaN] true since NaN.equal?(NaN) is true.
                if l.id() != r.id() {
                    // `rb_equal`: the element's own `==`, never `!=`.
                    if !vm.eq_values_bool(globals, l, r)? {
                        return Ok(Value::bool(false));
                    }
                    len = lhs.len();
                    if len != rhs.len() {
                        return Ok(Value::bool(false));
                    }
                }
                i += 1;
            }
            Ok(Value::bool(true))
        },
        Value::bool(true),
    )
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
    // `rb_check_array_type`: a `to_ary` that answers something other than
    // an Array or nil is a TypeError.
    let Some(rhs) = lfp.arg(0).check_array_type(vm, globals)? else {
        return Ok(Value::nil());
    };
    if self_val.id() == rhs.id() {
        return Ok(Value::integer(Ordering::Equal as i64));
    }
    // `rhs` may be a fresh Array `#to_ary` made, held only here across
    // every element's `<=>`: root it, or a collection there frees it.
    vm.with_temp_scope(|vm| {
        vm.temp_push(rhs.into());
        // `recursive_cmp`: the first pair whose `<=>` is not exactly the
        // Fixnum `0` decides, with its *raw* answer (`nil`, `±1`, or even
        // a String). The pairs are those below the shorter length *as it
        // was on entry*; an element's `<=>` may resize either Array, so
        // each element is read afresh, and one that is gone by then is
        // `nil` (`rb_ary_elt`).
        let mut decided = None;
        crate::value::exec_recursive_paired(
            self_val.id(),
            rhs.id(),
            || {
                let len = self_val.as_array().len().min(rhs.len());
                for i in 0..len {
                    let l = self_val.as_array().get(i).copied().unwrap_or_default();
                    let r = rhs.get(i).copied().unwrap_or_default();
                    let res =
                        vm.invoke_method_inner(globals, IdentId::_CMP, l, &[r], None, None)?;
                    if res.try_fixnum() != Some(0) {
                        decided = Some(res);
                        break;
                    }
                }
                Ok(Value::nil())
            },
            Value::nil(),
        )?;
        // No pair decided, or this very pair is already being compared
        // further up: the lengths as they are now do.
        Ok(decided
            .unwrap_or_else(|| Value::integer(self_val.as_array().len().cmp(&rhs.len()) as i64)))
    })
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
        subseq(vm, globals, ary, lfp.arg(0), arg1)
    } else {
        let idx = lfp.arg(0);
        // Non-fixnum, non-range index: delegate. The
        // `Enumerator::ArithmeticSequence` case is handed off to
        // `aseq.__slice_array(self)` — slicing logic lives in
        // `builtins/arithmetic_sequence.rs`.
        // We dispatch via ObjTy so singleton-classed AS values
        // (e.g. those returned by `Range#%`, which patches `inspect`)
        // also hit the fast path. Anything else falls through to the
        // existing `to_int` coercion path.
        if idx.try_fixnum().is_none() && idx.is_range().is_none() {
            if idx.ty() == Some(ObjTy::ARITHMETIC_SEQUENCE) {
                return vm.invoke_method_inner(
                    globals,
                    IdentId::get_id("__slice_array"),
                    idx,
                    &[lfp.self_val()],
                    None,
                    None,
                );
            }
            let i = idx.num2long(vm, globals)?;
            return ary.get_elem1(vm, globals, Value::integer(i));
        }
        ary.get_elem1(vm, globals, idx)
    }
}

/// `ary[start, len]`, both converted with `to_int` when they are not
/// Fixnums.
fn subseq(
    vm: &mut Executor,
    globals: &mut Globals,
    ary: Array,
    start: Value,
    len: Value,
) -> Result<Value> {
    let start = if start.try_fixnum().is_none() {
        Value::integer(start.num2long(vm, globals)?)
    } else {
        start
    };
    let len = if len.try_fixnum().is_none() {
        Value::integer(len.num2long(vm, globals)?)
    } else {
        len
    };
    ary.get_elem2(vm, globals, start, len)
}

///
/// ### Array#__aref (internal)
///
/// - __aref(nth) -> object | nil
/// - __aref(start, length) -> Array | nil
///
/// The element read of the methods in builtins/array.rb: `rb_ary_entry`
/// for one argument, which is converted as `NUM2LONG` converts it (a
/// Range is a TypeError here, as it is for `Array#at`), and `rb_ary_subseq`
/// for two. Private, and not `[]`: a subclass's `[]` is not consulted.
///
#[monoruby_builtin]
fn aref(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    if let Some(len) = lfp.try_arg(1) {
        return subseq(vm, globals, lfp.self_val().as_array(), lfp.arg(0), len);
    }
    let idx = Value::integer(lfp.arg(0).num2long(vm, globals)?);
    lfp.self_val().as_array().get_elem1(vm, globals, idx)
}

///
/// `__aref(i)` in line. A site in builtins/array.rb has no cache of the
/// index's class as a `[]` site has, so an index the state has not proven
/// is guarded here: anything but a Fixnum leaves for the interpreter,
/// which converts it or raises.
///
fn array_aref(
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
        recv, args, dst, ..
    } = *callsite;
    let Some(dst) = dst else {
        // Nothing to read, but an index that is not an Integer still has
        // to be converted (and may raise).
        return state.class(args) == Some(INTEGER_CLASS);
    };
    state.array_integer_index(ir, store, dst, recv, args);
    true
}

fn array_index(
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
    let mut ary = lfp.self_val().as_array_mut(&globals.store)?;
    if lfp.try_arg(2).is_none() {
        let i = lfp.arg(0);
        let val = lfp.arg(1);
        if let Some(idx) = i.try_fixnum() {
            ary.set_index(idx, val)
        } else if let Some(range) = i.is_range() {
            // Resolve the start. A negative start that is still out of bounds
            // after wrapping raises RangeError (unlike a bare negative Integer
            // index, which raises IndexError).
            let start = if range.start().is_nil() {
                0
            } else {
                let s = range.start().num2long(vm, globals)?;
                let s = if s < 0 { s + ary.len() as i64 } else { s };
                if s < 0 {
                    return Err(MonorubyErr::rangeerr(format!(
                        "{} out of range",
                        i.to_s(&globals.store)
                    )));
                }
                s as usize
            };
            // Resolve the length. Unlike the start, a negative end that is out
            // of bounds is not an error: it simply yields a zero-length
            // (insertion) replacement at `start`.
            let len = if range.end().is_nil() {
                // endless range: length is from start to end of array
                ary.len().checked_sub(start)
            } else {
                let e = range.end().num2long(vm, globals)?;
                let e = if e < 0 { e + ary.len() as i64 } else { e };
                let e = if range.exclude_end() { e } else { e + 1 };
                usize::try_from(e - start as i64).ok()
            };
            if let Some(len) = len {
                ary.set_index2(start, len, val)
            } else {
                // end < start: treat as zero-length replacement at start
                ary.set_index2(start, 0, val)
            }
        } else {
            let idx = i.num2long(vm, globals)?;
            ary.set_index(idx, val)
        }
    } else {
        let start = lfp.arg(0).num2long(vm, globals)?;
        let l = lfp.arg(1).num2long(vm, globals)?;
        if l < 0 {
            return Err(MonorubyErr::indexerr(format!("negative length ({})", l)));
        }
        let mut i = start;
        if i < 0 {
            i += ary.len() as i64;
            if i < 0 {
                return Err(MonorubyErr::index_too_small(start, -(ary.len() as i64)));
            }
        }
        let val = lfp.arg(2);
        // Try #to_ary conversion for multi-element assignment.
        let val = if val.try_array_ty().is_some() {
            val
        } else if let Some(a) = val.check_array_type(vm, globals)? {
            a.peel()
        } else {
            val
        };
        ary.set_index2(i as usize, l as usize, val)
    }
}

fn array_index_assign(
    state: &mut AbstractState,
    ir: &mut AsmIr,
    ctx: &JitContext,
    store: &Store,
    callid: CallSiteId,
    _: Option<CachedClass>,
    idx_class: Option<CachedClass>,
) -> bool {
    let callsite = &store[callid];
    if !callsite.is_simple() {
        return false;
    }
    match callsite.pos_num {
        2 => {
            let CallSiteInfo {
                recv, args, dst, ..
            } = *callsite;
            // An `IndexAssign` site hands over the index class its cache
            // observed and has no result register. A method call
            // (`__aset(i, v)` in builtins/array.rb) hands over neither:
            // the index has to be one the state has proven, and a call
            // whose value is used takes the ordinary path.
            if dst.is_some()
                || (idx_class != Some(CachedClass::INTEGER)
                    && state.class(args) != Some(INTEGER_CLASS))
            {
                return false;
            }
            state.array_integer_index_assign(ir, store, args + 1usize, recv, args);
            true
        }
        3 => array_slice_assign(state, ir, ctx, store, callid),
        _ => false,
    }
}

///
/// JIT-inline the *slice* form `ary[start, len] = other`.
///
/// The generic `index_assign` path costs two `coerce_to_int_i64`s, a
/// `#to_ary` probe and `ArrayInner::set_index2`'s full resize/`copy_within`
/// dance. But the overwhelmingly common shape — the one emulator inner loops
/// write, e.g. `@bg_pixels[@scroll_xfine, 8] = @bg_pattern_lut[…]` — replaces
/// a run with an equally long one *inside* the array, which is a plain copy:
/// nothing moves, nothing resizes, no element is dropped or added.
///
/// So the inline path takes `len` from a compile-time literal and checks the
/// rest at run time (`other` is an `Array`, `other.len() == len`,
/// `0 <= start` and `start + len <= self.len()`), copying `len` values when
/// they all hold. Anything else — a growing/shrinking splice, a negative
/// index — falls into `set_array_slice_pure`, which performs the builtin's
/// splice for an Array right-hand side. It runs no Ruby: for a right-hand
/// side that is not an Array (`#to_ary`), or where the builtin raises, it
/// declines, the JIT code deopts and the interpreter makes the call in a
/// frame; a site that keeps declining is stamped and takes the ordinary
/// call from its recompile on (#2156).
///
fn array_slice_assign(
    state: &mut AbstractState,
    ir: &mut AsmIr,
    ctx: &JitContext,
    store: &Store,
    callid: CallSiteId,
) -> bool {
    /// Beyond this the unrolled copy stops paying for the call it replaces.
    const MAX_INLINE_SLICE: i64 = 8;
    let callsite = &store[callid];
    let CallSiteInfo {
        recv, args, dst, ..
    } = *callsite;
    assert!(dst.is_none());
    if state.pc().impure_helper() {
        return false;
    }
    let start = args;
    let count = args + 1usize;
    let src = args + 2usize;
    // `idx_class` is only fed for 1-positional-argument sites, so gate on the
    // abstract state directly. The weak predicate is deliberate: the common
    // `ary[ivar, 8] = …` reads its start straight out of an instance variable,
    // which the state has not narrowed to Fixnum, and `load_fixnum`'s own
    // guard covers it. Only a slot *proven* to hold something else (a Float,
    // a Range) is refused, since that guard would deopt every time.
    if !state.may_be_fixnum(start) {
        return false;
    }
    // `len` must be a literal: it is what the copy is unrolled by.
    let Some(len) = state.is_fixnum_literal(count) else {
        return false;
    };
    let len = len.get();
    if !(0..=MAX_INLINE_SLICE).contains(&len) {
        return false;
    }
    let len = len as usize;

    state.load(ir, recv, GP::Rdi);
    let deopt = ir.new_deopt(state);
    ir.guard_frozen(deopt);
    state.load_fixnum(ir, start, GP::Rsi);
    state.load(ir, src, GP::Rdx);
    let using_fpr = state.get_using_fpr(ir);
    let helper = set_array_slice_pure as *const () as u64;
    ir.fpr_save(using_fpr);
    ir.inline(move |r#gen, _, _, _| r#gen.emit_array_slice_assign(helper, len));
    ir.fpr_restore(using_fpr);
    ctx.pure_helper_exit(state, ir);
    true
}

///
/// The out-of-line half of `array_slice_assign`: `ary[start, len] = val`
/// when the right-hand side is an Array, so that no `#to_ary` is called —
/// a helper called from JIT code has no frame and must not run Ruby
/// (#2156). The receiver's class and unfrozen-ness are already guarded by
/// the JIT, and `len` comes from the literal `array_slice_assign` bounded
/// to `0..=MAX_INLINE_SLICE`. It answers 0 — with no error set and the
/// receiver untouched — for any other right-hand side, and where the
/// builtin would raise (an index out of range, detected before anything
/// is stored): the JIT code deopts and the interpreter performs the
/// assignment in a frame.
///
extern "C" fn set_array_slice_pure(
    base: Value,
    mut start: i64,
    len: usize,
    val: Value,
    _: &mut Executor,
    _: &mut Globals,
) -> Option<Value> {
    val.try_array_ty()?;
    let mut ary = base.as_array();
    if start < 0 {
        start += ary.len() as i64;
        if start < 0 {
            return None;
        }
    }
    ary.set_index2(start as usize, len, val).ok()
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
    let mut ary = lfp.self_val().as_array_mut(&globals.store)?;
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
    let mut self_ary = lfp.self_val().as_array_mut(&globals.store)?;
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
    let mut ary = lfp.self_val().as_array_mut(&globals.store)?;

    if let Some(bh) = lfp.block() {
        // Block form: fill {}, fill(start) {}, fill(start, length) {}, fill(range) {}
        // The value comes from the block, so at most two positional args
        // (start, length) are allowed — a third is an ArgumentError.
        if lfp.try_arg(2).is_some() {
            return Err(MonorubyErr::wrong_number_of_arg_range(3, 0..=2));
        }
        let data = vm.get_block_data(globals, bh)?;
        let (start, end_idx) = if let Some(arg0) = lfp.try_arg(0) {
            if let Some(range) = arg0.is_range() {
                // fill(range) {|index| ... } -> self
                fill_range_indices(vm, globals, &ary, range)?
            } else {
                // fill(start, length = nil) {|index| ... } -> self
                let start = arg0.num2long(vm, globals)?;
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
                        let len = len_val.num2long(vm, globals)?;
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
            // The range form is `fill(val, range)` only — with a third
            // (length) argument, `arg1` is a start index, so a Range there
            // is a TypeError (Range has no `#to_int`), matching CRuby.
            if let Some(range) = arg1.is_range().filter(|_| lfp.try_arg(2).is_none()) {
                // fill(val, range) -> self
                let (start, end_idx) = fill_range_indices(vm, globals, &ary, range)?;
                if end_idx > ary.len() {
                    fill_resize(&mut ary, end_idx)?;
                }
                for i in start..end_idx {
                    ary[i] = val;
                }
            } else {
                // fill(val, start, length = nil) -> self.
                // A `nil` start means 0 (CRuby coerces nil to 0 here,
                // rather than raising as `Integer(nil)` would).
                let start = if arg1.is_nil() {
                    0i64
                } else {
                    arg1.num2long(vm, globals)?
                };
                let start = if start < 0 {
                    let s = ary.len() as i64 + start;
                    if s < 0 { 0i64 } else { s }
                } else {
                    start
                } as usize;
                // Only an *explicit* length extends the array; the
                // start-only and `nil`-length forms fill existing elements
                // from `start` to the end and never grow it (so a `start`
                // past the end is a no-op). `start..len` is an empty range
                // when `start >= len`, so no bounds check is needed.
                if let Some(len_val) = lfp.try_arg(2)
                    && !len_val.is_nil()
                {
                    let len = len_val.num2long(vm, globals)?;
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
                } else {
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

/// Grow the array for `fill`. As in `rb_ary_fill`, an end past
/// `ARY_MAX_SIZE` is an ArgumentError and a length the allocator refuses
/// a NoMemoryError (`[1].fill(0, 0, 2**40)`).
fn fill_resize(ary: &mut Array, new_len: usize) -> Result<()> {
    if new_len > ARY_MAX_SIZE {
        return Err(MonorubyErr::argumenterr("argument too big"));
    }
    if new_len > ARY_CAPA_PROBE && !crate::builtins::kernel::array_capa_available(new_len) {
        return Err(MonorubyErr::nomemoryerr());
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
        range.start().num2long(vm, globals)?
    };
    if start < 0 {
        start += len;
        if start < 0 {
            return Err(MonorubyErr::rangeerr(format!(
                "{}..{} out of range",
                range.start().num2long(vm, globals)?,
                if range.end().is_nil() {
                    String::new()
                } else {
                    range.end().num2long(vm, globals)?.to_string()
                }
            )));
        }
    }
    // nil end means array length (endless range)
    let end_idx = if range.end().is_nil() {
        len
    } else {
        let mut end_val = range.end().num2long(vm, globals)?;
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
    let num = lfp.arg(0).num2long(vm, globals)?;
    if num < 0 {
        return Err(MonorubyErr::argumenterr("attempt to drop negative size"));
    }
    let num = num as usize;
    if num >= ary.len() {
        return Ok(Value::array_empty());
    };
    let ary = &ary[num..];
    Ok(Value::array_from_slice(&ary))
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
    let each_id = IdentId::EACH;
    let zip_pull_id = IdentId::get_id("__zip_pull");

    vm.with_temp_scope(|vm| {
        // Phase 1: stash each converted argument as a GC-rooted Array on the
        // temp_stack at offsets [base..base + n_args].
        let base = vm.temp_len();
        for a in lfp.arg(0).as_array().iter() {
            if let Ok(ary) = a.coerce_to_array(vm, globals) {
                vm.temp_push(ary.into());
            } else if globals.check_method(*a, each_id).is_some() {
                // Collect with `each` + break (`Array#__zip_pull`,
                // CRuby's rb_ary_zip / take_items protocol) rather than
                // `to_enum` + `next`: no fiber in the path, and every
                // exception the argument's `each` raises — StopIteration
                // included — reaches the caller. An Enumerator-based
                // pull cannot make that distinction, because a
                // user-raised StopIteration and the enumerator's own
                // end-of-iteration signal arrive as the same exception
                // from `#next` (issue #1080).
                let pulled = vm.invoke_method_inner(
                    globals,
                    zip_pull_id,
                    lfp.self_val(),
                    &[*a, Value::integer(self_len as i64)],
                    None,
                    None,
                )?;
                vm.temp_push(pulled);
            } else {
                return Err(MonorubyErr::typeerr(format!(
                    "wrong argument type {} (must respond to :each)",
                    a.get_real_class_name(&globals.store),
                )));
            }
        }
        let n_args = vm.temp_len() - base;

        // Phase 2: build rows. Each `row` is consumed (passed to invoke_block
        // or pushed into result_ary) before the next Ruby callback, so it does
        // not need its own temp protection.
        if let Some(bh) = lfp.block() {
            let data = vm.get_block_data(globals, bh)?;
            for (i, val) in self_ary.iter().enumerate() {
                let mut row = Array::new_empty();
                row.push(*val);
                for j in 0..n_args {
                    let arg = vm.temp_at(base + j).as_array();
                    row.push(if i < arg.len() { arg[i] } else { Value::nil() });
                }
                vm.invoke_block(globals, &data, &[row.as_val()])?;
            }
            Ok(Value::nil())
        } else {
            let mut result_ary = Array::new_empty();
            for (i, val) in self_ary.iter().enumerate() {
                let mut row = Array::new_empty();
                row.push(*val);
                for j in 0..n_args {
                    let arg = vm.temp_at(base + j).as_array();
                    row.push(if i < arg.len() { arg[i] } else { Value::nil() });
                }
                result_ary.push(row.as_val());
            }
            Ok(result_ary.as_val())
        }
    })
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
    // A block is used only for the 0/1-argument forms. With two
    // arguments (init, sym) the block is ignored (CRuby warns).
    if let Some(bh) = lfp.block()
        && lfp.try_arg(1).is_none()
    {
        // Fold with the block, re-reading the receiver by index each step so
        // that growing the array inside the block is tolerated: CRuby visits
        // the appended elements too (and a shrink simply stops early). A
        // snapshot iterator would miss them.
        let self_val = lfp.self_val();
        let data = vm.get_block_data(globals, bh)?;
        let (mut res, mut i) = if lfp.try_arg(0).is_none() {
            let ary = self_val.as_array();
            if ary.len() == 0 {
                return Ok(Value::nil());
            }
            (ary[0], 1usize)
        } else {
            (lfp.arg(0), 0usize)
        };
        loop {
            let elem = {
                let ary = self_val.as_array();
                if i >= ary.len() {
                    break;
                }
                ary[i]
            };
            i += 1;
            res = vm.invoke_block(globals, &data, &[res, elem])?;
        }
        return Ok(res);
    }
    if lfp.block().is_some() && lfp.try_arg(1).is_some() {
        // CRuby emits this with an implicit `uplevel: 1`, so the message is
        // prefixed with the location of the `inject` call site.
        vm.ruby_warn_caller(globals, "warning: given block not used")?;
    }
    let self_ = lfp.self_val().as_array();
    // The `inject(sym)` form may consume the first element as the
    // initial accumulator; track where the remaining elements start.
    // A non-Symbol/String method name is coerced via `#to_str`.
    // The name is looked up as `rb_check_id` does: a String no Symbol
    // exists for stays a String, and each step is a `send` of it (which
    // makes the Symbol only for a receiver with a `method_missing` of
    // its own).
    let (name, mut res, mut start) = if let Some(arg0) = lfp.try_arg(0) {
        if let Some(arg1) = lfp.try_arg(1) {
            (
                super::kernel::checked_name(vm, globals, arg1)?,
                arg0,
                0usize,
            )
        } else {
            let name = super::kernel::checked_name(vm, globals, arg0)?;
            if self_.len() == 0 {
                return Ok(Value::nil());
            }
            (name, self_[0], 1usize)
        }
    } else {
        return Err(MonorubyErr::argumenterr("wrong number of arguments"));
    };
    let sym = match name {
        super::kernel::CheckedName::Id(id) => id,
        super::kernel::CheckedName::Unknown(name) => {
            let Some(v) = self_.get(start).copied() else {
                return Ok(res);
            };
            let id = super::kernel::send_unknown_name(globals, res, name, &[v], true)?;
            res = vm.invoke_method_inner(globals, id, res, &[v], None, None)?;
            start += 1;
            id
        }
    };
    // Fast path for `inject(:+)` on a Fixnum accumulator: skip method
    // dispatch entirely while elements stay in i64 range.
    if sym == IdentId::_ADD
        && let Some(acc_i) = res.try_fixnum()
    {
        let mut acc = acc_i;
        let len = self_.len();
        let mut i = start;
        while i < len {
            let v = self_[i];
            let Some(x) = v.try_fixnum() else { break };
            let Some(new) = acc.checked_add(x) else { break };
            acc = new;
            i += 1;
        }
        if i == len {
            return Ok(Value::integer(acc));
        }
        // Resume generic dispatch with the partial sum we have so far.
        res = Value::integer(acc);
        start = i;
    }
    for v in self_[start..].iter() {
        // `rb_funcallv_public`: a public method, found under the
        // refinements of the Ruby code that called `inject`.
        res = vm.invoke_method_public(globals, sym, res, &[*v], None, None)?;
    }
    Ok(res)
}

///
/// ### Array#join
///
/// - join(sep = $,) -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/join.html]
#[monoruby_builtin]
fn join(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let ary = lfp.self_val().as_array();
    let arg0 = lfp.try_arg(0);

    // CRuby warns about a non-nil `$,` whenever join would fall back
    // to it (no separator argument or an explicit nil). The warning
    // fires even for an empty array.
    let would_use_global_sep = match &arg0 {
        None => true,
        Some(v) => v.is_nil(),
    };
    if would_use_global_sep && !globals.ofs().is_nil() {
        // `:deprecated`-category (silent unless `Warning[:deprecated]`),
        // matching CRuby — see the `$;` warning in `String#split`.
        vm.warn_deprecated(globals, "warning: $, is set to non-nil value")?;
    }

    // Empty array shortcut: CRuby returns `""` tagged US-ASCII
    // without ever inspecting the separator (in particular, no
    // `#to_str` is sent to it). Doing this before separator
    // coercion is what lets the spec's `does not call #to_str on
    // the separator if the array is empty` test pass.
    if ary.is_empty() {
        let _ = arg0; // separator intentionally unused
        return Ok(Value::string_from_inner(RStringInner::from_encoding(
            b"",
            Encoding::UsAscii,
        )));
    }

    // Resolve the separator as a `(bytes, encoding, code_range)`
    // triple. Carrying the code range lets the join propagate
    // CRuby's "left-encoding wins on 7-bit content" rule even
    // when the separator is the first non-empty thing emitted.
    let (sep_bytes, sep_enc, sep_cr): (Vec<u8>, Encoding, CodeRange) =
        if let Some(sep) = &arg0 {
            if sep.is_nil() {
                match get_output_field_separator(globals) {
                    Some(s) => {
                        let inner = RStringInner::from_str_scanned(&s);
                        let bytes = inner.as_bytes().to_vec();
                        let enc = inner.encoding();
                        let cr = inner.code_range();
                        (bytes, enc, cr)
                    }
                    None => (Vec::new(), Encoding::UsAscii, CodeRange::SevenBit),
                }
            } else if let Some(inner) = sep.is_rstring_inner() {
                (
                    inner.as_bytes().to_vec(),
                    inner.encoding(),
                    inner.code_range(),
                )
            } else {
                let s = sep.coerce_to_string(vm, globals)?;
                let inner = RStringInner::from_str_scanned(s.as_str());
                let bytes = inner.as_bytes().to_vec();
                let enc = inner.encoding();
                let cr = inner.code_range();
                (bytes, enc, cr)
            }
        } else {
            match get_output_field_separator(globals) {
                Some(s) => {
                    let inner = RStringInner::from_str_scanned(s.as_str());
                    let bytes = inner.as_bytes().to_vec();
                    let enc = inner.encoding();
                    let cr = inner.code_range();
                    (bytes, enc, cr)
                }
                None => (Vec::new(), Encoding::UsAscii, CodeRange::SevenBit),
            }
        };

    let mut visited: HashSet<u64> = HashSet::default();
    visited.insert(ary.id());
    // Size the buffer from the elements that are already Strings (the
    // common case is all of them) plus the separators, so the join
    // allocates once instead of growing by doubling.
    let estimate = ary
        .iter()
        .map(|v| v.is_rstring_inner().map_or(8, |s| s.len()))
        .sum::<usize>()
        + sep_bytes.len() * ary.len().saturating_sub(1);
    let mut result: Vec<u8> = Vec::with_capacity(estimate);
    // `None` means "nothing appended yet"; the first emitted
    // fragment seeds the running encoding (CRuby's left-wins
    // rule on 7-bit content).
    let mut state: Option<(Encoding, CodeRange)> = None;
    array_join(
        vm,
        globals,
        ary,
        &sep_bytes,
        sep_enc,
        sep_cr,
        &mut visited,
        &mut result,
        &mut state,
    )?;
    // The running state carries the result's code range (the fold in
    // `merge_join_state`), so the buffer is adopted as-is: no copy and
    // no re-scan of the bytes just written.
    let (final_enc, final_cr) = state.unwrap_or((Encoding::UsAscii, CodeRange::SevenBit));
    let inner = RStringInner::from_vec_cr(result, final_enc, final_cr);
    Ok(Value::string_from_inner(inner))
}

/// Get the value of $, (output field separator) as a String.
fn get_output_field_separator(globals: &mut Globals) -> Option<String> {
    let v = globals.ofs();
    if v.is_nil() {
        None
    } else {
        Some(v.to_s(&globals.store))
    }
}

/// Combine the running `(enc, cr)` state with a newly-appended
/// fragment, using the same `Encoding::compatible` rule as
/// `String#+` / `String#==`. Returns the new running state, or
/// `None` when the two are incompatible (caller raises
/// `Encoding::CompatibilityError`).
///
/// `cur` of `None` means "nothing appended yet" — the first
/// fragment's encoding becomes the seed verbatim. CRuby's
/// `Array#join` "left-wins on 7-bit content" rule depends on
/// this: seeding the running state with US-ASCII would otherwise
/// make a UTF-8 7-bit literal collapse into US-ASCII.
fn merge_join_state(
    cur: Option<(Encoding, CodeRange)>,
    cur_len: usize,
    other_enc: Encoding,
    other_cr: CodeRange,
    other_len: usize,
) -> Option<(Encoding, CodeRange)> {
    let (cur_enc, cur_cr) = match cur {
        Some(state) => state,
        None => return Some((other_enc, other_cr)),
    };
    // `enc_compatible_latter`'s two empty-string rules come before
    // any question of ASCII compatibility: an empty piece changes
    // nothing (`[utf16].join("")`), and an empty accumulator takes the
    // piece's encoding unless it is ASCII-compatible and the piece is
    // 7-bit (`["".encode("EUC-JP"), "あ"].join` is UTF-8).
    if other_len == 0 {
        return Some((cur_enc, cur_cr));
    }
    if cur_len == 0 {
        return Some(
            if cur_enc.is_ascii_compatible() && matches!(other_cr, CodeRange::SevenBit) {
                (cur_enc, other_cr)
            } else {
                (other_enc, other_cr)
            },
        );
    }
    let combined_enc = Encoding::compatible(cur_enc, cur_cr, other_enc, other_cr)?;
    // The result's code range is the fold `concatenate_string_inner`
    // uses: two well-formed sides stay well-formed, and anything
    // involving a broken or unclassified side is left for a later scan
    // — a broken piece can complete an earlier broken one (`"\xE3"` +
    // `"\x81\x82"` joins to a valid `"あ"`), so `Broken` must not be
    // recorded as final. `Encoding::compatible` only asks whether a side
    // is 7-bit, so the negotiation sees no difference.
    let combined_cr = match (cur_cr, other_cr) {
        (CodeRange::SevenBit, CodeRange::SevenBit) => CodeRange::SevenBit,
        (
            CodeRange::SevenBit | CodeRange::Valid,
            CodeRange::SevenBit | CodeRange::Valid,
        ) => CodeRange::Valid,
        _ => CodeRange::Unknown,
    };
    Some((combined_enc, combined_cr))
}

/// Append `s`'s bytes to `out`, widening `(enc, cr)` to be compatible
/// with `s`. Returns Err when the two encodings can't co-exist
/// (CRuby raises `Encoding::CompatibilityError`).
fn append_string(
    out: &mut Vec<u8>,
    state: &mut Option<(Encoding, CodeRange)>,
    s: &RStringInner,
    store: &crate::globals::Store,
) -> Result<()> {
    let new_state = match merge_join_state(
        *state,
        out.len(),
        s.encoding(),
        s.code_range(),
        s.as_bytes().len(),
    ) {
        Some(st) => st,
        None => {
            let cur_enc = state.map(|(e, _)| e).unwrap_or(Encoding::UsAscii);
            return Err(MonorubyErr::incompatible_encoding(
                store,
                cur_enc,
                s.encoding(),
            ));
        }
    };
    out.extend_from_slice(s.as_bytes());
    *state = Some(new_state);
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn array_join(
    vm: &mut Executor,
    globals: &mut Globals,
    ary: Array,
    sep: &[u8],
    sep_enc: Encoding,
    sep_cr: CodeRange,
    visited: &mut HashSet<u64>,
    out: &mut Vec<u8>,
    state: &mut Option<(Encoding, CodeRange)>,
) -> Result<()> {
    // By index, re-reading `ary` at every step as CRuby's `ary_join_1`
    // does: the conversions below run Ruby, which may resize it.
    let mut i = 0;
    while let Some(&v) = ary.get(i) {
        if i > 0 {
            // Inline-append the separator: no temporary string,
            // just merge `(sep_enc, sep_cr)` into the running
            // state and copy bytes.
            let new_state = merge_join_state(*state, out.len(), sep_enc, sep_cr, sep.len()).ok_or_else(|| {
                let cur_enc = state.map(|(e, _)| e).unwrap_or(Encoding::UsAscii);
                MonorubyErr::incompatible_encoding(&globals.store, cur_enc, sep_enc)
            })?;
            out.extend_from_slice(sep);
            *state = Some(new_state);
        }
        i += 1;
        // If element is already a string, use its raw bytes + encoding
        if let Some(inner) = v.is_rstring_inner() {
            append_string(out, state, inner, &globals.store)?;
            continue;
        }
        // If element is an array, recursively join. Conversion order:
        // to_str -> to_ary -> to_s, the first two by
        // `rb_check_string_type` / `rb_check_array_type` (an answer of
        // the wrong class other than nil is a TypeError).
        let inner_ary = if let Some(inner_ary) = v.try_array_ty() {
            Some(inner_ary)
        } else if let Some(ret) = v.check_string_type(vm, globals)? {
            append_string(out, state, ret.as_rstring_inner(), &globals.store)?;
            continue;
        } else {
            v.check_array_type(vm, globals)?
        };
        if let Some(inner_ary) = inner_ary {
            if !visited.insert(inner_ary.id()) {
                return Err(MonorubyErr::argumenterr("recursive array join"));
            }
            // The nested Array may be a fresh one an element's `#to_ary`
            // made, or one the conversions of its elements remove from
            // `ary`; either way nothing else holds it while those run, so
            // root it, or a collection frees it mid-walk.
            vm.with_temp_scope(|vm| {
                vm.temp_push(inner_ary.into());
                array_join(
                    vm, globals, inner_ary, sep, sep_enc, sep_cr, visited, out, state,
                )
            })?;
            visited.remove(&inner_ary.id());
            continue;
        }
        if let Some(fid) = globals.check_method(v, IdentId::TO_S) {
            let ret = vm.invoke_func_inner(globals, fid, v, &[], None, None)?;
            if let Some(inner) = ret.is_rstring_inner() {
                append_string(out, state, inner, &globals.store)?;
                continue;
            }
            // `to_s` returned a non-String: fall back to the
            // default-format representation (CRuby behaviour for
            // `Object#to_s` returning something weird).
            let inner = RStringInner::from_str_scanned(&v.to_s(&globals.store));
            append_string(out, state, &inner, &globals.store)?;
            continue;
        }
        // No `to_str` / `to_ary` / `to_s` defined → CRuby raises
        // NoMethodError. (Ordinary objects always inherit
        // `Object#to_s`, so this only fires when the class has
        // explicitly `undef`ed it — the spec's "raises a
        // NoMethodError if an element does not respond to ..."
        // test exercises exactly that.)
        return Err(MonorubyErr::method_not_found(
            &globals.store,
            IdentId::TO_S,
            v,
        ));
    }
    Ok(())
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
        let n = lfp.arg(0).num2long(vm, globals)?;
        if n < 0 {
            return Err(MonorubyErr::argumenterr("negative array size"));
        }
        let n = if n as usize > ary.len() {
            ary.len()
        } else {
            n as usize
        };
        Ok(Value::array_from_slice(&ary[0..n]))
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
        let n = lfp.arg(0).num2long(vm, globals)?;
        if n < 0 {
            return Err(MonorubyErr::argumenterr("negative array size"));
        }
        let n = if n as usize > ary.len() {
            0
        } else {
            ary.len() - n as usize
        };
        Ok(Value::array_from_slice(&ary[n..]))
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
    let index = lfp.arg(0).num2long(vm, globals)?;
    let resolved = ary.get_array_index(index);
    if let Some(idx) = resolved {
        if let Some(val) = ary.get(idx) {
            return Ok(*val);
        }
    }
    // Index out of bounds
    if let Some(bh) = lfp.block() {
        if lfp.try_arg(1).is_some() {
            vm.ruby_warn_caller(globals, "warning: block supersedes default value argument")?;
        }
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
    let n = lfp.arg(0).num2long(vm, globals)?;
    if n < 0 {
        return Err(MonorubyErr::argumenterr("attempt to take negative size"));
    }
    let n = n as usize;
    if n > ary.len() {
        Ok(ary.as_val().dup())
    } else {
        Ok(Value::array_from_slice(&ary[0..n]))
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
    // Fast path: no block, integer init, all-Fixnum elements, no overflow.
    // Bypasses per-element add_values dispatch for ~30x speedup on Fixnum arrays.
    if lfp.block().is_none()
        && let Some(init_i) = sum.try_fixnum()
    {
        let mut acc: i64 = init_i;
        let mut fast_ok = true;
        for v in self_.iter() {
            let Some(x) = v.try_fixnum() else {
                fast_ok = false;
                break;
            };
            let Some(new) = acc.checked_add(x) else {
                fast_ok = false;
                break;
            };
            acc = new;
        }
        if fast_ok {
            return Ok(Value::integer(acc));
        }
        // Fall through to the generic loop with the original `sum`.
    }
    // Kahan–Neumaier compensated summation while both the running sum and
    // the element are Floats, matching CRuby's precise float `sum`.
    // `comp` accumulates the lost low-order bits and is folded back in
    // whenever we leave the all-Float fast path (and at the end); it is
    // *not* updated when the running total goes non-finite, so a
    // `±Infinity`/`NaN` element propagates instead of turning the
    // `Inf - Inf` compensation into a spurious NaN. The Integer→Float
    // transition happens naturally through `add_values`.
    //
    // The array is re-read by index every step (rather than held as an
    // iterator) so a block that grows the receiver mid-sum is tolerated
    // and the appended elements are summed too — matching CRuby.
    let self_val = lfp.self_val();
    let mut comp = 0.0f64;
    let block_data = match lfp.block() {
        Some(bh) => Some(vm.get_block_data(globals, bh)?),
        None => None,
    };
    // The running total is often a fresh object that only `sum` holds (a
    // String or Array from `sum("")` / `sum([])`, a Rational, a Bignum, a
    // heap Float) while the block and `+` run Ruby: keep it on the temp
    // stack at `sum_idx`, replaced whenever it changes to another heap
    // object, or a collection there frees it.
    fn root_sum(vm: &mut Executor, sum_idx: usize, sum: Value) {
        if !sum.is_packed_value() {
            vm.temp_clear(sum_idx);
            vm.temp_push(sum);
        }
    }
    vm.with_temp_scope(|vm| {
        let sum_idx = vm.temp_len();
        vm.temp_push(sum);
        let mut i = 0;
        loop {
            let v = {
                let ary = self_val.as_array();
                if i >= ary.len() {
                    break;
                }
                ary[i]
            };
            i += 1;
            let v = match &block_data {
                Some(data) => vm.invoke_block(globals, data, &[v])?,
                None => v,
            };
            if let (Some(f), Some(x)) = (sum.try_float(), v.try_float()) {
                let t = f + x;
                if t.is_finite() {
                    comp += if f.abs() >= x.abs() {
                        (f - t) + x
                    } else {
                        (x - t) + f
                    };
                }
                sum = Value::float(t);
            } else {
                if comp != 0.0
                    && let Some(f) = sum.try_float()
                {
                    sum = Value::float(f + comp);
                    root_sum(vm, sum_idx, sum);
                }
                comp = 0.0;
                sum = executor::op::add_values(vm, globals, sum, v, false)
                    .ok_or_else(|| vm.take_error())?;
            }
            root_sum(vm, sum_idx, sum);
        }
        if comp != 0.0
            && let Some(f) = sum.try_float()
        {
            sum = Value::float(f + comp);
        }
        Ok(sum)
    })
}

///
/// The `n` of `min(n)` / `max(n)`: `#to_int` conversion and validation,
/// matching CRuby's messages (and `Range#__minmax_n` in builtins/range.rb,
/// the Ruby-side twin of this).
///
fn minmax_n(vm: &mut Executor, globals: &mut Globals, n: Value) -> Result<usize> {
    let n = n.num2long(vm, globals)?;
    if n < 0 {
        // CRuby words this one per class: `Array#min` says "negative size
        // (-1)", `Range#min` "negative array size (or size too big)".
        return Err(MonorubyErr::argumenterr(format!("negative size ({n})")));
    }
    // `rb_nmin_run` keeps a buffer of four times the count, and refuses
    // one it cannot make before it looks at any element (#1956; the
    // Ruby-side twin is `Kernel#__nmin_n`).
    if n > i64::MAX / 4 {
        return Err(MonorubyErr::argumenterr("too big size"));
    }
    if n > i64::MAX / 8 / 4 {
        return Err(MonorubyErr::argumenterr("array size too big"));
    }
    Ok(n as usize)
}

///
/// The `min(n)` / `max(n)` forms: the `n` smallest (ascending) or largest
/// (descending) elements, by CRuby's `rb_nmin_run` (enum.c), so the
/// comparator is called with the same pairs in the same order (#2014).
///
/// The elements are gathered into a buffer of `4 * n`; whenever it fills
/// it is cut down to the `n` best by a quickselect (`nmin_filter`), and
/// the worst of those becomes a limit that later elements are compared
/// against before they are let in. At the end the buffer is filtered once
/// more, sorted (in `ruby_qsort`'s order, `merge_sort`), and reversed for
/// `max`.
///
fn min_max_n(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    n: Value,
    rev: bool,
) -> Result<Value> {
    let n = minmax_n(vm, globals, n)?;
    if n == 0 {
        return Ok(Value::array_empty());
    }
    let block = match lfp.block() {
        Some(bh) => Some(vm.get_block_data(globals, bh)?),
        None => None,
    };
    let recv = lfp.self_val().as_array();
    // The buffer can hold the only reference to an element once the
    // comparator has taken it out of the receiver, so it is an Array on
    // the temp stack.
    let mut buf = Array::new_empty();
    vm.with_temp_scope(|vm| {
        vm.temp_push(buf.into());
        let mut cmp = |vm: &mut Executor, globals: &mut Globals, a: Value, b: Value| match &block {
            Some(data) => {
                let res = vm.invoke_block(globals, data, &[a, b])?;
                vm.cmpint(globals, res, a, b)
            }
            None => vm.compare_values(globals, a, b),
        };
        let bufmax = n.saturating_mul(4);
        let mut limit: Option<Value> = None;
        let mut i = 0;
        // The length is read again on every round: the comparator is Ruby
        // code and may change the receiver.
        while i < recv.len() {
            let v = recv[i];
            i += 1;
            if let Some(limit) = limit {
                let mut c = cmp(vm, globals, v, limit)?;
                if rev {
                    c = c.reverse();
                }
                if c != std::cmp::Ordering::Less {
                    continue;
                }
            }
            buf.push(v);
            if buf.len() == bufmax {
                limit = nmin_filter(vm, globals, &mut buf, n, rev, &mut cmp)?;
            }
        }
        nmin_filter(vm, globals, &mut buf, n, rev, &mut cmp)?;
        executor::op::sort_by(&mut buf, |a, b| cmp(vm, globals, a, b))?;
        let mut res: Vec<Value> = buf.to_vec();
        if rev {
            res.reverse();
        }
        Ok(Value::array_from_vec(res))
    })
}

///
/// CRuby's `nmin_filter`: when `buf` holds more than `n` elements,
/// partition it (a quickselect, with the elements equal to each pivot
/// gathered at its side) until the `n` best are in front, cut it to them
/// and answer the last pivot, the limit later elements must beat.
///
fn nmin_filter(
    vm: &mut Executor,
    globals: &mut Globals,
    buf: &mut Array,
    n: usize,
    rev: bool,
    cmp: &mut impl FnMut(&mut Executor, &mut Globals, Value, Value) -> Result<std::cmp::Ordering>,
) -> Result<Option<Value>> {
    if buf.len() <= n {
        return Ok(None);
    }
    let n = n as isize;
    let mut left: isize = 0;
    let mut right: isize = buf.len() as isize - 1;
    let store_index = loop {
        let pivot_index = left + (right - left) / 2;
        let mut num_pivots: isize = 1;
        buf.swap(pivot_index as usize, right as usize);
        let pivot_index = right;
        let mut store_index = left;
        let mut i = left;
        while i <= right - num_pivots {
            let mut c = cmp(vm, globals, buf[i as usize], buf[pivot_index as usize])?;
            if rev {
                c = c.reverse();
            }
            if c == std::cmp::Ordering::Equal {
                buf.swap(i as usize, (right - num_pivots) as usize);
                num_pivots += 1;
                continue;
            }
            if c == std::cmp::Ordering::Less {
                buf.swap(i as usize, store_index as usize);
                store_index += 1;
            }
            i += 1;
        }
        let mut j = store_index;
        let mut i = right;
        while right - num_pivots < i {
            if j <= i {
                buf.swap(j as usize, i as usize);
                j += 1;
            }
            i -= 1;
        }
        if store_index <= n && n <= store_index + num_pivots {
            break store_index;
        }
        if n < store_index {
            right = store_index - 1;
        } else {
            left = store_index + num_pivots;
        }
    };
    let limit = buf[store_index as usize];
    buf.truncate(n as usize);
    Ok(Some(limit))
}

///
/// The `min` / `max` / `minmax` scan over a homogeneous receiver: one pass
/// in Rust, no `compare_values` dispatch per element. `None` when the
/// elements are not of one class with a known `<=>`.
///
/// Ties keep the earliest element, which is what the general path does
/// (it replaces only on a *strict* improvement) and what CRuby does.
///
fn min_max_homogeneous(globals: &Globals, ary: &[Value]) -> Option<(Value, Value)> {
    use executor::op::{HomogeneousOrd, cmp_redefined, homogeneous_ord};
    // Numbers are the shape `min`/`max` actually meet, and for those the
    // key *is* the ordering — so instead of classifying the receiver in one
    // pass and scanning it in another, extract and compare in a single
    // fused pass that gives up at the first element of another class. The
    // first element says which key to try, so nothing is wasted on an
    // array that has neither shape.
    if ary[0].is_fixnum() && !cmp_redefined(globals, INTEGER_CLASS) {
        if let Some(res) = min_max_by_key(ary, |v| v.try_fixnum()) {
            return Some(res);
        }
    }
    // A `NaN` key is left out, which drops the whole array to the general
    // path — CRuby raises on the comparison rather than ordering around it.
    if ary[0].try_float().is_some() && !cmp_redefined(globals, FLOAT_CLASS) {
        if let Some(res) = min_max_by_key(ary, |v| v.try_float().filter(|f| !f.is_nan())) {
            return Some(res);
        }
    }
    // Strings order by a comparison, not by an extractable key, so they
    // keep the classify-then-scan shape.
    match homogeneous_ord(globals, ary)? {
        HomogeneousOrd::String => {
            let (mut min, mut max) = (ary[0], ary[0]);
            // SAFETY of the unwraps: `homogeneous_ord` proved every element
            // is a String.
            let cmp = |a: Value, b: Value| {
                crate::builtins::string::string_byte_then_encoding_cmp(
                    a.is_rstring_inner().unwrap(),
                    b.is_rstring_inner().unwrap(),
                )
            };
            for &v in &ary[1..] {
                if cmp(v, min) == std::cmp::Ordering::Less {
                    min = v;
                } else if cmp(v, max) == std::cmp::Ordering::Greater {
                    max = v;
                }
            }
            Some((min, max))
        }
        // Both key shapes were tried above; reaching here means the
        // redefinition check turned one down.
        HomogeneousOrd::Fixnum | HomogeneousOrd::Float => None,
    }
}

///
/// One pass over a non-empty `ary`, keeping the elements with the smallest
/// and largest key — and, on a tie, the earlier one. `None` as soon as an
/// element has no key, with nothing else read.
///
fn min_max_by_key<K: PartialOrd>(
    ary: &[Value],
    key: impl Fn(Value) -> Option<K>,
) -> Option<(Value, Value)> {
    let (mut min, mut max) = (ary[0], ary[0]);
    let mut min_k = key(min)?;
    let mut max_k = key(max)?;
    for &v in &ary[1..] {
        let k = key(v)?;
        if k < min_k {
            min_k = k;
            min = v;
        } else if k > max_k {
            max_k = k;
            max = v;
        }
    }
    Some((min, max))
}

///
/// ### Array#min
///
/// - min -> object | nil
/// - min {|a, b| ... } -> object | nil
/// - min(n) -> Array
/// - min(n) {|a, b| ... } -> Array
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/min.html]
#[monoruby_builtin]
pub(crate) fn min(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    // An explicit `nil` for `n` means "no n" — `[3,1,2].min(nil)` is 1,
    // not a TypeError.
    if let Some(n) = lfp.try_arg(0)
        && !n.is_nil()
    {
        return min_max_n(vm, globals, lfp, n, false);
    }
    let ary = lfp.self_val().as_array();
    if ary.len() == 0 {
        return Ok(Value::nil());
    }
    if let Some(bh) = lfp.block() {
        let data = vm.get_block_data(globals, bh)?;
        min_max_block(vm, globals, ary, &data, std::cmp::Ordering::Less)
    } else {
        if let Some((min, _)) = min_max_homogeneous(globals, &ary) {
            return Ok(min);
        }
        let newarray = newarray_send(vm, globals);
        min_max_generic(vm, globals, ary, true, newarray)
    }
}

///
/// CRuby's `ary_min_generic` / `ary_max_generic`: the running extreme is
/// the receiver of `<=>` and the element its argument (`vmin <=> v`), so
/// `[1, nil].max` is "comparison of Integer with nil failed" and a
/// user-defined `<=>` sees the operands in array order (#2007). With a
/// block it is the other way round — the element comes first — which is
/// what `min` / `max` above do.
///
///
/// Written on the Array literal itself — `[x, y].min` — CRuby does not
/// call the method at all (`opt_newarray_send`) and compares the other way
/// round, `v <=> vmin`: `newarray` says which form the call site is.
///
fn min_max_generic(
    vm: &mut Executor,
    globals: &mut Globals,
    ary: Array,
    is_min: bool,
    newarray: bool,
) -> Result<Value> {
    let replace_on = if is_min == newarray {
        std::cmp::Ordering::Less
    } else {
        std::cmp::Ordering::Greater
    };
    // The length is read again on every round: `<=>` is Ruby code and may
    // shrink the receiver. For the same reason the running extreme and the
    // element compared with it are kept in temp slots, not only in Rust
    // locals: either may have left the receiver (#2172).
    vm.with_temp_scope(|vm| {
        let best_slot = vm.temp_len();
        vm.temp_push(ary[0]);
        vm.temp_push(Value::nil());
        let mut i = 1;
        while i < ary.len() {
            let v = ary[i];
            vm.temp_set(best_slot + 1, v);
            let best = vm.temp_at(best_slot);
            let ord = if newarray {
                vm.compare_values(globals, v, best)?
            } else {
                vm.compare_values(globals, best, v)?
            };
            if ord == replace_on {
                vm.temp_set(best_slot, v);
            }
            i += 1;
        }
        Ok(vm.temp_at(best_slot))
    })
}

///
/// `min` / `max` with a block (`ary_min` / `ary_max`, `rb_block_given_p`):
/// the element is the block's first argument and the running extreme the
/// second, and the extreme is replaced when the answer orders the element
/// `replace_on` it. The length is read every round and the two values are
/// kept in temp slots, since the block may shrink the receiver (#2172).
///
fn min_max_block(
    vm: &mut Executor,
    globals: &mut Globals,
    ary: Array,
    data: &ProcData,
    replace_on: std::cmp::Ordering,
) -> Result<Value> {
    vm.with_temp_scope(|vm| {
        let best_slot = vm.temp_len();
        vm.temp_push(ary[0]);
        vm.temp_push(Value::nil());
        let mut i = 1;
        while i < ary.len() {
            let v = ary[i];
            vm.temp_set(best_slot + 1, v);
            let best = vm.temp_at(best_slot);
            let block_res = vm.invoke_block(globals, data, &[v, best])?;
            if vm.cmpint(globals, block_res, v, best)? == replace_on {
                vm.temp_set(best_slot, v);
            }
            i += 1;
        }
        Ok(vm.temp_at(best_slot))
    })
}

///
/// Whether the running builtin was entered from a call site of the
/// `[e1, .., en].min` shape (`CallSiteInfo::newarray_recv`). Asked only on
/// the general path of `min` / `max`, once per call.
///
fn newarray_send(vm: &Executor, globals: &Globals) -> bool {
    let cfp = vm.cfp();
    let Some(caller) = cfp.prev() else {
        return false;
    };
    let caller_lfp = caller.lfp();
    if caller_lfp.meta().is_native() {
        return false;
    }
    let store = &globals.store;
    store[caller_lfp.func_id()]
        .is_iseq()
        .and_then(|iseq| super::kernel::call_site_id(store, iseq, cfp))
        .is_some_and(|id| store[id].newarray_recv)
}

///
/// ### Array#max
///
/// - max -> object | nil
/// - max {|a, b| ... } -> object | nil
/// - max(n) -> Array
/// - max(n) {|a, b| ... } -> Array
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/max.html]
#[monoruby_builtin]
pub(crate) fn max(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    if let Some(n) = lfp.try_arg(0)
        && !n.is_nil()
    {
        return min_max_n(vm, globals, lfp, n, true);
    }
    let ary = lfp.self_val().as_array();
    if ary.len() == 0 {
        return Ok(Value::nil());
    }
    if let Some(bh) = lfp.block() {
        let data = vm.get_block_data(globals, bh)?;
        min_max_block(vm, globals, ary, &data, std::cmp::Ordering::Greater)
    } else {
        if let Some((_, max)) = min_max_homogeneous(globals, &ary) {
            return Ok(max);
        }
        let newarray = newarray_send(vm, globals);
        min_max_generic(vm, globals, ary, false, newarray)
    }
}

///
/// ### Array#minmax
///
/// - minmax -> [object, object]
/// - minmax {|a, b| ... } -> [object, object]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/minmax.html]
#[monoruby_builtin]
fn minmax(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let ary = lfp.self_val().as_array();
    if ary.len() == 0 {
        return Ok(Value::array2(Value::nil(), Value::nil()));
    }
    let Some(bh) = lfp.block() else {
        if let Some((min, max)) = min_max_homogeneous(globals, &ary) {
            return Ok(Value::array2(min, max));
        }
        // CRuby's `rb_ary_minmax` is `[ary.min, ary.max]`: two scans,
        // each with the running extreme as the receiver of `<=>` — the
        // maximum first, the order the reference build evaluates the two
        // arguments in.
        let max = min_max_generic(vm, globals, ary, false, false)?;
        let min = min_max_generic(vm, globals, ary, true, false)?;
        return Ok(Value::array2(min, max));
    };
    // With a block CRuby hands over to `Enumerable#minmax` (`minmax_ii`):
    // the elements are taken two at a time, the pair is ordered with one
    // call of the block, and then the smaller is compared with the
    // minimum and the larger with the maximum — three calls for two
    // elements, where comparing each element with both takes four.
    let data = vm.get_block_data(globals, bh)?;
    let cmp = |vm: &mut Executor, globals: &mut Globals, a: Value, b: Value| {
        let res = vm.invoke_block(globals, &data, &[a, b])?;
        vm.cmpint(globals, res, a, b)
    };
    // The running extremes and the pair being placed live in temp slots
    // (#2172): the block may drop them from the receiver, and nothing
    // else would hold them across the next block call.
    vm.with_temp_scope(|vm| {
        let (min_slot, max_slot, lo_slot, hi_slot) = {
            let base = vm.temp_len();
            for _ in 0..4 {
                vm.temp_push(Value::nil());
            }
            (base, base + 1, base + 2, base + 3)
        };
        let mut first = true;
        let mut i = 0;
        while i < ary.len() {
            let (mut lo, mut hi) = (ary[i], ary[i]);
            if i + 1 < ary.len() {
                let (j, k) = (ary[i], ary[i + 1]);
                vm.temp_set(lo_slot, j);
                vm.temp_set(hi_slot, k);
                (lo, hi) = match cmp(vm, globals, j, k)? {
                    std::cmp::Ordering::Equal => (j, j),
                    std::cmp::Ordering::Less => (j, k),
                    std::cmp::Ordering::Greater => (k, j),
                };
            }
            i += 2;
            if first {
                first = false;
                vm.temp_set(min_slot, lo);
                vm.temp_set(max_slot, hi);
                continue;
            }
            vm.temp_set(lo_slot, lo);
            vm.temp_set(hi_slot, hi);
            let min = vm.temp_at(min_slot);
            if cmp(vm, globals, lo, min)? == std::cmp::Ordering::Less {
                vm.temp_set(min_slot, lo);
            }
            let max = vm.temp_at(max_slot);
            if cmp(vm, globals, hi, max)? == std::cmp::Ordering::Greater {
                vm.temp_set(max_slot, hi);
            }
        }
        Ok(Value::array2(vm.temp_at(min_slot), vm.temp_at(max_slot)))
    })
}

pub(super) fn sort_inner(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, mut ary: Array) -> Result<Value> {
    if let Some(bh) = lfp.block() {
        let data = vm.get_block_data(globals, bh)?;
        let f = |lhs: Value, rhs: Value| -> Result<std::cmp::Ordering> {
            let block_res = vm.invoke_block(globals, &data, &[lhs, rhs])?;
            vm.cmpint(globals, block_res, lhs, rhs)
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
    sort_inner(vm, globals, lfp, ary)
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
    // `ary` is a freshly-allocated Array that is not yet referenced from
    // any GC root. Push it onto the temp_stack inside a scope so that the
    // RValue (and so every element being sorted) survives any GC
    // triggered from the comparator block, and is automatically dropped
    // when this builtin returns or unwinds.
    let ary = Array::new_from_vec(lfp.self_val().as_array().to_vec());
    vm.with_temp_scope(|vm| {
        vm.temp_push(ary.into());
        sort_inner(vm, globals, lfp, ary)
    })
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
        // Collect alternating (key, elem) pairs onto the temp_stack so the
        // freshly-allocated keys returned from the block stay reachable
        // through GC, both during the collection loop and during the sort
        // (the comparator itself dispatches back into Ruby for non-Integer
        // results, which can also trigger GC). `with_temp_scope` rolls
        // those entries back regardless of how we exit.
        vm.with_temp_scope(|vm| {
            let base = vm.temp_len();
            sort_by_collect_pairs(vm, globals, &data, lfp.self_val())?;
            let n = (vm.temp_len() - base) / 2;
            // Snapshot keys as bit patterns; they remain valid as long as
            // we do not drop the scope holding the underlying Values.
            let keys: Vec<Value> = (0..n).map(|i| vm.temp_at(base + i * 2)).collect();
            let mut indices: Vec<usize> = (0..n).collect();
            // Keys of one ordered class — the usual `sort_by { |x| x.foo }`
            // — order without running Ruby, and without the
            // error-capturing comparator closure.
            if !crate::executor::op::sort_indices_by_homogeneous_keys(
                globals, &keys, &mut indices,
            ) {
                sort_indices_by_keys(vm, globals, &keys, &mut indices)?;
            }
            // Read elems back in sorted order from the temp_stack (still
            // rooted) and write them to the receiver.
            let sorted_elems: Vec<Value> = indices
                .iter()
                .map(|&i| vm.temp_at(base + i * 2 + 1))
                .collect();
            let mut ary = lfp.self_val().as_array();
            ary.truncate(0);
            for v in sorted_elems {
                ary.push(v);
            }
            Ok(ary.into())
        })
    } else {
        array_enumerator(vm, globals, lfp.self_val(), IdentId::get_id("sort_by!"), pc)
    }
}

/// The general case of `sort_by`: order `indices` by the keys they index,
/// each pair compared as `keys[left] <=> keys[right]` — CRuby's
/// `sort_by_cmp`, which is why `[1, nil].sort_by { _1 }` reports
/// "comparison of Integer with nil failed". The keys are rooted by the
/// caller.
fn sort_indices_by_keys(
    vm: &mut Executor,
    globals: &mut Globals,
    keys: &[Value],
    indices: &mut [usize],
) -> Result<()> {
    executor::op::sort_by(indices, |a, b| {
        Executor::compare_values(vm, globals, keys[a], keys[b])
    })
}

/// Build an `Enumerator` for an `Array` iterator that has the same size
/// as the receiver: every method here yields exactly `self.length`
/// elements (`each`, `select`, `reject`, `sort_by`, …), so we attach
/// that as the size hint so `enum.size` returns the right value
/// without running the iteration. Mirrors the size proc CRuby ships
/// with each builtin's no-block branch.
fn array_enumerator(
    vm: &mut Executor,
    globals: &Globals,
    self_val: Value,
    method: IdentId,
    pc: BytecodePtr,
) -> Result<Value> {
    // The Enumerator records the name the *call site* used, so
    // `[1, 2].select` reads `…:select` rather than `…:filter` (CRuby's
    // `rb_frame_this_func()`). `method` is the definition name, and the
    // fallback wherever the call site cannot be trusted to name it.
    let method = super::kernel::invoked_name(vm, globals, self_val, method);
    let size = Value::integer(self_val.as_array().len() as i64);
    vm.generate_enumerator_with_size(method, self_val, vec![], pc, Some(size))
}

fn sort_by_collect_pairs(
    vm: &mut Executor,
    globals: &mut Globals,
    data: &ProcData,
    self_val: Value,
) -> Result<()> {
    let mut i = 0;
    loop {
        let ary = self_val.as_array();
        if i >= ary.len() {
            break;
        }
        let elem = ary[i];
        let key = vm.invoke_block(globals, data, &[elem])?;
        vm.temp_push(key);
        vm.temp_push(elem);
        i += 1;
    }
    Ok(())
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
        // Same temp_stack-rooting strategy as Array#sort_by!: keep keys
        // and elems alive across block invocations and the comparator.
        // Also fixes a latent semantic issue with the previous impl, which
        // re-evaluated the block for both sides on every comparison.
        vm.with_temp_scope(|vm| {
            let base = vm.temp_len();
            sort_by_collect_pairs(vm, globals, &data, lfp.self_val())?;
            let n = (vm.temp_len() - base) / 2;
            let keys: Vec<Value> = (0..n).map(|i| vm.temp_at(base + i * 2)).collect();
            let mut indices: Vec<usize> = (0..n).collect();
            // Keys of one ordered class — the usual `sort_by { |x| x.foo }`
            // — order without running Ruby, and without the
            // error-capturing comparator closure.
            if !crate::executor::op::sort_indices_by_homogeneous_keys(
                globals, &keys, &mut indices,
            ) {
                sort_indices_by_keys(vm, globals, &keys, &mut indices)?;
            }
            let sorted_elems: Vec<Value> = indices
                .iter()
                .map(|&i| vm.temp_at(base + i * 2 + 1))
                .collect();
            // The new Array RValue is allocated while the temp_stack scope
            // is still live, so any GC triggered by the allocation can
            // still see the sorted elements through it.
            Ok(Value::array_from_vec(sorted_elems))
        })
    } else {
        array_enumerator(vm, globals, lfp.self_val(), IdentId::get_id("sort_by"), pc)
    }
}

/// Helper for delete_if, keep_if, filter!, reject! that properly tolerates
/// array mutation during iteration and handles errors gracefully.
///
/// When `negate` is true, elements for which the block returns true are removed
/// (delete_if/reject! semantics). When false, elements for which the block
/// returns false are removed (filter!/select!/keep_if semantics).
///
/// Returns true if any elements were removed.
fn retain_with_block(
    vm: &mut Executor,
    globals: &mut Globals,
    self_val: Value,
    data: &ProcData,
    negate: bool,
) -> Result<bool> {
    let mut i = 0;
    let mut del = 0;
    loop {
        // Re-read array each iteration to see mutations from the block.
        let ary = self_val.as_array();
        if i >= ary.len() {
            break;
        }
        let val = ary[i];
        match vm.invoke_block(globals, data, &[val]) {
            Ok(res) => {
                let keep = if negate {
                    !res.as_bool()
                } else {
                    res.as_bool()
                };
                if keep {
                    if del > 0 {
                        // Re-read ary in case the block mutated it.
                        let mut ary = self_val.as_array();
                        let val = ary[i];
                        ary[i - del] = val;
                    }
                } else {
                    del += 1;
                }
                i += 1;
            }
            Err(e) => {
                // On error: compact elements processed so far, then re-raise.
                if del > 0 {
                    let mut ary = self_val.as_array();
                    let len = ary.len();
                    // Shift remaining unprocessed elements left by `del`.
                    for j in i..len {
                        let v = ary[j];
                        ary[j - del] = v;
                    }
                    ary.truncate(len - del);
                }
                return Err(e);
            }
        }
    }
    if del > 0 {
        let mut ary = self_val.as_array();
        let new_len = ary.len() - del;
        ary.truncate(new_len);
        Ok(true)
    } else {
        Ok(false)
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
        let data = vm.get_block_data(globals, bh)?;
        let changed = retain_with_block(vm, globals, lfp.self_val(), &data, false)?;
        Ok(if changed {
            lfp.self_val()
        } else {
            Value::nil()
        })
    } else {
        array_enumerator(vm, globals, lfp.self_val(), IdentId::get_id("filter!"), pc)
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
        let data = vm.get_block_data(globals, bh)?;
        retain_with_block(vm, globals, lfp.self_val(), &data, false)?;
        Ok(lfp.self_val())
    } else {
        array_enumerator(vm, globals, lfp.self_val(), IdentId::get_id("keep_if"), pc)
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
        let data = vm.get_block_data(globals, bh)?;
        let changed = retain_with_block(vm, globals, lfp.self_val(), &data, true)?;
        Ok(if changed {
            lfp.self_val()
        } else {
            Value::nil()
        })
    } else {
        array_enumerator(vm, globals, lfp.self_val(), IdentId::get_id("reject!"), pc)
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
        let data = vm.get_block_data(globals, bh)?;
        retain_with_block(vm, globals, lfp.self_val(), &data, true)?;
        Ok(lfp.self_val())
    } else {
        array_enumerator(vm, globals, lfp.self_val(), IdentId::get_id("delete_if"), pc)
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
    let self_val = lfp.self_val();
    if let Some(bh) = lfp.block() {
        let data = vm.get_block_data(globals, bh)?;
        // Root the result Hash on the temp_stack before doing any block
        // invocations so that GC can reach it (and transitively all the
        // grouped elements) regardless of what the user's block does.
        // `with_temp_scope` rolls the entry back on success or error.
        vm.with_temp_scope(|vm| {
            let h_val = Value::hash(RubyMap::default());
            vm.temp_push(h_val);
            group_by_inner(vm, globals, &data, self_val, h_val)?;
            Ok(h_val)
        })
    } else {
        array_enumerator(vm, globals, lfp.self_val(), IdentId::get_id("group_by"), pc)
    }
}

fn group_by_inner(
    vm: &mut Executor,
    globals: &mut Globals,
    data: &ProcData,
    self_val: Value,
    h_val: Value,
) -> Result<()> {
    let mut i = 0;
    while i < self_val.as_array().len() {
        let elem = self_val.as_array()[i];
        let key = vm.invoke_block(globals, data, &[elem])?;
        // The key returned by the block is held only in this Rust local
        // and is freshly allocated for many types; root it (and the
        // freshly-allocated bucket array, when needed) before the hash
        // get/insert calls, both of which dispatch back into Ruby (via
        // `hash` / `eql?`) and may trigger GC. `with_temp_scope` rolls
        // both back regardless of how the iteration ends.
        vm.with_temp_scope(|vm| -> Result<()> {
            vm.temp_push(key);
            match h_val.as_hash().get(key, vm, globals)? {
                Some(existing) => {
                    existing.as_array().push(elem);
                }
                None => {
                    let new_arr = Value::array1(elem);
                    vm.temp_push(new_arr);
                    h_val.as_hash().aset(key, new_arr, vm, globals)?;
                }
            }
            Ok(())
        })?;
        i += 1;
    }
    Ok(())
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
            vm.ruby_warn_caller(globals, "warning: given block not used")?;
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
/// ### Array#find
/// ### Array#detect
///
/// - find(ifnone = nil) {|item| ... } -> object
/// - detect(ifnone = nil) {|item| ... } -> object
/// - find(ifnone = nil) -> Enumerator
/// - detect(ifnone = nil) -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/Enumerable/i/detect.html]
#[monoruby_builtin]
fn detect(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, pc: BytecodePtr) -> Result<Value> {
    array_find(vm, globals, lfp, pc, false)
}

///
/// ### Array#rfind
///
/// - rfind(ifnone = nil) {|item| ... } -> object
/// - rfind(ifnone = nil) -> Enumerator
///
/// Like `find`/`detect`, but scans the array from the end and returns the
/// **last** matching element. (Ruby 4.0)
///
/// [https://docs.ruby-lang.org/en/master/Array.html#method-i-rfind]
#[monoruby_builtin]
fn rfind(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, pc: BytecodePtr) -> Result<Value> {
    array_find(vm, globals, lfp, pc, true)
}

///
/// Shared body of `Array#find`/`#detect` (forward) and `Array#rfind`
/// (reverse, Ruby 4.0). With no block, returns a size-less `Enumerator`
/// that replays the same method, carrying the `ifnone` argument. The
/// array length is re-read every step so a block that mutates the
/// receiver observes the current size (CRuby "rechecks the array size
/// during iteration"): the forward scan stops at the shrinking end, and
/// the reverse scan skips tail indices that no longer exist.
///
fn array_find(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    pc: BytecodePtr,
    reverse: bool,
) -> Result<Value> {
    let self_val = lfp.self_val();
    let bh = match lfp.block() {
        Some(bh) => bh,
        None => {
            // `find`/`rfind` short-circuit, so the yield count isn't
            // predictable from `self.size`; CRuby leaves the enumerator's
            // `size` at nil. Forward the `ifnone` arg if it was given.
            let method = super::kernel::invoked_name(
                vm,
                globals,
                self_val,
                IdentId::get_id(if reverse { "rfind" } else { "find" }),
            );
            let args = match lfp.try_arg(0) {
                Some(ifnone) => vec![ifnone],
                None => vec![],
            };
            return vm.generate_enumerator(method, self_val, args, pc);
        }
    };
    let data = vm.get_block_data(globals, bh)?;
    if reverse {
        // Descend from the length captured at entry; guard each index
        // against the current length so a block that shrinks the array
        // does not revisit now-missing tail elements.
        for i in (0..self_val.as_array().len()).rev() {
            if i >= self_val.as_array().len() {
                continue;
            }
            let elem = self_val.as_array()[i];
            if vm.invoke_block(globals, &data, &[elem])?.as_bool() {
                return Ok(elem);
            }
        }
    } else {
        let mut i = 0;
        while i < self_val.as_array().len() {
            let elem = self_val.as_array()[i];
            if vm.invoke_block(globals, &data, &[elem])?.as_bool() {
                return Ok(elem);
            }
            i += 1;
        }
    }
    // No element matched. CRuby: if an `ifnone` callable was given (and
    // is non-nil), call it with no arguments and return its value;
    // otherwise return nil.
    if let Some(ifnone) = lfp.try_arg(0)
        && !ifnone.is_nil()
    {
        let call = IdentId::get_id("call");
        return vm.invoke_method_inner(globals, call, ifnone, &[], None, None);
    }
    Ok(Value::nil())
}

///
/// Match value `v` against `pattern` for `Array#grep`. For Regexp patterns,
/// uses `match?` semantics (`RegexpInner::match_pred`) so `$~` /
/// `Regexp.last_match` are NOT modified (matches CRuby's grep). For other
/// patterns, falls back to `===` via `cmp_teq_values_bool`.
fn grep_match(
    vm: &mut Executor,
    globals: &mut Globals,
    pattern: Value,
    v: Value,
) -> Result<bool> {
    if pattern.is_regex().is_some() {
        // `grep_regexp_i`: an element that is neither a Symbol nor
        // convertible to a String is no match; the rest are matched as
        // `Regexp#match?` matches them, in their own encoding (#2142).
        let converted = if v.try_symbol().is_some() || v.is_rstring().is_some() {
            Some(v)
        } else {
            crate::coerce::check_funcall(vm, globals, v, IdentId::get_id("to_str"))?
                .filter(|conv| conv.is_rstring().is_some())
        };
        match converted {
            Some(subject) => super::regexp::reg_match_p(vm, globals, pattern, subject, None),
            None => Ok(false),
        }
    } else {
        cmp_teq_values_bool(vm, globals, pattern, v)
    }
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
    let pattern = lfp.arg(0);
    if let Some(bh) = lfp.block() {
        return vm.with_temp_scope(|vm| {
            let bh = vm.get_block_data(globals, bh)?;
            // Block return values accumulate into a GC-rooted Array on the
            // temp_stack so they survive subsequent invoke_block / cmp_teq calls.
            vm.temp_array_new(Some(self_val.as_array().len()));
            let mut i = 0;
            while i < self_val.as_array().len() {
                let v = self_val.as_array()[i];
                // With a block, use `===` so `$~` is set for the block to read.
                if cmp_teq_values_bool(vm, globals, pattern, v)? {
                    let mapped = vm.invoke_block(globals, &bh, &[v])?;
                    vm.temp_array_push(mapped);
                }
                i += 1;
            }
            Ok(vm.temp_pop())
        });
    }
    // The matches go into a rooted Array: `pattern === elem` and `#to_str`
    // run Ruby that may replace the receiver's elements (#2172).
    vm.with_temp_scope(|vm| {
        vm.temp_array_new(None);
        let mut i = 0;
        while i < self_val.as_array().len() {
            let v = self_val.as_array()[i];
            if grep_match(vm, globals, pattern, v)? {
                vm.temp_array_push(v);
            }
            i += 1;
        }
        Ok(vm.temp_pop())
    })
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
    let mut search = executor::op::EqSearch::new(globals, lfp.arg(0));
    let mut i = 0;
    // The length is re-read every step: an element's `==` runs Ruby, which
    // may resize the receiver.
    while i < self_val.as_array().len() {
        let elem = self_val.as_array()[i];
        if vm.rb_equal_search(globals, &mut search, elem)? {
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
    let mut ary = lfp.self_val().as_array_mut(&globals.store)?;
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
fn transpose(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let alen = self_val.as_array().len();
    if alen == 0 {
        return Ok(Value::array_empty());
    }
    // Coerce every row once, up front. CRuby calls #to_ary exactly once
    // per element; doing it inside the inner loop would invoke the mock
    // (or user-defined `to_ary`) `len * n` times. A row `#to_ary` made is
    // held by nothing else while the next one's `#to_ary` runs, so the
    // rows go into a rooted Array; and the receiver is read by index, as
    // CRuby's `rb_ary_elt` reads it (nil past the end), since a `#to_ary`
    // may resize it.
    vm.with_temp_scope(|vm| {
        vm.temp_array_new(alen);
        let rows_idx = vm.temp_len() - 1;
        for i in 0..alen {
            let v = self_val.as_array().get(i).copied().unwrap_or_default();
            let row = coerce_to_array_for_transpose(vm, globals, v)?;
            vm.temp_at(rows_idx).as_array().push(row.into());
        }
        let rows = vm.temp_at(rows_idx).as_array();
        let len = rows[0].as_array().len();
        let mut trans = Array::new_empty();
        for i in 0..len {
            let mut temp = Array::new_empty();
            for row in rows.iter() {
                let row = row.as_array();
                if row.len() != len {
                    return Err(MonorubyErr::indexerr("Element size differs."));
                }
                temp.push(row[i]);
            }
            trans.push(temp.into());
        }
        Ok(trans.into())
    })
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
    if let Some(ret) = crate::coerce::check_funcall(vm, globals, val, IdentId::TO_ARY)? {
        if let Some(ary) = ret.try_array_ty() {
            return Ok(ary);
        }
    }
    Err(MonorubyErr::typeerr(format!(
        "no implicit conversion of {} into Array",
        val.builtin_class_name(globals)
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
        arg0.num2long(vm, globals)?
    } else {
        1
    };
    let mut ary = lfp.self_val().as_array();
    let ary_len = ary.len() as i64;
    if ary_len == 0 {
    } else if i > 0 {
        ary.rotate_left(wrap_rotate_count(i, ary_len));
    } else {
        // `-i` can overflow for `i == i64::MIN`; wrap first, then negate the
        // (in-range) remainder.
        ary.rotate_right(wrap_rotate_count(i, ary_len));
    };
    Ok(lfp.self_val())
}

///
/// `|cnt| % ary_len` for a rotation count, skipping the division when `cnt`
/// is already in range.
///
/// A hardware `div` costs tens of cycles and dominated `Array#rotate!` in
/// profiles, yet the overwhelmingly common rotation is by less than the
/// array's own length (`ary.rotate!(8)` on a 16-element array), where the
/// remainder is `|cnt|` itself.
///
fn wrap_rotate_count(cnt: i64, ary_len: i64) -> usize {
    debug_assert!(ary_len > 0);
    if cnt >= 0 {
        if cnt < ary_len { cnt } else { cnt % ary_len }
    } else {
        // `cnt % ary_len` is `-(|cnt| % ary_len)` in Rust (truncating), so
        // negating the remainder gives `|cnt| % ary_len` without ever
        // evaluating `-cnt` (which overflows at `i64::MIN`).
        if cnt > -ary_len { -cnt } else { -(cnt % ary_len) }
    }
    .try_into()
    .expect("wrapped rotation count is in 0..ary_len")
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
        arg0.num2long(vm, globals)?
    } else {
        1
    };
    let mut ary: Array = Array::new_from_vec(lfp.self_val().as_array().to_vec());
    let ary_len = ary.len() as i64;
    if ary_len == 0 {
    } else if i > 0 {
        ary.rotate_left(wrap_rotate_count(i, ary_len));
    } else {
        ary.rotate_right(wrap_rotate_count(i, ary_len));
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
    // `#to_ary` may hand back fresh Arrays that only this Rust Vec (and
    // later the lazy iterator) reference; root each across the remaining
    // coercions and the block calls below.
    vm.with_temp_scope(|vm| {
        let mut lists = vec![];
        for rhs in lfp.arg(0).as_array().into_iter() {
            let list = rhs.coerce_to_array(vm, globals)?;
            vm.temp_push(list.into());
            lists.push(list);
        }
        if let Some(bh) = lfp.block() {
            // Each combination goes to the block, so the count has no
            // limit.
            let iter = product_inner(lhs, lists);
            vm.invoke_block_iter1(globals, bh, iter)?;
            return Ok(lfp.self_val());
        }
        // `rb_ary_product` without a block builds the result, so it
        // computes the length first: one that overflows a `long` is a
        // RangeError, one past the Array size limit an ArgumentError
        // (`rb_ary_new2`), and one the allocator refuses a NoMemoryError
        // — the result's own buffer, or the combinations, which are all
        // created before the next collection can run.
        // An empty list ends the count there, as in CRuby.
        let mut total: usize = lhs.len();
        for l in &lists {
            if l.len() == 0 {
                total = 0;
                break;
            }
            total = total
                .checked_mul(l.len())
                .filter(|total| *total <= i64::MAX as usize)
                .ok_or_else(|| MonorubyErr::rangeerr("too big to product"))?;
        }
        if total > ARY_MAX_SIZE {
            return Err(MonorubyErr::argumenterr("array size too big"));
        }
        if total > ARY_CAPA_PROBE
            && !(crate::builtins::kernel::array_capa_available(total)
                && crate::alloc::ALLOC.with(|alloc| alloc.borrow().has_room_for(total)))
        {
            return Err(MonorubyErr::nomemoryerr());
        }
        Ok(Value::array_from_iter(product_inner(lhs, lists)))
    })
}

fn product_inner(lhs: Array, rhs: Vec<Array>) -> impl Iterator<Item = Value> {
    let mut arrays: Vec<Array> = Vec::with_capacity(rhs.len() + 1);
    arrays.push(lhs);
    arrays.extend(rhs);

    let dims: Vec<usize> = arrays.iter().map(|a| a.len()).collect();
    let n = arrays.len();
    let total: usize = if dims.iter().any(|&d| d == 0) {
        0
    } else {
        dims.iter().product()
    };

    (0..total).map(move |mut k| {
        let mut v = vec![Value::nil(); n];
        for i in (0..n).rev() {
            let d = dims[i];
            v[i] = arrays[i][k % d];
            k /= d;
        }
        Array::new_from_vec(v).into()
    })
}

///
/// ### Array#union
///
/// - union(*other_arrays) -> Array
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/union.html]
#[monoruby_builtin]
fn union(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let lhs = lfp.self_val().as_array();
    // `#to_ary` may hand back fresh Arrays referenced only by this Rust
    // Vec; root each across the later coercions and `eql?` dispatches.
    vm.with_temp_scope(|vm| {
        let others = coerce_args_to_arrays(lfp.arg(0).as_array(), vm, globals)?;
        ary_union_multi(lhs, &others, vm, globals)
    })
}

/// `to_ary` every argument, rooting each result in the current temp
/// scope.
fn coerce_args_to_arrays(
    args: Array,
    vm: &mut Executor,
    globals: &mut Globals,
) -> Result<Vec<Array>> {
    args.iter()
        .map(|v| {
            let a = v.coerce_to_array(vm, globals)?;
            vm.temp_push(a.into());
            Ok(a)
        })
        .collect()
}

///
/// ### Array#difference
///
/// - difference(*other_arrays) -> Array
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/difference.html]
#[monoruby_builtin]
fn difference(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let lhs = lfp.self_val().as_array();
    vm.with_temp_scope(|vm| {
        let others = coerce_args_to_arrays(lfp.arg(0).as_array(), vm, globals)?;
        // CRuby (`rb_ary_difference_multi`): an argument is hashed only
        // when both it and the receiver exceed the small-array limit;
        // the rest are scanned with `eql?`.
        let mut sets = Vec::with_capacity(others.len());
        for other in &others {
            sets.push(
                if lhs.len() > SMALL_ARRAY_LEN && other.len() > SMALL_ARRAY_LEN {
                    Some(ary_make_set(*other, vm, globals)?)
                } else {
                    None
                },
            );
        }
        let mut res = Array::new_empty();
        vm.temp_push(res.into());
        let mut i = 0;
        'outer: while i < lhs.len() {
            let elt = lhs[i];
            i += 1;
            for (other, set) in others.iter().zip(&sets) {
                let found = match set {
                    Some(set) => set.contains(&elt, vm, globals)?,
                    None => ary_includes_by_eql(*other, elt, vm, globals)?,
                };
                if found {
                    continue 'outer;
                }
            }
            res.push(elt);
        }
        Ok(res.into())
    })
}

///
/// ### Array#intersection
///
/// - intersection(*other_arrays) -> Array
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/intersection.html]
#[monoruby_builtin]
fn intersection(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    // CRuby (`rb_ary_intersection_multi`): a copy of the receiver folded
    // through `&` with each argument (so no argument means no `uniq`).
    let mut res = Array::new_from_vec(lfp.self_val().as_array().to_vec());
    vm.with_temp_scope(|vm| {
        vm.temp_push(res.into());
        for arg in lfp.arg(0).as_array().iter() {
            let other = arg.coerce_to_array(vm, globals)?;
            vm.temp_push(other.into());
            res = ary_and(res, other, vm, globals)?;
            vm.temp_push(res.into());
        }
        Ok(res.into())
    })
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
    let rhs_ary = lfp.arg(0).coerce_to_array(vm, globals)?;
    // A fresh `#to_ary` result is referenced only by this Rust local;
    // root it across the `eql?` dispatches below.
    vm.with_temp_scope(|vm| {
        vm.temp_push(rhs_ary.into());
        for rhs in rhs_ary.iter().cloned() {
            for lhs in lhs.iter().cloned() {
                if lhs.eql(&rhs, vm, globals)? {
                    return Ok(Value::bool(true));
                }
            }
        }
        Ok(Value::bool(false))
    })
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
    let self_val = lfp.self_val();
    // `rb_ary_uniq`: nothing to compare with, so neither `#hash` nor the
    // block is called.
    if self_val.as_array().len() <= 1 {
        return Ok(Value::array_from_vec(self_val.as_array().to_vec()));
    }
    vm.with_temp_scope(|vm| Ok(ary_uniq_values(vm, globals, self_val, lfp.block())?.into()))
}

#[monoruby_builtin]
fn uniq_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    self_val.ensure_not_frozen(&globals.store)?;
    if self_val.as_array().len() <= 1 {
        return Ok(Value::nil());
    }
    vm.with_temp_scope(|vm| {
        let res = ary_uniq_values(vm, globals, self_val, lfp.block())?;
        // `rb_ary_uniq_bang`: the receiver as it is *now* decides — a
        // key's method or the block may have resized or frozen it.
        if self_val.as_array().len() == res.len() {
            return Ok(Value::nil());
        }
        self_val.ensure_not_frozen(&globals.store)?;
        self_val.as_array().replace(res.to_vec());
        Ok(self_val)
    })
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
    let ary = lfp.self_val().as_array_mut(&globals.store)?;
    if let Some(arg1) = lfp.try_arg(1) {
        let start = match ary.get_array_index(lfp.arg(0).num2long(vm, globals)?) {
            Some(i) => i,
            None => return Ok(Value::nil()),
        };
        let len = arg1.num2long(vm, globals)?;
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
            match ary.get_array_index(range.start().num2long(vm, globals)?) {
                Some(i) => i,
                None => return Ok(Value::nil()),
            }
        };
        // nil end means array length (endless range)
        let end = if range.end().is_nil() {
            ary.len().saturating_sub(1)
        } else {
            match ary.get_array_index(range.end().num2long(vm, globals)?) {
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
        let index = lfp.arg(0).num2long(vm, globals)?;
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
    let template = lfp.arg(0).coerce_to_rstring(vm, globals)?;
    let template = template.to_str()?;
    let buffer = lfp.try_arg(1);
    let ary = lfp.self_val().as_array();
    // `pack` roots the `'p'` / `'P'` targets on the temp stack as it
    // collects them; the scope rolls them back, including on error.
    vm.with_temp_scope(|vm| rvalue::pack(vm, globals, &ary, &template, buffer))
}

/// Interpret the value `to_ary` (or `method_missing(:to_ary)`) produced.
fn to_ary_result(v: &Value, result: Value, globals: &mut Globals) -> Result<Option<Array>> {
    if let Some(ary) = result.try_array_ty() {
        return Ok(Some(ary));
    }
    if result.is_nil() {
        return Ok(None);
    }
    Err(MonorubyErr::cant_convert_error_ary(globals, *v, result))
}

/// The element-to-array probe of `Array#flatten`: CRuby's
/// `rb_check_funcall` for `to_ary`, then the usual reading of what it
/// answered (an Array converts, nil does not, anything else is a
/// TypeError).
fn try_convert_to_array(
    v: &Value,
    vm: &mut Executor,
    globals: &mut Globals,
) -> Result<Option<Array>> {
    if let Some(ary) = v.try_array_ty() {
        return Ok(Some(ary));
    }
    match crate::value::coerce::check_funcall(vm, globals, *v, IdentId::TO_ARY)? {
        Some(result) => to_ary_result(v, result, globals),
        None => Ok(None),
    }
}

/// Flatten `ary` onto the temp stack, each element pushed in turn on top
/// of what is there.
///
/// The result is collected there rather than in a Rust `Vec`, and `ary` is
/// read by index at every step, as CRuby's `flatten` reads it: the
/// `#to_ary` probes run Ruby, which may resize `ary` or remove from it
/// elements that would then be held only by the result. The Arrays being
/// walked are kept in `roots`, an Array on the temp stack, instead, so that
/// they do not land in the result.
fn flatten_inner(
    vm: &mut Executor,
    globals: &mut Globals,
    ary: Array,
    mut roots: Array,
    lv: Option<usize>,
    changed: &mut bool,
    seen: &mut Vec<u64>,
) -> Result<()> {
    let id = ary.id();
    if seen.contains(&id) {
        return Err(MonorubyErr::argumenterr("tried to flatten recursive array"));
    }
    seen.push(id);
    let mut i = 0;
    while let Some(&v) = ary.get(i) {
        i += 1;
        // At level 0, no flattening occurs — do not call to_ary.
        if let Some(0) = lv {
            vm.temp_push(v);
            continue;
        }
        if let Some(inner) = try_convert_to_array(&v, vm, globals)? {
            *changed = true;
            // `inner` may be a fresh Array an element's `#to_ary` made
            // (ActiveRecord's `Relation#to_ary` answers `records.dup`),
            // held only here while its own elements are probed: root it,
            // or a collection there frees it under the walk.
            roots.push(inner.into());
            let res = flatten_inner(
                vm,
                globals,
                inner,
                roots,
                lv.map(|lv| lv - 1),
                changed,
                seen,
            );
            roots.pop();
            res?;
        } else {
            vm.temp_push(v);
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
            match arg0.num2long(vm, globals)? {
                i if i >= 0 => Some(i as usize),
                _ => None,
            }
        }
    } else {
        None
    };
    let mut changed = false;
    let mut seen = vec![];
    vm.with_temp_scope(|vm| {
        let roots = Value::array_empty();
        vm.temp_push(roots);
        let base = vm.temp_len();
        flatten_inner(
            vm,
            globals,
            ary,
            roots.as_array(),
            lv,
            &mut changed,
            &mut seen,
        )?;
        Ok(Value::array_from_vec(vm.temp_values_from(base)))
    })
}

///
/// ### Array#flatten!
///
/// - flatten!(lv = nil) -> self | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/flatten.html]
#[monoruby_builtin]
fn flatten_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let mut ary = lfp.self_val().as_array_mut(&globals.store)?;
    let lv = if let Some(arg0) = lfp.try_arg(0) {
        if arg0.is_nil() {
            None
        } else {
            match arg0.num2long(vm, globals)? {
                i if i >= 0 => Some(i as usize),
                _ => None,
            }
        }
    } else {
        None
    };
    let mut changed = false;
    let mut seen = vec![];
    let res = vm.with_temp_scope(|vm| {
        let roots = Value::array_empty();
        vm.temp_push(roots);
        let base = vm.temp_len();
        flatten_inner(
            vm,
            globals,
            ary,
            roots.as_array(),
            lv,
            &mut changed,
            &mut seen,
        )?;
        Ok(vm.temp_values_from(base))
    })?;
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
    let mut ary = lfp.self_val().as_array_mut(&globals.store)?;
    Ok(if ary.retain(|v| Ok(!v.is_nil()))?.is_some() {
        lfp.self_val()
    } else {
        Value::nil()
    })
}

/// The generator behind CRuby's `RAND_UPTO` for one `Array#shuffle` /
/// `#sample` call, resolved from the `random:` keyword the way
/// `try_get_rnd` does.
enum Rng {
    /// The seeded global PRNG: no `random:` at all, or `random: Random`
    /// (the class itself is CRuby's default argument, and `try_get_rnd`
    /// maps it to the default generator).
    Global,
    /// A `Random` instance, subclasses included. Its generator is driven
    /// directly rather than through `#rand` — which is what CRuby does
    /// even when a subclass redefines `#rand`, and what keeps `Random`'s
    /// seed-plus-count representation from replaying its MT once per draw.
    ///
    /// Boxed because a `RandomDraw` carries a whole Mersenne Twister
    /// state (`[u32; 624]`). Inline it would set `size_of::<Rng>()` to
    /// ~2.5KB, and `resolve` returns this by value on *every* `shuffle` /
    /// `sample` call — including the overwhelmingly common `Global` one,
    /// which would then memcpy 2.5KB of nothing per call.
    Instance(Box<super::random::RandomDraw>),
    /// Anything else, duck-typed through `#rand`. `nil` lands here too, so
    /// `random: nil` raises NoMethodError as it does in CRuby — it is a
    /// genuine argument, not a stand-in for "not given".
    ///
    /// This is the only variant that can re-enter Ruby, and so the only
    /// one under which the receiver can change length mid-call.
    Duck(Value),
}

impl Rng {
    fn resolve(vm: &mut Executor, globals: &mut Globals, random: Option<Value>) -> Result<Self> {
        let Some(random) = random else {
            return Ok(Rng::Global);
        };
        let random_class = vm.get_qualified_constant(globals, OBJECT_CLASS, &["Random"])?;
        if random.id() == random_class.id() {
            return Ok(Rng::Global);
        }
        if let Some(class) = random_class.is_class()
            && random.is_kind_of(&globals.store, class.id())
        {
            return Ok(Rng::Instance(Box::new(super::random::RandomDraw::new(
                globals, random,
            ))));
        }
        Ok(Rng::Duck(random))
    }

    /// CRuby's `RAND_UPTO(max)`: a uniform integer in `[0, max)`. `max`
    /// must be positive, as it is at every one of CRuby's draw sites.
    fn rand_upto(
        &mut self,
        vm: &mut Executor,
        globals: &mut Globals,
        max: usize,
    ) -> Result<usize> {
        debug_assert!(max > 0);
        let limit = max as u64 - 1;
        match self {
            Rng::Global => Ok(globals.random_ulong_limited(limit) as usize),
            Rng::Instance(draw) => Ok(draw.ulong_limited(limit) as usize),
            Rng::Duck(random) => duck_rand_upto(vm, globals, *random, limit as usize),
        }
    }

    /// True for the one variant whose draws run Ruby code.
    fn reenters_ruby(&self) -> bool {
        matches!(self, Rng::Duck(_))
    }

    /// Persist a `Random` instance's advanced word count. Only the
    /// `Instance` variant carries state, and its draws cannot fail, so no
    /// caller has to unwind this on the error path.
    fn finish(self, globals: &mut Globals) -> Result<()> {
        match self {
            Rng::Instance(draw) => draw.finish(globals),
            _ => Ok(()),
        }
    }
}

/// CRuby `rb_random_ulong_limited`'s duck-typed fallback for a
/// non-`Random` `random:` object: `random.rand(max + 1)`, `to_int`-coerced
/// and range-checked against `[0, max]`.
fn duck_rand_upto(
    vm: &mut Executor,
    globals: &mut Globals,
    random: Value,
    max: usize,
) -> Result<usize> {
    // `rb_funcallv_public`: `Kernel#rand` is private, so a receiver that
    // only inherits it (`random: nil`) must raise, not quietly work.
    let v = vm.invoke_method_public(
        globals,
        IdentId::get_id("rand"),
        random,
        &[Value::integer(max as i64 + 1)],
        None,
        None,
    )?;
    let r = v.num2long(vm, globals)?;
    if r < 0 {
        return Err(MonorubyErr::rangeerr(format!(
            "random number too small {r}"
        )));
    }
    if r > max as i64 {
        return Err(MonorubyErr::rangeerr(format!("random number too big {r}")));
    }
    Ok(r as usize)
}

/// Fisher-Yates over `ary` in place — the shared body of `Array#shuffle`
/// and `#shuffle!`. `random` is the `random:` keyword value, absent when
/// the caller did not pass one.
///
/// Draws use CRuby's exact `RAND_UPTO` (`rb_random_ulong_limited`), so an
/// `srand(n)`-seeded shuffle reproduces CRuby's sequence. A duck-typed
/// generator re-enters Ruby and can resize `ary` underneath us; CRuby
/// rejects that with `modified during shuffle`, and so must we before
/// indexing again.
fn shuffle_inner(
    vm: &mut Executor,
    globals: &mut Globals,
    mut ary: Array,
    random: Option<Value>,
) -> Result<()> {
    let mut rng = Rng::resolve(vm, globals, random)?;
    let guard_len = rng.reenters_ruby().then(|| ary.len());
    let mut i = ary.len();
    while i > 1 {
        i -= 1;
        let j = rng.rand_upto(vm, globals, i + 1)?;
        if let Some(len) = guard_len
            && ary.len() != len
        {
            return Err(MonorubyErr::runtimeerr("modified during shuffle"));
        }
        ary.swap(i, j);
    }
    rng.finish(globals)
}

///
/// ### Array#shuffle
///
/// - shuffle -> Array
/// - shuffle(random: Random) -> Array
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/shuffle.html]
#[monoruby_builtin]
fn shuffle(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let ary = Array::new_from_vec(lfp.self_val().as_array().to_vec());
    let random = lfp.try_arg(0);
    // Root the fresh copy across the `random:` object's `#rand`
    // dispatches; unlike the receiver it is not reachable from `lfp`.
    vm.with_temp_scope(|vm| {
        vm.temp_push(ary.into());
        shuffle_inner(vm, globals, ary, random)?;
        Ok(ary.into())
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
fn shuffle_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let ary = lfp.self_val().as_array_mut(&globals.store)?;
    shuffle_inner(vm, globals, ary, lfp.try_arg(0))?;
    Ok(lfp.self_val())
}

/// `numberof(idx)` in CRuby's `ary_sample` — the count up to which every
/// index is drawn in advance, and the boundary of its special cases.
const SAMPLE_IDX_MAX: usize = 10;

///
/// ### Array#sample
///
/// - sample(random: Random) -> object | nil
/// - sample(n, random: Random) -> Array
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/sample.html]
#[monoruby_builtin]
fn sample(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let count = lfp.try_arg(0);
    let mut rng = Rng::resolve(vm, globals, lfp.try_arg(1))?;
    let result = sample_inner(vm, globals, &mut rng, lfp.self_val().as_array(), count)?;
    rng.finish(globals)?;
    Ok(result)
}

/// CRuby's `ary_sample` (array.c), reproduced draw for draw.
///
/// The small-`n` cases are not shortcuts: `n` draws of `RAND_UPTO(len - i)`
/// are consumed either way, but each case maps them onto a *different*
/// result than the partial Fisher-Yates in the general branch, so
/// collapsing them into one loop would silently change what `sample(2)`
/// returns for a given seed. The order here is CRuby's, and so is the
/// length re-read after the draws — a duck-typed `#rand` can shrink the
/// receiver while its own indices are being rolled.
fn sample_inner(
    vm: &mut Executor,
    globals: &mut Globals,
    rng: &mut Rng,
    ary: Array,
    count: Option<Value>,
) -> Result<Value> {
    let len = ary.len();

    // No count: one element, and no draw at all for a receiver too short
    // to offer a choice.
    let Some(count) = count else {
        let i = if len < 2 {
            0
        } else {
            rng.rand_upto(vm, globals, len)?
        };
        return Ok(ary.get(i).copied().unwrap_or_default());
    };

    let n = count.num2long(vm, globals)?;
    if n < 0 {
        return Err(MonorubyErr::argumenterr("negative sample number"));
    }
    let mut n = n.min(len as i64) as usize;

    let mut rnds = [0usize; SAMPLE_IDX_MAX];
    if n <= SAMPLE_IDX_MAX {
        for (i, rnd) in rnds.iter_mut().enumerate().take(n) {
            *rnd = rng.rand_upto(vm, globals, len - i)?;
        }
    }
    // Re-read: the draws above may have run Ruby. Indices rolled against
    // the old length are simply void if the receiver shrank past them.
    let shrunk = ary.len() < len;
    let len = ary.len();
    if shrunk && n <= SAMPLE_IDX_MAX && rnds[..n].iter().any(|&r| r >= len) {
        return Ok(Value::array_empty());
    }
    n = n.min(len);

    match n {
        0 => return Ok(Value::array_empty()),
        1 => return Ok(Value::array1(ary[rnds[0]])),
        2 => {
            let i = rnds[0];
            let mut j = rnds[1];
            if j >= i {
                j += 1;
            }
            return Ok(Value::array2(ary[i], ary[j]));
        }
        3 => {
            let i = rnds[0];
            let mut j = rnds[1];
            let mut k = rnds[2];
            // Lift `j` and then `k` over the slots already taken, so that
            // three draws from shrinking ranges become three distinct
            // indices.
            let (lo, hi) = if j >= i {
                j += 1;
                (i, j)
            } else {
                (j, i)
            };
            if k >= lo {
                k += 1;
                if k >= hi {
                    k += 1;
                }
            }
            return Ok(Array::new_from_vec(vec![ary[i], ary[j], ary[k]]).into());
        }
        _ => {}
    }

    // CRuby's threshold for "n is small enough relative to len that
    // copying the whole receiver to shuffle a prefix would be wasteful".
    let memo_threshold = if len < 2560 {
        len / 128
    } else if len < 5120 {
        len / 64
    } else if len < 10240 {
        len / 32
    } else {
        len / 16
    };

    if n <= SAMPLE_IDX_MAX {
        // 4..=10 elements: insert each draw into a sorted list of the
        // indices already taken, lifting it over each one it meets, which
        // turns the draws into distinct indices without touching the
        // receiver.
        let mut idx = [0usize; SAMPLE_IDX_MAX];
        let mut sorted = [0usize; SAMPLE_IDX_MAX];
        sorted[0] = rnds[0];
        idx[0] = rnds[0];
        for i in 1..n {
            let mut k = rnds[i];
            let mut j = 0;
            while j < i && k >= sorted[j] {
                k += 1;
                j += 1;
            }
            sorted.copy_within(j..i, j + 1);
            sorted[j] = k;
            idx[i] = k;
        }
        Ok(Value::array_from_iter(idx[..n].iter().map(|&i| ary[i])))
    } else if n <= memo_threshold / 2 {
        // Still a small slice of a big receiver: run the same partial
        // Fisher-Yates, but over a sparse map of the swaps instead of a
        // full copy. `swaps[x]` is where element `x` has been moved from,
        // so the two branches return identical results.
        let mut swaps: HashMap<usize, usize> = HashMap::default();
        let mut picks = Vec::with_capacity(n);
        let mut max_idx = 0;
        for i in 0..n {
            let r = rng.rand_upto(vm, globals, len - i)? + i;
            picks.push(r);
            max_idx = max_idx.max(r);
        }
        // Same re-read as above, for the same reason.
        let len = ary.len();
        if len <= max_idx {
            n = 0;
        } else {
            n = n.min(len);
        }
        let mut result = Vec::with_capacity(n);
        for (i, &j) in picks.iter().enumerate().take(n) {
            let from_i = swaps.get(&i).copied().unwrap_or(i);
            let from_j = swaps.get(&j).copied().unwrap_or(j);
            swaps.insert(j, from_i);
            result.push(ary[from_j]);
        }
        Ok(Array::new_from_vec(result).into())
    } else {
        // n is a large enough fraction of len that a copy is the cheaper
        // way: partial Fisher-Yates over a dup, truncated to n.
        let mut result = ary.to_vec();
        for i in 0..n {
            let j = rng.rand_upto(vm, globals, len - i)? + i;
            result.swap(i, j);
        }
        result.truncate(n);
        Ok(Array::new_from_vec(result).into())
    }
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
    let recv = lfp.self_val();
    let mut ary = recv.as_array();
    let arg0 = lfp.arg(0);
    let mut search = executor::op::EqSearch::new(globals, arg0);
    let mut has_match = false;
    for i in 0..ary.len() {
        if vm.rb_equal_search(globals, &mut search, ary[i])? {
            has_match = true;
            break;
        }
    }
    if !has_match {
        return if let Some(bh) = lfp.block() {
            vm.invoke_block_once(globals, bh, &[])
        } else {
            Ok(Value::nil())
        };
    }
    recv.ensure_not_frozen(&globals.store)?;
    let f = |v: &Value| Ok(!vm.rb_equal_search(globals, &mut search, *v)?);
    Ok(ary.retain(f)?.unwrap_or(Value::nil()))
}

///
/// ### Array#delete_at
///
/// - delete_at(pos) -> object | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/delete_at.html]
#[monoruby_builtin]
fn delete_at(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let mut ary = lfp.self_val().as_array_mut(&globals.store)?;
    let pos = lfp.arg(0).num2long(vm, globals)?;
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
            vm.ruby_warn_caller(globals, "warning: given block not used")?;
        }
        // `rb_equal(element, argument)` — the *element* is the receiver of
        // `==`, as in CRuby. This used to look `==` up on the argument and
        // call it the other way round, which answers differently for any
        // asymmetric `==`.
        let mut search = executor::op::EqSearch::new(globals, arg0);
        let mut i = 0;
        while i < self_val.as_array().len() {
            let elem = self_val.as_array()[i];
            if vm.rb_equal_search(globals, &mut search, elem)? {
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
        // `find_index` short-circuits on the first match, so the
        // total yield count isn't predictable from `self.size`.
        // CRuby leaves `.size` at `nil` here — match that with the
        // size-less `generate_enumerator` instead of routing
        // through `array_enumerator`.
        vm.generate_enumerator(IdentId::get_id("find_index"), lfp.self_val(), vec![], pc)
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
    let nth = lfp.arg(0).num2long(vm, globals)?;
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
        // `rb_ary_splice`: see `check_array_growth`.
        if nth > ARY_CAPA_PROBE {
            check_array_growth(nth, nth + val.len())?;
        }
        ary.resize(nth, Value::nil());
    }
    if val.len() == 1 {
        ary.insert(nth, val[0]);
    } else {
        ary.insert_many(nth, val.iter().cloned());
    }
    Ok(lfp.self_val())
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn array_new_via_class_new() {
        // `Array` no longer overrides `self.new`; it inherits the Ruby
        // `Class#new`. Cover every argument form, the error cases, and —
        // the semantic difference from the old override, which dispatched
        // the public `allocate` — that a subclass's `allocate` override is
        // NOT called (CRuby bypasses it in `Array.new`).
        run_test(
            r#"
            class MyA < Array
              def self.allocate; $called = true; super; end
            end
            $called = false
            res = []
            res << Array.new
            res << Array.new(3)
            res << Array.new(3, :x)
            res << Array.new(3) { |i| i * 2 }
            res << (begin; Array.new(-1); rescue ArgumentError => e; e.message; end)
            res << (begin; Array.new(1, 2, 3); rescue ArgumentError => e; e.message; end)
            a = MyA.new(2, :z)
            res << [a.class.to_s, a.to_a, $called]
            res
            "#,
        );
    }

    #[test]
    fn array_new_with_block_write_barrier() {
        // `Array#initialize`'s block form installs every element at once
        // by swapping the payload of the (already rooted, and therefore
        // possibly promoted-and-armed) receiver. Without a write barrier
        // on that swap the array never enters the remembered set, a minor
        // GC skips scanning it, and all of its elements are swept while
        // still reachable — a later read then trips `Dead object`. The
        // allocation churn after the build is what forces that minor GC.
        //
        // The large sizes exist only to trigger that GC organically in a
        // normal build; under gc-stress every safepoint already collects,
        // and marking a 300k-element array per safepoint would take >10min
        // on CI, so the stress build uses a small workload instead.
        let (build, churn) = if cfg!(feature = "gc-stress") {
            (2_000, 1_000)
        } else {
            (300_000, 200_000)
        };
        run_test_once(&format!(
            r#"
            a = Array.new({build}) {{ |i| [i % 1000] }}
            {churn}.times {{ [1] }}
            a.map {{ |x| x[0] }}.sum
            "#
        ));
    }

    #[test]
    fn sort_bang_rooted_merge_buffer() {
        // `sort!`'s comparator allocates (and so collects) in the middle
        // of every merge: each element has to stay reachable from the
        // receiver throughout. (A merge used to overwrite the receiver's
        // slots while its buffer held the only reference to one run, so
        // that buffer had to be GC-visible; the merge now completes in
        // the buffer before it is copied back.)
        //
        // As above: the large size makes a GC land in that window by
        // chance in a normal build, while under gc-stress every safepoint
        // collects anyway and a large sort would run for >10min on CI.
        let n = if cfg!(feature = "gc-stress") {
            2_000
        } else {
            200_000
        };
        run_test_once(&format!(
            r#"
            a = []
            {n}.times {{ |i| a << [(i * 7919) % 1000] }}
            a.sort! {{ |x, y| [x[0]] <=> [y[0]] }}
            [a.size, a.first[0], a.last[0]]
            "#
        ));
    }

    #[test]
    fn array_literal_chunked() {
        // An Array literal longer than LITERAL_CHUNK_LEN (256 elements) is
        // built in chunks (issue #706). Elements must be non-constant: a
        // constant-only Array literal is folded into a single deep-copied
        // Literal and never reaches the chunked construction path. Cover
        // ordering across the chunk boundary and splats in chunked literals;
        // `run_test`'s warm-up loop exercises the JIT lowering (ArrayConcat).
        let elems = (0..600)
            .map(|i| format!("n + {i}"))
            .collect::<Vec<_>>()
            .join(", ");
        run_test(&format!(
            "n = 1; a = [{elems}]; [a.size, a[0], a[255], a[256], a[-1]]"
        ));
        run_test_once(&format!(
            "n = 1; x = [-1, -2]; a = [*x, {elems}, *x]; [a.size, a[0], a[1], a[2], a[-1]]"
        ));
    }

    #[test]
    fn array_literal_huge() {
        // issue #706: keep frame register usage bounded for huge literals.
        // The leading `n` keeps the literal non-constant (see above).
        let elems = std::iter::once("n".to_string())
            .chain((1..4000).map(|i| i.to_string()))
            .collect::<Vec<_>>()
            .join(", ");
        run_test_once(&format!("n = 0; a = [{elems}]; [a.size, a[3999]]"));
    }

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
        run_tests(&[
            r##"[].to_a"##,
            r##"[1,2,3].to_a"##,
        ]);
    }

    #[test]
    fn to_h() {
        run_tests(&[
            r##"[[:foo, :bar], [1, 2]].to_h"##,
            r##"["foo", "bar"].to_h {|s| [s.ord, s]}"##,
        ]);
        run_test_error(r##"[[:foo, :bar], 3].to_h"##);
        run_test_error(r##"[[:foo, :bar], [1,2,3]].to_h"##);
    }

    #[test]
    fn size() {
        run_tests(&[
            r##"[].size"##,
            r##"[].length"##,
            r##"[1,2,3].size"##,
        ]);
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
        run_tests(&[
            r##"ary = [1, 2, 4, 2.0]; ary.count"##,
            r##"ary = [1, 2, 4, 2.0]; ary.count(2)"##,
            r##"ary = [1, 2, 4, 2.0]; ary.count{|x| x % 2 == 0 }"##,
            r##"ary = [1, 2, 4, 5]; ary.count(&:even?)"##,
            r##"[].empty?"##,
            r##"[1,2,3].empty?"##,
        ]);
    }

    #[test]
    fn shift() {
        run_tests(&[
            r##"
            a = [1,2,3,4]
            [a.shift, a]
        "##,
            r##"
            a = [1,2,3,4]
            [a.shift(3), a]
        "##,
            r##"
            a = [1,2,3,4]
            [a.shift(10), a]
        "##,
            r##"[].shift"##,
            r##"[].shift(2)"##,
        ]);
        run_test_error(r##"[1,2,3].shift(:e)"##);
        run_test_error(r##"[1,2,3].shift(-2)"##);
    }

    #[test]
    fn unshift() {
        run_tests(&[
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
            r##"["a", "b"].concat ["c", "d"]"##,
            r##"["a"].concat(["b"], ["c", "d"])"##,
            r##"a = [1,2]; a.concat(a, a)"##,
            r##"[1,2,3] + [4]"##,
            r##"
        a = [1,2,3]
        res = a + [4,5,6,7,8]
        [a, res]
        "##,
            r##"[1,2,3] - [2,5]"##,
            r##"
        a = [:a,:b,:c]
        res = a - [:b,:d]
        [a, res]
        "##,
            r##"[1,2,3] * 4"##,
            r##"
        a = [1,2,3]
        res = a * 4
        [a, res]
        "##,
            r##"[] * "|""##,
            r##"[1] * "|""##,
            r##"[1,2,3,4] * "|""##,
        ]);
    }

    #[test]
    fn and() {
        run_test(r##"[] & []"##);
        run_test_error(r##"[] & 100"##);
        run_tests(&[
            r##"[1,1,2,3,5,7,2,1,7] & []"##,
            r##"[1,1,2,3,5,7,2,1,7] & [1]"##,
            r##"[1,1,2,3,5,7,2,1,7] & [1,2]"##,
            r##"[1,1,2,3,5,7,2,1,7] & [1,3,7]"##,
            r##"[1,1,2,3,5,7,2,1,7] & [1,3,7,2]"##,
        ]);
    }

    #[test]
    fn or() {
        run_test(r##"[] | []"##);
        run_test_error(r##"[] | 100"##);
        run_tests(&[
            r##"[1,1,2,3,5,7,2,1,7] | []"##,
            r##"[1,1,2,3,5,7,2,1,7] | [1]"##,
            r##"[1,1,2,3,5,7,2,1,7] | [1,2]"##,
            r##"[1,1,2,3,5,7,2,1,7] | [1,3,7]"##,
            r##"[1,1,2,3,5,7,2,1,7] | [1,3,7,2]"##,
        ]);
    }

    #[test]
    fn shl() {
        run_tests(&[
            r##"a = [1,2,3]; a << 10; a"##,
            r##"a = [1,2,3]; a.<<(10); a"##,
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
            r##"
        array = [1, 2, 3]
        array.push 4
        array.push [5, 6]
        array.push 7, 8
        array 
        "##,
            r##"["a","c"] == ["a","c",7]"##,
            r##"["a","c"] === ["a","c",7]"##,
            r##"["a","c",7] == ["a","c",7]"##,
            r##"["a","c",7] === ["a","c",7]"##,
            r##"["a","c",7] == ["a","c","7"]"##,
            r##"["a","c",7] === ["a","c","7"]"##,
            r##"["a","c",7] == "a""##,
            r##"["a","c",7] === "a""##,
            r##"[ 1, 2, 3 ] <=> [ 1, 3, 2 ] "##,
            r##"[ 1, 2, 3 ] <=> [ 1, 2, 3 ] "##,
            r##"[ 1, 2, 3 ] <=> [ 1, 2 ] "##,
            r##"[ 1, 2 ] <=> [ 1, 2, 3 ] "##,
            // Self-referencing array: a == a should return true, not stack overflow
            "a = []; a << a; a == a",
            "a = [1]; a << a; a == a",
            // Two distinct recursive arrays with same structure
            "a = [1]; a << a; b = [1]; b << b; a == b",
            // Cross-recursive: a contains b, b contains a
            "a = [1]; b = [1]; a << b; b << a; a == b",
            // Different lengths: recursive but not equal
            "a = [1]; a << a; b = [1, 2]; b << b; a == b",
            // Non-recursive element differs
            "a = [1]; a << a; b = [2]; b << b; a == b",
            // Empty self-referencing
            "a = []; a << a; b = []; b << b; a == b",
            // Self-referencing array: a <=> a should return 0, not stack overflow
            "a = []; a << a; (a <=> a)",
            "a = [1]; a << a; (a <=> a)",
            // Cross-recursive same structure
            "a = [1]; b = [1]; a << b; b << a; (a <=> b)",
            // Different lengths with recursive element — returns nil, not ArgumentError
            "a = [1]; a << a; b = [1, 2]; b << b; (a <=> b)",
            "a = [1, 2]; a << a; b = [1]; b << b; (b <=> a)",
            // Incomparable elements return nil instead of ArgumentError
            "[1, :a] <=> [1, 2]",
            "[1, 2] <=> [1, :a]",
            // Guard must be cleaned up after use: non-recursive comparison after recursive
            r#"
            a = [1]; a << a
            r1 = a == a
            r2 = [1, 2] == [1, 2]
            [r1, r2]
            "#,
            // Multiple recursive operations in sequence
            r#"
            a = []; a << a
            r1 = a == a
            r2 = (a <=> a)
            r3 = a == a
            [r1, r2, r3]
            "#,
            r##"["a", "b", "c"].eql? ["a", "b", "c"]"##,
            r##"["a", "b", "c"].eql? ["a", "c", "b"]"##,
            r##"["a", "b", 1].eql? ["a", "b", 1.0]"##,
            // Self-referencing array eql?
            "a = []; a << a; a.eql?(a)",
            "a = [1]; a << a; b = [1]; b << b; a.eql?(b)",
            // Cross-recursive via hash
            r#"
            x, y, z = [], [], []
            a = {foo: x, bar: 42}; b = {foo: y, bar: 42}; c = {foo: z, bar: 42}
            x << a; y << c; z << b
            y.eql?(z)
            "#,
        ]);
    }

    #[test]
    fn index() {
        run_tests(&[
            r##"
        a = [1,2,3];
        a[2] = 42;
        a[4] = 99;
        a[-1] = 14;
        a
        "##,
            r##"
        ary = [0, 1, 2, 3]
        ary[1, 2] = ["a", "b", "c", "d"]
        ary
        "##,
            r##"
        ary = [0, 1, 2]
        ary[5, 1] = "Z"
        ary
        "##,
            r##"
        ary = [0, 1, 2, 3]
        ary[0, 10] = ["a"]
        ary
        "##,
            r##"
        ary = [0, 1, 2, 3]
        ary[-1, 10] = ["a"]
        ary
        "##,
        ]);
        run_test_error(
            r##"
        ary = [0, 1, 2, 3]
        ary[0, -1] = ["a"]
        ary
        "##,
        );
        run_tests(&[
            r##"
        a = [1,2,3];
        a.[]=(2, 42);
        a.[]=(4,99);
        a.[]=(-2, 14);
        a
        "##,
            r##"
        a = ["a","b","c","d","e"];
        [a[0..1], a[0...1], a[0..-1], a[-2..-1], a[-2..4], a[0..10], a[10..11], a[2..1], a[-1..-2], a[5..10]]
        "##,
            r##"
        a = [ "a", "b", "c", "d", "e" ];
        [a.[](0), a.[](1), a.[](-1), a.[](-2), a.[](10)]
        "##,
            r##"
        a = [ "a", "b", "c", "d", "e" ];
        [a.[](0..1), a.[](0...1), a.[](0..-1), a.[](-2..-1), a.[](-2..4), a.[](0..10), a.[](10..11), a.[](2..1), a.[](-1..-2), a.[](5..10)]
        "##,
            r##"
        a = [ "a", "b", "c", "d", "e" ];
        [a[0, 1], a[-1, 1], a[0, 10], a[0, 0], a[0, -1], a[10, 1], a[5], a[5, 1], a[5..10]]
        "##,
            r##"
        a = [*(0..10)];
        x = (a[5..8] = [6,7,8,9,10,11])
        y = (a[2...-2] = 6)
        [a, x, y]
        "##,
            r##"
        a = [*(0..10)];
        x = (a[8..6] = 7)
        [a, x]
        "##,
            r##"
        a = [*(0..10)];
        x = (a[8..-6] = [:a,:b,:c])
        [a, x]
        "##,
        ]);
    }

    #[test]
    fn index_nil_range() {
        run_tests(&[
            // Array#[] with beginless range (nil..n)
            r##"[1,2,3,4,5][nil..2]"##,
            r##"[1,2,3,4,5][nil...2]"##,
            // Array#[] with endless range (n..nil)
            r##"[1,2,3,4,5][2..nil]"##,
            r##"[1,2,3,4,5][2...nil]"##,
            // Array#[] with nil..nil
            r##"[1,2,3,4,5][nil..nil]"##,
            // Array#[] with beginless/endless using .. syntax
            r##"[1,2,3,4,5][..2]"##,
            r##"[1,2,3,4,5][...2]"##,
            r##"[1,2,3,4,5][2..]"##,
            // Array#[]= with beginless range
            r##"
        a = [1,2,3,4,5]
        a[nil..2] = [10,20]
        a
        "##,
            // Array#[]= with endless range
            r##"
        a = [1,2,3,4,5]
        a[2..nil] = [10,20]
        a
        "##,
            // Array#[]= with nil..nil
            r##"
        a = [1,2,3,4,5]
        a[nil..nil] = [10,20]
        a
        "##,
            // values_at with beginless/endless ranges
            r##"[1,2,3,4,5].values_at(..2)"##,
            r##"[1,2,3,4,5].values_at(2..)"##,
            r##"[1,2,3,4,5].values_at(nil..nil)"##,
            // Negative index beyond array size with count returns nil
            "[1, 2, 3][-10, 2]",
            // Negative index within range works normally
            "[1, 2, 3][-2, 2]",
            // Positive index beyond array size with count returns nil
            "[1, 2, 3][4, 2]",
            // At end returns empty
            "[1, 2, 3][3, 2]",
        ]);
    }

    #[test]
    fn fill() {
        run_tests(&[
            r##"
            a = [2, 3, 4, 5]
            a.fill(100)
            a"##,
            r##"
            a = [1, 2, 3, 4]
            a.fill("a", 0..3)
            a"##,
            r##"
            a = [1, 2, 3, 4]
            a.fill("x", 1, 2)
            a"##,
            r##"
            a = [1, 2, 3, 4]
            a.fill { |i| i * 10 }
            a"##,
            r##"
            a = [1, 2, 3, 4]
            a.fill(-2) { |i| i * 10 }
            a"##,
        ]);
        // fill with no arguments fills with nil
        run_test_error(
            r#"
            a = [1, 2, 3]
            a.fill
            a
            "#,
        );
        run_tests(&[
            // fill with range (non-block form)
            r##"
            a = [1, 2, 3, 4, 5]
            a.fill("z", 1..3)
            a"##,
            // fill with block and range
            r##"
            a = [1, 2, 3, 4]
            a.fill(0..2) { |i| i * 100 }
            a"##,
            // fill extending the array
            r##"
            a = [1, 2]
            a.fill("x", 0, 5)
            a"##,
            // fill with negative start
            r##"
            a = [1, 2, 3, 4]
            a.fill("y", -3, 2)
            a"##,
            // fill with beginless range
            r##"
            a = [1, 2, 3, 4, 5]
            a.fill("x", ..2)
            a"##,
            // fill with endless range
            r##"
            a = [1, 2, 3, 4, 5]
            a.fill("x", 2..)
            a"##,
            // fill with nil..nil range
            r##"
            a = [1, 2, 3, 4, 5]
            a.fill("x", nil..nil)
            a"##,
            // fill with block and beginless range
            r##"
            a = [1, 2, 3, 4, 5]
            a.fill(..2) { |i| i * 10 }
            a"##,
            // fill(val, start, nil) means fill to end
            r##"
            a = [0, 1, 2, 3, 4]
            a.fill(:a, 1, nil)
            a"##,
            // fill with block(start, nil) means fill to end
            r##"
            a = [0, 1, 2, 3, 4]
            a.fill(1, nil) { |i| i * 10 }
            a"##,
        ]);
    }

    #[test]
    fn drop() {
        run_tests(&[
            r##"
            a = [2, 3, 4, 5]
            [a.drop(2), a.drop(100), a]
            a"##,
        ]);
    }

    #[test]
    fn zip() {
        run_tests(&[
            r##"[1,2,3].zip([4,5,6], [7,8,9])"##,
            r##"[1,2].zip([:a,:b,:c], [:A,:B,:C,:D])"##,
            r##"[1,2,3,4,5].zip([:a,:b,:c], [:A,:B,:C,:D])"##,
            r##"
            a = []
            [1,2,3].zip([4,5,6], [7,8,9]) { |ary| a << ary }
            a
        "##,
            // zip with enumerator / non-array enumerable (uses #each fallback)
            "[1, 2, 3].zip((4..6))",
            // zip with infinite enumerator (must not hang)
            "[1, 2].zip(10.upto(Float::INFINITY))",
            // zip with shorter #each-based enumerable fills nil
            r##"
            o = Object.new
            def o.each; yield 10; end
            [1, 2].zip(o)
        "##,
        ]);
        // zip raises TypeError for non-enumerable
        run_test_error(r##"[1, 2].zip(42)"##);
    }

    /// Array#flatten's element probe follows CRuby's rb_check_funcall
    /// protocol (respond_to? veto, respond_to_missing? gate, user
    /// method_missing consult, NoMethodError re-raise rule).
    #[test]
    fn flatten_check_funcall_protocol() {
        run_tests(&[
            // plain BasicObject: no probe, not converted
            "bo = BasicObject.new; [bo].flatten.equal?(nil); [[bo].flatten.size, [bo].flatten[0].equal?(bo)]",
            // a user method_missing supplies to_ary
            r##"
            bo = BasicObject.new
            def bo.method_missing(name, *) = [1, 2]
            [bo].flatten
            "##,
            // a user respond_to? returning false vetoes even that
            r##"
            bo = BasicObject.new
            def bo.method_missing(name, *) = [1, 2]
            def bo.respond_to?(name, *) = false
            r1 = [bo].flatten.size
            def bo.respond_to?(name, *) = true
            [r1, [bo].flatten]
            "##,
            // respond_to_missing? => false gates the missing-dispatch
            r##"
            o = Object.new
            def o.respond_to_missing?(name, priv) = false
            def o.method_missing(name, *) = [3]
            [[o].flatten.size, [o].flatten[0].equal?(o)]
            "##,
            // respond_to_missing? => true + user method_missing converts
            r##"
            o = Object.new
            def o.respond_to_missing?(name, priv) = true
            def o.method_missing(name, *) = name == :to_ary ? [4, 5] : super
            [o].flatten
            "##,
        ]);
        // respond_to_missing? => true with only the default
        // method_missing: the default one is not consulted, so the
        // object does not convert
        run_test(
            r##"
            o = Object.new
            def o.respond_to_missing?(name, priv) = true
            [[o].flatten.size, [o].flatten[0].equal?(o)]
            "##,
        );
        // a to_ary that returns neither Array nor nil is a TypeError
        run_test_error(
            r##"
            o = Object.new
            def o.to_ary = 42
            [o].flatten
            "##,
        );
        // a to_ary returning nil means "not convertible"
        run_test(
            r##"
            o = Object.new
            def o.to_ary = nil
            [[o].flatten.size, [o].flatten[0].equal?(o)]
            "##,
        );
        // a user respond_to? => true with a real to_ary calls it
        run_test(
            r##"
            o = Object.new
            def o.respond_to?(name, *) = true
            def o.to_ary = [7, 8]
            [o].flatten
            "##,
        );
        // a method_missing raising NoMethodError for :to_ary means
        // "not convertible" when nothing claimed the object responds
        run_test(
            r##"
            o = Object.new
            def o.method_missing(name, *) = raise(NoMethodError.new("nope", name))
            [[o].flatten.size, [o].flatten[0].equal?(o)]
            "##,
        );
    }

    /// Array.new with a block fills the receiver incrementally: break
    /// keeps the values produced so far (and returns the break value).
    #[test]
    fn initialize_block_break() {
        run_tests(&[
            r##"
            a = [1, 2, 3]
            a.send(:initialize, 3) { |i| break if i == 2; i.to_s }
            a
            "##,
            "Array.new(3) { break :bv }",
            "Array.new(3) { |i| i * 2 }",
        ]);
    }

    /// The sign of a Bignum comparator result is read natively — a
    /// redefined Integer#<=> cannot interfere (rb_cmpint).
    #[test]
    fn sort_bignum_comparator() {
        run_test(
            r##"
            class Integer
              alias old_spaceship <=>
              def <=>(other) = raise("no")
            end
            r = [1, 2, 5, 10, 7, -4, 12].sort { |n, m| (n - m) * (2 ** 200) }
            class Integer
              alias <=> old_spaceship
            end
            r
            "##,
        );
    }

    /// shuffle/sample draw from the seeded global PRNG with CRuby's
    /// exact consumption, so srand-seeded runs match CRuby.
    #[test]
    fn shuffle_sample_srand() {
        run_tests(&[
            "srand(123); %w[a b c d e f g h i j k].shuffle",
            "srand(123); a = (1..30).to_a; a.shuffle!; a",
            "srand(7); [[1,2,3,4,5,6].sample, [1,2,3].sample]",
            "%w[a b c].shuffle(random: Random.new(1))",
        ]);
    }

    /// `random:` is honoured by both `shuffle` and `shuffle!` — they share
    /// one Fisher-Yates body, so the destructive form can no longer ignore
    /// the keyword and silently draw from the global PRNG.
    #[test]
    fn shuffle_random_keyword() {
        run_tests(&[
            "(1..10).to_a.shuffle!(random: Random.new(1))",
            "[(1..10).to_a.shuffle(random: Random.new(1)), (1..10).to_a.shuffle!(random: Random.new(1))]",
            // One instance drives a whole shuffle and stays in step with
            // its own subsequent draws.
            "r = Random.new(42); [(1..20).to_a.shuffle(random: r), (1..20).to_a.shuffle(random: r), r.rand(100)]",
            "r = Random.new(42); [(1..20).to_a.shuffle!(random: r), (1..20).to_a.shuffle!(random: r), r.rand(100)]",
            // `Random` itself is CRuby's default argument: the global PRNG.
            "srand(9); [[1,2,3,4,5].shuffle(random: Random), [1,2,3,4,5].shuffle!(random: Random)]",
            "srand(9); [[1,2,3,4,5].shuffle, [1,2,3,4,5].shuffle!]",
            // A `Random` subclass is drawn from directly, so a redefined
            // `#rand` is bypassed (CRuby's `try_get_rnd`)...
            "class R < Random; def rand(n) = 0; end; [1,2,3,4,5].shuffle(random: R.new(1))",
            // ...while an unrelated object is duck-typed through `#rand`.
            "class Q; def initialize; @i = 0; end; def rand(n) = (@i += 1) % n; end; [1,2,3,4,5].shuffle(random: Q.new)",
            // `nil` is an argument, not "not given": `Kernel#rand` is
            // private, so the public-only dispatch raises.
            "begin; [1,2].shuffle(random: nil); rescue => e; [e.class, e.message]; end",
            "begin; [1,2].shuffle!(random: nil); rescue => e; [e.class, e.message]; end",
            // Out-of-range draws are rejected at both ends.
            "o = Object.new; def o.rand(n) = 999; begin; [1,2,3].shuffle(random: o); rescue => e; [e.class, e.message]; end",
            "o = Object.new; def o.rand(n) = -1; begin; [1,2,3].shuffle(random: o); rescue => e; [e.class, e.message]; end",
            "o = Object.new; def o.rand(n) = 'x'; begin; [1,2,3].shuffle(random: o); rescue => e; [e.class, e.message]; end",
            // A `#rand` that truncates to a valid index is accepted.
            "o = Object.new; def o.rand(n) = 0.9; [1,2,3].shuffle(random: o)",
            // Resizing self from inside `#rand` is caught; `shuffle` works
            // on a private copy and so is unaffected.
            r##"
            a = [1,2,3,4,5,6,7,8]
            o = Object.new
            o.define_singleton_method(:rand) { |n| a.clear; 0 }
            begin; a.shuffle!(random: o); rescue => e; [e.class, e.message]; end
            "##,
            r##"
            b = [1,2,3,4,5,6,7,8]
            o = Object.new
            o.define_singleton_method(:rand) { |n| b.clear; 0 }
            b.shuffle(random: o)
            "##,
            // Degenerate receivers never draw at all.
            "[[].shuffle, [1].shuffle, [].shuffle!(random: Random.new(1)), [1].shuffle!(random: Random.new(1))]",
            "begin; [1,2,3].freeze.shuffle!; rescue => e; [e.class, e.message]; end",
            "[1,2,3].freeze.shuffle.sort",
        ]);
    }

    #[test]
    fn zip_propagates_each_errors() {
        // Pulling from an `#each`-based argument stops on StopIteration —
        // that is exhaustion — but any *other* exception is the argument's
        // own `each` failing and must reach the caller. Swallowing it padded
        // the row with nils and returned silently wrong data instead of
        // raising, which also disguised an unrelated GC failure as a bad
        // result rather than an error.
        run_test(
            r##"
            o = Object.new
            def o.each; raise "boom"; end
            begin; [1, 2].zip(o); rescue => e; [e.class, e.message]; end
            "##,
        );
        // ...including when it raises part-way through the iteration.
        run_test(
            r##"
            o = Object.new
            def o.each; yield 1; raise ArgumentError, "mid"; end
            begin; [1, 2].zip(o); rescue => e; [e.class, e.message]; end
            "##,
        );
        // A StopIteration raised by the argument's own `each` propagates
        // too: the pull collects with `each` + break (CRuby's rb_ary_zip
        // protocol), so there is no Enumerator end-of-iteration signal to
        // confuse it with.
        run_test(
            r##"
            o = Object.new
            def o.each; yield 1; raise StopIteration; end
            begin; [1, 2, 3].zip(o); rescue => e; [e.class, e.message]; end
            "##,
        );
        // Multi-value yields pack into an Array element, single values
        // stay bare, and an empty self never invokes `each` at all.
        run_tests(&[
            r##"
            o = Object.new
            def o.each; yield 1, 2; yield 3; end
            [10, 20].zip(o)
            "##,
            r##"[].zip(1.step)"##,
        ]);
    }

    #[test]
    fn clear() {
        run_tests(&[
            r##"
            a = [2, 3, 4, 5]
            a.clear
            a"##,
            r##"[2, 3, 4, 5].inject(0) {|result, item| result + item }"##,
            r##"[2, 3, 4, 5].inject {|result, item| result + item }"##,
            r##"[2, 3, 4, 5].inject(5) {|result, item| result + item**2 }"##,
            r##"[1, 2, 3, 4, 5].inject(:+)"##,
            r##"[1, 2, 3, 4, 5].inject(10, :+)"##,
            // method name given as a String.
            r##"[10, 1, 2, 3].inject("-")"##,
            r##"[1, 2, 3].inject(10, "-")"##,
            // method name coerced with #to_str.
            r##"(n=Object.new; def n.to_str; "-"; end; [10,1,2,3].inject(n))"##,
            r##"(n=Object.new; def n.to_str; "-"; end; [1,2,3].inject(10, n))"##,
            // non-Symbol/String, no #to_str -> TypeError.
            r##"begin; [10,1,2,3].inject(Object.new); :no; rescue TypeError; :te; end"##,
            r##"begin; [1,2,3].inject(10, Object.new); :no; rescue TypeError; :te; end"##,
            // two args + block: block ignored, symbol form used.
            r##"[1, 2, 3].inject(10, :-) { raise "unused" }"##,
            // Enumerable receiver mirrors the same rules.
            r##"(class E1; include Enumerable; def each; yield 10; yield 1; yield 2; end; end; begin; E1.new.inject(Object.new); :no; rescue TypeError; :te; end)"##,
            r##"[[1,2],[3,4]].flat_map { |x| x }"##,
            r##"[1, 2, 3].flat_map { |x| x }"##,
            // block result responding to #to_ary is flattened one level.
            r##"(o=Object.new; def o.to_ary; [:a,:b]; end; [1,2].flat_map { o })"##,
            // non-Array, non-to_ary result is kept as-is.
            r##"[1, 2].flat_map { |x| x * 2 }"##,
            r##"[1, 2].collect_concat { |x| [x, x] }"##,
            // #to_ary returning non-Array, non-nil -> TypeError.
            r##"begin; o=Object.new; def o.to_ary; 5; end; [1].flat_map { o }; :no; rescue TypeError; :te; end"##,
            r##"[2, 3, 4, 5].join"##,
            r##"[2, 3, 4, 5].join("-")"##,
            // BINARY ⊔ BINARY = BINARY: rgba blob (the doom regression repro).
            // 0xFF must NOT be lossily replaced with U+FFFD.
            r##"
            s = [255, 0, 0, 255].pack("CCCC")
            v = [s, s].join
            [v.bytes, v.length, v.encoding.name]
            "##,
            // BINARY separator forces BINARY result.
            r##"
            sep = [0xff].pack("C")
            r = ["a", "b", "c"].join(sep)
            [r.bytes, r.encoding.name]
            "##,
            // BINARY ⊔ UTF-8 = BINARY (CRuby promotes to the wider, lossless one).
            r##"
            bin = [0xff, 0x80].pack("CC")
            r = [bin, "x"].join
            [r.bytes, r.encoding.name]
            "##,
            // UTF-8 (multibyte) stays UTF-8 byte-for-byte.
            r##"["あ","い","う"].join("-")"##,
            r##"["あ","い","う"].join("-").encoding.name"##,
            // Numbers (via to_s): byte content matches CRuby (don't pin encoding,
            // CRuby reports US-ASCII for ASCII-only output and we keep UTF-8).
            r##"[1,2,3].join(",")"##,
            // Single-element binary array preserves binary encoding.
            r##"[[0xff].pack("C")].join.encoding.name"##,
            // The full doom rendering pipeline shape: palette table + index map.
            r##"
            pal = [[255, 0, 0, 255].pack("CCCC"),
                   [0, 255, 0, 255].pack("CCCC"),
                   [0, 0, 255, 255].pack("CCCC")]
            indices = [0, 1, 2, 0, 1, 2]
            blob = indices.map { |i| pal[i] }.join
            [blob.bytes, blob.bytesize, blob.encoding.name]
            "##,
            // Array#* with a string argument delegates to join — same encoding rule.
            r##"
            s = [255, 0, 0, 255].pack("CCCC")
            v = [s, s] * ""
            [v.bytes, v.encoding.name]
            "##,
            r##"
            sep = [0xff].pack("C")
            r = ["a", "b"] * sep
            [r.bytes, r.encoding.name]
            "##,
            r##"[[0,1,2,3].first, [].first]"##,
            r##"
        a = [0,1,2]
        [a.first(0), a.first(1), a.first(2), a.first(3), a.first(4)]
        "##,
            r##"[[0,1,2,3].last, [].last]"##,
            r##"
        a = [0,1,2]
        [a.last(0), a.last(1), a.last(2), a.last(3), a.last(4)]
        "##,
        ]);
    }

    #[test]
    fn flat_map_gc_safety() {
        // Regression: the accumulator used to be an unrooted Rust `Vec`, so
        // every value produced by the block was invisible to a GC triggered
        // by a later iteration. Under `gc-stress` (a collection at every
        // safepoint) the returned Array was full of freed RValues — this is
        // the optcarrot palette build (`Palette.defacto_palette`) reduced.
        run_test_once(
            r##"
        src = [[1.0, 1.0, 1.0], [1.0, 0.8, 0.81], [0.78, 0.94, 0.66], [0.79, 0.77, 0.63],
               [0.82, 0.83, 1.12], [0.81, 0.71, 0.87], [0.68, 0.79, 0.79], [0.70, 0.70, 0.70]]
        res = src.flat_map do |rf, gf, bf|
          (0...64).map { |i| [(i * rf).floor, (i * gf).floor, (i * bf).floor] }
        end
        GC.start
        [res.size, res.map { |a| a.sum }.sum]
        "##,
        );
    }

    #[test]
    fn fetch() {
        run_tests(&[
            r##"[0,1,2,3].fetch(1)"##,
            r##"[0,1,2,3].fetch(-1)"##,
            r##"[0,1,2,3].fetch(-4)"##,
            r##"[0,1,2,3].fetch(4, 999)"##,
            r##"[0,1,2,3].fetch(-5, 999)"##,
            r##"[0,1,2,3].fetch(4) { |i| i * 10 }"##,
        ]);
        run_test_error(r##"[0,1,2,3].fetch(4)"##);
        run_test_error(r##"[0,1,2,3].fetch(-5)"##);
    }

    #[test]
    fn take() {
        run_test_error(r##"[0,1,2,3].take(-5)"##);
        run_test_error(r##"[0,1,2,3].take(:a)"##);
        run_tests(&[
            r##"[0,1,2,3,4,5].take(3)"##,
            r##"[0,1,2,3,4,5].take(100)"##,
            r##"[0,1,2,3,4,5].take(0)"##,
            r##"
            a = [0,1,2,3,4,5];
            [a.take(3) << 5, a]
            "##,
        ]);
    }

    #[test]
    fn sum() {
        run_test(
            r##"[[].sum, [].sum(0.0), [1, 2, 3].sum, [3, 5.5].sum, [2.5, 3.0].sum(0.0) {|e| e * e }, ["a", "b", "c"].sum("")]"##,
        );
        run_test_error("[Object.new].sum");
    }

    #[test]
    fn array_literal_minmax_fusion() {
        // The JIT elides the Array of `[a, b].min`/`.max` and compares in
        // place (`doc/escape_analysis.md`); these run hot loops so
        // the fused code path is what's being compared against CRuby.
        run_test(
            r#"
            a, b, c = 3, 9, 5
            f, g = 1.5, -2.5
            s1, s2 = "abc", "abd"
            r = nil
            100.times do
              r = [
                [a, b].min, [b, a].min, [a, b].max, [b, a].max,
                [a].min, [].min, [].max,
                [f, g].min, [f, g].max, [a, f].min, [a, f].max,
                [s1, s2].min, [s1, s2].max,
                [a, b, c].min, [a, b, c].max,
                [1.0, 1].min.class.to_s, [1, 1.0].max.class.to_s,
              ]
            end
            r
            "#,
        );
        // Incomparable elements raise; the error must fire from the
        // fused path too.
        run_test(
            r#"
            r = nil
            100.times do
              begin
                [1, "x"].min
              rescue ArgumentError
                r = :raised
              end
            end
            r
            "#,
        );
        // Non-fusable shapes keep the normal builtin: a named receiver,
        // a block, a splat.
        run_test(
            r#"
            a, b = 3, 9
            x = [b, a]
            r = nil
            100.times do
              r = [x.min, x.max, [a, b].min { |p, q| q <=> p }, [*x, 1].min]
            end
            r
            "#,
        );
        // Redefinition takes effect (class-version deopt).
        run_test_once(
            r#"
            a, b = 3, 9
            r = []
            50.times { r << [a, b].min }
            class Array
              def min = :redefined
            end
            50.times { r << [a, b].min }
            [r.first, r.last]
            "#,
        );
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
        run_tests(&[
            r##"[10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0].partition {|i| i % 3 == 0 }"##,
        ]);
    }

    #[test]
    #[cfg_attr(
        target_os = "macos",
        ignore = "CRuby's Array#sort is documented unstable (introsort tie-breaking); \
                  monoruby uses a stable merge sort. They diverge on equal Integer/\
                  Float pairs like [100, 100, 100.0] — verified against CRuby 4.0.2 \
                  as well, so it is not a Ruby-version drift. The test input is \
                  inherently order-sensitive; the Linux CI Ruby happens to land on \
                  monoruby's order for these specific values."
    )]
    fn sort() {
        run_tests(&[
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
            r##"
            a = [999999999999999999999999999999, -42.4242, 100, 100.001, -555, 0.0, 100, 76543, 100.0]
            res = a.sort
            [res, a]
            "##,
        ]);
        run_test_error("[1,:hh].sort!");
        run_test_error("[1,:hh].sort");
        run_test_error("[Float::NAN, Float::NAN].sort!");
        run_test_error("[Float::NAN, Float::NAN].sort");
        run_tests(&[
            r#"
        fruits = %w{apple pear fig}
        fruits.sort_by! { |word| word.length }
        fruits
        "#,
            r#"
        fruits = %w{apple pear fig}
        new_fruits = fruits.sort_by { |word| word.length }
        [fruits, new_fruits]
        "#,
            r##"
        ary2 = ["9", "7", "10", "11", "8"]
        [ary2.sort{|a, b| a.to_i <=> b.to_i }, ary2]
        "##,
            r##"
        ary2 = ["9", "7", "10", "11", "8"]
        [ary2.sort, ary2]
        "##,
            r##"
        ary2 = ["9", "7", "10", "11", "8"]
        ary2.sort!{|a, b| a.to_i <=> b.to_i }
        ary2
        "##,
            r##"
        ary2 = ["9", "7", "10", "11", "8"]
        ary2.sort!
        ary2
        "##,
            // Enumerable#sort (delegates to to_a.sort)
            "[1,2,3].permutation(2).to_a.sort",
            "[1,2].repeated_combination(2).to_a.sort",
            "[1,2].repeated_permutation(2).to_a.sort",
            "[3,1,2].each.sort { |a,b| b <=> a }",
        ]);
    }

    #[test]
    fn group_by() {
        run_tests(&[
            r##"
            [*(1..6)].group_by {|i| i%3}
        "##,
            // GC-rooting regression: when keys and bucket arrays returned by
            // the block are heap-allocated, every block invocation can
            // trigger a collection (under `--features gc-stress`). Previously
            // the partial result Hash was held only in a Rust local and the
            // builtin worked around it by disabling GC; the rooted version
            // must keep both the Hash and freshly-allocated bucket arrays
            // alive throughout.
            r##"
            (1..50).to_a.group_by { |i| "k#{i % 7}" }
        "##,
            // GC-rooting regression: each `to_s` call returns a freshly
            // allocated heap String. Under `--features gc-stress` the old
            // `pairs: Vec<(Value, Value)>` collection loop would let GC
            // reclaim earlier keys before sorting saw them.
            r##"
            (1..50).to_a.sort_by { |i| i.to_s }
        "##,
            r##"
            ary = (1..50).to_a
            ary.sort_by! { |i| (i * 31).to_s }
            ary
        "##,
            // sort with comparator block where the block also allocates;
            // exercises the temp_push of the freshly-built copy in
            // Array#sort.
            r##"
            ary = (1..30).to_a.shuffle(random: Random.new(42))
            ary.sort { |a, b| a.to_s <=> b.to_s }
        "##,
            r##"
        x = 100
        [2, 3, 4, 5].each do |y|
          x += y
        end
        x
        "##,
            r##"
        x = []
        [2, 3, 4, 5].reverse_each do |y|
          x << y
        end
        x
        "##,
            r##"
        x = 100
        [2, 3, 4, 5].each_with_index do |item, index|
          x += item * 7 + index
        end
        x
        "##,
            r##"[1,2,3,4,5].select { |num| num.even? }"##,
            r##"[1,2,3,4,5].select(&:even?)"##,
            r##"[1,2,3,4,5].reject { |num| num.even? }"##,
            r##"[1,2,3,4,5].reject { true }"##,
            r##"[1,2,3,4,5].reject(&:even?)"##,
            r##"a=[1,2,3,4,5]; a.reject! { |num| num.even? }; a"##,
            r##"a=[1,2,3,4,5]; a.reject! { true }; a"##,
            r##"a=[1,2,3,4,5]; a.reject! { false }; a"##,
            r##"a=[1,2,3,4,5]; a.reject!(&:even?); a"##,
            r##"a=[1,2,3,4,5]; a.delete_if { |num| num.even? }; a"##,
            r##"a=[1,2,3,4,5]; a.delete_if { true }; a"##,
            r##"a=[1,2,3,4,5]; a.delete_if { false }; a"##,
            r##"a=[1,2,3,4,5]; a.delete_if(&:even?); a"##,
            r##"
        a = %w{ a b c d e f }
        b = a.select! {|v| v =~ /[a-z]/ }   # => nil
        [a, b] # => [["a", "b", "c", "d", "e", "f"], nil]
        "##,
            r##"
        a = %w{ a b c d e f }
        b = a.select! {|v| v =~ /[c-e]/ }
        [a, b]
        "##,
            r##"
        x = 10
        a = [2, 3, 4, 5, 6, 7, 8]
        res = a.map do |y|
          x + y
        end
        [res, a]
        "##,
            r##"
        x = 10
        a = [2, 3, 4, 5, 6, 7, 8]
        res = a.map! do |y|
          x + y
        end
        [res, a]
        "##,
            // Size == receiver length for the "yield once per element"
            // family. `run_tests` compares each expression's result
            // against CRuby.
            "[1,2,3,4].each.class",
            "[1,2,3,4].each.size",
            "[1,2,3,4].reverse_each.class",
            "[1,2,3,4].reverse_each.size",
            "[1,2,3,4].each_with_index.class",
            "[1,2,3,4].each_with_index.size",
            "[1,2,3,4].each_index.class",
            "[1,2,3,4].each_index.size",
            "[1,2,3,4].map.class",
            "[1,2,3,4].map.size",
            "[1,2,3,4].collect.size",
            "[1,2,3,4].map!.class",
            "[1,2,3,4].map!.size",
            "[1,2,3,4].collect!.size",
            "[1,2,3,4].select.class",
            "[1,2,3,4].select.size",
            "[1,2,3,4].filter.size",
            "[1,2,3,4].find_all.size",
            "[1,2,3,4].select!.class",
            "[1,2,3,4].select!.size",
            "[1,2,3,4].filter!.size",
            "[1,2,3,4].reject.class",
            "[1,2,3,4].reject.size",
            "[1,2,3,4].reject!.class",
            "[1,2,3,4].reject!.size",
            "[1,2,3,4].delete_if.class",
            "[1,2,3,4].delete_if.size",
            "[1,2,3,4].keep_if.class",
            "[1,2,3,4].keep_if.size",
            "[1,2,3,4].sort_by.class",
            "[1,2,3,4].sort_by.size",
            "[1,2,3,4].sort_by!.class",
            "[1,2,3,4].sort_by!.size",
            "[1,2,3,4].group_by.class",
            "[1,2,3,4].group_by.size",
            "[1,2,3,4].filter_map.class",
            "[1,2,3,4].filter_map.size",
            // `find_index` short-circuits, so CRuby reports `nil` size.
            "[1,2,3,4].find_index.size",
            // Empty receiver still returns an Enumerator with size 0.
            "[].each.size",
            "[].map.size",
            "[].select.size",
            // combination / repeated_combination: binomial coefficients
            // with the CRuby edge cases (k < 0 ⇒ 0, k == 0 ⇒ 1, k > size
            // ⇒ 0 for combination but k > size is valid for
            // repeated_combination).
            // C(4, k)
            "[1,2,3,4].combination(0).size",
            // => 1
            "[1,2,3,4].combination(1).size",
            // => 4
            "[1,2,3,4].combination(2).size",
            // => 6
            "[1,2,3,4].combination(3).size",
            // => 4
            "[1,2,3,4].combination(4).size",
            // => 1
            "[1,2,3,4].combination(5).size",
            // => 0 (k > size)
            "[1,2,3,4].combination(-1).size",
            // => 0
            "[].combination(0).size",
            // => 1
            "[].combination(1).size",
            // => 0
            // repeated_combination: C(size + k - 1, k)
            "[1,2,3].repeated_combination(0).size",
            // => 1
            "[1,2,3].repeated_combination(1).size",
            // => 3
            "[1,2,3].repeated_combination(2).size",
            // => 6
            "[1,2,3].repeated_combination(3).size",
            // => 10
            "[1,2,3].repeated_combination(-1).size",
        ]);
    }

    #[test]
    fn group_by_2() {
        run_tests(&[
            // => 0
            "[].repeated_combination(0).size",
            // => 1
            "[].repeated_combination(1).size",
            // permutation / repeated_permutation: descending factorial /
            // size**k. `Array#permutation` argument is optional (defaults
            // to size).
            // P(4, k)
            "[1,2,3,4].permutation.size",
            // => 24
            "[1,2,3,4].permutation(0).size",
            // => 1
            "[1,2,3,4].permutation(1).size",
            // => 4
            "[1,2,3,4].permutation(2).size",
            // => 12
            "[1,2,3,4].permutation(3).size",
            // => 24
            "[1,2,3,4].permutation(4).size",
            // => 24
            "[1,2,3,4].permutation(5).size",
            // => 0 (k > size)
            "[1,2,3,4].permutation(-1).size",
            // => 0
            "[].permutation(0).size",
            // => 1
            "[].permutation(2).size",
            // => 0
            // repeated_permutation: size ** k
            "[1,2,3].repeated_permutation(0).size",
            // => 1
            "[1,2,3].repeated_permutation(1).size",
            // => 3
            "[1,2,3].repeated_permutation(2).size",
            // => 9
            "[1,2,3].repeated_permutation(4).size",
            // => 81
            "[1,2,3].repeated_permutation(-1).size",
            // => 0
            "[].repeated_permutation(0).size",
            // => 1
            "[].repeated_permutation(2).size",
            // Block-less Enumerator round-trips: `.each` should iterate
            // back to the original array so the size and yields stay in
            // sync — guards against the size hint accidentally short-
            // circuiting the actual iteration.
            "[1,2,3].each.to_a",
            "[1,2,3].map.each.to_a",
            "[1,2,3].select.to_a",
            "[1,2,3].combination(2).to_a",
            "[1,2,3].permutation(2).to_a.sort",
            r#"[5,  6, 7].all? {|v| v > 0 }"#,
            r#"[5, -1, 7].all? {|v| v > 0 }"#,
            r#"[5, -1, 7].all?"#,
            r#"[5, nil, 7].all?"#,
            r#"[5, -1, false].all?"#,
            r#"[nil, false].all?"#,
            r#"[1, 2, 3].all?(Integer)"#,
            r#"[1, "a", 3].all?(Integer)"#,
            r#"[].all?(Integer)"#,
            r#"[5,  6, 7].any? {|v| v > 0 }"#,
            r#"[5, -1, 7].any? {|v| v > 0 }"#,
            r#"[5, -1, 7].any?"#,
            r#"[5, nil, 7].any?"#,
            r#"[5, -1, false].any?"#,
            r#"[nil, false].any?"#,
            r#"[1, "a", 3].any?(String)"#,
            r#"[1, 2, 3].any?(String)"#,
            r##"
            a = [1, 2, 3, 4, 5]
            [a.pop(2), a]"##,
            r##"
            a = [1, 2, 3]
            [a.pop(0), a]"##,
            r##"
            a = [1, 2, 3]
            [a.pop(10), a]"##,
            r#"[1, 2, 3, 4, 5].find {|i| i % 3 == 0 }"#,
            r#"[2, 2, 2, 2, 2].find {|i| i % 3 == 0 }"#,
            r#"['aa', 'bb', 'cc', 'dd', 'ee'].grep(/[bc]/)"#,
            //run_test(r#"Array.instance_methods.grep(/gr/)"#);
            r#"['aa', 'bb', 'cc', 'dd', 'ee'].grep(/[bc]/) {|s| s.upcase }"#,
            r#"[1, 2, 3, 4, 5].grep(Integer) {|n| n * 2 }"#,
            r#"[1, 'a', 2, 'b'].grep(String)"#,
        ]);
    }

    #[test]
    fn detect_find_ifnone() {
        run_tests(&[
            // ifnone proc is called (with no args) when nothing matches.
            r#"[].find(-> { "yay" }) { |e| true }"#,
            r#"[1, 2, 3].detect(-> { 99 }) { |e| e > 5 }"#,
            // A matching element short-circuits before ifnone.
            r#"[1, 2, 3].find(-> { 99 }) { |e| e == 2 }"#,
            // nil ifnone behaves like the no-arg form (returns nil).
            r#"[1, 2, 3].find(nil) { |e| e > 5 }.inspect"#,
            r#"[].find { |e| true }.inspect"#,
        ]);
    }

    #[test]
    fn rfind() {
        run_tests(&[
            // Scans from the end, returns the last match.
            r#"[1, 2, 3, 4, 5].rfind { |x| x % 2 == 0 }"#,
            r#"[1, 2, 3].rfind { |x| false }.inspect"#,
            // Reverse iteration order and short-circuit from the end.
            r#"v = []; [1, 2, 3].rfind { |x| v << x; false }; v"#,
            r#"v = []; [1, 2, 3, 4, 5].rfind { |x| v << x; x == 3 }; v"#,
            // ifnone proc: called (once) only when nothing matches.
            r#"[1, 2, 3].rfind(-> { "z" }) { |x| false }"#,
            r#"[1, 2, 3].rfind(-> { raise }) { |x| x == 3 }"#,
            r#"[1, 2, 3].rfind(nil) { |x| false }.inspect"#,
            // Does not destructure array elements.
            r#"[[1, 2], [3, 4]].rfind { |e| e == [1, 2] }"#,
            // Rechecks the array size: clearing mid-iteration stops it.
            r#"a = [4, 2, 1, 5, 1, 3]; seen = []; a.rfind { |x| seen << x; a.clear; false }; seen"#,
        ]);
    }

    #[test]
    fn find_rfind_no_block_enumerator() {
        run_tests(&[
            // No block → Enumerator (size nil), replaying find/rfind.
            r#"[1, 2, 3].find.class.name"#,
            r#"[1, 2, 3].rfind.class.name"#,
            r#"[1, 2, 3].find.size.inspect"#,
            r#"[1, 2, 3].find { |x| x == 2 }"#,
            r#"[1, 2, 3].rfind.each { |x| x == 2 }"#,
            // The ifnone proc is carried by the enumerator.
            r#"[1, 2, 3].rfind(-> { "z" }).each { |x| false }"#,
            r#"[1, 2, 3].find(-> { "z" }).each { |x| false }"#,
        ]);
    }

    #[test]
    fn each_with_object_enumerator_forwards_memo() {
        // Without a block, `each_with_object(memo)` returns an
        // Enumerator that still threads `memo` through `.each`.
        run_test(
            r#"
            acc = []
            initial = []
            e = [1, 2, 3].each_with_object(initial)
            r = e.each { |elem, obj| acc << elem }
            [r.equal?(initial), acc]
            "#,
        );
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
        run_tests(&[
            r#"
            a = [1, 2, 3, 4, 5]
            [a.reverse, a]
        "#,
            r#"
            a = [1, 2, 3, 4, 5]
            a.reverse!
            a
        "#,
        ]);
    }

    #[test]
    fn transpose() {
        run_tests(&[
            r#"[[1,2],[3,4],[5,6]].transpose"#,
            r#"[].transpose"#,
        ]);
        run_test_error(r#"[1,2,3].transpose"#);
        run_test_error(r#"[[1,2],[3,4,5],[6,7]].transpose"#);
    }

    #[test]
    fn rotate() {
        run_tests(&[
            r#"
        a = ["a","b","c","d"]
        [a.rotate, a]
        "#,
            r#"
        a = ["a","b","c","d"]
        [a.rotate(2), a]
        "#,
            r#"
        a = ["a","b","c","d"]
        [a.rotate(-3), a]
        "#,
            r#"
        a = []
        b = a.rotate
        [a, b, a.object_id == b.object_id]
        "#,
            r#"
        a = ["a","b","c","d"]
        [a.rotate!, a]
        "#,
            r#"
        a = ["a","b","c","d"]
        [a.rotate!(2), a]
        "#,
            r#"
        a = ["a","b","c","d"]
        [a.rotate!(-3), a]
        "#,
            r#"
        a = []
        b = a.rotate!
        [a, b, a.object_id == b.object_id]
        "#,
        ]);
    }

    #[test]
    fn product() {
        run_tests(&[
            r#"[1,2,3].product([4,5])"#,
            r#"[1,2].product([1,2])"#,
            r#"[1,2].product([3,4],[5,6])"#,
            r#"[1,2].product()"#,
            r#"[1,2].product([])"#,
            r#"
            a = []
            [1,2,3].product([4,5]) {|e| a << e} # => [1,2,3]
            a # => [[1,4],[1,5],[2,4],[2,5],[3,4],[3,5]]
            "#,
        ]);
        run_test_error(r#"[1,2].product(1,2,3)"#);
    }

    #[test]
    fn hash_mixes_element_hash_value() {
        run_tests(&[
            // Array#hash sends `#hash` to each element and mixes the resulting
            // integer, so an element whose `#hash` returns another object is
            // coerced via `#to_int` and hashes identically to the underlying
            // value. A numeric element must therefore mix its `Integer#hash` /
            // `Float#hash` digest (of the value), not its raw tagged bits.
            r#"
            x = 1.hash
            o = Object.new
            o.define_singleton_method(:hash) { x }
            [1].hash == [o].hash
            "#,
            r#"
            x = 2.5.hash
            o = Object.new
            o.define_singleton_method(:hash) { x }
            [2.5].hash == [o].hash
            "#,
            // Structural equality still implies hash equality for numeric content.
            r#"[[1, 2.0, 3].hash == [1, 2.0, 3].hash, [1].hash != [2].hash]"#,
        ]);
    }

    #[test]
    fn product_coerces_arg_via_method_missing() {
        // An argument that answers `#to_ary` only through `method_missing`
        // (no defined `to_ary`) is still coerced, matching CRuby.
        run_test(
            r#"
            ar = Object.new
            def ar.method_missing(name, *a); name == :to_ary ? [2, 3] : super; end
            [1].product(ar)
            "#,
        );
        // A non-coercible argument is still a TypeError.
        run_test(r#"begin; [1].product(5); rescue TypeError; :te; end"#);
        // An argument whose `#to_ary` returns a non-Array raises a TypeError.
        run_test(
            r#"ar = Object.new; def ar.to_ary; 5; end; begin; [1].product(ar); rescue TypeError; :te2; end"#,
        );
    }

    #[test]
    fn intersect() {
        run_tests(&[
            r#"
            a = [ "1", "2", "3" ]
            b = [ "3", "4", "5" ]
            c = [ "5", "6", "7" ]
            [
                a.intersect?(b),  # => true
                a.intersect?(c)   # => false
            ]
        "#,
            r#"["a", "b", "c"].intersect?([ "c", "d", "e" ])"#,
            r#"["a", "b", "c"].intersect?([ "d", "e", "f" ])"#,
        ]);
    }

    #[test]
    fn union() {
        run_tests(&[
            r#"["a", "b", "c"].union([ "c", "d", "a" ])"#,
            r#"["a"].union(["e", "b"], ["a", "c", "b"])"#,
            r#"["a"].union"#,
        ]);
        run_test_error(r#"["a"].union([], 100)"#);
    }

    #[test]
    fn uniq() {
        run_tests(&[
            r#"
        a = [1, 3, 2, 2.0, "2", "3", 3]
        b = a.uniq
        [a, b]
        "#,
            r#"
        a = [1, 3, 2, 2.0, "2", "3", 3]
        b = a.uniq {|n| n.to_s }
        [a, b]
        "#,
            r#"
        a = [1, 3, 2, 2.0, "2", "3", 3]
        a.uniq!
        a
        "#,
            r#"
        a = [1, 3, 2, 2.0, "2", "3", 3]
        a.uniq! {|n| n.to_s }
        a
        "#,
            r#"[*(0..20)].shuffle!.shuffle!().sum"#,
            r#"
        a = [ "a", "b", "c", "d", "e", "f", "g", "h" ]
        b = a.slice!(2, 3)
        [a, b]
        "#,
            r#"
        a = [ "a", "b", "c", "d", "e", "f", "g", "h" ]
        b = a.slice!(-5, 3)
        [a, b]
        "#,
            r#"
        a = [ "a", "b", "c", "d", "e", "f", "g", "h" ]
        b = a.slice!(-200, 3)
        [a, b]
        "#,
            r#"
        a = [ "a", "b", "c", "d", "e", "f", "g", "h" ]
        b = a.slice!(2, -200)
        [a, b]
        "#,
            r#"
        a = [ "a", "b", "c", "d", "e", "f", "g", "h" ]
        b = a.slice!(2..3)
        [a, b]
        "#,
            r#"
        a = [ "a", "b", "c", "d", "e", "f", "g", "h" ]
        b = a.slice!(2...5)
        [a, b]
        "#,
            r#"
        a = [ "a", "b", "c", "d", "e", "f", "g", "h" ]
        b = a.slice!(-100..3)
        [a, b]
        "#,
            r#"
        a = [ "a", "b", "c", "d", "e", "f", "g", "h" ]
        b = a.slice!(2..-100)
        [a, b]
        "#,
            r#"
        a = [ "a", "b", "c", "d", "e", "f", "g", "h" ]
        b = a.slice!(2..1)
        [a, b]
        "#,
            r#"
        a = [ "a", "b", "c", "d", "e", "f", "g", "h" ]
        b = a.slice!(-100..-30)
        [a, b]
        "#,
            // slice! with beginless range
            r#"
        a = [ "a", "b", "c", "d", "e" ]
        b = a.slice!(..2)
        [a, b]
        "#,
            // slice! with endless range
            r#"
        a = [ "a", "b", "c", "d", "e" ]
        b = a.slice!(2..)
        [a, b]
        "#,
            // slice! with nil..nil
            r#"
        a = [ "a", "b", "c", "d", "e" ]
        b = a.slice!(nil..nil)
        [a, b]
        "#,
            // `Array#[]` and `Array#slice` accept an
            // `Enumerator::ArithmeticSequence` as a stride-aware index
            // (`runtime::get_index` / `builtins::array::index` dispatch
            // to `aseq.__slice_array` instead of `coerce_to_int_i64`).
            "[10, 20, 30, 40, 50][(0..).step(2)]",
            "[10, 20, 30, 40, 50][(0..-1).step(2)]",
            "[10, 20, 30, 40, 50][(1..3).step(1)]",
            "[10, 20, 30, 40, 50][(0..3).step(2)]",
            // Negative step walks backwards from begin.
            "[10, 20, 30, 40, 50][(4..0).step(-1)]",
            // Same dispatch via `Array#slice`.
            "[10, 20, 30, 40, 50].slice((0..3).step(2))",
            "[1, 2, 3].slice((0..).step(2))",
            // `Range#%` produces the same kind of AS.
            "[10, 20, 30, 40, 50][(0..-1).%(2)]",
            // Manually-attached singleton method on a plain AS.
            r#"
              aseq = (0..-1).step(2)
              aseq.define_singleton_method(:foo) { :foo }
              [10, 20, 30, 40, 50][aseq]
            "#,
            // `Range#%` already calls `define_singleton_method(:inspect)`
            // internally, so this is the natural-occurring version of
            // the case above.
            "[10, 20, 30, 40, 50][(0..-1).%(2)]",
            r#"[*(0..100)].pack("C*")"#,
            r#"[0x12345678].pack("I")"#,
            r#"[0x12345678].pack("N")"#,
            r#"
            buf = "hello".b
            [3.14].pack("E", buffer: buf)
            [buf.bytesize, buf[0, 5]]
            "#,
            r#"
            buf = "".b
            buf << 0x08
            [42].pack("C", buffer: buf)
            buf.bytes.to_a
            "#,
            r#"[1,2,3].pack("L3").unpack("L3")"#,
            r#"[1,2,3,4,5].pack("L*").unpack("L*")"#,
            r#"[1,2,3].pack("S3").unpack("S3")"#,
            r#"[1,2,3,4].pack("C4").unpack("C4")"#,
            r#"[1,2,3].pack("Q2").unpack("Q2")"#,
            r#"[0x1234, 0x5678].pack("n2").unpack("n2")"#,
            r#"[0x12345678, 0xdeadbeef].pack("V2").unpack("V2")"#,
            r#"[0x1234].pack("s<").unpack("s<")"#,
            r#"[0x1234].pack("s>").unpack("s>")"#,
            r#"[0x12345678].pack("l<").unpack("l<")"#,
            r#"[0x12345678].pack("l>").unpack("l>")"#,
            r#"[0x123456789ABCDEF0].pack("q<").unpack("q<")"#,
            r#"[0x123456789ABCDEF0].pack("q>").unpack("q>")"#,
            r#"[0x1234].pack("S<").unpack("S<")"#,
            r#"[0x1234].pack("S>").unpack("S>")"#,
            r#"[0x12345678].pack("L<").unpack("L<")"#,
            r#"[0x12345678].pack("L>").unpack("L>")"#,
            // l!/L! — native long is 64-bit on x86-64
            r#"[0x123456789ABCDEF0].pack("l!").unpack("l!")"#,
            r#"[0x123456789ABCDEF0].pack("L!").unpack("L!")"#,
            // s!/S! — native short is still 16-bit
            r#"[0x1234].pack("s!").unpack("s!")"#,
            r#"[0x1234].pack("S!").unpack("S!")"#,
            // i!/I! — native int is still 32-bit
            r#"[0x12345678].pack("i!").unpack("i!")"#,
            r#"[0x12345678].pack("I!").unpack("I!")"#,
            // _ modifier is equivalent to !
            r#"[0x123456789ABCDEF0].pack("l_").unpack("l_")"#,
            r#"[0x123456789ABCDEF0].pack("L_").unpack("L_")"#,
            // j/J — pointer-sized integer (64-bit on x86-64)
            r#"[42].pack("j").unpack("j")"#,
            r#"[42].pack("J").unpack("J")"#,
            r#"[-1].pack("j").unpack("j")"#,
            r#"[123456789].pack("J").unpack("J")"#,
            // Whitespace between directives should be ignored
            r#"[1, 2].pack("C C").unpack("C C")"#,
            r#"[1, 2, 3].pack("C  C\tC").unpack("CCC")"#,
            // Comments should be ignored (# to end of line)
            r#"[1, 2].pack("C #comment\nC").unpack("CC")"#,
            r#"["10110001"].pack("b8").unpack("b8")"#,
            r#"["10110001"].pack("B8").unpack("B8")"#,
            r#"["1011"].pack("b4").unpack("b8")"#,
            r#"["1011"].pack("B4").unpack("B8")"#,
            r#"["10110001010"].pack("b*").unpack("b*")"#,
            r#"["10110001010"].pack("B*").unpack("B*")"#,
            r#"[65, 66, 67].pack("U3")"#,
            r#"[0x3042, 0x3044].pack("U*")"#,
            r#"[65, 0x3042, 0x1F600].pack("U*").unpack("U*")"#,
            r#""ABC".unpack("U3")"#,
            // `U` produces UTF-8; `M` / `m` / `u` produce US-ASCII;
            // mixing a binary directive with text falls to ASCII-8BIT
            // (the only encoding compatible with arbitrary bytes);
            // pure-binary template stays ASCII-8BIT.
            r#"[65, 0x3042].pack("U*").encoding.name"#,
            r#"[128].pack("U").encoding.name"#,
            r#"["abc"].pack("M").encoding.name"#,
            r#"["abc"].pack("m").encoding.name"#,
            r#"["abc"].pack("u").encoding.name"#,
        ]);
    }

    #[test]
    fn uniq_2() {
        run_tests(&[
            r#"["x", 65].pack("a*U").encoding.name"#,
            r#"[65, 66, 67].pack("CCC").encoding.name"#,
            // `pack("U")` extends past U+10FFFF using the extended
            // UTF-8 byte pattern (CRuby parity). 4-byte form covers
            // up to 0x1FFFFF; 5-/6-byte forms cover up to 2^31-1 /
            // 2^32-1.
            r#"[0x110000].pack("U").bytes"#,
            r#"[0x1FFFFF].pack("U").bytes"#,
        ]);
    }

    #[test]
    fn pack_utf8_range_errors() {
        // CRuby raises `RangeError` (not `ArgumentError`) for
        // negative input or values > 2^32-1.
        run_test_error(r#"[-1].pack("U")"#);
        run_test_error(r#"[0x100000000].pack("U")"#);
    }

    #[test]
    fn pack_base64_uuencode_line_length() {
        run_tests(&[
            // Counts of `1` and `2` are smaller than the 3-byte
            // Base64/uuencode group and CRuby clamps them to the
            // default 45. `0` means "no line breaks". `>= 3` is
            // honoured verbatim.
            r#"["abcdef"].pack("m1")"#,
            r#"["abcdef"].pack("m2")"#,
            r#"["abcdef"].pack("m3")"#,
            r#"["abcdef"].pack("m0")"#,
            r#"["abcdef"].pack("u1")"#,
            r#"["abcdef"].pack("u3")"#,
        ]);
    }

    #[test]
    fn pack_hex_alpha_and_x_star() {
        run_tests(&[
            // H/h: an ASCII letter contributes `(c + 9) & 0x0F`, so a
            // non-hex letter like 'H' (⇒ 1) wraps like a-f/A-F do.
            r#"["H"].pack("H")"#,
            r#"["H"].pack("h")"#,
            r#"["z"].pack("H")"#,
            r#"["G"].pack("h")"#,
            // Non-letters keep using their low nibble.
            r#"[":"].pack("H")"#,
            r#"["^"].pack("H")"#,
            // `X*` is a no-op; `X`/`Xn` remove bytes.
            r#"[1, 2, 3].pack("C2X*C")"#,
            r#"[1, 2, 3].pack("C3X")"#,
            r#"[1, 2, 3].pack("C3X2")"#,
        ]);
    }

    #[test]
    fn pack_w_bignum_and_u_line_length() {
        run_tests(&[
            // `w` (BER) handles Bignums, not just longs.
            r#"[2 ** 65].pack("w").bytes"#,
            r#"[0].pack("w").bytes"#,
            r#"[9999].pack("w").bytes"#,
            r#"[2 ** 128 + 12345].pack("w").bytes"#,
            // `u` per-line byte count floors to a multiple of 3.
            r#"["abcdefghijklm"].pack("u7")"#,
            r#"["abcdefghijklmnop"].pack("u5")"#,
            r#"["abcdefghijklmnop"].pack("u9")"#,
        ]);
    }

    #[test]
    fn pack_buffer_append_fast_path() {
        run_tests(&[
            // Append-only templates (no `@`/`X`) take the fast path that
            // extends the buffer in place instead of rebuilding it.
            r#"[65, 66].pack("cc", buffer: "abc".dup)"#,
            r#"[3.14].pack("E", buffer: "".b).bytes"#,
            r#"b = "".b; 100.times { [3.14].pack("E", buffer: b) }; b.bytesize"#,
            // The buffer's encoding is preserved; appending a byte that is
            // invalid in it degrades valid_encoding?, appending ASCII keeps it.
            r#"b = "あ".dup; [255].pack("C", buffer: b); [b.encoding.to_s, b.valid_encoding?]"#,
            r#"b = "あ".dup; [65].pack("C", buffer: b); [b, b.valid_encoding?]"#,
            // `X` with a count reaches back into the buffer's own bytes
            // (the seeded slow path).
            r#"[65].pack("X2C", buffer: "12345".dup)"#,
            // `X*` is a no-op (count 0) and keeps the fast path.
            r#"[65].pack("X*C", buffer: "123".dup)"#,
            // `@*` means `@0` in CRuby: rewind to position 0, discarding
            // the seeded buffer content.
            r#"[65].pack("@*C", buffer: "123".dup)"#,
            r#"["ab", 65].pack("a2@*C")"#,
            r#"["abc"].pack("a3@*")"#,
        ]);
    }

    #[test]
    fn pack_buffer_offset_and_type() {
        run_tests(&[
            // The buffer seeds the output; `@` rewinds within it.
            r#"[65, 66, 67].pack("@3ccc", buffer: "123456".dup)"#,
            r#"[65, 66, 67].pack("@6ccc", buffer: "123".dup)"#,
            r#"[65, 66, 67].pack("@3ccc", buffer: "1234567890".dup)"#,
            r#"[65, 66, 67].pack("ccc", buffer: "123".dup)"#,
            // Returns the same buffer object.
            r#"b = +"x"; [65].pack("C", buffer: b).equal?(b)"#,
            // A non-String buffer is a TypeError with CRuby's message.
            r#"begin; [1].pack("C", buffer: []); rescue TypeError => e; e.message; end"#,
        ]);
    }

    #[test]
    fn pack_base64_uuencode_nil_raises() {
        // `m` and `u` reject nil with TypeError (used to silently
        // treat nil as "").
        run_test_error(r#"[nil].pack("m")"#);
        run_test_error(r#"[nil].pack("u")"#);
    }

    #[test]
    fn pack_hex_non_hex_low_nibble() {
        run_tests(&[
            // CRuby contributes the low nibble of non-hex characters
            // (`s[i] & 0x0F`), not zero. `^` is 0x5E → low nibble 0xE.
            // `["^"].pack("H")` ⇒ `"\xE0"` (was `"\x00"`).
            r#"["^"].pack("H").bytes"#,
            r#"["^"].pack("h").bytes"#,
            r##"a = [1,2,[3,4,[5,6],7],8]; [a.flatten, a]"##,
            r##"[1,2,[3,4,[5,6],7],8].flatten(nil)"##,
            r##"[1,2,[3,4,[5,6],7],8].flatten(-1)"##,
            r##"[1,2,[3,4,[5,6],7],8].flatten(0)"##,
            r##"[1,2,[3,4,[5,6],7],8].flatten(1)"##,
            r##"a = [1,2,[3,4,[5,6],7],8]; [a.flatten!, a]"##,
            r##"a = [1,2,[3,4,[5,6],7],8]; [a.flatten!(nil), a]"##,
            r##"a = [1,2,[3,4,[5,6],7],8]; [a.flatten!(-1), a]"##,
            r##"a = [1,2,[3,4,[5,6],7],8]; [a.flatten!(0), a]"##,
            r##"a = [1,2,[3,4,[5,6],7],8]; [a.flatten!(1), a]"##,
            r##"
        ary = [1, nil, 2, nil, 3, nil]
        [ary.compact, ary]
        "##,
            r##"
        ary = [1, nil, 2, nil, 3, nil]
        [ary.compact!, ary]
        "##,
            r##"
        ary = [1, 2, 3]
        [ary.compact!, ary]
        "##,
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
            r##"[1, 0, 0, 1, 0].index(1) "##,
            r##"[1, 0, 0, 0, 0].index(1) "##,
            r##"[0, 0, 0, 0, 0].index(1)"##,
            r##"[0, 1, 0, 1, 0].index {|v| v > 0}"##,
            r##"
        res = []
        ary = [1, 2, 3]
        res << ary.insert(2, "a", "b")
        res << ary.insert(-2, "X")
        res << ary.insert(30, 5, 9)
        res
        "##,
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
            // Self-containing array
            r##"
        a = [1, 2]
        a << a
        a.to_s
        "##,
            // Same object appearing multiple times (not recursive)
            r#"a = [1]; b = [a, a]; b.to_s"#,
            r#"h = {x: 1}; [h, h].to_s"#,
            // User-defined inspect on custom objects inside arrays
            r##"
        class Foo
          def inspect
            "custom_foo"
          end
        end
        [Foo.new, 1, "hello"].inspect
        "##,
            // Empty array
            r#"[].inspect"#,
            r#"[].to_s"#,
            // Single element
            r#"[1].inspect"#,
            // Various types
            r#"[1, 2.5, "str", :sym, nil, true, false].inspect"#,
            // Nested arrays
            r#"[[1, 2], [3, [4, 5]]].inspect"#,
            // Array containing hash
            r#"[{a: 1, b: 2}, {c: 3}].inspect"#,
            // Mixed nesting
            r#"[1, [2, {a: 3}], "hello", :world].inspect"#,
            // Array with string containing special characters
            r#"["hello\nworld", "tab\there"].inspect"#,
            // Array with Range
            r#"[1..5, 1...5].inspect"#,
            // to_s is aliased to inspect
            r#"[1, 2, 3].to_s"#,
            // User-defined to_s should NOT affect inspect output
            // (inspect uses inspect, not to_s, for each element)
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
            // User-defined inspect inside nested structures
            r##"
        class MyObj
          def inspect
            "<my>"
          end
        end
        [[MyObj.new], {k: MyObj.new}].inspect
        "##,
        ]);
    }

    #[test]
    fn product_size_check() {
        // A billion combinations: more objects than the heap can hold
        // (CRuby runs out of memory part way; not something to run there).
        let res = run_test_no_result_check(
            r##"
        begin
          ([0] * 1000).product([0] * 1000, [0] * 1000)
          false
        rescue NoMemoryError
          true
        end
        "##,
        );
        assert_eq!(res, crate::Value::bool(true));
    }

    #[test]
    fn slice_bang_integer() {
        run_tests(&[
            r##"
        a = [1, 2, 3, 4, 5]
        [a.slice!(2), a]
        "##,
        ]);
    }

    /// `Array#<<` is JIT-inlined by `array_shl` / `emit_array_shl`: an append
    /// with spare capacity becomes a two-load / one-store sequence, in both
    /// the inline (`SmallVec`) and spilled residencies, and only a full buffer
    /// calls `ary_shl` to reallocate. A splat call site is not `simple` and
    /// keeps the generic path; a frozen receiver must deopt so the interpreter
    /// can raise (the inline store bypasses `Array::push`'s caller, which is
    /// where the check lives).
    #[test]
    fn shl_jit_inline() {
        run_test(
            r##"
        def app(a, v) = a << v
        def app_blk(a, v, &b) = a.<<(v, &b)
        50.times { app([], 1); app_blk([], 1) }
        res = []
        # Grows out of the inline buffer, spills, then reallocates repeatedly.
        a = []
        20.times { |i| app(a, i) }
        res << a
        # Already spilled, with spare capacity: `<<` returns the receiver.
        b = [0, 1, 2, 3, 4, 5]
        res << (app(b, 6).object_id == b.object_id)
        res << b
        # Heap children go through the write barrier.
        c = []
        6.times { |i| app(c, [i, "s#{i}"]) }
        res << c
        res << app_blk([0, 1], 2)
        begin
          app([1, 2].freeze, 3)
        rescue => e
          res << e.class
        end
        res
        "##,
        );
    }

    /// `Array#rotate!` is JIT-inlined by `array_rotate_`: the call site drops
    /// generic dispatch and `coerce_to_int_i64`, and `ary_rotate_` /
    /// `wrap_rotate_count` skip the `%` when the count is already in range.
    /// Covers both arities, all four `wrap_rotate_count` branches, the empty
    /// receiver, and the shapes that stay generic (a splat call site, a
    /// non-Integer count) plus the frozen deopt.
    #[test]
    fn rotate_jit_inline() {
        run_test(
            r##"
        class Cnt; def to_int = 2; end
        def rot0(a) = a.rotate!
        def rot(a, n) = a.rotate!(n)
        def rot_splat(a, args) = a.rotate!(*args)
        def rot_obj(a, n) = a.rotate!(n)
        50.times do
          rot0([1, 2, 3]); rot([1, 2, 3], 2); rot([1, 2, 3], -2); rot([1, 2, 3], 0)
          rot_splat([1, 2, 3], [2]); rot_obj([1, 2, 3], Cnt.new)
        end
        res = []
        res << rot([0, 1, 2, 3, 4, 5, 6, 7], 3)     # 0 < cnt < len
        res << rot([0, 1, 2, 3], 4)                 # cnt == len
        res << rot([0, 1, 2, 3], 10)                # cnt > len
        res << rot([0, 1, 2, 3, 4, 5, 6, 7], -3)    # -len < cnt < 0
        res << rot([0, 1, 2, 3], -10)               # cnt < -len
        res << rot([0, 1, 2, 3], -4611686018427387904)
        res << rot([0, 1, 2, 3], 0)
        res << rot([], 5)                           # empty receiver
        res << rot0([1, 2, 3, 4])
        res << rot0([])
        res << rot_splat([0, 1, 2, 3], [2])
        res << rot_obj([0, 1, 2, 3], Cnt.new)
        a = [1, 2, 3]
        res << (rot(a, 1).object_id == a.object_id)
        begin
          rot([1, 2].freeze, 1)
        rescue => e
          res << e.class
        end
        res
        "##,
        );
    }

    /// The slice form `ary[start, len] = other` is JIT-inlined by
    /// `array_slice_assign` when `len` is a literal in `0..=8`: an in-bounds
    /// replacement by an equally long `Array` is a plain copy. Covers the
    /// inline copy in all four buffer combinations (inline/spilled × source,
    /// receiver), and every run-time condition that has to fall back to
    /// `set_array_slice_pure` or, past it, the interpreter — a negative
    /// start (in and out of range), a non-`Array` right-hand side, one
    /// that answers `#to_ary`, a length
    /// mismatch, a run that reaches past the end, and self-assignment —
    /// plus the compile-time refusals (a start proven non-Integer, a
    /// non-literal or out-of-range length, a splat call site).
    #[test]
    fn index_assign_slice_jit_inline() {
        run_test(
            r##"
        class ToAry; def to_ary = [:x, :y, :z, :w]; end
        def sl0(a, i, v) = a[i, 0] = v
        def sl1(a, i, v) = a[i, 1] = v
        def sl4(a, i, v) = a[i, 4] = v
        def sl6(a, i, v) = a[i, 6] = v
        def sl_var(a, i, n, v) = a[i, n] = v      # `len` is not a literal
        def sl_wide(a, i, v) = a[i, 20] = v       # `len` > the unroll bound
        def sl_float(a, v) = a[1.5, 2] = v        # start proven non-Integer
        def sl_splat(a, args) = a.[]=(*args)      # not a `simple` call site
        def sl_arity(a, v) = a[1, 2, 3] = v       # neither the 2- nor 3-arg form
        def sl_range(a, r, v) = a[r] = v          # 2-arg form, index not an Integer
        50.times do
          sl_range([1, 2, 3], 0..1, [0])
          sl0([1], 0, []); sl1([1], 0, [0]); sl4([1, 2, 3, 4], 0, [0, 0, 0, 0])
          sl6([1, 2, 3, 4, 5, 6], 0, [0, 0, 0, 0, 0, 0])
          sl_var([1, 2], 0, 1, [0]); sl_wide([1, 2], 0, [0])
          sl_float([1, 2, 3], [0]); sl_splat([1, 2, 3], [0, 1, [0]])
          begin; sl_arity([1, 2], 0); rescue ArgumentError; end
        end
        res = []
        # --- the inline copy ---
        res << (a = [0, 1, 2]; sl1(a, 1, [:a]); a)                     # inline/inline
        res << (a = [*0..9]; sl4(a, 2, [:a, :b, :c, :d]); a)           # spilled/inline
        res << (a = [*0..9]; sl6(a, 1, [*:a..:f]); a)                  # spilled/spilled
        res << (a = [0, 1, 2]; sl0(a, 1, []); a)                       # zero-length
        res << (a = [*0..9]; sl4(a, 6, [:a, :b, :c, :d]); a)           # ends exactly at len
        # --- run-time fall-backs into `set_array_slice_pure` ---
        res << (a = [*0..9]; sl4(a, -5, [:a, :b, :c, :d]); a)          # negative start
        res << (a = [*0..9]; sl4(a, 0, [:a, :b]); a)                   # shorter -> shrinks
        res << (a = [*0..9]; sl4(a, 0, [*:a..:f]); a)                  # longer -> grows
        res << (a = [*0..9]; sl4(a, 8, [:a, :b, :c, :d]); a)           # runs past the end
        res << (a = [*0..9]; sl4(a, 20, [:a, :b, :c, :d]); a)          # start past the end
        res << (a = [*0..9]; sl4(a, 2, 42); a)                         # not an Array
        res << (a = [*0..9]; sl4(a, 2, ToAry.new); a)                  # answers #to_ary
        res << (a = [*0..9]; sl4(a, 2, nil); a)
        res << (a = [*0..7]; sl4(a, 2, a); a)                          # self-assignment
        res << (a = [0, 1, 2]; sl4(a, 0, [:a, :b, :c, :d]); a)         # inline receiver grows
        begin
          sl4([*0..9], -100, [:a, :b, :c, :d])
        rescue => e
          res << e.class
        end
        begin
          sl4([*0..9].freeze, 0, [:a, :b, :c, :d])
        rescue => e
          res << e.class
        end
        # --- shapes that never reach the inline path ---
        res << (a = [*0..9]; sl_var(a, 1, 3, [:a, :b, :c]); a)
        res << (a = [*0..9]; sl_wide(a, 1, [:a]); a)
        res << (a = [*0..9]; sl_float(a, [:a, :b]); a)
        res << (a = [*0..9]; sl_splat(a, [1, 2, [:a, :b]]); a)
        res << (a = [*0..9]; sl_range(a, 2..5, [:a, :b]); a)
        begin
          sl_arity([*0..9], 0)
        rescue => e
          res << e.class
        end
        begin
          a = [*0..9]
          a[1, -1] = [:a]
        rescue => e
          res << e.class
        end
        res
        "##,
        );
    }

    /// Issue #1838: an out-of-range negative start in the `[start, len] =`
    /// form reports `minimum: -size`, as the single-index form does — both
    /// from the builtin and from the JIT-compiled site, whose helper
    /// (`set_array_slice_pure`) leaves the raise to the interpreter.
    #[test]
    fn index_assign_slice_too_small_message() {
        run_test(
            r##"
        def sl2(a, i, v) = a[i, 2] = v
        def sln(a, i, n, v) = a[i, n] = v
        res = []
        [-100, -9].each do |i|
          a = [0] * 8
          begin; sl2(a, i, [1]); rescue IndexError => e; res << e.message; end
          begin; sln(a, i, 2, [1]); rescue IndexError => e; res << e.message; end
          begin; a[i] = 1; rescue IndexError => e; res << e.message; end
          begin; [][i, 0] = 1; rescue IndexError => e; res << e.message; end
        end
        res
        "##,
        );
    }

    #[test]
    fn index_assign_negative() {
        run_tests(&[
            r##"a = [1,2,3,4,5]; a[-2, 2] = []; a"##,
            r##"a = [1,2,3,4,5]; a[-3, 1] = [99]; a"##,
            r##"a = [1,2,3,4,5]; a[-5, 3] = [:a, :b]; a"##,
            r##"a = [1,2,3,4,5]; a[-1, 1] = [:x, :y, :z]; a"##,
            r##"a = [1,2,3]; a[-3, 0] = [:a]; a"##,
        ]);
        run_test_error(r##"a = [1,2,3]; a[-4, 1] = []"##);
    }

    #[test]
    fn index_assign_range_out_of_bounds() {
        // A negative range start that is still out of bounds after wrapping
        // raises RangeError (not IndexError).
        run_test(r##"a = [1,2,3,4]; begin; a[-5..-5] = ""; rescue RangeError; :re; end"##);
        run_test(r##"a = [1,2,3,4]; begin; a[-5...-4] = ""; rescue RangeError; :re; end"##);
        run_test(r##"a = [1,2,3,4]; begin; a[-5..10] = ""; rescue RangeError; :re; end"##);
        // A beginless range whose negative end is out of bounds inserts at the
        // beginning rather than raising.
        run_tests(&[
            r##"a = [1,2,3]; a[(..-7)] = [4]; a"##,
            r##"a = [1,2,3]; a[(...-10)] = [6]; a"##,
            r##"a = [1,2,3,4]; a[0..-9] = [1]; a"##,
        ]);
    }

    #[test]
    fn index_assign_self() {
        run_tests(&[
            // Self-assignment: b[start, length] = b
            r##"b = [1, 2, 3, 4, 5]; b[1, 0] = b; b"##,
            r##"b = [1, 2, 3, 4, 5]; b[0, 5] = b; b"##,
            r##"b = [1, 2, 3, 4, 5]; b[2, 2] = b; b"##,
            r##"b = [1, 2, 3]; b[0, 0] = b; b"##,
            r##"b = [1]; b[0, 1] = b; b"##,
            // Self-assignment via range
            r##"b = [1, 2, 3, 4, 5]; b[1..2] = b; b"##,
            r##"b = [1, 2, 3]; b[0..0] = b; b"##,
            // end < start with range: treated as zero-length insert at start
            r##"a = [1, 2, 3, 4, 5]; a[3..1] = [:a, :b]; a"##,
            // start beyond array size: fills with nil then inserts
            r##"a = [1, 2, 3]; a[5..2] = [99]; a"##,
            r##"a = [1, 2, 3]; a[5..2] = 42; a"##,
            r##"a = [1,2,3]; b = [4,5]; a.replace(b); [a, b]"##,
            r##"a = [1,2,3]; a.replace([]); a"##,
            r##"a = []; a.replace([1,2,3]); a"##,
            r##"a = [1,2,3]; a.replace(a); a"##,
            r##"
            res = []
            [1,2,3].cycle(2) { |x| res << x }
            res
            "##,
            r##"
            res = []
            [1,2].cycle(0) { |x| res << x }
            res
            "##,
            "[].cycle(3) { |x| x }",
            r##"
            res = []
            [1,2,3,4].combination(2) { |c| res << c }
            res
            "##,
            r##"
            res = []
            [1,2,3].combination(0) { |c| res << c }
            res
            "##,
            r##"
            res = []
            [1,2,3].combination(1) { |c| res << c }
            res
            "##,
            "[1,2,3,4,5].bsearch_index { |x| x >= 3 }",
            "[1,2,3,4,5].bsearch_index { |x| x >= 6 }",
            "[1,3,5,7,9].bsearch_index { |x| x <=> 5 }",
        ]);
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
        run_tests(&[
            r##"
            res = []
            [1,2,3].permutation(2) { |p| res << p }
            res
            "##,
            r##"
            res = []
            [1,2,3].permutation { |p| res << p }
            res
            "##,
            r##"
            res = []
            [1,2,3].permutation(0) { |p| res << p }
            res
            "##,
            r##"
            res = []
            [1,2,3].permutation(1) { |p| res << p }
            res
            "##,
            "[1,2,3].permutation(4).to_a",
            "[1,2,3].permutation(-1).to_a",
            r##"
            res = []
            [1,2,3].repeated_combination(2) { |c| res << c }
            res
            "##,
            r##"
            res = []
            [1,2].repeated_combination(3) { |c| res << c }
            res
            "##,
            "[1,2,3].repeated_combination(0).to_a",
            "[1,2,3].repeated_combination(1).to_a",
        ]);
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
        // Under `ARY_MAX_SIZE`, so the allocator is asked: a size no
        // address space holds, never one a machine with enough memory
        // and overcommit would grant (and then have to fill).
        run_test_error(r##"Array.new(1 << 59)"##);
    }

    #[test]
    fn pack_hex() {
        run_tests(&[
            r#"["6162"].pack("h4")"#,
            r#"["6162"].pack("H4")"#,
            r#"["6162636465"].pack("h*")"#,
            r#"["6162636465"].pack("H*")"#,
            r#""\x16\x26".unpack("h4")"#,
            r#""\x61\x62".unpack("H4")"#,
        ]);
    }

    // ===== Tests for new methods =====
    #[test]
    fn array_class_bracket() {
        run_tests(&[
            "Array[1, 2, 3]",
            "Array[]",
            "Array[42]",
        ]);
        // Subclass support
        run_test_with_prelude("A[1, 2, 3]", "class A < Array; end");
        run_test_with_prelude("A[1, 2, 3].class", "class A < Array; end");
    }

    #[test]
    fn array_try_convert() {
        run_tests(&[
            "Array.try_convert([1, 2])",
            "Array.try_convert(nil)",
            r#"Array.try_convert("string")"#,
        ]);
        // Object with to_ary
        run_test_with_prelude(
            "Array.try_convert(C.new)",
            "class C; def to_ary; [1, 2]; end; end",
        );
        // Object without to_ary returns nil
        run_test_with_prelude("Array.try_convert(C.new)", "class C; end");
        // to_ary returns non-Array raises TypeError
        run_test_error("class C; def to_ary; 'not array'; end; end; Array.try_convert(C.new)");
    }

    #[test]
    fn array_at() {
        run_tests(&[
            "[1, 2, 3].at(0)",
            "[1, 2, 3].at(1)",
            "[1, 2, 3].at(-1)",
            "[1, 2, 3].at(5)",
            // With value argument
            "[1, 2, 3, 2, 1].rindex(2)",
            "[1, 2, 3].rindex(4)",
            // With block
            "[1, 2, 3, 4].rindex {|x| x > 2}",
            "[1, 2, 3].rindex {|x| x > 10}",
            "res = []; [10, 20, 30].each_index {|i| res << i}; res",
            "[].each_index {|i| i}",
            "[1, 2, 3, 4, 5].difference([2, 4])",
            "[1, 2, 3].difference([1], [3])",
            "[1, 2, 3].difference([])",
            "[1, 2, 3].difference()",
            // Duplicates preserved from self
            "[1, 1, 2, 2, 3].difference([1])",
            "[1, 2, 3, 4].intersection([2, 3, 5])",
            "[1, 2, 3].intersection([2, 3], [3, 4])",
            "[1, 2, 3].intersection([])",
            // Duplicates removed
            "[1, 1, 2, 2].intersection([1, 2])",
            "[1, 2].repeated_permutation(2).to_a.sort",
            "[1, 2, 3].repeated_permutation(0).to_a",
            "[1, 2].repeated_permutation(1).to_a.sort",
            "[].repeated_permutation(2).to_a",
            "[1, 2, 3].join(nil)",
            "[1, 2, 3].join",
            r#"[1, 2, 3].join("-")"#,
            "[].join(nil)",
            // nested arrays use same separator
            r#"[[1, [2]], 3].join("-")"#,
            // * with string acts like join
            r#"[1, 2, 3] * ",""#,
            r#"[[1, 2], [3, 4]] * "-""#,
        ]);
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
    fn join_takes_an_empty_piece_as_no_piece_at_all() {
        // `enc_compatible_latter`'s two empty-string rules: an empty
        // piece leaves the accumulator alone even when its encoding
        // is incompatible, and an empty accumulator takes the piece's
        // encoding unless it is ASCII-compatible and the piece 7-bit.
        crate::tests::run_test_once(
            r##"
            u16 = "a".encode("UTF-16LE"); j = "\u3042"
            [[u16].join("").encoding.to_s, [u16, u16].join.encoding.to_s, u16.chars.join == u16,
             ["".encode("EUC-JP"), j].join.encoding.to_s, ["".encode("EUC-JP"), "a"].join.encoding.to_s,
             [""].join.encoding.to_s, [].join.encoding.to_s, ["a", ""].join.encoding.to_s,
             (["a".b, "\xff".b, j].join rescue $!.class), ["abc".b, j].join.encoding.to_s, [j, ""].join.encoding.to_s,
             ["", "".b].join.encoding.to_s, [u16, ""].join.encoding.to_s, [u16, "".b].join.encoding.to_s,
             ["", u16].join.encoding.to_s, ([u16, "a"].join rescue $!.class)]
            "##,
        );
    }

    #[test]
    fn join_result_code_range_is_trusted() {
        // The join adopts its buffer with the code range it tracked
        // while appending, instead of re-scanning the result, so the
        // tracked value has to be right — in particular a broken piece
        // completed by the next one must be re-classified, not recorded
        // as broken.
        run_tests(&[
            r#"a = ["\xE3".b.force_encoding("UTF-8"), "\x81\x82".b.force_encoding("UTF-8")]; j = a.join; [j.valid_encoding?, j.ascii_only?, j == "あ", j.encoding.name]"#,
            r#"a = ["a", "\xE3".b.force_encoding("UTF-8"), "\x81\x82".b.force_encoding("UTF-8"), "b"]; j = a.join("-"); [j.valid_encoding?, j.ascii_only?, j.bytes]"#,
            r#"j = ["abc", "あ", "def"].join; [j.valid_encoding?, j.ascii_only?, j.encoding.name, j.length]"#,
            r#"j = ["abc", "def"].join(", "); [j.valid_encoding?, j.ascii_only?, j.encoding.name]"#,
            r#"j = ["abc", "\xff".b.force_encoding("UTF-8")].join; [j.valid_encoding?, j.ascii_only?]"#,
            r#"j = ["abc", "\xff".b].join; [j.valid_encoding?, j.ascii_only?, j.encoding.name]"#,
            // Non-String elements (the size estimate is only a guess
            // for these) and results longer than the inline buffer.
            r#"j = [1, :sym, nil, 2.5, "x" * 40, [3, [4]]].join("/"); [j, j.bytesize, j.valid_encoding?, j.ascii_only?]"#,
            r#"j = (["あいう"] * 20).join("、"); [j.bytesize, j.length, j.valid_encoding?, j.ascii_only?]"#,
            r#"j = ["x"].join("あ"); [j, j.ascii_only?, j.encoding.name]"#,
        ]);
    }

    #[test]
    fn join_empty_array_us_ascii() {
        run_tests(&[
            // CRuby tags `[].join` (and `[].join(sep)`) as US-ASCII
            // — the universal "no content" encoding — not UTF-8.
            r#"[].join.encoding.name"#,
            r#"[].join("xxx").encoding.name"#,
            r#"[].join.bytes"#,
        ]);
    }

    #[test]
    fn join_empty_array_skips_separator_to_str() {
        // `[].join(sep)` must not call `sep.to_str` — short-
        // circuit before separator coercion.
        run_test_with_prelude(
            r#"[].join(C.new); :ok"#,
            r#"
            class C
              def to_str
                raise "to_str called on empty join separator"
              end
            end
            "#,
        );
    }

    #[test]
    fn join_left_wins_on_seven_bit() {
        run_tests(&[
            // The first emitted fragment seeds the running encoding,
            // so a UTF-8 head + US-ASCII tail (both 7-bit clean) stays
            // UTF-8 (used to drop to US-ASCII). Pure US-ASCII stays
            // US-ASCII.
            r#"["bar", "foo".dup.force_encoding("US-ASCII")].join.encoding.name"#,
            r#"a = "x".dup.force_encoding("US-ASCII"); b = "y".dup.force_encoding("US-ASCII"); [a, b].join.encoding.name"#,
        ]);
    }

    #[test]
    fn join_incompatible_encoding_raises() {
        // BINARY with real high bytes ⊔ UTF-8 with real multibyte
        // ⇒ no compatible encoding. CRuby raises
        // `Encoding::CompatibilityError`; we used to silently
        // collapse to ASCII-8BIT.
        run_test_error(
            r#"
            ["báz", "f\xff".dup.force_encoding("BINARY")].join
            "#,
        );
    }

    #[test]
    fn join_undef_to_s_raises_nomethod() {
        // An element that lacks `to_str`, `to_ary`, *and* `to_s`
        // raises NoMethodError instead of silently falling back to
        // the Rust-side `Value::to_s`.
        run_test_error(
            r#"
            obj = Object.new
            class << obj; undef :to_s; end
            [obj].join("-")
            "#,
        );
    }

    #[test]
    fn bsearch_nil_result() {
        run_tests(&[
            // nil block result treated as find-minimum (false)
            "[1, 2, 3].bsearch { nil }",
            "[1, 2, 3].bsearch_index { nil }",
            // empty block => nil
            "[1, 2, 3, 4].bsearch { |x| nil }",
        ]);
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
        run_tests(&[
            "[1, 2, 3].index(2) { |x| false }",
            "[1, 2, 3].rindex(2) { |x| false }",
        ]);
    }

    #[test]
    fn map_bang_frozen() {
        run_test_error("[1, 2, 3].freeze.map! { |x| x }");
        run_test_error("[].freeze.map! { |x| x }");
    }

    #[test]
    fn values_at_range_padding() {
        run_tests(&[
            "[1, 2, 3].values_at(1..5)",
            "[].values_at(0..2)",
            "[1, 2, 3, 4, 5].values_at(1..3)",
            // endless range
            "[1, 2, 3, 4].values_at(1..)",
            "[1, 2, 3, 4].values_at(3...)",
            "[1, 2, 3].values_at(0...5)",
        ]);
    }

    #[test]
    fn cycle_to_int_and_size() {
        run_tests(&[
            "[1, 2, 3].cycle(2).to_a",
            "[1, 2, 3].cycle(0).to_a",
            "[].cycle(5).to_a",
            // Enumerator size
            "[1, 2, 3].cycle(2).size",
            "[1, 2, 3].cycle(0).size",
            "[].cycle(2).size",
        ]);
        run_test_no_result_check("[1, 2].cycle.size");
        // A `to_int` that returns a non-Integer is a TypeError, not a
        // downstream NoMethodError.
        run_test(
            r#"
            o = Object.new
            def o.to_int; "2"; end
            begin; [1, 2].cycle(o) { |x| }; rescue TypeError; :type_error; end
            "#,
        );
    }

    #[test]
    fn fill_start_length_edge_cases() {
        run_tests(&[
            // No length / nil length never grows the array; a start past
            // the end is a no-op (only an explicit length extends).
            r#"[].fill("a", 1)"#,
            r#"[1, 2, 3].fill("x", 5)"#,
            r#"[1, 2, 3].fill("x", 5, nil)"#,
            r#"[].fill("a", 1, nil)"#,
            r#"[].fill("a", 1, 2)"#,
            // nil start means 0.
            r#"[1, 2, 3].fill("y", nil)"#,
            // A block with three positional args is an ArgumentError.
            r#"begin; [1, 2].fill(1, 2, 3) { |i| i }; rescue ArgumentError; :ae; end"#,
            // A Range start plus a length argument is a TypeError.
            r#"begin; [1, 2, 3].fill("x", 1..2, 5); rescue TypeError; :te; end"#,
        ]);
    }

    #[test]
    fn slice_before_enumerator_size_is_nil() {
        // The chunk count isn't derivable from the source size, so the
        // returned Enumerator reports a nil size (CRuby parity); the
        // chunks themselves are still produced.
        run_tests(&[
            "[1, 2, 3].slice_before(2).size.inspect",
            "[1, 2, 3, 4].slice_before { |x| x.even? }.to_a",
            "[1, 2, 3].slice_before(2).to_a",
        ]);
    }

    #[test]
    fn sum_kahan_and_growth() {
        // Kahan–Neumaier compensated summation: precise float sum.
        run_test_once(
            r#"[2.7800000000000002, 5.0, 2.5, 4.44, 3.89, 3.89, 4.44, 7.78, 5.0, 2.7800000000000002, 5.0, 2.5].sum"#,
        );
        run_test_once(r#"[1.0, 1e100, 1.0, -1e100].sum"#);
        // Ordinary / mixed / block / non-numeric paths unaffected.
        run_tests(&[
            r#"[1, 2, 3].sum"#,
            r#"[1, 2, 3].sum(10)"#,
            r#"[1.0, 2, 3].sum"#,
            r#"[1, 2, 3].sum { |x| x * 2 }"#,
            r#"[[1], [2]].sum([])"#,
        ]);
        // A block that grows the receiver mid-sum sees the appended
        // elements too (CRuby tolerates the size increase).
        run_test_once(
            r#"
            a = [1, 2, 3]
            seen = []
            i = 0
            a.sum(0) do |e|
              seen << e
              a << (10 + i) if i < 5
              i += 1
              0
            end
            seen
            "#,
        );
    }

    #[test]
    fn cmp_returns_raw_element_result() {
        run_tests(&[
            r#"[1, 2, 3] <=> [1, 2, 3]"#,
            r#"[1, 2, 3] <=> [1, 2, 4]"#,
            r#"[1, 2, 3] <=> [1, 2]"#,
            r#"[1, 2] <=> [1, 2, 3]"#,
            // The first non-zero element `<=>` result is returned raw —
            // including `nil` and non-Integer values — and later elements
            // are not compared.
            r#"a = Object.new; def a.<=>(o); "foobar"; end; [1, a, 1] <=> [1, a, 1]"#,
            r#"a = Object.new; def a.<=>(o); nil; end; ([1, a] <=> [1, a]).inspect"#,
            r#"a = Object.new; def a.<=>(o); 1; end; [a] <=> [a]"#,
            // A non-Array, non-coercible rhs is nil.
            r#"([1, 2] <=> 5).inspect"#,
        ]);
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
        run_tests(&[
            "[1, 2, 3].all?(Integer) { |x| false }",
            "[1, 2, 3].any?(String) { |x| true }",
            "[1, 2, 3].count(1) { |x| true }",
            "[1, 2, 3].none?(String) { |x| true }",
            "[1, 2, 3].one?(1) { |x| false }",
            "[1, 3, 2].minmax",
            "[1, 3, 2].minmax { |a, b| b <=> a }",
            "[].minmax",
            "[5].minmax",
        ]);
    }

    #[test]
    fn sample() {
        run_tests(&[
            // sample without arguments returns a single element
            "[1].sample",
            "[].sample",
            // sample(n) returns an array of n elements
            "[1, 2, 3, 4, 5].sample(0)",
            "[1, 2, 3].sample(1).size",
            "[1, 2, 3].sample(5).sort",
        ]);
        run_test_no_result_check("[1, 2, 3, 4, 5].sample(3).size");
        // sample with negative n raises error
        run_test_error("[1, 2, 3].sample(-1)");
    }

    /// `sample(n)` reproduces CRuby element for element, not merely
    /// "n distinct elements": each of CRuby's branches maps the same draws
    /// onto a different result, so an `srand`-seeded run pins the branch
    /// boundaries as well as the draw sequence.
    #[test]
    fn sample_cruby_parity() {
        // Every n across the special cases (0..=3), the sorted-index
        // branch (4..=10) and the general branch, on receivers short
        // enough that n saturates at len.
        let mut codes = vec![];
        for len in [0usize, 1, 2, 3, 5, 10, 11, 12, 25] {
            for n in 0..=len + 2 {
                codes.push(format!("srand(9876); (1..{len}).to_a.sample({n})"));
            }
            codes.push(format!("srand(9876); (1..{len}).to_a.sample"));
        }
        // The `memo_threshold` branch, and the len boundaries at which
        // that threshold changes divisor.
        for len in [2559usize, 2560, 5119, 5120, 10239, 10240] {
            for n in [11usize, 20, 21, 41, 81, 161, 321] {
                codes.push(format!("srand(31); (1..{len}).to_a.sample({n})"));
            }
        }
        run_tests(&codes.iter().map(String::as_str).collect::<Vec<_>>());
    }

    /// `random:` reaches `sample` through the same resolver as `shuffle`.
    #[test]
    fn sample_random_keyword() {
        run_tests(&[
            "(1..20).to_a.sample(random: Random.new(7))",
            "(1..20).to_a.sample(5, random: Random.new(7))",
            // One instance drives several samples and stays in step with
            // its own subsequent draws.
            "r = Random.new(3); [(1..20).to_a.sample(4, random: r), (1..20).to_a.sample(4, random: r), (1..20).to_a.sample(random: r), r.rand(1000)]",
            // `Random` itself is CRuby's default argument: the global PRNG.
            "srand(5); [(1..20).to_a.sample(4, random: Random), (1..20).to_a.sample(random: Random)]",
            "srand(5); [(1..20).to_a.sample(4), (1..20).to_a.sample]",
            // A `Random` subclass is drawn from directly, bypassing a
            // redefined `#rand`; an unrelated object is duck-typed.
            "class R < Random; def rand(n) = 0; end; (1..20).to_a.sample(4, random: R.new(2))",
            "class Q; def initialize; @i = 0; end; def rand(n) = (@i += 1) % n; end; [(1..20).to_a.sample(5, random: Q.new), (1..20).to_a.sample(random: Q.new)]",
            // `nil` is an argument, not "not given".
            "begin; [1,2].sample(random: nil); rescue => e; [e.class, e.message]; end",
            "begin; [1,2].sample(2, random: nil); rescue => e; [e.class, e.message]; end",
            "o = Object.new; def o.rand(n) = 999; begin; (1..20).to_a.sample(2, random: o); rescue => e; [e.class, e.message]; end",
            "o = Object.new; def o.rand(n) = -1; begin; (1..20).to_a.sample(2, random: o); rescue => e; [e.class, e.message]; end",
            // Shrinking self from inside `#rand` voids indices drawn
            // against the old length rather than reading out of bounds.
            r##"
            b = (1..20).to_a
            m = Object.new
            m.define_singleton_method(:rand) { |n| b.replace([1, 2]); 0 }
            b.sample(4, random: m)
            "##,
            r##"
            c = (1..20).to_a
            m = Object.new
            m.define_singleton_method(:rand) { |n| c.replace((1..3).to_a); 0 }
            c.sample(15, random: m)
            "##,
            // `n` is `to_int`-coerced; a subclass receiver still samples
            // into a plain Array.
            "class N; def to_int = 3; end; (1..20).to_a.sample(N.new).size",
            "class C < Array; end; [C.new([1,2,3]).sample(2).class, C.new([1,2,3]).sample.class]",
            "[1,2,3].freeze.sample(2).size",
        ]);
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
        run_tests(&[
            "[10, 20, 30].fetch_values(0, 2)",
            "[10, 20, 30].fetch_values(0, -1)",
            "[10, 20, 30].fetch_values",
            // with block for missing indices
            "[10, 20, 30].fetch_values(0, 5) { |i| i * 100 }",
        ]);
        // error for out-of-range without block
        run_test_error("[10, 20, 30].fetch_values(0, 5)");
    }

    #[test]
    fn min_max_homogeneous_fast_path() {
        run_tests(&[
            // Each class the fast path takes, and each it declines.
            "[3, 1, 2].min",
            "[3, 1, 2].max",
            "[3, 1, 2].minmax",
            "[3.5, 1.5, 2.5].min",
            "[3.5, 1.5, 2.5].max",
            "[3.5, 1.5, 2.5].minmax",
            r#"["b", "a", "c"].min"#,
            r#"["b", "a", "c"].max"#,
            r#"["b", "a", "c"].minmax"#,
            // Declined: mixed numerics (still ordered, by the general path),
            // a Bignum, a Symbol, one element, none.
            "[1, 2.5, 0].min",
            "[1, 2.5, 0].max",
            "[10**20, 1].min",
            "[10**20, 1].max",
            "[:b, :a].min",
            "[:b, :a].minmax",
            "[7].min",
            "[7].minmax",
            "[].min",
            "[].max",
            "[].minmax",
            // Ties keep the first equal element, which `equal?` can see.
            r#"a = "x"; b = "x"; [a, b].min.equal?(a)"#,
            r#"a = "x"; b = "x"; [a, b].max.equal?(a)"#,
        ]);
        // A NaN has no ordering: the general path raises, as CRuby does,
        // rather than the fast path ordering around it.
        run_test_error("[3.0, Float::NAN, 2.0].min");
        run_test_error("[3.0, Float::NAN, 2.0].max");
        run_test_error("[3.0, Float::NAN].minmax");
        // Incomparable elements raise, and name the operands CRuby's way.
        run_test_error(r#"[1, "a"].min"#);
        run_test_error(r#"[1, "a"].max"#);
        run_test_error(r#"[1, "a"].minmax"#);
    }

    #[test]
    fn min_max_redefined_cmp() {
        // A redefined `<=>` takes the receiver off the fast path — every
        // one of min / max / minmax must go through the redefinition.
        for m in ["min", "max", "minmax"] {
            run_test_with_prelude(
                &format!("[3, 1, 2].{m}"),
                "class Integer; def <=>(o) = (o.to_i - to_i); end",
            );
            run_test_with_prelude(
                &format!("[3.0, 1.0, 2.0].{m}"),
                "class Float; def <=>(o) = (o.to_f - to_f); end",
            );
            run_test_with_prelude(
                &format!(r#"["b", "a", "c"].{m}"#),
                "class String; def <=>(o) = (o.length - length); end",
            );
        }
    }

    #[test]
    fn min_max_n() {
        run_tests(&[
            "[3, 1, 2].min(2)",
            "[3, 1, 2].max(2)",
            // n at and past the edges, and the no-op forms.
            "[3, 1, 2].min(0)",
            "[3, 1, 2].max(0)",
            "[3, 1, 2].min(99)",
            "[3, 1, 2].max(99)",
            "[].min(2)",
            "[].max(2)",
            // An explicit nil for n means "no n", not a TypeError.
            "[3, 1, 2].min(nil)",
            "[3, 1, 2].max(nil)",
            "[3, 1, 2].min(nil) {|a, b| b <=> a}",
            // n is #to_int-converted.
            "[3, 1, 2].min(2.0)",
            r#"[3, 1, 2].min(Object.new.tap {|o| def o.to_int = 2 })"#,
            // With a comparator block, and over the other element types.
            "[3, 1, 2].min(2) {|a, b| b <=> a}",
            "[3, 1, 2].max(2) {|a, b| b <=> a}",
            r#"["b", "a", "c"].min(2)"#,
            r#"["b", "a", "c"].max(2)"#,
            "[3.5, 1.5, 2.5].min(2)",
            "[3.5, 1.5, 2.5].max(2)",
            // The receiver is not reordered by the ranking.
            "a = [3, 1, 2]; a.min(2); a",
            "a = [3, 1, 2]; a.max(2); a",
        ]);
        run_test_error("[3, 1, 2].min(-1)");
        run_test_error("[3, 1, 2].max(-2)");
        run_test_error(r#"[3, 1, 2].min("a")"#);
        run_test_error("[3, 1, 2].min(2**70)");
        run_test_error(r#"[1, "a"].min(2)"#);
    }

    #[test]
    fn min_max_with_block() {
        run_tests(&[
            // min with block
            "[2, 33, 4, 11].min {|a, b| a <=> b}",
            r#"["2","33","4","11"].min {|a,b| a.length <=> b.length}"#,
            "[2, 33, 4, 11].min {|a, b| b <=> a}",
            // max with block
            "[2, 33, 4, 11].max {|a, b| a <=> b}",
            r#"["2","33","4","11"].max {|a,b| a.length <=> b.length}"#,
            "[2, 33, 4, 11].max {|a, b| b <=> a}",
            // block returning constant
            "[1, 2, 3, 4].min {|a,b| 15}",
            "[1, 2, 3, 4].max {|a,b| 15}",
        ]);
        // nil block result raises ArgumentError
        run_test_error("[11, 12, 22, 33].min {|a, b| nil}");
        run_test_error("[11, 12, 22, 33].max {|a, b| nil}");
        run_tests(&[
            // empty array
            "[].min {|a,b| a <=> b}",
            "[].max {|a,b| a <=> b}",
        ]);
    }

    #[test]
    fn inspect_noncompat_component_escaped() {
        // When a component's #inspect returns a UTF-16/UTF-32 string,
        // Array#inspect / Hash#inspect must not splice the raw bytes
        // (or raise) — ASCII stays, non-ASCII becomes \uXXXX.
        run_tests(&[
            r#"
            o = Object.new
            def o.inspect; "abあc".encode("UTF-16BE"); end
            [o].inspect
            "#,
            r#"
            o = Object.new
            def o.inspect; "x\u{1F600}y".encode("UTF-32LE"); end
            [1, o].inspect
            "#,
            r#"
            o = Object.new
            def o.inspect; "kあ".encode("UTF-16LE"); end
            { a: o }.inspect
            "#,
            r#"
            o = Object.new
            def o.inspect; "v".encode("UTF-16BE"); end
            { 1 => o }.to_s
            "#,
        ]);
    }

    #[test]
    fn assoc() {
        run_tests(&[
            r#"
            s1 = [1, 2]
            s2 = [2, 3]
            a = [s1, s2]
            [a.assoc(1), a.assoc(2), a.assoc(42)]
            "#,
            // ignores non-array elements
            r#"["foo", [1, 2], [3, 4]].assoc(3)"#,
            r#"["foo", [1, 2], [3, 4]].assoc("bar")"#,
            r#"
            a = [[1, "one"], [2, "two"], [3, "three"]]
            [a.rassoc("two"), a.rassoc("four")]
            "#,
            // ignores non-array elements
            r#"["foo", [1, 2], [3, 4]].rassoc(4)"#,
            r#"["foo", [1, 2], [3, 4]].rassoc(99)"#,
            r#"
            a = [1, 2, 3, 4, 5]
            a.keep_if { |x| x > 2 }
            a
            "#,
            // returns self even when no changes
            r#"
            a = [1, 2, 3]
            a.keep_if { |x| true }.equal?(a)
            "#,
            "[1, 2, 3].keep_if { |x| x.odd? }",
            "[1, 2].append(3, 4)",
            "[].append(1)",
            r#"
            a = [1]
            a.append(2, 3)
            a
            "#,
            "[1, 2, 3, 4, 5].drop_while { |x| x < 3 }",
            "[1, 2, 3].drop_while { |x| true }",
            "[1, 2, 3].drop_while { |x| false }",
            "[].drop_while { |x| true }",
            "[1, 2, 3].to_ary",
            r#"
            a = [1, 2]
            a.to_ary.equal?(a)
            "#,
            "[1, 2, 3].deconstruct",
            r#"
            a = [1, 2]
            a.deconstruct.equal?(a)
            "#,
            // filter!/select! on frozen array without block should return Enumerator
            r#"[1,2,3].freeze.select!.class"#,
            r#"[1,2,3].freeze.filter!.class"#,
            // keep_if on frozen array without block should return Enumerator
            r#"[1,2,3].freeze.keep_if.class"#,
            // reject! on frozen array without block should return Enumerator
            r#"[1,2,3].freeze.reject!.class"#,
            // delete_if on frozen array without block should return Enumerator
            r#"[1,2,3].freeze.delete_if.class"#,
            // sort_by! on frozen array without block should return Enumerator
            r#"[1,2,3].freeze.sort_by!.class"#,
            // When pattern argument is given, block should be ignored
            r#"[1, 2, 3].all?(Integer) { |x| false }"#,
            r#"[1, 2, 3].any?(String) { |x| true }"#,
            r#"[1, 2, 3].none?(String) { |x| true }"#,
            r#"[1, 2, 3].one?(Integer) { |x| false }"#,
            r#"[1, 2, 3].count(2) { |x| true }"#,
            // inject(init, sym) ignores the block (a warning goes to stderr,
            // checked by ruby/spec; here we confirm the result is the folded
            // value, not the block's).
            r#"[1, 2, 3].inject(10, :-) { raise "never" }"#,
            r#"[1, 2, 3, 4].inject(0, :+) { 99 }"#,
            // inject with a block re-reads the receiver each step, so
            // appending inside the block is tolerated and the new elements
            // are folded in (CRuby).
            r#"
            a = [:a, :b, :c]
            i = 0
            visited = []
            a.inject(nil) { |_, e| visited << e; a << i if i < 5; i += 1 }
            visited
            "#,
            // The no-initial-value block form uses the first element as the
            // seed and still tolerates growth.
            r#"
            a = [1, 2, 3]
            n = 0
            a.inject { |s, e| a << 10 if n < 2; n += 1; s + e }
            "#,
            // No initial value and an empty receiver yields nil.
            r#"[].inject { |s, e| s + e }"#,
        ]);
    }

    #[test]
    fn sort_min_max_non_integer_comparator() {
        // Companion to #1076: `sort` / `min` / `max` / `minmax` all route a
        // comparator block's result through the same `rb_cmpint` rules, so a
        // Float, Bignum or Rational comparator sorts correctly...
        run_test(
            r#"
            a = [3, 1, 2]
            [
              a.sort { |x, y| (x <=> y) * 1.0 },
              a.sort { |x, y| (x <=> y) * (10 ** 20) },
              a.min { |x, y| (x <=> y) * 1.0 },
              a.max { |x, y| (x <=> y) * 1.0 },
              a.minmax { |x, y| Rational(x <=> y, 3) },
            ]
            "#,
        );
        // ...and an incomparable result raises with CRuby's wording: `nil`
        // names the two elements' classes, while a non-numeric result names
        // itself against the 0 it was compared with.
        run_test(
            r#"
            a = [[1], [2]]
            f = ->(m) { begin; a.send(m) { |x, y| nil }; rescue => e; [e.class, e.message]; end }
            g = ->(m) { begin; a.send(m) { |x, y| "z" }; rescue => e; [e.class, e.message]; end }
            [:sort, :min, :max, :minmax].map { |m| [f.(m), g.(m)] }
            "#,
        );
    }

    #[test]
    fn set_ops_small_arrays_skip_hash() {
        // CRuby's SMALL_ARRAY_LEN rules: up to 16 elements the set
        // operations compare with `eql?` and never send `#hash`.
        run_test_once(
            r##"
            poison = Object.new
            def poison.hash = raise("#hash called")
            a = Object.new
            r = []
            r << ([a, poison] - [poison]).size
            r << ([a, poison] & [poison]).size
            r << ([a, poison] | [poison]).size
            r << [a, poison].union([poison], [a]).size
            r << [a, poison].difference([poison], [a]).size
            r << [a, poison].intersection([poison], [poison]).size
            r << (Array.new(16) { Object.new } & Array.new(16, poison)).size
            r << (Array.new(17) { Object.new } - Array.new(16, poison)).size
            r << ((Array.new(17) { Object.new } & Array.new(16, poison)) rescue $!.message)
            r << ((Array.new(17) { Object.new } - Array.new(17, poison)) rescue $!.message)
            r << ((Array.new(16) { Object.new } | [poison]) rescue $!.message)
            r << (Array.new(17) { Object.new }.difference(Array.new(17, poison)) rescue $!.message)
            r << [1, 1].intersection << [1, 1, 2].intersection([2, 1, 1])
            r << ([1, 1.0, 1] | [1.0]) << ([1, 2, 2, 3] & [2, 3, 3])
            r
            "##,
        );
    }

    /// `sum` with a block keeps its running total — a fresh String, Array
    /// or Rational — rooted across the block, which runs Ruby (here a
    /// collection), and across `+`.
    #[test]
    fn sum_keeps_accumulator_across_block() {
        run_test_once(
            r##"
            r = nil
            20.times do
              r = [
                %w[a b c d e].sum("") { |w| GC.start; w.upcase * 2 },
                (1..8).to_a.sum(0r) { |x| GC.start; Rational(x, 7) },
                [1, 2, 3].sum([]) { |x| GC.start; [x.to_s * 2] },
              ]
            end
            r
            "##,
        );
    }

    /// `to_h` keeps the pair the block (or an element's `#to_ary`) made
    /// rooted while the key's `#hash` runs, here with a collection in it.
    #[test]
    fn to_h_keeps_pair_across_key_hash() {
        run_test_once(
            r##"
            class GcKey
              def initialize(n) = @n = n
              attr_reader :n
              def hash = (GC.start; @n.hash)
              def eql?(o) = o.is_a?(GcKey) && o.n == @n
            end
            class GcPair
              def initialize(n) = @n = n
              def to_ary = [GcKey.new(@n), "ary#{@n}" * 2]
            end
            r = nil
            20.times do
              r = [
                (1..5).to_a.to_h { |x| [GcKey.new(x), "val#{x}" * 2] }.values,
                (1..3).map { GcPair.new(_1) }.to_h.values,
              ]
            end
            r
            "##,
        );
    }

    /// `flatten`, `flatten!`, `join` and `*` keep a fresh Array an
    /// element's `#to_ary` answered (as ActiveRecord's `Relation#to_ary`
    /// answers `records.dup`) rooted while its own elements are probed,
    /// with a collection inside the probe.
    #[test]
    fn flatten_and_join_root_to_ary_results() {
        run_test_once(
            r##"
            class GcElem
              def initialize(n) = @n = n
              def to_ary = (GC.start; nil)
              def to_s = (GC.start; "g#{@n}")
              def inspect = "g#{@n}"
            end
            class GcRows
              def initialize(n) = @n = n
              def to_ary = ["s#{@n}" * 2, GcElem.new(@n), "t#{@n}" * 2]
            end
            r = nil
            20.times do
              a = [GcRows.new(1), GcRows.new(2)]
              b = [GcRows.new(3), [GcRows.new(4)]]
              b.flatten!
              r = [a.flatten, b, a.join(","), a * "-", a.flatten(1)]
            end
            r.map { |x| x.is_a?(Array) ? x.map { _1.is_a?(GcElem) ? _1.inspect : _1 } : x }
            "##,
        );
    }

    /// `transpose` keeps every row `#to_ary` made rooted while the next
    /// row's `#to_ary` runs, and `<=>` keeps the `#to_ary` answer rooted
    /// across the elements' `<=>`.
    #[test]
    fn transpose_and_cmp_root_to_ary_results() {
        run_test_once(
            r##"
            class GcRow
              def initialize(n) = @n = n
              def to_ary = (GC.start; ["a#{@n}" * 2, "b#{@n}" * 2])
            end
            class GcCmp
              def <=>(o) = (GC.start; 0)
            end
            class GcCmpRow
              def to_ary = [GcCmp.new, "x" * 3, GcCmp.new, "y" * 3]
            end
            r = nil
            20.times do
              r = [
                [GcRow.new(1), GcRow.new(2), GcRow.new(3)].transpose,
                [GcCmp.new, GcCmp.new, GcCmp.new, GcCmp.new] <=> GcCmpRow.new,
              ]
            end
            r
            "##,
        );
    }

    /// `select`, `reject`, `partition` and `flat_map` hold what they have
    /// collected in rooted Arrays, and `flat_map` reads the receiver as it
    /// goes: a block that drops elements from the receiver (and collects)
    /// must not leave them to be freed.
    #[test]
    fn select_family_roots_collected_elements() {
        run_test_once(
            r##"
            fresh = -> { (1..6).map { |i| "e#{i}" * 3 } }
            r = nil
            20.times do
              a = fresh.()
              k = 0
              sel = a.select { |x| a[k - 1] = "z" if k > 0; k += 1; GC.start; true }
              a = fresh.()
              rej = a.reject { |x| a.shift if a.size > 3; GC.start; false }
              a = fresh.()
              par = a.partition { |x| a.shift if a.size > 3; GC.start; true }
              a = fresh.()
              fm = a.flat_map { |x| a.pop if a.size > 3; GC.start; [x] }
              r = [sel, rej, par, fm]
            end
            r
            "##,
        );
    }
}
