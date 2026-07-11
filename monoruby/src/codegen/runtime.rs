use super::*;

mod args;
pub(crate) use args::*;

pub const PROCDATA_OUTER: i64 = std::mem::offset_of!(ProcData, outer) as _;
pub const PROCDATA_FUNCID: i64 = std::mem::offset_of!(ProcData, func_id) as _;

//
// Runtime functions.
//

/// Resolve the method for a call-site inline-cache miss.
///
/// Returns the resolved `FuncId` in the **low 32 bits** (0 = not found /
/// method_missing; the error, if any, is set on the executor) and the
/// `ClassId` to **tag the inline cache with** in the **high 32 bits**.
///
/// For most receivers the tag is the receiver's class. `true` / `false`
/// are unified under `BOOL_CLASS` *only when both `TrueClass` and
/// `FalseClass` resolve the name to the same method* (the
/// `check_bool_method_with_version` rule). Otherwise the cache is tagged
/// with the receiver's real class, so the *other* boolean misses and
/// re-resolves instead of hitting this entry and running the wrong
/// class's method (#713). Super dispatch (no callsite name) is tagged
/// with the real class unconditionally.
pub(super) extern "C" fn find_method(
    vm: &mut Executor,
    globals: &mut Globals,
    callid: CallSiteId,
    recv: Value,
) -> u64 {
    let name = globals[callid].name;
    let fid = if let Some(func_name) = name {
        let is_func_call = globals[callid].is_func_call();
        vm.find_method(globals, recv, func_name, is_func_call)
    } else {
        find_super(vm, globals, callid)
    }
    .map_err(|err| vm.set_error(err))
    .ok();
    if let Some(f) = fid {
        warn_unused_block(vm, globals, callid, f);
    }
    let cache_class = {
        let ic_class = recv.class_for_ic();
        if ic_class == BOOL_CLASS {
            let unified = match name {
                Some(name) => globals.store.check_method_for_class(BOOL_CLASS, name).is_some(),
                None => false,
            };
            if unified { BOOL_CLASS } else { recv.class() }
        } else {
            ic_class
        }
    };
    ((cache_class.u32() as u64) << 32) | fid.map_or(0, |f| f.get()) as u64
}

/// CRuby's Ruby-3.4 "unused block" warning: calling a method with a
/// literal block when the callee's body neither declares a block
/// parameter (named, anonymous, or `...`) nor uses the block (`yield` /
/// `super`; `block_given?` alone does not count). Gated on `$VERBOSE ==
/// true` or the `Warning[:strict_unused_block]` category. Running only
/// on the inline-cache-miss path also gives CRuby's once-per-call-site
/// behavior — subsequent calls hit the cache and skip this.
fn warn_unused_block(vm: &mut Executor, globals: &mut Globals, callid: CallSiteId, fid: FuncId) {
    let site = &globals[callid];
    if site.name.is_none() || site.block_fid.is_none() {
        return;
    }
    let callee = &globals.store[fid];
    if !callee.is_method() {
        return;
    }
    let Some(iseq_id) = callee.is_iseq() else {
        return;
    };
    let iseq = &globals.store[iseq_id];
    if iseq.uses_block || iseq.block_param().is_some() || iseq.args.forwarding() {
        return;
    }
    // CRuby dedup: once per callee method. The method is recorded even
    // when the gate below suppresses the printing — a later gated-on
    // call of an already-seen method stays silent (observable in
    // ruby/spec's strict_unused_block examples).
    if !globals.unused_block_warned.insert(fid.get() as u64) {
        return;
    }
    let verbose = globals
        .get_gvar(IdentId::get_id("$VERBOSE"))
        .is_some_and(|v| v.as_bool());
    if !verbose && !strict_unused_block_category(vm, globals) {
        return;
    }
    // Re-borrow after the potential Ruby invocation above.
    let iseq = &globals.store[iseq_id];
    let defined_at = format!(
        "{}:{}",
        iseq.sourceinfo.file_name(),
        iseq.sourceinfo.get_line(&iseq.loc)
    );
    let caller = {
        let caller_fid = vm.cfp().lfp().func_id();
        let Some(caller_iseq) = globals.store[caller_fid].is_iseq() else {
            return;
        };
        let caller_iseq = &globals.store[caller_iseq];
        let bc_pos = globals[callid].bc_pos.to_usize();
        let Some(loc) = caller_iseq.sourcemap.get(bc_pos).copied() else {
            return;
        };
        format!(
            "{}:{}",
            caller_iseq.sourceinfo.file_name(),
            caller_iseq.sourceinfo.get_line(&loc)
        )
    };
    // CRuby names the callee in the qualified `Owner#name` form (bare
    // name for a plain object's singleton method) — same rule as
    // backtrace entries, so reuse func_description.
    let name = globals.store.func_description(fid);
    let msg = format!(
        "{caller}: warning: the block passed to '{name}' defined at {defined_at} may be ignored\n"
    );
    let stderr = globals
        .get_gvar(IdentId::get_id("$stderr"))
        .unwrap_or_default();
    // A warning must never mask the call's result — ignore errors from
    // a broken/replaced $stderr.
    let _ = vm.invoke_method_inner(
        globals,
        IdentId::get_id("write"),
        stderr,
        &[Value::string(msg)],
        None,
        None,
    );
}

/// `Warning[:strict_unused_block]` — `false` when the Warning module
/// isn't loaded yet (bootstrap) or the lookup fails.
fn strict_unused_block_category(vm: &mut Executor, globals: &mut Globals) -> bool {
    let Some(warning_val) = globals
        .store
        .get_constant_noautoload(OBJECT_CLASS, IdentId::get_id("Warning"))
    else {
        return false;
    };
    let sym = Value::symbol(IdentId::get_id("strict_unused_block"));
    match vm.invoke_method_inner(globals, IdentId::_INDEX, warning_val, &[sym], None, None) {
        Ok(v) => v.as_bool(),
        Err(_) => false,
    }
}

fn find_super(vm: &mut Executor, globals: &mut Globals, callid: CallSiteId) -> Result<FuncId> {
    let func_id = vm.method_func_id();
    // zsuper (implicit-argument `super`) forwards the frame's
    // arguments by the *definition-time* parameter layout, which is
    // meaningless for a method created by define_method (the block's
    // captured outer frame is not this call's frame). CRuby raises
    // RuntimeError at call time; `super()` / explicit arguments are
    // fine. zsuper is the only super shape compiled with the
    // `forwarding` flag set on its callsite.
    if globals[callid].forwarding && globals.store[func_id].is_block_style() {
        return Err(MonorubyErr::runtimeerr(
            "implicit argument passing of super from method defined by define_method() is not supported. Specify all arguments explicitly.",
        ));
    }
    let self_val = vm.cfp().lfp().self_val();
    let func_name = globals.store[func_id].name().unwrap();
    let self_class = self_val.class();
    match globals.store.check_super(self_class, func_id, func_name) {
        Some(func_id) => Ok(func_id),
        None => Err(MonorubyErr::super_method_not_found(
            globals, func_name, self_val,
        )),
    }
}

pub(super) extern "C" fn enter_classdef<'a>(
    vm: &mut Executor,
    globals: &'a mut Globals,
    func_id: FuncId,
    self_value: Module,
) -> &'a FuncData {
    // The class definition's lexical context inherits from the
    // enclosing Ruby method. Walk past any builtin frames (Module#class_eval
    // string form, mspec, …) to find one — without this, the
    // `iseq(current_func)` lookup below panics when the immediate
    // outer is a builtin. If no Ruby frame is reachable at all (eval
    // body executed at the very top of the cfp stack), start with an
    // empty context.
    let mut lexical_context = {
        let mut frame = Some(vm.cfp());
        let mut found: Option<&[ClassId]> = None;
        while let Some(cfp) = frame {
            let fid = cfp.lfp().outermost().0.func_id();
            if let Some(iseq) = globals.store[fid].is_iseq() {
                found = Some(globals.store[iseq].lexical_context.as_slice());
                break;
            }
            frame = cfp.prev();
        }
        found.map(|s| s.to_vec()).unwrap_or_default()
    };
    lexical_context.push(self_value.id());
    if let Some(info) = globals.store.iseq_mut(func_id) {
        info.lexical_context = lexical_context;
    }
    globals.get_func_data(func_id)
}

pub(super) extern "C" fn exit_classdef(vm: &mut Executor, _globals: &mut Globals) {
    vm.pop_class_context();
}

#[derive(Debug, Clone, Default)]
#[repr(C)]
pub(crate) struct ProcData {
    outer: Option<Lfp>,
    func_id: Option<FuncId>,
}

impl ProcData {
    pub(crate) fn new(outer: Lfp, func_id: FuncId) -> Self {
        Self {
            outer: Some(outer),
            func_id: Some(func_id),
        }
    }

    pub(crate) fn func_id(&self) -> Option<FuncId> {
        self.func_id
    }

    pub(crate) fn from_proc(proc: &ProcInner) -> Self {
        Self {
            outer: proc.outer_lfp(),
            func_id: Some(proc.func_id()),
        }
    }

}

///
/// Get *BlockData* for yield.
///
/// ### in
/// - rdi: &mut Executor
/// - rsi: &mut Globals
///
/// ### out
/// - rax: outer Lfp
/// - rdx: FuncId
///
pub(super) extern "C" fn get_yield_data(vm: &mut Executor, globals: &mut Globals) -> ProcData {
    let bh = match vm.get_block() {
        Some(data) => data,
        None => {
            vm.set_error(MonorubyErr::no_block_given());
            return ProcData::default();
        }
    };
    match vm.get_block_data(globals, bh) {
        Ok(data) => data,
        Err(err) => {
            vm.set_error(err);
            ProcData::default()
        }
    }
}

pub(super) extern "C" fn block_arg(
    vm: &mut Executor,
    globals: &mut Globals,
    mut lfp: Lfp,
    pc: BytecodePtr,
) -> Option<Value> {
    let outer = pc.op1() as u32;
    for _ in 0..outer {
        lfp = lfp.outer().unwrap();
    }
    let bh = match lfp.block() {
        Some(bh) => bh,
        None => {
            return Some(Value::nil());
        }
    };
    if bh.get().is_nil() {
        return Some(Value::nil());
    }
    let mut cfp = vm.cfp();
    while cfp.lfp() != lfp {
        cfp = Executor::prev_cfp(vm, cfp).1;
    }
    match vm.generate_proc_inner(globals, cfp, bh, pc) {
        Ok(val) => Some(val.into()),
        Err(err) => {
            vm.set_error(err);
            None
        }
    }
}

pub(super) extern "C" fn gen_array(
    _vm: &mut Executor,
    globals: &mut Globals,
    callid: CallSiteId,
    ptr: *const Value,
) -> Option<Value> {
    let callsite = &globals.store[callid];
    if callsite.pos_num == 0 {
        Some(Value::array_empty())
    } else {
        let len = callsite.pos_num;
        let src = unsafe { ptr.sub(callsite.args.0 as usize) };
        let iter = unsafe {
            std::slice::from_raw_parts(src.sub(len - 1), len)
                .iter()
                .rev()
                .cloned()
        };
        if callsite.splat_pos.is_empty() {
            Some(Value::array_from_iter(iter))
        } else {
            let mut ary = Array::new_empty();
            for (i, v) in iter.enumerate() {
                if globals.store[callid].splat_pos.contains(&i) {
                    let a = v.try_array_ty().expect("splat arguments must be Array.");
                    ary.extend_from_slice(&a);
                } else {
                    ary.push(v);
                }
            }
            Some(ary.into())
        }
    }
}

pub(super) extern "C" fn array_teq(
    vm: &mut Executor,
    globals: &mut Globals,
    lhs: Value,
    rhs: Value,
) -> Option<Value> {
    array_teq_impl(vm, globals, lhs, rhs, op::cmp_teq_case_values)
}

/// `rescue *list` clause match (opcode 44): like `array_teq`, but each
/// element must be a Class or Module — `cmp_teq_rescue_values` raises
/// CRuby's "class or module required for rescue clause" TypeError
/// otherwise.
pub(super) extern "C" fn rescue_array_teq(
    vm: &mut Executor,
    globals: &mut Globals,
    lhs: Value,
    rhs: Value,
) -> Option<Value> {
    array_teq_impl(vm, globals, lhs, rhs, op::cmp_teq_rescue_values)
}

fn array_teq_impl(
    vm: &mut Executor,
    globals: &mut Globals,
    lhs: Value,
    rhs: Value,
    teq: extern "C" fn(&mut Executor, &mut Globals, Value, Value) -> Option<Value>,
) -> Option<Value> {
    if let Some(lhs_ary) = lhs.try_array_ty() {
        for lhs in lhs_ary.iter().cloned() {
            if teq(vm, globals, lhs, rhs)?.as_bool() {
                return Some(Value::bool(true));
            }
        }
        Some(Value::bool(false))
    } else {
        teq(vm, globals, lhs, rhs)
    }
}

/// Subject-less `case`/`when *arr` match: true iff any element of `arr` is
/// truthy. Mirrors CRuby's `checkmatch` with `VM_CHECKMATCH_TYPE_WHEN |
/// VM_CHECKMATCH_ARRAY` — plain truthiness (`RTEST`) of each element, with
/// no `===` and no user-visible method call. `val` is always an Array here
/// (the caller wraps the splat in an array literal); a non-Array is treated
/// as its own truthiness for safety.
pub(super) extern "C" fn array_any(_vm: &mut Executor, _globals: &mut Globals, val: Value) -> Value {
    let any = if let Some(ary) = val.try_array_ty() {
        ary.iter().any(|e| e.as_bool())
    } else {
        val.as_bool()
    };
    Value::bool(any)
}

pub(super) extern "C" fn gen_lambda(
    vm: &mut Executor,
    globals: &mut Globals,
    func_id: FuncId,
    pc: BytecodePtr,
) -> Value {
    vm.generate_lambda(globals, func_id, pc).into()
}

pub(super) extern "C" fn gen_hash(
    vm: &mut Executor,
    globals: &mut Globals,
    src: *const Value,
    len: usize,
) -> Option<Value> {
    match gen_hash_inner(vm, globals, src, len) {
        Ok(map) => Some(Value::hash(map)),
        Err(err) => {
            vm.set_error(err);
            None
        }
    }
}

fn gen_hash_inner(
    vm: &mut Executor,
    globals: &mut Globals,
    src: *const Value,
    len: usize,
) -> Result<RubyMap<Value, Value>> {
    let mut map = RubyMap::default();
    if len > 0 {
        let mut iter = unsafe { std::slice::from_raw_parts(src.sub(len * 2 - 1), len * 2) }
            .iter()
            .copied()
            .rev();
        while let Ok(chunk) = iter.next_chunk::<2>() {
            map.insert(chunk[0].frozen_hash_key(), chunk[1], vm, globals)?;
        }
    }
    Ok(map)
}

///
/// Insert `len` key/value pairs (the `len * 2` slots ending at `src`) into
/// the Hash `hash`. Used for the 2nd and later chunks of a chunked Hash
/// literal (op 42); duplicate keys overwrite, like `gen_hash`.
///
pub(super) extern "C" fn hash_insert(
    vm: &mut Executor,
    globals: &mut Globals,
    src: *const Value,
    len: usize,
    hash: Value,
) -> Option<Value> {
    let mut h = hash.as_hash();
    if len > 0 {
        // SAFETY: the bytecode compiler guarantees `len * 2` consecutive
        // value slots ending at `src` (same layout as `gen_hash`).
        let mut iter = unsafe { std::slice::from_raw_parts(src.sub(len * 2 - 1), len * 2) }
            .iter()
            .copied()
            .rev();
        while let Ok(chunk) = iter.next_chunk::<2>() {
            if let Err(err) = h.insert(chunk[0].frozen_hash_key(), chunk[1], vm, globals) {
                vm.set_error(err);
                return None;
            }
        }
    }
    Some(hash)
}

///
/// Concatenate the Array `src` onto the Array `dst`. Used for the 2nd and
/// later chunks of a chunked Array literal (op 41).
///
pub(super) extern "C" fn array_concat(
    _vm: &mut Executor,
    _globals: &mut Globals,
    dst: Value,
    src: Value,
) -> Option<Value> {
    let mut d = dst.as_array();
    let s = src.as_array();
    d.extend_from_slice(&s);
    Some(dst)
}

pub(super) extern "C" fn empty_hash() -> Value {
    let map = RubyMap::default();
    Value::hash(map)
}

pub(super) extern "C" fn gen_range(
    start: Value,
    end: Value,
    vm: &mut Executor,
    globals: &mut Globals,
    exclude_end: bool,
) -> Option<Value> {
    // Validate with `<=>` only when the endpoints are different classes
    // — same-class endpoints are always allowed (matches CRuby for
    // user-defined types, and avoids false positives when monoruby's
    // own `<=>` is incomplete e.g. on Time). Different-class endpoints
    // like `9155.."s"` still need to be rejected (`<=>` returns nil).
    if !start.is_nil()
        && !end.is_nil()
        && start.real_class(&globals.store).id() != end.real_class(&globals.store).id()
    {
        match vm.compare_values_inner(globals, start, end) {
            Ok(Some(_)) => {}
            Ok(None) => {
                vm.set_error(MonorubyErr::argumenterr("bad value for range"));
                return None;
            }
            Err(err) => {
                vm.set_error(err);
                return None;
            }
        }
    }
    Some(Value::range(start, end, exclude_end))
}

pub(super) extern "C" fn concatenate_string(
    vm: &mut Executor,
    globals: &mut Globals,
    arg: *mut Value,
    len: usize,
) -> Option<Value> {
    concatenate_string_inner(vm, globals, arg, len)
        .map_err(|err| vm.set_error(err))
        .ok()
}

fn concatenate_string_inner(
    vm: &mut Executor,
    globals: &mut Globals,
    arg: *mut Value,
    len: usize,
) -> Result<Value> {
    use crate::value::rvalue::{Encoding, RStringInner};
    // Build the result as raw bytes so invalid byte sequences in any
    // operand survive interpolation (going through a Rust `String`
    // would silently rewrite them as U+FFFD via `from_utf8_lossy`).
    // The result seeds its encoding from the FIRST string operand —
    // bytecodegen guarantees an interpolation starts with a (possibly
    // empty) literal segment carrying the source encoding — and each
    // further operand negotiates under CRuby's `compatible_encoding`
    // rules; an incompatible mix (two non-ASCII operands in different
    // encodings) raises Encoding::CompatibilityError like `<<`.
    let mut bytes: Vec<u8> = Vec::new();
    let mut enc: Option<Encoding> = None;
    for i in 0..len {
        let v = unsafe { *arg.sub(i) };
        let s_val = vm.invoke_tos(globals, v)?;
        if let Some(inner) = s_val.is_rstring_inner() {
            enc = Some(match enc {
                None => inner.encoding(),
                Some(prev) => RStringInner::from_encoding(&bytes, prev)
                    .compatible_encoding(&inner)
                    .ok_or_else(|| {
                        MonorubyErr::incompatible_encoding(
                            &globals.store,
                            prev,
                            inner.encoding(),
                        )
                    })?,
            });
            bytes.extend_from_slice(inner.as_bytes());
        } else {
            // `invoke_tos` returns the user-defined `to_s` result
            // verbatim for `RV::Object` receivers, so this branch is
            // only reached when that override returned a non-String.
            // Per CRuby, the bogus result is discarded and the
            // default `Object#to_s` form (`#<ClassName:0xADDR>`) of
            // the original receiver is emitted instead.
            let s = format!(
                "#<{}:0x{:016x}>",
                v.get_real_class_name(&globals.store),
                v.id()
            );
            enc = Some(match enc {
                None => Encoding::Utf8,
                Some(prev) => prev,
            });
            bytes.extend_from_slice(s.as_bytes());
        }
    }
    Ok(Value::string_from_inner(RStringInner::from_encoding(
        &bytes,
        enc.unwrap_or(Encoding::Utf8),
    )))
}

pub(super) extern "C" fn concatenate_regexp(
    vm: &mut Executor,
    globals: &mut Globals,
    arg: *mut Value,
    len: usize,
) -> Option<Value> {
    use crate::value::rvalue::Encoding;
    // Build the interpolated source as a String first, so each operand's
    // bytes and encoding combine under the same rules as `"#{}"` — a
    // non-ASCII embedded String (e.g. EUC-JP) then upgrades the regexp's
    // encoding (`/#{euc_str}/.encoding == EUC-JP`).
    let s_val = match concatenate_string_inner(vm, globals, arg, len) {
        Ok(v) => v,
        Err(err) => {
            vm.set_error(err);
            return None;
        }
    };
    let inner = s_val.as_rstring_inner();
    let bytes = inner.as_bytes().to_vec();
    let enc = inner.encoding();
    // The matching engine only understands UTF-8/ASCII; feed it a
    // best-effort UTF-8 view while the raw bytes + encoding drive
    // `Regexp#source` / `#encoding`.
    let reg_str = String::from_utf8_lossy(&bytes).into_owned();
    let onig_enc = if enc == Encoding::Ascii8 {
        onigmo_regex::OnigmoEncoding::ASCII
    } else {
        onigmo_regex::OnigmoEncoding::UTF8
    };
    let inner = match RegexpInner::with_option_kcode_source(
        reg_str,
        0,
        onig_enc,
        None,
        Some(enc),
        Some(bytes),
    ) {
        Ok(inner) => inner,
        Err(err) => {
            vm.set_error(err);
            return None;
        }
    };
    Some(Value::regexp(inner))
}

pub(super) extern "C" fn expand_array(
    vm: &mut Executor,
    globals: &mut Globals,
    src: Value,
    dst: *mut Value,
    len: usize,
    rest: usize,
) -> Option<Value> {
    // Destructuring (multiple assignment and block/proc `|(a, b)|` params)
    // coerces a non-Array `src` once via `#to_ary`: an Array result is
    // expanded, `nil` or a missing `#to_ary` leaves `src` a scalar, and any
    // other result raises `TypeError`. Returns `None` (a null in `rax`) so
    // the VM / JIT error path fires.
    //
    // CRuby gates the `#to_ary` call on `respond_to?(:to_ary, true)` — the
    // *dynamic* predicate, which a user may override — rather than a raw
    // method-table lookup, so honour an overridden `respond_to?` here too.
    let src = if src.is_array_ty() {
        src
    } else if globals
        .check_method(src, IdentId::get_id("respond_to?"))
        .is_none()
    {
        // An object that does not even respond to `#respond_to?` (a bare
        // `BasicObject`) cannot be coerced: leave it a scalar rather than
        // raising `NoMethodError`, matching CRuby.
        src
    } else if match vm.invoke_method_inner(
        globals,
        IdentId::get_id("respond_to?"),
        src,
        &[Value::symbol(IdentId::TO_ARY), Value::bool(true)],
        None,
        None,
    ) {
        Ok(v) => v.as_bool(),
        Err(err) => {
            vm.set_error(err);
            return None;
        }
    } {
        match vm.invoke_method_inner(globals, IdentId::TO_ARY, src, &[], None, None) {
            Ok(v) if v.is_array_ty() => v,
            Ok(v) if v.is_nil() => src,
            Ok(v) => {
                vm.set_error(MonorubyErr::cant_convert_error_ary(globals, src, v));
                return None;
            }
            Err(err) => {
                vm.set_error(err);
                return None;
            }
        }
    } else {
        src
    };
    let rest_pos: Option<usize> = if rest == 0 { None } else { Some(rest - 1) };
    match src.try_array_ty() {
        Some(ary) => {
            if let Some(rest_pos) = rest_pos {
                if ary.len() >= len - 1 {
                    for i in 0..rest_pos {
                        unsafe { *dst.sub(i) = ary[i] }
                    }
                    unsafe {
                        *dst.sub(rest_pos) = Value::array_from_iter(
                            ary[rest_pos..ary.len() - (len - (rest_pos + 1))]
                                .iter()
                                .cloned(),
                        )
                    }
                    for i in rest_pos + 1..len {
                        unsafe { *dst.sub(i) = ary[ary.len() + i - len] }
                    }
                } else if ary.len() <= rest_pos {
                    for i in 0..ary.len() {
                        unsafe { *dst.sub(i) = ary[i] }
                    }
                    for i in ary.len()..rest_pos {
                        unsafe { *dst.sub(i) = Value::nil() }
                    }
                    unsafe { *dst.sub(rest_pos) = Value::array_empty() }
                    for i in rest_pos + 1..len {
                        unsafe { *dst.sub(i) = Value::nil() }
                    }
                } else {
                    for i in 0..rest_pos {
                        unsafe { *dst.sub(i) = ary[i] }
                    }
                    unsafe { *dst.sub(rest_pos) = Value::array_empty() }
                    for i in rest_pos + 1..ary.len() + 1 {
                        unsafe { *dst.sub(i) = ary[i - 1] }
                    }
                    for i in ary.len() + 1..len {
                        unsafe { *dst.sub(i) = Value::nil() }
                    }
                }
            } else {
                if len <= ary.len() {
                    for i in 0..len {
                        unsafe { *dst.sub(i) = ary[i] }
                    }
                } else {
                    for i in 0..ary.len() {
                        unsafe { *dst.sub(i) = ary[i] }
                    }
                    for i in ary.len()..len {
                        unsafe { *dst.sub(i) = Value::nil() }
                    }
                }
            }
        }
        None => {
            if let Some(rest_pos) = rest_pos {
                if len == 1 {
                    assert_eq!(rest_pos, 0);
                    unsafe { *dst = Value::array1(src) };
                } else if rest_pos == 0 {
                    unsafe { *dst = Value::array_empty() };
                    unsafe { *dst.sub(1) = src }
                    for i in 2..len {
                        unsafe { *dst.sub(i) = Value::nil() }
                    }
                } else {
                    unsafe { *dst = src };
                    for i in 1..len {
                        if i == rest_pos {
                            unsafe { *dst.sub(i) = Value::array_empty() };
                        } else {
                            unsafe { *dst.sub(i) = Value::nil() }
                        }
                    }
                }
            } else {
                unsafe { *dst = src };
                for i in 1..len {
                    unsafe { *dst.sub(i) = Value::nil() }
                }
            }
        }
    }
    Some(Value::nil())
}

pub(crate) extern "C" fn create_array(src: *mut Value, len: usize) -> Value {
    if len == 0 {
        return Value::array_empty();
    }
    let slice = unsafe { std::slice::from_raw_parts(src.sub(len - 1), len) };
    Value::array_from_iter(slice.iter().rev().copied())
}

#[repr(C)]
pub(super) struct RestKwData {
    name: Option<IdentId>,
    id: u32,
}

pub(super) extern "C" fn correct_rest_kw(mut ptr: *const RestKwData, lfp: Lfp) -> Value {
    let mut map = RubyMap::default();
    unsafe {
        while let RestKwData {
            name: Some(name),
            id,
        } = ptr.read()
        {
            let v = lfp.register(SlotId(id as u16)).unwrap();
            map.insert_sym(RubySymbol::new(name), v);
            ptr = ptr.add(1);
        }
    }
    Value::hash(map)
}

/// Diagnostic for the aarch64 VM bring-up: the unimplemented-opcode
/// dispatch slot calls this (with the opcode in x0) before trapping, so a
/// missing handler reports *which* opcode rather than a bare `brk`.
#[cfg(target_arch = "aarch64")]
pub extern "C" fn report_unimpl_op(op: u64) {
    eprintln!("[aarch64 VM] unimplemented opcode: {}", op);
}

/// Like `vm_get_constant`, but returns `nil` instead of raising when the
/// constant is undefined (the `CheckConst` op, used for conditional const
/// definition such as `X ||= ...`).
pub(crate) extern "C" fn opt_case(
    _vm: &mut Executor,
    globals: &mut Globals,
    callid: OptCaseId,
    idx: Value,
) -> u32 {
    globals.store[callid].find(idx)
}

pub(crate) extern "C" fn invoke_method_missing(
    vm: &mut Executor,
    globals: &mut Globals,
    receiver: Value,
    lfp: Lfp,
    callsite: CallSiteId,
) -> Option<Value> {
    if globals[callsite].name.is_none() {
        // A super call that found no superclass method: like CRuby,
        // fall through to method_missing with the calling method's
        // name (the default method_missing then raises the
        // super-flavored NoMethodError). One exception: the
        // define_method-zsuper RuntimeError from `find_super` is a
        // hard error, not a missing method.
        if vm
            .exception()
            .is_some_and(|err| !matches!(err.kind(), MonorubyErrKind::NotMethod { .. }))
        {
            return None;
        }
    }
    vm.discard_error();
    vm.invoke_method_missing(globals, receiver, lfp, callsite)
}

pub(crate) extern "C" fn vm_check_constant(
    vm: &mut Executor,
    globals: &mut Globals,
    site_id: ConstSiteId,
    const_version: usize,
) -> Option<Value> {
    if let Some(cache) = &globals.store[site_id].cache {
        let base_class = globals.store[site_id]
            .base
            .map(|base| unsafe { vm.get_slot(base) }.unwrap());
        if cache.version == const_version && cache.base_class == base_class {
            return Some(cache.value);
        };
    }
    // The `||=` / `&&=` definedness check must not fire `const_missing`
    // (CRuby checks definedness only), so use the no-missing resolver.
    match vm.find_constant_no_missing(globals, site_id) {
        Ok((value, base_class)) => {
            globals.store[site_id].cache = Some(ConstCache {
                version: const_version,
                base_class,
                value,
            });
            Some(value)
        }
        Err(_) => Some(Value::nil()),
    }
}

pub(crate) extern "C" fn vm_get_constant(
    vm: &mut Executor,
    globals: &mut Globals,
    site_id: ConstSiteId,
    const_version: usize,
) -> Option<Value> {
    if let Some(cache) = &globals.store[site_id].cache {
        let base_class = globals.store[site_id]
            .base
            .map(|base| unsafe { vm.get_slot(base) }.unwrap());
        if cache.version == const_version && cache.base_class == base_class {
            return Some(cache.value);
        };
    }
    match vm.find_constant(globals, site_id) {
        Ok((value, base_class)) => {
            globals.store[site_id].cache = Some(ConstCache {
                version: const_version,
                base_class,
                value,
            });
            Some(value)
        }
        Err(err) => {
            vm.set_error(err);
            None
        }
    }
}

pub(super) extern "C" fn vm_handle_arguments(
    vm: &mut Executor,
    globals: &mut Globals,
    caller_lfp: Lfp,
    callee_lfp: Lfp,
    callid: CallSiteId,
) -> Option<Value> {
    match set_frame_arguments(vm, globals, callee_lfp, caller_lfp, callid) {
        Ok(_) => {
            set_frame_block(&globals.store[callid], callee_lfp, caller_lfp);
            Some(Value::nil())
        }
        Err(mut err) => {
            err.push_internal_trace(callee_lfp.func_id());
            vm.set_error(err);
            None
        }
    }
}

pub(super) extern "C" fn jit_handle_arguments_no_block(
    vm: &mut Executor,
    globals: &mut Globals,
    caller_lfp: Lfp,
    callee_lfp: Lfp,
    callid: CallSiteId,
) -> Option<Value> {
    match set_frame_arguments(vm, globals, callee_lfp, caller_lfp, callid) {
        Ok(_) => Some(Value::nil()),
        Err(mut err) => {
            err.push_internal_trace(callee_lfp.func_id());
            vm.set_error(err);
            None
        }
    }
}

pub(super) extern "C" fn jit_handle_arguments_no_block_for_send(
    vm: &mut Executor,
    globals: &mut Globals,
    caller_lfp: Lfp,
    callee_lfp: Lfp,
    callid: CallSiteId,
) -> Option<Value> {
    let src = caller_lfp.register_ptr(globals.store[callid].args) as *const Value;
    match set_frame_arguments_simple(
        vm,
        globals,
        callee_lfp,
        caller_lfp,
        callid,
        unsafe { src.sub(1) },
        globals.store[callid].pos_num - 1,
    ) {
        Ok(_) => Some(Value::nil()),
        Err(err) => {
            vm.set_error(err);
            None
        }
    }
}

pub(super) extern "C" fn jit_handle_arguments_no_block_for_send_splat(
    vm: &mut Executor,
    globals: &mut Globals,
    caller_lfp: Lfp,
    callee_lfp: Lfp,
    callid: CallSiteId,
) -> Option<Value> {
    assert_eq!(globals.store[callid].pos_num, 1);
    let src = caller_lfp.register_ptr(globals.store[callid].args) as _;
    match set_frame_arguments_send_splat(globals, callee_lfp, src) {
        Ok(_) => Some(Value::nil()),
        Err(err) => {
            vm.set_error(err);
            None
        }
    }
}

#[repr(C)]
pub(super) struct ClassIdSlot {
    base: ClassId,
    idx: ClassId,
}

thread_local! {
    /// Cached `ClassId` of `Enumerator::ArithmeticSequence`. The
    /// class is defined in Ruby (`monoruby/builtins/enumerable.rb`)
    /// so its `ClassId` isn't known at compile time, but it's
    /// stable across the lifetime of a `Globals` once the startup
    /// files have loaded. We pay one constant lookup the first
    /// time, then a plain `ClassId` compare from there.
    static AS_CLASS_ID_CACHE: std::cell::Cell<Option<ClassId>> = const {
        std::cell::Cell::new(None)
    };
}

/// Fast check: is `v` an instance of `Enumerator::ArithmeticSequence`?
/// Mirrors the way Array's `[]` dispatch needs to recognise an AS
/// index — without the per-call cost of walking the class chain and
/// joining names into a string.
///
/// Uses `real_class` rather than `Value::class` so a singleton class
/// attached to the AS (e.g. `Range#%` calls `define_singleton_method`
/// to swap in a custom `inspect`) doesn't make the cached `ClassId`
/// compare miss. Without this, `arr[(0..-1).%(2)]` fell through to
/// the `to_int` path and raised `TypeError`.
pub(crate) fn is_arithmetic_sequence(globals: &Globals, v: Value) -> bool {
    let v_class = v.real_class(&globals.store).id();
    AS_CLASS_ID_CACHE.with(|cell| {
        if let Some(cached) = cell.get() {
            return v_class == cached;
        }
        // First-time lookup: resolve `Enumerator::ArithmeticSequence`
        // via the constant table. Cache the resulting `ClassId` so
        // subsequent calls bypass the lookup entirely.
        let Some(enum_const) = globals
            .store
            .get_constant(OBJECT_CLASS, IdentId::get_id("Enumerator"))
        else {
            return false;
        };
        let enum_class_id = match enum_const.loaded_value() {
            Some(val) if val.is_class_or_module().is_some() => val.as_class_id(),
            _ => return false,
        };
        let Some(as_const) = globals
            .store
            .get_constant(enum_class_id, IdentId::get_id("ArithmeticSequence"))
        else {
            return false;
        };
        let as_class_id = match as_const.loaded_value() {
            Some(val) if val.is_class_or_module().is_some() => val.as_class_id(),
            _ => return false,
        };
        cell.set(Some(as_class_id));
        v_class == as_class_id
    })
}

///
/// Generic index operation.
///
/// ### in
///
/// - base: Value
/// - index: Value
/// - class_slot: &mut ClassIdSlot
///
/// ### out
///
/// Some(Value) if succeeded.
/// None if failed.
///
pub(super) extern "C" fn get_index(
    vm: &mut Executor,
    globals: &mut Globals,
    base: Value,
    index: Value,
    class_slot: &mut ClassIdSlot,
) -> Option<Value> {
    let base_classid = base.class();
    class_slot.base = base_classid;
    class_slot.idx = index.class();
    match base_classid {
        ARRAY_CLASS => {
            // Non-fixnum, non-range index: ask the index to slice
            // the array itself. `Enumerator::ArithmeticSequence`
            // defines `#[]` (in `enumerable.rb`) for this — the
            // slicing logic lives on the AS, not parasitically in
            // Array's `[]` path. Other types fall through to the
            // existing `to_int` coercion. Mirrors `builtins/
            // array.rs::index`, but `vm_index` / the JIT inline
            // path land here, so both need the dispatch.
            let idx = if index.try_fixnum().is_none() && index.is_range().is_none() {
                if is_arithmetic_sequence(globals, index) {
                    return match vm.invoke_method_inner(
                        globals,
                        IdentId::_INDEX,
                        index,
                        &[base],
                        None,
                        None,
                    ) {
                        Ok(val) => Some(val),
                        Err(err) => {
                            vm.set_error(err);
                            None
                        }
                    };
                }
                match index.coerce_to_int_i64(vm, globals) {
                    Ok(i) => Value::integer(i),
                    Err(err) => {
                        vm.set_error(err);
                        return None;
                    }
                }
            } else {
                index
            };
            return match base.as_array().get_elem1(vm, globals, idx) {
                Ok(val) => Some(val),
                Err(err) => {
                    vm.set_error(err);
                    None
                }
            };
        }
        HASH_CLASS => {
            return match Hashmap::new(base).index(vm, globals, index) {
                Ok(val) => Some(val),
                Err(err) => {
                    vm.set_error(err);
                    None
                }
            };
        }
        /*INTEGER_CLASS => {
            // Try to_int coercion for non-integer index
            let idx = match index.unpack() {
                RV::Fixnum(_) | RV::BigInt(_) => index,
                _ => {
                    if index.is_range().is_some() {
                        index
                    } else {
                        match index.coerce_to_int(vm, globals) {
                            Ok(i) => i,
                            Err(err) => {
                                vm.set_error(err);
                                return None;
                            }
                        }
                    }
                }
            };
            return match op::integer_index1(vm, globals, base, idx) {
                Ok(val) => Some(val),
                Err(err) => {
                    vm.set_error(err);
                    None
                }
            };
        }*/
        METHOD_CLASS => {
            let method = base.as_method();
            let receiver = method.receiver();
            if let Some(target) = method.method_missing_name() {
                vm.reset_method_missing_vcall();
                return vm.invoke_method(
                    globals,
                    IdentId::METHOD_MISSING,
                    true,
                    receiver,
                    &[Value::symbol(target), index],
                    None,
                    None,
                );
            }
            let func_id = method.func_id();
            return vm.invoke_func(globals, func_id, receiver, &[index], None, None);
        }
        _ => {}
    }
    vm.invoke_method_simple(globals, IdentId::_INDEX, base, &[index])
}

pub(super) extern "C" fn set_index(
    vm: &mut Executor,
    globals: &mut Globals,
    base: Value,
    index: Value,
    src: Value,
    class_slot: &mut ClassIdSlot,
) -> Option<Value> {
    let base_classid = base.class();
    class_slot.base = base_classid;
    class_slot.idx = index.class();
    if base_classid == ARRAY_CLASS
        && let Some(idx) = index.try_fixnum()
    {
        class_slot.idx = INTEGER_CLASS;
        if base.is_frozen() {
            vm.set_error(MonorubyErr::cant_modify_frozen(&globals.store, base));
            return None;
        }
        return match base.as_array().set_index(idx, src) {
            Ok(val) => Some(val),
            Err(err) => {
                vm.set_error(err);
                None
            }
        };
    }
    vm.invoke_method_simple(globals, IdentId::_INDEX_ASSIGN, base, &[index, src])
}

/*///
/// Get Constant.
///
/// rax: Option<Value>
///
pub(super) extern "C" fn get_constant(
    vm: &mut Executor,
    globals: &mut Globals,
    site_id: ConstSiteId,
) -> Option<Value> {
    match vm.find_constant(globals, site_id) {
        Ok(val) => Some(val),
        Err(err) => {
            vm.set_error(err);
            None
        }
    }
}*/

///
/// Set Constant.
///
pub(super) extern "C" fn set_constant(
    vm: &mut Executor,
    globals: &mut Globals,
    id: ConstSiteId,
    val: Value,
) -> Option<Value> {
    match vm.set_constant(globals, id, val) {
        Ok(_) => Some(Value::nil()),
        Err(err) => {
            vm.set_error(err);
            None
        }
    }
}

///
/// Get class variable.
///
pub(super) extern "C" fn get_class_var(
    vm: &mut Executor,
    globals: &mut Globals,
    name: IdentId,
) -> Option<Value> {
    match vm.find_class_variable(globals, name) {
        Ok(val) => Some(val),
        Err(err) => {
            vm.set_error(err);
            None
        }
    }
}

///
/// Check class variable.
///
pub(super) extern "C" fn check_class_var(
    vm: &mut Executor,
    globals: &mut Globals,
    name: IdentId,
) -> Value {
    match vm.find_class_variable(globals, name) {
        Ok(val) => val,
        Err(_) => Value::nil(),
    }
}

///
/// Set class variable.
///
pub(super) extern "C" fn set_class_var(
    vm: &mut Executor,
    globals: &mut Globals,
    name: IdentId,
    src: Value,
) -> Option<Value> {
    match vm.set_class_variable(globals, name, src) {
        Ok(_) => Some(Value::nil()),
        Err(err) => {
            vm.set_error(err);
            None
        }
    }
}

///
/// Get Global variable.
///
/// rax: Value
///
pub(super) extern "C" fn get_global_var(
    vm: &mut Executor,
    globals: &mut Globals,
    name: IdentId,
) -> Value {
    GvarTable::get(vm, globals, name)
}

pub(super) extern "C" fn set_global_var(
    vm: &mut Executor,
    globals: &mut Globals,
    name: IdentId,
    val: Value,
) -> Option<Value> {
    match GvarTable::set(vm, globals, name, val) {
        Ok(()) => Some(Value::nil()),
        Err(err) => {
            vm.set_error(err);
            None
        }
    }
}

///
/// Alias global variable `new_name` to `old_name`.
///
pub(super) extern "C" fn alias_global_var(
    globals: &mut Globals,
    new_name: IdentId,
    old_name: IdentId,
) {
    globals.alias_global_variable(new_name, old_name);
}

pub(super) extern "C" fn define_class(
    vm: &mut Executor,
    globals: &mut Globals,
    name: IdentId,
    superclass: Option<Value>,
    is_module: u32,
    base: Option<Value>,
) -> Option<Value> {
    match vm.define_class(globals, base, name, superclass, is_module == 1) {
        Ok(val) => Some(val),
        Err(err) => {
            vm.set_error(err);
            None
        }
    }
}

pub(super) extern "C" fn define_singleton_class(
    vm: &mut Executor,
    globals: &mut Globals,
    base: Value,
) -> Option<Value> {
    let self_val = match base.get_singleton(&mut globals.store) {
        Ok(val) => val,
        Err(err) => {
            vm.set_error(err);
            return None;
        }
    };
    vm.push_class_context(self_val.as_class_id());
    Some(self_val)
}

pub(super) extern "C" fn define_method(
    vm: &mut Executor,
    globals: &mut Globals,
    name: IdentId,
    func: FuncId,
) -> Option<Value> {
    match vm.define_method(globals, name, func) {
        Ok(v) => Some(v),
        Err(err) => {
            vm.set_error(err);
            None
        }
    }
}

pub(super) extern "C" fn singleton_define_method(
    vm: &mut Executor,
    globals: &mut Globals,
    name: IdentId,
    func: FuncId,
    obj: Value,
) -> Option<Value> {
    let class = obj.class();
    if class == INTEGER_CLASS || class == FLOAT_CLASS || class == SYMBOL_CLASS {
        vm.set_error(MonorubyErr::typeerr("can't define singleton"));
        return None;
    }
    let current_func = vm.definition_func_id(globals);
    if let Some(iseq) = globals.store[func].is_iseq() {
        // See `Executor::define_method`: the parent frame may be a
        // builtin (string-form `class_eval` inside an mspec wrapper,
        // for instance). Fall back to an empty lexical context rather
        // than panicking when the parent isn't a Ruby iseq.
        let parent_ctx = match globals.store[current_func].is_iseq() {
            Some(parent) => globals.store[parent].lexical_context.clone(),
            None => Vec::new(),
        };
        globals.store[iseq].lexical_context = parent_ctx;
        // `def expr.m` captures the *surrounding* cref, not the
        // receiver's singleton: a plain nested `def` in its body
        // targets the scope this definition appears in (CRuby:
        // `def T.m; def nested; end; end` in class C defines C#nested).
        if let Ok(cref_class) = vm.plain_def_definee(globals) {
            globals.store[iseq].nested_definee = Some(cref_class);
        }
    }
    // `def obj.foo` on a frozen object raises FrozenError — except for the
    // special singletons nil / true / false, which accept singleton methods
    // (they are defined on NilClass / TrueClass / FalseClass) despite being
    // frozen.
    if !obj.is_nil() && obj != Value::bool(true) && obj != Value::bool(false) {
        if let Err(err) = obj.ensure_not_frozen(&globals.store) {
            vm.set_error(err);
            return None;
        }
    }
    let singleton = match obj.get_singleton(&mut globals.store) {
        Ok(val) => val,
        Err(err) => {
            vm.set_error(err);
            return None;
        }
    };
    // The class actually being modified is the receiver's *singleton
    // class* — it can be frozen independently of the receiver
    // (`c.singleton_class.freeze; def c.foo`). CRuby names the
    // receiver in the message: "can't modify frozen Class: #{obj}".
    if singleton.is_frozen() {
        vm.set_error(MonorubyErr::cant_modify_frozen(&globals.store, obj));
        return None;
    }
    let class_id = singleton.as_class_id();
    match vm.add_public_method(globals, class_id, name, func) {
        Ok(_) => Some(Value::nil()),
        Err(err) => {
            vm.set_error(err);
            None
        }
    }
}

pub(super) extern "C" fn undef_method(
    vm: &mut Executor,
    globals: &mut Globals,
    method: IdentId,
) -> Option<Value> {
    // Prefer the runtime class context (`class` body / `class_eval` /
    // `module_eval` push it) so `klass.class_eval { undef foo }` targets
    // `klass` rather than the block's captured lexical class (Object at
    // top level). When no runtime context is active — e.g. `undef` in a
    // plain method body (`def self.x; undef foo; end`) where the method
    // frame is empty — fall back to the iseq's lexical class, matching
    // CRuby's `cref->klass`.
    let class_id = match vm.definee_class_id_opt(globals) {
        Ok(Some(class_id)) => class_id,
        Ok(None) => vm.cfp().lfp().func_id().lexical_class(globals),
        Err(err) => {
            vm.set_error(err);
            return None;
        }
    };
    match globals.undef_method_for_class(class_id, method) {
        Err(err) => {
            vm.set_error(err);
            None
        }
        Ok(_) => match vm.invoke_method_undefined(globals, class_id, method) {
            Ok(_) => Some(Value::nil()),
            Err(err) => {
                vm.set_error(err);
                None
            }
        },
    }
}

pub(super) extern "C" fn alias_method(
    vm: &mut Executor,
    globals: &mut Globals,
    old: Value,
    new: Value,
) -> Option<Value> {
    let new = match new.expect_symbol_or_string(&globals.store) {
        Ok(id) => id,
        Err(err) => {
            vm.set_error(err);
            return None;
        }
    };
    let old = match old.expect_symbol_or_string(&globals.store) {
        Ok(id) => id,
        Err(err) => {
            vm.set_error(err);
            return None;
        }
    };
    // Target the *runtime* class context (`module_eval` /
    // `class_eval` push it onto a stack) rather than the iseq's
    // *lexical* class — the iseq is captured at compile time, so a
    // block created at top level and run inside
    // `Module.new do … end` still has Object as its lexical class.
    // `alias` should target the module that's currently being
    // defined, matching CRuby's `cref->klass`. An `instance_eval`
    // receiver context resolves to the receiver's singleton class,
    // which raises `TypeError` for an immediate (`1.instance_eval {
    // alias … }`), matching `def`.
    let class_id = match vm.definee_class_id_opt(globals) {
        Ok(Some(class_id)) => class_id,
        Ok(None) => OBJECT_CLASS,
        Err(err) => {
            vm.set_error(err);
            return None;
        }
    };
    // `Executor::alias_method_for_class` already fires the
    // `method_added` hook, so the alias-keyword path matches
    // `Module#alias_method`'s behaviour for it.
    match vm.alias_method_for_class(globals, class_id, new, old) {
        Ok(_) => Some(Value::nil()),
        Err(err) => {
            vm.set_error(err);
            None
        }
    }
}

pub(super) extern "C" fn defined_const(
    vm: &mut Executor,
    globals: &mut Globals,
    reg: *mut Value,
    site_id: ConstSiteId,
) {
    // CRuby's `defined?` probes the constant table without firing
    // autoload for the final segment (`rb_const_defined` calls
    // `rb_const_defined_0` with `autoload_load = FALSE`). Mirror that
    // via `probe_constant`: intermediate qualifiers are resolved
    // normally so we can walk into the right class, but the leaf
    // does not trigger `require`.
    if !vm.probe_constant(globals, site_id) {
        unsafe { *reg = Value::nil() }
    }
}

///
/// Check if global var `name` exists.
///
/// Set `dst`` to `nil` if not exists.
///
/// `defined?` returns a frozen String — build one for the runtime checks.
fn defined_frozen_str(s: &str) -> Value {
    let mut v = Value::string_from_str(s);
    v.set_frozen();
    v
}

pub(super) extern "C" fn defined_gvar(
    vm: &mut Executor,
    globals: &mut Globals,
    name: IdentId,
) -> Value {
    if GvarTable::defined_runtime(vm, globals, name) {
        defined_frozen_str("global-variable")
    } else {
        Value::nil()
    }
}

pub(super) extern "C" fn defined_cvar(
    vm: &mut Executor,
    globals: &mut Globals,
    name: IdentId,
) -> Value {
    if vm.find_class_variable(globals, name).is_ok() {
        defined_frozen_str("class variable")
    } else {
        Value::nil()
    }
}

pub(super) extern "C" fn defined_ivar(
    vm: &mut Executor,
    globals: &mut Globals,
    reg: *mut Value,
    name: IdentId,
) {
    let self_val = vm.cfp().lfp().self_val();
    if globals.store.get_ivar(self_val, name).is_none() {
        unsafe { *reg = Value::nil() }
    }
}

pub(super) extern "C" fn defined_method(
    vm: &mut Executor,
    globals: &mut Globals,
    reg: *mut Value,
    recv: Value,
    name: IdentId,
) {
    use crate::executor::Visibility;
    let is_func_call = vm.cfp().lfp().self_val() == recv;
    if let Some(entry) = globals.store.check_method_for_class(recv.class(), name) {
        if entry.func_id().is_some() {
            let visible = match entry.visibility() {
                Visibility::Public => true,
                // A private method counts as defined only through an
                // implicit (function-call) receiver.
                Visibility::Private => is_func_call,
                // A protected method counts as defined when the caller's
                // `self` is a kind of the class/module that *owns* the
                // method — CRuby checks the defining class, not the
                // receiver's class.
                Visibility::Protected => {
                    let caller_self = vm.cfp().lfp().self_val();
                    caller_self.is_kind_of(&globals.store, entry.owner())
                }
                // An explicitly `undef`ined method is not defined.
                Visibility::Undefined => false,
            };
            if visible {
                return;
            }
        }
    }
    // CRuby's `defined?(recv.meth)` also consults `respond_to_missing?`
    // (with `include_private` = the func-call form). A truthy result
    // reports the call as "method".
    if let Some(fid) = globals.check_method(recv, IdentId::RESPOND_TO_MISSING_) {
        match vm.invoke_func_inner(
            globals,
            fid,
            recv,
            &[Value::symbol(name), Value::bool(is_func_call)],
            None,
            None,
        ) {
            Ok(v) if v.as_bool() => return,
            Ok(_) => {}
            // `defined?` never propagates an exception raised by
            // `respond_to_missing?`; discard it and report nil. The error
            // is carried in the `Err` value (not the executor's slot), so
            // use `discard_error` rather than `take_error` (which would
            // unwrap an empty slot and panic).
            Err(_) => {
                vm.discard_error();
            }
        }
    }
    unsafe { *reg = Value::nil() }
}

///
/// Check if `super` is callable.
///
/// return "super" if callable, `nil` if not.
///
pub(super) extern "C" fn defined_super(vm: &mut Executor, globals: &mut Globals) -> Value {
    let func_id = vm.method_func_id();
    let self_val = vm.cfp().lfp().self_val();
    let name = globals.store[func_id].name().unwrap();
    let self_class = self_val.class();
    if globals.check_super(self_class, func_id, name).is_some() {
        defined_frozen_str("super")
    } else {
        Value::nil()
    }
}

///
/// Check if `super` is callable.
///
/// return "super" if callable, `nil` if not.
///
pub(super) extern "C" fn defined_yield(vm: &mut Executor, _globals: &mut Globals) -> Value {
    if vm.cfp().block_given() {
        defined_frozen_str("yield")
    } else {
        Value::nil()
    }
}

// error handling

pub(super) extern "C" fn panic(_: &mut Executor, _: &mut Globals) {
    panic!("panic in jit code.");
}

pub(super) extern "C" fn err_divide_by_zero(vm: &mut Executor) {
    vm.err_divide_by_zero();
}

pub(super) extern "C" fn err_method_return(vm: &mut Executor, globals: &mut Globals, val: Value) {
    // `return` is only compiled to a method-return inside a block (a
    // brace/`do` block — a lambda literal returns locally). At runtime
    // the same block may have been promoted to a lambda by `Kernel#lambda`
    // (`set_method_style`), in which case `return` exits the lambda
    // itself rather than the creation-site method. So the unwind target
    // is decided here from the frame's *current* style: a real block
    // returns non-locally to its home method (`outermost_lfp`), a lambda
    // returns from its own frame.
    let mut cfp = vm.cfp();
    // A `return` written in a singleton-class body (`class << obj`) —
    // the only class-body position bytecodegen compiles to a
    // method-return — exits the method executing the sclass expression.
    // The body runs as a direct call from that frame, so hop to the
    // caller (skipping nested class bodies and native trampolines) and
    // resolve the target as if the return were written there.
    while globals[cfp.lfp().func_id()].is_classdef() {
        let Some(prev) = cfp.prev() else { break };
        cfp = prev;
        while cfp.lfp().meta().is_native() {
            let Some(prev) = cfp.prev() else { break };
            cfp = prev;
        }
    }
    let cfp = cfp;
    let target_lfp = if globals[cfp.lfp().func_id()].is_block_style() {
        let target = cfp.outermost_lfp();
        // A synchronous thread-body boundary (see
        // `Executor::break_barriers`) also stops non-local returns: a
        // bare `return` in a thread body raises LocalJumpError at the
        // return site, rescuable inside the body (CRuby).
        if !lfp_reachable_within_barrier(vm, target) {
            vm.set_error(MonorubyErr::localjumperr_with_val("unexpected return", val));
            return;
        }
        // A return whose home chain crosses a class/module body is
        // invalid (CRuby: `class A; 1.times { return }; end` raises
        // LocalJumpError at runtime).
        let mut hop = cfp.lfp();
        loop {
            if globals[hop.func_id()].is_classdef() {
                vm.set_error(MonorubyErr::localjumperr_with_val("unexpected return", val));
                return;
            }
            if hop == target {
                break;
            }
            match hop.outer() {
                Some(outer) => hop = outer,
                None => break,
            }
        }
        target
    } else {
        cfp.lfp()
    };
    vm.set_error(MonorubyErr::method_return(val, target_lfp));
}

/// Whether `target` is on the current stack, without crossing the
/// innermost break barrier (a synchronous thread-body boundary).
fn lfp_reachable_within_barrier(vm: &Executor, target: Lfp) -> bool {
    let barrier = vm.break_barrier();
    let mut cfp = Some(vm.cfp());
    while let Some(f) = cfp {
        if Some(f) == barrier {
            return false;
        }
        if f.lfp() == target {
            return true;
        }
        cfp = f.prev();
    }
    false
}

pub(super) extern "C" fn err_block_break(vm: &mut Executor, globals: &mut Globals, val: Value) {
    // In a lambda (including a block promoted by Kernel#lambda) and in
    // a `define_method` body (which behaves exactly like a lambda),
    // break is local: it exits the frame itself, like return.
    let lfp = vm.cfp().lfp();
    if !globals[lfp.func_id()].is_block_style() || lfp.meta().is_proc_method() {
        vm.set_error(MonorubyErr::method_return(val, lfp));
        return;
    }
    // `break` escapes to the invocation of the call that received this
    // block literal (CRuby's BREAK catch-table semantics): the unwinder
    // stops at the block's *defining* frame and resumes it only when
    // its in-progress call site is the one carrying this block; any
    // other route (a materialized Proc invoked later, a re-yield of a
    // captured proc) is a LocalJumpError. Pre-check that the defining
    // frame is on this stack at all, so a proc whose creation scope has
    // returned (or lives on another thread/fiber stack) fails fast.
    let block_fid = lfp.func_id();
    if let Some(outer) = lfp.outer()
        && lfp_reachable_within_barrier(vm, outer)
    {
        vm.set_error(MonorubyErr::block_break(val, block_fid, outer));
    } else {
        vm.set_error(MonorubyErr::new(
            MonorubyErrKind::LocalJump,
            "break from proc-closure".to_string(),
        ));
    }
}

pub(super) extern "C" fn err_retry(vm: &mut Executor) {
    vm.set_error(MonorubyErr::retry());
}

pub(super) extern "C" fn err_redo(vm: &mut Executor) {
    vm.set_error(MonorubyErr::redo());
}

#[allow(dead_code)] // extern "C" helper, currently unreferenced.
pub(super) extern "C" fn check_err(vm: &mut Executor) -> usize {
    vm.exception().is_some().into()
}

///
/// `EnsureEnd` opcode helper: the `ensure` body for the current frame has
/// just finished. Restore a deferred `MethodReturn` / `Throw` (suspended
/// in [`Executor::defer_unwind`] while the body ran) unless the body
/// raised its own error, which takes precedence. Returns non-zero when an
/// error is pending and the caller must re-enter `entry_raise`.
///
pub(super) extern "C" fn ensure_end(vm: &mut Executor) -> usize {
    let lfp = vm.cfp().lfp();
    vm.finish_ensure(lfp).into()
}

pub(super) extern "C" fn raise_err(vm: &mut Executor, err_val: Value) {
    match err_val.is_exception() {
        Some(ex) => vm.set_error(MonorubyErr::new_from_exception(ex).with_original(err_val)),
        // The `Raise` opcode only re-raises an in-flight exception object
        // stashed in `err_reg` by the exception dispatcher, so a
        // non-exception value here means an internal invariant was
        // violated (e.g. `err_reg` was clobbered before the re-raise).
        // Surface it as an uncatchable `FatalError` rather than panicking
        // across the `extern "C"` boundary, which would abort the process.
        None => vm.set_error(MonorubyErr::fatal(
            "raise: re-raised value is not an exception object (internal error)",
        )),
    }
}

pub(super) extern "C" fn to_a(
    vm: &mut Executor,
    globals: &mut Globals,
    src: Value,
) -> Option<Value> {
    // A splat of a value that is already an Array (including an Array
    // subclass) uses it directly — `#to_a` is NOT invoked (`a, b = *ary`
    // and `m(*ary)` must not call a user-defined `Array#to_a`). The
    // downstream array build makes the copy / normalizes to a plain Array.
    if src.is_array_ty() {
        return Some(src);
    }
    // A splat of `nil` yields an empty array without invoking any method
    // (`*nil` ⇒ `[]`; CRuby special-cases nil rather than calling `to_a`).
    if src.is_nil() {
        return Some(Value::array_empty());
    }
    // Like `#to_ary` destructuring above, CRuby gates the `#to_a` call on
    // `respond_to?(:to_a, true)` (a user may override it), not a raw
    // method-table lookup. An object without `#respond_to?` (a bare
    // `BasicObject`) falls back to the raw lookup — CRuby's
    // `rb_check_funcall` still calls a `to_a` defined on it.
    let responds = if globals
        .check_method(src, IdentId::get_id("respond_to?"))
        .is_none()
    {
        globals.check_method(src, IdentId::TO_A).is_some()
    } else {
        match vm.invoke_method_inner(
            globals,
            IdentId::get_id("respond_to?"),
            src,
            &[Value::symbol(IdentId::TO_A), Value::bool(true)],
            None,
            None,
        ) {
            Ok(v) => v.as_bool(),
            Err(err) => {
                vm.set_error(err);
                return None;
            }
        }
    };
    if responds {
        let ary = match vm.invoke_method_inner(globals, IdentId::TO_A, src, &[], None, None) {
            Ok(v) => v,
            Err(err) => {
                vm.set_error(err);
                return None;
            }
        };
        if ary.is_array_ty() {
            Some(ary)
        } else if ary.is_nil() {
            // `#to_a` returning nil is treated like a missing `#to_a`:
            // the splatted object is wrapped in a one-element array,
            // matching CRuby (`m(*o)` with `o.to_a == nil` -> `[o]`).
            Some(Value::array1(src))
        } else {
            let src_class = src.class().get_name(&globals.store);
            vm.set_error(MonorubyErr::typeerr(format!(
                "can't convert {src_class} into Array"
            )));
            None
        }
    } else {
        Some(Value::array1(src))
    }
}

pub extern "C" fn _dump_reg(reg: u64) {
    eprintln!("{:016x}", reg);
}

pub extern "C" fn _dump_stacktrace(vm: &mut Executor, globals: &mut Globals) {
    let mut cfp = vm.cfp();
    eprintln!("-----begin stacktrace");
    unsafe {
        for i in 0..16 {
            eprintln!("  [{}]: {:?} {:?}", i, cfp, cfp.lfp());
            let prev_cfp = cfp.prev();
            globals.dump_frame_info(cfp.lfp());
            if let Some(prev_cfp) = prev_cfp {
                cfp = prev_cfp;
            } else {
                break;
            }
        }
    }
    eprintln!("-----end stacktrace");
}

pub extern "C" fn _check_stack(vm: &mut Executor, globals: &mut Globals) -> bool {
    let mut invalid = false;
    let mut cfp = vm.cfp();
    unsafe {
        for _ in 0..16 {
            let prev_cfp = cfp.prev();
            if globals.check_frame_info(cfp.lfp()) {
                invalid = true;
            };
            if let Some(prev_cfp) = prev_cfp {
                cfp = prev_cfp;
            } else {
                break;
            }
        }
    }
    invalid
}
