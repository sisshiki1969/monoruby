use super::*;

mod args;
pub(crate) use args::*;

pub const PROCDATA_OUTER: i64 = std::mem::offset_of!(ProcData, outer) as _;
pub const PROCDATA_FUNCID: i64 = std::mem::offset_of!(ProcData, func_id) as _;

//
// Runtime functions.
//

pub(super) extern "C" fn find_method(
    vm: &mut Executor,
    globals: &mut Globals,
    callid: CallSiteId,
    recv: Value,
) -> Option<FuncId> {
    if let Some(func_name) = globals[callid].name {
        let is_func_call = globals[callid].is_func_call();
        vm.find_method(globals, recv, func_name, is_func_call)
    } else {
        find_super(vm, globals)
    }
    .map_err(|err| vm.set_error(err))
    .ok()
}

fn find_super(vm: &mut Executor, globals: &mut Globals) -> Result<FuncId> {
    let func_id = vm.method_func_id();
    let self_val = vm.cfp().lfp().self_val();
    let owner = globals.store[func_id].owner_class().unwrap();
    let func_name = globals.store[func_id].name().unwrap();
    let self_class = self_val.class();
    match globals.store.check_super(self_class, owner, func_name) {
        Some(func_id) => Ok(func_id),
        None => Err(MonorubyErr::method_not_found(globals, func_name, self_val)),
    }
}

pub(super) extern "C" fn enter_classdef<'a>(
    vm: &mut Executor,
    globals: &'a mut Globals,
    func_id: FuncId,
    self_value: Module,
) -> &'a FuncData {
    let current_func = vm.method_func_id();
    let mut lexical_context = globals.store.iseq(current_func).lexical_context.clone();
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

    pub(crate) fn from_proc(proc: &ProcInner) -> Self {
        Self {
            outer: Some(proc.outer_lfp()),
            func_id: Some(proc.func_id()),
        }
    }

    pub(crate) fn from_proxy(executor: &Executor, proxy: (FuncId, u16)) -> Self {
        let mut cfp = executor.cfp();
        for _ in 0..proxy.1 {
            cfp = cfp.prev().unwrap();
        }
        ProcData {
            outer: Some(cfp.lfp()),
            func_id: Some(proxy.0),
        }
    }

    pub(crate) fn to_proc(self) -> Option<ProcInner> {
        if let Some(func_id) = self.func_id {
            Some(ProcInner::new(self.outer.unwrap(), func_id))
        } else {
            None
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
    let bh = match vm.cfp().get_block() {
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
    _: &mut Globals,
    block_handler: Option<BlockHandler>,
) -> Option<Value> {
    let bh = match block_handler {
        Some(bh) => bh,
        None => {
            return Some(Value::nil());
        }
    };
    match vm.generate_proc(bh) {
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
    if let Some(lhs_ary) = lhs.try_array_ty() {
        for lhs in lhs_ary.iter().cloned() {
            if op::cmp_teq_values(vm, globals, lhs, rhs)?.as_bool() {
                return Some(Value::bool(true));
            }
        }
        Some(Value::bool(false))
    } else {
        op::cmp_teq_values(vm, globals, lhs, rhs)
    }
}

pub(super) extern "C" fn gen_lambda(vm: &mut Executor, _: &mut Globals, func_id: FuncId) -> Value {
    vm.generate_lambda(func_id).into()
}

pub(super) extern "C" fn gen_hash(
    vm: &mut Executor,
    globals: &mut Globals,
    src: *const Value,
    len: usize,
) -> Value {
    let mut map = RubyMap::default();
    if len > 0 {
        let mut iter = unsafe { std::slice::from_raw_parts(src.sub(len * 2 - 1), len * 2) }
            .iter()
            .copied()
            .rev();
        while let Ok(chunk) = iter.next_chunk::<2>() {
            map.insert(chunk[0], chunk[1], vm, globals).unwrap();
        }
    }
    Value::hash(map)
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
    match globals.generate_range(start, end, exclude_end) {
        Ok(val) => Some(val),
        Err(err) => {
            vm.set_error(err);
            None
        }
    }
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
    let mut res = String::new();
    for i in 0..len {
        let v = unsafe { *arg.sub(i) };
        res += vm.invoke_tos(globals, v)?.expect_str(globals)?;
    }
    Ok(Value::string(res))
}

pub(super) extern "C" fn concatenate_regexp(
    vm: &mut Executor,
    globals: &mut Globals,
    arg: *mut Value,
    len: usize,
) -> Option<Value> {
    let mut res = String::new();
    for i in 0..len {
        let v = unsafe { *arg.sub(i) };
        res += &v.to_s(&globals.store);
    }
    let inner = match RegexpInner::with_option(res, 0) {
        Ok(inner) => inner,
        Err(err) => {
            vm.set_error(err);
            return None;
        }
    };
    Some(Value::regexp(inner))
}

pub(super) extern "C" fn expand_array(src: Value, dst: *mut Value, len: usize, rest: usize) {
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
}

pub(crate) extern "C" fn create_array(src: *mut Value, len: usize) -> Value {
    if len == 0 {
        return Value::array_empty();
    }
    let slice = unsafe { std::slice::from_raw_parts(src.sub(len - 1), len) };
    Value::array_from_iter(slice.iter().rev().copied())
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
            return match base.as_array().get_elem1(globals, index) {
                Ok(val) => Some(val),
                Err(err) => {
                    vm.set_error(err);
                    None
                }
            }
        }
        HASH_CLASS => {
            return match Hashmap::new(base).index(vm, globals, index) {
                Ok(val) => Some(val),
                Err(err) => {
                    vm.set_error(err);
                    None
                }
            }
        }
        INTEGER_CLASS => {
            return match op::integer_index1(globals, base, index) {
                Ok(val) => Some(val),
                Err(err) => {
                    vm.set_error(err);
                    None
                }
            }
        }
        METHOD_CLASS => {
            let method = base.as_method();
            let func_id = method.func_id();
            let receiver = method.receiver();
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
) {
    vm.set_constant(globals, id, val).unwrap();
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
pub(super) extern "C" fn get_global_var(globals: &mut Globals, name: IdentId) -> Value {
    globals.get_gvar(name).unwrap_or_default()
}

pub(super) extern "C" fn set_global_var(globals: &mut Globals, name: IdentId, val: Value) {
    globals.set_gvar(name, val);
}

///
/// Get special variable.
///
/// id: 0 -> $&
/// id: 1 -> $'
/// id: 100 + n -> $<n> (n >= 1)
///
pub(super) extern "C" fn get_special_var(vm: &Executor, globals: &Globals, id: u32) -> Value {
    match id {
        // $&
        ruruby_parse::SPECIAL_LASTMATCH => vm.sp_last_match(),
        // $'
        ruruby_parse::SPECIAL_POSTMATCH => vm.sp_post_match(),
        // $LOAD_PATH
        ruruby_parse::SPECIAL_LOADPATH => globals.get_load_path(),
        // $LOADED_FEATURES
        ruruby_parse::SPECIAL_LOADEDFEATURES => globals.get_loaded_features(),
        // $1, $2, ..
        id if id >= 100 => vm.get_special_matches(id as i64 - 100),
        _ => unreachable!(),
    }
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
    let self_val = globals.store.get_singleton(base);
    vm.push_class_context(self_val.id());
    Some(self_val.as_val())
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
) {
    let current_func = vm.method_func_id();
    if let Some(iseq) = globals.store[func].is_iseq() {
        globals.store[iseq].lexical_context =
            globals.store.iseq(current_func).lexical_context.clone();
    }
    let class_id = globals.store.get_singleton(obj).id();
    globals.add_public_method(class_id, name, func);
}

pub(super) extern "C" fn undef_method(
    vm: &mut Executor,
    globals: &mut Globals,
    method: IdentId,
) -> Option<Value> {
    let func_id = vm.cfp().lfp().func_id();
    let class_id = func_id.lexical_class(globals);
    globals
        .undef_method_for_class(class_id, method)
        .map_or_else(
            |err| {
                vm.set_error(err);
                None
            },
            |_| Some(Value::nil()),
        )
}

pub(super) extern "C" fn alias_method(
    vm: &mut Executor,
    globals: &mut Globals,
    old: IdentId,
    new: IdentId,
) -> Option<Value> {
    let func_id = vm.cfp().lfp().func_id();
    let class_id = func_id.lexical_class(globals);
    globals
        .alias_method_for_class(class_id, new, old)
        .map_or_else(
            |err| {
                vm.set_error(err);
                None
            },
            |_| Some(Value::nil()),
        )
}

pub(super) extern "C" fn defined_const(
    vm: &mut Executor,
    globals: &mut Globals,
    reg: *mut Value,
    site_id: ConstSiteId,
) {
    if vm.find_constant(globals, site_id).is_err() {
        unsafe { *reg = Value::nil() }
    }
}

///
/// Check if global var `name` exists.
///
/// Set `dst`` to `nil` if not exists.
///
pub(super) extern "C" fn defined_gvar(
    _vm: &mut Executor,
    globals: &mut Globals,
    name: IdentId,
) -> Value {
    if globals.get_gvar(name).is_some() {
        Value::string_from_str("global-variable")
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
    let is_func_call = vm.cfp().lfp().self_val() == recv;
    if vm.find_method(globals, recv, name, is_func_call).is_err() {
        unsafe { *reg = Value::nil() }
    }
}

///
/// Check if `super` is callable.
///
/// return "super" if callable, `nil` if not.
///
pub(super) extern "C" fn defined_super(vm: &mut Executor, globals: &mut Globals) -> Value {
    let func_id = vm.method_func_id();
    let self_val = vm.cfp().lfp().self_val();
    let owner = globals.store[func_id].owner_class().unwrap();
    let name = globals.store[func_id].name().unwrap();
    let self_class = self_val.class();
    if globals.check_super(self_class, owner, name).is_some() {
        Value::string_from_str("super")
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
        Value::string_from_str("yield")
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

pub(super) extern "C" fn err_method_return(vm: &mut Executor, _globals: &mut Globals, val: Value) {
    let target_lfp = vm.cfp().outermost_lfp();
    vm.set_error(MonorubyErr::method_return(val, target_lfp));
}

pub(super) extern "C" fn check_err(vm: &mut Executor) -> usize {
    vm.exception().is_some().into()
}

pub(super) extern "C" fn raise_err(vm: &mut Executor, err_val: Value) {
    match err_val.is_exception() {
        Some(ex) => vm.set_error(MonorubyErr::new_from_exception(ex)),
        None => unimplemented!(),
    }
}

pub(super) extern "C" fn to_a(
    vm: &mut Executor,
    globals: &mut Globals,
    src: Value,
) -> Option<Value> {
    if let Some(func_id) = globals.check_method(src, IdentId::TO_A) {
        let ary = vm.invoke_func(globals, func_id, src, &[], None, None)?;
        if ary.is_array_ty() {
            Some(ary)
        } else {
            let src_class = src.class().get_name(&globals.store);
            let res_class = ary.class().get_name(&globals.store);
            vm.set_error(MonorubyErr::typeerr(format!(
                "can't convert {src_class} to Array ({src_class}#to_a gives {res_class})"
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
