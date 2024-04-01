use indexmap::IndexMap;

use super::*;

mod args;
pub(crate) use args::*;

//
// Runtime functions.
//

///
/// Get FuncId of the given method.
///
/// If no method was found or the number of arguments was invalid, return None (==0u64).
///
pub(super) extern "C" fn find_method(
    vm: &mut Executor,
    globals: &mut Globals,
    callid: CallSiteId,
) -> Option<FuncId> {
    if let Some(func_name) = globals.store[callid].name {
        let recv_reg = globals.store[callid].recv;
        let recv = unsafe { vm.get_slot(recv_reg).unwrap() };
        let is_func_call = recv_reg.is_self();
        match globals.find_method(recv, func_name, is_func_call) {
            Ok(id) => Some(id),
            Err(err) => {
                vm.set_error(err);
                None
            }
        }
    } else {
        let self_val = vm.cfp().lfp().self_val();
        let func_id = vm.method_func_id();
        let func_name = globals.store[func_id].name().unwrap();
        match globals.check_super(self_val, func_name) {
            Some(entry) => Some(entry.func_id()),
            None => {
                vm.set_error(MonorubyErr::method_not_found(func_name, self_val));
                None
            }
        }
    }
}

pub(super) extern "C" fn find_method2(
    vm: &mut Executor,
    globals: &mut Globals,
    callid: CallSiteId,
    recv: Value,
) -> Option<FuncId> {
    if let Some(func_name) = globals.store[callid].name {
        let is_func_call = globals.store[callid].recv.is_self();
        match globals.find_method(recv, func_name, is_func_call) {
            Ok(id) => Some(id),
            Err(err) => {
                vm.set_error(err);
                None
            }
        }
    } else {
        let self_val = vm.cfp().lfp().self_val();
        let func_id = vm.method_func_id();
        let func_name = globals.store[func_id].name().unwrap();
        match globals.check_super(self_val, func_name) {
            Some(entry) => Some(entry.func_id()),
            None => {
                vm.set_error(MonorubyErr::method_not_found(func_name, self_val));
                None
            }
        }
    }
}

pub(super) extern "C" fn vm_find_method(
    vm: &mut Executor,
    globals: &mut Globals,
    callid: CallSiteId,
) -> Option<FuncId> {
    let func_id = if let Some(func_name) = globals.store[callid].name {
        let recv_reg = globals.store[callid].recv;
        let recv = unsafe { vm.get_slot(recv_reg).unwrap() };
        let is_func_call = globals.store[callid].recv.is_self();
        match globals.find_method(recv, func_name, is_func_call) {
            Ok(id) => id,
            Err(err) => {
                vm.set_error(err);
                return None;
            }
        }
    } else {
        let self_val = vm.cfp().lfp().self_val();
        let func_id = vm.method_func_id();
        let func_name = globals.store[func_id].name().unwrap();
        match globals.check_super(self_val, func_name) {
            Some(entry) => entry.func_id(),
            None => {
                vm.set_error(MonorubyErr::method_not_found(func_name, self_val));
                return None;
            }
        }
    };
    Some(func_id)
}

pub(super) extern "C" fn enter_classdef<'a>(
    vm: &mut Executor,
    globals: &'a mut Globals,
    func_id: FuncId,
    self_value: Module,
) -> &'a FuncData {
    let current_func = vm.method_func_id();
    let mut lexical_context = globals[current_func].as_ruby_func().lexical_context.clone();
    lexical_context.push(self_value);
    globals[func_id].as_ruby_func_mut().lexical_context = lexical_context;
    globals.get_func_data(func_id)
}

#[derive(Debug, Clone, Default)]
#[repr(C)]
pub(crate) struct ProcData {
    outer: Option<Lfp>,
    func_id: Option<FuncId>,
}

///
/// Get *BlockData* for yield.
///
/// ### in
/// - rdi: &mut Executor
/// - rsi: &mut Globals
///
/// ### out
/// - rax: outer
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
        Ok(data) => ProcData {
            outer: Some(data.outer_lfp()),
            func_id: Some(data.func_id()),
        },
        Err(err) => {
            vm.set_error(err);
            ProcData::default()
        }
    }
}

pub(super) extern "C" fn block_arg(
    vm: &mut Executor,
    globals: &mut Globals,
    block_handler: BlockHandler,
) -> Option<Value> {
    match vm.generate_proc(globals, block_handler) {
        Ok(val) => Some(val.into()),
        Err(err) => {
            vm.set_error(err);
            None
        }
    }
}

pub(super) extern "C" fn gen_array(
    vm: &mut Executor,
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
            let to_a = IdentId::get_id("to_a");
            for (i, v) in iter.enumerate() {
                if globals.store[callid].splat_pos.contains(&i) {
                    if let Some(fid) = globals.check_method(v, to_a) {
                        let a = vm.invoke_func(globals, fid, v, &[], None)?;
                        if let Some(a) = a.try_array_ty() {
                            ary.extend_from_slice(&a);
                        } else {
                            vm.set_error(MonorubyErr::typeerr(
                                "`to_a' method should return Array.",
                                TypeErrKind::Other,
                            ));
                            return None;
                        }
                    } else {
                        ary.push(v);
                    }
                } else {
                    ary.push(v);
                }
            }
            Some(ary.into())
        }
    }
}

pub(super) extern "C" fn gen_hash(src: *const Value, len: usize) -> Value {
    let mut map = IndexMap::default();
    if len > 0 {
        let mut iter = unsafe { std::slice::from_raw_parts(src.sub(len * 2 - 1), len * 2) }
            .iter()
            .copied()
            .rev();
        while let Ok(chunk) = iter.next_chunk::<2>() {
            map.insert(HashKey(chunk[0]), chunk[1]);
        }
    }
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
    globals: &Globals,
    arg: *mut Value,
    len: usize,
) -> Value {
    let mut res = String::new();
    for i in 0..len {
        let v = unsafe { *arg.sub(i) };
        res += &v.to_s(globals);
    }
    Value::string(res)
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
        res += &v.to_s(globals);
    }
    let inner = match RegexpInner::from_string(globals, res) {
        Ok(inner) => inner,
        Err(err) => {
            vm.set_error(err);
            return None;
        }
    };
    Some(Value::regexp(inner))
}

pub(super) extern "C" fn expand_array(src: Value, dst: *mut Value, len: usize) {
    match src.try_array_ty() {
        Some(ary) => {
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
        None => {
            unsafe { *dst = src };
            for i in 1..len {
                unsafe { *dst.sub(i) = Value::nil() }
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
    src: *const Value,
    callee_lfp: Lfp,
    callid: CallSiteId,
) -> Option<Value> {
    let caller_lfp = vm.cfp().lfp();
    match set_frame_arguments(
        globals,
        callee_lfp,
        caller_lfp,
        callid,
        src,
        globals.store[callid].pos_num,
    ) {
        Ok(_) => {
            set_frame_block(&globals.store[callid], callee_lfp, caller_lfp);
            Some(Value::nil())
        }
        Err(err) => {
            vm.set_error(err);
            None
        }
    }
}

pub(super) extern "C" fn jit_handle_arguments_no_block(
    vm: &mut Executor,
    globals: &mut Globals,
    src: *const Value,
    callee_lfp: Lfp,
    callid: CallSiteId,
) -> Option<Value> {
    let caller_lfp = vm.cfp().lfp();
    match set_frame_arguments(
        globals,
        callee_lfp,
        caller_lfp,
        callid,
        src,
        globals.store[callid].pos_num,
    ) {
        Ok(_) => Some(Value::nil()),
        Err(err) => {
            vm.set_error(err);
            None
        }
    }
}

pub(super) extern "C" fn jit_handle_arguments_no_block_for_send(
    vm: &mut Executor,
    globals: &mut Globals,
    src: *const Value,
    callee_lfp: Lfp,
    callid: CallSiteId,
) -> Option<Value> {
    let caller_lfp = vm.cfp().lfp();
    match set_frame_arguments(
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
    src: *const Value,
    callee_lfp: Lfp,
    callid: CallSiteId,
) -> Option<Value> {
    let caller_lfp = vm.cfp().lfp();
    match set_frame_arguments_send_splat(
        globals,
        callee_lfp,
        caller_lfp,
        callid,
        src,
        globals.store[callid].pos_num,
    ) {
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
            return match base.as_array().get_elem1(index) {
                Ok(val) => Some(val),
                Err(err) => {
                    vm.set_error(err);
                    None
                }
            }
        }
        HASH_CLASS => return Some(base.as_hash().get(index).unwrap_or_default()),
        INTEGER_CLASS => {
            return match op::integer_index1(base, index) {
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
            return vm.invoke_func(globals, func_id, receiver, &[index], None);
        }
        _ => {}
    }
    vm.invoke_method(globals, IdentId::_INDEX, base, &[index], None)
}

pub(super) extern "C" fn set_index(
    vm: &mut Executor,
    globals: &mut Globals,
    mut base: Value,
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
        return match base.as_array_mut().set_index(idx, src) {
            Ok(val) => Some(val),
            Err(err) => {
                vm.set_error(err);
                None
            }
        };
    }
    vm.invoke_method(globals, IdentId::_INDEX_ASSIGN, base, &[index, src], None)
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
    match vm.define_class(globals, base, name, superclass, is_module) {
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
    let self_val = globals.get_singleton(base);
    vm.push_class_context(self_val.id());
    Some(self_val.as_val())
}

pub(super) extern "C" fn exit_classdef(vm: &mut Executor, _globals: &mut Globals) {
    vm.pop_class_context();
}

pub(super) extern "C" fn define_method(
    vm: &mut Executor,
    globals: &mut Globals,
    name: IdentId,
    func: FuncId,
) -> Option<Value> {
    vm.define_method(globals, name, func)
}

pub(super) extern "C" fn singleton_define_method(
    vm: &mut Executor,
    globals: &mut Globals,
    name: IdentId,
    func: FuncId,
    obj: Value,
) {
    let current_func = vm.method_func_id();
    globals[func].as_ruby_func_mut().lexical_context =
        globals[current_func].as_ruby_func().lexical_context.clone();
    let class_id = globals.get_singleton(obj).id();
    globals.add_public_method(class_id, name, func);
    globals.class_version_inc();
}

pub(super) extern "C" fn alias_method(
    vm: &mut Executor,
    globals: &mut Globals,
    self_val: Value,
    new: Value,
    old: Value,
    meta: Meta,
) -> Option<Value> {
    let new = new.try_symbol().unwrap();
    let old = old.try_symbol().unwrap();
    match if meta.is_class_def() {
        globals.alias_method_for_class(self_val.as_class().id(), new, old)
    } else {
        globals.alias_method(self_val, new, old)
    } {
        Ok(_) => {}
        Err(err) => {
            vm.set_error(err);
            return None;
        }
    }
    Some(Value::nil())
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

pub(super) extern "C" fn defined_gvar(
    _vm: &mut Executor,
    globals: &mut Globals,
    reg: *mut Value,
    name: IdentId,
) {
    if globals.get_gvar(name).is_none() {
        unsafe { *reg = Value::nil() }
    }
}

pub(super) extern "C" fn defined_ivar(
    vm: &mut Executor,
    globals: &mut Globals,
    reg: *mut Value,
    name: IdentId,
) {
    let self_val = vm.cfp().lfp().self_val();
    if globals.get_ivar(self_val, name).is_none() {
        unsafe { *reg = Value::nil() }
    }
}

pub(super) extern "C" fn defined_method(
    _vm: &mut Executor,
    globals: &mut Globals,
    reg: *mut Value,
    recv: Value,
    name: IdentId,
) {
    let is_func_call = _vm.cfp().lfp().self_val() == recv;
    if globals.find_method(recv, name, is_func_call).is_err() {
        unsafe { *reg = Value::nil() }
    }
}

pub(super) extern "C" fn defined_yield(vm: &mut Executor, _globals: &mut Globals, reg: *mut Value) {
    if !vm.cfp().block_given() {
        unsafe { *reg = Value::nil() }
    }
}

// error handling

pub(super) extern "C" fn unimplemented_inst(
    vm: &mut Executor,
    _: &mut Globals,
    opcode: u16,
) -> Option<Value> {
    vm.set_error(MonorubyErr::runtimeerr(format!(
        "unimplemented instruction. {:04x}",
        opcode
    )));
    None
    //panic!("unimplemented inst. {opcode:016x}");
}

pub(super) extern "C" fn panic(_: &mut Executor, _: &mut Globals) {
    panic!("panic in jit code.");
}

pub(super) extern "C" fn illegal_classid(v: Value) {
    panic!("illegal Value for get_class(): {:016x}", v.id());
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

#[repr(C)]
pub(super) struct ErrorReturn {
    dest: Option<BcPc>,
    value: Option<Value>,
}

impl ErrorReturn {
    fn return_err() -> Self {
        Self {
            dest: None,
            value: None,
        }
    }

    fn return_normal(val: Value) -> Self {
        Self {
            dest: None,
            value: Some(val),
        }
    }

    fn goto(dest: BcPc) -> Self {
        Self {
            dest: Some(dest),
            value: None,
        }
    }
}

pub(super) extern "C" fn handle_error(
    vm: &mut Executor,
    globals: &mut Globals,
    meta: Meta,
    pc: BcPc,
) -> ErrorReturn {
    let func_info = &globals[meta.func_id()];
    match &func_info.kind {
        FuncKind::ISeq(info) => {
            // check exception table.
            let mut lfp = vm.cfp().lfp();
            // First, we check method_return.
            if let MonorubyErrKind::MethodReturn(val, target_lfp) =
                vm.exception().unwrap().kind().clone()
            {
                return if let Some((_, Some(ensure), _)) = info.get_exception_dest(pc) {
                    ErrorReturn::goto(ensure)
                } else if lfp == target_lfp {
                    vm.take_error();
                    ErrorReturn::return_normal(val)
                } else {
                    ErrorReturn::return_err()
                };
            }
            let bc_base = func_info.pc();
            let sourceinfo = info.sourceinfo.clone();
            let loc = info.sourcemap[pc - bc_base];
            vm.push_error_location(loc, sourceinfo);
            if let Some((Some(rescue), _, err_reg)) = info.get_exception_dest(pc) {
                let err_val = vm.take_ex_obj(globals);
                globals.set_gvar(IdentId::get_id("$!"), err_val);
                if let Some(err_reg) = err_reg {
                    unsafe { lfp.set_register(err_reg.0 as _, Some(err_val)) };
                }
                return ErrorReturn::goto(rescue);
            }
        }
        FuncKind::Builtin { .. } => {}
        _ => unreachable!(),
    }
    ErrorReturn::return_err()
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
