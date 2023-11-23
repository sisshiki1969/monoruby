use super::*;

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
        let receiver = unsafe { vm.register(recv_reg.0 as usize).unwrap() };
        match globals.find_method(receiver, func_name, globals.store[callid].recv.is_zero()) {
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
                vm.set_error(MonorubyErr::method_not_found(globals, func_name, self_val));
                None
            }
        }
    }
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

///
/// Get *BlockData* for yield.
///
/// - rdi: CFP
/// - rsi: &mut Globals
///
pub(super) extern "C" fn get_yield_data(vm: &mut Executor, globals: &mut Globals) -> Option<Proc> {
    let res = globals.get_yield_data(vm.cfp());
    if res.is_none() {
        vm.set_error(MonorubyErr::no_block_given());
    }
    res
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
            let mut ary = Array::new();
            let to_a = IdentId::get_id("to_a");
            for (i, v) in iter.enumerate() {
                if globals.store[callid].splat_pos.contains(&i) {
                    if let Some(fid) = globals.check_method(v, to_a) {
                        let a = vm.invoke_func(globals, fid, v, &[], None)?;
                        if let Some(a) = a.is_array() {
                            ary.extend_from_slice(&a);
                        } else {
                            vm.set_error(MonorubyErr::typeerr(
                                "`to_a' method should return Array.",
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
        res += &globals.to_s(v);
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
        res += &globals.to_s(v);
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
    match src.is_array() {
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

pub(super) extern "C" fn vm_handle_arguments(
    vm: &mut Executor,
    globals: &mut Globals,
    callid: CallSiteId,
    arg_num: usize,
    callee_lfp: LFP,
) -> Option<Value> {
    let callee_func_id = callee_lfp.meta().func_id();
    match &globals[callee_func_id].kind {
        FuncKind::ISeq(info) => {
            let caller = &globals.store[callid];
            if info.key_num() == 0 && caller.kw_len() != 0 {
                // positional
                let mut h = IndexMap::default();
                for (k, id) in caller.kw_args.iter() {
                    let v = unsafe { vm.register(caller.kw_pos.0 as usize + *id).unwrap() };
                    h.insert(HashKey(Value::symbol(*k)), v);
                }
                let ex: Value = Value::hash(h);
                if let Some((arg_num, range)) =
                    handle_positional(&info, arg_num, callee_lfp, Some(ex))
                {
                    vm.err_wrong_number_of_arg_range(arg_num, range);
                    return None;
                };
            } else {
                // positional
                if let Some((arg_num, range)) = handle_positional(&info, arg_num, callee_lfp, None)
                {
                    vm.err_wrong_number_of_arg_range(arg_num, range);
                    return None;
                };
                // keyword
                handle_keyword(&info, &globals.store[callid], vm.cfp().lfp(), callee_lfp);
            }
        }
        _ => {} // no keyword param and rest param for native func, attr_accessor, etc.
    }

    let CallSiteInfo {
        block_fid,
        block_arg,
        ..
    } = &globals.store[callid];

    let bh = if let Some(block_fid) = block_fid {
        let bh = BlockHandler::from(*block_fid);
        Some(bh)
    } else if let Some(block_arg) = block_arg {
        unsafe { Some(BlockHandler(vm.register(block_arg.0 as usize).unwrap())) }
    } else {
        None
    };
    callee_lfp.set_block(bh);
    Some(Value::nil())
}

///
/// if argument mismatch occurs, return Some((usize, usize..=usize)).
///
pub(super) fn handle_positional(
    info: &ISeqInfo,
    arg_num: usize,
    mut callee_lfp: LFP,
    ex: Option<Value>,
) -> Option<(usize, std::ops::RangeInclusive<usize>)> {
    let req_num = info.required_num();
    let reqopt_num = info.reqopt_num();
    let pos_num = info.pos_num();
    let is_rest = pos_num != reqopt_num;
    let is_block_style = info.is_block_style;
    let ex_num = ex.is_some() as usize;
    unsafe {
        if (arg_num + ex_num) > reqopt_num {
            if is_rest {
                let len = arg_num - reqopt_num;
                let mut ary: Vec<_> = callee_lfp.slice(reqopt_num, len).collect();
                if let Some(ex) = ex {
                    ary.push(ex);
                }
                callee_lfp.set_register(1 + reqopt_num, Some(Value::array_from_vec(ary)));
            } else if !is_block_style {
                return Some(((arg_num + ex_num), req_num..=reqopt_num));
            }
            return None;
        }

        if (arg_num + ex_num) >= req_num {
            let len = reqopt_num - arg_num;
            fill(callee_lfp, reqopt_num, len, None);
        } else {
            if !is_block_style {
                return Some(((arg_num + ex_num), req_num..=reqopt_num));
            }
            let len = req_num - (arg_num + ex_num);
            fill(callee_lfp, req_num, len, Some(Value::nil()));
            let len = reqopt_num - req_num;
            fill(callee_lfp, reqopt_num, len, None);
        }
        if let Some(ex) = ex {
            callee_lfp.set_register(1 + arg_num, Some(ex));
        }

        if is_rest {
            callee_lfp.set_register(1 + reqopt_num, Some(Value::array_empty()));
        }
    }
    None
}

///
/// Handle keyword arguments.
///
fn handle_keyword(info: &ISeqInfo, callsite: &CallSiteInfo, caller_lfp: LFP, mut callee_lfp: LFP) {
    let CallSiteInfo {
        kw_pos,
        kw_args,
        hash_splat_pos,
        ..
    } = callsite;

    let callee_kw_pos = info.pos_num() + 1;

    for (id, param_name) in info.args.keyword_names.iter().enumerate() {
        unsafe {
            let v = match kw_args.get(param_name) {
                Some(id) => Some(caller_lfp.register(kw_pos.0 as usize + id).unwrap()),
                None => None,
            };
            callee_lfp.set_register(callee_kw_pos + id, v);
        }
    }

    for h in hash_splat_pos
        .iter()
        .map(|pos| unsafe { caller_lfp.register(pos.0 as usize).unwrap() })
    {
        for (id, param_name) in info.args.keyword_names.iter().enumerate() {
            unsafe {
                let ptr = callee_lfp.register_ptr(callee_kw_pos + id);
                let sym = Value::symbol(*param_name);
                if let Some(v) = h.as_hash().get(sym) {
                    if (*ptr).is_some() {
                        eprintln!(
                            " warning: key :{} is duplicated and overwritten",
                            param_name
                        );
                    }
                    *ptr = Some(v);
                }
            }
        }
    }
}

unsafe fn fill(lfp: LFP, start_pos: usize, len: usize, val: Option<Value>) {
    let ptr = lfp.register_ptr(start_pos);
    std::slice::from_raw_parts_mut(ptr, len).fill(val);
}

#[repr(C)]
pub(super) struct ClassIdSlot {
    base: ClassId,
    idx: ClassId,
}

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
        HASH_CLASS => return Some(base.as_hash().get(index).unwrap_or_default()),
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
    match base_classid {
        ARRAY_CLASS => {
            if let Some(idx) = index.try_fixnum() {
                class_slot.idx = INTEGER_CLASS;
                return match base.as_array_mut().set_index(idx, src) {
                    Ok(val) => Some(val),
                    Err(err) => {
                        vm.set_error(err);
                        None
                    }
                };
            }
        }
        _ => {}
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
    name: IdentId,
    val: Value,
) {
    vm.set_constant(globals, name, val)
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
) -> Option<Value> {
    match vm.define_class(globals, name, superclass, is_module) {
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
    globals.add_method(class_id, name, func, Visibility::Public);
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

pub(super) extern "C" fn err_wrong_number_of_arguments_range(
    vm: &mut Executor,
    given: usize,
    min: usize,
    max: usize,
) -> Option<Value> {
    if let Err(err) = MonorubyErr::check_number_of_arguments_range(given, min..=max) {
        vm.set_error(err);
        return None;
    };
    Some(Value::nil())
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
                } else {
                    if lfp == target_lfp {
                        vm.take_error();
                        ErrorReturn::return_normal(val)
                    } else {
                        ErrorReturn::return_err()
                    }
                };
            }
            let bc_base = func_info.data.pc();
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
