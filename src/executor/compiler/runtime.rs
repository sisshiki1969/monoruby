use super::*;

//
// Runtime functions.
//

///
/// Get *const FuncData of the given method.
///
/// If no method was found or the number of arguments was invalid, return None (==0u64).
///
pub(super) extern "C" fn find_method(
    vm: &mut Executor,
    globals: &mut Globals,
    callid: CallSiteId,
    receiver: Value,
    // register id of *self*
    recv_reg: u16,
) -> Option<FuncDataPtr> {
    let func_name = globals.func[callid].name.unwrap();
    let func_id = match globals.find_method(receiver, func_name, recv_reg == 0) {
        Ok(id) => id,
        Err(err) => {
            vm.set_error(err);
            return None;
        }
    };
    let func_data = globals.compile_on_demand(func_id);
    Some(func_data.as_ptr())
}

pub(super) extern "C" fn get_classdef_data<'a>(
    vm: &mut Executor,
    globals: &'a mut Globals,
    func_id: FuncId,
    self_value: Module,
) -> &'a FuncData {
    let current_func = vm.method_func_id();
    let mut lexical_context = globals[current_func].as_ruby_func().lexical_context.clone();
    lexical_context.push(self_value);
    globals[func_id].as_ruby_func_mut().lexical_context = lexical_context;
    globals.compile_on_demand(func_id)
}

pub(super) extern "C" fn get_super_data(
    vm: &Executor,
    globals: &mut Globals,
    self_val: Value,
) -> Option<FuncDataPtr> {
    let func_id = vm.method_func_id();
    let func_name = globals.func[func_id].name().unwrap();
    let super_id = match globals.check_super(self_val, func_name) {
        Some(entry) => Some(entry),
        None => {
            vm.err_method_not_found(globals, func_name, self_val);
            None
        }
    }?
    .func_id();

    let func_data = globals.compile_on_demand(super_id);
    Some(func_data.as_ptr())
}

pub(super) extern "C" fn get_yield_data(vm: &Executor, globals: &mut Globals) -> BlockData {
    vm.cfp()
        .get_block()
        .map(|bh| vm.get_block_data(globals, bh))
        .unwrap_or_default()
}

pub(super) extern "C" fn gen_array(src: *const Value, len: usize) -> Value {
    if len == 0 {
        Value::new_empty_array()
    } else {
        let iter = unsafe {
            std::slice::from_raw_parts(src.sub(len - 1), len)
                .iter()
                .rev()
                .cloned()
        };
        Value::new_array_from_iter(iter)
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
    Value::new_hash(map)
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
    Value::new_string(res)
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

#[repr(C)]
pub(super) struct HandleArguments {
    caller_reg: *const Value,
    callee: *const FuncData,
    callee_reg: *mut Option<Value>,
}

pub(super) extern "C" fn vm_handle_arguments(
    vm: &mut Executor,
    globals: &mut Globals,
    callid: CallSiteId,
    ha: &HandleArguments,
    arg_num: usize,
) -> Option<Value> {
    let callee_func_id = unsafe { (*ha.callee).meta.func_id() };
    match &globals[callee_func_id].kind {
        FuncKind::ISeq(info) => {
            // required + optional + rest
            if let Some((arg_num, range)) = handle_req_opt_rest(&info, arg_num, ha.callee_reg) {
                vm.err_wrong_number_of_arg_range(arg_num, range);
                return None;
            };
            // keyword
            handle_keyword(&info, &globals.func[callid], ha.caller_reg, ha.callee_reg);
        }
        _ => {} // no keyword param and rest param for native func, attr_accessor, etc.
    }
    Some(Value::nil())
}

pub(super) extern "C" fn handle_invoker_arguments(
    globals: &Globals,
    callee_meta: Meta,
    callee_reg: *mut Option<Value>,
    mut arg_num: usize,
) -> usize {
    let callee_func_id = callee_meta.func_id();
    match &globals[callee_func_id].kind {
        FuncKind::ISeq(info) => unsafe {
            // expand array for block
            arg_num = expand_array_for_block(info, arg_num, callee_reg);

            // required + optional + rest
            handle_req_opt_rest(info, arg_num, callee_reg);
            // keyword
            let params = &info.args.keyword_names;
            let callee_kw_pos = info.args.pos_num + 1;
            for (id, _) in params.iter().enumerate() {
                *callee_reg.sub(callee_kw_pos + id) = Some(Value::nil());
            }
        },
        _ => {} // no keyword param and rest param for native func, attr_accessor, etc.
    }
    arg_num
}

/// deconstruct array for block
fn expand_array_for_block(
    info: &ISeqInfo,
    arg_num: usize,
    callee_reg: *mut Option<Value>,
) -> usize {
    let req_num = info.args.required_num;
    let reqopt_num = info.args.reqopt_num;
    if info.is_block_style && arg_num == 1 && reqopt_num > 1 {
        unsafe {
            let v = (*callee_reg.sub(1)).unwrap();
            if v.is_array().is_some() {
                let ptr = callee_reg.sub(1) as _;
                return block_expand_array(v, ptr, req_num);
            }
        }
    }
    arg_num
}

///
/// if argument mismatch occurs, return Some((usize, usize..=usize)).
///
fn handle_req_opt_rest(
    info: &ISeqInfo,
    arg_num: usize,
    callee_reg: *mut Option<Value>,
) -> Option<(usize, std::ops::RangeInclusive<usize>)> {
    let req_num = info.args.required_num;
    let reqopt_num = info.args.reqopt_num;
    let pos_num = info.args.pos_num;
    let is_rest = pos_num != reqopt_num;
    let is_block_style = info.is_block_style;
    unsafe {
        if arg_num > reqopt_num {
            if is_rest {
                let len = arg_num - reqopt_num;
                let ptr = callee_reg.sub(arg_num);
                let iter = std::slice::from_raw_parts(ptr, len)
                    .iter()
                    .rev()
                    .map(|v| v.unwrap());
                *callee_reg.sub(1 + reqopt_num) = Some(Value::new_array_from_iter(iter));
            } else if !is_block_style {
                return Some((arg_num, req_num..=reqopt_num));
            }
        } else if arg_num >= req_num {
            let len = reqopt_num - arg_num;
            let ptr = callee_reg.sub(reqopt_num);
            fill(ptr, len, None);
            if is_rest {
                *callee_reg.sub(1 + reqopt_num) = Some(Value::new_empty_array());
            }
        } else {
            if !is_block_style {
                return Some((arg_num, req_num..=reqopt_num));
            }
            let len = req_num - arg_num;
            let ptr = callee_reg.sub(req_num);
            fill(ptr, len, Some(Value::nil()));
            let len = reqopt_num - req_num;
            let ptr = callee_reg.sub(reqopt_num);
            fill(ptr, len, None);
            if is_rest {
                *callee_reg.sub(1 + reqopt_num) = Some(Value::new_empty_array());
            }
        }
    }
    None
}

fn handle_keyword(
    info: &ISeqInfo,
    callsite: &CallSiteInfo,
    caller_reg: *const Value,
    callee_reg: *mut Option<Value>,
) {
    let CallSiteInfo {
        kw_pos,
        kw_args,
        hash_splat_pos,
        ..
    } = callsite;
    let callee_kw_pos = info.args.pos_num + 1;
    for (id, param_name) in info.args.keyword_names.iter().enumerate() {
        unsafe {
            let ptr = callee_reg.sub(callee_kw_pos + id);
            match kw_args.get(param_name) {
                Some(id) => *ptr = Some(*caller_reg.sub(kw_pos.0 as usize + id)),
                None => *ptr = None,
            }
        }
    }
    for h in hash_splat_pos
        .iter()
        .map(|pos| unsafe { *caller_reg.sub(pos.0 as usize) })
    {
        for (id, param_name) in info.args.keyword_names.iter().enumerate() {
            unsafe {
                let ptr = callee_reg.sub(callee_kw_pos + id);
                let sym = Value::new_symbol(*param_name);
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

pub(super) extern "C" fn jit_handle_hash_splat(
    vm: &mut Executor,
    globals: &mut Globals,
    callid: CallSiteId,
    callee_reg: *mut Option<Value>,
    callee_func_id: FuncId,
) {
    let lfp = vm.cfp().lfp();
    let callsite = &globals.func[callid];
    let CallSiteInfo { hash_splat_pos, .. } = callsite;
    let info = globals.func[callee_func_id].as_ruby_func();
    let callee_kw_pos = info.args.pos_num + 1;
    for (id, param_name) in info.args.keyword_names.iter().enumerate() {
        for hash in hash_splat_pos {
            unsafe {
                let h = lfp.register(hash.0 as usize);
                // We must check whether h is a hash.
                if let Some(v) = h.as_hash().get(Value::new_symbol(*param_name)) {
                    *callee_reg.sub(callee_kw_pos + id) = Some(v);
                }
            }
        }
    }
}

fn fill(ptr: *mut Option<Value>, len: usize, val: Option<Value>) {
    unsafe {
        std::slice::from_raw_parts_mut(ptr, len).fill(val);
    }
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
        _ => {}
    }
    class_slot.idx = index.class();
    vm.invoke_method(globals, IdentId::_INDEX, base, &[index])
}

pub(super) extern "C" fn get_array_integer_index(base: Value, index: i64) -> Option<Value> {
    base.as_array().get_index(index)
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
    class_slot.idx = index.class();
    vm.invoke_method(globals, IdentId::_INDEX_ASSIGN, base, &[index, src])
}

pub(super) extern "C" fn set_array_integer_index(
    mut base: Value,
    index: i64,
    vm: &mut Executor,
    globals: &mut Globals,
    src: Value,
) -> Option<Value> {
    match base.as_array_mut().set_index(index, src) {
        Ok(val) => Some(val),
        Err(err) => {
            vm.set_error(err);
            None
        }
    }
}

///
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
}

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
/// Get instance variable.
///
/// rax <= the value of instance variable. <Value>
///
pub(super) extern "C" fn get_instance_var(
    base: Value,
    name: IdentId,
    globals: &mut Globals,
) -> Value {
    globals.get_ivar(base, name).unwrap_or_default()
}

///
/// Set instance variable.
///
/// rax <= Some(*val*). If error("can't modify frozen object") occured, returns None.
///
pub(super) extern "C" fn set_instance_var(
    vm: &mut Executor,
    globals: &mut Globals,
    base: Value,
    name: IdentId,
    val: Value,
) -> Option<Value> {
    if let Err(err) = globals.set_ivar(base, name, val) {
        vm.set_error(err);
        return None;
    };
    Some(val)
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
        ruruby_parse::SPECIAL_LASTMATCH => {
            // $&
            vm.sp_last_match.unwrap_or_default()
        }
        ruruby_parse::SPECIAL_POSTMATCH => {
            // $'
            vm.sp_post_match.unwrap_or_default()
        }
        ruruby_parse::SPECIAL_LOADPATH => {
            // $LOAD_PATH
            globals.get_load_path()
        }
        ruruby_parse::SPECIAL_LOADEDFEATURES => {
            // $LOADED_FEATURES
            globals.get_loaded_features()
        }
        id if id >= 100 => {
            // $1, $2, ..
            vm.get_special_matches(id as i64 - 100)
        }
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

pub(super) extern "C" fn pop_class_context(vm: &mut Executor, _globals: &mut Globals) {
    vm.pop_class_context();
}

pub(super) extern "C" fn define_method(
    vm: &mut Executor,
    globals: &mut Globals,
    name: IdentId,
    func: FuncId,
) {
    let Cref {
        class_id,
        module_function,
        visibility,
    } = vm.get_class_context();
    let current_func = vm.method_func_id();
    globals[func].as_ruby_func_mut().lexical_context =
        globals[current_func].as_ruby_func().lexical_context.clone();
    globals.add_method(class_id, name, func, visibility);
    if module_function {
        globals.add_singleton_method(class_id, name, func, visibility);
    }
    globals.class_version_inc();
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
    let new = new.as_symbol();
    let old = old.as_symbol();
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
    if globals.find_method(recv, name, false).is_err() {
        unsafe { *reg = Value::nil() }
    }
}

pub(super) extern "C" fn defined_yield(vm: &mut Executor, _globals: &mut Globals, reg: *mut Value) {
    if !vm.cfp().block_given() {
        unsafe { *reg = Value::nil() }
    }
}

// error handling

pub(super) extern "C" fn unimplemented_inst(_: &mut Executor, _: &mut Globals, opcode: u64) {
    panic!("unimplemented inst. {opcode:016x}");
}

pub(super) extern "C" fn panic(_: &mut Executor, _: &mut Globals) {
    panic!("panic in jit code.");
}

pub(super) extern "C" fn err_divide_by_zero(vm: &mut Executor) {
    vm.err_divide_by_zero();
}

pub(super) extern "C" fn err_no_block_given(vm: &mut Executor) {
    vm.err_no_block_given();
}

pub(super) extern "C" fn err_wrong_number_of_arguments_range(
    vm: &mut Executor,
    given: usize,
    min: usize,
    max: usize,
) -> Option<Value> {
    if let Err(err) = Executor::check_number_of_arguments(given, min..=max) {
        vm.set_error(err);
        return None;
    };
    Some(Value::nil())
}

pub(super) extern "C" fn err_method_return(vm: &Executor, globals: &mut Globals, val: Value) {
    let target_lfp = vm.cfp().outermost_lfp();
    vm.set_error(MonorubyErr {
        kind: MonorubyErrKind::MethodReturn(val, target_lfp),
        msg: String::new(),
        loc: vec![],
    });
}

pub(super) extern "C" fn check_err(vm: &mut Executor) -> usize {
    vm.error().is_some().into()
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
            if let MonorubyErrKind::MethodReturn(val, target_lfp) = vm.error().unwrap().kind {
                if let Some((_, Some(ensure), _)) = info.get_exception_dest(pc) {
                    return ErrorReturn::goto(ensure);
                } else {
                    if lfp == target_lfp {
                        vm.take_error().unwrap();
                        return ErrorReturn::return_normal(val);
                    } else {
                        return ErrorReturn::return_err();
                    }
                }
            }
            if let Some((Some(rescue), _, err_reg)) = info.get_exception_dest(pc) {
                let err_val = vm.take_error_obj(globals);
                globals.set_gvar(IdentId::get_id("$!"), err_val);
                if let Some(err_reg) = err_reg {
                    unsafe { lfp.set_register(err_reg.0 as usize, err_val) };
                }
                return ErrorReturn::goto(rescue);
            } else {
                let bc_base = func_info.data.pc();
                let sourceinfo = info.sourceinfo.clone();
                let loc = info.sourcemap[pc - bc_base];
                vm.push_error_location(loc, sourceinfo);
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
            eprint!("  [{}]: {:?} {:?}", i, cfp, cfp.lfp());
            let ret_addr = cfp.return_addr();
            eprintln!(" ret adr: {ret_addr:?} ");
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
