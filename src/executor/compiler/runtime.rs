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
    globals: &mut Globals,
    callid: CallSiteId,
    receiver: Value,
    // register id of *self*
    recv_reg: u16,
) -> Option<FuncDataPtr> {
    let func_name = globals.func[callid].name;
    let func_id = globals.find_method(receiver, func_name, recv_reg == 0)?;
    let func_data = globals.compile_on_demand(func_id);
    Some(func_data.as_ptr())
}

pub(super) extern "C" fn get_classdef_data<'a>(
    executor: &mut Executor,
    globals: &'a mut Globals,
    func_id: FuncId,
    self_value: Module,
) -> &'a FuncData {
    let current_func = executor.cfp.method_func_id();
    let mut lexical_context = globals[current_func].as_ruby_func().lexical_context.clone();
    lexical_context.push(self_value);
    globals[func_id].as_ruby_func_mut().lexical_context = lexical_context;
    globals.compile_on_demand(func_id)
}

pub(super) extern "C" fn get_yield_data(executor: &Executor, globals: &mut Globals) -> BlockData {
    executor
        .cfp
        .get_block()
        .map(|bh| executor.get_block_data(globals, bh))
        .unwrap_or_default()
}

pub(super) extern "C" fn gen_array(src: *const Value, len: usize) -> Value {
    let mut v = if len == 0 {
        vec![]
    } else {
        unsafe { std::slice::from_raw_parts(src.sub(len - 1), len).to_vec() }
    };
    v.reverse();
    Value::new_array_from_vec(v)
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
    globals: &mut Globals,
    exclude_end: bool,
) -> Option<Value> {
    globals.generate_range(start, end, exclude_end)
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

pub(super) extern "C" fn expand_array(src: Value, dst: *mut Value, len: usize) -> usize {
    match src.is_array() {
        Some(ary) => {
            if len <= ary.len() {
                for i in 0..len {
                    unsafe { *dst.sub(i) = ary[i] }
                }
                len
            } else {
                for i in 0..ary.len() {
                    unsafe { *dst.sub(i) = ary[i] }
                }
                for i in ary.len()..len {
                    unsafe { *dst.sub(i) = Value::nil() }
                }
                ary.len()
            }
        }
        None => {
            unsafe { *dst = src };
            1
        }
    }
}

pub(super) extern "C" fn make_splat(src: *mut Value) {
    unsafe { *src = Value::new_splat(*src) };
}

pub(super) extern "C" fn vm_handle_arguments(
    globals: &mut Globals,
    callid: CallSiteId,
    caller_reg: *const Value,
    callee: &FuncData,
    callee_reg: *mut Option<Value>,
    arg_num: usize,
) -> Option<Value> {
    let callee_func_id = callee.meta.func_id();
    match &globals[callee_func_id].kind {
        FuncKind::ISeq(info) => {
            // required + optional + rest
            if let Some((arg_num, range)) = handle_req_opt_rest(info, arg_num, callee_reg) {
                globals.err_wrong_number_of_arg_range(arg_num, range);
                return None;
            };
            // keyword
            handle_keyword(info, &globals.func[callid], caller_reg, callee_reg);
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
            /*let CallSiteInfo {
                name: _,
                kw_pos,
                kw_args,
            } = &globals.func[callid];*/
            let params = &info.args.keyword_args;
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
                let v = std::slice::from_raw_parts(ptr, len)
                    .iter()
                    .rev()
                    .map(|v| v.unwrap())
                    .collect();
                *callee_reg.sub(1 + reqopt_num) = Some(Value::new_array_from_vec(v));
            } else if !is_block_style {
                return Some((arg_num, req_num..=reqopt_num));
            }
        } else if arg_num >= req_num {
            let len = reqopt_num - arg_num;
            let ptr = callee_reg.sub(reqopt_num);
            fill(ptr, len, None);
            if is_rest {
                *callee_reg.sub(1 + reqopt_num) = Some(Value::new_array_from_vec(vec![]));
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
                *callee_reg.sub(1 + reqopt_num) = Some(Value::new_array_from_vec(vec![]));
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
        kw_pos, kw_args, ..
    } = callsite;
    let callee_kw_pos = info.args.pos_num + 1;
    for (id, (param_name, _)) in info.args.keyword_args.iter().enumerate() {
        unsafe {
            *callee_reg.sub(callee_kw_pos + id) = kw_args
                .get(param_name)
                .map(|id| *caller_reg.sub(*kw_pos as usize + id));
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
    interp: &mut Executor,
    globals: &mut Globals,
    base: Value,
    index: Value,
    class_slot: &mut ClassIdSlot,
) -> Option<Value> {
    let base_classid = base.class();
    class_slot.base = base_classid;
    match base_classid {
        ARRAY_CLASS => {
            if let Some(idx) = index.try_fixnum() {
                class_slot.idx = INTEGER_CLASS;
                return base.as_array().get_index(idx);
            } else if let Some(range) = index.is_range() {
                class_slot.idx = RANGE_CLASS;
                return base.as_array().get_index_range(globals, range);
            }
        }
        _ => {}
    }
    class_slot.idx = index.class();
    interp.invoke_method(globals, IdentId::_INDEX, base, &[index])
}

pub(super) extern "C" fn get_array_integer_index(base: Value, index: i64) -> Option<Value> {
    base.as_array().get_index(index)
}

pub(super) extern "C" fn set_index(
    interp: &mut Executor,
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
                return base.as_array_mut().set_index(globals, idx, src);
            }
        }
        _ => {}
    }
    class_slot.idx = index.class();
    interp.invoke_method(globals, IdentId::_INDEX_ASSIGN, base, &[index, src])
}

pub(super) extern "C" fn set_array_integer_index(
    mut base: Value,
    index: i64,
    globals: &mut Globals,
    src: Value,
) -> Option<Value> {
    base.as_array_mut().set_index(globals, index, src)
}

///
/// Get Constant.
///
/// rax: Option<Value>
///
pub(super) extern "C" fn get_constant(
    executor: &mut Executor,
    globals: &mut Globals,
    site_id: ConstSiteId,
) -> Option<Value> {
    executor.find_constant(globals, site_id)
}

///
/// Set Constant.
///
pub(super) extern "C" fn set_constant(
    executor: &mut Executor,
    globals: &mut Globals,
    name: IdentId,
    val: Value,
) {
    executor.set_constant(globals, name, val)
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
    globals: &mut Globals,
    base: Value,
    name: IdentId,
    val: Value,
) -> Option<Value> {
    globals.set_ivar(base, name, val)?;
    Some(val)
}

///
/// Get Global variable.
///
/// rax: Value
///
pub(super) extern "C" fn get_global_var(globals: &mut Globals, name: IdentId) -> Value {
    globals.get_gvar(name)
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
pub(super) extern "C" fn get_special_var(executor: &Executor, id: u32) -> Value {
    if id == 0 {
        // $&
        executor.sp_last_match.unwrap_or_default()
    } else if id == 1 {
        // $'
        executor.sp_post_match.unwrap_or_default()
    } else if id >= 100 {
        // $1, $2, ..
        executor.get_special_matches(id as i64 - 100)
    } else {
        unreachable!()
    }
}

pub(super) extern "C" fn define_class(
    executor: &mut Executor,
    globals: &mut Globals,
    name: IdentId,
    superclass: Option<Value>,
    is_module: u32,
) -> Option<Value> {
    let parent = executor.context_class_id();
    let self_val = match globals.get_constant(parent, name) {
        Some(val) => {
            val.expect_class_or_module(globals)?;
            if let Some(superclass) = superclass {
                assert!(is_module != 1);
                let superclass_id = superclass.expect_class(globals)?;
                if Some(superclass_id) != val.as_class().superclass_id() {
                    globals.err_superclass_mismatch(name);
                    return None;
                }
            }
            val.as_class()
        }
        None => {
            let superclass = match superclass {
                Some(superclass) => {
                    assert!(is_module != 1);
                    superclass.expect_class(globals)?;
                    superclass.as_class()
                }
                None => OBJECT_CLASS.get_obj(globals),
            };
            globals.define_class(name, Some(superclass), parent, is_module == 1)
        }
    };
    executor.push_class_context(self_val.class_id());
    Some(self_val.as_val())
}

pub(super) extern "C" fn define_singleton_class(
    executor: &mut Executor,
    globals: &mut Globals,
    base: Value,
) -> Option<Value> {
    let self_val = globals.get_singleton(base);
    executor.push_class_context(self_val.class_id());
    Some(self_val.as_val())
}

pub(super) extern "C" fn pop_class_context(executor: &mut Executor, _globals: &mut Globals) {
    executor.pop_class_context();
}

pub(super) extern "C" fn define_method(
    executor: &mut Executor,
    globals: &mut Globals,
    name: IdentId,
    func: FuncId,
) {
    let Cref {
        class_id,
        module_function,
        visibility,
    } = executor.get_class_context();
    let current_func = executor.cfp.method_func_id();
    globals[func].as_ruby_func_mut().lexical_context =
        globals[current_func].as_ruby_func().lexical_context.clone();
    globals.add_method(class_id, name, func, visibility);
    if module_function {
        globals.add_singleton_method(class_id, name, func, visibility);
    }
    globals.class_version_inc();
}

pub(super) extern "C" fn singleton_define_method(
    executor: &mut Executor,
    globals: &mut Globals,
    name: IdentId,
    func: FuncId,
    obj: Value,
) {
    let current_func = executor.cfp.method_func_id();
    globals[func].as_ruby_func_mut().lexical_context =
        globals[current_func].as_ruby_func().lexical_context.clone();
    let class_id = globals.get_singleton(obj).class_id();
    globals.add_method(class_id, name, func, Visibility::Public);
    globals.class_version_inc();
}

pub(super) extern "C" fn alias_method(
    globals: &mut Globals,
    self_val: Value,
    new: Value,
    old: Value,
    meta: Meta,
) -> Option<Value> {
    let new = new.as_symbol();
    let old = old.as_symbol();
    if meta.is_class_def() {
        globals.alias_method_for_class(self_val.as_class().class_id(), new, old)?
    } else {
        globals.alias_method(self_val, new, old)?
    };
    Some(Value::nil())
}

// error handling

pub(super) extern "C" fn unimplemented_inst(_: &mut Executor, _: &mut Globals, opcode: u64) {
    panic!("unimplemented inst. {opcode:016x}");
}

pub(super) extern "C" fn panic(_: &mut Executor, _: &mut Globals) {
    panic!("panic in jit code.");
}

pub(super) extern "C" fn err_divide_by_zero(globals: &mut Globals) {
    globals.err_divide_by_zero();
}

pub(super) extern "C" fn err_no_block_given(globals: &mut Globals) {
    globals.err_no_block_given();
}

pub(super) extern "C" fn err_wrong_number_of_arguments_range(
    globals: &mut Globals,
    given: usize,
    min: usize,
    max: usize,
) {
    globals.check_number_of_arguments(given, min..=max);
}

pub(super) extern "C" fn get_error_location(
    _interp: &mut Executor,
    globals: &mut Globals,
    meta: Meta,
    pc: BcPc,
) {
    let func_info = &globals[meta.func_id()];
    let bc_base = func_info.data.pc;
    let normal_info = match &func_info.kind {
        FuncKind::ISeq(info) => info,
        FuncKind::Builtin { .. } => return,
        FuncKind::AttrReader { .. } => return,
        FuncKind::AttrWriter { .. } => return,
    };
    let sourceinfo = normal_info.sourceinfo.clone();
    let loc = normal_info.sourcemap[pc - bc_base];
    globals.push_error_location(loc, sourceinfo);
}

pub unsafe extern "C" fn _dump_stacktrace(executor: &mut Executor, globals: &mut Globals) {
    let mut cfp = executor.cfp;
    eprintln!("-----begin stacktrace");
    for i in 0..16 {
        eprint!("  [{}]: {:?} {:?}", i, cfp, cfp.lfp());
        let ret_addr = cfp.return_addr();
        eprintln!(" ret adr: {ret_addr:?} ");
        let prev_cfp = cfp.prev();
        globals.dump_frame_info(cfp.lfp());
        if prev_cfp.is_null() {
            break;
        }
        cfp = prev_cfp;
    }
    eprintln!("-----end stacktrace");
}
