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
    func_name: IdentId,
    args_len: usize,
    receiver: Value,
    // register id of *self*
    recv_reg: u16,
) -> Option<std::ptr::NonNull<FuncData>> {
    let func_id = globals.find_method(receiver, func_name, recv_reg == 0)?;
    globals.check_arg(func_id, args_len)?;
    let func_data = globals.compile_on_demand(func_id);
    Some(std::ptr::NonNull::new(func_data as *const _ as _).unwrap())
}

pub(super) extern "C" fn get_classdef_data<'a>(
    executor: &mut Executor,
    globals: &'a mut Globals,
    func_id: FuncId,
    self_value: Module,
) -> &'a FuncData {
    let current_func = executor.cfp.method_func_id();
    let mut lexical_context = globals.func[current_func]
        .as_ruby_func()
        .lexical_context
        .clone();
    lexical_context.push(self_value);
    globals.func[func_id].as_ruby_func_mut().lexical_context = lexical_context;
    globals.compile_on_demand(func_id)
}

pub(super) extern "C" fn get_block_data(executor: &Executor, globals: &mut Globals) -> BlockData {
    executor
        .cfp
        .lfp()
        .block()
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

pub(super) extern "C" fn distibute_keyword_arguments(
    globals: &Globals,
    reg: *mut Option<Value>,
    keyword: Option<Value>,
    meta: Meta,
) -> *mut Option<Value> {
    let func_id = meta.func_id.unwrap();
    let keyword = match keyword {
        Some(kw) if kw != Value::nil() => kw,
        _ => {
            match &globals.func[func_id].kind {
                FuncKind::ISeq(info) => {
                    let params = &info.args.keyword_args;
                    let len = params.len();
                    unsafe {
                        for (id, _) in params.iter().enumerate() {
                            *reg.sub(id) = None;
                        }
                        return reg.sub(len);
                    }
                }
                _ => {}
            };
            return reg;
        }
    };
    match &globals.func[func_id].kind {
        FuncKind::ISeq(info) => {
            let kw_arg = keyword.as_hash();
            let params = &info.args.keyword_args;
            unsafe {
                let len = params.len();
                for (id, (param_name, _)) in params.iter().enumerate() {
                    *reg.sub(id) = kw_arg.get(Value::new_symbol(*param_name));
                }
                reg.sub(len)
            }
        }
        _ => reg,
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
    globals.func[func].as_ruby_func_mut().lexical_context = globals.func[current_func]
        .as_ruby_func()
        .lexical_context
        .clone();
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
    globals.func[func].as_ruby_func_mut().lexical_context = globals.func[current_func]
        .as_ruby_func()
        .lexical_context
        .clone();
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
    let func_info = &globals.func[meta.func_id()];
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
        eprintln!("ret adr: {ret_addr:?} ");
        let prev_cfp = cfp.prev();
        globals.dump_frame_info(cfp.lfp());
        if prev_cfp.is_null() {
            break;
        }
        cfp = prev_cfp;
    }
    eprintln!("-----end stacktrace");
}
