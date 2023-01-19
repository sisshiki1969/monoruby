use super::*;

//
// Runtime functions.
//

///
/// Get an absolute address of the given method.
///
/// If no method was found, return None (==0u64).
///
pub(super) extern "C" fn find_method(
    globals: &mut Globals,
    func_name: IdentId,
    args_len: usize,
    receiver: Value,
) -> Option<std::ptr::NonNull<FuncData>> {
    let func_id = globals.find_method_checked(receiver, func_name, args_len)?;
    let func_data = globals.compile_on_demand(func_id);
    Some(std::ptr::NonNull::new(func_data as *const _ as _).unwrap())
}

pub(super) extern "C" fn get_func_data(globals: &mut Globals, func_id: FuncId) -> &FuncData {
    globals.compile_on_demand(func_id)
}

pub(super) extern "C" fn get_block_data(
    globals: &mut Globals,
    block_handler: BlockHandler,
    interp: &Executor,
) -> BlockData {
    globals.get_block_data(block_handler, interp)
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
    let base_classid = base.class_id();
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
    class_slot.idx = index.class_id();
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
    let base_classid = base.class_id();
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
    class_slot.idx = index.class_id();
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

pub(super) extern "C" fn get_instance_var(
    base: Value,
    name: IdentId,
    globals: &mut Globals,
) -> Value {
    globals.get_ivar(base, name).unwrap_or_default()
}

pub(super) extern "C" fn set_instance_var(
    globals: &mut Globals,
    base: Value,
    name: IdentId,
    val: Value,
) -> Option<Value> {
    globals.set_ivar(base, name, val)?;
    Some(val)
}

pub(super) extern "C" fn define_class(
    interp: &mut Executor,
    globals: &mut Globals,
    name: IdentId,
    superclass: Option<Value>,
) -> Option<Value> {
    let parent = interp.get_class_context();
    let self_val = match globals.get_constant(parent, name) {
        Some(val) => {
            let class = val.expect_class(globals)?;
            if let Some(superclass) = superclass {
                let super_class = superclass.expect_class(globals)?;
                if Some(super_class) != class.super_class(globals) {
                    globals.err_superclass_mismatch(name);
                    return None;
                }
            }
            val
        }
        None => {
            let superclass = match superclass {
                Some(superclass) => superclass.expect_class(globals)?,
                None => OBJECT_CLASS,
            };
            globals.define_class_by_ident_id(name, Some(superclass), parent)
        }
    };
    //globals.get_singleton_id(self_val.as_class());
    interp.push_class_context(self_val.as_class());
    Some(self_val)
}

pub(super) extern "C" fn pop_class_context(interp: &mut Executor, _globals: &mut Globals) {
    interp.pop_class_context();
}

pub(super) extern "C" fn unimplemented_inst(_: &mut Executor, _: &mut Globals, opcode: u64) {
    panic!("unimplemented inst. {:016x}", opcode);
}

pub(super) extern "C" fn panic(_: &mut Executor, _: &mut Globals) {
    panic!("panic in jit code.");
}

/*pub extern "C" fn eprintln(rdi: u64, rsi: u64) {
  eprintln!("rdi:{:016x} rsi:{:016x}", rdi, rsi);
}*/

pub(super) extern "C" fn err_divide_by_zero(globals: &mut Globals) {
    globals.err_divide_by_zero();
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
