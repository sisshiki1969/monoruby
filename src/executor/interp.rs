use super::compiler::JitGen;
use super::*;
use monoasm::*;
use monoasm_macro::monoasm;
use paste::paste;
mod vmgen;

///
/// Program counter base.
///
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct BcPcBase(*const u64);

impl std::ops::Add<usize> for BcPcBase {
    type Output = BcPc;
    fn add(self, rhs: usize) -> BcPc {
        BcPc(unsafe { self.0.offset(rhs as isize) })
    }
}

impl std::ops::Add<InstId> for BcPcBase {
    type Output = BcPc;
    fn add(self, rhs: InstId) -> BcPc {
        BcPc(unsafe { self.0.offset(rhs.0 as isize) })
    }
}

impl BcPcBase {
    pub(super) fn new(func: &NormalFuncInfo) -> Self {
        BcPcBase(&func.bytecode()[0] as *const _)
    }
}

///
/// Program counter
///
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct BcPc(*const u64);

impl std::ops::Sub<BcPcBase> for BcPc {
    type Output = usize;
    fn sub(self, rhs: BcPcBase) -> usize {
        let offset = unsafe { self.0.offset_from(rhs.0) };
        assert!(offset >= 0);
        offset as usize
    }
}

impl std::ops::AddAssign<i32> for BcPc {
    fn add_assign(&mut self, offset: i32) {
        unsafe {
            *self = BcPc(self.0.offset(offset as isize));
        }
    }
}

impl std::default::Default for BcPc {
    fn default() -> Self {
        Self(std::ptr::null())
    }
}

/*fn conv(reg: u16) -> i64 {
    reg as i64 * 8 + 16
}*/

#[derive(Debug, Clone)]
#[repr(C)]
struct FuncData {
    offset: usize,
    address: u64,
    pc: BcPc,
}

#[derive(Debug, Clone, Copy)]
#[repr(transparent)]
struct EncodedCallInfo(u64);

impl EncodedCallInfo {
    fn new(func_id: FuncId, args: u16, len: u16) -> Self {
        Self((func_id.0 as u64) + ((args as u64) << 48) + ((len as u64) << 32))
    }

    fn none() -> Self {
        Self(-1i64 as u64)
    }
}

///
/// Bytecode interpreter.
///
pub struct Interp {
    cur_fn: FuncId,
    pub jit_gen: JitGen,
    pub error: Option<MonorubyErr>,
    dispatch: Vec<CodePtr>,
    class_version: usize,
    vm_entry: DestLabel,
}

impl Interp {
    fn new(main: &NormalFuncInfo) -> Self {
        let mut jit_gen = JitGen::new();
        let vm_entry = jit_gen.jit.label();
        // dispatch table.
        let entry_panic = jit_gen.jit.get_current_address();
        monoasm! { jit_gen.jit,
                movq rdi, rbx;
                movq rsi, r12;
                movq rax, (super::compiler::unimplemented_inst);
                call rax;
                leave;
                ret;
        };
        let dispatch = vec![entry_panic; 256];
        Self {
            cur_fn: main.id,
            jit_gen,
            error: None,
            dispatch,
            class_version: 0,
            vm_entry,
        }
    }

    fn find_method(
        &mut self,
        globals: &mut Globals,
        callsite_id: CallsiteId,
    ) -> Option<(FuncId, u16, u16)> {
        let CallsiteInfo {
            name,
            args,
            len,
            cache: (version, cached_func),
        } = globals.func[callsite_id];
        let func_id = if version == self.class_version {
            cached_func
        } else {
            match globals.get_method(name) {
                Some(func_id) => {
                    globals.func[callsite_id].cache = (self.class_version, func_id);
                    func_id
                }
                None => {
                    self.error = Some(MonorubyErr::method_not_found(name));
                    return None;
                }
            }
        };
        let info = &globals.func[func_id];
        if info.arity() != len as usize {
            self.error = Some(MonorubyErr::wrong_arguments(info.arity(), len as usize));
            None
        } else {
            Some((func_id, args, len))
        }
    }

    pub fn jit_exec_toplevel(globals: &mut Globals) -> Result<Value> {
        let main = globals.func[globals.get_main_func()].as_normal();
        let mut eval = Self::new(main);
        let f = eval.jit_gen.exec_toplevel(globals);
        let res = f(&mut eval, globals);
        res.ok_or_else(|| std::mem::take(&mut eval.error).unwrap())
    }

    pub fn eval_toplevel(globals: &mut Globals) -> Result<Value> {
        let main_id = globals.get_main_func();
        let main = globals.func[main_id].as_normal();
        let mut eval = Self::new(main);

        let entry = eval.construct_vm();
        let vm_entry = eval.jit_gen.jit.get_label_address(eval.vm_entry);
        globals.func.precompile(&mut eval.jit_gen, vm_entry);

        let addr: fn(&mut Interp, &mut Globals, FuncId) -> Option<Value> =
            unsafe { std::mem::transmute(entry.0) };
        match addr(&mut eval, globals, main_id) {
            Some(val) => Ok(val),
            None => Err(MonorubyErr::Unimplemented(format!("_"))),
        }
    }
}
