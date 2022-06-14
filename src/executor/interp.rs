use super::compiler::JitGen;
use super::*;

///
/// Program counter base.
///
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
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

impl std::ops::Sub<BcPc> for BcPc {
    type Output = usize;
    fn sub(self, rhs: BcPc) -> usize {
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

///
/// Bytecode interpreter.
///
pub struct Interp {
    pub jit_gen: JitGen,
    pub error: Option<MonorubyErr>,
}

impl Interp {
    fn new() -> Self {
        Self {
            jit_gen: JitGen::new(),
            error: None,
        }
    }

    pub fn get_method(
        &mut self,
        globals: &Globals,
        class_id: ClassId,
        func_name: IdentId,
        args_len: usize,
    ) -> Option<FuncId> {
        let func_id = match globals.get_method(class_id, func_name) {
            Some(id) => id,
            None => {
                self.error = Some(MonorubyErr::method_not_found(func_name));
                return None;
            }
        };
        let arity = globals.func[func_id].arity();
        if arity != args_len {
            self.error = Some(MonorubyErr::wrong_arguments(arity, args_len));
            return None;
        }
        Some(func_id)
    }

    pub fn find_method(
        &mut self,
        globals: &mut Globals,
        callsite_id: CallsiteId,
        receiver: Value,
        class_version: usize,
    ) -> Option<(FuncId, u16, u16, u16)> {
        let CallsiteInfo {
            ret,
            name,
            args,
            len,
            cache: (version, cached_class_id, cached_func),
        } = globals.func[callsite_id];
        let recv_class = receiver.class();
        let func_id = if version == class_version && cached_class_id == recv_class {
            cached_func
        } else {
            match self.get_method(globals, recv_class, name, len as usize) {
                Some(id) => {
                    globals.func[callsite_id].cache = (class_version, recv_class, id);
                    id
                }
                None => return None,
            }
        };
        Some((func_id, args, len, ret))
    }

    pub fn jit_exec_toplevel(globals: &mut Globals) -> Result<Value> {
        let mut eval = Self::new();
        let f = eval.jit_gen.exec_toplevel(globals);
        let res = f(&mut eval, globals);
        res.ok_or_else(|| std::mem::take(&mut eval.error).unwrap())
    }

    pub fn eval_toplevel(globals: &mut Globals) -> Result<Value> {
        let main_id = globals.get_main_func();
        let mut eval = Self::new();

        let entry = eval.jit_gen.construct_vm();
        let vm_entry = eval.jit_gen.jit.get_label_address(eval.jit_gen.vm_entry);
        globals.func.precompile(&mut eval.jit_gen, vm_entry);

        let addr: fn(&mut Interp, &mut Globals, FuncId) -> Option<Value> =
            unsafe { std::mem::transmute(entry.as_ptr()) };
        match addr(&mut eval, globals, main_id) {
            Some(val) => Ok(val),
            None => Err(eval.error.unwrap()),
        }
    }
}
