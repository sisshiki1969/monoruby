use super::compiler::Codegen;
use super::*;

///
/// Bytecode interpreter.
///
pub struct Interp {
    pub codegen: Codegen,
}

impl Interp {
    pub fn new(no_jit: bool, main_object: Value) -> Self {
        Self {
            codegen: Codegen::new(no_jit, main_object),
        }
    }

    /// Execute top level method.
    pub fn eval_toplevel(globals: &mut Globals, func_id: FuncId) -> Result<Value> {
        let mut eval = Self::new(globals.no_jit, Value::new_object());
        eval.eval(globals, func_id)
    }

    /// Execute top level method.
    pub fn eval(&mut self, globals: &mut Globals, func_id: FuncId) -> Result<Value> {
        if !globals.no_jit {
            self.codegen.set_jit_stab(&mut globals.func)
        } else {
            self.codegen.set_vm_stab(&mut globals.func)
        }
        let main_data = self.get_func_data(globals, func_id) as *const _;

        let entry_point = self.codegen.entry_point;
        let res = entry_point(self, globals, main_data);
        globals.flush_stdout();

        #[cfg(feature = "emit-bc")]
        globals.dump_bc();

        res.ok_or_else(|| globals.take_error().unwrap())
    }
}

impl Interp {
    /// Invoke method from native function.
    pub fn invoke_method(
        &mut self,
        globals: &mut Globals,
        method: IdentId,
        receiver: Value,
        args: &[Value],
    ) -> Option<Value> {
        let len = args.len();
        let func_id = globals.get_method(receiver.class_id(), method, len)?;
        let data = self.get_func_data(globals, func_id) as *const _;
        (self.codegen.invoker)(self, globals, data, receiver, args.as_ptr(), len)
    }

    pub fn get_func_data<'a>(&mut self, globals: &'a mut Globals, func_id: FuncId) -> &'a FuncData {
        self.codegen.compile_on_demand(globals, func_id);
        &globals.func[func_id].data
    }
}
