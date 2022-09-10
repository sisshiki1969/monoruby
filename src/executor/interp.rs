use super::compiler::Codegen;
use super::*;

///
/// Bytecode interpreter.
///
pub struct Interp {
    pub codegen: Codegen,
}

impl Interp {
    fn new(no_jit: bool, main_object: Value) -> Self {
        Self {
            codegen: Codegen::new(no_jit, main_object),
        }
    }

    /// Execute top level method.
    pub fn eval_toplevel(globals: &mut Globals) -> Result<Value> {
        let mut eval = Self::new(globals.no_jit, Value::new_object());
        if !globals.no_jit {
            eval.codegen.set_jit_stab(&mut globals.func)
        } else {
            eval.codegen.set_vm_stab(&mut globals.func)
        }
        let main_id = globals.get_main_func();
        let main_data = eval.get_func_data(globals, main_id) as *const _;

        let res = (eval.codegen.entry_point)(&mut eval, globals, main_data);
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
