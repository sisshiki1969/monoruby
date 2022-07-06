use super::compiler::Codegen;
use super::*;

///
/// Bytecode interpreter.
///
pub struct Interp {
    pub codegen: Codegen,
}

impl Interp {
    fn new() -> Self {
        Self {
            codegen: Codegen::new(),
        }
    }

    /// Execute top level method.
    pub fn eval_toplevel(globals: &mut Globals, jit_flag: bool) -> Result<Value> {
        let mut eval = Self::new();
        let main_id = globals.get_main_func();
        let main_data = eval.get_func_data(globals, main_id) as *const _;

        if !jit_flag {
            eval.codegen.precompile(&mut globals.func)
        };

        let res = (eval.codegen.entry_point)(&mut eval, globals, main_data);
        globals.flush_stdout();
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
        (self.codegen.invoker)(self, globals, func_id, receiver, args.as_ptr(), len)
    }

    pub fn get_func_data<'a>(&mut self, globals: &'a mut Globals, func_id: FuncId) -> &'a FuncData {
        let _ = self.codegen.compile_on_demand(globals, func_id);
        &globals.func[func_id].data
    }
}
