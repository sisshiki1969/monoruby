use super::compiler::Codegen;
use super::*;

/*fn conv(reg: u16) -> i64 {
    reg as i64 * 8 + 16
}*/

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

    pub fn jit_exec_toplevel(globals: &mut Globals) -> Result<Value> {
        let mut eval = Self::new();
        let main_id = globals.get_main_func();

        let f = eval.codegen.exec_toplevel(globals);
        let res = f(&mut eval, globals, main_id);
        globals.stdout.flush().unwrap();
        res.ok_or_else(|| globals.take_error().unwrap())
    }

    pub fn eval_toplevel(globals: &mut Globals) -> Result<Value> {
        let mut eval = Self::new();
        let main_id = globals.get_main_func();

        let f = eval.codegen.vm_entry_point;
        eval.codegen.precompile(&mut globals.func);

        let res = f(&mut eval, globals, main_id);
        globals.stdout.flush().unwrap();
        res.ok_or_else(|| globals.take_error().unwrap())
    }
}

impl Interp {
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
}
