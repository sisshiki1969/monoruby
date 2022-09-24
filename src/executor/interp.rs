use super::compiler::Codegen;
use super::*;

///
/// Bytecode interpreter.
///
pub struct Interp {
    pub codegen: Codegen,
    lexical_class: Vec<ClassId>,
}

impl Interp {
    pub fn new(no_jit: bool) -> Self {
        Self {
            codegen: Codegen::new(no_jit, Value::main_object()),
            lexical_class: vec![],
        }
    }

    pub(crate) fn push_class_context(&mut self, class_id: ClassId) {
        self.lexical_class.push(class_id);
    }

    pub(crate) fn pop_class_context(&mut self) -> Option<ClassId> {
        self.lexical_class.pop()
    }

    pub(crate) fn get_class_context(&self) -> ClassId {
        self.lexical_class.last().cloned().unwrap_or(OBJECT_CLASS)
    }

    pub(crate) fn class_context_stack(&self) -> &[ClassId] {
        &self.lexical_class
    }

    /// Execute top level method.
    pub fn eval_toplevel(globals: &mut Globals, func_id: FuncId) -> Result<Value> {
        let mut eval = Self::new(globals.no_jit);
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
