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
    pub fn new(globals: &mut Globals, no_jit: bool) -> Self {
        Self {
            codegen: Codegen::new(no_jit, Value::main_object(globals)),
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
    pub(crate) fn eval_toplevel(globals: &mut Globals, func_id: FuncId) -> Result<Value> {
        let mut eval = Self::new(globals, globals.no_jit);
        eval.eval(globals, func_id)
    }

    pub(crate) fn class_version_inc(&mut self) {
        unsafe { *self.codegen.class_version_addr += 1 }
    }

    /// Execute top level method.
    pub fn eval(&mut self, globals: &mut Globals, func_id: FuncId) -> Result<Value> {
        let main_data = self.get_func_data(globals, func_id) as *const _;

        #[cfg(feature = "emit-bc")]
        globals.dump_bc();

        let entry_point = self.codegen.entry_point;
        let res = entry_point(self, globals, main_data);
        globals.flush_stdout();

        res.ok_or_else(|| globals.take_error().unwrap())
    }
}

impl Interp {
    ///
    /// Find Constant in current class context.
    ///
    pub(crate) fn find_constant(
        &self,
        globals: &mut Globals,
        site_id: ConstSiteId,
    ) -> Option<Value> {
        let current = self.class_context_stack();
        globals.find_constant(site_id, current)
    }

    pub(crate) fn set_constant(&self, globals: &mut Globals, name: IdentId, val: Value) {
        let parent = self.get_class_context();
        if globals.set_constant(parent, name, val).is_some() && globals.warning >= 1 {
            eprintln!(
                "warning: already initialized constant {}",
                IdentId::get_name(name)
            )
        }
    }
}

impl Interp {
    ///
    /// Invoke method for *receiver* and *method* from native function.
    ///
    pub(crate) fn invoke_method(
        &mut self,
        globals: &mut Globals,
        method: IdentId,
        receiver: Value,
        args: &[Value],
    ) -> Option<Value> {
        let len = args.len();
        let func_id = globals.find_method_checked(receiver, method, len)?;
        self.invoke_func(globals, func_id, receiver, args)
    }

    ///
    /// Invoke func with *args*: &\[Value\].
    ///
    pub(crate) fn invoke_func(
        &mut self,
        globals: &mut Globals,
        func_id: FuncId,
        receiver: Value,
        args: &[Value],
    ) -> Option<Value> {
        let data = self.get_func_data(globals, func_id) as *const _;
        (self.codegen.invoker)(self, globals, data, receiver, args.as_ptr(), args.len())
    }

    pub(crate) fn invoke_block(
        &mut self,
        globals: &mut Globals,
        block: Value,
        receiver: Value,
        args: &[Value],
    ) -> Option<Value> {
        let func_id = match block.unpack() {
            RV::Integer(id) => FuncId(id as u32),
            _ => unimplemented!(),
        };
        let data = self.get_func_data(globals, func_id) as *const _;
        (self.codegen.invoker)(self, globals, data, receiver, args.as_ptr(), args.len())
    }

    ///
    /// Invoke func with *args*: Args.
    ///
    pub(crate) fn invoke_func2(
        &mut self,
        globals: &mut Globals,
        func_id: FuncId,
        receiver: Value,
        args: Arg,
        len: usize,
    ) -> Option<Value> {
        let data = self.get_func_data(globals, func_id) as *const _;
        (self.codegen.invoker2)(self, globals, data, receiver, args, len)
    }

    pub(crate) fn get_func_data<'a>(
        &mut self,
        globals: &'a mut Globals,
        func_id: FuncId,
    ) -> &'a FuncData {
        self.codegen.compile_on_demand(globals, func_id);
        &globals.func[func_id].data
    }
}
