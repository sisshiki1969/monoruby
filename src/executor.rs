use super::*;

mod builtins;
mod bytecodegen;
mod compiler;
mod globals;
mod inst;
mod op;
pub use builtins::*;
use bytecodegen::*;
pub use compiler::*;
pub use globals::*;
use inst::*;
use op::*;

type Result<T> = std::result::Result<T, MonorubyErr>;
pub type BuiltinFn =
    extern "C" fn(&mut Executor, &mut Globals, Value, Arg, usize, Option<Value>) -> Option<Value>;

pub(self) const OFFSET_CFP: i64 = 8;
pub(self) const OFFSET_OUTER: i64 = 16;
/// Meta 8bytes
pub(self) const OFFSET_META: i64 = 24;
/// Meta::Regnum 2bytes
pub(self) const OFFSET_REGNUM: i64 = OFFSET_META - 4;
/// Meta::FuncId 4bytes
pub(self) const OFFSET_FUNCID: i64 = OFFSET_META;
pub(self) const OFFSET_BLOCK: i64 = 32;
pub(self) const OFFSET_SELF: i64 = 40;
pub(self) const OFFSET_ARG0: i64 = OFFSET_SELF + 8;

///
/// Bytecode interpreter.
///
#[repr(C)]
pub struct Executor {
    pub cfp: usize,
    pub codegen: Codegen,
    lexical_class: Vec<ClassId>,
}

impl Executor {
    pub fn new(globals: &mut Globals, no_jit: bool) -> Self {
        Self {
            cfp: 0,
            codegen: Codegen::new(no_jit, Value::main_object(globals)),
            lexical_class: vec![],
        }
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
}

impl Executor {
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

impl Executor {
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
        let data = self.get_func_data(globals, func_id) as *const _;
        (self.codegen.method_invoker)(self, globals, data, receiver, args.as_ptr(), args.len())
    }

    ///
    /// Invoke block for *receiver* and *method* from native function.
    ///
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
        (self.codegen.block_invoker)(self, globals, data, receiver, args.as_ptr(), args.len())
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
        (self.codegen.method_invoker2)(self, globals, data, receiver, args, len)
    }

    pub(crate) fn get_func_data<'a>(
        &mut self,
        globals: &'a mut Globals,
        func_id: FuncId,
    ) -> &'a FuncData {
        self.codegen.compile_on_demand(globals, func_id)
    }
}

#[derive(Clone, Copy, PartialEq, PartialOrd)]
pub(crate) struct SlotId(u16);

impl SlotId {
    pub(crate) fn new(reg: u16) -> Self {
        Self(reg)
    }

    pub(crate) fn self_() -> Self {
        Self(0)
    }

    pub(crate) fn is_zero(&self) -> bool {
        self.0 == 0
    }

    pub(crate) fn ret_str(&self) -> String {
        match self.0 {
            0 => "_".to_string(),
            ret => format!("%{}", ret),
        }
    }
}

impl std::fmt::Debug for SlotId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "%{}", self.0)
    }
}

impl std::ops::Add<u16> for SlotId {
    type Output = Self;
    fn add(self, rhs: u16) -> Self {
        Self(self.0 + rhs)
    }
}
