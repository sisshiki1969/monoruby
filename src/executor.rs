use super::*;

mod builtins;
mod bytecodegen;
mod globals;
mod inst;
mod op;
pub use builtins::*;
use bytecodegen::*;
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
#[derive(Default)]
#[repr(C)]
pub struct Executor {
    pub cfp: usize,
    lexical_class: Vec<ClassId>,
}

impl Executor {
    ///
    /// Execute top level method.
    ///
    /// *main* object is set to *self*.
    pub fn eval(&mut self, globals: &mut Globals, func_id: FuncId) -> Result<Value> {
        let main_data = globals.compile_on_demand(func_id) as *const _;

        #[cfg(feature = "emit-bc")]
        globals.dump_bc();

        let entry_point = globals.codegen.entry_point;
        let res = entry_point(self, globals, main_data);
        globals.flush_stdout();
        #[cfg(feature = "log-jit")]
        {
            eprintln!();
            eprintln!("deoptimization stats");
            eprintln!(
                "{:15} FuncId({:3}) [{:05}]  {:10}",
                "func name", "", "index", "count"
            );
            for ((func_id, index), count) in &globals.deopt_stats {
                let name = globals.func[*func_id].as_ruby_func().name();
                eprintln!(
                    "{:15} FuncId({:3}) [{:05}]  {:10}",
                    name, func_id.0, index, count
                );
            }
        }

        res.ok_or_else(|| globals.take_error().unwrap())
    }

    pub fn eval_script(
        &mut self,
        globals: &mut Globals,
        code: String,
        path: &std::path::Path,
    ) -> Option<Value> {
        let fid = match globals.compile_script(code, path) {
            Ok(fid) => fid,
            Err(err) => {
                globals.set_error(err);
                return None;
            }
        };
        match self.eval(globals, fid) {
            Ok(val) => Some(val),
            Err(err) => {
                globals.set_error(err);
                None
            }
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
    /// Invoke method for *receiver* and *method*.
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
        let data = globals.compile_on_demand(func_id) as *const _;
        (globals.codegen.method_invoker)(self, globals, data, receiver, args.as_ptr(), args.len())
    }

    ///
    /// Invoke block for *receiver* and *method*.
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
        let data = globals.compile_on_demand(func_id) as *const _;
        (globals.codegen.block_invoker)(self, globals, data, receiver, args.as_ptr(), args.len())
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
        let data = globals.compile_on_demand(func_id) as *const _;
        (globals.codegen.method_invoker2)(self, globals, data, receiver, args, len)
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

///
/// Metadata.
///
/// ~~~text
///   7   6   5   4    3   2    1    0
/// +-------+-------+---------+----+----+
/// |    FuncId     | reg_num |kind|mode|
/// +-------+-------+---------+----+----+
///
/// kind:   0 VM
///         1 JIT
///         2 NATIVE
///
/// mode:   0 method
///         1 class def
/// ~~~
///
#[derive(Clone, Copy, PartialEq, Eq, Default)]
#[repr(transparent)]
pub struct Meta(u64);

impl std::fmt::Debug for Meta {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "kind:{} mode:{} {:?} regs:{}",
            match self.kind() {
                0 => "VM",
                1 => "JIT",
                2 => "NATIVE",
                _ => "INVALID",
            },
            match self.mode() {
                0 => "method",
                1 => "class_def",
                _ => "INVALID",
            },
            self.func_id(),
            self.reg_num()
        )
    }
}

impl Meta {
    pub(crate) fn new(meta: u64) -> Self {
        Self(meta)
    }

    pub(crate) fn vm_method(func_id: FuncId, reg_num: i64) -> Self {
        // kind = VM, mode = method
        Self(((reg_num as i16 as u16 as u64) << 32) + (func_id.0 as u64))
    }

    pub(crate) fn vm_classdef(func_id: FuncId, reg_num: i64) -> Self {
        // kind = VM, mode = classdef
        Self((1 << 56) + ((reg_num as i16 as u16 as u64) << 32) + (func_id.0 as u64))
    }

    pub(crate) fn native(func_id: FuncId, reg_num: i64) -> Self {
        // kind = NATIVE, mode = method
        Self((2 << 48) + ((reg_num as i16 as u16 as u64) << 32) + (func_id.0 as u64))
    }

    pub(crate) fn func_id(&self) -> FuncId {
        FuncId(self.0 as u32)
    }

    pub(crate) fn reg_num(&self) -> i64 {
        (self.0 >> 32) as u16 as i16 as i64
    }

    pub(crate) fn kind(&self) -> u8 {
        (self.0 >> 48) as u8
    }

    pub(crate) fn mode(&self) -> u8 {
        (self.0 >> 56) as u8
    }

    ///
    /// Set JIT flag in Meta.
    ///
    pub(crate) fn set_jit(&mut self) {
        let meta = (self.0 & 0xff00_ffff_ffff_ffff) | (1 << 48);
        self.0 = meta;
    }

    ///
    /// Set the number of registers in Meta.
    ///
    pub(crate) fn set_reg_num(&mut self, reg_num: i64) {
        let meta = (self.0 & 0xffff_0000_ffff_ffff) | ((reg_num as i16 as u16 as u64) << 32);
        self.0 = meta;
    }
}

#[derive(Debug, Clone, PartialEq, Default)]
#[repr(C)]
pub struct FuncData {
    /// address of function.
    pub codeptr: Option<CodePtr>,
    /// the address of program counter
    pub pc: BcPc,
    /// metadata of this function.
    pub meta: Meta,
}

impl FuncData {
    fn set_reg_num(&mut self, reg_num: i64) {
        self.meta.set_reg_num(reg_num);
    }
}

#[test]
fn meta_test() {
    let meta = Meta::vm_method(FuncId(12), 42);
    assert_eq!(FuncId(12), meta.func_id());
    assert_eq!(42, meta.reg_num());
    assert_eq!(0, meta.mode());
    let mut meta = Meta::vm_method(FuncId(42), -1);
    assert_eq!(FuncId(42), meta.func_id());
    assert_eq!(-1, meta.reg_num());
    meta.set_reg_num(12);
    assert_eq!(FuncId(42), meta.func_id());
    assert_eq!(12, meta.reg_num());
    let mut meta = Meta::vm_classdef(FuncId(12), 42);
    assert_eq!(1, meta.mode());
    meta.set_reg_num(12);
    meta.set_jit();
    assert_eq!(1, meta.mode());
    assert_eq!(8, std::mem::size_of::<i64>());
    assert_eq!(8, std::mem::size_of::<Option<CodePtr>>());
    assert_eq!(8, std::mem::size_of::<BcPc>());
    assert_eq!(8, std::mem::size_of::<Meta>());
}
