use super::*;

mod builtins;
mod bytecodegen;
mod compiler;
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

pub(self) const BP_PREV_CFP: i64 = 8;
pub(self) const BP_LFP: i64 = 16;
pub(self) const BP_OUTER: i64 = 24;
/// Meta 8bytes
pub(self) const BP_META: i64 = 32;
/// Meta::Regnum 2bytes
pub(self) const BP_META_REGNUM: i64 = BP_META - 4;
/// Meta::FuncId 4bytes
pub(self) const BP_META_FUNCID: i64 = BP_META;
pub(self) const BP_BLOCK: i64 = 40;
pub(self) const BP_SELF: i64 = 48;
pub(self) const BP_ARG0: i64 = BP_SELF + 8;

#[derive(Debug, Clone, Copy)]
#[repr(transparent)]
pub struct CFP(*const CFP);

impl std::default::Default for CFP {
    fn default() -> Self {
        Self(std::ptr::null())
    }
}

impl CFP {
    ///
    /// Get CFP of previous frame of *self*.
    ///
    fn prev(&self) -> Self {
        unsafe { *self.0 }
    }

    fn is_null(&self) -> bool {
        self.0.is_null()
    }

    fn return_addr(&self) -> *const usize {
        unsafe { (*self.0.add(2)).0 as _ }
    }

    fn bp(&self) -> *const usize {
        unsafe { self.0.add(BP_PREV_CFP as usize / 8) as _ }
    }

    ///
    /// Get LFP.
    ///
    fn lfp(&self) -> LFP {
        let bp = self.bp();
        LFP(unsafe { *bp.sub(BP_LFP as usize / 8) as _ })
    }
}

#[derive(Debug, Clone, Copy)]
#[repr(transparent)]
pub struct LFP(*const u8);

impl LFP {
    ///
    /// Get outer.
    ///
    fn outer(&self) -> usize {
        unsafe { *(self.0.sub(BP_OUTER as usize) as *const usize) }
    }

    ///
    /// Get Meta.
    ///
    fn meta(&self) -> Meta {
        Meta::new(unsafe { *(self.0.sub(BP_META as usize) as *const u64) })
    }

    ///
    /// Get block.
    ///
    fn block(&self) -> Option<Value> {
        unsafe { *(self.0.sub(BP_BLOCK as usize) as *const Option<Value>) }
    }

    ///
    /// Get a value of register slot *index*.
    ///
    fn register(&self, index: usize) -> Value {
        unsafe { *(self.0.sub(BP_SELF as usize + 8 * index) as *const Value) }
    }
}

///
/// Bytecode interpreter.
///
#[derive(Default)]
#[repr(C)]
pub struct Executor {
    pub cfp: CFP,
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
            eprintln!("{:20} FuncId [{:05}]  {:10}", "func name", "index", "count");
            eprintln!("-----------------------------------------------");
            let mut v: Vec<_> = globals.deopt_stats.iter().collect();
            v.sort_unstable_by(|(_, a), (_, b)| b.cmp(a));
            for ((func_id, index), count) in v {
                let name = globals.func[*func_id].as_ruby_func().name();
                eprintln!("{:20}  {:5} [{:05}]  {:10}", name, func_id.0, index, count);
            }
            eprintln!();
            eprintln!("method cache stats");
            eprintln!("{:20} {:15} {:10}", "func name", "class", "count");
            eprintln!("-----------------------------------------------");
            let mut v: Vec<_> = globals.method_cache_stats.iter().collect();
            v.sort_unstable_by(|(_, a), (_, b)| b.cmp(a));
            for ((class_id, name), count) in v {
                eprintln!(
                    "{:20} {:15} {:10}",
                    IdentId::get_name(*name),
                    class_id.get_name(globals),
                    count
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
        block_handler: Value,
        receiver: Value,
        args: &[Value],
    ) -> Option<Value> {
        let data = globals.get_block_data(block_handler, self);
        (globals.codegen.block_invoker)(
            self,
            globals,
            &data as _,
            receiver,
            args.as_ptr(),
            args.len(),
        )
    }

    pub(crate) fn invoke_method2_if_exists(
        &mut self,
        globals: &mut Globals,
        method: IdentId,
        receiver: Value,
        args: Arg,
        len: usize,
    ) -> Option<Value> {
        if let Some(func_id) = globals.find_method(receiver, method) {
            globals.check_arg(func_id, len)?;
            self.invoke_func2(globals, func_id, receiver, args, len)
        } else {
            Some(Value::nil())
        }
    }

    ///
    /// Invoke func with *args*: Args.
    ///
    fn invoke_func2(
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
#[repr(C)]
pub struct Meta {
    func_id: FuncId,
    reg_num: u16,
    kind: u8,
    mode: u8,
}

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
        unsafe { std::mem::transmute(meta) }
    }

    pub(crate) fn get(&self) -> u64 {
        unsafe { std::mem::transmute(*self) }
    }

    pub(crate) fn vm_method(func_id: FuncId, reg_num: i64) -> Self {
        // kind = VM, mode = method
        let reg_num = reg_num as i16 as u16;
        Self {
            func_id,
            reg_num,
            kind: 0,
            mode: 0,
        }
    }

    pub(crate) fn vm_classdef(func_id: FuncId, reg_num: i64) -> Self {
        // kind = VM, mode = classdef
        let reg_num = reg_num as i16 as u16;
        Self {
            func_id,
            reg_num,
            kind: 0,
            mode: 1,
        }
    }

    pub(crate) fn native(func_id: FuncId, reg_num: i64) -> Self {
        // kind = NATIVE, mode = method
        let reg_num = reg_num as i16 as u16;
        Self {
            func_id,
            reg_num,
            kind: 2,
            mode: 0,
        }
    }

    pub(crate) fn func_id(&self) -> FuncId {
        self.func_id
    }

    pub(crate) fn reg_num(&self) -> i64 {
        self.reg_num as i16 as i64
    }

    pub(crate) fn kind(&self) -> u8 {
        self.kind
    }

    pub(crate) fn mode(&self) -> u8 {
        self.mode
    }

    ///
    /// Set JIT flag in Meta.
    ///
    pub(crate) fn set_jit(&mut self) {
        self.kind = 1;
    }

    ///
    /// Set the number of registers in Meta.
    ///
    pub(crate) fn set_reg_num(&mut self, reg_num: i64) {
        self.reg_num = reg_num as i16 as u16;
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
