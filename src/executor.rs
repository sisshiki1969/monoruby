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

const BP_PREV_CFP: i64 = 8;
const BP_LFP: i64 = 16;
const LBP_OUTER: i64 = 24;
/// Meta 8bytes
const LBP_META: i64 = 32;
/// Meta::Regnum 2bytes
const LBP_META_REGNUM: i64 = LBP_META - 4;
/// Meta::FuncId 4bytes
const LBP_META_FUNCID: i64 = LBP_META;
const LBP_BLOCK: i64 = 40;
const LBP_SELF: i64 = 48;
const LBP_ARG0: i64 = LBP_SELF + 8;

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
#[repr(transparent)]
struct CFP(*const CFP);

impl std::default::Default for CFP {
    fn default() -> Self {
        Self(std::ptr::null())
    }
}

impl CFP {
    ///
    /// Get CFP of previous frame of *self*.
    ///
    unsafe fn prev(&self) -> Self {
        *self.0
    }

    fn is_null(&self) -> bool {
        self.0.is_null()
    }

    unsafe fn return_addr(&self) -> *const usize {
        (*self.0.add(2)).0 as _
    }

    unsafe fn bp(&self) -> *const usize {
        self.0.add(BP_PREV_CFP as usize / 8) as _
    }

    ///
    /// Get LFP.
    ///
    unsafe fn lfp(&self) -> LFP {
        let bp = self.bp();
        LFP(*bp.sub(BP_LFP as usize / 8) as _)
    }

    ///
    /// Set LFP.
    ///
    unsafe fn set_lfp(&mut self, lfp: LFP) {
        let bp = self.bp() as *mut usize;
        *bp.sub(BP_LFP as usize / 8) = lfp.0 as _;
    }
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
#[repr(transparent)]
struct LFP(*const u8);

impl std::default::Default for LFP {
    fn default() -> Self {
        Self(std::ptr::null())
    }
}

impl LFP {
    unsafe fn cfp(&self) -> CFP {
        CFP(self.0.sub(BP_PREV_CFP as usize) as _)
    }

    unsafe fn outer_address(&self) -> DFP {
        DFP(self.0.sub(LBP_OUTER as usize) as _)
    }

    ///
    /// Get outer.
    ///
    unsafe fn outer(&self) -> DFP {
        self.outer_address().outer()
    }

    ///
    /// Set outer.
    ///
    unsafe fn set_outer(&mut self, outer: DFP) {
        *self.outer_address().0 = outer;
    }

    ///
    /// Get Meta.
    ///
    unsafe fn meta(&self) -> Meta {
        Meta::new(*(self.0.sub(LBP_META as usize) as *const u64))
    }

    ///
    /// Get block.
    ///
    unsafe fn block(&self) -> Option<Value> {
        *(self.0.sub(LBP_BLOCK as usize) as *const Option<Value>)
    }

    ///
    /// Get a value of register slot *index*.
    ///
    unsafe fn register(&self, index: usize) -> Value {
        *(self.0.sub(LBP_SELF as usize + 8 * index) as *const Value)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
#[repr(transparent)]
struct DFP(*mut DFP);

impl std::default::Default for DFP {
    fn default() -> Self {
        Self(std::ptr::null_mut())
    }
}

impl DFP {
    ///
    /// Get CFP of previous frame of *self*.
    ///
    unsafe fn outer(&self) -> Self {
        *self.0
    }

    fn is_null(&self) -> bool {
        self.0.is_null()
    }

    ///
    /// Get LFP.
    ///
    unsafe fn lfp(&self) -> LFP {
        LFP(self.0.add(LBP_OUTER as usize / 8) as _)
    }
}

#[derive(Debug, Clone)]
#[repr(C)]
pub(crate) struct BlockData {
    outer_lfp: LFP,
    func_data: *const FuncData,
}

///
/// Bytecode interpreter.
///
#[derive(Default)]
#[repr(C)]
pub struct Executor {
    cfp: CFP,     // [rbx]
    lfp_top: LFP, // [rbx + 8]
    lexical_class: Vec<ClassId>,
}

impl Executor {
    fn within_stack(&self, lfp: LFP) -> bool {
        self.lfp_top >= lfp && lfp.0 > self.cfp.0 as _
    }
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

        let res = globals.execute(self, main_data);
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

    fn eval_script(
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

    fn push_class_context(&mut self, class_id: ClassId) {
        self.lexical_class.push(class_id);
    }

    fn pop_class_context(&mut self) -> Option<ClassId> {
        self.lexical_class.pop()
    }

    fn get_class_context(&self) -> ClassId {
        self.lexical_class.last().cloned().unwrap_or(OBJECT_CLASS)
    }

    fn class_context_stack(&self) -> &[ClassId] {
        &self.lexical_class
    }
}

impl Executor {
    ///
    /// Find Constant in current class context.
    ///
    fn find_constant(&self, globals: &mut Globals, site_id: ConstSiteId) -> Option<Value> {
        let current = self.class_context_stack();
        globals.find_constant(site_id, current)
    }

    fn set_constant(&self, globals: &mut Globals, name: IdentId, val: Value) {
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
    fn invoke_method(
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
    /// Invoke block for *block_handler*.
    ///
    fn invoke_block(
        &mut self,
        globals: &mut Globals,
        block_handler: Value,
        args: &[Value],
    ) -> Option<Value> {
        let data = globals.get_block_data(block_handler, self);
        (globals.codegen.block_invoker)(
            self,
            globals,
            &data as _,
            Value::nil(),
            args.as_ptr(),
            args.len(),
        )
    }

    ///
    /// Invoke proc.
    ///
    fn invoke_proc(
        &mut self,
        globals: &mut Globals,
        block_data: &BlockData,
        args: &[Value],
    ) -> Option<Value> {
        (globals.codegen.block_invoker)(
            self,
            globals,
            block_data as _,
            Value::nil(),
            args.as_ptr(),
            args.len(),
        )
    }

    fn invoke_method2_if_exists(
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

///
/// Program counter base.
///
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
struct BcPcBase(*const Bc);

impl std::ops::Add<usize> for BcPcBase {
    type Output = BcPc;
    fn add(self, rhs: usize) -> BcPc {
        BcPc(unsafe { self.0.add(rhs) })
    }
}

impl std::ops::Add<InstId> for BcPcBase {
    type Output = BcPc;
    fn add(self, rhs: InstId) -> BcPc {
        BcPc(unsafe { self.0.offset(rhs.0 as isize) })
    }
}

impl BcPcBase {
    pub(super) fn new(func: &ISeqInfo) -> Self {
        BcPcBase(func.bytecode_top())
    }
}

///
/// Program counter
///
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
struct BcPc(*const Bc);

impl BcPc {
    pub(crate) fn from(bc: &Bc) -> Self {
        Self(bc as *const _)
    }

    pub(crate) fn from_u64(ptr: u64) -> Self {
        Self(ptr as *const _)
    }

    pub(crate) fn get_u64(self) -> u64 {
        self.0 as _
    }

    pub(crate) fn write2(self, data: u64) {
        unsafe { *((self.0 as *mut u64).add(1)) = data }
    }
}

impl std::ops::Sub<BcPcBase> for BcPc {
    type Output = usize;
    fn sub(self, rhs: BcPcBase) -> usize {
        let offset = unsafe { self.0.offset_from(rhs.0) };
        assert!(offset >= 0, "self:{:?} rhs:{:?}", self, rhs);
        offset as usize
    }
}

impl std::ops::Sub<BcPc> for BcPc {
    type Output = usize;
    fn sub(self, rhs: BcPc) -> usize {
        let offset = unsafe { self.0.offset_from(rhs.0) };
        assert!(offset >= 0, "self:{:?} rhs:{:?}", self, rhs);
        offset as usize
    }
}

impl std::ops::Add<isize> for BcPc {
    type Output = BcPc;
    fn add(self, rhs: isize) -> BcPc {
        BcPc(unsafe { self.0.offset(rhs) })
    }
}

impl std::ops::Sub<isize> for BcPc {
    type Output = BcPc;
    fn sub(self, rhs: isize) -> BcPc {
        BcPc(unsafe { self.0.offset(-rhs) })
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

impl std::ops::Deref for BcPc {
    type Target = Bc;
    fn deref(&self) -> &Self::Target {
        unsafe { &*self.0 }
    }
}

impl BcPc {
    fn get_ir(&self) -> TraceIr {
        TraceIr::from_bc(*self)
    }
}

impl BcPc {
    #[cfg(feature = "emit-bc")]
    pub fn format(&self, globals: &Globals, i: usize) -> Option<String> {
        fn optstr(opt: bool) -> &'static str {
            if opt {
                "_"
            } else {
                ""
            }
        }
        let s = match self.get_ir() {
            TraceIr::InitMethod {
                reg_num,
                arg_num,
                pos_num,
                req_num,
                block_pos,
                stack_offset,
            } => {
                format!(
                    "init_method reg:{reg_num} arg:{arg_num} pos:{pos_num} req:{req_num} block:{block_pos} stack_offset:{stack_offset}",
                )
            }
            TraceIr::InitBlock {
                reg_num,
                arg_num,
                pos_num,
                req_num,
                block_pos,
                stack_offset,
            } => {
                format!(
                    "init_block reg:{reg_num} arg:{arg_num} pos:{pos_num} req:{req_num} block:{block_pos} stack_offset:{stack_offset}",
                )
            }
            TraceIr::CheckLocal(local, disp) => {
                format!("check_local({:?}) =>:{:05}", local, i as i32 + 1 + disp)
            }
            TraceIr::Br(disp) => {
                format!("br =>:{:05}", i as i32 + 1 + disp)
            }
            TraceIr::CondBr(reg, disp, opt, kind) => {
                format!(
                    "cond{}br {}{:?} =>:{:05}",
                    kind.to_s(),
                    optstr(opt),
                    reg,
                    i as i32 + 1 + disp
                )
            }
            TraceIr::Integer(reg, num) => format!("{:?} = {}: i32", reg, num),
            TraceIr::Symbol(reg, id) => format!("{:?} = :{}", reg, IdentId::get_name(id)),
            TraceIr::Range {
                ret,
                start,
                end,
                exclude_end,
            } => format!(
                "{:?} = {:?} {} {:?}",
                ret,
                start,
                if exclude_end { "..." } else { ".." },
                end
            ),
            TraceIr::Literal(reg, val) => {
                format!("{:?} = literal[{}]", reg, globals.val_inspect(val))
            }
            TraceIr::Array(ret, src, len) => {
                format!("{:?} = array[{:?}; {}]", ret, src, len)
            }
            TraceIr::Index(ret, base, idx) => {
                let op1 = format!("{:?} = {:?}.[{:?}]", ret, base, idx);
                format!(
                    "{:36} [{}][{}]",
                    op1,
                    self.classid1().get_name(globals),
                    self.classid2().get_name(globals)
                )
            }
            TraceIr::IndexAssign(src, base, idx) => {
                format!("{:?}:.[{:?}:] = {:?}", base, idx, src,)
            }
            TraceIr::LoadConst(reg, id) => {
                let ConstSiteInfo {
                    name,
                    prefix,
                    toplevel,
                    ..
                } = globals.func[id].clone();
                let mut const_name = if toplevel { "::" } else { "" }.to_string();
                for c in prefix {
                    const_name += &format!("{}::", IdentId::get_name(c));
                }
                const_name += &IdentId::get_name(name);
                let op1 = format!("{:?} = const[{}]", reg, const_name);
                format!(
                    "{:36} [{}]",
                    op1,
                    match self.value() {
                        None => "<INVALID>".to_string(),
                        Some(val) => val.inspect(globals),
                    }
                )
            }
            TraceIr::StoreConst(reg, id) => {
                format!("const[{}] = {:?}", IdentId::get_name(id), reg)
            }
            TraceIr::BlockArgProxy(dst) => {
                format!("{:?} = block_arg", dst)
            }
            TraceIr::LoadDynVar(ret, src) => {
                format!("{:?} = {:?}", ret, src)
            }
            TraceIr::StoreDynVar(dst, src) => {
                format!("{:?} = {:?}", dst, src)
            }
            TraceIr::LoadIvar(reg, id, class_id, ivar_id) => {
                format!(
                    "{:?} = {}: {}[{:?}]",
                    reg,
                    IdentId::get_name(id),
                    class_id.get_name(globals),
                    ivar_id,
                )
            }
            TraceIr::StoreIvar(reg, id, class_id, ivar_id) => {
                format!(
                    "{}: {}[{:?}] = {:?}",
                    IdentId::get_name(id),
                    class_id.get_name(globals),
                    ivar_id,
                    reg
                )
            }
            TraceIr::LoadGvar { ret, name } => {
                format!("{:?} = ${}", ret, IdentId::get_name(name),)
            }
            TraceIr::StoreGvar { val, name } => {
                format!("${} = {:?}", IdentId::get_name(name), val)
            }
            TraceIr::Nil(reg) => format!("{:?} = nil", reg),
            TraceIr::Neg(dst, src) => {
                let op1 = format!("{:?} = neg {:?}", dst, src);
                format!("{:36} [{}]", op1, self.classid1().get_name(globals),)
            }
            TraceIr::BinOp {
                kind,
                ret,
                lhs,
                rhs,
            }
            | TraceIr::IntegerBinOp {
                kind,
                ret,
                lhs,
                rhs,
            }
            | TraceIr::FloatBinOp {
                kind,
                ret,
                lhs,
                rhs,
            } => {
                let op1 = format!("{:?} = {:?} {} {:?}", ret, lhs, kind, rhs);
                format!(
                    "{:36} [{}][{}]",
                    op1,
                    self.classid1().get_name(globals),
                    self.classid2().get_name(globals)
                )
            }
            TraceIr::BinOpRi {
                kind,
                ret,
                lhs,
                rhs,
            } => {
                let op1 = format!("{:?} = {:?} {} {}: i16", ret, lhs, kind, rhs,);
                format!(
                    "{:36} [{}][{}]",
                    op1,
                    self.classid1().get_name(globals),
                    self.classid2().get_name(globals)
                )
            }
            TraceIr::BinOpIr {
                kind,
                ret,
                lhs,
                rhs,
            } => {
                let op1 = format!("{:?} = {}: i16 {} {:?}", ret, lhs, kind, rhs,);
                format!(
                    "{:36} [{}][{}]",
                    op1,
                    self.classid1().get_name(globals),
                    self.classid2().get_name(globals)
                )
            }
            TraceIr::Cmp(kind, dst, lhs, rhs, opt) => {
                let op1 = format!("{}{:?} = {:?} {:?} {:?}", optstr(opt), dst, lhs, kind, rhs,);
                format!(
                    "{:36} [{}][{}]",
                    op1,
                    self.classid1().get_name(globals),
                    self.classid2().get_name(globals)
                )
            }
            TraceIr::Cmpri(kind, dst, lhs, rhs, opt) => {
                let op1 = format!(
                    "{}{:?} = {:?} {:?} {}: i16",
                    optstr(opt),
                    dst,
                    lhs,
                    kind,
                    rhs,
                );
                format!(
                    "{:36} [{}][{}]",
                    op1,
                    self.classid1().get_name(globals),
                    self.classid2().get_name(globals)
                )
            }

            TraceIr::Ret(reg) => format!("ret {:?}", reg),
            TraceIr::Mov(dst, src) => format!("{:?} = {:?}", dst, src),
            TraceIr::MethodCall {
                ret,
                name,
                _class,
                has_splat,
                ..
            } => {
                let args_pc = *self + 1;
                let (recv, args, len) = match args_pc.get_ir() {
                    TraceIr::MethodArgs(method_info) => {
                        let MethodInfo {
                            recv, args, len, ..
                        } = method_info;
                        (recv, args, len)
                    }
                    _ => unreachable!(),
                };
                let name = IdentId::get_name(name);
                let op1 = if len == 0 {
                    format!("{} = {:?}.call {}()", ret.ret_str(), recv, name,)
                } else {
                    format!(
                        "{} = {:?}.call {}({:?}; {}){}",
                        ret.ret_str(),
                        recv,
                        name,
                        args,
                        len,
                        if has_splat { "*" } else { "" }
                    )
                };
                format!("{:36} [{}]", op1, _class.get_name(globals))
            }
            TraceIr::Yield { ret, args, len } => {
                if len == 0 {
                    format!("{} = yield", ret.ret_str())
                } else {
                    format!("{} = yield({:?}; {})", ret.ret_str(), args, len)
                }
            }
            TraceIr::MethodCallBlock {
                ret,
                name,
                _class,
                has_splat,
                ..
            } => {
                let args_pc = *self + 1;
                let (recv, args, len) = match args_pc.get_ir() {
                    TraceIr::MethodArgs(method_info) => {
                        let MethodInfo {
                            recv, args, len, ..
                        } = method_info;
                        (recv, args, len)
                    }
                    _ => unreachable!(),
                };
                let name = IdentId::get_name(name);
                let op1 = if len == 0 {
                    format!("{} = {:?}.call {}(&{:?})", ret.ret_str(), recv, name, args)
                } else {
                    format!(
                        "{} = {:?}.call {}({:?}; {} &{:?}){}",
                        ret.ret_str(),
                        recv,
                        name,
                        args + 1,
                        len,
                        args,
                        if has_splat { "*" } else { "" }
                    )
                };
                format!("{:36} [{}]", op1, _class.get_name(globals))
            }
            TraceIr::MethodArgs(..) => return None,
            TraceIr::MethodDef(name, func_id) => {
                let name = IdentId::get_name(name);
                format!("method_def {:?}: {:?}", name, func_id)
            }
            TraceIr::ClassDef {
                ret,
                superclass,
                name,
                func_id,
            } => {
                let name = IdentId::get_name(name);
                format!(
                    "{} = class_def {:?} < {}: {:?}",
                    ret.ret_str(),
                    name,
                    superclass.ret_str(),
                    func_id
                )
            }
            TraceIr::ConcatStr(ret, args, len) => {
                format!("{} = concat({:?}; {})", ret.ret_str(), args, len)
            }
            TraceIr::ExpandArray(src, dst, len) => {
                format!("{:?}; {} = expand({:?})", dst, len, src)
            }
            TraceIr::Splat(src) => {
                format!("splat({:?})", src)
            }
            TraceIr::AliasMethod { new, old } => {
                format!("alias_method({:?}<-{:?})", new, old)
            }
            TraceIr::LoopStart(count) => format!(
                "loop_start counter={} jit-addr={:016x}",
                count,
                self.into_jit_addr()
            ),
            TraceIr::LoopEnd => "loop_end".to_string(),
        };
        Some(s)
    }
}

#[derive(Clone, Copy, PartialEq, PartialOrd)]
struct SlotId(u16);

impl SlotId {
    fn new(reg: u16) -> Self {
        Self(reg)
    }

    fn self_() -> Self {
        Self(0)
    }

    fn is_zero(&self) -> bool {
        self.0 == 0
    }

    #[cfg(feature = "emit-bc")]
    fn ret_str(&self) -> String {
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
/// kinds of binary operation.
///
#[derive(Debug, Clone, Copy, PartialEq)]
enum BinOpK {
    Add = 0,
    Sub = 1,
    Mul = 2,
    Div = 3,
    BitOr = 4,
    BitAnd = 5,
    BitXor = 6,
    Shr = 7,
    Shl = 8,
    Rem = 9,
    Exp = 10,
}

use std::fmt;
impl fmt::Display for BinOpK {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match *self {
            BinOpK::Add => "+",
            BinOpK::Sub => "-",
            BinOpK::Mul => "*",
            BinOpK::Div => "/",
            BinOpK::BitOr => "|",
            BinOpK::BitAnd => "&",
            BinOpK::BitXor => "^",
            BinOpK::Shr => ">>",
            BinOpK::Shl => "<<",
            BinOpK::Rem => "%",
            BinOpK::Exp => "**",
        };
        write!(f, "{}", s)
    }
}

impl BinOpK {
    fn from(i: u16) -> Self {
        match i {
            0 => BinOpK::Add,
            1 => BinOpK::Sub,
            2 => BinOpK::Mul,
            3 => BinOpK::Div,
            4 => BinOpK::BitOr,
            5 => BinOpK::BitAnd,
            6 => BinOpK::BitXor,
            7 => BinOpK::Shr,
            8 => BinOpK::Shl,
            9 => BinOpK::Rem,
            10 => BinOpK::Exp,
            _ => unreachable!(),
        }
    }

    fn generic_func(
        &self,
    ) -> extern "C" fn(&mut Executor, &mut Globals, Value, Value) -> Option<Value> {
        match self {
            BinOpK::Add => add_values,
            BinOpK::Sub => sub_values,
            BinOpK::Mul => mul_values,
            BinOpK::Div => div_values,
            BinOpK::BitOr => bitor_values,
            BinOpK::BitAnd => bitand_values,
            BinOpK::BitXor => bitxor_values,
            BinOpK::Shr => shr_values,
            BinOpK::Shl => shl_values,
            BinOpK::Rem => rem_values,
            BinOpK::Exp => pow_values,
        }
    }
}

#[derive(Clone)]
struct DynVar {
    reg: SlotId,
    outer: usize,
}

impl std::fmt::Debug for DynVar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "dynvar({}, {:?})", self.outer, self.reg)
    }
}

///
/// ID of instruction.
///
#[derive(Clone, Copy, PartialEq, Eq, Hash, Default)]
struct InstId(pub u32);

impl std::fmt::Debug for InstId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, ":{:05}", self.0)
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
struct Meta {
    func_id: FuncId,
    reg_num: u16,
    /// interpreter:0 native:2
    kind: u8,
    /// method:0 class_def:1
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
    fn new(meta: u64) -> Self {
        unsafe { std::mem::transmute(meta) }
    }

    fn get(&self) -> u64 {
        unsafe { std::mem::transmute(*self) }
    }

    fn vm_method(func_id: FuncId, reg_num: i64) -> Self {
        // kind = VM, mode = method
        let reg_num = reg_num as i16 as u16;
        Self {
            func_id,
            reg_num,
            kind: 0,
            mode: 0,
        }
    }

    fn vm_classdef(func_id: FuncId, reg_num: i64) -> Self {
        // kind = VM, mode = classdef
        let reg_num = reg_num as i16 as u16;
        Self {
            func_id,
            reg_num,
            kind: 0,
            mode: 1,
        }
    }

    fn native(func_id: FuncId, reg_num: i64) -> Self {
        // kind = NATIVE, mode = method
        let reg_num = reg_num as i16 as u16;
        Self {
            func_id,
            reg_num,
            kind: 2,
            mode: 0,
        }
    }

    fn func_id(&self) -> FuncId {
        self.func_id
    }

    fn reg_num(&self) -> i64 {
        self.reg_num as i16 as i64
    }

    /// interpreter:0 JIT code: 1 native:2
    fn kind(&self) -> u8 {
        self.kind
    }

    /// method:0 class_def:1
    fn mode(&self) -> u8 {
        self.mode
    }

    ///
    /// Set JIT flag in Meta.
    ///
    fn set_jit(&mut self) {
        self.kind = 1;
    }

    ///
    /// Set the number of registers in Meta.
    ///
    fn set_reg_num(&mut self, reg_num: i64) {
        self.reg_num = reg_num as i16 as u16;
    }
}

#[derive(Debug, Clone, PartialEq, Default)]
#[repr(C)]
struct FuncData {
    /// address of function.
    codeptr: Option<CodePtr>,
    /// metadata of this function.
    meta: Meta,
    /// the address of program counter
    pc: BcPc,
}

impl FuncData {
    fn set_reg_num(&mut self, reg_num: i64) {
        self.meta.set_reg_num(reg_num);
    }
}

///
/// Compile the Ruby method.
///
extern "C" fn exec_jit_compile(
    globals: &mut Globals,
    func_id: FuncId,
    self_value: Value,
) -> CodePtr {
    globals.func[func_id].data.meta.set_jit();
    let label = globals.jit_compile_ruby(func_id, self_value, None);
    globals.codegen.jit.get_label_address(label)
}

extern "C" fn exec_jit_recompile(
    globals: &mut Globals,
    func_id: FuncId,
    self_value: Value,
) -> CodePtr {
    let codeptr = exec_jit_compile(globals, func_id, self_value);
    let target = globals.func[func_id].data.codeptr.unwrap();
    let offset = codeptr - target - 5;
    unsafe { *(target.as_ptr().add(1) as *mut i32) = offset as i32 };
    codeptr
}

///
/// Compile the loop.
///
extern "C" fn exec_jit_partial_compile(
    globals: &mut Globals,
    func_id: FuncId,
    self_value: Value,
    pc: BcPc,
) {
    let label = globals.jit_compile_ruby(func_id, self_value, Some(pc));
    let codeptr = globals.codegen.jit.get_label_address(label);
    pc.write2(codeptr.as_ptr() as u64);
}

#[cfg(test)]
mod test {
    use super::*;

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
}
