use super::*;

mod builtins;
mod bytecodegen;
mod compiler;
mod globals;
mod inst;
mod op;
pub use builtins::*;
use bytecodegen::*;
use fancy_regex::Captures;
pub use globals::*;
use inst::*;
use op::*;

type Result<T> = std::result::Result<T, MonorubyErr>;
pub type BuiltinFn = extern "C" fn(
    &mut Executor,
    &mut Globals,
    Value,
    Arg,
    usize,
    Option<BlockHandler>,
) -> Option<Value>;

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
    fn lfp(&self) -> LFP {
        unsafe {
            let bp = self.bp();
            LFP(*bp.sub(BP_LFP as usize / 8) as _)
        }
    }

    ///
    /// Set LFP.
    ///
    unsafe fn set_lfp(&mut self, lfp: LFP) {
        let bp = self.bp() as *mut usize;
        *bp.sub(BP_LFP as usize / 8) = lfp.0 as _;
    }

    ///
    /// Get func_id of a current method / classdef.
    ///
    fn method_func_id(&self) -> FuncId {
        unsafe {
            let mut lfp = self.lfp();
            loop {
                if lfp.outer().is_null() {
                    break;
                }
                lfp = lfp.outer().lfp();
            }
            lfp.meta().func_id()
        }
    }

    ///
    /// Get *BlockHandler* of a current method / classdef.
    ///
    fn get_block(&self) -> Option<BlockHandler> {
        unsafe {
            let mut lfp = self.lfp();
            loop {
                if lfp.outer().is_null() {
                    break;
                }
                lfp = lfp.outer().lfp();
            }

            lfp.block().map(|bh| match bh.0.try_fixnum() {
                Some(mut i) => {
                    let mut cfp = *self;
                    loop {
                        if cfp.lfp() == lfp {
                            break;
                        }
                        if cfp.prev().is_null() {
                            unreachable!()
                        }
                        i += 1;
                        cfp = cfp.prev();
                    }
                    BlockHandler::new(Value::new_integer(i))
                }
                None => bh,
            })
        }
    }

    ///
    /// Get func_id of a current source position.
    ///
    fn get_source_pos(&self) -> FuncId {
        let mut cfp = *self;
        unsafe {
            loop {
                if !cfp.lfp().meta().is_native() {
                    return cfp.lfp().meta().func_id();
                }
                let prev_cfp = cfp.prev();
                if prev_cfp.is_null() {
                    unreachable!("get_source_pos: non-native method not found.");
                };
                cfp = prev_cfp;
            }
        }
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
        Meta::from(*(self.0.sub(LBP_META as usize) as *const u64))
    }

    ///
    /// Get block.
    ///
    fn block(&self) -> Option<BlockHandler> {
        unsafe { *(self.0.sub(LBP_BLOCK as usize) as *const _) }
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

impl std::default::Default for BlockData {
    fn default() -> Self {
        Self {
            outer_lfp: LFP::default(),
            func_data: std::ptr::null(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
#[repr(transparent)]
pub struct BlockHandler(Value);

impl BlockHandler {
    pub fn new(val: Value) -> Self {
        Self(val)
    }

    pub fn try_proxy(&self) -> Option<(FuncId, u16)> {
        self.0.try_fixnum().map(|i| {
            let i = i as u64;
            let func_id = FuncId::new(u32::try_from(i >> 16).unwrap());
            let idx = i as u16;
            (func_id, idx)
        })
    }

    pub(crate) fn as_proc(&self) -> &BlockData {
        self.0.as_proc()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum OpMode {
    RR(SlotId, SlotId),
    RI(SlotId, i16),
    IR(i16, SlotId),
}

impl OpMode {
    fn is_float_op(&self, pc: &Bc) -> bool {
        match self {
            Self::RR(..) => pc.is_float_binop(),
            Self::RI(..) => pc.is_float1(),
            Self::IR(..) => pc.is_float2(),
        }
    }

    fn is_integer_op(&self, pc: &Bc) -> bool {
        match self {
            Self::RR(..) => pc.is_integer_binop(),
            Self::RI(..) => pc.is_integer1(),
            Self::IR(..) => pc.is_integer2(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Cref {
    class_id: ClassId,
    module_function: bool,
    visibility: Visibility,
}

impl Cref {
    fn new(class_id: ClassId, module_function: bool, visibility: Visibility) -> Self {
        Self {
            class_id,
            module_function,
            visibility,
        }
    }
}

///
/// Bytecode interpreter.
///
#[derive(Default)]
#[repr(C)]
pub struct Executor {
    cfp: CFP,     // [rbx]
    lfp_top: LFP, // [rbx + 8]
    lexical_class: Vec<Cref>,
    sp_last_match: Option<Value>,   // $&        : Regexp.last_match(0)
    sp_post_match: Option<Value>,   // $'        : Regexp.post_match
    sp_matches: Vec<Option<Value>>, // $1 ... $n : Regexp.last_match(n)
}

impl alloc::GC<RValue> for Executor {
    fn mark(&self, alloc: &mut alloc::Allocator<RValue>) {
        self.sp_matches.iter().for_each(|v| {
            if let Some(v) = v {
                v.mark(alloc)
            }
        });
        if let Some(v) = self.sp_last_match {
            v.mark(alloc)
        };
        if let Some(v) = self.sp_post_match {
            v.mark(alloc)
        };
        let mut cfp = self.cfp;
        unsafe {
            loop {
                let lfp = cfp.lfp();
                let meta = lfp.meta();
                for r in 0..meta.reg_num() as usize {
                    let v = lfp.register(r);
                    v.mark(alloc);
                }
                if let Some(v) = lfp.block() {
                    v.0.mark(alloc)
                };

                cfp = cfp.prev();
                if cfp.is_null() {
                    break;
                }
            }
        }
    }
}

impl Executor {
    pub fn init(globals: &mut Globals) -> Self {
        let mut executor = Self::default();
        let path = std::path::Path::new("startup/startup.rb");
        let code = include_str!("../startup/startup.rb").to_string();
        if executor.eval_script(globals, code, path).is_none() {
            let err = globals.take_error().unwrap();
            err.show_error_message_and_all_loc();
            panic!("error occurred in startup.");
        };
        executor
    }

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

        let res = (globals.codegen.entry_point)(self, globals, main_data);
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
                let name = globals[*func_id].as_ruby_func().name();
                eprintln!(
                    "{:20}  {:5} [{:05}]  {:10}",
                    name,
                    func_id.get(),
                    index,
                    count
                );
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

    pub(super) fn get_block_data(
        &self,
        globals: &mut Globals,
        block_handler: BlockHandler,
    ) -> BlockData {
        if let Some((func_id, idx)) = block_handler.try_proxy() {
            let mut cfp = self.cfp;
            unsafe {
                for _ in 0..idx {
                    cfp = cfp.prev();
                }
                let func_data = globals.compile_on_demand(func_id) as _;
                BlockData {
                    outer_lfp: cfp.lfp(),
                    func_data,
                }
            }
        } else {
            block_handler.as_proc().clone()
        }
    }

    fn eval_script(
        &mut self,
        globals: &mut Globals,
        code: String,
        path: &std::path::Path,
    ) -> Option<Value> {
        match globals
            .compile_script(code, path)
            .map(|fid| self.eval(globals, fid))
            .flatten()
        {
            Ok(v) => Some(v),
            Err(err) => {
                globals.set_error(err);
                None
            }
        }
    }

    fn push_class_context(&mut self, class_id: ClassId) {
        self.lexical_class
            .push(Cref::new(class_id, false, Visibility::Public));
    }

    fn pop_class_context(&mut self) -> Option<ClassId> {
        self.lexical_class.pop().map(|x| x.class_id)
    }

    fn set_module_function(&mut self) {
        self.lexical_class.last_mut().unwrap().module_function = true;
    }

    fn get_class_context(&self) -> Cref {
        self.lexical_class
            .last()
            .cloned()
            .unwrap_or_else(|| Cref::new(OBJECT_CLASS, false, Visibility::Private))
    }

    fn context_class_id(&self) -> ClassId {
        self.lexical_class
            .last()
            .map(|cref| cref.class_id)
            .unwrap_or(OBJECT_CLASS)
    }

    fn context_visibility(&self) -> Visibility {
        self.lexical_class
            .last()
            .map(|cref| cref.visibility)
            .unwrap_or(Visibility::Private)
    }

    fn set_context_visibility(&mut self, visi: Visibility) {
        self.lexical_class.last_mut().unwrap().visibility = visi;
    }
}

impl Executor {
    ///
    /// Find Constant in current class context.
    ///
    fn find_constant(&self, globals: &mut Globals, site_id: ConstSiteId) -> Option<Value> {
        let current_func = self.cfp.method_func_id();
        globals.find_constant(site_id, current_func)
    }

    fn set_constant(&self, globals: &mut Globals, name: IdentId, val: Value) {
        let parent = self.context_class_id();
        globals.set_constant(parent, name, val);
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
        let func_id = globals.find_method(receiver, method, false)?;
        globals.check_arg(func_id, len)?;
        let data = globals.compile_on_demand(func_id) as *const _;
        (globals.codegen.method_invoker)(self, globals, data, receiver, args.as_ptr(), args.len())
    }

    ///
    /// Invoke block for *block_handler*.
    ///
    pub(crate) fn invoke_block(
        &mut self,
        globals: &mut Globals,
        data: BlockData,
        args: &[Value],
    ) -> Option<Value> {
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
        if let Some(func_id) = globals.check_method(receiver, method) {
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
        globals.check_arg(func_id, len)?;
        let data = globals.compile_on_demand(func_id) as *const _;
        (globals.codegen.method_invoker2)(self, globals, data, receiver, args, len)
    }
}

// Handling special variables.

impl Executor {
    /// Save captured strings to special variables.
    /// $n (n:0,1,2,3...) <- The string which matched with nth parenthesis in the last successful match.
    /// $& <- The string which matched successfully at last.
    /// $' <- The string after $&.
    pub(crate) fn get_captures(&mut self, captures: &Captures, given: &str) {
        //let id1 = IdentId::get_id("$&");
        //let id2 = IdentId::get_id("$'");
        match captures.get(0) {
            Some(m) => {
                self.sp_last_match = Some(Value::new_string_from_str(&given[m.start()..m.end()]));
                self.sp_post_match = Some(Value::new_string_from_str(&given[m.end()..]));
            }
            None => {
                self.sp_last_match = None;
                self.sp_post_match = None;
            }
        };

        self.sp_matches.clear();
        for i in 0..captures.len() {
            self.sp_matches.push(
                captures
                    .get(i)
                    .map(|m| Value::new_string_from_str(&given[m.start()..m.end()])),
            );
        }
    }

    pub(crate) fn get_special_matches(&self, mut nth: i64) -> Value {
        if nth < 0 {
            nth += self.sp_matches.len() as i64
        }
        if nth >= 0 {
            self.sp_matches
                .get(nth as usize)
                .cloned()
                .unwrap_or_default()
                .unwrap_or_default()
        } else {
            Value::nil()
        }
    }

    pub(crate) fn get_last_matchdata(&self) -> Value {
        Value::new_array_from_vec(
            self.sp_matches
                .iter()
                .map(|v| v.unwrap_or_default())
                .collect(),
        )
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
    fn get_ir(&self, fnstore: &FnStore) -> TraceIr {
        TraceIr::from_bc(*self, fnstore)
    }
}

impl BcPc {
    #[cfg(any(feature = "emit-bc", feature = "emit-asm", feature = "log-jit"))]
    pub fn format(&self, globals: &Globals, i: usize) -> Option<String> {
        fn optstr(opt: bool) -> &'static str {
            if opt {
                "_"
            } else {
                ""
            }
        }
        let s = match self.get_ir(&globals.func) {
            TraceIr::InitMethod(info) => {
                format!("init_method {info:?}")
            }
            TraceIr::InitBlock(info) => {
                format!("init_block {info:?}")
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
            TraceIr::Array { ret, args, len } => {
                format!("{:?} = array[{:?}; {}]", ret, args, len)
            }
            TraceIr::Hash { ret, args, len } => {
                format!("{:?} = hash[{:?}; {}]", ret, args, len)
            }
            TraceIr::Index { ret, base, idx } => {
                let op1 = format!("{:?} = {:?}.[{:?}]", ret, base, idx);
                format!(
                    "{:36} [{}][{}]",
                    op1,
                    self.classid1().get_name(globals),
                    self.classid2().get_name(globals)
                )
            }
            TraceIr::IndexAssign { src, base, idx } => {
                format!("{:?}:.[{:?}:] = {:?}", base, idx, src,)
            }
            TraceIr::LoadConst(reg, id) => {
                let ConstSiteInfo {
                    name,
                    prefix,
                    toplevel,
                    ..
                } = &globals.func[id];
                let mut const_name = if *toplevel { "::" } else { "" }.to_string();
                for c in prefix {
                    const_name += &format!("{}::", IdentId::get_name(*c));
                }
                const_name += &IdentId::get_name(*name);
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
            TraceIr::BlockArgProxy(dst, outer) => {
                format!("{:?} = block_arg({outer})", dst)
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
            TraceIr::LoadGvar { dst: ret, name } => {
                format!("{:?} = ${}", ret, IdentId::get_name(name),)
            }
            TraceIr::StoreGvar { src, name } => {
                format!("${} = {:?}", IdentId::get_name(name), src)
            }
            TraceIr::LoadSvar { dst: ret, id } => {
                // 0 => $&
                // 1 => $'
                // 100 + n => $n
                format!(
                    "{:?} = ${}",
                    ret,
                    match id {
                        0 => "&".to_string(),
                        1 => "'".to_string(),
                        n => (n - 100).to_string(),
                    }
                )
            }
            TraceIr::Nil(reg) => format!("{:?} = nil", reg),
            TraceIr::Neg(dst, src) => {
                let op1 = format!("{:?} = neg {:?}", dst, src);
                format!("{:36} [{}]", op1, self.classid1().get_name(globals),)
            }
            TraceIr::BinOp {
                kind,
                ret,
                mode: OpMode::RR(lhs, rhs),
            }
            | TraceIr::IntegerBinOp {
                kind,
                ret,
                mode: OpMode::RR(lhs, rhs),
            }
            | TraceIr::FloatBinOp {
                kind,
                ret,
                mode: OpMode::RR(lhs, rhs),
            } => {
                let op1 = format!("{:?} = {:?} {} {:?}", ret, lhs, kind, rhs);
                format!(
                    "{:36} [{}][{}]",
                    op1,
                    self.classid1().get_name(globals),
                    self.classid2().get_name(globals)
                )
            }
            TraceIr::BinOp {
                kind,
                ret,
                mode: OpMode::RI(lhs, rhs),
            }
            | TraceIr::IntegerBinOp {
                kind,
                ret,
                mode: OpMode::RI(lhs, rhs),
            }
            | TraceIr::FloatBinOp {
                kind,
                ret,
                mode: OpMode::RI(lhs, rhs),
            } => {
                let op1 = format!("{:?} = {:?} {} {}: i16", ret, lhs, kind, rhs,);
                format!(
                    "{:36} [{}][{}]",
                    op1,
                    self.classid1().get_name(globals),
                    self.classid2().get_name(globals)
                )
            }
            TraceIr::BinOp {
                kind,
                ret,
                mode: OpMode::IR(lhs, rhs),
            }
            | TraceIr::IntegerBinOp {
                kind,
                ret,
                mode: OpMode::IR(lhs, rhs),
            }
            | TraceIr::FloatBinOp {
                kind,
                ret,
                mode: OpMode::IR(lhs, rhs),
            } => {
                let op1 = format!("{:?} = {}: i16 {} {:?}", ret, lhs, kind, rhs,);
                format!(
                    "{:36} [{}][{}]",
                    op1,
                    self.classid1().get_name(globals),
                    self.classid2().get_name(globals)
                )
            }
            TraceIr::Cmp(kind, dst, mode, opt) => {
                let op1 = match mode {
                    OpMode::RR(lhs, rhs) => {
                        format!("{}{:?} = {:?} {:?} {:?}", optstr(opt), dst, lhs, kind, rhs,)
                    }
                    OpMode::RI(lhs, rhs) => format!(
                        "{}{:?} = {:?} {:?} {}: i16",
                        optstr(opt),
                        dst,
                        lhs,
                        kind,
                        rhs,
                    ),
                    _ => unreachable!(),
                };
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
                info,
                ..
            } => {
                let MethodInfo {
                    recv, args, len, ..
                } = info;
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
            TraceIr::InlineCall {
                ret,
                method,
                _class,
                info,
                ..
            } => {
                let MethodInfo {
                    recv, args, len, ..
                } = info;
                let op1 = if len == 0 {
                    format!("{} = {:?}.inline {:?}()", ret.ret_str(), recv, method,)
                } else {
                    format!(
                        "{} = {:?}.inline {:?}({:?}; {})",
                        ret.ret_str(),
                        recv,
                        method,
                        args,
                        len,
                    )
                };
                format!("{:36} [{}]", op1, _class.get_name(globals))
            }
            TraceIr::MethodCallBlock {
                ret,
                name,
                _class,
                has_splat,
                info,
                ..
            } => {
                let MethodInfo {
                    recv, args, len, ..
                } = info;
                let name = IdentId::get_name(name);
                let op1 = if len == 0 {
                    format!(
                        "{} = {:?}.call {}(&{:?} kw:{:?})",
                        ret.ret_str(),
                        recv,
                        name,
                        args,
                        args + 1
                    )
                } else {
                    format!(
                        "{} = {:?}.call {}({:?}; {} &{:?} kw:{:?}){}",
                        ret.ret_str(),
                        recv,
                        name,
                        args + 2,
                        len,
                        args,
                        args + 1,
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
            TraceIr::MethodArgs(..) => return None,
            TraceIr::MethodDef { name, func_id } => {
                let name = IdentId::get_name(name);
                format!("method_def {:?}: {:?}", name, func_id)
            }
            TraceIr::SingletonMethodDef { obj, name, func_id } => {
                let name = IdentId::get_name(name);
                format!(
                    "singleton_method_def {}.{:?}: {:?}",
                    obj.ret_str(),
                    name,
                    func_id
                )
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
            TraceIr::ModuleDef { ret, name, func_id } => {
                let name = IdentId::get_name(name);
                format!("{} = module_def {:?}: {:?}", ret.ret_str(), name, func_id)
            }
            TraceIr::SingletonClassDef { ret, base, func_id } => {
                format!(
                    "{} = singleton_class_def << {:?}: {:?}",
                    ret.ret_str(),
                    base,
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
pub(crate) struct SlotId(u16);

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

    #[cfg(any(feature = "emit-bc", feature = "emit-asm", feature = "log-jit"))]
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
    func_id: Option<FuncId>,
    reg_num: u16,
    /// interpreter:0 native:2
    kind: u8,
    /// bit 2-1: public:0 private:1 protected:2
    /// bit 0: method:0 class_def:1
    mode: u8,
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum Visibility {
    Public = 0,
    Protected = 1,
    Private = 2,
}

impl Visibility {
    pub fn is_public(&self) -> bool {
        self == &Self::Public
    }

    pub fn is_private(&self) -> bool {
        self == &Self::Private
    }
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
            if self.is_class_def() {
                "class_def"
            } else {
                "method"
            },
            self.func_id(),
            self.reg_num()
        )
    }
}

impl Meta {
    fn new(func_id: Option<FuncId>, reg_num: u16, kind: u8, is_class_def: bool) -> Self {
        Self {
            func_id,
            reg_num,
            kind,
            mode: is_class_def as u8,
        }
    }

    fn from(meta: u64) -> Self {
        unsafe { std::mem::transmute(meta) }
    }

    fn get(&self) -> u64 {
        unsafe { std::mem::transmute(*self) }
    }

    fn vm_method(func_id: impl Into<Option<FuncId>>, reg_num: i64) -> Self {
        // kind = VM, mode = method
        let reg_num = reg_num as i16 as u16;
        Self::new(func_id.into(), reg_num, 0, false)
    }

    fn vm_classdef(func_id: impl Into<Option<FuncId>>, reg_num: i64) -> Self {
        // kind = VM, mode = classdef
        let reg_num = reg_num as i16 as u16;
        Self::new(func_id.into(), reg_num, 0, true)
    }

    fn native(func_id: FuncId, reg_num: i64) -> Self {
        // kind = NATIVE, mode = method
        let reg_num = reg_num as i16 as u16;
        Self::new(Some(func_id), reg_num, 2, false)
    }

    fn func_id(&self) -> FuncId {
        self.func_id.unwrap()
    }

    fn reg_num(&self) -> i64 {
        self.reg_num as i16 as i64
    }

    /// interpreter:0 JIT code: 1 native:2
    fn kind(&self) -> u8 {
        self.kind
    }

    fn is_native(&self) -> bool {
        self.kind == 2
    }

    /// method:0 class_def:1
    fn is_class_def(&self) -> bool {
        (self.mode & 0b1) == 1
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
    codeptr: Option<monoasm::CodePtr>,
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
) -> monoasm::CodePtr {
    globals[func_id].data.meta.set_jit();
    let label = globals.jit_compile_ruby(func_id, self_value, None);
    globals.codegen.jit.get_label_address(label)
}

extern "C" fn exec_jit_recompile(
    globals: &mut Globals,
    func_id: FuncId,
    self_value: Value,
) -> monoasm::CodePtr {
    let codeptr = exec_jit_compile(globals, func_id, self_value);
    let target = globals[func_id].data.codeptr.unwrap();
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
        let meta = Meta::vm_method(FuncId::new(12), 42);
        assert_eq!(FuncId::new(12), meta.func_id());
        assert_eq!(42, meta.reg_num());
        assert_eq!(false, meta.is_class_def());
        let mut meta = Meta::vm_method(FuncId::new(42), -1);
        assert_eq!(FuncId::new(42), meta.func_id());
        assert_eq!(-1, meta.reg_num());
        meta.set_reg_num(12);
        assert_eq!(FuncId::new(42), meta.func_id());
        assert_eq!(12, meta.reg_num());
        let mut meta = Meta::vm_classdef(FuncId::new(12), 42);
        assert_eq!(true, meta.is_class_def());
        meta.set_reg_num(12);
        meta.set_jit();
        assert_eq!(true, meta.is_class_def());
        assert_eq!(8, std::mem::size_of::<i64>());
        assert_eq!(8, std::mem::size_of::<Option<monoasm::CodePtr>>());
        assert_eq!(8, std::mem::size_of::<BcPc>());
        assert_eq!(8, std::mem::size_of::<Meta>());
    }
}
