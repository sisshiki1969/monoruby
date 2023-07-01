use super::*;
use monoasm::*;
use ruruby_parse::{Loc, Node};

mod builtins;
pub mod bytecodegen;
mod compiler;
mod error;
mod frame;
mod globals;
mod inline;
mod op;
pub use builtins::*;
pub use bytecodegen::*;
use fancy_regex::Captures;
pub use frame::*;
pub use globals::*;
use op::*;

pub type Result<T> = std::result::Result<T, MonorubyErr>;
pub type BuiltinFn = extern "C" fn(&mut Executor, &mut Globals, LFP, Arg, usize) -> Option<Value>;
pub type BinaryOpFn = extern "C" fn(&mut Executor, &mut Globals, Value, Value) -> Option<Value>;

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

#[derive(Debug, Clone)]
#[repr(C)]
pub struct BlockData {
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

impl alloc::GC<RValue> for BlockData {
    fn mark(&self, alloc: &mut alloc::Allocator<RValue>) {
        self.outer_lfp.mark(alloc)
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

    pub fn try_proc(&self) -> bool {
        self.0.is_proc().is_some()
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
#[repr(C)]
pub struct Executor {
    cfp: Option<CFP>, // [rbx]
    lfp_top: LFP,     // [rbx + 8]
    lexical_class: Vec<Vec<Cref>>,
    sp_last_match: Option<Value>,   // $&        : Regexp.last_match(0)
    sp_post_match: Option<Value>,   // $'        : Regexp.post_match
    sp_matches: Vec<Option<Value>>, // $1 ... $n : Regexp.last_match(n)
    temp_stack: Vec<Value>,
    /// error information.
    exception: Option<MonorubyErr>,
}

impl std::default::Default for Executor {
    fn default() -> Self {
        Self {
            cfp: None,
            lfp_top: LFP::default(),
            lexical_class: vec![vec![]],
            sp_last_match: None,
            sp_post_match: None,
            sp_matches: vec![],
            temp_stack: vec![],
            exception: None,
        }
    }
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
        self.temp_stack.iter().for_each(|v| v.mark(alloc));
        let mut cfp = self.cfp;
        while let Some(inner_cfp) = cfp {
            inner_cfp.lfp().mark(alloc);
            cfp = inner_cfp.prev();
        }
    }
}

impl Executor {
    pub fn init(globals: &mut Globals) -> Self {
        let mut executor = Self::default();
        let path = std::path::Path::new("startup/startup.rb");
        let code = include_str!("../startup/startup.rb").to_string();
        if let Err(err) = executor.eval_script(globals, code, path) {
            err.show_error_message_and_all_loc();
            panic!("error occurred in startup.");
        };
        #[cfg(feature = "emit-bc")]
        {
            globals.startup_flag = true;
        }
        executor
    }

    fn cfp(&self) -> CFP {
        self.cfp.unwrap()
    }

    fn method_func_id(&self) -> FuncId {
        self.cfp().method_func_id()
    }

    fn within_stack(&self, lfp: LFP) -> bool {
        self.lfp_top >= lfp && lfp > self.cfp.unwrap()
    }

    fn temp_len(&self) -> usize {
        self.temp_stack.len()
    }

    fn temp_push(&mut self, val: Value) {
        self.temp_stack.push(val);
    }

    fn temp_append(&mut self, mut v: Vec<Value>) -> usize {
        let len = self.temp_stack.len();
        self.temp_stack.append(&mut v);
        len
    }

    fn temp_clear(&mut self, len: usize) {
        self.temp_stack.truncate(len);
    }

    fn temp_tear(&mut self, len: usize) -> Vec<Value> {
        self.temp_stack.drain(len..).collect()
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
        res.ok_or_else(|| self.take_error())
    }

    pub(super) fn get_block_data(
        &self,
        globals: &mut Globals,
        block_handler: BlockHandler,
    ) -> BlockData {
        if let Some((func_id, idx)) = block_handler.try_proxy() {
            let mut cfp = self.cfp();
            for _ in 0..idx {
                cfp = cfp.prev().unwrap();
            }
            let func_data = globals.compile_on_demand(func_id) as _;
            BlockData {
                outer_lfp: cfp.lfp(),
                func_data,
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
    ) -> Result<Value> {
        globals
            .compile_script(code, path)
            .and_then(|fid| self.eval(globals, fid))
    }

    fn enter_class_context(&mut self) {
        self.lexical_class.push(vec![]);
    }

    fn exit_class_context(&mut self) {
        self.lexical_class.pop();
    }

    fn push_class_context(&mut self, class_id: ClassId) {
        self.lexical_class
            .last_mut()
            .unwrap()
            .push(Cref::new(class_id, false, Visibility::Public));
    }

    fn pop_class_context(&mut self) -> Option<ClassId> {
        self.lexical_class
            .last_mut()
            .unwrap()
            .pop()
            .map(|x| x.class_id)
    }

    fn set_module_function(&mut self) {
        self.lexical_class
            .last_mut()
            .unwrap()
            .last_mut()
            .unwrap()
            .module_function = true;
    }

    fn get_class_context(&self) -> Cref {
        self.lexical_class
            .last()
            .unwrap()
            .last()
            .cloned()
            .unwrap_or_else(|| Cref::new(OBJECT_CLASS, false, Visibility::Private))
    }

    fn context_class_id(&self) -> ClassId {
        self.lexical_class
            .last()
            .unwrap()
            .last()
            .map(|cref| cref.class_id)
            .unwrap_or(OBJECT_CLASS)
    }

    fn context_visibility(&self) -> Visibility {
        self.lexical_class
            .last()
            .unwrap()
            .last()
            .map(|cref| cref.visibility)
            .unwrap_or(Visibility::Private)
    }

    fn set_context_visibility(&mut self, visi: Visibility) {
        self.lexical_class
            .last_mut()
            .unwrap()
            .last_mut()
            .unwrap()
            .visibility = visi;
    }
}

impl Executor {
    ///
    /// Find Constant in current class context.
    ///
    fn find_constant(&self, globals: &mut Globals, site_id: ConstSiteId) -> Result<Value> {
        let current_func = self.method_func_id();
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
        let func_id = match globals.find_method(receiver, method, false) {
            Ok(id) => id,
            Err(err) => {
                self.set_error(err);
                return None;
            }
        };
        let data = globals.compile_on_demand(func_id) as *const _;
        (globals.codegen.method_invoker)(self, globals, data, receiver, args.as_ptr(), args.len())
    }

    ///
    /// Invoke method for *receiver* and *method*.
    ///
    fn invoke_method_inner(
        &mut self,
        globals: &mut Globals,
        method: IdentId,
        receiver: Value,
        args: &[Value],
    ) -> Result<Value> {
        let func_id = globals.find_method(receiver, method, false)?;
        let data = globals.compile_on_demand(func_id) as *const _;
        match (globals.codegen.method_invoker)(
            self,
            globals,
            data,
            receiver,
            args.as_ptr(),
            args.len(),
        ) {
            Some(res) => Ok(res),
            None => Err(self.take_error()),
        }
    }

    ///
    /// Invoke block for *block_handler*.
    ///
    /// To get BlockData, use Executor.get_block_data().
    ///  
    /// let data = vm.get_block_data(globals, block);
    ///
    pub(crate) fn invoke_block(
        &mut self,
        globals: &mut Globals,
        data: BlockData,
        args: &[Value],
    ) -> Result<Value> {
        match (globals.codegen.block_invoker)(
            self,
            globals,
            &data as _,
            Value::nil(),
            args.as_ptr(),
            args.len(),
        ) {
            Some(val) => Ok(val),
            None => Err(self.take_error()),
        }
    }

    pub(crate) fn invoke_block_with_self(
        &mut self,
        globals: &mut Globals,
        data: BlockData,
        self_val: Value,
        args: &[Value],
    ) -> Result<Value> {
        match (globals.codegen.block_invoker_with_self)(
            self,
            globals,
            &data as _,
            self_val,
            args.as_ptr(),
            args.len(),
        ) {
            Some(val) => Ok(val),
            None => Err(self.take_error()),
        }
    }

    pub(crate) fn invoke_block_once(
        &mut self,
        globals: &mut Globals,
        block_handler: BlockHandler,
        args: &[Value],
    ) -> Result<Value> {
        let data = self.get_block_data(globals, block_handler);
        self.invoke_block(globals, data, args)
    }

    pub(crate) fn invoke_block_iter1(
        &mut self,
        globals: &mut Globals,
        block_handler: BlockHandler,
        iter: impl Iterator<Item = Value>,
    ) -> Result<()> {
        let block_data = self.get_block_data(globals, block_handler);
        for val in iter {
            self.invoke_block(globals, block_data.clone(), &[val])?;
        }
        Ok(())
    }

    pub(crate) fn invoke_block_map1(
        &mut self,
        globals: &mut Globals,
        block_handler: BlockHandler,
        iter: impl Iterator<Item = Value>,
    ) -> Result<Vec<Value>> {
        let block_data = self.get_block_data(globals, block_handler);
        let t = self.temp_len();
        for v in iter {
            let res = self.invoke_block(globals, block_data.clone(), &[v])?;
            self.temp_push(res);
        }
        let vec = self.temp_tear(t);
        Ok(vec)
    }

    pub(crate) fn invoke_block_fold1(
        &mut self,
        globals: &mut Globals,
        block_handler: BlockHandler,
        iter: impl Iterator<Item = Value>,
        mut res: Value,
    ) -> Result<Value> {
        let data = self.get_block_data(globals, block_handler);
        for elem in iter {
            res = self.invoke_block(globals, data.clone(), &[res, elem])?;
        }
        Ok(res)
    }

    ///
    /// Invoke proc.
    ///
    fn invoke_proc(
        &mut self,
        globals: &mut Globals,
        block_data: &BlockData,
        args: &[Value],
    ) -> Result<Value> {
        (globals.codegen.block_invoker)(
            self,
            globals,
            block_data as _,
            Value::nil(),
            args.as_ptr(),
            args.len(),
        )
        .ok_or_else(|| self.take_error())
    }

    fn invoke_method2_if_exists(
        &mut self,
        globals: &mut Globals,
        method: IdentId,
        receiver: Value,
        args: Arg,
        len: usize,
    ) -> Result<Value> {
        if let Some(func_id) = globals.check_method(receiver, method) {
            self.invoke_func(globals, func_id, receiver, args, len)
        } else {
            Ok(Value::nil())
        }
    }

    ///
    /// Invoke func with *args*: Args.
    ///
    fn invoke_func(
        &mut self,
        globals: &mut Globals,
        func_id: FuncId,
        receiver: Value,
        args: Arg,
        len: usize,
    ) -> Result<Value> {
        let data = globals.compile_on_demand(func_id) as *const _;
        (globals.codegen.method_invoker2)(self, globals, data, receiver, args, len)
            .ok_or_else(|| self.take_error())
    }

    fn define_class(
        &mut self,
        globals: &mut Globals,
        name: IdentId,
        superclass: Option<Value>,
        is_module: u32,
    ) -> Result<Value> {
        let parent = self.context_class_id();
        let self_val = match globals.get_constant(parent, name) {
            Some(val) => {
                val.expect_class_or_module(globals)?;
                if let Some(superclass) = superclass {
                    assert!(is_module != 1);
                    let superclass_id = superclass.expect_class(globals)?;
                    if Some(superclass_id) != val.as_class().superclass_id() {
                        return Err(MonorubyErr::superclass_mismatch(name));
                    }
                }
                val.as_class()
            }
            None => {
                let superclass = match superclass {
                    Some(superclass) => {
                        assert!(is_module != 1);
                        superclass.expect_class(globals)?;
                        superclass.as_class()
                    }
                    None => OBJECT_CLASS.get_obj(globals),
                };
                globals.define_class(name, Some(superclass), parent, is_module == 1)
            }
        };
        self.push_class_context(self_val.id());
        Ok(self_val.as_val())
    }
}

// Handling special variables.

impl Executor {
    ///
    /// Save captured strings to special variables.
    ///
    /// - $n (n:0,1,2,3...) <- The string which matched with nth parenthesis in the last successful match.
    ///
    /// - $& <- The string which matched successfully at last.
    ///
    /// - $' <- The string after $&.
    ///
    pub(crate) fn save_captures(&mut self, captures: &Captures, given: &str) {
        //let id1 = IdentId::get_id("$&");
        //let id2 = IdentId::get_id("$'");
        match captures.get(0) {
            Some(m) => {
                self.sp_last_match = Some(Value::string_from_str(&given[m.start()..m.end()]));
                self.sp_post_match = Some(Value::string_from_str(&given[m.end()..]));
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
                    .map(|m| Value::string_from_str(&given[m.start()..m.end()])),
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
        Value::array_from_iter(self.sp_matches.iter().map(|v| v.unwrap_or_default()))
    }
}

///
/// Program counter base.
///
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
struct BcPcBase(std::ptr::NonNull<Bc>);

impl std::ops::Add<usize> for BcPcBase {
    type Output = BcPc;
    fn add(self, rhs: usize) -> BcPc {
        BcPc::new(unsafe { self.as_ptr().add(rhs) })
    }
}

impl std::ops::Add<BcIndex> for BcPcBase {
    type Output = BcPc;
    fn add(self, rhs: BcIndex) -> BcPc {
        BcPc(unsafe { std::ptr::NonNull::new(self.as_ptr().offset(rhs.0 as isize)).unwrap() })
    }
}

impl BcPcBase {
    fn as_ptr(&self) -> *mut Bc {
        self.0.as_ptr()
    }
}

///
/// Program counter
///
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(transparent)]
struct BcPc(std::ptr::NonNull<Bc>);

impl BcPc {
    fn new(ptr: *mut Bc) -> Self {
        Self(std::ptr::NonNull::new(ptr).unwrap())
    }

    fn as_ptr(&self) -> *mut Bc {
        self.0.as_ptr()
    }

    fn is_loop(&self) -> bool {
        matches!(self.get_ir(), TraceIr::LoopStart(_))
    }

    pub(crate) fn from(bc: &Bc) -> Self {
        Self(std::ptr::NonNull::from(bc))
    }

    pub(crate) fn get_u64(self) -> u64 {
        self.0.as_ptr() as _
    }

    pub(crate) fn write2(self, data: u64) {
        unsafe { *((self.as_ptr() as *mut u64).add(1)) = data }
    }
}

impl std::ops::Sub<BcPcBase> for BcPc {
    type Output = usize;
    fn sub(self, rhs: BcPcBase) -> usize {
        let offset = unsafe { self.as_ptr().offset_from(rhs.as_ptr()) };
        assert!(offset >= 0, "self:{:?} rhs:{:?}", self, rhs);
        offset as usize
    }
}

impl std::ops::Sub<BcPc> for BcPc {
    type Output = usize;
    fn sub(self, rhs: BcPc) -> usize {
        let offset = unsafe { self.as_ptr().offset_from(rhs.as_ptr()) };
        assert!(offset >= 0, "self:{:?} rhs:{:?}", self, rhs);
        offset as usize
    }
}

impl std::ops::Add<isize> for BcPc {
    type Output = BcPc;
    fn add(self, rhs: isize) -> BcPc {
        BcPc::new(unsafe { self.as_ptr().offset(rhs) })
    }
}

impl std::ops::Sub<isize> for BcPc {
    type Output = BcPc;
    fn sub(self, rhs: isize) -> BcPc {
        BcPc::new(unsafe { self.as_ptr().offset(-rhs) })
    }
}

impl std::ops::AddAssign<i32> for BcPc {
    fn add_assign(&mut self, offset: i32) {
        unsafe {
            *self = BcPc::new(self.as_ptr().offset(offset as isize));
        }
    }
}

impl std::ops::Deref for BcPc {
    type Target = Bc;
    fn deref(&self) -> &Self::Target {
        unsafe { self.0.as_ref() }
    }
}

impl BcPc {
    fn get_ir(&self) -> TraceIr {
        TraceIr::from_bc(*self)
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
        let s = match TraceIr::from_bc(*self) {
            TraceIr::InitMethod(info) => {
                format!("init_method {info:?}")
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
            TraceIr::Symbol(reg, id) => format!("{:?} = :{id}", reg),
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
                format!("{:?} = literal[{}]", reg, globals.inspect(val))
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
                } = &globals.store[id];
                let mut const_name = if *toplevel { "::" } else { "" }.to_string();
                for c in prefix {
                    c.append_to(&mut const_name);
                    const_name += "::";
                }
                name.append_to(&mut const_name);
                let op1 = format!("{:?} = const[{}]", reg, const_name);
                format!(
                    "{:36} [{}]",
                    op1,
                    match self.value() {
                        None => "<INVALID>".to_string(),
                        Some(val) => globals.inspect(val),
                    }
                )
            }
            TraceIr::StoreConst(reg, id) => {
                format!("const[{id}] = {:?}", reg)
            }
            TraceIr::BlockArgProxy(dst, outer) => {
                format!("{:?} = block_proxy({outer})", dst)
            }
            TraceIr::BlockArg(dst, outer) => {
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
                    "{:?} = {id}: {}[{:?}]",
                    reg,
                    class_id.get_name(globals),
                    ivar_id,
                )
            }
            TraceIr::StoreIvar(reg, id, class_id, ivar_id) => {
                format!(
                    "{id}: {}[{:?}] = {:?}",
                    class_id.get_name(globals),
                    ivar_id,
                    reg
                )
            }
            TraceIr::LoadGvar { dst: ret, name } => {
                format!("{:?} = {name}", ret)
            }
            TraceIr::StoreGvar { src, name } => {
                format!("{name} = {:?}", src)
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
            TraceIr::BitNot { ret, src } => {
                let op1 = format!("{:?} = ~{:?}", ret, src);
                format!("{:36} [{}]", op1, self.classid1().get_name(globals),)
            }
            TraceIr::Pos { ret, src } => {
                let op1 = format!("{:?} = +{:?}", ret, src);
                format!("{:36} [{}]", op1, self.classid1().get_name(globals),)
            }
            TraceIr::Neg { ret, src } => {
                let op1 = format!("{:?} = -{:?}", ret, src);
                format!("{:36} [{}]", op1, self.classid1().get_name(globals),)
            }
            TraceIr::Not { ret, src } => {
                let op1 = format!("{:?} = !{:?}", ret, src);
                format!("{:36} [{}]", op1, self.classid1().get_name(globals),)
            }
            TraceIr::BinOp {
                kind,
                ret,
                mode: OpMode::RR(lhs, rhs),
            }
            | TraceIr::IBinOp {
                kind,
                ret,
                mode: OpMode::RR(lhs, rhs),
            }
            | TraceIr::FBinOp {
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
            | TraceIr::IBinOp {
                kind,
                ret,
                mode: OpMode::RI(lhs, rhs),
            }
            | TraceIr::FBinOp {
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
            | TraceIr::IBinOp {
                kind,
                ret,
                mode: OpMode::IR(lhs, rhs),
            }
            | TraceIr::FBinOp {
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
            TraceIr::MethodRet(reg) => format!("method_ret {:?}", reg),
            TraceIr::Break(reg) => format!("break {:?}", reg),
            TraceIr::Raise(reg) => format!("raise {:?}", reg),
            TraceIr::EnsureEnd => format!("ensure_end"),
            TraceIr::Mov(dst, src) => format!("{:?} = {:?}", dst, src),
            TraceIr::MethodCall {
                ret,
                callid,
                has_splat,
                info,
                class,
                ..
            } => {
                let callsite = &globals.store[callid];
                let name = callsite.name.unwrap();
                let MethodInfo {
                    recv, args, len, ..
                } = info;
                let kw_len = callsite.kw_args.len();
                let op1 = format!(
                    "{} = {:?}.{name}({}{}){}",
                    ret.ret_str(),
                    recv,
                    if len == 0 {
                        "".to_string()
                    } else {
                        format!("{:?};{}", args, len)
                    },
                    if kw_len == 0 {
                        "".to_string()
                    } else {
                        format!(" kw:{:?};{}", args + len, kw_len)
                    },
                    if has_splat { "*" } else { "" }
                );
                format!("{:36} [{}]", op1, class.get_name(globals))
            }
            TraceIr::MethodCallBlock {
                ret,
                callid,
                has_splat,
                info,
                class,
                ..
            } => {
                let callsite = &globals.store[callid];
                let name = callsite.name.unwrap();
                let MethodInfo {
                    recv, args, len, ..
                } = info;
                let kw_len = callsite.kw_args.len();
                let op1 = format!(
                    "{} = {:?}.{name}({}{} &{:?}){}",
                    ret.ret_str(),
                    recv,
                    if len == 0 {
                        "".to_string()
                    } else {
                        format!("{:?};{}", args + 1, len)
                    },
                    if kw_len == 0 {
                        "".to_string()
                    } else {
                        format!(" kw:{:?};{}", args + len + 1, kw_len)
                    },
                    args,
                    if has_splat { "*" } else { "" }
                );
                format!("{:36} [{}]", op1, class.get_name(globals))
            }
            TraceIr::Super {
                ret,
                callid,
                class,
                info,
                ..
            } => {
                let callsite = &globals.store[callid];
                let MethodInfo { args, len, .. } = info;
                let kw_len = callsite.kw_args.len();
                let op1 = format!(
                    "{} = super({}{}){}",
                    ret.ret_str(),
                    if len == 0 {
                        "".to_string()
                    } else {
                        format!("{:?};{}", args, len)
                    },
                    if kw_len == 0 {
                        "".to_string()
                    } else {
                        format!(" kw:{:?};{}", args + len, kw_len)
                    },
                    ""
                );
                format!("{:36} [{}]", op1, class.get_name(globals))
            }
            TraceIr::InlineCall {
                ret,
                inline_id,
                info,
                class,
                ..
            } => {
                let MethodInfo {
                    recv, args, len, ..
                } = info;
                let name = &globals.store.get_inline_info(inline_id).2;
                let op1 = if len == 0 {
                    format!("{} = {:?}.inline {name}()", ret.ret_str(), recv,)
                } else {
                    format!(
                        "{} = {:?}.inline {name}({:?}; {})",
                        ret.ret_str(),
                        recv,
                        args,
                        len,
                    )
                };
                format!("{:36} [{}]", op1, class.get_name(globals))
            }
            TraceIr::Yield {
                ret,
                args,
                len,
                callid: _,
            } => {
                if len == 0 {
                    format!("{} = yield", ret.ret_str())
                } else {
                    format!("{} = yield({:?}; {})", ret.ret_str(), args, len)
                }
            }
            TraceIr::MethodArgs(..) => return None,
            TraceIr::MethodDef { name, func_id } => {
                format!("method_def {name}: {:?}", func_id)
            }
            TraceIr::SingletonMethodDef { obj, name, func_id } => {
                format!(
                    "singleton_method_def {}.{name}: {:?}",
                    obj.ret_str(),
                    func_id
                )
            }
            TraceIr::ClassDef {
                ret,
                superclass,
                name,
                func_id,
            } => {
                format!(
                    "{} = class_def {name} < {}: {:?}",
                    ret.ret_str(),
                    superclass.ret_str(),
                    func_id
                )
            }
            TraceIr::ModuleDef { ret, name, func_id } => {
                format!("{} = module_def {name}: {:?}", ret.ret_str(), func_id)
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
            TraceIr::ConcatRegexp(ret, args, len) => {
                format!("{} = concat_regexp({:?}; {})", ret.ret_str(), args, len)
            }
            TraceIr::ExpandArray(src, dst, len) => {
                format!("{:?}; {} = expand({:?})", dst, len, src)
            }
            TraceIr::AliasMethod { new, old } => {
                format!("alias_method({:?}<-{:?})", new, old)
            }
            TraceIr::DefinedYield { ret } => format!("{} = defined?(yield)", ret.ret_str()),
            TraceIr::DefinedConst { ret, siteid } => {
                let ConstSiteInfo {
                    name,
                    prefix,
                    toplevel,
                    ..
                } = &globals.store[siteid];
                let mut const_name = if *toplevel { "::" } else { "" }.to_string();
                for c in prefix {
                    c.append_to(&mut const_name);
                    const_name += "::";
                }
                name.append_to(&mut const_name);
                format!("{} = defined?(constant) {const_name}", ret.ret_str())
            }
            TraceIr::DefinedMethod { ret, recv, name } => {
                format!("{} = defined?(method) {:?}.{}", ret.ret_str(), recv, name)
            }
            TraceIr::DefinedGvar { ret, name } => {
                format!("{} = defined?(gvar) {}", ret.ret_str(), name)
            }
            TraceIr::DefinedIvar { ret, name } => {
                format!("{} = defined?(ivar) {}", ret.ret_str(), name)
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

    fn generic_func(&self) -> BinaryOpFn {
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
/// an index of bytecode instruction.
///
#[derive(Clone, Copy, PartialEq, Eq, Hash, Default, PartialOrd)]
pub(crate) struct BcIndex(u32);

impl std::fmt::Debug for BcIndex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, ":{:05}", self.0)
    }
}

impl std::fmt::Display for BcIndex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, ":{:05}", self.0)
    }
}

impl std::ops::Add<usize> for BcIndex {
    type Output = Self;
    fn add(self, rhs: usize) -> Self::Output {
        Self((self.0 as usize + rhs) as u32)
    }
}

impl std::ops::Add<i64> for BcIndex {
    type Output = Self;
    fn add(self, rhs: i64) -> Self::Output {
        Self((self.0 as i64 + rhs) as u32)
    }
}

impl std::ops::Add<i32> for BcIndex {
    type Output = Self;
    fn add(self, rhs: i32) -> Self::Output {
        Self((self.0 as i64 + rhs as i64) as u32)
    }
}

impl std::iter::Step for BcIndex {
    fn steps_between(start: &Self, end: &Self) -> Option<usize> {
        if start > end {
            None
        } else {
            Some((end.0 - start.0) as usize)
        }
    }

    fn forward_checked(start: Self, count: usize) -> Option<Self> {
        Some(start + count)
    }

    fn backward_checked(start: Self, count: usize) -> Option<Self> {
        if (start.0 as usize) < count {
            None
        } else {
            Some(BcIndex((start.0 as usize - count) as _))
        }
    }
}

impl BcIndex {
    pub(crate) fn from(i: usize) -> Self {
        Self(i as u32)
    }

    pub(crate) fn to_usize(&self) -> usize {
        self.0 as usize
    }
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
    /// interpreter:0 JIT code:1 native:2
    kind: u8,
    /// bit 2-1: public:0 private:1 protected:2
    /// bit 0: method:0 class_def:1
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

type FuncDataPtr = std::ptr::NonNull<FuncData>;

#[derive(Debug, Clone, PartialEq, Default)]
#[repr(C)]
struct FuncData {
    /// address of function.
    codeptr: Option<monoasm::CodePtr>,
    /// metadata of this function.
    meta: Meta,
    /// the address of program counter
    pc: Option<BcPc>,
}

impl FuncData {
    fn pc(&self) -> BcPc {
        self.pc.unwrap()
    }

    fn set_pc(&mut self, pc: BcPc) {
        self.pc = Some(pc);
    }

    fn set_reg_num(&mut self, reg_num: i64) {
        self.meta.set_reg_num(reg_num);
    }

    fn as_ptr(&self) -> FuncDataPtr {
        std::ptr::NonNull::new(self as *const _ as _).unwrap()
    }
}

extern "C" fn exec_jit_compile_patch(
    globals: &mut Globals,
    func_id: FuncId,
    self_value: Value,
    entry: DestLabel,
) {
    let patch_point = globals.codegen.jit.label();
    let entry_label = globals.codegen.jit.label();
    let guard = globals.codegen.jit.label();
    globals
        .codegen
        .class_guard_stub(self_value.class(), patch_point, entry_label, guard);

    assert!(globals[func_id]
        .add_jit_code(self_value.class(), patch_point)
        .is_none());
    globals.exec_jit_compile_method(func_id, self_value, entry_label);

    globals.codegen.jit.apply_jmp_patch(entry, guard);
}

extern "C" fn exec_jit_recompile_method(globals: &mut Globals, func_id: FuncId, self_value: Value) {
    let entry_label = globals.codegen.jit.label();
    globals.exec_jit_compile_method(func_id, self_value, entry_label);
    let patch_point = globals[func_id].get_jit_code(self_value.class()).unwrap();
    globals
        .codegen
        .jit
        .apply_jmp_patch(patch_point, entry_label);
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
    let entry_label = globals.codegen.jit.label();
    globals.exec_jit_compile(func_id, self_value, Some(pc), entry_label);
    let codeptr = globals.codegen.jit.get_label_address(entry_label);
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

///
/// Parameters information in *ISeqInfo*.
///
#[derive(Debug, Clone, Default, PartialEq)]
struct ParamsInfo {
    required_num: usize,
    // required + optional
    reqopt_num: usize,
    // required + optional + rest
    pos_num: usize,
    // for param, req(incl. destruct slot), opt, rest, keyword, destructed local, block
    args_names: Vec<Option<IdentId>>,
    keyword_names: Vec<IdentId>,
    block_param: Option<IdentId>,
}

///
/// Information for bytecode compiler.
///
/// this includes AST and information for initialization of optional, keyword, destructuring parameters.
///
struct CompileInfo {
    /// AST.
    ast: Node,
    /// keyword params initializers.
    keyword_initializers: Vec<Option<Box<Node>>>,
    /// param expansion info
    destruct_info: Vec<DestructureInfo>,
    /// optional parameters initializers.
    optional_info: Vec<OptionalInfo>,
    /// *for* statement parameters info.
    for_param_info: Vec<ForParamInfo>,
    loc: Loc,
}

impl CompileInfo {
    fn new(
        ast: Node,
        keyword_initializers: Vec<Option<Box<Node>>>,
        expand_info: Vec<DestructureInfo>,
        optional_info: Vec<OptionalInfo>,
        for_param_info: Vec<ForParamInfo>,
        loc: Loc,
    ) -> Self {
        Self {
            ast,
            keyword_initializers,
            destruct_info: expand_info,
            optional_info,
            for_param_info,
            loc,
        }
    }
}

#[derive(Debug, Clone, Default, PartialEq)]
struct DestructureInfo {
    src: usize,
    dst: usize,
    len: usize,
}

#[derive(Debug, Clone, Default, PartialEq)]
struct OptionalInfo {
    local: BcLocal,
    initializer: Node,
}

#[derive(Debug, Clone, Default, PartialEq)]
struct ForParamInfo {
    dst_outer: usize,
    dst_reg: BcLocal,
    src_reg: usize,
}

struct Root<'a, 'b> {
    globals: &'a Globals,
    executor: &'b Executor,
}

impl<'a, 'b> alloc::GC<RValue> for Root<'a, 'b> {
    fn mark(&self, alloc: &mut alloc::Allocator<RValue>) {
        self.globals.mark(alloc);
        self.executor.mark(alloc);
    }
}

impl<'a, 'b> alloc::GCRoot<RValue> for Root<'a, 'b> {
    fn startup_flag(&self) -> bool {
        true
    }
}

///
/// Execute garbage collection.
///
extern "C" fn execute_gc(globals: &Globals, executor: &Executor) {
    alloc::ALLOC.with(|alloc| alloc.borrow_mut().gc(&Root { globals, executor }));
}
