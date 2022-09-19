use super::*;
use std::pin::Pin;

///
/// ID of function.
///
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
#[repr(transparent)]
pub struct FuncId(pub u32);

impl From<FuncId> for u32 {
    fn from(id: FuncId) -> u32 {
        id.0
    }
}

#[derive(Clone, PartialEq)]
pub struct Funcs(Vec<FuncInfo>);

impl std::ops::Index<FuncId> for Funcs {
    type Output = FuncInfo;
    fn index(&self, index: FuncId) -> &FuncInfo {
        &self.0[index.0 as usize]
    }
}

impl std::ops::IndexMut<FuncId> for Funcs {
    fn index_mut(&mut self, index: FuncId) -> &mut FuncInfo {
        &mut self.0[index.0 as usize]
    }
}

impl Funcs {
    fn new(sourceinfo: SourceInfoRef, is_classdef: bool) -> Self {
        Self(vec![FuncInfo::new_ruby(
            None,
            FuncId(0),
            vec![],
            Node::new_nil(Loc(0, 0)),
            sourceinfo,
            is_classdef,
        )])
    }

    fn next_func_id(&self) -> FuncId {
        FuncId(self.0.len() as u32)
    }

    fn add_ruby_func(
        &mut self,
        name: Option<String>,
        args: Vec<String>,
        ast: Node,
        sourceinfo: SourceInfoRef,
        is_classdef: bool,
    ) -> FuncId {
        let fid = self.next_func_id();
        self.0.push(FuncInfo::new_ruby(
            name,
            fid,
            args,
            ast,
            sourceinfo,
            is_classdef,
        ));
        fid
    }

    fn add_native_func(&mut self, name: String, address: BuiltinFn, arity: i32) -> FuncId {
        let id = self.next_func_id();
        self.0.push(FuncInfo::new_native(id, name, address, arity));
        id
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ConstSiteInfo {
    /// Name of constants.
    pub name: IdentId,
    /// Qualifier.
    pub prefix: Vec<IdentId>,
    /// Is toplevel?. (e.g. ::Foo)
    pub toplevel: bool,
    /// Inline constant cache.
    pub cache: (usize, Option<Value>), //(version, value)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
pub struct ConstSiteId(pub u32);

impl ConstSiteId {
    pub fn get(&self) -> u32 {
        self.0
    }
}

/*#[derive(Debug, Clone, Default, PartialEq)]
pub struct MethodDefInfo {
    pub name: IdentId,
    pub func: FuncId,
}*/

#[derive(Clone, PartialEq)]
pub struct FnStore {
    functions: Funcs,
    /// const access site info.
    constsite_info: Vec<ConstSiteInfo>,
}

impl std::ops::Index<FuncId> for FnStore {
    type Output = FuncInfo;
    fn index(&self, index: FuncId) -> &FuncInfo {
        &self.functions[index]
    }
}

impl std::ops::IndexMut<FuncId> for FnStore {
    fn index_mut(&mut self, index: FuncId) -> &mut FuncInfo {
        &mut self.functions[index]
    }
}

impl std::ops::Index<ConstSiteId> for FnStore {
    type Output = ConstSiteInfo;
    fn index(&self, index: ConstSiteId) -> &ConstSiteInfo {
        &self.constsite_info[index.0 as usize]
    }
}

impl std::ops::IndexMut<ConstSiteId> for FnStore {
    fn index_mut(&mut self, index: ConstSiteId) -> &mut ConstSiteInfo {
        &mut self.constsite_info[index.0 as usize]
    }
}

impl FnStore {
    pub fn new() -> Self {
        Self {
            functions: Funcs::new(SourceInfoRef::default(), false),
            constsite_info: vec![],
        }
    }

    pub fn functions(&self) -> &Vec<FuncInfo> {
        &self.functions.0
    }

    pub fn functions_mut(&mut self) -> &mut Vec<FuncInfo> {
        &mut self.functions.0
    }

    fn len(&self) -> usize {
        self.functions.0.len()
    }

    pub fn funcs_mut(&mut self) -> &mut Vec<FuncInfo> {
        &mut self.functions.0
    }

    pub(crate) fn add_ruby_func(
        &mut self,
        name: Option<String>,
        args: Vec<String>,
        ast: Node,
        sourceinfo: SourceInfoRef,
        is_classdef: bool,
    ) -> FuncId {
        self.functions
            .add_ruby_func(name, args, ast, sourceinfo, is_classdef)
    }
}

impl FnStore {
    pub(super) fn compile_script(
        &mut self,
        ast: Node,
        id_store: &mut IdentifierTable,
        sourceinfo: SourceInfoRef,
    ) -> Result<FuncId> {
        let main_fid = self.functions.add_ruby_func(
            Some("/main".to_string()),
            vec![],
            ast,
            sourceinfo.clone(),
            false,
        );
        let mut fid = main_fid;

        while self.len() > fid.0 as usize {
            self.compile_func(fid, id_store)?;
            fid = FuncId(fid.0 + 1);
        }

        Ok(main_fid)
    }

    /// Generate bytecode for a function which has *func_id*.
    fn compile_func(&mut self, func_id: FuncId, id_store: &mut IdentifierTable) -> Result<()> {
        let mut info = std::mem::take(self[func_id].as_ruby_func_mut());
        let mut ir = IrContext::compile_ast(&mut info, self, id_store)?;
        ir.ir_to_bytecode(&mut info, self);

        let regs = info.total_reg_num();
        std::mem::swap(&mut info, self[func_id].as_ruby_func_mut());
        self[func_id].data.pc = self[func_id].as_ruby_func().get_bytecode_address(0);
        self[func_id].data.set_reg_num(regs as i64);
        Ok(())
    }

    pub(super) fn add_builtin_func(
        &mut self,
        name: String,
        address: BuiltinFn,
        arity: i32,
    ) -> FuncId {
        self.functions.add_native_func(name, address, arity)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum FuncKind {
    Normal(RubyFuncInfo),
    Builtin { abs_address: u64 },
}

impl std::default::Default for FuncKind {
    fn default() -> Self {
        Self::Builtin { abs_address: 0 }
    }
}

pub const FUNCDATA_OFFSET_CODEPTR: u64 = 0;
pub const FUNCDATA_OFFSET_PC: u64 = 8;
pub const FUNCDATA_OFFSET_META: u64 = 16;

///
/// Metadata.
///~~~
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
///~~~
#[derive(Clone, Copy, PartialEq, Default)]
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
    pub fn new(meta: u64) -> Self {
        Self(meta)
    }

    pub fn get(&self) -> u64 {
        self.0
    }

    pub fn vm_method(func_id: FuncId, reg_num: i64) -> Self {
        // kind = VM, mode = method
        Self(((reg_num as i16 as u16 as u64) << 32) + (func_id.0 as u64))
    }

    pub fn vm_classdef(func_id: FuncId, reg_num: i64) -> Self {
        // kind = VM, mode = classdef
        Self((1 << 56) + ((reg_num as i16 as u16 as u64) << 32) + (func_id.0 as u64))
    }

    pub fn native(func_id: FuncId, reg_num: i64) -> Self {
        // kind = NATIVE, mode = method
        Self((2 << 48) + ((reg_num as i16 as u16 as u64) << 32) + (func_id.0 as u64))
    }

    pub fn func_id(&self) -> FuncId {
        FuncId(self.0 as u32)
    }

    pub fn reg_num(&self) -> i64 {
        (self.0 >> 32) as u16 as i16 as i64
    }

    pub fn kind(&self) -> u8 {
        (self.0 >> 48) as u8
    }

    pub fn mode(&self) -> u8 {
        (self.0 >> 56) as u8
    }

    pub fn set_jit(&mut self) {
        let meta = (self.0 & 0xff00_ffff_ffff_ffff) | (1 << 48);
        self.0 = meta;
    }

    pub fn set_reg_num(&mut self, reg_num: i64) {
        let meta = (self.0 & 0xffff_0000_ffff_ffff) | ((reg_num as i16 as u16 as u64) << 32);
        self.0 = meta;
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

#[derive(Debug, Clone, PartialEq, Default)]
pub struct FuncInfo {
    /// name of this function.
    name: Option<String>,
    /// arity of this function.
    /// -1 for variable numbers.
    arity: i32,
    pub(crate) data: FuncData,
    pub(crate) kind: FuncKind,
}

impl FuncInfo {
    fn new_ruby(
        name: Option<String>,
        func_id: FuncId,
        args: Vec<String>,
        ast: Node,
        sourceinfo: SourceInfoRef,
        is_classdef: bool,
    ) -> Self {
        let info = RubyFuncInfo::new(func_id, name.clone(), args, ast, sourceinfo);
        Self {
            name,
            arity: info.args.len() as i32,
            data: FuncData {
                codeptr: None,
                pc: BcPc::default(),
                meta: if is_classdef {
                    Meta::vm_classdef(info.id, 0)
                } else {
                    Meta::vm_method(info.id, 0)
                },
            },
            kind: FuncKind::Normal(info),
        }
    }

    fn new_native(func_id: FuncId, name: String, address: BuiltinFn, arity: i32) -> Self {
        let reg_num = if arity == -1 { -1 } else { arity as i64 };
        Self {
            name: Some(name),
            arity,
            data: FuncData {
                codeptr: None,
                pc: BcPc::default(),
                meta: Meta::native(func_id, reg_num),
            },
            kind: FuncKind::Builtin {
                abs_address: address as *const u8 as u64,
            },
        }
    }

    pub(crate) fn name(&self) -> Option<&String> {
        self.name.as_ref()
    }

    pub(crate) fn arity(&self) -> i32 {
        self.arity
    }

    pub(crate) fn as_ruby_func(&self) -> &RubyFuncInfo {
        match &self.kind {
            FuncKind::Normal(info) => info,
            FuncKind::Builtin { .. } => unreachable!(),
        }
    }

    pub(crate) fn as_ruby_func_mut(&mut self) -> &mut RubyFuncInfo {
        match &mut self.kind {
            FuncKind::Normal(info) => info,
            FuncKind::Builtin { .. } => unreachable!(),
        }
    }
}

impl FuncInfo {
    #[cfg(feature = "emit-bc")]
    pub(crate) fn dump_bc(&self, globals: &Globals) {
        let info = self.as_ruby_func();
        fn optstr(opt: bool) -> &'static str {
            if opt {
                "_"
            } else {
                ""
            }
        }
        eprintln!("------------------------------------");
        eprintln!(
            "{:?} name:{} args:{:?} bc:{:?} meta:{:?}",
            info.id,
            match &self.name {
                Some(name) => name,
                None => "<ANONYMOUS>",
            },
            info.args.iter().collect::<Vec<_>>(),
            BcPcBase::new(info),
            self.data.meta,
        );
        let mut buf = None;
        let mut skip = false;
        let bb_info = info.get_bb_info();
        for (i, pc) in info.bytecode().iter().enumerate() {
            let pc = BcPc::from(pc);
            if skip {
                skip = false;
                continue;
            }
            let bcop1 = pc.op1();
            if let BcOp::MethodArgs(..) = bcop1 {
            } else {
                eprint!("{}:{:05} ", if bb_info[i].is_some() { "+" } else { " " }, i)
            };
            match bcop1 {
                BcOp::Br(disp) => {
                    eprintln!("br =>:{:05}", i as i32 + 1 + disp);
                }
                BcOp::CondBr(reg, disp, opt, kind) => {
                    eprintln!(
                        "cond{}br {}{:?} =>:{:05}",
                        kind.to_s(),
                        optstr(opt),
                        reg,
                        i as i32 + 1 + disp
                    );
                }
                BcOp::Integer(reg, num) => eprintln!("{:?} = {}: i32", reg, num),
                BcOp::Symbol(reg, id) => {
                    eprintln!("{:?} = :{}", reg, globals.id_store.get_name(id))
                }
                BcOp::Literal(reg, val) => {
                    eprintln!("{:?} = literal[{}]", reg, globals.val_inspect(val))
                }
                BcOp::Array(ret, src, len) => {
                    eprintln!("{:?} = array[{:?}; {}]", ret, src, len)
                }
                BcOp::Index(ret, base, idx) => {
                    eprintln!("{:?} = {:?}.[{:?}]", ret, base, idx)
                }
                BcOp::IndexAssign(src, base, idx) => {
                    eprintln!("{:?}.[{:?}] = {:?}", base, idx, src)
                }
                BcOp::LoadConst(reg, id) => {
                    let name = globals.func[id].name;
                    let op1 = format!("{:?} = const[{}]", reg, globals.id_store.get_name(name));
                    eprintln!(
                        "{:36} [{}]",
                        op1,
                        match pc.value() {
                            None => "<invalid>".to_string(),
                            Some(val) => val.inspect(globals),
                        }
                    );
                }
                BcOp::StoreConst(reg, id) => {
                    eprintln!("const[{}] = {:?}", globals.id_store.get_name(id), reg)
                }
                BcOp::LoadIvar(reg, id) => {
                    eprintln!("{:?} = {}", reg, globals.id_store.get_name(id))
                }
                BcOp::StoreIvar(reg, id) => {
                    eprintln!("{} = {:?}", globals.id_store.get_name(id), reg)
                }
                BcOp::Nil(reg) => eprintln!("{:?} = nil", reg),
                BcOp::Neg(dst, src) => {
                    let op1 = format!("{:?} = neg {:?}", dst, src);
                    eprintln!("{:36} [{}]", op1, pc.classid1().get_name(globals),);
                }
                BcOp::BinOp(kind, dst, lhs, rhs) => {
                    let op1 = format!("{:?} = {:?} {} {:?}", dst, lhs, kind, rhs);
                    eprintln!(
                        "{:36} [{}][{}]",
                        op1,
                        pc.classid1().get_name(globals),
                        pc.classid2().get_name(globals)
                    );
                }
                BcOp::BinOpRi(kind, dst, lhs, rhs) => {
                    let op1 = format!("{:?} = {:?} {} {}: i16", dst, lhs, kind, rhs,);
                    eprintln!(
                        "{:36} [{}][{}]",
                        op1,
                        pc.classid1().get_name(globals),
                        pc.classid2().get_name(globals)
                    );
                }
                BcOp::BinOpIr(kind, dst, lhs, rhs) => {
                    let op1 = format!("{:?} = {}: i16 {} {:?}", dst, lhs, kind, rhs,);
                    eprintln!(
                        "{:36} [{}][{}]",
                        op1,
                        pc.classid1().get_name(globals),
                        pc.classid2().get_name(globals)
                    );
                }
                BcOp::Cmp(kind, dst, lhs, rhs, opt) => {
                    let op1 = format!("{}{:?} = {:?} {:?} {:?}", optstr(opt), dst, lhs, kind, rhs,);
                    eprintln!(
                        "{:36} [{}][{}]",
                        op1,
                        pc.classid1().get_name(globals),
                        pc.classid2().get_name(globals)
                    );
                }
                BcOp::Cmpri(kind, dst, lhs, rhs, opt) => {
                    let op1 = format!(
                        "{}{:?} = {:?} {:?} {}: i16",
                        optstr(opt),
                        dst,
                        lhs,
                        kind,
                        rhs,
                    );
                    eprintln!(
                        "{:36} [{}][{}]",
                        op1,
                        pc.classid1().get_name(globals),
                        pc.classid2().get_name(globals)
                    );
                }

                BcOp::Ret(reg) => eprintln!("ret {:?}", reg),
                BcOp::Mov(dst, src) => eprintln!("{:?} = {:?}", dst, src),
                BcOp::MethodCall(..) => {
                    assert!(buf.is_none());
                    buf = Some((bcop1.clone(), pc.classid1()));
                }
                BcOp::MethodArgs(recv, args, len) => {
                    let (recv, ret, name, class_id) = match std::mem::take(&mut buf).unwrap() {
                        (BcOp::MethodCall(ret, name), class_id) => (recv, ret, name, class_id),
                        _ => unreachable!(),
                    };
                    let name = globals.id_store.get_name(name);
                    let op1 = format!(
                        "{} = {:?}.call {}({:?}; {})",
                        ret.ret_str(),
                        recv,
                        name,
                        args,
                        len,
                    );
                    eprintln!("{:36} [{}]", op1, class_id.get_name(globals));
                    skip = true;
                }
                BcOp::MethodDef(name, func_id) => {
                    let name = globals.id_store.get_name(name);
                    eprintln!("mthod_def {:?}: {:?}", name, func_id)
                }
                BcOp::ClassDef(ret, name, func_id) => {
                    let name = globals.id_store.get_name(name);
                    eprintln!("{} = class_def {:?}: {:?}", ret.ret_str(), name, func_id)
                }
                BcOp::ConcatStr(ret, args, len) => {
                    eprintln!("{} = concat({:?}; {})", ret.ret_str(), args, len)
                }
                BcOp::LoopStart(count) => eprintln!(
                    "loop_start counter={} jit-addr={:016x}",
                    count,
                    pc.from_jit_addr()
                ),
                BcOp::LoopEnd => eprintln!("loop_end"),
            }
        }
        eprintln!("------------------------------------");
    }
}

#[derive(Debug, Clone, Default, PartialEq)]
pub(crate) struct RubyFuncInfo {
    /// ID of this function.
    pub(crate) id: FuncId,
    name: Option<String>,
    /// Bytecode.
    pub(super) bytecode: Option<Pin<Box<[Bc]>>>,
    /// Source map.
    pub sourcemap: Vec<Loc>,
    /// the name of arguments.
    args: Vec<String>,
    /// local variables.
    locals: HashMap<String, u16>,
    /// The current register id.
    pub temp: u16,
    /// The number of temporary registers.
    temp_num: u16,
    /// AST.
    pub ast: Option<Node>,
    pub sourceinfo: SourceInfoRef,
}

impl RubyFuncInfo {
    pub(crate) fn new(
        id: FuncId,
        name: Option<String>,
        args: Vec<String>,
        ast: Node,
        sourceinfo: SourceInfoRef,
    ) -> Self {
        let mut info = RubyFuncInfo {
            id,
            name,
            bytecode: None,
            sourcemap: vec![],
            args: args.clone(),
            locals: HashMap::default(),
            temp: 0,
            temp_num: 0,
            ast: Some(ast),
            sourceinfo,
        };
        args.into_iter().for_each(|name| {
            info.add_local(name);
        });
        info
    }

    /// set bytecode.
    pub(crate) fn set_bytecode(&mut self, bc: Vec<Bc>) {
        self.bytecode = Some(Box::into_pin(bc.into_boxed_slice()));
    }

    /// get bytecode address.
    pub(crate) fn get_bytecode_address(&self, index: usize) -> BcPc {
        BcPcBase::new(self) + index
    }

    /// get a number of registers.
    pub(crate) fn total_reg_num(&self) -> usize {
        1 + self.locals.len() + self.temp_num as usize
    }

    /// get a number of arguments(includes *self*).
    pub(crate) fn total_arg_num(&self) -> usize {
        1 + self.args.len()
    }

    /// get name.
    #[cfg(any(feature = "emit-asm", feature = "log-jit", feature = "emit-tir"))]
    pub(crate) fn name(&self) -> &Option<String> {
        &self.name
    }

    /// get bytecode.
    pub(crate) fn bytecode(&self) -> &[Bc] {
        &self.bytecode.as_ref().unwrap()
    }

    pub(crate) fn get_pc(&self, idx: usize) -> BcPc {
        BcPc::from(&self.bytecode()[idx])
    }

    /// get bytecode length.
    pub(crate) fn bytecode_len(&self) -> usize {
        self.bytecode().len()
    }

    /// get bytecode address.
    pub(crate) fn bytecode_top(&self) -> *const Bc {
        self.bytecode().as_ptr()
    }

    /// get the next register id.
    pub(crate) fn next_reg(&self) -> BcTemp {
        BcTemp(self.temp)
    }

    pub(crate) fn push(&mut self) -> BcTemp {
        let reg = BcTemp(self.temp);
        self.temp += 1;
        if self.temp > self.temp_num {
            self.temp_num = self.temp;
        }
        reg
    }

    pub(crate) fn pop(&mut self) -> BcTemp {
        self.temp -= 1;
        BcTemp(self.temp)
    }

    pub(crate) fn popn(&mut self, len: usize) {
        self.temp -= len as u16;
    }

    pub(crate) fn find_local(&mut self, ident: &str) -> BcLocal {
        match self.locals.get(ident) {
            Some(local) => BcLocal(*local),
            None => self.add_local(ident.to_owned()),
        }
    }

    /// Add a variable identifier without checking duplicates.
    pub(crate) fn add_local(&mut self, ident: String) -> BcLocal {
        let local = self.locals.len() as u16;
        assert!(self.locals.insert(ident, local).is_none());
        BcLocal(local)
    }

    pub(crate) fn is_local(&mut self, node: &Node) -> Option<BcLocal> {
        if let NodeKind::LocalVar(name) = &node.kind {
            Some(self.find_local(name))
        } else {
            None
        }
    }

    pub(crate) fn get_bb_info(&self) -> Vec<Option<(usize, Vec<usize>)>> {
        let mut info = vec![vec![]; self.bytecode_len() + 1];
        let mut skip = false;
        for (idx, pc) in self.bytecode().iter().enumerate() {
            let pc = BcPc::from(pc);
            if skip {
                skip = false;
                continue;
            }
            match pc.op1() {
                BcOp::MethodArgs(..) => {
                    skip = true;
                }
                BcOp::Br(disp) | BcOp::CondBr(_, disp, _, _) => {
                    info[((idx + 1) as i32 + disp) as usize].push(idx);
                }
                _ => {}
            }
        }
        assert_eq!(0, info[self.bytecode_len()].len());
        let mut bb_id = 1;
        let mut bb_info: Vec<_> = info
            .into_iter()
            .map(|e| {
                if e.len() > 0 {
                    let id = bb_id;
                    bb_id += 1;
                    Some((id, e))
                } else {
                    None
                }
            })
            .collect();
        for (idx, pc) in self.bytecode().iter().enumerate() {
            let pc = BcPc::from(pc);
            if skip {
                skip = false;
                continue;
            }
            match pc.op1() {
                BcOp::MethodArgs(..) => {
                    skip = true;
                    match bb_info[idx + 2] {
                        Some(ref mut elem) => {
                            elem.1.push(idx);
                        }
                        None => {}
                    }
                }
                BcOp::Br(_) | BcOp::Ret(_) => {}
                _ => match bb_info[idx + 1] {
                    Some(ref mut elem) => {
                        elem.1.push(idx);
                    }
                    None => {}
                },
            }
        }
        if bb_info[0].is_none() {
            bb_info[0] = Some((0, vec![]));
        }
        /*for (id, i, v) in bb_info.iter().enumerate().filter_map(|(i, e)| match e {
            None => None,
            Some((id, v)) => Some((id, i, v)),
        }) {
            eprintln!("{} {} {:?}", id, i, v);
        }*/
        bb_info
    }
}

impl RubyFuncInfo {
    pub(crate) fn get_index(&self, reg: &BcReg) -> SlotId {
        let id = match reg {
            BcReg::Self_ => 0,
            BcReg::Temp(i) => 1 + self.locals.len() as u16 + i.0,
            BcReg::Local(i) => 1 + i.0,
        };
        SlotId(id)
    }

    pub(crate) fn add_constsite(
        &self,
        store: &mut FnStore,
        name: IdentId,
        prefix: Vec<IdentId>,
        toplevel: bool,
    ) -> ConstSiteId {
        let info = ConstSiteInfo {
            name,
            prefix,
            toplevel,
            cache: (usize::MAX, None),
        };
        let id = store.constsite_info.len();
        store.constsite_info.push(info);
        ConstSiteId(id as u32)
    }
}
