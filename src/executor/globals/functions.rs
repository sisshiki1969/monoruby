use super::*;

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
    fn new(sourceinfo: SourceInfoRef) -> Self {
        Self(vec![FuncInfo::new_normal(
            None,
            FuncId(0),
            vec![],
            Node::new_nil(Loc(0, 0)),
            sourceinfo,
        )])
    }

    fn next_func_id(&self) -> FuncId {
        FuncId(self.0.len() as u32)
    }

    fn add_normal_func(
        &mut self,
        name: Option<String>,
        args: Vec<String>,
        ast: Node,
        sourceinfo: SourceInfoRef,
    ) -> FuncId {
        let fid = self.next_func_id();
        self.0
            .push(FuncInfo::new_normal(name, fid, args, ast, sourceinfo));
        fid
    }

    fn add_builtin_func(&mut self, name: String, address: BuiltinFn, arity: i32) -> FuncId {
        let id = self.next_func_id();
        self.0.push(FuncInfo::new_builtin(id, name, address, arity));
        id
    }
}

#[derive(Debug, Clone, Default, PartialEq)]
pub struct CallsiteInfo {
    /// Return register. 0 for none.
    pub ret: u16,
    /// Name of function.
    pub name: IdentId,
    /// Argument register.
    pub args: u16,
    /// Length of arguments.
    pub len: u16,
    /// Inline method cache.
    pub cache: (usize, ClassId, FuncId), //(version, class_id, func_id)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
pub struct CallsiteId(pub u32);

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

#[derive(Debug, Clone, Default, PartialEq)]
pub struct MethodDefInfo {
    pub name: IdentId,
    pub func: FuncId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
pub struct MethodDefId(pub u32);

#[derive(Clone, PartialEq)]
pub struct FnStore {
    functions: Funcs,
    pub main: Option<FuncId>,
    /// method define info.
    method_def_info: Vec<MethodDefInfo>,
    /// callsite info.
    callsite_info: Vec<CallsiteInfo>,
    /// const access site info.
    constsite_info: Vec<ConstSiteInfo>,
    /// literal values.
    literals: Vec<Value>,
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

impl std::ops::Index<MethodDefId> for FnStore {
    type Output = MethodDefInfo;
    fn index(&self, index: MethodDefId) -> &MethodDefInfo {
        &self.method_def_info[index.0 as usize]
    }
}

impl std::ops::Index<CallsiteId> for FnStore {
    type Output = CallsiteInfo;
    fn index(&self, index: CallsiteId) -> &CallsiteInfo {
        &self.callsite_info[index.0 as usize]
    }
}

impl std::ops::IndexMut<CallsiteId> for FnStore {
    fn index_mut(&mut self, index: CallsiteId) -> &mut CallsiteInfo {
        &mut self.callsite_info[index.0 as usize]
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
            functions: Funcs::new(SourceInfoRef::default()),
            main: None,
            method_def_info: vec![],
            callsite_info: vec![],
            constsite_info: vec![],
            literals: vec![],
        }
    }

    fn len(&self) -> usize {
        self.functions.0.len()
    }

    pub fn funcs_mut(&mut self) -> &mut Vec<FuncInfo> {
        &mut self.functions.0
    }

    pub(crate) fn add_normal_func(
        &mut self,
        name: Option<String>,
        args: Vec<String>,
        ast: Node,
        sourceinfo: SourceInfoRef,
    ) -> FuncId {
        self.functions.add_normal_func(name, args, ast, sourceinfo)
    }

    pub(crate) fn add_method_def(&mut self, name: IdentId, func: FuncId) -> MethodDefId {
        let info = MethodDefInfo { name, func };
        let id = self.method_def_info.len();
        self.method_def_info.push(info);
        MethodDefId(id as u32)
    }

    /// get a value of literal.
    pub(crate) fn get_literal(&self, id: u32) -> Value {
        self.literals[id as usize]
    }

    /// register a new literal.
    pub(crate) fn new_literal(&mut self, val: Value) -> u32 {
        let constants = self.literals.len();
        self.literals.push(val);
        constants as u32
    }
}

impl FnStore {
    pub(super) fn compile_script(
        &mut self,
        ast: Node,
        id_store: &mut IdentifierTable,
        sourceinfo: SourceInfoRef,
    ) -> Result<()> {
        let mut fid = self.functions.add_normal_func(
            Some("/main".to_string()),
            vec![],
            ast,
            sourceinfo.clone(),
        );
        self.main = Some(fid);

        while self.len() > fid.0 as usize {
            self.compile_func(fid, id_store)?;
            fid = FuncId(fid.0 + 1);
        }

        Ok(())
    }

    /// Generate bytecode for a function which has *func_id*.
    fn compile_func(&mut self, func_id: FuncId, id_store: &mut IdentifierTable) -> Result<()> {
        let mut info = std::mem::take(self[func_id].as_normal_mut());
        let mut ir = IrContext::compile_ast(&mut info, self, id_store)?;
        ir.ir_to_bytecode(&mut info, self);
        #[cfg(feature = "emit-bc")]
        info.dump(id_store, self);
        let regs = info.total_reg_num();
        std::mem::swap(&mut info, self[func_id].as_normal_mut());
        self[func_id].data.pc = BcPcBase::new(self[func_id].as_normal()) + 0;
        self[func_id].data.reg_num = regs as i64;
        Ok(())
    }

    pub(super) fn add_builtin_func(
        &mut self,
        name: String,
        address: BuiltinFn,
        arity: i32,
    ) -> FuncId {
        self.functions.add_builtin_func(name, address, arity)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum FuncKind {
    Normal(NormalFuncInfo),
    Builtin { abs_address: u64 },
}

impl std::default::Default for FuncKind {
    fn default() -> Self {
        Self::Builtin { abs_address: 0 }
    }
}

pub const FUNCDATA_OFFSET_REGNUM: u64 = 0;
pub const FUNCDATA_OFFSET_CODEPTR: u64 = 8;
pub const FUNCDATA_OFFSET_PC: u64 = 16;
pub const FUNCDATA_OFFSET_FUNCID: u64 = 24;

///
/// Metadata.
///
/// ```
/// let meta = Meta::from(FuncId(12), 42);
/// assert_eq!(FuncId(12), meta.func_id());
/// assert_eq!(42, meta.reg_num());
///
/// let meta = Meta::from(FuncId(42), -1);
/// assert_eq!(FuncId(42), meta.func_id());
/// assert_eq!(-1, meta.reg_num());
/// ```
#[derive(Debug, Clone, PartialEq, Default)]
#[repr(transparent)]
pub struct Meta(u64);

impl Meta {
    pub fn from(func_id: FuncId, reg_num: i64) -> Self {
        Self(((reg_num as i16 as u16 as u64) << 32) + (func_id.0 as u64))
    }

    pub fn func_id(&self) -> FuncId {
        FuncId(self.0 as u32)
    }

    pub fn reg_num(&self) -> i64 {
        (self.0 >> 32) as u16 as i16 as i64
    }

    pub fn set_reg_num(&mut self, reg_num: i64) {
        let meta = (self.0 & 0xffff_0000_ffff_ffff) | (reg_num as i16 as u16 as u64) << 32;
        self.0 = meta;
    }
}

#[test]
fn meta_test() {
    let meta = Meta::from(FuncId(12), 42);
    assert_eq!(FuncId(12), meta.func_id());
    assert_eq!(42, meta.reg_num());
    let mut meta = Meta::from(FuncId(42), -1);
    assert_eq!(FuncId(42), meta.func_id());
    assert_eq!(-1, meta.reg_num());
    meta.set_reg_num(12);
    assert_eq!(FuncId(42), meta.func_id());
    assert_eq!(12, meta.reg_num());
}

#[derive(Debug, Clone, PartialEq, Default)]
#[repr(C)]
pub struct FuncData {
    /// stack offset. (only used in calling vm_entry)
    pub reg_num: i64,
    /// address of function.
    pub codeptr: Option<CodePtr>,
    /// the address of program counter
    pub pc: BcPc,
    /// ID of this function.
    pub func_id: FuncId,
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
    fn new_normal(
        name: Option<String>,
        func_id: FuncId,
        args: Vec<String>,
        ast: Node,
        sourceinfo: SourceInfoRef,
    ) -> Self {
        let info = NormalFuncInfo::new(func_id, name.clone(), args, ast, sourceinfo);
        Self {
            name,
            arity: info.args.len() as i32,
            data: FuncData {
                reg_num: 0,
                codeptr: None,
                pc: BcPc::default(),
                func_id: info.id,
            },
            kind: FuncKind::Normal(info),
        }
    }

    fn new_builtin(func_id: FuncId, name: String, address: BuiltinFn, arity: i32) -> Self {
        let reg_num = if arity == -1 { -1 } else { arity as i64 };
        Self {
            name: Some(name),
            arity,
            data: FuncData {
                reg_num,
                codeptr: None,
                pc: BcPc::default(),
                func_id,
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

    pub(crate) fn as_normal(&self) -> &NormalFuncInfo {
        match &self.kind {
            FuncKind::Normal(info) => info,
            FuncKind::Builtin { .. } => unreachable!(),
        }
    }

    pub(crate) fn as_normal_mut(&mut self) -> &mut NormalFuncInfo {
        match &mut self.kind {
            FuncKind::Normal(info) => info,
            FuncKind::Builtin { .. } => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, Default, PartialEq)]
pub(crate) struct NormalFuncInfo {
    /// ID of this function.
    pub(super) id: FuncId,
    name: Option<String>,
    /// Bytecode.
    pub bytecode: Vec<u64>,
    /// Source map.
    pub sourcemap: Vec<Loc>,
    /// the name of arguments.
    args: Vec<String>,
    /// local variables.
    locals: HashMap<String, u16>,
    /// The current register id.
    pub temp: u16,
    /// The number of temporary registers.
    reg_num: u16,
    /// AST.
    pub ast: Option<Node>,
    pub sourceinfo: SourceInfoRef,
}

impl NormalFuncInfo {
    pub(crate) fn new(
        id: FuncId,
        name: Option<String>,
        args: Vec<String>,
        ast: Node,
        sourceinfo: SourceInfoRef,
    ) -> Self {
        let mut info = NormalFuncInfo {
            id,
            name,
            bytecode: vec![],
            sourcemap: vec![],
            args: args.clone(),
            locals: HashMap::default(),
            temp: 0,
            reg_num: 0,
            ast: Some(ast),
            sourceinfo,
        };
        args.into_iter().for_each(|name| {
            info.add_local(name);
        });
        info
    }

    /// get a number of registers.
    pub(crate) fn total_reg_num(&self) -> usize {
        1 + self.locals.len() + self.reg_num as usize
    }

    /// get bytecode.
    pub(crate) fn bytecode(&self) -> &Vec<u64> {
        &self.bytecode
    }

    /// get the next register id.
    pub(crate) fn next_reg(&self) -> BcTemp {
        BcTemp(self.temp)
    }

    pub(crate) fn push(&mut self) -> BcTemp {
        let reg = BcTemp(self.temp);
        self.temp += 1;
        if self.temp > self.reg_num {
            self.reg_num = self.temp;
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

    pub(crate) fn load_local(&mut self, ident: &str, loc: Loc) -> Result<BcLocal> {
        match self.locals.get(ident) {
            Some(local) => Ok(BcLocal(*local)),
            None => Err(MonorubyErr::undefined_local(
                ident.to_owned(),
                loc,
                self.sourceinfo.clone(),
            )),
        }
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

    #[cfg(feature = "emit-bc")]
    pub(crate) fn dump(&self, id_store: &IdentifierTable, store: &FnStore) {
        eprintln!("------------------------------------");
        eprintln!(
            "{:?} name:{} args:{:?} bc:{:?}",
            self.id,
            match &self.name {
                Some(name) => name,
                None => "<ANONYMOUS>",
            },
            self.args.iter().collect::<Vec<_>>(),
            BcPcBase::new(self)
        );
        for (i, inst) in self.bytecode.iter().enumerate() {
            eprint!(":{:05} ", i);
            match BcOp::from_u64(*inst) {
                BcOp::Br(disp) => {
                    eprintln!("br =>:{:05}", i as i32 + 1 + disp);
                }
                BcOp::CondBr(reg, disp) => {
                    eprintln!("condbr %{} =>:{:05}", reg, i as i32 + 1 + disp);
                }
                BcOp::CondNotBr(reg, disp) => {
                    eprintln!("condnbr %{} =>:{:05}", reg, i as i32 + 1 + disp);
                }
                BcOp::CondBrO(reg, disp) => {
                    eprintln!("condbr _%{} =>:{:05}", reg, i as i32 + 1 + disp);
                }
                BcOp::CondNotBrO(reg, disp) => {
                    eprintln!("condnbr _%{} =>:{:05}", reg, i as i32 + 1 + disp);
                }
                BcOp::Integer(reg, num) => eprintln!("%{} = {}: i32", reg, num),
                BcOp::Symbol(reg, id) => eprintln!("%{} = :{}", reg, id_store.get_name(id)),
                BcOp::Literal(reg, id) => {
                    let v = store.get_literal(id);
                    eprintln!("%{} = literal[{:?}]", reg, v)
                }
                BcOp::LoadConst(reg, id) => {
                    let name = store[id].name;
                    eprintln!("%{} = const[{}]", reg, id_store.get_name(name))
                }
                BcOp::StoreConst(reg, id) => {
                    eprintln!("const[{}] = %{}", id_store.get_name(id), reg)
                }
                BcOp::Nil(reg) => eprintln!("%{} = nil", reg),
                BcOp::Neg(dst, src) => eprintln!("%{} = neg %{}", dst, src),
                BcOp::Add(dst, lhs, rhs) => eprintln!("%{} = %{} + %{}", dst, lhs, rhs),
                BcOp::Addri(dst, lhs, rhs) => {
                    eprintln!("%{} = %{} + {}: i16", dst, lhs, rhs)
                }
                BcOp::Sub(dst, lhs, rhs) => eprintln!("%{} = %{} - %{}", dst, lhs, rhs),
                BcOp::Subri(dst, lhs, rhs) => {
                    eprintln!("%{} = %{} - {}: i16", dst, lhs, rhs)
                }
                BcOp::Mul(dst, lhs, rhs) => eprintln!("%{} = %{} * %{}", dst, lhs, rhs),
                BcOp::Div(dst, lhs, rhs) => eprintln!("%{} = %{} / %{}", dst, lhs, rhs),
                BcOp::BitOr(dst, lhs, rhs) => eprintln!("%{} = %{} | %{}", dst, lhs, rhs),
                BcOp::BitAnd(dst, lhs, rhs) => eprintln!("%{} = %{} & %{}", dst, lhs, rhs),
                BcOp::BitXor(dst, lhs, rhs) => eprintln!("%{} = %{} ^ %{}", dst, lhs, rhs),
                BcOp::Shr(dst, lhs, rhs) => eprintln!("%{} = %{} >> %{}", dst, lhs, rhs),
                BcOp::Shl(dst, lhs, rhs) => eprintln!("%{} = %{} << %{}", dst, lhs, rhs),
                BcOp::Cmp(kind, dst, lhs, rhs) => {
                    eprintln!("%{} = %{} {:?} %{}", dst, lhs, kind, rhs)
                }
                BcOp::Cmpri(kind, dst, lhs, rhs) => {
                    eprintln!("%{} = %{} {:?} {}: i16", dst, lhs, kind, rhs)
                }
                BcOp::CmpO(kind, dst, lhs, rhs) => {
                    eprintln!("_%{} = %{} {:?} %{}", dst, lhs, kind, rhs)
                }
                BcOp::CmpriO(kind, dst, lhs, rhs) => {
                    eprintln!("_%{} = %{} {:?} {}: i16", dst, lhs, kind, rhs)
                }

                BcOp::Ret(reg) => eprintln!("ret %{}", reg),
                BcOp::Mov(dst, src) => eprintln!("%{} = %{}", dst, src),
                BcOp::MethodCall(recv, id) => {
                    let CallsiteInfo {
                        ret,
                        name,
                        args,
                        len,
                        cache: _,
                    } = store[id];
                    let name = id_store.get_name(name);
                    match ret {
                        0 => {
                            eprintln!("_ = %{}.call {}(%{}; {})", recv, name, args, len)
                        }
                        ret => {
                            eprintln!("%{:?} = %{}.call {}(%{}; {})", ret, recv, name, args, len)
                        }
                    }
                }
                BcOp::MethodDef(id) => {
                    let MethodDefInfo { name, func } = store[id];
                    let name = id_store.get_name(name);
                    eprintln!("define {:?}: {:?}", name, func)
                }
                BcOp::ConcatStr(ret, args, len) => match ret {
                    0 => eprintln!("_ = concat(%{}; {})", args, len),
                    ret => eprintln!("%{:?} = concat(%{}; {})", ret, args, len),
                },
            }
        }
        eprintln!("------------------------------------");
    }
}

impl NormalFuncInfo {
    pub(crate) fn get_index(&self, reg: &BcReg) -> u16 {
        match reg {
            BcReg::Self_ => 0,
            BcReg::Temp(i) => 1 + self.locals.len() as u16 + i.0,
            BcReg::Local(i) => 1 + i.0,
        }
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

    pub(crate) fn add_callsite(
        &self,
        store: &mut FnStore,
        ret: Option<BcReg>,
        name: IdentId,
        args: BcTemp,
        len: usize,
    ) -> CallsiteId {
        let info = CallsiteInfo {
            ret: match ret {
                None => 0,
                Some(ret) => self.get_index(&ret),
            },
            name,
            args: self.get_index(&BcReg::from(args)),
            len: len as u16,
            cache: (usize::MAX, ClassId::default(), FuncId::default()),
        };
        let id = store.callsite_info.len();
        store.callsite_info.push(info);
        CallsiteId(id as u32)
    }
}
