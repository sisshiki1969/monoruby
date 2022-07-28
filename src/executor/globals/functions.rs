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
            constsite_info: vec![],
            literals: vec![],
        }
    }

    pub fn functions(&self) -> &Vec<FuncInfo> {
        &self.functions.0
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

        let regs = info.total_reg_num();
        std::mem::swap(&mut info, self[func_id].as_normal_mut());
        self[func_id].data.pc = self[func_id].as_normal().get_bytecode_address(0);
        self[func_id].data.set_reg_num(regs as i64);
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

pub const FUNCDATA_OFFSET_CODEPTR: u64 = 0;
pub const FUNCDATA_OFFSET_PC: u64 = 8;
pub const FUNCDATA_OFFSET_META: u64 = 16;

///
/// Metadata.
///~~~
///   7   6   5   4    3   2    1   0
/// +-------+-------+---------+--------+
/// |    FuncId     | reg_num |  kind  |
/// +-------+-------+---------+--------+
///~~~
#[derive(Clone, Copy, PartialEq, Default)]
#[repr(transparent)]
pub struct Meta(u64);

impl std::fmt::Debug for Meta {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} {:?} regs:{}",
            match self.kind() {
                0 => "VM",
                1 => "JIT",
                2 => "NATIVE",
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

    pub fn from(func_id: FuncId, reg_num: i64) -> Self {
        // kind = VM
        Self(((reg_num as i16 as u16 as u64) << 32) + (func_id.0 as u64))
    }

    pub fn native(func_id: FuncId, reg_num: i64) -> Self {
        Self((2 << 48) + ((reg_num as i16 as u16 as u64) << 32) + (func_id.0 as u64))
    }

    pub fn func_id(&self) -> FuncId {
        FuncId(self.0 as u32)
    }

    pub fn reg_num(&self) -> i64 {
        (self.0 >> 32) as u16 as i16 as i64
    }

    pub fn kind(&self) -> u16 {
        (self.0 >> 48) as u16
    }

    pub fn set_jit(&mut self) {
        let meta = (self.0 & 0x0000_ffff_ffff_ffff) | (1 << 48);
        self.0 = meta;
    }

    pub fn set_reg_num(&mut self, reg_num: i64) {
        let meta = (self.0 & 0xffff_0000_ffff_ffff) | ((reg_num as i16 as u16 as u64) << 32);
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
                codeptr: None,
                pc: BcPc::default(),
                meta: Meta::from(info.id, 0),
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
    pub(crate) id: FuncId,
    name: Option<String>,
    /// Bytecode.
    pub(super) bytecode: Box<[Bc]>,
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
            bytecode: Box::new([]),
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
        self.bytecode = bc.into_boxed_slice();
    }

    /// get bytecode address.
    pub(crate) fn get_bytecode_address(&self, index: usize) -> BcPc {
        BcPcBase::new(self) + index
    }

    /// get a number of registers.
    pub(crate) fn total_reg_num(&self) -> usize {
        1 + self.locals.len() + self.temp_num as usize
    }

    /// get name.
    #[cfg(any(feature = "emit-asm", feature = "log-jit"))]
    pub(crate) fn name(&self) -> &Option<String> {
        &self.name
    }

    /// get bytecode.
    pub(crate) fn bytecode(&self) -> &[Bc] {
        &self.bytecode
    }

    /// get bytecode address.
    pub(crate) fn bytecode_top(&self) -> *const Bc {
        self.bytecode.as_ptr()
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

    pub(crate) fn get_bb_info(&self) -> Vec<Option<usize>> {
        let mut info = vec![None; self.bytecode().len()];
        let mut skip = false;
        for (idx, op) in self.bytecode().iter().enumerate() {
            if skip {
                skip = false;
                continue;
            }
            let ops = BcOp1::from_bc(*op);
            match ops {
                BcOp1::MethodArgs(..) => {
                    skip = true;
                }
                BcOp1::Br(disp) | BcOp1::CondBr(_, disp, _, _) => {
                    info[((idx + 1) as i32 + disp) as usize] = Some(0);
                }
                _ => {}
            }
        }
        let mut bb_id = 0;
        info.into_iter()
            .map(|e| {
                e.map(|_| {
                    let id = bb_id;
                    bb_id += 1;
                    id
                })
            })
            .collect()
    }

    #[cfg(feature = "emit-bc")]
    pub(crate) fn dump_bc(&self, globals: &Globals) {
        fn optstr(opt: bool) -> &'static str {
            if opt {
                "_"
            } else {
                ""
            }
        }
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
        let mut buf = None;
        let mut skip = false;
        for (i, inst) in self.bytecode.iter().enumerate() {
            if skip {
                skip = false;
                continue;
            }
            let bcop1 = BcOp1::from_bc(*inst);
            if let BcOp1::MethodArgs(..) = bcop1 {
            } else {
                eprint!(":{:05} ", i)
            };
            match bcop1 {
                BcOp1::Br(disp) => {
                    eprintln!("br =>:{:05}", i as i32 + 1 + disp);
                }
                BcOp1::CondBr(reg, disp, opt, kind) => {
                    eprintln!(
                        "cond{}br {}%{} =>:{:05}",
                        kind.to_s(),
                        optstr(opt),
                        reg,
                        i as i32 + 1 + disp
                    );
                }
                BcOp1::Integer(reg, num) => eprintln!("%{} = {}: i32", reg, num),
                BcOp1::Symbol(reg, id) => {
                    eprintln!("%{} = :{}", reg, globals.id_store.get_name(id))
                }
                BcOp1::Literal(reg, id) => {
                    let v = globals.func.get_literal(id);
                    eprintln!("%{} = literal[{}]", reg, globals.val_inspect(v))
                }
                BcOp1::LoadConst(reg, id) => {
                    let name = globals.func[id].name;
                    eprintln!("%{} = const[{}]", reg, globals.id_store.get_name(name))
                }
                BcOp1::StoreConst(reg, id) => {
                    eprintln!("const[{}] = %{}", globals.id_store.get_name(id), reg)
                }
                BcOp1::Nil(reg) => eprintln!("%{} = nil", reg),
                BcOp1::Neg(dst, src) => {
                    let op1 = format!("%{} = neg %{}", dst, src);
                    eprintln!("{:36} [{}]", op1, inst.classid().get_name(globals),);
                }
                BcOp1::BinOp(kind, dst, lhs, rhs) => {
                    let op1 = format!("%{} = %{} {} %{}", dst, lhs, kind, rhs);
                    eprintln!(
                        "{:36} [{}][{}]",
                        op1,
                        inst.classid().get_name(globals),
                        inst.classid2().get_name(globals)
                    );
                }
                BcOp1::BinOpRi(kind, dst, lhs, rhs) => {
                    let op1 = format!("%{} = %{} {} {}: i16", dst, lhs, kind, rhs,);
                    eprintln!(
                        "{:36} [{}][{}]",
                        op1,
                        inst.classid().get_name(globals),
                        inst.classid2().get_name(globals)
                    );
                }
                BcOp1::Cmp(kind, dst, lhs, rhs, opt) => {
                    let op1 = format!("{}%{} = %{} {:?} %{}", optstr(opt), dst, lhs, kind, rhs,);
                    eprintln!(
                        "{:36} [{}][{}]",
                        op1,
                        inst.classid().get_name(globals),
                        inst.classid2().get_name(globals)
                    );
                }
                BcOp1::Cmpri(kind, dst, lhs, rhs, opt) => {
                    let op1 =
                        format!("{}%{} = %{} {:?} {}: i16", optstr(opt), dst, lhs, kind, rhs,);
                    eprintln!(
                        "{:36} [{}][{}]",
                        op1,
                        inst.classid().get_name(globals),
                        inst.classid2().get_name(globals)
                    );
                }

                BcOp1::Ret(reg) => eprintln!("ret %{}", reg),
                BcOp1::Mov(dst, src) => eprintln!("%{} = %{}", dst, src),
                BcOp1::MethodCall(..) => {
                    assert!(buf.is_none());
                    buf = Some((bcop1.clone(), inst.classid()));
                }
                BcOp1::MethodArgs(recv, args, len) => {
                    let (recv, ret, name, class_id) = match std::mem::take(&mut buf).unwrap() {
                        (BcOp1::MethodCall(ret, name), class_id) => (recv, ret, name, class_id),
                        _ => unreachable!(),
                    };
                    let name = globals.id_store.get_name(name);
                    let op1 = format!(
                        "{} = %{}.call {}(%{}; {})",
                        match ret {
                            SlotId(0) => "_".to_string(),
                            ret => format!("%{:?}", ret),
                        },
                        recv,
                        name,
                        args,
                        len,
                    );
                    eprintln!("{:36} [{}]", op1, class_id.get_name(globals));
                    skip = true;
                }
                BcOp1::MethodDef(id) => {
                    let MethodDefInfo { name, func } = globals.func[id];
                    let name = globals.id_store.get_name(name);
                    eprintln!("define {:?}: {:?}", name, func)
                }
                BcOp1::ConcatStr(ret, args, len) => match ret {
                    SlotId(0) => eprintln!("_ = concat(%{}; {})", args, len),
                    ret => eprintln!("%{:?} = concat(%{}; {})", ret, args, len),
                },
                BcOp1::LoopStart(count) => eprintln!(
                    "loop_start counter={} jit-addr={:016x}",
                    count,
                    Bc2::from_jit_addr(*inst)
                ),
                BcOp1::LoopEnd => eprintln!("loop_end"),
            }
        }
        eprintln!("------------------------------------");
    }
}

impl NormalFuncInfo {
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
