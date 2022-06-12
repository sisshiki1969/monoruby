use super::*;
use crate::executor::interp::{BcPc, BcPcBase};
use num::BigInt;
use paste::paste;

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

    fn add_builtin_func(&mut self, name: String, address: BuiltinFn, arity: usize) -> FuncId {
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

    fn add_method_def(&mut self, name: IdentId, func: FuncId) -> MethodDefId {
        let info = MethodDefInfo { name, func };
        let id = self.method_def_info.len();
        self.method_def_info.push(info);
        MethodDefId(id as u32)
    }

    /// get a value of literal.
    pub(super) fn get_literal(&self, id: u32) -> Value {
        self.literals[id as usize]
    }

    /// register a new literal.
    fn new_literal(&mut self, val: Value) -> u32 {
        let constants = self.literals.len();
        self.literals.push(val);
        constants as u32
    }

    pub fn precompile(&mut self, jit: &mut JitGen, vm_entry: CodePtr) {
        for func in &mut self.functions.0 {
            match &func.kind {
                FuncKind::Normal(_) => {
                    func.set_jit_label(vm_entry);
                }
                FuncKind::Builtin { abs_address } => {
                    let label = jit.wrap_builtin(*abs_address, func.arity());
                    func.set_jit_label(label);
                }
            };
        }
    }
}

impl FnStore {
    pub(super) fn compile_script(
        &mut self,
        ast: Node,
        id_store: &mut IdentifierTable,
        sourceinfo: SourceInfoRef,
    ) -> Result<()> {
        let mut fid = self
            .functions
            .add_normal_func(None, vec![], ast, sourceinfo.clone());
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
        let ir = info.compile_ast(self, id_store)?;
        info.ir_to_bytecode(ir, self);
        #[cfg(feature = "emit-bc")]
        info.dump(id_store, self);
        let regs = info.total_reg_num();
        std::mem::swap(&mut info, self[func_id].as_normal_mut());
        self[func_id].inst_pc = BcPcBase::new(self[func_id].as_normal()) + 0;
        self[func_id].stack_offset = (regs + regs % 2) * 8 + 16;
        Ok(())
    }

    pub(super) fn add_builtin_func(
        &mut self,
        name: String,
        address: BuiltinFn,
        arity: usize,
    ) -> FuncId {
        self.functions.add_builtin_func(name, address, arity)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(super) enum FuncKind {
    Normal(NormalFuncInfo),
    Builtin { abs_address: u64 },
}

impl std::default::Default for FuncKind {
    fn default() -> Self {
        Self::Builtin { abs_address: 0 }
    }
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct FuncInfo {
    /// ID of this function.
    id: FuncId,
    /// name of this function.
    name: Option<String>,
    /// arity of this function.
    arity: usize,
    /// address of JIT function.
    jit_label: Option<CodePtr>,
    /// stack offset
    stack_offset: usize,
    /// the address of program counter
    inst_pc: BcPc,
    pub(super) kind: FuncKind,
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
            id: info.id,
            name,
            arity: info.args.len(),
            jit_label: None,
            stack_offset: 0,
            inst_pc: BcPc::default(),
            kind: FuncKind::Normal(info),
        }
    }

    fn new_builtin(id: FuncId, name: String, address: BuiltinFn, arity: usize) -> Self {
        Self {
            id,
            name: Some(name),
            arity,
            jit_label: None,
            stack_offset: (arity + arity % 2) * 8 + 16,
            inst_pc: BcPc::default(),
            kind: FuncKind::Builtin {
                abs_address: address as *const u8 as u64,
            },
        }
    }

    pub(super) fn id(&self) -> FuncId {
        self.id
    }

    pub(super) fn arity(&self) -> usize {
        self.arity
    }

    pub(super) fn stack_offset(&self) -> usize {
        self.stack_offset
    }

    pub(super) fn inst_pc(&self) -> BcPc {
        self.inst_pc
    }

    pub(super) fn jit_label(&self) -> Option<CodePtr> {
        self.jit_label
    }

    pub(super) fn set_jit_label(&mut self, label: CodePtr) {
        self.jit_label = Some(label);
    }

    pub(super) fn as_normal(&self) -> &NormalFuncInfo {
        match &self.kind {
            FuncKind::Normal(info) => info,
            FuncKind::Builtin { .. } => unreachable!(),
        }
    }

    pub(super) fn as_normal_mut(&mut self) -> &mut NormalFuncInfo {
        match &mut self.kind {
            FuncKind::Normal(info) => info,
            FuncKind::Builtin { .. } => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
enum LoopKind {
    For,
    While,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct IrContext {
    /// bytecode IR.
    ir: Vec<(BcIr, Loc)>,
    /// destination labels.
    labels: Vec<Option<InstId>>,
    /// loop information.
    loops: Vec<(LoopKind, usize, Option<BcReg>)>, // (kind, label for exit, return register)
}

impl IrContext {
    pub(crate) fn new() -> Self {
        Self {
            ir: vec![],
            labels: vec![],
            loops: vec![],
        }
    }

    fn push(&mut self, op: BcIr, loc: Loc) {
        self.ir.push((op, loc));
    }

    /// get new destination label.
    fn new_label(&mut self) -> usize {
        let label = self.labels.len();
        self.labels.push(None);
        label
    }

    /// apply current instruction pointer to the destination label.
    fn apply_label(&mut self, label: usize) {
        let pos = InstId(self.ir.len() as u32);
        self.labels[label] = Some(pos);
    }

    fn gen_br(&mut self, cond_pos: usize) {
        self.push(BcIr::Br(cond_pos), Loc::default());
    }

    fn gen_condbr(&mut self, cond: BcReg, then_pos: usize) {
        self.push(BcIr::CondBr(cond, then_pos), Loc::default());
    }

    fn gen_condnotbr(&mut self, cond: BcReg, else_pos: usize) {
        self.push(BcIr::CondNotBr(cond, else_pos), Loc::default());
    }
}

#[derive(Debug, Clone, Default, PartialEq)]
pub(super) struct NormalFuncInfo {
    /// ID of this function.
    pub(super) id: FuncId,
    name: Option<String>,
    /// Bytecode.
    bytecode: Vec<u64>,
    /// Source map.
    pub sourcemap: Vec<Loc>,
    /// the name of arguments.
    args: Vec<String>,
    /// local variables.
    locals: HashMap<String, u16>,
    /// The current register id.
    temp: u16,
    /// The number of temporary registers.
    reg_num: u16,
    /// AST.
    ast: Option<Node>,
    pub sourceinfo: SourceInfoRef,
}

impl NormalFuncInfo {
    pub fn new(
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
    pub(super) fn total_reg_num(&self) -> usize {
        1 + self.locals.len() + self.reg_num as usize
    }

    /// get bytecode.
    pub(super) fn bytecode(&self) -> &Vec<u64> {
        &self.bytecode
    }

    /// get the next register id.
    fn next_reg(&self) -> BcTemp {
        BcTemp(self.temp)
    }

    fn push(&mut self) -> BcTemp {
        let reg = BcTemp(self.temp);
        self.temp += 1;
        if self.temp > self.reg_num {
            self.reg_num = self.temp;
        }
        reg
    }

    fn pop(&mut self) -> BcTemp {
        self.temp -= 1;
        BcTemp(self.temp)
    }

    fn popn(&mut self, len: usize) {
        self.temp -= len as u16;
    }

    fn load_local(&mut self, ident: &str, loc: Loc) -> Result<BcLocal> {
        match self.locals.get(ident) {
            Some(local) => Ok(BcLocal(*local)),
            None => Err(MonorubyErr::undefined_local(
                ident.to_owned(),
                loc,
                self.sourceinfo.clone(),
            )),
        }
    }

    fn find_local(&mut self, ident: &str) -> BcLocal {
        match self.locals.get(ident) {
            Some(local) => BcLocal(*local),
            None => self.add_local(ident.to_owned()),
        }
    }

    /// Add a variable identifier without checking duplicates.
    fn add_local(&mut self, ident: String) -> BcLocal {
        let local = self.locals.len() as u16;
        assert!(self.locals.insert(ident, local).is_none());
        BcLocal(local)
    }

    fn gen_load_const(
        &mut self,
        ir: &mut IrContext,
        dst: Option<BcLocal>,
        name: IdentId,
        loc: Loc,
    ) {
        let reg = match dst {
            Some(local) => local.into(),
            None => self.push().into(),
        };
        ir.push(BcIr::LoadConst(reg, name), loc);
    }

    fn gen_store_const(&mut self, ir: &mut IrContext, src: BcReg, name: IdentId, loc: Loc) {
        ir.push(BcIr::StoreConst(src, name), loc);
    }

    fn gen_literal(
        &mut self,
        ctx: &mut FnStore,
        ir: &mut IrContext,
        dst: Option<BcLocal>,
        v: Value,
    ) {
        let reg = match dst {
            Some(local) => local.into(),
            None => self.push().into(),
        };
        let id = ctx.new_literal(v);
        ir.push(BcIr::Literal(reg, id), Loc::default());
    }

    fn gen_integer(&mut self, ctx: &mut FnStore, ir: &mut IrContext, dst: Option<BcLocal>, i: i64) {
        if let Ok(i) = i32::try_from(i) {
            let reg = match dst {
                Some(local) => local.into(),
                None => self.push().into(),
            };
            ir.push(BcIr::Integer(reg, i), Loc::default());
        } else {
            self.gen_literal(ctx, ir, dst, Value::integer(i));
        }
    }

    fn gen_float(&mut self, ctx: &mut FnStore, ir: &mut IrContext, dst: Option<BcLocal>, f: f64) {
        self.gen_literal(ctx, ir, dst, Value::float(f));
    }

    fn gen_symbol(&mut self, ir: &mut IrContext, dst: Option<BcLocal>, sym: IdentId) {
        let reg = match dst {
            Some(local) => local.into(),
            None => self.push().into(),
        };
        ir.push(BcIr::Symbol(reg, sym), Loc::default());
    }

    fn gen_string(
        &mut self,
        ctx: &mut FnStore,
        ir: &mut IrContext,
        dst: Option<BcLocal>,
        b: Vec<u8>,
    ) {
        self.gen_literal(ctx, ir, dst, Value::string(b));
    }

    fn gen_bigint(
        &mut self,
        ctx: &mut FnStore,
        ir: &mut IrContext,
        dst: Option<BcLocal>,
        bigint: BigInt,
    ) {
        self.gen_literal(ctx, ir, dst, Value::bigint(bigint));
    }

    fn gen_nil(&mut self, ir: &mut IrContext, dst: Option<BcLocal>) {
        let reg = match dst {
            Some(local) => local.into(),
            None => self.push().into(),
        };
        ir.push(BcIr::Nil(reg), Loc::default());
    }

    fn gen_neg(&mut self, ir: &mut IrContext, local: Option<BcLocal>, loc: Loc) {
        match local {
            Some(local) => {
                let local = local.into();
                ir.push(BcIr::Neg(local, local), loc);
            }
            None => {
                let src = self.pop().into();
                let dst = self.push().into();
                ir.push(BcIr::Neg(dst, src), loc);
            }
        };
    }

    fn gen_ret(&mut self, ir: &mut IrContext, local: Option<BcLocal>) {
        let ret = match local {
            Some(local) => local.into(),
            None => self.pop().into(),
        };
        assert_eq!(0, self.temp);
        ir.push(BcIr::Ret(ret), Loc::default());
    }

    fn gen_mov(&mut self, ir: &mut IrContext, dst: BcReg, src: BcReg) {
        ir.push(BcIr::Mov(dst, src), Loc::default());
    }

    fn gen_temp_mov(&mut self, ir: &mut IrContext, rhs: BcReg) {
        let lhs = self.push();
        self.gen_mov(ir, lhs.into(), rhs);
    }

    fn dump(&self, id_store: &IdentifierTable, store: &FnStore) {
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
                BcOp::Integer(reg, num) => eprintln!("%{} = {}: i32", reg, num),
                BcOp::Symbol(reg, id) => eprintln!("%{} = :{}", reg, id_store.get_name(id)),
                BcOp::Literal(reg, id) => eprintln!("%{} = literal[{}]", reg, id),
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

pub fn is_smi(node: &Node) -> Option<i16> {
    if let NodeKind::Integer(i) = &node.kind {
        if *i == *i as i16 as i64 {
            return Some(*i as i16);
        }
    }
    None
}

pub fn is_local(node: &Node) -> Option<&String> {
    if let NodeKind::LocalVar(name) = &node.kind {
        Some(name)
    } else {
        None
    }
}

impl NormalFuncInfo {
    fn compile_ast(
        &mut self,
        ctx: &mut FnStore,
        id_store: &mut IdentifierTable,
    ) -> Result<IrContext> {
        let mut ir = IrContext::new();
        let ast = std::mem::take(&mut self.ast).unwrap();
        self.gen_expr(ctx, &mut ir, id_store, ast, true, true)?;
        assert_eq!(0, self.temp);
        Ok(ir)
    }

    fn gen_comp_stmts(
        &mut self,
        ctx: &mut FnStore,
        ir: &mut IrContext,
        id_store: &mut IdentifierTable,
        mut nodes: Vec<Node>,
        ret: Option<BcLocal>,
        use_value: bool,
        is_ret: bool,
    ) -> Result<()> {
        let last = match nodes.pop() {
            Some(node) => node,
            None => Node::new_nil(Loc(0, 0)),
        };
        for node in nodes.into_iter() {
            self.gen_expr(ctx, ir, id_store, node, false, false)?;
        }
        match ret {
            Some(ret) => {
                self.gen_store_expr(ctx, ir, id_store, ret, last, use_value)?;
                if is_ret {
                    self.gen_ret(ir, ret.into());
                }
            }
            None => {
                self.gen_expr(ctx, ir, id_store, last, use_value, is_ret)?;
            }
        }
        Ok(())
    }

    /// Generate bytecode Ir that evaluate *expr* and assign it to a temporary register.
    fn gen_temp_expr(
        &mut self,
        ctx: &mut FnStore,
        ir: &mut IrContext,
        id_store: &mut IdentifierTable,
        expr: Node,
    ) -> Result<BcTemp> {
        self.gen_expr(ctx, ir, id_store, expr, true, false)?;
        Ok(self.pop())
    }

    /// Generate bytecode Ir for binary operations.
    fn gen_binop(
        &mut self,
        ctx: &mut FnStore,
        ir: &mut IrContext,
        id_store: &mut IdentifierTable,
        op: BinOp,
        lhs: Node,
        rhs: Node,
        dst: Option<BcLocal>,
        loc: Loc,
    ) -> Result<()> {
        match op {
            BinOp::Add => self.gen_add(ctx, ir, id_store, dst, lhs, rhs, loc)?,
            BinOp::Sub => self.gen_sub(ctx, ir, id_store, dst, lhs, rhs, loc)?,
            BinOp::Mul => self.gen_mul(ctx, ir, id_store, dst, lhs, rhs, loc)?,
            BinOp::Div => self.gen_div(ctx, ir, id_store, dst, lhs, rhs, loc)?,
            BinOp::BitOr => self.gen_bitor(ctx, ir, id_store, dst, lhs, rhs, loc)?,
            BinOp::BitAnd => self.gen_bitand(ctx, ir, id_store, dst, lhs, rhs, loc)?,
            BinOp::BitXor => self.gen_bitxor(ctx, ir, id_store, dst, lhs, rhs, loc)?,
            BinOp::Shr => self.gen_shr(ctx, ir, id_store, dst, lhs, rhs, loc)?,
            BinOp::Shl => self.gen_shl(ctx, ir, id_store, dst, lhs, rhs, loc)?,
            BinOp::Eq => self.gen_cmp(ctx, ir, id_store, dst, CmpKind::Eq, lhs, rhs, loc)?,
            BinOp::Ne => self.gen_cmp(ctx, ir, id_store, dst, CmpKind::Ne, lhs, rhs, loc)?,
            BinOp::Ge => self.gen_cmp(ctx, ir, id_store, dst, CmpKind::Ge, lhs, rhs, loc)?,
            BinOp::Gt => self.gen_cmp(ctx, ir, id_store, dst, CmpKind::Gt, lhs, rhs, loc)?,
            BinOp::Le => self.gen_cmp(ctx, ir, id_store, dst, CmpKind::Le, lhs, rhs, loc)?,
            BinOp::Lt => self.gen_cmp(ctx, ir, id_store, dst, CmpKind::Lt, lhs, rhs, loc)?,
            _ => {
                return Err(MonorubyErr::unsupported_operator(
                    op,
                    loc,
                    self.sourceinfo.clone(),
                ))
            }
        };
        Ok(())
    }

    /// Generate bytecode Ir for *expr*.
    fn gen_expr(
        &mut self,
        ctx: &mut FnStore,
        ir: &mut IrContext,
        id_store: &mut IdentifierTable,
        expr: Node,
        use_value: bool,
        is_ret: bool,
    ) -> Result<()> {
        if !use_value {
            match &expr.kind {
                NodeKind::Nil
                | NodeKind::Bool(_)
                | NodeKind::SelfValue
                | NodeKind::Integer(_)
                | NodeKind::Float(_) => return Ok(()),
                _ => {}
            }
        }
        let loc = expr.loc;
        match expr.kind {
            NodeKind::Nil => self.gen_nil(ir, None),
            NodeKind::Bool(b) => self.gen_literal(ctx, ir, None, Value::bool(b)),
            NodeKind::SelfValue => self.gen_temp_mov(ir, BcReg::Self_),
            NodeKind::Integer(i) => {
                self.gen_integer(ctx, ir, None, i);
            }
            NodeKind::Symbol(sym) => {
                let sym = id_store.get_ident_id_from_string(sym);
                self.gen_symbol(ir, None, sym);
            }
            NodeKind::Bignum(bigint) => self.gen_bigint(ctx, ir, None, bigint),
            NodeKind::Float(f) => self.gen_float(ctx, ir, None, f),
            NodeKind::String(s) => self.gen_string(ctx, ir, None, s.into_bytes()),
            NodeKind::UnOp(op, box rhs) => {
                assert!(op == UnOp::Neg);
                match rhs.kind {
                    //NodeKind::Integer(i) => self.gen_integer(ctx, ir, None, -i),
                    NodeKind::Float(f) => self.gen_float(ctx, ir, None, -f),
                    _ => {
                        self.gen_expr(ctx, ir, id_store, rhs, true, false)?;
                        self.gen_neg(ir, None, loc);
                    }
                };
            }
            NodeKind::AssignOp(op, box lhs, box rhs) => {
                match &lhs.kind {
                    NodeKind::LocalVar(lhs_) | NodeKind::Ident(lhs_) => {
                        let local = self.find_local(lhs_);
                        self.gen_binop(ctx, ir, id_store, op, lhs, rhs, Some(local), loc)?;
                        if is_ret {
                            self.gen_ret(ir, Some(local));
                        } else if use_value {
                            self.gen_temp_mov(ir, local.into());
                        }
                        return Ok(());
                    }
                    NodeKind::Const { toplevel, name } => {
                        assert!(!toplevel);
                        let name = id_store.get_ident_id(name);
                        let src = self.next_reg();
                        let lhs_loc = lhs.loc;
                        self.gen_binop(ctx, ir, id_store, op, lhs, rhs, None, loc)?;
                        self.gen_store_const(ir, src.into(), name, lhs_loc);
                    }
                    _ => return Err(MonorubyErr::unsupported_lhs(lhs, self.sourceinfo.clone())),
                };
            }
            NodeKind::BinOp(op, box lhs, box rhs) => {
                self.gen_binop(ctx, ir, id_store, op, lhs, rhs, None, loc)?
            }
            NodeKind::MulAssign(mut mlhs, mut mrhs) => {
                if mlhs.len() == 1 && mrhs.len() == 1 {
                    let (lhs, rhs) = (mlhs.remove(0), mrhs.remove(0));
                    match lhs.kind {
                        NodeKind::LocalVar(lhs) | NodeKind::Ident(lhs) => {
                            let local = self.find_local(&lhs);
                            if is_ret {
                                self.gen_store_expr(ctx, ir, id_store, local, rhs, false)?;
                                self.gen_ret(ir, Some(local));
                            } else {
                                self.gen_store_expr(ctx, ir, id_store, local, rhs, use_value)?;
                            }
                            return Ok(());
                        }
                        NodeKind::Const { toplevel, name } => {
                            assert!(!toplevel);
                            let name = id_store.get_ident_id_from_string(name);
                            let src = self.next_reg();
                            self.gen_expr(ctx, ir, id_store, rhs, true, false)?;
                            self.gen_store_const(ir, src.into(), name, loc);
                        }
                        _ => {
                            return Err(MonorubyErr::unsupported_lhs(lhs, self.sourceinfo.clone()))
                        }
                    }
                } else {
                    return self.gen_mul_assign(ctx, ir, id_store, mlhs, mrhs, use_value, is_ret);
                }
            }
            NodeKind::LocalVar(ident) => {
                let local = self.load_local(&ident, loc)?;
                if is_ret {
                    self.gen_ret(ir, Some(local));
                } else if use_value {
                    self.gen_temp_mov(ir, local.into());
                }
                return Ok(());
            }
            NodeKind::Const { toplevel, name } => {
                assert!(!toplevel);
                let name = id_store.get_ident_id_from_string(name);
                self.gen_load_const(ir, None, name, loc);
            }
            NodeKind::MethodCall {
                box receiver,
                method,
                arglist,
                safe_nav: false,
            } => {
                let ret = if use_value {
                    Some(self.push().into())
                } else {
                    None
                };
                return self.gen_method_call(
                    ctx, ir, id_store, method, receiver, arglist, ret, is_ret, loc,
                );
            }
            NodeKind::FuncCall {
                method,
                arglist,
                safe_nav: false,
            } => {
                let ret = if use_value {
                    Some(self.push().into())
                } else {
                    None
                };
                return self.gen_func_call(ctx, ir, id_store, method, arglist, ret, is_ret, loc);
            }
            NodeKind::Ident(method) => {
                let arglist = ArgList::default();
                let ret = if use_value {
                    Some(self.push().into())
                } else {
                    None
                };
                return self.gen_func_call(ctx, ir, id_store, method, arglist, ret, is_ret, loc);
            }
            NodeKind::If {
                box cond,
                box then_,
                box else_,
            } => {
                let then_pos = ir.new_label();
                let succ_pos = ir.new_label();
                let cond = self.gen_temp_expr(ctx, ir, id_store, cond)?.into();
                ir.gen_condbr(cond, then_pos);
                self.gen_expr(ctx, ir, id_store, else_, use_value, is_ret)?;
                if !is_ret {
                    ir.gen_br(succ_pos);
                    if use_value {
                        self.pop();
                    }
                }
                ir.apply_label(then_pos);
                self.gen_expr(ctx, ir, id_store, then_, use_value, is_ret)?;
                ir.apply_label(succ_pos);
                return Ok(());
            }
            NodeKind::While {
                box cond,
                box body,
                cond_op,
            } => {
                assert!(cond_op);
                self.gen_while(ctx, ir, id_store, cond, body, use_value)?;
                if is_ret {
                    self.gen_ret(ir, None);
                }
                return Ok(());
            }
            NodeKind::For {
                param,
                box iter,
                body,
            } => {
                self.gen_for(ctx, ir, id_store, param, iter, body, use_value)?;
                if is_ret {
                    self.gen_ret(ir, None);
                }
                return Ok(());
            }
            NodeKind::Break(box val) => {
                let (_kind, break_pos, ret_reg) = match ir.loops.last() {
                    Some(data) => data.clone(),
                    None => {
                        return Err(MonorubyErr::escape_from_eval(loc, self.sourceinfo.clone()))
                    }
                };
                match ret_reg {
                    Some(reg) => {
                        let temp = self.gen_temp_expr(ctx, ir, id_store, val)?;
                        self.gen_mov(ir, reg, temp.into())
                    }
                    None => {}
                }
                ir.push(BcIr::Br(break_pos), loc);
                return Ok(());
            }
            NodeKind::Return(box expr) => {
                if let Some(local) = is_local(&expr) {
                    let local = self.load_local(local, expr.loc)?;
                    self.gen_ret(ir, Some(local));
                } else {
                    self.gen_expr(ctx, ir, id_store, expr, true, true)?;
                }
                if use_value && !is_ret {
                    unreachable!();
                }
                return Ok(());
            }
            NodeKind::CompStmt(nodes) => {
                return self.gen_comp_stmts(ctx, ir, id_store, nodes, None, use_value, is_ret)
            }
            NodeKind::Begin {
                box body,
                rescue,
                else_: None,
                ensure: None,
            } => {
                assert!(rescue.len() == 0);
                self.gen_expr(ctx, ir, id_store, body, use_value, is_ret)?;
                return Ok(());
            }
            NodeKind::MethodDef(name, params, box node, _lv) => {
                self.gen_method_def(ctx, ir, id_store, name.clone(), params, node)?;
                if use_value {
                    self.gen_symbol(ir, None, id_store.get_ident_id_from_string(name));
                }
                if is_ret {
                    self.gen_ret(ir, None);
                }
                return Ok(());
            }
            NodeKind::InterporatedString(nodes) => {
                let len = nodes.len();
                let arg = self.next_reg();
                for expr in nodes {
                    self.gen_expr(ctx, ir, id_store, expr, true, false)?;
                }
                self.temp -= len as u16;
                let ret = match use_value {
                    true => Some(self.push().into()),
                    false => None,
                };
                ir.push(BcIr::ConcatStr(ret, arg, len), Loc::default());
                if is_ret {
                    self.gen_ret(ir, None);
                }
                return Ok(());
            }
            _ => return Err(MonorubyErr::unsupported_node(expr, self.sourceinfo.clone())),
        }
        if is_ret {
            self.gen_ret(ir, None);
        } else if !use_value {
            self.pop();
        }
        Ok(())
    }

    fn gen_store_expr(
        &mut self,
        ctx: &mut FnStore,
        ir: &mut IrContext,
        id_store: &mut IdentifierTable,
        local: BcLocal,
        rhs: Node,
        use_value: bool,
    ) -> Result<()> {
        let loc = rhs.loc;
        match rhs.kind {
            NodeKind::Nil => self.gen_nil(ir, Some(local)),
            NodeKind::Bool(b) => self.gen_literal(ctx, ir, Some(local), Value::bool(b)),
            NodeKind::SelfValue => self.gen_mov(ir, local.into(), BcReg::Self_),
            NodeKind::Integer(i) => self.gen_integer(ctx, ir, Some(local), i),
            NodeKind::Symbol(sym) => {
                let sym = id_store.get_ident_id_from_string(sym);
                self.gen_symbol(ir, Some(local), sym)
            }
            NodeKind::Bignum(bigint) => self.gen_bigint(ctx, ir, Some(local), bigint),
            NodeKind::Float(f) => self.gen_float(ctx, ir, Some(local), f),
            NodeKind::String(s) => self.gen_string(ctx, ir, Some(local), s.into_bytes()),
            NodeKind::UnOp(op, box rhs) => {
                assert!(op == UnOp::Neg);
                match rhs.kind {
                    NodeKind::Integer(i) => self.gen_integer(ctx, ir, Some(local), -i),
                    NodeKind::Float(f) => self.gen_float(ctx, ir, Some(local), -f),
                    _ => {
                        self.gen_store_expr(ctx, ir, id_store, local, rhs, false)?;
                        self.gen_neg(ir, Some(local), loc);
                    }
                };
            }
            NodeKind::BinOp(op, box lhs, box rhs) => {
                self.gen_binop(ctx, ir, id_store, op, lhs, rhs, Some(local), loc)?
            }
            NodeKind::MulAssign(mut mlhs, mut mrhs) => {
                if mlhs.len() == 1 && mrhs.len() == 1 {
                    let (lhs, rhs) = (mlhs.remove(0), mrhs.remove(0));
                    match lhs.kind {
                        NodeKind::LocalVar(lhs) | NodeKind::Ident(lhs) => {
                            let src = self.find_local(&lhs);
                            self.gen_store_expr(ctx, ir, id_store, src, rhs, false)?;
                            self.gen_mov(ir, local.into(), src.into());
                        }
                        _ => {
                            return Err(MonorubyErr::unsupported_lhs(lhs, self.sourceinfo.clone()))
                        }
                    }
                } else {
                    self.gen_mul_assign(ctx, ir, id_store, mlhs, mrhs, true, false)?;
                    let temp = self.pop().into();
                    self.gen_mov(ir, local.into(), temp);
                }
            }
            NodeKind::LocalVar(ident) => {
                let local2 = self.load_local(&ident, loc)?;
                self.gen_mov(ir, local.into(), local2.into());
            }
            NodeKind::Const { toplevel, name } => {
                assert!(!toplevel);
                let name = id_store.get_ident_id_from_string(name);
                self.gen_load_const(ir, local.into(), name, loc);
                return Ok(());
            }
            NodeKind::MethodCall {
                box receiver,
                method,
                arglist,
                safe_nav: false,
            } => {
                let ret = Some(local.into());
                self.gen_method_call(
                    ctx, ir, id_store, method, receiver, arglist, ret, false, loc,
                )?;
            }
            NodeKind::FuncCall {
                method,
                arglist,
                safe_nav: false,
            } => {
                let ret = Some(local.into());
                self.gen_func_call(ctx, ir, id_store, method, arglist, ret, false, loc)?;
            }
            NodeKind::Return(_) => unreachable!(),
            NodeKind::CompStmt(nodes) => {
                return self.gen_comp_stmts(ctx, ir, id_store, nodes, Some(local), use_value, false)
            }
            _ => {
                let ret = self.next_reg();
                self.gen_expr(ctx, ir, id_store, rhs, true, false)?;
                self.gen_mov(ir, local.into(), ret.into());
                if !use_value {
                    self.pop();
                }
                return Ok(());
            }
        };
        if use_value {
            self.gen_temp_mov(ir, local.into());
        }
        Ok(())
    }

    fn gen_method_def(
        &mut self,
        ctx: &mut FnStore,
        ir: &mut IrContext,
        id_store: &mut IdentifierTable,
        name: String,
        params: Vec<FormalParam>,
        node: Node,
    ) -> Result<()> {
        let mut args = vec![];
        for param in params {
            match param.kind {
                ParamKind::Param(name) => args.push(name),
                _ => {
                    return Err(MonorubyErr::unsupported_parameter_kind(
                        param.kind,
                        param.loc,
                        self.sourceinfo.clone(),
                    ))
                }
            }
        }
        let func_id =
            ctx.functions
                .add_normal_func(Some(name.clone()), args, node, self.sourceinfo.clone());
        let name = id_store.get_ident_id_from_string(name);
        ir.push(BcIr::MethodDef(name, func_id), Loc::default());
        Ok(())
    }

    fn gen_args(
        &mut self,
        ctx: &mut FnStore,
        ir: &mut IrContext,
        id_store: &mut IdentifierTable,
        args: Vec<Node>,
    ) -> Result<BcTemp> {
        let arg = self.next_reg();
        for arg in args {
            self.gen_expr(ctx, ir, id_store, arg, true, false)?;
        }
        Ok(arg)
    }

    fn check_fast_call(
        &mut self,
        ctx: &mut FnStore,
        ir: &mut IrContext,
        id_store: &mut IdentifierTable,
        arglist: ArgList,
    ) -> Result<(BcTemp, usize)> {
        assert!(arglist.kw_args.len() == 0);
        assert!(arglist.hash_splat.len() == 0);
        assert!(arglist.block.is_none());
        assert!(!arglist.delegate);
        self.check_fast_call_inner(ctx, ir, id_store, arglist.args)
    }

    fn check_fast_call_inner(
        &mut self,
        ctx: &mut FnStore,
        ir: &mut IrContext,
        id_store: &mut IdentifierTable,
        args: Vec<Node>,
    ) -> Result<(BcTemp, usize)> {
        let len = args.len();
        let arg = self.gen_args(ctx, ir, id_store, args)?;
        self.temp -= len as u16;
        Ok((arg, len))
    }

    fn gen_method_call(
        &mut self,
        ctx: &mut FnStore,
        ir: &mut IrContext,
        id_store: &mut IdentifierTable,
        method: String,
        receiver: Node,
        arglist: ArgList,
        ret: Option<BcReg>,
        is_ret: bool,
        loc: Loc,
    ) -> Result<()> {
        let (arg, len) = self.check_fast_call(ctx, ir, id_store, arglist)?;
        let method = id_store.get_ident_id_from_string(method);
        if receiver.kind == NodeKind::SelfValue {
            ir.push(BcIr::MethodCall(BcReg::Self_, method, ret, arg, len), loc);
        } else {
            self.gen_expr(ctx, ir, id_store, receiver, true, false)?;
            let recv = self.pop().into();
            ir.push(BcIr::MethodCall(recv, method, ret, arg, len), loc);
        }
        if is_ret {
            self.gen_ret(ir, None);
        }
        return Ok(());
    }

    fn gen_func_call(
        &mut self,
        ctx: &mut FnStore,
        ir: &mut IrContext,
        id_store: &mut IdentifierTable,
        method: String,
        arglist: ArgList,
        ret: Option<BcReg>,
        is_ret: bool,
        loc: Loc,
    ) -> Result<()> {
        let (arg, len) = self.check_fast_call(ctx, ir, id_store, arglist)?;
        let method = id_store.get_ident_id_from_string(method);
        ir.push(BcIr::MethodCall(BcReg::Self_, method, ret, arg, len), loc);
        if is_ret {
            self.gen_ret(ir, None);
        }
        return Ok(());
    }

    fn gen_binary(
        &mut self,
        ctx: &mut FnStore,
        ir: &mut IrContext,
        id_store: &mut IdentifierTable,
        dst: Option<BcLocal>,
        lhs: Node,
        rhs: Node,
    ) -> Result<(BcReg, BcReg, BcReg)> {
        let (lhs, rhs) = match (is_local(&lhs), is_local(&rhs)) {
            (Some(lhs), Some(rhs)) => {
                let lhs = self.find_local(lhs).into();
                let rhs = self.find_local(rhs).into();
                (lhs, rhs)
            }
            (Some(lhs), None) => {
                let lhs = self.find_local(lhs).into();
                let rhs = self.gen_temp_expr(ctx, ir, id_store, rhs)?.into();
                (lhs, rhs)
            }
            (None, Some(rhs)) => {
                let lhs = self.gen_temp_expr(ctx, ir, id_store, lhs)?.into();
                let rhs = self.find_local(rhs).into();
                (lhs, rhs)
            }
            (None, None) => {
                self.gen_expr(ctx, ir, id_store, lhs, true, false)?;
                self.gen_expr(ctx, ir, id_store, rhs, true, false)?;
                let rhs = self.pop().into();
                let lhs = self.pop().into();
                (lhs, rhs)
            }
        };
        let dst = match dst {
            None => self.push().into(),
            Some(local) => local.into(),
        };
        Ok((dst, lhs, rhs))
    }

    fn gen_singular(
        &mut self,
        ctx: &mut FnStore,
        ir: &mut IrContext,
        id_store: &mut IdentifierTable,
        dst: Option<BcLocal>,
        lhs: Node,
    ) -> Result<(BcReg, BcReg)> {
        let lhs = match is_local(&lhs) {
            Some(lhs) => self.find_local(lhs).into(),
            None => self.gen_temp_expr(ctx, ir, id_store, lhs)?.into(),
        };
        let dst = match dst {
            None => self.push().into(),
            Some(local) => local.into(),
        };
        Ok((dst, lhs))
    }
}

macro_rules! gen_ops {
    (($op:ident, $inst:ident)) => {
        paste! {
            fn [<gen_ $op>](
                &mut self,
                ctx: &mut FnStore,
                ir: &mut IrContext,
                id_store: &mut IdentifierTable,
                dst: Option<BcLocal>,
                lhs: Node,
                rhs: Node,
                loc: Loc,
            ) -> Result<()> {
                let (dst, lhs, rhs) = self.gen_binary(ctx, ir, id_store, dst, lhs, rhs)?;
                ir.push(BcIr::$inst(dst, lhs, rhs), loc);
                Ok(())
            }
        }
    };
    (($op1:ident, $inst1:ident), $(($op2:ident, $inst2:ident)),+) => {
        gen_ops!(($op1, $inst1));
        gen_ops!($(($op2, $inst2)),+);
    };
}

macro_rules! gen_ri_ops {
    (($op:ident, $inst:ident)) => {
        paste! {
            fn [<gen_ $op>](
                &mut self,
                ctx: &mut FnStore,
                ir: &mut IrContext,
                id_store: &mut IdentifierTable,
                dst: Option<BcLocal>,
                lhs: Node,
                rhs: Node,
                loc: Loc,
            ) -> Result<()> {
                if let Some(i) = is_smi(&rhs) {
                    let (dst, lhs) = self.gen_singular(ctx, ir, id_store, dst, lhs)?;
                    ir.push(BcIr::[<$inst ri>](dst, lhs, i), loc);
                } else {
                    let (dst, lhs, rhs) = self.gen_binary(ctx, ir, id_store, dst, lhs, rhs)?;
                    ir.push(BcIr::$inst(dst, lhs, rhs), loc);
                }
                Ok(())
            }
        }
    };
    (($op1:ident, $inst1:ident), $(($op2:ident, $inst2:ident)),+) => {
        gen_ri_ops!(($op1, $inst1));
        gen_ri_ops!($(($op2, $inst2)),+);
    };
}

impl NormalFuncInfo {
    gen_ri_ops!((add, Add), (sub, Sub));
    gen_ops!(
        (mul, Mul),
        (div, Div),
        (bitor, BitOr),
        (bitand, BitAnd),
        (bitxor, BitXor),
        (shr, Shr),
        (shl, Shl)
    );
}

impl NormalFuncInfo {
    fn gen_cmp(
        &mut self,
        ctx: &mut FnStore,
        ir: &mut IrContext,
        id_store: &mut IdentifierTable,
        dst: Option<BcLocal>,
        kind: CmpKind,
        lhs: Node,
        rhs: Node,
        loc: Loc,
    ) -> Result<()> {
        if let Some(i) = is_smi(&rhs) {
            let (dst, lhs) = self.gen_singular(ctx, ir, id_store, dst, lhs)?;
            ir.push(BcIr::Cmpri(kind, dst, lhs, i), loc);
        } else {
            let (dst, lhs, rhs) = self.gen_binary(ctx, ir, id_store, dst, lhs, rhs)?;
            ir.push(BcIr::Cmp(kind, dst, lhs, rhs), loc);
        }
        Ok(())
    }

    fn gen_mul_assign(
        &mut self,
        ctx: &mut FnStore,
        ir: &mut IrContext,
        id_store: &mut IdentifierTable,
        mlhs: Vec<Node>,
        mrhs: Vec<Node>,
        use_value: bool,
        is_ret: bool,
    ) -> Result<()> {
        let mlhs_len = mlhs.len();
        assert!(mlhs_len == mrhs.len());
        let mut temp_reg = self.next_reg();
        // At first we evaluate right-hand side values and save them in temporory registers.
        for rhs in mrhs {
            self.gen_expr(ctx, ir, id_store, rhs, true, false)?;
        }
        // Assign values to left-hand side expressions.
        for lhs in mlhs {
            match lhs.kind {
                NodeKind::LocalVar(lhs) | NodeKind::Ident(lhs) => {
                    let local = self.find_local(&lhs);
                    self.gen_mov(ir, local.into(), temp_reg.into());
                }
                _ => return Err(MonorubyErr::unsupported_lhs(lhs, self.sourceinfo.clone())),
            }
            temp_reg += 1;
        }
        self.popn(mlhs_len);
        // TODO: This is not correct. We must make an Array.
        if is_ret {
            self.gen_nil(ir, None);
            self.gen_ret(ir, None);
        } else if use_value {
            self.gen_nil(ir, None);
        }
        Ok(())
    }

    fn gen_for(
        &mut self,
        ctx: &mut FnStore,
        ir: &mut IrContext,
        id_store: &mut IdentifierTable,
        param: Vec<String>,
        iter: Node,
        body: BlockInfo,
        use_value: bool,
    ) -> Result<()> {
        assert_eq!(1, param.len());
        let counter = self.find_local(&param[0]);
        let break_pos = ir.new_label();
        ir.loops.push((
            LoopKind::For,
            break_pos,
            match use_value {
                true => Some(self.next_reg().into()),
                false => None,
            },
        ));
        let loc = iter.loc;
        if let NodeKind::Range {
            box start,
            box end,
            exclude_end: false,
            ..
        } = iter.kind
        {
            let loop_entry = ir.new_label();
            let loop_exit = ir.new_label();
            self.gen_store_expr(ctx, ir, id_store, counter, start, false)?;
            self.gen_temp_expr(ctx, ir, id_store, end)?;
            let end = self.push().into();

            ir.apply_label(loop_entry);
            let dst = self.push().into();
            ir.push(BcIr::Cmp(CmpKind::Gt, dst, counter.into(), end), loc);
            ir.gen_condbr(dst, loop_exit);
            self.pop();

            self.gen_expr(ctx, ir, id_store, *body.body, false, false)?;

            ir.push(BcIr::Addri(counter.into(), counter.into(), 1), loc);
            ir.gen_br(loop_entry);

            ir.apply_label(loop_exit);
            self.pop();
        } else {
            unimplemented!()
        }
        if use_value {
            // TODO: we must return iter object.
            self.gen_nil(ir, None);
        }
        ir.loops.pop().unwrap();
        ir.apply_label(break_pos);
        Ok(())
    }

    fn gen_while(
        &mut self,
        ctx: &mut FnStore,
        ir: &mut IrContext,
        id_store: &mut IdentifierTable,
        cond: Node,
        body: Node,
        use_value: bool,
    ) -> Result<()> {
        let cond_pos = ir.new_label();
        let succ_pos = ir.new_label();
        let break_pos = ir.new_label();
        ir.loops.push((
            LoopKind::While,
            break_pos,
            match use_value {
                true => Some(self.next_reg().into()),
                false => None,
            },
        ));
        ir.apply_label(cond_pos);
        let cond = self.gen_temp_expr(ctx, ir, id_store, cond)?.into();
        ir.gen_condnotbr(cond, succ_pos);
        self.gen_expr(ctx, ir, id_store, body, false, false)?;
        ir.gen_br(cond_pos);
        ir.apply_label(succ_pos);

        if use_value {
            self.gen_nil(ir, None);
        }
        ir.loops.pop().unwrap();
        ir.apply_label(break_pos);

        Ok(())
    }
}

impl NormalFuncInfo {
    fn get_index(&self, reg: &BcReg) -> u16 {
        match reg {
            BcReg::Self_ => 0,
            BcReg::Temp(i) => 1 + self.locals.len() as u16 + i.0,
            BcReg::Local(i) => 1 + i.0,
        }
    }

    fn add_constsite(
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
        let id = store.callsite_info.len();
        store.constsite_info.push(info);
        ConstSiteId(id as u32)
    }

    fn add_callsite(
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

    pub(crate) fn ir_to_bytecode(&mut self, ir: IrContext, store: &mut FnStore) {
        let mut ops = vec![];
        let mut locs = vec![];
        for (idx, (inst, loc)) in ir.ir.iter().enumerate() {
            let op = match inst {
                BcIr::Br(dst) => {
                    let dst = ir.labels[*dst].unwrap().0 as i32;
                    BcOp::Br(dst - idx as i32 - 1)
                }
                BcIr::CondBr(reg, dst) => {
                    let dst = ir.labels[*dst].unwrap().0 as i32;
                    BcOp::CondBr(self.get_index(reg), dst - idx as i32 - 1)
                }
                BcIr::CondNotBr(reg, dst) => {
                    let dst = ir.labels[*dst].unwrap().0 as i32;
                    BcOp::CondNotBr(self.get_index(reg), dst - idx as i32 - 1)
                }
                BcIr::Integer(reg, num) => BcOp::Integer(self.get_index(reg), *num),
                BcIr::Symbol(reg, name) => BcOp::Symbol(self.get_index(reg), *name),
                BcIr::Literal(reg, num) => BcOp::Literal(self.get_index(reg), *num),
                BcIr::LoadConst(reg, name) => BcOp::LoadConst(
                    self.get_index(reg),
                    self.add_constsite(store, *name, vec![], false),
                ),
                BcIr::StoreConst(reg, name) => BcOp::StoreConst(self.get_index(reg), *name),
                BcIr::Nil(reg) => BcOp::Nil(self.get_index(reg)),
                BcIr::Neg(dst, src) => BcOp::Neg(self.get_index(dst), self.get_index(src)),
                BcIr::Add(dst, lhs, rhs) => BcOp::Add(
                    self.get_index(dst),
                    self.get_index(lhs),
                    self.get_index(rhs),
                ),
                BcIr::Addri(dst, lhs, rhs) => {
                    BcOp::Addri(self.get_index(dst), self.get_index(lhs), *rhs)
                }
                BcIr::Sub(dst, lhs, rhs) => BcOp::Sub(
                    self.get_index(dst),
                    self.get_index(lhs),
                    self.get_index(rhs),
                ),
                BcIr::Subri(dst, lhs, rhs) => {
                    BcOp::Subri(self.get_index(dst), self.get_index(lhs), *rhs)
                }
                BcIr::Mul(dst, lhs, rhs) => BcOp::Mul(
                    self.get_index(dst),
                    self.get_index(lhs),
                    self.get_index(rhs),
                ),
                BcIr::Div(dst, lhs, rhs) => BcOp::Div(
                    self.get_index(dst),
                    self.get_index(lhs),
                    self.get_index(rhs),
                ),
                BcIr::BitOr(dst, lhs, rhs) => BcOp::BitOr(
                    self.get_index(dst),
                    self.get_index(lhs),
                    self.get_index(rhs),
                ),
                BcIr::BitAnd(dst, lhs, rhs) => BcOp::BitAnd(
                    self.get_index(dst),
                    self.get_index(lhs),
                    self.get_index(rhs),
                ),
                BcIr::BitXor(dst, lhs, rhs) => BcOp::BitXor(
                    self.get_index(dst),
                    self.get_index(lhs),
                    self.get_index(rhs),
                ),
                BcIr::Shr(dst, lhs, rhs) => BcOp::Shr(
                    self.get_index(dst),
                    self.get_index(lhs),
                    self.get_index(rhs),
                ),
                BcIr::Shl(dst, lhs, rhs) => BcOp::Shl(
                    self.get_index(dst),
                    self.get_index(lhs),
                    self.get_index(rhs),
                ),
                BcIr::Cmp(kind, dst, lhs, rhs) => {
                    let dst = self.get_index(dst);
                    let lhs = self.get_index(lhs);
                    let rhs = self.get_index(rhs);
                    BcOp::Cmp(*kind, dst, lhs, rhs)
                }
                BcIr::Cmpri(kind, dst, lhs, rhs) => {
                    let dst = self.get_index(dst);
                    let lhs = self.get_index(lhs);
                    let rhs = *rhs;
                    BcOp::Cmpri(*kind, dst, lhs, rhs)
                }
                BcIr::Ret(reg) => BcOp::Ret(self.get_index(reg)),
                BcIr::Mov(dst, src) => BcOp::Mov(self.get_index(dst), self.get_index(src)),
                BcIr::MethodCall(recv, name, ret, args, len) => {
                    let id = self.add_callsite(store, *ret, *name, *args, *len);
                    let recv = self.get_index(recv);
                    BcOp::MethodCall(recv, id)
                }
                BcIr::MethodDef(name, func_id) => {
                    BcOp::MethodDef(store.add_method_def(*name, *func_id))
                }
                BcIr::ConcatStr(ret, arg, len) => {
                    let ret = ret.map_or(0, |ret| self.get_index(&ret));
                    BcOp::ConcatStr(ret, self.get_index(&BcReg::from(*arg)), *len as u16)
                }
            };
            ops.push(op.to_u64());
            locs.push(*loc);
        }
        self.bytecode = ops;
        self.sourcemap = locs;
    }
}
