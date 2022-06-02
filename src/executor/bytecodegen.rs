use super::*;
use crate::executor::interp::{BcPc, BcPcBase};
use num::BigInt;
use paste::paste;
use std::hash::Hash;

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
    fn new() -> Self {
        Self(vec![])
    }

    fn next_func_id(&self) -> FuncId {
        FuncId(self.0.len() as u32)
    }

    fn add_normal_func(&mut self, name: Option<IdentId>, args: Vec<IdentId>, ast: Node) -> FuncId {
        let fid = self.next_func_id();
        self.0.push(FuncInfo::new_normal(name, fid, args, ast));
        fid
    }

    fn add_builtin_func(&mut self, name_id: IdentId, address: BuiltinFn, arity: usize) -> FuncId {
        let id = self.next_func_id();
        self.0
            .push(FuncInfo::new_builtin(id, name_id, address, arity));
        id
    }
}

#[derive(Debug, Clone, Default, PartialEq)]
pub struct CallsiteInfo {
    pub name: IdentId,
    pub args: u16,
    pub len: u16,
    pub cache: (usize, FuncId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
pub struct CallsiteId(pub u32);

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
    func_map: FxHashMap<IdentId, FuncId>,
    pub main: Option<FuncId>,
    /// method define info.
    method_def_info: Vec<MethodDefInfo>,
    /// callsite info.
    callsite_info: Vec<CallsiteInfo>,
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

impl FnStore {
    pub fn new() -> Self {
        Self {
            functions: Funcs::new(),
            func_map: FxHashMap::default(),
            main: None,
            method_def_info: vec![],
            callsite_info: vec![],
            literals: vec![],
        }
    }

    pub fn insert(&mut self, func_name: IdentId, func_id: FuncId) {
        self.func_map.insert(func_name, func_id);
    }

    pub fn get(&self, name: IdentId) -> Option<&FuncId> {
        self.func_map.get(&name)
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

    /// get a constant.
    pub(super) fn get_literal(&self, id: u32) -> Value {
        self.literals[id as usize]
    }

    /// register a new constant.
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
                    let label = jit.jit_compile_builtin(*abs_address, func.arity());
                    func.set_jit_label(label);
                }
            };
        }
    }
}

impl FnStore {
    pub(super) fn compile_main(&mut self, ast: Node, id_store: &IdentifierTable) -> Result<()> {
        let mut fid = self.functions.add_normal_func(None, vec![], ast);
        self.main = Some(fid);

        while self.len() > fid.0 as usize {
            self.compile_func(fid, id_store)?;
            fid = FuncId(fid.0 + 1);
        }

        Ok(())
    }

    /// Generate bytecode for a function which has *func_id*.
    fn compile_func(&mut self, func_id: FuncId, id_store: &IdentifierTable) -> Result<()> {
        let mut info = std::mem::take(self[func_id].as_normal_mut());
        let ir = info.compile_ast(self)?;
        info.ir_to_bytecode(ir, id_store, self);
        let regs = info.total_reg_num();
        std::mem::swap(&mut info, self[func_id].as_normal_mut());
        self[func_id].inst_pc = BcPcBase::new(self[func_id].as_normal()) + 0;
        self[func_id].stack_offset = (regs + regs % 2) * 8 + 16;
        Ok(())
    }

    pub(super) fn add_builtin_func(
        &mut self,
        name_id: IdentId,
        address: BuiltinFn,
        arity: usize,
    ) -> FuncId {
        let id = self.functions.add_builtin_func(name_id, address, arity);
        self.func_map.insert(name_id, id);
        id
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
    name: Option<IdentId>,
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
    fn new_normal(name: Option<IdentId>, func_id: FuncId, args: Vec<IdentId>, ast: Node) -> Self {
        let info = NormalFuncInfo::new(func_id, name, args, ast);
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

    fn new_builtin(id: FuncId, name: IdentId, address: BuiltinFn, arity: usize) -> Self {
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
    ir: Vec<BcIr>,
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

    fn push(&mut self, op: BcIr) {
        self.ir.push(op);
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
}

#[derive(Debug, Clone, Default, PartialEq)]
pub(super) struct NormalFuncInfo {
    /// ID of this function.
    pub(super) id: FuncId,
    name_id: Option<IdentId>,
    /// Bytecode.
    bytecode: Vec<u64>,
    /// the name of arguments.
    args: Vec<IdentId>,
    /// local variables.
    locals: FxHashMap<IdentId, u16>,
    /// The current register id.
    temp: u16,
    /// The number of temporary registers.
    reg_num: u16,
    /// AST.
    ast: Option<Node>,
}

impl NormalFuncInfo {
    pub fn new(id: FuncId, name_id: Option<IdentId>, args: Vec<IdentId>, ast: Node) -> Self {
        let mut info = NormalFuncInfo {
            id,
            name_id,
            bytecode: vec![],
            args: args.clone(),
            locals: FxHashMap::default(),
            temp: 0,
            reg_num: 0,
            ast: Some(ast),
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

    fn load_local(&mut self, ident: IdentId) -> Result<BcLocal> {
        match self.locals.get(&ident) {
            Some(local) => Ok(BcLocal(*local)),
            None => Err(MonorubyErr::undefined_local(ident)),
        }
    }

    fn find_local(&mut self, ident: IdentId) -> BcLocal {
        match self.locals.get(&ident) {
            Some(local) => BcLocal(*local),
            None => self.add_local(ident.clone()),
        }
    }

    /// Add a variable identifier without checking duplicates.
    fn add_local(&mut self, ident: IdentId) -> BcLocal {
        let local = self.locals.len() as u16;
        assert!(self.locals.insert(ident, local).is_none());
        BcLocal(local)
    }

    fn gen_integer(&mut self, ctx: &mut FnStore, ir: &mut IrContext, dst: Option<BcLocal>, i: i64) {
        if let Ok(i) = i32::try_from(i) {
            let reg = match dst {
                Some(local) => local.into(),
                None => self.push().into(),
            };
            ir.push(BcIr::Integer(reg, i));
        } else {
            self.gen_const(ctx, ir, dst, Value::integer(i));
        }
    }

    fn gen_const(&mut self, ctx: &mut FnStore, ir: &mut IrContext, dst: Option<BcLocal>, v: Value) {
        let reg = match dst {
            Some(local) => local.into(),
            None => self.push().into(),
        };
        let id = ctx.new_literal(v);
        ir.push(BcIr::Const(reg, id));
    }

    fn gen_float(&mut self, ctx: &mut FnStore, ir: &mut IrContext, dst: Option<BcLocal>, f: f64) {
        self.gen_const(ctx, ir, dst, Value::float(f));
    }

    fn gen_bigint(
        &mut self,
        ctx: &mut FnStore,
        ir: &mut IrContext,
        dst: Option<BcLocal>,
        bigint: BigInt,
    ) {
        self.gen_const(ctx, ir, dst, Value::bigint(bigint));
    }

    fn gen_nil(&mut self, ir: &mut IrContext, dst: Option<BcLocal>) {
        let reg = match dst {
            Some(local) => local.into(),
            None => self.push().into(),
        };
        ir.push(BcIr::Nil(reg));
    }

    fn gen_neg(&mut self, ir: &mut IrContext, local: Option<BcLocal>) {
        match local {
            Some(local) => {
                let local = local.into();
                ir.push(BcIr::Neg(local, local));
            }
            None => {
                let src = self.pop().into();
                let dst = self.push().into();
                ir.push(BcIr::Neg(dst, src));
            }
        };
    }

    fn gen_ret(&mut self, ir: &mut IrContext, local: Option<BcLocal>) {
        let ret = match local {
            Some(local) => local.into(),
            None => self.pop().into(),
        };
        assert_eq!(0, self.temp);
        ir.push(BcIr::Ret(ret));
    }

    fn gen_mov(&mut self, ir: &mut IrContext, dst: BcReg, src: BcReg) {
        ir.push(BcIr::Mov(dst, src));
    }

    fn gen_temp_mov(&mut self, ir: &mut IrContext, rhs: BcReg) {
        let lhs = self.push();
        self.gen_mov(ir, lhs.into(), rhs);
    }

    fn dump(&self, globals: &IdentifierTable, store: &FnStore) {
        eprintln!("------------------------------------");
        eprintln!(
            "{:?} name:{} args:{:?}",
            self.id,
            match self.name_id {
                Some(name_id) => globals.get_name(name_id),
                None => "<ANONYMOUS>",
            },
            self.args
                .iter()
                .map(|id| globals.get_name(*id))
                .collect::<Vec<_>>()
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
                BcOp::Const(reg, id) => eprintln!("%{} = constants[{}]", reg, id),
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
                BcOp::Eq(dst, lhs, rhs) => {
                    eprintln!("%{} = %{} {:?} %{}", dst, lhs, CmpKind::Eq, rhs)
                }
                BcOp::Ne(dst, lhs, rhs) => {
                    eprintln!("%{} = %{} {:?} %{}", dst, lhs, CmpKind::Ne, rhs)
                }
                BcOp::Gt(dst, lhs, rhs) => {
                    eprintln!("%{} = %{} {:?} %{}", dst, lhs, CmpKind::Gt, rhs)
                }
                BcOp::Ge(dst, lhs, rhs) => {
                    eprintln!("%{} = %{} {:?} %{}", dst, lhs, CmpKind::Ge, rhs)
                }
                BcOp::Lt(dst, lhs, rhs) => {
                    eprintln!("%{} = %{} {:?} %{}", dst, lhs, CmpKind::Lt, rhs)
                }
                BcOp::Le(dst, lhs, rhs) => {
                    eprintln!("%{} = %{} {:?} %{}", dst, lhs, CmpKind::Le, rhs)
                }

                BcOp::Eqri(dst, lhs, rhs) => {
                    eprintln!("%{} = %{} {:?} {}: i16", dst, lhs, CmpKind::Eq, rhs)
                }
                BcOp::Neri(dst, lhs, rhs) => {
                    eprintln!("%{} = %{} {:?} {}: i16", dst, lhs, CmpKind::Ne, rhs)
                }
                BcOp::Gtri(dst, lhs, rhs) => {
                    eprintln!("%{} = %{} {:?} {}: i16", dst, lhs, CmpKind::Gt, rhs)
                }
                BcOp::Geri(dst, lhs, rhs) => {
                    eprintln!("%{} = %{} {:?} {}: i16", dst, lhs, CmpKind::Ge, rhs)
                }
                BcOp::Ltri(dst, lhs, rhs) => {
                    eprintln!("%{} = %{} {:?} {}: i16", dst, lhs, CmpKind::Lt, rhs)
                }
                BcOp::Leri(dst, lhs, rhs) => {
                    eprintln!("%{} = %{} {:?} {}: i16", dst, lhs, CmpKind::Le, rhs)
                }

                BcOp::Ret(reg) => eprintln!("ret %{}", reg),
                BcOp::Mov(dst, src) => eprintln!("%{} = %{}", dst, src),
                BcOp::FnCall(ret, id) => {
                    let CallsiteInfo {
                        name,
                        args,
                        len,
                        cache: _,
                    } = store[id];
                    let name = globals.get_name(name);
                    match ret {
                        0 => {
                            eprintln!("_ = call {}(%{}; {})", name, args, len)
                        }
                        ret => {
                            eprintln!("%{:?} = call {}(%{}; {})", ret, name, args, len)
                        }
                    }
                }
                BcOp::MethodDef(id) => {
                    let MethodDefInfo { name, func } = store[id];
                    let name = globals.get_name(name);
                    eprintln!("define {:?}: {:?}", name, func)
                }
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

pub fn is_local(node: &Node) -> Option<IdentId> {
    if let NodeKind::LocalVar(id) = &node.kind {
        Some(*id)
    } else {
        None
    }
}

impl NormalFuncInfo {
    fn compile_ast(&mut self, ctx: &mut FnStore) -> Result<IrContext> {
        let mut ir = IrContext::new();
        let ast = std::mem::take(&mut self.ast).unwrap();
        self.gen_expr(ctx, &mut ir, ast, true, true)?;
        if self.temp == 1 {
            self.gen_ret(&mut ir, None);
        };
        Ok(ir)
    }

    fn gen_comp_stmts(
        &mut self,
        ctx: &mut FnStore,
        ir: &mut IrContext,
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
            self.gen_expr(ctx, ir, node, false, false)?;
        }
        match ret {
            Some(ret) => {
                self.gen_store_expr(ctx, ir, ret, last, use_value)?;
                if is_ret {
                    self.gen_ret(ir, ret.into());
                }
            }
            None => {
                self.gen_expr(ctx, ir, last, use_value, is_ret)?;
            }
        }
        Ok(())
    }

    /// Generate bytecode Ir that evaluate *expr* and assign it to a temporary register.
    fn gen_temp_expr(
        &mut self,
        ctx: &mut FnStore,
        ir: &mut IrContext,
        expr: Node,
    ) -> Result<BcTemp> {
        self.gen_expr(ctx, ir, expr, true, false)?;
        Ok(self.pop())
    }

    /// Generate bytecode Ir for binary operations.
    fn gen_binop(
        &mut self,
        ctx: &mut FnStore,
        ir: &mut IrContext,
        op: BinOp,
        lhs: Node,
        rhs: Node,
        dst: Option<BcLocal>,
    ) -> Result<()> {
        match op {
            BinOp::Add => self.gen_add(ctx, ir, dst, lhs, rhs)?,
            BinOp::Sub => self.gen_sub(ctx, ir, dst, lhs, rhs)?,
            BinOp::Mul => self.gen_mul(ctx, ir, dst, lhs, rhs)?,
            BinOp::Div => self.gen_div(ctx, ir, dst, lhs, rhs)?,
            BinOp::BitOr => self.gen_bitor(ctx, ir, dst, lhs, rhs)?,
            BinOp::BitAnd => self.gen_bitand(ctx, ir, dst, lhs, rhs)?,
            BinOp::BitXor => self.gen_bitxor(ctx, ir, dst, lhs, rhs)?,
            BinOp::Shr => self.gen_shr(ctx, ir, dst, lhs, rhs)?,
            BinOp::Shl => self.gen_shl(ctx, ir, dst, lhs, rhs)?,
            BinOp::Eq => self.gen_cmp(ctx, ir, dst, CmpKind::Eq, lhs, rhs)?,
            BinOp::Ne => self.gen_cmp(ctx, ir, dst, CmpKind::Ne, lhs, rhs)?,
            BinOp::Ge => self.gen_cmp(ctx, ir, dst, CmpKind::Ge, lhs, rhs)?,
            BinOp::Gt => self.gen_cmp(ctx, ir, dst, CmpKind::Gt, lhs, rhs)?,
            BinOp::Le => self.gen_cmp(ctx, ir, dst, CmpKind::Le, lhs, rhs)?,
            BinOp::Lt => self.gen_cmp(ctx, ir, dst, CmpKind::Lt, lhs, rhs)?,
            _ => return Err(MonorubyErr::unsupported_operator(op)),
        };
        Ok(())
    }

    /// Generate bytecode Ir for *expr*.
    fn gen_expr(
        &mut self,
        ctx: &mut FnStore,
        ir: &mut IrContext,
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
        match expr.kind {
            NodeKind::Nil => self.gen_nil(ir, None),
            NodeKind::Bool(b) => self.gen_const(ctx, ir, None, Value::bool(b)),
            NodeKind::SelfValue => self.gen_temp_mov(ir, BcReg::Self_),
            NodeKind::Integer(i) => {
                self.gen_integer(ctx, ir, None, i);
            }
            NodeKind::Bignum(bigint) => {
                self.gen_bigint(ctx, ir, None, bigint);
            }
            NodeKind::Float(f) => {
                self.gen_float(ctx, ir, None, f);
            }
            NodeKind::UnOp(op, box rhs) => {
                assert!(op == UnOp::Neg);
                match rhs.kind {
                    //NodeKind::Integer(i) => self.gen_integer(ctx, ir, None, -i),
                    NodeKind::Float(f) => self.gen_float(ctx, ir, None, -f),
                    _ => {
                        self.gen_expr(ctx, ir, rhs, true, false)?;
                        self.gen_neg(ir, None);
                    }
                };
            }
            NodeKind::AssignOp(op, box lhs, box rhs) => {
                let local = match lhs.kind {
                    NodeKind::LocalVar(lhs) | NodeKind::Ident(lhs) => self.find_local(lhs),
                    _ => return Err(MonorubyErr::unsupported_lhs(lhs.kind)),
                };
                self.gen_binop(ctx, ir, op, lhs, rhs, Some(local))?;
                if is_ret {
                    self.gen_ret(ir, Some(local));
                } else if use_value {
                    self.gen_temp_mov(ir, local.into());
                }
                return Ok(());
            }
            NodeKind::BinOp(op, box lhs, box rhs) => self.gen_binop(ctx, ir, op, lhs, rhs, None)?,
            NodeKind::MulAssign(mut mlhs, mut mrhs) => {
                if mlhs.len() == 1 && mrhs.len() == 1 {
                    let (lhs, rhs) = (mlhs.remove(0), mrhs.remove(0));
                    match lhs.kind {
                        NodeKind::LocalVar(lhs) | NodeKind::Ident(lhs) => {
                            let local = self.find_local(lhs);
                            if is_ret {
                                self.gen_store_expr(ctx, ir, local, rhs, false)?;
                                self.gen_ret(ir, Some(local));
                            } else {
                                self.gen_store_expr(ctx, ir, local, rhs, use_value)?;
                            }
                        }
                        _ => return Err(MonorubyErr::unsupported_lhs(lhs.kind)),
                    }
                } else {
                    self.gen_mul_assign(ctx, ir, mlhs, mrhs, use_value, is_ret)?;
                }
                return Ok(());
            }
            NodeKind::LocalVar(ident) => {
                let local = self.load_local(ident)?;
                if is_ret {
                    self.gen_ret(ir, Some(local));
                } else if use_value {
                    self.gen_temp_mov(ir, local.into());
                }
                return Ok(());
            }
            NodeKind::MethodCall {
                box receiver,
                method,
                arglist,
                safe_nav: false,
            } if receiver.kind == NodeKind::SelfValue => {
                let (arg, len) = self.check_fast_call(ctx, ir, arglist)?;
                let ret = if use_value {
                    Some(self.push().into())
                } else {
                    None
                };
                ir.push(BcIr::FnCall(method, ret, arg, len));
                return Ok(());
            }
            NodeKind::FuncCall {
                method,
                arglist,
                safe_nav: false,
            } => {
                let (arg, len) = self.check_fast_call(ctx, ir, arglist)?;
                let ret = if use_value {
                    Some(self.push().into())
                } else {
                    None
                };
                ir.push(BcIr::FnCall(method, ret, arg, len));
                return Ok(());
            }
            NodeKind::Ident(method) => {
                let (arg, len) = self.check_fast_call_inner(ctx, ir, vec![])?;
                let ret = if use_value {
                    Some(self.push().into())
                } else {
                    None
                };
                ir.push(BcIr::FnCall(method, ret, arg, len));
                return Ok(());
            }
            NodeKind::If {
                box cond,
                box then_,
                box else_,
            } => {
                let then_pos = ir.new_label();
                let succ_pos = ir.new_label();
                let cond = self.gen_temp_expr(ctx, ir, cond)?.into();
                let inst = BcIr::CondBr(cond, then_pos);
                ir.push(inst);
                self.gen_expr(ctx, ir, else_, use_value, false)?;
                ir.push(BcIr::Br(succ_pos));
                if use_value {
                    self.pop();
                }
                ir.apply_label(then_pos);
                self.gen_expr(ctx, ir, then_, use_value, false)?;
                ir.apply_label(succ_pos);
                return Ok(());
            }
            NodeKind::While {
                box cond,
                box body,
                cond_op,
            } => {
                assert!(cond_op);
                return self.gen_while(ctx, ir, cond, body, use_value);
            }
            NodeKind::For {
                param,
                box iter,
                body,
            } => {
                return self.gen_for(ctx, ir, param, iter, body, use_value);
            }
            NodeKind::Break(box val) => {
                let (_kind, break_pos, ret_reg) = match ir.loops.last() {
                    Some(data) => data.clone(),
                    None => return Err(MonorubyErr::escape_from_eval()),
                };
                match ret_reg {
                    Some(reg) => {
                        let temp = self.gen_temp_expr(ctx, ir, val)?;
                        ir.push(BcIr::Mov(reg, temp.into()));
                    }
                    None => {}
                }
                ir.push(BcIr::Br(break_pos));
                return Ok(());
            }
            NodeKind::Return(box expr) => {
                if let Some(local) = is_local(&expr) {
                    let local = self.load_local(local)?;
                    self.gen_ret(ir, Some(local));
                } else {
                    self.gen_expr(ctx, ir, expr, true, false)?;
                    self.gen_ret(ir, None);
                }
                return Ok(());
            }
            NodeKind::CompStmt(nodes) => {
                return self.gen_comp_stmts(ctx, ir, nodes, None, use_value, is_ret)
            }
            NodeKind::Begin {
                box body,
                rescue,
                else_: None,
                ensure: None,
            } => {
                assert!(rescue.len() == 0);
                self.gen_expr(ctx, ir, body, use_value, is_ret)?;
                return Ok(());
            }
            NodeKind::MethodDef(name, params, box node, _lv) => {
                self.gen_method_def(ctx, ir, name, params, node)?;
                if use_value {
                    // TODO: This should be a Symbol.
                    self.gen_nil(ir, None);
                }
                return Ok(());
            }
            _ => return Err(MonorubyErr::unsupported_node(expr.kind)),
        }
        if !use_value {
            self.pop();
        }
        Ok(())
    }

    fn gen_store_expr(
        &mut self,
        ctx: &mut FnStore,
        ir: &mut IrContext,
        local: BcLocal,
        rhs: Node,
        use_value: bool,
    ) -> Result<()> {
        match rhs.kind {
            NodeKind::Nil => self.gen_nil(ir, Some(local)),
            NodeKind::Bool(b) => self.gen_const(ctx, ir, Some(local), Value::bool(b)),
            NodeKind::SelfValue => self.gen_mov(ir, local.into(), BcReg::Self_),
            NodeKind::Integer(i) => {
                self.gen_integer(ctx, ir, Some(local), i);
            }
            NodeKind::Bignum(bigint) => {
                self.gen_bigint(ctx, ir, Some(local), bigint);
            }
            NodeKind::Float(f) => {
                self.gen_float(ctx, ir, Some(local), f);
            }
            NodeKind::UnOp(op, box rhs) => {
                assert!(op == UnOp::Neg);
                match rhs.kind {
                    NodeKind::Integer(i) => self.gen_integer(ctx, ir, Some(local), -i),
                    NodeKind::Float(f) => self.gen_float(ctx, ir, Some(local), -f),
                    _ => {
                        self.gen_store_expr(ctx, ir, local, rhs, false)?;
                        self.gen_neg(ir, Some(local));
                    }
                };
            }
            NodeKind::BinOp(op, box lhs, box rhs) => {
                self.gen_binop(ctx, ir, op, lhs, rhs, Some(local))?
            }
            NodeKind::MulAssign(mut mlhs, mut mrhs) => {
                if mlhs.len() == 1 && mrhs.len() == 1 {
                    let (lhs, rhs) = (mlhs.remove(0), mrhs.remove(0));
                    match lhs.kind {
                        NodeKind::LocalVar(lhs) | NodeKind::Ident(lhs) => {
                            let src = self.find_local(lhs);
                            self.gen_store_expr(ctx, ir, src, rhs, false)?;
                            self.gen_mov(ir, local.into(), src.into());
                        }
                        _ => return Err(MonorubyErr::unsupported_lhs(lhs.kind)),
                    }
                } else {
                    self.gen_mul_assign(ctx, ir, mlhs, mrhs, true, false)?;
                    let temp = self.pop().into();
                    self.gen_mov(ir, local.into(), temp);
                }
            }
            NodeKind::LocalVar(ident) => {
                let local2 = self.load_local(ident)?;
                self.gen_mov(ir, local.into(), local2.into());
            }
            NodeKind::MethodCall {
                box receiver,
                method,
                arglist,
                safe_nav: false,
            } if receiver.kind == NodeKind::SelfValue => {
                let (arg, len) = self.check_fast_call(ctx, ir, arglist)?;
                let inst = BcIr::FnCall(method, Some(local.into()), arg, len);
                ir.push(inst);
            }
            NodeKind::FuncCall {
                method,
                arglist,
                safe_nav: false,
            } => {
                let (arg, len) = self.check_fast_call(ctx, ir, arglist)?;
                let inst = BcIr::FnCall(method, Some(local.into()), arg, len);
                ir.push(inst);
            }
            NodeKind::Return(_) => unreachable!(),
            NodeKind::CompStmt(nodes) => {
                return self.gen_comp_stmts(ctx, ir, nodes, Some(local), use_value, false)
            }
            _ => {
                let ret = self.next_reg();
                self.gen_expr(ctx, ir, rhs, true, false)?;
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
        name: IdentId,
        params: Vec<FormalParam>,
        node: Node,
    ) -> Result<()> {
        let mut args = vec![];
        for param in params {
            match param.kind {
                ParamKind::Param(name) => args.push(name),
                _ => return Err(MonorubyErr::unsupported_parameter_kind(param.kind)),
            }
        }
        let func_id = ctx.functions.add_normal_func(Some(name), args, node);
        ir.push(BcIr::MethodDef(name, func_id));
        Ok(())
    }

    fn gen_args(
        &mut self,
        ctx: &mut FnStore,
        ir: &mut IrContext,
        args: Vec<Node>,
    ) -> Result<BcTemp> {
        let arg = self.next_reg();
        for arg in args {
            self.gen_expr(ctx, ir, arg, true, false)?;
        }
        Ok(arg)
    }

    fn check_fast_call(
        &mut self,
        ctx: &mut FnStore,
        ir: &mut IrContext,
        arglist: ArgList,
    ) -> Result<(BcTemp, usize)> {
        assert!(arglist.kw_args.len() == 0);
        assert!(arglist.hash_splat.len() == 0);
        assert!(arglist.block.is_none());
        assert!(!arglist.delegate);
        self.check_fast_call_inner(ctx, ir, arglist.args)
    }

    fn check_fast_call_inner(
        &mut self,
        ctx: &mut FnStore,
        ir: &mut IrContext,
        args: Vec<Node>,
    ) -> Result<(BcTemp, usize)> {
        let len = args.len();
        let arg = self.gen_args(ctx, ir, args)?;
        self.temp -= len as u16;
        Ok((arg, len))
    }

    fn gen_binary(
        &mut self,
        ctx: &mut FnStore,
        ir: &mut IrContext,
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
                let rhs = self.gen_temp_expr(ctx, ir, rhs)?.into();
                (lhs, rhs)
            }
            (None, Some(rhs)) => {
                let lhs = self.gen_temp_expr(ctx, ir, lhs)?.into();
                let rhs = self.find_local(rhs).into();
                (lhs, rhs)
            }
            (None, None) => {
                self.gen_expr(ctx, ir, lhs, true, false)?;
                self.gen_expr(ctx, ir, rhs, true, false)?;
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
        dst: Option<BcLocal>,
        lhs: Node,
    ) -> Result<(BcReg, BcReg)> {
        let lhs = match is_local(&lhs) {
            Some(lhs) => self.find_local(lhs).into(),
            None => self.gen_temp_expr(ctx, ir, lhs)?.into(),
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
                dst: Option<BcLocal>,
                lhs: Node,
                rhs: Node,
            ) -> Result<()> {
                let (dst, lhs, rhs) = self.gen_binary(ctx, ir, dst, lhs, rhs)?;
                ir.push(BcIr::$inst(dst, lhs, rhs));
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
                dst: Option<BcLocal>,
                lhs: Node,
                rhs: Node,
            ) -> Result<()> {
                if let Some(i) = is_smi(&rhs) {
                    let (dst, lhs) = self.gen_singular(ctx, ir, dst, lhs)?;
                    ir.push(BcIr::[<$inst ri>](dst, lhs, i));
                } else {
                    let (dst, lhs, rhs) = self.gen_binary(ctx, ir, dst, lhs, rhs)?;
                    ir.push(BcIr::$inst(dst, lhs, rhs));
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
        dst: Option<BcLocal>,
        kind: CmpKind,
        lhs: Node,
        rhs: Node,
    ) -> Result<()> {
        if let Some(i) = is_smi(&rhs) {
            let (dst, lhs) = self.gen_singular(ctx, ir, dst, lhs)?;
            ir.push(BcIr::Cmpri(kind, dst, lhs, i));
        } else {
            let (dst, lhs, rhs) = self.gen_binary(ctx, ir, dst, lhs, rhs)?;
            ir.push(BcIr::Cmp(kind, dst, lhs, rhs));
        }
        Ok(())
    }

    fn gen_mul_assign(
        &mut self,
        ctx: &mut FnStore,
        ir: &mut IrContext,
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
            self.gen_expr(ctx, ir, rhs, true, false)?;
        }
        // Assign values to left-hand side expressions.
        for lhs in mlhs {
            match lhs.kind {
                NodeKind::LocalVar(lhs) | NodeKind::Ident(lhs) => {
                    let local = self.find_local(lhs);
                    self.gen_mov(ir, local.into(), temp_reg.into());
                }
                _ => return Err(MonorubyErr::unsupported_lhs(lhs.kind)),
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
        param: Vec<IdentId>,
        iter: Node,
        body: BlockInfo,
        use_value: bool,
    ) -> Result<()> {
        assert_eq!(1, param.len());
        let counter = self.find_local(param[0]);
        let break_pos = ir.new_label();
        ir.loops.push((
            LoopKind::For,
            break_pos,
            match use_value {
                true => Some(self.next_reg().into()),
                false => None,
            },
        ));
        if let NodeKind::Range {
            box start,
            box end,
            exclude_end: false,
            ..
        } = iter.kind
        {
            let loop_entry = ir.new_label();
            let loop_exit = ir.new_label();
            self.gen_store_expr(ctx, ir, counter, start, false)?;
            self.gen_temp_expr(ctx, ir, end)?;
            let end = self.push().into();

            ir.apply_label(loop_entry);
            let dst = self.push().into();
            ir.push(BcIr::Cmp(CmpKind::Gt, dst, counter.into(), end));
            ir.push(BcIr::CondBr(dst, loop_exit));
            self.pop();

            self.gen_expr(ctx, ir, *body.body, false, false)?;

            ir.push(BcIr::Addri(counter.into(), counter.into(), 1));
            ir.push(BcIr::Br(loop_entry));

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
        let cond = self.gen_temp_expr(ctx, ir, cond)?.into();
        let inst = BcIr::CondNotBr(cond, succ_pos);
        ir.push(inst);
        self.gen_expr(ctx, ir, body, false, false)?;
        ir.push(BcIr::Br(cond_pos));
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

    fn add_callsite(
        &self,
        store: &mut FnStore,
        name: IdentId,
        args: BcTemp,
        len: usize,
    ) -> CallsiteId {
        let info = CallsiteInfo {
            name,
            args: self.get_index(&BcReg::from(args)),
            len: len as u16,
            cache: (usize::MAX, FuncId::default()),
        };
        let id = store.callsite_info.len();
        store.callsite_info.push(info);
        CallsiteId(id as u32)
    }

    pub(crate) fn ir_to_bytecode(
        &mut self,
        ir: IrContext,
        id_store: &IdentifierTable,
        store: &mut FnStore,
    ) {
        let mut ops = vec![];
        for (idx, inst) in ir.ir.iter().enumerate() {
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
                BcIr::Const(reg, num) => BcOp::Const(self.get_index(reg), *num),
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
                    match kind {
                        CmpKind::Eq => BcOp::Eq(dst, lhs, rhs),
                        CmpKind::Ne => BcOp::Ne(dst, lhs, rhs),
                        CmpKind::Gt => BcOp::Gt(dst, lhs, rhs),
                        CmpKind::Ge => BcOp::Ge(dst, lhs, rhs),
                        CmpKind::Lt => BcOp::Lt(dst, lhs, rhs),
                        CmpKind::Le => BcOp::Le(dst, lhs, rhs),
                    }
                }
                BcIr::Cmpri(kind, dst, lhs, rhs) => {
                    let dst = self.get_index(dst);
                    let lhs = self.get_index(lhs);
                    let rhs = *rhs;
                    match kind {
                        CmpKind::Eq => BcOp::Eqri(dst, lhs, rhs),
                        CmpKind::Ne => BcOp::Neri(dst, lhs, rhs),
                        CmpKind::Gt => BcOp::Gtri(dst, lhs, rhs),
                        CmpKind::Ge => BcOp::Geri(dst, lhs, rhs),
                        CmpKind::Lt => BcOp::Ltri(dst, lhs, rhs),
                        CmpKind::Le => BcOp::Leri(dst, lhs, rhs),
                    }
                }
                BcIr::Ret(reg) => BcOp::Ret(self.get_index(reg)),
                BcIr::Mov(dst, src) => BcOp::Mov(self.get_index(dst), self.get_index(src)),
                BcIr::FnCall(id, ret, args, len) => {
                    let id = self.add_callsite(store, *id, *args, *len);
                    let ret = ret.map_or(0, |ret| self.get_index(&ret));
                    BcOp::FnCall(ret, id)
                }
                BcIr::MethodDef(name, func_id) => {
                    BcOp::MethodDef(store.add_method_def(*name, *func_id))
                }
            };
            ops.push(op.to_u64());
        }
        self.bytecode = ops;
        #[cfg(feature = "emit-bc")]
        self.dump(id_store, store);
    }
}
