use std::collections::{BTreeSet, HashMap};

use super::parse::Span;
use super::{CmpKind, Decl, Expr, Stmt, Type, Value};

///
/// A state of MIR.
///
#[derive(Clone, PartialEq)]
pub struct MirContext {
    /// Basic blocks.
    pub basic_block: Vec<MirBasicBlock>,
    cur_bb: usize,
    /// Functions.
    pub functions: Vec<MirFunction>,
    cur_fn: usize,
}

impl std::fmt::Debug for MirContext {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "MirContxt {{")?;

        for func in &self.functions {
            writeln!(f, "\tFunction {} {{", func.name)?;
            writeln!(f, "\t\tSsaInfo {:?}", func.reginfo)?;
            for i in func.bbs.iter() {
                let bb = &self.basic_block[*i];
                writeln!(f, "\t\tBasicBlock {} {{ owner:{:?}", i, bb.owner_function)?;
                for hir in &bb.insts {
                    let s = match hir {
                        Mir::Integer(ret, i) => {
                            format!("{:?}: {:?} = {}: i32", ret, func[*ret].ty, i)
                        }
                        Mir::Float(ret, f) => {
                            format!("{:?}: {:?} = {}: f64", ret, func[*ret].ty, f)
                        }
                        Mir::CastIntFloat(op) => {
                            format!(
                                "{:?}: {:?} = cast {:?} i32 to f64",
                                op.ret, func[op.ret].ty, op.src
                            )
                        }
                        Mir::INeg(op) => {
                            format!("{:?}: {:?} = ineg {:?}", op.ret, func[op.ret].ty, op.src)
                        }
                        Mir::FNeg(op) => {
                            format!("{:?}: {:?} = fneg {:?}", op.ret, func[op.ret].ty, op.src)
                        }
                        Mir::IAdd(op) => format!(
                            "{:?}: {:?} = iadd {:?}, {:?}",
                            op.ret, func[op.ret].ty, op.lhs, op.rhs
                        ),
                        Mir::FAdd(op) => format!(
                            "{:?}: {:?} = fadd {:?}, {:?}",
                            op.ret, func[op.ret].ty, op.lhs, op.rhs
                        ),
                        Mir::ISub(op) => format!(
                            "{:?}: {:?} = isub {:?}, {:?}",
                            op.ret, func[op.ret].ty, op.lhs, op.rhs
                        ),
                        Mir::FSub(op) => format!(
                            "{:?}: {:?} = fsub {:?}, {:?}",
                            op.ret, func[op.ret].ty, op.lhs, op.rhs
                        ),
                        Mir::IMul(op) => format!(
                            "{:?}: {:?} = imul {:?}, {:?}",
                            op.ret, func[op.ret].ty, op.lhs, op.rhs
                        ),
                        Mir::FMul(op) => format!(
                            "{:?}: {:?} = fmul {:?}, {:?}",
                            op.ret, func[op.ret].ty, op.lhs, op.rhs
                        ),
                        Mir::IDiv(op) => format!(
                            "{:?}: {:?} = idiv {:?}, {:?}",
                            op.ret, func[op.ret].ty, op.lhs, op.rhs
                        ),
                        Mir::FDiv(op) => format!(
                            "{:?}: {:?} = fdiv {:?}, {:?}",
                            op.ret, func[op.ret].ty, op.lhs, op.rhs
                        ),
                        Mir::ICmp(kind, op) => format!(
                            "{:?}: {:?} = icmp {:?} {:?}, {:?}",
                            op.ret, func[op.ret].ty, kind, op.lhs, op.rhs
                        ),
                        Mir::FCmp(kind, op) => format!(
                            "{:?}: {:?} = fcmp {:?} {:?}, {:?}",
                            op.ret, func[op.ret].ty, kind, op.lhs, op.rhs
                        ),
                        Mir::Ret(ret) => format!("ret {:?}", ret),
                        Mir::LocalStore(ret, ident, rhs) => {
                            if let Some(ret) = ret {
                                format!("${}: {:?} | {:?} = {:?}", ident.0, ident.1, ret, rhs)
                            } else {
                                format!("${}: {:?} = {:?}", ident.0, ident.1, rhs)
                            }
                        }
                        Mir::LocalLoad(ident, lhs) => {
                            format!("{:?} = ${}: {:?}", lhs, ident.0, ident.1)
                        }
                        Mir::Call(id, ret, arg) => {
                            let name = &self.functions[*id].name;
                            match ret {
                                Some(ret) => format!("{:?} = call {} ({:?})", ret, name, arg),
                                None => format!("%_ = call {} ({:?})", name, arg),
                            }
                        }
                        Mir::Br(dest) => format!("br {}", dest),
                        Mir::ICmpBr(kind, lhs, rhs, then_, else_) => {
                            format!(
                                "cmpbr ({:?} {:?}, {:?}) then {} else {}",
                                kind, lhs, rhs, then_, else_
                            )
                        }
                        Mir::FCmpBr(kind, lhs, rhs, then_, else_) => {
                            format!(
                                "cmpbr ({:?} {:?}, {:?}) then {} else {}",
                                kind, lhs, rhs, then_, else_
                            )
                        }
                        Mir::CondBr(cond, then_, else_) => {
                            format!("condbr {:?} then {} else {}", cond, then_, else_)
                        }
                        Mir::Phi(ret, phi) => {
                            let phi_s = phi
                                .iter()
                                .map(|(bb, r, ty)| format!("({}, {:?}: {:?})", bb, r, ty))
                                .collect::<Vec<String>>()
                                .join(", ");
                            format!("{:?} = phi {}", ret, phi_s)
                        }
                    };
                    writeln!(f, "\t\t\t{}", s)?;
                }
                writeln!(f, "\t\t}}")?;
            }
            writeln!(f, "\t}}")?;
        }
        writeln!(f, "}}")
    }
}

impl std::ops::Deref for MirContext {
    type Target = MirBasicBlock;

    fn deref(&self) -> &Self::Target {
        &self.basic_block[self.cur_bb]
    }
}

impl std::ops::DerefMut for MirContext {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.basic_block[self.cur_bb]
    }
}

impl std::ops::Index<usize> for MirContext {
    type Output = MirBasicBlock;

    fn index(&self, i: usize) -> &MirBasicBlock {
        &self.basic_block[i]
    }
}

impl std::ops::IndexMut<usize> for MirContext {
    fn index_mut(&mut self, i: usize) -> &mut MirBasicBlock {
        &mut self.basic_block[i]
    }
}

impl MirContext {
    pub fn new() -> Self {
        let cur_bb = 0;
        let cur_fn = 0;
        let basic_block = MirBasicBlock::new(cur_fn);
        let mut function = MirFunction::new("/main".to_string(), cur_bb, vec![]);
        function.bbs.insert(cur_bb);
        MirContext {
            basic_block: vec![basic_block],
            cur_bb,
            functions: vec![function],
            cur_fn,
        }
    }

    fn new_bb(&mut self) -> usize {
        let bb = MirBasicBlock::new(self.cur_fn);
        let next = self.basic_block.len();
        self.functions[self.cur_fn].bbs.insert(next);
        self.basic_block.push(bb);
        next
    }

    fn func(&self) -> &MirFunction {
        &self.functions[self.cur_fn]
    }

    fn func_mut(&mut self) -> &mut MirFunction {
        &mut self.functions[self.cur_fn]
    }

    fn enter_new_func(&mut self, name: String, args: Vec<(String, Type)>) -> usize {
        let entry_bb = self.basic_block.len();
        let next_fn = self.functions.len();

        let bb = MirBasicBlock::new(next_fn);
        self.basic_block.push(bb);

        let mut func = MirFunction::new(name, entry_bb, args);
        func.bbs.insert(entry_bb);
        self.functions.push(func);

        self.cur_fn = next_fn;
        self.cur_bb = entry_bb;
        next_fn
    }

    fn add_assign(&mut self, hir: Mir, ty: Type) -> SsaReg {
        let ret_reg = self.next_reg();
        self.func_mut().reginfo.push(SsaRegInfo::new(ty));
        self.insts.push(hir);
        ret_reg
    }

    pub fn register_num(&self) -> usize {
        self.func().reginfo.len()
    }

    fn next_reg(&self) -> SsaReg {
        SsaReg(self.register_num())
    }

    fn new_integer(&mut self, i: i32) -> SsaReg {
        self.add_assign(Mir::Integer(self.next_reg(), i), Type::Integer)
    }

    fn new_float(&mut self, f: f64) -> SsaReg {
        self.add_assign(Mir::Float(self.next_reg(), f), Type::Float)
    }

    fn new_as_float_operand(&mut self, src: MirOperand) -> MirOperand {
        let ret = self.next_reg();
        let reg = self.add_assign(Mir::CastIntFloat(MirUnop { ret, src }), Type::Float);
        MirOperand::Reg(reg)
    }

    fn new_as_float(&mut self, src: SsaReg) -> SsaReg {
        let ret = self.next_reg();
        self.add_assign(
            Mir::CastIntFloat(MirUnop {
                ret,
                src: MirOperand::Reg(src),
            }),
            Type::Float,
        )
    }

    fn new_as_float_imm(&mut self, src: i32) -> SsaReg {
        let ret = self.next_reg();
        self.add_assign(
            Mir::CastIntFloat(MirUnop {
                ret,
                src: MirOperand::Const(Value::Integer(src)),
            }),
            Type::Float,
        )
    }

    fn new_ineg(&mut self, src: SsaReg) -> SsaReg {
        let ret = self.next_reg();
        self.add_assign(
            Mir::INeg(MirUnop {
                ret,
                src: MirOperand::Reg(src),
            }),
            Type::Integer,
        )
    }

    fn new_fneg(&mut self, src: SsaReg) -> SsaReg {
        let ret = self.next_reg();
        self.add_assign(
            Mir::FNeg(MirUnop {
                ret,
                src: MirOperand::Reg(src),
            }),
            Type::Float,
        )
    }

    fn new_iadd(&mut self, lhs: MirOperand, rhs: MirOperand) -> SsaReg {
        let ret = self.next_reg();
        self.add_assign(Mir::IAdd(MirBinop2 { ret, lhs, rhs }), Type::Integer)
    }

    fn new_fadd(&mut self, lhs: MirOperand, rhs: MirOperand) -> SsaReg {
        let ret = self.next_reg();
        self.add_assign(Mir::FAdd(MirBinop2 { ret, lhs, rhs }), Type::Float)
    }

    fn new_isub(&mut self, lhs: MirOperand, rhs: MirOperand) -> SsaReg {
        let ret = self.next_reg();
        self.add_assign(Mir::ISub(MirBinop2 { ret, lhs, rhs }), Type::Integer)
    }

    fn new_fsub(&mut self, lhs: MirOperand, rhs: MirOperand) -> SsaReg {
        let ret = self.next_reg();
        self.add_assign(Mir::FSub(MirBinop2 { ret, lhs, rhs }), Type::Float)
    }

    fn new_imul(&mut self, lhs: SsaReg, rhs: SsaReg) -> SsaReg {
        let ret = self.next_reg();
        self.add_assign(Mir::IMul(MirBinop { ret, lhs, rhs }), Type::Integer)
    }

    fn new_fmul(&mut self, lhs: MirOperand, rhs: MirOperand) -> SsaReg {
        let ret = self.next_reg();
        self.add_assign(Mir::FMul(MirBinop2 { ret, lhs, rhs }), Type::Float)
    }

    fn new_idiv(&mut self, lhs: SsaReg, rhs: SsaReg) -> SsaReg {
        let ret = self.next_reg();
        self.add_assign(Mir::IDiv(MirBinop { ret, lhs, rhs }), Type::Integer)
    }

    fn new_fdiv(&mut self, lhs: MirOperand, rhs: MirOperand) -> SsaReg {
        let ret = self.next_reg();
        self.add_assign(Mir::FDiv(MirBinop2 { ret, lhs, rhs }), Type::Float)
    }

    fn new_icmp(&mut self, kind: CmpKind, lhs: MirOperand, rhs: MirOperand) -> SsaReg {
        let ret = self.next_reg();
        self.add_assign(Mir::ICmp(kind, MirBinop2 { ret, lhs, rhs }), Type::Bool)
    }

    fn new_fcmp(&mut self, kind: CmpKind, lhs: SsaReg, rhs: SsaReg) -> SsaReg {
        let ret = self.next_reg();
        self.add_assign(Mir::FCmp(kind, MirBinop { ret, lhs, rhs }), Type::Bool)
    }

    fn new_call(&mut self, name: &str, args: Vec<MirOperand>, ty: Type) -> Result<SsaReg> {
        let ret = self.next_reg();
        let id = self.get_function(&name)?;
        Ok(self.add_assign(Mir::Call(id, Some(ret), args), ty))
    }

    fn new_call_nouse(&mut self, name: &str, args: Vec<MirOperand>) -> Result<()> {
        let id = self.get_function(&name)?;
        let hir = Mir::Call(id, None, args);
        self.insts.push(hir);
        Ok(())
    }

    fn get_function(&mut self, name: &str) -> Result<usize> {
        let id = self
            .functions
            .iter()
            .enumerate()
            .find(|(_, func)| &func.name == name)
            .ok_or(MirErr::UndefinedMethod(name.to_string()))?
            .0;
        Ok(id)
    }

    fn new_ret(&mut self, lhs: SsaReg) {
        let hir = Mir::Ret(MirOperand::Reg(lhs));
        self.insts.push(hir);
    }

    fn new_local_store(
        &mut self,
        local_map: &mut HashMap<String, (usize, Type)>,
        ident: &String,
        rhs: SsaReg,
    ) -> Result<SsaReg> {
        let ty = self.func()[rhs].ty;
        let info = self.add_local_var_if_new(local_map, ident, ty);
        if info.1 != ty {
            return Err(MirErr::TypeMismatch(info.1, ty));
        }
        let ret = self.next_reg();
        self.add_assign(Mir::LocalStore(Some(ret), info, rhs), ty);
        Ok(ret)
    }

    fn add_local_var_if_new(
        &mut self,
        local_map: &mut HashMap<String, (usize, Type)>,
        ident: &String,
        ty: Type,
    ) -> (usize, Type) {
        let len = local_map.len();
        match local_map.get(ident) {
            Some(info) => info.clone(),
            None => {
                let info = (len, ty);
                local_map.insert(ident.to_string(), info.clone());
                info
            }
        }
    }

    fn new_local_store_nouse(
        &mut self,
        local_map: &mut HashMap<String, (usize, Type)>,
        ident: &String,
        rhs: SsaReg,
    ) -> Result<()> {
        let ty = self.func()[rhs].ty;
        let len = local_map.len();
        let info = match local_map.get(ident) {
            Some(info) => info.clone(),
            None => {
                let info = (len, ty);
                local_map.insert(ident.to_string(), info.clone());
                info
            }
        };
        if info.1 != ty {
            return Err(MirErr::TypeMismatch(info.1, ty));
        }
        let hir = Mir::LocalStore(None, info, rhs);
        self.insts.push(hir);
        Ok(())
    }

    fn new_local_load(
        &mut self,
        local_map: &mut HashMap<String, (usize, Type)>,
        ident: &String,
    ) -> Result<SsaReg> {
        let info = match local_map.get(ident) {
            Some(info) => info.clone(),
            None => return Err(MirErr::UndefinedLocal(ident.clone())),
        };
        let ty = info.1;
        let hir = Mir::LocalLoad(info, self.next_reg());
        Ok(self.add_assign(hir, ty))
    }

    fn new_phi(&mut self, phi: Vec<(usize, SsaReg, Type)>) -> SsaReg {
        let ty = phi[0].2;
        assert!(phi.iter().all(|(_, _, ty)| ty == ty));
        let ret = self.next_reg();
        self.add_assign(Mir::Phi(ret, phi), ty)
    }
}

#[derive(Clone, PartialEq)]
pub struct MirFunction {
    pub name: String,
    pub entry_bb: usize,
    pub ret: Option<SsaReg>,
    pub ret_ty: Option<Type>,
    /// SSA register information.
    reginfo: Vec<SsaRegInfo>,
    pub bbs: BTreeSet<usize>,
    pub locals: HashMap<String, (usize, Type)>,
    pub args: Vec<(String, Type)>,
}

impl std::ops::Index<SsaReg> for MirFunction {
    type Output = SsaRegInfo;

    fn index(&self, i: SsaReg) -> &SsaRegInfo {
        &self.reginfo[i.to_usize()]
    }
}

impl std::ops::IndexMut<SsaReg> for MirFunction {
    fn index_mut(&mut self, i: SsaReg) -> &mut SsaRegInfo {
        &mut self.reginfo[i.to_usize()]
    }
}

impl MirFunction {
    fn new(name: String, entry_bb: usize, args: Vec<(String, Type)>) -> Self {
        Self {
            name,
            entry_bb,
            ret: None,
            ret_ty: None,
            reginfo: vec![],
            bbs: BTreeSet::default(),
            locals: HashMap::default(),
            args,
        }
    }

    pub fn register_num(&self) -> usize {
        self.reginfo.len()
    }
}

#[derive(Clone, PartialEq)]
pub struct MirBasicBlock {
    /// HIR instructions.
    pub insts: Vec<Mir>,
    /// The function this bb is owned.
    pub owner_function: usize,
}

impl MirBasicBlock {
    fn new(owner_function: usize) -> Self {
        Self {
            insts: vec![],
            owner_function,
        }
    }
}

#[derive(Debug, Clone)]
pub enum MirErr {
    UndefinedLocal(String),
    UndefinedMethod(String),
    TypeMismatch(Type, Type),
}

type Result<T> = std::result::Result<T, MirErr>;

///
/// Instructions of High-level IR.
///
#[derive(Clone, Debug, PartialEq)]
pub enum Mir {
    Br(usize),
    CondBr(SsaReg, usize, usize),
    ICmpBr(CmpKind, SsaReg, MirOperand, usize, usize),
    FCmpBr(CmpKind, SsaReg, SsaReg, usize, usize),
    Phi(SsaReg, Vec<(usize, SsaReg, Type)>), // ret, [(bb, reg, type)]
    Integer(SsaReg, i32),
    Float(SsaReg, f64),
    CastIntFloat(MirUnop),
    INeg(MirUnop),
    FNeg(MirUnop),
    IAdd(MirBinop2),
    ISub(MirBinop2),
    IMul(MirBinop),
    IDiv(MirBinop),
    FAdd(MirBinop2),
    FSub(MirBinop2),
    FMul(MirBinop2),
    FDiv(MirBinop2),
    ICmp(CmpKind, MirBinop2),
    FCmp(CmpKind, MirBinop),
    Ret(MirOperand),
    LocalStore(Option<SsaReg>, (usize, Type), SsaReg), // (ret, (offset, type), rhs)
    LocalLoad((usize, Type), SsaReg),
    Call(usize, Option<SsaReg>, Vec<MirOperand>), // (id, ret, arg)
}

///
/// Binary operations.
///
#[derive(Clone, Debug, PartialEq)]
pub struct MirBinop {
    /// Register ID of return value.
    pub ret: SsaReg,
    /// Register ID of left-hand side.
    pub lhs: SsaReg,
    /// Register ID of right-hand side.
    pub rhs: SsaReg,
}

#[derive(Clone, Debug, PartialEq)]
pub struct MirBinop2 {
    /// Register ID of return value.
    pub ret: SsaReg,
    /// Register ID of left-hand side.
    pub lhs: MirOperand,
    /// Register ID of right-hand side.
    pub rhs: MirOperand,
}

///
/// Unary operations.
///
#[derive(Clone, Debug, PartialEq)]
pub struct MirUnop {
    /// Register ID of return value.
    pub ret: SsaReg,
    /// Register ID of source value.
    pub src: MirOperand,
}

#[derive(Clone, PartialEq)]
pub enum MirOperand {
    Reg(SsaReg),
    Const(Value),
}

impl std::fmt::Debug for MirOperand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Reg(r) => write!(f, "%{}", r.to_usize()),
            Self::Const(c) => write!(f, "{:?}", c),
        }
    }
}

///
/// ID of SSA registers.
///
#[derive(Clone, Copy, PartialEq)]
pub struct SsaReg(usize);

impl std::fmt::Debug for SsaReg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "%{}", self.0)
    }
}

impl SsaReg {
    pub fn to_usize(self) -> usize {
        self.0
    }
}

///
/// Information of SSA registers.
///
#[derive(Clone, PartialEq)]
pub struct SsaRegInfo {
    /// *Type* of the register.
    pub ty: Type,
}

impl std::fmt::Debug for SsaRegInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.ty)
    }
}

impl SsaRegInfo {
    fn new(ty: Type) -> Self {
        Self { ty }
    }
}

macro_rules! binary_ops {
    ($self:ident, $map:ident, $lhs:ident, $rhs:ident, $i_op:ident, $f_op:ident) => {{
        let lhs = $self.gen_operand($map, &$lhs.0)?;
        let rhs = $self.gen_operand($map, &$rhs.0)?;
        let lhs_ty = $self.get_operand_ty(&lhs);
        let rhs_ty = $self.get_operand_ty(&rhs);
        match (lhs_ty, rhs_ty) {
            (Type::Integer, Type::Integer) => Ok($self.$i_op(lhs, rhs)),
            (Type::Integer, Type::Float) => {
                let lhs = $self.new_as_float_operand(lhs);
                Ok($self.$f_op(lhs, rhs))
            }
            (Type::Float, Type::Integer) => {
                let rhs = $self.new_as_float_operand(rhs);
                Ok($self.$f_op(lhs, rhs))
            }
            (Type::Float, Type::Float) => Ok($self.$f_op(lhs, rhs)),
            (ty_l, ty_r) => Err(MirErr::TypeMismatch(ty_l, ty_r)),
        }
    }};
}

impl MirContext {
    /// Generate HIR in top level from [(Stmt, Span)].
    pub fn from_ast(&mut self, ast: &[(Stmt, Span)]) -> Result<()> {
        assert_eq!(0, self.cur_fn);
        let mut local_map = HashMap::default();
        let len = ast.len();
        let ret = if len == 0 {
            self.new_integer(0)
        } else {
            self.gen_stmts(&mut local_map, ast)?
        };
        let ty = self.func()[ret].ty;
        self.func_mut().locals = local_map.clone();
        self.new_ret(ret);
        self.func_mut().ret = Some(ret);
        self.func_mut().ret_ty = Some(ty);
        Ok(())
    }

    /// Generate HIR in new function from [(Stmt, Span)].
    pub fn new_func_from_ast(
        &mut self,
        func_name: String,
        args: Vec<(String, Type)>,
        ast: &[(Expr, Span)],
    ) -> Result<usize> {
        let save = (self.cur_fn, self.cur_bb);
        let mut local_map = HashMap::default();
        args.iter().for_each(|(arg, _)| {
            self.add_local_var_if_new(&mut local_map, arg, Type::Integer);
        });
        let func = self.enter_new_func(func_name, args);
        let len = ast.len();
        let ret = if len == 0 {
            self.new_integer(0)
        } else {
            self.gen_stmts(
                &mut local_map,
                &ast.iter()
                    .map(|(expr, span)| (Stmt::Expr((expr.clone(), span.clone())), span.clone()))
                    .collect::<Vec<(Stmt, Span)>>(),
            )?
        };
        let ty = self.func()[ret].ty;
        self.func_mut().locals = local_map;
        self.new_ret(ret);
        self.func_mut().ret = Some(ret);
        self.func_mut().ret_ty = Some(ty);
        (self.cur_fn, self.cur_bb) = save;
        Ok(func)
    }

    fn gen_operand(
        &mut self,
        local_map: &mut HashMap<String, (usize, Type)>,
        expr: &Expr,
    ) -> Result<MirOperand> {
        let res = match expr {
            Expr::Integer(i) => MirOperand::Const(Value::Integer(*i)),
            Expr::Float(f) => MirOperand::Const(Value::Float(*f)),
            _ => MirOperand::Reg(self.gen_expr(local_map, expr)?),
        };
        Ok(res)
    }

    fn get_operand_ty(&self, op: &MirOperand) -> Type {
        match op {
            MirOperand::Reg(reg) => self.func()[*reg].ty,
            MirOperand::Const(val) => val.ty(),
        }
    }

    /// Generate HIR from [(Stmt, Span)].
    fn gen_stmts(
        &mut self,
        local_map: &mut HashMap<String, (usize, Type)>,
        ast: &[(Stmt, Span)],
    ) -> Result<SsaReg> {
        let len = ast.len();
        for (node, _) in &ast[..len - 1] {
            match node {
                Stmt::Expr(expr) => self.gen_expr_nouse(local_map, &expr.0)?,
                Stmt::Decl(decl) => self.gen_decl_nouse(&decl.0)?,
            }
        }
        match &ast[len - 1].0 {
            Stmt::Expr(expr) => self.gen_expr(local_map, &expr.0),
            Stmt::Decl(decl) => self.gen_decl(&decl.0),
        }
    }

    fn gen_exprs(
        &mut self,
        local_map: &mut HashMap<String, (usize, Type)>,
        ast: &[(Expr, Span)],
    ) -> Result<SsaReg> {
        let len = ast.len();
        for (expr, _) in &ast[..len - 1] {
            self.gen_expr_nouse(local_map, &expr)?;
        }
        self.gen_expr(local_map, &ast[len - 1].0)
    }

    fn gen_exprs_nouse(
        &mut self,
        local_map: &mut HashMap<String, (usize, Type)>,
        ast: &[(Expr, Span)],
    ) -> Result<()> {
        for (expr, _) in ast {
            self.gen_expr_nouse(local_map, &expr)?;
        }
        Ok(())
    }

    /// Generate HIR from an *Expr*.
    fn gen_expr(
        &mut self,
        local_map: &mut HashMap<String, (usize, Type)>,
        ast: &Expr,
    ) -> Result<SsaReg> {
        match ast {
            Expr::Integer(i) => Ok(self.new_integer(*i)),
            Expr::Float(f) => Ok(self.new_float(*f)),
            Expr::Neg(box (lhs, _)) => {
                match lhs {
                    Expr::Integer(i) => return Ok(self.new_integer(-i)),
                    Expr::Float(f) => return Ok(self.new_float(-f)),
                    _ => {}
                };
                let lhs_i = self.gen_expr(local_map, lhs)?;
                let ssa = match self.func()[lhs_i].ty {
                    Type::Integer => self.new_ineg(lhs_i),
                    Type::Float => self.new_fneg(lhs_i),
                    ty => return Err(MirErr::TypeMismatch(ty, ty)),
                };
                Ok(ssa)
            }
            Expr::Add(box lhs, box rhs) => {
                binary_ops!(self, local_map, lhs, rhs, new_iadd, new_fadd)
            }
            Expr::Sub(box lhs, box rhs) => {
                binary_ops!(self, local_map, lhs, rhs, new_isub, new_fsub)
            }
            Expr::Cmp(kind, box (lhs, _), box (rhs, _)) => {
                let lhs_op = self.gen_operand(local_map, lhs)?;
                let rhs_op = self.gen_operand(local_map, rhs)?;
                let lhs_ty = self.get_operand_ty(&lhs_op);
                let rhs_ty = self.get_operand_ty(&rhs_op);
                match (lhs_ty, rhs_ty) {
                    (Type::Integer, Type::Integer) => Ok(self.new_icmp(*kind, lhs_op, rhs_op)),
                    (Type::Integer, Type::Float) => {
                        let lhs = self.gen_expr(local_map, lhs)?;
                        let lhs = self.new_as_float(lhs);
                        let rhs = self.gen_expr(local_map, rhs)?;
                        Ok(self.new_fcmp(*kind, lhs, rhs))
                    }
                    (Type::Float, Type::Integer) => {
                        let lhs = self.gen_expr(local_map, lhs)?;
                        let rhs = self.gen_expr(local_map, rhs)?;
                        let rhs = self.new_as_float(rhs);
                        Ok(self.new_fcmp(*kind, lhs, rhs))
                    }
                    (Type::Float, Type::Float) => {
                        let lhs = self.gen_expr(local_map, lhs)?;
                        let rhs = self.gen_expr(local_map, rhs)?;
                        Ok(self.new_fcmp(*kind, lhs, rhs))
                    }
                    (ty_l, ty_r) => Err(MirErr::TypeMismatch(ty_l, ty_r)),
                }
            }
            Expr::Mul(box (lhs, _), box (rhs, _)) => {
                let lhs = self.gen_expr(local_map, lhs)?;
                let rhs = self.gen_expr(local_map, rhs)?;
                let lhs_ty = self.func()[lhs].ty;
                let rhs_ty = self.func()[rhs].ty;
                match (lhs_ty, rhs_ty) {
                    (Type::Integer, Type::Integer) => Ok(self.new_imul(lhs, rhs)),
                    (Type::Integer, Type::Float) => {
                        let lhs = self.new_as_float(lhs);
                        Ok(self.new_fmul(MirOperand::Reg(lhs), MirOperand::Reg(rhs)))
                    }
                    (Type::Float, Type::Integer) => {
                        let rhs = self.new_as_float(rhs);
                        Ok(self.new_fmul(MirOperand::Reg(lhs), MirOperand::Reg(rhs)))
                    }
                    (Type::Float, Type::Float) => {
                        Ok(self.new_fmul(MirOperand::Reg(lhs), MirOperand::Reg(rhs)))
                    }
                    (ty_l, ty_r) => Err(MirErr::TypeMismatch(ty_l, ty_r)),
                }
            }
            Expr::Div(box (lhs, _), box (rhs, _)) => {
                let lhs = self.gen_expr(local_map, lhs)?;
                let rhs = self.gen_expr(local_map, rhs)?;
                let lhs_ty = self.func()[lhs].ty;
                let rhs_ty = self.func()[rhs].ty;
                match (lhs_ty, rhs_ty) {
                    (Type::Integer, Type::Integer) => Ok(self.new_idiv(lhs, rhs)),
                    (Type::Integer, Type::Float) => {
                        let lhs = self.new_as_float(lhs);
                        Ok(self.new_fdiv(MirOperand::Reg(lhs), MirOperand::Reg(rhs)))
                    }
                    (Type::Float, Type::Integer) => {
                        let rhs = self.new_as_float(rhs);
                        Ok(self.new_fdiv(MirOperand::Reg(lhs), MirOperand::Reg(rhs)))
                    }
                    (Type::Float, Type::Float) => {
                        Ok(self.new_fdiv(MirOperand::Reg(lhs), MirOperand::Reg(rhs)))
                    }
                    (ty_l, ty_r) => Err(MirErr::TypeMismatch(ty_l, ty_r)),
                }
            }
            Expr::LocalStore(ident, box (rhs, _)) => {
                let rhs = self.gen_expr(local_map, rhs)?;
                self.new_local_store(local_map, ident, rhs)
            }
            Expr::LocalLoad(ident) => self.new_local_load(local_map, ident),
            Expr::Call(name, args) => {
                let mut arg_regs = vec![];
                for arg in args {
                    let reg = self.gen_operand(local_map, &arg.0)?;
                    assert_eq!(self.get_operand_ty(&reg), Type::Integer);
                    arg_regs.push(reg);
                }
                let ty = Type::Integer;
                self.new_call(name, arg_regs, ty)
            }
            Expr::If(box (cond_, _), then_, else_) => {
                let else_bb = self.new_bb();
                let then_bb = self.new_bb();
                let succ_bb = self.new_bb();
                self.gen_cond(cond_, then_bb, else_bb, local_map)?;
                // generate bb for else clause
                self.cur_bb = else_bb;
                // return value of else clause.
                let else_reg = self.gen_exprs(local_map, else_)?;
                // terminal bb of else clause.
                let else_bb = self.cur_bb;
                self.insts.push(Mir::Br(succ_bb));

                // generate bb for then clause
                self.cur_bb = then_bb;
                // return value of then clause.
                let then_reg = self.gen_exprs(local_map, then_)?;
                // terminal bb of then clause.
                let then_bb = self.cur_bb;
                self.insts.push(Mir::Br(succ_bb));

                // check types of return values of then and else clause.
                let then_ty = self.func()[then_reg].ty;
                let else_ty = self.func()[else_reg].ty;

                if then_ty != else_ty {
                    return Err(MirErr::TypeMismatch(then_ty, else_ty));
                }

                // generate phi on the top of successor bb.
                self.cur_bb = succ_bb;
                let ret = self.new_phi(vec![
                    (then_bb, then_reg, then_ty),
                    (else_bb, else_reg, else_ty),
                ]);
                Ok(ret)
            }
            Expr::While(box (cond, _), body) => self.gen_while(cond, body, local_map),
        }
    }

    fn gen_cond(
        &mut self,
        cond_: &Expr,
        then_bb: usize,
        else_bb: usize,
        local_map: &mut HashMap<String, (usize, Type)>,
    ) -> Result<()> {
        if let Expr::Cmp(kind, box (lhs, _), box (rhs, _)) = cond_ {
            let lhs = self.gen_expr(local_map, lhs)?;
            let lhs_ty = self.func()[lhs].ty;
            if let Expr::Integer(rhs) = rhs {
                match lhs_ty {
                    Type::Integer => {
                        self.insts.push(Mir::ICmpBr(
                            *kind,
                            lhs,
                            MirOperand::Const(Value::Integer(*rhs)),
                            then_bb,
                            else_bb,
                        ));
                    }
                    Type::Float => {
                        let rhs = self.new_as_float_imm(*rhs);
                        self.insts
                            .push(Mir::FCmpBr(*kind, lhs, rhs, then_bb, else_bb));
                    }
                    _ => return Err(MirErr::TypeMismatch(lhs_ty, Type::Integer)),
                };
            } else {
                let rhs = self.gen_expr(local_map, rhs)?;
                let rhs_ty = self.func()[rhs].ty;
                match (lhs_ty, rhs_ty) {
                    (Type::Integer, Type::Integer) => {
                        self.insts.push(Mir::ICmpBr(
                            *kind,
                            lhs,
                            MirOperand::Reg(rhs),
                            then_bb,
                            else_bb,
                        ));
                    }
                    (Type::Float, Type::Float) => {
                        self.insts
                            .push(Mir::FCmpBr(*kind, lhs, rhs, then_bb, else_bb));
                    }
                    (Type::Integer, Type::Float) => {
                        let lhs = self.new_as_float(lhs);
                        self.insts
                            .push(Mir::FCmpBr(*kind, lhs, rhs, then_bb, else_bb));
                    }
                    (Type::Float, Type::Integer) => {
                        let rhs = self.new_as_float(rhs);
                        self.insts
                            .push(Mir::FCmpBr(*kind, lhs, rhs, then_bb, else_bb));
                    }
                    (ty_l, ty_r) => return Err(MirErr::TypeMismatch(ty_l, ty_r)),
                };
            }
        } else {
            let cond_ = self.gen_expr(local_map, cond_)?;
            match self.func()[cond_].ty {
                Type::Bool => {}
                ty => return Err(MirErr::TypeMismatch(ty, Type::Bool)),
            };
            self.insts.push(Mir::CondBr(cond_, then_bb, else_bb));
        }
        Ok(())
    }

    fn gen_while(
        &mut self,
        cond: &Expr,
        body: &[(Expr, Span)],
        local_map: &mut HashMap<String, (usize, Type)>,
    ) -> Result<SsaReg> {
        let cond_bb = self.new_bb();
        self.insts.push(Mir::Br(cond_bb));
        self.cur_bb = cond_bb;
        let body_bb = self.new_bb();
        let succ_bb = self.new_bb();
        self.gen_cond(cond, body_bb, succ_bb, local_map)?;
        self.cur_bb = body_bb;
        self.gen_exprs_nouse(local_map, body)?;
        self.insts.push(Mir::Br(cond_bb));
        self.cur_bb = succ_bb;
        let ret = self.new_integer(0);
        Ok(ret)
    }

    /// Generate HIR from an *Expr*.
    fn gen_expr_nouse(
        &mut self,
        local_map: &mut HashMap<String, (usize, Type)>,
        ast: &Expr,
    ) -> Result<()> {
        match ast {
            Expr::Neg(box (lhs, _)) => {
                match lhs {
                    Expr::Integer(_) | Expr::Float(_) => {}
                    _ => self.gen_expr_nouse(local_map, lhs)?,
                };
            }
            Expr::Add(box (lhs, _), box (rhs, _)) => {
                self.gen_expr_nouse(local_map, lhs)?;
                self.gen_expr_nouse(local_map, rhs)?;
            }
            Expr::Sub(box (lhs, _), box (rhs, _)) => {
                self.gen_expr_nouse(local_map, lhs)?;
                self.gen_expr_nouse(local_map, rhs)?;
            }
            Expr::Mul(box (lhs, _), box (rhs, _)) => {
                self.gen_expr_nouse(local_map, lhs)?;
                self.gen_expr_nouse(local_map, rhs)?;
            }
            Expr::Div(box (lhs, _), box (rhs, _)) => {
                self.gen_expr_nouse(local_map, lhs)?;
                self.gen_expr_nouse(local_map, rhs)?;
            }
            Expr::LocalStore(ident, box (rhs, _)) => {
                let rhs = self.gen_expr(local_map, rhs)?;
                self.new_local_store_nouse(local_map, ident, rhs)?;
            }
            Expr::If(box (cond_, _), then_, else_) => {
                let then_bb = self.new_bb();
                let else_bb = self.new_bb();
                let succ_bb = self.new_bb();
                self.gen_cond(cond_, then_bb, else_bb, local_map)?;

                self.cur_bb = else_bb;
                self.gen_exprs_nouse(local_map, else_)?;
                self.insts.push(Mir::Br(succ_bb));

                self.cur_bb = then_bb;
                self.gen_exprs_nouse(local_map, then_)?;
                self.insts.push(Mir::Br(succ_bb));

                self.cur_bb = succ_bb;
            }
            Expr::While(box (cond, _), body) => {
                let _ = self.gen_while(cond, body, local_map)?;
            }
            Expr::Call(name, args) => {
                let mut arg_regs = vec![];
                for arg in args {
                    let reg = self.gen_operand(local_map, &arg.0)?;
                    assert_eq!(self.get_operand_ty(&reg), Type::Integer);
                    arg_regs.push(reg);
                }
                self.new_call_nouse(name, arg_regs)?;
            }
            Expr::Integer(_) => {}
            Expr::Float(_) => {}
            Expr::LocalLoad(_) => {}
            Expr::Cmp(_, _, _) => {}
        };
        Ok(())
    }

    fn gen_decl(&mut self, decl: &Decl) -> Result<SsaReg> {
        self.gen_decl_nouse(decl)?;
        Ok(self.new_integer(0))
    }

    fn gen_decl_nouse(&mut self, decl: &Decl) -> Result<()> {
        match decl {
            Decl::MethodDef(name, arg_name, body) => {
                let args = arg_name
                    .iter()
                    .map(|arg_name| (arg_name.to_string(), Type::Integer))
                    .collect();
                let _ = self.new_func_from_ast(name.to_string(), args, body)?;
                Ok(())
            }
        }
    }
}