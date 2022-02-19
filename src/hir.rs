use std::collections::HashMap;

use super::*;

#[derive(Debug, Clone)]
pub enum HirErr {
    UndefinedLocal(String),
}

type Result<T> = std::result::Result<T, HirErr>;

///
/// Instructions of High-level IR.
///
#[derive(Clone, Debug, PartialEq)]
pub enum Hir {
    Integer(SsaReg, i32),
    Float(SsaReg, f64),
    IntAsFloat(HirUnop),
    INeg(HirUnop),
    FNeg(HirUnop),
    IAdd(HirBinop2),
    ISub(HirBinop2),
    IMul(HIRBinop),
    IDiv(HIRBinop),
    FAdd(HirBinop2),
    FSub(HirBinop2),
    FMul(HirBinop2),
    FDiv(HirBinop2),
    Ret(HirOperand),
    LocalStore((usize, Type), SsaReg),
    LocalLoad((usize, Type), SsaReg),
}

///
/// Binary operations.
///
#[derive(Clone, Debug, PartialEq)]
pub struct HIRBinop {
    /// Register ID of return value.
    pub ret: SsaReg,
    /// Register ID of left-hand side.
    pub lhs: SsaReg,
    /// Register ID of right-hand side.
    pub rhs: SsaReg,
}

#[derive(Clone, Debug, PartialEq)]
pub struct HirBinop2 {
    /// Register ID of return value.
    pub ret: SsaReg,
    /// Register ID of left-hand side.
    pub lhs: HirOperand,
    /// Register ID of right-hand side.
    pub rhs: HirOperand,
}

///
/// Unary operations.
///
#[derive(Clone, Debug, PartialEq)]
pub struct HirUnop {
    /// Register ID of return value.
    pub ret: SsaReg,
    /// Register ID of source value.
    pub src: HirOperand,
}

#[derive(Clone, PartialEq)]
pub enum HirOperand {
    Reg(SsaReg),
    Const(Value),
}

impl std::fmt::Debug for HirOperand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Reg(r) => write!(f, "%{}", r.to_usize()),
            Self::Const(c) => write!(f, "{:?}", c),
        }
    }
}

impl HirOperand {
    fn integer(n: i32) -> Self {
        Self::Const(Value::Integer(n))
    }

    fn float(n: f64) -> Self {
        Self::Const(Value::Float(n))
    }

    fn reg(r: SsaReg) -> Self {
        Self::Reg(r)
    }
}

///
/// ID of SSA registers.
///
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct SsaReg(usize);

impl std::fmt::Display for SsaReg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl SsaReg {
    pub fn to_usize(self) -> usize {
        self.0
    }
}

///
/// A state of HIR.
///
#[derive(PartialEq)]
pub struct HIRContext {
    /// HIR instructions.
    pub insts: Vec<Hir>,
    /// SSA register information.
    pub reginfo: Vec<SsaRegInfo>,
    //pub local_info: &'a mut HashMap<String, (usize, Type)>,
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

impl std::fmt::Debug for HIRContext {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "HIRContext {{")?;
        for hir in &self.insts {
            let s = match hir {
                Hir::Integer(ret, i) => format!("%{}: {:?} = {}: i32", ret, self[*ret].ty, i),
                Hir::Float(ret, f) => format!("%{}: {:?} = {}: f64", ret, self[*ret].ty, f),
                Hir::IntAsFloat(op) => {
                    format!(
                        "%{}: {:?} = i32_to_f64 {:?}",
                        op.ret, self[op.ret].ty, op.src
                    )
                }
                Hir::INeg(op) => format!("%{}: {:?} = ineg {:?}", op.ret, self[op.ret].ty, op.src),
                Hir::FNeg(op) => format!("%{}: {:?} = fneg {:?}", op.ret, self[op.ret].ty, op.src),
                Hir::IAdd(op) => format!(
                    "%{}: {:?} = iadd {:?}, {:?}",
                    op.ret, self[op.ret].ty, op.lhs, op.rhs
                ),
                Hir::FAdd(op) => format!(
                    "%{}: {:?} = fadd {:?}, {:?}",
                    op.ret, self[op.ret].ty, op.lhs, op.rhs
                ),
                Hir::ISub(op) => format!(
                    "%{}: {:?} = isub {:?}, {:?}",
                    op.ret, self[op.ret].ty, op.lhs, op.rhs
                ),
                Hir::FSub(op) => format!(
                    "%{}: {:?} = fsub {:?}, {:?}",
                    op.ret, self[op.ret].ty, op.lhs, op.rhs
                ),
                Hir::IMul(op) => format!(
                    "%{}: {:?} = imul %{}, %{}",
                    op.ret, self[op.ret].ty, op.lhs, op.rhs
                ),
                Hir::FMul(op) => format!(
                    "%{}: {:?} = fmul {:?}, {:?}",
                    op.ret, self[op.ret].ty, op.lhs, op.rhs
                ),
                Hir::IDiv(op) => format!(
                    "%{}: {:?} = idiv %{}, %{}",
                    op.ret, self[op.ret].ty, op.lhs, op.rhs
                ),
                Hir::FDiv(op) => format!(
                    "%{}: {:?} = fdiv {:?}, {:?}",
                    op.ret, self[op.ret].ty, op.lhs, op.rhs
                ),
                Hir::Ret(ret) => format!("ret {:?}", ret),
                Hir::LocalStore(ident, rhs) => {
                    format!("${}: {:?} = %{}", ident.0, ident.1, rhs)
                }
                Hir::LocalLoad(ident, lhs) => {
                    format!("%{} = ${}: {:?}", lhs, ident.0, ident.1)
                }
            };
            writeln!(f, "\t{}", s)?;
        }
        write!(f, "}}")
    }
}

impl std::ops::Index<SsaReg> for HIRContext {
    type Output = SsaRegInfo;

    fn index(&self, i: SsaReg) -> &SsaRegInfo {
        &self.reginfo[i.to_usize()]
    }
}

impl std::ops::IndexMut<SsaReg> for HIRContext {
    fn index_mut(&mut self, i: SsaReg) -> &mut SsaRegInfo {
        &mut self.reginfo[i.to_usize()]
    }
}

impl HIRContext {
    pub fn new() -> Self {
        HIRContext {
            insts: vec![],
            reginfo: vec![],
        }
    }

    fn add_assign(&mut self, hir: Hir, ty: Type) -> SsaReg {
        let ret_reg = self.next_reg();
        self.reginfo.push(SsaRegInfo::new(ty));
        self.insts.push(hir);
        ret_reg
    }

    pub fn register_num(&self) -> usize {
        self.reginfo.len()
    }

    fn next_reg(&self) -> SsaReg {
        SsaReg(self.reginfo.len())
    }

    fn new_integer(&mut self, i: i32) -> SsaReg {
        self.add_assign(Hir::Integer(self.next_reg(), i), Type::Integer)
    }

    fn new_float(&mut self, f: f64) -> SsaReg {
        self.add_assign(Hir::Float(self.next_reg(), f), Type::Float)
    }

    fn new_as_float(&mut self, src: SsaReg) -> SsaReg {
        let ret = self.next_reg();
        self.add_assign(
            Hir::IntAsFloat(HirUnop {
                ret,
                src: HirOperand::Reg(src),
            }),
            Type::Float,
        )
    }

    fn new_as_float_imm(&mut self, src: i32) -> SsaReg {
        let ret = self.next_reg();
        self.add_assign(
            Hir::IntAsFloat(HirUnop {
                ret,
                src: HirOperand::Const(Value::Integer(src)),
            }),
            Type::Float,
        )
    }

    fn new_ineg(&mut self, src: SsaReg) -> SsaReg {
        let ret = self.next_reg();
        self.add_assign(
            Hir::INeg(HirUnop {
                ret,
                src: HirOperand::Reg(src),
            }),
            Type::Integer,
        )
    }

    fn new_fneg(&mut self, src: SsaReg) -> SsaReg {
        let ret = self.next_reg();
        self.add_assign(
            Hir::FNeg(HirUnop {
                ret,
                src: HirOperand::Reg(src),
            }),
            Type::Float,
        )
    }

    fn new_iadd(&mut self, lhs: HirOperand, rhs: HirOperand) -> SsaReg {
        let ret = self.next_reg();
        self.add_assign(Hir::IAdd(HirBinop2 { ret, lhs, rhs }), Type::Integer)
    }

    fn new_fadd(&mut self, lhs: HirOperand, rhs: HirOperand) -> SsaReg {
        let ret = self.next_reg();
        self.add_assign(Hir::FAdd(HirBinop2 { ret, lhs, rhs }), Type::Float)
    }

    fn new_isub(&mut self, lhs: HirOperand, rhs: HirOperand) -> SsaReg {
        let ret = self.next_reg();
        self.add_assign(Hir::ISub(HirBinop2 { ret, lhs, rhs }), Type::Integer)
    }

    fn new_fsub(&mut self, lhs: HirOperand, rhs: HirOperand) -> SsaReg {
        let ret = self.next_reg();
        self.add_assign(Hir::FSub(HirBinop2 { ret, lhs, rhs }), Type::Float)
    }

    fn new_imul(&mut self, lhs: SsaReg, rhs: SsaReg) -> SsaReg {
        let ret = self.next_reg();
        self.add_assign(Hir::IMul(HIRBinop { ret, lhs, rhs }), Type::Integer)
    }

    fn new_fmul(&mut self, lhs: HirOperand, rhs: HirOperand) -> SsaReg {
        let ret = self.next_reg();
        self.add_assign(Hir::FMul(HirBinop2 { ret, lhs, rhs }), Type::Float)
    }

    fn new_idiv(&mut self, lhs: SsaReg, rhs: SsaReg) -> SsaReg {
        let ret = self.next_reg();
        self.add_assign(Hir::IDiv(HIRBinop { ret, lhs, rhs }), Type::Integer)
    }

    fn new_fdiv(&mut self, lhs: HirOperand, rhs: HirOperand) -> SsaReg {
        let ret = self.next_reg();
        self.add_assign(Hir::FDiv(HirBinop2 { ret, lhs, rhs }), Type::Float)
    }

    fn new_ret(&mut self, lhs: SsaReg) {
        let hir = Hir::Ret(HirOperand::Reg(lhs));
        self.insts.push(hir);
    }

    fn new_local_store(
        &mut self,
        local_map: &mut HashMap<String, (usize, Type)>,
        ident: &String,
        rhs: SsaReg,
    ) -> SsaReg {
        let ty = self[rhs].ty;
        let len = local_map.len();
        let info = match local_map.get(ident) {
            Some(info) => info.clone(),
            None => {
                let info = (len, ty);
                local_map.insert(ident.to_string(), info.clone());
                info
            }
        };
        let hir = Hir::LocalStore(info, rhs);
        self.insts.push(hir);
        rhs
    }

    fn new_local_load(
        &mut self,
        local_map: &mut HashMap<String, (usize, Type)>,
        ident: &String,
    ) -> Result<SsaReg> {
        let info = match local_map.get(ident) {
            Some(info) => info.clone(),
            None => return Err(HirErr::UndefinedLocal(ident.clone())),
        };
        let ty = info.1;
        let hir = Hir::LocalLoad(info, self.next_reg());
        Ok(self.add_assign(hir, ty))
    }
}

macro_rules! binary_ops {
    ($self:ident, $map:ident, $lhs:ident, $rhs:ident, $i_op:ident, $f_op:ident) => {
        match ($lhs, $rhs) {
            (Expr::Integer(lhs_), Expr::Float(rhs_)) => {
                let lhs = $self.new_as_float_imm(*lhs_);
                Ok($self.$f_op(HirOperand::reg(lhs), HirOperand::float(*rhs_)))
            }
            (Expr::Integer(lhs_), Expr::Integer(rhs_)) => {
                Ok($self.$i_op(HirOperand::integer(*lhs_), HirOperand::integer(*rhs_)))
            }
            (Expr::Integer(lhs_), _) => {
                let rhs = $self.gen($map, $rhs)?;
                let rhs_ty = $self[rhs].ty;
                match rhs_ty {
                    Type::Integer => {
                        Ok($self.$i_op(HirOperand::integer(*lhs_), HirOperand::reg(rhs)))
                    }
                    Type::Float => {
                        let lhs = $self.new_as_float_imm(*lhs_);
                        Ok($self.$f_op(HirOperand::Reg(lhs), HirOperand::Reg(rhs)))
                    }
                }
            }
            (Expr::Float(lhs_), Expr::Integer(rhs_)) => {
                let rhs = $self.new_as_float_imm(*rhs_);
                Ok($self.$f_op(HirOperand::float(*lhs_), HirOperand::reg(rhs)))
            }
            (Expr::Float(lhs_), Expr::Float(rhs_)) => {
                Ok($self.$f_op(HirOperand::float(*lhs_), HirOperand::float(*rhs_)))
            }
            (Expr::Float(lhs_), _) => {
                let rhs = $self.gen($map, $rhs)?;
                let rhs_ty = $self[rhs].ty;
                match rhs_ty {
                    Type::Integer => {
                        let rhs = $self.new_as_float(rhs);
                        Ok($self.$f_op(HirOperand::float(*lhs_), HirOperand::reg(rhs)))
                    }
                    Type::Float => Ok($self.$f_op(HirOperand::float(*lhs_), HirOperand::reg(rhs))),
                }
            }
            (_, Expr::Integer(rhs_)) => {
                let lhs = $self.gen($map, $lhs)?;
                let lhs_ty = $self[lhs].ty;
                match lhs_ty {
                    Type::Integer => {
                        Ok($self.$i_op(HirOperand::reg(lhs), HirOperand::integer(*rhs_)))
                    }
                    Type::Float => {
                        Ok($self.$f_op(HirOperand::reg(lhs), HirOperand::float(*rhs_ as f64)))
                    }
                }
            }
            (_, Expr::Float(rhs_)) => {
                let lhs = $self.gen($map, $lhs)?;
                let lhs_ty = $self[lhs].ty;
                match lhs_ty {
                    Type::Integer => {
                        let lhs = $self.new_as_float(lhs);
                        Ok($self.$f_op(HirOperand::reg(lhs), HirOperand::float(*rhs_)))
                    }
                    Type::Float => Ok($self.$f_op(HirOperand::reg(lhs), HirOperand::float(*rhs_))),
                }
            }
            _ => {
                let lhs = $self.gen($map, $lhs)?;
                let rhs = $self.gen($map, $rhs)?;
                let lhs_ty = $self[lhs].ty;
                let rhs_ty = $self[rhs].ty;
                match (lhs_ty, rhs_ty) {
                    (Type::Integer, Type::Integer) => {
                        Ok($self.$i_op(HirOperand::Reg(lhs), HirOperand::Reg(rhs)))
                    }
                    (Type::Integer, Type::Float) => {
                        let lhs = $self.new_as_float(lhs);
                        Ok($self.$f_op(HirOperand::Reg(lhs), HirOperand::Reg(rhs)))
                    }
                    (Type::Float, Type::Integer) => {
                        let rhs = $self.new_as_float(rhs);
                        Ok($self.$f_op(HirOperand::Reg(lhs), HirOperand::Reg(rhs)))
                    }
                    (Type::Float, Type::Float) => {
                        Ok($self.$f_op(HirOperand::Reg(lhs), HirOperand::Reg(rhs)))
                    }
                }
            }
        }
    };
}

impl HIRContext {
    /// Generate HIR from AST.
    pub fn from_ast(
        &mut self,
        local_map: &mut HashMap<String, (usize, Type)>,
        ast: &[Expr],
    ) -> Result<(SsaReg, Type)> {
        let len = ast.len();
        let ret = if len == 0 {
            self.new_integer(0)
        } else {
            for node in &ast[..len - 1] {
                self.gen(local_map, node)?;
            }
            self.gen(local_map, &ast[len - 1])?
        };
        let ty = self[ret].ty;
        self.new_ret(ret);
        Ok((ret, ty))
    }

    /// Generate HIR from an *Expr*.
    fn gen(
        &mut self,
        local_map: &mut HashMap<String, (usize, Type)>,
        ast: &Expr,
    ) -> Result<SsaReg> {
        match ast {
            Expr::Integer(i) => Ok(self.new_integer(*i)),
            Expr::Float(f) => Ok(self.new_float(*f)),
            Expr::Neg(box lhs) => {
                match lhs {
                    Expr::Integer(i) => return Ok(self.new_integer(-i)),
                    Expr::Float(f) => return Ok(self.new_float(-f)),
                    _ => {}
                };
                let lhs_i = self.gen(local_map, lhs)?;
                let ssa = match self[lhs_i].ty {
                    Type::Integer => self.new_ineg(lhs_i),
                    Type::Float => self.new_fneg(lhs_i),
                };
                Ok(ssa)
            }
            Expr::Add(box lhs, box rhs) => {
                binary_ops!(self, local_map, lhs, rhs, new_iadd, new_fadd)
            }
            Expr::Sub(box lhs, box rhs) => {
                binary_ops!(self, local_map, lhs, rhs, new_isub, new_fsub)
            }
            Expr::Mul(box lhs, box rhs) => {
                let lhs = self.gen(local_map, lhs)?;
                let rhs = self.gen(local_map, rhs)?;
                let lhs_ty = self[lhs].ty;
                let rhs_ty = self[rhs].ty;
                match (lhs_ty, rhs_ty) {
                    (Type::Integer, Type::Integer) => Ok(self.new_imul(lhs, rhs)),
                    (Type::Integer, Type::Float) => {
                        let lhs = self.new_as_float(lhs);
                        Ok(self.new_fmul(HirOperand::Reg(lhs), HirOperand::Reg(rhs)))
                    }
                    (Type::Float, Type::Integer) => {
                        let rhs = self.new_as_float(rhs);
                        Ok(self.new_fmul(HirOperand::Reg(lhs), HirOperand::Reg(rhs)))
                    }
                    (Type::Float, Type::Float) => {
                        Ok(self.new_fmul(HirOperand::Reg(lhs), HirOperand::Reg(rhs)))
                    }
                }
            }
            Expr::Div(box lhs, box rhs) => {
                let lhs = self.gen(local_map, lhs)?;
                let rhs = self.gen(local_map, rhs)?;
                let lhs_ty = self[lhs].ty;
                let rhs_ty = self[rhs].ty;
                match (lhs_ty, rhs_ty) {
                    (Type::Integer, Type::Integer) => Ok(self.new_idiv(lhs, rhs)),
                    (Type::Integer, Type::Float) => {
                        let lhs = self.new_as_float(lhs);
                        Ok(self.new_fdiv(HirOperand::Reg(lhs), HirOperand::Reg(rhs)))
                    }
                    (Type::Float, Type::Integer) => {
                        let rhs = self.new_as_float(rhs);
                        Ok(self.new_fdiv(HirOperand::Reg(lhs), HirOperand::Reg(rhs)))
                    }
                    (Type::Float, Type::Float) => {
                        Ok(self.new_fdiv(HirOperand::Reg(lhs), HirOperand::Reg(rhs)))
                    }
                }
            }
            Expr::LocalStore(ident, box rhs) => {
                let rhs = self.gen(local_map, rhs)?;
                Ok(self.new_local_store(local_map, ident, rhs))
            }
            Expr::LocalLoad(ident) => self.new_local_load(local_map, ident),
        }
    }
}
