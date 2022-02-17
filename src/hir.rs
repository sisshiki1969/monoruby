use std::collections::HashMap;

use super::*;

///
/// Instructions of High-level IR.
///
#[derive(Clone, Debug, PartialEq)]
pub enum HIR {
    Integer(SsaReg, i32),
    Float(SsaReg, f64),
    IntAsFloat(HIRUnop),
    INeg(HIRUnop),
    FNeg(HIRUnop),
    IAdd(HIRBinop2),
    ISub(HIRBinop2),
    IMul(HIRBinop),
    IDiv(HIRBinop),
    FAdd(HIRBinop2),
    FSub(HIRBinop2),
    FMul(HIRBinop2),
    FDiv(HIRBinop2),
    Ret(HIROperand),
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
pub struct HIRBinop2 {
    /// Register ID of return value.
    pub ret: SsaReg,
    /// Register ID of left-hand side.
    pub lhs: HIROperand,
    /// Register ID of right-hand side.
    pub rhs: HIROperand,
}

///
/// Unary operations.
///
#[derive(Clone, Debug, PartialEq)]
pub struct HIRUnop {
    /// Register ID of return value.
    pub ret: SsaReg,
    /// Register ID of source value.
    pub src: HIROperand,
}

#[derive(Clone, PartialEq)]
pub enum HIROperand {
    Reg(SsaReg),
    Const(Value),
}

impl std::fmt::Debug for HIROperand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Reg(r) => write!(f, "%{}", r.to_usize()),
            Self::Const(c) => write!(f, "{:?}", c),
        }
    }
}

impl HIROperand {
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

/*#[derive(Clone, PartialEq)]
pub enum Const {
    Integer(i32),
    Float(f64),
}*/

/*impl std::fmt::Debug for Const {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Integer(n) => write!(f, "{}: i32", n),
            Self::Float(n) => write!(f, "{}: f64", n),
        }
    }
}*/

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
    pub insts: Vec<HIR>,
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
                HIR::Integer(ret, i) => format!("%{}: {:?} = {}: i32", ret, self[*ret].ty, i),
                HIR::Float(ret, f) => format!("%{}: {:?} = {}: f64", ret, self[*ret].ty, f),
                HIR::IntAsFloat(op) => {
                    format!(
                        "%{}: {:?} = i32_to_f64 {:?}",
                        op.ret, self[op.ret].ty, op.src
                    )
                }
                HIR::INeg(op) => format!("%{}: {:?} = ineg {:?}", op.ret, self[op.ret].ty, op.src),
                HIR::FNeg(op) => format!("%{}: {:?} = fneg {:?}", op.ret, self[op.ret].ty, op.src),
                HIR::IAdd(op) => format!(
                    "%{}: {:?} = iadd {:?}, {:?}",
                    op.ret, self[op.ret].ty, op.lhs, op.rhs
                ),
                HIR::FAdd(op) => format!(
                    "%{}: {:?} = fadd {:?}, {:?}",
                    op.ret, self[op.ret].ty, op.lhs, op.rhs
                ),
                HIR::ISub(op) => format!(
                    "%{}: {:?} = isub {:?}, {:?}",
                    op.ret, self[op.ret].ty, op.lhs, op.rhs
                ),
                HIR::FSub(op) => format!(
                    "%{}: {:?} = fsub {:?}, {:?}",
                    op.ret, self[op.ret].ty, op.lhs, op.rhs
                ),
                HIR::IMul(op) => format!(
                    "%{}: {:?} = imul %{}, %{}",
                    op.ret, self[op.ret].ty, op.lhs, op.rhs
                ),
                HIR::FMul(op) => format!(
                    "%{}: {:?} = fmul {:?}, {:?}",
                    op.ret, self[op.ret].ty, op.lhs, op.rhs
                ),
                HIR::IDiv(op) => format!(
                    "%{}: {:?} = idiv %{}, %{}",
                    op.ret, self[op.ret].ty, op.lhs, op.rhs
                ),
                HIR::FDiv(op) => format!(
                    "%{}: {:?} = fdiv {:?}, {:?}",
                    op.ret, self[op.ret].ty, op.lhs, op.rhs
                ),
                HIR::Ret(ret) => format!("ret {:?}", ret),
                HIR::LocalStore(ident, rhs) => {
                    format!("${}: {:?} = %{}", ident.0, ident.1, rhs)
                }
                HIR::LocalLoad(ident, lhs) => {
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

    fn add_assign(&mut self, hir: HIR, ty: Type) -> SsaReg {
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
        self.add_assign(HIR::Integer(self.next_reg(), i), Type::Integer)
    }

    fn new_float(&mut self, f: f64) -> SsaReg {
        self.add_assign(HIR::Float(self.next_reg(), f), Type::Float)
    }

    fn new_as_float(&mut self, src: SsaReg) -> SsaReg {
        let ret = self.next_reg();
        self.add_assign(
            HIR::IntAsFloat(HIRUnop {
                ret,
                src: HIROperand::Reg(src),
            }),
            Type::Float,
        )
    }

    fn new_as_float_imm(&mut self, src: i32) -> SsaReg {
        let ret = self.next_reg();
        self.add_assign(
            HIR::IntAsFloat(HIRUnop {
                ret,
                src: HIROperand::Const(Value::Integer(src)),
            }),
            Type::Float,
        )
    }

    fn new_ineg(&mut self, src: SsaReg) -> SsaReg {
        let ret = self.next_reg();
        self.add_assign(
            HIR::INeg(HIRUnop {
                ret,
                src: HIROperand::Reg(src),
            }),
            Type::Integer,
        )
    }

    fn new_fneg(&mut self, src: SsaReg) -> SsaReg {
        let ret = self.next_reg();
        self.add_assign(
            HIR::FNeg(HIRUnop {
                ret,
                src: HIROperand::Reg(src),
            }),
            Type::Float,
        )
    }

    fn new_iadd(&mut self, lhs: HIROperand, rhs: HIROperand) -> SsaReg {
        let ret = self.next_reg();
        self.add_assign(HIR::IAdd(HIRBinop2 { ret, lhs, rhs }), Type::Integer)
    }

    fn new_fadd(&mut self, lhs: HIROperand, rhs: HIROperand) -> SsaReg {
        let ret = self.next_reg();
        self.add_assign(HIR::FAdd(HIRBinop2 { ret, lhs, rhs }), Type::Float)
    }

    fn new_isub(&mut self, lhs: HIROperand, rhs: HIROperand) -> SsaReg {
        let ret = self.next_reg();
        self.add_assign(HIR::ISub(HIRBinop2 { ret, lhs, rhs }), Type::Integer)
    }

    fn new_fsub(&mut self, lhs: HIROperand, rhs: HIROperand) -> SsaReg {
        let ret = self.next_reg();
        self.add_assign(HIR::FSub(HIRBinop2 { ret, lhs, rhs }), Type::Float)
    }

    fn new_imul(&mut self, lhs: SsaReg, rhs: SsaReg) -> SsaReg {
        let ret = self.next_reg();
        self.add_assign(HIR::IMul(HIRBinop { ret, lhs, rhs }), Type::Integer)
    }

    fn new_fmul(&mut self, lhs: HIROperand, rhs: HIROperand) -> SsaReg {
        let ret = self.next_reg();
        self.add_assign(HIR::FMul(HIRBinop2 { ret, lhs, rhs }), Type::Float)
    }

    fn new_idiv(&mut self, lhs: SsaReg, rhs: SsaReg) -> SsaReg {
        let ret = self.next_reg();
        self.add_assign(HIR::IDiv(HIRBinop { ret, lhs, rhs }), Type::Integer)
    }

    fn new_fdiv(&mut self, lhs: HIROperand, rhs: HIROperand) -> SsaReg {
        let ret = self.next_reg();
        self.add_assign(HIR::FDiv(HIRBinop2 { ret, lhs, rhs }), Type::Float)
    }

    fn new_ret(&mut self, lhs: SsaReg) {
        let hir = HIR::Ret(HIROperand::Reg(lhs));
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
        let hir = HIR::LocalStore(info, rhs);
        self.insts.push(hir);
        rhs
    }

    fn new_local_load(
        &mut self,
        local_map: &mut HashMap<String, (usize, Type)>,
        ident: &String,
    ) -> SsaReg {
        let info = match local_map.get(ident) {
            Some(info) => info.clone(),
            None => panic!("undefined local variable."),
        };
        let ty = info.1;
        let hir = HIR::LocalLoad(info, self.next_reg());
        self.add_assign(hir, ty)
    }
}

macro_rules! binary_ops {
    ($self:ident, $map:ident, $lhs:ident, $rhs:ident, $i_op:ident, $f_op:ident) => {
        match ($lhs, $rhs) {
            (Expr::Integer(lhs_), Expr::Float(rhs_)) => {
                let lhs = $self.new_as_float_imm(*lhs_);
                $self.$f_op(HIROperand::reg(lhs), HIROperand::float(*rhs_))
            }
            (Expr::Integer(lhs_), Expr::Integer(rhs_)) => {
                $self.$i_op(HIROperand::integer(*lhs_), HIROperand::integer(*rhs_))
            }
            (Expr::Integer(lhs_), _) => {
                let rhs = $self.gen($map, $rhs);
                let rhs_ty = $self[rhs].ty;
                match rhs_ty {
                    Type::Integer => $self.$i_op(HIROperand::integer(*lhs_), HIROperand::reg(rhs)),
                    Type::Float => {
                        let lhs = $self.new_as_float_imm(*lhs_);
                        $self.$f_op(HIROperand::Reg(lhs), HIROperand::Reg(rhs))
                    }
                }
            }
            (Expr::Float(lhs_), Expr::Integer(rhs_)) => {
                let rhs = $self.new_as_float_imm(*rhs_);
                $self.$f_op(HIROperand::float(*lhs_), HIROperand::reg(rhs))
            }
            (Expr::Float(lhs_), Expr::Float(rhs_)) => {
                $self.$f_op(HIROperand::float(*lhs_), HIROperand::float(*rhs_))
            }
            (Expr::Float(lhs_), _) => {
                let rhs = $self.gen($map, $rhs);
                let rhs_ty = $self[rhs].ty;
                match rhs_ty {
                    Type::Integer => {
                        let rhs = $self.new_as_float(rhs);
                        $self.$f_op(HIROperand::float(*lhs_), HIROperand::reg(rhs))
                    }
                    Type::Float => $self.$f_op(HIROperand::float(*lhs_), HIROperand::reg(rhs)),
                }
            }
            (_, Expr::Integer(rhs_)) => {
                let lhs = $self.gen($map, $lhs);
                let lhs_ty = $self[lhs].ty;
                match lhs_ty {
                    Type::Integer => $self.$i_op(HIROperand::reg(lhs), HIROperand::integer(*rhs_)),
                    Type::Float => {
                        $self.$f_op(HIROperand::reg(lhs), HIROperand::float(*rhs_ as f64))
                    }
                }
            }
            (_, Expr::Float(rhs_)) => {
                let lhs = $self.gen($map, $lhs);
                let lhs_ty = $self[lhs].ty;
                match lhs_ty {
                    Type::Integer => {
                        let lhs = $self.new_as_float(lhs);
                        $self.$f_op(HIROperand::reg(lhs), HIROperand::float(*rhs_))
                    }
                    Type::Float => $self.$f_op(HIROperand::reg(lhs), HIROperand::float(*rhs_)),
                }
            }
            _ => {
                let lhs = $self.gen($map, $lhs);
                let rhs = $self.gen($map, $rhs);
                let lhs_ty = $self[lhs].ty;
                let rhs_ty = $self[rhs].ty;
                match (lhs_ty, rhs_ty) {
                    (Type::Integer, Type::Integer) => {
                        $self.$i_op(HIROperand::Reg(lhs), HIROperand::Reg(rhs))
                    }
                    (Type::Integer, Type::Float) => {
                        let lhs = $self.new_as_float(lhs);
                        $self.$f_op(HIROperand::Reg(lhs), HIROperand::Reg(rhs))
                    }
                    (Type::Float, Type::Integer) => {
                        let rhs = $self.new_as_float(rhs);
                        $self.$f_op(HIROperand::Reg(lhs), HIROperand::Reg(rhs))
                    }
                    (Type::Float, Type::Float) => {
                        $self.$f_op(HIROperand::Reg(lhs), HIROperand::Reg(rhs))
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
        ast: &Expr,
    ) -> (SsaReg, Type) {
        let ret = self.gen(local_map, ast);
        let ty = self[ret].ty;
        self.new_ret(ret);
        (ret, ty)
    }

    /// Generate HIR from an *Expr*.
    fn gen(&mut self, local_map: &mut HashMap<String, (usize, Type)>, ast: &Expr) -> SsaReg {
        match ast {
            Expr::Integer(i) => self.new_integer(*i),
            Expr::Float(f) => self.new_float(*f),
            Expr::Neg(box lhs) => {
                match lhs {
                    Expr::Integer(i) => return self.new_integer(-i),
                    Expr::Float(f) => return self.new_float(-f),
                    _ => {}
                };
                let lhs_i = self.gen(local_map, lhs);
                match self[lhs_i].ty {
                    Type::Integer => self.new_ineg(lhs_i),
                    Type::Float => self.new_fneg(lhs_i),
                }
            }
            Expr::Add(box lhs, box rhs) => {
                binary_ops!(self, local_map, lhs, rhs, new_iadd, new_fadd)
            }
            Expr::Sub(box lhs, box rhs) => {
                binary_ops!(self, local_map, lhs, rhs, new_isub, new_fsub)
            }
            Expr::Mul(box lhs, box rhs) => {
                let lhs = self.gen(local_map, lhs);
                let rhs = self.gen(local_map, rhs);
                let lhs_ty = self[lhs].ty;
                let rhs_ty = self[rhs].ty;
                match (lhs_ty, rhs_ty) {
                    (Type::Integer, Type::Integer) => self.new_imul(lhs, rhs),
                    (Type::Integer, Type::Float) => {
                        let lhs = self.new_as_float(lhs);
                        self.new_fmul(HIROperand::Reg(lhs), HIROperand::Reg(rhs))
                    }
                    (Type::Float, Type::Integer) => {
                        let rhs = self.new_as_float(rhs);
                        self.new_fmul(HIROperand::Reg(lhs), HIROperand::Reg(rhs))
                    }
                    (Type::Float, Type::Float) => {
                        self.new_fmul(HIROperand::Reg(lhs), HIROperand::Reg(rhs))
                    }
                }
            }
            Expr::Div(box lhs, box rhs) => {
                let lhs = self.gen(local_map, lhs);
                let rhs = self.gen(local_map, rhs);
                let lhs_ty = self[lhs].ty;
                let rhs_ty = self[rhs].ty;
                match (lhs_ty, rhs_ty) {
                    (Type::Integer, Type::Integer) => self.new_idiv(lhs, rhs),
                    (Type::Integer, Type::Float) => {
                        let lhs = self.new_as_float(lhs);
                        self.new_fdiv(HIROperand::Reg(lhs), HIROperand::Reg(rhs))
                    }
                    (Type::Float, Type::Integer) => {
                        let rhs = self.new_as_float(rhs);
                        self.new_fdiv(HIROperand::Reg(lhs), HIROperand::Reg(rhs))
                    }
                    (Type::Float, Type::Float) => {
                        self.new_fdiv(HIROperand::Reg(lhs), HIROperand::Reg(rhs))
                    }
                }
            }
            Expr::LocalStore(ident, box rhs) => {
                let rhs = self.gen(local_map, rhs);
                self.new_local_store(local_map, ident, rhs)
            }
            Expr::LocalLoad(ident) => self.new_local_load(local_map, ident),
        }
    }
}
