use super::*;

#[derive(Clone, PartialEq)]
pub struct HIRContext {
    pub hirs: Vec<HIR>,
    pub reginfo: Vec<SsaRegInfo>,
}

impl std::fmt::Debug for HIRContext {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for hir in &self.hirs {
            let s = match hir {
                HIR::Integer(ret, i) => format!("%{}: {:?} = {}: i32", ret, self[*ret].ty, i),
                HIR::Float(ret, f) => format!("%{}: {:?} = {}: f64", ret, self[*ret].ty, f),
                HIR::IntAsFloat(op) => {
                    format!(
                        "%{}: {:?} = i32_to_f64 %{}",
                        op.ret, self[op.ret].ty, op.src
                    )
                }
                HIR::INeg(op) => format!("%{}: {:?} = ineg %{}", op.ret, self[op.ret].ty, op.src),
                HIR::FNeg(op) => format!("%{}: {:?} = fneg %{}", op.ret, self[op.ret].ty, op.src),
                HIR::IAdd(op) => format!(
                    "%{}: {:?} = iadd %{}, %{}",
                    op.ret, self[op.ret].ty, op.lhs, op.rhs
                ),
                HIR::FAdd(op) => format!(
                    "%{}: {:?} = fadd %{}, %{}",
                    op.ret, self[op.ret].ty, op.lhs, op.rhs
                ),
                HIR::ISub(op) => format!(
                    "%{}: {:?} = isub %{}, %{}",
                    op.ret, self[op.ret].ty, op.lhs, op.rhs
                ),
                HIR::FSub(op) => format!(
                    "%{}: {:?} = fsub %{}, %{}",
                    op.ret, self[op.ret].ty, op.lhs, op.rhs
                ),
                HIR::IMul(op) => format!(
                    "%{}: {:?} = imul %{}, %{}",
                    op.ret, self[op.ret].ty, op.lhs, op.rhs
                ),
                HIR::FMul(op) => format!(
                    "%{}: {:?} = fmul %{}, %{}",
                    op.ret, self[op.ret].ty, op.lhs, op.rhs
                ),
                HIR::IDiv(op) => format!(
                    "%{}: {:?} = idiv %{}, %{}",
                    op.ret, self[op.ret].ty, op.lhs, op.rhs
                ),
                HIR::FDiv(op) => format!(
                    "%{}: {:?} = fdiv %{}, %{}",
                    op.ret, self[op.ret].ty, op.lhs, op.rhs
                ),
                HIR::Ret(ret) => format!("ret %{}: {:?}", ret, self[*ret].ty),
            };
            writeln!(f, "{}", s)?;
        }
        Ok(())
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
            hirs: vec![],
            reginfo: vec![],
        }
    }

    fn add_assign(&mut self, hir: HIR, ty: Type) -> SsaReg {
        let ret_reg = self.next_reg();
        self.reginfo.push(SsaRegInfo::new(ty));
        self.hirs.push(hir);
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
        self.add_assign(HIR::IntAsFloat(HIRUnop { ret, src }), Type::Float)
    }

    fn new_ineg(&mut self, src: SsaReg) -> SsaReg {
        let ret = self.next_reg();
        self.add_assign(HIR::INeg(HIRUnop { ret, src }), Type::Integer)
    }

    fn new_fneg(&mut self, src: SsaReg) -> SsaReg {
        let ret = self.next_reg();
        self.add_assign(HIR::FNeg(HIRUnop { ret, src }), Type::Float)
    }

    fn new_iadd(&mut self, lhs: SsaReg, rhs: SsaReg) -> SsaReg {
        let ret = self.next_reg();
        self.add_assign(HIR::IAdd(HIRBinop { ret, lhs, rhs }), Type::Integer)
    }

    fn new_fadd(&mut self, lhs: SsaReg, rhs: SsaReg) -> SsaReg {
        let ret = self.next_reg();
        self.add_assign(HIR::FAdd(HIRBinop { ret, lhs, rhs }), Type::Float)
    }

    fn new_isub(&mut self, lhs: SsaReg, rhs: SsaReg) -> SsaReg {
        let ret = self.next_reg();
        self.add_assign(HIR::ISub(HIRBinop { ret, lhs, rhs }), Type::Integer)
    }

    fn new_fsub(&mut self, lhs: SsaReg, rhs: SsaReg) -> SsaReg {
        let ret = self.next_reg();
        self.add_assign(HIR::FSub(HIRBinop { ret, lhs, rhs }), Type::Float)
    }

    fn new_imul(&mut self, lhs: SsaReg, rhs: SsaReg) -> SsaReg {
        let ret = self.next_reg();
        self.add_assign(HIR::IMul(HIRBinop { ret, lhs, rhs }), Type::Integer)
    }

    fn new_fmul(&mut self, lhs: SsaReg, rhs: SsaReg) -> SsaReg {
        let ret = self.next_reg();
        self.add_assign(HIR::FMul(HIRBinop { ret, lhs, rhs }), Type::Float)
    }

    fn new_idiv(&mut self, lhs: SsaReg, rhs: SsaReg) -> SsaReg {
        let ret = self.next_reg();
        self.add_assign(HIR::IDiv(HIRBinop { ret, lhs, rhs }), Type::Integer)
    }

    fn new_fdiv(&mut self, lhs: SsaReg, rhs: SsaReg) -> SsaReg {
        let ret = self.next_reg();
        self.add_assign(HIR::FDiv(HIRBinop { ret, lhs, rhs }), Type::Float)
    }

    fn new_ret(&mut self, lhs: SsaReg) {
        let hir = HIR::Ret(lhs);
        self.hirs.push(hir);
    }
}

impl HIRContext {
    pub fn from_ast(&mut self, ast: &Expr) -> (SsaReg, Type) {
        let ret = self.gen(ast);
        let ty = self[ret].ty;
        self.new_ret(ret);
        (ret, ty)
    }

    fn gen(&mut self, ast: &Expr) -> SsaReg {
        match ast {
            Expr::Integer(i) => self.new_integer(*i),
            Expr::Float(f) => self.new_float(*f),
            Expr::Neg(box lhs) => {
                match lhs {
                    Expr::Integer(i) => return self.new_integer(-i),
                    Expr::Float(f) => return self.new_float(-f),
                    _ => {}
                };
                let lhs_i = self.gen(lhs);
                match self[lhs_i].ty {
                    Type::Integer => self.new_ineg(lhs_i),
                    Type::Float => self.new_fneg(lhs_i),
                }
            }
            Expr::Add(box lhs, box rhs) => {
                let lhs = self.gen(lhs);
                let rhs = self.gen(rhs);
                let lhs_ty = self[lhs].ty;
                let rhs_ty = self[rhs].ty;
                match (lhs_ty, rhs_ty) {
                    (Type::Integer, Type::Integer) => self.new_iadd(lhs, rhs),
                    (Type::Integer, Type::Float) => {
                        let lhs = self.new_as_float(lhs);
                        self.new_fadd(lhs, rhs)
                    }
                    (Type::Float, Type::Integer) => {
                        let rhs = self.new_as_float(rhs);
                        self.new_fadd(lhs, rhs)
                    }
                    (Type::Float, Type::Float) => self.new_fadd(lhs, rhs),
                }
            }
            Expr::Sub(box lhs, box rhs) => {
                let lhs = self.gen(lhs);
                let rhs = self.gen(rhs);
                let lhs_ty = self[lhs].ty;
                let rhs_ty = self[rhs].ty;
                match (lhs_ty, rhs_ty) {
                    (Type::Integer, Type::Integer) => self.new_isub(lhs, rhs),
                    (Type::Integer, Type::Float) => {
                        let lhs = self.new_as_float(lhs);
                        self.new_fsub(lhs, rhs)
                    }
                    (Type::Float, Type::Integer) => {
                        let rhs = self.new_as_float(rhs);
                        self.new_fsub(lhs, rhs)
                    }
                    (Type::Float, Type::Float) => self.new_fsub(lhs, rhs),
                }
            }
            Expr::Mul(box lhs, box rhs) => {
                let lhs = self.gen(lhs);
                let rhs = self.gen(rhs);
                let lhs_ty = self[lhs].ty;
                let rhs_ty = self[rhs].ty;
                match (lhs_ty, rhs_ty) {
                    (Type::Integer, Type::Integer) => self.new_imul(lhs, rhs),
                    (Type::Integer, Type::Float) => {
                        let lhs = self.new_as_float(lhs);
                        self.new_fmul(lhs, rhs)
                    }
                    (Type::Float, Type::Integer) => {
                        let rhs = self.new_as_float(rhs);
                        self.new_fmul(lhs, rhs)
                    }
                    (Type::Float, Type::Float) => self.new_fmul(lhs, rhs),
                }
            }
            Expr::Div(box lhs, box rhs) => {
                let lhs = self.gen(lhs);
                let rhs = self.gen(rhs);
                let lhs_ty = self[lhs].ty;
                let rhs_ty = self[rhs].ty;
                match (lhs_ty, rhs_ty) {
                    (Type::Integer, Type::Integer) => self.new_idiv(lhs, rhs),
                    (Type::Integer, Type::Float) => {
                        let lhs = self.new_as_float(lhs);
                        self.new_fdiv(lhs, rhs)
                    }
                    (Type::Float, Type::Integer) => {
                        let rhs = self.new_as_float(rhs);
                        self.new_fdiv(lhs, rhs)
                    }
                    (Type::Float, Type::Float) => self.new_fdiv(lhs, rhs),
                }
            }
        }
    }
}

#[derive(Clone, PartialEq)]
pub struct SsaRegInfo {
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

#[derive(Clone, Debug, PartialEq)]
pub enum HIR {
    Integer(SsaReg, i32),
    Float(SsaReg, f64),
    IntAsFloat(HIRUnop),
    INeg(HIRUnop),
    FNeg(HIRUnop),
    IAdd(HIRBinop),
    FAdd(HIRBinop),
    ISub(HIRBinop),
    FSub(HIRBinop),
    IMul(HIRBinop),
    FMul(HIRBinop),
    IDiv(HIRBinop),
    FDiv(HIRBinop),
    Ret(SsaReg),
}

#[derive(Clone, Debug, PartialEq)]
pub struct HIRBinop {
    pub ret: SsaReg,
    pub lhs: SsaReg,
    pub rhs: SsaReg,
}

#[derive(Clone, Debug, PartialEq)]
pub struct HIRUnop {
    pub ret: SsaReg,
    pub src: SsaReg,
}
