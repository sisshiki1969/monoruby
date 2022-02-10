use super::*;

#[derive(Clone, Debug, PartialEq)]
pub struct HIRContext {
    pub hirs: Vec<HIR>,
    pub cur_reg: usize,
    pub reg_types: Vec<Type>,
}

impl HIRContext {
    pub fn new() -> Self {
        HIRContext {
            hirs: vec![],
            cur_reg: 0,
            reg_types: vec![],
        }
    }

    fn add_assign(&mut self, hir: HIR) -> usize {
        let ret_reg = self.cur_reg;
        self.reg_types.push(hir.ty);
        self.hirs.push(hir);
        self.cur_reg += 1;
        ret_reg
    }

    pub fn register_num(&self) -> usize {
        self.cur_reg
    }

    fn new_integer(&mut self, i: i32) -> usize {
        self.add_assign(HIR::new_integer(self.cur_reg, i))
    }

    fn new_float(&mut self, f: f64) -> usize {
        self.add_assign(HIR::new_float(self.cur_reg, f))
    }

    fn new_as_float(&mut self, lhs: usize) -> usize {
        self.add_assign(HIR::new_as_float(self.cur_reg, lhs))
    }

    fn new_ineg(&mut self, lhs: usize) -> usize {
        self.add_assign(HIR::new_ineg(self.cur_reg, lhs))
    }

    fn new_fneg(&mut self, lhs: usize) -> usize {
        self.add_assign(HIR::new_fneg(self.cur_reg, lhs))
    }

    fn new_iadd(&mut self, lhs: usize, rhs: usize) -> usize {
        self.add_assign(HIR::new_iadd(self.cur_reg, lhs, rhs))
    }

    fn new_fadd(&mut self, lhs: usize, rhs: usize) -> usize {
        self.add_assign(HIR::new_fadd(self.cur_reg, lhs, rhs))
    }

    fn new_isub(&mut self, lhs: usize, rhs: usize) -> usize {
        self.add_assign(HIR::new_isub(self.cur_reg, lhs, rhs))
    }

    fn new_fsub(&mut self, lhs: usize, rhs: usize) -> usize {
        self.add_assign(HIR::new_fsub(self.cur_reg, lhs, rhs))
    }

    fn new_imul(&mut self, lhs: usize, rhs: usize) -> usize {
        self.add_assign(HIR::new_imul(self.cur_reg, lhs, rhs))
    }

    fn new_fmul(&mut self, lhs: usize, rhs: usize) -> usize {
        self.add_assign(HIR::new_fmul(self.cur_reg, lhs, rhs))
    }

    fn new_idiv(&mut self, lhs: usize, rhs: usize) -> usize {
        self.add_assign(HIR::new_idiv(self.cur_reg, lhs, rhs))
    }

    fn new_fdiv(&mut self, lhs: usize, rhs: usize) -> usize {
        self.add_assign(HIR::new_fdiv(self.cur_reg, lhs, rhs))
    }
}

#[derive(Clone, PartialEq)]
pub struct HIR {
    ret_reg: usize,
    ty: Type,
    op: HIRKind,
}

impl std::fmt::Debug for HIR {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "%{}: {:?} = {:?}", self.ret_reg, self.ty, self.op)
    }
}

impl HIR {
    fn new(ret_reg: usize, ty: Type, op: HIRKind) -> Self {
        HIR { ret_reg, ty, op }
    }

    pub fn kind(&self) -> &HIRKind {
        &self.op
    }

    pub fn reg(&self) -> usize {
        self.ret_reg
    }

    fn new_integer(ret_reg: usize, i: i32) -> Self {
        HIR::new(ret_reg, Type::Integer, HIRKind::Integer(i))
    }

    fn new_float(ret_reg: usize, f: f64) -> Self {
        HIR::new(ret_reg, Type::Float, HIRKind::Float(f))
    }

    fn new_as_float(ret_reg: usize, hir: usize) -> Self {
        HIR::new(ret_reg, Type::Float, HIRKind::IntAsFloat(hir))
    }

    fn new_ineg(ret_reg: usize, lhs: usize) -> Self {
        HIR::new(ret_reg, Type::Integer, HIRKind::INeg(lhs))
    }

    fn new_fneg(ret_reg: usize, lhs: usize) -> Self {
        HIR::new(ret_reg, Type::Float, HIRKind::FNeg(lhs))
    }

    fn new_iadd(ret_reg: usize, lhs: usize, rhs: usize) -> Self {
        HIR::new(ret_reg, Type::Integer, HIRKind::IAdd(lhs, rhs))
    }

    fn new_fadd(ret_reg: usize, lhs: usize, rhs: usize) -> Self {
        HIR::new(ret_reg, Type::Float, HIRKind::FAdd(lhs, rhs))
    }

    fn new_isub(ret_reg: usize, lhs: usize, rhs: usize) -> Self {
        HIR::new(ret_reg, Type::Integer, HIRKind::ISub(lhs, rhs))
    }

    fn new_fsub(ret_reg: usize, lhs: usize, rhs: usize) -> Self {
        HIR::new(ret_reg, Type::Float, HIRKind::FSub(lhs, rhs))
    }

    fn new_imul(ret_reg: usize, lhs: usize, rhs: usize) -> Self {
        HIR::new(ret_reg, Type::Integer, HIRKind::IMul(lhs, rhs))
    }

    fn new_fmul(ret_reg: usize, lhs: usize, rhs: usize) -> Self {
        HIR::new(ret_reg, Type::Float, HIRKind::FMul(lhs, rhs))
    }

    fn new_idiv(ret_reg: usize, lhs: usize, rhs: usize) -> Self {
        HIR::new(ret_reg, Type::Integer, HIRKind::IDiv(lhs, rhs))
    }

    fn new_fdiv(ret_reg: usize, lhs: usize, rhs: usize) -> Self {
        HIR::new(ret_reg, Type::Float, HIRKind::FDiv(lhs, rhs))
    }

    fn new_ret(lhs: usize, ty: Type) -> Self {
        HIR::new(lhs, ty, HIRKind::Ret(lhs))
    }
}

impl HIRContext {
    pub fn from_ast(&mut self, ast: &Expr) -> (usize, Type) {
        let ret = self.gen(ast);
        let ty = self.reg_types[ret];
        self.hirs.push(HIR::new_ret(ret, ty));
        (ret, ty)
    }

    fn gen(&mut self, ast: &Expr) -> usize {
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
                match self.hirs[lhs_i].ty {
                    Type::Integer => self.new_ineg(lhs_i),
                    Type::Float => self.new_fneg(lhs_i),
                }
            }
            Expr::Add(box lhs, box rhs) => {
                let lhs = self.gen(lhs);
                let rhs = self.gen(rhs);
                let lhs_ty = self.hirs[lhs].ty;
                let rhs_ty = self.hirs[rhs].ty;
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
                let lhs_ty = self.hirs[lhs].ty;
                let rhs_ty = self.hirs[rhs].ty;
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
                let lhs_ty = self.hirs[lhs].ty;
                let rhs_ty = self.hirs[rhs].ty;
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
                let lhs_ty = self.hirs[lhs].ty;
                let rhs_ty = self.hirs[rhs].ty;
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
pub enum HIRKind {
    Integer(i32),
    IntAsFloat(usize),
    Float(f64),
    INeg(usize),
    FNeg(usize),
    IAdd(usize, usize),
    FAdd(usize, usize),
    ISub(usize, usize),
    FSub(usize, usize),
    IMul(usize, usize),
    FMul(usize, usize),
    IDiv(usize, usize),
    FDiv(usize, usize),
    Ret(usize),
}

impl std::fmt::Debug for HIRKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Self::Integer(i) => format!("{}: i32", i),
            Self::Float(f) => format!("{}: f64", f),
            Self::IntAsFloat(lhs) => format!("i32_to_f64 %{}", lhs),
            Self::INeg(lhs) => format!("ineg %{}", lhs),
            Self::FNeg(lhs) => format!("fneg %{}", lhs),
            Self::IAdd(lhs, rhs) => format!("iadd %{}, %{}", lhs, rhs),
            Self::FAdd(lhs, rhs) => format!("fadd %{}, %{}", lhs, rhs),
            Self::ISub(lhs, rhs) => format!("isub %{}, %{}", lhs, rhs),
            Self::FSub(lhs, rhs) => format!("fsub %{}, %{}", lhs, rhs),
            Self::IMul(lhs, rhs) => format!("imul %{}, %{}", lhs, rhs),
            Self::FMul(lhs, rhs) => format!("fmul %{}, %{}", lhs, rhs),
            Self::IDiv(lhs, rhs) => format!("idiv %{}, %{}", lhs, rhs),
            Self::FDiv(lhs, rhs) => format!("fdiv %{}, %{}", lhs, rhs),
            Self::Ret(lhs) => format!("ret %{}", lhs),
        };
        write!(f, "{}", s)
    }
}
