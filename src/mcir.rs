use super::*;

#[derive(Clone, PartialEq)]
pub struct McIRContext {
    pub mcirs: Vec<McIR>,
    g_reginfo: Vec<GRegInfo>,
    f_reginfo: Vec<FRegInfo>,
    ssa_map: SsaMap,
}

#[derive(Clone, Debug, PartialEq)]
struct SsaMap(Vec<Option<McReg>>);

impl std::ops::Index<SsaReg> for SsaMap {
    type Output = Option<McReg>;

    fn index(&self, i: SsaReg) -> &Option<McReg> {
        &self.0[i.to_usize()]
    }
}

impl std::ops::IndexMut<SsaReg> for SsaMap {
    fn index_mut(&mut self, i: SsaReg) -> &mut Option<McReg> {
        &mut self.0[i.to_usize()]
    }
}

impl std::ops::Index<GReg> for McIRContext {
    type Output = GRegInfo;

    fn index(&self, i: GReg) -> &GRegInfo {
        &self.g_reginfo[i.to_usize()]
    }
}

impl std::ops::IndexMut<GReg> for McIRContext {
    fn index_mut(&mut self, i: GReg) -> &mut GRegInfo {
        &mut self.g_reginfo[i.to_usize()]
    }
}

impl std::ops::Index<FReg> for McIRContext {
    type Output = FRegInfo;

    fn index(&self, i: FReg) -> &FRegInfo {
        &self.f_reginfo[i.to_usize()]
    }
}

impl std::ops::IndexMut<FReg> for McIRContext {
    fn index_mut(&mut self, i: FReg) -> &mut FRegInfo {
        &mut self.f_reginfo[i.to_usize()]
    }
}

impl std::fmt::Debug for McIRContext {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "McIRContext {{")?;
        for hir in &self.mcirs {
            let s = match hir {
                McIR::Integer(ret, i) => format!("%{:?} = {}: i32", ret, i),
                McIR::Float(ret, f) => format!("%{:?} = {}: f64", ret, f),
                McIR::IntAsFloat(ret, src) => {
                    format!("%{:?} = i32_to_f64 %{:?}", ret, src)
                }
                McIR::INeg(reg) => format!("%{:?} = ineg %{:?}", reg, reg),
                McIR::FNeg(reg) => format!("%{:?} = fneg %{:?}", reg, reg),
                McIR::IAdd(dst, src) => format!("%{:?} = iadd %{:?}, %{:?}", dst, dst, src),
                McIR::FAdd(dst, src) => format!("%{:?} = fadd %{:?}, %{:?}", dst, dst, src),
                McIR::ISub(dst, src) => format!("%{:?} = isub %{:?}, %{:?}", dst, dst, src),
                McIR::FSub(dst, src) => format!("%{:?} = fsub %{:?}, %{:?}", dst, dst, src),
                McIR::IMul(dst, src) => format!("%{:?} = imul %{:?}, %{:?}", dst, dst, src),
                McIR::FMul(dst, src) => format!("%{:?} = fmul %{:?}, %{:?}", dst, dst, src),
                McIR::IDiv(dst, src) => format!("%{:?} = idiv %{:?}, %{:?}", dst, dst, src),
                McIR::FDiv(dst, src) => format!("%{:?} = fdiv %{:?}, %{:?}", dst, dst, src),
                McIR::IRet(ret) => format!("ret %{:?}: {:?}", ret, "i32"),
                McIR::FRet(ret) => format!("ret %{:?}: {:?}", ret, "f32"),
            };
            writeln!(f, "\t{}", s)?;
        }
        writeln!(f, "}}")?;
        Ok(())
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum McReg {
    GReg(GReg),
    FReg(FReg),
}

impl McReg {
    fn as_g(self) -> GReg {
        match self {
            McReg::GReg(r) => r,
            _ => unreachable!(),
        }
    }

    fn as_f(self) -> FReg {
        match self {
            McReg::FReg(r) => r,
            _ => unreachable!(),
        }
    }
}

impl McIRContext {
    pub fn new() -> Self {
        Self {
            mcirs: vec![],
            g_reginfo: vec![],
            f_reginfo: vec![],
            ssa_map: SsaMap(vec![]),
        }
    }

    pub fn g_reg_num(&self) -> usize {
        self.g_reginfo.len()
    }

    pub fn f_reg_num(&self) -> usize {
        self.f_reginfo.len()
    }

    pub fn from_hir(&mut self, hir_context: &HIRContext) {
        self.ssa_map = SsaMap(vec![None; hir_context.register_num()]);
        for hir in &hir_context.hirs {
            match hir {
                HIR::Integer(ssa, i) => {
                    let reg = self.new_greg(*ssa);
                    self.ssa_map[*ssa] = Some(McReg::GReg(reg));
                    self.mcirs.push(McIR::Integer(reg, *i));
                }
                HIR::Float(ssa, f) => {
                    let reg = self.get_freg(*ssa);
                    self.ssa_map[*ssa] = Some(McReg::FReg(reg));
                    self.mcirs.push(McIR::Float(reg, *f));
                }
                HIR::IntAsFloat(op) => {
                    let dst = self.get_freg(op.ret);
                    self.ssa_map[op.ret] = Some(McReg::FReg(dst));
                    let src = self.ssa_map[op.src].unwrap().as_g();
                    self[src].invalidate();
                    self.mcirs.push(McIR::IntAsFloat(dst, src));
                }
                HIR::IAdd(op) => {
                    let lhs = self.ssa_map[op.lhs].unwrap().as_g();
                    let rhs = self.ssa_map[op.rhs].unwrap().as_g();
                    self.ssa_map[op.ret] = Some(McReg::GReg(lhs));
                    self[rhs].invalidate();
                    self.mcirs.push(McIR::IAdd(lhs, rhs));
                }
                HIR::ISub(op) => {
                    let lhs = self.ssa_map[op.lhs].unwrap().as_g();
                    let rhs = self.ssa_map[op.rhs].unwrap().as_g();
                    self.ssa_map[op.ret] = Some(McReg::GReg(lhs));
                    self[rhs].invalidate();
                    self.mcirs.push(McIR::ISub(lhs, rhs));
                }
                HIR::IMul(op) => {
                    let lhs = self.ssa_map[op.lhs].unwrap().as_g();
                    let rhs = self.ssa_map[op.rhs].unwrap().as_g();
                    self.ssa_map[op.ret] = Some(McReg::GReg(lhs));
                    self[rhs].invalidate();
                    self.mcirs.push(McIR::IMul(lhs, rhs));
                }
                HIR::IDiv(op) => {
                    let lhs = self.ssa_map[op.lhs].unwrap().as_g();
                    let rhs = self.ssa_map[op.rhs].unwrap().as_g();
                    self.ssa_map[op.ret] = Some(McReg::GReg(lhs));
                    self[rhs].invalidate();
                    self.mcirs.push(McIR::IDiv(lhs, rhs));
                }
                HIR::FAdd(op) => {
                    let lhs = self.ssa_map[op.lhs].unwrap().as_f();
                    let rhs = self.ssa_map[op.rhs].unwrap().as_f();
                    self.ssa_map[op.ret] = Some(McReg::FReg(lhs));
                    self[rhs].invalidate();
                    self.mcirs.push(McIR::FAdd(lhs, rhs));
                }
                HIR::FSub(op) => {
                    let lhs = self.ssa_map[op.lhs].unwrap().as_f();
                    let rhs = self.ssa_map[op.rhs].unwrap().as_f();
                    self.ssa_map[op.ret] = Some(McReg::FReg(lhs));
                    self[rhs].invalidate();
                    self.mcirs.push(McIR::FSub(lhs, rhs));
                }
                HIR::FMul(op) => {
                    let lhs = self.ssa_map[op.lhs].unwrap().as_f();
                    let rhs = self.ssa_map[op.rhs].unwrap().as_f();
                    self.ssa_map[op.ret] = Some(McReg::FReg(lhs));
                    self[rhs].invalidate();
                    self.mcirs.push(McIR::FMul(lhs, rhs));
                }
                HIR::FDiv(op) => {
                    let lhs = self.ssa_map[op.lhs].unwrap().as_f();
                    let rhs = self.ssa_map[op.rhs].unwrap().as_f();
                    self.ssa_map[op.ret] = Some(McReg::FReg(lhs));
                    self[rhs].invalidate();
                    self.mcirs.push(McIR::FDiv(lhs, rhs));
                }
                HIR::Ret(ssa) => match hir_context[*ssa].ty {
                    Type::Integer => {
                        let reg = self.ssa_map[*ssa].unwrap().as_g();
                        self.mcirs.push(McIR::IRet(reg));
                    }
                    Type::Float => {
                        let reg = self.ssa_map[*ssa].unwrap().as_f();
                        self.mcirs.push(McIR::FRet(reg));
                    }
                },
                _ => {}
            }
        }
    }

    fn new_greg(&mut self, ssareg: SsaReg) -> GReg {
        for (i, r) in self.g_reginfo.iter_mut().enumerate() {
            if r.ssareg.is_none() {
                r.apply(ssareg);
                return GReg(i);
            }
        }
        let new = GReg(self.g_reginfo.len());
        self.g_reginfo.push(GRegInfo::new(ssareg));
        new
    }

    fn get_freg(&mut self, ssareg: SsaReg) -> FReg {
        for (i, r) in self.f_reginfo.iter_mut().enumerate() {
            if r.ssareg.is_none() {
                r.apply(ssareg);
                return FReg(i);
            }
        }
        let new = self.f_reginfo.len();
        self.f_reginfo.push(FRegInfo::new(ssareg));
        FReg(new)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum McIR {
    Integer(GReg, i32),
    Float(FReg, f64),
    IntAsFloat(FReg, GReg),
    INeg(GReg),
    FNeg(FReg),
    IAdd(GReg, GReg),
    FAdd(FReg, FReg),
    ISub(GReg, GReg),
    FSub(FReg, FReg),
    IMul(GReg, GReg),
    FMul(FReg, FReg),
    IDiv(GReg, GReg),
    FDiv(FReg, FReg),
    IRet(GReg),
    FRet(FReg),
}

#[derive(Clone, Debug, PartialEq)]
pub struct GRegInfo {
    ssareg: Option<SsaReg>,
}

impl GRegInfo {
    fn new(ssareg: SsaReg) -> Self {
        let ssareg = Some(ssareg);
        Self { ssareg }
    }

    fn apply(&mut self, ssa: SsaReg) {
        self.ssareg = Some(ssa);
    }

    fn invalidate(&mut self) {
        self.ssareg = None;
    }
}

#[derive(Clone, Copy, PartialEq)]
pub struct GReg(usize);

impl std::fmt::Debug for GReg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "G{}", self.0)
    }
}

impl GReg {
    pub fn to_usize(self) -> usize {
        self.0
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct FRegInfo {
    ssareg: Option<SsaReg>,
}

impl FRegInfo {
    fn new(ssareg: SsaReg) -> Self {
        let ssareg = Some(ssareg);
        Self { ssareg }
    }

    fn apply(&mut self, ssa: SsaReg) {
        self.ssareg = Some(ssa);
    }

    fn invalidate(&mut self) {
        self.ssareg = None;
    }
}

#[derive(Clone, Copy, PartialEq)]
pub struct FReg(usize);

impl std::fmt::Debug for FReg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "F{}", self.0)
    }
}

impl FReg {
    pub fn to_usize(self) -> usize {
        self.0
    }
}
