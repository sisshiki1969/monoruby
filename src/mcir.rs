use super::*;

#[derive(Clone, PartialEq)]
pub struct McIrContext {
    //pub insts: Vec<McIR>,
    g_reginfo: Vec<GRegInfo>,
    f_reginfo: Vec<FRegInfo>,
    ssa_map: SsaMap,
    cur_block: usize,
    pub blocks: Vec<McIrBlock>,
}

impl std::ops::Deref for McIrContext {
    type Target = McIrBlock;

    fn deref(&self) -> &McIrBlock {
        &self.blocks[self.cur_block]
    }
}

impl std::ops::DerefMut for McIrContext {
    fn deref_mut(&mut self) -> &mut McIrBlock {
        &mut self.blocks[self.cur_block]
    }
}

impl std::ops::Index<usize> for McIrContext {
    type Output = McIrBlock;

    fn index(&self, i: usize) -> &McIrBlock {
        &self.blocks[i]
    }
}

impl std::ops::IndexMut<usize> for McIrContext {
    fn index_mut(&mut self, i: usize) -> &mut McIrBlock {
        &mut self.blocks[i]
    }
}

impl McIrContext {
    fn invalidate(&mut self, reg: McReg) {
        match reg {
            McReg::FReg(f) => self[f].invalidate(),
            McReg::GReg(g) => self[g].invalidate(),
        }
    }

    fn alloc_reg(&mut self, ssareg: SsaReg, ty: Type) -> McReg {
        match ty {
            Type::Integer => McReg::GReg(self.alloc_greg(ssareg)),
            Type::Float => McReg::FReg(self.alloc_freg(ssareg)),
        }
    }
}

#[derive(Clone, PartialEq)]
pub struct McIrBlock {
    pub insts: Vec<McIR>,
}

impl McIrBlock {
    fn new() -> Self {
        Self { insts: vec![] }
    }
}

#[derive(Clone, PartialEq)]
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

impl std::fmt::Debug for SsaMap {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut v = vec![];
        for (i, info) in self.0.iter().enumerate() {
            match info {
                Some(reg) => v.push(format!("{}:{:?}", i, reg)),
                None => v.push(format!("{}:None", i)),
            }
        }
        write!(f, "SSA_MAP: [{}]", v.join(", "))
    }
}

impl std::ops::Index<GReg> for McIrContext {
    type Output = GRegInfo;

    fn index(&self, i: GReg) -> &GRegInfo {
        &self.g_reginfo[i.to_usize()]
    }
}

impl std::ops::IndexMut<GReg> for McIrContext {
    fn index_mut(&mut self, i: GReg) -> &mut GRegInfo {
        &mut self.g_reginfo[i.to_usize()]
    }
}

impl std::ops::Index<FReg> for McIrContext {
    type Output = FRegInfo;

    fn index(&self, i: FReg) -> &FRegInfo {
        &self.f_reginfo[i.to_usize()]
    }
}

impl std::ops::IndexMut<FReg> for McIrContext {
    fn index_mut(&mut self, i: FReg) -> &mut FRegInfo {
        &mut self.f_reginfo[i.to_usize()]
    }
}

impl std::fmt::Debug for McIrContext {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "McIRContext {{")?;
        for (i, block) in self.blocks.iter().enumerate() {
            writeln!(f, "\tBlock {} {{", i)?;
            for hir in &block.insts {
                let s = match hir {
                    McIR::Jmp(dest) => format!("jmp {}", dest),
                    McIR::CondJmp(cond, dest) => format!("cond_jmp {:?} {}", cond, dest),
                    McIR::In(reg) => format!("{:?} = %%", reg),
                    McIR::Out(reg) => format!("%% = {:?}", reg),
                    McIR::Integer(ret, i) => format!("%{:?} = {}: i32", ret, i),
                    McIR::Float(ret, f) => format!("%{:?} = {}: f64", ret, f),
                    McIR::IntAsFloat(ret, src) => {
                        format!("%{:?} = i32_to_f64 {:?}", ret, src)
                    }
                    McIR::INeg(reg) => format!("%{:?} = ineg %{:?}", reg, reg),
                    McIR::FNeg(reg) => format!("%{:?} = fneg %{:?}", reg, reg),
                    McIR::IAdd(dst, src) => format!("%{:?} = iadd %{:?}, {:?}", dst, dst, src),
                    McIR::ISub(dst, src) => format!("%{:?} = isub %{:?}, {:?}", dst, dst, src),
                    McIR::IMul(dst, src) => format!("%{:?} = imul %{:?}, %{:?}", dst, dst, src),
                    McIR::IDiv(dst, src) => format!("%{:?} = idiv %{:?}, %{:?}", dst, dst, src),
                    McIR::FAdd(dst, src) => format!("%{:?} = fadd %{:?}, {:?}", dst, dst, src),
                    McIR::FSub(dst, src) => format!("%{:?} = fsub %{:?}, {:?}", dst, dst, src),
                    McIR::FMul(dst, src) => format!("%{:?} = fmul %{:?}, {:?}", dst, dst, src),
                    McIR::FDiv(dst, src) => format!("%{:?} = fdiv %{:?}, {:?}", dst, dst, src),
                    McIR::IRet(ret) => format!("ret {:?}: i32", ret),
                    McIR::FRet(ret) => format!("ret {:?}: f64", ret),
                    McIR::LocalStore(ofs, reg) => format!("store ${}, {:?}", ofs, reg),
                    McIR::LocalLoad(ofs, reg) => format!("load ${}, {:?}", ofs, reg),
                };
                writeln!(f, "\t\t{}", s)?;
            }
            writeln!(f, "\t}}")?;
        }
        /*write!(f, "\tG_REG_INFO ")?;
        for (i, info) in self.g_reginfo.iter().enumerate() {
            match info {
                GRegInfo { ssareg } => match ssareg {
                    Some(reg) => write!(f, "{}:[{:?}] ", i, reg)?,
                    None => write!(f, "{}:[vacant] ", i)?,
                },
            }
        }
        writeln!(f)?;
        write!(f, "\tF_REG_INFO ")?;
        for (i, info) in self.f_reginfo.iter().enumerate() {
            match info {
                FRegInfo { ssareg } => match ssareg {
                    Some(reg) => write!(f, "{}:[{:?}] ", i, reg)?,
                    None => write!(f, "{}:[vacant] ", i)?,
                },
            }
        }
        writeln!(f)?;*/
        //writeln!(f, "\t{:?}", self.ssa_map)?;
        write!(f, "}}")
    }
}

#[derive(Clone, Copy, PartialEq)]
pub enum McReg {
    GReg(GReg),
    FReg(FReg),
}

impl std::fmt::Debug for McReg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::GReg(r) => write!(f, "%G{}", r.0),
            Self::FReg(r) => write!(f, "%F{}", r.0),
        }
    }
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

macro_rules! float_ops {
    ($self:ident, $op:ident, $v:ident) => {
        match (&$op.lhs, &$op.rhs) {
            (HirOperand::Reg(lhs), HirOperand::Reg(rhs)) => {
                let lhs = $self.ssa_map[*lhs].unwrap().as_f();
                let rhs = $self.ssa_map[*rhs].unwrap().as_f();
                $self.ssa_map[$op.ret] = Some(McReg::FReg(lhs));
                $self[rhs].invalidate();
                $self.insts.push(McIR::$v(lhs, McFloatOperand::Reg(rhs)));
            }
            (HirOperand::Reg(lhs), HirOperand::Const(rhs)) => {
                let lhs = $self.ssa_map[*lhs].unwrap().as_f();
                $self.ssa_map[$op.ret] = Some(McReg::FReg(lhs));
                $self
                    .insts
                    .push(McIR::$v(lhs, McFloatOperand::Float(rhs.as_f())));
            }
            (HirOperand::Const(lhs), HirOperand::Reg(rhs)) => {
                let n = lhs.as_f();
                let lhs = $self.alloc_freg($op.ret);
                $self.insts.push(McIR::Float(lhs, n));
                let rhs = $self.ssa_map[*rhs].unwrap().as_f();
                $self[rhs].invalidate();
                $self.insts.push(McIR::$v(lhs, McFloatOperand::Reg(rhs)));
            }
            (HirOperand::Const(lhs), HirOperand::Const(rhs)) => {
                let n = lhs.as_f();
                let lhs = $self.alloc_freg($op.ret);
                $self.insts.push(McIR::Float(lhs, n));
                $self
                    .insts
                    .push(McIR::$v(lhs, McFloatOperand::Float(rhs.as_f())));
            }
        }
    };
}

impl McIrContext {
    fn new(ssa_map: SsaMap) -> Self {
        Self {
            g_reginfo: vec![],
            f_reginfo: vec![],
            ssa_map,
            cur_block: 0,
            blocks: vec![],
        }
    }

    pub fn g_reg_num(&self) -> usize {
        self.g_reginfo.len()
    }

    pub fn f_reg_num(&self) -> usize {
        self.f_reginfo.len()
    }

    pub fn from_hir(hir_context: &mut HIRContext) -> Self {
        let mut ctx = Self::new(SsaMap(vec![None; hir_context.register_num()]));
        for bb in &hir_context.basic_block {
            ctx.blocks.push(McIrBlock::new());
            ctx.g_reginfo = vec![];
            ctx.f_reginfo = vec![];
            for hir in &bb.insts {
                match hir {
                    Hir::Integer(ssa, i) => {
                        let reg = ctx.alloc_greg(*ssa);
                        ctx.insts.push(McIR::Integer(reg, *i));
                    }
                    Hir::Float(ssa, f) => {
                        let reg = ctx.alloc_freg(*ssa);
                        ctx.insts.push(McIR::Float(reg, *f));
                    }
                    Hir::IntAsFloat(op) => {
                        let dst = ctx.alloc_freg(op.ret);
                        let src = match &op.src {
                            HirOperand::Const(c) => McGeneralOperand::Integer(c.as_i()),
                            HirOperand::Reg(r) => {
                                let src = ctx.ssa_map[*r].unwrap().as_g();
                                ctx[src].invalidate();
                                McGeneralOperand::Reg(src)
                            }
                        };
                        ctx.insts.push(McIR::IntAsFloat(dst, src));
                    }
                    Hir::IAdd(op) => match (&op.lhs, &op.rhs) {
                        (HirOperand::Reg(lhs), HirOperand::Reg(rhs)) => {
                            let lhs = ctx.ssa_map[*lhs].unwrap().as_g();
                            let rhs = ctx.ssa_map[*rhs].unwrap().as_g();
                            ctx.ssa_map[op.ret] = Some(McReg::GReg(lhs));
                            ctx[rhs].invalidate();
                            ctx.insts.push(McIR::IAdd(lhs, McGeneralOperand::Reg(rhs)));
                        }
                        (HirOperand::Reg(lhs), HirOperand::Const(rhs)) => {
                            let lhs = ctx.ssa_map[*lhs].unwrap().as_g();
                            ctx.ssa_map[op.ret] = Some(McReg::GReg(lhs));
                            ctx.insts
                                .push(McIR::IAdd(lhs, McGeneralOperand::Integer(rhs.as_i())));
                        }
                        (HirOperand::Const(lhs), HirOperand::Reg(rhs)) => {
                            let n = lhs.as_i();
                            let lhs = ctx.alloc_greg(op.ret);
                            ctx.insts.push(McIR::Integer(lhs, n));
                            let rhs = ctx.ssa_map[*rhs].unwrap().as_g();
                            ctx[rhs].invalidate();
                            ctx.insts.push(McIR::IAdd(lhs, McGeneralOperand::Reg(rhs)));
                        }
                        (HirOperand::Const(lhs), HirOperand::Const(rhs)) => {
                            let n = lhs.as_i();
                            let lhs = ctx.alloc_greg(op.ret);
                            ctx.insts.push(McIR::Integer(lhs, n));
                            ctx.insts
                                .push(McIR::IAdd(lhs, McGeneralOperand::Integer(rhs.as_i())));
                        }
                    },
                    Hir::ISub(op) => match (&op.lhs, &op.rhs) {
                        (HirOperand::Reg(lhs), HirOperand::Reg(rhs)) => {
                            let lhs = ctx.ssa_map[*lhs].unwrap().as_g();
                            let rhs = ctx.ssa_map[*rhs].unwrap().as_g();
                            ctx.ssa_map[op.ret] = Some(McReg::GReg(lhs));
                            ctx[rhs].invalidate();
                            ctx.insts.push(McIR::ISub(lhs, McGeneralOperand::Reg(rhs)));
                        }
                        (HirOperand::Reg(lhs), HirOperand::Const(rhs)) => {
                            let lhs = ctx.ssa_map[*lhs].unwrap().as_g();
                            ctx.ssa_map[op.ret] = Some(McReg::GReg(lhs));
                            ctx.insts
                                .push(McIR::ISub(lhs, McGeneralOperand::Integer(rhs.as_i())));
                        }
                        (HirOperand::Const(lhs), HirOperand::Reg(rhs)) => {
                            let n = lhs.as_i();
                            let lhs = ctx.alloc_greg(op.ret);
                            ctx.insts.push(McIR::Integer(lhs, n));
                            let rhs = ctx.ssa_map[*rhs].unwrap().as_g();
                            ctx[rhs].invalidate();
                            ctx.insts.push(McIR::ISub(lhs, McGeneralOperand::Reg(rhs)));
                        }
                        (HirOperand::Const(lhs), HirOperand::Const(rhs)) => {
                            let n = lhs.as_i();
                            let lhs = ctx.alloc_greg(op.ret);
                            ctx.insts.push(McIR::Integer(lhs, n));
                            ctx.insts
                                .push(McIR::ISub(lhs, McGeneralOperand::Integer(rhs.as_i())));
                        }
                    },
                    Hir::IMul(op) => {
                        let lhs = ctx.ssa_map[op.lhs].unwrap().as_g();
                        let rhs = ctx.ssa_map[op.rhs].unwrap().as_g();
                        ctx.ssa_map[op.ret] = Some(McReg::GReg(lhs));
                        ctx[rhs].invalidate();
                        ctx.insts.push(McIR::IMul(lhs, rhs));
                    }
                    Hir::IDiv(op) => {
                        let lhs = ctx.ssa_map[op.lhs].unwrap().as_g();
                        let rhs = ctx.ssa_map[op.rhs].unwrap().as_g();
                        ctx.ssa_map[op.ret] = Some(McReg::GReg(lhs));
                        ctx[rhs].invalidate();
                        ctx.insts.push(McIR::IDiv(lhs, rhs));
                    }
                    Hir::FAdd(op) => float_ops!(ctx, op, FAdd),
                    Hir::FSub(op) => float_ops!(ctx, op, FSub),
                    Hir::FMul(op) => float_ops!(ctx, op, FMul),
                    Hir::FDiv(op) => float_ops!(ctx, op, FDiv),
                    Hir::Ret(op) => match op {
                        HirOperand::Reg(ssa) => match hir_context[*ssa].ty {
                            Type::Integer => {
                                let reg = ctx.ssa_map[*ssa].unwrap().as_g();
                                ctx[reg].invalidate();
                                ctx.insts.push(McIR::IRet(McGeneralOperand::Reg(reg)));
                            }
                            Type::Float => {
                                let reg = ctx.ssa_map[*ssa].unwrap().as_f();
                                ctx[reg].invalidate();
                                ctx.insts.push(McIR::FRet(McFloatOperand::Reg(reg)));
                            }
                        },
                        HirOperand::Const(c) => match c {
                            Value::Integer(i) => {
                                ctx.insts.push(McIR::IRet(McGeneralOperand::Integer(*i)))
                            }
                            Value::Float(f) => {
                                ctx.insts.push(McIR::FRet(McFloatOperand::Float(*f)))
                            }
                        },
                    },
                    Hir::INeg(op) => match &op.src {
                        HirOperand::Const(c) => {
                            let n = c.as_i();
                            let reg = ctx.alloc_greg(op.ret);
                            ctx.insts.push(McIR::Integer(reg, -n));
                        }
                        HirOperand::Reg(src) => {
                            let reg = ctx.ssa_map[*src].unwrap().as_g();
                            ctx.ssa_map[op.ret] = Some(McReg::GReg(reg));
                            ctx.insts.push(McIR::INeg(reg));
                        }
                    },
                    Hir::FNeg(op) => match &op.src {
                        HirOperand::Const(c) => {
                            let n = c.as_f();
                            let reg = ctx.alloc_freg(op.ret);
                            ctx.insts.push(McIR::Float(reg, -n));
                        }
                        HirOperand::Reg(src) => {
                            let reg = ctx.ssa_map[*src].unwrap().as_f();
                            ctx.ssa_map[op.ret] = Some(McReg::FReg(reg));
                            ctx.insts.push(McIR::FNeg(reg));
                        }
                    },
                    Hir::LocalStore(ret, info, reg) => {
                        let ty = info.1;
                        assert_eq!(ty, hir_context[*reg].ty);
                        let reg = ctx.ssa_map[*reg].unwrap();
                        if let Some(ret) = ret {
                            ctx.ssa_map[*ret] = Some(reg);
                        } else {
                            ctx.invalidate(reg);
                        }
                        ctx.insts.push(McIR::LocalStore(info.0, reg));
                    }
                    Hir::LocalLoad(info, reg) => {
                        let ty = info.1;
                        assert_eq!(ty, hir_context[*reg].ty);
                        let reg = ctx.alloc_reg(*reg, ty);
                        ctx.insts.push(McIR::LocalLoad(info.0, reg));
                    }
                    Hir::Br(next_bb) => {
                        let move_list = hir_context[*next_bb]
                            .insts
                            .iter()
                            .filter_map(|ir| match ir {
                                Hir::Phi(_, phi) => phi.iter().find_map(|(i, r)| {
                                    if ctx.cur_block == *i {
                                        Some(r)
                                    } else {
                                        None
                                    }
                                }),
                                _ => None,
                            })
                            .collect::<Vec<_>>();
                        if move_list.len() == 0 {
                            ctx.insts.push(McIR::Jmp(*next_bb));
                        } else {
                            assert_eq!(1, move_list.len());
                            let src = move_list[0];
                            let src_reg = ctx.ssa_map[*src].unwrap();
                            ctx.insts.push(McIR::Out(src_reg));
                            ctx.insts.push(McIR::Jmp(*next_bb));
                            ctx.invalidate(src_reg);
                        }
                    }
                    Hir::CondBr(cond_, then_bb, else_bb) => {
                        let cond_ = ctx.ssa_map[*cond_].unwrap();
                        ctx.insts.push(McIR::CondJmp(cond_, *else_bb));
                        ctx.insts.push(McIR::Jmp(*then_bb));
                    }
                    Hir::Phi(ret, _) => {
                        let reg = ctx.alloc_reg(*ret, hir_context[*ret].ty);
                        ctx.insts.push(McIR::In(reg));
                    }
                }
            }
            ctx.cur_block += 1;
        }
        ctx
    }

    /// Get a vacant general register and update a SSA map.
    fn alloc_greg(&mut self, ssareg: SsaReg) -> GReg {
        fn new_greg(ctx: &mut McIrContext, ssareg: SsaReg) -> GReg {
            for (i, r) in ctx.g_reginfo.iter_mut().enumerate() {
                if r.ssareg.is_none() {
                    r.apply(ssareg);
                    return GReg(i);
                }
            }
            let new = GReg(ctx.g_reginfo.len());
            ctx.g_reginfo.push(GRegInfo::new(ssareg));
            new
        }
        if let Some(reg) = self.ssa_map[ssareg] {
            return reg.as_g();
        }
        let reg = new_greg(self, ssareg);
        self.ssa_map[ssareg] = Some(McReg::GReg(reg));
        reg
    }

    /// Get a vacant floating point register.
    fn alloc_freg(&mut self, ssareg: SsaReg) -> FReg {
        fn new_freg(ctx: &mut McIrContext, ssareg: SsaReg) -> FReg {
            for (i, r) in ctx.f_reginfo.iter_mut().enumerate() {
                if r.ssareg.is_none() {
                    r.apply(ssareg);
                    return FReg(i);
                }
            }
            let new = ctx.f_reginfo.len();
            ctx.f_reginfo.push(FRegInfo::new(ssareg));
            FReg(new)
        }
        if let Some(reg) = self.ssa_map[ssareg] {
            return reg.as_f();
        }
        let reg = new_freg(self, ssareg);
        self.ssa_map[ssareg] = Some(McReg::FReg(reg));
        reg
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum McIR {
    Jmp(usize),
    CondJmp(McReg, usize),
    Out(McReg),
    In(McReg),
    Integer(GReg, i32),
    Float(FReg, f64),
    IntAsFloat(FReg, McGeneralOperand),
    INeg(GReg),
    FNeg(FReg),
    IAdd(GReg, McGeneralOperand),
    ISub(GReg, McGeneralOperand),
    IMul(GReg, GReg),
    IDiv(GReg, GReg),
    FAdd(FReg, McFloatOperand),
    FSub(FReg, McFloatOperand),
    FMul(FReg, McFloatOperand),
    FDiv(FReg, McFloatOperand),
    IRet(McGeneralOperand),
    FRet(McFloatOperand),
    LocalStore(usize, McReg),
    LocalLoad(usize, McReg),
}

#[derive(Clone, PartialEq)]
pub enum McGeneralOperand {
    Reg(GReg),
    Integer(i32),
}

impl std::fmt::Debug for McGeneralOperand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Reg(r) => write!(f, "%G{}", r.to_usize()),
            Self::Integer(c) => write!(f, "{:?}: i32", c),
        }
    }
}

#[derive(Clone, PartialEq)]
pub enum McFloatOperand {
    Reg(FReg),
    Float(f64),
}

impl std::fmt::Debug for McFloatOperand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Reg(r) => write!(f, "%F{}", r.to_usize()),
            Self::Float(c) => write!(f, "{:?}: f64", c),
        }
    }
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
