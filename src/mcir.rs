use std::collections::BTreeSet;

use super::*;

#[derive(Clone, PartialEq)]
pub struct McIrContext {
    //pub insts: Vec<McIR>,
    g_reginfo: Vec<GRegInfo>,
    f_reginfo: Vec<FRegInfo>,
    ssa_map: SsaMap,
    cur_block: usize,
    pub blocks: Vec<McIrBlock>,
    pub functions: Vec<McIrFunc>,
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
        for func in &self.functions {
            writeln!(
                f,
                "\tFunc {} g_reg:{} f_reg:{} local:{:?} {{",
                func.name, func.g_regs, func.f_regs, func.locals
            )?;
            for bbi in &func.bbs {
                let block = &self.blocks[*bbi];
                writeln!(f, "\t\tBlock {} {{", *bbi)?;
                for hir in &block.insts {
                    let s = match hir {
                        McIR::Jmp(dest) => format!("jmp {}", dest),
                        McIR::CondJmp(cond, dest) => format!("cond_jmp {:?} {}", cond, dest),
                        McIR::ICmpJmp(kind, lhs, rhs, dest) => {
                            format!("cmp_jmp ({:?} %{:?}, %{:?}) {}", kind, lhs, rhs, dest)
                        }
                        McIR::FCmpJmp(kind, lhs, rhs, dest) => {
                            format!("cmp_jmp ({:?} %{:?}, %{:?}) {}", kind, lhs, rhs, dest)
                        }
                        McIR::In(reg) => format!("{:?} = %%", reg),
                        McIR::Out(reg) => format!("%% = {:?}", reg),
                        McIR::Integer(ret, i) => format!("%{:?} = {}: i32", ret, i),
                        McIR::Float(ret, f) => format!("%{:?} = {}: f64", ret, f),
                        McIR::CastIntFloat(ret, src) => {
                            format!("%{:?} = cast {:?} i32 to f64", ret, src)
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
                        McIR::ICmp(kind, dst, src) => {
                            format!("%{:?} = icmp {:?} %{:?}, {:?}", dst, kind, dst, src)
                        }
                        McIR::FCmp(kind, ret, lhs, rhs) => {
                            format!("%{:?} = fcmp {:?} %{:?}, {:?}", ret, kind, lhs, rhs)
                        }
                        McIR::IRet(ret, ty) => format!("ret {:?}:{:?}", ret, ty),
                        McIR::FRet(ret) => format!("ret {:?}: f64", ret),
                        McIR::LocalStore(ofs, reg) => format!("store ${}, {:?}", ofs, reg),
                        McIR::LocalLoad(ofs, reg) => format!("load ${}, {:?}", ofs, reg),
                    };
                    writeln!(f, "\t\t\t{}", s)?;
                }
                writeln!(f, "\t\t}}")?;
            }
            writeln!(f, "\t}}")?;
        }
        write!(f, "}}")
    }
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
            McReg::FReg(f) => self[f].release(),
            McReg::GReg(g) => self[g].release(),
        }
    }

    fn alloc_reg(&mut self, ssareg: SsaReg, ty: Type) -> McReg {
        match ty {
            Type::Integer | Type::Bool => McReg::GReg(self.alloc_greg(ssareg)),
            Type::Float => McReg::FReg(self.alloc_freg(ssareg)),
        }
    }

    fn hir_to_general_operand(&mut self, rhs: &HirOperand) -> McGeneralOperand {
        match rhs {
            HirOperand::Reg(rhs) => {
                let rhs = self.ssa_map[*rhs].unwrap().as_g();
                self[rhs].release();
                McGeneralOperand::Reg(rhs)
            }
            HirOperand::Const(rhs) => McGeneralOperand::Integer(rhs.as_i()),
        }
    }

    fn hir_to_greg(&mut self, op: &HirOperand, ret: SsaReg) -> GReg {
        match &op {
            HirOperand::Reg(lhs) => {
                let lhs = self.ssa_map[*lhs].unwrap();
                self.ssa_map[ret] = Some(lhs);
                lhs.as_g()
            }

            HirOperand::Const(lhs) => {
                let n = lhs.as_i();
                let lhs = self.alloc_greg(ret);
                self.insts.push(McIR::Integer(lhs, n));
                lhs
            }
        }
    }

    fn hir_to_float_operand(&mut self, rhs: &HirOperand) -> McFloatOperand {
        match rhs {
            HirOperand::Reg(rhs) => {
                let rhs = self.ssa_map[*rhs].unwrap().as_f();
                self[rhs].release();
                McFloatOperand::Reg(rhs)
            }
            HirOperand::Const(rhs) => McFloatOperand::Float(rhs.as_f()),
        }
    }

    fn hir_to_freg(&mut self, op: &HirOperand, ret: SsaReg) -> FReg {
        match &op {
            HirOperand::Reg(lhs) => {
                let lhs = self.ssa_map[*lhs].unwrap();
                self.ssa_map[ret] = Some(lhs);
                lhs.as_f()
            }

            HirOperand::Const(lhs) => {
                let n = lhs.as_f();
                let lhs = self.alloc_freg(ret);
                self.insts.push(McIR::Float(lhs, n));
                lhs
            }
        }
    }

    /// Get a vacant general register and update a SSA map.
    fn alloc_greg(&mut self, ssareg: SsaReg) -> GReg {
        fn new_greg(ctx: &mut McIrContext, ssareg: SsaReg) -> GReg {
            for (i, r) in ctx.g_reginfo.iter_mut().enumerate() {
                if r.ssareg.is_none() {
                    r.assign(ssareg);
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
                    r.assign(ssareg);
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

/// Function information of McIr.
#[derive(Clone, PartialEq)]
pub struct McIrFunc {
    /// Name of the function.
    pub name: String,
    /// Basic blocks which belong to this function.
    pub bbs: BTreeSet<usize>,
    /// Number of virtual general registers.
    pub g_regs: usize,
    /// Number of virtual float registers.
    pub f_regs: usize,
    /// Offsets and types of local variables.
    pub locals: HashMap<String, (usize, Type)>,
    /// Type of return value.
    pub ret_ty: Type,
}

impl McIrFunc {
    fn new(name: String, bbs: BTreeSet<usize>, locals: HashMap<String, (usize, Type)>) -> Self {
        Self {
            name,
            bbs,
            g_regs: 0,
            f_regs: 0,
            locals,
            ret_ty: Type::Integer,
        }
    }
}

#[derive(Clone, PartialEq)]
pub struct McIrBlock {
    pub insts: Vec<McIR>,
    pub owner_function: usize,
}

impl McIrBlock {
    fn new(owner_function: usize) -> Self {
        Self {
            insts: vec![],
            owner_function,
        }
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
    ($self:ident, $op:ident, $v:ident) => {{
        let lhs = $self.hir_to_freg(&$op.lhs, $op.ret);
        let rhs = $self.hir_to_float_operand(&$op.rhs);
        $self.insts.push(McIR::$v(lhs, rhs));
    }};
}

impl McIrContext {
    fn new() -> Self {
        Self {
            g_reginfo: vec![],
            f_reginfo: vec![],
            ssa_map: SsaMap(vec![]),
            cur_block: 0,
            blocks: vec![],
            functions: vec![],
        }
    }

    pub fn g_reg_num(&self) -> usize {
        self.g_reginfo.len()
    }

    pub fn f_reg_num(&self) -> usize {
        self.f_reginfo.len()
    }

    pub fn from_hir(hir_context: &mut HIRContext) -> Self {
        let mut ctx = Self::new();
        ctx.functions = hir_context
            .functions
            .iter()
            .map(|hir_func| {
                McIrFunc::new(
                    hir_func.name.clone(),
                    hir_func.bbs.clone(),
                    hir_func.locals.clone(),
                )
            })
            .collect();
        ctx.blocks = hir_context
            .basic_block
            .iter()
            .map(|hir_bb| McIrBlock::new(hir_bb.owner_function))
            .collect();
        for (i, func) in hir_context.functions.iter().enumerate() {
            ctx.ssa_map = SsaMap(vec![None; func.register_num()]);
            let mut g_reg_num = 0;
            let mut f_reg_num = 0;
            for bbi in &func.bbs {
                ctx.cur_block = *bbi;
                let bb = &hir_context.basic_block[*bbi];
                ctx.compile_bb(bb, hir_context);
                g_reg_num = std::cmp::max(g_reg_num, ctx.g_reg_num());
                f_reg_num = std::cmp::max(f_reg_num, ctx.f_reg_num());
            }
            ctx.functions[i].g_regs = g_reg_num;
            ctx.functions[i].f_regs = f_reg_num;
            ctx.functions[i].ret_ty = func.ret_ty.unwrap();
        }
        ctx
    }

    fn compile_bb(&mut self, bb: &HirBasicBlock, hir_context: &HIRContext) {
        self.g_reginfo = vec![];
        self.f_reginfo = vec![];
        let func = &hir_context.functions[bb.owner_function];
        for hir in &bb.insts {
            match hir {
                Hir::Integer(ssa, i) => {
                    let reg = self.alloc_greg(*ssa);
                    self.insts.push(McIR::Integer(reg, *i));
                }
                Hir::Float(ssa, f) => {
                    let reg = self.alloc_freg(*ssa);
                    self.insts.push(McIR::Float(reg, *f));
                }
                Hir::CastIntFloat(op) => {
                    let dst = self.alloc_freg(op.ret);
                    let src = match &op.src {
                        HirOperand::Const(c) => McGeneralOperand::Integer(c.as_i()),
                        HirOperand::Reg(r) => {
                            let src = self.ssa_map[*r].unwrap().as_g();
                            self[src].release();
                            McGeneralOperand::Reg(src)
                        }
                    };
                    self.insts.push(McIR::CastIntFloat(dst, src));
                }
                Hir::IAdd(op) => {
                    let lhs = self.hir_to_greg(&op.lhs, op.ret);
                    let rhs = self.hir_to_general_operand(&op.rhs);
                    self.insts.push(McIR::IAdd(lhs, rhs));
                }
                Hir::ISub(op) => {
                    let lhs = self.hir_to_greg(&op.lhs, op.ret);
                    let rhs = self.hir_to_general_operand(&op.rhs);
                    self.insts.push(McIR::ISub(lhs, rhs));
                }
                Hir::IMul(op) => {
                    let lhs = self.ssa_map[op.lhs].unwrap().as_g();
                    let rhs = self.ssa_map[op.rhs].unwrap().as_g();
                    self.ssa_map[op.ret] = Some(McReg::GReg(lhs));
                    self[rhs].release();
                    self.insts.push(McIR::IMul(lhs, rhs));
                }
                Hir::IDiv(op) => {
                    let lhs = self.ssa_map[op.lhs].unwrap().as_g();
                    let rhs = self.ssa_map[op.rhs].unwrap().as_g();
                    self.ssa_map[op.ret] = Some(McReg::GReg(lhs));
                    self[rhs].release();
                    self.insts.push(McIR::IDiv(lhs, rhs));
                }
                Hir::FAdd(op) => float_ops!(self, op, FAdd),
                Hir::FSub(op) => float_ops!(self, op, FSub),
                Hir::FMul(op) => float_ops!(self, op, FMul),
                Hir::FDiv(op) => float_ops!(self, op, FDiv),

                Hir::ICmp(kind, op) => {
                    let lhs = self.hir_to_greg(&op.lhs, op.ret);
                    let rhs = self.hir_to_general_operand(&op.rhs);
                    self.insts.push(McIR::ICmp(*kind, lhs, rhs));
                }
                Hir::FCmp(kind, op) => {
                    let lhs = self.ssa_map[op.lhs].unwrap().as_f();
                    let rhs = self.ssa_map[op.rhs].unwrap().as_f();
                    let ret = self.alloc_greg(op.ret);
                    self.ssa_map[op.ret] = Some(McReg::GReg(ret));
                    self[lhs].release();
                    self[rhs].release();
                    self.insts.push(McIR::FCmp(*kind, ret, lhs, rhs));
                }
                Hir::ICmpBr(kind, lhs, rhs, then_bb, else_bb) => {
                    let lhs = self.ssa_map[*lhs].unwrap().as_g();
                    let rhs = self.hir_to_general_operand(rhs);
                    self[lhs].release();
                    self.insts.push(McIR::ICmpJmp(*kind, lhs, rhs, *then_bb));
                    self.insts.push(McIR::Jmp(*else_bb));
                }
                Hir::FCmpBr(kind, lhs, rhs, then_bb, else_bb) => {
                    let lhs = self.ssa_map[*lhs].unwrap().as_f();
                    let rhs = self.ssa_map[*rhs].unwrap().as_f();
                    self[lhs].release();
                    self[rhs].release();
                    self.insts.push(McIR::FCmpJmp(*kind, lhs, rhs, *then_bb));
                    self.insts.push(McIR::Jmp(*else_bb));
                }

                Hir::Ret(op) => match op {
                    HirOperand::Reg(ssa) => {
                        let ty = func[*ssa].ty;
                        match ty {
                            Type::Integer | Type::Bool => {
                                let reg = self.ssa_map[*ssa].unwrap().as_g();
                                self[reg].release();
                                self.insts.push(McIR::IRet(McGeneralOperand::Reg(reg), ty));
                            }
                            Type::Float => {
                                let reg = self.ssa_map[*ssa].unwrap().as_f();
                                self[reg].release();
                                self.insts.push(McIR::FRet(McFloatOperand::Reg(reg)));
                            }
                        }
                    }
                    HirOperand::Const(c) => match c {
                        Value::Integer(i) => self
                            .insts
                            .push(McIR::IRet(McGeneralOperand::Integer(*i), Type::Integer)),
                        Value::Float(f) => self.insts.push(McIR::FRet(McFloatOperand::Float(*f))),
                        Value::Bool(b) => {
                            let b = if *b { 1 } else { 0 };
                            self.insts
                                .push(McIR::IRet(McGeneralOperand::Integer(b), Type::Bool))
                        }
                    },
                },
                Hir::INeg(op) => match &op.src {
                    HirOperand::Const(c) => {
                        let n = c.as_i();
                        let reg = self.alloc_greg(op.ret);
                        self.insts.push(McIR::Integer(reg, -n));
                    }
                    HirOperand::Reg(src) => {
                        let reg = self.ssa_map[*src].unwrap().as_g();
                        self.ssa_map[op.ret] = Some(McReg::GReg(reg));
                        self.insts.push(McIR::INeg(reg));
                    }
                },
                Hir::FNeg(op) => match &op.src {
                    HirOperand::Const(c) => {
                        let n = c.as_f();
                        let reg = self.alloc_freg(op.ret);
                        self.insts.push(McIR::Float(reg, -n));
                    }
                    HirOperand::Reg(src) => {
                        let reg = self.ssa_map[*src].unwrap().as_f();
                        self.ssa_map[op.ret] = Some(McReg::FReg(reg));
                        self.insts.push(McIR::FNeg(reg));
                    }
                },
                Hir::LocalStore(ret, info, reg) => {
                    let ty = info.1;
                    assert_eq!(ty, func[*reg].ty);
                    let reg = self.ssa_map[*reg].unwrap();
                    if let Some(ret) = ret {
                        self.ssa_map[*ret] = Some(reg);
                    } else {
                        self.invalidate(reg);
                    }
                    self.insts.push(McIR::LocalStore(info.0, reg));
                }
                Hir::LocalLoad(info, reg) => {
                    let ty = info.1;
                    assert_eq!(ty, func[*reg].ty);
                    let reg = self.alloc_reg(*reg, ty);
                    self.insts.push(McIR::LocalLoad(info.0, reg));
                }
                Hir::Br(next_bb) => {
                    let move_list = hir_context[*next_bb]
                        .insts
                        .iter()
                        .filter_map(|ir| match ir {
                            Hir::Phi(_, phi) => phi.iter().find_map(|(i, r)| {
                                if self.cur_block == *i {
                                    Some(r)
                                } else {
                                    None
                                }
                            }),
                            _ => None,
                        })
                        .collect::<Vec<_>>();
                    if move_list.len() == 0 {
                        self.insts.push(McIR::Jmp(*next_bb));
                    } else {
                        assert_eq!(1, move_list.len());
                        let src = move_list[0];
                        let src_reg = self.ssa_map[*src].unwrap();
                        self.insts.push(McIR::Out(src_reg));
                        self.insts.push(McIR::Jmp(*next_bb));
                        self.invalidate(src_reg);
                    }
                }
                Hir::CondBr(cond_, then_bb, else_bb) => {
                    let cond_ = self.ssa_map[*cond_].unwrap();
                    self.insts.push(McIR::CondJmp(cond_, *else_bb));
                    self.insts.push(McIR::Jmp(*then_bb));
                }
                Hir::Phi(ret, _) => {
                    let reg = self.alloc_reg(*ret, func[*ret].ty);
                    self.insts.push(McIR::In(reg));
                }
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum McIR {
    Jmp(usize),
    ICmpJmp(CmpKind, GReg, McGeneralOperand, usize),
    FCmpJmp(CmpKind, FReg, FReg, usize),
    CondJmp(McReg, usize),
    Out(McReg),
    In(McReg),
    Integer(GReg, i32),
    Float(FReg, f64),
    CastIntFloat(FReg, McGeneralOperand),
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
    ICmp(CmpKind, GReg, McGeneralOperand),
    FCmp(CmpKind, GReg, FReg, FReg),
    IRet(McGeneralOperand, Type),
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

    fn assign(&mut self, ssa: SsaReg) {
        self.ssareg = Some(ssa);
    }

    fn release(&mut self) {
        self.ssareg = None;
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

    fn assign(&mut self, ssa: SsaReg) {
        self.ssareg = Some(ssa);
    }

    fn release(&mut self) {
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
