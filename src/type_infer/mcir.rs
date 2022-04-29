use super::*;

#[derive(Clone, Debug, PartialEq)]
pub enum McIR {
    Jmp(usize),
    ICmpJmp(CmpKind, GReg, McGeneralOperand, usize, usize), // kind, lhs, rhs, then_bb, else_bb
    FCmpJmp(CmpKind, FReg, FReg, usize, usize),             // kind, lhs, rhs, then_bb, else_bb
    CondJmp(McReg, usize, usize),
    GMove(GReg, GReg),
    FMove(FReg, FReg),
    Integer(GReg, i32),
    Float(FReg, f64),
    Nil(GReg),
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
    LocalStore(usize, Option<McReg>, McOperand),
    LocalLoad(usize, McReg),
    Call(usize, Option<GReg>, Vec<McOperand>, Vec<GReg>, Vec<FReg>), // func_id, ret, arg, using_general_registers
}

#[derive(Clone, Debug, PartialEq)]
pub enum McOperand {
    General(McGeneralOperand),
    Float(McFloatOperand),
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

#[derive(Clone, PartialEq)]
pub struct McIrContext {
    g_reginfo: Vec<GRegInfo>,
    f_reginfo: Vec<FRegInfo>,
    ssa_map: SsaMap,
    cur_fn: usize,
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
                "\tFunc {} {} g_reg:{} f_reg:{} local:{:?} {{",
                func.id, func.name, func.g_regs, func.f_regs, func.locals
            )?;
            for (bbi, block) in func.blocks.iter().enumerate() {
                writeln!(f, "\t\tBlock {} {{", bbi)?;
                for hir in &block.insts {
                    let s = match hir {
                        McIR::Jmp(dest) => format!("jmp {}", dest),
                        McIR::CondJmp(cond, then_, else_) => {
                            format!("cond_jmp {:?} {} {}", cond, then_, else_)
                        }
                        McIR::ICmpJmp(kind, lhs, rhs, then_, else_) => {
                            format!(
                                "cmp_jmp ({:?} %{:?}, %{:?}) then {} else {}",
                                kind, lhs, rhs, then_, else_
                            )
                        }
                        McIR::FCmpJmp(kind, lhs, rhs, then_, else_) => {
                            format!(
                                "cmp_jmp ({:?} %{:?}, %{:?}) then {} else {}",
                                kind, lhs, rhs, then_, else_
                            )
                        }
                        McIR::GMove(src, dst) => format!("%{:?} = %{:?}", dst, src),
                        McIR::FMove(src, dst) => format!("%{:?} = %{:?}", dst, src),
                        McIR::Integer(ret, i) => format!("%{:?} = {}: i32", ret, i),
                        McIR::Float(ret, f) => format!("%{:?} = {}: f64", ret, f),
                        McIR::Nil(ret) => format!("%{:?} = nil", ret),
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
                        McIR::LocalStore(ofs, ret, reg) => match ret {
                            Some(ret) => format!("store ${}|{:?}, {:?}", ofs, ret, reg),
                            None => format!("store ${}, {:?}", ofs, reg),
                        },
                        McIR::LocalLoad(ofs, reg) => format!("load ${}, {:?}", ofs, reg),
                        McIR::Call(fid, ret, arg, g_using, f_using) => {
                            if let Some(ret) = ret {
                                format!(
                                    "%{:?} = call {} ({:?}) save_reg:{:?} {:?}",
                                    ret, self.functions[*fid].name, arg, g_using, f_using
                                )
                            } else {
                                format!(
                                    "%_ = call {} ({:?}) save_reg:{:?}",
                                    self.functions[*fid].name, arg, g_using
                                )
                            }
                        }
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

impl McIrContext {
    fn func_mut(&mut self) -> &mut McIrFunc {
        &mut self.functions[self.cur_fn]
    }

    /*fn invalidate(&mut self, reg: McReg) {
        match reg {
            McReg::FReg(f) => self[f].release(),
            McReg::GReg(g) => self[g].release(),
        }
    }*/

    fn alloc_reg(&mut self, ssareg: SsaReg, ty: Type) -> McReg {
        match ty {
            Type::Integer | Type::Bool | Type::Nil => McReg::GReg(self.alloc_greg(ssareg)),
            Type::Float => McReg::FReg(self.alloc_freg(ssareg)),
        }
    }

    fn mir_to_operand(&mut self, op: &MirOperand) -> McOperand {
        match op {
            MirOperand::Reg(reg) => match self.ssa_map[*reg].unwrap() {
                McReg::GReg(reg) => {
                    self[reg].release();
                    McOperand::General(McGeneralOperand::Reg(reg))
                }
                McReg::FReg(reg) => {
                    self[reg].release();
                    McOperand::Float(McFloatOperand::Reg(reg))
                }
            },
            MirOperand::Const(rhs) => match rhs.unpack() {
                RV::Integer(i) => McOperand::General(McGeneralOperand::Integer(i)),
                RV::Float(f) => McOperand::Float(McFloatOperand::Float(f)),
                _ => unreachable!(),
            },
        }
    }

    fn mir_to_operand_(
        &mut self,
        op: &MirOperand,
        ret: &Option<SsaReg>,
    ) -> (McOperand, Option<McReg>) {
        match op {
            MirOperand::Reg(reg) => {
                let mcreg = self.ssa_map[*reg].unwrap();
                match mcreg {
                    McReg::GReg(reg) => {
                        let op = McOperand::General(McGeneralOperand::Reg(reg));
                        match ret {
                            Some(ret) => {
                                self.ssa_map[*ret] = Some(mcreg);
                                self[reg].assign(*ret);
                                (op, Some(mcreg))
                            }
                            None => {
                                self[reg].release();
                                (op, None)
                            }
                        }
                    }
                    McReg::FReg(reg) => {
                        let op = McOperand::Float(McFloatOperand::Reg(reg));
                        match ret {
                            Some(ret) => {
                                self.ssa_map[*ret] = Some(mcreg);
                                self[reg].assign(*ret);
                                (op, Some(mcreg))
                            }
                            None => {
                                self[reg].release();
                                (op, None)
                            }
                        }
                    }
                }
            }
            MirOperand::Const(rhs) => match rhs.unpack() {
                RV::Integer(i) => {
                    let op = McOperand::General(McGeneralOperand::Integer(i));
                    match ret {
                        None => (op, None),
                        Some(ret) => {
                            let mcreg = self.alloc_reg(*ret, Type::Integer);
                            (op, Some(mcreg))
                        }
                    }
                }
                RV::Float(f) => {
                    let op = McOperand::Float(McFloatOperand::Float(f));
                    match ret {
                        None => (op, None),
                        Some(ret) => {
                            let mcreg = self.alloc_reg(*ret, Type::Float);
                            (op, Some(mcreg))
                        }
                    }
                }
                _ => unreachable!(),
            },
        }
    }

    fn mir_to_general_operand(&mut self, rhs: &MirOperand) -> McGeneralOperand {
        match rhs {
            MirOperand::Reg(rhs) => {
                let rhs = self.ssa_map[*rhs].unwrap().as_g();
                self[rhs].release();
                McGeneralOperand::Reg(rhs)
            }
            MirOperand::Const(rhs) => McGeneralOperand::Integer(rhs.as_i()),
        }
    }

    fn mir_to_greg(&mut self, op: &MirOperand, ret: SsaReg) -> GReg {
        match &op {
            MirOperand::Reg(lhs) => {
                let lhs = self.ssa_map[*lhs].unwrap();
                self.ssa_map[ret] = Some(lhs);
                lhs.as_g()
            }

            MirOperand::Const(lhs) => {
                let n = lhs.as_i();
                let lhs = self.alloc_greg(ret);
                self.func_mut().insts.push(McIR::Integer(lhs, n));
                lhs
            }
        }
    }

    fn mir_to_float_operand(&mut self, rhs: &MirOperand) -> McFloatOperand {
        match rhs {
            MirOperand::Reg(rhs) => {
                let rhs = self.ssa_map[*rhs].unwrap().as_f();
                self[rhs].release();
                McFloatOperand::Reg(rhs)
            }
            MirOperand::Const(rhs) => McFloatOperand::Float(rhs.as_f()),
        }
    }

    fn mir_to_freg(&mut self, op: &MirOperand, ret: SsaReg) -> FReg {
        match &op {
            MirOperand::Reg(lhs) => {
                let lhs = self.ssa_map[*lhs].unwrap();
                self.ssa_map[ret] = Some(lhs);
                lhs.as_f()
            }

            MirOperand::Const(lhs) => {
                let n = lhs.as_f();
                let lhs = self.alloc_freg(ret);
                self.func_mut().insts.push(McIR::Float(lhs, n));
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
    pub id: usize,
    /// Name of the function.
    pub name: String,
    /// Number of virtual general registers.
    pub g_regs: usize,
    /// Number of virtual float registers.
    pub f_regs: usize,
    pub args: Vec<(String, Type)>,
    /// Offsets and types of local variables.
    pub locals: Vec<Type>,
    /// Type of return value.
    pub ret_ty: Type,
    cur_block: usize,
    pub blocks: Vec<McIrBlock>,
}

impl McIrFunc {
    fn new(id: usize, name: String, args: Vec<(String, Type)>, locals: &Vec<Option<Type>>) -> Self {
        let locals = locals.iter().map(|ty| ty.unwrap()).collect();
        Self {
            id,
            name,
            g_regs: 0,
            f_regs: 0,
            args,
            locals,
            ret_ty: Type::Integer,
            cur_block: 0,
            blocks: vec![],
        }
    }
}

impl std::ops::Deref for McIrFunc {
    type Target = McIrBlock;

    fn deref(&self) -> &McIrBlock {
        &self.blocks[self.cur_block]
    }
}

impl std::ops::DerefMut for McIrFunc {
    fn deref_mut(&mut self) -> &mut McIrBlock {
        &mut self.blocks[self.cur_block]
    }
}

impl std::ops::Index<usize> for McIrFunc {
    type Output = McIrBlock;

    fn index(&self, i: usize) -> &McIrBlock {
        &self.blocks[i]
    }
}

impl std::ops::IndexMut<usize> for McIrFunc {
    fn index_mut(&mut self, i: usize) -> &mut McIrBlock {
        &mut self.blocks[i]
    }
}

#[derive(Clone, PartialEq)]
pub struct McIrBlock {
    pub insts: Vec<McIR>,
    pub owner_function: usize,
    using_reg: Option<(usize, usize)>, // using_greg, using_freg
}

impl McIrBlock {
    fn new(owner_function: usize) -> Self {
        Self {
            insts: vec![],
            owner_function,
            using_reg: None,
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
    pub fn as_g(self) -> GReg {
        match self {
            McReg::GReg(r) => r,
            _ => unreachable!(),
        }
    }

    pub fn as_f(self) -> FReg {
        match self {
            McReg::FReg(r) => r,
            _ => unreachable!(),
        }
    }
}

macro_rules! float_ops {
    ($self:ident, $op:ident, $v:ident) => {{
        let lhs = $self.mir_to_freg(&$op.lhs, $op.ret);
        let rhs = $self.mir_to_float_operand(&$op.rhs);
        $self.func_mut().insts.push(McIR::$v(lhs, rhs));
    }};
}

impl McIrContext {
    pub fn new() -> Self {
        Self {
            g_reginfo: vec![],
            f_reginfo: vec![],
            ssa_map: SsaMap(vec![]),
            cur_fn: 0,
            functions: vec![],
        }
    }

    pub fn g_reg_num(&self) -> usize {
        self.g_reginfo.len()
    }

    pub fn f_reg_num(&self) -> usize {
        self.f_reginfo.len()
    }

    pub fn from_mir(&mut self, mir_func: &MirFunction) -> usize {
        let next_fn = self.functions.len();
        let mut mcir_func = McIrFunc::new(
            next_fn,
            mir_func.name.clone(),
            mir_func.args.clone(),
            &mir_func.locals,
        );
        mcir_func.blocks = mir_func
            .basic_block
            .iter()
            .map(|mir_bb| McIrBlock::new(mir_bb.owner_function.to_usize()))
            .collect();
        self.functions.push(mcir_func);
        self.cur_fn = next_fn;
        self.ssa_map = SsaMap(vec![None; mir_func.register_num()]);
        let mut g_reg_num = 0;
        let mut f_reg_num = 0;
        for (i, bb) in mir_func.basic_block.iter().enumerate() {
            self.func_mut().cur_block = i;
            self.compile_bb(bb, mir_func);
            g_reg_num = std::cmp::max(g_reg_num, self.g_reg_num());
            f_reg_num = std::cmp::max(f_reg_num, self.f_reg_num());
        }
        self.func_mut().g_regs = g_reg_num;
        self.func_mut().f_regs = f_reg_num;
        self.func_mut().ret_ty = mir_func.ret_ty.unwrap();
        //dbg!(self);
        next_fn
    }

    fn compile_bb(&mut self, bb: &MirBasicBlock, hir_func: &MirFunction) {
        self.g_reginfo = vec![];
        self.f_reginfo = vec![];
        for hir in &bb.insts {
            match hir {
                Mir::Integer(ssa, i) => {
                    let reg = self.alloc_greg(*ssa);
                    self.func_mut().insts.push(McIR::Integer(reg, *i));
                }
                Mir::Float(ssa, f) => {
                    let reg = self.alloc_freg(*ssa);
                    self.func_mut().insts.push(McIR::Float(reg, *f));
                }
                Mir::Nil(ssa) => {
                    let reg = self.alloc_greg(*ssa);
                    self.func_mut().insts.push(McIR::Nil(reg));
                }
                Mir::CastIntFloat(op) => {
                    let dst = self.alloc_freg(op.ret);
                    let src = match &op.src {
                        MirOperand::Const(c) => McGeneralOperand::Integer(c.as_i()),
                        MirOperand::Reg(r) => {
                            let src = self.ssa_map[*r].unwrap().as_g();
                            self[src].release();
                            McGeneralOperand::Reg(src)
                        }
                    };
                    self.func_mut().insts.push(McIR::CastIntFloat(dst, src));
                }
                Mir::IAdd(op) => {
                    let lhs = self.mir_to_greg(&op.lhs, op.ret);
                    let rhs = self.mir_to_general_operand(&op.rhs);
                    self.func_mut().insts.push(McIR::IAdd(lhs, rhs));
                }
                Mir::ISub(op) => {
                    let lhs = self.mir_to_greg(&op.lhs, op.ret);
                    let rhs = self.mir_to_general_operand(&op.rhs);
                    self.func_mut().insts.push(McIR::ISub(lhs, rhs));
                }
                Mir::IMul(op) => {
                    let lhs = self.ssa_map[op.lhs].unwrap().as_g();
                    let rhs = self.ssa_map[op.rhs].unwrap().as_g();
                    self.ssa_map[op.ret] = Some(McReg::GReg(lhs));
                    self[rhs].release();
                    self.func_mut().insts.push(McIR::IMul(lhs, rhs));
                }
                Mir::IDiv(op) => {
                    let lhs = self.ssa_map[op.lhs].unwrap().as_g();
                    let rhs = self.ssa_map[op.rhs].unwrap().as_g();
                    self.ssa_map[op.ret] = Some(McReg::GReg(lhs));
                    self[rhs].release();
                    self.func_mut().insts.push(McIR::IDiv(lhs, rhs));
                }
                Mir::FAdd(op) => float_ops!(self, op, FAdd),
                Mir::FSub(op) => float_ops!(self, op, FSub),
                Mir::FMul(op) => float_ops!(self, op, FMul),
                Mir::FDiv(op) => float_ops!(self, op, FDiv),

                Mir::ICmp(kind, op) => {
                    let lhs = self.mir_to_greg(&op.lhs, op.ret);
                    let rhs = self.mir_to_general_operand(&op.rhs);
                    self.func_mut().insts.push(McIR::ICmp(*kind, lhs, rhs));
                }
                Mir::FCmp(kind, op) => {
                    let lhs = self.ssa_map[op.lhs].unwrap().as_f();
                    let rhs = self.ssa_map[op.rhs].unwrap().as_f();
                    let ret = self.alloc_greg(op.ret);
                    self.ssa_map[op.ret] = Some(McReg::GReg(ret));
                    self[lhs].release();
                    self[rhs].release();
                    self.func_mut().insts.push(McIR::FCmp(*kind, ret, lhs, rhs));
                }
                Mir::ICmpBr(kind, lhs, rhs, then_bb, else_bb) => {
                    let lhs = self.ssa_map[*lhs].unwrap().as_g();
                    let rhs = self.mir_to_general_operand(rhs);
                    self[lhs].release();
                    self.func_mut().insts.push(McIR::ICmpJmp(
                        *kind,
                        lhs,
                        rhs,
                        then_bb.to_usize(),
                        else_bb.to_usize(),
                    ));
                }
                Mir::FCmpBr(kind, lhs, rhs, then_bb, else_bb) => {
                    let lhs = self.ssa_map[*lhs].unwrap().as_f();
                    let rhs = self.ssa_map[*rhs].unwrap().as_f();
                    self[lhs].release();
                    self[rhs].release();
                    self.func_mut().insts.push(McIR::FCmpJmp(
                        *kind,
                        lhs,
                        rhs,
                        then_bb.to_usize(),
                        else_bb.to_usize(),
                    ));
                }

                Mir::Ret(op) => match op {
                    MirOperand::Reg(ssa) => {
                        let ty = hir_func[*ssa].ty;
                        match ty {
                            Type::Integer | Type::Bool | Type::Nil => {
                                let reg = self.ssa_map[*ssa].unwrap().as_g();
                                self[reg].release();
                                self.func_mut()
                                    .insts
                                    .push(McIR::IRet(McGeneralOperand::Reg(reg), ty));
                            }
                            Type::Float => {
                                let reg = self.ssa_map[*ssa].unwrap().as_f();
                                self[reg].release();
                                self.func_mut()
                                    .insts
                                    .push(McIR::FRet(McFloatOperand::Reg(reg)));
                            }
                        }
                    }
                    MirOperand::Const(c) => match c.unpack() {
                        RV::Integer(i) => self
                            .func_mut()
                            .insts
                            .push(McIR::IRet(McGeneralOperand::Integer(i), Type::Integer)),
                        RV::Float(f) => self
                            .func_mut()
                            .insts
                            .push(McIR::FRet(McFloatOperand::Float(f))),
                        RV::Bool(b) => {
                            let b = if b { 1 } else { 0 };
                            self.func_mut()
                                .insts
                                .push(McIR::IRet(McGeneralOperand::Integer(b), Type::Bool))
                        }
                        RV::Nil => self
                            .func_mut()
                            .insts
                            .push(McIR::IRet(McGeneralOperand::Integer(0), Type::Nil)),
                    },
                },

                Mir::INeg(op) => match &op.src {
                    MirOperand::Const(c) => {
                        let n = c.as_i();
                        let reg = self.alloc_greg(op.ret);
                        self.func_mut().insts.push(McIR::Integer(reg, -n));
                    }
                    MirOperand::Reg(src) => {
                        let reg = self.ssa_map[*src].unwrap().as_g();
                        self.ssa_map[op.ret] = Some(McReg::GReg(reg));
                        self.func_mut().insts.push(McIR::INeg(reg));
                    }
                },
                Mir::FNeg(op) => match &op.src {
                    MirOperand::Const(c) => {
                        let n = c.as_f();
                        let reg = self.alloc_freg(op.ret);
                        self.func_mut().insts.push(McIR::Float(reg, -n));
                    }
                    MirOperand::Reg(src) => {
                        let reg = self.ssa_map[*src].unwrap().as_f();
                        self.ssa_map[op.ret] = Some(McReg::FReg(reg));
                        self.func_mut().insts.push(McIR::FNeg(reg));
                    }
                },
                Mir::LocalStore(ret, info, reg) => {
                    let ty = info.1;
                    assert_eq!(ty, hir_func.get_operand_ty(reg));
                    let (rhs, ret) = self.mir_to_operand_(reg, ret);
                    self.func_mut()
                        .insts
                        .push(McIR::LocalStore(info.0, ret, rhs));
                }
                Mir::LocalLoad(info, reg) => {
                    let ty = info.1;
                    assert_eq!(ty, hir_func[*reg].ty);
                    let reg = self.alloc_reg(*reg, ty);
                    self.func_mut().insts.push(McIR::LocalLoad(info.0, reg));
                }
                Mir::Call(func_id, ret, args) => {
                    let args = args.iter().map(|arg| self.mir_to_operand(arg)).collect();
                    let g_using: Vec<_> = self
                        .g_reginfo
                        .iter()
                        .enumerate()
                        .filter_map(|(i, info)| info.ssareg.map(|_| GReg(i)))
                        .collect();
                    let f_using: Vec<_> = self
                        .f_reginfo
                        .iter()
                        .enumerate()
                        .filter_map(|(i, info)| info.ssareg.map(|_| FReg(i)))
                        .collect();
                    let ret = ret.map(|ret| self.alloc_greg(ret));
                    self.func_mut().insts.push(McIR::Call(
                        func_id.to_usize(),
                        ret,
                        args,
                        g_using,
                        f_using,
                    ));
                }
                Mir::Br(next_bb) => {
                    let move_list = hir_func[*next_bb]
                        .insts
                        .iter()
                        .filter_map(|ir| match ir {
                            Mir::Phi(_, phi) => phi.iter().find_map(|(i, r, ty)| {
                                if self.func_mut().cur_block == i.to_usize() {
                                    Some((r, ty))
                                } else {
                                    None
                                }
                            }),
                            _ => None,
                        })
                        .collect::<Vec<_>>();
                    if move_list.len() == 0 {
                        self.func_mut().insts.push(McIR::Jmp(next_bb.to_usize()));
                        let using_reg = &mut self.func_mut().blocks[next_bb.to_usize()].using_reg;
                        match using_reg {
                            Some((0, 0)) => {}
                            None => *using_reg = Some((0, 0)),
                            using_reg => panic!("abnormal using_reg info. {:?}", using_reg),
                        };
                    } else {
                        assert_eq!(1, move_list.len());
                        let mut f_reg = 0;
                        let mut g_reg = 0;
                        for src in move_list {
                            let src_reg = self.ssa_map[*src.0].unwrap();
                            match src.1 {
                                &Type::Float => {
                                    let reg = src_reg.as_f();
                                    self.func_mut().insts.push(McIR::FMove(reg, FReg(f_reg)));
                                    self[reg].release();
                                    f_reg += 1;
                                }
                                _ => {
                                    let reg = src_reg.as_g();
                                    self.func_mut().insts.push(McIR::GMove(reg, GReg(g_reg)));
                                    self[reg].release();
                                    g_reg += 1;
                                }
                            }
                            self.func_mut().insts.push(McIR::Jmp(next_bb.to_usize()));
                            let using_reg =
                                &mut self.func_mut().blocks[next_bb.to_usize()].using_reg;
                            match using_reg {
                                Some(using) => assert!(*using == (g_reg, f_reg)),
                                None => *using_reg = Some((g_reg, f_reg)),
                            };
                        }
                    }
                }
                Mir::CondBr(cond_, then_bb, else_bb) => {
                    let cond_ = self.ssa_map[*cond_].unwrap();
                    self.func_mut().insts.push(McIR::CondJmp(
                        cond_,
                        then_bb.to_usize(),
                        else_bb.to_usize(),
                    ));
                }
                Mir::Phi(ret, _) => {
                    let _reg = self.alloc_reg(*ret, hir_func[*ret].ty);
                    //self.insts.push(McIR::In(reg));*/
                }
            }
        }
    }
}
