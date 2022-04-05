use std::collections::HashMap;

use crate::hir::{Hir, HirBBId, HirFuncId, HirFunction, HirOperand, SsaReg};

use super::{CmpKind, Type, Value, RV};

type Result<T> = std::result::Result<T, MirErr>;

///
/// Instructions of High-level IR.
///
#[derive(Clone, Debug, PartialEq)]
pub enum Mir {
    Br(MirBBId),
    CondBr(SsaReg, MirBBId, MirBBId),
    ICmpBr(CmpKind, SsaReg, MirOperand, MirBBId, MirBBId),
    FCmpBr(CmpKind, SsaReg, SsaReg, MirBBId, MirBBId),
    Phi(SsaReg, Vec<(MirBBId, SsaReg, Type)>), // ret, [(bb, reg, type)]
    Integer(SsaReg, i32),
    Float(SsaReg, f64),
    Nil(SsaReg),
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
    LocalStore(Option<SsaReg>, (usize, Type), MirOperand), // (ret, (offset, type), rhs)
    LocalLoad((usize, Type), SsaReg),
    Call(MirFuncId, Option<SsaReg>, Vec<MirOperand>), // (id, ret, arg)
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
            Self::Reg(r) => write!(f, "{:?}", r),
            Self::Const(c) => write!(f, "{:?}", c),
        }
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
        Self { ty: ty }
    }
}

///
/// A state of MIR.
///
#[derive(Clone, PartialEq)]
pub struct MirContext {
    /// Functions.
    pub functions: Vec<MirFunction>,
    cur_fn: MirFuncId,
    func_map: HashMap<HirFuncId, MirFuncId>,
}

impl std::fmt::Debug for MirContext {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "MirContxt {{")?;

        for func in &self.functions {
            writeln!(
                f,
                "\tFunction {} ret:{:?} args:{:?}{{",
                func.name, func.ret_ty, func.args
            )?;
            writeln!(f, "\t\tSsaInfo {:?}", func.reginfo)?;
            for (i, bb) in func.basic_block.iter().enumerate() {
                writeln!(f, "\t\tBasicBlock {} {{ owner:{:?}", i, bb.owner_function)?;
                for hir in &bb.insts {
                    let s = match hir {
                        Mir::Integer(ret, i) => {
                            format!("{:?}: {:?} = {}: i32", ret, func[*ret].ty, i)
                        }
                        Mir::Float(ret, f) => {
                            format!("{:?}: {:?} = {}: f64", ret, func[*ret].ty, f)
                        }
                        Mir::Nil(ret) => {
                            format!("{:?}: {:?} = nil", ret, func[*ret].ty,)
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
                            let name = &self[*id].name;
                            match ret {
                                Some(ret) => format!("{:?} = call {} ({:?})", ret, name, arg),
                                None => format!("%_ = call {} ({:?})", name, arg),
                            }
                        }
                        Mir::Br(dest) => format!("br {:?}", dest),
                        Mir::ICmpBr(kind, lhs, rhs, then_, else_) => {
                            format!(
                                "icmp_br ({:?} {:?}, {:?}) then {:?} else {:?}",
                                kind, lhs, rhs, then_, else_
                            )
                        }
                        Mir::FCmpBr(kind, lhs, rhs, then_, else_) => {
                            format!(
                                "fcmp_br ({:?} {:?}, {:?}) then {:?} else {:?}",
                                kind, lhs, rhs, then_, else_
                            )
                        }
                        Mir::CondBr(cond, then_, else_) => {
                            format!("condbr {:?} then {:?} else {:?}", cond, then_, else_)
                        }
                        Mir::Phi(ret, phi) => {
                            let phi_s = phi
                                .iter()
                                .map(|(bb, r, ty)| format!("({:?}, {:?}: {:?})", bb, r, ty))
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
    type Target = MirFunction;

    fn deref(&self) -> &Self::Target {
        let cur_fn = self.cur_fn;
        &self[cur_fn]
    }
}

impl std::ops::DerefMut for MirContext {
    fn deref_mut(&mut self) -> &mut Self::Target {
        let cur_fn = self.cur_fn;
        &mut self[cur_fn]
    }
}

impl std::ops::Index<MirFuncId> for MirContext {
    type Output = MirFunction;

    fn index(&self, i: MirFuncId) -> &MirFunction {
        &self.functions[i.0]
    }
}

impl std::ops::IndexMut<MirFuncId> for MirContext {
    fn index_mut(&mut self, i: MirFuncId) -> &mut MirFunction {
        &mut self.functions[i.0]
    }
}

impl std::ops::Index<MirBBId> for MirContext {
    type Output = MirBasicBlock;

    fn index(&self, i: MirBBId) -> &MirBasicBlock {
        let cur_fn = self.cur_fn;
        &self[cur_fn][i]
    }
}

impl std::ops::IndexMut<MirBBId> for MirContext {
    fn index_mut(&mut self, i: MirBBId) -> &mut MirBasicBlock {
        let cur_fn = self.cur_fn;
        &mut self[cur_fn][i]
    }
}

impl MirContext {
    pub fn new() -> Self {
        let cur_fn = MirFuncId::default();
        MirContext {
            functions: vec![],
            cur_fn,
            func_map: HashMap::default(),
        }
    }

    fn func(&self) -> &MirFunction {
        let cur_fn = self.cur_fn;
        &self.functions[cur_fn.0]
    }

    fn next_fn(&self) -> MirFuncId {
        MirFuncId(self.functions.len())
    }

    /// Generate MIR in new function from HirFunction.
    pub fn new_func_from_hir(
        &mut self,
        func_name: String,
        args: Vec<(String, Type)>,
        hir: &HirFunction,
    ) -> Result<MirFuncId> {
        let next_fn = self.next_fn();
        let func = MirFunction::new(next_fn, func_name, args, hir.locals.len());
        //assert_eq!(0, func.new_bb().0);
        self.functions.push(func);
        self.cur_fn = next_fn;
        self.func_map.insert(hir.id, next_fn);
        self.gen_func_from_hir(hir)?;
        Ok(self.cur_fn)
    }

    /*fn new_call(&mut self, name: &str, args: Vec<MirOperand>, ty: Type) -> Result<SsaReg> {
        let ret = self.func().next_reg();
        let id = self.get_function(&name)?;
        Ok(self
            .func_mut()
            .add_assign(Mir::Call(id, Some(ret), args), ty))
    }

    fn new_call_nouse(&mut self, name: &str, args: Vec<MirOperand>) -> Result<()> {
        let id = self.get_function(&name)?;
        let hir = Mir::Call(id, None, args);
        self.func_mut().insts.push(hir);
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
    }*/
}

///
/// ID of MIR function.
///
#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub struct MirFuncId(usize);

impl MirFuncId {
    pub fn to_usize(&self) -> usize {
        self.0
    }
}

///
/// ID of MIR basic block.
///
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default)]
pub struct MirBBId(usize);

impl MirBBId {
    pub fn to_usize(&self) -> usize {
        self.0
    }

    fn from(hirbb: HirBBId) -> Self {
        Self(hirbb.to_usize())
    }
}

#[derive(Clone, PartialEq)]
pub struct MirFunction {
    pub id: MirFuncId,
    pub name: String,
    pub ret: Vec<MirOperand>,
    pub ret_ty: Option<Type>,
    /// SSA register information.
    reginfo: Vec<SsaRegInfo>,
    pub locals: Vec<Option<Type>>,
    pub args: Vec<(String, Type)>,
    /// Basic blocks.
    pub basic_block: Vec<MirBasicBlock>,
    cur_bb: MirBBId,
    /// Map for relation between HirSsa and MirSsa.
    hir_mir_map: HashMap<SsaReg, SsaReg>,
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

impl std::ops::Deref for MirFunction {
    type Target = MirBasicBlock;

    fn deref(&self) -> &Self::Target {
        let cur_bb = self.cur_bb;
        &self[cur_bb]
    }
}

impl std::ops::DerefMut for MirFunction {
    fn deref_mut(&mut self) -> &mut Self::Target {
        let cur_bb = self.cur_bb;
        &mut self[cur_bb]
    }
}

impl std::ops::Index<MirBBId> for MirFunction {
    type Output = MirBasicBlock;

    fn index(&self, i: MirBBId) -> &MirBasicBlock {
        &self.basic_block[i.0]
    }
}

impl std::ops::IndexMut<MirBBId> for MirFunction {
    fn index_mut(&mut self, i: MirBBId) -> &mut MirBasicBlock {
        &mut self.basic_block[i.0]
    }
}

impl MirFunction {
    fn new(id: MirFuncId, name: String, args: Vec<(String, Type)>, local_len: usize) -> Self {
        let mut locals = vec![None; local_len];
        for (i, (_, ty)) in args.iter().enumerate() {
            locals[i] = Some(*ty);
        }
        Self {
            id,
            name,
            ret: vec![],
            ret_ty: None,
            reginfo: vec![],
            locals,
            args,
            basic_block: vec![],
            cur_bb: MirBBId::default(),
            hir_mir_map: HashMap::default(),
        }
    }

    pub fn register_num(&self) -> usize {
        self.reginfo.len()
    }

    fn next_reg(&self) -> SsaReg {
        SsaReg::new(self.register_num())
    }

    fn new_bb(&mut self) -> MirBBId {
        let bb = MirBasicBlock::new(self.id);
        let next = self.basic_block.len();
        self.basic_block.push(bb);
        MirBBId(next)
    }

    fn add_assign(&mut self, mir: Mir, ty: Type, hirssa: Option<SsaReg>) -> SsaReg {
        let ret_reg = self.next_reg();
        self.reginfo.push(SsaRegInfo::new(ty));
        self.insts.push(mir);
        if let Some(hirssa) = hirssa {
            self.hir_mir_map.insert(hirssa, ret_reg);
        }
        ret_reg
    }

    fn new_integer(&mut self, i: i32, hirssa: Option<SsaReg>) -> SsaReg {
        self.add_assign(Mir::Integer(self.next_reg(), i), Type::Integer, hirssa)
    }

    fn new_float(&mut self, f: f64, hirssa: Option<SsaReg>) -> SsaReg {
        self.add_assign(Mir::Float(self.next_reg(), f), Type::Float, hirssa)
    }

    fn new_nil(&mut self, hirssa: Option<SsaReg>) -> SsaReg {
        self.add_assign(Mir::Nil(self.next_reg()), Type::Nil, hirssa)
    }

    fn new_as_float_operand(&mut self, src: MirOperand, hirssa: Option<SsaReg>) -> SsaReg {
        let ret = self.next_reg();
        self.add_assign(Mir::CastIntFloat(MirUnop { ret, src }), Type::Float, hirssa)
    }

    fn new_as_float(&mut self, src: SsaReg, hirssa: Option<SsaReg>) -> SsaReg {
        let ret = self.next_reg();
        self.add_assign(
            Mir::CastIntFloat(MirUnop {
                ret,
                src: MirOperand::Reg(src),
            }),
            Type::Float,
            hirssa,
        )
    }

    fn new_ineg(&mut self, src: SsaReg, hirssa: SsaReg) -> SsaReg {
        let ret = self.next_reg();
        self.add_assign(
            Mir::INeg(MirUnop {
                ret,
                src: MirOperand::Reg(src),
            }),
            Type::Integer,
            Some(hirssa),
        )
    }

    fn new_fneg(&mut self, src: SsaReg, hirssa: SsaReg) -> SsaReg {
        let ret = self.next_reg();
        self.add_assign(
            Mir::FNeg(MirUnop {
                ret,
                src: MirOperand::Reg(src),
            }),
            Type::Float,
            Some(hirssa),
        )
    }

    fn new_iadd(&mut self, lhs: MirOperand, rhs: MirOperand, hirssa: SsaReg) -> SsaReg {
        let ret = self.next_reg();
        self.add_assign(
            Mir::IAdd(MirBinop2 { ret, lhs, rhs }),
            Type::Integer,
            Some(hirssa),
        )
    }

    fn new_fadd(&mut self, lhs: MirOperand, rhs: MirOperand, hirssa: SsaReg) -> SsaReg {
        let ret = self.next_reg();
        self.add_assign(
            Mir::FAdd(MirBinop2 { ret, lhs, rhs }),
            Type::Float,
            Some(hirssa),
        )
    }

    fn new_isub(&mut self, lhs: MirOperand, rhs: MirOperand, hirssa: SsaReg) -> SsaReg {
        let ret = self.next_reg();
        self.add_assign(
            Mir::ISub(MirBinop2 { ret, lhs, rhs }),
            Type::Integer,
            Some(hirssa),
        )
    }

    fn new_fsub(&mut self, lhs: MirOperand, rhs: MirOperand, hirssa: SsaReg) -> SsaReg {
        let ret = self.next_reg();
        self.add_assign(
            Mir::FSub(MirBinop2 { ret, lhs, rhs }),
            Type::Float,
            Some(hirssa),
        )
    }

    fn operand_to_ssa_i(&mut self, op: &MirOperand) -> SsaReg {
        match op {
            MirOperand::Reg(reg) => *reg,
            MirOperand::Const(v) => self.new_integer(v.as_i(), None),
        }
    }

    fn operand_to_ssa_f(&mut self, op: &MirOperand) -> SsaReg {
        match op {
            MirOperand::Reg(reg) => *reg,
            MirOperand::Const(v) => self.new_float(v.as_f(), None),
        }
    }

    fn new_imul(&mut self, lhs: MirOperand, rhs: MirOperand, hirssa: SsaReg) -> SsaReg {
        let lhs = self.operand_to_ssa_i(&lhs);
        let rhs = self.operand_to_ssa_i(&rhs);
        let ret = self.next_reg();
        self.add_assign(
            Mir::IMul(MirBinop { ret, lhs, rhs }),
            Type::Integer,
            Some(hirssa),
        )
    }

    fn new_fmul(&mut self, lhs: MirOperand, rhs: MirOperand, hirssa: SsaReg) -> SsaReg {
        let ret = self.next_reg();
        self.add_assign(
            Mir::FMul(MirBinop2 { ret, lhs, rhs }),
            Type::Float,
            Some(hirssa),
        )
    }

    fn new_idiv(&mut self, lhs: MirOperand, rhs: MirOperand, hirssa: SsaReg) -> SsaReg {
        let lhs = self.operand_to_ssa_i(&lhs);
        let rhs = self.operand_to_ssa_i(&rhs);
        let ret = self.next_reg();
        self.add_assign(
            Mir::IDiv(MirBinop { ret, lhs, rhs }),
            Type::Integer,
            Some(hirssa),
        )
    }

    fn new_fdiv(&mut self, lhs: MirOperand, rhs: MirOperand, hirssa: SsaReg) -> SsaReg {
        let ret = self.next_reg();
        self.add_assign(
            Mir::FDiv(MirBinop2 { ret, lhs, rhs }),
            Type::Float,
            Some(hirssa),
        )
    }

    fn new_icmp(
        &mut self,
        kind: CmpKind,
        lhs: MirOperand,
        rhs: MirOperand,
        hirssa: SsaReg,
    ) -> SsaReg {
        let ret = self.next_reg();
        self.add_assign(
            Mir::ICmp(kind, MirBinop2 { ret, lhs, rhs }),
            Type::Bool,
            Some(hirssa),
        )
    }

    fn new_fcmp(&mut self, kind: CmpKind, lhs: SsaReg, rhs: SsaReg, hirssa: SsaReg) -> SsaReg {
        let ret = self.next_reg();
        self.add_assign(
            Mir::FCmp(kind, MirBinop { ret, lhs, rhs }),
            Type::Bool,
            Some(hirssa),
        )
    }

    fn new_rec_call(&mut self, args: Vec<MirOperand>, ty: Type, hirssa: SsaReg) -> SsaReg {
        let ret = self.next_reg();
        let id = self.id;
        self.add_assign(Mir::Call(id, Some(ret), args), ty, Some(hirssa))
    }

    fn new_rec_call_nouse(&mut self, args: Vec<MirOperand>) {
        let id = self.id;
        let hir = Mir::Call(id, None, args);
        self.insts.push(hir);
    }

    fn new_ret(&mut self, lhs: MirOperand) {
        let hir = Mir::Ret(lhs);
        self.insts.push(hir);
    }

    fn new_local_store(&mut self, index: usize, rhs: MirOperand, hirssa: SsaReg) -> Result<SsaReg> {
        let ty = self.get_operand_ty(&rhs);
        if let Some(ret_ty) = self.locals[index] {
            if ret_ty != ty {
                return Err(MirErr::TypeMismatch("local_store".to_string(), ret_ty, ty));
            }
        } else {
            self.locals[index] = Some(ty);
        }
        let ret = self.next_reg();
        self.add_assign(
            Mir::LocalStore(Some(ret), (index, ty), rhs),
            ty,
            Some(hirssa),
        );
        Ok(ret)
    }

    fn new_local_store_nouse(&mut self, index: usize, rhs: MirOperand) -> Result<()> {
        let ty = self.get_operand_ty(&rhs);
        if let Some(ret_ty) = self.locals[index] {
            if ret_ty != ty {
                return Err(MirErr::TypeMismatch("local_store".to_string(), ret_ty, ty));
            }
        } else {
            self.locals[index] = Some(ty);
        }
        let hir = Mir::LocalStore(None, (index, ty), rhs);
        self.insts.push(hir);
        Ok(())
    }

    fn new_local_load(&mut self, index: usize, hirssa: SsaReg) -> Result<SsaReg> {
        let ty = self.locals[index].unwrap();
        let hir = Mir::LocalLoad((index, ty), self.next_reg());
        Ok(self.add_assign(hir, ty, Some(hirssa)))
    }

    fn new_phi(&mut self, phi: Vec<(MirBBId, SsaReg, Type)>, hirssa: SsaReg) -> SsaReg {
        let ty = phi[0].2;
        assert!(phi.iter().all(|(_, _, ty)| ty == ty));
        let ret = self.next_reg();
        self.add_assign(Mir::Phi(ret, phi), ty, Some(hirssa))
    }

    fn gen_reg(&self, reg: SsaReg) -> (SsaReg, Type) {
        let reg = *self.hir_mir_map.get(&reg).unwrap();
        let ty = self[reg].ty;
        (reg, ty)
    }

    fn gen_operand(&self, op: &HirOperand) -> (MirOperand, Type) {
        match op {
            HirOperand::Reg(rhs) => {
                let reg = *self.hir_mir_map.get(rhs).unwrap();
                (MirOperand::Reg(reg), self[reg].ty)
            }
            HirOperand::Const(rhs) => (MirOperand::Const(rhs.clone()), rhs.ty()),
        }
    }

    pub fn get_operand_ty(&self, op: &MirOperand) -> Type {
        match op {
            MirOperand::Reg(reg) => self[*reg].ty,
            MirOperand::Const(val) => val.ty(),
        }
    }
}

#[derive(Clone, PartialEq)]
pub struct MirBasicBlock {
    /// HIR instructions.
    pub insts: Vec<Mir>,
    /// The function this bb is owned.
    pub owner_function: MirFuncId,
}

impl MirBasicBlock {
    fn new(owner_function: MirFuncId) -> Self {
        Self {
            insts: vec![],
            owner_function,
        }
    }
}

#[derive(Debug, Clone)]
pub enum MirErr {
    UndefinedMethod(String),
    TypeMismatch(String, Type, Type),
    IncompatibleOperands(String, Type, Type),
    FuncCall,
}

macro_rules! binary_ops {
    ($self:ident, $binop:ident, $i_op:ident, $f_op:ident, $name:expr) => {{
        let (lhs, lhs_ty) = $self.gen_operand(&$binop.lhs);
        let (rhs, rhs_ty) = $self.gen_operand(&$binop.rhs);
        match (lhs_ty, rhs_ty) {
            (Type::Float, Type::Float) => {
                $self.$f_op(lhs, rhs, $binop.ret);
            }
            (Type::Integer, Type::Float) => {
                let lhs = $self.new_as_float_operand(lhs, None);
                $self.$f_op(MirOperand::Reg(lhs), rhs, $binop.ret);
            }
            (Type::Float, Type::Integer) => {
                let rhs = $self.new_as_float_operand(rhs, None);
                $self.$f_op(lhs, MirOperand::Reg(rhs), $binop.ret);
            }
            (Type::Integer, Type::Integer) => {
                $self.$i_op(lhs, rhs, $binop.ret);
            }
            _ => return Err(MirErr::TypeMismatch($name.to_string(), lhs_ty, rhs_ty)),
        }
    }};
}

impl MirContext {
    fn gen_func_from_hir(&mut self, hir: &HirFunction) -> Result<()> {
        for hirbb in &hir.basic_block {
            let bbi = self.new_bb();
            self.cur_bb = bbi;
            for inst in &hirbb.insts {
                match inst {
                    Hir::Br(dest) => {
                        self.insts.push(Mir::Br(MirBBId::from(*dest)));
                    }
                    Hir::CondBr(hirreg, then_bb, else_bb) => {
                        let (mir_ret, _) = self.gen_reg(*hirreg);
                        self.insts.push(Mir::CondBr(
                            mir_ret,
                            MirBBId::from(*then_bb),
                            MirBBId::from(*else_bb),
                        ));
                    }
                    Hir::CmpBr(kind, lhs, rhs, then_bb, else_bb) => {
                        let (lhs, lhs_ty) = self.gen_reg(*lhs);
                        let (rhs, rhs_ty) = self.gen_operand(rhs);
                        let mir = match (lhs_ty, rhs_ty) {
                            (Type::Float, Type::Float) => {
                                let rhs = match rhs {
                                    MirOperand::Const(v) => self.new_float(v.as_f(), None),
                                    MirOperand::Reg(reg) => reg,
                                };
                                Mir::FCmpBr(
                                    *kind,
                                    lhs,
                                    rhs,
                                    MirBBId::from(*then_bb),
                                    MirBBId::from(*else_bb),
                                )
                            }
                            (Type::Float, Type::Integer) => {
                                let rhs = self.new_as_float_operand(rhs, None);
                                Mir::FCmpBr(
                                    *kind,
                                    lhs,
                                    rhs,
                                    MirBBId::from(*then_bb),
                                    MirBBId::from(*else_bb),
                                )
                            }
                            (Type::Integer, Type::Float) => {
                                let lhs = self.new_as_float(lhs, None);
                                let rhs = match rhs {
                                    MirOperand::Const(v) => self.new_float(v.as_f(), None),
                                    MirOperand::Reg(reg) => reg,
                                };
                                Mir::FCmpBr(
                                    *kind,
                                    lhs,
                                    rhs,
                                    MirBBId::from(*then_bb),
                                    MirBBId::from(*else_bb),
                                )
                            }
                            (Type::Integer, Type::Integer) => Mir::ICmpBr(
                                *kind,
                                lhs,
                                rhs,
                                MirBBId::from(*then_bb),
                                MirBBId::from(*else_bb),
                            ),
                            _ => unimplemented!(),
                        };
                        self.insts.push(mir);
                    }
                    Hir::Phi(ret, phi) => {
                        let phi = phi
                            .iter()
                            .map(|(hirbb, hirreg)| {
                                let bb = MirBBId::from(*hirbb);
                                let (reg, ty) = self.gen_reg(*hirreg);
                                (bb, reg, ty)
                            })
                            .collect();
                        self.new_phi(phi, *ret);
                    }
                    Hir::Integer(hreg, i) => {
                        self.new_integer(*i, Some(*hreg));
                    }
                    Hir::Float(hreg, f) => {
                        self.new_float(*f, Some(*hreg));
                    }
                    Hir::Nil(hreg) => {
                        self.new_nil(Some(*hreg));
                    }
                    Hir::Neg(op) => {
                        match &op.src {
                            HirOperand::Const(val) => match val.unpack() {
                                RV::Integer(i) => {
                                    self.new_integer(i, Some(op.ret));
                                }
                                RV::Float(f) => {
                                    self.new_float(f, Some(op.ret));
                                }
                                v => {
                                    return Err(MirErr::IncompatibleOperands(
                                        "Neg".to_string(),
                                        val.ty(),
                                        val.ty(),
                                    ))
                                }
                            },
                            HirOperand::Reg(hreg) => {
                                let mreg = *self.hir_mir_map.get(hreg).unwrap();
                                match self.func()[mreg].ty {
                                    Type::Integer => {
                                        self.new_ineg(mreg, op.ret);
                                    }
                                    Type::Float => {
                                        self.new_fneg(mreg, op.ret);
                                    }
                                    ty => {
                                        return Err(MirErr::IncompatibleOperands(
                                            "Neg".to_string(),
                                            ty,
                                            ty,
                                        ))
                                    }
                                }
                            }
                        };
                    }
                    Hir::Ret(op) => {
                        let (op, ty) = self.gen_operand(op);
                        match self.ret_ty {
                            Some(ret_ty) => {
                                if ty != ret_ty {
                                    return Err(MirErr::TypeMismatch(
                                        "Ret".to_string(),
                                        ty,
                                        ret_ty,
                                    ));
                                }
                            }
                            None => {
                                self.ret_ty = Some(ty);
                            }
                        }
                        self.ret.push(op.clone());
                        self.new_ret(op);
                    }
                    Hir::LocalStore(ret, index, rhs) => {
                        let (rhs, _) = self.gen_operand(rhs);
                        match ret {
                            Some(ret) => {
                                self.new_local_store(*index, rhs, *ret)?;
                            }
                            None => {
                                self.new_local_store_nouse(*index, rhs)?;
                            }
                        }
                    }
                    Hir::LocalLoad(index, hir_rhs) => {
                        self.new_local_load(*index, *hir_rhs)?;
                    }
                    Hir::Add(binop) => binary_ops!(self, binop, new_iadd, new_fadd, "Add"),
                    Hir::Sub(binop) => binary_ops!(self, binop, new_isub, new_fsub, "Sub"),
                    Hir::Mul(binop) => binary_ops!(self, binop, new_imul, new_fmul, "Mul"),
                    Hir::Div(binop) => binary_ops!(self, binop, new_idiv, new_fdiv, "Div"),
                    Hir::Cmp(kind, binop) => {
                        let (lhs, lhs_ty) = self.gen_operand(&binop.lhs);
                        let (rhs, rhs_ty) = self.gen_operand(&binop.rhs);
                        match (lhs_ty, rhs_ty) {
                            (Type::Float, Type::Float) => {
                                let lhs = self.operand_to_ssa_f(&lhs);
                                let rhs = self.operand_to_ssa_f(&rhs);
                                self.new_fcmp(*kind, lhs, rhs, binop.ret);
                            }
                            (Type::Integer, Type::Float) => {
                                let lhs = self.new_as_float_operand(lhs, None);
                                let rhs = self.operand_to_ssa_f(&rhs);
                                self.new_fcmp(*kind, lhs, rhs, binop.ret);
                            }
                            (Type::Float, Type::Integer) => {
                                let lhs = self.operand_to_ssa_f(&lhs);
                                let rhs = self.new_as_float_operand(rhs, None);
                                self.new_fcmp(*kind, lhs, rhs, binop.ret);
                            }
                            (Type::Integer, Type::Integer) => {
                                self.new_icmp(*kind, lhs, rhs, binop.ret);
                            }
                            _ => {
                                return Err(MirErr::TypeMismatch("Cmp".to_string(), lhs_ty, rhs_ty))
                            }
                        }
                    }
                    Hir::Call(hir_id, ret, args) => {
                        match self.func_map.get(hir_id) {
                            Some(id) if id == &self.id => {}
                            _ => return Err(MirErr::FuncCall),
                        };
                        let args = args.iter().map(|hir| self.gen_operand(hir).0).collect();
                        match ret {
                            Some(ret) => {
                                self.new_rec_call(args, Type::Integer, *ret);
                            }
                            None => {
                                self.new_rec_call_nouse(args);
                            }
                        }
                    }
                }
            }
        }

        Ok(())
    }
}
