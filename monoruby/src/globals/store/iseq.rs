use bytecodegen::{
    inst::{BrKind, DynVar, FnInitInfo},
    BinOpK, UnOpK,
};
use jitgen::trace_ir::{OpMode, TraceIr};
use ruruby_parse::CmpKind;

use super::*;
use crate::{
    bytecodegen::BcIndex, compiler::jitgen::BasicBlockId, compiler::jitgen::BasicBlockInfo,
};

#[derive(Clone, Debug)]
struct ExceptionMapEntry {
    range: std::ops::Range<BytecodePtr>, // range of capturing exception
    rescue_pc: Option<BytecodePtr>,      // rescue destination pc
    ensure_pc: Option<BytecodePtr>,      // ensure destination pc
    error_slot: Option<SlotId>,          // a slot where an error object is assigned
}

impl ExceptionMapEntry {
    fn new(
        range: std::ops::Range<BytecodePtr>,
        rescue_pc: Option<BytecodePtr>,
        ensure_pc: Option<BytecodePtr>,
        error_slot: Option<SlotId>,
    ) -> Self {
        ExceptionMapEntry {
            range,
            rescue_pc,
            ensure_pc,
            error_slot,
        }
    }
}

///
/// Information of instruction sequences.
///
#[derive(Clone)]
pub struct ISeqInfo {
    ///
    /// *FuncId* of this function.
    ///
    id: FuncId,
    ///
    /// Mother method.
    ///
    pub mother: (FuncId, usize),
    ///
    /// Name of this function.
    ///
    name: Option<IdentId>,
    ///
    /// Bytecode.
    ///
    bytecode: Option<Pin<Box<[Bytecode]>>>,
    ///
    /// Location of the function in a source code.
    ///
    pub loc: Loc,
    ///
    /// Source map.
    ///
    pub sourcemap: Vec<Loc>,
    ///
    /// Valid temp register information.
    ///
    pub(crate) sp: Vec<SlotId>,
    ///
    /// Exception handling map.
    ///
    exception_map: Vec<ExceptionMapEntry>,
    ///
    /// Information of parameters.
    ///
    pub(crate) args: ParamsInfo,
    ///
    /// Name of local variables
    ///
    pub(crate) locals: IndexMap<IdentId, bytecodegen::BcLocal>,
    ///
    /// outer local variables. (dynamic_locals, block_param)
    ///
    pub(crate) outer_locals: ExternalContext,
    ///
    /// literal values. (for GC)
    ///
    pub literals: Vec<Value>,
    ///
    /// The number of non-temporary registers.
    ///
    pub non_temp_num: u16,
    ///
    /// The number of temporary registers.
    ///
    pub temp_num: u16,
    pub lexical_context: Vec<Module>,
    pub sourceinfo: SourceInfoRef,
    is_block_style: bool,
    ///
    /// Basic block information.
    ///
    pub(crate) bb_info: BasicBlockInfo,
}

impl std::fmt::Debug for ISeqInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "RubyFuncInfo {{ id:{} name:{} method:{:?} args: {} non_temp: {} temp: {}}}",
            self.id().get(),
            self.name(),
            self.mother,
            self.args.args_names.len(),
            self.non_temp_num,
            self.temp_num
        )
    }
}

impl alloc::GC<RValue> for ISeqInfo {
    fn mark(&self, alloc: &mut alloc::Allocator<RValue>) {
        self.literals.iter().for_each(|v| v.mark(alloc));
        self.lexical_context.iter().for_each(|m| m.mark(alloc));
    }
}

impl ISeqInfo {
    fn new(
        id: FuncId,
        mother: (FuncId, usize),
        outer_locals: ExternalContext,
        name: Option<IdentId>,
        args: ParamsInfo,
        loc: Loc,
        sourceinfo: SourceInfoRef,
        is_block_style: bool,
    ) -> Self {
        ISeqInfo {
            id,
            mother,
            name,
            bytecode: None,
            loc,
            sourcemap: vec![],
            sp: vec![],
            exception_map: vec![],
            args: args.clone(),
            locals: IndexMap::default(),
            outer_locals,
            literals: vec![],
            non_temp_num: 0,
            temp_num: 0,
            lexical_context: vec![],
            sourceinfo,
            is_block_style,
            bb_info: BasicBlockInfo::default(),
        }
    }

    pub fn is_block_style(&self) -> bool {
        self.is_block_style
    }

    pub(super) fn new_block(
        id: FuncId,
        mother: (FuncId, usize),
        outer: (FuncId, ExternalContext),
        args: ParamsInfo,
        loc: Loc,
        sourceinfo: SourceInfoRef,
    ) -> Self {
        Self::new(id, mother, outer.1, None, args, loc, sourceinfo, true)
    }

    pub(super) fn new_method(
        id: FuncId,
        name: Option<IdentId>,
        args: ParamsInfo,
        loc: Loc,
        sourceinfo: SourceInfoRef,
    ) -> Self {
        Self::new(
            id,
            (id, 0),
            ExternalContext::new(),
            name,
            args,
            loc,
            sourceinfo,
            false,
        )
    }

    pub(crate) fn id(&self) -> FuncId {
        self.id
    }

    ///
    /// Set bytecode to the *ISeqInfo*.
    ///
    pub(crate) fn set_bytecode(&mut self, bc: Vec<Bytecode>) {
        self.bytecode = Some(Box::into_pin(bc.into_boxed_slice()));
    }

    ///
    /// Get a number of registers.
    ///
    pub(crate) fn total_reg_num(&self) -> usize {
        1 + (self.non_temp_num + self.temp_num) as usize
    }

    ///
    /// Get a number of non-temp registers.
    ///
    pub(crate) fn local_num(&self) -> usize {
        self.non_temp_num as usize
    }

    ///
    /// Get a number of required + optional + rest arguments.
    ///
    pub(crate) fn pos_num(&self) -> usize {
        self.args.pos_num
    }

    ///
    /// Get a block argument name.
    ///
    pub(crate) fn block_param(&self) -> Option<IdentId> {
        self.args.block_param
    }

    ///
    /// Get names of local variables.
    ///
    pub(crate) fn local_variables(&self) -> Vec<Value> {
        let mut map = IndexSet::default();
        self.locals.keys().for_each(|id| {
            map.insert(*id);
        });

        self.outer_locals.scope.iter().for_each(|(locals, block)| {
            locals.keys().for_each(|id| {
                map.insert(*id);
            });
            if let Some(id) = block {
                map.insert(*id);
            }
        });
        map.into_iter().map(Value::symbol).collect()
    }

    ///
    /// Get the name of iseq.
    ///
    pub(crate) fn name(&self) -> String {
        match &self.name {
            Some(name) => name.to_string(),
            None => "<unnamed>".to_string(),
        }
    }

    ///
    /// Get a reference of bytecode.
    ///
    pub(crate) fn bytecode(&self) -> &[Bytecode] {
        self.bytecode.as_ref().unwrap()
    }

    ///
    /// Get pc(*BytecodePtr*) for instruction index(*idx*).
    ///
    pub(crate) fn get_pc(&self, idx: BcIndex) -> BytecodePtr {
        BytecodePtr::from_bc(&self.bytecode()[idx.0 as usize])
    }

    ///
    /// Get pc(*BytecodePtr*) for the beginning of the basic block(*idx*).
    ///
    pub(crate) fn get_bb_pc(&self, idx: BasicBlockId) -> BytecodePtr {
        self.get_pc(self.bb_info[idx].begin)
    }

    ///
    /// Get pc(*BytecodePtr*) for instruction index(*idx*).
    ///
    pub(crate) fn get_top_pc(&self) -> BytecodePtr {
        BytecodePtr::from_bc(&self.bytecode()[0])
    }

    ///
    /// Get an instruction index(*usize*) corresponding to pc(*BytecodePtr*).
    ///
    pub(crate) fn get_pc_index(&self, pc: Option<BytecodePtr>) -> BcIndex {
        let i = if let Some(pos) = pc {
            pos - self.get_top_pc()
        } else {
            0
        };
        BcIndex::from(i)
    }

    pub(crate) fn get_sp(&self, i: BcIndex) -> SlotId {
        self.sp[i.0 as usize]
    }

    fn get_bb(&self, bc_pos: BcIndex) -> BasicBlockId {
        self.bb_info.is_bb_head(bc_pos).unwrap()
    }

    pub(crate) fn get_location(&self) -> String {
        let loc = self.loc;
        format!(
            "{}:{}",
            self.sourceinfo.short_file_name(),
            self.sourceinfo.get_line(&loc)
        )
    }

    ///
    /// Explore exception table for pc(*BcPc*) and return error handler's pc(*BcPc*) and the slot where an error object is to be stored.
    ///
    pub(crate) fn get_exception_dest(
        &self,
        pc: BytecodePtr,
    ) -> Option<(Option<BytecodePtr>, Option<BytecodePtr>, Option<SlotId>)> {
        self.exception_map
            .iter()
            .filter_map(|entry| {
                if entry.range.contains(&pc) {
                    Some((entry.rescue_pc, entry.ensure_pc, entry.error_slot))
                } else {
                    None
                }
            })
            .nth(0)
    }

    pub(crate) fn exception_push(
        &mut self,
        range: std::ops::Range<BytecodePtr>,
        rescue: Option<BytecodePtr>,
        ensure: Option<BytecodePtr>,
        err_reg: Option<SlotId>,
    ) {
        self.exception_map
            .push(ExceptionMapEntry::new(range, rescue, ensure, err_reg));
    }

    #[cfg(feature = "emit-bc")]
    pub(super) fn get_exception_map(
        &self,
    ) -> Vec<(
        std::ops::Range<BcIndex>,
        Option<BcIndex>,
        Option<BcIndex>,
        Option<SlotId>,
    )> {
        self.exception_map
            .iter()
            .map(|entry| {
                let start = self.get_pc_index(Some(entry.range.start));
                let end = self.get_pc_index(Some(entry.range.end));
                let rescue = entry.rescue_pc.map(|pc| self.get_pc_index(Some(pc)));
                let ensure = entry.ensure_pc.map(|pc| self.get_pc_index(Some(pc)));
                (start..end, rescue, ensure, entry.error_slot)
            })
            .collect::<Vec<_>>()
    }
}

impl ISeqInfo {
    pub(crate) fn trace_ir(&self, store: &Store, bc_pos: BcIndex) -> jitgen::trace_ir::TraceIr {
        let pc = self.get_pc(bc_pos);
        let op1 = pc.op1;
        let op2 = pc.op2;
        let opcode = pc.opcode();
        if opcode & 0xc0 == 0 {
            let (op1_w, op1_l) = dec_wl(op1);
            match opcode {
                1 => TraceIr::SingletonMethodDef {
                    obj: SlotId::new(op1_w),
                    name: IdentId::from(op2.0 as u32),
                    func_id: FuncId::new((op2.0 >> 32) as u32),
                },
                2 => TraceIr::MethodDef {
                    name: IdentId::from((op2.0) as u32),
                    func_id: FuncId::new((op2.0 >> 32) as u32),
                },
                3 => {
                    let dest = self.get_bb(bc_pos + 1 + op1_l as i32);
                    TraceIr::Br(dest)
                }
                4 => {
                    let dest = self.get_bb(bc_pos + 1 + op1_l as i32);
                    TraceIr::CondBr(SlotId::new(op1_w), dest, false, BrKind::BrIf)
                }
                5 => {
                    let dest = self.get_bb(bc_pos + 1 + op1_l as i32);
                    TraceIr::CondBr(SlotId::new(op1_w), dest, false, BrKind::BrIfNot)
                }
                6 => TraceIr::Integer(SlotId::new(op1_w), op1_l as i32),
                7 => TraceIr::Literal(SlotId::new(op1_w), op2.get_value()),
                8 => TraceIr::Nil(SlotId::new(op1_w)),
                9 => TraceIr::Symbol(SlotId::new(op1_w), IdentId::from(op1_l)),
                10 | 18 => TraceIr::LoadConst(SlotId::new(op1_w), ConstSiteId(op1_l)),
                11 => TraceIr::StoreConst(SlotId::new(op1_w), ConstSiteId(op1_l)),
                12..=13 => {
                    let dest = self.get_bb(bc_pos + 1 + op1_l as i32);
                    TraceIr::CondBr(SlotId::new(op1_w), dest, true, BrKind::from(opcode - 12))
                }
                14 => TraceIr::LoopStart {
                    counter: op1_l,
                    jit_addr: pc.into_jit_addr(),
                },
                15 => TraceIr::LoopEnd,
                16 => {
                    if let Some(class) = pc.cached_class0() {
                        let ivar = pc.cached_ivarid();
                        TraceIr::LoadIvar(SlotId::new(op1_w), IdentId::from(op1_l), class, ivar)
                    } else {
                        TraceIr::Deoptimize
                    }
                }
                17 => {
                    if let Some(class) = pc.cached_class0() {
                        let ivar = pc.cached_ivarid();
                        TraceIr::StoreIvar(SlotId::new(op1_w), IdentId::from(op1_l), class, ivar)
                    } else {
                        TraceIr::Deoptimize
                    }
                }
                20 => {
                    let dest = self.get_bb(bc_pos + 1 + op1_l as i32);
                    TraceIr::CheckLocal(SlotId::new(op1_w), dest)
                }
                21 => TraceIr::BlockArgProxy(SlotId::new(op1_w), op1_l as usize),
                22 => TraceIr::SingletonClassDef {
                    dst: SlotId::from(op1_w),
                    base: SlotId::new(op1_l as u16),
                    func_id: FuncId::new((op2.0 >> 32) as u32),
                },
                23 => TraceIr::BlockArg(SlotId::new(op1_w), op1_l as usize),
                24 => TraceIr::CheckCvar {
                    dst: SlotId::new(op1_w),
                    name: IdentId::from(op1_l),
                },
                25 => TraceIr::LoadGvar {
                    dst: SlotId::new(op1_w),
                    name: IdentId::from(op1_l),
                },
                26 => TraceIr::StoreGvar {
                    src: SlotId::new(op1_w),
                    name: IdentId::from(op1_l),
                },
                27 => TraceIr::LoadCvar {
                    dst: SlotId::new(op1_w),
                    name: IdentId::from(op1_l),
                },
                28 => TraceIr::LoadSvar {
                    dst: SlotId::new(op1_w),
                    id: op1_l,
                },
                29 => TraceIr::StoreCvar {
                    src: SlotId::new(op1_w),
                    name: IdentId::from(op1_l),
                },
                30..=31 => TraceIr::MethodCall {
                    callid: op1_l.into(),
                    recv_class: pc.cached_class1(),
                    fid: pc.cached_fid(),
                    version: (pc + 1).cached_version(),
                },
                32..=33 => TraceIr::MethodCallWithBlock {
                    callid: op1_l.into(),
                    recv_class: pc.cached_class1(),
                    fid: pc.cached_fid(),
                    version: (pc + 1).cached_version(),
                },
                34..=35 => TraceIr::Yield {
                    callid: op1_l.into(),
                },
                36 => {
                    let optid = OptCaseId::from(op1_l);
                    let OptCaseInfo {
                        min,
                        max,
                        offsets,
                        branch_table,
                    } = &store[optid];
                    let dest_bb: Box<[_]> = offsets
                        .iter()
                        .map(|ofs| self.get_bb(bc_pos + 1 + (*ofs as i32)))
                        .collect();
                    let branch_table: Box<[_]> = branch_table
                        .iter()
                        .map(|ofs| self.get_bb(bc_pos + 1 + (*ofs as i32)))
                        .collect();

                    TraceIr::OptCase {
                        cond: SlotId::new(op1_w),
                        min: *min,
                        max: *max,
                        dest_bb,
                        branch_table,
                    }
                }
                37 => {
                    let dest = self.get_bb(bc_pos + 1 + op1_l as i32);
                    TraceIr::NilBr(SlotId::new(op1_w), dest)
                }
                38 => TraceIr::Lambda {
                    dst: SlotId::new(op1_w),
                    func_id: FuncId::new(op1_l),
                },
                39 => TraceIr::Array {
                    dst: SlotId::new(op1_w),
                    callid: CallSiteId::from(op1_l),
                },
                _ => unreachable!("{:016x}", op1),
            }
        } else {
            let (op1_w1, op2_w2, op3_w3) = dec_www(op1);
            match opcode {
                64 => TraceIr::DefinedYield {
                    dst: SlotId::new(op1_w1),
                },
                65 => TraceIr::DefinedConst {
                    dst: SlotId::new(op1_w1),
                    siteid: ConstSiteId(op2.0 as u32),
                },
                66 => TraceIr::DefinedMethod {
                    dst: SlotId::new(op1_w1),
                    recv: SlotId::new(op2_w2),
                    name: IdentId::from(op2.0 as u32),
                },
                67 => TraceIr::DefinedGvar {
                    dst: SlotId::new(op1_w1),
                    name: IdentId::from(op2.0 as u32),
                },
                68 => TraceIr::DefinedIvar {
                    dst: SlotId::new(op1_w1),
                    name: IdentId::from(op2.0 as u32),
                },
                70 => TraceIr::ClassDef {
                    dst: SlotId::from(op1_w1),
                    base: SlotId::from(op2_w2),
                    superclass: SlotId::from(op3_w3),
                    name: IdentId::from((op2.0) as u32),
                    func_id: FuncId::new((op2.0 >> 32) as u32),
                },
                71 => TraceIr::ModuleDef {
                    dst: SlotId::from(op1_w1),
                    base: SlotId::from(op2_w2),
                    name: IdentId::from((op2.0) as u32),
                    func_id: FuncId::new((op2.0 >> 32) as u32),
                },
                80 => TraceIr::Ret(SlotId::new(op1_w1)),
                81 => TraceIr::MethodRet(SlotId::new(op1_w1)),
                82 => TraceIr::BlockBreak(SlotId::new(op1_w1)),
                83 => TraceIr::Raise(SlotId::new(op1_w1)),
                85 => TraceIr::EnsureEnd,
                86 => TraceIr::ConcatRegexp(SlotId::from(op1_w1), SlotId::new(op2_w2), op3_w3),
                126 => {
                    let kind = UnOpK::Pos;
                    let dst = SlotId::new(op1_w1);
                    let src = SlotId::new(op2_w2);
                    if pc.is_integer1() {
                        TraceIr::IUnOp { kind, dst, src }
                    } else if pc.is_float1() {
                        TraceIr::FUnOp { kind, dst, src }
                    } else if let Some(src_class) = pc.classid1() {
                        TraceIr::UnOp {
                            kind,
                            dst,
                            src,
                            src_class,
                        }
                    } else {
                        TraceIr::Deoptimize
                    }
                }
                127 => {
                    if let Some(src_class) = pc.classid1() {
                        TraceIr::BitNot {
                            dst: SlotId::new(op1_w1),
                            src: SlotId::new(op2_w2),
                            src_class,
                        }
                    } else {
                        TraceIr::Deoptimize
                    }
                }
                128 => TraceIr::Not {
                    dst: SlotId::new(op1_w1),
                    src: SlotId::new(op2_w2),
                },
                129 => {
                    let kind = UnOpK::Neg;
                    let dst = SlotId::new(op1_w1);
                    let src = SlotId::new(op2_w2);
                    if pc.is_integer1() {
                        TraceIr::IUnOp { kind, dst, src }
                    } else if pc.is_float1() {
                        TraceIr::FUnOp { kind, dst, src }
                    } else if let Some(src_class) = pc.classid1() {
                        TraceIr::UnOp {
                            kind,
                            dst,
                            src,
                            src_class,
                        }
                    } else {
                        TraceIr::Deoptimize
                    }
                }
                130 => TraceIr::InlineCache,
                132 => {
                    if let Some(base_class) = pc.classid1()
                        && let Some(idx_class) = pc.classid2()
                    {
                        TraceIr::Index {
                            dst: SlotId::new(op1_w1),
                            base: SlotId::new(op2_w2),
                            idx: SlotId::new(op3_w3),
                            base_class,
                            idx_class,
                        }
                    } else {
                        TraceIr::Deoptimize
                    }
                }
                133 => {
                    if let Some(base_class) = pc.classid1()
                        && let Some(idx_class) = pc.classid2()
                    {
                        TraceIr::IndexAssign {
                            src: SlotId::new(op1_w1),
                            base: SlotId::new(op2_w2),
                            idx: SlotId::new(op3_w3),
                            base_class,
                            idx_class,
                        }
                    } else {
                        TraceIr::Deoptimize
                    }
                }
                134..=141 => {
                    let kind = CmpKind::from(opcode - 134);
                    let dst = SlotId::from(op1_w1);
                    let mode = OpMode::RR(SlotId::new(op2_w2), SlotId::new(op3_w3));
                    let lhs_class = pc.classid1();
                    let rhs_class = pc.classid2();
                    if pc.is_float_binop() {
                        TraceIr::FCmp {
                            kind,
                            dst,
                            mode,
                            lhs_class: lhs_class.unwrap(),
                            rhs_class: rhs_class.unwrap(),
                        }
                    } else if pc.is_integer_binop() {
                        TraceIr::ICmp { kind, dst, mode }
                    } else if let Some(lhs_class) = lhs_class
                        && let Some(rhs_class) = rhs_class
                    {
                        TraceIr::Cmp {
                            kind,
                            dst,
                            mode,
                            lhs_class,
                            rhs_class,
                        }
                    } else {
                        TraceIr::Deoptimize
                    }
                }
                142..=149 => {
                    let kind = CmpKind::from(opcode - 142);
                    let dst = SlotId::from(op1_w1);
                    let mode = OpMode::RI(SlotId::new(op2_w2), op3_w3 as i16);
                    let lhs_class = pc.classid1();
                    let rhs_class = Some(INTEGER_CLASS);
                    if pc.is_float1() {
                        TraceIr::FCmp {
                            kind,
                            dst,
                            mode,
                            lhs_class: lhs_class.unwrap(),
                            rhs_class: rhs_class.unwrap(),
                        }
                    } else if pc.is_integer1() {
                        TraceIr::ICmp { kind, dst, mode }
                    } else if let Some(lhs_class) = lhs_class
                        && let Some(rhs_class) = rhs_class
                    {
                        TraceIr::Cmp {
                            kind,
                            dst,
                            mode,
                            lhs_class,
                            rhs_class,
                        }
                    } else {
                        TraceIr::Deoptimize
                    }
                }
                150 => TraceIr::LoadDynVar(
                    SlotId::new(op1_w1),
                    DynVar {
                        reg: SlotId::new(op2_w2),
                        outer: op3_w3 as usize,
                    },
                ),
                151 => TraceIr::StoreDynVar(
                    DynVar {
                        reg: SlotId::new(op1_w1),
                        outer: op2_w2 as usize,
                    },
                    SlotId::new(op3_w3),
                ),
                154..=161 => {
                    let kind = CmpKind::from(opcode - 154);
                    let dst = SlotId::from(op1_w1);
                    let mode = OpMode::RR(SlotId(op2_w2), SlotId(op3_w3));
                    let lhs_class = pc.classid1();
                    let rhs_class = pc.classid2();
                    let (dest, brkind) = match self.trace_ir(store, bc_pos + 1) {
                        TraceIr::CondBr(_, dest, true, brkind) => (dest, brkind),
                        _ => unreachable!(),
                    };
                    if pc.is_float_binop() {
                        TraceIr::FCmpBr {
                            kind,
                            dst,
                            mode,
                            lhs_class: lhs_class.unwrap(),
                            rhs_class: rhs_class.unwrap(),
                            dest,
                            brkind,
                        }
                    } else if pc.is_integer_binop() {
                        TraceIr::ICmpBr {
                            kind,
                            dst,
                            mode,
                            dest,
                            brkind,
                        }
                    } else if let Some(lhs_class) = lhs_class
                        && let Some(rhs_class) = rhs_class
                    {
                        TraceIr::CmpBr {
                            kind,
                            dst,
                            mode,
                            lhs_class,
                            rhs_class,
                            dest,
                            brkind,
                        }
                    } else {
                        TraceIr::Deoptimize
                    }
                }
                162..=169 => {
                    let kind = CmpKind::from(opcode - 162);
                    let dst = SlotId::from(op1_w1);
                    let mode = OpMode::RI(SlotId::new(op2_w2), op3_w3 as i16);
                    let lhs_class = pc.classid1();
                    let rhs_class = Some(INTEGER_CLASS);
                    let (dest, brkind) = match self.trace_ir(store, bc_pos + 1) {
                        TraceIr::CondBr(_, dest, true, brkind) => (dest, brkind),
                        _ => unreachable!(),
                    };
                    if pc.is_float1() {
                        TraceIr::FCmpBr {
                            kind,
                            dst,
                            mode,
                            lhs_class: lhs_class.unwrap(),
                            rhs_class: rhs_class.unwrap(),
                            dest,
                            brkind,
                        }
                    } else if pc.is_integer1() {
                        TraceIr::ICmpBr {
                            kind,
                            dst,
                            mode,
                            dest,
                            brkind,
                        }
                    } else if let Some(lhs_class) = lhs_class
                        && let Some(rhs_class) = rhs_class
                    {
                        TraceIr::CmpBr {
                            kind,
                            dst,
                            mode,
                            lhs_class,
                            rhs_class,
                            dest,
                            brkind,
                        }
                    } else {
                        TraceIr::Deoptimize
                    }
                }
                170 => TraceIr::InitMethod(FnInitInfo {
                    reg_num: op1_w1 as usize,
                    arg_num: op2_w2 as usize,
                    stack_offset: op3_w3 as usize,
                }),
                171 => TraceIr::ExpandArray {
                    src: SlotId::new(op1_w1),
                    dst: (SlotId::new(op2_w2), op3_w3),
                },
                173 => {
                    let (new, old) = op2.get_ident2();
                    TraceIr::AliasMethod { new, old }
                }
                174 => TraceIr::Hash {
                    dst: SlotId::new(op1_w1),
                    args: SlotId::new(op2_w2),
                    len: op3_w3,
                },
                176 => TraceIr::Mov(SlotId::new(op1_w1), SlotId::new(op2_w2)),
                177..=178 => TraceIr::Range {
                    dst: SlotId::new(op1_w1),
                    start: SlotId::new(op2_w2),
                    end: SlotId::new(op3_w3),
                    exclude_end: match opcode - 177 {
                        0 => false,
                        1 => true,
                        _ => unreachable!(),
                    },
                },
                179 => TraceIr::ConcatStr(SlotId::from(op1_w1), SlotId::new(op2_w2), op3_w3),
                180..=189 => {
                    let kind = BinOpK::from(opcode - 180);
                    let dst = SlotId::from(op1_w1);
                    let mode = OpMode::IR(op2_w2 as i16, SlotId::new(op3_w3));
                    let lhs_class = Some(INTEGER_CLASS);
                    let rhs_class = pc.classid2();
                    if pc.is_integer2() {
                        TraceIr::IBinOp { kind, dst, mode }
                    } else if pc.is_float2() {
                        TraceIr::FBinOp {
                            kind,
                            dst,
                            mode,
                            lhs_class: lhs_class.unwrap(),
                            rhs_class: rhs_class.unwrap(),
                        }
                    } else if let Some(lhs_class) = lhs_class
                        && let Some(rhs_class) = rhs_class
                    {
                        TraceIr::BinOp {
                            kind,
                            dst,
                            mode,
                            lhs_class,
                            rhs_class,
                        }
                    } else {
                        TraceIr::Deoptimize
                    }
                }
                190..=199 => {
                    let kind = BinOpK::from(opcode - 190);
                    let dst = SlotId::from(op1_w1);
                    let mode = OpMode::RI(SlotId::new(op2_w2), op3_w3 as i16);
                    let lhs_class = pc.classid1();
                    let rhs_class = Some(INTEGER_CLASS);
                    if pc.is_integer1() {
                        TraceIr::IBinOp { kind, dst, mode }
                    } else if pc.is_float1() {
                        TraceIr::FBinOp {
                            kind,
                            dst,
                            mode,
                            lhs_class: lhs_class.unwrap(),
                            rhs_class: rhs_class.unwrap(),
                        }
                    } else if let Some(lhs_class) = lhs_class
                        && let Some(rhs_class) = rhs_class
                    {
                        TraceIr::BinOp {
                            kind,
                            dst,
                            mode,
                            lhs_class,
                            rhs_class,
                        }
                    } else {
                        TraceIr::Deoptimize
                    }
                }
                200..=209 => {
                    let kind = BinOpK::from(opcode - 200);
                    let dst = SlotId::from(op1_w1);
                    let mode = OpMode::RR(SlotId::new(op2_w2), SlotId::new(op3_w3));
                    let lhs_class = pc.classid1();
                    let rhs_class = pc.classid2();
                    if pc.is_integer_binop() {
                        TraceIr::IBinOp { kind, dst, mode }
                    } else if pc.is_float_binop() {
                        TraceIr::FBinOp {
                            kind,
                            dst,
                            mode,
                            lhs_class: lhs_class.unwrap(),
                            rhs_class: rhs_class.unwrap(),
                        }
                    } else if let Some(lhs_class) = lhs_class
                        && let Some(rhs_class) = rhs_class
                    {
                        TraceIr::BinOp {
                            kind,
                            dst,
                            mode,
                            lhs_class,
                            rhs_class,
                        }
                    } else {
                        TraceIr::Deoptimize
                    }
                }
                _ => unreachable!("{:016x}", op1),
            }
        }
    }
}

fn dec_wl(op: u64) -> (u16, u32) {
    ((op >> 32) as u16, op as u32)
}

fn dec_www(op: u64) -> (u16, u16, u16) {
    ((op >> 32) as u16, (op >> 16) as u16, op as u16)
}

///
/// Parameters information in *ISeqInfo*.
///
#[derive(Debug, Clone, Default, PartialEq)]
pub(crate) struct ParamsInfo {
    required_num: usize,
    // required + optional
    reqopt_num: usize,
    // required + optional + rest
    pos_num: usize,
    // for param, req(incl. destruct slot), opt, rest, keyword, kw_rest, destructed local, block
    pub args_names: Vec<Option<IdentId>>,
    pub kw_names: Vec<IdentId>,
    pub kw_rest: Option<SlotId>,
    block_param: Option<IdentId>,
}

impl ParamsInfo {
    pub fn new(
        required_num: usize,
        reqopt_num: usize,
        pos_num: usize,
        args_names: Vec<Option<IdentId>>,
        keyword_names: Vec<IdentId>,
        kw_rest: Option<SlotId>,
        block_param: Option<IdentId>,
    ) -> Self {
        ParamsInfo {
            required_num,
            reqopt_num,
            pos_num,
            args_names,
            kw_names: keyword_names,
            kw_rest,
            block_param,
        }
    }

    pub fn new_attr_reader() -> Self {
        ParamsInfo::default()
    }

    pub fn new_attr_writer() -> Self {
        ParamsInfo {
            required_num: 1,
            reqopt_num: 1,
            pos_num: 1,
            args_names: vec![],
            kw_names: vec![],
            kw_rest: None,
            block_param: None,
        }
    }

    pub fn new_native(min: usize, max: usize, rest: bool, kw_names: Vec<IdentId>) -> Self {
        ParamsInfo {
            required_num: min,
            reqopt_num: max,
            pos_num: max + rest as usize,
            args_names: vec![],
            kw_names,
            kw_rest: None,
            block_param: None,
        }
    }

    ///
    /// The number of required arguments.
    ///
    pub(crate) fn req_num(&self) -> usize {
        self.required_num
    }

    ///
    /// The number of required + optional arguments.
    ///
    pub(crate) fn reqopt_num(&self) -> usize {
        self.reqopt_num
    }

    ///
    /// The number of optional + rest arguments.
    ///
    pub(crate) fn opt_rest_num(&self) -> usize {
        self.pos_num - self.required_num
    }

    ///
    /// The number of required + optional + rest arguments.
    ///
    pub fn pos_num(&self) -> usize {
        self.pos_num
    }

    pub fn max_positional_args(&self) -> usize {
        self.reqopt_num
    }

    pub fn total_args(&self) -> usize {
        self.pos_num
            + self.kw_names.len()
            + self.kw_rest.is_some() as usize
            + self.block_param.is_some() as usize
    }

    pub fn is_rest(&self) -> bool {
        self.pos_num != self.reqopt_num
    }

    ///
    /// If `self` is "simple", return true.
    ///
    /// "simple" means that the function has no optional, rest, keyword, keywoed rest, and block parameters.
    ///
    pub fn is_simple(&self) -> bool {
        self.opt_rest_num() == 0
            && self.kw_names.is_empty()
            && self.kw_rest.is_none()
            && self.block_param.is_none()
    }
}
