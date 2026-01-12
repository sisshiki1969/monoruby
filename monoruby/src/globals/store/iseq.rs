use bytecodegen::{
    BinOpK, UnOpK,
    inst::{BrKind, DynVar, FnInitInfo},
};
use jitgen::trace_ir::TraceIr;
use ruruby_parse::CmpKind;

use super::*;
use crate::{
    bytecodegen::BcIndex,
    codegen::jitgen::{BasicBlockId, BasicBlockInfo},
};

#[derive(Clone, Debug)]
struct ExceptionMapEntry {
    range: std::ops::Range<BcIndex>, // range of capturing exception
    rescue_pc: Option<BcIndex>,      // rescue destination pc
    ensure_pc: Option<BcIndex>,      // ensure destination pc
    error_slot: Option<SlotId>,      // a slot where an error object is assigned
}

impl ExceptionMapEntry {
    fn new(
        range: std::ops::Range<BcIndex>,
        rescue_pc: Option<BcIndex>,
        ensure_pc: Option<BcIndex>,
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
/// ID of ISEQ.
///
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct ISeqId(usize);

impl From<ISeqId> for usize {
    fn from(id: ISeqId) -> usize {
        id.0
    }
}

impl ISeqId {
    pub const fn new(id: usize) -> Self {
        Self(id)
    }

    pub fn get(&self) -> usize {
        self.0
    }
}

#[derive(Debug, Clone)]
pub struct JitInfo {
    pub entry: DestLabel,
    pub class_version_label: DestLabel,
    pub inline_cache_map: Vec<(ClassId, Option<IdentId>, FuncId)>,
}

///
/// Information of instruction sequences.
///
#[derive(Clone)]
pub struct ISeqInfo {
    ///
    /// *FuncId* of this function.
    ///
    func_id: FuncId,
    ///
    /// Mother method.
    ///
    mother: (ISeqId, usize),
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
    pub(crate) locals: indexmap::IndexMap<IdentId, bytecodegen::BcLocal>,
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
    ///
    /// Lexical module stack of this method.
    ///
    pub lexical_context: Vec<ClassId>,
    ///
    /// Source code information.
    ///
    pub sourceinfo: SourceInfoRef,
    is_constant_fn: Option<Value>,
    ///
    /// JIT code info for each class of *self*.
    ///
    pub(super) jit_entry: HashMap<ClassId, JitInfo>,
    pub(super) jit_invalidated: bool,
    ///
    /// Basic block information.
    ///
    pub(crate) bb_info: BasicBlockInfo,
    ///
    /// Map for BcIndex to CallsiteId.
    ///
    pub(super) callsite_map: HashMap<BcIndex, CallSiteId>,
}

impl std::fmt::Debug for ISeqInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "RubyFuncInfo {{ id:{} name:{} method:{:?} args: {} non_temp: {} temp: {}}}",
            self.func_id().get(),
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
    }
}

impl ISeqInfo {
    fn new(
        id: FuncId,
        mother: (ISeqId, usize),
        outer_locals: ExternalContext,
        name: Option<IdentId>,
        args: ParamsInfo,
        loc: Loc,
        sourceinfo: SourceInfoRef,
    ) -> Self {
        ISeqInfo {
            func_id: id,
            mother,
            name,
            bytecode: None,
            loc,
            sourcemap: vec![],
            sp: vec![],
            exception_map: vec![],
            args,
            locals: Default::default(),
            outer_locals,
            literals: vec![],
            non_temp_num: 0,
            temp_num: 0,
            lexical_context: vec![],
            sourceinfo,
            is_constant_fn: None,
            jit_entry: HashMap::default(),
            jit_invalidated: false,
            bb_info: BasicBlockInfo::default(),
            callsite_map: HashMap::default(),
        }
    }

    pub fn is_const_fn(&self) -> Option<Value> {
        self.is_constant_fn
    }

    pub fn set_const_fn(&mut self, v: Value) {
        self.is_constant_fn = Some(v);
    }

    pub(super) fn new_block(
        id: FuncId,
        mother: (ISeqId, usize),
        outer: (FuncId, ExternalContext),
        args: ParamsInfo,
        loc: Loc,
        sourceinfo: SourceInfoRef,
    ) -> Self {
        Self::new(id, mother, outer.1, None, args, loc, sourceinfo)
    }

    pub(super) fn new_method(
        id: FuncId,
        iseq_id: ISeqId,
        name: Option<IdentId>,
        args: ParamsInfo,
        loc: Loc,
        sourceinfo: SourceInfoRef,
    ) -> Self {
        Self::new(
            id,
            (iseq_id, 0),
            ExternalContext::new(),
            name,
            args,
            loc,
            sourceinfo,
        )
    }

    pub(crate) fn func_id(&self) -> FuncId {
        self.func_id
    }

    pub(crate) fn mother(&self) -> (ISeqId, usize) {
        self.mother
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
    /// Get a number of non-temp registers. (not includes `self`)
    ///
    pub(crate) fn local_num(&self) -> usize {
        self.non_temp_num as usize
    }

    pub(crate) fn stack_offset(&self) -> usize {
        let reg_num = self.total_reg_num() - 1;
        (reg_num * 8 + (RSP_LOCAL_FRAME + LFP_ARG0) as usize + 31) / 16 * 16 + 16
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
        let mut map = indexmap::IndexSet::<IdentId>::default();
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
    fn bytecode(&self) -> &[Bytecode] {
        self.bytecode.as_ref().unwrap()
    }

    ///
    /// Get length of bytecode.
    ///
    #[cfg(feature = "emit-bc")]
    pub(crate) fn bytecode_len(&self) -> usize {
        self.bytecode.as_ref().unwrap().len()
    }

    ///
    /// Get pc(*BytecodePtr*) for instruction index(*idx*).
    ///
    pub(crate) fn get_pc(&self, idx: BcIndex) -> BytecodePtr {
        BytecodePtr::from_bc(&self.bytecode()[idx.to_usize()])
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
    pub(crate) fn get_top_pc(&self) -> BytecodePtrBase {
        BytecodePtrBase::from_bc(&self.bytecode()[0])
    }

    ///
    /// Get an instruction index(*usize*) corresponding to pc(*BytecodePtr*).
    ///
    pub(crate) fn get_pc_index(&self, pc: Option<BytecodePtr>) -> BcIndex {
        if let Some(pos) = pc {
            pos - self.get_top_pc()
        } else {
            BcIndex::from(0)
        }
    }

    pub(crate) fn get_sp(&self, i: BcIndex) -> SlotId {
        self.sp[i.to_usize()]
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
        pc: BcIndex,
    ) -> Option<(Option<BcIndex>, Option<BcIndex>, Option<SlotId>)> {
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
        range: std::ops::Range<BcIndex>,
        rescue: Option<BcIndex>,
        ensure: Option<BcIndex>,
        err_reg: Option<SlotId>,
    ) {
        self.exception_map
            .push(ExceptionMapEntry::new(range, rescue, ensure, err_reg));
    }

    pub(crate) fn no_ensure(&self) -> bool {
        self.exception_map.iter().all(|map| map.ensure_pc.is_none())
    }

    #[cfg(feature = "emit-bc")]
    pub(crate) fn get_exception_map(
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
                let start = entry.range.start;
                let end = entry.range.end;
                let rescue = entry.rescue_pc;
                let ensure = entry.ensure_pc;
                (start..end, rescue, ensure, entry.error_slot)
            })
            .collect::<Vec<_>>()
    }

    pub(crate) fn add_jit_code(
        &mut self,
        self_class: ClassId,
        entry: DestLabel,
        class_version_label: DestLabel,
    ) -> Option<JitInfo> {
        self.jit_entry.insert(
            self_class,
            JitInfo {
                entry,
                class_version_label,
                inline_cache_map: Vec::new(),
            },
        )
    }

    pub(crate) fn get_cache_map(
        &self,
        self_class: ClassId,
    ) -> &Vec<(ClassId, Option<IdentId>, FuncId)> {
        &self.jit_entry.get(&self_class).unwrap().inline_cache_map
    }

    pub(crate) fn set_cache_map(
        &mut self,
        self_class: ClassId,
        cache: Vec<(ClassId, Option<IdentId>, FuncId)>,
    ) {
        self.jit_entry
            .get_mut(&self_class)
            .map(|info| info.inline_cache_map = cache);
    }

    pub(crate) fn jit_invalidated(&self) -> bool {
        self.jit_invalidated
    }

    pub(crate) fn get_jit_entry(&self, self_class: ClassId) -> Option<DestLabel> {
        if self.jit_invalidated {
            return None;
        }
        self.jit_entry
            .get(&self_class)
            .map(|info| info.entry.clone())
    }

    pub(crate) fn get_jit_class_version(&self, self_class: ClassId) -> Option<DestLabel> {
        self.jit_entry
            .get(&self_class)
            .map(|info| info.class_version_label.clone())
    }

    pub(crate) fn invalidate_jit_code(&mut self) {
        self.jit_invalidated = true;
        self.jit_entry.clear();
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
                16 => TraceIr::LoadIvar(
                    SlotId::new(op1_w),
                    IdentId::from(op1_l),
                    if let Some(class) = pc.cached_class0() {
                        let ivar = pc.cached_ivarid();
                        Some((class, ivar))
                    } else {
                        None
                    },
                ),
                17 => TraceIr::StoreIvar(
                    SlotId::new(op1_w),
                    IdentId::from(op1_l),
                    if let Some(class) = pc.cached_class0() {
                        let ivar = pc.cached_ivarid();
                        Some((class, ivar))
                    } else {
                        None
                    },
                ),
                19 => TraceIr::CheckKwRest(SlotId::new(op1_w)),
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
                30..=33 => {
                    let callid = op1_l.into();
                    let polymorphic = match pc.opcode_sub() {
                        0 => false,
                        1 => true,
                        _ => unreachable!(),
                    };
                    let cache = if let Some(cache) = pc.method_cache() {
                        Some(cache)
                    } else {
                        None
                    };
                    TraceIr::MethodCall {
                        _polymorphic: polymorphic,
                        callid,
                        cache,
                    }
                }
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
                    let else_dest = self.get_bb(bc_pos + 1 + (offsets[0] as i32));
                    let branch_table: Box<[_]> = branch_table
                        .iter()
                        .map(|ofs| self.get_bb(bc_pos + 1 + (*ofs as i32)))
                        .collect();
                    TraceIr::OptCase {
                        cond: SlotId::new(op1_w),
                        min: *min,
                        max: *max,
                        else_dest,
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
                40 => {
                    let (_, op1_w2, op1_w3) = dec_www(op1);
                    TraceIr::ArrayTEq {
                        lhs: SlotId::new(op1_w2),
                        rhs: SlotId::new(op1_w3),
                    }
                }
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
                69 => TraceIr::DefinedSuper {
                    dst: SlotId::new(op1_w1),
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
                120 => TraceIr::Not {
                    dst: SlotId::new(op1_w1),
                    src: SlotId::new(op2_w2),
                },
                121..=123 => {
                    let kind = UnOpK::from(opcode - 121);
                    let dst = SlotId::new(op1_w1);
                    let src = SlotId::new(op2_w2);
                    TraceIr::UnOp {
                        kind,
                        dst,
                        src,
                        ic: pc.classid1(),
                    }
                }
                130 => TraceIr::InlineCache,
                132 => TraceIr::Index {
                    dst: SlotId::new(op1_w1),
                    base: SlotId::new(op2_w2),
                    idx: SlotId::new(op3_w3),
                    class: if let Some(base_class) = pc.classid1()
                        && let Some(idx_class) = pc.classid2()
                    {
                        Some((base_class, idx_class))
                    } else {
                        None
                    },
                },
                133 => TraceIr::IndexAssign {
                    base: SlotId::new(op2_w2),
                    idx: SlotId::new(op3_w3),
                    src: SlotId::new(op1_w1),
                    class: if let Some(base_class) = pc.classid1()
                        && let Some(idx_class) = pc.classid2()
                    {
                        Some((base_class, idx_class))
                    } else {
                        None
                    },
                },
                140..=146 => {
                    let kind = CmpKind::from(opcode - 140);
                    let dst = SlotId::from(op1_w1);
                    let lhs = SlotId::new(op2_w2);
                    let rhs = SlotId::new(op3_w3);
                    let ic = if let Some(lhs_class) = pc.classid1()
                        && let Some(rhs_class) = pc.classid2()
                    {
                        Some((lhs_class, rhs_class))
                    } else {
                        None
                    };
                    TraceIr::BinCmp {
                        kind,
                        dst,
                        lhs,
                        rhs,
                        ic,
                    }
                }

                148 => TraceIr::LoadDynVar(
                    SlotId::new(op1_w1),
                    DynVar {
                        reg: SlotId::new(op2_w2),
                        outer: op3_w3 as usize,
                    },
                ),
                149 => TraceIr::StoreDynVar(
                    DynVar {
                        reg: SlotId::new(op1_w1),
                        outer: op2_w2 as usize,
                    },
                    SlotId::new(op3_w3),
                ),
                150..=156 => {
                    let kind = CmpKind::from(opcode - 150);
                    let dst = SlotId::from(op1_w1);
                    let lhs = SlotId::new(op2_w2);
                    let rhs = SlotId::new(op3_w3);
                    let ic = if let Some(lhs_class) = pc.classid1()
                        && let Some(rhs_class) = pc.classid2()
                    {
                        Some((lhs_class, rhs_class))
                    } else {
                        None
                    };
                    let (dest, brkind) = match self.trace_ir(store, bc_pos + 1) {
                        TraceIr::CondBr(_, dest, true, brkind) => (dest, brkind),
                        _ => unreachable!(),
                    };
                    TraceIr::BinCmpBr {
                        kind,
                        _dst: dst,
                        lhs,
                        rhs,
                        dest_bb: dest,
                        brkind,
                        ic,
                    }
                }
                160..=168 => {
                    let kind = BinOpK::from(opcode - 160);
                    let dst = SlotId::from(op1_w1);
                    let lhs = SlotId::new(op2_w2);
                    let rhs = SlotId::new(op3_w3);
                    let ic = if let Some(lhs_class) = pc.classid1()
                        && let Some(rhs_class) = pc.classid2()
                    {
                        Some((lhs_class, rhs_class))
                    } else {
                        None
                    };
                    TraceIr::BinOp {
                        kind,
                        dst,
                        lhs,
                        rhs,
                        ic,
                    }
                }

                170 => TraceIr::InitMethod(FnInitInfo {
                    reg_num: op1_w1 as usize,
                    arg_num: op2_w2 as usize,
                    stack_offset: op3_w3 as usize,
                }),
                171 => TraceIr::ExpandArray {
                    src: SlotId::new(op1_w1),
                    dst: (SlotId::new(op2_w2), op3_w3, {
                        let rest = op2.get_u16();
                        if rest == 0 { None } else { Some(rest - 1) }
                    }),
                },
                172 => {
                    let undef = IdentId::from(dec_wl(op1).1);
                    TraceIr::UndefMethod { undef }
                }
                173 => {
                    let (new, old) = op2.get_ident2();
                    TraceIr::AliasMethod { new, old }
                }
                174 => TraceIr::Hash {
                    dst: SlotId::new(op1_w1),
                    args: SlotId::new(op2_w2),
                    len: op3_w3,
                },
                175 => TraceIr::ToA {
                    dst: SlotId::new(op1_w1),
                    src: SlotId::new(op2_w2),
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
    /// required
    required_num: usize,
    /// optional
    optional_num: usize,
    /// rest
    rest: Option<usize>,
    /// post
    post_num: usize,
    // for param, req(incl. destruct slot), opt, rest, keyword, kw_rest, destructed local, block
    pub args_names: Vec<Option<IdentId>>,
    pub kw_names: Vec<IdentId>,
    pub kw_rest: Option<SlotId>,
    block_param: Option<IdentId>,
    forwarding: bool,
}

impl ParamsInfo {
    pub fn new(
        required_num: usize,
        optional_num: usize,
        rest: Option<usize>,
        post_num: usize,
        args_names: Vec<Option<IdentId>>,
        keyword_names: Vec<IdentId>,
        kw_rest: Option<SlotId>,
        block_param: Option<IdentId>,
        forwarding: bool,
    ) -> Self {
        ParamsInfo {
            required_num,
            optional_num,
            rest,
            post_num,
            args_names,
            kw_names: keyword_names,
            kw_rest,
            block_param,
            forwarding,
        }
    }

    pub fn new_attr_reader() -> Self {
        ParamsInfo::default()
    }

    pub fn new_attr_writer() -> Self {
        ParamsInfo {
            required_num: 1,
            optional_num: 0,
            rest: None,
            post_num: 0,
            args_names: vec![],
            kw_names: vec![],
            kw_rest: None,
            block_param: None,
            forwarding: false,
        }
    }

    pub fn new_native(
        min: usize,
        max: usize,
        rest: bool,
        kw_names: Vec<IdentId>,
        kw_rest: bool,
    ) -> Self {
        let mut p = max;
        let kw_num = kw_names.len();
        ParamsInfo {
            required_num: min,
            optional_num: max - min,
            rest: if rest {
                p += 1;
                Some(max)
            } else {
                None
            },
            post_num: 0,
            args_names: vec![],
            kw_names,
            kw_rest: if kw_rest {
                Some(SlotId::new((1 + p + kw_num) as u16))
            } else {
                None
            },
            block_param: None,
            forwarding: false,
        }
    }

    ///
    /// The number of required arguments.
    ///
    pub(crate) fn req_num(&self) -> usize {
        self.required_num
    }

    ///
    /// The number of required arguments.
    ///
    pub(crate) fn opt_num(&self) -> usize {
        self.optional_num
    }

    ///
    /// The number of required + optional arguments.
    ///
    pub(crate) fn reqopt_num(&self) -> usize {
        self.required_num + self.optional_num
    }

    ///
    /// The number of post arguments.
    ///
    pub(crate) fn post_num(&self) -> usize {
        self.post_num
    }

    ///
    pub fn is_rest(&self) -> Option<u16> {
        self.rest.map(|i| i as u16)
    }

    pub fn forwarding(&self) -> bool {
        self.forwarding
    }

    ///
    /// The number of required + post arguments.
    ///
    pub fn min_positional_args(&self) -> usize {
        self.required_num + self.post_num
    }

    ///
    /// The number of required + optional + post arguments.
    ///
    pub fn max_positional_args(&self) -> usize {
        self.required_num + self.optional_num + self.post_num
    }

    ///
    /// The number of required + optional + rest + post arguments.
    ///
    pub fn total_positional_args(&self) -> usize {
        self.max_positional_args() + self.is_rest().is_some() as usize
    }

    /// The posiiton of keyword arguments.
    pub(crate) fn kw_reg_pos(&self) -> SlotId {
        // 1 is for self.
        SlotId(self.total_positional_args() as u16 + 1)
    }

    pub fn total_args(&self) -> usize {
        self.required_num
            + self.optional_num
            + self.post_num
            + self.rest.is_some() as usize
            + self.kw_names.len()
            + self.kw_rest.is_some() as usize
            + self.block_param.is_some() as usize
    }

    ///
    /// If `self` is "simple", return true.
    ///
    /// "simple" means that the function has no optional, post, rest, keyword, keywoed rest, and block parameters.
    ///
    pub fn is_simple(&self) -> bool {
        self.optional_num == 0
            && self.post_num == 0
            && self.is_rest().is_none()
            && self.kw_names.is_empty()
            && self.kw_rest.is_none()
            && self.block_param.is_none()
    }
}
