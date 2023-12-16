use monoasm_macro::monoasm;
use paste::paste;
use ruruby_parse::CmpKind;

use crate::bytecodegen::{BcIndex, UnOpK};

pub(crate) use self::basic_block::BasicBlockInfo;
pub(self) use self::basic_block::{BasciBlockInfoEntry, BasicBlockId};

use super::*;
use analysis::{ExitType, SlotInfo};
use asmir::*;
use slot::SlotState;
use trace_ir::*;

pub mod analysis;
pub mod asmir;
mod basic_block;
mod definition;
mod guard;
mod init_method;
mod merge;
mod slot;
pub mod trace_ir;

const RECOMPILE_COUNT: i32 = 10;

//
// Just-in-time compiler module.
//

///
/// Context for JIT compilation.
///
struct JitContext {
    ///
    /// Destination labels for jump instructions.
    ///
    inst_labels: HashMap<BcIndex, DestLabel>,
    branch_labels: Vec<Option<DestLabel>>,
    ///
    /// Basic block information.
    ///
    bb_scan: Vec<(ExitType, SlotInfo)>,
    ///
    /// Back edges to the loop head.
    ///
    loop_backedges: HashMap<BasicBlockId, SlotInfo>,
    ///
    /// Loop
    ///
    /// ### key
    /// start basic block.
    ///
    /// ### value
    /// (end basic block, slot_info)
    ///
    loop_exit: HashMap<BasicBlockId, (BasicBlockId, SlotInfo)>,
    ///
    /// Nested loop count.
    ///
    loop_count: usize,
    ///
    /// true for a loop, false for a method.
    ///
    is_loop: bool,
    ///
    /// A map for bytecode position and branches.
    ///
    branch_map: HashMap<BcIndex, Vec<BranchEntry>>,
    ///
    /// Target context (BBContext) for an each instruction.
    ///
    target_ctx: HashMap<BcIndex, BBContext>,
    ///
    /// A map for backward branches.
    ///
    backedge_map: HashMap<BcIndex, (DestLabel, BBContext, Vec<SlotId>)>,
    ///
    /// the number of slots.
    ///
    total_reg_num: usize,
    ///
    /// the number of local variables.
    ///
    local_num: usize,
    ///
    /// *self* for this loop/method.
    ///
    self_value: Value,
    ///
    /// source map.
    ///
    sourcemap: Vec<(BcIndex, usize)>,
    ///
    /// IR for machine code generator.
    ///
    bridges: Vec<(AsmIr, BranchLabel, DestLabel)>,
    continuation_bridge: Option<(AsmIr, BranchLabel)>,
    opt_case: Vec<OptCaseAsmInfo>,
    ///
    /// The start offset of a machine code corresponding to thhe current basic block.
    ///
    #[cfg(feature = "emit-asm")]
    start_codepos: usize,
}

impl std::ops::Index<BranchLabel> for JitContext {
    type Output = DestLabel;
    fn index(&self, index: BranchLabel) -> &Self::Output {
        self.branch_labels[index.0].as_ref().unwrap()
    }
}

///
/// Information of incoming branches to an each basic block.
///
#[derive(Clone)]
pub(crate) struct Incoming(Vec<Vec<BcIndex>>);

impl std::ops::Deref for Incoming {
    type Target = Vec<Vec<BcIndex>>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::ops::Index<BcIndex> for Incoming {
    type Output = Vec<BcIndex>;
    fn index(&self, index: BcIndex) -> &Self::Output {
        &self.0[index.0 as usize]
    }
}

#[derive(Clone, Copy, Debug)]
struct BranchLabel(usize);

#[derive(Debug, Clone)]
struct OptCaseAsmInfo {
    id: OptCaseId,
    bb_pos: BcIndex,
    label_map: HashMap<BcIndex, BranchLabel>,
}

impl JitContext {
    ///
    /// Create new JitContext.
    ///
    fn new(
        func: &ISeqInfo,
        store: &Store,
        codegen: &mut Codegen,
        is_loop: bool,
        self_value: Value,
    ) -> Self {
        let mut inst_labels = HashMap::default();
        for i in 0..func.bytecode_len() {
            let idx = BcIndex::from(i);
            if func.bb_info.is_bb_head(idx) {
                inst_labels.insert(idx, codegen.jit.label());
            }
        }
        let bb_scan = func.bb_info.init_bb_scan(func, store);

        #[cfg(feature = "emit-asm")]
        let start_codepos = codegen.jit.get_current();

        let total_reg_num = func.total_reg_num();
        let local_num = func.local_num();
        Self {
            inst_labels,
            branch_labels: vec![],
            bb_scan,
            loop_backedges: HashMap::default(),
            loop_exit: HashMap::default(),
            loop_count: 0,
            is_loop,
            branch_map: HashMap::default(),
            target_ctx: HashMap::default(),
            backedge_map: HashMap::default(),
            total_reg_num,
            local_num,
            self_value,
            sourcemap: vec![],
            bridges: vec![],
            continuation_bridge: None,
            opt_case: vec![],
            #[cfg(feature = "emit-asm")]
            start_codepos,
        }
    }

    fn branch_label(&mut self) -> BranchLabel {
        let label = BranchLabel(self.branch_labels.len());
        self.branch_labels.push(None);
        label
    }

    ///
    /// Add new branch from *src_idx* to *dest* with the context *bbctx*.
    ///
    fn new_branch(
        &mut self,
        func: &ISeqInfo,
        src_idx: BcIndex,
        dest: BcIndex,
        mut bbctx: BBContext,
        label: BranchLabel,
    ) {
        bbctx.sp = func.get_sp(src_idx);
        #[cfg(feature = "jit-debug")]
        eprintln!("   new_branch: [{:?}]{src_idx}->{dest}", bbctx.sp);
        self.branch_map.entry(dest).or_default().push(BranchEntry {
            src_idx,
            bbctx,
            label,
            cont: false,
        });
    }

    ///
    /// Add new continuation branch from *src_idx* to *dest* with the context *bbctx*.
    ///
    fn new_continue(
        &mut self,
        func: &ISeqInfo,
        src_idx: BcIndex,
        dest: BcIndex,
        mut bbctx: BBContext,
        label: BranchLabel,
    ) {
        bbctx.sp = func.get_sp(src_idx);
        #[cfg(feature = "jit-debug")]
        eprintln!("   new_continue:[{:?}] {src_idx}->{dest}", bbctx.sp);
        self.branch_map.entry(dest).or_default().push(BranchEntry {
            src_idx,
            bbctx,
            label,
            cont: true,
        })
    }

    ///
    /// Add new backward branch from *src_idx* to *dest* with the context *bbctx*.
    ///
    fn new_backedge(
        &mut self,
        func: &ISeqInfo,
        bbctx: &mut BBContext,
        bb_pos: BcIndex,
        dest_label: DestLabel,
        unused: Vec<SlotId>,
    ) {
        bbctx.sp = func.get_sp(bb_pos);
        #[cfg(feature = "jit-debug")]
        eprintln!("   new_backedge:[{:?}] {bb_pos}", bbctx.sp);
        self.backedge_map
            .insert(bb_pos, (dest_label, bbctx.clone(), unused));
    }

    fn compile_bb_inner(
        &mut self,
        ir: &mut AsmIr,
        store: &Store,
        func: &ISeqInfo,
        bbctx: &mut BBContext,
        position: Option<BcPc>,
        bb_begin: BcIndex,
        bb_end: BcIndex,
    ) -> bool {
        for bb_pos in bb_begin..=bb_end {
            ir.bc_index(bb_pos);
            bbctx.next_sp = func.get_sp(bb_pos);

            match self.compile_inst(ir, bbctx, store, func, bb_pos) {
                CompileResult::Continue => {}
                CompileResult::Exit => {
                    return true;
                }
                CompileResult::Recompile => {
                    let pc = func.get_pc(bb_pos);
                    ir.recompile_and_deopt(&bbctx, pc, position);
                    return true;
                }
                CompileResult::Break => break,
            }

            bbctx.clear();
            bbctx.sp = bbctx.next_sp;
        }

        false
    }

    fn compile_inst(
        &mut self,
        ir: &mut AsmIr,
        bbctx: &mut BBContext,
        store: &Store,
        func: &ISeqInfo,
        bb_pos: BcIndex,
    ) -> CompileResult {
        let pc = func.get_pc(bb_pos);
        match pc.trace_ir() {
            TraceIr::InitMethod { .. } => {}
            TraceIr::LoopStart(_) => {
                self.loop_count += 1;
            }
            TraceIr::LoopEnd => {
                assert_ne!(0, self.loop_count);
                self.loop_count -= 1;
                if self.is_loop && self.loop_count == 0 {
                    ir.deopt(&bbctx, pc);
                    return CompileResult::Break;
                }
            }
            TraceIr::Integer(dst, i) => {
                bbctx.link_literal(dst, Value::i32(i));
            }
            TraceIr::Symbol(dst, id) => {
                bbctx.link_literal(dst, Value::symbol(id));
            }
            TraceIr::Nil(dst) => {
                bbctx.link_literal(dst, Value::nil());
            }
            TraceIr::Literal(dst, val) => {
                bbctx.release(dst);
                if val.is_packed_value() || val.class() == FLOAT_CLASS {
                    bbctx.link_literal(dst, val);
                } else {
                    ir.deep_copy_lit(&bbctx, val);
                    ir.rax2acc(bbctx, dst);
                }
            }
            TraceIr::Array { dst, callid } => {
                let CallSiteInfo { args, pos_num, .. } = store[callid];
                ir.fetch_range(bbctx, args, pos_num as u16);
                bbctx.release(dst);
                ir.new_array(&bbctx, callid);
                ir.rax2acc(bbctx, dst);
            }
            TraceIr::Hash { dst, args, len } => {
                ir.fetch_range(bbctx, args, len * 2);
                bbctx.release(dst);
                ir.new_hash(&bbctx, args, len as _);
                ir.rax2acc(bbctx, dst);
            }
            TraceIr::Range {
                dst,
                start,
                end,
                exclude_end,
            } => {
                ir.fetch_slots(bbctx, &[start, end]);
                bbctx.release(dst);
                ir.new_range(bbctx, pc, start, end, exclude_end);
                ir.rax2acc(bbctx, dst);
            }
            TraceIr::Index { dst, base, idx } => {
                if pc.classid1().0 == 0 || pc.classid2().0 == 0 {
                    return CompileResult::Recompile;
                }
                if pc.classid1() == ARRAY_CLASS && pc.classid2() == INTEGER_CLASS {
                    bbctx.array_index(ir, dst, base, idx, pc);
                } else {
                    ir.fetch_slots(bbctx, &[base, idx]);
                    bbctx.release(dst);
                    ir.generic_index(&bbctx, pc, base, idx);
                }
                ir.rax2acc(bbctx, dst);
            }
            TraceIr::IndexAssign { src, base, idx } => {
                if pc.classid1().0 == 0 || pc.classid2().0 == 0 {
                    return CompileResult::Recompile;
                }
                if pc.classid1() == ARRAY_CLASS && pc.classid2() == INTEGER_CLASS {
                    bbctx.array_index_assign(ir, src, base, idx, pc);
                } else {
                    ir.fetch_slots(bbctx, &[base, idx, src]);
                    ir.generic_index_assign(&bbctx, pc, base, idx, src);
                }
            }
            TraceIr::LoadConst(dst, id) => {
                bbctx.release(dst);

                if let (cached_version, cached_baseclass, Some(cached_val)) = store[id].cache {
                    let base_slot = store[id].base;
                    if let Some(slot) = base_slot {
                        if let Some(base_class) = cached_baseclass {
                            ir.fetch_to_reg(bbctx, slot, GP::Rax);
                            let deopt = ir.new_deopt(pc, bbctx.get_write_back());
                            ir.inst.push(AsmInst::GuardBaseClass { base_class, deopt });
                        } else {
                            return CompileResult::Recompile;
                        }
                    }
                    let deopt = ir.new_deopt(pc, bbctx.get_write_back());
                    if let Some(f) = cached_val.try_float() {
                        let fdst = bbctx.link_new_both(dst);
                        ir.inst.push(AsmInst::LoadFloatConstant {
                            fdst,
                            f,
                            cached_version,
                            deopt,
                        });
                    } else {
                        ir.inst.push(AsmInst::LoadGenericConstant {
                            cached_val,
                            cached_version,
                            deopt,
                        });
                    }
                    ir.rax2acc(bbctx, dst);
                } else {
                    return CompileResult::Recompile;
                }
            }
            TraceIr::StoreConst(src, name) => {
                ir.fetch_to_reg(bbctx, src, GP::Rax);
                let using_xmm = bbctx.get_using_xmm();
                ir.inst.push(AsmInst::StoreConstant { name, using_xmm });
            }
            TraceIr::BlockArgProxy(ret, outer) => {
                bbctx.release(ret);
                ir.block_arg_proxy(ret, outer);
            }
            TraceIr::BlockArg(ret, outer) => {
                bbctx.release(ret);
                ir.block_arg(&bbctx, pc, ret, outer);
            }
            TraceIr::LoadIvar(ret, id, cached_class, cached_ivarid) => {
                if let Some(cached_class) = cached_class {
                    bbctx.jit_load_ivar(ir, id, ret, cached_class, cached_ivarid);
                } else {
                    return CompileResult::Recompile;
                }
            }
            TraceIr::StoreIvar(src, id, cached_class, cached_ivarid) => {
                if let Some(cached_class) = cached_class {
                    bbctx.jit_store_ivar(ir, id, src, pc, cached_class, cached_ivarid);
                } else {
                    return CompileResult::Recompile;
                }
            }
            TraceIr::LoadGvar { dst, name } => {
                ir.jit_load_gvar(bbctx, name, dst);
            }
            TraceIr::StoreGvar { src: val, name } => {
                ir.jit_store_gvar(bbctx, name, val);
            }
            TraceIr::LoadSvar { dst, id } => {
                bbctx.release(dst);
                ir.load_svar(&bbctx, id);
                ir.rax2acc(bbctx, dst);
            }
            TraceIr::LoadDynVar(dst, src) => {
                bbctx.release(dst);
                if !dst.is_zero() {
                    ir.inst.push(AsmInst::LoadDynVar { src });
                    ir.rax2acc(bbctx, dst);
                }
            }
            TraceIr::StoreDynVar(dst, src) => {
                ir.fetch_to_reg(bbctx, src, GP::Rdi);
                ir.inst.push(AsmInst::StoreDynVar { dst, src: GP::Rdi });
            }
            TraceIr::BitNot { dst, src } => {
                if pc.classid1().0 == 0 {
                    return CompileResult::Recompile;
                }
                ir.fetch_to_reg(bbctx, src, GP::Rdi);
                bbctx.release(dst);
                if pc.classid1().0 == 0 {
                    return CompileResult::Recompile;
                } else {
                    ir.generic_unop(&bbctx, pc, bitnot_value);
                    ir.rax2acc(bbctx, dst);
                }
            }
            TraceIr::Not { dst, src } => {
                ir.fetch_to_reg(bbctx, src, GP::Rdi);
                bbctx.release(dst);
                ir.inst.push(AsmInst::Not);
                ir.rax2acc(bbctx, dst);
            }
            TraceIr::UnOp { kind, dst, src } => {
                if pc.classid1().0 == 0 {
                    return CompileResult::Recompile;
                }
                if pc.is_float1() {
                    let deopt = ir.new_deopt(pc, bbctx.get_write_back());
                    let fsrc = ir.fetch_float_assume_float(bbctx, src, deopt);
                    let dst = bbctx.xmm_write(dst);
                    ir.xmm_move(fsrc, dst);
                    ir.inst.push(AsmInst::XmmUnOp { kind, dst });
                } else {
                    ir.fetch_to_reg(bbctx, src, GP::Rdi);
                    bbctx.release(dst);
                    ir.generic_unop(&bbctx, pc, kind.generic_func());
                    ir.rax2acc(bbctx, dst);
                }
            }
            TraceIr::IBinOp { kind, dst, mode } => {
                ir.gen_binop_integer(bbctx, pc, kind, dst, mode);
            }
            TraceIr::FBinOp { kind, dst, mode } => {
                let deopt = ir.new_deopt(pc, bbctx.get_write_back());
                let fmode = ir.fmode(&mode, bbctx, pc, deopt);
                if let Some(ret) = dst {
                    let dst = bbctx.xmm_write(ret);
                    let using_xmm = bbctx.get_using_xmm();
                    ir.xmm_binop(kind, fmode, dst, using_xmm);
                }
            }
            TraceIr::BinOp {
                kind, dst, mode, ..
            } => {
                if pc.classid1().0 == 0 || pc.classid2().0 == 0 {
                    return CompileResult::Recompile;
                }
                ir.fetch_binary(bbctx, mode);
                bbctx.release(dst);
                ir.generic_binop(&bbctx, pc, kind);
                ir.rax2acc(bbctx, dst);
            }
            TraceIr::Cmp(kind, ret, mode, false) => {
                if pc.classid1().0 == 0 || pc.classid2().0 == 0 {
                    return CompileResult::Recompile;
                }
                if mode.is_float_op(&pc) && kind != CmpKind::Cmp {
                    let deopt = ir.new_deopt(pc, bbctx.get_write_back());
                    let mode = ir.fmode(&mode, bbctx, pc, deopt);
                    bbctx.release(ret);
                    ir.inst.push(AsmInst::FloatCmp { kind, mode });
                } else if mode.is_integer_op(&pc) {
                    ir.fetch_fixnum_binary(bbctx, pc, &mode);
                    bbctx.release(ret);
                    ir.inst.push(AsmInst::IntegerCmp { kind, mode });
                } else {
                    ir.fetch_binary(bbctx, mode);
                    bbctx.release(ret);
                    ir.generic_cmp(&bbctx, pc, kind);
                }
                ir.rax2acc(bbctx, ret);
            }

            TraceIr::Cmp(kind, ret, mode, true) => {
                let index = bb_pos + 1;
                match (pc + 1).trace_ir() {
                    TraceIr::CondBr(_, disp, true, brkind) => {
                        let dest_idx = index + disp + 1;
                        let branch_dest = self.branch_label();
                        if mode.is_float_op(&pc) {
                            let deopt = ir.new_deopt(pc, bbctx.get_write_back());
                            let mode = ir.fmode(&mode, bbctx, pc, deopt);
                            bbctx.release(ret);
                            ir.float_cmp_br(mode, kind, brkind, branch_dest);
                        } else {
                            if mode.is_integer_op(&pc) {
                                ir.fetch_fixnum_binary(bbctx, pc, &mode);
                                bbctx.release(ret);
                                ir.integer_cmp_br(mode, kind, brkind, branch_dest);
                            } else {
                                ir.fetch_binary(bbctx, mode);
                                bbctx.release(ret);
                                ir.generic_cmp(&bbctx, pc, kind);
                                ir.inst.push(AsmInst::GenericCondBr {
                                    brkind,
                                    branch_dest,
                                });
                            }
                        }
                        self.new_branch(func, index, dest_idx, bbctx.clone(), branch_dest);
                    }
                    _ => unreachable!(),
                }
            }
            TraceIr::Mov(dst, src) => {
                bbctx.copy_slot(ir, src, dst);
            }
            TraceIr::ConcatStr(dst, arg, len) => {
                ir.fetch_range(bbctx, arg, len);
                bbctx.release(dst);
                ir.concat_str(&bbctx, arg, len);
                ir.rax2acc(bbctx, dst);
            }
            TraceIr::ConcatRegexp(dst, arg, len) => {
                ir.fetch_range(bbctx, arg, len);
                bbctx.release(dst);
                ir.concat_regexp(&bbctx, pc, arg, len);
                ir.rax2acc(bbctx, dst);
            }
            TraceIr::ExpandArray(src, dst, len) => {
                ir.fetch_to_reg(bbctx, src, GP::Rdi);
                for reg in dst.0..dst.0 + len {
                    bbctx.release(SlotId(reg));
                }
                ir.expand_array(&bbctx, dst, len);
            }
            TraceIr::AliasMethod { new, old } => {
                ir.fetch_slots(bbctx, &[new, old]);
                ir.alias_method(&bbctx, pc, new, old);
            }
            TraceIr::MethodCall { callid } | TraceIr::MethodCallBlock { callid } => {
                // We must write back and unlink all local vars since they may be accessed from block.
                if store[callid].block_fid.is_some() {
                    ir.write_back_locals(bbctx);
                }
                if let Some(fid) = pc.cached_fid() {
                    if ir.gen_call(store, bbctx, fid, callid, pc).is_none() {
                        return CompileResult::Recompile;
                    }
                } else {
                    return CompileResult::Recompile;
                }
            }
            TraceIr::InlineCall {
                inline_id, callid, ..
            } => {
                let inline_gen = store.get_inline_info(inline_id).0;
                ir.writeback_acc(bbctx);
                let deopt = ir.new_deopt(pc, bbctx.get_write_back());
                ir.guard_class_version(pc, deopt);
                inline_gen(ir, store, bbctx, &store[callid], pc);
            }
            TraceIr::Yield { callid } => {
                ir.fetch_callargs(bbctx, &store[callid]);
                bbctx.release(store[callid].dst);
                ir.writeback_acc(bbctx);
                let using_xmm = bbctx.get_using_xmm();
                let error = ir.new_error(pc, bbctx.get_write_back());
                ir.inst.push(AsmInst::Yield {
                    callid,
                    using_xmm,
                    error,
                });
                ir.rax2acc(bbctx, store[callid].dst);
            }
            TraceIr::InlineCache => {}
            TraceIr::MethodDef { name, func_id } => {
                let using_xmm = bbctx.get_using_xmm();
                ir.inst.push(AsmInst::MethodDef {
                    name,
                    func_id,
                    using_xmm,
                });
            }
            TraceIr::SingletonMethodDef { obj, name, func_id } => {
                ir.fetch_slots(bbctx, &[obj]);
                let using_xmm = bbctx.get_using_xmm();
                ir.inst.push(AsmInst::SingletonMethodDef {
                    obj,
                    name,
                    func_id,
                    using_xmm,
                });
            }
            TraceIr::ClassDef {
                dst,
                superclass,
                name,
                func_id,
            } => {
                bbctx.class_def(ir, dst, superclass, name, func_id, false, pc);
            }
            TraceIr::ModuleDef { dst, name, func_id } => {
                bbctx.class_def(ir, dst, SlotId::new(0), name, func_id, true, pc);
            }
            TraceIr::SingletonClassDef { dst, base, func_id } => {
                bbctx.singleton_class_def(ir, dst, base, func_id, pc);
            }
            TraceIr::DefinedYield { dst } => {
                ir.fetch_slots(bbctx, &[dst]);
                let using_xmm = bbctx.get_using_xmm();
                ir.inst.push(AsmInst::DefinedYield { dst, using_xmm });
            }
            TraceIr::DefinedConst { dst, siteid } => {
                ir.fetch_slots(bbctx, &[dst]);
                let using_xmm = bbctx.get_using_xmm();
                ir.inst.push(AsmInst::DefinedConst {
                    dst,
                    siteid,
                    using_xmm,
                });
            }
            TraceIr::DefinedMethod { dst, recv, name } => {
                ir.fetch_slots(bbctx, &[dst, recv]);
                let using_xmm = bbctx.get_using_xmm();
                ir.inst.push(AsmInst::DefinedMethod {
                    dst,
                    recv,
                    name,
                    using_xmm,
                });
            }
            TraceIr::DefinedGvar { dst, name } => {
                ir.fetch_slots(bbctx, &[dst]);
                let using_xmm = bbctx.get_using_xmm();
                ir.inst.push(AsmInst::DefinedGvar {
                    dst,
                    name,
                    using_xmm,
                });
            }
            TraceIr::DefinedIvar { dst, name } => {
                ir.fetch_slots(bbctx, &[dst]);
                let using_xmm = bbctx.get_using_xmm();
                ir.inst.push(AsmInst::DefinedIvar {
                    dst,
                    name,
                    using_xmm,
                });
            }
            TraceIr::Ret(ret) => {
                ir.write_back_locals(bbctx);
                ir.fetch_to_reg(bbctx, ret, GP::Rax);
                ir.inst.push(AsmInst::Ret);
                return CompileResult::Exit;
            }
            TraceIr::MethodRet(ret) => {
                ir.write_back_locals(bbctx);
                ir.fetch_to_reg(bbctx, ret, GP::Rax);
                ir.inst.push(AsmInst::MethodRet(pc));
                return CompileResult::Exit;
            }
            TraceIr::Break(ret) => {
                ir.write_back_locals(bbctx);
                ir.fetch_to_reg(bbctx, ret, GP::Rax);
                ir.inst.push(AsmInst::Break);
                return CompileResult::Exit;
            }
            TraceIr::Raise(ret) => {
                ir.write_back_locals(bbctx);
                ir.fetch_to_reg(bbctx, ret, GP::Rax);
                ir.inst.push(AsmInst::Raise);
                return CompileResult::Exit;
            }
            TraceIr::EnsureEnd => {
                ir.write_back_locals(bbctx);
                ir.inst.push(AsmInst::EnsureEnd);
            }
            TraceIr::Br(disp) => {
                let next_idx = bb_pos + 1;
                let dest_idx = next_idx + disp;
                let branch_dest = self.branch_label();
                ir.inst.push(AsmInst::Br(branch_dest));
                self.new_branch(func, bb_pos, dest_idx, bbctx.clone(), branch_dest);
                return CompileResult::Exit;
            }
            TraceIr::CondBr(cond_, disp, false, brkind) => {
                let dest_idx = bb_pos + 1 + disp;
                let branch_dest = self.branch_label();
                ir.fetch_to_reg(bbctx, cond_, GP::Rax);
                ir.inst.push(AsmInst::CondBr(brkind, branch_dest));
                self.new_branch(func, bb_pos, dest_idx, bbctx.clone(), branch_dest);
            }
            TraceIr::CondBr(_, _, true, _) => {}
            TraceIr::CheckLocal(local, disp) => {
                let dest_idx = bb_pos + 1 + disp;
                let branch_dest = self.branch_label();
                ir.fetch_to_reg(bbctx, local, GP::Rax);
                ir.inst.push(AsmInst::CheckLocal(branch_dest));
                self.new_branch(func, bb_pos, dest_idx, bbctx.clone(), branch_dest);
            }
            TraceIr::OptCase { cond, optid } => {
                let OptCaseInfo {
                    min, max, offsets, ..
                } = &store[optid];
                let mut label_map = HashMap::default();
                for ofs in offsets {
                    let dest_idx = bb_pos + 1 + (*ofs as i32);
                    let branch_dest = self.branch_label();
                    label_map.insert(dest_idx, branch_dest);
                    self.new_branch(func, bb_pos, dest_idx, bbctx.clone(), branch_dest);
                }
                let else_idx = bb_pos + 1 + (offsets[0] as i32);
                let else_dest = label_map.get(&else_idx).cloned().unwrap();

                let opt_case_info = OptCaseAsmInfo {
                    id: optid,
                    bb_pos,
                    label_map,
                };
                let opt_case_id = self.opt_case.len();
                self.opt_case.push(opt_case_info);

                let deopt = ir.new_deopt(pc, bbctx.get_write_back());
                ir.fetch_to_reg(bbctx, cond, GP::Rdi);
                ir.guard_fixnum(GP::Rdi, deopt);
                ir.opt_case(*max, *min, opt_case_id, else_dest);
                return CompileResult::Exit;
            }
        }
        CompileResult::Continue
    }
}

///
/// The information for branches.
///
#[derive(Debug)]
struct BranchEntry {
    /// source instruction index of the branch.
    src_idx: BcIndex,
    /// context of the source basic block.
    bbctx: BBContext,
    /// `DestLabel` for the destination basic block.
    label: BranchLabel,
    /// true if the branch is a continuation branch.
    /// 'continuation' means the destination is adjacent to the source basic block on the bytecode.
    cont: bool,
}

pub(crate) fn conv(reg: SlotId) -> i64 {
    reg.0 as i64 * 8 + LBP_SELF
}

///
/// The strust holds information for writing back Value's in xmm registers or accumulator to the corresponding stack slots.
///
/// Currently supports `literal`s, `xmm` registers and a `R15` register (as an accumulator).
///
#[derive(Clone)]
pub(crate) struct WriteBack {
    xmm: Vec<(Xmm, Vec<SlotId>)>,
    literal: Vec<(Value, SlotId)>,
    r15: Option<SlotId>,
}

impl WriteBack {
    fn new(
        xmm: Vec<(Xmm, Vec<SlotId>)>,
        literal: Vec<(Value, SlotId)>,
        r15: Option<SlotId>,
    ) -> Self {
        Self { xmm, literal, r15 }
    }
}

///
/// Context of an each basic block.
///
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct BBContext {
    /// state stack slots.
    slot_state: SlotState,
    /// stack top register.
    sp: SlotId,
    next_sp: SlotId,
    /// *self* value
    self_value: Value,
}

impl std::ops::Deref for BBContext {
    type Target = SlotState;
    fn deref(&self) -> &Self::Target {
        &self.slot_state
    }
}

impl std::ops::DerefMut for BBContext {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.slot_state
    }
}

impl BBContext {
    fn new(cc: &JitContext) -> Self {
        Self {
            slot_state: SlotState::new(cc),
            sp: SlotId(cc.local_num as u16),
            next_sp: SlotId(cc.local_num as u16),
            self_value: cc.self_value,
        }
    }

    fn reg_num(&self) -> usize {
        self.slot_state.len()
    }

    fn remove_unused(&mut self, unused: &[SlotId]) {
        unused.iter().for_each(|reg| self.release(*reg));
    }

    fn merge(&mut self, other: &Self) {
        self.slot_state.merge(&other.slot_state);
    }

    fn merge_entries(entries: &[BranchEntry]) -> Self {
        let mut merge_ctx = entries.last().unwrap().bbctx.clone();
        for BranchEntry {
            src_idx: _src_idx,
            bbctx,
            label: _,
            ..
        } in entries.iter()
        {
            #[cfg(feature = "jit-debug")]
            eprintln!("  <-{:?}:[{:?}] {:?}", _src_idx, bbctx.sp, bbctx.slot_state);
            merge_ctx.merge(bbctx);
        }
        #[cfg(feature = "jit-debug")]
        eprintln!("  merged_entries: {:?}", &merge_ctx.slot_state);
        merge_ctx
    }

    fn is_i16_literal(&self, slot: SlotId) -> Option<i16> {
        if let LinkMode::Literal(v) = self[slot] {
            let i = v.try_fixnum()?;
            i16::try_from(i).ok()
        } else {
            None
        }
    }

    fn is_u16_literal(&self, slot: SlotId) -> Option<u16> {
        if let LinkMode::Literal(v) = self[slot] {
            let i = v.try_fixnum()?;
            u16::try_from(i).ok()
        } else {
            None
        }
    }

    pub(crate) fn is_u8_literal(&self, slot: SlotId) -> Option<u8> {
        if let LinkMode::Literal(v) = self[slot] {
            let i = v.try_fixnum()?;
            u8::try_from(i).ok()
        } else {
            None
        }
    }

    pub(crate) fn get_using_xmm(&self) -> UsingXmm {
        self.slot_state.get_using_xmm(self.sp)
    }

    pub(crate) fn get_write_back(&self) -> WriteBack {
        self.slot_state.get_write_back(self.sp)
    }

    pub(super) fn clear_r15(&mut self) -> Option<SlotId> {
        self.slot_state.clear_r15()
    }

    pub(super) fn clear(&mut self) {
        self.slot_state.clear(self.next_sp);
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
#[repr(transparent)]
pub(crate) struct Xmm(u16);

impl Xmm {
    fn new(id: u16) -> Self {
        Self(id)
    }

    pub fn enc(&self) -> u64 {
        self.0 as u64 + 2
    }
}

pub(crate) type UsingXmm = bitvec::prelude::BitArr!(for 14, in u16);

///
/// Mode of linkage between stack slot and xmm registers.
///
#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum LinkMode {
    ///
    /// Linked to an xmm register and we can read and write.
    ///
    /// mutation of the corresponding xmm register (lazily) affects the stack slot.
    ///
    Xmm(Xmm),
    ///
    /// Linked to an xmm register but we can only read to keep consistency.
    ///
    Both(Xmm),
    ///
    /// No linkage with xmm regiter.
    ///
    Stack,
    ///
    /// Literal.
    ///
    Literal(Value),
    ///
    /// On R15 register.
    ///
    R15,
}

impl Codegen {
    pub(super) fn compile(
        &mut self,
        store: &Store,
        func_id: FuncId,
        self_value: Value,
        position: Option<BcPc>,
        entry_label: DestLabel,
    ) -> Vec<(BcIndex, usize)> {
        #[cfg(feature = "log-jit")]
        let now = std::time::Instant::now();

        self.jit.bind_label(entry_label);

        let func = store[func_id].as_ruby_func();
        let start_pos = func.get_pc_index(position);

        let mut ctx = JitContext::new(func, store, self, position.is_some(), self_value);
        for (loop_start, loop_end) in func.bb_info.loops() {
            let (backedge, exit) = ctx.analyse_loop(func, *loop_start, *loop_end);
            ctx.loop_backedges.insert(*loop_start, backedge);
            ctx.loop_exit.insert(*loop_start, (*loop_end, exit));
        }

        let bbctx = BBContext::new(&ctx);

        if let Some(pc) = position {
            // generate class guard of *self* for loop JIT
            // We must pass pc + 1 because pc (= LoopStart) cause an infinite loop.
            let side_exit = self.gen_deopt(pc + 1, &bbctx);
            monoasm!( &mut self.jit,
                movq rdi, [r14 - (LBP_SELF)];
            );
            self.guard_class_rdi(self_value.class(), side_exit);
        } else {
            // for method JIT, class of *self* is already checked in an entry stub.
            let pc = func.get_top_pc();
            self.prologue(pc);
        }

        #[cfg(feature = "jit-debug")]
        eprintln!("   new_branch_init: {}->{}", BcIndex(0), start_pos);
        let label = ctx.branch_label();
        ctx.branch_map.insert(
            start_pos,
            vec![BranchEntry {
                src_idx: BcIndex(0),
                bbctx,
                label,
                cont: true,
            }],
        );

        let bb_begin = func.bb_info.get_bb_id(start_pos);
        let bb_end = match func.bb_info.get_loop(bb_begin) {
            Some((a, b)) => {
                assert_eq!(a, bb_begin);
                b
            }
            None => BasicBlockId(func.bb_info.len() - 1),
        };

        for BasciBlockInfoEntry { begin, end, .. } in &func.bb_info[bb_begin..=bb_end] {
            self.compile_bb(store, func, &mut ctx, position, *begin, *end);
        }

        ctx.backedge_branches(func);
        self.gen_bridges(store, &mut ctx);
        let sourcemap = std::mem::take(&mut ctx.sourcemap);

        self.jit.finalize();
        #[cfg(any(feature = "jit-debug", feature = "log-jit"))]
        {
            self.jit.select_page(0);
            eprintln!("    total bytes(0):{:?}", self.jit.get_current());
            self.jit.select_page(1);
            eprintln!("    total bytes(1):{:?}", self.jit.get_current());
            self.jit.select_page(0);
        }
        #[cfg(feature = "log-jit")]
        {
            let elapsed = now.elapsed();
            eprintln!("<== finished compile. elapsed:{:?}", elapsed);
        }
        #[cfg(any(feature = "emit-asm", feature = "jit-debug"))]
        eprintln!("<== finished compile.");

        sourcemap
    }
}

macro_rules! load_store {
    ($reg: ident) => {
        paste! {
            ///
            /// store $reg to *reg*
            ///
            #[allow(dead_code)]
            pub(crate) fn [<store_ $reg>](&mut self, reg: impl Into<Option<SlotId>>) {
                let reg = reg.into();
                if let Some(reg) = reg {
                    monoasm!{ &mut self.jit,
                        movq [r14 - (conv(reg))], $reg;
                    }
                }
            }

            ///
            /// load *reg* to $reg
            ///
            #[allow(dead_code)]
            pub(crate) fn [<load_ $reg>](&mut self, reg: SlotId) {
                monoasm!( &mut self.jit,
                    movq $reg, [r14 - (conv(reg))];
                );
            }
        }
    };
}

enum CompileResult {
    Continue,
    Break,
    Exit,
    Recompile,
}

impl Codegen {
    load_store!(rax);
    load_store!(rdi);
    load_store!(rsi);
    load_store!(rcx);
    load_store!(r15);

    ///
    /// move xmm(*src*) to xmm(*dst*).
    ///
    fn xmm_mov(&mut self, src: Xmm, dst: Xmm) {
        if src != dst {
            monoasm!( &mut self.jit,
                movq  xmm(dst.enc()), xmm(src.enc());
            );
        }
    }

    ///
    /// swap xmm(*l*) and xmm(*r*).
    ///
    fn xmm_swap(&mut self, l: Xmm, r: Xmm) {
        monoasm!( &mut self.jit,
            movq  xmm0, xmm(l.enc());
            movq  xmm(l.enc()), xmm(r.enc());
            movq  xmm(r.enc()), xmm0;
        );
    }

    pub(crate) fn xmm_save(&mut self, using_xmm: UsingXmm) {
        if using_xmm.not_any() {
            return;
        }
        let len = using_xmm.count_ones();
        let sp_offset = (len + len % 2) * 8;
        monoasm!( &mut self.jit,
            subq rsp, (sp_offset);
        );
        let mut i = 0;
        for (x, b) in using_xmm.iter().enumerate() {
            if *b {
                monoasm!( &mut self.jit,
                    movq [rsp + (8 * i)], xmm(Xmm::new(x as _).enc());
                );
                i += 1;
            }
        }
    }

    pub(crate) fn xmm_restore(&mut self, using_xmm: UsingXmm) {
        if using_xmm.not_any() {
            return;
        }
        let len = using_xmm.count_ones();
        let sp_offset = (len + len % 2) * 8;
        let mut i = 0;
        for (x, b) in using_xmm.iter().enumerate() {
            if *b {
                monoasm!( &mut self.jit,
                    movq xmm(Xmm::new(x as _).enc()), [rsp + (8 * i)];
                );
                i += 1;
            }
        }
        monoasm!( &mut self.jit,
            addq rsp, (sp_offset);
        );
    }

    fn compile_bb(
        &mut self,
        store: &Store,
        func: &ISeqInfo,
        ctx: &mut JitContext,
        position: Option<BcPc>,
        bb_begin: BcIndex,
        bb_end: BcIndex,
    ) {
        self.jit.bind_label(ctx.inst_labels[&bb_begin]);
        let mut bbctx = if let Some(bbctx) = ctx.target_ctx.remove(&bb_begin) {
            bbctx
        } else if let Some(bbctx) = ctx.incoming_context(func, bb_begin) {
            self.gen_continuation_code(store, ctx);
            bbctx
        } else {
            #[cfg(feature = "jit-debug")]
            eprintln!("=== no entry");
            return;
        };

        let mut ir = AsmIr::new();
        let res =
            ctx.compile_bb_inner(&mut ir, store, func, &mut bbctx, position, bb_begin, bb_end);
        self.gen_code(store, ctx, ir);

        if res {
            return;
        }

        let next_idx = bb_end + 1;
        if func.bb_info.is_bb_head(next_idx) {
            let label = ctx.branch_label();
            ctx.new_continue(func, bb_end, next_idx, bbctx, label);
            if let Some(target_ctx) = ctx.incoming_context(func, next_idx) {
                self.gen_continuation_code(store, ctx);
                assert!(ctx.target_ctx.insert(next_idx, target_ctx).is_none());
            }
        }
    }

    fn recompile_and_deopt(&mut self, position: Option<BcPc>, deopt: DestLabel) {
        let recompile = self.jit.label();
        let dec = self.jit.label();
        let counter = self.jit.const_i32(RECOMPILE_COUNT);

        monoasm!( &mut self.jit,
            xorq rdi, rdi;
            cmpl [rip + counter], 0;
            jlt deopt;
            jeq recompile;
        dec:
            subl [rip + counter], 1;
            jmp deopt;
        );

        self.jit.select_page(1);
        monoasm!( &mut self.jit,
        recompile:
            movq rdi, rbx;
            movq rsi, r12;
        );
        if let Some(pc) = position {
            monoasm!( &mut self.jit,
                movq rdx, (pc.get_u64());
                movq rax, (exec_jit_partial_compile);
                call rax;
            );
        } else {
            monoasm!( &mut self.jit,
                movq rax, (exec_jit_recompile_method);
                call rax;
            );
        }
        monoasm!( &mut self.jit,
            xorq rdi, rdi;
            jmp dec;
        );
        self.jit.select_page(0);
        #[cfg(feature = "jit-debug")]
        eprintln!(" => deopt");
    }
}

impl Codegen {
    ///
    /// Handle error in JIT code.
    ///
    /// - pc: current PC
    ///
    /*pub(crate) fn jit_handle_error(&mut self, wb: &WriteBack, pc: BcPc) {
        let raise = self.entry_raise;
        if self.jit.get_page() == 0 {
            let error = self.jit.label();
            monoasm!( &mut self.jit,
                testq rax, rax; // Option<Value>
                jeq  error;
            );
            self.jit.select_page(1);
            monoasm!( &mut self.jit,
            error:
            );
            self.gen_write_back(wb);
            monoasm!( &mut self.jit,
                movq r13, ((pc + 1).get_u64());
                jmp  raise;
            );
            self.jit.select_page(0);
        } else {
            let cont = self.jit.label();
            monoasm!( &mut self.jit,
                testq rax, rax; // Option<Value>
                jne  cont;
            );
            self.gen_write_back(wb);
            monoasm!( &mut self.jit,
                movq r13, ((pc + 1).get_u64());
                jmp  raise;
            cont:
            );
        }
    }*/

    fn gen_handle_error(&mut self, pc: BcPc, wb: WriteBack, entry: DestLabel) {
        let raise = self.entry_raise;
        self.jit.select_page(1);
        monoasm!( &mut self.jit,
        entry:
        );
        self.gen_write_back(&wb);
        monoasm!( &mut self.jit,
            movq r13, ((pc + 1).get_u64());
            jmp  raise;
        );
        self.jit.select_page(0);
    }

    ///
    /// Generate a code which write back all xmm registers to corresponding stack slots.
    ///
    /// xmms are not deallocated.
    ///
    fn gen_write_back(&mut self, wb: &WriteBack) {
        for (freg, v) in &wb.xmm {
            self.xmm_to_both(*freg, v);
        }
        for (v, slot) in &wb.literal {
            self.literal_to_stack(*slot, *v);
        }
        if let Some(slot) = wb.r15 {
            self.store_r15(slot);
        }
    }

    fn xmm_to_both(&mut self, freg: Xmm, v: &[SlotId]) {
        if v.is_empty() {
            return;
        }
        #[cfg(feature = "jit-debug")]
        eprintln!("      wb: {:?}->{:?}", freg, v);
        let f64_to_val = self.f64_to_val;
        monoasm!( &mut self.jit,
            movq xmm0, xmm(freg.enc());
            call f64_to_val;
        );
        for reg in v {
            self.store_rax(*reg);
        }
    }

    fn literal_to_stack(&mut self, reg: SlotId, v: Value) {
        let i = v.id() as i64;
        if i32::try_from(i).is_ok() {
            monoasm! { &mut self.jit,
                movq [r14 - (conv(reg))], (v.id());
            }
        } else {
            monoasm! { &mut self.jit,
                movq rax, (v.id());
                movq [r14 - (conv(reg))], rax;
            }
        }
    }

    ///
    /// Get *DestLabel* for write-back and fallback to interpreter.
    ///
    /// ### in
    /// - rdi: deopt-reason:Value
    ///
    pub(crate) fn gen_deopt(&mut self, pc: BcPc, ctx: &BBContext) -> DestLabel {
        let entry = self.jit.label();
        let wb = ctx.get_write_back();
        self.gen_deopt_with_label(pc, &wb, entry);
        entry
    }

    ///
    /// Get *DestLabel* for fallback to interpreter. (without write-back)
    ///
    /// ### in
    /// - rdi: deopt-reason:Value
    ///
    fn gen_deopt_with_label(&mut self, pc: BcPc, wb: &WriteBack, entry: DestLabel) {
        assert_eq!(0, self.jit.get_page());
        self.jit.select_page(1);
        self.jit.bind_label(entry);
        self.gen_write_back(wb);
        monoasm!( &mut self.jit,
            movq r13, (pc.get_u64());
        );
        #[cfg(any(feature = "log-jit", feature = "profile"))]
        monoasm!( &mut self.jit,
            movq rcx, rdi; // the Value which caused this deopt.
            movq rdi, rbx;
            movq rsi, r12;
            movq rdx, r13;
            movq rax, (crate::globals::log_deoptimize);
            call rax;
        );
        let fetch = self.vm_fetch;
        monoasm!( &mut self.jit,
            jmp fetch;
        );
        self.jit.select_page(0);
    }
}

impl BBContext {
    ///
    /// Copy *src* to *dst*.
    ///
    fn copy_slot(&mut self, ir: &mut AsmIr, src: SlotId, dst: SlotId) {
        match self[src] {
            LinkMode::Xmm(x) | LinkMode::Both(x) => {
                self.link_xmm(dst, x);
            }
            LinkMode::Stack => {
                self.release(dst);
                ir.stack2reg(src, GP::Rax);
                ir.reg2stack(GP::Rax, dst);
            }
            LinkMode::Literal(v) => {
                self.link_literal(dst, v);
            }
            LinkMode::R15 => {
                ir.reg2stack(GP::R15, src);
                self.release(src);
                self.link_r15(dst);
            }
        }
    }
}
