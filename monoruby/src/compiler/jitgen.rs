use monoasm_macro::monoasm;
use paste::paste;
use ruruby_parse::CmpKind;

use crate::bytecodegen::{BcIndex, UnOpK};

pub(crate) use self::basic_block::BasicBlockInfo;
use self::basic_block::{BasciBlockInfoEntry, BasicBlockId};
use self::builtins::{object_send, object_send_splat};
use self::slot::Guarded;

use super::*;
use analysis::{ExitType, SlotInfo};
use asmir::*;
use slot::SlotContext;
use trace_ir::*;

pub mod analysis;
pub mod asmir;
mod basic_block;
mod guard;
mod init_method;
pub mod trace_ir;

//
// Just-in-time compiler module.
//

struct ContinuationInfo(BBContext, MergeContext, BytecodePtr);

impl ContinuationInfo {
    fn new(bb: BBContext, ctx: MergeContext, pc: BytecodePtr) -> Self {
        Self(bb, ctx, pc)
    }
}

///
/// Context for JIT compilation.
///
struct JitContext {
    ///
    /// Destination labels for each TraceIr.
    ///
    inst_labels: HashMap<BasicBlockId, DestLabel>,
    ///
    /// Destination labels for AsmLabels.
    ///
    asm_labels: Vec<Option<DestLabel>>,
    ///
    /// Basic block information.
    ///
    bb_scan: Vec<(ExitType, SlotInfo)>,
    ///
    /// Backedges to the loop head.
    ///
    loop_backedges: HashMap<BasicBlockId, SlotInfo>,
    ///
    /// Loop information.
    ///
    /// ### key
    /// the entry basic block of the loop.
    ///
    /// ### value
    /// (the last basic block, slot_info at the loop exit)
    ///
    loop_info: HashMap<BasicBlockId, (BasicBlockId, SlotInfo)>,
    ///
    /// Nested loop count.
    ///
    loop_count: usize,
    ///
    /// Flag whether this context is a loop.
    ///
    is_loop: bool,
    ///
    /// Map for bytecode position and branches.
    ///
    branch_map: HashMap<BcIndex, Vec<BranchEntry>>,
    ///
    /// Target `BBContext` for an each instruction.
    ///
    target_ctx: HashMap<BasicBlockId, BBContext>,
    ///
    /// Map for backward branches.
    ///
    backedge_map: HashMap<BasicBlockId, BackedgeInfo>,
    ///
    /// Number of slots.
    ///
    total_reg_num: usize,
    ///
    /// Number of local variables.
    ///
    local_num: usize,
    ///
    /// *self* for this loop/method.
    ///
    self_value: Value,
    ///
    /// Source map.
    ///
    sourcemap: Vec<(BcIndex, usize)>,
    ///
    /// Information for bridges.
    ///
    bridges: Vec<(AsmIr, AsmLabel, BasicBlockId)>,
    ///
    /// Information for continuation bridge.
    ///
    continuation_bridge: Option<(Option<ContinuationInfo>, AsmLabel)>,
    ///
    /// Information for opt_case table.
    ///
    opt_case: Vec<OptCaseAsmInfo>,
    ///
    /// Class version at compile time.
    ///
    #[allow(dead_code)]
    class_version: u32,
    ///
    /// BOP redefinition flag at compile time.
    ///
    #[allow(dead_code)]
    bop_redefine_flags: u32,
    ///
    /// Start offset of a machine code corresponding to the current basic block.
    ///
    #[cfg(feature = "emit-asm")]
    start_codepos: usize,
}

impl std::ops::Index<AsmLabel> for JitContext {
    type Output = DestLabel;
    fn index(&self, index: AsmLabel) -> &Self::Output {
        self.asm_labels[index.0].as_ref().unwrap()
    }
}

#[derive(Debug)]
struct BackedgeInfo {
    target_ctx: MergeContext,
    unused: Vec<SlotId>,
}

/*
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
*/

#[derive(Clone, Copy, Debug)]
struct AsmLabel(usize);

#[derive(Debug, Clone)]
struct OptCaseAsmInfo {
    id: OptCaseId,
    bb_pos: BcIndex,
    label_map: HashMap<BcIndex, AsmLabel>,
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
        for i in 0..func.bb_info.len() {
            let idx = BasicBlockId(i);
            inst_labels.insert(idx, codegen.jit.label());
        }
        let bb_scan = func.bb_info.init_bb_scan(func, store);

        let total_reg_num = func.total_reg_num();
        let local_num = func.local_num();
        Self {
            inst_labels,
            asm_labels: vec![],
            bb_scan,
            loop_backedges: HashMap::default(),
            loop_info: HashMap::default(),
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
            class_version: codegen.class_version(),
            bop_redefine_flags: codegen.bop_redefine_flags(),
            #[cfg(feature = "emit-asm")]
            start_codepos: codegen.jit.get_current(),
        }
    }

    fn asm_label(&mut self) -> AsmLabel {
        let label = AsmLabel(self.asm_labels.len());
        self.asm_labels.push(None);
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
        mut bb: BBContext,
        label: AsmLabel,
    ) {
        bb.sp = func.get_sp(src_idx);
        #[cfg(feature = "jit-debug")]
        eprintln!("   new_branch: [{:?}]{src_idx}->{dest}", bb.sp);
        self.branch_map.entry(dest).or_default().push(BranchEntry {
            src_idx,
            bb,
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
        mut bb: BBContext,
        label: AsmLabel,
    ) {
        bb.sp = func.get_sp(src_idx);
        #[cfg(feature = "jit-debug")]
        eprintln!("   new_continue:[{:?}] {src_idx}->{dest}", bb.sp);
        self.branch_map.entry(dest).or_default().push(BranchEntry {
            src_idx,
            bb,
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
        bb: &mut BBContext,
        bb_pos: BasicBlockId,
        unused: Vec<SlotId>,
    ) {
        bb.sp = func.get_sp(func.bb_info[bb_pos].begin);
        #[cfg(feature = "jit-debug")]
        eprintln!("   new_backedge:[{:?}] {:?}", bb.sp, bb_pos);
        self.backedge_map.insert(
            bb_pos,
            BackedgeInfo {
                target_ctx: MergeContext::new(bb),
                unused,
            },
        );
    }

    fn compile_bb(
        &mut self,
        ir: &mut AsmIr,
        store: &Store,
        func: &ISeqInfo,
        position: Option<BytecodePtr>,
        bbid: BasicBlockId,
    ) {
        ir.inst.push(AsmInst::DestLabel(self.inst_labels[&bbid]));
        let mut bb = if let Some(bb) = self.target_ctx.remove(&bbid) {
            bb
        } else if let Some(bb) = self.incoming_context(ir, func, bbid) {
            self.gen_continuation(ir);
            bb
        } else {
            #[cfg(feature = "jit-debug")]
            eprintln!("=== no entry");
            return;
        };

        let BasciBlockInfoEntry {
            begin: bb_begin,
            end: bb_end,
            ..
        } = func.bb_info[bbid];
        for bb_pos in bb_begin..=bb_end {
            ir.bc_index(bb_pos);
            bb.next_sp = func.get_sp(bb_pos);

            match self.compile_inst(ir, &mut bb, store, func, bb_pos) {
                CompileResult::Continue => {}
                CompileResult::Exit => return,
                CompileResult::Recompile => {
                    let pc = func.get_pc(bb_pos);
                    ir.recompile_and_deopt(&mut bb, pc, position);
                    return;
                }
                CompileResult::Break => break,
            }

            ir.clear(&mut bb);
            bb.sp = bb.next_sp;
        }

        let next_idx = bb_end + 1;
        if let Some(next_bbid) = func.bb_info.is_bb_head(next_idx) {
            let label = self.asm_label();
            self.new_continue(func, bb_end, next_idx, bb, label);
            if let Some(target_ctx) = self.incoming_context(ir, func, next_bbid) {
                self.gen_continuation(ir);
                assert!(self.target_ctx.insert(next_bbid, target_ctx).is_none());
            }
        }
    }

    fn gen_continuation(&mut self, ir: &mut AsmIr) {
        if let Some((data, entry)) = std::mem::take(&mut self.continuation_bridge) {
            ir.inst.push(AsmInst::Label(entry));
            if let Some(ContinuationInfo(from, to, pc)) = data {
                ir.write_back_for_target(from, &to, pc);
            }
        }
    }

    fn compile_inst(
        &mut self,
        ir: &mut AsmIr,
        bb: &mut BBContext,
        store: &Store,
        func: &ISeqInfo,
        bb_pos: BcIndex,
    ) -> CompileResult {
        let pc = func.get_pc(bb_pos);
        let trace_ir = pc.trace_ir(store);
        match trace_ir {
            TraceIr::InitMethod { .. } => {}
            TraceIr::LoopStart(_) => {
                self.loop_count += 1;
            }
            TraceIr::LoopEnd => {
                assert_ne!(0, self.loop_count);
                self.loop_count -= 1;
                if self.is_loop && self.loop_count == 0 {
                    ir.deopt(bb, pc);
                    return CompileResult::Break;
                }
            }
            TraceIr::Integer(dst, i) => {
                ir.store_literal(bb, dst, Value::i32(i));
            }
            TraceIr::Symbol(dst, id) => {
                ir.store_literal(bb, dst, Value::symbol(id));
            }
            TraceIr::Nil(dst) => {
                ir.store_literal(bb, dst, Value::nil());
            }
            TraceIr::Literal(dst, val) => {
                ir.unlink(bb, dst);
                if val.is_packed_value() || val.is_float() {
                    ir.store_literal(bb, dst, val);
                } else {
                    ir.deep_copy_lit(bb, val);
                    ir.reg2acc_guarded(bb, GP::Rax, dst, Guarded::from_literal(val));
                }
            }
            TraceIr::Array { dst, callid } => {
                let CallSiteInfo { args, pos_num, .. } = store[callid];
                ir.write_back_range(bb, args, pos_num as u16);
                ir.unlink(bb, dst);
                ir.new_array(bb, callid);
                ir.reg2acc_guarded(bb, GP::Rax, dst, Guarded::ArrayTy);
            }
            TraceIr::Lambda { dst, func_id } => {
                ir.unlink(bb, dst);
                ir.new_lambda(bb, func_id);
                ir.rax2acc(bb, dst);
            }
            TraceIr::Hash { dst, args, len } => {
                ir.write_back_range(bb, args, len * 2);
                ir.unlink(bb, dst);
                ir.new_hash(bb, args, len as _);
                ir.rax2acc(bb, dst);
            }
            TraceIr::Range {
                dst,
                start,
                end,
                exclude_end,
            } => {
                ir.write_back_slots(bb, &[start, end]);
                ir.unlink(bb, dst);
                ir.new_range(bb, pc, start, end, exclude_end);
                ir.rax2acc(bb, dst);
            }
            TraceIr::Index { dst, base, idx } => {
                if pc.classid1().is_none() || pc.classid2().is_none() {
                    return CompileResult::Recompile;
                }
                ir.index(bb, dst, base, idx, pc);
            }
            TraceIr::IndexAssign { src, base, idx } => {
                if pc.classid1().is_none() || pc.classid2().is_none() {
                    return CompileResult::Recompile;
                }
                ir.index_assign(bb, src, base, idx, pc);
            }
            TraceIr::LoadConst(dst, id) => {
                ir.unlink(bb, dst);

                if let ConstCache {
                    cached_version,
                    cached_base_class,
                    cached_value: Some(cached_val),
                } = store[id].cache
                {
                    let base_slot = store[id].base;
                    if let Some(slot) = base_slot {
                        if let Some(base_class) = cached_base_class {
                            ir.fetch_to_reg(bb, slot, GP::Rax);
                            let deopt = ir.new_deopt(bb, pc);
                            ir.inst.push(AsmInst::GuardBaseClass { base_class, deopt });
                        } else {
                            return CompileResult::Recompile;
                        }
                    }
                    let deopt = ir.new_deopt(bb, pc);
                    if let Some(f) = cached_val.try_float() {
                        let fdst = ir.store_new_both(bb, dst, Guarded::Float);
                        ir.inst.push(AsmInst::LoadFloatConstant {
                            fdst,
                            f,
                            cached_version,
                            deopt,
                        });
                        ir.reg2stack(GP::Rax, dst);
                    } else {
                        ir.inst.push(AsmInst::LoadGenericConstant {
                            cached_val,
                            cached_version,
                            deopt,
                        });
                        ir.rax2acc(bb, dst);
                    }
                } else {
                    return CompileResult::Recompile;
                }
            }
            TraceIr::StoreConst(src, id) => {
                ir.fetch_to_reg(bb, src, GP::Rax);
                let using_xmm = bb.get_using_xmm();
                ir.inst.push(AsmInst::StoreConstant { id, using_xmm });
            }
            TraceIr::BlockArgProxy(ret, outer) => {
                ir.unlink(bb, ret);
                ir.block_arg_proxy(ret, outer);
            }
            TraceIr::BlockArg(ret, outer) => {
                ir.unlink(bb, ret);
                ir.block_arg(bb, pc, ret, outer);
            }
            TraceIr::LoadIvar(ret, id, cached_class, cached_ivarid) => {
                if let Some(cached_class) = cached_class {
                    ir.load_ivar(bb, id, ret, cached_class, cached_ivarid);
                } else {
                    return CompileResult::Recompile;
                }
            }
            TraceIr::StoreIvar(src, id, cached_class, cached_ivarid) => {
                if let Some(cached_class) = cached_class {
                    ir.store_ivar(bb, id, src, pc, cached_class, cached_ivarid);
                } else {
                    return CompileResult::Recompile;
                }
            }
            TraceIr::LoadCvar { dst, name } => {
                ir.jit_load_cvar(bb, pc, name, dst);
            }
            TraceIr::CheckCvar { dst, name } => {
                ir.jit_check_cvar(bb, name, dst);
            }
            TraceIr::StoreCvar { src: val, name } => {
                ir.jit_store_cvar(bb, pc, name, val);
            }
            TraceIr::LoadGvar { dst, name } => {
                ir.jit_load_gvar(bb, name, dst);
            }
            TraceIr::StoreGvar { src: val, name } => {
                ir.jit_store_gvar(bb, name, val);
            }
            TraceIr::LoadSvar { dst, id } => {
                ir.unlink(bb, dst);
                ir.load_svar(bb, id);
                ir.rax2acc(bb, dst);
            }
            TraceIr::LoadDynVar(dst, src) => {
                ir.unlink(bb, dst);
                if !dst.is_self() {
                    ir.inst.push(AsmInst::LoadDynVar { src });
                    ir.rax2acc(bb, dst);
                }
            }
            TraceIr::StoreDynVar(dst, src) => {
                ir.fetch_to_reg(bb, src, GP::Rdi);
                ir.inst.push(AsmInst::StoreDynVar { dst, src: GP::Rdi });
            }
            TraceIr::BitNot { dst, src } => {
                if pc.classid1().is_none() {
                    return CompileResult::Recompile;
                }
                ir.fetch_to_reg(bb, src, GP::Rdi);
                ir.generic_unop(bb, pc, bitnot_value);
                ir.rax2acc(bb, dst);
            }
            TraceIr::Not { dst, src } => {
                ir.fetch_to_reg(bb, src, GP::Rdi);
                ir.inst.push(AsmInst::Not);
                ir.rax2acc(bb, dst);
            }
            TraceIr::FUnOp { kind, dst, src } => {
                let deopt = ir.new_deopt(bb, pc);
                let fsrc = ir.fetch_float_assume_float(bb, src, deopt);
                let dst = ir.xmm_write(bb, dst);
                ir.xmm_move(fsrc, dst);
                ir.inst.push(AsmInst::XmmUnOp { kind, dst });
            }
            TraceIr::IUnOp { kind, dst, src } => {
                ir.fetch_to_reg(bb, src, GP::Rdi);
                ir.generic_unop(bb, pc, kind.generic_func());
                ir.rax2acc(bb, dst);
            }
            TraceIr::UnOp { kind, dst, src } => {
                if pc.classid1().is_none() {
                    return CompileResult::Recompile;
                }
                ir.fetch_to_reg(bb, src, GP::Rdi);
                ir.generic_unop(bb, pc, kind.generic_func());
                ir.rax2acc(bb, dst);
            }
            TraceIr::IBinOp { kind, dst, mode } => {
                ir.gen_binop_integer(bb, pc, kind, dst, mode);
            }
            TraceIr::FBinOp { kind, dst, mode } => {
                let deopt = ir.new_deopt(bb, pc);
                let fmode = ir.fmode(&mode, bb, pc, deopt);
                if let Some(ret) = dst {
                    let dst = ir.xmm_write(bb, ret);
                    let using_xmm = bb.get_using_xmm();
                    ir.xmm_binop(kind, fmode, dst, using_xmm);
                }
            }
            TraceIr::BinOp {
                kind, dst, mode, ..
            } => {
                if pc.classid1().is_none() || pc.classid2().is_none() {
                    return CompileResult::Recompile;
                }
                ir.fetch_binary(bb, mode);
                ir.generic_binop(bb, pc, kind);
                ir.rax2acc(bb, dst);
            }
            TraceIr::Cmp(kind, ret, mode, false) => {
                if pc.classid1().is_none() || pc.classid2().is_none() {
                    return CompileResult::Recompile;
                }
                if mode.is_float_op(&pc) && kind != CmpKind::Cmp {
                    let deopt = ir.new_deopt(bb, pc);
                    let mode = ir.fmode(&mode, bb, pc, deopt);
                    ir.unlink(bb, ret);
                    ir.clear(bb);
                    ir.inst.push(AsmInst::FloatCmp { kind, mode });
                } else if mode.is_integer_op(&pc) {
                    ir.fetch_fixnum_binary(bb, pc, &mode);
                    ir.inst.push(AsmInst::IntegerCmp { kind, mode });
                } else {
                    ir.fetch_binary(bb, mode);
                    ir.generic_cmp(bb, pc, kind);
                }
                ir.rax2acc(bb, ret);
            }

            TraceIr::Cmp(kind, ret, mode, true) => {
                let index = bb_pos + 1;
                match (pc + 1).trace_ir(store) {
                    TraceIr::CondBr(_, disp, true, brkind) => {
                        let dest_idx = index + disp + 1;
                        let branch_dest = self.asm_label();
                        if mode.is_float_op(&pc) {
                            let deopt = ir.new_deopt(bb, pc);
                            let mode = ir.fmode(&mode, bb, pc, deopt);
                            ir.unlink(bb, ret);
                            ir.clear(bb);
                            ir.float_cmp_br(mode, kind, brkind, branch_dest);
                        } else if mode.is_integer_op(&pc) {
                            ir.fetch_fixnum_binary(bb, pc, &mode);
                            ir.unlink(bb, ret);
                            ir.clear(bb);
                            ir.integer_cmp_br(mode, kind, brkind, branch_dest);
                        } else {
                            ir.fetch_binary(bb, mode);
                            ir.unlink(bb, ret);
                            ir.clear(bb);
                            ir.generic_cmp(bb, pc, kind);
                            ir.inst.push(AsmInst::GenericCondBr {
                                brkind,
                                branch_dest,
                            });
                        }
                        self.new_branch(func, index, dest_idx, bb.clone(), branch_dest);
                    }
                    _ => unreachable!(),
                }
            }
            TraceIr::Mov(dst, src) => {
                ir.copy_slot(bb, src, dst);
            }
            TraceIr::ConcatStr(dst, arg, len) => {
                ir.write_back_range(bb, arg, len);
                ir.unlink(bb, dst);
                let error = ir.new_error(bb, pc);
                ir.concat_str(bb, arg, len);
                ir.handle_error(error);
                ir.rax2acc(bb, dst);
            }
            TraceIr::ConcatRegexp(dst, arg, len) => {
                ir.write_back_range(bb, arg, len);
                ir.unlink(bb, dst);
                let error = ir.new_error(bb, pc);
                ir.concat_regexp(bb, arg, len);
                ir.handle_error(error);
                ir.rax2acc(bb, dst);
            }
            TraceIr::ExpandArray {
                src,
                dst: (dst, len),
            } => {
                ir.fetch_to_reg(bb, src, GP::Rdi);
                for reg in dst.0..dst.0 + len {
                    ir.unlink(bb, SlotId(reg));
                }
                ir.expand_array(bb, dst, len);
            }
            TraceIr::AliasMethod { new, old } => {
                //ir.write_back_slots(bb, &[new, old]);
                ir.alias_method(bb, pc, new, old);
            }
            TraceIr::MethodCall { callid } | TraceIr::MethodCallBlock { callid } => {
                if let Some(fid) = pc.cached_fid()
                //&& self.class_version == (pc + 1).cached_version()
                {
                    if ir.gen_call(store, bb, fid, callid, pc).is_none() {
                        return CompileResult::Recompile;
                    }
                } else {
                    return CompileResult::Recompile;
                }
            }
            TraceIr::InlineCall { inline_id, callid } => {
                let recv = store[callid].recv;
                ir.fetch_to_reg(bb, recv, GP::Rdi);
                let (deopt, error) = ir.new_deopt_error(bb, pc);
                let using_xmm = bb.get_using_xmm();
                ir.guard_version(pc, using_xmm, deopt, error);
                (store.get_inline_info(inline_id).inline_gen)(ir, store, bb, callid, pc);
            }
            TraceIr::InlineObjectSend { callid, .. } => {
                let recv = store[callid].recv;
                ir.fetch_to_reg(bb, recv, GP::Rdi);
                let (deopt, error) = ir.new_deopt_error(bb, pc);
                let using_xmm = bb.get_using_xmm();
                ir.guard_version(pc, using_xmm, deopt, error);
                object_send(ir, store, bb, callid, pc);
            }
            TraceIr::InlineObjectSendSplat { callid, .. } => {
                let recv = store[callid].recv;
                ir.fetch_to_reg(bb, recv, GP::Rdi);
                let (deopt, error) = ir.new_deopt_error(bb, pc);
                let using_xmm = bb.get_using_xmm();
                ir.guard_version(pc, using_xmm, deopt, error);
                object_send_splat(ir, store, bb, callid, pc);
            }
            TraceIr::Yield { callid } => {
                let callinfo = &store[callid];
                let dst = callinfo.dst;
                ir.write_back_callargs(bb, &callinfo);
                ir.unlink(bb, dst);
                ir.writeback_acc(bb);
                let using_xmm = bb.get_using_xmm();
                let error = ir.new_error(bb, pc);
                let evict = ir.new_evict();
                ir.inst.push(AsmInst::Yield {
                    callid,
                    using_xmm,
                    error,
                    evict,
                });
                ir.rax2acc(bb, dst);
            }
            TraceIr::InlineCache => {}
            TraceIr::MethodDef { name, func_id } => {
                let using_xmm = bb.get_using_xmm();
                ir.inst.push(AsmInst::MethodDef {
                    name,
                    func_id,
                    using_xmm,
                });
                ir.check_bop(bb, pc);
            }
            TraceIr::SingletonMethodDef { obj, name, func_id } => {
                ir.write_back_slots(bb, &[obj]);
                let using_xmm = bb.get_using_xmm();
                ir.inst.push(AsmInst::SingletonMethodDef {
                    obj,
                    name,
                    func_id,
                    using_xmm,
                });
                ir.check_bop(bb, pc);
            }
            TraceIr::ClassDef {
                dst,
                base,
                superclass,
                name,
                func_id,
            } => {
                self.class_def(ir, bb, dst, base, superclass, name, func_id, false, pc);
            }
            TraceIr::ModuleDef {
                dst,
                base,
                name,
                func_id,
            } => {
                self.class_def(ir, bb, dst, base, None, name, func_id, true, pc);
            }
            TraceIr::SingletonClassDef { dst, base, func_id } => {
                self.singleton_class_def(ir, bb, dst, base, func_id, pc);
            }
            TraceIr::DefinedYield { dst } => {
                ir.write_back_slots(bb, &[dst]);
                let using_xmm = bb.get_using_xmm();
                ir.inst.push(AsmInst::DefinedYield { dst, using_xmm });
            }
            TraceIr::DefinedConst { dst, siteid } => {
                ir.write_back_slots(bb, &[dst]);
                let using_xmm = bb.get_using_xmm();
                ir.inst.push(AsmInst::DefinedConst {
                    dst,
                    siteid,
                    using_xmm,
                });
            }
            TraceIr::DefinedMethod { dst, recv, name } => {
                ir.write_back_slots(bb, &[dst, recv]);
                let using_xmm = bb.get_using_xmm();
                ir.inst.push(AsmInst::DefinedMethod {
                    dst,
                    recv,
                    name,
                    using_xmm,
                });
            }
            TraceIr::DefinedGvar { dst, name } => {
                ir.write_back_slots(bb, &[dst]);
                let using_xmm = bb.get_using_xmm();
                ir.inst.push(AsmInst::DefinedGvar {
                    dst,
                    name,
                    using_xmm,
                });
            }
            TraceIr::DefinedIvar { dst, name } => {
                ir.write_back_slots(bb, &[dst]);
                let using_xmm = bb.get_using_xmm();
                ir.inst.push(AsmInst::DefinedIvar {
                    dst,
                    name,
                    using_xmm,
                });
            }
            TraceIr::Ret(ret) => {
                ir.write_back_locals(bb);
                ir.fetch_to_reg(bb, ret, GP::Rax);
                ir.inst.push(AsmInst::Ret);
                return CompileResult::Exit;
            }
            TraceIr::MethodRet(ret) => {
                ir.write_back_locals(bb);
                ir.fetch_to_reg(bb, ret, GP::Rax);
                ir.inst.push(AsmInst::MethodRet(pc));
                return CompileResult::Exit;
            }
            TraceIr::Break(ret) => {
                ir.write_back_locals(bb);
                ir.fetch_to_reg(bb, ret, GP::Rax);
                ir.inst.push(AsmInst::Break);
                return CompileResult::Exit;
            }
            TraceIr::Raise(ret) => {
                ir.write_back_locals(bb);
                ir.fetch_to_reg(bb, ret, GP::Rax);
                ir.inst.push(AsmInst::Raise);
                return CompileResult::Exit;
            }
            TraceIr::EnsureEnd => {
                ir.write_back_locals(bb);
                ir.inst.push(AsmInst::EnsureEnd);
            }
            TraceIr::Br(disp) => {
                let next_idx = bb_pos + 1;
                let dest_idx = next_idx + disp;
                let branch_dest = self.asm_label();
                ir.inst.push(AsmInst::Br(branch_dest));
                self.new_branch(func, bb_pos, dest_idx, bb.clone(), branch_dest);
                return CompileResult::Exit;
            }
            TraceIr::CondBr(cond_, disp, false, brkind) => {
                let dest_idx = bb_pos + 1 + disp;
                let branch_dest = self.asm_label();
                ir.fetch_to_reg(bb, cond_, GP::Rax);
                ir.inst.push(AsmInst::CondBr(brkind, branch_dest));
                self.new_branch(func, bb_pos, dest_idx, bb.clone(), branch_dest);
            }
            TraceIr::NilBr(cond_, disp) => {
                let dest_idx = bb_pos + 1 + disp;
                let branch_dest = self.asm_label();
                ir.fetch_to_reg(bb, cond_, GP::Rax);
                ir.inst.push(AsmInst::NilBr(branch_dest));
                self.new_branch(func, bb_pos, dest_idx, bb.clone(), branch_dest);
            }
            TraceIr::CondBr(_, _, true, _) => {}
            TraceIr::CheckLocal(local, disp) => {
                let dest_idx = bb_pos + 1 + disp;
                let branch_dest = self.asm_label();
                ir.fetch_to_reg(bb, local, GP::Rax);
                ir.inst.push(AsmInst::CheckLocal(branch_dest));
                self.new_branch(func, bb_pos, dest_idx, bb.clone(), branch_dest);
            }
            TraceIr::OptCase { cond, optid } => {
                let OptCaseInfo {
                    min, max, offsets, ..
                } = &store[optid];
                let mut label_map = HashMap::default();
                for ofs in offsets {
                    let dest_idx = bb_pos + 1 + (*ofs as i32);
                    let branch_dest = self.asm_label();
                    label_map.insert(dest_idx, branch_dest);
                    self.new_branch(func, bb_pos, dest_idx, bb.clone(), branch_dest);
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

                let deopt = ir.new_deopt(bb, pc);
                ir.fetch_guard_fixnum(bb, cond, GP::Rdi, deopt);
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
    bb: BBContext,
    /// `DestLabel` for the destination basic block.
    label: AsmLabel,
    /// true if the branch is a continuation branch.
    /// 'continuation' means the destination is adjacent to the source basic block on the bytecode.
    cont: bool,
}

pub(crate) fn conv(reg: SlotId) -> i32 {
    reg.0 as i32 * 8 + LFP_SELF
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
    alias: Vec<(SlotId, Vec<SlotId>)>,
    r15: Option<SlotId>,
}

impl WriteBack {
    fn new(
        xmm: Vec<(Xmm, Vec<SlotId>)>,
        literal: Vec<(Value, SlotId)>,
        alias: Vec<(SlotId, Vec<SlotId>)>,
        r15: Option<SlotId>,
    ) -> Self {
        Self {
            xmm,
            literal,
            alias,
            r15,
        }
    }
}

///
/// Context of an each basic block.
///
#[derive(Debug, Clone)]
pub(crate) struct BBContext {
    /// state stack slots.
    slot_state: SlotContext,
    /// stack top register.
    sp: SlotId,
    next_sp: SlotId,
    /// *self* value
    self_value: Value,
}

impl std::ops::Deref for BBContext {
    type Target = SlotContext;
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
            slot_state: SlotContext::new(cc),
            sp: SlotId(cc.local_num as u16),
            next_sp: SlotId(cc.local_num as u16),
            self_value: cc.self_value,
        }
    }

    fn union(entries: &[BranchEntry]) -> MergeContext {
        let mut merge_ctx = MergeContext::new(&entries.last().unwrap().bb);
        for BranchEntry {
            src_idx: _src_idx,
            bb,
            label: _,
            ..
        } in entries.iter()
        {
            #[cfg(feature = "jit-debug")]
            eprintln!("  <-{:?}:[{:?}] {:?}", _src_idx, bb.sp, bb.slot_state);
            merge_ctx.union(bb);
        }
        #[cfg(feature = "jit-debug")]
        eprintln!("  union_entries: {:?}", &merge_ctx);
        merge_ctx
    }

    pub(crate) fn get_using_xmm(&self) -> UsingXmm {
        self.slot_state.get_using_xmm(self.sp)
    }

    pub(crate) fn get_write_back(&self) -> WriteBack {
        self.slot_state.get_write_back(self.sp)
    }

    pub(crate) fn get_register(&self) -> WriteBack {
        self.slot_state.get_register()
    }
}

pub(crate) type UsingXmm = bitvec::prelude::BitArr!(for 14, in u16);

///
/// Mode of linkage between stack slot and xmm registers.
///
#[derive(Debug, Clone, Copy, PartialEq, Default)]
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
    #[default]
    Stack,
    ///
    /// Alias of *SlotId*.
    ///
    Alias(SlotId),
    ///
    /// Literal.
    ///
    Literal(Value),
    ///
    /// On R15 register.
    ///
    R15,
}

#[derive(Debug, Clone)]
struct MergeContext(BBContext);

impl std::ops::Deref for MergeContext {
    type Target = BBContext;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::ops::DerefMut for MergeContext {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl MergeContext {
    fn new(bb: &BBContext) -> Self {
        MergeContext(bb.clone())
    }

    fn get(self) -> BBContext {
        self.0
    }

    fn remove_unused(&mut self, unused: &[SlotId]) {
        let mut ir = AsmIr::new();
        unused.iter().for_each(|reg| ir.unlink(&mut self.0, *reg));
    }
}

impl Codegen {
    pub(super) fn compile(
        &mut self,
        store: &Store,
        func_id: FuncId,
        self_value: Value,
        position: Option<BytecodePtr>,
        entry_label: DestLabel,
    ) -> Vec<(BcIndex, usize)> {
        #[cfg(feature = "jit-log")]
        let now = std::time::Instant::now();

        self.jit.bind_label(entry_label);

        let func = store[func_id].as_ruby_func();
        let start_pos = func.get_pc_index(position);

        let mut ctx = JitContext::new(func, store, self, position.is_some(), self_value);
        let mut ir = AsmIr::new();
        for (loop_start, loop_end) in func.bb_info.loops() {
            let (backedge, exit) = ctx.analyse_loop(func, *loop_start, *loop_end);
            ctx.loop_backedges.insert(*loop_start, backedge);
            ctx.loop_info.insert(*loop_start, (*loop_end, exit));
        }

        let bb = BBContext::new(&ctx);

        if let Some(pc) = position {
            // generate class guard of *self* for loop JIT
            // We must pass pc + 1 because pc (= LoopStart) cause an infinite loop.
            let side_exit = self.gen_deopt(pc + 1, &bb);
            monoasm!( &mut self.jit,
                movq rdi, [r14 - (LFP_SELF)];
            );
            self.guard_class_rdi(self_value.class(), side_exit);
        } else {
            // for method JIT, class of *self* is already checked in an entry stub.
            let pc = func.get_top_pc();
            self.prologue(store, pc);
        }

        #[cfg(feature = "jit-debug")]
        eprintln!("   new_branch_init: {}->{}", BcIndex(0), start_pos);
        let label = ctx.asm_label();
        ctx.branch_map.insert(
            start_pos,
            vec![BranchEntry {
                src_idx: BcIndex(0),
                bb,
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

        for bbid in bb_begin..=bb_end {
            ctx.compile_bb(&mut ir, store, func, position, bbid);
        }

        ctx.backedge_branches(func);
        self.gen_code(ir, store, &mut ctx);

        let sourcemap = std::mem::take(&mut ctx.sourcemap);

        self.jit.finalize();
        #[cfg(any(feature = "jit-debug", feature = "jit-log"))]
        {
            self.jit.select_page(0);
            eprintln!("    total bytes(0):{:?}", self.jit.get_current());
            self.jit.select_page(1);
            eprintln!("    total bytes(1):{:?}", self.jit.get_current());
            self.jit.select_page(0);
        }
        #[cfg(feature = "jit-log")]
        {
            let elapsed = now.elapsed();
            eprintln!("<== finished compile. elapsed:{:?}", elapsed);
            self.jit_compile_time += elapsed;
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

    fn recompile_and_deopt(&mut self, position: Option<BytecodePtr>, deopt: DestLabel) {
        let recompile = self.jit.label();
        let dec = self.jit.label();
        let counter = self.jit.data_i32(COUNT_DEOPT_RECOMPILE);

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
                movq rdx, (pc.as_ptr());
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
    fn gen_handle_error(&mut self, pc: BytecodePtr, wb: WriteBack, entry: DestLabel) {
        let raise = self.entry_raise;
        self.jit.select_page(1);
        monoasm!( &mut self.jit,
        entry:
        );
        self.gen_write_back(&wb);
        monoasm!( &mut self.jit,
            movq r13, ((pc + 1).as_ptr());
            jmp  raise;
        );
        self.jit.select_page(0);
    }

    ///
    /// Generate a code which write back all xmm registers to corresponding stack slots.
    ///
    /// xmms are not deallocated.
    ///
    pub(super) fn gen_write_back(&mut self, wb: &WriteBack) {
        for (xmm, v) in &wb.xmm {
            self.xmm_to_stack(*xmm, v);
        }
        for (v, slot) in &wb.literal {
            self.literal_to_stack(*slot, *v);
        }
        if let Some(slot) = wb.r15 {
            self.store_r15(slot);
        }
        for (origin, v) in &wb.alias {
            if !v.is_empty() {
                self.load_rax(*origin);
                for reg in v {
                    self.store_rax(*reg);
                }
            }
        }
    }

    ///
    /// Generate convert code from xmm to stack slots.
    ///
    /// ### out
    /// - rax: Value
    ///
    /// ### destroy
    /// - rcx
    ///
    fn xmm_to_stack(&mut self, xmm: Xmm, v: &[SlotId]) {
        if v.is_empty() {
            return;
        }
        #[cfg(feature = "jit-debug")]
        eprintln!("      wb: {:?}->{:?}", xmm, v);
        let f64_to_val = self.f64_to_val;
        monoasm!( &mut self.jit,
            movq xmm0, xmm(xmm.enc());
            call f64_to_val;
        );
        for reg in v {
            self.store_rax(*reg);
        }
    }

    ///
    /// ### destroy
    /// - rax
    ///
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
    pub(crate) fn gen_deopt(&mut self, pc: BytecodePtr, bb: &BBContext) -> DestLabel {
        let entry = self.jit.label();
        let wb = bb.get_write_back();
        self.gen_deopt_with_label(pc, &wb, entry);
        entry
    }

    ///
    /// Get *DestLabel* for fallback to interpreter by deoptimization.
    ///
    /// ### in
    /// - rdi: deopt-reason:Value
    ///
    fn gen_deopt_with_label(&mut self, pc: BytecodePtr, wb: &WriteBack, entry: DestLabel) {
        self.side_exit_with_label(pc, wb, entry, false)
    }

    ///
    /// Get *DestLabel* for fallback to interpreter by immediate eviction.
    ///
    fn gen_evict_with_label(&mut self, pc: BytecodePtr, wb: &WriteBack, entry: DestLabel) {
        self.side_exit_with_label(pc, wb, entry, true)
    }

    ///
    /// Get *DestLabel* for fallback to interpreter.
    ///
    /// ### in
    /// - rdi: deopt-reason:Value
    ///
    fn side_exit_with_label(
        &mut self,
        pc: BytecodePtr,
        wb: &WriteBack,
        entry: DestLabel,
        _is_evict: bool,
    ) {
        assert_eq!(0, self.jit.get_page());
        self.jit.select_page(1);
        self.jit.bind_label(entry);
        self.gen_write_back(wb);
        monoasm!( &mut self.jit,
            movq r13, (pc.as_ptr());
        );

        #[cfg(any(feature = "deopt", feature = "profile"))]
        {
            if _is_evict {
                monoasm!( &mut self.jit,
                    movq rcx, (Value::symbol_from_str("__immediate_evict").id());
                );
            } else {
                monoasm!( &mut self.jit,
                    movq rcx, rdi; // the Value which caused this deopt.
                );
            }
            monoasm!( &mut self.jit,
                movq rdi, rbx;
                movq rsi, r12;
                movq rdx, r13;
                movq rax, (crate::globals::log_deoptimize);
                call rax;
            );
        }
        let fetch = self.vm_fetch;
        monoasm!( &mut self.jit,
            jmp fetch;
        );
        self.jit.select_page(0);
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn float_test() {
        let gen = Codegen::new(false);

        let from_f64_entry = gen.jit.get_label_address(gen.f64_to_val);
        let from_f64: fn(f64) -> Value = unsafe { std::mem::transmute(from_f64_entry.as_ptr()) };

        for lhs in [
            0.0,
            4.2,
            35354354354.2135365,
            -3535354345111.5696876565435432,
            f64::MAX,
            f64::MAX / 10.0,
            f64::MIN * 10.0,
            f64::NAN,
        ] {
            let v = from_f64(lhs);
            let rhs = match v.unpack() {
                RV::Float(f) => f,
                _ => panic!(),
            };
            if lhs.is_nan() {
                assert!(rhs.is_nan());
            } else {
                assert_eq!(lhs, rhs);
            }
        }
    }

    #[test]
    fn float_test2() {
        let mut gen = Codegen::new(false);

        let panic = gen.entry_panic;
        let assume_int_to_f64 = gen.jit.label();
        let x = Xmm(0);
        monoasm!(&mut gen.jit,
        assume_int_to_f64:
            pushq rbp;
        );
        gen.integer_val_to_f64(GP::Rdi, x, panic);
        monoasm!(&mut gen.jit,
            movq xmm0, xmm(x.enc());
            popq rbp;
            ret;
        );
        gen.jit.finalize();
        let int_to_f64_entry = gen.jit.get_label_address(assume_int_to_f64);

        let int_to_f64: fn(Value) -> f64 =
            unsafe { std::mem::transmute(int_to_f64_entry.as_ptr()) };
        assert_eq!(143.0, int_to_f64(Value::integer(143)));
        assert_eq!(14354813558.0, int_to_f64(Value::integer(14354813558)));
        assert_eq!(-143.0, int_to_f64(Value::integer(-143)));
    }
}
