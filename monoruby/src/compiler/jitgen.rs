use monoasm_macro::monoasm;
use paste::paste;
use ruruby_parse::CmpKind;

use crate::bytecodegen::{BcIndex, UnOpK};

pub(crate) use self::basic_block::{BasciBlockInfoEntry, BasicBlockId, BasicBlockInfo};
use self::builtins::object_send_splat;
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

#[derive(Debug)]
struct BackedgeInfo {
    target_ctx: MergeContext,
    unused: Vec<SlotId>,
}

struct ContinuationInfo {
    from: BBContext,
    to: MergeContext,
    pc: BytecodePtr,
}

impl ContinuationInfo {
    fn new(from: BBContext, to: MergeContext, pc: BytecodePtr) -> Self {
        Self { from, to, pc }
    }
}

///
/// Context for JIT compilation.
///
struct JitContext {
    ///
    /// Destination labels for each BasicBlock.
    ///
    basic_block_labels: HashMap<BasicBlockId, DestLabel>,
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
    branch_map: HashMap<BasicBlockId, Vec<BranchEntry>>,
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
    bridges: Vec<(AsmIr, DestLabel, BasicBlockId)>,
    ///
    /// Information for continuation bridge.
    ///
    continuation_bridge: Option<(Option<ContinuationInfo>, DestLabel)>,
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
        let mut basic_block_labels = HashMap::default();
        for i in 0..func.bb_info.len() {
            let idx = BasicBlockId(i);
            basic_block_labels.insert(idx, codegen.jit.label());
        }
        let bb_scan = func.bb_info.init_bb_scan(func, store);

        let total_reg_num = func.total_reg_num();
        let local_num = func.local_num();
        Self {
            basic_block_labels,
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
            class_version: codegen.class_version(),
            bop_redefine_flags: codegen.bop_redefine_flags(),
            #[cfg(feature = "emit-asm")]
            start_codepos: codegen.jit.get_current(),
        }
    }

    ///
    /// Add new branch from *src_idx* to *dest* with the context *bbctx*.
    ///
    fn new_branch(
        &mut self,
        func: &ISeqInfo,
        src_idx: BcIndex,
        dest: BasicBlockId,
        mut bbctx: BBContext,
        branch_dest: DestLabel,
    ) {
        bbctx.sp = func.get_sp(src_idx);
        #[cfg(feature = "jit-debug")]
        eprintln!("   new_branch: [{:?}]{src_idx}->{:?}", bbctx.sp, dest);
        self.branch_map.entry(dest).or_default().push(BranchEntry {
            src_idx,
            bbctx,
            branch_dest,
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
        dest: BasicBlockId,
        mut bbctx: BBContext,
        branch_dest: DestLabel,
    ) {
        bbctx.sp = func.get_sp(src_idx);
        #[cfg(feature = "jit-debug")]
        eprintln!("   new_continue:[{:?}] {src_idx}->{:?}", bbctx.sp, dest);
        self.branch_map.entry(dest).or_default().push(BranchEntry {
            src_idx,
            bbctx,
            branch_dest,
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
        codegen: &mut Codegen,
        store: &Store,
        func: &ISeqInfo,
        position: Option<BytecodePtr>,
        bbid: BasicBlockId,
    ) -> AsmIr {
        let mut ir = AsmIr::new();
        ir.inst.push(AsmInst::Label(self.basic_block_labels[&bbid]));
        let mut bbctx = if let Some(bb) = self.target_ctx.remove(&bbid) {
            bb
        } else if let Some(bb) = self.incoming_context(&mut ir, func, bbid) {
            self.gen_continuation(&mut ir);
            bb
        } else {
            #[cfg(feature = "jit-debug")]
            eprintln!("=== no entry");
            return ir;
        };

        let BasciBlockInfoEntry { begin, end, .. } = func.bb_info[bbid];
        for bc_pos in begin..=end {
            ir.bc_index(bc_pos);
            bbctx.next_sp = func.get_sp(bc_pos);

            match self.compile_inst(codegen, &mut ir, &mut bbctx, store, func, bc_pos) {
                CompileResult::Continue => {}
                CompileResult::Exit => return ir,
                CompileResult::Recompile => {
                    let pc = func.get_pc(bc_pos);
                    ir.recompile_and_deopt(&mut bbctx, pc, position);
                    return ir;
                }
                CompileResult::Break => break,
            }

            ir.clear(&mut bbctx);
            bbctx.sp = bbctx.next_sp;
        }

        let next_idx = end + 1;
        if let Some(next_bbid) = func.bb_info.is_bb_head(next_idx) {
            let label = codegen.jit.label();
            self.new_continue(func, end, next_bbid, bbctx, label);
            if let Some(target_ctx) = self.incoming_context(&mut ir, func, next_bbid) {
                self.gen_continuation(&mut ir);
                assert!(self.target_ctx.insert(next_bbid, target_ctx).is_none());
            }
        }
        ir
    }

    fn compile_inst(
        &mut self,
        codegen: &mut Codegen,
        ir: &mut AsmIr,
        bbctx: &mut BBContext,
        store: &Store,
        func: &ISeqInfo,
        bc_pos: BcIndex,
    ) -> CompileResult {
        let pc = func.get_pc(bc_pos);
        let trace_ir = func.trace_ir(store, bc_pos);
        match trace_ir {
            TraceIr::InitMethod { .. } => {}
            TraceIr::LoopStart { .. } => {
                self.loop_count += 1;
            }
            TraceIr::LoopEnd => {
                assert_ne!(0, self.loop_count);
                self.loop_count -= 1;
                if self.is_loop && self.loop_count == 0 {
                    ir.deopt(bbctx, pc);
                    return CompileResult::Break;
                }
            }
            TraceIr::Integer(dst, i) => {
                ir.store_concrete_value(bbctx, dst, Value::i32(i));
            }
            TraceIr::Symbol(dst, id) => {
                ir.store_concrete_value(bbctx, dst, Value::symbol(id));
            }
            TraceIr::Nil(dst) => {
                ir.store_concrete_value(bbctx, dst, Value::nil());
            }
            TraceIr::Literal(dst, val) => {
                ir.unlink(bbctx, dst);
                if val.is_packed_value() || val.is_float() {
                    ir.store_concrete_value(bbctx, dst, val);
                } else {
                    ir.deep_copy_lit(bbctx, val);
                    ir.reg2acc_guarded(bbctx, GP::Rax, dst, Guarded::from_concrete_value(val));
                }
            }
            TraceIr::Array { dst, callid } => {
                let CallSiteInfo { args, pos_num, .. } = store[callid];
                ir.write_back_range(bbctx, args, pos_num as u16);
                ir.unlink(bbctx, dst);
                ir.new_array(bbctx, callid);
                ir.reg2acc_guarded(bbctx, GP::Rax, dst, Guarded::ArrayTy);
            }
            TraceIr::Lambda { dst, func_id } => {
                ir.unlink(bbctx, dst);
                ir.new_lambda(bbctx, func_id);
                ir.rax2acc(bbctx, dst);
            }
            TraceIr::Hash { dst, args, len } => {
                ir.write_back_range(bbctx, args, len * 2);
                ir.unlink(bbctx, dst);
                ir.new_hash(bbctx, args, len as _);
                ir.rax2acc(bbctx, dst);
            }
            TraceIr::Range {
                dst,
                start,
                end,
                exclude_end,
            } => {
                ir.write_back_slots(bbctx, &[start, end]);
                ir.unlink(bbctx, dst);
                ir.new_range(bbctx, pc, start, end, exclude_end);
                ir.rax2acc(bbctx, dst);
            }
            TraceIr::ArrayIndex { dst, base, idx } => {
                ir.index(
                    bbctx,
                    dst,
                    base,
                    idx,
                    Some(ARRAY_CLASS),
                    Some(INTEGER_CLASS),
                    pc,
                );
            }
            TraceIr::Index {
                dst,
                base,
                idx,
                base_class,
                idx_class,
            } => {
                if base_class.is_none() || idx_class.is_none() {
                    return CompileResult::Recompile;
                }
                ir.index(bbctx, dst, base, idx, base_class, idx_class, pc);
            }
            TraceIr::ArrayIndexAssign { src, base, idx } => {
                ir.index_assign(
                    bbctx,
                    src,
                    base,
                    idx,
                    Some(ARRAY_CLASS),
                    Some(INTEGER_CLASS),
                    pc,
                );
            }
            TraceIr::IndexAssign {
                src,
                base,
                idx,
                base_class,
                idx_class,
            } => {
                if base_class.is_none() || idx_class.is_none() {
                    return CompileResult::Recompile;
                }
                ir.index_assign(bbctx, src, base, idx, base_class, idx_class, pc);
            }
            TraceIr::LoadConst(dst, id) => {
                ir.unlink(bbctx, dst);

                if let ConstCache {
                    cached_version,
                    cached_base_class,
                    cached_value: Some(cached_val),
                } = store[id].cache
                {
                    let base_slot = store[id].base;
                    if let Some(slot) = base_slot {
                        if let Some(base_class) = cached_base_class {
                            ir.fetch_to_reg(bbctx, slot, GP::Rax);
                            let deopt = ir.new_deopt(bbctx, pc);
                            ir.inst.push(AsmInst::GuardBaseClass { base_class, deopt });
                        } else {
                            return CompileResult::Recompile;
                        }
                    }
                    let deopt = ir.new_deopt(bbctx, pc);
                    if let Some(f) = cached_val.try_float() {
                        let fdst = ir.store_new_both(bbctx, dst, Guarded::Float);
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
                        ir.rax2acc(bbctx, dst);
                    }
                } else {
                    return CompileResult::Recompile;
                }
            }
            TraceIr::StoreConst(src, id) => {
                ir.fetch_to_reg(bbctx, src, GP::Rax);
                let using_xmm = bbctx.get_using_xmm();
                ir.inst.push(AsmInst::StoreConstant { id, using_xmm });
            }
            TraceIr::BlockArgProxy(ret, outer) => {
                ir.unlink(bbctx, ret);
                ir.block_arg_proxy(ret, outer);
            }
            TraceIr::BlockArg(ret, outer) => {
                ir.unlink(bbctx, ret);
                ir.block_arg(bbctx, pc, ret, outer);
            }
            TraceIr::LoadIvar(ret, id, cached_class, cached_ivarid) => {
                if let Some(cached_class) = cached_class {
                    ir.load_ivar(bbctx, id, ret, cached_class, cached_ivarid);
                } else {
                    return CompileResult::Recompile;
                }
            }
            TraceIr::StoreIvar(src, id, cached_class, cached_ivarid) => {
                if let Some(cached_class) = cached_class {
                    ir.store_ivar(bbctx, id, src, pc, cached_class, cached_ivarid);
                } else {
                    return CompileResult::Recompile;
                }
            }
            TraceIr::LoadCvar { dst, name } => {
                ir.jit_load_cvar(bbctx, pc, name, dst);
            }
            TraceIr::CheckCvar { dst, name } => {
                ir.jit_check_cvar(bbctx, name, dst);
            }
            TraceIr::StoreCvar { src: val, name } => {
                ir.jit_store_cvar(bbctx, pc, name, val);
            }
            TraceIr::LoadGvar { dst, name } => {
                ir.jit_load_gvar(bbctx, name, dst);
            }
            TraceIr::StoreGvar { src: val, name } => {
                ir.jit_store_gvar(bbctx, name, val);
            }
            TraceIr::LoadSvar { dst, id } => {
                ir.unlink(bbctx, dst);
                ir.load_svar(bbctx, id);
                ir.rax2acc(bbctx, dst);
            }
            TraceIr::LoadDynVar(dst, src) => {
                ir.unlink(bbctx, dst);
                if !dst.is_self() {
                    ir.inst.push(AsmInst::LoadDynVar { src });
                    ir.rax2acc(bbctx, dst);
                }
            }
            TraceIr::StoreDynVar(dst, src) => {
                ir.fetch_to_reg(bbctx, src, GP::Rdi);
                ir.inst.push(AsmInst::StoreDynVar { dst, src: GP::Rdi });
            }
            TraceIr::BitNot {
                dst,
                src,
                src_class,
            } => {
                if src_class.is_none() {
                    return CompileResult::Recompile;
                }
                ir.fetch_to_reg(bbctx, src, GP::Rdi);
                ir.generic_unop(bbctx, pc, bitnot_value);
                ir.rax2acc(bbctx, dst);
            }
            TraceIr::Not { dst, src, .. } => {
                if bbctx.is_truthy(src) {
                    ir.store_concrete_value(bbctx, dst, Value::bool(false));
                } else if bbctx.is_falsy(src) {
                    ir.store_concrete_value(bbctx, dst, Value::bool(true));
                } else {
                    ir.fetch_to_reg(bbctx, src, GP::Rdi);
                    ir.inst.push(AsmInst::Not);
                    ir.rax2acc(bbctx, dst);
                }
            }
            TraceIr::FUnOp { kind, dst, src } => {
                let deopt = ir.new_deopt(bbctx, pc);
                let fsrc = ir.fetch_float_assume_float(bbctx, src, deopt);
                let dst = ir.xmm_write(bbctx, dst);
                ir.xmm_move(fsrc, dst);
                ir.inst.push(AsmInst::XmmUnOp { kind, dst });
            }
            TraceIr::IUnOp { kind, dst, src } => {
                ir.fetch_to_reg(bbctx, src, GP::Rdi);
                ir.generic_unop(bbctx, pc, kind.generic_func());
                ir.rax2acc(bbctx, dst);
            }
            TraceIr::UnOp {
                kind,
                dst,
                src,
                src_class,
            } => {
                if src_class.is_none() {
                    return CompileResult::Recompile;
                }
                ir.fetch_to_reg(bbctx, src, GP::Rdi);
                ir.generic_unop(bbctx, pc, kind.generic_func());
                ir.rax2acc(bbctx, dst);
            }
            TraceIr::IBinOp {
                kind, dst, mode, ..
            } => {
                ir.gen_binop_integer(bbctx, pc, kind, dst, mode);
            }
            TraceIr::FBinOp {
                kind,
                dst,
                mode,
                lhs_class,
                rhs_class,
            } => {
                let deopt = ir.new_deopt(bbctx, pc);
                let fmode = ir.fmode(&mode, bbctx, lhs_class, rhs_class, deopt);
                if let Some(ret) = dst {
                    let dst = ir.xmm_write(bbctx, ret);
                    let using_xmm = bbctx.get_using_xmm();
                    ir.xmm_binop(kind, fmode, dst, using_xmm);
                }
            }
            TraceIr::BinOp {
                kind,
                dst,
                mode,
                lhs_class,
                rhs_class,
            } => {
                if lhs_class.is_none() || rhs_class.is_none() {
                    return CompileResult::Recompile;
                }
                ir.fetch_binary(bbctx, mode);
                ir.generic_binop(bbctx, pc, kind);
                ir.rax2acc(bbctx, dst);
            }
            TraceIr::FCmp {
                kind,
                dst,
                mode,
                lhs_class,
                rhs_class,
            } => {
                if kind != CmpKind::Cmp {
                    let deopt = ir.new_deopt(bbctx, pc);
                    let mode = ir.fmode(&mode, bbctx, lhs_class, rhs_class, deopt);
                    ir.unlink(bbctx, dst);
                    ir.clear(bbctx);
                    ir.inst.push(AsmInst::FloatCmp { kind, mode });
                } else {
                    ir.fetch_binary(bbctx, mode);
                    ir.generic_cmp(bbctx, pc, kind);
                }
                ir.rax2acc(bbctx, dst);
            }
            TraceIr::ICmp { kind, dst, mode } => {
                ir.fetch_fixnum_binary(bbctx, pc, &mode);
                ir.inst.push(AsmInst::IntegerCmp { kind, mode });
                ir.rax2acc(bbctx, dst);
            }
            TraceIr::Cmp {
                kind,
                dst,
                mode,
                lhs_class,
                rhs_class,
            } => {
                if lhs_class.is_none() || rhs_class.is_none() {
                    return CompileResult::Recompile;
                }
                ir.fetch_binary(bbctx, mode);
                ir.generic_cmp(bbctx, pc, kind);
                ir.rax2acc(bbctx, dst);
            }
            TraceIr::FCmpBr {
                kind,
                dst,
                mode,
                lhs_class,
                rhs_class,
                dest,
                brkind,
            } => {
                let index = bc_pos + 1;
                let branch_dest = codegen.jit.label();
                let deopt = ir.new_deopt(bbctx, pc);
                let mode = ir.fmode(&mode, bbctx, lhs_class, rhs_class, deopt);
                ir.unlink(bbctx, dst);
                ir.clear(bbctx);
                ir.float_cmp_br(mode, kind, brkind, branch_dest);
                self.new_branch(func, index, dest, bbctx.clone(), branch_dest);
            }
            TraceIr::ICmpBr {
                kind,
                dst,
                mode,
                dest,
                brkind,
            } => {
                let index = bc_pos + 1;
                let branch_dest = codegen.jit.label();
                ir.fetch_fixnum_binary(bbctx, pc, &mode);
                ir.unlink(bbctx, dst);
                ir.clear(bbctx);
                ir.integer_cmp_br(mode, kind, brkind, branch_dest);
                self.new_branch(func, index, dest, bbctx.clone(), branch_dest);
            }
            TraceIr::CmpBr {
                kind,
                dst,
                mode,
                dest,
                brkind,
                ..
            } => {
                let index = bc_pos + 1;
                let branch_dest = codegen.jit.label();
                ir.fetch_binary(bbctx, mode);
                ir.unlink(bbctx, dst);
                ir.clear(bbctx);
                ir.generic_cmp(bbctx, pc, kind);
                ir.inst.push(AsmInst::GenericCondBr {
                    brkind,
                    branch_dest,
                });
                self.new_branch(func, index, dest, bbctx.clone(), branch_dest);
            }
            TraceIr::Mov(dst, src) => {
                ir.copy_slot(bbctx, src, dst);
            }
            TraceIr::ConcatStr(dst, arg, len) => {
                ir.write_back_range(bbctx, arg, len);
                ir.unlink(bbctx, dst);
                let error = ir.new_error(bbctx, pc);
                ir.concat_str(bbctx, arg, len);
                ir.handle_error(error);
                ir.rax2acc(bbctx, dst);
            }
            TraceIr::ConcatRegexp(dst, arg, len) => {
                ir.write_back_range(bbctx, arg, len);
                ir.unlink(bbctx, dst);
                let error = ir.new_error(bbctx, pc);
                ir.concat_regexp(bbctx, arg, len);
                ir.handle_error(error);
                ir.rax2acc(bbctx, dst);
            }
            TraceIr::ExpandArray {
                src,
                dst: (dst, len),
            } => {
                ir.fetch_to_reg(bbctx, src, GP::Rdi);
                for reg in dst.0..dst.0 + len {
                    ir.unlink(bbctx, SlotId(reg));
                }
                ir.expand_array(bbctx, dst, len);
            }
            TraceIr::AliasMethod { new, old } => {
                //ir.write_back_slots(bb, &[new, old]);
                ir.alias_method(bbctx, pc, new, old);
            }
            TraceIr::MethodCall {
                callid,
                recv_class,
                fid,
                version,
            }
            | TraceIr::MethodCallBlock {
                callid,
                recv_class,
                fid,
                version,
            } => {
                if let Some(fid) = fid {
                    let recv_class = recv_class.unwrap();
                    if store[callid].block_fid.is_none()
                        && let Some(info) = store.inline_info.get_inline(fid)
                    {
                        let is_simple = store[callid].is_simple();
                        if fid == OBJECT_SEND_FUNCID && store[callid].object_send_single_splat() {
                            let f = object_send_splat;
                            self.gen_inline_call(
                                ir, store, bbctx, f, fid, callid, recv_class, version, pc,
                            );
                        } else if is_simple {
                            let f = &info.inline_gen;
                            self.gen_inline_call(
                                ir, store, bbctx, f, fid, callid, recv_class, version, pc,
                            );
                            //}
                        } else {
                            if ir
                                .gen_call(store, bbctx, fid, recv_class, version, callid, pc)
                                .is_none()
                            {
                                return CompileResult::Recompile;
                            }
                        }
                    } else {
                        if ir
                            .gen_call(store, bbctx, fid, recv_class, version, callid, pc)
                            .is_none()
                        {
                            return CompileResult::Recompile;
                        }
                    }
                } else {
                    return CompileResult::Recompile;
                }
            }
            TraceIr::Yield { callid } => {
                let callinfo = &store[callid];
                let dst = callinfo.dst;
                ir.write_back_callargs(bbctx, &callinfo);
                ir.unlink(bbctx, dst);
                ir.writeback_acc(bbctx);
                let using_xmm = bbctx.get_using_xmm();
                let error = ir.new_error(bbctx, pc);
                let evict = ir.new_evict();
                ir.inst.push(AsmInst::Yield {
                    callid,
                    using_xmm,
                    error,
                    evict,
                });
                ir.rax2acc(bbctx, dst);
            }
            TraceIr::InlineCache => {}
            TraceIr::MethodDef { name, func_id } => {
                let using_xmm = bbctx.get_using_xmm();
                ir.inst.push(AsmInst::MethodDef {
                    name,
                    func_id,
                    using_xmm,
                });
                ir.check_bop(bbctx, pc);
            }
            TraceIr::SingletonMethodDef { obj, name, func_id } => {
                ir.write_back_slots(bbctx, &[obj]);
                let using_xmm = bbctx.get_using_xmm();
                ir.inst.push(AsmInst::SingletonMethodDef {
                    obj,
                    name,
                    func_id,
                    using_xmm,
                });
                ir.check_bop(bbctx, pc);
            }
            TraceIr::ClassDef {
                dst,
                base,
                superclass,
                name,
                func_id,
            } => {
                self.class_def(ir, bbctx, dst, base, superclass, name, func_id, false, pc);
            }
            TraceIr::ModuleDef {
                dst,
                base,
                name,
                func_id,
            } => {
                self.class_def(ir, bbctx, dst, base, None, name, func_id, true, pc);
            }
            TraceIr::SingletonClassDef { dst, base, func_id } => {
                self.singleton_class_def(ir, bbctx, dst, base, func_id, pc);
            }
            TraceIr::DefinedYield { dst } => {
                ir.write_back_slots(bbctx, &[dst]);
                let using_xmm = bbctx.get_using_xmm();
                ir.inst.push(AsmInst::DefinedYield { dst, using_xmm });
            }
            TraceIr::DefinedConst { dst, siteid } => {
                ir.write_back_slots(bbctx, &[dst]);
                let using_xmm = bbctx.get_using_xmm();
                ir.inst.push(AsmInst::DefinedConst {
                    dst,
                    siteid,
                    using_xmm,
                });
            }
            TraceIr::DefinedMethod { dst, recv, name } => {
                ir.write_back_slots(bbctx, &[dst, recv]);
                let using_xmm = bbctx.get_using_xmm();
                ir.inst.push(AsmInst::DefinedMethod {
                    dst,
                    recv,
                    name,
                    using_xmm,
                });
            }
            TraceIr::DefinedGvar { dst, name } => {
                ir.write_back_slots(bbctx, &[dst]);
                let using_xmm = bbctx.get_using_xmm();
                ir.inst.push(AsmInst::DefinedGvar {
                    dst,
                    name,
                    using_xmm,
                });
            }
            TraceIr::DefinedIvar { dst, name } => {
                ir.write_back_slots(bbctx, &[dst]);
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
            TraceIr::Br(dest_idx) => {
                self.gen_branch(codegen, ir, bbctx, func, bc_pos, dest_idx);
                return CompileResult::Exit;
            }
            TraceIr::CondBr(cond_, dest_idx, false, brkind) => {
                if bbctx.is_truthy(cond_) {
                    if brkind == BrKind::BrIf {
                        self.gen_branch(codegen, ir, bbctx, func, bc_pos, dest_idx);
                        return CompileResult::Exit;
                    }
                } else if bbctx.is_falsy(cond_) {
                    if brkind == BrKind::BrIfNot {
                        self.gen_branch(codegen, ir, bbctx, func, bc_pos, dest_idx);
                        return CompileResult::Exit;
                    }
                } else {
                    let branch_dest = codegen.jit.label();
                    ir.fetch_to_reg(bbctx, cond_, GP::Rax);
                    ir.inst.push(AsmInst::CondBr(brkind, branch_dest));
                    self.new_branch(func, bc_pos, dest_idx, bbctx.clone(), branch_dest);
                }
            }
            TraceIr::NilBr(cond_, dest_idx) => {
                let branch_dest = codegen.jit.label();
                ir.fetch_to_reg(bbctx, cond_, GP::Rax);
                ir.inst.push(AsmInst::NilBr(branch_dest));
                self.new_branch(func, bc_pos, dest_idx, bbctx.clone(), branch_dest);
            }
            TraceIr::CondBr(_, _, true, _) => {}
            TraceIr::CheckLocal(local, dest_idx) => {
                let branch_dest = codegen.jit.label();
                ir.fetch_to_reg(bbctx, local, GP::Rax);
                ir.inst.push(AsmInst::CheckLocal(branch_dest));
                self.new_branch(func, bc_pos, dest_idx, bbctx.clone(), branch_dest);
            }
            TraceIr::OptCase {
                cond,
                min,
                max,
                dest_bb,
                branch_table,
            } => {
                let else_idx = dest_bb[0];
                for bbid in dest_bb {
                    let branch_dest = codegen.jit.label();
                    self.new_branch(func, bc_pos, bbid, bbctx.clone(), branch_dest);
                }
                let deopt = ir.new_deopt(bbctx, pc);
                ir.fetch_guard_fixnum(bbctx, cond, GP::Rdi, deopt);
                ir.opt_case(max, min, else_idx, branch_table);
                return CompileResult::Exit;
            }
        }
        CompileResult::Continue
    }

    fn gen_branch(
        &mut self,
        codegen: &mut Codegen,
        ir: &mut AsmIr,
        bb: &mut BBContext,
        func: &ISeqInfo,
        bc_pos: BcIndex,
        dest: BasicBlockId,
    ) {
        let branch_dest = codegen.jit.label();
        ir.inst.push(AsmInst::Br(branch_dest));
        self.new_branch(func, bc_pos, dest, bb.clone(), branch_dest);
    }

    fn gen_inline_call(
        &mut self,
        ir: &mut AsmIr,
        store: &Store,
        bb: &mut BBContext,
        f: impl Fn(&mut AsmIr, &Store, &mut BBContext, CallSiteId, BytecodePtr),
        fid: FuncId,
        callid: CallSiteId,
        recv_class: ClassId,
        version: u32,
        pc: BytecodePtr,
    ) {
        let recv = store[callid].recv;
        ir.fetch_to_reg(bb, recv, GP::Rdi);
        let (deopt, error) = ir.new_deopt_error(bb, pc);
        let using_xmm = bb.get_using_xmm();
        ir.guard_version(fid, version, callid, using_xmm, deopt, error);
        if !recv.is_self() && !bb.is_class(recv, recv_class) {
            ir.guard_class(bb, recv, GP::Rdi, recv_class, deopt);
        }
        f(ir, store, bb, callid, pc);
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
    branch_dest: DestLabel,
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

impl std::fmt::Debug for WriteBack {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut s = String::new();
        for (xmm, slots) in &self.xmm {
            s.push_str(&format!(" {:?}->", xmm));
            for slot in slots {
                s.push_str(&format!("{:?}", slot));
            }
        }
        for (val, slot) in &self.literal {
            s.push_str(&format!(" {:?}->{:?}", val, slot));
        }
        for (slot, slots) in &self.alias {
            s.push_str(&format!(" {:?}->", slot));
            for slot in slots {
                s.push_str(&format!("{:?}", slot));
            }
        }
        if let Some(slot) = self.r15 {
            s.push_str(&format!(" R15->{:?}", slot));
        }
        write!(f, "WriteBack({})", s)
    }
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
        let mut merge_ctx = MergeContext::new(&entries.last().unwrap().bbctx);
        for BranchEntry {
            src_idx: _src_idx,
            bbctx,
            ..
        } in entries.iter()
        {
            #[cfg(feature = "jit-debug")]
            eprintln!("  <-{:?}:[{:?}] {:?}", _src_idx, bbctx.sp, bbctx.slot_state);
            merge_ctx.union(bbctx);
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

#[derive(Clone, Copy)]
pub(crate) struct UsingXmm {
    inner: bitvec::prelude::BitArr!(for 14, in u16),
}

impl std::fmt::Debug for UsingXmm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut s = String::new();
        for i in 0..14 {
            if self.inner[i] {
                s.push_str(&format!("%{i}"));
            }
        }
        write!(f, "UsingXmm({})", s)
    }
}

impl std::ops::Deref for UsingXmm {
    type Target = bitvec::prelude::BitArr!(for 14, in u16);
    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl std::ops::DerefMut for UsingXmm {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl UsingXmm {
    fn new() -> Self {
        Self {
            inner: bitvec::prelude::BitArray::new([0; 1]),
        }
    }
}

///
/// Mode of linkage between stack slot and xmm registers.
///
#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub(crate) enum LinkMode {
    ///
    /// No linkage with xmm regiter.
    ///
    #[default]
    Stack,
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
    /// Alias of *SlotId*.
    ///
    Alias(SlotId),
    ///
    /// Concrete value..
    ///
    ConcreteValue(Value),
    ///
    /// On accumulator (r15).
    ///
    Accumulator,
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
        for (loop_start, loop_end) in func.bb_info.loops() {
            let (backedge, exit) = ctx.analyse_loop(func, *loop_start, *loop_end);
            ctx.loop_backedges.insert(*loop_start, backedge);
            ctx.loop_info.insert(*loop_start, (*loop_end, exit));
        }

        let bbctx = BBContext::new(&ctx);

        if let Some(pc) = position {
            // generate class guard of *self* for loop JIT
            // We must pass pc + 1 because pc (= LoopStart) cause an infinite loop.
            let side_exit = self.gen_deopt(pc + 1, &bbctx);
            monoasm!( &mut self.jit,
                movq rdi, [r14 - (LFP_SELF)];
            );
            self.guard_class_rdi(self_value.class(), side_exit);
        } else {
            // for method JIT, class of *self* is already checked in an entry stub.
            self.prologue(func, store);
        }

        #[cfg(feature = "jit-debug")]
        eprintln!("   new_branch_init: {}->{}", BcIndex(0), start_pos);
        let bb_begin = func.bb_info.get_bb_id(start_pos);
        let branch_dest = self.jit.label();
        ctx.branch_map.insert(
            bb_begin,
            vec![BranchEntry {
                src_idx: BcIndex(0),
                bbctx,
                branch_dest,
                cont: true,
            }],
        );

        let bb_end = match func.bb_info.get_loop(bb_begin) {
            Some((a, b)) => {
                assert_eq!(a, bb_begin);
                b
            }
            None => BasicBlockId(func.bb_info.len() - 1),
        };

        let mut bbir = vec![];
        for bbid in bb_begin..=bb_end {
            let ir = ctx.compile_bb(self, store, func, position, bbid);
            bbir.push((bbid, ir));
        }

        ctx.backedge_branches(func);

        #[cfg(feature = "emit-cfg")]
        Self::dump_cfg(func, store, bb_begin, bb_end);

        // generate machine code for a main context
        for (_bbid, ir) in bbir.into_iter() {
            #[cfg(feature = "emit-asm")]
            eprintln!("{:?}", _bbid);
            self.gen_asm(ir, store, &mut ctx, None, None);
        }

        // generate machine code for bridges
        for (ir, entry, exit) in std::mem::take(&mut ctx.bridges) {
            self.gen_asm(ir, store, &mut ctx, Some(entry), Some(exit));
        }
        assert!(ctx.continuation_bridge.is_none());

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

    #[cfg(feature = "emit-cfg")]
    fn dump_cfg(func: &ISeqInfo, store: &Store, bb_begin: BasicBlockId, bb_end: BasicBlockId) {
        let mut s = format!(
            r###"digraph graph_name {{
  graph [
    charset = "UTF-8";
    label = "{}",
    labelloc = "t",
    labeljust = "c",
    bgcolor = "#343434",
    fontcolor = white,
    fontsize = 20,
    rankdir = TB,
    margin = 0.2,
    splines = spline,
    nodesep = 0.8,
    ranksep = 1.1
  ];

  node [
    colorscheme = "accent8"
    shape = box,
    style = "solid,filled",
    fontsize = 16,
    fontcolor = 5,
    fontname = "Consolas",
    color = 5,
    fillcolor = 4,
  ];

  edge [
    style = solid,
    fontsize = 14,
    fontcolor = white,
    fontname = "Migu 1M",
    color = white,
    labelfloat = true,
    labeldistance = 2.5,
    labelangle = 70
  ];"###,
            func.name()
        );
        s += "\n";
        for bbid in bb_begin..=bb_end {
            s += &format!("  {:?} [\n    shape=record\n    label=\"{{{:?}", bbid, bbid);
            let BasciBlockInfoEntry { begin, end, .. } = func.bb_info[bbid];
            for bc in begin..=end {
                if let Some(inst) = func.trace_ir(store, bc).format(store) {
                    s += "|";
                    let html = html_escape::encode_text(&inst).replace('|', "\\|");
                    s += &format!("{} {}\\l", bc, html);
                }
            }
            s += "}\"\n  ];\n";
        }

        for bbid in bb_begin..=bb_end {
            let entry = &func.bb_info[bbid];
            for succ in &entry.succ {
                s += &format!("  {:?} -> {:?} [headport = n, tailport = s];\n", bbid, succ);
            }
        }

        s += "}\n";
        std::fs::write(format!("func_id-{}.dot", func.id().get()), s).unwrap();
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
