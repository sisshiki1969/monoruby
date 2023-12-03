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
mod binary_op;
mod constants;
mod definition;
mod guard;
mod index;
mod init_method;
mod ivar;
mod merge;
mod method_call;
mod read_slot;
mod slot;
pub mod trace_ir;

//
// Just-in-time compiler module.
//

///
/// Context for JIT compilation.
///
struct JitContext {
    fid: FuncId,
    ///
    /// Destination labels for jump instructions.
    ///
    labels: HashMap<BcIndex, DestLabel>,
    ///
    /// Basic block information.
    ///
    bb_scan: Vec<(ExitType, SlotInfo)>,
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
    asmir: Vec<(AsmIr, DestLabel, Option<DestLabel>)>,
    ///
    /// The start offset of a machine code corresponding to thhe current basic block.
    ///
    #[cfg(feature = "emit-asm")]
    start_codepos: usize,
}

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
        let mut labels = HashMap::default();
        for i in 0..func.bytecode_len() {
            let idx = BcIndex::from(i);
            if func.bb_info.is_bb_head(idx) {
                labels.insert(idx, codegen.jit.label());
            }
        }
        let bb_scan = func.bb_info.init_bb_scan(func, store);

        #[cfg(feature = "emit-asm")]
        let start_codepos = codegen.jit.get_current();

        let total_reg_num = func.total_reg_num();
        let local_num = func.local_num();
        Self {
            fid: func.id(),
            labels,
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
            asmir: vec![],
            #[cfg(feature = "emit-asm")]
            start_codepos,
        }
    }

    ///
    /// Add new branch from *src_idx to *dest* with *bbctx*.
    ///
    fn new_branch(
        &mut self,
        func: &ISeqInfo,
        src_idx: BcIndex,
        dest: BcIndex,
        mut bbctx: BBContext,
        label: DestLabel,
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

    fn new_continue(
        &mut self,
        func: &ISeqInfo,
        src_idx: BcIndex,
        dest: BcIndex,
        mut bbctx: BBContext,
        label: DestLabel,
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
}

#[derive(Debug)]
struct BranchEntry {
    src_idx: BcIndex,
    bbctx: BBContext,
    label: DestLabel,
    cont: bool,
}

pub(crate) fn conv(reg: SlotId) -> i64 {
    reg.0 as i64 * 8 + LBP_SELF
}

struct WriteBack {
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
/// Context of the current Basic block.
///
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct BBContext {
    fid: FuncId,
    /// Information for stack slots.
    slot_state: SlotState,
    /// Stack top register.
    sp: SlotId,
    next_sp: SlotId,
    self_value: Value,
    local_num: usize,
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
            fid: cc.fid,
            slot_state: SlotState::new(cc),
            sp: SlotId(cc.local_num as u16),
            next_sp: SlotId(cc.local_num as u16),
            self_value: cc.self_value,
            local_num: cc.local_num,
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

    pub(crate) fn get_xmm_using(&self) -> UsingXmm {
        self.slot_state.get_xmm_using(self.sp)
    }

    fn get_write_back(&self) -> WriteBack {
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
    /// Linked to an xmm register but we can only read.
    ///
    Both(Xmm),
    ///
    /// No linkage with any xmm regiter.
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
            let side_exit = self.gen_side_deopt(pc + 1, &bbctx);
            monoasm!( &mut self.jit,
                movq rdi, [r14 - (LBP_SELF)];
            );
            self.guard_class(self_value.class(), side_exit);
        } else {
            // for method JIT, class of *self* is already checked in an entry stub.
            let pc = func.get_top_pc();
            self.prologue(pc);
        }

        #[cfg(feature = "jit-debug")]
        eprintln!("   new_branch_init: {}->{}", BcIndex(0), start_pos);
        ctx.branch_map.insert(
            start_pos,
            vec![BranchEntry {
                src_idx: BcIndex(0),
                bbctx,
                label: self.jit.label(),
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
        self.gen_asm(&mut ctx);
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

    pub(crate) fn xmm_save(&mut self, xmm_using: UsingXmm) {
        if xmm_using.not_any() {
            return;
        }
        let len = xmm_using.count_ones();
        let sp_offset = (len + len % 2) * 8;
        monoasm!( &mut self.jit,
            subq rsp, (sp_offset);
        );
        let mut i = 0;
        for b in xmm_using {
            if b {
                monoasm!( &mut self.jit,
                    movq [rsp + (8 * i)], xmm(Xmm::new(i as _).enc());
                );
                i += 1;
            }
        }
    }

    pub(crate) fn xmm_restore(&mut self, xmm_using: UsingXmm) {
        if xmm_using.not_any() {
            return;
        }
        let len = xmm_using.count_ones();
        let sp_offset = (len + len % 2) * 8;
        let mut i = 0;
        for b in xmm_using {
            if b {
                monoasm!( &mut self.jit,
                    movq xmm(Xmm::new(i as _).enc()), [rsp + (8 * i)];
                );
                i += 1;
            }
        }
        monoasm!( &mut self.jit,
            addq rsp, (sp_offset);
        );
    }

    ///
    /// Confirm a value in the slot is Float.
    ///
    /// side-exit if not Float.
    ///
    /// ### registers destroyed
    ///
    /// - rdi
    ///
    fn slot_guard_float(&mut self, reg: SlotId, side_exit: DestLabel) {
        self.load_rdi(reg);
        self.guard_float(side_exit);
    }

    ///
    /// Copy *src* to *dst*.
    ///
    fn copy_slot(&mut self, ctx: &mut BBContext, src: SlotId, dst: SlotId) {
        match ctx[src] {
            LinkMode::Xmm(x) | LinkMode::Both(x) => {
                ctx.link_xmm(dst, x);
            }
            LinkMode::Stack => {
                ctx.release(dst);
                self.load_rax(src);
                self.store_rax(dst);
            }
            LinkMode::Literal(v) => {
                ctx.link_literal(dst, v);
            }
            LinkMode::R15 => {
                self.store_r15(src);
                ctx.release(src);
                ctx.link_r15(dst);
            }
        }
    }

    fn writeback_acc(&mut self, ctx: &mut BBContext) {
        if let Some(slot) = ctx.clear_r15() {
            self.store_r15(slot);
        }
    }

    fn save_rdi_to_acc(&mut self, ctx: &mut BBContext, dst: impl Into<Option<SlotId>>) {
        let dst = dst.into();
        if let Some(dst) = dst {
            ctx.clear();
            self.writeback_acc(ctx);
            monoasm! { &mut self.jit,
                movq r15, rdi;
            }
            ctx.link_r15(dst);
        }
    }

    fn save_rax_to_acc(&mut self, ctx: &mut BBContext, dst: impl Into<Option<SlotId>>) {
        let dst = dst.into();
        if let Some(dst) = dst {
            ctx.clear();
            self.writeback_acc(ctx);
            monoasm! { &mut self.jit,
                movq r15, rax;
            }
            ctx.link_r15(dst);
        }
    }

    fn compile_bb(
        &mut self,
        store: &Store,
        func: &ISeqInfo,
        cc: &mut JitContext,
        position: Option<BcPc>,
        bb_begin: BcIndex,
        bb_end: BcIndex,
    ) {
        self.jit.bind_label(cc.labels[&bb_begin]);
        let mut ctx = if let Some(ctx) = cc.target_ctx.remove(&bb_begin) {
            ctx
        } else if let Some(ctx) = cc.incoming_context(func, bb_begin) {
            self.gen_asm(cc);
            ctx
        } else {
            #[cfg(feature = "jit-debug")]
            eprintln!("=== no entry");
            return;
        };
        for bb_pos in bb_begin..=bb_end {
            let pc = func.get_pc(bb_pos);
            #[cfg(feature = "emit-asm")]
            cc.sourcemap
                .push((bb_pos, self.jit.get_current() - cc.start_codepos));
            ctx.next_sp = func.get_sp(bb_pos);
            match pc.trace_ir() {
                TraceIr::InitMethod { .. } => {}
                TraceIr::LoopStart(_) => {
                    cc.loop_count += 1;
                }
                TraceIr::LoopEnd => {
                    assert_ne!(0, cc.loop_count);
                    cc.loop_count -= 1;
                    if cc.is_loop && cc.loop_count == 0 {
                        let mut ir = AsmIr::new();
                        ir.deopt(&ctx, pc);
                        self.gen_code(ir);
                        break;
                    }
                }
                TraceIr::Integer(dst, i) => {
                    ctx.link_literal(dst, Value::i32(i));
                }
                TraceIr::Symbol(dst, id) => {
                    ctx.link_literal(dst, Value::symbol(id));
                }
                TraceIr::Nil(dst) => {
                    ctx.link_literal(dst, Value::nil());
                }
                TraceIr::Literal(dst, val) => {
                    let mut ir = AsmIr::new();
                    ctx.release(dst);
                    if val.is_packed_value() || val.class() == FLOAT_CLASS {
                        ctx.link_literal(dst, val);
                    } else {
                        ir.deep_copy_lit(&ctx, val);
                        ir.rax2acc(&mut ctx, dst);
                    }
                    self.gen_code(ir);
                }
                TraceIr::Array { dst, callid } => {
                    let mut ir = AsmIr::new();
                    let CallSiteInfo { args, pos_num, .. } = store[callid];
                    ctx.fetch_range(&mut ir, args, pos_num as u16);
                    ctx.release(dst);
                    ir.new_array(&ctx, callid);
                    ir.rax2acc(&mut ctx, dst);
                    self.gen_code(ir);
                }
                TraceIr::Hash { dst, args, len } => {
                    let mut ir = AsmIr::new();
                    ctx.fetch_range(&mut ir, args, len * 2);
                    ctx.release(dst);
                    ir.new_hash(&ctx, args, len as _);
                    ir.rax2acc(&mut ctx, dst);
                    self.gen_code(ir);
                }
                TraceIr::Range {
                    dst,
                    start,
                    end,
                    exclude_end,
                } => {
                    let mut ir = AsmIr::new();
                    ctx.fetch_slots(&mut ir, &[start, end]);
                    ctx.release(dst);
                    ir.new_range(&mut ctx, pc, start, end, exclude_end);
                    ir.rax2acc(&mut ctx, dst);
                    self.gen_code(ir);
                }
                TraceIr::Index { dst, base, idx } => {
                    self.jit_get_array_index(&mut ctx, dst, base, idx, pc);
                }
                TraceIr::IndexAssign { src, base, idx } => {
                    self.jit_array_index_assign(&mut ctx, src, base, idx, pc);
                }
                TraceIr::LoadConst(dst, id) => {
                    ctx.release(dst);

                    if let (version, base_class, Some(v)) = store[id].cache {
                        let base_slot = store[id].base;
                        let deopt = self.gen_side_deopt(pc, &ctx);
                        if let Some(f) = v.try_float() {
                            let fdst = ctx.link_new_both(dst);
                            self.load_float_constant(
                                &mut ctx, fdst, deopt, f, version, base_class, base_slot,
                            );
                        } else {
                            self.load_generic_constant(
                                &mut ctx, deopt, v, version, base_class, base_slot,
                            );
                        }
                        self.save_rax_to_acc(&mut ctx, dst);
                    } else {
                        let mut ir = AsmIr::new();
                        ir.recompile_and_deopt(&ctx, pc, position);
                        self.gen_code(ir);
                        return;
                    }
                }
                TraceIr::StoreConst(src, id) => {
                    self.jit_store_constant(&mut ctx, id, src);
                }
                TraceIr::BlockArgProxy(ret, outer) => {
                    let mut ir = AsmIr::new();
                    ctx.release(ret);
                    ir.block_arg_proxy(ret, outer);
                    self.gen_code(ir);
                }
                TraceIr::BlockArg(ret, outer) => {
                    let mut ir = AsmIr::new();
                    ctx.release(ret);
                    ir.block_arg(&ctx, pc, ret, outer);
                    self.gen_code(ir);
                }
                TraceIr::LoadIvar(ret, id, cached_class, cached_ivarid) => {
                    if let Some(cached_class) = cached_class {
                        self.jit_load_ivar(&mut ctx, id, ret, cached_class, cached_ivarid);
                    } else {
                        let mut ir = AsmIr::new();
                        ir.recompile_and_deopt(&ctx, pc, position);
                        self.gen_code(ir);
                        return;
                    }
                }
                TraceIr::StoreIvar(src, id, cached_class, cached_ivarid) => {
                    if let Some(cached_class) = cached_class {
                        self.jit_store_ivar(&mut ctx, id, src, pc, cached_class, cached_ivarid);
                    } else {
                        let mut ir = AsmIr::new();
                        ir.recompile_and_deopt(&ctx, pc, position);
                        self.gen_code(ir);
                        return;
                    }
                }
                TraceIr::LoadGvar { dst, name } => self.jit_load_gvar(&mut ctx, name, dst),
                TraceIr::StoreGvar { src: val, name } => self.jit_store_gvar(&mut ctx, name, val),
                TraceIr::LoadSvar { dst, id } => self.jit_load_svar(&mut ctx, id, dst),
                TraceIr::LoadDynVar(dst, src) => {
                    let mut ir = AsmIr::new();
                    ctx.release(dst);
                    ir.inst.push(AsmInst::LoadDynVar { dst, src });
                    self.gen_code(ir);
                }
                TraceIr::StoreDynVar(dst, src) => {
                    let mut ir = AsmIr::new();
                    ctx.fetch_to_reg(&mut ir, src, GP::Rdi);
                    ir.inst.push(AsmInst::StoreDynVar {
                        dst: dst.clone(),
                        src: GP::Rdi,
                    });
                    self.gen_code(ir);
                }
                TraceIr::BitNot { dst, src } => {
                    let mut ir = AsmIr::new();
                    ctx.fetch_to_reg(&mut ir, src, GP::Rdi);
                    ctx.release(dst);
                    if pc.classid1().0 == 0 {
                        ir.recompile_and_deopt(&ctx, pc, position);
                        self.gen_code(ir);
                        return;
                    } else {
                        self.gen_code(ir);
                        self.jit_call_unop(&ctx, pc, bitnot_value);
                        self.store_rax(dst);
                    }
                }
                TraceIr::Not { dst, src } => {
                    self.fetch_to_rdi(&mut ctx, src);
                    ctx.release(dst);
                    self.not_rdi_to_rax();
                    self.store_rax(dst);
                }
                TraceIr::UnOp { kind, dst, src } => {
                    let mut ir = AsmIr::new();
                    if pc.is_float1() {
                        let fsrc = ctx.fetch_float_assume_float(&mut ir, src, pc);
                        let fdst = ctx.xmm_write(dst);
                        ir.xmm_move(fsrc, fdst);
                        self.gen_code(ir);
                        match kind {
                            UnOpK::Neg => {
                                let imm = self.jit.const_i64(0x8000_0000_0000_0000u64 as i64);
                                monoasm!( &mut self.jit,
                                    xorps xmm(fdst.enc()), [rip + imm];
                                );
                            }
                            UnOpK::Pos => {}
                        }
                    } else {
                        ctx.fetch_to_reg(&mut ir, src, GP::Rdi);
                        ctx.release(dst);
                        if pc.classid1().0 == 0 {
                            ir.recompile_and_deopt(&ctx, pc, position);
                            self.gen_code(ir);
                            return;
                        } else {
                            self.gen_code(ir);
                            self.jit_call_unop(&ctx, pc, kind.generic_func());
                            self.store_rax(dst);
                        }
                    }
                }
                TraceIr::IBinOp {
                    kind, dst, mode, ..
                } => {
                    self.gen_binop_integer(&mut ctx, pc, kind, dst, mode);
                }
                TraceIr::FBinOp {
                    kind, dst, mode, ..
                } => {
                    let mut ir = AsmIr::new();
                    let fmode = match mode {
                        OpMode::RR(lhs, rhs) => {
                            let (flhs, frhs) = ctx.fetch_float_binary(&mut ir, lhs, rhs, pc);
                            FMode::RR(flhs, frhs)
                        }
                        OpMode::RI(lhs, rhs) => {
                            let flhs = ctx.fetch_float_assume_float(&mut ir, lhs, pc);
                            FMode::RI(flhs, rhs)
                        }
                        OpMode::IR(lhs, rhs) => {
                            let frhs = ctx.fetch_float_assume_float(&mut ir, rhs, pc);
                            FMode::IR(lhs, frhs)
                        }
                    };
                    if let Some(ret) = dst {
                        let dst = ctx.xmm_write(ret);
                        let using_xmm = ctx.get_xmm_using();
                        ir.xmm_binop(kind, fmode, dst, using_xmm);
                    }
                    self.gen_code(ir);
                }
                TraceIr::BinOp {
                    kind, dst, mode, ..
                } => {
                    self.fetch_binary(&mut ctx, &mode);
                    ctx.release(dst);
                    if pc.classid1().0 == 0 || pc.classid2().0 == 0 {
                        let mut ir = AsmIr::new();
                        ir.recompile_and_deopt(&ctx, pc, position);
                        self.gen_code(ir);
                        return;
                    } else {
                        self.load_binary_args_with_mode(&mode);
                        self.generic_binop(&mut ctx, dst, kind, pc);
                    }
                }
                TraceIr::Cmp(kind, ret, mode, false) => {
                    if mode.is_float_op(&pc) && kind != CmpKind::Cmp {
                        match mode {
                            OpMode::RR(lhs, rhs) => {
                                let (flhs, frhs) = self.fetch_float_binary(&mut ctx, lhs, rhs, pc);
                                ctx.release(ret);
                                monoasm! { &mut self.jit,
                                    xorq rax, rax;
                                    ucomisd xmm(flhs.enc()), xmm(frhs.enc());
                                };
                            }
                            OpMode::RI(lhs, rhs) => {
                                let rhs_label = self.jit.const_f64(rhs as f64);
                                let flhs = self.fetch_float_assume_float(&mut ctx, lhs, pc);
                                ctx.release(ret);
                                monoasm! { &mut self.jit,
                                    xorq rax, rax;
                                    ucomisd xmm(flhs.enc()), [rip + rhs_label];
                                };
                            }
                            _ => unreachable!(),
                        }
                        self.setflag_float(kind);
                        self.store_rax(ret);
                    } else if mode.is_integer_op(&pc) {
                        self.fetch_binary(&mut ctx, &mode);
                        ctx.release(ret);
                        let deopt = self.gen_side_deopt(pc, &ctx);
                        self.load_and_guard_binary_fixnum_with_mode(deopt, &mode);
                        self.integer_cmp(kind);
                        self.jit_handle_error(&ctx, pc);
                        self.store_rax(ret);
                    } else {
                        self.fetch_binary(&mut ctx, &mode);
                        ctx.release(ret);
                        if pc.classid1().0 == 0 || pc.classid2().0 == 0 {
                            let mut ir = AsmIr::new();
                            ir.recompile_and_deopt(&ctx, pc, position);
                            self.gen_code(ir);
                            return;
                        } else {
                            self.load_binary_args_with_mode(&mode);
                            self.generic_cmp(kind, &ctx);
                            self.jit_handle_error(&ctx, pc);
                            self.store_rax(ret);
                        }
                    }
                }

                TraceIr::Cmp(kind, ret, mode, true) => {
                    let index = bb_pos + 1;
                    self.gen_cmp_opt(&mut ctx, cc, func, mode, kind, ret, pc, index);
                }
                TraceIr::Mov(dst, src) => {
                    self.copy_slot(&mut ctx, src, dst);
                }
                TraceIr::ConcatStr(dst, arg, len) => {
                    let mut ir = AsmIr::new();
                    ctx.fetch_range(&mut ir, arg, len);
                    if let Some(dst) = dst {
                        ctx.release(dst);
                    }
                    let using_xmm = ctx.get_xmm_using();
                    ir.inst.push(AsmInst::ConcatStr {
                        arg,
                        len,
                        using_xmm,
                    });
                    ir.reg2stack(GP::Rax, dst);
                    self.gen_code(ir);
                }
                TraceIr::ConcatRegexp(ret, arg, len) => {
                    let mut ir = AsmIr::new();
                    ctx.fetch_range(&mut ir, arg, len);
                    self.gen_code(ir);
                    if let Some(ret) = ret {
                        ctx.release(ret);
                    }
                    let xmm_using = ctx.get_xmm_using();
                    self.xmm_save(xmm_using);
                    monoasm!( &mut self.jit,
                        movq rdi, rbx;
                        movq rsi, r12;
                        lea rdx, [r14 - (conv(arg))];
                        movq rcx, (len);
                        movq rax, (runtime::concatenate_regexp);
                        call rax;
                    );
                    self.xmm_restore(xmm_using);
                    self.jit_handle_error(&ctx, pc);
                    self.store_rax(ret);
                }
                TraceIr::ExpandArray(src, dst, len) => {
                    self.fetch_slots(&mut ctx, &[src]);
                    for reg in dst.0..dst.0 + len {
                        ctx.release(SlotId(reg));
                    }
                    let xmm_using = ctx.get_xmm_using();
                    self.xmm_save(xmm_using);
                    self.load_rdi(src);
                    monoasm!( &mut self.jit,
                        lea rsi, [r14 - (conv(dst))];
                        movq rdx, (len);
                        movq rax, (runtime::expand_array);
                        call rax;
                    );
                    self.xmm_restore(xmm_using);
                }
                TraceIr::AliasMethod { new, old } => {
                    self.fetch_slots(&mut ctx, &[new, old]);
                    let xmm_using = ctx.get_xmm_using();
                    self.xmm_save(xmm_using);
                    monoasm!( &mut self.jit,
                        movq rdi, rbx;
                        movq rsi, r12;
                        movq rdx, [r14 - (LBP_SELF)];
                        movq rcx, [r14 - (conv(new))];
                        movq r8, [r14 - (conv(old))];
                        movq r9, [r14 - (LBP_META)];
                        movq rax, (runtime::alias_method);
                        call rax;
                    );
                    self.xmm_restore(xmm_using);
                    self.jit_handle_error(&ctx, pc);
                }
                TraceIr::MethodCall { callid } | TraceIr::MethodCallBlock { callid } => {
                    // We must write back and unlink all local vars since they may be accessed from block.
                    if store[callid].block_fid.is_some() {
                        self.gen_write_back_locals(&mut ctx);
                    }
                    if let Some(fid) = pc.cached_fid() {
                        self.gen_call(store, &mut ctx, fid, callid, pc);
                    } else {
                        let mut ir = AsmIr::new();
                        ir.recompile_and_deopt(&ctx, pc, position);
                        self.gen_code(ir);
                        return;
                    }
                }
                TraceIr::InlineCall {
                    inline_id, callid, ..
                } => {
                    let gen = store.get_inline_info(inline_id).0;
                    self.gen_inlinable(&mut ctx, &store[callid], gen, pc);
                }
                TraceIr::Yield { callid } => {
                    self.gen_yield(&mut ctx, store, callid, pc);
                }
                TraceIr::InlineCache => {}
                TraceIr::MethodDef { name, func_id } => {
                    let xmm_using = ctx.get_xmm_using();
                    self.xmm_save(xmm_using);
                    monoasm!( &mut self.jit,
                        movq rdi, rbx; // &mut Interp
                        movq rsi, r12; // &Globals
                        movq rdx, (u32::from(name)); // IdentId
                        movq rcx, (u32::from(func_id)); // FuncId
                        movq rax, (runtime::define_method);
                        call rax;
                    );
                    self.xmm_restore(xmm_using);
                }
                TraceIr::SingletonMethodDef { obj, name, func_id } => {
                    self.fetch_slots(&mut ctx, &[obj]);
                    let xmm_using = ctx.get_xmm_using();
                    self.xmm_save(xmm_using);
                    monoasm!( &mut self.jit,
                        movq rdi, rbx; // &mut Interp
                        movq rsi, r12; // &Globals
                        movq rdx, (u32::from(name)); // IdentId
                        movq rcx, (u32::from(func_id)); // FuncId
                        movq r8, [r14 - (conv(obj))];
                        movq rax, (runtime::singleton_define_method);
                        call rax;
                    );
                    self.xmm_restore(xmm_using);
                }
                TraceIr::ClassDef {
                    ret,
                    superclass,
                    name,
                    func_id,
                } => {
                    self.jit_class_def(&mut ctx, ret, superclass, name, func_id, false, pc);
                }
                TraceIr::ModuleDef { ret, name, func_id } => {
                    self.jit_class_def(&mut ctx, ret, SlotId::new(0), name, func_id, true, pc);
                }
                TraceIr::SingletonClassDef { ret, base, func_id } => {
                    self.jit_singleton_class_def(&mut ctx, ret, base, func_id, pc);
                }
                TraceIr::DefinedYield { ret } => {
                    self.fetch_slots(&mut ctx, &[ret]);
                    monoasm! { &mut self.jit,
                        movq rdi, rbx;  // &mut Interp
                        movq rsi, r12;  // &mut Globals
                        lea  rdx, [r14 - (conv(ret))];
                        movq rax, (runtime::defined_yield);
                        call rax;
                    };
                }
                TraceIr::DefinedConst { ret, siteid } => {
                    self.fetch_slots(&mut ctx, &[ret]);
                    monoasm! { &mut self.jit,
                        movq rdi, rbx;  // &mut Interp
                        movq rsi, r12;  // &mut Globals
                        lea  rdx, [r14 - (conv(ret))];
                        movl rcx, (siteid.0);
                        movq rax, (runtime::defined_const);
                        call rax;
                    };
                }
                TraceIr::DefinedMethod { ret, recv, name } => {
                    self.fetch_slots(&mut ctx, &[ret, recv]);
                    monoasm! { &mut self.jit,
                        movq rdi, rbx;  // &mut Interp
                        movq rsi, r12;  // &mut Globals
                        lea  rdx, [r14 - (conv(ret))];
                        movq rcx, [r14 - (conv(recv))];
                        movl r8, (name.get());
                        movq rax, (runtime::defined_method);
                        call rax;
                    };
                }
                TraceIr::DefinedGvar { ret, name } => {
                    self.fetch_slots(&mut ctx, &[ret]);
                    monoasm! { &mut self.jit,
                        movq rdi, rbx;  // &mut Interp
                        movq rsi, r12;  // &mut Globals
                        lea  rdx, [r14 - (conv(ret))];
                        movl rcx, (name.get());
                        movq rax, (runtime::defined_gvar);
                        call rax;
                    };
                }
                TraceIr::DefinedIvar { ret, name } => {
                    self.fetch_slots(&mut ctx, &[ret]);
                    monoasm! { &mut self.jit,
                        movq rdi, rbx;  // &mut Interp
                        movq rsi, r12;  // &mut Globals
                        lea  rdx, [r14 - (conv(ret))];
                        movl rcx, (name.get());
                        movq rax, (runtime::defined_ivar);
                        call rax;
                    };
                }
                TraceIr::Ret(lhs) => {
                    self.gen_write_back_locals(&mut ctx);
                    self.fetch_to_rax(&mut ctx, lhs);
                    self.epilogue();
                    return;
                }
                TraceIr::MethodRet(lhs) => {
                    self.gen_write_back_locals(&mut ctx);
                    self.fetch_to_rax(&mut ctx, lhs);
                    monoasm! { &mut self.jit,
                        movq r13, ((pc + 1).get_u64());
                    };
                    self.method_return();
                    return;
                }
                TraceIr::Break(lhs) => {
                    self.gen_write_back_locals(&mut ctx);
                    self.fetch_to_rax(&mut ctx, lhs);
                    self.block_break();
                    self.epilogue();
                    return;
                }
                TraceIr::Raise(src) => {
                    let raise = self.entry_raise;
                    self.fetch_to_rax(&mut ctx, src);
                    monoasm! { &mut self.jit,
                        movq rdi, rbx;
                        movq rsi, rax;
                        movq rax, (runtime::raise_err);
                        call rax;
                        jmp  raise;
                    };
                    return;
                }
                TraceIr::EnsureEnd => {
                    self.gen_write_back_locals(&mut ctx);
                    let raise = self.entry_raise;
                    monoasm! { &mut self.jit,
                        movq rdi, rbx;
                        movq rax, (runtime::check_err);
                        call rax;
                        testq rax, rax;
                        jne  raise;
                    };
                }
                TraceIr::Br(disp) => {
                    let next_idx = bb_pos + 1;
                    let dest_idx = next_idx + disp;
                    let branch_dest = self.jit.label();
                    cc.new_branch(func, bb_pos, dest_idx, ctx, branch_dest);
                    monoasm!( &mut self.jit,
                        jmp branch_dest;
                    );
                    return;
                }
                TraceIr::CondBr(cond_, disp, false, kind) => {
                    let dest_idx = bb_pos + 1 + disp;
                    let branch_dest = self.jit.label();
                    self.fetch_to_rax(&mut ctx, cond_);
                    cc.new_branch(func, bb_pos, dest_idx, ctx.clone(), branch_dest);
                    monoasm!( &mut self.jit,
                        orq rax, 0x10;
                        cmpq rax, (FALSE_VALUE);
                    );
                    match kind {
                        BrKind::BrIf => monoasm!( &mut self.jit, jne branch_dest;),
                        BrKind::BrIfNot => monoasm!( &mut self.jit, jeq branch_dest;),
                    }
                }
                TraceIr::CondBr(_, _, true, _) => {}
                TraceIr::CheckLocal(local, disp) => {
                    let dest_idx = bb_pos + 1 + disp;
                    let branch_dest = self.jit.label();
                    cc.new_branch(func, bb_pos, dest_idx, ctx.clone(), branch_dest);
                    self.load_rax(local);
                    monoasm!( &mut self.jit,
                        testq rax, rax;
                        jnz  branch_dest;
                    );
                }
                TraceIr::OptCase { cond, optid } => {
                    let OptCaseInfo {
                        min,
                        max,
                        branch_table,
                        offsets,
                    } = &store[optid];
                    let mut label_map = HashMap::default();
                    for ofs in offsets {
                        let dest_idx = bb_pos + 1 + (*ofs as i32);
                        let branch_dest = self.jit.label();
                        label_map.insert(dest_idx, branch_dest);
                        cc.new_branch(func, bb_pos, dest_idx, ctx.clone(), branch_dest);
                    }
                    let else_idx = bb_pos + 1 + (offsets[0] as i32);
                    let else_dest = label_map.get(&else_idx).cloned().unwrap();

                    let jump_table = self.jit.const_align8();
                    for ofs in branch_table.iter() {
                        let idx = bb_pos + 1 + (*ofs as i32);
                        let dest_label = label_map.get(&idx).cloned().unwrap();
                        self.jit.abs_address(dest_label);
                    }

                    self.fetch_to_rdi(&mut ctx, cond);
                    self.guard_class(INTEGER_CLASS, else_dest);

                    monoasm! {&mut self.jit,
                        sarq rdi, 1;
                        cmpq rdi, (*max);
                        jgt  else_dest;
                        subq rdi, (*min);
                        jlt  else_dest;
                        lea  rax, [rip + jump_table];
                        jmp  [rax + rdi * 8];
                    };
                    return;
                }
            }
            ctx.clear();
            ctx.sp = ctx.next_sp;
        }
        let next_idx = bb_end + 1;
        if func.bb_info.is_bb_head(next_idx) {
            let branch_dest = self.jit.label();
            cc.new_continue(func, bb_end, next_idx, ctx, branch_dest);
            if let Some(target_ctx) = cc.incoming_context(func, next_idx) {
                self.gen_asm(cc);
                assert!(cc.target_ctx.insert(next_idx, target_ctx).is_none());
            }
        }
    }

    fn recompile_and_deopt(&mut self, position: Option<BcPc>, deopt: DestLabel) {
        let recompile = self.jit.label();
        let dec = self.jit.label();
        let counter = self.jit.const_i32(5);

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
    pub(crate) fn jit_handle_error(&mut self, ctx: &BBContext, pc: BcPc) {
        let raise = self.entry_raise;
        let wb = ctx.get_write_back();
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
            self.gen_write_back(&wb);
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
            self.gen_write_back(&wb);
            monoasm!( &mut self.jit,
                movq r13, ((pc + 1).get_u64());
                jmp  raise;
            cont:
            );
        }
    }

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

    fn gen_write_back_locals(&mut self, ctx: &mut BBContext) {
        let wb = ctx.get_locals_write_back();
        self.gen_write_back(&wb);
        ctx.release_locals();
    }

    ///
    /// Get *DestLabel* for write-back and fallback to interpreter.
    ///
    /// ### in
    /// - rdi: deopt-reason:Value
    ///
    fn gen_side_deopt(&mut self, pc: BcPc, ctx: &BBContext) -> DestLabel {
        let entry = self.jit.label();
        let wb = ctx.get_write_back();
        self.gen_side_deopt_with_label(pc, &wb, entry);
        entry
    }

    ///
    /// Get *DestLabel* for fallback to interpreter. (without write-back)
    ///
    /// ### in
    /// - rdi: deopt-reason:Value
    ///
    fn gen_side_deopt_with_label(&mut self, pc: BcPc, wb: &WriteBack, entry: DestLabel) {
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

    fn load_binary_args(&mut self, lhs: SlotId, rhs: SlotId) {
        self.load_rdi(lhs);
        self.load_rsi(rhs);
    }

    ///
    /// Call unary operator function.
    ///
    /// ### in
    /// - rdi: receiver
    ///
    /// ### out
    /// - rax: result
    ///
    fn jit_call_unop(&mut self, ctx: &BBContext, pc: BcPc, func: UnaryOpFn) {
        let xmm_using = ctx.get_xmm_using();
        self.xmm_save(xmm_using);
        self.call_unop(func);
        self.xmm_restore(xmm_using);
        self.jit_handle_error(&ctx, pc);
    }
}

impl Codegen {
    ///
    /// Get an instance variable.
    ///
    /// #### in
    ///
    /// - rdi: &RValue
    ///
    /// - rsi: IvarId
    ///
    /// #### out
    ///
    /// - rax: Value
    ///
    fn get_ivar(&mut self, using: UsingXmm) {
        self.xmm_save(using);
        monoasm!( &mut self.jit,
            movq rax, (RValue::get_ivar);
            call rax;
        );
        self.xmm_restore(using);
    }

    ///
    /// Set an instance variable.
    ///
    /// #### in
    ///
    /// - rdi: base: &mut RValue
    /// - rsi: IvarId
    /// - rdx: src: Value
    ///
    /// #### destroy
    ///
    /// - caller-save registers
    ///
    fn set_ivar(&mut self, using: UsingXmm) {
        self.xmm_save(using);
        monoasm!( &mut self.jit,
            movq rax, (RValue::set_ivar);
            call rax;
        );
        self.xmm_restore(using);
    }
}
