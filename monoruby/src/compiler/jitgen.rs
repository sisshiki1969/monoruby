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
    labels: HashMap<BcIndex, DestLabel>,
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
    asmir: Vec<(AsmIr, DestLabel, Option<DestLabel>)>,
    ///
    /// The start offset of a machine code corresponding to thhe current basic block.
    ///
    #[cfg(feature = "emit-asm")]
    start_codepos: usize,
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
    /// Add new branch from *src_idx* to *dest* with the context *bbctx*.
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

    ///
    /// Add new continuation branch from *src_idx* to *dest* with the context *bbctx*.
    ///
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
    label: DestLabel,
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
        cc: &mut JitContext,
        position: Option<BcPc>,
        bb_begin: BcIndex,
        bb_end: BcIndex,
    ) {
        self.jit.bind_label(cc.labels[&bb_begin]);
        let mut ctx = if let Some(ctx) = cc.target_ctx.remove(&bb_begin) {
            ctx
        } else if let Some(ctx) = cc.incoming_context(func, bb_begin) {
            self.gen_bridges(store, cc);
            ctx
        } else {
            #[cfg(feature = "jit-debug")]
            eprintln!("=== no entry");
            return;
        };

        let mut ir = AsmIr::new();
        let res = self.compile_bb_inner(
            &mut ir, store, func, cc, &mut ctx, position, bb_begin, bb_end,
        );
        let _map = self.gen_code(store, ir);

        #[cfg(feature = "emit-asm")]
        {
            let map = _map
                .into_iter()
                .map(|(pc, pos)| (pc, pos - cc.start_codepos));
            cc.sourcemap.extend(map);
        }

        if res {
            return;
        }

        let next_idx = bb_end + 1;
        if func.bb_info.is_bb_head(next_idx) {
            let branch_dest = self.jit.label();
            cc.new_continue(func, bb_end, next_idx, ctx, branch_dest);
            if let Some(target_ctx) = cc.incoming_context(func, next_idx) {
                self.gen_bridges(store, cc);
                assert!(cc.target_ctx.insert(next_idx, target_ctx).is_none());
            }
        }
    }

    fn compile_bb_inner(
        &mut self,
        ir: &mut AsmIr,
        store: &Store,
        func: &ISeqInfo,
        cc: &mut JitContext,
        ctx: &mut BBContext,
        position: Option<BcPc>,
        bb_begin: BcIndex,
        bb_end: BcIndex,
    ) -> bool {
        for bb_pos in bb_begin..=bb_end {
            ir.bc_index(bb_pos);
            ctx.next_sp = func.get_sp(bb_pos);

            match self.compile_inst(ir, cc, ctx, store, func, bb_pos) {
                CompileResult::Continue => {}
                CompileResult::Exit => {
                    return true;
                }
                CompileResult::Recompile => {
                    let pc = func.get_pc(bb_pos);
                    ir.recompile_and_deopt(&ctx, pc, position);
                    return true;
                }
                CompileResult::Break => break,
            }

            ctx.clear();
            ctx.sp = ctx.next_sp;
        }

        false
    }

    fn compile_inst(
        &mut self,
        ir: &mut AsmIr,
        cc: &mut JitContext,
        ctx: &mut BBContext,
        store: &Store,
        func: &ISeqInfo,
        bb_pos: BcIndex,
    ) -> CompileResult {
        let pc = func.get_pc(bb_pos);
        match pc.trace_ir() {
            TraceIr::InitMethod { .. } => {}
            TraceIr::LoopStart(_) => {
                cc.loop_count += 1;
            }
            TraceIr::LoopEnd => {
                assert_ne!(0, cc.loop_count);
                cc.loop_count -= 1;
                if cc.is_loop && cc.loop_count == 0 {
                    ir.deopt(&ctx, pc);
                    return CompileResult::Break;
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
                ctx.release(dst);
                if val.is_packed_value() || val.class() == FLOAT_CLASS {
                    ctx.link_literal(dst, val);
                } else {
                    ir.deep_copy_lit(&ctx, val);
                    ir.rax2acc(ctx, dst);
                }
            }
            TraceIr::Array { dst, callid } => {
                let CallSiteInfo { args, pos_num, .. } = store[callid];
                ir.fetch_range(ctx, args, pos_num as u16);
                ctx.release(dst);
                ir.new_array(&ctx, callid);
                ir.rax2acc(ctx, dst);
            }
            TraceIr::Hash { dst, args, len } => {
                ir.fetch_range(ctx, args, len * 2);
                ctx.release(dst);
                ir.new_hash(&ctx, args, len as _);
                ir.rax2acc(ctx, dst);
            }
            TraceIr::Range {
                dst,
                start,
                end,
                exclude_end,
            } => {
                ir.fetch_slots(ctx, &[start, end]);
                ctx.release(dst);
                ir.new_range(ctx, pc, start, end, exclude_end);
                ir.rax2acc(ctx, dst);
            }
            TraceIr::Index { dst, base, idx } => {
                if pc.classid1().0 == 0 || pc.classid2().0 == 0 {
                    return CompileResult::Recompile;
                }
                if pc.classid1() == ARRAY_CLASS && pc.classid2() == INTEGER_CLASS {
                    ctx.array_index(ir, dst, base, idx, pc);
                } else {
                    ir.fetch_slots(ctx, &[base, idx]);
                    ctx.release(dst);
                    ir.generic_index(&ctx, pc, base, idx);
                }
                ir.rax2acc(ctx, dst);
            }
            TraceIr::IndexAssign { src, base, idx } => {
                if pc.classid1().0 == 0 || pc.classid2().0 == 0 {
                    return CompileResult::Recompile;
                }
                if pc.classid1() == ARRAY_CLASS && pc.classid2() == INTEGER_CLASS {
                    ctx.array_index_assign(ir, src, base, idx, pc);
                } else {
                    ir.fetch_slots(ctx, &[base, idx, src]);
                    ir.generic_index_assign(&ctx, pc, base, idx, src);
                }
            }
            TraceIr::LoadConst(dst, id) => {
                ctx.release(dst);

                if let (cached_version, cached_baseclass, Some(cached_val)) = store[id].cache {
                    let base_slot = store[id].base;
                    if let Some(slot) = base_slot {
                        if let Some(base_class) = cached_baseclass {
                            ir.fetch_to_reg(ctx, slot, GP::Rax);
                            let deopt = ir.new_deopt(pc, ctx.get_write_back());
                            ir.inst.push(AsmInst::GuardBaseClass { base_class, deopt });
                        } else {
                            return CompileResult::Recompile;
                        }
                    }
                    let deopt = ir.new_deopt(pc, ctx.get_write_back());
                    if let Some(f) = cached_val.try_float() {
                        let fdst = ctx.link_new_both(dst);
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
                    ir.rax2acc(ctx, dst);
                } else {
                    return CompileResult::Recompile;
                }
            }
            TraceIr::StoreConst(src, name) => {
                ir.fetch_to_reg(ctx, src, GP::Rax);
                let using_xmm = ctx.get_using_xmm();
                ir.inst.push(AsmInst::StoreConstant { name, using_xmm });
            }
            TraceIr::BlockArgProxy(ret, outer) => {
                ctx.release(ret);
                ir.block_arg_proxy(ret, outer);
            }
            TraceIr::BlockArg(ret, outer) => {
                ctx.release(ret);
                ir.block_arg(&ctx, pc, ret, outer);
            }
            TraceIr::LoadIvar(ret, id, cached_class, cached_ivarid) => {
                if let Some(cached_class) = cached_class {
                    ctx.jit_load_ivar(ir, id, ret, cached_class, cached_ivarid);
                } else {
                    return CompileResult::Recompile;
                }
            }
            TraceIr::StoreIvar(src, id, cached_class, cached_ivarid) => {
                if let Some(cached_class) = cached_class {
                    ctx.jit_store_ivar(ir, id, src, pc, cached_class, cached_ivarid);
                } else {
                    return CompileResult::Recompile;
                }
            }
            TraceIr::LoadGvar { dst, name } => {
                ir.jit_load_gvar(ctx, name, dst);
            }
            TraceIr::StoreGvar { src: val, name } => {
                ir.jit_store_gvar(ctx, name, val);
            }
            TraceIr::LoadSvar { dst, id } => {
                ctx.release(dst);
                ir.load_svar(&ctx, id);
                ir.rax2acc(ctx, dst);
            }
            TraceIr::LoadDynVar(dst, src) => {
                ctx.release(dst);
                if !dst.is_zero() {
                    ir.inst.push(AsmInst::LoadDynVar { src });
                    ir.rax2acc(ctx, dst);
                }
            }
            TraceIr::StoreDynVar(dst, src) => {
                ir.fetch_to_reg(ctx, src, GP::Rdi);
                ir.inst.push(AsmInst::StoreDynVar { dst, src: GP::Rdi });
            }
            TraceIr::BitNot { dst, src } => {
                if pc.classid1().0 == 0 {
                    return CompileResult::Recompile;
                }
                ir.fetch_to_reg(ctx, src, GP::Rdi);
                ctx.release(dst);
                if pc.classid1().0 == 0 {
                    return CompileResult::Recompile;
                } else {
                    ir.generic_unop(&ctx, pc, bitnot_value);
                    ir.rax2acc(ctx, dst);
                }
            }
            TraceIr::Not { dst, src } => {
                ir.fetch_to_reg(ctx, src, GP::Rdi);
                ctx.release(dst);
                ir.inst.push(AsmInst::Not);
                ir.rax2acc(ctx, dst);
            }
            TraceIr::UnOp { kind, dst, src } => {
                if pc.classid1().0 == 0 {
                    return CompileResult::Recompile;
                }
                if pc.is_float1() {
                    let deopt = ir.new_deopt(pc, ctx.get_write_back());
                    let fsrc = ir.fetch_float_assume_float(ctx, src, deopt);
                    let dst = ctx.xmm_write(dst);
                    ir.xmm_move(fsrc, dst);
                    ir.inst.push(AsmInst::XmmUnOp { kind, dst });
                } else {
                    ir.fetch_to_reg(ctx, src, GP::Rdi);
                    ctx.release(dst);
                    ir.generic_unop(&ctx, pc, kind.generic_func());
                    ir.rax2acc(ctx, dst);
                }
            }
            TraceIr::IBinOp { kind, dst, mode } => {
                ir.gen_binop_integer(ctx, pc, kind, dst, mode);
            }
            TraceIr::FBinOp { kind, dst, mode } => {
                let deopt = ir.new_deopt(pc, ctx.get_write_back());
                let fmode = ir.fmode(&mode, ctx, pc, deopt);
                if let Some(ret) = dst {
                    let dst = ctx.xmm_write(ret);
                    let using_xmm = ctx.get_using_xmm();
                    ir.xmm_binop(kind, fmode, dst, using_xmm);
                }
            }
            TraceIr::BinOp {
                kind, dst, mode, ..
            } => {
                if pc.classid1().0 == 0 || pc.classid2().0 == 0 {
                    return CompileResult::Recompile;
                }
                ir.fetch_binary(ctx, mode);
                ctx.release(dst);
                ir.generic_binop(&ctx, pc, kind);
                ir.rax2acc(ctx, dst);
            }
            TraceIr::Cmp(kind, ret, mode, false) => {
                if pc.classid1().0 == 0 || pc.classid2().0 == 0 {
                    return CompileResult::Recompile;
                }
                if mode.is_float_op(&pc) && kind != CmpKind::Cmp {
                    let deopt = ir.new_deopt(pc, ctx.get_write_back());
                    let mode = ir.fmode(&mode, ctx, pc, deopt);
                    ctx.release(ret);
                    ir.inst.push(AsmInst::FloatCmp { kind, mode });
                } else if mode.is_integer_op(&pc) {
                    ir.fetch_fixnum_binary(ctx, pc, &mode);
                    ctx.release(ret);
                    ir.inst.push(AsmInst::IntegerCmp { kind, mode });
                } else {
                    ir.fetch_binary(ctx, mode);
                    ctx.release(ret);
                    ir.generic_cmp(&ctx, pc, kind);
                }
                ir.rax2acc(ctx, ret);
            }

            TraceIr::Cmp(kind, ret, mode, true) => {
                let index = bb_pos + 1;
                match (pc + 1).trace_ir() {
                    TraceIr::CondBr(_, disp, true, brkind) => {
                        let dest_idx = index + disp + 1;
                        let branch_dest = self.jit.label();
                        if mode.is_float_op(&pc) {
                            let deopt = ir.new_deopt(pc, ctx.get_write_back());
                            let mode = ir.fmode(&mode, ctx, pc, deopt);
                            ctx.release(ret);
                            ir.float_cmp_br(mode, kind, brkind, branch_dest);
                        } else {
                            if mode.is_integer_op(&pc) {
                                ir.fetch_fixnum_binary(ctx, pc, &mode);
                                ctx.release(ret);
                                ir.integer_cmp_br(mode, kind, brkind, branch_dest);
                            } else {
                                ir.fetch_binary(ctx, mode);
                                ctx.release(ret);
                                ir.generic_cmp(&ctx, pc, kind);
                                ir.inst.push(AsmInst::GenericCondBr {
                                    brkind,
                                    branch_dest,
                                });
                            }
                        }
                        cc.new_branch(func, index, dest_idx, ctx.clone(), branch_dest);
                    }
                    _ => unreachable!(),
                }
            }
            TraceIr::Mov(dst, src) => {
                ctx.copy_slot(ir, src, dst);
            }
            TraceIr::ConcatStr(dst, arg, len) => {
                ir.fetch_range(ctx, arg, len);
                ctx.release(dst);
                ir.concat_str(&ctx, arg, len);
                ir.rax2acc(ctx, dst);
            }
            TraceIr::ConcatRegexp(dst, arg, len) => {
                ir.fetch_range(ctx, arg, len);
                ctx.release(dst);
                ir.concat_regexp(&ctx, pc, arg, len);
                ir.rax2acc(ctx, dst);
            }
            TraceIr::ExpandArray(src, dst, len) => {
                ir.fetch_to_reg(ctx, src, GP::Rdi);
                for reg in dst.0..dst.0 + len {
                    ctx.release(SlotId(reg));
                }
                ir.expand_array(&ctx, dst, len);
            }
            TraceIr::AliasMethod { new, old } => {
                ir.fetch_slots(ctx, &[new, old]);
                ir.alias_method(&ctx, pc, new, old);
            }
            TraceIr::MethodCall { callid } | TraceIr::MethodCallBlock { callid } => {
                // We must write back and unlink all local vars since they may be accessed from block.
                if store[callid].block_fid.is_some() {
                    ir.write_back_locals(ctx);
                }
                if let Some(fid) = pc.cached_fid() {
                    if ir.gen_call(store, ctx, fid, callid, pc).is_none() {
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
                ir.writeback_acc(ctx);
                let deopt = ir.new_deopt(pc, ctx.get_write_back());
                ir.guard_class_version(pc, deopt);
                inline_gen(ir, store, ctx, &store[callid], pc);
            }
            TraceIr::Yield { callid } => {
                ir.fetch_callargs(ctx, &store[callid]);
                ctx.release(store[callid].dst);
                ir.writeback_acc(ctx);
                let using_xmm = ctx.get_using_xmm();
                let error = ir.new_error(pc, ctx.get_write_back());
                ir.inst.push(AsmInst::Yield {
                    callid,
                    using_xmm,
                    error,
                });
                ir.rax2acc(ctx, store[callid].dst);
            }
            TraceIr::InlineCache => {}
            TraceIr::MethodDef { name, func_id } => {
                let using_xmm = ctx.get_using_xmm();
                ir.inst.push(AsmInst::MethodDef {
                    name,
                    func_id,
                    using_xmm,
                });
            }
            TraceIr::SingletonMethodDef { obj, name, func_id } => {
                ir.fetch_slots(ctx, &[obj]);
                let using_xmm = ctx.get_using_xmm();
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
                ctx.class_def(ir, dst, superclass, name, func_id, false, pc);
            }
            TraceIr::ModuleDef { dst, name, func_id } => {
                ctx.class_def(ir, dst, SlotId::new(0), name, func_id, true, pc);
            }
            TraceIr::SingletonClassDef { dst, base, func_id } => {
                ctx.singleton_class_def(ir, dst, base, func_id, pc);
            }
            TraceIr::DefinedYield { dst } => {
                ir.fetch_slots(ctx, &[dst]);
                let using_xmm = ctx.get_using_xmm();
                ir.inst.push(AsmInst::DefinedYield { dst, using_xmm });
            }
            TraceIr::DefinedConst { dst, siteid } => {
                ir.fetch_slots(ctx, &[dst]);
                let using_xmm = ctx.get_using_xmm();
                ir.inst.push(AsmInst::DefinedConst {
                    dst,
                    siteid,
                    using_xmm,
                });
            }
            TraceIr::DefinedMethod { dst, recv, name } => {
                ir.fetch_slots(ctx, &[dst, recv]);
                let using_xmm = ctx.get_using_xmm();
                ir.inst.push(AsmInst::DefinedMethod {
                    dst,
                    recv,
                    name,
                    using_xmm,
                });
            }
            TraceIr::DefinedGvar { dst, name } => {
                ir.fetch_slots(ctx, &[dst]);
                let using_xmm = ctx.get_using_xmm();
                ir.inst.push(AsmInst::DefinedGvar {
                    dst,
                    name,
                    using_xmm,
                });
            }
            TraceIr::DefinedIvar { dst, name } => {
                ir.fetch_slots(ctx, &[dst]);
                let using_xmm = ctx.get_using_xmm();
                ir.inst.push(AsmInst::DefinedIvar {
                    dst,
                    name,
                    using_xmm,
                });
            }
            TraceIr::Ret(ret) => {
                ir.write_back_locals(ctx);
                ir.fetch_to_reg(ctx, ret, GP::Rax);
                ir.inst.push(AsmInst::Ret);
                return CompileResult::Exit;
            }
            TraceIr::MethodRet(ret) => {
                ir.write_back_locals(ctx);
                ir.fetch_to_reg(ctx, ret, GP::Rax);
                ir.inst.push(AsmInst::MethodRet(pc));
                return CompileResult::Exit;
            }
            TraceIr::Break(ret) => {
                ir.write_back_locals(ctx);
                ir.fetch_to_reg(ctx, ret, GP::Rax);
                ir.inst.push(AsmInst::Break);
                return CompileResult::Exit;
            }
            TraceIr::Raise(ret) => {
                ir.write_back_locals(ctx);
                ir.fetch_to_reg(ctx, ret, GP::Rax);
                ir.inst.push(AsmInst::Raise);
                return CompileResult::Exit;
            }
            TraceIr::EnsureEnd => {
                ir.write_back_locals(ctx);
                ir.inst.push(AsmInst::EnsureEnd);
            }
            TraceIr::Br(disp) => {
                let next_idx = bb_pos + 1;
                let dest_idx = next_idx + disp;
                let branch_dest = self.jit.label();
                ir.inst.push(AsmInst::Br(branch_dest));
                cc.new_branch(func, bb_pos, dest_idx, ctx.clone(), branch_dest);
                return CompileResult::Exit;
            }
            TraceIr::CondBr(cond_, disp, false, brkind) => {
                let dest_idx = bb_pos + 1 + disp;
                let branch_dest = self.jit.label();
                ir.fetch_to_reg(ctx, cond_, GP::Rax);
                ir.inst.push(AsmInst::CondBr(brkind, branch_dest));
                cc.new_branch(func, bb_pos, dest_idx, ctx.clone(), branch_dest);
            }
            TraceIr::CondBr(_, _, true, _) => {}
            TraceIr::CheckLocal(local, disp) => {
                let dest_idx = bb_pos + 1 + disp;
                let branch_dest = self.jit.label();
                ir.fetch_to_reg(ctx, local, GP::Rax);
                ir.inst.push(AsmInst::CheckLocal(branch_dest));
                cc.new_branch(func, bb_pos, dest_idx, ctx.clone(), branch_dest);
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

                let deopt = ir.new_deopt(pc, ctx.get_write_back());
                ir.fetch_to_reg(ctx, cond, GP::Rdi);
                ir.guard_fixnum(GP::Rdi, deopt);
                ir.opt_case(*max, *min, jump_table, else_dest);
                return CompileResult::Exit;
            }
        }
        CompileResult::Continue
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
