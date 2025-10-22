use std::collections::HashSet;

use monoasm_macro::monoasm;
use paste::paste;
use ruruby_parse::CmpKind;

use crate::bytecodegen::{BcIndex, UnOpK};

pub(crate) use self::basic_block::{BasciBlockInfoEntry, BasicBlockId, BasicBlockInfo};
pub use self::context::JitContext;
use self::slot::Guarded;

use super::*;
//use analysis::{ExitType, SlotInfo};
use asmir::*;
use context::{JitArgumentInfo, JitType};
use slot::{Liveness, SlotContext};
use trace_ir::*;

//pub mod analysis;
pub mod asmir;
mod basic_block;
mod binary_op;
mod compile;
mod context;
mod definition;
mod deoptimize;
mod guard;
mod index;
mod init_method;
mod merge;
mod method_call;
mod slot;
pub mod trace_ir;
mod variables;

///
/// Compile result of the current instruction.
///
///
#[derive(Debug)]
enum CompileResult {
    /// continue to the next instruction.
    Continue,
    /// exit from the loop.
    ExitLoop,
    /// jump to another basic block.
    Branch,
    /// leave the current method/block.
    Leave,
    /// deoptimize and recompile.
    Recompile(RecompileReason),
    /// internal error.
    #[allow(dead_code)]
    Abort,
}

#[derive(Debug, Clone, Copy, PartialEq)]
struct JitLabel(usize);

#[derive(Debug, PartialEq)]
enum BranchMode {
    ///
    /// Continuation branch.
    ///
    /// 'continuation' means the destination is adjacent to the source basic block on the bytecode.
    ///
    Continue,
    ///
    /// Side branch. (conditional branch)
    ///
    /// The machine code for the branch is outlined.
    ///
    Side { dest: JitLabel },
    ///
    /// Branch. (unconditional branch)
    ///
    /// The machine code for the branch is inlined.
    ///
    Branch,
}

///
/// The information for branches.
///
#[derive(Debug)]
struct BranchEntry {
    /// source BasicBlockId of the branch.
    src_bb: BasicBlockId,
    /// context of the source basic block.
    bbctx: BBContext,
    /// true if the branch is a continuation branch.
    /// 'continuation' means the destination is adjacent to the source basic block on the bytecode.
    mode: BranchMode,
}

pub(crate) fn conv(reg: SlotId) -> i32 {
    reg.0 as i32 * 8 + LFP_SELF
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
    class_version_guarded: bool,
    pc: Option<BytecodePtr>,
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
        let sp = SlotId(cc.local_num() as u16 + 1);
        Self {
            slot_state: SlotContext::from(cc),
            sp,
            next_sp: sp,
            class_version_guarded: false,
            pc: None,
        }
    }

    fn new_with_args(cc: &JitContext) -> Self {
        let sp = SlotId(cc.local_num() as u16 + 1);
        Self {
            slot_state: SlotContext::from_args(cc),
            sp,
            next_sp: sp,
            class_version_guarded: false,
            pc: None,
        }
    }

    fn from_target(ctx: &JitContext, target: &SlotContext, use_set: &[(SlotId, bool)]) -> Self {
        let mut bbctx = BBContext::new(ctx);
        bbctx.set_guard_from(target);
        for (slot, coerced) in use_set {
            match target.mode(*slot) {
                LinkMode::S => {}
                LinkMode::C(v) => {
                    if v.is_float() {
                        bbctx.def_new_F(*slot);
                    }
                }
                LinkMode::F(r) if !coerced => {
                    bbctx.def_F(*slot, r);
                }
                LinkMode::Sf(r) | LinkMode::F(r) => {
                    bbctx.def_Sf(*slot, r, Guarded::Value);
                }
                LinkMode::G | LinkMode::V => unreachable!(),
            };
        }
        bbctx
    }

    fn set_guard_from(&mut self, merger: &SlotContext) {
        self.slot_state.set_guard_from(merger)
    }

    fn pc(&self) -> BytecodePtr {
        self.pc.unwrap()
    }

    fn set_pc(&mut self, pc: BytecodePtr) {
        self.pc = Some(pc);
    }

    fn set_class_version_guard(&mut self) {
        self.class_version_guarded = true;
    }

    fn unset_class_version_guard(&mut self) {
        self.class_version_guarded = false;
    }

    fn join_entries(entries: &[BranchEntry]) -> Self {
        let mut merge_ctx = entries.last().unwrap().bbctx.clone();
        for BranchEntry {
            src_bb: _src_bb,
            bbctx,
            ..
        } in entries.iter()
        {
            #[cfg(feature = "jit-debug")]
            eprintln!("  <-{:?}:[{:?}] {:?}", _src_bb, bbctx.sp, bbctx.slot_state);
            merge_ctx.join(bbctx);
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

    pub(crate) fn def_rax2acc(&mut self, ir: &mut AsmIr, dst: impl Into<Option<SlotId>>) {
        self.def_reg2acc(ir, GP::Rax, dst);
    }

    pub(crate) fn def_reg2acc(&mut self, ir: &mut AsmIr, src: GP, dst: impl Into<Option<SlotId>>) {
        self.def_reg2acc_guarded(ir, src, dst, slot::Guarded::Value)
    }

    pub(crate) fn def_reg2acc_fixnum(
        &mut self,
        ir: &mut AsmIr,
        src: GP,
        dst: impl Into<Option<SlotId>>,
    ) {
        self.def_reg2acc_guarded(ir, src, dst, slot::Guarded::Fixnum)
    }

    pub(crate) fn def_reg2acc_class(
        &mut self,
        ir: &mut AsmIr,
        src: GP,
        dst: impl Into<Option<SlotId>>,
        class: ClassId,
    ) {
        self.def_reg2acc_guarded(ir, src, dst, slot::Guarded::Class(class))
    }

    pub(crate) fn def_reg2acc_concrete_value(
        &mut self,
        ir: &mut AsmIr,
        src: GP,
        dst: impl Into<Option<SlotId>>,
        v: Value,
    ) {
        self.def_reg2acc_guarded(ir, src, dst, Guarded::from_concrete_value(v))
    }

    fn def_reg2acc_guarded(
        &mut self,
        ir: &mut AsmIr,
        src: GP,
        dst: impl Into<Option<SlotId>>,
        guarded: slot::Guarded,
    ) {
        if let Some(dst) = dst.into() {
            self.clear_above_next_sp();
            self.def_acc(ir, dst, guarded);
            ir.push(AsmInst::RegToAcc(src));
        }
    }

    pub(crate) fn writeback_acc(&mut self, ir: &mut AsmIr) {
        let sp = self.sp;
        self.slot_state.write_back_acc(ir, sp);
    }

    ///
    /// Guard for the base class object of the constant in *slot*.
    ///
    /// ### destroy
    /// - rax
    ///
    pub fn guard_const_base_class(&mut self, ir: &mut AsmIr, slot: SlotId, base_class: Value) {
        self.fetch(ir, slot, GP::Rax);
        let deopt = ir.new_deopt(self);
        ir.inst
            .push(AsmInst::GuardConstBaseClass { base_class, deopt });
    }

    pub fn exec_gc(&self, ir: &mut AsmIr, check_stack: bool) {
        let wb = self.get_gc_write_back();
        let error = ir.new_error(self);
        ir.exec_gc(wb, error, check_stack);
    }

    pub fn load_constant(&mut self, ir: &mut AsmIr, dst: SlotId, cache: &ConstCache) {
        let ConstCache { version, value, .. } = cache;
        let deopt = ir.new_deopt(self);
        ir.push(AsmInst::GuardConstVersion {
            const_version: *version,
            deopt,
        });
        ir.lit2reg(*value, GP::Rax);
        if let Some(f) = value.try_float() {
            let fdst = self.def_new_Sf_float(dst);
            ir.f64_to_xmm(f, fdst);
            ir.reg2stack(GP::Rax, dst);
        } else {
            self.def_reg2acc(ir, GP::Rax, dst);
        }
    }

    pub(super) fn generic_unop(&mut self, ir: &mut AsmIr, func: UnaryOpFn) {
        let using_xmm = self.get_using_xmm();
        let error = ir.new_error(self);
        ir.push(AsmInst::GenericUnOp { func, using_xmm });
        ir.handle_error(error);
    }
}

///
/// The struct holds information for writing back Value's in xmm registers or accumulator to the corresponding stack slots.
///
/// Currently supports `literal`s, `xmm` registers and a `R15` register (as an accumulator).
///
#[derive(Clone)]
pub(crate) struct WriteBack {
    xmm: Vec<(Xmm, Vec<SlotId>)>,
    literal: Vec<(Value, SlotId)>,
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
        r15: Option<SlotId>,
    ) -> Self {
        Self { xmm, literal, r15 }
    }

    fn is_empty(&self) -> bool {
        self.xmm.is_empty() && self.literal.is_empty() && self.r15.is_none()
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
enum LinkMode {
    ///
    /// No Value.
    ///
    /// this is used for the temp slots above sp.
    ///
    V,
    ///
    /// On the stack slot.
    ///
    #[default]
    S,
    ///
    /// On the floating point register (xmm).
    ///
    /// mutation of the corresponding FPR lazily affects the stack slot.
    ///
    F(Xmm),
    ///
    /// On the stack slot and on the floating point register (xmm) which is read-only.
    ///
    Sf(Xmm),
    ///
    /// Concrete value.
    ///
    C(Value),
    ///
    /// On the general-purpose register (r15).
    ///
    G,
}

impl Codegen {
    pub(super) fn jit_compile(
        &mut self,
        store: &Store,
        iseq_id: ISeqId,
        self_class: ClassId,
        position: Option<BytecodePtr>,
        entry_label: DestLabel,
        class_version: u32,
        class_version_label: DestLabel,
    ) -> Vec<(BcIndex, InlineCacheType)> {
        let jit_type = if let Some(pos) = position {
            JitType::Loop(pos)
        } else {
            JitType::Method
        };

        let mut ctx = JitContext::new(
            store,
            iseq_id,
            jit_type,
            class_version,
            class_version_label,
            self_class,
            0,
        );
        ctx.compile(store);
        self.jit.finalize();

        let inline_cache = std::mem::take(&mut ctx.inline_method_cache);

        self.gen_machine_code(ctx, store, entry_label, 0);

        inline_cache
            .into_iter()
            .map(|(pc, entry)| (pc, InlineCacheType::Method(entry)))
            .collect()
    }

    fn gen_machine_code(
        &mut self,
        mut ctx: JitContext,
        store: &Store,
        entry_label: DestLabel,
        level: usize,
    ) {
        for context::SpecializeInfo {
            entry: specialized_entry,
            ctx: specialized_ctx,
            patch_point,
        } in std::mem::take(&mut ctx.specialized_methods)
        {
            if !ctx.is_specialized() {
                let patch_point = ctx.resolve_label(&mut self.jit, patch_point.unwrap());
                self.specialized_info.push((
                    specialized_ctx.iseq_id(),
                    specialized_ctx.self_class(),
                    patch_point,
                ));
            }
            let entry = ctx.resolve_label(&mut self.jit, specialized_entry);
            self.gen_machine_code(specialized_ctx, store, entry, level + 1);
        }

        self.jit.bind_label(entry_label);
        #[cfg(any(feature = "emit-asm", feature = "jit-log"))]
        {
            if self.startup_flag {
                let iseq = &store[ctx.iseq_id()];
                let name = store.func_description(iseq.func_id());
                eprintln!(
                    "  {}>>> [{}] {:?} <{}> self_class:{} {:?} {:?}",
                    " ".repeat(level * 3),
                    ctx.specialize_level(),
                    ctx.iseq_id(),
                    name,
                    store.debug_class_name(ctx.self_class()),
                    ctx.stack_frame,
                    ctx.jit_type(),
                );
            }
        }

        #[cfg(feature = "emit-asm")]
        {
            ctx.start_codepos = self.jit.get_current();
        }

        #[cfg(feature = "perf")]
        let pair = self.get_address_pair();

        let ir_vec = std::mem::take(&mut ctx.ir);

        let mut live_bb: HashSet<BasicBlockId> = HashSet::default();
        ir_vec.iter().for_each(|(bb, ir)| {
            if let Some(bb) = bb {
                if !ir.inst.is_empty() || ctx.inline_bridges.contains_key(&bb) {
                    live_bb.insert(*bb);
                }
            }
        });

        // generate machine code for a main context and inlined bridges.
        for (bbid, ir) in ir_vec.into_iter() {
            self.gen_asm(ir, store, &mut ctx, None, None);
            // generate machine code for bridges
            if let Some(bbid) = bbid
                && let Some((ir, exit)) = ctx.inline_bridges.remove(&bbid)
            {
                let exit = if let Some(exit) = exit
                    && (bbid >= exit || ((bbid + 1)..exit).any(|bb| live_bb.contains(&bb)))
                {
                    Some(exit)
                } else {
                    None
                };
                self.gen_asm(ir, store, &mut ctx, None, exit);
            }
        }

        // generate machine code for outlined bridges
        for (ir, entry, exit) in std::mem::take(&mut ctx.outline_bridges) {
            let entry = ctx.resolve_label(&mut self.jit, entry);
            self.gen_asm(ir, store, &mut ctx, Some(entry), Some(exit));
        }

        if !ctx.is_specialized() {
            self.specialized_base = self.specialized_info.len();
        }
        self.jit.finalize();

        #[cfg(feature = "emit-asm")]
        if self.startup_flag {
            let iseq_id = ctx.iseq_id();
            self.dump_disas(store, &ctx.sourcemap, iseq_id);
            eprintln!("  <<<");
        }

        #[cfg(feature = "perf")]
        {
            let iseq_id = ctx.iseq_id();
            let fid = store[iseq_id].func_id();
            let desc = format!("JIT:<{}>", store.func_description(fid));
            self.perf_info(pair, &desc);
        }
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

impl JitModule {
    load_store!(rax);
    load_store!(rdi);
    load_store!(rsi);
    load_store!(rdx);
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

    ///
    /// Save floating point registers in use.
    ///
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

    ///
    /// Restore floating point registers in use.
    ///
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

    ///
    /// Convert xmm to stack slots *v*.
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
        let f64_to_val = self.f64_to_val.clone();
        monoasm!( &mut self.jit,
            movq xmm0, xmm(xmm.enc());
            call f64_to_val;
        );
        for reg in v {
            self.store_rax(*reg);
        }
    }

    ///
    /// Move Value *v* to stack slot *reg*.
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
    /// Deep copy *v* and store it to `rax`.
    ///
    /// ### out
    /// - rax: Value
    ///
    /// ### destroy
    /// - caller save registers
    ///
    fn deepcopy_literal(&mut self, v: Value, using_xmm: UsingXmm) {
        self.xmm_save(using_xmm);
        monoasm!( &mut self.jit,
          movq rdi, (v.id());
          movq rax, (Value::value_deep_copy);
          call rax;
        );
        self.xmm_restore(using_xmm);
    }

    ///
    /// Test whether the current local frame is on the heap ("captured").
    ///
    /// if the frame is on the heap, jump to *label*.
    ///
    pub(super) fn branch_if_captured(&mut self, label: &DestLabel) {
        monoasm! { &mut self.jit,
            testb [r14 - (LFP_META - META_KIND)], (0b1000_0000_u8 as i8);
            jnz label;
        }
    }

    ///
    /// Test whether the current local frame is on the stack.
    ///
    /// if the frame is not on the heap, jump to *label*.
    ///
    fn branch_if_not_captured(&mut self, label: &DestLabel) {
        monoasm! { &mut self.jit,
            testb [r14 - (LFP_META - META_KIND)], (0b1000_0000_u8 as i8);
            jz label;
        }
    }

    ///
    /// Generate a code which write back all xmm registers to corresponding stack slots.
    ///
    /// xmms are not deallocated.
    ///
    /// ### destroy
    ///
    /// - rax, rcx
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
    }

    ///
    /// Generate a code which write back all xmm registers to corresponding stack slots.
    ///
    /// xmms are not deallocated.
    ///
    /// ### destroy
    ///
    /// - rax, rcx
    ///
    pub(super) fn gen_write_back_if_captured(&mut self, wb: &WriteBack) {
        if wb.is_empty() {
            return;
        }
        let exit = self.jit.label();
        self.branch_if_not_captured(&exit);
        self.gen_write_back(wb);
        self.bind_label(exit);
    }
}

impl Codegen {
    fn gen_handle_error(&mut self, pc: BytecodePtr, wb: WriteBack, entry: DestLabel) {
        let raise = self.entry_raise();
        assert_eq!(0, self.jit.get_page());
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
        let fetch = self.vm_fetch();
        monoasm!( &mut self.jit,
            jmp fetch;
        );
        self.jit.select_page(0);
    }
}

#[test]
fn float_test() {
    let r#gen = Codegen::new();

    let from_f64_entry = r#gen.jit.get_label_address(&r#gen.f64_to_val);
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
    let mut r#gen = Codegen::new();

    let assume_int_to_f64 = r#gen.jit.label();
    let x = Xmm(0);
    monoasm!(&mut r#gen.jit,
    assume_int_to_f64:
        pushq rbp;
    );
    r#gen.integer_val_to_f64(GP::Rdi, x);
    monoasm!(&mut r#gen.jit,
        movq xmm0, xmm(x.enc());
        popq rbp;
        ret;
    );
    r#gen.jit.finalize();
    let int_to_f64_entry = r#gen.jit.get_label_address(&assume_int_to_f64);

    let int_to_f64: fn(Value) -> f64 = unsafe { std::mem::transmute(int_to_f64_entry.as_ptr()) };
    assert_eq!(143.0, int_to_f64(Value::integer(143)));
    assert_eq!(14354813558.0, int_to_f64(Value::integer(14354813558)));
    assert_eq!(-143.0, int_to_f64(Value::integer(-143)));
}
