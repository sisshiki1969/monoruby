#[cfg(target_arch = "x86_64")]
use std::collections::HashSet;

#[cfg(target_arch = "x86_64")]
use monoasm_macro::monoasm;
#[cfg(target_arch = "x86_64")]
use paste::paste;

use crate::ast::CmpKind;

use crate::{
    bytecodegen::{BcIndex, UnOpK},
    codegen::jitgen::context::{AsmInfo, JitStackFrame},
};

pub(crate) use crate::basic_block::{BasicBlockId, BasicBlockInfoEntry};
pub(crate) use self::context::JitContext;
pub(crate) use self::state::{AbstractFrame, AbstractState};
use state::{DeoptPoint, LinkMode, ReturnState, TransferIR};

use super::*;
use asmir::*;
use context::{JitArgumentInfo, JitType};
use state::{Liveness, SlotState};
use trace_ir::*;

pub mod asmir;
mod compile;
mod context;
mod definition;
#[allow(dead_code)]
mod gp_alloc;
#[cfg(target_arch = "x86_64")]
mod deoptimize;
// Type / class guards, split per arch (mirrors the asmir `compile` backend):
// each arch's lowering lives under `arch/<arch>/guard.rs`.
#[cfg(target_arch = "x86_64")]
#[path = "arch/x86_64/guard.rs"]
mod guard;
#[cfg(target_arch = "aarch64")]
#[path = "arch/aarch64/guard.rs"]
mod guard;
// Unified low-level IR (Phase-1 Stage 1: data model only, not yet wired in).
pub(in crate::codegen) mod lir;
mod merge;
mod state;
pub mod trace_ir;

type JitResult<T> = std::result::Result<T, CompileError>;

pub(super) struct CompileError;

const RBP_LOCAL_FRAME: i32 = 24;

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
    Branch(BasicBlockId),
    Cease,
    /// raise error.
    Raise,
    /// return from the current method/block.
    Return(ReturnState),
    /// method return from the current method/block.
    MethodReturn(ReturnState),
    /// break from the current method/block.
    Break(ReturnState),
    /// deoptimize and recompile.
    Recompile(RecompileReason),
    /// deoptimize to the VM without recompiling (e.g. a `method_missing`
    /// dispatch site, which the JIT cannot lower but the VM handles — recompiling
    /// would loop forever on `NotCached`).
    Deopt,
    /// internal error.
    #[allow(dead_code)]
    Abort,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct JitLabel(usize);

#[derive(Debug, Clone, PartialEq)]
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
#[derive(Debug, Clone)]
struct BranchEntry {
    /// source BasicBlockId of the branch.
    src_bb: Option<BasicBlockId>,
    /// the abstract state of the source basic block.
    state: AbstractState,
    /// true if the branch is a continuation branch.
    /// 'continuation' means the destination is adjacent to the source basic block on the bytecode.
    mode: BranchMode,
}

pub(crate) fn conv(reg: SlotId) -> i32 {
    reg.0 as i32 * 8 + LFP_SELF
}

pub(crate) fn rbp_local(reg: SlotId) -> i32 {
    RBP_LOCAL_FRAME + reg.0 as i32 * 8 + LFP_SELF
}

///
/// The struct holds information for writing back Value's in fpr registers or pool registers to the corresponding stack slots.
///
/// Currently supports `literal`s, `fpr` registers and `gp` pool registers.
///
#[derive(Clone, PartialEq, Eq)]
pub(crate) struct WriteBack {
    fpr: Vec<(FPReg, Vec<SlotId>)>,
    literal: Vec<(Value, SlotId)>,
    void: Vec<SlotId>,
    /// §9 9d-B: slots resident in the allocatable GP pool (x86-64 `r8`–`r11`),
    /// each paired with the physical register holding it. Written back to the
    /// slot's frame home at every flush / deopt / GC safepoint. Empty until
    /// the GP allocator places a pool slot, so shipping
    /// builds carry an always-empty vec and emit byte-identical code.
    gp: Vec<(GP, SlotId)>,
    /// Deferred forwarding-rest materialization (D1). Each entry
    /// `(dst, src, len)`: the rest-parameter slot `dst` of a
    /// forwarding-trampoline frame was *not* materialized as an `Array`
    /// on the fast path; its positional source args live at
    /// `src .. src + len`. On any side-exit / GC safepoint / frame
    /// capture inside the deferral window the interpreter (or the heap
    /// frame copy) reads the rest local, so the array must be built
    /// here from the source slots and stored into `dst`.
    forward_rest: Vec<(SlotId, SlotId, u16)>,
}

impl Hash for WriteBack {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        for (fpr, slots) in &self.fpr {
            fpr.hash(state);
            for slot in slots {
                slot.hash(state);
            }
        }
        for (val, slot) in &self.literal {
            val.id().hash(state);
            slot.hash(state);
        }
        for slot in &self.void {
            slot.hash(state);
        }
        for (reg, slot) in &self.gp {
            reg.hash(state);
            slot.hash(state);
        }
        for (dst, src, len) in &self.forward_rest {
            dst.hash(state);
            src.hash(state);
            len.hash(state);
        }
    }
}

impl std::fmt::Debug for WriteBack {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut s = String::new();
        for (fpr, slots) in &self.fpr {
            s.push_str(&format!(" {:?}->", fpr));
            for slot in slots {
                s.push_str(&format!("{:?}", slot));
            }
        }
        for (val, slot) in &self.literal {
            s.push_str(&format!(" {:?}->{:?}", val, slot));
        }
        for slot in &self.void {
            s.push_str(&format!(" nil->{:?}", slot));
        }
        for (reg, slot) in &self.gp {
            s.push_str(&format!(" {:?}->{:?}", reg, slot));
        }
        for (dst, src, len) in &self.forward_rest {
            s.push_str(&format!(" fwdrest[{:?};{}]->{:?}", src, len, dst));
        }
        write!(f, "WriteBack({})", s)
    }
}

impl WriteBack {
    fn new(
        fpr: Vec<(FPReg, Vec<SlotId>)>,
        literal: Vec<(Value, SlotId)>,
        void: Vec<SlotId>,
        gp: Vec<(GP, SlotId)>,
        forward_rest: Vec<(SlotId, SlotId, u16)>,
    ) -> Self {
        Self {
            fpr,
            literal,
            void,
            gp,
            forward_rest,
        }
    }
}

#[derive(Clone, Copy, PartialEq)]
pub(crate) struct UsingFpr {
    inner: bitvec::prelude::BitArr!(for 14, in u16),
}

impl std::fmt::Debug for UsingFpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut s = String::new();
        for i in 0..14 {
            if self.inner[i] {
                s.push_str(&format!("%{i}"));
            }
        }
        write!(f, "UsingFpr({})", s)
    }
}

impl std::ops::Deref for UsingFpr {
    type Target = bitvec::prelude::BitArr!(for 14, in u16);
    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl std::ops::DerefMut for UsingFpr {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl UsingFpr {
    fn new() -> Self {
        Self {
            inner: bitvec::prelude::BitArray::new([0; 1]),
        }
    }

    fn offset(&self) -> usize {
        let len = self.count_ones();
        (len + len % 2) * 8
    }
}

#[allow(dead_code)]
#[derive(Debug)]
pub(super) struct SpecializedCodeInfo {
    iseq_id: ISeqId,
    self_class: ClassId,
    childs: Vec<SpecializedCodeInfo>,
}

impl SpecializedCodeInfo {
    fn from(info: &AsmInfo) -> Self {
        let childs = info
            .specialized_methods
            .iter()
            .map(|context::SpecializeInfo { info, .. }| Self::from(info))
            .collect();
        Self {
            iseq_id: info.iseq_id,
            self_class: info.self_class,
            childs,
        }
    }

    #[cfg(feature = "jit-log")]
    pub(super) fn format(&self, store: &Store) -> String {
        let mut buf = String::new();
        self.format_inner(store, &mut buf, 0);
        buf
    }

    #[cfg(feature = "jit-log")]
    fn format_inner(&self, store: &Store, buf: &mut String, level: usize) {
        let indent = " ".repeat(level * 2);
        buf.push_str(&format!(
            "{}- [{:?}] <{}> self_class:{}\n",
            indent,
            self.iseq_id,
            store.func_description(store[self.iseq_id].func_id()),
            store.debug_class_name(self.self_class)
        ));
        for child in &self.childs {
            child.format_inner(store, buf, level + 1);
        }
    }
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
        const_version: u64,
    ) -> JitResult<(
        Vec<(ClassId, Option<IdentId>, FuncId)>,
        SpecializedCodeInfo,
        DestLabel,
    )> {
        let jit_type = if let Some(pos) = position {
            JitType::Loop(pos)
        } else {
            JitType::Entry
        };
        let frame = JitStackFrame::new(store, jit_type, 0, iseq_id, None, self_class, None);
        let mut ctx = JitContext::new(store, true, class_version, const_version, vec![]);
        let mut frame = ctx.traceir_to_asmir(frame)?;
        let specialized_info = SpecializedCodeInfo::from(&frame);

        // Now that every frame's `stack_offset` has been finalised
        // and recorded in `JitContext::specialized_frame_sizes`,
        // rewrite every `DynVarOffset::Hint(...)` in the AsmIr tree
        // into a concrete byte offset before code generation runs.
        ctx.resolve_dyn_var_offsets(&mut frame.asm_info);

        let inline_cache = std::mem::take(&mut ctx.inline_method_cache);

        // Front-end (TraceIR→AsmIR) is arch-neutral and has run by here. The
        // shared `gen_machine_code` driver then drives the per-arch `gen_asm`
        // lowering, which lowers every `AsmInst` on both arches — so machine-code
        // generation never bails (the front-end's `?` above is the only way to
        // fall back to the interpreter).
        self.jit.finalize();
        let class_version_label = self.jit.const_i32(class_version as _);
        // Stage-1 placement shadow (§24): bracket the whole ③ emission (the
        // `gen_machine_code` driver recurses into inlined callees, so one
        // begin/take pair captures the entire compilation unit's FP placement).
        #[cfg(feature = "shadow-placement")]
        crate::codegen::placement_shadow::begin();
        self.gen_machine_code(
            frame.asm_info,
            store,
            entry_label,
            0,
            class_version_label.clone(),
        );
        #[cfg(feature = "shadow-placement")]
        if let Some(fp) = crate::codegen::placement_shadow::take() {
            eprintln!(
                "[shadow] iseq={:?} type={} n={} digest={:#018x}",
                iseq_id,
                if position.is_some() { "loop" } else { "entry" },
                fp.len(),
                crate::codegen::placement_shadow::digest(&fp),
            );
        }
        // x86 finalizes the freshly emitted code here (the old `gen_machine_code`
        // did this internally); aarch64's outer caller (`compile` /
        // `compile_partial` / `compile_patch`) finalizes after any trailing
        // emission (e.g. the class-guard stub).
        #[cfg(target_arch = "x86_64")]
        self.jit.finalize();
        Ok((inline_cache, specialized_info, class_version_label))
    }
}

impl Codegen {
    /// Arch-neutral driver: emit machine code for a whole method and its
    /// inlined specialized callees (recursively), calling the per-arch
    /// `gen_asm` per basic block / bridge. Both arches lower every `AsmInst`,
    /// so this never bails. Does not `finalize`; `jit_compile` does that (x86)
    /// or the outer caller does (aarch64).
    fn gen_machine_code(
        &mut self,
        mut frame: AsmInfo,
        store: &Store,
        entry_label: DestLabel,
        level: usize,
        class_version: DestLabel,
    ) {
        for context::SpecializeInfo {
            entry: specialized_entry,
            info: specialized_info,
            patch_point,
        } in std::mem::take(&mut frame.specialized_methods)
        {
            if !frame.is_specialized() {
                let patch_point = frame.resolve_label(&mut self.jit, patch_point.unwrap());
                self.specialized_info.push((
                    specialized_info.iseq_id,
                    specialized_info.self_class,
                    patch_point,
                ));
            }
            let entry = frame.resolve_label(&mut self.jit, specialized_entry);
            self.gen_machine_code(
                specialized_info,
                store,
                entry,
                level + 1,
                class_version.clone(),
            );
        }

        self.jit.bind_label(entry_label);
        #[cfg(any(feature = "jit-log"))]
        {
            if self.startup_flag {
                let iseq = &store[frame.iseq_id];
                let name = store.func_description(iseq.func_id());
                eprintln!(
                    "  {}>>> [{}] {:?} <{}> self_class:{}",
                    " ".repeat(level * 3),
                    frame.specialize_level(),
                    frame.iseq_id,
                    name,
                    store.debug_class_name(frame.self_class),
                );
            }
        }

        // Sourcemap base for the shared `BcIndex` lowering (emit-asm/perf
        // disassembly only; correctness-neutral). Set unconditionally so both
        // arches agree.
        frame.start_codepos = self.jit.get_current();

        #[cfg(all(feature = "perf", target_arch = "x86_64"))]
        let pair = self.get_address_pair();

        let mut ir_vec = frame.detach_ir();

        // §21 — AsmIR optimization seam (Path 2). `inst` is the ordered,
        // replayable instruction stream; this is the arch-neutral layer where
        // peephole/optimization passes run, between AsmIR construction
        // (`traceir_to_asmir`) and machine-code emission below. The pass runs
        // over every main block and over the inline/outline bridges (their own
        // `AsmIr`s, still held by `frame`), so the seam covers every emitted
        // stream. Optimizing the bridges *before* `thread_empty_outline_bridges`
        // lets a bridge that collapses to nothing be jump-threaded away as usual.
        let peephole_removed: usize = ir_vec
            .iter_mut()
            .map(|(_, ir)| ir.optimize_peephole())
            .sum::<usize>()
            + frame
                .iter_outline_bridges_mut()
                .map(|(ir, _, _)| ir.optimize_peephole())
                .sum::<usize>()
            + frame
                .iter_inline_bridges_mut()
                .map(|(ir, _)| ir.optimize_peephole())
                .sum::<usize>();
        #[cfg(feature = "jit-log")]
        if peephole_removed > 0 {
            eprintln!("  [peephole] removed {peephole_removed} self-move(s)");
        }
        #[cfg(not(feature = "jit-log"))]
        let _ = peephole_removed;

        // Jump-threading: drop empty outline-bridge forwarders and alias their
        // entry labels straight to the destination block. Done *before* the
        // main emission loop so the source branches (emitted there) resolve
        // through the alias. The surviving non-empty bridges are emitted after
        // the main loop as before.
        let outline_bridges = frame.thread_empty_outline_bridges();

        let mut live_bb: HashSet<BasicBlockId> = HashSet::default();
        ir_vec.iter().for_each(|(bb, ir)| {
            if let Some(bb) = bb {
                if !ir.is_empty() || frame.inline_bridge_exists(*bb) {
                    live_bb.insert(*bb);
                }
            }
        });

        // generate machine code for a main context and inlined bridges.
        //
        // `fallthrough_in` tracks whether the next emission is reachable by
        // fall-through from the code just laid down. The first block is
        // entered from the prologue (which falls into it) and via
        // `entry_label`, both of which land *before* the block's inline
        // side-exit handlers, so it starts `true`. Once a block ends in an
        // unconditional terminator (or a bridge ends in an unconditional
        // `b exit`), the following block is reached only by branches to its
        // body label — which bind *after* the handlers — so its `b skip`
        // over those handlers is dead and `gen_asm` (aarch64) drops it.
        let mut fallthrough_in = true;
        for (bbid, ir) in ir_vec.into_iter() {
            let main_ends = ir.ends_unconditionally();
            self.gen_asm(ir, store, &mut frame, None, None, class_version.clone(), fallthrough_in);
            fallthrough_in = !main_ends;
            // generate machine code for the inlined bridge
            if let Some((ir, exit)) = frame.remove_inline_bridge(bbid) {
                let exit = if let Some(bbid) = bbid {
                    if let Some(exit) = exit
                        && (bbid >= exit || ((bbid + 1)..exit).any(|bb| live_bb.contains(&bb)))
                    {
                        Some(exit)
                    } else {
                        None
                    }
                } else {
                    None
                };
                let bridge_ends = exit.is_some() || ir.ends_unconditionally();
                self.gen_asm(ir, store, &mut frame, None, exit, class_version.clone(), fallthrough_in);
                fallthrough_in = !bridge_ends;
            }
        }

        // generate machine code for the surviving (non-empty) outlined
        // bridges. Each is reached only via its `entry` label (it is
        // *outlined* cold code, never fallen into) and ends in an
        // unconditional `b exit`, so neither it nor its successor is
        // fall-through reachable.
        for (ir, entry, exit) in outline_bridges {
            let entry = frame.resolve_label(&mut self.jit, entry);
            self.gen_asm(
                ir,
                store,
                &mut frame,
                Some(entry),
                Some(exit),
                class_version.clone(),
                false,
            );
        }

        if !frame.is_specialized() {
            self.specialized_base = self.specialized_info.len();
        }

        #[cfg(feature = "emit-asm")]
        if self.startup_flag {
            // Resolve branch displacements so the listing shows real targets
            // (the real `finalize` happens in `jit_compile` / the outer caller).
            self.jit.finalize();
            #[cfg(target_arch = "aarch64")]
            {
                let fid = store[frame.iseq_id].func_id();
                eprintln!("  >>> JIT (aarch64) <{}>", store.func_description(fid));
            }
            let iseq_id = frame.iseq_id;
            self.dump_disas(store, &frame.sourcemap, iseq_id);
            eprintln!("  <<<");
        }

        #[cfg(all(feature = "perf", target_arch = "x86_64"))]
        {
            let iseq_id = frame.iseq_id;
            let fid = store[iseq_id].func_id();
            let desc = format!("JIT:<{}>", store.func_description(fid));
            self.perf_info(pair, &desc);
        }
    }
}

#[cfg(target_arch = "x86_64")]
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
                        movq [rbp - (rbp_local(reg))], $reg;
                    }
                }
            }

            ///
            /// load *reg* to $reg
            ///
            #[allow(dead_code)]
            pub(crate) fn [<load_ $reg>](&mut self, reg: SlotId) {
                monoasm!( &mut self.jit,
                    movq $reg, [rbp - (rbp_local(reg))];
                );
            }
        }
    };
}

#[cfg(target_arch = "x86_64")]
impl JitModule {
    load_store!(rax);
    load_store!(rdi);
    load_store!(rsi);
    load_store!(rdx);
    load_store!(rcx);
    load_store!(r15);

    pub(crate) fn fpr_save(&mut self, using_fpr: UsingFpr) {
        self.fpr_save_with_cont(using_fpr, false);
    }

    ///
    /// Save floating point registers in use.
    ///
    /// ### stack pointer adjustment
    /// - -`using_fpr`.offset()
    ///
    fn fpr_save_with_cont(&mut self, using_fpr: UsingFpr, cont: bool) {
        if using_fpr.not_any() && !cont {
            return;
        }
        let sp_offset = using_fpr.offset() + if cont { CONTINUATION_FRAME_SIZE } else { 0 };
        monoasm!( &mut self.jit,
            subq rsp, (sp_offset);
        );
        let mut i = 0;
        for (x, b) in using_fpr.iter().enumerate() {
            if *b {
                monoasm!( &mut self.jit,
                    movq [rsp + (8 * i)], xmm(x as u64 + 2);
                );
                i += 1;
            }
        }
    }

    pub(crate) fn fpr_restore(&mut self, using_fpr: UsingFpr) {
        self.fpr_restore_with_cont(using_fpr, false);
    }

    ///
    /// Restore floating point registers in use.
    ///
    fn fpr_restore_with_cont(&mut self, using_fpr: UsingFpr, cont: bool) {
        if using_fpr.not_any() && !cont {
            return;
        }
        let sp_offset = using_fpr.offset() + if cont { CONTINUATION_FRAME_SIZE } else { 0 };
        let mut i = 0;
        for (x, b) in using_fpr.iter().enumerate() {
            if *b {
                monoasm!( &mut self.jit,
                    movq xmm(x as u64 + 2), [rsp + (8 * i)];
                );
                i += 1;
            }
        }
        monoasm!( &mut self.jit,
            addq rsp, (sp_offset);
        );
    }

    ///
    /// Convert fpr to stack slots *v*. Spill-aware: when *fpr* is
    /// pool-resident the value is moved into xmm0 directly; when it
    /// is spilled it is loaded from `[rbp - spill_off]` into xmm0
    /// before the call to f64_to_val.
    ///
    /// ### out
    /// - rax: Value
    ///
    /// ### destroy
    /// - rcx
    ///
    fn fpr_to_stack(&mut self, fpr: FPReg, v: &[SlotId], base: usize) {
        if v.is_empty() {
            return;
        }
        #[cfg(feature = "jit-debug")]
        eprintln!("      wb: {:?}->{:?}", fpr, v);
        self.load_fpr_into_xmm0(fpr, base);
        let f64_to_val = self.f64_to_val.clone();
        monoasm!( &mut self.jit,
            call f64_to_val;
        );
        for reg in v {
            self.store_rax(*reg);
        }
    }

    fn fpr_to_stack2(&mut self, fpr: FPReg, v: &[SlotId], base: usize) {
        if v.is_empty() {
            return;
        }
        #[cfg(feature = "jit-debug")]
        eprintln!("      wb: {:?}->{:?}", fpr, v);
        self.load_fpr_into_xmm0(fpr, base);
        let f64_to_val = self.f64_to_val.clone();
        monoasm!( &mut self.jit,
            call f64_to_val;
        );
        for reg in v {
            monoasm! { &mut self.jit,
                movq [r14 - (conv(*reg))], rax;
            }
        }
    }

    ///
    /// Move a `VirtFPReg` value into xmm0, choosing the cheapest
    /// instruction based on whether the operand is in the phys pool
    /// or a spill slot. Used by call-trampoline preludes (fpr_to_stack
    /// and CFunc_*) where the helper expects its argument in xmm0.
    ///
    pub(crate) fn load_fpr_into_xmm0(&mut self, fpr: FPReg, base: usize) {
        let pool = PHYS_FPR_POOL;
        if fpr.0 < pool {
            let p = fpr.0 as u64 + 2;
            monoasm!( &mut self.jit,
                movq xmm0, xmm(p);
            );
        } else {
            let n = fpr.0 - pool;
            let off = (base as i32) - 24 + 8 * (n as i32);
            monoasm!( &mut self.jit,
                movq xmm0, [rbp - (off)];
            );
        }
    }

    ///
    /// Move a `VirtFPReg` value into xmm1 — the second SysV f64 arg
    /// register, used by CFunc_FF_F. Pool ids resolve to xmm2..xmm15
    /// so a Phys source never aliases xmm1.
    ///
    pub(crate) fn load_fpr_into_xmm1(&mut self, fpr: FPReg, base: usize) {
        let pool = PHYS_FPR_POOL;
        if fpr.0 < pool {
            let p = fpr.0 as u64 + 2;
            monoasm!( &mut self.jit,
                movq xmm1, xmm(p);
            );
        } else {
            let n = fpr.0 - pool;
            let off = (base as i32) - 24 + 8 * (n as i32);
            monoasm!( &mut self.jit,
                movq xmm1, [rbp - (off)];
            );
        }
    }

    ///
    /// Store xmm0 (a C-call's f64 return value) into the destination
    /// `VirtFPReg`'s home — phys reg or spill slot.
    ///
    pub(crate) fn store_fpr_into_xmm(&mut self, fpr: FPReg, base: usize) {
        let pool = PHYS_FPR_POOL;
        if fpr.0 < pool {
            let p = fpr.0 as u64 + 2;
            monoasm!( &mut self.jit,
                movq xmm(p), xmm0;
            );
        } else {
            let n = fpr.0 - pool;
            let off = (base as i32) - 24 + 8 * (n as i32);
            monoasm!( &mut self.jit,
                movq [rbp - (off)], xmm0;
            );
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
                movq [rbp - (rbp_local(reg))], (v.id());
            }
        } else {
            monoasm! { &mut self.jit,
                movq rax, (v.id());
                movq [rbp - (rbp_local(reg))], rax;
            }
        }
    }

    ///
    /// Move Value *v* to stack slot *reg*.
    ///
    /// ### destroy
    /// - rax
    ///
    fn literal_to_stack2(&mut self, reg: SlotId, v: Value) {
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
    fn deepcopy_literal(&mut self, v: Value, using_fpr: UsingFpr) {
        self.fpr_save(using_fpr);
        monoasm!( &mut self.jit,
          movq rdi, (v.id());
          movq rax, (Value::value_deep_copy);
          call rax;
        );
        self.fpr_restore(using_fpr);
    }

    //
    // Test whether the current local frame is on the stack.
    //
    // if the frame is not on the heap, jump to *label*.
    //
    //fn branch_if_not_captured(&mut self, label: &DestLabel) {
    //    monoasm! { &mut self.jit,
    //        testb [r14 - (LFP_META - META_KIND)], (0b1000_0000_u8 as i8);
    //        jz label;
    //    }
    //}

    ///
    /// Generate a code which write back all fpr registers to corresponding stack slots.
    ///
    /// fprs are not deallocated.
    ///
    /// ### destroy
    ///
    /// - rax, rcx
    ///
    pub(super) fn gen_write_back(&mut self, wb: &WriteBack, base: usize) {
        for (fpr, v) in &wb.fpr {
            self.fpr_to_stack(*fpr, v, base);
        }
        for (v, slot) in &wb.literal {
            self.literal_to_stack(*slot, *v);
        }
        for slot in &wb.void {
            self.literal_to_stack(*slot, Value::nil());
        }
        for (reg, slot) in &wb.gp {
            monoasm! { &mut *self,
                movq [rbp - (rbp_local(*slot))], R(*reg as _);
            }
        }
    }

    ///
    /// Generate a code which write back all fpr registers to corresponding stack slots for deopt.
    ///
    /// We must use r14-based addressing here, because the local frame can be on the heap just after returning from a method.
    ///
    /// fprs are not deallocated.
    ///
    /// ### destroy
    ///
    /// - rax, rcx
    ///
    pub(super) fn gen_write_back_for_deopt(&mut self, wb: &WriteBack, base: usize) {
        for (fpr, v) in &wb.fpr {
            self.fpr_to_stack2(*fpr, v, base);
        }
        for (v, slot) in &wb.literal {
            self.literal_to_stack2(*slot, *v);
        }
        for slot in &wb.void {
            self.literal_to_stack2(*slot, Value::nil());
        }
        for (reg, slot) in &wb.gp {
            monoasm! { &mut *self,
                movq [r14 - (conv(*slot))], R(*reg as _);
            }
        }
        // D1: materialize deferred forwarding-rest arrays. Runs last so
        // the literal loop above has already written the `dst` slot
        // (mode `C(nil)`) — keeping the frame GC-consistent during the
        // `create_array` call (which may itself allocate). The source
        // positional args live in the *caller* (outermost, non-
        // specialized) JIT frame, so they are addressed `rbp`-relative
        // (rbp is the stable outermost frame pointer, valid until the
        // JIT method returns — and at an in-window side-exit neither the
        // caller nor `f` has returned). `dst` is `f`'s rest local,
        // addressed `r14`-relative like every other deopt restore.
        for (dst, src, len) in wb.forward_rest.clone() {
            self.gen_forward_rest_materialize(dst, src, len);
        }
    }

    fn gen_forward_rest_materialize(&mut self, dst: SlotId, src: SlotId, len: u16) {
        // The deferred trampoline frame `f` established its own `rbp`
        // (`init_func`'s `pushq rbp; movq rbp, rsp`), so the dynamic
        // caller's `rbp` is the value `f` saved at `[rbp]`. The
        // structural gate guarantees the caller is exactly one
        // (outermost, non-specialized) level up, so the positional
        // source slots live at `[caller_rbp - rbp_local(src + i)]`.
        // `dst` (f's rest local) is r14-relative like every other deopt
        // restore.
        monoasm! { self,
            movq rcx, [rbp];
            lea  rdi, [rcx - (rbp_local(src))];
            movq rsi, (len as usize);
            movq rax, (runtime::create_array);
            call rax;
            movq [r14 - (conv(dst))], rax;
        }
    }
}

#[cfg(target_arch = "x86_64")]
impl Codegen {
    pub(in crate::codegen) fn gen_handle_error(
        &mut self,
        pc: BytecodePtr,
        wb: WriteBack,
        entry: DestLabel,
        base: usize,
    ) {
        let raise = self.entry_raise();
        assert_eq!(0, self.jit.get_page());
        self.jit.select_page(1);
        monoasm!( &mut self.jit,
        entry:
        );
        self.gen_write_back_for_deopt(&wb, base);
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
    pub(in crate::codegen) fn gen_deopt_with_label(
        &mut self,
        pc: BytecodePtr,
        wb: &WriteBack,
        entry: DestLabel,
        loop_jit_spill_bytes: usize,
        base: usize,
    ) {
        self.side_exit_with_label(pc, wb, entry, false, None, loop_jit_spill_bytes, base)
    }

    ///
    /// Like `gen_deopt_with_label`, but after the deopt write-back
    /// (so all live Ruby values are on the LFP and GC-safe) and
    /// before falling back to the interpreter, recompile the
    /// method/loop with *reason* once a small miss counter is
    /// exhausted. Used for the receiver-class guard of
    /// monomorphic-compiled BinCmp sites so they flip to the
    /// non-deopting polymorphic path (Part B).
    ///
    pub(in crate::codegen) fn gen_recompile_deopt_with_label(
        &mut self,
        pc: BytecodePtr,
        wb: &WriteBack,
        reason: RecompileReason,
        position: Option<BytecodePtr>,
        entry: DestLabel,
        loop_jit_spill_bytes: usize,
        base: usize,
    ) {
        self.side_exit_with_label(
            pc,
            wb,
            entry,
            false,
            Some((reason, position)),
            loop_jit_spill_bytes,
            base,
        )
    }

    ///
    /// Get *DestLabel* for fallback to interpreter by immediate eviction.
    ///
    pub(in crate::codegen) fn gen_evict_with_label(
        &mut self,
        pc: BytecodePtr,
        wb: &WriteBack,
        entry: DestLabel,
        loop_jit_spill_bytes: usize,
        base: usize,
    ) {
        self.side_exit_with_label(pc, wb, entry, true, None, loop_jit_spill_bytes, base)
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
        recompile: Option<(RecompileReason, Option<BytecodePtr>)>,
        loop_jit_spill_bytes: usize,
        base: usize,
    ) {
        assert_eq!(0, self.jit.get_page());
        self.jit.select_page(1);
        self.jit.bind_label(entry);
        // Deopt write-back FIRST, while the Loop-JIT rsp bump is still
        // in effect. The write-back boxes spilled floats with `call`s
        // (e.g. `f64_to_val`), and a `call` pushes its return address at
        // `[rsp-8]`. The bump is what keeps rsp *below* the spill region
        // (`[rbp - (base-24+8n)]`); if we undid it first, those pushed
        // return addresses would land on the very spill slots the
        // write-back is about to read, corrupting a live float (it would
        // box a code pointer back to the VM). See the deopt-bridge
        // analysis in doc/regalloc_separation.md §39.
        self.gen_write_back_for_deopt(wb, base);
        // Now undo the rsp bump that Loop JIT applied at its entry (see
        // `AsmInst::LoopJitRspBump`). Method / specialized JITs restore
        // rsp implicitly via their `leave; ret` epilogue, so the
        // adjustment is Loop-specific. `loop_jit_spill_bytes` is `0` for
        // non-Loop frames or Loop frames without spill, matching the
        // entry-side `subq rsp, _` exactly.
        if loop_jit_spill_bytes > 0 {
            monoasm!( &mut self.jit,
                addq rsp, (loop_jit_spill_bytes as i32);
            );
        }
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
        // Part B: counter-gated, one-shot recompile. Emitted AFTER
        // the write-back above (LFP holds all live Ruby values, so a
        // GC inside `jit_recompile_loop` is safe) and BEFORE the
        // interpreter fallback. The counter lets the interpreter run
        // the generic op a few times first (so the VM has set the
        // site's POLY bit) before we recompile; once exhausted it
        // never recompiles again (monotone / one-shot).
        if let Some((reason, position)) = recompile {
            let recompile_lbl = self.jit.label();
            let skip = self.jit.label();
            let counter = self.jit.data_i32(COUNT_DEOPT_RECOMPILE);
            monoasm!( &mut self.jit,
                cmpl [rip + counter], 0;
                jle  skip;
                subl [rip + counter], 1;
                jne  skip;
            );
            self.gen_recompile(position, recompile_lbl, reason, None);
            monoasm!( &mut self.jit,
            skip:
            );
        }
        let fetch = self.vm_fetch();
        monoasm!( &mut self.jit,
            jmp fetch;
        );
        self.jit.select_page(0);
    }
}

#[cfg(target_arch = "x86_64")]
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

#[cfg(target_arch = "x86_64")]
#[test]
fn float_test2() {
    let mut r#gen = Codegen::new();

    let assume_int_to_f64 = r#gen.jit.label();
    let x = 2;
    monoasm!(&mut r#gen.jit,
    assume_int_to_f64:
        pushq rbp;
    );
    r#gen.integer_val_to_f64(GP::Rdi, x);
    monoasm!(&mut r#gen.jit,
        movq xmm0, xmm(x);
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
