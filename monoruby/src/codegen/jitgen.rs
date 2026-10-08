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
use self::context::CondBrSink;
pub(in crate::codegen) use self::context::SplicePlan;
pub(crate) use self::state::{AbstractFrame, AbstractState};
#[cfg(feature = "profile")]
pub(crate) use self::state::join_profile;
use state::{DeoptPoint, FrameRef, Keep, LinkMode, ReturnState};

use super::*;
use asmir::*;
use context::{JitArgumentInfo, JitType};
use state::Liveness;
use trace_ir::*;

pub mod asmir;
mod compile;
pub(in crate::codegen) mod context;
mod definition;
pub(crate) mod deopt_log;
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
mod spec_memo;
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
pub(crate) struct JitLabel(usize);

///
/// What the CPU condition flags hold after a flag-setting AsmIR instruction
/// (`CmpFlags` / `CmpImmFlags` / `TestBitFlags` / `FloatCmpFlags`), i.e. how
/// to read a Ruby truth value out of them.
///
/// A comparison or predicate whose boolean is consumed only by the following
/// conditional branch (bytecodegen marked that `CondBr` optimizable) leaves
/// its answer here instead of materializing a `true`/`false` `Value`: the
/// producer records the flags in the [`JitContext`]
/// (`JitContext::set_cond_flags`) and the `CondBr` branches on them directly
/// (`AsmInst::BrFlags`).
///
#[derive(Clone, Copy, Debug, PartialEq)]
pub(crate) enum CondFlags {
    /// Integer flags: the predicate is true when the signed integer
    /// condition `kind` holds (`TEq` reads as `Eq`). After a bit test
    /// (`TestBitFlags`), `Ne` means the bit is set.
    Int(CmpKind),
    /// Float flags (`ucomisd` / `fcmp`): the predicate is `kind`, NaN-aware
    /// (an unordered result is false for everything but `!=`).
    Float(CmpKind),
}

///
/// What a binary inline generator did.
///
pub(crate) enum BinaryInlineOutcome {
    /// Code (or a constant fold) was emitted; state updated. A comparison
    /// whose result feeds the next conditional branch may have left it in
    /// the condition flags instead (`JitContext::set_cond_flags`).
    Done,
    /// The generator declined; the caller rolls back and takes the ordinary
    /// method-call path.
    Declined,
}

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
    /// Deferred forwarding-kwrest materialization (K1). Each entry
    /// `(dst, [(name, caller slot)])`: the `**kwrest` slot `dst` of a
    /// forwarding-trampoline frame was *not* materialized as a Hash on
    /// the fast path (the caller passed only literal keywords, routed
    /// straight to the forwarded callee); a side exit rebuilds the Hash
    /// from the caller's kw slots via `correct_rest_kw`.
    forward_kwrest: Vec<(SlotId, Box<[(IdentId, SlotId)]>)>,
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
        for (dst, table) in &self.forward_kwrest {
            dst.hash(state);
            for (name, slot) in table.iter() {
                name.hash(state);
                slot.hash(state);
            }
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
        for (dst, table) in &self.forward_kwrest {
            s.push_str(&format!(" fwdkwrest[{:?}]->{:?}", table, dst));
        }
        write!(f, "WriteBack({})", s)
    }
}

impl WriteBack {
    /// Whether a chain-deopt replay of this write-back would do nothing —
    /// i.e. the suspended frame is already in interpreter-consistent shape
    /// and only its return address has to be rewritten. Counted under
    /// `jit-log` to size the "write everything back at cross-unit calls"
    /// question: if conversions are mostly empty already, the conservative
    /// spill costs little and removes the replay entirely.
    #[cfg(feature = "jit-log")]
    pub(crate) fn is_replay_empty(&self) -> bool {
        self.fpr.is_empty()
            && self.literal.is_empty()
            && self.void.is_empty()
            && self.gp.is_empty()
            && self.forward_rest.is_empty()
            && self.forward_kwrest.is_empty()
    }

    #[cfg(feature = "jit-log")]
    pub(crate) fn has_unboxed_float(&self) -> bool {
        !self.fpr.is_empty()
    }

    /// Read-only views for the compiled replay stub, which needs the same
    /// data the interpreted replay walked.
    pub(crate) fn fpr_entries(&self) -> &[(FPReg, Vec<SlotId>)] {
        &self.fpr
    }

    pub(crate) fn literal_entries(&self) -> &[(Value, SlotId)] {
        &self.literal
    }

    pub(crate) fn void_entries(&self) -> &[SlotId] {
        &self.void
    }

    pub(crate) fn gp_is_empty(&self) -> bool {
        self.gp.is_empty()
    }

    pub(crate) fn set_void(&mut self, void: Vec<SlotId>) {
        self.void = void;
    }

    pub(crate) fn forward_rest_entries(&self) -> &[(SlotId, SlotId, u16)] {
        &self.forward_rest
    }

    pub(crate) fn forward_kwrest_entries(&self) -> &[(SlotId, Box<[(IdentId, SlotId)]>)] {
        &self.forward_kwrest
    }


    fn new(
        fpr: Vec<(FPReg, Vec<SlotId>)>,
        literal: Vec<(Value, SlotId)>,
        void: Vec<SlotId>,
        gp: Vec<(GP, SlotId)>,
        forward_rest: Vec<(SlotId, SlotId, u16)>,
        forward_kwrest: Vec<(SlotId, Box<[(IdentId, SlotId)]>)>,
    ) -> Self {
        Self {
            fpr,
            literal,
            void,
            gp,
            forward_rest,
            forward_kwrest,
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

impl Default for UsingFpr {
    fn default() -> Self {
        Self::new()
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

    /// Every register in *other* is in `self`.
    pub(crate) fn is_superset_of(&self, other: &Self) -> bool {
        (0..PHYS_FPR_POOL).all(|i| !other.inner[i] || self.inner[i])
    }
}

///
/// Compile-time half of a call site's chain-deopt registration
/// (`doc/chain_deopt.md` §2/§9.3): everything the eager replay needs that is
/// known while building `AsmIr`. The frame's spill `base` is attached at
/// lowering time (`AsmInst::ChainExit` → [`ChainReplay`]), where
/// `base_stack_offset` is available.
///
#[derive(Debug, Clone)]
pub(crate) struct ChainExitSpec {
    wb: WriteBack,
    using_fpr: UsingFpr,
    dst: Option<SlotId>,
    pc: BytecodePtr,
}

impl ChainExitSpec {
    ///
    /// Capture this call site's replay data. Must be called at the point
    /// where the outgoing frame is fully set up and `dst` has already been
    /// discarded — i.e. next to `new_error` — so the write-back describes the
    /// frame exactly as the post-call continuation expects to find it: every
    /// live value homed in the LFP, `dst` still untouched (the continuation
    /// stores it from the callee's return value).
    ///
    /// `using_fpr` is the call's `FprSave` set, which fixes the layout of the
    /// save area the replay reads the pool-resident floats out of.
    ///
    pub(crate) fn new(
        state: &AbstractFrame,
        using_fpr: UsingFpr,
        dst: Option<SlotId>,
    ) -> Self {
        Self {
            wb: state.get_write_back(),
            using_fpr,
            dst,
            pc: state.pc(),
        }
    }

    /// The same snapshot from a write-back taken by the caller — one that
    /// also covers the window's unwritten slots when the frame is itself
    /// frameless (`AsmIr::exit_write_back`), for `AsmInst::InlineCall`.
    pub(crate) fn new_with(
        wb: WriteBack,
        using_fpr: UsingFpr,
        dst: Option<SlotId>,
        pc: BytecodePtr,
    ) -> Self {
        Self {
            wb,
            using_fpr,
            dst,
            pc,
        }
    }

    pub(in crate::codegen) fn into_replay(self, base: usize) -> ChainReplay {
        ChainReplay {
            wb: self.wb,
            base,
            using_fpr: self.using_fpr,
            dst: self.dst,
            pc: self.pc,
        }
    }
}

///
/// Runtime half of a call site's chain-deopt registration: the value of
/// `Codegen::chain_deopt_table`, keyed by the call's return address. Carries
/// what [`Codegen::gen_chain_replay_stub`] compiles into the site's
/// conversion stub: (a) the suspended caller frame's write-back replay and
/// (b) the per-site post-call data (`dst`, resume-pc advance) the stub hands
/// the shared VM continuation stub through the cont-frame pad slot.
///
#[derive(Debug, Clone)]
pub(crate) struct ChainReplay {
    wb: WriteBack,
    /// The caller frame's spill `base` (`base_stack_offset`), which fixes the
    /// `[rbp/x29 - (base - 24 + 8n)]` addresses of its spilled `FPReg`s.
    base: usize,
    using_fpr: UsingFpr,
    dst: Option<SlotId>,
    /// The call-site pc.
    pc: BytecodePtr,
}

impl ChainReplay {
    #[cfg(feature = "jit-log")]
    pub(crate) fn write_back(&self) -> &WriteBack {
        &self.wb
    }


    pub(crate) fn write_back_all(&self) -> &WriteBack {
        &self.wb
    }

    pub(crate) fn base(&self) -> usize {
        self.base
    }

    /// The call-site pc.
    pub(crate) fn pc(&self) -> BytecodePtr {
        self.pc
    }

    /// Where the call's `FprSave` put a pool-resident `FPReg`: one slot per
    /// set bit of `using_fpr`, in bit order, from `callee_bp + 32`. `None`
    /// for a register outside the pool, whose value stayed in the caller's
    /// own spill slot.
    pub(crate) fn fpr_save_index(&self, fpr: FPReg) -> Option<usize> {
        (fpr.0 < PHYS_FPR_POOL).then(|| self.using_fpr[..fpr.0].count_ones())
    }

    ///
    /// The word the walk stores into the callee frame's cont-frame pad slot
    /// for the shared continuation stub: high 32 bits = `conv(dst)` (`0` =
    /// no destination slot; `conv` is never 0 since `conv(SlotId(0)) ==
    /// LFP_SELF`), low 32 bits = the byte advance from the call-site pc to
    /// the next instruction (16 or 32 — `BytecodePtr::next` is
    /// per-opcode-size aware, which is what makes one shared stub correct
    /// for both 2-unit sends and 1-unit operator sites).
    ///
    pub(crate) fn cont_data(&self) -> u64 {
        let dst = match self.dst {
            Some(dst) => conv(dst) as u64,
            None => 0,
        };
        let advance = (self.pc.next().as_ptr() as u64) - (self.pc.as_ptr() as u64);
        (dst << 32) | advance
    }
}

#[allow(dead_code)]
#[derive(Debug)]
pub(super) struct SpecializedCodeInfo {
    iseq_id: ISeqId,
    self_class: Option<ClassId>,
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
        self_class: Option<ClassId>,
        position: Option<BytecodePtr>,
        entry_label: DestLabel,
        jit_class_version: u32,
        const_version: u64,
    ) -> JitResult<(
        Vec<InlineCacheEntry>,
        Vec<ClassId>,
        SpecializedCodeInfo,
        DestLabel,
        Vec<(ClassId, IdentId)>,
        ConstSalvageMap,
    )> {
        let jit_type = if let Some(pos) = position {
            JitType::Loop(pos)
        } else {
            JitType::Entry
        };
        let frame = JitStackFrame::new(store, jit_type, 0, iseq_id, None, self_class, None);
        // The set the body resolves under is a compile-time constant of
        // the iseq (`doc/refinements.md` §6.1); `using` moves the class
        // version, which is what re-checks it.
        let refinements = store.iseq_refinements(iseq_id);
        // Two version words, two jobs. The context validates the VM's
        // inline caches, which the VM stamps with *its* word, so it gets
        // that one; the unit's own snapshot and guards (below) take the
        // JIT's word, `jit_class_version`, which is what they compare.
        let vm_class_version = self.class_version();
        let mut ctx = JitContext::new(
            store,
            true,
            vm_class_version,
            const_version,
            refinements,
            vec![],
        );
        let mut frame = ctx.traceir_to_asmir(frame, None)?;
        let specialized_info = SpecializedCodeInfo::from(&frame);

        // Now that every frame's `stack_offset` has been finalised
        // and recorded in `JitContext::specialized_frame_sizes`,
        // rewrite every `DynVarOffset::Hint(...)` in the AsmIr tree
        // into a concrete byte offset before code generation runs.
        ctx.resolve_dyn_var_offsets(&mut frame.asm_info);

        let inline_cache = std::mem::take(&mut ctx.inline_method_cache);
        let singleton_deps = std::mem::take(&mut ctx.singleton_deps);
        let const_folds = std::mem::take(&mut ctx.const_fold_cache);
        // The basic-op invariants this body inlined without a runtime guard.
        // Handed back so the iseq can remember what a later redefinition
        // would actually invalidate (see `JitContext::assume_basic_op`).
        let bop_deps = std::mem::take(&mut ctx.bop_deps);

        // Front-end (TraceIR→AsmIR) is arch-neutral and has run by here. The
        // shared `gen_machine_code` driver then drives the per-arch `gen_asm`
        // lowering, which lowers every `AsmInst` on both arches — so machine-code
        // generation never bails (the front-end's `?` above is the only way to
        // fall back to the interpreter).
        self.jit.finalize();
        let class_version_label = self.jit.const_i32(jit_class_version as _);
        // The unit's single const-version snapshot word: only materialized
        // when the body folded a constant (otherwise no const guard exists
        // and there is nothing to patch). All const guards in the unit —
        // children included — read this one word.
        let const_version_label = (!const_folds.is_empty())
            .then(|| self.jit.const_i64(const_version as _));
        self.unit_const_version = const_version_label.clone();
        // Every `**kwrest` call site's table, for the same reason: laid down
        // now so the emission site can name it by address.
        self.resolve_rest_kw_tables(&mut frame.asm_info);
        // Bind the fresh snapshot words now: the aarch64 lowering bakes their
        // *addresses* into the guard sequences as immediates, so the labels
        // must be resolved before `gen_machine_code` runs (x86 reads them
        // rip-relative and doesn't care).
        self.jit.finalize();
        // aarch64 branch relaxation (see `Codegen::far_branch_mode`): decided
        // once for the whole unit, root and inlined callees together, from the
        // unit's total AsmIr instruction count. 64 bytes per AsmInst is a
        // generous per-instruction bound, so 8192 instructions stays
        // comfortably inside the ±1 MiB Imm19/Adr reach even doubled by the
        // long forms; anything larger emits BB edges in long form and inlines
        // opt_case's jump tables. The reach that matters is not frame-local:
        // constants — opt_case's near-form jump tables among them — are
        // emitted at finalize behind the *entire* unit, so an `adr` in a
        // small inlined frame still spans everything emitted after it. A
        // per-frame decision let exactly that overflow (a generated sqlite
        // module placed a jump table 1.55 MiB past its `adr`).
        #[cfg(target_arch = "aarch64")]
        {
            self.far_branch_mode = unit_inst_len(&mut frame.asm_info) > 8192;
        }
        // x86: collect this unit's class-version imm32 patch sites (one per
        // emitted guard, root and inlined children alike — one compilation
        // is atomic at one version). Cleared here so an earlier aborted
        // compile can't leak stale labels into this unit's list.
        self.unit_version_patch_sites.clear();
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
            (iseq_id, self_class, position),
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
        // Stamp the unit's real class version over every guard's
        // emission-time `VERSION_IMM_SENTINEL` (resolvable only now,
        // after `finalize`; the code is not yet published, so nothing
        // can execute a sentinel compare), and register the sites under
        // the unit's snapshot-word address — `set_class_version` looks
        // them up by the same `DestLabel` the salvage records carry, so
        // the salvage plumbing stays word-keyed. Empty on aarch64 (its
        // guards read the word).
        {
            let sites = std::mem::take(&mut self.unit_version_patch_sites);
            if !sites.is_empty() {
                self.stamp_version_imm_sites(&sites, jit_class_version);
                self.jit.set_executable();
                let key = self.jit.get_label_address(&class_version_label).as_ptr() as u64;
                self.version_imm_sites.insert(key, sites);
            }
        }
        self.unit_const_version = None;
        // Snapshot the involved names' epochs *at compile time*; a later
        // guard failure compares against these to prove the folds unchanged.
        let mut name_epochs: Vec<(IdentId, u64)> = vec![];
        for site in &const_folds {
            for &name in &site.names {
                if !name_epochs.iter().any(|(n, _)| *n == name) {
                    name_epochs.push((name, crate::globals::const_epoch::name_epoch(name)));
                }
            }
        }
        let const_map = ConstSalvageMap {
            name_epochs,
            wildcard: crate::globals::const_epoch::wildcard(),
            sites: const_folds,
            version_word: const_version_label,
            stale_defers: 0,
        };
        Ok((
            inline_cache,
            singleton_deps,
            specialized_info,
            class_version_label,
            bop_deps,
            const_map,
        ))
    }

    /// Lay down every `RestKw`'s (name, slot-id) table in the constant area
    /// and record its label in the instruction, before any of the unit's
    /// code is emitted.
    ///
    /// The emission site needs the table's *address*, and nothing else. On
    /// aarch64 it cannot take that address PC-relatively: constants are
    /// emitted at `finalize`, behind the whole unit, and `adr` reaches
    /// ±1 MiB, so a call site early in a large unit could not name its own
    /// table. Resolving the label first lets the backend bake an absolute
    /// address instead, which has no range at all — what the unit's
    /// class-version word beside this already does. x86 reads the table
    /// rip-relative either way; building it here keeps one path.
    fn resolve_rest_kw_tables(&mut self, info: &mut AsmInfo) {
        for (_, ir) in info.iter_ir_mut() {
            self.build_rest_kw_table_in(ir);
        }
        for (ir, _, _) in info.iter_outline_bridges_mut() {
            self.build_rest_kw_table_in(ir);
        }
        for (ir, _) in info.iter_inline_bridges_mut() {
            self.build_rest_kw_table_in(ir);
        }
        for context::SpecializeInfo { info, .. } in info.iter_specialized_methods_mut() {
            self.resolve_rest_kw_tables(info);
        }
    }

    fn build_rest_kw_table_in(&mut self, ir: &mut AsmIr) {
        for inst in ir.inst_iter_mut() {
            if let AsmInst::RestKw { rest_kw, table } = inst {
                let data = self.jit.const_align8();
                for (slot, name) in rest_kw.iter() {
                    self.jit.const_i32(name.get() as i32);
                    self.jit.const_i32(slot.0 as i32);
                }
                // Terminator: `correct_rest_kw` reads until a zero name.
                self.jit.const_i32(0);
                self.jit.const_i32(0);
                *table = Some(data);
            }
        }
    }
}

/// Total AsmIr instruction count of a whole compilation unit: the frame's
/// blocks and bridges plus, recursively, every inlined specialized callee's.
#[cfg(target_arch = "aarch64")]
fn unit_inst_len(info: &mut AsmInfo) -> usize {
    let mut total: usize = info
        .iter_ir_mut()
        .map(|(_, ir)| ir.inst_len())
        .sum::<usize>()
        + info
            .iter_outline_bridges_mut()
            .map(|(ir, _, _)| ir.inst_len())
            .sum::<usize>()
        + info
            .iter_inline_bridges_mut()
            .map(|(ir, _)| ir.inst_len())
            .sum::<usize>();
    for specialized in info.specialized_methods.iter_mut() {
        total += unit_inst_len(&mut specialized.info);
    }
    total
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
        root: (ISeqId, Option<ClassId>, Option<BytecodePtr>),
    ) {
        for context::SpecializeInfo {
            entry: specialized_entry,
            info: specialized_info,
        } in std::mem::take(&mut frame.specialized_methods)
        {
            // Only a top-level specialized callee gets an entry: it names
            // the unit a recompile request rebuilds, and a body nested
            // inside another specialized body is rebuilt with it.
            if !frame.is_specialized() {
                self.specialized_info.push(SpecializedPatchEntry {
                    iseq_id: specialized_info.iseq_id,
                    owner: Some(root),
                    class_version_label: class_version.clone(),
                });
            }
            // A frameless callee is not a function of its own: its body is
            // emitted inside this frame's, where the `InlineCall` that runs
            // it is lowered (`gen_inline_call`). It still took its patch
            // entry above, so the indices the frame's exits were compiled
            // with (`RecompileTarget::Specialized`) stay in step.
            if specialized_info.frameless {
                let id = specialized_info.specialized_id;
                let prev = self.inline_bodies.insert(id, (specialized_info, root));
                assert!(prev.is_none(), "inline body {id:?} parked twice");
                continue;
            }
            let entry = frame.resolve_label(&mut self.jit, specialized_entry);
            self.gen_machine_code(
                specialized_info,
                store,
                entry,
                level + 1,
                class_version.clone(),
                root,
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
        frame.side_exit_watermark = self.jit.get_current();

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

        // The first outlined bridge is laid down right after the last main
        // block. Where that block ends with a branch to the bridge's entry
        // (an inline callee's single `ret` edge: `Br(seg)` then the segment
        // at `seg`), the branch is to the next instruction and goes; the
        // bridge is then fall-through reachable. Only where the bridges
        // are hot code: x86 outlines them to the cold page except inside
        // an inline callee, aarch64 has no cold page.
        let mut first_bridge_fallthrough = false;
        // The same for a body with no bridge at all whose last main block
        // ends in the `Ret` (see `elide_last_ret` below).
        if cfg!(target_arch = "x86_64")
            && frame.frameless
            && outline_bridges.is_empty()
            && let Some((bbid, ir)) = ir_vec.last_mut()
            && !frame.inline_bridge_exists_for(*bbid)
            && ir.ends_with_ret()
        {
            ir.pop_trailing_ret();
        }
        if (cfg!(target_arch = "aarch64") || frame.frameless)
            && let Some((_, entry, _)) = outline_bridges.first()
            && let Some((bbid, ir)) = ir_vec.last_mut()
            && !frame.inline_bridge_exists_for(*bbid)
            && ir
                .trailing_br()
                .is_some_and(|l| frame.canonical_label(l) == frame.canonical_label(*entry))
        {
            ir.pop_trailing_br();
            first_bridge_fallthrough = true;
        }

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
        let had_outline_bridges = !outline_bridges.is_empty();
        // x86: an inline callee's `Ret` is a jump to the `InlineCall`'s
        // continuation (`emit_inline_ret`), which `gen_inline_call` binds
        // right after this body; nothing else of the body goes on the hot
        // page after the last bridge, so a `Ret` ending the last bridge
        // (or, with no bridge, the last main block) is a jump to the next
        // instruction and goes. aarch64 lays its side-exit island after the
        // bridges, so the jump stays there.
        let elide_last_ret = cfg!(target_arch = "x86_64") && frame.frameless;
        let n = outline_bridges.len();
        for (i, (mut ir, entry, exit)) in outline_bridges.into_iter().enumerate() {
            let entry = frame.resolve_label(&mut self.jit, entry);
            let mut exit = Some(exit);
            if elide_last_ret && i + 1 == n && ir.ends_with_ret() {
                ir.pop_trailing_ret();
                exit = None;
            }
            self.gen_asm(
                ir,
                store,
                &mut frame,
                Some(entry),
                exit,
                class_version.clone(),
                i == 0 && first_bridge_fallthrough,
            );
        }

        // aarch64: emit the frame's remaining side-exit handlers as the
        // final outlined island (see `a64_drain_side_exits`; x86 outlines
        // its handlers to the cold page inside its own `gen_asm`). An
        // outline bridge always ends in `b exit`, so after any bridge the
        // island cannot be fallen into; otherwise the last main block
        // decides.
        #[cfg(target_arch = "aarch64")]
        self.a64_drain_side_exits(&mut frame, !had_outline_bridges && fallthrough_in);
        // The pending jump tables are the whole unit's, and an inline body
        // ends with its caller's still referencing blocks not yet emitted;
        // the outermost frame patches them all.
        #[cfg(target_arch = "aarch64")]
        if self.inline_ctx.is_empty() {
            self.a64_patch_jump_tables();
        }
        #[cfg(not(target_arch = "aarch64"))]
        let _ = had_outline_bridges;

        if !frame.is_specialized() {
            self.specialized_base = self.specialized_info.len();
        }

        // An inline body is listed as part of its caller's code (and a
        // `finalize` here would meet the caller's unresolved forward
        // branches).
        #[cfg(feature = "emit-asm")]
        if self.startup_flag && self.inline_ctx.is_empty() {
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

impl Codegen {
    ///
    /// Emit the body of the frameless callee *spec_id* in place
    /// (`AsmInst::InlineCall`). The caller has filled the callee's window
    /// (`AbstractState::fill_window`) and saved its live fprs
    /// (`using_fpr`); from here:
    ///
    /// ```text
    ///     <frame pointer, LFP -= delta>
    ///     <callee body>            Ret -> done, every side exit -> redo
    /// done:
    ///     <frame pointer, LFP += delta>; <restore fprs>
    /// redo:  (cold: the x86 cold page, or behind a `b cont` on aarch64)
    ///     <frame pointer, LFP += delta>; <restore fprs>; jmp deopt
    /// cont:
    /// ```
    ///
    /// With the frame pointer moved, every frame-pointer-relative access the
    /// body makes — its slots on x86, its spill slots on both arches — and
    /// every LFP-relative one lands in the window, so the body is emitted
    /// exactly as it would be for a frame of its own, prologue and `ret`
    /// aside (`LInst::InlineRet`, the `Init` lowering). Nothing in the
    /// body reads the native frame link above the frame pointer: that is
    /// what `frameless_call::eligible` and the frameless violations keep
    /// out of it. A side exit after a side effect does not come back this
    /// way: it makes the window a frame and leaves for the interpreter
    /// (`Self::gen_frameless_materialize`), with *replay* — this frame's
    /// state at the call — to convert this frame by.
    ///
    fn gen_inline_call(
        &mut self,
        store: &Store,
        spec_id: context::SpecializedId,
        delta: i32,
        deopt: DestLabel,
        using_fpr: UsingFpr,
        replay: ChainReplay,
        class_version: DestLabel,
    ) {
        let (info, root) = self
            .inline_bodies
            .remove(&spec_id)
            .unwrap_or_else(|| panic!("inline body {spec_id:?} was not parked"));
        debug_assert!(info.frameless);
        let meta = store[store[info.iseq_id].func_id()].meta().get();
        let done = self.jit.label();
        let redo = self.jit.label();
        let cont = self.jit.label();
        let level = self.inline_ctx.len() + 1;
        self.inline_frame_shift(delta);
        self.inline_ctx.push(InlineCtx {
            done: done.clone(),
            redo: redo.clone(),
            delta,
            using_fpr,
            replay,
            meta,
        });
        let entry = self.jit.label();
        self.gen_machine_code(info, store, entry, level, class_version, root);
        self.inline_ctx.pop();
        self.jit.bind_label(done);
        self.inline_frame_unshift(delta);
        self.inline_fpr_restore(using_fpr);
        self.inline_redo_trampoline(redo, delta, using_fpr, &deopt, &cont);
        self.jit.bind_label(cont);
    }
}

#[cfg(target_arch = "x86_64")]
impl Codegen {
    /// Move the native frame pointer and the LFP down into an inline
    /// callee's window.
    fn inline_frame_shift(&mut self, delta: i32) {
        monoasm! { &mut self.jit,
            subq rbp, (delta);
            subq r14, (delta);
        }
    }

    fn inline_frame_unshift(&mut self, delta: i32) {
        monoasm! { &mut self.jit,
            addq rbp, (delta);
            addq r14, (delta);
        }
    }

    fn inline_fpr_restore(&mut self, using_fpr: UsingFpr) {
        self.fpr_restore(using_fpr);
    }

    /// The cold way out of an inline body: undo the frame shift, bring the
    /// caller's fprs back and take the caller's deopt. On the cold page, so
    /// the hot path falls straight through to *cont*.
    fn inline_redo_trampoline(
        &mut self,
        redo: DestLabel,
        delta: i32,
        using_fpr: UsingFpr,
        deopt: &DestLabel,
        _cont: &DestLabel,
    ) {
        assert_eq!(0, self.jit.get_page());
        self.jit.select_page(1);
        self.jit.bind_label(redo);
        self.inline_frame_unshift(delta);
        self.inline_fpr_restore(using_fpr);
        monoasm! { &mut self.jit,
            jmp deopt;
        }
        self.jit.select_page(0);
    }

    /// `LInst::InlineRet`: leave the inline callee's body for the
    /// continuation of the `InlineCall` running it.
    pub(in crate::codegen) fn emit_inline_ret(&mut self) {
        let done = self
            .inline_ctx
            .last()
            .expect("InlineRet outside an inline body")
            .done
            .clone();
        monoasm! { &mut self.jit,
            jmp done;
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
    /// With `cont`, additionally reserve the 16-byte continuation
    /// frame at the bottom (`[rsp, rsp+16)` = the callee frame's
    /// CFP+16..+32 region) and place the xmm saves *above* it, so the
    /// call-site pc store (`ContFramePc`, `[rsp]`) and the callee
    /// never touch a live saved float.
    pub(crate) fn fpr_save_with_cont(&mut self, using_fpr: UsingFpr, cont: bool) {
        if using_fpr.not_any() && !cont {
            return;
        }
        let pad = if cont { CONTINUATION_FRAME_SIZE as i32 } else { 0 };
        let sp_offset = using_fpr.offset() + pad as usize;
        monoasm!( &mut self.jit,
            subq rsp, (sp_offset);
        );
        let mut i = 0;
        for (x, b) in using_fpr.iter().enumerate() {
            if *b {
                monoasm!( &mut self.jit,
                    movq [rsp + (pad + 8 * i)], xmm(x as u64 + 2);
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
    pub(crate) fn fpr_restore_with_cont(&mut self, using_fpr: UsingFpr, cont: bool) {
        if using_fpr.not_any() && !cont {
            return;
        }
        let pad = if cont { CONTINUATION_FRAME_SIZE as i32 } else { 0 };
        let sp_offset = using_fpr.offset() + pad as usize;
        let mut i = 0;
        for (x, b) in using_fpr.iter().enumerate() {
            if *b {
                monoasm!( &mut self.jit,
                    movq xmm(x as u64 + 2), [rsp + (pad + 8 * i)];
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
            monoasm! { self,
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
        // GP residents first: the fpr and literal stores below use rax as
        // scratch, and rax may itself be a resident (a call result left in
        // place, `def_rax2gp`).
        for (reg, slot) in &wb.gp {
            monoasm! { &mut *self,
                movq [rbp - (rbp_local(*slot))], R(*reg as _);
            }
        }
        for (fpr, v) in &wb.fpr {
            self.fpr_to_stack(*fpr, v, base);
        }
        for (v, slot) in &wb.literal {
            self.literal_to_stack(*slot, *v);
        }
        for slot in &wb.void {
            self.literal_to_stack(*slot, Value::nil());
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
        // GP residents first: the fpr and literal stores below use rax as
        // scratch, and rax may itself be a resident (a call result left in
        // place, `def_rax2gp`).
        for (reg, slot) in &wb.gp {
            monoasm! { &mut *self,
                movq [r14 - (conv(*slot))], R(*reg as _);
            }
        }
        for (fpr, v) in &wb.fpr {
            self.fpr_to_stack2(*fpr, v, base);
        }
        for (v, slot) in &wb.literal {
            self.literal_to_stack2(*slot, *v);
        }
        for slot in &wb.void {
            self.literal_to_stack2(*slot, Value::nil());
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
        // K1: materialize deferred `**kwrest` Hashes after the rest
        // arrays (each helper call may allocate; every not-yet-written
        // deferred slot still physically holds the `nil` the caller
        // stored, so the frame stays GC-consistent throughout).
        for (dst, table) in wb.forward_kwrest.clone() {
            self.gen_forward_kwrest_materialize(dst, &table);
        }
    }

    /// K1: rebuild a deferred `**kwrest` Hash from the caller's kw
    /// slots and store it into `dst` (the trampoline's kwrest local).
    /// Same caller addressing as `gen_forward_rest_materialize`; the
    /// caller `Lfp` handed to `correct_rest_kw` is derived from the
    /// saved caller rbp (`lfp == rbp - RBP_LOCAL_FRAME` in every JIT
    /// frame).
    fn gen_forward_kwrest_materialize(&mut self, dst: SlotId, table: &[(IdentId, SlotId)]) {
        let data = self.const_align8();
        for (name, slot) in table {
            self.const_i32(name.get() as i32);
            self.const_i32(slot.0 as i32);
        }
        self.const_i32(0);
        self.const_i32(0);
        monoasm! { self,
            movq rsi, [rbp];
            subq rsi, (RBP_LOCAL_FRAME);
            lea  rdi, [rip + data];
            movq rax, (runtime::correct_rest_kw);
            call rax;
            movq [r14 - (conv(dst))], rax;
        }
    }

    ///
    /// Emit this call site's chain-deopt conversion as machine code, once,
    /// at compile time, and return its entry.
    ///
    /// This is the compiled form of the former Rust-side replay + the two
    /// frame stores that followed it: everything the conversion does is fixed when
    /// the site is compiled (which slots, which spill offsets, which
    /// literals, the continuation word), so a walk that reaches this frame
    /// has nothing left to decide — it calls here.
    ///
    /// ### calling convention
    /// - `rdi` — the *callee* frame's bp (the suspended call's own frame)
    /// - `rsi` — the *caller* frame's bp (the frame being converted)
    /// - `rdx` — the caller frame's `Lfp` (read from its CFP, so a frame
    ///   promoted to the heap is followed correctly)
    ///
    /// Clobbers the C caller-saved set; the walk passes nothing else live.
    ///
    #[cfg(target_arch = "x86_64")]
    pub(in crate::codegen) fn gen_chain_replay_stub(
        &mut self,
        replay: &ChainReplay,
        cont_stub: CodePtr,
    ) -> CodePtr {
        let entry = self.jit.get_current_address();
        let wb = replay.write_back_all();
        let base = replay.base();
        // Floats first: each is loaded from wherever the call left it and
        // boxed, exactly as the Rust-side replay did. A pool-resident
        // `FPReg` was spilled by the call's `FprSave` into the callee's save
        // area; the rest sit in the caller's own spill slots.
        for (fpr, slots) in wb.fpr_entries() {
            if slots.is_empty() {
                continue;
            }
            let i = replay.fpr_save_index(*fpr);
            match i {
                Some(i) => {
                    let off = 32 + 8 * i as i32;
                    monoasm! { &mut *self, movq xmm0, [rdi + (off)]; }
                }
                None => {
                    let off = (base as i32) - 24 + 8 * ((fpr.0 - PHYS_FPR_POOL) as i32);
                    monoasm! { &mut *self, movq xmm0, [rsi - (off)]; }
                }
            }
            // `f64_to_val` can allocate a heap Float, so keep the frame
            // pointers across it. Three pushes leave rsp 16-aligned for the
            // call: the stub was entered with rsp ≡ 8 (mod 16) and 24 bytes
            // of pushes bring it back to ≡ 0.
            let f64_to_val = self.f64_to_val.clone();
            monoasm! { &mut *self,
                pushq rdi;
                pushq rsi;
                pushq rdx;
                call  f64_to_val;
                popq  rdx;
                popq  rsi;
                popq  rdi;
            }
            for slot in slots {
                monoasm! { &mut *self, movq [rdx - (conv(*slot))], rax; }
            }
        }
        for (v, slot) in wb.literal_entries() {
            monoasm! { &mut self.jit,
                movq rax, (v.id());
                movq [rdx - (conv(*slot))], rax;
            }
        }
        for slot in wb.void_entries() {
            monoasm! { &mut *self,
                movq rax, (Value::nil().id());
                movq [rdx - (conv(*slot))], rax;
            }
        }
        debug_assert!(wb.gp_is_empty());
        // Deferred rest / kwrest last, so the literal stores above have
        // already homed each deferred `dst` and the frame stays
        // GC-consistent across the allocating helper calls. The source slots
        // live in the caller's *own* caller, reached through the bp it
        // saved — `[rsi]`, matching `gen_forward_rest_materialize`'s `[rbp]`.
        for (dst, src, len) in wb.forward_rest_entries() {
            monoasm! { &mut *self,
                pushq rdi;
                pushq rsi;
                pushq rdx;
                movq  rcx, [rsi];
                lea   rdi, [rcx - (rbp_local(*src))];
                movq  rsi, (*len as usize);
                movq  rax, (runtime::create_array);
                call  rax;
                popq  rdx;
                popq  rsi;
                popq  rdi;
                movq  [rdx - (conv(*dst))], rax;
            }
        }
        for (dst, table) in wb.forward_kwrest_entries() {
            // Same table-in-JIT-memory shape as
            // `gen_forward_kwrest_materialize`, terminated by a zero pair.
            let data = self.const_align8();
            for (name, slot) in table.iter() {
                self.const_i32(name.get() as i32);
                self.const_i32(slot.0 as i32);
            }
            self.const_i32(0);
            self.const_i32(0);
            monoasm! { &mut *self,
                pushq rdi;
                pushq rsi;
                pushq rdx;
                movq  rsi, [rsi];
                subq  rsi, (RBP_LOCAL_FRAME);
                lea   rdi, [rip + data];
                movq  rax, (runtime::correct_rest_kw);
                call  rax;
                popq  rdx;
                popq  rsi;
                popq  rdi;
                movq  [rdx - (conv(*dst))], rax;
            }
        }
        // The two frame stores the walk used to make itself: the per-site
        // continuation word into the callee's cont-frame pad (CFP+32 ==
        // callee_bp - BP_CFP + 32), and the callee's return address (the
        // slot above its saved bp) pointed at the shared VM stub.
        let cont = replay.cont_data();
        let pad_off = 32 - BP_CFP;
        monoasm! { &mut *self,
            movq rax, (cont);
            movq [rdi + (pad_off)], rax;
            movq rax, (cont_stub.as_ptr() as u64);
            movq [rdi + 8], rax;
            ret;
        }
        entry
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
        chain: u32,
    ) {
        let raise = self.entry_raise();
        assert_eq!(0, self.jit.get_page());
        self.jit.select_page(1);
        monoasm!( &mut self.jit,
        entry:
        );
        self.gen_write_back_for_deopt(&wb, base);
        // Chain escalation (`doc/chain_deopt.md` §5 step 4): the raise may be
        // rescued *inside* this frame (resuming it in the interpreter) or
        // unwind through the suspended callers — either way this unit's own
        // suspended frames must be converted first. `chain` is how many
        // (this frame's depth in the compilation); frames below the unit's
        // root keep their compiled error handling, which is what a
        // root-frame raise has always done. The write-back has run, so the
        // frame is fully homed for the walk.
        if chain != 0 {
            monoasm!( &mut self.jit,
                movq rdi, rbx;
                movl rsi, (chain);
                movq rax, (runtime::chain_deopt);
                call rax;
            );
        }
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
        chain: u32,
        #[cfg(feature = "deopt")] exit_id: u32,
    ) {
        self.side_exit_with_label(
            pc,
            wb,
            entry,
            false,
            None,
            loop_jit_spill_bytes,
            base,
            chain,
            #[cfg(feature = "deopt")]
            exit_id,
        )
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
        target: RecompileTarget,
        entry: DestLabel,
        loop_jit_spill_bytes: usize,
        base: usize,
        chain: u32,
        #[cfg(feature = "deopt")] exit_id: u32,
    ) {
        self.side_exit_with_label(
            pc,
            wb,
            entry,
            false,
            Some((reason, target)),
            loop_jit_spill_bytes,
            base,
            chain,
            #[cfg(feature = "deopt")]
            exit_id,
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
        #[cfg(feature = "deopt")] exit_id: u32,
    ) {
        // Evict handlers are only entered through a chain-wide eviction walk,
        // which converts (or patches) every suspended frame in one pass — no
        // per-handler escalation needed.
        self.side_exit_with_label(
            pc,
            wb,
            entry,
            true,
            None,
            loop_jit_spill_bytes,
            base,
            0,
            #[cfg(feature = "deopt")]
            exit_id,
        )
    }

    ///
    /// The counter-gated, one-shot recompile a `RecompileDeoptimize` exit
    /// runs before it leaves the compiled code (see `side_exit_with_label`).
    /// Falls through when it does not recompile, and after it did.
    ///
    fn gen_recompile_hook(
        &mut self,
        pc: BytecodePtr,
        reason: RecompileReason,
        target: RecompileTarget,
    ) {
        let recompile_lbl = self.jit.label();
        let skip = self.jit.label();
        let counter = self.jit.data_i32(match target {
            RecompileTarget::Whole(_) => COUNT_DEOPT_RECOMPILE,
            RecompileTarget::Specialized(_) => COUNT_DEOPT_RECOMPILE_SPECIALIZED,
        });
        // `BecamePolymorphic` is checked, not assumed: recompile only
        // once the VM has actually stamped the site's POLY byte
        // (`opcode_sub`, op1 bits 63:56 — the interpreter sets it on an
        // operand/receiver *class* change). A miss the profile cannot
        // describe as a class change re-executes in the VM without
        // moving the byte, and a recompile against an unchanged profile
        // would reproduce the same guard: the activerecord
        // `out_of_range?` shape recompiled 8,756 times that way. With
        // the gate such a site just deopts plainly, byte-for-byte the
        // pre-heal behavior. (Binop/cmp ICs record a heap Integer under
        // the `BIGNUM_CLASS` tag, so a Bignum miss *is* a class change
        // there and heals into the dispatch; the gate still protects
        // the send-side exits, whose ICs class every Integer alike.)
        if reason == RecompileReason::BecamePolymorphic {
            let poly_byte = pc.as_ptr() as usize + 7;
            monoasm!( &mut self.jit,
                movq rax, (poly_byte);
                cmpb [rax], 0;
                jeq  skip;
            );
        }
        monoasm!( &mut self.jit,
            cmpl [rip + counter], 0;
            jle  skip;
            subl [rip + counter], 1;
            jne  skip;
        );
        match target {
            RecompileTarget::Whole(position) => {
                self.gen_recompile(position, recompile_lbl, reason, None)
            }
            RecompileTarget::Specialized(idx) => {
                self.gen_recompile_specialized(self.specialized_base + idx, recompile_lbl, reason)
            }
        }
        monoasm!( &mut self.jit,
        skip:
        );
    }

    ///
    /// The side exit of a frameless specialized callee (`AsmInfo::frameless`).
    ///
    /// The callee has no frame to write back into and nothing to resume:
    /// it leaves for the redo trampoline of the `InlineCall` running it
    /// (`Codegen::gen_inline_call`), which re-executes the whole call in
    /// the interpreter from the caller's frame. Sound because every such
    /// exit precedes the callee's first side effect. A recompile exit still
    /// runs its counter first, so a guard that keeps missing heals the unit
    /// as it would anywhere else.
    ///
    pub(in crate::codegen) fn gen_frameless_redo(
        &mut self,
        pc: BytecodePtr,
        entry: DestLabel,
        recompile: Option<(RecompileReason, RecompileTarget)>,
    ) {
        assert_eq!(0, self.jit.get_page());
        self.jit.select_page(1);
        self.jit.bind_label(entry);
        if let Some((reason, target)) = recompile {
            self.gen_recompile_hook(pc, reason, target);
        }
        let redo = self
            .inline_ctx
            .last()
            .expect("frameless redo outside an inline body")
            .redo
            .clone();
        monoasm!( &mut self.jit,
            jmp redo;
        );
        self.jit.select_page(0);
    }

    ///
    /// The side exit of a frameless specialized callee that cannot redo the
    /// call (`LSideExitKind::Materialize`): the callee has done something
    /// observable, so it is resumed in the interpreter where it is, in a
    /// frame of its own, and its callers are converted as a deopt converts
    /// a suspended chain (`doc/chain_deopt.md`).
    ///
    /// The window already has the layout of a frame
    /// (`JitContext::inline_window_delta`); what it lacks is written here:
    ///
    /// 1. the callee's write-back, like any deopt's (the frame pointer and
    ///    the LFP are still the callee's);
    /// 2. for every inline level, innermost first: the LFP header (outer
    ///    `0`, the callee's meta, svar `0`, block `0`), the control words
    ///    (prev cfp = the enclosing frame's, the lfp slot, the saved frame
    ///    pointer, the return address = the VM continuation stub, the
    ///    call-site pc, the continuation word with the result slot and the
    ///    resume advance — what a chain conversion stores for a suspended
    ///    call site), then the enclosing frame's registers (undo the
    ///    shift, bring its fprs back) and its write-back at the call
    ///    (`InlineCtx::replay`). The innermost frame becomes `Executor::cfp`;
    /// 3. with the framed caller's registers restored: the deopt log, the
    ///    chain walk for *its* callers (`chain` counts the materialized
    ///    frames too — the walk skips them by their VM return address —
    ///    and then the framed caller's depth in the unit), the recompile
    ///    hook;
    /// 4. the frame pointer and the LFP back to the callee's, and the VM
    ///    fetch at `pc` — or `entry_raise`, for an error exit.
    ///
    /// When the callee `ret`s in the interpreter it `leave`s its window and
    /// lands in the continuation stub, which stores its result into the
    /// enclosing frame's result slot and resumes that frame, interpreted,
    /// after the call: the stack pointer it leaves at is the enclosing
    /// frame's own VM depth, since the window sits exactly where the
    /// interpreter would have built the frame.
    ///
    pub(in crate::codegen) fn gen_frameless_materialize(
        &mut self,
        pc: BytecodePtr,
        wb: &WriteBack,
        entry: DestLabel,
        base: usize,
        chain: u32,
        recompile: Option<(RecompileReason, RecompileTarget)>,
        error: bool,
        #[cfg(feature = "deopt")] exit_id: u32,
    ) {
        assert_eq!(0, self.jit.get_page());
        self.jit.select_page(1);
        self.jit.bind_label(entry);
        self.gen_write_back_for_deopt(wb, base);
        let cont_stub = self.jit.get_label_address(&self.chain_cont_stub).as_ptr() as u64;
        let levels: Vec<(i32, UsingFpr, ChainReplay, u64)> = self
            .inline_ctx
            .iter()
            .rev()
            .map(|ctx| (ctx.delta, ctx.using_fpr, ctx.replay.clone(), ctx.meta))
            .collect();
        assert!(!levels.is_empty(), "frameless materialize outside an inline body");
        let mut total_delta = 0;
        for (i, (delta, using_fpr, replay, meta)) in levels.into_iter().enumerate() {
            // The caller's residents were flushed before the window was
            // filled; the body has clobbered every register since.
            debug_assert!(replay.write_back_all().gp_is_empty());
            let site_pc = replay.pc().as_ptr() as u64;
            let cont_data = replay.cont_data();
            monoasm! { &mut self.jit,
                // LFP header.
                movq [r14 - (LFP_OUTER)], 0;
                movq rax, (meta);
                movq [r14 - (LFP_META)], rax;
                movq [r14 - (LFP_SVAR)], 0;
                movq [r14 - (LFP_BLOCK)], 0;
                // Control frame: lfp, prev cfp.
                movq [rbp - (BP_CFP + CFP_LFP)], r14;
                lea  rax, [rbp + (delta - BP_CFP)];
                movq [rbp - (BP_CFP)], rax;
                // Continuation frame: saved frame pointer, return address,
                // call-site pc, continuation word.
                lea  rax, [rbp + (delta)];
                movq [rbp], rax;
                movq rax, (cont_stub);
                movq [rbp + 8], rax;
                movq rax, (site_pc);
                movq [rbp + 16], rax;
                movq rax, (cont_data);
                movq [rbp + 24], rax;
            }
            if i == 0 {
                monoasm! { &mut self.jit,
                    lea  rax, [rbp - (BP_CFP)];
                    movq [rbx + (EXECUTOR_CFP)], rax;
                }
            }
            self.inline_frame_unshift(delta);
            self.inline_fpr_restore(using_fpr);
            self.gen_write_back_for_deopt(replay.write_back_all(), replay.base());
            total_delta += delta;
        }
        monoasm!( &mut self.jit,
            movq r13, (pc.as_ptr());
        );
        #[cfg(any(feature = "deopt", feature = "profile"))]
        {
            monoasm!( &mut self.jit,
                movq rdi, rbx;
                movq rsi, r12;
                movq rdx, r13;
            );
            #[cfg(feature = "deopt")]
            monoasm!( &mut self.jit,
                movl rcx, (exit_id);
            );
            monoasm!( &mut self.jit,
                movq rax, (crate::globals::log_deoptimize);
                call rax;
            );
        }
        if chain != 0 {
            monoasm!( &mut self.jit,
                movq rdi, rbx;
                movl rsi, (chain);
                movq rax, (runtime::chain_deopt);
                call rax;
            );
        }
        // After the walk and with every frame homed, as in
        // `side_exit_with_label`: the recompile can run a GC.
        if let Some((reason, target)) = recompile {
            self.gen_recompile_hook(pc, reason, target);
        }
        self.inline_frame_shift(total_delta);
        if error {
            let raise = self.entry_raise();
            monoasm!( &mut self.jit,
                movq r13, ((pc + 1).as_ptr());
                jmp  raise;
            );
        } else {
            let fetch = self.vm_fetch();
            monoasm!( &mut self.jit,
                jmp fetch;
            );
        }
        self.jit.select_page(0);
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
        recompile: Option<(RecompileReason, RecompileTarget)>,
        loop_jit_spill_bytes: usize,
        base: usize,
        chain: u32,
        #[cfg(feature = "deopt")] exit_id: u32,
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
        // Now release the spill region that Loop JIT entry pinned rsp
        // below (see `AsmInst::LoopJitRspBump`). Method / specialized
        // JITs restore rsp implicitly via their `leave; ret` epilogue, so
        // the adjustment is Loop-specific. `loop_jit_spill_bytes` is `0`
        // for non-Loop frames or Loop frames without spill.
        //
        // This is not the inverse of the entry, which pins an absolute
        // depth rather than subtracting: adding the spill region back to
        // `rbp - (total - PROLOGUE_OVERHEAD)` lands at
        // `rbp - (base - PROLOGUE_OVERHEAD)` — the local area the VM's
        // `init_method` reserves — whichever producer built the frame.
        // Deliberate: the VM resumes here, and that is its own depth.
        if loop_jit_spill_bytes > 0 {
            monoasm!( &mut self.jit,
                addq rsp, (loop_jit_spill_bytes as i32);
            );
        }
        monoasm!( &mut self.jit,
            movq r13, (pc.as_ptr());
        );

        // The call site keeps its original position — after the write-back,
        // with nothing live in registers and the VM re-entry next. What the
        // guard was looking at no longer has to survive in a register to be
        // reported: its trampoline copied it into the `Executor` *before*
        // the write-back ran (see `jitgen::deopt_log`), which is what makes
        // the reported cause trustworthy now. Under `profile` alone the call
        // is byte-for-byte what it always was.
        #[cfg(any(feature = "deopt", feature = "profile"))]
        {
            let _ = _is_evict;
            let _ = &recompile;
            monoasm!( &mut self.jit,
                movq rdi, rbx;
                movq rsi, r12;
                movq rdx, r13;
            );
            #[cfg(feature = "deopt")]
            monoasm!( &mut self.jit,
                movl rcx, (exit_id);
            );
            monoasm!( &mut self.jit,
                movq rax, (crate::globals::log_deoptimize);
                call rax;
            );
        }
        // Chain escalation (`doc/chain_deopt.md` §5 step 4): convert this
        // compilation unit's `chain` suspended frames before this frame
        // resumes in the interpreter. Runs after the write-back (the frame is
        // fully homed in the LFP) and before the recompile hook / fetch.
        if chain != 0 {
            monoasm!( &mut self.jit,
                movq rdi, rbx;
                movl rsi, (chain);
                movq rax, (runtime::chain_deopt);
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
        if let Some((reason, target)) = recompile {
            self.gen_recompile_hook(pc, reason, target);
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
