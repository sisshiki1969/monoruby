//! # Unified low-level IR (LIR) — arch-neutral machine-level instructions
//!
//! **Stage 1 of the Phase-1 "unified low-level IR" effort.** This module
//! defines the *data model only*; it is not yet wired into the compilation
//! pipeline (hence the module-level `dead_code` allow). Subsequent stages
//! migrate the per-arch `emit_*` primitives onto it one instruction family at a
//! time. See `doc/lir.md` for the full design and migration plan.
//!
//! ## Where it sits
//!
//! ```text
//! TraceIR → AsmIR (AsmInst, arch-neutral, register-allocated)
//!         → [compile_asmir dispatcher]
//!         → LIR (this module: arch-neutral machine ops + logical addressing)   ← NEW
//!         → [per-arch LirEncode + legalize]
//!         → monoasm! / monoasm_arm64!  → bytes
//! ```
//!
//! Today `emit_*` lowers each `AsmInst` *directly* to machine code, so each
//! backend re-implements register moves, frame addressing, immediate-range
//! handling, and FP-pool save/restore by hand (≈145 primitives on x86, ≈143 on
//! aarch64). The aarch64 side already factors the immediate-range handling into
//! helpers like `a64_frame_load` / `a64_field_load` / `a64_sp_sub`, each of
//! which materializes an over-large offset into a scratch register. LIR
//! promotes that pattern into an explicit contract:
//!
//! - **Operands are *logical*.** `LMem` carries an unbounded displacement; the
//!   per-arch encoder is responsible for **legalization** (folding a small
//!   displacement into the instruction's immediate field, or materializing a
//!   large one into a scratch register). A single legalizer replaces the ~two
//!   dozen ad-hoc range checks currently spread across the aarch64 backend.
//! - **Registers are already arch-neutral.** `GP` (general) and `FPReg`
//!   (virtual FP, phys-xmm-or-spill) are defined in `codegen.rs` and map to
//!   physical registers per `target_arch`, so LIR reuses them as-is.
//!
//! The payoff is no longer closing aarch64 *bails* (the full port `#704`
//! already removed those): it is collapsing the two parallel `emit_*` sets into
//! one description, which is also the concrete code-generation target the future
//! interpreter/JIT DSL (Phase-1 item ③) lowers to.

// Stage-1 scaffolding: the model exists but nothing consumes it yet.
#![allow(dead_code)]

use super::*;
use crate::ast::CmpKind;
use crate::bytecodegen::{BinOpK, UnOpK};

/// A general-purpose-register operand or an inline integer immediate.
///
/// The encoder folds an `Imm` into the instruction's immediate field when it
/// fits, and materializes it into a scratch register otherwise — the same
/// legalization story as `LMem` displacements.
#[derive(Debug, Clone, Copy, PartialEq)]
pub(in crate::codegen::jitgen) enum LOperand {
    Reg(GP),
    Imm(i64),
}

impl From<GP> for LOperand {
    fn from(r: GP) -> Self {
        LOperand::Reg(r)
    }
}

impl From<i64> for LOperand {
    fn from(i: i64) -> Self {
        LOperand::Imm(i)
    }
}

/// A *logical* memory location. Displacements are unbounded here; the per-arch
/// A register operand that is either an allocatable general-purpose register or
/// the per-arch reserved *scratch* pointer register. The scratch is for
/// intermediate pointers (e.g. dereferencing an object's heap var-table) that
/// must not clobber an allocated value; it maps to **rdx** on x86 and **x9** on
/// aarch64. There is deliberately no general-purpose name for these (aarch64's
/// x9 is outside the `GP` enum's allocatable mapping), so they need their own
/// operand kind.
#[derive(Debug, Clone, Copy, PartialEq)]
pub(in crate::codegen::jitgen) enum LReg {
    Gp(GP),
    Scratch,
}

impl From<GP> for LReg {
    fn from(r: GP) -> Self {
        LReg::Gp(r)
    }
}

/// `LirEncode` implementation legalizes them (immediate field vs.
/// scratch-register materialization) when lowering.
#[derive(Debug, Clone, Copy, PartialEq)]
pub(in crate::codegen::jitgen) enum LMem {
    /// An LFP-relative local-variable / temporary slot. Slots live at a
    /// *negative* byte displacement from the local frame pointer:
    /// `[rbp - rbp_local(slot)]` on x86, `[lfp - (slot*8 + LFP_SELF)]` on
    /// aarch64. The encoder owns that arch-specific displacement formula.
    Slot(SlotId),
    /// An object field at a *positive* byte displacement from a base register
    /// (which may be the scratch pointer). `disp` may exceed any
    /// single-instruction immediate range; the encoder materializes it into a
    /// scratch register when needed (cf. `a64_field_load`).
    Field { base: LReg, disp: i32 },
    /// A callee-frame argument slot at a *positive* byte displacement below the
    /// stack pointer, used while marshalling arguments before a call
    /// (cf. the aarch64 `a64_rsp_*` helpers).
    RspRel { disp: i32 },
}

/// Condition for a `CondBr` following a `Cmp`. Integer Ruby comparisons are
/// signed; float/NaN-aware comparisons are *not* modelled here — they get
/// dedicated `LInst` variants in a later stage because their condition-code
/// mapping differs per arch (x86 `ucomisd`+`setp`, aarch64 `fcmp`+MI/LS).
#[derive(Debug, Clone, Copy, PartialEq)]
pub(in crate::codegen::jitgen) enum LCond {
    Eq,
    Ne,
    /// signed `<`
    Lt,
    /// signed `<=`
    Le,
    /// signed `>`
    Gt,
    /// signed `>=`
    Ge,
}

impl LCond {
    /// Map an integer-comparison `CmpKind` to its signed LIR condition. Returns
    /// `None` for `TEq` (`===`), which is not a plain signed integer comparison
    /// and is lowered through another path.
    pub(in crate::codegen::jitgen) fn from_int_cmp(kind: CmpKind) -> Option<Self> {
        Some(match kind {
            CmpKind::Eq => LCond::Eq,
            CmpKind::Ne => LCond::Ne,
            CmpKind::Lt => LCond::Lt,
            CmpKind::Le => LCond::Le,
            CmpKind::Gt => LCond::Gt,
            CmpKind::Ge => LCond::Ge,
            CmpKind::TEq => return None,
        })
    }

    /// The condition that is true exactly when `self` is false.
    pub(in crate::codegen::jitgen) fn invert(self) -> Self {
        match self {
            LCond::Eq => LCond::Ne,
            LCond::Ne => LCond::Eq,
            LCond::Lt => LCond::Ge,
            LCond::Le => LCond::Gt,
            LCond::Gt => LCond::Le,
            LCond::Ge => LCond::Lt,
        }
    }
}

/// An integer ALU operation on general-purpose registers.
///
/// This is the *machine-level* op set (no overflow/deopt semantics — those are
/// expressed by the surrounding `LInst`s / guards). Ruby's `BinOpK` maps onto a
/// subset; bit-shift and division corner cases are handled by the higher tiers
/// before reaching LIR.
#[derive(Debug, Clone, Copy, PartialEq)]
pub(in crate::codegen::jitgen) enum LAluOp {
    Add,
    Sub,
    Mul,
    And,
    Or,
    Xor,
    /// logical shift left
    Shl,
    /// arithmetic shift right
    Sar,
}

impl LAluOp {
    /// Map the directly-lowerable subset of `BinOpK` to an `LAluOp`. `Div` /
    /// `Rem` (which need an idiv / call sequence) and any op routed through a
    /// method call upstream return `None`.
    pub(in crate::codegen::jitgen) fn from_binop(kind: BinOpK) -> Option<Self> {
        Some(match kind {
            BinOpK::Add => LAluOp::Add,
            BinOpK::Sub => LAluOp::Sub,
            BinOpK::Mul => LAluOp::Mul,
            BinOpK::BitAnd => LAluOp::And,
            BinOpK::BitOr => LAluOp::Or,
            BinOpK::BitXor => LAluOp::Xor,
            BinOpK::Shl => LAluOp::Shl,
            BinOpK::Shr => LAluOp::Sar,
            BinOpK::Div | BinOpK::Rem | BinOpK::Exp => return None,
        })
    }
}

/// One arch-neutral low-level instruction.
///
/// The set intentionally starts small — it covers the integer move / memory /
/// ALU / compare-branch core that Stage 2 migrates first (the families behind
/// `emit_reg_move`, `emit_reg_to_stack`, `emit_stack_to_reg`, `emit_lit_to_reg`,
/// `emit_lit_to_stack`, `emit_reg_add/sub`, `emit_integer_cmp_br`). FP
/// transfer/compare, runtime calls (with FP-pool save/restore), and the
/// patch/recompile machinery are added in later stages as their `AsmInst`
/// families are ported onto LIR.
///
/// Branch targets carry a *resolved* monoasm `DestLabel` (not the front-end
/// `JitLabel`), because the encoder runs after label resolution — the `emit_*`
/// primitives already receive `DestLabel`s. `DestLabel` is `Clone` but not
/// `Copy`, so `LInst` is `Clone`/`PartialEq` but not `Copy`.
#[derive(Debug, Clone, PartialEq)]
pub(in crate::codegen::jitgen) enum LInst {
    /// `dst <- src`. A no-op when `src == dst` (the encoder elides it).
    Mov { dst: GP, src: GP },
    /// `dst <- imm` — materialize a full 64-bit immediate (e.g. a tagged
    /// `Value`'s bit pattern) into a register.
    LoadImm { dst: GP, imm: u64 },
    /// `dst <- [mem]`. `dst` may be the scratch pointer (intermediate deref).
    Load { dst: LReg, mem: LMem },
    /// `[mem] <- src`.
    Store { src: GP, mem: LMem },
    /// `[mem] <- imm`. aarch64 has no store-immediate, so the encoder routes it
    /// through a scratch register without clobbering any allocated GP.
    StoreImm { imm: u64, mem: LMem },
    /// `dst <- lhs <op> rhs`.
    Alu {
        op: LAluOp,
        dst: GP,
        lhs: GP,
        rhs: LOperand,
    },
    /// Set condition flags from `lhs - rhs` (no result register written). The
    /// immediate of an `Imm` rhs is the operand's raw bit pattern (e.g. a tagged
    /// fixnum `Value`), compared against the raw register bits.
    Cmp { lhs: GP, rhs: LOperand },
    /// Bind `label` at the current code position.
    Label(DestLabel),
    /// Unconditional branch to `target`.
    Br(DestLabel),
    /// Branch to `target` when the preceding `Cmp` satisfied `cond`.
    CondBr { cond: LCond, target: DestLabel },
    /// Branch to `target` on the Ruby truthiness of the accumulator (everything
    /// except `nil`/`false` is truthy). `negate` branches on *falsy* instead.
    BranchTruthy { negate: bool, target: DestLabel },
    /// Branch to `target` if the accumulator is `nil`.
    BranchIfNil { target: DestLabel },
    /// Branch to `target` if the accumulator is non-zero (e.g. an already-set
    /// local slot).
    BranchIfNonzero { target: DestLabel },
    /// Generational-GC write barrier after storing `value` into a field of the
    /// heap object `parent`: if `parent` is an old object not yet remembered and
    /// `value` is a heap pointer, record `parent` in the remembered set. A
    /// macro-op — the encoder emits the arch's inline fast-path + slow-path call
    /// (x86 fixes `parent` in rdi).
    WriteBarrier { parent: GP, value: GP },
    /// `reg <- nil` if `reg == 0` (an unset inline-ivar slot reads as 0). The
    /// arches differ structurally — x86 branches over a `mov`, aarch64 uses a
    /// branchless `csel` — so this is its own op rather than a Load + branch.
    NilIfZero { reg: GP },
    /// Class guard: branch to the side-exit `deopt` unless `reg`'s runtime class
    /// is `class`. `deopt` is a resolved side-exit label (the deopt model:
    /// guards carry the side-exit they fall through to).
    GuardClass {
        reg: GP,
        class: ClassId,
        deopt: DestLabel,
    },
    /// Type guard: deopt unless `reg` holds an `Array`.
    GuardArrayTy { reg: GP, deopt: DestLabel },
    /// Deopt if the receiver (rdi) is frozen.
    GuardFrozen { deopt: DestLabel },
    /// Constant-load guard: deopt unless the cached base class (in the
    /// accumulator) equals `base_class`.
    GuardConstBaseClass {
        base_class: Value,
        deopt: DestLabel,
    },
    /// Constant-load guard: deopt if the global constant version moved away from
    /// `const_version` since compilation.
    GuardConstVersion {
        const_version: usize,
        deopt: DestLabel,
    },
    /// Block-passing side-effect guard: deopt if the current frame was captured
    /// or invalidated.
    GuardCapture { deopt: DestLabel },
    /// Basic-operator-redefinition guard: deopt if any BOP was redefined.
    CheckBOP { deopt: DestLabel },
    /// Fixnum fast-path arithmetic (`lhs <op> rhs`) on tagged integers, with an
    /// overflow side-exit to `deopt`. A macro-op: the encoder emits the arch's
    /// tagged-arith sequence and overflow handler (x86 outlines the handler to a
    /// cold page; aarch64 lays it inline), so the tag manipulation and the
    /// idiv/imul-vs-aarch64 lowering stay per-arch.
    IntegerBinOp {
        kind: BinOpK,
        mode: OpMode,
        lhs: GP,
        rhs: GP,
        deopt: DestLabel,
    },
    // ---- floating-point transfer / convert -----------------------------------
    // FP operands are virtual `FPReg`s (physical xmm/d-reg or a stack spill); the
    // encoders resolve the spill via `FPReg::loc(base)`, so these carry the
    // frame's `base` spill offset.
    /// `dst <- src` between FP registers (spill-aware; no-op when equal).
    FprMove {
        src: FPReg,
        dst: FPReg,
        base: usize,
    },
    /// `dst <- f` — materialize an f64 constant into an FP register.
    F64ToFpr { f: f64, dst: FPReg, base: usize },
    /// `dst <- (src as fixnum) as f64` — untag and convert.
    FixnumToFpr {
        src: GP,
        dst: FPReg,
        base: usize,
    },
    /// `[slot] <- Value::float(src)` — box the FP register into a `Value` and
    /// store it to a frame slot.
    FprToStack {
        src: FPReg,
        slot: SlotId,
        base: usize,
    },
    /// Swap two FP registers (spill-aware).
    FprSwap {
        lhs: FPReg,
        rhs: FPReg,
        base: usize,
    },
    /// `dst <- decode_float(src)` — unbox a `Float` Value in GP `src` to f64;
    /// deopt if `src` is not a Float.
    FloatToFpr {
        src: GP,
        dst: FPReg,
        deopt: DestLabel,
        base: usize,
    },
    /// Materialize the constant integer `i` as both a boxed `Value` (into frame
    /// `slot`) and an f64 (into FP register `dst`).
    I64ToBoth {
        i: i64,
        slot: SlotId,
        dst: FPReg,
        base: usize,
    },
    /// `dst <- lhs <op> rhs` in FP registers (the four arithmetic ops).
    FloatBinOp {
        kind: BinOpK,
        lhs: FPReg,
        rhs: FPReg,
        dst: FPReg,
        base: usize,
    },
    /// `dst <- <op> dst` in an FP register (`Neg` flips the sign bit; `Pos` is a
    /// no-op).
    FloatUnOp {
        kind: UnOpK,
        dst: FPReg,
        base: usize,
    },
    /// Float comparison producing a Ruby boolean in the accumulator. NaN
    /// compares false for every operator except `!=`; the encoder picks
    /// NaN-correct condition codes per arch.
    FloatCmp {
        kind: CmpKind,
        lhs: FPReg,
        rhs: FPReg,
        base: usize,
    },
    /// Fused float compare + conditional branch to `dest` (NaN-correct).
    FloatCmpBr {
        kind: CmpKind,
        lhs: FPReg,
        rhs: FPReg,
        brkind: BrKind,
        dest: DestLabel,
        base: usize,
    },
}

/// A straight-line sequence of `LInst`s produced by lowering one (or more)
/// `AsmInst`. Stage 2 builds one of these per migrated family and hands it to
/// the per-arch encoder; for now it is a thin, ergonomic builder over `Vec`.
#[derive(Debug, Clone, Default)]
pub(in crate::codegen::jitgen) struct Lir {
    insts: Vec<LInst>,
}

impl Lir {
    pub(in crate::codegen::jitgen) fn new() -> Self {
        Self::default()
    }

    pub(in crate::codegen::jitgen) fn is_empty(&self) -> bool {
        self.insts.is_empty()
    }

    pub(in crate::codegen::jitgen) fn len(&self) -> usize {
        self.insts.len()
    }

    pub(in crate::codegen::jitgen) fn iter(&self) -> std::slice::Iter<'_, LInst> {
        self.insts.iter()
    }

    pub(in crate::codegen::jitgen) fn push(&mut self, inst: LInst) -> &mut Self {
        self.insts.push(inst);
        self
    }

    pub(in crate::codegen::jitgen) fn mov(&mut self, dst: GP, src: GP) -> &mut Self {
        self.push(LInst::Mov { dst, src })
    }

    pub(in crate::codegen::jitgen) fn load_imm(&mut self, dst: GP, imm: u64) -> &mut Self {
        self.push(LInst::LoadImm { dst, imm })
    }

    pub(in crate::codegen::jitgen) fn load(&mut self, dst: impl Into<LReg>, mem: LMem) -> &mut Self {
        self.push(LInst::Load {
            dst: dst.into(),
            mem,
        })
    }

    pub(in crate::codegen::jitgen) fn store(&mut self, src: GP, mem: LMem) -> &mut Self {
        self.push(LInst::Store { src, mem })
    }

    pub(in crate::codegen::jitgen) fn store_imm(&mut self, imm: u64, mem: LMem) -> &mut Self {
        self.push(LInst::StoreImm { imm, mem })
    }

    pub(in crate::codegen::jitgen) fn alu(
        &mut self,
        op: LAluOp,
        dst: GP,
        lhs: GP,
        rhs: impl Into<LOperand>,
    ) -> &mut Self {
        self.push(LInst::Alu {
            op,
            dst,
            lhs,
            rhs: rhs.into(),
        })
    }

    pub(in crate::codegen::jitgen) fn cmp(&mut self, lhs: GP, rhs: impl Into<LOperand>) -> &mut Self {
        self.push(LInst::Cmp {
            lhs,
            rhs: rhs.into(),
        })
    }

    pub(in crate::codegen::jitgen) fn label(&mut self, label: DestLabel) -> &mut Self {
        self.push(LInst::Label(label))
    }

    pub(in crate::codegen::jitgen) fn br(&mut self, target: DestLabel) -> &mut Self {
        self.push(LInst::Br(target))
    }

    pub(in crate::codegen::jitgen) fn cond_br(&mut self, cond: LCond, target: DestLabel) -> &mut Self {
        self.push(LInst::CondBr { cond, target })
    }
}

// ## The per-arch lowering seam
//
// The encoder is an inherent `Codegen::encode_linst(inst: LInst)` method,
// defined once per arch in the file that the `compile` module includes for the
// active `target_arch` (`arch/x86_64/compile/mod.rs` emits via `monoasm!`;
// `arch/aarch64/compile.rs` via `monoasm_arm64!`). Because only one of those
// files compiles per target, no trait/dynamic dispatch is needed — the right
// `encode_linst` is selected by `cfg`. Each backend pattern-matches an `LInst`,
// performs immediate/displacement **legalization** (folding small immediates,
// materializing large ones into scratch x9/x10 on aarch64), and emits
// byte-identical output to the `emit_*` primitive it replaces.
//
// Stage 2-A wires only the `Mov` family (behind `emit_reg_move`); the remaining
// variants `todo!()` until their `AsmInst` family is migrated.

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lcond_invert_is_involution() {
        for c in [
            LCond::Eq,
            LCond::Ne,
            LCond::Lt,
            LCond::Le,
            LCond::Gt,
            LCond::Ge,
        ] {
            assert_eq!(c, c.invert().invert());
        }
        assert_eq!(LCond::Lt.invert(), LCond::Ge);
        assert_eq!(LCond::Le.invert(), LCond::Gt);
    }

    #[test]
    fn builder_records_in_order() {
        let mut lir = Lir::new();
        lir.mov(GP::Rax, GP::Rdi)
            .load(GP::Rcx, LMem::Slot(SlotId::new(3)))
            .alu(LAluOp::Add, GP::Rax, GP::Rax, GP::Rcx)
            .cmp(GP::Rax, 0i64);
        assert_eq!(lir.len(), 4);
        assert_eq!(
            lir.iter().next().cloned(),
            Some(LInst::Mov {
                dst: GP::Rax,
                src: GP::Rdi
            })
        );
    }
}
