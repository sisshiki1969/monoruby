//! Structured provenance for JIT deoptimizations (`feature = "deopt"`).
//!
//! # Why this exists
//!
//! The deopt log used to report a "cause" that was simply whatever sat in
//! rdi when the side exit ran. That was wrong twice over, and the two
//! defects compounded into four consecutive misdiagnoses while chasing the
//! activerecord deopt storm:
//!
//! 1. **The value was stale.** The exit handler read rdi *after* the deopt
//!    write-back, and the write-back calls into C (`f64_to_val`,
//!    `create_array`, …), which clobbers rdi. Most of the time the log
//!    printed garbage — usually `UNDEFINED`, the rendering of a zero word.
//! 2. **The guard was unidentifiable.** Deopt exits are deduplicated by
//!    `(pc, write_back, chain)`, and a single [`AsmDeopt`] is routinely
//!    handed to several guards even before that. So neither the handler nor
//!    the `AsmDeopt` index names the branch that was actually taken.
//!
//! # The fix
//!
//! Identity has to be recorded where the branch *is*, so each lowering site
//! that wants a deopt label gets a small trampoline of its own:
//!
//! ```text
//! site_NNN:
//!     movq [rbx + EXECUTOR_DEOPT_CAUSE], <cause register>
//!     movl [rbx + EXECUTOR_DEOPT_SITE],  <site id>
//!     jmp  <deduplicated deopt handler>
//! ```
//!
//! rbx holds `&mut Executor` throughout a JIT body, so this needs no
//! scratch register, no stack traffic, and no flag-clobbering instruction.
//! It runs before the write-back, so the operand is captured while it is
//! still the one the guard saw — that fixes (1) without moving the log
//! call, which stays where it always was (after the write-back, with
//! nothing live in registers). And it is per branch, which fixes (2). The
//! handlers themselves stay deduplicated; only the tiny trampolines
//! multiply.
//!
//! One caveat is inherited rather than introduced: the write-back can
//! trigger a GC between the capture and the log, so a heap object that was
//! reachable only from the guard's register may be gone by the time the
//! bits are rendered. [`crate::globals::dump`] therefore validates the word
//! (`Value::debug_check`) before decoding it and always prints the raw
//! bits, so a stale word reads as `<not a Value>` rather than as a lie.
//!
//! Two registries back the ids: [`DeoptSite`] describes a branch (which
//! guard, which emitter, what operand), [`DeoptExit`] describes a handler
//! (deopt / evict / counter-gated recompile). The handler bakes its exit id
//! as an immediate, so that half is correct by construction rather than by
//! a marker symbol left in a register.
use super::*;

///
/// What a guard was looking at when it decided to deoptimize.
///
/// Declared by the lowering site, which is the only place that knows which
/// register (if any) still holds a meaningful operand on *every* path that
/// reaches the label. The rule for choosing: if some edge into this label
/// leaves the register undefined, the honest answer is [`Self::Static`].
///
/// The type exists in every build; only `deopt` builds emit code for it.
///
#[derive(Clone, Copy, Debug)]
pub(crate) enum DeoptCause {
    /// A Ruby `Value` lives in this GP register at the branch.
    Value(GP),
    /// A Ruby `Value` in this GP register whose *class* the guard tested
    /// against the one baked in at compile time. Both halves matter: a
    /// class guard's story is "expected X, got a Y", and the expected half
    /// is not recoverable from the operand alone.
    ClassGuard(GP, ClassId),
    /// A Ruby `Value` in this GP register, which the guard compared
    /// against a `Value` baked into the code at compile time.
    ValueVsBaked(GP, crate::Value),
    /// Bits in a GP register that are *not* a `Value` (raw pointers, byte
    /// counts, …). Logged as a hex word, never decoded.
    #[allow(dead_code)] // reserved for non-Value operands (fiddle pointers, byte counts)
    Raw(GP),
    /// No runtime operand worth recording: the guard tested global state (a
    /// version word, a BOP flag, a counter), or is unconditional, or looks
    /// at an unboxed float.
    ///
    /// Floats are deliberately in this bucket. An [`FPReg`] is *virtual* —
    /// resolving one needs the frame's `base_stack_offset`, since a spilled
    /// float lives on the stack rather than in an `xmm` — and threading a
    /// frame through every guard's lowering to recover a operand for two
    /// call sites is not worth it. The string names the guard instead.
    Static(&'static str),
}

#[cfg(feature = "deopt")]
pub(crate) use enabled::*;

#[cfg(feature = "deopt")]
mod enabled {
    use super::*;
    use std::panic::Location;
    use std::sync::RwLock;

    ///
    /// One branch into a deopt handler: a single trampoline.
    ///
    #[derive(Clone, Copy)]
    pub(crate) struct DeoptSite {
        /// Where the *guard* was lowered — identifies the guard family
        /// (`GuardClass`, `GuardConstBaseClass`, …).
        pub(crate) lowered_at: &'static Location<'static>,
        /// Where the exit this branch targets was created — the front-end
        /// emitter that decided a deopt was needed here.
        pub(crate) created_at: Option<&'static Location<'static>>,
        pub(crate) cause: DeoptCause,
    }

    ///
    /// One emitted side-exit handler. Several sites may target the same one.
    ///
    #[derive(Clone, Copy)]
    pub(crate) enum DeoptExit {
        Deopt { chain: bool },
        Evict,
        Recompile { reason: RecompileReason, chain: bool },
    }

    static SITES: RwLock<Vec<DeoptSite>> = RwLock::new(Vec::new());
    static EXITS: RwLock<Vec<DeoptExit>> = RwLock::new(Vec::new());

    pub(crate) fn register_site(site: DeoptSite) -> u32 {
        let mut sites = SITES.write().unwrap();
        sites.push(site);
        (sites.len() - 1) as u32
    }

    pub(crate) fn site(id: u32) -> Option<DeoptSite> {
        SITES.read().unwrap().get(id as usize).copied()
    }

    pub(crate) fn register_exit(exit: DeoptExit) -> u32 {
        let mut exits = EXITS.write().unwrap();
        exits.push(exit);
        (exits.len() - 1) as u32
    }

    pub(crate) fn exit(id: u32) -> Option<DeoptExit> {
        EXITS.read().unwrap().get(id as usize).copied()
    }

    impl std::fmt::Display for DeoptExit {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Self::Deopt { chain } => {
                    write!(f, "deopt{}", if *chain { " (chained)" } else { "" })
                }
                Self::Evict => write!(f, "evict"),
                Self::Recompile { reason, chain } => write!(
                    f,
                    "recompile[{reason:?}]{}",
                    if *chain { " (chained)" } else { "" }
                ),
            }
        }
    }
}
