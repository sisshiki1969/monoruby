//! POSIX signal → Ruby exception mapping.
//!
//! See doc/signal_handling.md for the full plan. This module is the
//! single source of truth for:
//!
//! 1. Which signals monoruby installs an async-signal-safe handler for.
//! 2. Which Ruby class / message each pending signal lowers to at the
//!    next poll point.
//!
//! Deliberately omitted: SIGSEGV / SIGBUS / SIGFPE / SIGILL / SIGABRT.
//! These are genuine runtime bugs (or explicit `abort`s); the kernel's
//! default core-dump path is the right response, not a Ruby `rescue`.

use crate::{MonorubyErr, Value};

/// List of POSIX signals monoruby installs an async-signal handler
/// for *by default*. Only SIGINT is here: every other signal's CRuby
/// default action is *terminate the process* (or be signal-delivered
/// up to `Process.wait`), not raise — installing an unconditional
/// catch-and-convert would break `Process.kill("TERM", child); $?.signaled?`
/// patterns that fork tests rely on. The remaining entries in
/// `signo_to_error` are intentionally pre-wired so that A7
/// (`Signal.trap`) only needs to register the handler at trap-time,
/// not also teach the runtime a new mapping.
///
/// Order is delivery priority — lowest signo bit is consumed first.
pub(crate) const POSIX_SIGNALS: &[i32] = &[libc::SIGINT];

/// Signals `Signal.trap` is allowed to install a handler for. An
/// async-signal-safe asm stub is pre-generated for each of these at
/// `Codegen::new` time (see codegen.rs), so trapping at runtime only
/// has to `sigaction(2)` the existing stub — no JIT codegen on a live
/// buffer.
///
/// Deliberately excluded:
/// - SIGKILL / SIGSTOP — uncatchable by the kernel.
/// - SIGILL / SIGTRAP / SIGABRT / SIGBUS / SIGFPE / SIGSEGV — genuine
///   faults; the kernel core-dump path is the right response (see the
///   module header). CRuby also forbids trapping most of these.
/// Deliberately excluded:
/// - SIGKILL / SIGSTOP — uncatchable by the kernel (see `is_uncatchable`).
/// - SIGILL / SIGTRAP / SIGABRT / SIGBUS / SIGFPE / SIGSEGV — genuine
///   faults; the kernel core-dump path is the right response. CRuby also
///   forbids trapping SEGV/BUS/FPE/ILL (and SIGVTALRM, which it uses
///   internally) with `ArgumentError: can't trap reserved signal`.
// SIGPWR is Linux-only (signal 30); macOS / BSDs do not define it, so we
// drop it from the trappable set on those hosts. The rest of the table is
// POSIX-portable. Two cfg-gated definitions keep `TRAPPABLE_SIGNALS` a
// plain `&[i32]` const so callers in codegen.rs can iterate it as before.
#[cfg(target_os = "linux")]
pub(crate) const TRAPPABLE_SIGNALS: &[i32] = &[
    libc::SIGHUP,
    libc::SIGINT,
    libc::SIGQUIT,
    libc::SIGUSR1,
    libc::SIGUSR2,
    libc::SIGPIPE,
    libc::SIGALRM,
    libc::SIGTERM,
    libc::SIGCHLD,
    libc::SIGCONT,
    libc::SIGTSTP,
    libc::SIGTTIN,
    libc::SIGTTOU,
    libc::SIGURG,
    libc::SIGXCPU,
    libc::SIGXFSZ,
    libc::SIGPROF,
    libc::SIGWINCH,
    libc::SIGIO,
    libc::SIGPWR,
    libc::SIGSYS,
];

#[cfg(not(target_os = "linux"))]
pub(crate) const TRAPPABLE_SIGNALS: &[i32] = &[
    libc::SIGHUP,
    libc::SIGINT,
    libc::SIGQUIT,
    libc::SIGUSR1,
    libc::SIGUSR2,
    libc::SIGPIPE,
    libc::SIGALRM,
    libc::SIGTERM,
    libc::SIGCHLD,
    libc::SIGCONT,
    libc::SIGTSTP,
    libc::SIGTTIN,
    libc::SIGTTOU,
    libc::SIGURG,
    libc::SIGXCPU,
    libc::SIGXFSZ,
    libc::SIGPROF,
    libc::SIGWINCH,
    libc::SIGIO,
    libc::SIGSYS,
];

/// Whether `signo` may be trapped via `Signal.trap` / `Kernel#trap`.
pub(crate) fn is_trappable(signo: i32) -> bool {
    TRAPPABLE_SIGNALS.contains(&signo)
}

/// Signals the kernel does not allow catching at all. CRuby surfaces a
/// trap attempt on these as `Errno::EINVAL` ("Invalid argument") or an
/// `ArgumentError` ("Signal already used by VM or OS"); we use the
/// latter.
pub(crate) fn is_uncatchable(signo: i32) -> bool {
    signo == libc::SIGKILL || signo == libc::SIGSTOP
}

/// Whether `signo` is part of the default-install set (its async stub is
/// `sigaction`'d at startup). On `Signal.trap(sig, "DEFAULT")` such a
/// signal keeps monoruby's runtime default (e.g. SIGINT ⇒ raise
/// `Interrupt`) rather than reverting to the kernel's `SIG_DFL`.
pub(crate) fn is_default_installed(signo: i32) -> bool {
    POSIX_SIGNALS.contains(&signo)
}

/// What should happen when a trapped signal is observed at the next
/// poll point, and what the *next* `Signal.trap` reports as the previous
/// handler. Stored per-signo in `Globals` (see globals.rs) and set by
/// `Signal.trap`.
#[derive(Clone, Copy)]
pub(crate) enum SignalDisposition {
    /// monoruby's runtime default: lower to the `signo_to_error`
    /// exception for default-installed signals (SIGINT ⇒ Interrupt),
    /// otherwise the OS `SIG_DFL` applies and the bit is never set.
    /// Reported back to Ruby as `"DEFAULT"`.
    Default,
    /// The OS default disposition (`SIG_DFL`). Reported as
    /// `"SYSTEM_DEFAULT"`. This is the initial state of every signal
    /// except the default-installed set.
    SystemDefault,
    /// Swallow the signal (`SIG_IGN` at the OS level, so the pending bit
    /// is normally never set — the poll arm is a defensive drop).
    /// `from_nil` records whether it was requested via `nil` (reported
    /// back as `nil`) or via "IGNORE"/"SIG_IGN" (reported as `"IGNORE"`).
    Ignore { from_nil: bool },
    /// Invoke this Ruby object (passed the signo) via `#call` at the poll
    /// point. May be a `Proc`, `Method`, or any callable — non-callables
    /// raise `NoMethodError` at delivery, matching CRuby.
    Handler(Value),
}

/// Convert a pending signo into the Ruby error the poll point should
/// raise. Returns `None` for signals we do not handle (defensive: the
/// asm stub only sets bits for signals we installed, so this should be
/// unreachable in practice).
pub(crate) fn signo_to_error(signo: i32) -> Option<MonorubyErr> {
    let err = match signo {
        libc::SIGINT => MonorubyErr::interrupt("Interrupt"),
        libc::SIGTERM => MonorubyErr::signalexception("SIGTERM"),
        libc::SIGHUP => MonorubyErr::signalexception("SIGHUP"),
        libc::SIGUSR1 => MonorubyErr::signalexception("SIGUSR1"),
        libc::SIGUSR2 => MonorubyErr::signalexception("SIGUSR2"),
        libc::SIGQUIT => MonorubyErr::signalexception("SIGQUIT"),
        libc::SIGPIPE => MonorubyErr::signalexception("SIGPIPE"),
        _ => return None,
    };
    Some(err)
}

/// The lowest-numbered pending signo in `bitmap`, or `None` if no bits
/// are set. Bit `n` corresponds to signo `n + 1` (so SIGINT = 2 ⇒ bit
/// 1). Lowest-numbered wins, matching A6 delivery priority. The poll
/// point looks the signo up in the per-signal trap table before deciding
/// whether to run a handler, ignore it, or fall back to `signo_to_error`.
pub(crate) fn lowest_pending_signo(bitmap: u32) -> Option<i32> {
    if bitmap == 0 {
        None
    } else {
        Some(bitmap.trailing_zeros() as i32 + 1)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::MonorubyErrKind;
    use crate::globals::{INTERRUPT_CLASS, SIGNAL_EXCEPTION_CLASS};

    /// SIGINT maps to the dedicated `Interrupt` class with the
    /// conventional "Interrupt" message (A4 in doc/signal_handling.md).
    #[test]
    fn sigint_maps_to_interrupt() {
        let err = signo_to_error(libc::SIGINT).expect("SIGINT must map");
        assert_eq!(err.kind(), &MonorubyErrKind::Other(INTERRUPT_CLASS));
        assert_eq!(err.message(), "Interrupt");
    }

    /// Every pre-wired terminate-class signal maps to `SignalException`
    /// with a "SIG…" message (A3 / A6 in doc/signal_handling.md). These
    /// arms are unreachable through the default install set (SIGINT
    /// only), so this is their only coverage until A7 (`Signal.trap`)
    /// flips the install bit.
    #[test]
    fn terminate_signals_map_to_signalexception() {
        for (signo, msg) in [
            (libc::SIGTERM, "SIGTERM"),
            (libc::SIGHUP, "SIGHUP"),
            (libc::SIGUSR1, "SIGUSR1"),
            (libc::SIGUSR2, "SIGUSR2"),
            (libc::SIGQUIT, "SIGQUIT"),
            (libc::SIGPIPE, "SIGPIPE"),
        ] {
            let err = signo_to_error(signo).unwrap_or_else(|| panic!("{msg} must map"));
            assert_eq!(
                err.kind(),
                &MonorubyErrKind::Other(SIGNAL_EXCEPTION_CLASS),
                "{msg} should lower to SignalException"
            );
            assert_eq!(err.message(), msg);
        }
    }

    /// Signals deliberately left to the kernel (SIGSEGV, …) and any
    /// other unmapped signo hit the defensive fall-through and return
    /// `None`.
    #[test]
    fn unmapped_signo_returns_none() {
        assert!(signo_to_error(libc::SIGSEGV).is_none());
        assert!(signo_to_error(libc::SIGCHLD).is_none());
    }

    /// An empty bitmap drains to no signo.
    #[test]
    fn empty_bitmap_is_none() {
        assert!(lowest_pending_signo(0).is_none());
    }

    /// A single pending bit drains to its signo, which maps to the
    /// matching error. Bit `n` corresponds to signo `n + 1`, so SIGINT
    /// (signo 2) is bit 1.
    #[test]
    fn single_pending_bit_drains() {
        let bit = 1u32 << (libc::SIGINT - 1) as u32;
        let signo = lowest_pending_signo(bit).expect("SIGINT bit must drain");
        assert_eq!(signo, libc::SIGINT);
        let err = signo_to_error(signo).expect("SIGINT must map");
        assert_eq!(err.kind(), &MonorubyErrKind::Other(INTERRUPT_CLASS));
    }

    /// When several signals are pending the lowest signo wins (delivery
    /// priority, per A6). SIGINT (2) beats SIGTERM (15).
    #[test]
    fn lowest_signo_wins() {
        let bitmap = (1u32 << (libc::SIGINT - 1) as u32) | (1u32 << (libc::SIGTERM - 1) as u32);
        let signo = lowest_pending_signo(bitmap).expect("must drain");
        assert_eq!(signo, libc::SIGINT, "SIGINT must win over SIGTERM");
    }

    /// A pending bit for an unmapped signo (SIGTRAP, signo 5) drains to
    /// its signo but `signo_to_error` returns `None` — the poll point
    /// then drops it (Default disposition, no mapping).
    #[test]
    fn pending_unmapped_bit_is_none() {
        let bit = 1u32 << (libc::SIGTRAP - 1) as u32;
        let signo = lowest_pending_signo(bit).expect("bit must drain to a signo");
        assert_eq!(signo, libc::SIGTRAP);
        assert!(signo_to_error(signo).is_none());
    }
}
