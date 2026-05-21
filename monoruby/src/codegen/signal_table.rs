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

use crate::MonorubyErr;

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

/// Drain the pending-signal bitmap into the first Ruby error to raise.
/// Lowest-numbered pending signal wins.
pub(crate) fn lowest_pending_to_error(bitmap: u32) -> Option<MonorubyErr> {
    if bitmap == 0 {
        return None;
    }
    let signo = bitmap.trailing_zeros() as i32 + 1;
    signo_to_error(signo)
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

    /// An empty bitmap drains to nothing.
    #[test]
    fn empty_bitmap_is_none() {
        assert!(lowest_pending_to_error(0).is_none());
    }

    /// A single pending bit lowers to the matching error. Bit `n`
    /// corresponds to signo `n + 1`, so SIGINT (signo 2) is bit 1.
    #[test]
    fn single_pending_bit_drains() {
        let bit = 1u32 << (libc::SIGINT - 1) as u32;
        let err = lowest_pending_to_error(bit).expect("SIGINT bit must drain");
        assert_eq!(err.kind(), &MonorubyErrKind::Other(INTERRUPT_CLASS));
    }

    /// When several signals are pending the lowest signo wins (delivery
    /// priority, per A6). SIGINT (2) beats SIGTERM (15).
    #[test]
    fn lowest_signo_wins() {
        let bitmap = (1u32 << (libc::SIGINT - 1) as u32) | (1u32 << (libc::SIGTERM - 1) as u32);
        let err = lowest_pending_to_error(bitmap).expect("must drain");
        assert_eq!(
            err.kind(),
            &MonorubyErrKind::Other(INTERRUPT_CLASS),
            "SIGINT must win over SIGTERM"
        );
    }

    /// A pending bit for an unmapped signo (SIGTRAP, signo 5) drains to
    /// `None` rather than panicking — exercises the `signo_to_error`
    /// `None` passthrough inside `lowest_pending_to_error`.
    #[test]
    fn pending_unmapped_bit_is_none() {
        let bit = 1u32 << (libc::SIGTRAP - 1) as u32;
        assert!(lowest_pending_to_error(bit).is_none());
    }
}
