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
