//! Optional hang watchdog (doc/signal.md B+).
//!
//! monoruby runs Ruby on a single OS thread with no preemptive
//! scheduler, so some inherently-concurrent programs — and a handful of
//! single-thread-incompatible specs — can spin or block with no way to
//! make forward progress. When `MONORUBY_HANG_WATCHDOG_SEC=N` (N > 0)
//! is set, this module arms a `SIGALRM`-driven watchdog that aborts the
//! process after N seconds without the interpreter reaching a poll
//! point, turning an opaque CI timeout into a clear, fast failure.
//!
//! Mechanism:
//! - [`init`] reads the env var once and, if armed, installs a `SIGALRM`
//!   handler plus a 1 Hz interval timer (`setitimer`).
//! - [`poll`] is called from the VM poll point (`execute_gc`); it resets
//!   the countdown to the full budget — i.e. "progress was made".
//! - The `SIGALRM` handler decrements the countdown once per second and,
//!   when it reaches zero, writes a message and `_exit`s. The abort
//!   decision lives in the handler — not at the poll point — because a
//!   genuine hang never reaches the poll point.
//!
//! Disabled by default (env var unset or `<= 0`): [`init`] installs
//! nothing and [`poll`] is a single relaxed atomic load.

use std::sync::atomic::{AtomicI32, Ordering};

/// Configured budget in seconds (`0` = disabled). Set once by [`init`].
static BUDGET: AtomicI32 = AtomicI32::new(0);

/// Seconds remaining before the watchdog fires. Reset to [`BUDGET`] at
/// every poll point; decremented once per second by the `SIGALRM`
/// handler; aborts when it reaches zero.
static COUNTDOWN: AtomicI32 = AtomicI32::new(0);

/// Arm the watchdog from `MONORUBY_HANG_WATCHDOG_SEC`, at most once per
/// process. A no-op when the variable is unset, unparsable, or `<= 0`.
pub(crate) fn init() {
    static ONCE: std::sync::Once = std::sync::Once::new();
    ONCE.call_once(arm_from_env);
}

fn arm_from_env() {
    let secs = match std::env::var("MONORUBY_HANG_WATCHDOG_SEC") {
        Ok(s) => s.trim().parse::<i32>().unwrap_or(0),
        Err(_) => 0,
    };
    if secs <= 0 {
        return;
    }
    BUDGET.store(secs, Ordering::Relaxed);
    COUNTDOWN.store(secs, Ordering::Relaxed);

    // SAFETY: textbook libc signal/timer setup. The handler is
    // async-signal-safe (only atomic ops, `write(2)` and `_exit(2)`),
    // and we install it before arming the timer so it can never observe
    // an uninitialised state.
    unsafe {
        let mut sa: libc::sigaction = std::mem::zeroed();
        sa.sa_sigaction = handler as *const () as usize;
        sa.sa_flags = libc::SA_RESTART;
        libc::sigemptyset(&mut sa.sa_mask);
        if libc::sigaction(libc::SIGALRM, &sa, std::ptr::null_mut()) != 0 {
            return;
        }
        let it = libc::itimerval {
            it_interval: libc::timeval {
                tv_sec: 1,
                tv_usec: 0,
            },
            it_value: libc::timeval {
                tv_sec: 1,
                tv_usec: 0,
            },
        };
        libc::setitimer(libc::ITIMER_REAL, &it, std::ptr::null_mut());
    }

    eprintln!("monoruby: hang watchdog armed (MONORUBY_HANG_WATCHDOG_SEC={secs})");
}

/// Whether the watchdog is armed (env var set to a positive budget).
/// When armed, the watchdog owns `SIGALRM`: the runtime's default
/// signal-conversion install skips ALRM so the watchdog's handler is
/// not displaced (see `Codegen::new`); if the watchdog arms *after*
/// codegen init, its own `sigaction` overwrites the stub anyway.
pub(crate) fn armed() -> bool {
    BUDGET.load(Ordering::Relaxed) > 0
}

/// Record interpreter progress. Called from the VM poll point; resets
/// the countdown so the watchdog only fires after a full [`BUDGET`]
/// seconds with no poll point reached. Cheap (one relaxed load) and a
/// no-op when the watchdog is disarmed.
#[inline]
pub(crate) fn poll() {
    let budget = BUDGET.load(Ordering::Relaxed);
    if budget > 0 {
        COUNTDOWN.store(budget, Ordering::Relaxed);
    }
}

/// `SIGALRM` handler. Async-signal-safe: only atomic ops, `write(2)`
/// and `_exit(2)`.
extern "C" fn handler(_signo: libc::c_int) {
    // `fetch_sub` returns the previous value: still > 1 means there is
    // budget left after this tick, so keep waiting.
    if COUNTDOWN.fetch_sub(1, Ordering::Relaxed) > 1 {
        return;
    }
    const MSG: &[u8] = b"\nmonoruby: hang watchdog fired - no interpreter progress within MONORUBY_HANG_WATCHDOG_SEC; aborting\n";
    // SAFETY: `write` and `_exit` are async-signal-safe; `MSG` is static.
    unsafe {
        libc::write(2, MSG.as_ptr() as *const libc::c_void, MSG.len());
        libc::_exit(134);
    }
}
