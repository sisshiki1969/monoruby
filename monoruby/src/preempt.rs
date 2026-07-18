//! Phase-1 preemptive scheduling for the M:1 green-thread runtime.
//!
//! A dedicated timer OS thread "nudges" the interpreter's GC poll flag
//! every [`TICK`] while two or more green threads are alive. The VM and
//! JIT already poll that flag (`>= 8` calls `execute_gc`) at every
//! method call and loop back-edge, so the nudge makes the running
//! thread reach `execute_gc`, which consumes the request and calls
//! `scheduler::pass` — i.e. preemption is exactly "as if every thread
//! called `Thread.pass` at its next safepoint", reusing the existing
//! cooperative switching machinery unchanged: no new context-switch
//! paths and zero codegen changes.
//!
//! ## The flag protocol
//!
//! The poll flag is a `u32` in JIT memory, shared by several writers:
//! - the RValue-arena path adds `+1` per nearly-full page,
//! - the signal stubs add `+10`,
//! - `request_gc` / the malloc trigger lift it into the `>= 8` band,
//! - the preempt timer ORs in [`PREEMPT_BIT`].
//!
//! [`consume_poll_flag`] (top of `execute_gc`) strips `PREEMPT_BIT` and
//! reports `(base, preempt)`: a collection is wanted only when
//! `base >= 8`, so a pure preempt tick never triggers a spurious full
//! GC, and page-fill accumulation below the trigger band is preserved.
//! `PREEMPT_BIT` is bit 30, not 31: the x86-64 poll is `cmpl ...; jge`
//! (a *signed* compare), so bit 31 would read as negative and the poll
//! would never fire.
//!
//! ## Lifetime safety
//!
//! The timer runs on another OS thread and writes into JIT memory owned
//! by this thread's `Codegen`. `flag_addr` is behind a mutex: the timer
//! locks it around every write, and [`codegen_dropped`] (called from
//! `Codegen::drop`) zeroes it under the same lock before the memory is
//! freed — after that the timer can never dereference it again.
//!
//! ## Env switches
//!
//! - `MONORUBY_NO_PREEMPT=1` — never start the timer (cooperative-only,
//!   for debugging and bisection).
//! - `MONORUBY_PREEMPT_STRESS=1` — treat *every* poll-site visit as a
//!   preempt request and re-arm the flag after each poll, so every
//!   safepoint performs a switch attempt: the deterministic torture
//!   mode, the scheduling analog of `gc-stress`.

use std::cell::RefCell;
use std::sync::atomic::{AtomicBool, AtomicU32, Ordering};
use std::sync::{Arc, Mutex};

/// See the module doc: bit 30, not 31 (the x86-64 poll compare is signed).
pub(crate) const PREEMPT_BIT: u32 = 1 << 30;

/// Timeslice; the same order of magnitude as CRuby's thread quantum.
const TICK: std::time::Duration = std::time::Duration::from_millis(10);

struct Shared {
    /// Tells the timer thread to exit at its next tick.
    stop: AtomicBool,
    /// Address of this interpreter's poll flag (0 = detached). The mutex
    /// makes timer writes and detachment mutually exclusive, so the
    /// timer can never write into freed JIT memory.
    flag_addr: Mutex<usize>,
}

struct State {
    shared: Arc<Shared>,
    timer: Option<std::thread::JoinHandle<()>>,
}

thread_local! {
    /// Per interpreter OS thread, like `ALLOC` / `CODEGEN` / `SCHEDULER`.
    static STATE: RefCell<State> = RefCell::new(State {
        shared: Arc::new(Shared {
            stop: AtomicBool::new(false),
            flag_addr: Mutex::new(0),
        }),
        timer: None,
    });

    /// Owning-thread copy of the flag address for the hot Rust-side poll
    /// (`flag_pending`): a plain `Cell` read, no `RefCell` or mutex on
    /// the per-block-invocation fast path. Kept in sync with
    /// `Shared::flag_addr` by `register_flag` / `codegen_dropped`.
    static FLAG_HOT: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
}

fn no_preempt() -> bool {
    static ON: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ON.get_or_init(|| std::env::var_os("MONORUBY_NO_PREEMPT").is_some())
}

pub(crate) fn stress() -> bool {
    static ON: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ON.get_or_init(|| std::env::var_os("MONORUBY_PREEMPT_STRESS").is_some())
}

/// Register the interpreter's poll-flag address (from `Codegen` init).
pub(crate) fn register_flag(addr: *mut u32) {
    STATE.with(|st| {
        *st.borrow().shared.flag_addr.lock().unwrap() = addr as usize;
    });
    FLAG_HOT.with(|c| c.set(addr as usize));
    if stress() {
        // Arm the very first poll; `stress_renudge` re-arms after each one.
        // SAFETY: `addr` is the live poll flag just registered.
        unsafe { &*(addr as *const AtomicU32) }.fetch_or(PREEMPT_BIT, Ordering::Relaxed);
    }
}

/// `Codegen` is being dropped: detach the flag so the timer can never
/// write into freed JIT memory, and let the timer wind down.
pub(crate) fn codegen_dropped() {
    // `try_with`: thread-local teardown order is unspecified; if `STATE`
    // is already gone its timer got no flag to write through anyway.
    let _ = STATE.try_with(|st| {
        let st = st.borrow();
        *st.shared.flag_addr.lock().unwrap() = 0;
        st.shared.stop.store(true, Ordering::Relaxed);
    });
    let _ = FLAG_HOT.try_with(|c| c.set(0));
}

/// Whether the poll flag is in its trigger band — the Rust-side
/// safepoint check (`Executor::poll_safepoint`). One `Cell` read plus
/// one relaxed atomic load on the fast path.
#[inline]
pub(crate) fn flag_pending() -> bool {
    FLAG_HOT.with(|c| {
        let addr = c.get();
        // SAFETY: owning-thread read; the address is cleared before the
        // flag's JIT memory is freed (`codegen_dropped`).
        addr != 0 && unsafe { &*(addr as *const AtomicU32) }.load(Ordering::Relaxed) >= 8
    })
}

/// The live (non-dead) thread count changed. The timer runs exactly
/// while a timeslice switch could be useful (>= 2 live threads), so a
/// single-threaded program pays nothing: no timer, no nudges, no extra
/// poll hits.
pub(crate) fn on_thread_count(live: usize) {
    if no_preempt() || stress() {
        return;
    }
    STATE.with(|st| {
        let mut st = st.borrow_mut();
        if live >= 2 {
            st.shared.stop.store(false, Ordering::Relaxed);
            let running = st.timer.as_ref().is_some_and(|t| !t.is_finished());
            if !running {
                if let Some(t) = st.timer.take() {
                    let _ = t.join();
                }
                let shared = st.shared.clone();
                st.timer = Some(std::thread::spawn(move || timer_loop(shared)));
            }
        } else {
            st.shared.stop.store(true, Ordering::Relaxed);
        }
    });
}

fn timer_loop(shared: Arc<Shared>) {
    loop {
        std::thread::sleep(TICK);
        if shared.stop.load(Ordering::Relaxed) {
            return;
        }
        let addr = shared.flag_addr.lock().unwrap();
        if *addr != 0 {
            // SAFETY: non-zero only while the owning `Codegen` is alive;
            // the lock excludes concurrent detachment.
            let flag = unsafe { &*(*addr as *const AtomicU32) };
            flag.fetch_or(PREEMPT_BIT, Ordering::Relaxed);
        }
    }
}

/// Consume the poll flag at `execute_gc` entry: strip `PREEMPT_BIT` and
/// return `(base, preempt)`. `base >= 8` means an actual GC request
/// (page-fill accumulation, malloc trigger, `GC.start`, or a signal
/// stub's `+10`); a pure preempt tick leaves `base` below the band.
pub(crate) fn consume_poll_flag() -> (u32, bool) {
    STATE.with(|st| {
        let st = st.borrow();
        let addr = *st.shared.flag_addr.lock().unwrap();
        if addr == 0 {
            // Defensive: a direct `execute_gc` call with no registered
            // flag behaves exactly as before this module existed.
            return (8, false);
        }
        // SAFETY: this is the owning interpreter thread — the flag (JIT
        // memory in this thread's `Codegen`) is alive while it executes.
        let flag = unsafe { &*(addr as *const AtomicU32) };
        let v = flag.load(Ordering::Relaxed);
        let preempt = v & PREEMPT_BIT != 0;
        if preempt {
            flag.fetch_and(!PREEMPT_BIT, Ordering::Relaxed);
        }
        (v & !PREEMPT_BIT, preempt || stress())
    })
}

/// Stress mode: re-arm the flag so the very next poll site fires again.
pub(crate) fn stress_renudge() {
    if !stress() {
        return;
    }
    STATE.with(|st| {
        let addr = *st.borrow().shared.flag_addr.lock().unwrap();
        if addr != 0 {
            // SAFETY: owning-thread access, as in `consume_poll_flag`.
            unsafe { &*(addr as *const AtomicU32) }.fetch_or(PREEMPT_BIT, Ordering::Relaxed);
        }
    });
}
