//! Phase-1 preemptive scheduling for the M:1 green-thread runtime.
//!
//! A dedicated timer OS thread arms the poll word's PREEMPT lane every
//! [`TICK`] while two or more green threads are alive. The VM and JIT
//! poll that word (any non-zero lane calls `execute_gc`) at every method
//! call and loop back-edge, so the tick makes the running thread reach
//! `execute_gc`, which consumes the lane and calls `scheduler::pass` —
//! i.e. preemption is exactly "as if every thread called `Thread.pass`
//! at its next safepoint", reusing the existing cooperative switching
//! machinery unchanged: no new context-switch paths and zero codegen
//! changes.
//!
//! The lane protocol (who sets/clears which byte of the word) lives in
//! poll_flag.rs; `execute_gc` consumes the PREEMPT lane through
//! [`crate::poll_flag::consume_preempt`]. This module owns only the
//! timer thread and the stress switches.
//!
//! ## Lifetime safety
//!
//! The timer runs on another OS thread and writes into JIT memory owned
//! by this thread's `Codegen`. `flag_addr` is behind a mutex: the timer
//! locks it around every write, and [`codegen_dropped`] (called from
//! `Codegen::drop`) zeroes it under the same lock before the memory is
//! freed — after that the timer can never dereference it again. (This is
//! why the timer does not use poll_flag.rs's registries: excluding
//! teardown needs the lock.)
//!
//! ## Env switches
//!
//! - `MONORUBY_NO_PREEMPT=1` — never start the timer (cooperative-only,
//!   for debugging and bisection).
//! - `MONORUBY_PREEMPT_STRESS=1` — treat *every* poll-site visit as a
//!   preempt request and re-arm the lane after each poll, so every
//!   safepoint performs a switch attempt: the deterministic torture
//!   mode, the scheduling analog of `gc-stress`.

use std::cell::RefCell;
use std::sync::atomic::{AtomicBool, AtomicU32, Ordering};
use std::sync::{Arc, Mutex};

use crate::poll_flag::PREEMPT_LANE;

/// Timeslice; the same order of magnitude as CRuby's thread quantum.
const TICK: std::time::Duration = std::time::Duration::from_millis(10);

struct Shared {
    /// Tells the timer thread to exit at its next tick.
    stop: AtomicBool,
    /// Address of this interpreter's poll word (0 = detached). The mutex
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
}

fn no_preempt() -> bool {
    static ON: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ON.get_or_init(|| std::env::var_os("MONORUBY_NO_PREEMPT").is_some())
}

pub(crate) fn stress() -> bool {
    static ON: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ON.get_or_init(|| std::env::var_os("MONORUBY_PREEMPT_STRESS").is_some())
}

/// Register the interpreter's poll-word address (from `Codegen` init).
pub(crate) fn register_flag(addr: *mut u32) {
    STATE.with(|st| {
        *st.borrow().shared.flag_addr.lock().unwrap() = addr as usize;
    });
    if stress() {
        // Arm the very first poll; `stress_renudge` re-arms after each one.
        // SAFETY: `addr` is the live poll word just registered.
        unsafe { &*(addr as *const AtomicU32) }.fetch_or(PREEMPT_LANE, Ordering::Relaxed);
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
            flag.fetch_or(PREEMPT_LANE, Ordering::Relaxed);
        }
    }
}

/// Stress mode: re-arm the lane so the very next poll site fires again.
pub(crate) fn stress_renudge() {
    if !stress() {
        return;
    }
    crate::poll_flag::set_preempt();
}
