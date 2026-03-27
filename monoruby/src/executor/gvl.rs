///
/// Global VM Lock (GVL) for multi-threaded Ruby execution.
///
/// The GVL ensures that only one Ruby thread executes Ruby code at a time,
/// matching CRuby's threading semantics. Threads can release the GVL during
/// blocking I/O operations to allow other threads to run.
///
use std::sync::{Condvar, Mutex};

/// Global VM Lock state.
struct GvlState {
    /// The thread that currently holds the GVL. `None` means unlocked.
    owner: Option<std::thread::ThreadId>,
    /// Reentrant lock count. The GVL is fully released when this reaches 0.
    count: usize,
}

/// The Global VM Lock.
pub struct Gvl {
    state: Mutex<GvlState>,
    condvar: Condvar,
}

impl Gvl {
    fn new() -> Self {
        Self {
            state: Mutex::new(GvlState {
                owner: None,
                count: 0,
            }),
            condvar: Condvar::new(),
        }
    }

    /// Acquire the GVL for the current thread. Blocks until available.
    /// Reentrant: if the current thread already holds the GVL, increments the count.
    pub fn acquire(&self) {
        let tid = std::thread::current().id();
        let mut state = self.state.lock().unwrap();
        if state.owner == Some(tid) {
            state.count += 1;
            return;
        }
        while state.owner.is_some() {
            state = self.condvar.wait(state).unwrap();
        }
        state.owner = Some(tid);
        state.count = 1;
    }

    /// Release the GVL. Decrements the reentrant count; only truly releases
    /// when the count reaches 0.
    pub fn release(&self) {
        let mut state = self.state.lock().unwrap();
        debug_assert_eq!(state.owner, Some(std::thread::current().id()));
        debug_assert!(state.count > 0);
        state.count -= 1;
        if state.count == 0 {
            state.owner = None;
            self.condvar.notify_one();
        }
    }

    /// Check if the current thread holds the GVL.
    #[allow(dead_code)]
    pub fn is_held(&self) -> bool {
        let state = self.state.lock().unwrap();
        state.owner == Some(std::thread::current().id())
    }
}

/// The singleton GVL instance.
static GVL: std::sync::LazyLock<Gvl> = std::sync::LazyLock::new(Gvl::new);

/// Acquire the GVL for the current thread. Blocks until available.
pub fn gvl_acquire() {
    GVL.acquire();
}

/// Release the GVL, allowing other threads to acquire it.
pub fn gvl_release() {
    GVL.release();
}

/// Check if the current thread holds the GVL.
#[allow(dead_code)]
pub fn gvl_is_held() -> bool {
    GVL.is_held()
}

/// Execute a closure without the GVL.
///
/// Releases the GVL, executes the closure, then re-acquires the GVL.
/// Use this for blocking I/O operations to allow other Ruby threads to run.
#[allow(dead_code)]
pub fn without_gvl<F, R>(f: F) -> R
where
    F: FnOnce() -> R,
{
    gvl_release();
    let result = f();
    gvl_acquire();
    result
}
