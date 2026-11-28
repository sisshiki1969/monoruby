//! Kernel-blocking operations (see doc/threads.md §9).
//!
//! Some syscalls block *in the kernel* with no fd to poll — `flock(2)`
//! waiting for a lock, `open(2)` on a FIFO waiting for its peer,
//! `fcntl(F_SETLKW)` — and so do foreign calls a binding declared
//! blocking. The builtin packages the operation as a [`NativeOp`] — raw
//! fds, paths and integers only, never heap references — and hands it to
//! [`run_blocking`], which runs it on the calling kernel thread with the
//! GVL released (`scheduler::native::run_blocking`), so the other Ruby
//! threads keep running meanwhile.

/// A kernel-blocking operation. Raw bytes and fds only: it runs without
/// the GVL and must never touch the Ruby heap or the interpreter.
pub(crate) enum NativeOp {
    /// `flock(fd, op)` — `op` without `LOCK_NB` (the non-blocking form
    /// is executed inline by the caller).
    Flock { fd: i32, op: i32 },
    /// Blocking `open(2)` — used for FIFOs, whose open blocks until the
    /// peer end appears.
    Open { path: std::ffi::CString, flags: i32, mode: u32 },
    /// `fcntl(fd, cmd, &arg)` for a command that waits in the kernel —
    /// `F_SETLKW` (and its open-file-description twin where the platform
    /// has one), which blocks until the record lock it asks for is
    /// available. `arg` is this job's own copy of the packed
    /// `struct flock`: the waiting commands only read it, so nothing has
    /// to travel back.
    Fcntl { fd: i32, cmd: i32, arg: Vec<u8> },
    /// A foreign call the binding declared blocking (`attach_function ...,
    /// blocking: true`). Unlike the two syscalls above, what runs here is
    /// arbitrary C chosen by the Ruby program, so the raw-data discipline is
    /// enforced on the fiddle side: `FfiWorkerCall` carries only the address
    /// of an immortal descriptor and already-marshalled C arguments, and its
    /// `run` never touches the Ruby heap. `ret` is the raw 64-bit result,
    /// which the waiter boxes; `errno` is unused.
    Ffi(crate::builtins::fiddle::FfiWorkerCall),
    /// A dynamically loaded extension's `call_blocking` (`src/ext.rs`):
    /// its own function and argument, raw, as `Ffi`. (The sqlite3
    /// extension opens and closes its connections here — they touch the
    /// filesystem — and its SQLite is `SQLITE_THREADSAFE=1`, so a
    /// connection is usable from whichever thread holds the GVL next.)
    Ext(crate::ext::ExtWorkerCall),
}

pub(crate) struct Completion {
    pub ret: i64,
    pub errno: i32,
}

impl NativeOp {
    /// Whether the caller may re-issue this operation after `EINTR`: the
    /// three syscalls are idempotent waits, a foreign call is not.
    pub(crate) fn retries_eintr(&self) -> bool {
        !matches!(self, NativeOp::Ffi(_) | NativeOp::Ext(_))
    }
}

/// Perform `op` once on the calling kernel thread, as `(ret, errno)`.
/// An `EINTR` is returned to the caller, which handles the signal under
/// the GVL and issues the operation again (`native::run_blocking`).
pub(crate) fn run_op(op: &NativeOp) -> (i64, i32) {
    match op {
        NativeOp::Flock { fd, op } => {
            // SAFETY: plain flock(2) on a raw fd owned by the caller's
            // IO object (kept alive by the waiting caller's frame).
            let r = unsafe { libc::flock(*fd, *op) };
            if r == 0 {
                return (0, 0);
            }
            let errno = std::io::Error::last_os_error().raw_os_error().unwrap_or(0);
            (r as i64, errno)
        }
        NativeOp::Open { path, flags, mode } => {
            // SAFETY: NUL-terminated path, plain open(2).
            let r = unsafe { libc::open(path.as_ptr(), *flags, *mode as libc::c_uint) };
            if r >= 0 {
                return (r as i64, 0);
            }
            let errno = std::io::Error::last_os_error().raw_os_error().unwrap_or(0);
            (-1, errno)
        }
        NativeOp::Fcntl { fd, cmd, arg } => {
            // SAFETY: fcntl(2) with a pointer third argument — `arg` is
            // this operation's own buffer, alive for the call.
            let r = unsafe { libc::fcntl(*fd, *cmd, arg.as_ptr()) };
            if r != -1 {
                return (r as i64, 0);
            }
            let errno = std::io::Error::last_os_error().raw_os_error().unwrap_or(0);
            (-1, errno)
        }
        // No EINTR loop: the foreign function owns its own restart policy,
        // and retrying an arbitrary C call would not generally be safe.
        NativeOp::Ffi(call) => (call.run(), 0),
        NativeOp::Ext(call) => (call.run(), 0),
    }
}

/// Run `op` with the GVL released and hand back its outcome. A signal
/// that ends the wait early runs the poll point and the operation is
/// issued again; an interrupt queued meanwhile (kill / raise) propagates.
pub(crate) fn run_blocking(
    vm: &mut crate::executor::Executor,
    globals: &mut crate::globals::Globals,
    op: NativeOp,
) -> crate::executor::Result<Completion> {
    crate::scheduler::native::run_blocking(vm, globals, op)
}
