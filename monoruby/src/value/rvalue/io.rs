mod buf;

use std::{
    cell::{Cell, RefCell},
    collections::HashSet,
    io::{BufRead, IsTerminal, Read, Seek, SeekFrom},
    mem::ManuallyDrop,
    os::fd::{AsRawFd, FromRawFd, IntoRawFd},
    os::unix::process::ExitStatusExt,
    rc::Rc,
    sync::{LazyLock, Mutex, MutexGuard},
};

use buf::{DrainErr, IoReader, IoWriter, StdFd, WriteBuf};

use super::*;

/// monoruby's own buffers for the three standard descriptors.
///
/// Process-global rather than per-`IO`-object: `$stdout` / `STDOUT` and
/// any other IO naming fd 1 must share one buffer, or their output
/// interleaves in the wrong order. A `Mutex` rather than a `thread_local!`
/// for the same reason across the test harness, which runs one
/// interpreter per OS thread over the same standard descriptors. Ruby
/// `Thread`s are green threads multiplexed on one OS thread, so this is
/// uncontended in normal use, and the guard is never held across a
/// context switch (each call locks, works, and returns).
static STDIN_BUF: LazyLock<Mutex<IoReader<StdFd>>> =
    LazyLock::new(|| Mutex::new(IoReader::new(StdFd::new(0))));
static STDOUT_BUF: LazyLock<Mutex<IoWriter<StdFd>>> = LazyLock::new(|| {
    let fd = StdFd::new(1);
    // CRuby: `$stdout.sync` is false, on a TTY as well — a TTY instead
    // writes through via `FMODE_TTY`.
    let tty = fd.is_terminal();
    Mutex::new(IoWriter::new(fd, false, tty))
});
static STDERR_BUF: LazyLock<Mutex<IoWriter<StdFd>>> = LazyLock::new(|| {
    let fd = StdFd::new(2);
    let tty = fd.is_terminal();
    // CRuby: `$stderr.sync` is true.
    Mutex::new(IoWriter::new(fd, true, tty))
});

fn stdin_buf() -> MutexGuard<'static, IoReader<StdFd>> {
    STDIN_BUF.lock().unwrap()
}

pub(crate) fn stdout_buf() -> MutexGuard<'static, IoWriter<StdFd>> {
    STDOUT_BUF.lock().unwrap()
}

fn stderr_buf() -> MutexGuard<'static, IoWriter<StdFd>> {
    STDERR_BUF.lock().unwrap()
}

/// Push the standard streams' buffers out to the kernel. Called at
/// interpreter exit, where there is no longer anywhere to report a
/// failure to.
pub fn flush_std_streams() {
    let _ = stdout_buf().drain(&signal_pending);
    let _ = stderr_buf().drain(&signal_pending);
}

/// Append to monoruby's stdout buffer, flushing per the stream's policy.
/// Used by `Kernel#p` / `#print`, which write to the process's stdout
/// without going through a Ruby `IO` object — sharing the one buffer
/// keeps their output ordered against `$stdout.write`.
pub fn write_stdout(bytes: &[u8], store: &Store) -> Result<()> {
    let mut progress = 0;
    stdout_buf()
        .write(bytes, &mut progress, &signal_pending)
        .map_err(|e| drain_err(e, store))
}

pub fn flush_stdout(store: &Store) -> Result<()> {
    stdout_buf()
        .drain(&signal_pending)
        .map_err(|e| drain_err(e, store))
}

/// Map a stalled drain to the internal marker / error the IO builtins
/// expect. Bytes the kernel took are already out of the buffer, so the
/// builtin's retry resumes without duplicating output.
fn drain_err(e: DrainErr, store: &Store) -> MonorubyErr {
    match e {
        DrainErr::Signal => MonorubyErr::signal_interrupt(),
        DrainErr::WouldBlock => MonorubyErr::would_block_interrupt(),
        // Surface the OS error as the matching Errno::* (e.g. Errno::EPIPE
        // on a closed pipe -- SIGPIPE is ignored at startup, as in CRuby).
        DrainErr::Io(e) => MonorubyErr::errno_plain(store, &e),
    }
}

thread_local! {
    /// File descriptors currently *owned* (autoclose = true) by a live
    /// `FileDescriptor` — i.e. fds that will be `close(2)`d when their
    /// `FileDescriptor` drops.
    ///
    /// monoruby stores every fd inside a Rust `std::fs::File` (an `OwnedFd`),
    /// and Rust's std **aborts the process** ("IO Safety violation: owned
    /// file descriptor already closed") if the same fd is closed twice. So
    /// `IO.new(existing_io.fileno)` — which by default (`autoclose: true`)
    /// would wrap the *already-owned* fd in a second closing `OwnedFd` —
    /// must not create a second owner. `io_new` consults this set and, when
    /// the fd is already owned, opens the new IO as a *borrow*
    /// (`autoclose: false`, released via `into_raw_fd` without closing) so
    /// only the original owner ever closes the fd.
    static OWNED_FDS: RefCell<HashSet<i32>> = RefCell::new(HashSet::new());
}

/// Whether `fd` is already owned by a live autoclosing `FileDescriptor`.
pub fn fd_is_owned(fd: i32) -> bool {
    OWNED_FDS.with(|s| s.borrow().contains(&fd))
}

fn register_owned_fd(fd: i32) {
    OWNED_FDS.with(|s| {
        s.borrow_mut().insert(fd);
    });
}

fn unregister_owned_fd(fd: i32) {
    OWNED_FDS.with(|s| {
        s.borrow_mut().remove(&fd);
    });
}

/// Recover the raw POSIX `wait(2)` status word from an `ExitStatus` so that
/// Ruby-side `Process::Status` can decode exit code vs termination signal
/// uniformly. `ExitStatus::code()` returns `None` for signal-terminated
/// children, which loses information; using the raw status preserves it.
fn encode_wait_status(status: &std::process::ExitStatus) -> i32 {
    status.into_raw()
}

/// Whether an async signal handler has recorded a pending signal that the
/// VM has not yet drained at a poll point.
fn signal_pending() -> bool {
    crate::codegen::signal_table::PENDING_SIGNALS.load(std::sync::atomic::Ordering::Relaxed) != 0
}

/// Whether an io error out of the primitives below is one of the two
/// restartable interrupts: `Interrupted` (EINTR with a signal pending) or
/// `WouldBlock` (EAGAIN on an fd the green-thread scheduler put in
/// non-blocking mode). Both are surfaced as internal marker errors that
/// the IO builtins intercept and restart; see [`interrupt_marker`].
fn is_interrupt_kind(kind: std::io::ErrorKind) -> bool {
    matches!(
        kind,
        std::io::ErrorKind::Interrupted | std::io::ErrorKind::WouldBlock
    )
}

/// The internal marker error corresponding to a restartable interrupt
/// kind: the signal-interrupt marker for `Interrupted`, the would-block
/// marker for `WouldBlock` (see `MonorubyErr::{signal,would_block}_interrupt`).
fn interrupt_marker(kind: std::io::ErrorKind) -> MonorubyErr {
    if kind == std::io::ErrorKind::WouldBlock {
        MonorubyErr::would_block_interrupt()
    } else {
        MonorubyErr::signal_interrupt()
    }
}

/// Map an io error out of the interruptible primitives below: the
/// restartable interrupts (see [`is_interrupt_kind`]) become their internal
/// marker so the IO builtins can park/poll and restart; anything else goes
/// through `f` (the call site's existing mapping).
fn map_read_err(e: std::io::Error, f: impl FnOnce(String) -> MonorubyErr) -> MonorubyErr {
    if is_interrupt_kind(e.kind()) {
        interrupt_marker(e.kind())
    } else {
        f(e.to_string())
    }
}

/// RAII guard for the green-thread scheduler's blocking-IO emulation: put
/// `fd` into non-blocking mode so that a read/write that would block the
/// whole OS process returns `EAGAIN` instead (surfaced upward as the
/// would-block marker), and restore the original file-status flags on
/// drop. The restore keeps the non-blocking mode from leaking to child
/// processes spawned later and to other processes sharing the open file
/// description (e.g. a terminal).
///
/// `set` returns `None` — and the operation simply keeps its plain
/// blocking behavior — when the flag was already set (a `read_nonblock`
/// user fd: nothing to restore, `EAGAIN` already surfaces) or `fcntl`
/// failed.
pub(crate) struct NonblockGuard {
    fd: i32,
    flags: i32,
}

impl NonblockGuard {
    pub(crate) fn set(fd: i32) -> Option<Self> {
        // SAFETY: fcntl is called on a caller-supplied fd that is open for
        // the duration of the guarded operation; F_GETFL/F_SETFL do not
        // touch memory.
        unsafe {
            let flags = libc::fcntl(fd, libc::F_GETFL);
            if flags < 0 || flags & libc::O_NONBLOCK != 0 {
                return None;
            }
            if libc::fcntl(fd, libc::F_SETFL, flags | libc::O_NONBLOCK) < 0 {
                return None;
            }
            Some(Self { fd, flags })
        }
    }
}

impl Drop for NonblockGuard {
    fn drop(&mut self) {
        // SAFETY: best-effort restore of the flags captured in `set` on the
        // same fd.
        unsafe {
            libc::fcntl(self.fd, libc::F_SETFL, self.flags);
        }
    }
}

/// One read through `reader`, like `Read::read` but signal-aware: a bare
/// `EINTR` (no pending signal — e.g. SIGCHLD with SA_RESTART unset on
/// another handler) is retried, while `EINTR` with a pending signal is
/// surfaced as `Interrupted` so the caller can reach a VM poll point.
/// Rust std's own helpers (`read_to_end`, `Bytes`, `read_until`) retry
/// `Interrupted` unconditionally, which is exactly what makes a blocked
/// read un-killable by SIGTERM — never use them on fds that can block.
fn read_step(reader: &mut impl Read, buf: &mut [u8]) -> std::io::Result<usize> {
    loop {
        match reader.read(buf) {
            Err(e) if e.kind() == std::io::ErrorKind::Interrupted && !signal_pending() => continue,
            r => return r,
        }
    }
}

/// How much a single unbuffered `read(2)` asks for at most.
const READ_CHUNK: usize = 8192;

/// Signal-interruptible replacement for `bytes().take(len).collect()`:
/// append up to `len` bytes to `out`, stopping early at EOF. On
/// `Interrupted`, bytes read so far remain in `out` so the caller can
/// preserve them (pushback) before surfacing the interrupt.
fn read_upto(reader: &mut impl Read, len: usize, out: &mut Vec<u8>) -> std::io::Result<()> {
    while out.len() < len {
        // A signal delivered while we were in userspace (e.g. right after
        // the previous chunk) sets the pending bit without EINTR-ing
        // anything; entering a blocking read with the bit already set
        // would block unkillably. Check before every kernel entry.
        if signal_pending() {
            return Err(std::io::ErrorKind::Interrupted.into());
        }
        // Read straight into `out`'s tail. A fixed `[0u8; 8192]` scratch
        // array here cost an 8 KiB `memset` *per call* — 60% of `IO#getc`,
        // which asks for a single byte.
        let want = (len - out.len()).min(READ_CHUNK);
        let start = out.len();
        out.resize(start + want, 0);
        let n = match read_step(reader, &mut out[start..]) {
            Ok(n) => n,
            Err(e) => {
                // Drop the uninitialised tail but keep what was read, so
                // the caller can push it back before the interrupt
                // propagates.
                out.truncate(start);
                return Err(e);
            }
        };
        out.truncate(start + n);
        if n == 0 {
            break;
        }
    }
    Ok(())
}

/// Signal-interruptible replacement for `read_to_end`. On `Interrupted`,
/// bytes read so far remain in `out`.
fn read_all(reader: &mut impl Read, out: &mut Vec<u8>) -> std::io::Result<()> {
    loop {
        // See `read_upto` on why this is checked before every kernel entry.
        if signal_pending() {
            return Err(std::io::ErrorKind::Interrupted.into());
        }
        let start = out.len();
        out.resize(start + READ_CHUNK, 0);
        let n = match read_step(reader, &mut out[start..]) {
            Ok(n) => n,
            Err(e) => {
                out.truncate(start);
                return Err(e);
            }
        };
        out.truncate(start + n);
        if n == 0 {
            return Ok(());
        }
    }
}

/// Signal-interruptible replacement for `BufRead::read_until`. Returns the
/// number of bytes appended to `out`; on `Interrupted`, bytes read so far
/// remain in `out`.
fn read_until_step(
    reader: &mut impl BufRead,
    delim: u8,
    out: &mut Vec<u8>,
) -> std::io::Result<usize> {
    let mut total = 0;
    loop {
        // See `read_upto` on why this is checked before every kernel entry.
        if signal_pending() {
            return Err(std::io::ErrorKind::Interrupted.into());
        }
        let (found, used) = {
            let avail = match reader.fill_buf() {
                Ok(a) => a,
                Err(e) if e.kind() == std::io::ErrorKind::Interrupted => {
                    if signal_pending() {
                        return Err(e);
                    }
                    continue;
                }
                Err(e) => return Err(e),
            };
            match avail.iter().position(|&b| b == delim) {
                Some(i) => {
                    out.extend_from_slice(&avail[..=i]);
                    (true, i + 1)
                }
                None => {
                    out.extend_from_slice(avail);
                    (false, avail.len())
                }
            }
        };
        reader.consume(used);
        total += used;
        if found || used == 0 {
            return Ok(total);
        }
    }
}

/// Pull up to `need` bytes from a buffered reader for `readpartial`.
/// When `no_block` is set (ungetc pushback already produced data),
/// only the bytes already sitting in the internal buffer are taken;
/// otherwise `fill_buf` may block once to fetch more.
fn read_partial_chunk<T: Read>(
    reader: &mut IoReader<T>,
    need: usize,
    no_block: bool,
) -> Result<Vec<u8>> {
    let avail: &[u8] = if no_block {
        reader.buffer()
    } else {
        loop {
            match reader.fill_buf() {
                Ok(_) => break,
                Err(e) if e.kind() == std::io::ErrorKind::Interrupted => {
                    if signal_pending() {
                        return Err(MonorubyErr::signal_interrupt());
                    }
                }
                Err(e) if e.kind() == std::io::ErrorKind::WouldBlock => {
                    return Err(MonorubyErr::would_block_interrupt());
                }
                Err(e) => return Err(MonorubyErr::ioerr(e.to_string())),
            }
        }
        reader.buffer()
    };
    let n = avail.len().min(need);
    let chunk = avail[..n].to_vec();
    reader.consume(n);
    Ok(chunk)
}

/// How many more bytes are needed to complete a UTF-8 character that
/// `buf` may end in the middle of. 0 when the tail is a complete
/// character (or invalid, in which case there is nothing to complete).
fn utf8_missing_bytes(buf: &[u8]) -> usize {
    let len = buf.len();
    // A UTF-8 lead byte is at most 3 bytes from the end of a truncated
    // sequence (4-byte character missing its last byte).
    let start = len.saturating_sub(3);
    for i in (start..len).rev() {
        let b = buf[i];
        if b & 0b1100_0000 == 0b1000_0000 {
            continue; // continuation byte — keep looking for the lead
        }
        let need: usize = match b {
            0x00..=0x7f => 1,
            0xc0..=0xdf => 2,
            0xe0..=0xef => 3,
            0xf0..=0xf7 => 4,
            _ => 1, // invalid lead — treat as complete
        };
        return need.saturating_sub(len - i);
    }
    0
}

#[derive(Debug)]
pub struct FileDescriptor {
    reader: ManuallyDrop<IoReader<std::fs::File>>,
    name: String,
    /// The path as originally passed to `File.open`/`File.new`: raw
    /// bytes plus the argument's encoding tag, so `IO#path`/`#to_path`
    /// can reproduce it exactly (`name` is its lossy display form).
    path_raw: Option<(Vec<u8>, crate::value::Encoding)>,
    /// Whether `name` is a real filesystem path (surfaced via `IO#path`).
    /// `false` for placeholder names like `fd 3`/`pipe` created from a raw
    /// fd without an explicit `path:` option — CRuby's `IO#path` is `nil`
    /// in that case.
    has_path: bool,
    /// Access mode the descriptor was opened with. Read operations on a
    /// non-`readable` descriptor and write operations on a non-`writable`
    /// one raise `IOError`, matching CRuby.
    readable: bool,
    writable: bool,
    /// CRuby's `IO#autoclose=` semantics. When `true` (the default), the
    /// underlying fd is closed when this `FileDescriptor` is dropped. When
    /// `false`, ownership is released via `into_raw_fd` so the fd is *not*
    /// closed — required for the `File.new(other_io.fileno, ...)` pattern
    /// (see `logger/log_device.rb#fixup_mode`) where the original IO is
    /// expected to relinquish ownership of the fd to the new wrapper.
    autoclose: Cell<bool>,
    /// Bytes pushed back via `IO#ungetc` / `IO#ungetbyte`, served before
    /// the underlying reader on the next read. Stored in read order (front
    /// = next byte out); each unget splices its bytes at the front.
    pushback: RefCell<Vec<u8>>,
    /// monoruby's own write buffer over the *same* descriptor the reader
    /// owns (a `"r+"` file is read and written through one fd). Held
    /// behind a `RefCell` because writes reach here through `&self` —
    /// `&std::fs::File` is itself a `Write`, so no unique borrow of the
    /// descriptor is needed.
    wbuf: RefCell<WriteBuf>,
}

impl FileDescriptor {
    /// Push this descriptor's write buffer out to the fd. Writes go
    /// through `&std::fs::File`, so no unique borrow of the reader is
    /// needed and a buffered write can be flushed from a `&self` path.
    fn drain_wbuf(&self) -> std::result::Result<(), DrainErr> {
        let mut wbuf = self.wbuf.borrow_mut();
        if wbuf.buffered_len() == 0 {
            return Ok(());
        }
        let mut sink: &std::fs::File = self.reader.get_ref();
        wbuf.drain(&mut sink, &signal_pending)
    }
}

impl Drop for FileDescriptor {
    fn drop(&mut self) {
        let fd = self.reader.get_ref().as_raw_fd();
        // SAFETY: `reader` is wrapped in `ManuallyDrop` and is only taken
        // here, exactly once, in `Drop`. After this, `self.reader` must not
        // be accessed.
        let reader = unsafe { ManuallyDrop::take(&mut self.reader) };
        if self.autoclose.get() {
            // Normal case: dropping the `IoReader<File>` closes the fd via
            // `OwnedFd::drop`. This descriptor was the owner; release the
            // fd from the owned-fd set (before the number can be reused).
            unregister_owned_fd(fd);
            drop(reader);
        } else {
            // Borrowed-fd case: release ownership without closing. Some
            // other Ruby IO is responsible for the fd's lifetime.
            let _fd = reader.into_inner().into_raw_fd();
        }
    }
}

#[derive(Debug)]
pub struct PopenDescriptor {
    child: std::process::Child,
    pub(crate) reader: Option<IoReader<std::process::ChildStdout>>,
    pub(crate) writer: Option<IoWriter<std::process::ChildStdin>>,
    /// See `FileDescriptor::pushback`.
    pushback: RefCell<Vec<u8>>,
}

/// What an [`IoInner`] is a stream *over*.
#[derive(Debug)]
pub enum IoKind {
    Stdin,
    Stdout,
    Stderr,
    File(Rc<FileDescriptor>),
    Popen(Rc<PopenDescriptor>),
    /// Closed stream. Retains the filesystem path of a path-backed File
    /// (CRuby keeps `File#path` readable after close; tempfile.rb relies
    /// on `File.unlink(file.path)` from its cleanup/finalizer paths).
    Closed(Option<Box<String>>),
}

/// Which of `IO#external_encoding`'s four states a stream is in.
///
/// `Unset` and `Nil` are distinct: a stream that was never given one
/// resolves `Encoding.default_external` when read, while one explicitly
/// set to nil reports nil. (The old hidden-ivar encoding said `Unset` by
/// having no ivar at all and `Nil` by storing `nil` in it.)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExtEnc {
    Unset,
    Nil,
    /// Follow `Encoding.default_external` live, so later changes to it
    /// are picked up (`set_encoding(nil)`).
    Dynamic,
    /// The object in [`IoInner::ext_enc`].
    Fixed,
}

/// A Ruby `IO` object's state.
///
/// The per-object fields below used to live in hidden instance variables
/// (`/lineno`, `/enc_ext`, `/enc_int`, `/binmode`, and the
/// `__io_dynamic_default_external__` marker). Reading one cost a hash of
/// the name's `IdentId` plus an `IndexMap` probe for its slot, and the
/// encoding ones then parsed the encoding *name* out of the Encoding
/// object — `IO#gets` paid that six times and `IO#getc` eight, per call.
///
/// They are plain fields now. `IoInner` stays within `ObjKind`'s 48-byte
/// payload; the encodings are stored resolved (`Encoding` is 2 bytes) and
/// the Encoding *object* is reconstructed on demand, which is exact
/// because those objects are singletons.
#[derive(Debug, Clone)]
pub struct IoInner {
    kind: IoKind,
    /// `IO#lineno`. Per object: `#dup` copies it and the copy then
    /// advances independently.
    lineno: i64,
    /// `IO#external_encoding`'s Encoding *object*, or `None` when unset.
    ///
    /// The object, not the resolved `Encoding`: that enum is lossy
    /// (IBM866 folds to ASCII-8BIT) and `#external_encoding` has to hand
    /// back the one the stream was opened with.
    ext: Option<Value>,
    /// `IO#internal_encoding`'s object; `None` when unset.
    int: Option<Value>,
    ext_state: ExtEnc,
    /// `IO#binmode?`.
    binmode: bool,
}

/// Outcome of a non-blocking `IO#read_nonblock`.
pub enum NonblockRead {
    Data(Vec<u8>),
    WouldBlock,
    Eof,
}

/// Outcome of a non-blocking `IO#write_nonblock`.
pub enum NonblockWrite {
    Written(usize),
    WouldBlock,
}

impl std::clone::Clone for IoKind {
    fn clone(&self) -> Self {
        match self {
            Self::Stdin => Self::Stdin,
            Self::Stdout => Self::Stdout,
            Self::Stderr => Self::Stderr,
            Self::File(file) => Self::File(file.clone()),
            Self::Popen(popen) => Self::Popen(popen.clone()),
            Self::Closed(p) => Self::Closed(p.clone()),
        }
    }
}

impl std::fmt::Display for IoKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Stdin => write!(f, "#<IO:<STDIN>>"),
            Self::Stdout => write!(f, "#<IO:<STDOUT>>"),
            Self::Stderr => write!(f, "#<IO:<STDERR>>"),
            Self::File(file) => write!(f, "#<File:{}>", file.name),
            Self::Popen(_) => write!(f, "#<IO:popen>"),
            Self::Closed(..) => write!(f, "#<IO:(closed)>"),
        }
    }
}

impl IoInner {
    fn with_kind(kind: IoKind) -> Self {
        Self {
            kind,
            lineno: 0,
            ext: None,
            int: None,
            ext_state: ExtEnc::Unset,
            binmode: false,
        }
    }

    pub fn kind(&self) -> &IoKind {
        &self.kind
    }

    /// `IO#lineno`.
    pub fn lineno(&self) -> i64 {
        self.lineno
    }

    pub fn set_lineno(&mut self, n: i64) {
        self.lineno = n;
    }

    /// Advance `IO#lineno` by one and return the new value.
    pub fn bump_lineno(&mut self) -> i64 {
        self.lineno += 1;
        self.lineno
    }

    /// `IO#binmode?`.
    pub fn binmode(&self) -> bool {
        self.binmode
    }

    pub fn set_binmode(&mut self) {
        self.binmode = true;
    }

    /// `IO#external_encoding`'s state, and its object when `Fixed`.
    pub fn ext_state(&self) -> ExtEnc {
        self.ext_state
    }

    pub fn ext_enc(&self) -> Option<Value> {
        self.ext
    }

    pub fn set_ext_enc(&mut self, state: ExtEnc, obj: Option<Value>) {
        self.ext_state = state;
        self.ext = obj;
    }

    /// `IO#internal_encoding`'s object; `None` when unset.
    pub fn int_enc(&self) -> Option<Value> {
        self.int
    }

    pub fn set_int_enc(&mut self, int: Option<Value>) {
        self.int = int;
    }

    /// The encoding objects are GC roots reachable only from here.
    pub fn mark(&self, alloc: &mut alloc::Allocator<RValue>) {
        if let Some(v) = self.ext {
            v.mark(alloc);
        }
        if let Some(v) = self.int {
            v.mark(alloc);
        }
    }


    /// Push whatever monoruby has buffered out to the kernel.
    ///
    /// Only monoruby's own buffers are involved — there is no `fsync`
    /// here, matching CRuby's `IO#flush` (`IO#fsync` is separate).
    pub fn flush(&mut self, store: &Store) -> Result<()> {
        let res = match &mut self.kind {
            IoKind::Stdin => return Ok(()),
            IoKind::Stdout => stdout_buf().drain(&signal_pending),
            IoKind::Stderr => stderr_buf().drain(&signal_pending),
            IoKind::File(file) => file.drain_wbuf(),
            IoKind::Popen(popen) => {
                let popen = Rc::get_mut(popen).unwrap();
                match popen.writer {
                    Some(ref mut writer) => writer.drain(&signal_pending),
                    None => return Ok(()),
                }
            }
            IoKind::Closed(..) => return Err(MonorubyErr::ioerr("closed stream")),
        };
        res.map_err(|e| drain_err(e, store))
    }

    /// `IO#sync`.
    pub fn sync(&self) -> bool {
        match &self.kind {
            IoKind::Stdout => stdout_buf().sync(),
            IoKind::Stderr => stderr_buf().sync(),
            IoKind::File(file) => file.wbuf.borrow().sync(),
            IoKind::Popen(p) => p.writer.as_ref().map(|w| w.sync()).unwrap_or(false),
            // A read-only stream has no write buffer to bypass; CRuby
            // reports false.
            IoKind::Stdin | IoKind::Closed(..) => false,
        }
    }

    /// `IO#sync=`. Turning it on does not itself flush — CRuby only
    /// changes the policy for subsequent writes.
    pub fn set_sync(&mut self, sync: bool) {
        match &mut self.kind {
            IoKind::Stdout => stdout_buf().set_sync(sync),
            IoKind::Stderr => stderr_buf().set_sync(sync),
            IoKind::File(file) => file.wbuf.borrow_mut().set_sync(sync),
            IoKind::Popen(p) => {
                if let Some(w) = Rc::get_mut(p).unwrap().writer.as_mut() {
                    w.set_sync(sync)
                }
            }
            IoKind::Stdin | IoKind::Closed(..) => {}
        }
    }

    pub fn is_closed(&self) -> bool {
        matches!(self.kind, IoKind::Closed(..))
    }

    /// Whether the stream may be read from.
    pub fn is_readable(&self) -> bool {
        match &self.kind {
            IoKind::Stdin => true,
            IoKind::Stdout | IoKind::Stderr | IoKind::Closed(..) => false,
            IoKind::File(f) => f.readable,
            IoKind::Popen(p) => p.reader.is_some(),
        }
    }

    /// Whether the stream may be written to.
    pub fn is_writable(&self) -> bool {
        match &self.kind {
            IoKind::Stdout | IoKind::Stderr => true,
            IoKind::Stdin | IoKind::Closed(..) => false,
            IoKind::File(f) => f.writable,
            IoKind::Popen(p) => p.writer.is_some(),
        }
    }

    /// `IOError` unless the stream is open for reading.
    pub fn ensure_readable(&self) -> Result<()> {
        if self.is_closed() {
            return Err(MonorubyErr::ioerr("closed stream"));
        }
        if !self.is_readable() {
            return Err(MonorubyErr::ioerr("not opened for reading"));
        }
        Ok(())
    }

    /// `IOError` unless the stream is open for writing.
    pub fn ensure_writable(&self) -> Result<()> {
        if self.is_closed() {
            return Err(MonorubyErr::ioerr("closed stream"));
        }
        if !self.is_writable() {
            return Err(MonorubyErr::ioerr("not opened for writing"));
        }
        Ok(())
    }

    /// Close the IO. Returns `(raw_wait_status, pid)` for Popen, `None`
    /// otherwise. `raw_wait_status` is the POSIX `wait(2)` status word, so
    /// callers (and `Process::Status`) can distinguish exit code, signal
    /// termination, and core-dump state.
    pub fn close(&mut self, store: &Store) -> Result<Option<(i32, u32)>> {
        if self.is_closed() {
            return Err(MonorubyErr::ioerr("closed stream"));
        }
        // Anything still buffered has to reach the fd before it goes away
        // (CRuby's `finish_writeconv` / `io_fflush` in `rb_io_close`).
        if self.is_writable() {
            self.flush(store)?;
        }
        let popen_result = if let IoKind::Popen(popen) = &mut self.kind {
            let popen = Rc::get_mut(popen).unwrap();
            popen.reader = None;
            popen.writer = None;
            popen.child.stdout.take();
            let pid = popen.child.id();
            let raw_status = match popen.child.wait() {
                Ok(s) => encode_wait_status(&s),
                Err(_) => 0,
            };
            Some((raw_status, pid))
        } else {
            None
        };
        // Retain a path-backed File's path across close: CRuby keeps
        // `File#path` readable after close (tempfile.rb's cleanup calls
        // `File.unlink(file.path)` on a closed file).
        let retained = match &self.kind {
            IoKind::File(file) if file.has_path => Some(file.name.clone()),
            _ => None,
        };
        self.kind = IoKind::Closed(retained.map(Box::new));
        Ok(popen_result)
    }

    /// A freshly-closed stream (`IO.allocate`, `IO.pipe`'s pre-init
    /// placeholder). `path` is the `File#path` a path-backed File keeps
    /// readable after close.
    pub fn closed(path: Option<String>) -> Self {
        Self::with_kind(IoKind::Closed(path.map(Box::new)))
    }

    /// Copy the per-object state (`#lineno`, encodings, binmode) from
    /// `other`, leaving the stream itself alone.
    ///
    /// `IO#dup` and `IO#reopen` build a fresh descriptor but must carry
    /// this state over — these used to be instance variables, which
    /// `#initialize_copy` copied for free and `reopen` simply left in
    /// place.
    pub fn copy_state_from(&mut self, other: &Self) {
        self.lineno = other.lineno;
        self.ext = other.ext;
        self.int = other.int;
        self.ext_state = other.ext_state;
        self.binmode = other.binmode;
    }

    /// Replace the underlying stream while keeping the per-object state
    /// (`#lineno`, encodings, binmode). CRuby's `IO#close_read` /
    /// `#close_write` and the popen close paths do the same.
    pub fn close_kind(&mut self) {
        let path = match &self.kind {
            IoKind::Closed(p) => p.clone(),
            _ => None,
        };
        self.kind = IoKind::Closed(path);
    }

    /// `IO#close_write` on a popen stream: drop the write side, and close
    /// the whole thing once the read side is gone too.
    pub fn popen_close_write(&mut self) {
        if let IoKind::Popen(popen) = &mut self.kind {
            let popen = Rc::get_mut(popen).unwrap();
            popen.writer = None;
            if popen.reader.is_none() {
                self.close_kind();
            }
        }
    }

    /// `IO#close_read` on a popen stream; mirror of
    /// [`Self::popen_close_write`].
    pub fn popen_close_read(&mut self) {
        if let IoKind::Popen(popen) = &mut self.kind {
            let popen = Rc::get_mut(popen).unwrap();
            popen.reader = None;
            if popen.writer.is_none() {
                self.close_kind();
            }
        }
    }

    pub(super) fn stdin() -> Self {
        Self::with_kind(IoKind::Stdin)
    }

    pub(super) fn stdout() -> Self {
        Self::with_kind(IoKind::Stdout)
    }

    pub(super) fn stderr() -> Self {
        Self::with_kind(IoKind::Stderr)
    }

    pub(super) fn file(
        file: std::fs::File,
        name: String,
        path_raw: Option<(Vec<u8>, crate::value::Encoding)>,
        readable: bool,
        writable: bool,
    ) -> Self {
        register_owned_fd(file.as_raw_fd());
        let is_tty = file.is_terminal();
        Self::with_kind(IoKind::File(Rc::new(FileDescriptor {
            reader: ManuallyDrop::new(IoReader::new(file)),
            name,
            path_raw,
            has_path: true,
            readable,
            writable,
            autoclose: Cell::new(true),
            pushback: RefCell::new(Vec::new()),
            wbuf: RefCell::new(WriteBuf::new(false, is_tty)),
        })))
    }

    /// Wrap a socket fd (connected stream or listener). Like [`Self::file`]
    /// but with no filesystem path (`IO#path` is nil for sockets) and
    /// always opened read/write; `name` only feeds `Display`/inspect.
    pub(super) fn socket(file: std::fs::File, name: String) -> Self {
        register_owned_fd(file.as_raw_fd());
        let is_tty = file.is_terminal();
        // CRuby marks sockets synchronized (`rb_io_synchronized`).
        let sync = true;
        Self::with_kind(IoKind::File(Rc::new(FileDescriptor {
            reader: ManuallyDrop::new(IoReader::new(file)),
            name,
            path_raw: None,
            has_path: false,
            readable: true,
            writable: true,
            autoclose: Cell::new(true),
            pushback: RefCell::new(Vec::new()),
            wbuf: RefCell::new(WriteBuf::new(sync, is_tty)),
        })))
    }

    pub(crate) fn popen(mut child: std::process::Child) -> Self {
        let reader = child.stdout.take().map(IoReader::new);
        // Never a TTY; synchronized, as CRuby's `IO.popen` is — the child
        // is waiting on the other end of this pipe.
        let writer = child.stdin.take().map(|w| IoWriter::new(w, true, false));
        Self::with_kind(IoKind::Popen(Rc::new(PopenDescriptor {
            child,
            reader,
            writer,
            pushback: RefCell::new(Vec::new()),
        })))
    }

    pub(crate) fn pid(&self) -> Option<u32> {
        match &self.kind {
            IoKind::Popen(popen) => Some(popen.child.id()),
            _ => None,
        }
    }

    /// Like `from_raw_fd`, but with an explicit initial `autoclose`. When
    /// `autoclose` is true this `FileDescriptor` becomes the fd's owner and
    /// is recorded in `OWNED_FDS`; when false it merely borrows the fd (the
    /// caller guarantees another owner closes it), so it is not recorded and
    /// releases the fd via `into_raw_fd` on drop without closing.
    pub(crate) fn from_raw_fd_autoclose(
        fd: i32,
        name: String,
        has_path: bool,
        readable: bool,
        writable: bool,
        autoclose: bool,
    ) -> Self {
        // SAFETY: fd is a valid file descriptor obtained from pipe() or an
        // already-open descriptor supplied to `IO.new`.
        let file = unsafe { std::fs::File::from_raw_fd(fd) };
        let is_tty = file.is_terminal();
        if autoclose {
            register_owned_fd(fd);
        }
        Self::with_kind(IoKind::File(Rc::new(FileDescriptor {
            reader: ManuallyDrop::new(IoReader::new(file)),
            name,
            path_raw: None,
            has_path,
            readable,
            writable,
            autoclose: Cell::new(autoclose),
            pushback: RefCell::new(Vec::new()),
            wbuf: RefCell::new(WriteBuf::new(false, is_tty)),
        })))
    }

    /// Accept `data[*progress..]` into this stream's buffer, then push
    /// the buffer to the kernel if the stream writes through (`sync` or a
    /// TTY) or the buffer is full.
    ///
    /// Signal-interruptible: a bare `EINTR` is retried, while `EINTR`
    /// with a pending signal surfaces the internal signal-interrupt
    /// marker. `*progress` records what has been *accepted*, and the
    /// buffer itself records what the kernel has taken, so the builtin's
    /// `blocking_region` retry after a `Signal.trap` handler (or after an
    /// `EAGAIN` park) resumes exactly where it stopped and never
    /// duplicates output.
    pub fn write(&mut self, data: &[u8], progress: &mut usize, store: &Store) -> Result<()> {
        self.ensure_writable()?;
        let res = match &mut self.kind {
            IoKind::Stdout => stdout_buf().write(data, progress, &signal_pending),
            IoKind::Stderr => stderr_buf().write(data, progress, &signal_pending),
            IoKind::File(file) => {
                let mut wbuf = file.wbuf.borrow_mut();
                let mut sink: &std::fs::File = file.reader.get_ref();
                wbuf.write(&mut sink, data, progress, &signal_pending)
            }
            IoKind::Popen(popen) => {
                let popen = Rc::get_mut(popen).unwrap();
                // `ensure_writable` guaranteed the writer is present.
                let writer = popen.writer.as_mut().unwrap();
                writer.write(data, progress, &signal_pending)
            }
            // `ensure_writable` already rejected non-writable streams.
            IoKind::Stdin | IoKind::Closed(..) => unreachable!(),
        };
        res.map_err(|e| drain_err(e, store))
    }

    /// Push out anything a *write* left buffered before reading or
    /// repositioning.
    ///
    /// A `File` reads and writes through one descriptor, so a pending
    /// write has to reach the fd first — otherwise the read returns stale
    /// content and the file offset is wrong. CRuby does the same
    /// (`io_fflush` ahead of `io_fillbuf` / `io_seek`). A `Popen` needs
    /// nothing: the child's stdin and stdout are separate descriptors.
    fn flush_wbuf_before_read(&self) -> Result<()> {
        match &self.kind {
            IoKind::File(file) => file.drain_wbuf().map_err(|e| match e {
                DrainErr::Signal => MonorubyErr::signal_interrupt(),
                DrainErr::WouldBlock => MonorubyErr::would_block_interrupt(),
                DrainErr::Io(e) => MonorubyErr::ioerr(e.to_string()),
            }),
            _ => Ok(()),
        }
    }

    /// Whether a read can be satisfied without touching the fd: ungetc
    /// pushback or bytes already sitting in the internal BufReader. Used
    /// by the green-thread IO scheduler to skip the fd-readiness park.
    pub fn has_buffered_data(&self) -> bool {
        if self.pushback_len() > 0 {
            return true;
        }
        match &self.kind {
            IoKind::Stdin => !stdin_buf().buffer().is_empty(),
            IoKind::File(f) => !f.reader.buffer().is_empty(),
            IoKind::Popen(p) => p
                .reader
                .as_ref()
                .map(|r| !r.buffer().is_empty())
                .unwrap_or(false),
            _ => false,
        }
    }

    /// Bytes currently sitting in the `ungetc`/`ungetbyte` pushback buffer.
    pub fn pushback_len(&self) -> usize {
        match &self.kind {
            IoKind::File(f) => f.pushback.borrow().len(),
            IoKind::Popen(p) => p.pushback.borrow().len(),
            _ => 0,
        }
    }

    fn pushback_cell(&self) -> Option<&RefCell<Vec<u8>>> {
        match &self.kind {
            IoKind::File(f) => Some(&f.pushback),
            IoKind::Popen(p) => Some(&p.pushback),
            _ => None,
        }
    }

    /// Push `bytes` back so the next read returns them first. CRuby raises
    /// `IOError` on closed streams and on streams not opened for reading
    /// (`STDOUT`/`STDERR`). Each call splices at the front, so successive
    /// ungets behave LIFO while a single multi-byte unget preserves order.
    pub fn unget(&mut self, bytes: &[u8]) -> Result<()> {
        match &self.kind {
            IoKind::Closed(..) => Err(MonorubyErr::ioerr("closed stream")),
            IoKind::Stdin | IoKind::Stdout | IoKind::Stderr => {
                Err(MonorubyErr::ioerr("not opened for reading"))
            }
            IoKind::File(f) if !f.readable => {
                Err(MonorubyErr::ioerr("not opened for reading"))
            }
            IoKind::File(_) | IoKind::Popen(_) => {
                let cell = self.pushback_cell().unwrap();
                let mut pb = cell.borrow_mut();
                pb.splice(0..0, bytes.iter().copied());
                Ok(())
            }
        }
    }

    /// Take up to `max` bytes (all if `None`) from the front of the
    /// pushback buffer.
    fn take_pushback(&mut self, max: Option<usize>) -> Vec<u8> {
        let cell = match self.pushback_cell() {
            Some(c) => c,
            None => return vec![],
        };
        let mut pb = cell.borrow_mut();
        let n = match max {
            Some(m) => m.min(pb.len()),
            None => pb.len(),
        };
        pb.drain(..n).collect()
    }

    pub fn read(&mut self, length: Option<usize>) -> Result<Vec<u8>> {
        self.read_impl(length, true)
    }

    /// Like [`Self::read`], but a sized read may fill the BufReader's
    /// page from the fd (the classic buffered behavior). The
    /// character-oriented readers (getc/getbyte) use this — CRuby
    /// buffers those, which the `#ungetc` + `#readpartial` interplay
    /// observes — while `IO#read(n)` consumes the fd exactly.
    pub fn read_buffered(&mut self, length: Option<usize>) -> Result<Vec<u8>> {
        self.read_impl(length, false)
    }

    fn read_impl(&mut self, length: Option<usize>, exact: bool) -> Result<Vec<u8>> {
        if self.pushback_len() > 0 {
            match length {
                Some(0) => return Ok(vec![]),
                Some(n) if n <= self.pushback_len() => {
                    return Ok(self.take_pushback(Some(n)));
                }
                Some(n) => {
                    let out = self.take_pushback(None);
                    let need = n - out.len();
                    return self.read_more_preserving(out, Some(need), exact);
                }
                None => {
                    let out = self.take_pushback(None);
                    return self.read_more_preserving(out, None, exact);
                }
            }
        }
        self.read_underlying(length, exact)
    }

    /// Extend already-drained pushback bytes (`out`) with an underlying
    /// read. On a restartable interrupt out of the read, `out` must go
    /// back into the pushback buffer before the marker propagates — the
    /// caller retries the whole operation and would otherwise lose those
    /// bytes. (`read_underlying` has already pushed back its own partial
    /// data at that point; ungetting `out` afterwards splices it in front,
    /// preserving stream order.)
    fn read_more_preserving(
        &mut self,
        mut out: Vec<u8>,
        length: Option<usize>,
        exact: bool,
    ) -> Result<Vec<u8>> {
        match self.read_underlying(length, exact) {
            Ok(chunk) => {
                out.extend_from_slice(&chunk);
                Ok(out)
            }
            Err(err) => {
                if (err.is_signal_interrupt() || err.is_would_block_interrupt())
                    && !out.is_empty()
                {
                    let _ = self.unget(&out);
                }
                Err(err)
            }
        }
    }

    fn read_underlying(&mut self, length: Option<usize>, exact: bool) -> Result<Vec<u8>> {
        self.flush_wbuf_before_read()?;
        // On a restartable interrupt (`Interrupted`/`WouldBlock` out of the
        // read helpers), bytes already consumed from the fd are pushed back
        // so that the retried read (after a `Signal.trap` handler ran, or
        // after the green thread was parked until the fd became ready)
        // returns them first and no data is lost. `pushback` is `None` for
        // Stdin, which has no pushback cell — an interrupted stdin read may
        // drop the partial data, like the pre-existing ungetc limitation
        // there.
        let interrupted = |kind: std::io::ErrorKind,
                           partial: Vec<u8>,
                           pushback: Option<&RefCell<Vec<u8>>>|
         -> MonorubyErr {
            if !partial.is_empty()
                && let Some(cell) = pushback
            {
                cell.borrow_mut().splice(0..0, partial);
            }
            interrupt_marker(kind)
        };
        match &mut self.kind {
            IoKind::Closed(..) => return Err(MonorubyErr::ioerr("closed stream")),
            IoKind::Stdin => {
                let mut buf = vec![];
                let res = if let Some(length) = length {
                    read_upto(&mut *stdin_buf(), length, &mut buf)
                } else {
                    read_all(&mut *stdin_buf(), &mut buf)
                };
                match res {
                    Ok(()) => Ok(buf),
                    Err(e) => Err(map_read_err(e, MonorubyErr::runtimeerr)),
                }
            }
            IoKind::Stdout => Err(MonorubyErr::argumenterr("can't read from $stdin")),
            IoKind::Stderr => Err(MonorubyErr::argumenterr("can't read from $stderr")),
            IoKind::File(file) => {
                if !file.readable {
                    return Err(MonorubyErr::ioerr("not opened for reading"));
                }
                let file = Rc::get_mut(file).unwrap();
                let reader = &mut *file.reader;
                let mut buf = vec![];
                let res = if let Some(length) = length {
                    if exact {
                        // A sized read takes exactly `length` bytes: drain
                        // what the BufReader already holds, then read the
                        // remainder from the fd directly. Letting the
                        // BufReader fill its 8K buffer here would advance
                        // the fd far past the logical position, which
                        // `#syswrite`, `#dup` (shared file offset) and
                        // write-after-read positioning all observe (CRuby
                        // reads exactly `length` too).
                        let avail = reader.buffer().len().min(length);
                        if avail > 0 {
                            buf.extend_from_slice(&reader.buffer()[..avail]);
                            reader.consume(avail);
                        }
                        if buf.len() < length {
                            read_upto(reader.get_mut(), length, &mut buf)
                        } else {
                            Ok(())
                        }
                    } else {
                        read_upto(reader, length, &mut buf)
                    }
                } else {
                    read_all(reader, &mut buf)
                };
                match res {
                    Ok(()) => Ok(buf),
                    Err(e) if is_interrupt_kind(e.kind()) => {
                        Err(interrupted(e.kind(), buf, Some(&file.pushback)))
                    }
                    Err(e) => Err(MonorubyErr::runtimeerr(e.to_string())),
                }
            }
            IoKind::Popen(popen) => {
                let popen = Rc::get_mut(popen).unwrap();
                let reader = popen
                    .reader
                    .as_mut()
                    .ok_or_else(|| MonorubyErr::ioerr("not opened for reading"))?;
                let mut buf = vec![];
                let res = if let Some(length) = length {
                    read_upto(reader, length, &mut buf)
                } else {
                    read_all(reader, &mut buf)
                };
                match res {
                    Ok(()) => Ok(buf),
                    Err(e) if is_interrupt_kind(e.kind()) => {
                        Err(interrupted(e.kind(), buf, Some(&popen.pushback)))
                    }
                    Err(e) => Err(MonorubyErr::ioerr(e.to_string())),
                }
            }
        }
    }

    /// Low-level read used by `IO#sysread`: read up to `maxlen` bytes
    /// with a single underlying read, bypassing the buffered reader.
    ///
    /// For a `File`, the `BufReader` is first seeked to its logical
    /// position — which discards its internal buffer — so the
    /// subsequent direct read on the underlying file starts at the
    /// right offset and leaves the `BufReader` consistent for any
    /// later buffered reads. Any ungetc pushback is drained first.
    /// Returns an empty `Vec` only at end of file (the caller raises
    /// `EOFError`).
    pub fn sysread(&mut self, maxlen: usize) -> Result<Vec<u8>> {
        self.flush_wbuf_before_read()?;
        use std::io::{Seek, SeekFrom};
        let mut out = if self.pushback_len() > 0 {
            self.take_pushback(Some(maxlen))
        } else {
            vec![]
        };
        if out.len() >= maxlen {
            return Ok(out);
        }
        let need = maxlen - out.len();
        let chunk = match &mut self.kind {
            IoKind::Closed(..) => return Err(MonorubyErr::ioerr("closed stream")),
            IoKind::Stdout | IoKind::Stderr => {
                return Err(MonorubyErr::ioerr("not opened for reading"));
            }
            IoKind::Stdin => {
                // `sysread` bypasses the buffer (CRuby raises if anything
                // is buffered; monoruby just reads the fd directly).
                let mut buf = vec![0u8; need];
                let n = read_step(stdin_buf().get_mut(), &mut buf)
                    .map_err(|e| map_read_err(e, MonorubyErr::runtimeerr))?;
                buf.truncate(n);
                buf
            }
            IoKind::File(file) => {
                if !file.readable {
                    return Err(MonorubyErr::ioerr("not opened for reading"));
                }
                let fdesc = Rc::get_mut(file).unwrap();
                let reader = &mut *fdesc.reader;
                // Sync the underlying fd to the logical position and
                // discard the BufReader buffer. Best-effort: pipe /
                // socket fds (also stored as `File`) are not seekable
                // (`ESPIPE`), in which case the single direct read
                // below simply returns whatever is available.
                let _ = reader.seek(SeekFrom::Current(0));
                let mut buf = vec![0u8; need];
                let n = match read_step(reader.get_mut(), &mut buf) {
                    Ok(n) => n,
                    Err(e) if is_interrupt_kind(e.kind()) => {
                        // Preserve pushback bytes already drained into
                        // `out` for the retry after the trap handler /
                        // fd-readiness park.
                        if !out.is_empty() {
                            fdesc.pushback.borrow_mut().splice(0..0, out);
                        }
                        return Err(interrupt_marker(e.kind()));
                    }
                    Err(e) => return Err(MonorubyErr::runtimeerr(e.to_string())),
                };
                buf.truncate(n);
                buf
            }
            IoKind::Popen(popen) => {
                let popen = Rc::get_mut(popen).unwrap();
                let reader = match popen.reader.as_mut() {
                    Some(r) => r,
                    None => return Err(MonorubyErr::ioerr("not opened for reading")),
                };
                let mut buf = vec![0u8; need];
                let n = match read_step(reader.get_mut(), &mut buf) {
                    Ok(n) => n,
                    Err(e) if is_interrupt_kind(e.kind()) => {
                        if !out.is_empty() {
                            popen.pushback.borrow_mut().splice(0..0, out);
                        }
                        return Err(interrupt_marker(e.kind()));
                    }
                    Err(e) => return Err(MonorubyErr::ioerr(e.to_string())),
                };
                buf.truncate(n);
                buf
            }
        };
        out.extend(chunk);
        Ok(out)
    }

    /// Set `O_NONBLOCK` on the underlying fd (idempotent).
    fn set_nonblock(&self) -> Result<()> {
        let fd = self.fileno()?;
        // SAFETY: fd is a valid descriptor for the lifetime of this IO.
        unsafe {
            let flags = libc::fcntl(fd, libc::F_GETFL);
            if flags < 0 || libc::fcntl(fd, libc::F_SETFL, flags | libc::O_NONBLOCK) < 0 {
                return Err(MonorubyErr::ioerr(
                    std::io::Error::last_os_error().to_string(),
                ));
            }
        }
        Ok(())
    }

    /// `IO#read_nonblock` core: a single non-blocking read of up to
    /// `maxlen` bytes. Drains ungetc pushback first; otherwise sets
    /// `O_NONBLOCK` and issues one raw `read(2)`. Reports `WouldBlock`
    /// on `EAGAIN`/`EWOULDBLOCK` and `Eof` on a 0-byte read.
    pub fn read_nonblock(&mut self, maxlen: usize, store: &Store) -> Result<NonblockRead> {
        self.flush_wbuf_before_read()?;
        if self.pushback_len() > 0 {
            return Ok(NonblockRead::Data(self.take_pushback(Some(maxlen))));
        }
        if !self.is_readable() {
            return Err(MonorubyErr::ioerr("not opened for reading"));
        }
        // Best-effort: sync a seekable File's BufReader to its logical
        // position (discarding its buffer) so the raw read below is at
        // the right offset; pipes/sockets aren't seekable and skip it.
        if let IoKind::File(file) = &mut self.kind {
            let reader = &mut *Rc::get_mut(file).unwrap().reader;
            let _ = reader.seek(SeekFrom::Current(0));
        }
        let fd = self.fileno()?;
        self.set_nonblock()?;
        let mut buf = vec![0u8; maxlen];
        // SAFETY: fd is valid; buf has `maxlen` bytes of capacity.
        let n = unsafe { libc::read(fd, buf.as_mut_ptr() as *mut libc::c_void, maxlen) };
        if n > 0 {
            buf.truncate(n as usize);
            Ok(NonblockRead::Data(buf))
        } else if n == 0 {
            Ok(NonblockRead::Eof)
        } else {
            let err = std::io::Error::last_os_error();
            match err.raw_os_error() {
                Some(e) if e == libc::EAGAIN || e == libc::EWOULDBLOCK => {
                    Ok(NonblockRead::WouldBlock)
                }
                _ => Err(MonorubyErr::from_io_err(store, &err, "read_nonblock".to_string())),
            }
        }
    }

    /// `IO#write_nonblock` core: a single non-blocking `write(2)`.
    /// Reports `WouldBlock` on `EAGAIN`/`EWOULDBLOCK`; a hard error
    /// (e.g. `EPIPE`) is surfaced as the matching `Errno` exception.
    pub fn write_nonblock(&mut self, bytes: &[u8], store: &Store) -> Result<NonblockWrite> {
        if !self.is_writable() {
            return Err(MonorubyErr::ioerr("not opened for writing"));
        }
        let fd = self.fileno()?;
        self.set_nonblock()?;
        // SAFETY: fd is valid; bytes is a valid buffer of `bytes.len()`.
        let n = unsafe {
            libc::write(fd, bytes.as_ptr() as *const libc::c_void, bytes.len())
        };
        if n >= 0 {
            Ok(NonblockWrite::Written(n as usize))
        } else {
            let err = std::io::Error::last_os_error();
            match err.raw_os_error() {
                Some(e) if e == libc::EAGAIN || e == libc::EWOULDBLOCK => {
                    Ok(NonblockWrite::WouldBlock)
                }
                _ => Err(MonorubyErr::from_io_err(store, &err, "write_nonblock".to_string())),
            }
        }
    }

    /// `IO#readpartial` core: return up to `maxlen` bytes, blocking
    /// only when no data is buffered or available yet. Unlike
    /// `sysread` this reads *through* the `BufReader` (so already-
    /// buffered bytes are returned) and unlike `read` it never blocks
    /// to fill the whole `maxlen`. An empty result signals EOF.
    pub fn readpartial(&mut self, maxlen: usize) -> Result<Vec<u8>> {
        self.flush_wbuf_before_read()?;
        // Drain ungetc pushback first. When pushback supplied any
        // bytes, we must not block for more — only append bytes that
        // are *already* buffered (CRuby returns the available data).
        let had_pushback = self.pushback_len() > 0;
        let mut out = if had_pushback {
            self.take_pushback(Some(maxlen))
        } else {
            vec![]
        };
        if out.len() >= maxlen {
            return Ok(out);
        }
        let need = maxlen - out.len();
        match &mut self.kind {
            IoKind::Closed(..) => return Err(MonorubyErr::ioerr("closed stream")),
            IoKind::Stdout | IoKind::Stderr => {
                return Err(MonorubyErr::ioerr("not opened for reading"));
            }
            IoKind::Stdin => {
                if !had_pushback {
                    let chunk = read_partial_chunk(&mut stdin_buf(), need, false)?;
                    out.extend(chunk);
                }
            }
            IoKind::File(file) => {
                if !file.readable {
                    return Err(MonorubyErr::ioerr("not opened for reading"));
                }
                let reader = &mut *Rc::get_mut(file).unwrap().reader;
                let chunk = read_partial_chunk(reader, need, had_pushback)?;
                out.extend(chunk);
            }
            IoKind::Popen(popen) => {
                let popen = Rc::get_mut(popen).unwrap();
                let reader = popen
                    .reader
                    .as_mut()
                    .ok_or_else(|| MonorubyErr::ioerr("not opened for reading"))?;
                let chunk = read_partial_chunk(reader, need, had_pushback)?;
                out.extend(chunk);
            }
        }
        Ok(out)
    }

    pub fn read_line(&mut self) -> Result<Option<String>> {
        Ok(self
            .read_line_bytes()?
            .map(|v| String::from_utf8_lossy(&v).into_owned()))
    }

    /// Read one `\n`-terminated line as raw bytes (pushback-aware).
    pub fn read_line_bytes(&mut self) -> Result<Option<Vec<u8>>> {
        self.flush_wbuf_before_read()?;
        if self.pushback_len() > 0 {
            let cell = self.pushback_cell().unwrap();
            let nl = cell.borrow().iter().position(|&b| b == b'\n');
            match nl {
                Some(idx) => {
                    return Ok(Some(self.take_pushback(Some(idx + 1))));
                }
                None => {
                    let mut line = self.take_pushback(None);
                    if let Some(rest) = self.read_line_bytes_underlying()? {
                        line.extend_from_slice(&rest);
                    }
                    // `line` is non-empty: the pushback supplied at least
                    // one byte.
                    return Ok(Some(line));
                }
            }
        }
        self.read_line_bytes_underlying()
    }

    fn read_line_bytes_underlying(&mut self) -> Result<Option<Vec<u8>>> {
        let mut buf = Vec::new();
        // See `read_underlying`: on a restartable interrupt, push already-
        // read bytes back so the retried getline sees them first.
        let interrupted = |kind: std::io::ErrorKind,
                           partial: Vec<u8>,
                           pushback: Option<&RefCell<Vec<u8>>>|
         -> MonorubyErr {
            if !partial.is_empty()
                && let Some(cell) = pushback
            {
                cell.borrow_mut().splice(0..0, partial);
            }
            interrupt_marker(kind)
        };
        let size = match &mut self.kind {
            IoKind::Closed(..) => return Err(MonorubyErr::ioerr("closed stream")),
            IoKind::Stdin => read_until_step(&mut *stdin_buf(), b'\n', &mut buf)
                .map_err(|e| map_read_err(e, MonorubyErr::runtimeerr))?,
            IoKind::Stdout => return Err(MonorubyErr::argumenterr("can't read from $stdin")),
            IoKind::Stderr => return Err(MonorubyErr::argumenterr("can't read from $stderr")),
            IoKind::File(file) => {
                if !file.readable {
                    return Err(MonorubyErr::ioerr("not opened for reading"));
                }
                let file = Rc::get_mut(file).unwrap();
                match read_until_step(&mut *file.reader, b'\n', &mut buf) {
                    Ok(n) => n,
                    Err(e) if is_interrupt_kind(e.kind()) => {
                        return Err(interrupted(e.kind(), buf, Some(&file.pushback)));
                    }
                    Err(e) => return Err(MonorubyErr::runtimeerr(e.to_string())),
                }
            }
            IoKind::Popen(popen) => {
                let popen = Rc::get_mut(popen).unwrap();
                let reader = popen
                    .reader
                    .as_mut()
                    .ok_or_else(|| MonorubyErr::ioerr("not opened for reading"))?;
                match read_until_step(reader, b'\n', &mut buf) {
                    Ok(n) => n,
                    Err(e) if is_interrupt_kind(e.kind()) => {
                        return Err(interrupted(e.kind(), buf, Some(&popen.pushback)));
                    }
                    Err(e) => return Err(MonorubyErr::ioerr(e.to_string())),
                }
            }
        };
        if size == 0 {
            return Ok(None);
        }
        Ok(Some(buf))
    }

    /// Read one byte (pushback-aware). `None` at EOF.
    fn read1(&mut self) -> Result<Option<u8>> {
        Ok(self.read(Some(1))?.first().copied())
    }

    /// `read1` for the getline accumulation loops: on a restartable
    /// interrupt (signal or would-block), push the bytes accumulated so far
    /// (`acc`) back into the pushback buffer so the getline retried after a
    /// `Signal.trap` handler / fd-readiness park re-reads them and no data
    /// is lost.
    fn read1_preserving(&mut self, acc: &[u8]) -> Result<Option<u8>> {
        match self.read1() {
            Err(err) => {
                if (err.is_signal_interrupt() || err.is_would_block_interrupt())
                    && !acc.is_empty()
                {
                    let _ = self.unget(acc);
                }
                Err(err)
            }
            ok => ok,
        }
    }

    /// General line reader implementing CRuby's `IO#gets` semantics:
    ///
    /// - `sep == None` — slurp the rest of the stream (up to `limit`).
    /// - `sep == Some(b"")` — paragraph mode: skip blank lines, then read
    ///   up to and including the `"\n\n"` that ends the paragraph.
    /// - otherwise — read up to and including `sep` (multi-byte separators
    ///   are matched even across buffer refills).
    ///
    /// `limit` caps the number of bytes read; when the cap cuts a UTF-8
    /// character in half and `complete_utf8` is set, up to 16 extra bytes
    /// are read to finish that character (CRuby reads on to the character
    /// boundary of the external encoding).
    ///
    /// Returns `None` at EOF (except `limit == Some(0)`, which returns an
    /// empty line without consuming anything, like CRuby).
    pub fn getline(
        &mut self,
        sep: Option<&[u8]>,
        limit: Option<usize>,
        complete_utf8: bool,
    ) -> Result<Option<Vec<u8>>> {
        if limit == Some(0) {
            return Ok(Some(Vec::new()));
        }
        match sep {
            None => {
                let mut buf = self.read(limit)?;
                if buf.is_empty() {
                    return Ok(None);
                }
                if complete_utf8 && limit == Some(buf.len()) {
                    self.complete_partial_char(&mut buf)?;
                }
                Ok(Some(buf))
            }
            Some([]) => {
                // Paragraph mode: skip the blank lines between paragraphs.
                let mut buf = Vec::new();
                loop {
                    match self.read1_preserving(&buf)? {
                        None => return Ok(None),
                        Some(b'\n') => continue,
                        Some(b) => {
                            buf.push(b);
                            break;
                        }
                    }
                }
                let mut at_sep = false;
                loop {
                    if let Some(l) = limit
                        && buf.len() >= l
                    {
                        if complete_utf8 {
                            self.complete_partial_char(&mut buf)?;
                        }
                        break;
                    }
                    match self.read1_preserving(&buf)? {
                        None => break,
                        Some(b) => {
                            buf.push(b);
                            if buf.ends_with(b"\n\n") {
                                at_sep = true;
                                break;
                            }
                        }
                    }
                }
                if at_sep {
                    // Swallow any further blank lines so the stream is
                    // positioned at the start of the next paragraph
                    // (CRuby's swallow(io, '\n')).
                    loop {
                        match self.read1_preserving(&buf)? {
                            Some(b'\n') => continue,
                            Some(b) => {
                                let _ = self.unget(&[b]);
                                break;
                            }
                            None => break,
                        }
                    }
                }
                Ok(Some(buf))
            }
            Some(s) => {
                // Fast path for the default record separator.
                if s == b"\n" && limit.is_none() {
                    return self.read_line_bytes();
                }
                let mut buf = Vec::new();
                loop {
                    if let Some(l) = limit
                        && buf.len() >= l
                    {
                        if complete_utf8 {
                            self.complete_partial_char(&mut buf)?;
                        }
                        break;
                    }
                    match self.read1_preserving(&buf)? {
                        None => break,
                        Some(b) => {
                            buf.push(b);
                            if buf.ends_with(s) {
                                break;
                            }
                        }
                    }
                }
                if buf.is_empty() {
                    Ok(None)
                } else {
                    Ok(Some(buf))
                }
            }
        }
    }

    /// After a limit cut, read up to 16 extra bytes to complete a UTF-8
    /// character the cut may have split. Like CRuby, the scan keeps
    /// consuming while the tail still looks like an unfinished character
    /// — re-anchoring on each new lead byte — so a run of invalid
    /// "lead, continuation, lead, …" bytes is consumed up to the 16-byte
    /// cap rather than stopping at the first invalid boundary.
    fn complete_partial_char(&mut self, buf: &mut Vec<u8>) -> Result<()> {
        for _ in 0..16 {
            if utf8_missing_bytes(buf) == 0 {
                return Ok(());
            }
            match self.read1_preserving(buf)? {
                None => return Ok(()),
                Some(b) => buf.push(b),
            }
        }
        Ok(())
    }

    pub fn fileno(&self) -> Result<i32> {
        match &self.kind {
            IoKind::Stdin => Ok(0),
            IoKind::Stdout => Ok(1),
            IoKind::Stderr => Ok(2),
            IoKind::File(file) => Ok(file.reader.get_ref().as_raw_fd()),
            IoKind::Popen(popen) => {
                if let Some(ref stdout) = popen.child.stdout {
                    Ok(stdout.as_raw_fd())
                } else if let Some(ref reader) = popen.reader {
                    Ok(reader.get_ref().as_raw_fd())
                } else {
                    Err(MonorubyErr::ioerr("closed stream"))
                }
            }
            IoKind::Closed(..) => Err(MonorubyErr::ioerr("closed stream")),
        }
    }

    /// The fd a poll for `events` actually applies to: like [`Self::fileno`],
    /// except that a `POLLOUT` wait on a `Popen` resolves to the child's
    /// stdin (the pipe end this process writes) rather than the read side
    /// `fileno` reports.
    pub fn wait_fd_for(&self, events: i16) -> Result<i32> {
        if events & libc::POLLOUT != 0
            && let IoKind::Popen(popen) = &self.kind
        {
            if let Some(ref stdin) = popen.child.stdin {
                return Ok(stdin.as_raw_fd());
            }
            if let Some(ref writer) = popen.writer {
                return Ok(writer.get_ref().as_raw_fd());
            }
        }
        self.fileno()
    }

    /// Seek the underlying file. `whence` follows POSIX: 0 = SEEK_SET,
    /// 1 = SEEK_CUR, 2 = SEEK_END. Returns the new absolute position.
    /// Fails with `ESPIPE` for streams that do not support seeking
    /// (stdin/stdout/stderr, pipes) and with `EINVAL` for unsupported
    /// `whence` values or a negative `SEEK_SET` offset.
    pub fn seek(&mut self, offset: i64, whence: i32) -> std::io::Result<u64> {
        const EINVAL: i32 = 22;
        const ESPIPE: i32 = 29;
        const EINTR: i32 = 4;
        let seek_from = match whence {
            0 => {
                if offset < 0 {
                    return Err(std::io::Error::from_raw_os_error(EINVAL));
                }
                SeekFrom::Start(offset as u64)
            }
            1 => SeekFrom::Current(offset),
            2 => SeekFrom::End(offset),
            _ => return Err(std::io::Error::from_raw_os_error(EINVAL)),
        };
        match &mut self.kind {
            IoKind::File(file) => {
                // A pending write has to land before the offset moves,
                // or it would be written at the *new* position.
                if let Err(e) = file.drain_wbuf() {
                    return Err(match e {
                        DrainErr::Io(e) => e,
                        _ => std::io::Error::from_raw_os_error(EINTR),
                    });
                }
                Rc::get_mut(file).unwrap().reader.seek(seek_from)
            }
            IoKind::Closed(..) => Err(std::io::Error::from_raw_os_error(9)), // EBADF
            _ => Err(std::io::Error::from_raw_os_error(ESPIPE)),
        }
    }

    pub fn isatty(&self) -> bool {
        match &self.kind {
            IoKind::Stdin => stdin_buf().get_ref().is_terminal(),
            IoKind::Stdout => stdout_buf().get_ref().is_terminal(),
            IoKind::Stderr => stderr_buf().get_ref().is_terminal(),
            IoKind::File(_) | IoKind::Popen(_) | IoKind::Closed(..) => false,
        }
    }

    /// Returns the file name/path if this is a File IO, None otherwise.
    pub fn name(&self) -> Option<&str> {
        match &self.kind {
            IoKind::File(file) => Some(&file.name),
            _ => None,
        }
    }

    /// CRuby `IO#path` / `IO#to_path`. Returns the pseudo-path for the
    /// standard streams, the real filesystem path for file-backed IO (and
    /// raw-fd IO opened with an explicit `path:`), and `nil` for pipes,
    /// `popen`, raw fds without a path, and closed streams.
    pub fn path(&self) -> Option<String> {
        match &self.kind {
            IoKind::Stdin => Some("<STDIN>".to_string()),
            IoKind::Stdout => Some("<STDOUT>".to_string()),
            IoKind::Stderr => Some("<STDERR>".to_string()),
            IoKind::File(file) if file.has_path => Some(file.name.clone()),
            IoKind::Closed(p) => p.as_deref().cloned(),
            IoKind::File(_) | IoKind::Popen(_) => None,
        }
    }

    /// The exact path bytes + encoding as passed at open time, when the
    /// stream was opened from a path (`IO#path` preserves the argument's
    /// encoding — core/file/to_path_spec.rb).
    pub fn path_raw(&self) -> Option<&(Vec<u8>, crate::value::Encoding)> {
        match &self.kind {
            IoKind::File(file) if file.has_path => file.path_raw.as_ref(),
            _ => None,
        }
    }

    /// CRuby `IO#fsync` / `IO#fdatasync`. Flushes user-space buffers, then
    /// asks the kernel to flush to permanent storage. `data_only` selects
    /// `fdatasync(2)` (skip metadata) over `fsync(2)`. Returns `0` on
    /// success (matching CRuby), `IOError` on a closed stream.
    pub fn fsync(&mut self, data_only: bool, store: &Store) -> Result<i32> {
        self.flush(store)?;
        let fd = self.fileno()?;
        // `fdatasync(2)` is Linux/POSIX-realtime; macOS doesn't ship it
        // (the closest equivalent is `fcntl(fd, F_FULLFSYNC)`, which is
        // stronger than fsync). For build-portability on non-Linux hosts
        // we fall back to plain `fsync` — same semantics on the "skip
        // metadata flush" optimization is just not available.
        let ret = unsafe {
            if data_only {
                #[cfg(target_os = "linux")]
                {
                    libc::fdatasync(fd)
                }
                #[cfg(not(target_os = "linux"))]
                {
                    libc::fsync(fd)
                }
            } else {
                libc::fsync(fd)
            }
        };
        if ret == -1 {
            let err = std::io::Error::last_os_error();
            return Err(MonorubyErr::ioerr(err.to_string()));
        }
        Ok(0)
    }

    /// CRuby `IO#close_on_exec?`. Reads the `FD_CLOEXEC` flag via
    /// `fcntl(F_GETFD)`. `IOError` on a closed stream.
    pub fn close_on_exec(&self) -> Result<bool> {
        let fd = self.fileno()?;
        let flags = unsafe { libc::fcntl(fd, libc::F_GETFD) };
        if flags == -1 {
            let err = std::io::Error::last_os_error();
            return Err(MonorubyErr::ioerr(err.to_string()));
        }
        Ok(flags & libc::FD_CLOEXEC != 0)
    }

    /// CRuby `IO#close_on_exec=`. Sets/clears `FD_CLOEXEC` via
    /// `fcntl(F_GETFD)`/`fcntl(F_SETFD)`. `IOError` on a closed stream.
    pub fn set_close_on_exec(&self, value: bool) -> Result<()> {
        let fd = self.fileno()?;
        let flags = unsafe { libc::fcntl(fd, libc::F_GETFD) };
        if flags == -1 {
            let err = std::io::Error::last_os_error();
            return Err(MonorubyErr::ioerr(err.to_string()));
        }
        let new_flags = if value {
            flags | libc::FD_CLOEXEC
        } else {
            flags & !libc::FD_CLOEXEC
        };
        if unsafe { libc::fcntl(fd, libc::F_SETFD, new_flags) } == -1 {
            let err = std::io::Error::last_os_error();
            return Err(MonorubyErr::ioerr(err.to_string()));
        }
        Ok(())
    }

    /// Set the autoclose flag for a File IO. No-op for stdio/pipe/popen/closed
    /// because their fd lifetime is not owned by this `IoInner`. Keeps the
    /// `OWNED_FDS` set in sync: enabling autoclose makes this descriptor the
    /// fd's owner, disabling it relinquishes ownership.
    pub fn set_autoclose(&self, value: bool) {
        if let IoKind::File(file) = &self.kind {
            let prev = file.autoclose.get();
            if prev != value {
                file.autoclose.set(value);
                let fd = file.reader.get_ref().as_raw_fd();
                if value {
                    register_owned_fd(fd);
                } else {
                    unregister_owned_fd(fd);
                }
            }
        }
    }

    /// Read the autoclose flag. Always `true` for variants whose fd is owned
    /// elsewhere (stdio inherits the process fd, popen owns its own ends).
    pub fn is_autoclose(&self) -> bool {
        match &self.kind {
            IoKind::File(file) => file.autoclose.get(),
            _ => true,
        }
    }
}



