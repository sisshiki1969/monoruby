use std::{
    cell::{Cell, RefCell},
    collections::HashSet,
    io::{BufRead, IsTerminal, Read, Seek, SeekFrom, Write},
    mem::ManuallyDrop,
    os::fd::{AsRawFd, FromRawFd, IntoRawFd},
    os::unix::process::ExitStatusExt,
    rc::Rc,
};

use super::*;

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

/// Signal-interruptible replacement for `bytes().take(len).collect()`:
/// append up to `len` bytes to `out`, stopping early at EOF. On
/// `Interrupted`, bytes read so far remain in `out` so the caller can
/// preserve them (pushback) before surfacing the interrupt.
fn read_upto(reader: &mut impl Read, len: usize, out: &mut Vec<u8>) -> std::io::Result<()> {
    let mut buf = [0u8; 8192];
    while out.len() < len {
        // A signal delivered while we were in userspace (e.g. right after
        // the previous chunk) sets the pending bit without EINTR-ing
        // anything; entering a blocking read with the bit already set
        // would block unkillably. Check before every kernel entry.
        if signal_pending() {
            return Err(std::io::ErrorKind::Interrupted.into());
        }
        let want = (len - out.len()).min(buf.len());
        match read_step(reader, &mut buf[..want])? {
            0 => break,
            n => out.extend_from_slice(&buf[..n]),
        }
    }
    Ok(())
}

/// Signal-interruptible replacement for `read_to_end`. On `Interrupted`,
/// bytes read so far remain in `out`.
fn read_all(reader: &mut impl Read, out: &mut Vec<u8>) -> std::io::Result<()> {
    let mut buf = [0u8; 8192];
    loop {
        // See `read_upto` on why this is checked before every kernel entry.
        if signal_pending() {
            return Err(std::io::ErrorKind::Interrupted.into());
        }
        match read_step(reader, &mut buf)? {
            0 => return Ok(()),
            n => out.extend_from_slice(&buf[..n]),
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
    reader: &mut std::io::BufReader<T>,
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
    reader: ManuallyDrop<std::io::BufReader<std::fs::File>>,
    name: String,
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
}

impl Drop for FileDescriptor {
    fn drop(&mut self) {
        let fd = self.reader.get_ref().as_raw_fd();
        // SAFETY: `reader` is wrapped in `ManuallyDrop` and is only taken
        // here, exactly once, in `Drop`. After this, `self.reader` must not
        // be accessed.
        let reader = unsafe { ManuallyDrop::take(&mut self.reader) };
        if self.autoclose.get() {
            // Normal case: dropping the `BufReader<File>` closes the fd via
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
    pub(crate) reader: Option<std::io::BufReader<std::process::ChildStdout>>,
    pub(crate) writer: Option<std::process::ChildStdin>,
    /// See `FileDescriptor::pushback`.
    pushback: RefCell<Vec<u8>>,
}

#[derive(Debug)]
pub enum IoInner {
    Stdin,
    Stdout,
    Stderr,
    File(Rc<FileDescriptor>),
    Popen(Rc<PopenDescriptor>),
    /// Closed stream. Retains the filesystem path of a path-backed File
    /// (CRuby keeps `File#path` readable after close; tempfile.rb relies
    /// on `File.unlink(file.path)` from its cleanup/finalizer paths).
    Closed(Option<String>),
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

impl std::clone::Clone for IoInner {
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

impl std::fmt::Display for IoInner {
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
    pub fn flush(&mut self) -> Result<()> {
        let res = match self {
            Self::Stdin => return Ok(()),
            Self::Stdout => std::io::stdout().flush(),
            Self::Stderr => std::io::stderr().flush(),
            Self::File(file) => file.reader.get_ref().flush(),
            Self::Popen(popen) => {
                let popen = Rc::get_mut(popen).unwrap();
                if let Some(ref mut writer) = popen.writer {
                    writer
                        .flush()
                        .map_err(|e| MonorubyErr::ioerr(e.to_string()))?;
                }
                return Ok(());
            }
            Self::Closed(..) => return Err(MonorubyErr::ioerr("closed stream")),
        };
        res.map_err(|err| MonorubyErr::runtimeerr(err.to_string()))
    }

    pub fn is_closed(&self) -> bool {
        matches!(self, Self::Closed(..))
    }

    /// Whether the stream may be read from.
    pub fn is_readable(&self) -> bool {
        match self {
            Self::Stdin => true,
            Self::Stdout | Self::Stderr | Self::Closed(..) => false,
            Self::File(f) => f.readable,
            Self::Popen(p) => p.reader.is_some(),
        }
    }

    /// Whether the stream may be written to.
    pub fn is_writable(&self) -> bool {
        match self {
            Self::Stdout | Self::Stderr => true,
            Self::Stdin | Self::Closed(..) => false,
            Self::File(f) => f.writable,
            Self::Popen(p) => p.writer.is_some(),
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
    pub fn close(&mut self) -> Result<Option<(i32, u32)>> {
        if self.is_closed() {
            return Err(MonorubyErr::ioerr("closed stream"));
        }
        let popen_result = if let Self::Popen(popen) = self {
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
        let retained = match &*self {
            Self::File(file) if file.has_path => Some(file.name.clone()),
            _ => None,
        };
        *self = Self::Closed(retained);
        Ok(popen_result)
    }

    pub(super) fn stdin() -> Self {
        Self::Stdin
    }

    pub(super) fn stdout() -> Self {
        Self::Stdout
    }

    pub(super) fn stderr() -> Self {
        Self::Stderr
    }

    pub(super) fn file(file: std::fs::File, name: String, readable: bool, writable: bool) -> Self {
        register_owned_fd(file.as_raw_fd());
        Self::File(Rc::new(FileDescriptor {
            reader: ManuallyDrop::new(std::io::BufReader::new(file)),
            name,
            has_path: true,
            readable,
            writable,
            autoclose: Cell::new(true),
            pushback: RefCell::new(Vec::new()),
        }))
    }

    /// Wrap a socket fd (connected stream or listener). Like [`Self::file`]
    /// but with no filesystem path (`IO#path` is nil for sockets) and
    /// always opened read/write; `name` only feeds `Display`/inspect.
    pub(super) fn socket(file: std::fs::File, name: String) -> Self {
        register_owned_fd(file.as_raw_fd());
        Self::File(Rc::new(FileDescriptor {
            reader: ManuallyDrop::new(std::io::BufReader::new(file)),
            name,
            has_path: false,
            readable: true,
            writable: true,
            autoclose: Cell::new(true),
            pushback: RefCell::new(Vec::new()),
        }))
    }

    pub(crate) fn popen(mut child: std::process::Child) -> Self {
        let reader = child.stdout.take().map(std::io::BufReader::new);
        let writer = child.stdin.take();
        Self::Popen(Rc::new(PopenDescriptor {
            child,
            reader,
            writer,
            pushback: RefCell::new(Vec::new()),
        }))
    }

    pub(crate) fn pid(&self) -> Option<u32> {
        match self {
            Self::Popen(popen) => Some(popen.child.id()),
            _ => None,
        }
    }

    pub(crate) fn from_raw_fd(
        fd: i32,
        name: String,
        has_path: bool,
        readable: bool,
        writable: bool,
    ) -> Self {
        Self::from_raw_fd_autoclose(fd, name, has_path, readable, writable, true)
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
        if autoclose {
            register_owned_fd(fd);
        }
        Self::File(Rc::new(FileDescriptor {
            reader: ManuallyDrop::new(std::io::BufReader::new(file)),
            name,
            has_path,
            readable,
            writable,
            autoclose: Cell::new(autoclose),
            pushback: RefCell::new(Vec::new()),
        }))
    }

    /// Write all of `data[*progress..]`, advancing `*progress` past every
    /// byte accepted by the kernel (fixing silent short writes on pipes).
    ///
    /// Signal-interruptible: a bare `EINTR` is retried, while `EINTR`
    /// with a pending signal surfaces the internal signal-interrupt
    /// marker. Because `*progress` records exactly what was flushed, the
    /// builtin's `blocking_region` retry after a `Signal.trap` handler
    /// resumes mid-buffer without duplicating output.
    pub fn write(&mut self, data: &[u8], progress: &mut usize) -> Result<()> {
        self.ensure_writable()?;
        fn write_all(
            writer: &mut impl Write,
            data: &[u8],
            progress: &mut usize,
            map: impl Fn(String) -> MonorubyErr,
        ) -> Result<()> {
            while *progress < data.len() {
                // A blocking pipe write that already transferred bytes
                // returns the partial count on a signal instead of EINTR,
                // and a signal can also land while we are in userspace
                // between chunks; either way the pending bit is set and
                // re-entering write(2) would block unkillably. Check
                // before every kernel entry.
                if signal_pending() {
                    return Err(MonorubyErr::signal_interrupt());
                }
                match writer.write(&data[*progress..]) {
                    Ok(0) => return Err(map("write returned 0".to_string())),
                    Ok(n) => *progress += n,
                    Err(e) if e.kind() == std::io::ErrorKind::Interrupted => {
                        if signal_pending() {
                            return Err(MonorubyErr::signal_interrupt());
                        }
                    }
                    // EAGAIN on an fd the green-thread scheduler put in
                    // non-blocking mode: `*progress` records what was
                    // flushed, so the restart after the fd-readiness park
                    // resumes mid-buffer without duplicating output.
                    Err(e) if e.kind() == std::io::ErrorKind::WouldBlock => {
                        return Err(MonorubyErr::would_block_interrupt());
                    }
                    Err(e) => return Err(map(e.to_string())),
                }
            }
            Ok(())
        }
        match self {
            Self::Stdout => write_all(
                &mut std::io::stdout(),
                data,
                progress,
                MonorubyErr::rangeerr,
            ),
            Self::Stderr => write_all(
                &mut std::io::stderr(),
                data,
                progress,
                MonorubyErr::rangeerr,
            ),
            Self::File(file) => write_all(
                Rc::get_mut(file).unwrap().reader.get_mut(),
                data,
                progress,
                MonorubyErr::rangeerr,
            ),
            Self::Popen(popen) => {
                let popen = Rc::get_mut(popen).unwrap();
                // `ensure_writable` guaranteed the writer is present.
                let writer = popen.writer.as_mut().unwrap();
                write_all(writer, data, progress, MonorubyErr::ioerr)
            }
            // `ensure_writable` already rejected non-writable streams.
            Self::Stdin | Self::Closed(..) => unreachable!(),
        }
    }

    /// Whether a read can be satisfied without touching the fd: ungetc
    /// pushback or bytes already sitting in the internal BufReader. Used
    /// by the green-thread IO scheduler to skip the fd-readiness park.
    pub fn has_buffered_data(&self) -> bool {
        if self.pushback_len() > 0 {
            return true;
        }
        match self {
            Self::File(f) => !f.reader.buffer().is_empty(),
            Self::Popen(p) => p
                .reader
                .as_ref()
                .map(|r| !r.buffer().is_empty())
                .unwrap_or(false),
            _ => false,
        }
    }

    /// Bytes currently sitting in the `ungetc`/`ungetbyte` pushback buffer.
    pub fn pushback_len(&self) -> usize {
        match self {
            Self::File(f) => f.pushback.borrow().len(),
            Self::Popen(p) => p.pushback.borrow().len(),
            _ => 0,
        }
    }

    fn pushback_cell(&self) -> Option<&RefCell<Vec<u8>>> {
        match self {
            Self::File(f) => Some(&f.pushback),
            Self::Popen(p) => Some(&p.pushback),
            _ => None,
        }
    }

    /// Push `bytes` back so the next read returns them first. CRuby raises
    /// `IOError` on closed streams and on streams not opened for reading
    /// (`STDOUT`/`STDERR`). Each call splices at the front, so successive
    /// ungets behave LIFO while a single multi-byte unget preserves order.
    pub fn unget(&mut self, bytes: &[u8]) -> Result<()> {
        match self {
            Self::Closed(..) => Err(MonorubyErr::ioerr("closed stream")),
            Self::Stdin | Self::Stdout | Self::Stderr => {
                Err(MonorubyErr::ioerr("not opened for reading"))
            }
            Self::File(f) if !f.readable => {
                Err(MonorubyErr::ioerr("not opened for reading"))
            }
            Self::File(_) | Self::Popen(_) => {
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
        if self.pushback_len() > 0 {
            match length {
                Some(0) => return Ok(vec![]),
                Some(n) if n <= self.pushback_len() => {
                    return Ok(self.take_pushback(Some(n)));
                }
                Some(n) => {
                    let out = self.take_pushback(None);
                    let need = n - out.len();
                    return self.read_more_preserving(out, Some(need));
                }
                None => {
                    let out = self.take_pushback(None);
                    return self.read_more_preserving(out, None);
                }
            }
        }
        self.read_underlying(length)
    }

    /// Extend already-drained pushback bytes (`out`) with an underlying
    /// read. On a restartable interrupt out of the read, `out` must go
    /// back into the pushback buffer before the marker propagates — the
    /// caller retries the whole operation and would otherwise lose those
    /// bytes. (`read_underlying` has already pushed back its own partial
    /// data at that point; ungetting `out` afterwards splices it in front,
    /// preserving stream order.)
    fn read_more_preserving(&mut self, mut out: Vec<u8>, length: Option<usize>) -> Result<Vec<u8>> {
        match self.read_underlying(length) {
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

    fn read_underlying(&mut self, length: Option<usize>) -> Result<Vec<u8>> {
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
        match self {
            Self::Closed(..) => return Err(MonorubyErr::ioerr("closed stream")),
            Self::Stdin => {
                let mut buf = vec![];
                let res = if let Some(length) = length {
                    read_upto(&mut std::io::stdin(), length, &mut buf)
                } else {
                    read_all(&mut std::io::stdin(), &mut buf)
                };
                match res {
                    Ok(()) => Ok(buf),
                    Err(e) => Err(map_read_err(e, MonorubyErr::runtimeerr)),
                }
            }
            Self::Stdout => Err(MonorubyErr::argumenterr("can't read from $stdin")),
            Self::Stderr => Err(MonorubyErr::argumenterr("can't read from $stderr")),
            Self::File(file) => {
                if !file.readable {
                    return Err(MonorubyErr::ioerr("not opened for reading"));
                }
                let file = Rc::get_mut(file).unwrap();
                let reader = &mut *file.reader;
                let mut buf = vec![];
                let res = if let Some(length) = length {
                    read_upto(reader, length, &mut buf)
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
            Self::Popen(popen) => {
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
        let chunk = match self {
            Self::Closed(..) => return Err(MonorubyErr::ioerr("closed stream")),
            Self::Stdout | Self::Stderr => {
                return Err(MonorubyErr::ioerr("not opened for reading"));
            }
            Self::Stdin => {
                let mut buf = vec![0u8; need];
                let n = read_step(&mut std::io::stdin(), &mut buf)
                    .map_err(|e| map_read_err(e, MonorubyErr::runtimeerr))?;
                buf.truncate(n);
                buf
            }
            Self::File(file) => {
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
            Self::Popen(popen) => {
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
        if self.pushback_len() > 0 {
            return Ok(NonblockRead::Data(self.take_pushback(Some(maxlen))));
        }
        if !self.is_readable() {
            return Err(MonorubyErr::ioerr("not opened for reading"));
        }
        // Best-effort: sync a seekable File's BufReader to its logical
        // position (discarding its buffer) so the raw read below is at
        // the right offset; pipes/sockets aren't seekable and skip it.
        if let Self::File(file) = self {
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
        match self {
            Self::Closed(..) => return Err(MonorubyErr::ioerr("closed stream")),
            Self::Stdout | Self::Stderr => {
                return Err(MonorubyErr::ioerr("not opened for reading"));
            }
            Self::Stdin => {
                if !had_pushback {
                    let mut buf = vec![0u8; need];
                    let n = std::io::stdin()
                        .read(&mut buf)
                        .map_err(|e| MonorubyErr::ioerr(e.to_string()))?;
                    buf.truncate(n);
                    out.extend(buf);
                }
            }
            Self::File(file) => {
                if !file.readable {
                    return Err(MonorubyErr::ioerr("not opened for reading"));
                }
                let reader = &mut *Rc::get_mut(file).unwrap().reader;
                let chunk = read_partial_chunk(reader, need, had_pushback)?;
                out.extend(chunk);
            }
            Self::Popen(popen) => {
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
        let size = match self {
            Self::Closed(..) => return Err(MonorubyErr::ioerr("closed stream")),
            Self::Stdin => read_until_step(&mut std::io::stdin().lock(), b'\n', &mut buf)
                .map_err(|e| map_read_err(e, MonorubyErr::runtimeerr))?,
            Self::Stdout => return Err(MonorubyErr::argumenterr("can't read from $stdin")),
            Self::Stderr => return Err(MonorubyErr::argumenterr("can't read from $stderr")),
            Self::File(file) => {
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
            Self::Popen(popen) => {
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
        match self {
            Self::Stdin => Ok(0),
            Self::Stdout => Ok(1),
            Self::Stderr => Ok(2),
            Self::File(file) => Ok(file.reader.get_ref().as_raw_fd()),
            Self::Popen(popen) => {
                if let Some(ref stdout) = popen.child.stdout {
                    Ok(stdout.as_raw_fd())
                } else if let Some(ref reader) = popen.reader {
                    Ok(reader.get_ref().as_raw_fd())
                } else {
                    Err(MonorubyErr::ioerr("closed stream"))
                }
            }
            Self::Closed(..) => Err(MonorubyErr::ioerr("closed stream")),
        }
    }

    /// The fd a poll for `events` actually applies to: like [`Self::fileno`],
    /// except that a `POLLOUT` wait on a `Popen` resolves to the child's
    /// stdin (the pipe end this process writes) rather than the read side
    /// `fileno` reports.
    pub fn wait_fd_for(&self, events: i16) -> Result<i32> {
        if events & libc::POLLOUT != 0
            && let Self::Popen(popen) = self
        {
            if let Some(ref stdin) = popen.child.stdin {
                return Ok(stdin.as_raw_fd());
            }
            if let Some(ref writer) = popen.writer {
                return Ok(writer.as_raw_fd());
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
        match self {
            Self::File(file) => Rc::get_mut(file).unwrap().reader.seek(seek_from),
            Self::Closed(..) => Err(std::io::Error::from_raw_os_error(9)), // EBADF
            _ => Err(std::io::Error::from_raw_os_error(ESPIPE)),
        }
    }

    pub fn isatty(&self) -> bool {
        match self {
            Self::Stdin => std::io::stdin().is_terminal(),
            Self::Stdout => std::io::stdout().is_terminal(),
            Self::Stderr => std::io::stderr().is_terminal(),
            Self::File(_) | Self::Popen(_) | Self::Closed(..) => false,
        }
    }

    /// Returns the file name/path if this is a File IO, None otherwise.
    pub fn name(&self) -> Option<&str> {
        match self {
            Self::File(file) => Some(&file.name),
            _ => None,
        }
    }

    /// CRuby `IO#path` / `IO#to_path`. Returns the pseudo-path for the
    /// standard streams, the real filesystem path for file-backed IO (and
    /// raw-fd IO opened with an explicit `path:`), and `nil` for pipes,
    /// `popen`, raw fds without a path, and closed streams.
    pub fn path(&self) -> Option<String> {
        match self {
            Self::Stdin => Some("<STDIN>".to_string()),
            Self::Stdout => Some("<STDOUT>".to_string()),
            Self::Stderr => Some("<STDERR>".to_string()),
            Self::File(file) if file.has_path => Some(file.name.clone()),
            Self::Closed(p) => p.clone(),
            Self::File(_) | Self::Popen(_) => None,
        }
    }

    /// CRuby `IO#fsync` / `IO#fdatasync`. Flushes user-space buffers, then
    /// asks the kernel to flush to permanent storage. `data_only` selects
    /// `fdatasync(2)` (skip metadata) over `fsync(2)`. Returns `0` on
    /// success (matching CRuby), `IOError` on a closed stream.
    pub fn fsync(&mut self, data_only: bool) -> Result<i32> {
        self.flush()?;
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
        if let Self::File(file) = self {
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
        match self {
            Self::File(file) => file.autoclose.get(),
            _ => true,
        }
    }
}
