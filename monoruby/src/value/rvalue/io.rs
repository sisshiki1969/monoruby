use std::{
    cell::{Cell, RefCell},
    io::{BufRead, IsTerminal, Read, Seek, SeekFrom, Write},
    mem::ManuallyDrop,
    os::fd::{AsRawFd, FromRawFd, IntoRawFd},
    os::unix::process::ExitStatusExt,
    rc::Rc,
};

use super::*;

/// Recover the raw POSIX `wait(2)` status word from an `ExitStatus` so that
/// Ruby-side `Process::Status` can decode exit code vs termination signal
/// uniformly. `ExitStatus::code()` returns `None` for signal-terminated
/// children, which loses information; using the raw status preserves it.
fn encode_wait_status(status: &std::process::ExitStatus) -> i32 {
    status.into_raw()
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
        // SAFETY: `reader` is wrapped in `ManuallyDrop` and is only taken
        // here, exactly once, in `Drop`. After this, `self.reader` must not
        // be accessed.
        let reader = unsafe { ManuallyDrop::take(&mut self.reader) };
        if self.autoclose.get() {
            // Normal case: dropping the `BufReader<File>` closes the fd via
            // `OwnedFd::drop`.
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
    Closed,
}

impl std::clone::Clone for IoInner {
    fn clone(&self) -> Self {
        match self {
            Self::Stdin => Self::Stdin,
            Self::Stdout => Self::Stdout,
            Self::Stderr => Self::Stderr,
            Self::File(file) => Self::File(file.clone()),
            Self::Popen(popen) => Self::Popen(popen.clone()),
            Self::Closed => Self::Closed,
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
            Self::Closed => write!(f, "#<IO:(closed)>"),
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
            Self::Closed => return Err(MonorubyErr::ioerr("closed stream")),
        };
        res.map_err(|err| MonorubyErr::runtimeerr(err.to_string()))
    }

    pub fn is_closed(&self) -> bool {
        matches!(self, Self::Closed)
    }

    /// Whether the stream may be read from.
    pub fn is_readable(&self) -> bool {
        match self {
            Self::Stdin => true,
            Self::Stdout | Self::Stderr | Self::Closed => false,
            Self::File(f) => f.readable,
            Self::Popen(p) => p.reader.is_some(),
        }
    }

    /// Whether the stream may be written to.
    pub fn is_writable(&self) -> bool {
        match self {
            Self::Stdout | Self::Stderr => true,
            Self::Stdin | Self::Closed => false,
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
        *self = Self::Closed;
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
        // SAFETY: fd is a valid file descriptor obtained from pipe().
        let file = unsafe { std::fs::File::from_raw_fd(fd) };
        Self::File(Rc::new(FileDescriptor {
            reader: ManuallyDrop::new(std::io::BufReader::new(file)),
            name,
            has_path,
            readable,
            writable,
            autoclose: Cell::new(true),
            pushback: RefCell::new(Vec::new()),
        }))
    }

    pub fn write(&mut self, data: &[u8]) -> Result<()> {
        self.ensure_writable()?;
        match self {
            Self::Stdout => match std::io::stdout().write(data) {
                Ok(_) => Ok(()),
                Err(e) => Err(MonorubyErr::rangeerr(e.to_string())),
            },
            Self::Stderr => match std::io::stderr().write(data) {
                Ok(_) => Ok(()),
                Err(e) => Err(MonorubyErr::rangeerr(e.to_string())),
            },
            Self::File(file) => {
                let _ = Rc::get_mut(file)
                    .unwrap()
                    .reader
                    .get_mut()
                    .write(data)
                    .map_err(|e| MonorubyErr::rangeerr(e.to_string()))?;
                Ok(())
            }
            Self::Popen(popen) => {
                let popen = Rc::get_mut(popen).unwrap();
                // `ensure_writable` guaranteed the writer is present.
                let writer = popen.writer.as_mut().unwrap();
                writer
                    .write(data)
                    .map_err(|e| MonorubyErr::ioerr(e.to_string()))?;
                Ok(())
            }
            // `ensure_writable` already rejected non-writable streams.
            Self::Stdin | Self::Closed => unreachable!(),
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
            Self::Closed => Err(MonorubyErr::ioerr("closed stream")),
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
                    let mut out = self.take_pushback(None);
                    let need = n - out.len();
                    out.extend_from_slice(&self.read_underlying(Some(need))?);
                    return Ok(out);
                }
                None => {
                    let mut out = self.take_pushback(None);
                    out.extend_from_slice(&self.read_underlying(None)?);
                    return Ok(out);
                }
            }
        }
        self.read_underlying(length)
    }

    fn read_underlying(&mut self, length: Option<usize>) -> Result<Vec<u8>> {
        match self {
            Self::Closed => return Err(MonorubyErr::ioerr("closed stream")),
            Self::Stdin => {
                if let Some(length) = length {
                    let buf = match std::io::stdin().bytes().take(length).collect() {
                        Ok(buf) => buf,
                        Err(e) => return Err(MonorubyErr::runtimeerr(e.to_string())),
                    };
                    Ok(buf)
                } else {
                    let mut buf = vec![];
                    match std::io::stdin().read_to_end(&mut buf) {
                        Ok(_) => {}
                        Err(e) => return Err(MonorubyErr::runtimeerr(e.to_string())),
                    }
                    Ok(buf)
                }
            }
            Self::Stdout => Err(MonorubyErr::argumenterr("can't read from $stdin")),
            Self::Stderr => Err(MonorubyErr::argumenterr("can't read from $stderr")),
            Self::File(file) => {
                if !file.readable {
                    return Err(MonorubyErr::ioerr("not opened for reading"));
                }
                // `&mut *...reader` peels the ManuallyDrop wrapper to yield
                // `&mut BufReader<File>`, which is needed because `Read::bytes`
                // takes `self` by value and would otherwise try to move out of
                // the ManuallyDrop.
                let file = &mut *Rc::get_mut(file).unwrap().reader;
                if let Some(length) = length {
                    let buf = match file.bytes().take(length).collect() {
                        Ok(buf) => buf,
                        Err(e) => return Err(MonorubyErr::runtimeerr(e.to_string())),
                    };
                    Ok(buf)
                } else {
                    let mut buf = vec![];
                    file.read_to_end(&mut buf)
                        .map_err(|e| MonorubyErr::runtimeerr(e.to_string()))?;
                    Ok(buf)
                }
            }
            Self::Popen(popen) => {
                let popen = Rc::get_mut(popen).unwrap();
                let reader = popen
                    .reader
                    .as_mut()
                    .ok_or_else(|| MonorubyErr::ioerr("not opened for reading"))?;
                if let Some(length) = length {
                    let buf: std::result::Result<Vec<u8>, _> =
                        reader.bytes().take(length).collect();
                    buf.map_err(|e| MonorubyErr::ioerr(e.to_string()))
                } else {
                    let mut buf = vec![];
                    reader
                        .read_to_end(&mut buf)
                        .map_err(|e| MonorubyErr::ioerr(e.to_string()))?;
                    Ok(buf)
                }
            }
        }
    }

    pub fn read_line(&mut self) -> Result<Option<String>> {
        if self.pushback_len() > 0 {
            let cell = self.pushback_cell().unwrap();
            let nl = cell.borrow().iter().position(|&b| b == b'\n');
            match nl {
                Some(idx) => {
                    let line = self.take_pushback(Some(idx + 1));
                    return Ok(Some(String::from_utf8_lossy(&line).into_owned()));
                }
                None => {
                    let mut line = self.take_pushback(None);
                    match self.read_line_underlying()? {
                        Some(rest) => line.extend_from_slice(rest.as_bytes()),
                        None if line.is_empty() => return Ok(None),
                        None => {}
                    }
                    return Ok(Some(String::from_utf8_lossy(&line).into_owned()));
                }
            }
        }
        self.read_line_underlying()
    }

    fn read_line_underlying(&mut self) -> Result<Option<String>> {
        match self {
            Self::Closed => return Err(MonorubyErr::ioerr("closed stream")),
            Self::Stdin => {
                let mut buf = String::new();
                std::io::stdin()
                    .read_line(&mut buf)
                    .map_err(|e| MonorubyErr::runtimeerr(e.to_string()))?;
                Ok(Some(buf))
            }
            Self::Stdout => Err(MonorubyErr::argumenterr("can't read from $stdin")),
            Self::Stderr => Err(MonorubyErr::argumenterr("can't read from $stderr")),
            Self::File(file) => {
                if !file.readable {
                    return Err(MonorubyErr::ioerr("not opened for reading"));
                }
                let file = &mut *Rc::get_mut(file).unwrap().reader;
                let mut buf = String::new();
                let size = file
                    .read_line(&mut buf)
                    .map_err(|e| MonorubyErr::runtimeerr(e.to_string()))?;
                if size == 0 {
                    return Ok(None);
                }
                Ok(Some(buf))
            }
            Self::Popen(popen) => {
                let popen = Rc::get_mut(popen).unwrap();
                let reader = popen
                    .reader
                    .as_mut()
                    .ok_or_else(|| MonorubyErr::ioerr("not opened for reading"))?;
                let mut buf = String::new();
                let size = reader
                    .read_line(&mut buf)
                    .map_err(|e| MonorubyErr::ioerr(e.to_string()))?;
                if size == 0 {
                    return Ok(None);
                }
                Ok(Some(buf))
            }
        }
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
            Self::Closed => Err(MonorubyErr::ioerr("closed stream")),
        }
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
            Self::Closed => Err(std::io::Error::from_raw_os_error(9)), // EBADF
            _ => Err(std::io::Error::from_raw_os_error(ESPIPE)),
        }
    }

    pub fn isatty(&self) -> bool {
        match self {
            Self::Stdin => std::io::stdin().is_terminal(),
            Self::Stdout => std::io::stdout().is_terminal(),
            Self::Stderr => std::io::stderr().is_terminal(),
            Self::File(_) | Self::Popen(_) | Self::Closed => false,
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
            Self::File(_) | Self::Popen(_) | Self::Closed => None,
        }
    }

    /// CRuby `IO#fsync` / `IO#fdatasync`. Flushes user-space buffers, then
    /// asks the kernel to flush to permanent storage. `data_only` selects
    /// `fdatasync(2)` (skip metadata) over `fsync(2)`. Returns `0` on
    /// success (matching CRuby), `IOError` on a closed stream.
    pub fn fsync(&mut self, data_only: bool) -> Result<i32> {
        self.flush()?;
        let fd = self.fileno()?;
        let ret = unsafe {
            if data_only {
                libc::fdatasync(fd)
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
    /// because their fd lifetime is not owned by this `IoInner`.
    pub fn set_autoclose(&self, value: bool) {
        if let Self::File(file) = self {
            file.autoclose.set(value);
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
