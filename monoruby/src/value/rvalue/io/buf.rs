//! monoruby's own IO buffering.
//!
//! Ruby IO deliberately does **not** use Rust's buffering types
//! (`std::io::BufReader`, the `LineWriter` inside `std::io::Stdout`) nor
//! the `std::io::{stdin, stdout, stderr}` handles, because their policy
//! is not Ruby's:
//!
//! * `std::io::Stdout` is line buffered, so it flushes at every newline
//!   even when stdout is a file. CRuby buffers fully there and only
//!   writes at capacity, `#flush`, `#close`, or exit.
//! * `std::io::Stderr` is unbuffered with no way to turn buffering on.
//! * Neither honours `IO#sync=`, and neither exposes how much is
//!   buffered — which the green-thread scheduler needs in order to skip
//!   an fd-readiness park when a read can already be satisfied.
//!
//! The buffers here are monoruby's own and implement CRuby's policy
//! (`io_binwrite` in CRuby's io.c): a write goes straight to the fd when
//! the stream is `sync` or is a TTY (CRuby's `FMODE_SYNC | FMODE_TTY`),
//! and is otherwise accumulated up to [`IO_BUF_CAPA`].
//!
//! The std `Read` / `BufRead` / `Seek` *traits* are implemented so the
//! signal-aware helpers in the parent module stay generic; none of std's
//! buffering *types* are involved.

use std::io::{BufRead, Read, Seek, SeekFrom, Write};
use std::mem::ManuallyDrop;
use std::os::fd::{AsRawFd, FromRawFd, RawFd};

/// Capacity of both the read and the write buffer. Matches CRuby's
/// `IO_WBUF_CAPA_MIN`.
pub(crate) const IO_BUF_CAPA: usize = 8192;

// ---------------------------------------------------------------------
// standard descriptors
// ---------------------------------------------------------------------

/// One of the process's standard descriptors (0/1/2), used *instead of*
/// `std::io::{stdin, stdout, stderr}` so that no buffering happens
/// outside monoruby's own.
///
/// The fd is borrowed: the `File` is wrapped in `ManuallyDrop`, so
/// dropping this never closes a standard descriptor.
pub(crate) struct StdFd(ManuallyDrop<std::fs::File>);

impl StdFd {
    /// # Safety-relevant invariant
    /// `fd` must be one of the process's standard descriptors, which stay
    /// open for its whole lifetime.
    pub(crate) fn new(fd: RawFd) -> Self {
        // SAFETY: fd 0/1/2 are open for the process's lifetime, and the
        // `File` never closes them (`ManuallyDrop`, no `Drop` impl below).
        Self(ManuallyDrop::new(unsafe { std::fs::File::from_raw_fd(fd) }))
    }

    pub(crate) fn is_terminal(&self) -> bool {
        use std::io::IsTerminal;
        self.0.is_terminal()
    }
}

impl std::fmt::Debug for StdFd {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "StdFd({})", self.0.as_raw_fd())
    }
}

impl AsRawFd for StdFd {
    fn as_raw_fd(&self) -> RawFd {
        self.0.as_raw_fd()
    }
}

impl Read for StdFd {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        (&*self.0).read(buf)
    }
}

impl Write for StdFd {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        (&*self.0).write(buf)
    }
    fn flush(&mut self) -> std::io::Result<()> {
        // Nothing is held back below this point: the bytes are already in
        // the kernel.
        Ok(())
    }
}

// ---------------------------------------------------------------------
// read side
// ---------------------------------------------------------------------

/// Refill-on-demand read buffer.
pub(crate) struct IoReader<T> {
    inner: T,
    buf: Box<[u8]>,
    /// `buf[pos..cap]` has been read from the fd but not yet consumed.
    pos: usize,
    cap: usize,
}

impl<T: std::fmt::Debug> std::fmt::Debug for IoReader<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("IoReader")
            .field("inner", &self.inner)
            .field("buffered", &(self.cap - self.pos))
            .finish()
    }
}

impl<T> IoReader<T> {
    pub(crate) fn new(inner: T) -> Self {
        Self {
            inner,
            buf: vec![0u8; IO_BUF_CAPA].into_boxed_slice(),
            pos: 0,
            cap: 0,
        }
    }

    /// Bytes read from the fd but not yet consumed.
    pub(crate) fn buffer(&self) -> &[u8] {
        &self.buf[self.pos..self.cap]
    }

    pub(crate) fn get_ref(&self) -> &T {
        &self.inner
    }

    pub(crate) fn get_mut(&mut self) -> &mut T {
        &mut self.inner
    }

    pub(crate) fn into_inner(self) -> T {
        self.inner
    }

    fn discard_buffer(&mut self) {
        self.pos = 0;
        self.cap = 0;
    }
}

impl<T: Read> Read for IoReader<T> {
    fn read(&mut self, out: &mut [u8]) -> std::io::Result<usize> {
        // With nothing buffered and a request at least as large as the
        // buffer, read straight into the caller's slice: buffering here
        // would only add a copy.
        if self.pos == self.cap && out.len() >= self.buf.len() {
            self.discard_buffer();
            return self.inner.read(out);
        }
        let n = {
            let avail = self.fill_buf()?;
            let n = avail.len().min(out.len());
            out[..n].copy_from_slice(&avail[..n]);
            n
        };
        self.consume(n);
        Ok(n)
    }
}

impl<T: Read> std::io::BufRead for IoReader<T> {
    fn fill_buf(&mut self) -> std::io::Result<&[u8]> {
        if self.pos >= self.cap {
            // The buffer is spent, so refilling cannot lose anything. On
            // error `pos`/`cap` are left as they are (both meaning
            // "empty"), so a retry simply reads again.
            self.cap = self.inner.read(&mut self.buf)?;
            self.pos = 0;
        }
        Ok(&self.buf[self.pos..self.cap])
    }

    fn consume(&mut self, n: usize) {
        self.pos = (self.pos + n).min(self.cap);
    }
}

impl<T: Seek> Seek for IoReader<T> {
    /// Seeking is relative to the *logical* position — the one Ruby sees —
    /// which is behind the fd's position by however much is still sitting
    /// in the buffer. A relative seek therefore folds that remainder in,
    /// and every seek drops the buffer.
    ///
    /// `seek(SeekFrom::Current(0))` is consequently both "report the
    /// logical position" and "rewind the fd to it", which is what the
    /// unbuffered read/write paths rely on before touching the fd
    /// directly.
    fn seek(&mut self, pos: SeekFrom) -> std::io::Result<u64> {
        let result = if let SeekFrom::Current(n) = pos {
            let remainder = (self.cap - self.pos) as i64;
            match n.checked_sub(remainder) {
                Some(offset) => self.inner.seek(SeekFrom::Current(offset))?,
                None => {
                    // `n - remainder` overflowed: undo the read-ahead
                    // first, then apply `n` from there.
                    self.inner.seek(SeekFrom::Current(-remainder))?;
                    self.discard_buffer();
                    self.inner.seek(SeekFrom::Current(n))?
                }
            }
        } else {
            self.inner.seek(pos)?
        };
        self.discard_buffer();
        Ok(result)
    }
}

// ---------------------------------------------------------------------
// write side
// ---------------------------------------------------------------------

/// Why a buffer drain stopped early. Bytes already accepted by the kernel
/// have been removed from the buffer, so a retry resumes exactly where it
/// left off and never duplicates output.
pub(crate) enum DrainErr {
    /// A signal is pending: the caller must unwind to a VM poll point (to
    /// run the Ruby handler) and retry.
    Signal,
    /// `EAGAIN` on an fd the green-thread scheduler put in non-blocking
    /// mode: the caller parks until the fd is writable, then retries.
    WouldBlock,
    Io(std::io::Error),
}

/// Write buffer implementing CRuby's `io_binwrite` policy.
///
/// Holds no sink: a `File` opened `"r+"` is read *and* written through
/// the same descriptor, which the read buffer already owns, so the sink
/// is supplied per call.
pub(crate) struct WriteBuf {
    buf: Vec<u8>,
    /// `IO#sync`. When set, every write reaches the fd before returning.
    sync: bool,
    /// Whether the stream is a TTY. CRuby writes through on a TTY exactly
    /// as it does for `sync` (`FMODE_SYNC | FMODE_TTY` in `io_binwrite`),
    /// so a prompt without a trailing newline still appears.
    tty: bool,
}

impl std::fmt::Debug for WriteBuf {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("WriteBuf")
            .field("buffered", &self.buf.len())
            .field("sync", &self.sync)
            .field("tty", &self.tty)
            .finish()
    }
}

impl WriteBuf {
    pub(crate) fn new(sync: bool, tty: bool) -> Self {
        Self {
            buf: Vec::new(),
            sync,
            tty,
        }
    }

    pub(crate) fn sync(&self) -> bool {
        self.sync
    }

    pub(crate) fn set_sync(&mut self, sync: bool) {
        self.sync = sync;
    }

    /// Bytes accepted from Ruby but not yet handed to the kernel.
    pub(crate) fn buffered_len(&self) -> usize {
        self.buf.len()
    }

    /// Whether writes bypass the buffer entirely.
    pub(crate) fn writes_through(&self) -> bool {
        self.sync || self.tty
    }

    /// Hand the buffer to the kernel, `write(2)` at a time.
    ///
    /// `signal_pending` is polled before every kernel entry: a signal that
    /// arrived while we were in userspace sets no `EINTR`, and entering a
    /// blocking write with it already pending would block unkillably.
    pub(crate) fn drain(
        &mut self,
        sink: &mut impl Write,
        signal_pending: &dyn Fn() -> bool,
    ) -> std::result::Result<(), DrainErr> {
        let mut written = 0usize;
        let res = loop {
            if written >= self.buf.len() {
                break Ok(());
            }
            if signal_pending() {
                break Err(DrainErr::Signal);
            }
            match sink.write(&self.buf[written..]) {
                Ok(0) => break Err(DrainErr::Io(std::io::Error::other("write returned 0"))),
                Ok(n) => written += n,
                Err(e) if e.kind() == std::io::ErrorKind::Interrupted => {
                    // A bare EINTR with no Ruby-visible signal pending is
                    // just a restart; anything else has to reach a poll
                    // point.
                    if signal_pending() {
                        break Err(DrainErr::Signal);
                    }
                }
                Err(e) if e.kind() == std::io::ErrorKind::WouldBlock => {
                    break Err(DrainErr::WouldBlock);
                }
                Err(e) => break Err(DrainErr::Io(e)),
            }
        };
        // Drop exactly what the kernel took, whichever way the loop ended,
        // so a retry neither duplicates nor loses bytes.
        self.buf.drain(..written);
        res
    }

    /// Accept `data[*progress..]` per this stream's policy.
    ///
    /// A write-through stream (`sync`, or a TTY) never touches the buffer:
    /// CRuby's `io_binwrite` hands the bytes straight to `write(2)`, so a
    /// failure leaves nothing behind for a later `#flush` or `#close` to
    /// re-raise — which is what a broken pipe depends on.
    ///
    /// Otherwise the bytes are accumulated, *unless* they would not fit
    /// alongside what is already buffered: then the buffer is drained and
    /// the data goes straight to the kernel, exactly as CRuby does
    /// (`fptr->wbuf.capa <= fptr->wbuf.len + len`). Copying a 64 KiB
    /// write through an 8 KiB buffer would cost more than it saves.
    ///
    /// `*progress` records what has been accepted — bytes copied into the
    /// buffer, or bytes the kernel took — so a restart after a signal or
    /// an `EAGAIN` park resumes exactly where it stopped and never
    /// duplicates output.
    pub(crate) fn write(
        &mut self,
        sink: &mut impl Write,
        data: &[u8],
        progress: &mut usize,
        signal_pending: &dyn Fn() -> bool,
    ) -> std::result::Result<(), DrainErr> {
        if !self.writes_through() && self.buf.len() + (data.len() - *progress) < IO_BUF_CAPA {
            self.buf.extend_from_slice(&data[*progress..]);
            *progress = data.len();
            return Ok(());
        }
        // Anything a previous buffered phase left behind goes first, or
        // the output would be reordered.
        self.drain(sink, signal_pending)?;
        write_direct(sink, data, progress, signal_pending)
    }
}

/// `write(2)` until `data[*progress..]` is gone, polling `signal_pending`
/// before every kernel entry (a signal delivered while we were in
/// userspace sets no `EINTR`, and entering a blocking write with it
/// already pending would block unkillably).
fn write_direct(
    sink: &mut impl Write,
    data: &[u8],
    progress: &mut usize,
    signal_pending: &dyn Fn() -> bool,
) -> std::result::Result<(), DrainErr> {
    while *progress < data.len() {
        if signal_pending() {
            return Err(DrainErr::Signal);
        }
        match sink.write(&data[*progress..]) {
            Ok(0) => return Err(DrainErr::Io(std::io::Error::other("write returned 0"))),
            Ok(n) => *progress += n,
            Err(e) if e.kind() == std::io::ErrorKind::Interrupted => {
                // A bare EINTR with no Ruby-visible signal pending is just
                // a restart; anything else has to reach a poll point.
                if signal_pending() {
                    return Err(DrainErr::Signal);
                }
            }
            Err(e) if e.kind() == std::io::ErrorKind::WouldBlock => {
                return Err(DrainErr::WouldBlock);
            }
            Err(e) => return Err(DrainErr::Io(e)),
        }
    }
    Ok(())
}

/// A [`WriteBuf`] paired with the sink it owns — the standard
/// descriptors and a child's stdin, which are write-only.
pub(crate) struct IoWriter<T> {
    inner: T,
    buf: WriteBuf,
}

impl<T: std::fmt::Debug> std::fmt::Debug for IoWriter<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("IoWriter")
            .field("inner", &self.inner)
            .field("buf", &self.buf)
            .finish()
    }
}

impl<T> IoWriter<T> {
    pub(crate) fn new(inner: T, sync: bool, tty: bool) -> Self {
        Self {
            inner,
            buf: WriteBuf::new(sync, tty),
        }
    }

    pub(crate) fn sync(&self) -> bool {
        self.buf.sync()
    }

    pub(crate) fn set_sync(&mut self, sync: bool) {
        self.buf.set_sync(sync);
    }

    pub(crate) fn get_ref(&self) -> &T {
        &self.inner
    }
}

impl<T: Write> IoWriter<T> {
    pub(crate) fn drain(
        &mut self,
        signal_pending: &dyn Fn() -> bool,
    ) -> std::result::Result<(), DrainErr> {
        self.buf.drain(&mut self.inner, signal_pending)
    }

    pub(crate) fn write(
        &mut self,
        data: &[u8],
        progress: &mut usize,
        signal_pending: &dyn Fn() -> bool,
    ) -> std::result::Result<(), DrainErr> {
        self.buf
            .write(&mut self.inner, data, progress, signal_pending)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Sink that records everything written and can be told to fail the
    /// next `write` with a chosen error, or to accept only `chunk` bytes
    /// at a time (short writes, as a pipe does).
    #[derive(Default, Debug)]
    struct MockSink {
        written: Vec<u8>,
        chunk: Option<usize>,
        fail_with: Vec<std::io::ErrorKind>,
        calls: usize,
    }

    impl Write for MockSink {
        fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
            self.calls += 1;
            if !self.fail_with.is_empty() {
                return Err(self.fail_with.remove(0).into());
            }
            let n = self.chunk.map_or(buf.len(), |c| c.min(buf.len()));
            self.written.extend_from_slice(&buf[..n]);
            Ok(n)
        }
        fn flush(&mut self) -> std::io::Result<()> {
            Ok(())
        }
    }

    fn never() -> impl Fn() -> bool {
        || false
    }

    fn write_all(w: &mut WriteBuf, sink: &mut MockSink, data: &[u8]) {
        let mut progress = 0;
        w.write(sink, data, &mut progress, &never()).ok().unwrap();
        assert_eq!(progress, data.len());
    }

    // ---------------------------------------------------------------
    // write side
    // ---------------------------------------------------------------

    #[test]
    fn buffered_write_holds_until_capacity() {
        let mut w = WriteBuf::new(false, false);
        let mut sink = MockSink::default();
        write_all(&mut w, &mut sink, b"hello");
        // Nothing reaches the sink yet.
        assert_eq!(w.buffered_len(), 5);
        assert!(sink.written.is_empty());
        w.drain(&mut sink, &never()).ok().unwrap();
        assert_eq!(sink.written, b"hello");
        assert_eq!(w.buffered_len(), 0);
    }

    #[test]
    fn buffered_write_spills_at_capacity() {
        let mut w = WriteBuf::new(false, false);
        let mut sink = MockSink::default();
        // Just under capacity stays buffered...
        let small = vec![b'a'; IO_BUF_CAPA - 1];
        write_all(&mut w, &mut sink, &small);
        assert!(sink.written.is_empty());
        // ...and the byte that would fill it pushes everything out.
        write_all(&mut w, &mut sink, b"b");
        assert_eq!(sink.written.len(), IO_BUF_CAPA);
        assert_eq!(w.buffered_len(), 0);
    }

    #[test]
    fn write_larger_than_buffer_goes_direct() {
        let mut w = WriteBuf::new(false, false);
        let mut sink = MockSink::default();
        write_all(&mut w, &mut sink, b"pending");
        let big = vec![b'z'; IO_BUF_CAPA * 2];
        write_all(&mut w, &mut sink, &big);
        // The buffered prefix is flushed first, so ordering holds, and
        // the large write is not copied through the buffer.
        assert_eq!(&sink.written[..7], b"pending");
        assert_eq!(sink.written.len(), 7 + big.len());
        assert_eq!(w.buffered_len(), 0);
    }

    #[test]
    fn sync_and_tty_write_through() {
        for (sync, tty) in [(true, false), (false, true)] {
            let mut w = WriteBuf::new(sync, tty);
            assert!(w.writes_through());
            let mut sink = MockSink::default();
            write_all(&mut w, &mut sink, b"now");
            assert_eq!(sink.written, b"now");
            assert_eq!(w.buffered_len(), 0);
        }
    }

    #[test]
    fn set_sync_flushes_the_buffered_prefix_on_the_next_write() {
        let mut w = WriteBuf::new(false, false);
        let mut sink = MockSink::default();
        write_all(&mut w, &mut sink, b"buffered");
        assert!(!w.sync());
        w.set_sync(true);
        assert!(w.sync());
        write_all(&mut w, &mut sink, b"-through");
        assert_eq!(sink.written, b"buffered-through");
    }

    #[test]
    fn short_writes_are_retried_until_everything_is_out() {
        let mut w = WriteBuf::new(true, false);
        let mut sink = MockSink {
            chunk: Some(3),
            ..Default::default()
        };
        write_all(&mut w, &mut sink, b"0123456789");
        assert_eq!(sink.written, b"0123456789");
        assert!(sink.calls >= 4);
    }

    #[test]
    fn bare_eintr_is_retried() {
        let mut w = WriteBuf::new(false, false);
        let mut sink = MockSink {
            fail_with: vec![std::io::ErrorKind::Interrupted],
            ..Default::default()
        };
        write_all(&mut w, &mut sink, b"x");
        // No signal is pending, so the EINTR is just a restart.
        w.drain(&mut sink, &never()).ok().unwrap();
        assert_eq!(sink.written, b"x");
    }

    #[test]
    fn a_pending_signal_stops_the_drain_and_keeps_the_rest() {
        let mut w = WriteBuf::new(false, false);
        let mut sink = MockSink {
            chunk: Some(2),
            ..Default::default()
        };
        write_all(&mut w, &mut sink, b"abcdef");
        // Signal pending from the start: nothing is written, everything
        // stays buffered for the retry.
        let err = w.drain(&mut sink, &|| true).err().unwrap();
        assert!(matches!(err, DrainErr::Signal));
        assert_eq!(w.buffered_len(), 6);
        assert!(sink.written.is_empty());
        // The retry, with no signal pending, completes it.
        w.drain(&mut sink, &never()).ok().unwrap();
        assert_eq!(sink.written, b"abcdef");
    }

    #[test]
    fn would_block_keeps_only_what_the_kernel_refused() {
        let mut w = WriteBuf::new(false, false);
        let mut sink = MockSink::default();
        write_all(&mut w, &mut sink, b"abcdef");
        sink.chunk = Some(2);
        sink.fail_with = vec![];
        // First `write(2)` takes 2 bytes, the second reports EAGAIN.
        let mut sink = MockSink {
            chunk: Some(2),
            fail_with: vec![],
            ..Default::default()
        };
        w.drain(&mut sink, &never()).ok().unwrap();
        assert_eq!(sink.written, b"abcdef");

        // Now a sink that refuses outright.
        let mut w = WriteBuf::new(false, false);
        let mut sink = MockSink {
            fail_with: vec![std::io::ErrorKind::WouldBlock],
            ..Default::default()
        };
        write_all(&mut w, &mut sink, b"xy");
        let err = w.drain(&mut sink, &never()).err().unwrap();
        assert!(matches!(err, DrainErr::WouldBlock));
        assert_eq!(w.buffered_len(), 2);
    }

    #[test]
    fn a_hard_error_surfaces_and_a_zero_write_is_an_error() {
        let mut w = WriteBuf::new(false, false);
        let mut sink = MockSink {
            fail_with: vec![std::io::ErrorKind::BrokenPipe],
            ..Default::default()
        };
        write_all(&mut w, &mut sink, b"x");
        assert!(matches!(
            w.drain(&mut sink, &never()).err().unwrap(),
            DrainErr::Io(_)
        ));

        let mut w = WriteBuf::new(false, false);
        let mut sink = MockSink {
            chunk: Some(0),
            ..Default::default()
        };
        write_all(&mut w, &mut sink, b"x");
        assert!(matches!(
            w.drain(&mut sink, &never()).err().unwrap(),
            DrainErr::Io(_)
        ));
    }

    #[test]
    fn write_through_failure_leaves_nothing_buffered() {
        // A broken pipe must not strand the bytes: a later `#flush` or
        // `#close` would raise EPIPE all over again.
        let mut w = WriteBuf::new(true, false);
        let mut sink = MockSink {
            fail_with: vec![std::io::ErrorKind::BrokenPipe],
            ..Default::default()
        };
        let mut progress = 0;
        let err = w
            .write(&mut sink, b"gone", &mut progress, &never())
            .err()
            .unwrap();
        assert!(matches!(err, DrainErr::Io(_)));
        assert_eq!(w.buffered_len(), 0);
        assert_eq!(progress, 0);
    }

    #[test]
    fn io_writer_pairs_the_buffer_with_its_sink() {
        let mut w = IoWriter::new(MockSink::default(), false, false);
        let mut progress = 0;
        w.write(b"pair", &mut progress, &never()).ok().unwrap();
        assert_eq!(w.buf.buffered_len(), 4);
        assert!(!w.sync());
        w.set_sync(true);
        assert!(w.sync());
        w.drain(&never()).ok().unwrap();
        assert_eq!(w.get_ref().written, b"pair");
    }

    // ---------------------------------------------------------------
    // read side
    // ---------------------------------------------------------------

    #[test]
    fn reader_refills_on_demand_and_reports_what_it_holds() {
        let data: Vec<u8> = (0..100u8).collect();
        let mut r = IoReader::new(std::io::Cursor::new(data.clone()));
        assert!(r.buffer().is_empty());
        let mut out = [0u8; 10];
        assert_eq!(r.read(&mut out).unwrap(), 10);
        assert_eq!(out, data[..10]);
        // One refill pulled everything; the rest is buffered.
        assert_eq!(r.buffer().len(), 90);
        let mut rest = Vec::new();
        r.read_to_end(&mut rest).unwrap();
        assert_eq!(rest, data[10..]);
        assert_eq!(r.read(&mut out).unwrap(), 0);
    }

    #[test]
    fn a_request_at_least_as_large_as_the_buffer_bypasses_it() {
        let data = vec![7u8; IO_BUF_CAPA * 2];
        let mut r = IoReader::new(std::io::Cursor::new(data.clone()));
        let mut out = vec![0u8; IO_BUF_CAPA * 2];
        let n = r.read(&mut out).unwrap();
        assert_eq!(n, data.len());
        assert!(r.buffer().is_empty());
    }

    #[test]
    fn fill_buf_and_consume_walk_the_buffer() {
        let mut r = IoReader::new(std::io::Cursor::new(b"abcdef".to_vec()));
        assert_eq!(r.fill_buf().unwrap(), b"abcdef");
        r.consume(2);
        assert_eq!(r.buffer(), b"cdef");
        // Over-consuming saturates rather than running past the buffer.
        r.consume(100);
        assert!(r.buffer().is_empty());
        assert_eq!(r.fill_buf().unwrap(), b"");
    }

    #[test]
    fn seek_is_relative_to_the_logical_position() {
        let data: Vec<u8> = (0..50u8).collect();
        let mut r = IoReader::new(std::io::Cursor::new(data));
        let mut out = [0u8; 4];
        r.read(&mut out).unwrap(); // logical position 4, fd at 50
        assert_eq!(r.buffer().len(), 46);
        // `Current(0)` reports the logical position and drops the buffer.
        assert_eq!(r.seek(SeekFrom::Current(0)).unwrap(), 4);
        assert!(r.buffer().is_empty());
        r.read(&mut out).unwrap();
        assert_eq!(out, [4, 5, 6, 7]);
        // Absolute and relative seeks both discard the read-ahead.
        assert_eq!(r.seek(SeekFrom::Start(10)).unwrap(), 10);
        r.read(&mut out).unwrap();
        assert_eq!(out, [10, 11, 12, 13]);
        assert_eq!(r.seek(SeekFrom::Current(-4)).unwrap(), 10);
        assert_eq!(r.seek(SeekFrom::End(-1)).unwrap(), 49);
    }

    /// The `Debug` impls are diagnostics, but they read the buffer state
    /// through the same accessors the hot paths use — a wrong field here
    /// is a wrong field everywhere.
    #[test]
    fn debug_reports_the_buffer_state() {
        let mut r = IoReader::new(std::io::Cursor::new(b"abcdef".to_vec()));
        let mut out = [0u8; 2];
        r.read(&mut out).unwrap();
        assert!(format!("{r:?}").contains("buffered: 4"));

        let mut w = IoWriter::new(MockSink::default(), true, false);
        let mut progress = 0;
        w.write(b"xyz", &mut progress, &never()).ok().unwrap();
        let s = format!("{w:?}");
        assert!(s.contains("sync: true"), "{s}");
        assert!(s.contains("buffered: 0"), "{s}");

        // A standard descriptor reports its fd, and reads/writes reach it
        // without going through `std::io::stdout()`.
        let devnull = std::fs::OpenOptions::new()
            .write(true)
            .open("/dev/null")
            .unwrap();
        let fd = devnull.as_raw_fd();
        let mut std_fd = StdFd::new(fd);
        assert_eq!(std_fd.as_raw_fd(), fd);
        assert_eq!(format!("{std_fd:?}"), format!("StdFd({fd})"));
        assert_eq!(std_fd.write(b"discarded").unwrap(), 9);
        std_fd.flush().unwrap();
        // `StdFd` borrows: dropping it must not close the descriptor.
        drop(std_fd);
        assert_eq!((&devnull).write(b"still open").unwrap(), 10);

        let zero_file = std::fs::File::open("/dev/zero").unwrap();
        let mut zero = StdFd::new(zero_file.as_raw_fd());
        let mut buf = [1u8; 4];
        assert_eq!(zero.read(&mut buf).unwrap(), 4);
        assert_eq!(buf, [0, 0, 0, 0]);
    }

    #[test]
    fn accessors_reach_the_wrapped_stream() {
        let mut r = IoReader::new(std::io::Cursor::new(b"xy".to_vec()));
        assert_eq!(r.get_ref().position(), 0);
        r.get_mut().set_position(1);
        assert_eq!(r.into_inner().position(), 1);
    }
}
