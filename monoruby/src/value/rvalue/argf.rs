use super::*;

/// Where an ARGF object is in its walk over the input streams.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ArgfState {
    /// Before the first read decides between `$stdin` and the ARGV files.
    Init,
    /// Walking the file queue (a stream may or may not be open right now).
    Reading,
    /// Every input stream has been consumed.
    Done,
}

///
/// The native payload of an ARGF object (`ObjTy::ARGF`).
///
/// The process-wide `ARGF` shares the very array behind `ARGV` / `$*`
/// as its file queue; `ARGF.class.new(*names)` (mspec's `argf` helper)
/// builds instances with a queue of their own. All heap `Value`s here
/// are GC roots via [`ArgfInner::mark`].
///
#[derive(Debug, Clone)]
pub struct ArgfInner {
    /// The file-name queue (an Array value); names are shifted off the
    /// front as streams are consumed.
    pub argv: Value,
    /// The open stream: a `File` object, or the `$stdin` IO object.
    pub current: Option<Value>,
    /// Name of the stream `current` was opened from (a String;
    /// `"-"` for stdin). Survives the stream being closed so
    /// `ARGF.filename` keeps answering after EOF.
    pub filename: Option<Value>,
    /// Cumulative line number across every stream read so far.
    pub lineno: i64,
    /// `lineno` as it stood when `current` became the current stream —
    /// what `ARGF.rewind` winds the counter back to.
    pub file_start_lineno: i64,
    /// The stream most recently retired by the walk, kept so
    /// `ARGF.file` still answers after the last file was consumed.
    pub last_current: Option<Value>,
    pub state: ArgfState,
    /// `set_encoding` arguments, remembered and re-applied to every
    /// stream opened after the call (CRuby keeps them the same way).
    pub enc_args: Vec<Value>,
    /// The current stream's resolved (external, internal) encoding
    /// pair, cached so the per-line reader skips the gvar + ivar
    /// resolution. Cleared whenever the stream or its encoding setup
    /// changes.
    pub cur_encs: Option<(Encoding, Option<Encoding>)>,
    pub binmode: bool,
    /// In-place-edit extension (`-i` / `ARGF.inplace_mode=`):
    /// `Some("")` = no backup, `Some(".bak")` = keep backups.
    pub inplace: Option<String>,
    /// The `$stdout` to restore when in-place editing moves past the
    /// current file (the original stdout the redirect replaced).
    pub inplace_saved_stdout: Option<Value>,
}

impl ArgfInner {
    /// A fresh ARGF over `argv` (an Array of file-name Strings).
    pub fn new(argv: Value) -> Self {
        Self {
            argv,
            current: None,
            filename: None,
            lineno: 0,
            file_start_lineno: 0,
            last_current: None,
            state: ArgfState::Init,
            enc_args: Vec::new(),
            cur_encs: None,
            binmode: false,
            inplace: None,
            inplace_saved_stdout: None,
        }
    }

    pub(crate) fn mark(&self, alloc: &mut Allocator<RValue>) {
        self.argv.mark(alloc);
        if let Some(v) = self.current {
            v.mark(alloc);
        }
        if let Some(v) = self.last_current {
            v.mark(alloc);
        }
        if let Some(v) = self.filename {
            v.mark(alloc);
        }
        for v in &self.enc_args {
            v.mark(alloc);
        }
        if let Some(v) = self.inplace_saved_stdout {
            v.mark(alloc);
        }
    }
}
