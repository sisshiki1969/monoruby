use super::*;
use std::fs::File;
use std::os::unix::process::CommandExt;
use std::process::{Command, Stdio};
use std::rc::Rc;

//
// IO class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_under_obj("IO", IO_CLASS, ObjTy::IO);
    globals.define_builtin_class_func_with(IO_CLASS, "new", io_new, 1, 3, false);
    globals.store[IO_CLASS].set_alloc_func(io_alloc_func);
    globals.define_builtin_func(IO_CLASS, "<<", shl, 1);
    globals.define_builtin_func_with(IO_CLASS, "puts", puts, 0, 0, true);
    globals.define_builtin_func_with(IO_CLASS, "print", print, 0, 0, true);
    globals.define_builtin_func_with(IO_CLASS, "printf", printf, 1, 1, true);
    globals.define_builtin_func(IO_CLASS, "flush", flush, 0);
    globals.define_builtin_func_with(IO_CLASS, "gets", gets, 0, 2, false);
    globals.define_builtin_funcs(IO_CLASS, "isatty", &["tty?"], isatty, 0);
    globals.define_builtin_func(IO_CLASS, "close", close, 0);
    globals.define_builtin_func(IO_CLASS, "close_write", close_write, 0);
    globals.define_builtin_func(IO_CLASS, "close_read", close_read, 0);
    globals.define_builtin_func(IO_CLASS, "closed?", closed_, 0);
    globals.define_builtin_func(IO_CLASS, "sync=", assign_sync, 1);
    globals.define_builtin_func_with(IO_CLASS, "seek", seek, 1, 2, false);
    globals.define_builtin_func_with(IO_CLASS, "read", read, 0, 2, false);
    globals.define_builtin_class_func_with(IO_CLASS, "read", io_class_read, 1, 4, false);
    globals.define_builtin_class_func_with(IO_CLASS, "readlines", io_class_readlines, 1, 3, false);
    globals.define_builtin_class_func_with(IO_CLASS, "sysopen", io_sysopen, 1, 3, false);
    globals.define_builtin_class_func_with(IO_CLASS, "pipe", io_pipe, 0, 3, false);
    globals.define_builtin_class_func_rest(IO_CLASS, "popen", io_popen);
    globals.define_builtin_func(IO_CLASS, "pid", io_pid, 0);
    globals.define_builtin_func(IO_CLASS, "fileno", io_fileno, 0);
    globals.define_builtin_func(IO_CLASS, "to_i", io_fileno, 0);
    globals.define_builtin_func(IO_CLASS, "to_io", io_to_io, 0);
    globals.define_builtin_func_with(IO_CLASS, "write", io_write_method, 0, 0, true);
    globals.define_builtin_func_with(IO_CLASS, "syswrite", io_syswrite, 1, 1, false);
    globals.define_builtin_func_with(IO_CLASS, "readlines", io_readlines, 0, 2, false);
    globals.define_builtin_func(IO_CLASS, "binmode", io_binmode, 0);
    globals.define_builtin_func(IO_CLASS, "binmode?", io_binmode_, 0);
    globals.define_builtin_func(IO_CLASS, "autoclose=", io_autoclose_set, 1);
    globals.define_builtin_func(IO_CLASS, "autoclose?", io_autoclose_, 0);
    globals.define_builtin_func_with(IO_CLASS, "advise", io_advise, 1, 3, false);
    globals.define_builtin_funcs(IO_CLASS, "pos", &["tell"], io_pos, 0);
    globals.define_builtin_func(IO_CLASS, "pos=", io_pos_set, 1);
    globals.define_builtin_func(IO_CLASS, "rewind", io_rewind, 0);
    globals.define_builtin_funcs(IO_CLASS, "eof?", &["eof"], io_eof_, 0);
    globals.define_builtin_func(IO_CLASS, "getbyte", io_getbyte, 0);
    globals.define_builtin_func(IO_CLASS, "getc", io_getc, 0);
    globals.define_builtin_func(IO_CLASS, "ungetc", io_ungetc, 1);
    globals.define_builtin_func(IO_CLASS, "ungetbyte", io_ungetbyte, 1);
    globals.define_builtin_func_with(IO_CLASS, "sysseek", io_sysseek, 1, 2, false);
    globals.define_builtin_func(IO_CLASS, "lineno", io_lineno, 0);
    globals.define_builtin_func(IO_CLASS, "lineno=", io_lineno_set, 1);
    globals.define_builtin_funcs(IO_CLASS, "path", &["to_path"], io_path, 0);
    globals.define_builtin_func(IO_CLASS, "fsync", io_fsync, 0);
    globals.define_builtin_func(IO_CLASS, "fdatasync", io_fdatasync, 0);
    globals.define_builtin_func(IO_CLASS, "close_on_exec?", io_close_on_exec_, 0);
    globals.define_builtin_func(IO_CLASS, "close_on_exec=", io_close_on_exec_set, 1);
    globals.define_builtin_class_func_with(IO_CLASS, "select", io_select, 1, 4, false);
    globals.define_builtin_class_func_with(IO_CLASS, "foreach", io_foreach, 1, 3, false);
    globals.define_builtin_class_func_with(IO_CLASS, "copy_stream", io_copy_stream, 2, 4, false);
    globals.define_builtin_func_with(IO_CLASS, "set_encoding", set_encoding, 1, 3, false);
    globals.define_builtin_func(IO_CLASS, "external_encoding", external_encoding, 0);
    globals.define_builtin_func(IO_CLASS, "internal_encoding", internal_encoding, 0);
    globals.define_builtin_func_rest(IO_CLASS, "wait", io_wait);
    globals.define_builtin_func_with(IO_CLASS, "wait_readable", io_wait_readable, 0, 1, false);
    globals.define_builtin_func_with(IO_CLASS, "wait_writable", io_wait_writable, 0, 1, false);
    globals.define_builtin_func_with(IO_CLASS, "wait_priority", io_wait_priority, 0, 1, false);
    // IO::READABLE / WRITABLE / PRIORITY event masks (same values as POSIX
    // POLLIN / POLLOUT / POLLPRI; these are the constants ruby/spec's
    // `library/io-wait` expects).
    globals.set_constant_by_str(IO_CLASS, "READABLE", Value::integer(1));
    globals.set_constant_by_str(IO_CLASS, "PRIORITY", Value::integer(2));
    globals.set_constant_by_str(IO_CLASS, "WRITABLE", Value::integer(4));
    let stdin = Value::new_io_stdin();
    globals.set_constant_by_str(OBJECT_CLASS, "STDIN", stdin);
    globals.set_gvar(IdentId::get_id("$stdin"), stdin);

    let stdout = Value::new_io_stdout();
    globals.set_constant_by_str(OBJECT_CLASS, "STDOUT", stdout);
    globals.set_gvar(IdentId::get_id("$stdout"), stdout);
    globals.set_gvar(IdentId::get_id("$>"), stdout);

    let stderr = Value::new_io_stderr();
    globals.set_constant_by_str(OBJECT_CLASS, "STDERR", stderr);
    globals.set_gvar(IdentId::get_id("$stderr"), stderr);
}

#[monoruby_builtin]
fn io_new(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    // IO.new(fd [, mode [, opt]]) -> creates an IO object from an integer file descriptor.
    if let Some(fd) = lfp.arg(0).try_fixnum() {
        let fd_i32 = fd as i32;
        if fd_i32 < 0 || unsafe { libc::fcntl(fd_i32, libc::F_GETFD) } == -1 {
            let err = std::io::Error::from_raw_os_error(9); // EBADF
            return Err(MonorubyErr::errno_with_path(
                &globals.store,
                &err,
                "rb_sysopen",
                &format!("fd {}", fd),
            ));
        }
        // Pick up `:autoclose` and `:path` from the options Hash. See
        // `io_open_opts` for the rationale.
        let (name, has_path, autoclose) = io_open_opts(vm, globals, lfp, 1..3, fd)?;
        let (readable, writable) = fd_rw_mode(fd_i32);
        let io_inner = IoInner::from_raw_fd(fd_i32, name, has_path, readable, writable);
        io_inner.set_autoclose(autoclose);
        let res = Value::new_io(io_inner);
        let mode_for_enc = lfp
            .try_arg(1)
            .and_then(|a| a.is_str().map(|s| s.to_string()))
            .unwrap_or_else(|| {
                match (readable, writable) {
                    (true, true) => "r+",
                    (false, true) => "w",
                    _ => "r",
                }
                .to_string()
            });
        init_io_encodings(vm, globals, lfp, res, &mode_for_enc, readable, 1..3)?;
        return Ok(res);
    }
    Err(MonorubyErr::argumenterr(
        "IO.new requires an integer file descriptor",
    ))
}

/// Parse the trailing options Hash for `IO.new(fd, ...)` / `File.new(fd, ...)`,
/// returning `(display_name, autoclose)`.
///
/// CRuby accepts `:path` (display name surfaced via `IO#path`) and `:autoclose`
/// (whether the wrapping IO closes the fd on close/GC). The latter is
/// load-bearing: `File.new(other.fileno, autoclose: false, path: ...)` (used
/// in `logger/log_device.rb`'s feature detection and `fixup_mode`) hands a
/// borrowed fd to a new wrapper while keeping the original IO responsible
/// for closing it. Without honoring `:autoclose` we end up with two owners
/// for the same fd, triggering `IO Safety violation: owned file descriptor
/// already closed` on drop.
pub(super) fn io_open_opts(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    range: std::ops::Range<usize>,
    fd: i64,
) -> Result<(String, bool, bool)> {
    let mut autoclose = true;
    let mut name = format!("fd {}", fd);
    let mut has_path = false;
    for i in range {
        if let Some(arg) = lfp.try_arg(i)
            && let Some(h) = arg.try_hash_ty()
        {
            if let Some(v) = h.get(Value::symbol(IdentId::get_id("autoclose")), vm, globals)? {
                autoclose = v.as_bool();
            }
            if let Some(v) = h.get(Value::symbol(IdentId::get_id("path")), vm, globals)?
                && !v.is_nil()
            {
                name = v.coerce_to_string(vm, globals)?;
                has_path = true;
            }
        }
    }
    Ok((name, has_path, autoclose))
}

/// Query a raw fd's access mode via `fcntl(F_GETFL)` and map `O_ACCMODE`
/// to `(readable, writable)`. Falls back to `(true, true)` if the query
/// fails so we never spuriously reject I/O on a valid fd.
pub(super) fn fd_rw_mode(fd: i32) -> (bool, bool) {
    let flags = unsafe { libc::fcntl(fd, libc::F_GETFL) };
    if flags == -1 {
        return (true, true);
    }
    match flags & libc::O_ACCMODE {
        libc::O_RDONLY => (true, false),
        libc::O_WRONLY => (false, true),
        _ => (true, true),
    }
}

// ----------------------------------------------------------------------
// IO external/internal encoding (state + reporting; no content transcode)
//
// Mirrors CRuby's `rb_io_ext_int_to_encs`: an IO's encodings are resolved
// once at creation (and again on `#set_encoding`) from the explicit
// encodings (mode-string suffix / options / args) and the *current*
// `Encoding.default_{external,internal}`. The resolved values are stored
// as ivars so they survive `#close`. The only case that tracks the
// default live is "no explicit encodings, default_internal unset, read
// mode" (CRuby leaves `io->encs.enc` unset and reads default_external on
// demand).
// ----------------------------------------------------------------------

const ENC_EXT_IVAR: &str = "/enc_ext";
const ENC_INT_IVAR: &str = "/enc_int";
const ENC_DYN_MARKER: &str = "__io_dynamic_default_external__";
const BINMODE_IVAR: &str = "/binmode";

#[derive(Clone, Copy)]
enum ExtSlot {
    Fixed(Value),
    /// Track `Encoding.default_external` live.
    Dynamic,
    Nil,
}

/// Encoding *object* (preserving identity, unlike the `Encoding` enum
/// which folds e.g. IBM866 -> ASCII-8BIT) for a name.
fn enc_by_name(globals: &Globals, name: &str) -> Option<Value> {
    super::encoding::find_encoding_object(globals, name)
}

/// `Encoding.default_external` object (UTF-8 if unset).
fn enc_default_external_obj(globals: &mut Globals) -> Value {
    if let Some(v) = globals.get_gvar(IdentId::get_id("$DEFAULT_EXTERNAL"))
        && !v.is_nil()
    {
        return v;
    }
    enc_by_name(globals, "UTF-8").unwrap_or(Value::nil())
}

/// `Encoding.default_internal` object, or `None` if unset.
fn enc_default_internal_obj(globals: &mut Globals) -> Option<Value> {
    let v = globals.get_gvar(IdentId::get_id("$DEFAULT_INTERNAL"))?;
    if v.is_nil() {
        None
    } else {
        Some(v)
    }
}

fn enc_norm(s: &str) -> String {
    s.to_uppercase().replace(['-', '_'], "")
}

fn enc_is_binary(globals: &Globals, v: Value) -> bool {
    super::encoding::encoding_object_name(globals, v)
        .map(|n| {
            let u = n.to_uppercase();
            u == "ASCII-8BIT" || u == "BINARY"
        })
        .unwrap_or(false)
}

fn enc_same(globals: &Globals, a: Value, b: Value) -> bool {
    if a.id() == b.id() {
        return true;
    }
    match (
        super::encoding::encoding_object_name(globals, a),
        super::encoding::encoding_object_name(globals, b),
    ) {
        (Some(x), Some(y)) => enc_norm(&x) == enc_norm(&y),
        _ => false,
    }
}

/// Argument `Value` -> Encoding object: an `Encoding` is taken as-is, a
/// String is resolved by name; anything else (incl. `nil`) -> `None`.
fn arg_to_enc_obj(globals: &Globals, v: Value) -> Option<Value> {
    if v.is_nil() {
        return None;
    }
    if v.class() == super::encoding::encoding_class(globals) {
        return Some(v);
    }
    if let Some(s) = v.is_str() {
        return enc_by_name(globals, &s);
    }
    None
}

/// CRuby's `rb_io_ext_int_to_encs` external/internal resolution,
/// operating on Encoding objects.
fn resolve_io_encodings(
    globals: &Globals,
    explicit_ext: Option<Value>,
    explicit_int: Option<Value>,
    binmode: bool,
    readable: bool,
    de: Value,
    di: Option<Value>,
    at_creation: bool,
) -> (ExtSlot, Option<Value>) {
    // `b`/binmode forces a BINARY external only when none was given.
    let ext = if explicit_ext.is_none() && binmode {
        enc_by_name(globals, "ASCII-8BIT")
    } else {
        explicit_ext
    };
    if let Some(e) = ext {
        let mut i = explicit_int;
        if enc_is_binary(globals, e) || i.map(|x| enc_same(globals, x, e)).unwrap_or(false) {
            i = None;
        }
        return (ExtSlot::Fixed(e), i);
    }
    if explicit_int.is_none() {
        match di {
            Some(di) => {
                let i = if enc_is_binary(globals, de) || enc_same(globals, di, de) {
                    None
                } else {
                    Some(di)
                };
                (ExtSlot::Fixed(de), i)
            }
            None => {
                if at_creation && readable {
                    (ExtSlot::Dynamic, None)
                } else {
                    (ExtSlot::Nil, None)
                }
            }
        }
    } else {
        let it = explicit_int.unwrap();
        let i = if enc_is_binary(globals, de) || enc_same(globals, it, de) {
            None
        } else {
            Some(it)
        };
        (ExtSlot::Fixed(de), i)
    }
}

/// Persist a resolved encoding pair as ivars on the IO object.
fn store_io_encodings(globals: &mut Globals, io: Value, ext: ExtSlot, int: Option<Value>) {
    let ext_val = match ext {
        ExtSlot::Fixed(v) => v,
        ExtSlot::Dynamic => Value::symbol(IdentId::get_id(ENC_DYN_MARKER)),
        ExtSlot::Nil => Value::nil(),
    };
    let int_val = int.unwrap_or_else(Value::nil);
    let _ = globals
        .store
        .set_ivar(io, IdentId::get_id(ENC_EXT_IVAR), ext_val);
    let _ = globals
        .store
        .set_ivar(io, IdentId::get_id(ENC_INT_IVAR), int_val);
}

/// Split a mode string's `":ext[:int]"` suffix into encoding names.
fn mode_encoding_names(mode: &str) -> (Option<&str>, Option<&str>) {
    let mut it = mode.split(':');
    let _base = it.next();
    let ext = it.next().filter(|s| !s.is_empty());
    let int = it.next().filter(|s| !s.is_empty());
    (ext, int)
}

/// Parse the explicit external/internal encoding objects and binmode
/// flag from a mode string and an options Hash. Precedence:
/// `:external_encoding` / `:internal_encoding` > `:encoding` >
/// mode-string suffix.
fn parse_open_encodings(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    opt_range: std::ops::Range<usize>,
    mode: &str,
) -> Result<(Option<Value>, Option<Value>, bool)> {
    let base = mode.split(':').next().unwrap_or("");
    let binmode = base.contains('b');
    let (mext, mint) = mode_encoding_names(mode);
    let mut ext = mext.and_then(|s| enc_by_name(globals, s));
    let mut int = mint.and_then(|s| enc_by_name(globals, s));

    for i in opt_range {
        let Some(arg) = lfp.try_arg(i) else { continue };
        let Some(h) = arg.try_hash_ty() else { continue };
        if let Some(v) = h.get(Value::symbol(IdentId::get_id("encoding")), vm, globals)?
            && !v.is_nil()
        {
            if let Some(s) = v.is_str() {
                let mut parts = s.split(':');
                ext = parts
                    .next()
                    .filter(|x| !x.is_empty())
                    .and_then(|x| enc_by_name(globals, x));
                int = parts
                    .next()
                    .filter(|x| !x.is_empty())
                    .and_then(|x| enc_by_name(globals, x));
            } else {
                ext = arg_to_enc_obj(globals, v);
            }
        }
        if let Some(v) = h.get(Value::symbol(IdentId::get_id("external_encoding")), vm, globals)?
            && !v.is_nil()
        {
            ext = arg_to_enc_obj(globals, v);
        }
        if let Some(v) = h.get(Value::symbol(IdentId::get_id("internal_encoding")), vm, globals)?
            && !v.is_nil()
        {
            int = arg_to_enc_obj(globals, v);
        }
    }
    Ok((ext, int, binmode))
}

/// Pull `(mode, ext, int)` out of a single options Hash element (mode
/// string suffix + `:encoding`/`:external_encoding`/`:internal_encoding`).
fn read_opts_from_hash(
    vm: &mut Executor,
    globals: &mut Globals,
    h: crate::value::Hashmap,
    mode: &mut String,
    ext: &mut Option<Value>,
    int: &mut Option<Value>,
) -> Result<()> {
    if let Some(m) = h.get(Value::symbol(IdentId::get_id("mode")), vm, globals)?
        && let Some(s) = m.is_str()
    {
        *mode = s.to_string();
        let (me, mi) = mode_encoding_names(mode);
        if let Some(e) = me {
            *ext = enc_by_name(globals, e);
        }
        if let Some(i) = mi {
            *int = enc_by_name(globals, i);
        }
    }
    if let Some(v) = h.get(Value::symbol(IdentId::get_id("encoding")), vm, globals)?
        && !v.is_nil()
    {
        if let Some(s) = v.is_str() {
            let mut parts = s.split(':');
            *ext = parts
                .next()
                .filter(|x| !x.is_empty())
                .and_then(|x| enc_by_name(globals, x));
            *int = parts
                .next()
                .filter(|x| !x.is_empty())
                .and_then(|x| enc_by_name(globals, x));
        } else {
            *ext = arg_to_enc_obj(globals, v);
        }
    }
    if let Some(v) = h.get(Value::symbol(IdentId::get_id("external_encoding")), vm, globals)?
        && !v.is_nil()
    {
        *ext = arg_to_enc_obj(globals, v);
    }
    if let Some(v) = h.get(Value::symbol(IdentId::get_id("internal_encoding")), vm, globals)?
        && !v.is_nil()
    {
        *int = arg_to_enc_obj(globals, v);
    }
    Ok(())
}

/// Resolve `(mode, ext, int)` for `IO.read`. When `:open_args` is given
/// it fully determines mode/encoding and the other options are ignored
/// (CRuby semantics).
fn class_read_opts(
    vm: &mut Executor,
    globals: &mut Globals,
    opts: Option<crate::value::Hashmap>,
) -> Result<(String, Option<Value>, Option<Value>)> {
    let mut mode = "r".to_string();
    let mut ext = None;
    let mut int = None;
    let Some(h) = opts else {
        return Ok((mode, ext, int));
    };
    if let Some(oa) = h.get(Value::symbol(IdentId::get_id("open_args")), vm, globals)?
        && let Some(ary) = oa.try_array_ty()
    {
        for v in ary.iter() {
            if let Some(s) = v.is_str() {
                mode = s.to_string();
                let (me, mi) = mode_encoding_names(&mode);
                if let Some(e) = me {
                    ext = enc_by_name(globals, e);
                }
                if let Some(i) = mi {
                    int = enc_by_name(globals, i);
                }
            } else if let Some(hh) = v.try_hash_ty() {
                read_opts_from_hash(vm, globals, hh, &mut mode, &mut ext, &mut int)?;
            }
        }
        return Ok((mode, ext, int));
    }
    read_opts_from_hash(vm, globals, h, &mut mode, &mut ext, &mut int)?;
    Ok((mode, ext, int))
}

/// Resolve + store IO encodings at creation time. `opt_range` is the
/// `lfp` arg span to scan for a trailing options Hash.
pub(super) fn init_io_encodings(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    io: Value,
    mode: &str,
    readable: bool,
    opt_range: std::ops::Range<usize>,
) -> Result<()> {
    let (ext, int, binmode) = parse_open_encodings(vm, globals, lfp, opt_range, mode)?;
    let de = enc_default_external_obj(globals);
    let di = enc_default_internal_obj(globals);
    let (slot, i) = resolve_io_encodings(globals, ext, int, binmode, readable, de, di, true);
    store_io_encodings(globals, io, slot, i);
    if binmode {
        let _ = globals
            .store
            .set_ivar(io, IdentId::get_id(BINMODE_IVAR), Value::bool(true));
    }
    Ok(())
}

/// Allocator for `IO` and its subclasses.
pub(crate) extern "C" fn io_alloc_func(class_id: ClassId, _: &mut Globals) -> Value {
    Value::new_io_with_class(IoInner::Closed, class_id)
}

///
/// ### IO#<<
///
/// - self << object -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/=3c=3c.html]
#[monoruby_builtin]
fn shl(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let mut self_ = lfp.self_val();
    let io = self_.as_io_inner_mut();
    if let Some(b) = lfp.arg(0).try_bytes() {
        io.write(b.as_bytes())?;
    } else {
        let s = vm.to_s(globals, lfp.arg(0))?;
        io.write(s.as_bytes())?;
    };
    io.flush()?;
    Ok(lfp.self_val())
}

///
/// ### IO#puts
///
/// - puts(*obj) -> nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/puts.html]
#[monoruby_builtin]
fn puts(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    fn decompose(collector: &mut Vec<Value>, val: Value, seen: &mut Vec<u64>) {
        match val.try_array_ty() {
            Some(ary) => {
                let id = val.id();
                if seen.contains(&id) {
                    collector.push(Value::string("[...]".to_string()));
                } else {
                    seen.push(id);
                    ary.iter().for_each(|v| decompose(collector, *v, seen));
                    seen.pop();
                }
            }
            None => collector.push(val),
        }
    }
    let mut collector = Vec::new();
    let mut seen = Vec::new();
    let args = lfp.arg(0).as_array();
    for v in args.iter().cloned() {
        decompose(&mut collector, v, &mut seen);
    }

    let self_val = lfp.self_val();
    let write_id = IdentId::get_id("write");
    if collector.is_empty() {
        // puts with no args writes a newline
        let newline = Value::string_from_str("\n");
        vm.invoke_method_inner(globals, write_id, self_val, &[newline], None, None)?;
    } else {
        for v in collector {
            let s = if v.is_nil() {
                String::new()
            } else if let Some(rs) = v.is_rstring() {
                String::from_utf8_lossy(rs.as_bytes()).into_owned()
            } else {
                v.to_s(globals)
            };
            let needs_newline = !s.ends_with('\n');
            let write_str = if needs_newline {
                Value::string(s + "\n")
            } else {
                Value::string(s)
            };
            vm.invoke_method_inner(globals, write_id, self_val, &[write_str], None, None)?;
        }
    }
    // Flush after all writes
    let mut self_ = lfp.self_val();
    self_.as_io_inner_mut().flush()?;
    Ok(Value::nil())
}

///
/// ### IO#print
///
/// - print(*arg) -> nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/print.html]
#[monoruby_builtin]
fn print(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let write_id = IdentId::get_id("write");
    for v in lfp.arg(0).as_array().iter().cloned() {
        let str_val = if v.is_rstring().is_some() {
            v
        } else {
            let s = vm.to_s(globals, v)?;
            Value::string(s)
        };
        vm.invoke_method_inner(globals, write_id, self_val, &[str_val], None, None)?;
    }
    Ok(Value::nil())
}

///
/// ### IO#printf
///
/// - printf(format, *arg) -> nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/printf.html]
#[monoruby_builtin]
fn printf(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let format_str = lfp.arg(0).coerce_to_string(vm, globals)?;
    let mut self_ = lfp.self_val();
    let io = self_.as_io_inner_mut();
    let args = lfp.arg(1).as_array();

    let buf = vm.format_by_args(globals, &format_str, &args)?;
    io.write(buf.as_bytes())?;

    Ok(Value::nil())
}

///
/// ### IO#flush
///
/// - flush -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/flush.html]
#[monoruby_builtin]
fn flush(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let mut self_ = lfp.self_val();
    self_.as_io_inner_mut().flush()?;

    Ok(lfp.self_val())
}

///
/// ### IO#gets
///
/// - gets([NOT SUPPORTED]rs = $/) -> String | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/gets.html]
#[monoruby_builtin]
fn gets(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let mut self_ = lfp.self_val();
    let line = self_.as_io_inner_mut().read_line()?;
    match line {
        Some(buf) => {
            let cur = globals
                .store
                .get_ivar(self_, IdentId::get_id("/lineno"))
                .and_then(|v| v.try_fixnum())
                .unwrap_or(0);
            let n = cur + 1;
            globals
                .store
                .set_ivar(self_, IdentId::get_id("/lineno"), Value::integer(n))?;
            globals.set_gvar(IdentId::get_id("$."), Value::integer(n));
            let s = tagged_read_string(globals, self_, buf.into_bytes(), false);
            globals.set_gvar(IdentId::get_id("$_"), s);
            Ok(s)
        }
        None => {
            globals.set_gvar(IdentId::get_id("$_"), Value::nil());
            Ok(Value::nil())
        }
    }
}

///
/// ### IO#isatty
///
/// - isatty -> bool
/// - tty? -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/isatty.html]
#[monoruby_builtin]
fn isatty(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    ensure_io_open(lfp.self_val())?;
    Ok(Value::bool(lfp.self_val().as_io_inner_mut().isatty()))
}

///
/// ### IO#close
///
/// - close -> nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/close.html]
#[monoruby_builtin]
fn close(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let popen_result = lfp.self_val().as_io_inner_mut().close()?;
    if let Some((exit_status, pid)) = popen_result {
        // Set $? (Process::Status) via Process::Status.new(exitstatus, pid)
        let status_class =
            vm.get_qualified_constant(globals, OBJECT_CLASS, &["Process", "Status"])?;
        let status_obj = vm.invoke_method_inner(
            globals,
            IdentId::NEW,
            status_class,
            &[
                Value::integer(exit_status as i64),
                Value::integer(pid as i64),
            ],
            None,
            None,
        )?;
        globals.set_gvar(IdentId::get_id("$?"), status_obj);
    }
    Ok(Value::nil())
}

///
/// ### IO#closed?
///
/// ### IO#close_write
#[monoruby_builtin]
fn close_write(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let mut self_ = lfp.self_val();
    let io = self_.as_io_inner_mut();
    let fully_closed = match io {
        IoInner::Popen(popen) => {
            let popen = Rc::get_mut(popen).unwrap();
            popen.writer = None;
            popen.reader.is_none()
        }
        IoInner::Closed => return Err(MonorubyErr::ioerr("closed stream")),
        _ => return Err(MonorubyErr::ioerr("closing non-duplex IO for writing")),
    };
    if fully_closed {
        *io = IoInner::Closed;
    }
    Ok(Value::nil())
}

/// ### IO#close_read
#[monoruby_builtin]
fn close_read(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let mut self_ = lfp.self_val();
    let io = self_.as_io_inner_mut();
    let fully_closed = match io {
        IoInner::Popen(popen) => {
            let popen = Rc::get_mut(popen).unwrap();
            popen.reader = None;
            popen.writer.is_none()
        }
        IoInner::Closed => return Err(MonorubyErr::ioerr("closed stream")),
        _ => return Err(MonorubyErr::ioerr("closing non-duplex IO for reading")),
    };
    if fully_closed {
        *io = IoInner::Closed;
    }
    Ok(Value::nil())
}

/// - closed? -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/closed=3f.html]
#[monoruby_builtin]
fn closed_(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::bool(lfp.self_val().as_io_inner().is_closed()))
}

#[monoruby_builtin]
fn assign_sync(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    ensure_io_open(lfp.self_val())?;
    Ok(lfp.arg(0))
}

///
/// ### IO#seek
///
/// - seek(offset, whence = IO::SEEK_SET) -> 0
///
/// `whence` may be an Integer (`IO::SEEK_SET` = 0, `IO::SEEK_CUR` = 1,
/// `IO::SEEK_END` = 2) or one of the symbols `:SET`, `:CUR`, `:END`
/// (also `:START` / `:BEGIN` for 0).
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/seek.html]
#[monoruby_builtin]
fn seek(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    ensure_io_open(lfp.self_val())?;
    let offset = lfp.arg(0).coerce_to_int_i64(vm, globals)?;
    let whence = match lfp.try_arg(1) {
        None => 0i32,
        Some(v) if v.is_nil() => 0i32,
        Some(v) => {
            if let Some(sym) = v.try_symbol() {
                let name = sym.get_name();
                match name.as_str() {
                    "SET" | "START" | "BEGIN" => 0,
                    "CUR" => 1,
                    "END" => 2,
                    _ => {
                        return Err(MonorubyErr::argumenterr(format!(
                            "invalid whence: :{}",
                            name
                        )));
                    }
                }
            } else {
                v.coerce_to_int_i64(vm, globals)? as i32
            }
        }
    };
    let mut self_ = lfp.self_val();
    self_
        .as_io_inner_mut()
        .seek(offset, whence)
        .map_err(|e| MonorubyErr::errno_with_msg(&globals.store, &e, ""))?;
    Ok(Value::integer(0))
}

///
/// ### IO#read
///
/// - read(length = nil, [NOT SUPPRTED] outbuf = "") -> String | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/read.html
#[monoruby_builtin]
fn read(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let length = match lfp.try_arg(0) {
        Some(v) => {
            if v.is_nil() {
                None
            } else {
                let length = v.coerce_to_int_i64(vm, globals)?;
                if length < 0 {
                    return Err(MonorubyErr::argumenterr("negative length"));
                }
                Some(length as usize)
            }
        }
        None => None,
    };
    let buf = lfp.self_val().as_io_inner_mut().read(length)?;
    if buf.is_empty() && length.is_some() && length != Some(0) {
        return Ok(Value::nil());
    }
    Ok(tagged_read_string(
        globals,
        lfp.self_val(),
        buf,
        length.is_some(),
    ))
}

///
/// ### IO.read
///
/// - read(path) -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/s/read.html]
#[monoruby_builtin]
fn io_class_read(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    use std::io::{Read, Seek, SeekFrom};
    let filename = lfp
        .arg(0)
        .coerce_to_path_rstring(vm, globals)?
        .to_str()?
        .to_string();

    // Trailing options Hash (anywhere in args 1..4).
    let mut opts = None;
    for i in 1..4 {
        if let Some(a) = lfp.try_arg(i)
            && let Some(h) = a.try_hash_ty()
        {
            opts = Some(h);
        }
    }
    // length = arg1 (Integer); nil / Hash / absent => whole file.
    let length = match lfp.try_arg(1) {
        Some(v) if v.try_fixnum().is_some() => {
            let n = v.try_fixnum().unwrap();
            if n < 0 {
                return Err(MonorubyErr::argumenterr(format!("negative length {n} given")));
            }
            Some(n as usize)
        }
        _ => None,
    };
    // offset = arg2 (Integer); nil / absent => 0.
    let offset = match lfp.try_arg(2) {
        Some(v) if v.try_fixnum().is_some() => {
            let n = v.try_fixnum().unwrap();
            if n < 0 {
                return Err(MonorubyErr::argumenterr("negative offset"));
            }
            n as u64
        }
        _ => 0,
    };

    let (mode, ext_obj, int_obj) = class_read_opts(vm, globals, opts)?;
    // A write/append-only mode can't be used for reading.
    let base = mode.split(':').next().unwrap_or("").replace('b', "");
    if base == "w" || base == "a" {
        return Err(MonorubyErr::ioerr("not opened for reading"));
    }

    let mut file = File::open(&filename).map_err(|err| {
        MonorubyErr::errno_with_path(&globals.store, &err, "rb_sysopen", &filename)
    })?;
    if offset > 0 {
        file.seek(SeekFrom::Start(offset)).map_err(|err| {
            MonorubyErr::errno_with_path(&globals.store, &err, "rb_io_seek", &filename)
        })?;
    }

    if let Some(n) = length {
        let mut buf = Vec::new();
        Read::by_ref(&mut file)
            .take(n as u64)
            .read_to_end(&mut buf)
            .map_err(|e| MonorubyErr::errno_with_path(&globals.store, &e, "rb_io_read", &filename))?;
        if buf.is_empty() && n > 0 {
            return Ok(Value::nil());
        }
        // A sized read is always ASCII-8BIT.
        let mut s = Value::string_from_vec(buf);
        s.as_rstring_inner_mut()
            .set_encoding(crate::value::Encoding::Ascii8);
        return Ok(s);
    }

    let mut buf = Vec::new();
    file.read_to_end(&mut buf)
        .map_err(|e| MonorubyErr::errno_with_path(&globals.store, &e, "rb_io_read", &filename))?;

    use crate::value::Encoding as E;
    let ext = match ext_obj {
        Some(o) => enc_obj_to_enum(globals, o).unwrap_or(E::Utf8),
        None => {
            let de = enc_default_external_obj(globals);
            enc_obj_to_enum(globals, de).unwrap_or(E::Utf8)
        }
    };
    let intl = int_obj.and_then(|o| enc_obj_to_enum(globals, o));
    let (out, final_enc) = match intl {
        Some(i) if i != ext => {
            let topts = super::encoding::TranscodeOpts {
                invalid_replace: false,
                undef_replace: false,
                replace: None,
            };
            match super::encoding::transcode_bytes_with_opts(&buf, ext, i, &topts, &globals.store) {
                Ok(b) => (b, i),
                Err(_) => (buf, ext),
            }
        }
        _ => (buf, ext),
    };
    let mut s = Value::string_from_vec(out);
    s.as_rstring_inner_mut().set_encoding(final_enc);
    Ok(s)
}

///
/// ### IO.readlines
///
/// - readlines(path, [NOT SUPPORTED] sep = $/, [NOT SUPPORTED] limit = nil) -> [String]
///
/// Reads the entire file at `path` as a list of lines. Trailing arguments
/// that are Hashes (CRuby option / keyword forms like `chomp: true`) are
/// accepted but currently ignored.
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/s/readlines.html]
#[monoruby_builtin]
fn io_class_readlines(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let path = lfp.arg(0).coerce_to_str(vm, globals)?;
    // Validate any subsequent positional arguments. Hash arguments
    // (forwarded keyword opts) are accepted; Integer limit is accepted but
    // not enforced.
    for i in 1..3 {
        if let Some(arg) = lfp.try_arg(i)
            && !arg.is_nil()
            && arg.try_hash_ty().is_none()
            && arg.try_fixnum().is_none()
            && arg.is_rstring().is_none()
        {
            return Err(MonorubyErr::typeerr(format!(
                "no implicit conversion of {} into String",
                globals.get_class_name(arg.class()),
            )));
        }
    }
    let content = std::fs::read_to_string(&path)
        .map_err(|e| MonorubyErr::errno_with_path(&globals.store, &e, "rb_sysopen", &path))?;
    let mut lines: Vec<Value> = Vec::new();
    let mut start = 0;
    let bytes = content.as_bytes();
    while start < bytes.len() {
        if let Some(pos) = content[start..].find('\n') {
            let end = start + pos + 1;
            lines.push(Value::string(content[start..end].to_string()));
            start = end;
        } else {
            lines.push(Value::string(content[start..].to_string()));
            break;
        }
    }
    Ok(Value::array_from_vec(lines))
}

///
/// ### IO.foreach
///
/// - foreach(path, sep = "\n") {|line| ... } -> nil
/// - foreach(path, sep = "\n") -> Enumerator
///
/// Opens the file at `path`, reads each line separated by `sep`,
/// and yields each line to the block. Without a block, returns an Enumerator.
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/s/foreach.html]
#[monoruby_builtin]
fn io_foreach(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let path = lfp.arg(0).coerce_to_str(vm, globals)?;
    let bh = match lfp.block() {
        Some(bh) => bh,
        None => {
            return Err(MonorubyErr::runtimeerr(
                "IO.foreach without block is not yet supported",
            ));
        }
    };
    let p = vm.get_block_data(globals, bh)?;
    let content = std::fs::read_to_string(&path)
        .map_err(|e| MonorubyErr::runtimeerr(format!("{}: {}", path, e)))?;
    // arg1 may be a separator (String/nil), a limit (Integer), or an options
    // Hash forwarded from `**opts` (CRuby allows
    // `IO.foreach(path, chomp: true)` etc.). arg2, if present, is normally
    // the limit, but may also be the options Hash. Both options and limit
    // are currently accepted but not enforced — line slicing returns whole
    // lines and the chomp/mode flags are ignored.
    let sep = if let Some(sep_val) = lfp.try_arg(1) {
        if sep_val.is_nil() {
            None
        } else if sep_val.try_fixnum().is_some() {
            // arg1 is a limit, sep defaults to "\n".
            Some("\n".to_string())
        } else if sep_val.try_hash_ty().is_some() {
            // arg1 is the options hash; sep defaults to "\n".
            Some("\n".to_string())
        } else {
            Some(sep_val.coerce_to_str(vm, globals)?)
        }
    } else {
        Some("\n".to_string())
    };
    if let Some(arg2) = lfp.try_arg(2)
        && !arg2.is_nil()
        && arg2.try_hash_ty().is_none()
    {
        let _ = arg2.coerce_to_int_i64(vm, globals)?;
    }
    match sep {
        None => {
            // When sep is nil, yield the entire content as one string
            vm.invoke_block(globals, &p, &[Value::string(content)])?;
        }
        Some(sep) => {
            let mut start = 0;
            let content_bytes = content.as_bytes();
            let sep_bytes = sep.as_bytes();
            if sep_bytes.is_empty() {
                // Paragraph mode: split on double newlines
                let parts: Vec<&str> = content.split("\n\n").collect();
                for part in parts {
                    let trimmed = part.trim_start_matches('\n');
                    if !trimmed.is_empty() {
                        let mut line = trimmed.to_string();
                        line.push('\n');
                        vm.invoke_block(globals, &p, &[Value::string(line)])?;
                    }
                }
            } else {
                while start < content_bytes.len() {
                    if let Some(pos) = content[start..].find(&sep) {
                        let end = start + pos + sep.len();
                        let line = &content[start..end];
                        vm.invoke_block(globals, &p, &[Value::string(line.to_string())])?;
                        start = end;
                    } else {
                        // Last line without separator
                        let line = &content[start..];
                        vm.invoke_block(globals, &p, &[Value::string(line.to_string())])?;
                        break;
                    }
                }
            }
        }
    }
    Ok(Value::nil())
}

///
/// ### IO.pipe
///
/// ### IO.sysopen
///
/// - sysopen(path, mode_str = "r", perm = 0666) -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/s/sysopen.html]
#[monoruby_builtin]
fn io_sysopen(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    use std::os::unix::io::IntoRawFd;
    let path = lfp.arg(0).coerce_to_str(vm, globals)?;
    let mode_str = if let Some(m) = lfp.try_arg(1) {
        m.coerce_to_str(vm, globals)?
    } else {
        "r".to_string()
    };
    let mut opts = std::fs::OpenOptions::new();
    // Strip encoding suffix (e.g. ":UTF-8") and remove 'b' (binary) flag.
    let mode_base = mode_str.split(':').next().unwrap().replace('b', "");
    match mode_base.as_str() {
        "r" => {
            opts.read(true);
        }
        "w" => {
            opts.write(true).create(true).truncate(true);
        }
        "a" => {
            opts.append(true).create(true);
        }
        "r+" | "+r" => {
            opts.read(true).write(true);
        }
        "w+" | "+w" => {
            opts.read(true).write(true).create(true).truncate(true);
        }
        "a+" | "+a" => {
            opts.read(true).append(true).create(true);
        }
        _ => {
            opts.read(true);
        }
    }
    let file = opts
        .open(&path)
        .map_err(|e| MonorubyErr::errno_with_path(&globals.store, &e, "rb_sysopen", &path))?;
    let fd = file.into_raw_fd();
    Ok(Value::integer(fd as i64))
}

/// - pipe -> [read_io, write_io]
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/s/pipe.html]
#[monoruby_builtin]
fn io_pipe(_vm: &mut Executor, globals: &mut Globals, _lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let mut fds: [libc::c_int; 2] = [0; 2];
    // SAFETY: fds is a valid pointer to a 2-element array of c_int.
    let ret = unsafe { libc::pipe(fds.as_mut_ptr()) };
    if ret == -1 {
        let err = std::io::Error::last_os_error();
        return Err(MonorubyErr::errno_with_msg(&globals.store, &err, "pipe(2)"));
    }
    let read_io = Value::new_io(IoInner::from_raw_fd(
        fds[0],
        "pipe".to_string(),
        false,
        true,
        false,
    ));
    let write_io = Value::new_io(IoInner::from_raw_fd(
        fds[1],
        "pipe".to_string(),
        false,
        false,
        true,
    ));
    Ok(Value::array2(read_io, write_io))
}

/// ### IO.popen
///
/// - IO.popen(command) -> IO
/// - IO.popen(command) {|io| ... } -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/s/popen.html]
#[monoruby_builtin]
fn io_popen(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let raw_args = lfp.arg(0).as_array();
    if raw_args.is_empty() {
        return Err(MonorubyErr::argumenterr(
            "wrong number of arguments (given 0, expected 1+)",
        ));
    }
    // CRuby `IO.popen([env,] cmd_or_argv [, mode] [, opts])`. When the first
    // argument is a Hash, treat it as the environment and shift everything
    // else left.
    let mut env_hash: Option<Value> = None;
    let mut idx = 0usize;
    if raw_args[0].try_hash_ty().is_some() {
        env_hash = Some(raw_args[0]);
        idx = 1;
    }
    if idx >= raw_args.len() {
        return Err(MonorubyErr::argumenterr(
            "wrong number of arguments (given 1, expected 2+)",
        ));
    }
    let args: Vec<Value> = raw_args.iter().skip(idx).cloned().collect();
    let cmd_val = args[0];

    // Build the command from either a String or an Array of strings.
    let (mut command, cmd_name) = if let Some(ary) = cmd_val.try_array_ty() {
        let parts: Vec<String> = ary.iter().map(|v| v.to_s(globals)).collect();
        if parts.is_empty() {
            return Err(MonorubyErr::argumenterr("popen: empty command array"));
        }
        let name = parts[0].clone();
        let mut cmd = Command::new(&parts[0]);
        for part in &parts[1..] {
            cmd.arg(part);
        }
        (cmd, name)
    } else {
        let cmd_str = cmd_val.coerce_to_str(vm, globals)?;
        let mut cmd = Command::new("sh");
        cmd.arg("-c").arg(cmd_str.to_string());
        (cmd, cmd_str)
    };

    // Apply the leading ENV Hash (after `command` exists so we can call .env).
    if let Some(env) = env_hash {
        let h = env.as_hash();
        for (k, v) in h.iter() {
            let key = k.to_s(globals);
            if v.is_nil() {
                command.env_remove(&key);
            } else {
                command.env(&key, v.to_s(globals));
            }
        }
    }

    // Parse mode and options.
    //   IO.popen(cmd)
    //   IO.popen(cmd, mode)
    //   IO.popen(cmd, mode, opts)
    //   IO.popen(cmd, opts)   -- Hash as second arg means spawn options
    let mut opts_hash = None;
    let mode = if args.len() > 1 {
        if args[1].try_hash_ty().is_some() {
            opts_hash = Some(args[1]);
            "r".to_string()
        } else {
            let m = args[1].coerce_to_str(vm, globals)?;
            if args.len() > 2 && args[2].try_hash_ty().is_some() {
                opts_hash = Some(args[2]);
            }
            m
        }
    } else {
        "r".to_string()
    };
    let readable = mode.contains('r') || mode.contains('+');
    let writable = mode.contains('w') || mode.contains('+');

    if writable {
        command.stdin(Stdio::piped());
    } else {
        command.stdin(Stdio::null());
    }
    if readable {
        command.stdout(Stdio::piped());
    } else {
        command.stdout(Stdio::inherit());
    }

    // Handle err: [:child, :out] option to redirect stderr to stdout
    let mut stderr_to_stdout = false;
    if let Some(opts) = opts_hash {
        let err_key = Value::symbol_from_str("err");
        if let Ok(Some(err_val)) = opts.as_hash().get(err_key, vm, globals) {
            if let Some(ary) = err_val.try_array_ty() {
                if ary.len() == 2
                    && ary[0].try_symbol_or_string() == Some(IdentId::get_id("child"))
                    && ary[1].try_symbol_or_string() == Some(IdentId::get_id("out"))
                {
                    stderr_to_stdout = true;
                }
            }
        }
    }
    if stderr_to_stdout {
        // SAFETY: dup2(1, 2) duplicates stdout fd to stderr fd in the child
        // process right before exec; no Rust state is touched.
        unsafe {
            command.pre_exec(|| {
                libc::dup2(1, 2);
                Ok(())
            });
        }
    }
    command.stderr(Stdio::inherit());

    let child = command
        .spawn()
        .map_err(|e| MonorubyErr::errno_with_path(&globals.store, &e, "rb_f_spawn", &cmd_name))?;

    let io_val = Value::new_io(IoInner::popen(child));

    if let Some(bh) = lfp.block() {
        let data = vm.get_block_data(globals, bh)?;
        let res = vm.invoke_block(globals, &data, &[io_val]);
        let mut io_close = io_val;
        if let Ok(Some((exit_status, pid))) = io_close.as_io_inner_mut().close() {
            if let Ok(status_class) =
                vm.get_qualified_constant(globals, OBJECT_CLASS, &["Process", "Status"])
            {
                if let Ok(status_obj) = vm.invoke_method_inner(
                    globals,
                    IdentId::NEW,
                    status_class,
                    &[
                        Value::integer(exit_status as i64),
                        Value::integer(pid as i64),
                    ],
                    None,
                    None,
                ) {
                    globals.set_gvar(IdentId::get_id("$?"), status_obj);
                }
            }
        }
        res
    } else {
        Ok(io_val)
    }
}

/// ### IO#pid
///
/// - pid -> Integer | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/pid.html]
#[monoruby_builtin]
fn io_pid(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    ensure_io_open(lfp.self_val())?;
    let self_ = lfp.self_val();
    match self_.as_io_inner().pid() {
        Some(pid) => Ok(Value::integer(pid as i64)),
        None => Ok(Value::nil()),
    }
}

/// ### IO#fileno
///
/// - fileno -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/fileno.html]
#[monoruby_builtin]
fn io_fileno(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let self_ = lfp.self_val();
    let fd = self_.as_io_inner().fileno()?;
    Ok(Value::integer(fd as i64))
}

///
/// ### IO#to_io
/// - to_io -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/to_io.html]
#[monoruby_builtin]
fn io_to_io(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    Ok(lfp.self_val())
}

///
/// ### IO#readlines
/// - readlines(rs = $/, [NOT SUPPORTED] limit, [NOT SUPPORTED] chomp: false) -> [String]
///
/// Reads all remaining lines from the stream and returns them as an Array
/// of String.
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/readlines.html]
#[monoruby_builtin]
fn io_readlines(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let mut self_ = lfp.self_val();
    let mut lines = Vec::new();
    while let Some(s) = self_.as_io_inner_mut().read_line()? {
        lines.push(s);
    }
    let result = lines
        .into_iter()
        .map(|s| tagged_read_string(globals, self_, s.into_bytes(), false))
        .collect();
    Ok(Value::array_from_vec(result))
}

///
/// ### IO#binmode
/// - binmode -> self
///
/// On platforms with text-mode/binary-mode distinction, switches the stream
/// to binary mode. monoruby treats all streams as binary, so this is a
/// no-op that returns self.
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/binmode.html]
#[monoruby_builtin]
fn io_binmode(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let self_ = lfp.self_val();
    ensure_io_open(self_)?;
    // CRuby: binmode forces external = ASCII-8BIT, internal = nil.
    let bin = enc_by_name(globals, "ASCII-8BIT")
        .map(ExtSlot::Fixed)
        .unwrap_or(ExtSlot::Nil);
    store_io_encodings(globals, self_, bin, None);
    let _ = globals
        .store
        .set_ivar(self_, IdentId::get_id(BINMODE_IVAR), Value::bool(true));
    Ok(self_)
}

///
/// ### IO#binmode?
/// - binmode? -> bool
///
/// True once the stream has been put in binary mode (opened with a `b`
/// mode flag or via `#binmode`).
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/binmode=3f.html]
#[monoruby_builtin]
fn io_binmode_(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    ensure_io_open(lfp.self_val())?;
    let b = globals
        .store
        .get_ivar(lfp.self_val(), IdentId::get_id(BINMODE_IVAR))
        .map(|v| v.as_bool())
        .unwrap_or(false);
    Ok(Value::bool(b))
}

///
/// ### IO#autoclose=
/// - autoclose=(bool) -> bool
///
/// Set the autoclose flag. When `false`, the fd is NOT closed when the IO
/// is closed or garbage-collected — the caller is then responsible for the
/// fd's lifetime. Required by the `File.new(other.fileno, ...)` pattern
/// (e.g. `logger/log_device.rb#fixup_mode`) where the original IO must
/// relinquish ownership of the fd to the new wrapper.
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/autoclose=3d.html]
#[monoruby_builtin]
fn io_autoclose_set(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    ensure_io_open(lfp.self_val())?;
    let value = lfp.arg(0).as_bool();
    lfp.self_val().as_io_inner().set_autoclose(value);
    Ok(Value::bool(value))
}

///
/// ### IO#autoclose?
/// - autoclose? -> bool
///
/// Returns the current autoclose flag. Defaults to `true`; only changes via
/// `IO#autoclose=`. Stdio / popen / closed IOs always report `true` because
/// their fd lifetime is not owned by the IO object.
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/autoclose=3f.html]
#[monoruby_builtin]
fn io_autoclose_(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    ensure_io_open(lfp.self_val())?;
    Ok(Value::bool(lfp.self_val().as_io_inner().is_autoclose()))
}

///
/// ### IO#advise
/// - advise(advice, offset = 0, length = 0) -> nil
///
/// `posix_fadvise(2)` hint. Recognized advice symbols: `:normal`,
/// `:sequential`, `:random`, `:willneed`, `:dontneed`, `:noreuse`. Other
/// values raise `RuntimeError`. monoruby validates the arguments but
/// does not actually issue the syscall.
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/advise.html]
#[monoruby_builtin]
fn io_advise(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    ensure_io_open(lfp.self_val())?;
    let arg0 = lfp.arg(0);
    let sym = arg0.try_symbol().ok_or_else(|| {
        MonorubyErr::typeerr(format!(
            "no implicit conversion of {} into Symbol",
            globals.get_class_name(arg0.class()),
        ))
    })?;
    let name = sym.get_name();
    match name.as_str() {
        "normal" | "sequential" | "random" | "willneed" | "dontneed" | "noreuse" => {}
        _ => {
            return Err(MonorubyErr::runtimeerr(format!(
                "advise: unknown advice :{}",
                name
            )));
        }
    }
    if let Some(arg1) = lfp.try_arg(1) {
        let _ = arg1.coerce_to_int_i64(vm, globals)?;
    }
    if let Some(arg2) = lfp.try_arg(2) {
        let _ = arg2.coerce_to_int_i64(vm, globals)?;
    }
    Ok(Value::nil())
}

///
/// ### IO#pos / IO#tell
/// - pos -> Integer
/// - tell -> Integer
///
/// Returns the current byte offset of the stream.
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/pos.html]
#[monoruby_builtin]
fn io_pos(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    ensure_io_open(lfp.self_val())?;
    let mut self_ = lfp.self_val();
    let io = self_.as_io_inner_mut();
    let pb = io.pushback_len() as i64;
    let pos = io
        .seek(0, 1)
        .map_err(|e| MonorubyErr::errno_with_msg(&globals.store, &e, ""))?;
    Ok(Value::integer((pos as i64 - pb).max(0)))
}

///
/// ### IO#pos=
/// - pos=(n) -> Integer
///
/// Seeks to absolute byte offset `n`.
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/pos=3d.html]
#[monoruby_builtin]
fn io_pos_set(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    ensure_io_open(lfp.self_val())?;
    let n = lfp.arg(0).coerce_to_int_i64(vm, globals)?;
    let mut self_ = lfp.self_val();
    self_
        .as_io_inner_mut()
        .seek(n, 0)
        .map_err(|e| MonorubyErr::errno_with_msg(&globals.store, &e, ""))?;
    Ok(Value::integer(n))
}

///
/// ### IO#rewind
/// - rewind -> 0
///
/// Seeks to the beginning of the stream.
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/rewind.html]
#[monoruby_builtin]
fn io_rewind(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    ensure_io_open(lfp.self_val())?;
    let mut self_ = lfp.self_val();
    self_
        .as_io_inner_mut()
        .seek(0, 0)
        .map_err(|e| MonorubyErr::errno_with_msg(&globals.store, &e, ""))?;
    globals
        .store
        .set_ivar(self_, IdentId::get_id("/lineno"), Value::integer(0))?;
    Ok(Value::integer(0))
}

///
/// ### IO#eof? / IO#eof
/// - eof? -> bool
/// - eof -> bool
///
/// Returns `true` when the stream cursor is past the last byte. For seekable
/// streams the test is non-destructive; for pipes/stdin a successfully read
/// byte may be consumed.
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/eof.html]
#[monoruby_builtin]
fn io_eof_(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let mut self_ = lfp.self_val();
    let io = self_.as_io_inner_mut();
    if io.pushback_len() > 0 {
        return Ok(Value::bool(false));
    }
    // Read 1 byte; if empty, we're at EOF. Then push it back via seek(-1).
    let buf = io.read(Some(1))?;
    if buf.is_empty() {
        return Ok(Value::bool(true));
    }
    // Try to seek back. If seek isn't supported (pipe/stdin), accept that the
    // byte is consumed — matches CRuby's pipe semantics where eof? blocks
    // for a read and discards the byte if it appears.
    let _ = io.seek(-1, 1);
    Ok(Value::bool(false))
}

///
/// ### IO#getbyte
/// - getbyte -> Integer | nil
///
/// Reads one byte, returning its value as an Integer or `nil` at EOF.
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/getbyte.html]
#[monoruby_builtin]
fn io_getbyte(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let mut self_ = lfp.self_val();
    let io = self_.as_io_inner_mut();
    let buf = io.read(Some(1))?;
    if buf.is_empty() {
        Ok(Value::nil())
    } else {
        Ok(Value::integer(buf[0] as i64))
    }
}

/// Read a single UTF-8 character (1-4 bytes) from the IO. Returns the bytes
/// of the character, an empty Vec on EOF, or the first byte if the byte
/// sequence is not valid UTF-8 (matching CRuby's invalid-byte behavior in
/// binary-mode reads, which is what monoruby uses everywhere).
fn read_one_char(io: &mut IoInner) -> Result<Vec<u8>> {
    let first = io.read(Some(1))?;
    if first.is_empty() {
        return Ok(vec![]);
    }
    let b = first[0];
    let total = if b & 0x80 == 0 {
        1
    } else if b & 0xE0 == 0xC0 {
        2
    } else if b & 0xF0 == 0xE0 {
        3
    } else if b & 0xF8 == 0xF0 {
        4
    } else {
        return Ok(first);
    };
    let mut buf = first;
    while buf.len() < total {
        let next = io.read(Some(total - buf.len()))?;
        if next.is_empty() {
            break;
        }
        buf.extend_from_slice(&next);
    }
    Ok(buf)
}

/// Per-encoding character width from the lead byte (mirrors
/// `CharByteIter::next`). UTF-8 is handled separately because it needs
/// post-read validation; this returns 0 for it.
fn char_width_from_lead(enc: crate::value::Encoding, b: u8) -> usize {
    use crate::value::Encoding as E;
    match enc {
        E::Ascii8 | E::UsAscii | E::Iso8859(_) | E::Other(_) | E::Iso2022Jp => 1,
        E::EucJp => match b {
            0x8e => 2,
            0x8f => 3,
            0xa1..=0xfe => 2,
            _ => 1,
        },
        E::Sjis(_) => match b {
            0x81..=0x9f | 0xe0..=0xfc => 2,
            _ => 1,
        },
        E::Utf16Le | E::Utf16Be => 2,
        E::Utf32Le | E::Utf32Be => 4,
        E::Utf8 => 0,
    }
}

/// Read one character of `enc` from the stream. UTF-8 delegates to
/// `read_one_char` so the (validated, EOF-tolerant) UTF-8 path stays
/// byte-identical to before.
fn read_one_char_enc(io: &mut IoInner, enc: crate::value::Encoding) -> Result<Vec<u8>> {
    if enc == crate::value::Encoding::Utf8 {
        return read_one_char(io);
    }
    let first = io.read(Some(1))?;
    if first.is_empty() {
        return Ok(vec![]);
    }
    let total = char_width_from_lead(enc, first[0]);
    let mut buf = first;
    while buf.len() < total {
        let next = io.read(Some(total - buf.len()))?;
        if next.is_empty() {
            break;
        }
        buf.extend_from_slice(&next);
    }
    Ok(buf)
}

///
/// ### IO#getc
/// - getc -> String | nil
///
/// Reads one character in the stream's external encoding and returns it
/// as a String (transcoded to the internal encoding when one is set),
/// or `nil` at EOF.
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/getc.html]
#[monoruby_builtin]
fn io_getc(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let mut self_ = lfp.self_val();
    let ext_obj = read_io_encoding(globals, self_, false);
    let ext = enc_obj_to_enum(globals, ext_obj).unwrap_or(crate::value::Encoding::Utf8);
    let buf = read_one_char_enc(self_.as_io_inner_mut(), ext)?;
    if buf.is_empty() {
        Ok(Value::nil())
    } else {
        Ok(tagged_read_string(globals, self_, buf, false))
    }
}

///
/// ### IO#ungetbyte
/// - ungetbyte(string or integer) -> nil
///
/// Pushes bytes back so the next read returns them first. An Integer is
/// reduced modulo 256 (never raises RangeError); `nil` is a no-op.
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/ungetbyte.html]
#[monoruby_builtin]
fn io_ungetbyte(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let arg = lfp.arg(0);
    if arg.is_nil() {
        return Ok(Value::nil());
    }
    let bytes = if arg.is_integer() {
        vec![(arg.coerce_to_pack_u64(vm, globals)? & 0xff) as u8]
    } else {
        arg.coerce_to_string(vm, globals)?.into_bytes()
    };
    lfp.self_val().as_io_inner_mut().unget(&bytes)?;
    Ok(Value::nil())
}

///
/// ### IO#ungetc
/// - ungetc(string or integer) -> nil
///
/// Pushes a character back so the next read returns it first. An Integer
/// is interpreted as a codepoint.
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/ungetc.html]
#[monoruby_builtin]
fn io_ungetc(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let arg = lfp.arg(0);
    if arg.is_nil() {
        return Err(MonorubyErr::typeerr(
            "no implicit conversion of nil into Integer",
        ));
    }
    let bytes = if arg.is_integer() {
        let cp = (arg.coerce_to_pack_u64(vm, globals)? & 0xffff_ffff) as u32;
        match char::from_u32(cp) {
            Some(c) => c.to_string().into_bytes(),
            None => vec![cp as u8],
        }
    } else {
        arg.coerce_to_string(vm, globals)?.into_bytes()
    };
    lfp.self_val().as_io_inner_mut().unget(&bytes)?;
    Ok(Value::nil())
}

///
/// ### IO#sysseek
/// - sysseek(offset, whence = IO::SEEK_SET) -> Integer
///
/// Seeks via `lseek(2)` directly, bypassing any user-space buffering.
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/sysseek.html]
#[monoruby_builtin]
fn io_sysseek(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    ensure_io_open(lfp.self_val())?;
    let offset = lfp.arg(0).coerce_to_int_i64(vm, globals)?;
    let whence = match lfp.try_arg(1) {
        None => 0i32,
        Some(v) if v.is_nil() => 0i32,
        Some(v) => v.coerce_to_int_i64(vm, globals)? as i32,
    };
    let mut self_ = lfp.self_val();
    let pos = self_
        .as_io_inner_mut()
        .seek(offset, whence)
        .map_err(|e| MonorubyErr::errno_with_msg(&globals.store, &e, ""))?;
    Ok(Value::integer(pos as i64))
}

///
/// ### IO#lineno
/// - lineno -> Integer
///
/// Returns the current line number: incremented by each `gets`/`readline`
/// (and the default-separator `each_line`), reset to 0 by `rewind`, and
/// overridable via `lineno=`.
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/lineno.html]
#[monoruby_builtin]
fn io_lineno(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    lfp.self_val().as_io_inner().ensure_readable()?;
    let stored = globals
        .store
        .get_ivar(lfp.self_val(), IdentId::get_id("/lineno"));
    Ok(stored.unwrap_or_else(|| Value::integer(0)))
}

///
/// ### IO#lineno=
/// - lineno=(n) -> Integer
///
/// Stores `n` as the value returned by subsequent `lineno` reads.
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/lineno=3d.html]
#[monoruby_builtin]
fn io_lineno_set(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    lfp.self_val().as_io_inner().ensure_readable()?;
    let n = lfp.arg(0).coerce_to_int_i64(vm, globals)?;
    if n > i32::MAX as i64 || n < i32::MIN as i64 {
        return Err(MonorubyErr::rangeerr(format!(
            "integer {n} too big to convert to `int'"
        )));
    }
    globals.store.set_ivar(
        lfp.self_val(),
        IdentId::get_id("/lineno"),
        Value::integer(n),
    )?;
    Ok(Value::integer(n))
}

///
/// ### IO#path / IO#to_path
/// - path -> String | nil
/// - to_path -> String | nil
///
/// Returns the path associated with the IO, the pseudo-path for the
/// standard streams (`<STDIN>` etc.), or `nil` for pipes/`popen`/raw fds
/// without an explicit `path:` and for closed streams.
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/path.html]
#[monoruby_builtin]
fn io_path(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(match lfp.self_val().as_io_inner().path() {
        Some(p) => Value::string(p),
        None => Value::nil(),
    })
}

///
/// ### IO#fsync
/// - fsync -> 0
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/fsync.html]
#[monoruby_builtin]
fn io_fsync(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let ret = lfp.self_val().as_io_inner_mut().fsync(false)?;
    Ok(Value::integer(ret as i64))
}

///
/// ### IO#fdatasync
/// - fdatasync -> 0
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/fdatasync.html]
#[monoruby_builtin]
fn io_fdatasync(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let ret = lfp.self_val().as_io_inner_mut().fsync(true)?;
    Ok(Value::integer(ret as i64))
}

///
/// ### IO#close_on_exec?
/// - close_on_exec? -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/close_on_exec=3f.html]
#[monoruby_builtin]
fn io_close_on_exec_(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    Ok(Value::bool(lfp.self_val().as_io_inner().close_on_exec()?))
}

///
/// ### IO#close_on_exec=
/// - close_on_exec = bool -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/close_on_exec=3d.html]
#[monoruby_builtin]
fn io_close_on_exec_set(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let v = lfp.arg(0);
    lfp.self_val().as_io_inner().set_close_on_exec(v.as_bool())?;
    Ok(v)
}

/// ### IO#write
///
/// - write(*str) -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/write.html]
#[monoruby_builtin]
fn io_write_method(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let mut self_ = lfp.self_val();
    let io = self_.as_io_inner_mut();
    let args = lfp.arg(0).as_array();
    let mut total = 0i64;
    for v in args.iter().cloned() {
        let bytes = if let Some(b) = v.try_bytes() {
            b.to_vec()
        } else {
            let s = vm.to_s(globals, v)?;
            s.into_bytes()
        };
        total += bytes.len() as i64;
        io.write(&bytes)?;
    }
    Ok(Value::integer(total))
}

/// ### IO#syswrite
///
/// - syswrite(string) -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/syswrite.html]
#[monoruby_builtin]
fn io_syswrite(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let mut self_ = lfp.self_val();
    let io = self_.as_io_inner_mut();
    let bytes = if let Some(b) = lfp.arg(0).try_bytes() {
        b.to_vec()
    } else {
        let s = vm.to_s(globals, lfp.arg(0))?;
        s.into_bytes()
    };
    let fd = io.fileno()?;
    // SAFETY: fd is a valid file descriptor, bytes is a valid buffer.
    let written = unsafe { libc::write(fd, bytes.as_ptr() as *const libc::c_void, bytes.len()) };
    if written < 0 {
        let err = std::io::Error::last_os_error();
        return Err(MonorubyErr::errno_with_msg(
            &globals.store,
            &err,
            "syswrite",
        ));
    }
    Ok(Value::integer(written as i64))
}

/// Helper: extract raw fd from a Value that is an IO (or responds to to_io).
fn value_to_fd(globals: &Globals, v: Value) -> Result<i32> {
    if let Some(rv) = v.try_rvalue() {
        if rv.ty() == ObjTy::IO {
            return v.as_io_inner().fileno();
        }
    }
    Err(MonorubyErr::typeerr(format!(
        "no implicit conversion of {} into IO",
        globals.get_class_name(v.class())
    )))
}

/// ### IO.select
///
/// - IO.select(read_array [, write_array [, error_array [, timeout]]]) -> array or nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/s/select.html]
#[monoruby_builtin]
fn io_select(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let read_arg = lfp.arg(0);
    let write_arg = lfp.try_arg(1).unwrap_or(Value::nil());
    let error_arg = lfp.try_arg(2).unwrap_or(Value::nil());
    let timeout_arg = lfp.try_arg(3).unwrap_or(Value::nil());

    // Parse timeout
    let timeout = if timeout_arg.is_nil() {
        None // block forever
    } else {
        let f = timeout_arg.coerce_to_f64(vm, globals)?;
        if f.is_nan() {
            return Err(MonorubyErr::rangeerr("NaN out of Time range"));
        }
        if f < 0.0 {
            return Err(MonorubyErr::argumenterr(
                "time interval must not be negative",
            ));
        }
        let secs = f.floor() as libc::time_t;
        let usecs = ((f - f.floor()) * 1_000_000.0) as libc::suseconds_t;
        Some(libc::timeval {
            tv_sec: secs,
            tv_usec: usecs,
        })
    };

    // Collect IO values and their fds
    let read_ios: Vec<Value> = if read_arg.is_nil() {
        vec![]
    } else {
        let ary = read_arg
            .try_array_ty()
            .ok_or_else(|| MonorubyErr::typeerr("no implicit conversion of Object into Array"))?;
        ary.to_vec()
    };

    let write_ios: Vec<Value> = if write_arg.is_nil() {
        vec![]
    } else {
        let ary = write_arg
            .try_array_ty()
            .ok_or_else(|| MonorubyErr::typeerr("no implicit conversion of Object into Array"))?;
        ary.to_vec()
    };

    let error_ios: Vec<Value> = if error_arg.is_nil() {
        vec![]
    } else {
        let ary = error_arg
            .try_array_ty()
            .ok_or_else(|| MonorubyErr::typeerr("no implicit conversion of Object into Array"))?;
        ary.to_vec()
    };

    // Get fds
    let read_fds: Vec<i32> = read_ios
        .iter()
        .map(|v| value_to_fd(globals, *v))
        .collect::<Result<Vec<_>>>()?;
    let write_fds: Vec<i32> = write_ios
        .iter()
        .map(|v| value_to_fd(globals, *v))
        .collect::<Result<Vec<_>>>()?;
    let error_fds: Vec<i32> = error_ios
        .iter()
        .map(|v| value_to_fd(globals, *v))
        .collect::<Result<Vec<_>>>()?;

    // If all arrays are empty with no timeout, return nil immediately.
    // In CRuby this would block forever, but monoruby is single-threaded
    // so nothing could ever wake us up.
    if read_ios.is_empty() && write_ios.is_empty() && error_ios.is_empty() && timeout.is_none() {
        return Ok(Value::nil());
    }

    // Find max fd
    let nfds = read_fds
        .iter()
        .chain(write_fds.iter())
        .chain(error_fds.iter())
        .copied()
        .max()
        .map(|m| m + 1)
        .unwrap_or(0);

    // SAFETY: libc fd_set operations are standard POSIX.
    unsafe {
        let mut readfds: libc::fd_set = std::mem::zeroed();
        let mut writefds: libc::fd_set = std::mem::zeroed();
        let mut errorfds: libc::fd_set = std::mem::zeroed();

        libc::FD_ZERO(&mut readfds);
        libc::FD_ZERO(&mut writefds);
        libc::FD_ZERO(&mut errorfds);

        for &fd in &read_fds {
            libc::FD_SET(fd, &mut readfds);
        }
        for &fd in &write_fds {
            libc::FD_SET(fd, &mut writefds);
        }
        for &fd in &error_fds {
            libc::FD_SET(fd, &mut errorfds);
        }

        let mut tv_storage = timeout.unwrap_or(libc::timeval {
            tv_sec: 0,
            tv_usec: 0,
        });
        let timeout_ptr = if timeout.is_some() {
            &mut tv_storage as *mut libc::timeval
        } else {
            std::ptr::null_mut()
        };

        let ret = libc::select(
            nfds,
            if read_fds.is_empty() {
                std::ptr::null_mut()
            } else {
                &mut readfds
            },
            if write_fds.is_empty() {
                std::ptr::null_mut()
            } else {
                &mut writefds
            },
            if error_fds.is_empty() {
                std::ptr::null_mut()
            } else {
                &mut errorfds
            },
            timeout_ptr,
        );

        if ret < 0 {
            let err = std::io::Error::last_os_error();
            return Err(MonorubyErr::errno_with_msg(
                &globals.store,
                &err,
                "select(2)",
            ));
        }

        if ret == 0 {
            return Ok(Value::nil());
        }

        // Build result arrays
        let mut ready_read = vec![];
        for (i, &fd) in read_fds.iter().enumerate() {
            if libc::FD_ISSET(fd, &readfds) {
                ready_read.push(read_ios[i]);
            }
        }
        let mut ready_write = vec![];
        for (i, &fd) in write_fds.iter().enumerate() {
            if libc::FD_ISSET(fd, &writefds) {
                ready_write.push(write_ios[i]);
            }
        }
        let mut ready_error = vec![];
        for (i, &fd) in error_fds.iter().enumerate() {
            if libc::FD_ISSET(fd, &errorfds) {
                ready_error.push(error_ios[i]);
            }
        }

        let r = Value::array_from_vec(ready_read);
        let w = Value::array_from_vec(ready_write);
        let e = Value::array_from_vec(ready_error);
        Ok(Value::array_from_vec(vec![r, w, e]))
    }
}

///
/// ### IO#set_encoding
///
/// - set_encoding(ext_enc) -> self
/// - set_encoding(ext_enc, int_enc) -> self
/// - set_encoding(ext_enc, int_enc, opt) -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/set_encoding.html]
#[monoruby_builtin]
fn set_encoding(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let self_ = lfp.self_val();
    let arg0 = lfp.arg(0);
    let (mut ext, mut int) = (None, None);
    if let Some(s) = arg0.is_str() {
        // A single string may carry both as "ext:int".
        let mut parts = s.split(':');
        if let Some(e) = parts.next().filter(|x| !x.is_empty()) {
            ext = Some(enc_by_name(globals, e).ok_or_else(|| {
                MonorubyErr::argumenterr(format!("unknown encoding name - {e}"))
            })?);
        }
        if let Some(i) = parts.next().filter(|x| !x.is_empty()) {
            int = Some(enc_by_name(globals, i).ok_or_else(|| {
                MonorubyErr::argumenterr(format!("unknown encoding name - {i}"))
            })?);
        }
    } else if !arg0.is_nil() {
        ext = arg_to_enc_obj(globals, arg0);
    }
    if int.is_none()
        && let Some(arg1) = lfp.try_arg(1)
        && !arg1.is_nil()
    {
        int = arg_to_enc_obj(globals, arg1);
    }
    let readable = self_.as_io_inner().is_readable();
    let de = enc_default_external_obj(globals);
    let di = enc_default_internal_obj(globals);
    let (slot, i) = resolve_io_encodings(globals, ext, int, false, readable, de, di, false);
    store_io_encodings(globals, self_, slot, i);
    Ok(self_)
}

/// An Encoding object -> the internal `Encoding` enum (folding the
/// names the enum doesn't represent distinctly down to ASCII-8BIT).
fn enc_obj_to_enum(globals: &Globals, v: Value) -> Option<crate::value::Encoding> {
    super::encoding::encoding_object_name(globals, v)
        .and_then(|n| crate::value::Encoding::try_from_str(&n).ok())
}

/// Build the String returned by a read: tag it with the IO's external
/// encoding, transcoding external -> internal when an internal encoding
/// is set. `sized` reads (an explicit byte count) return ASCII-8BIT,
/// matching CRuby.
fn tagged_read_string(
    globals: &mut Globals,
    io: Value,
    bytes: Vec<u8>,
    sized: bool,
) -> Value {
    use crate::value::Encoding as E;
    if sized {
        let mut s = Value::string_from_vec(bytes);
        s.as_rstring_inner_mut().set_encoding(E::Ascii8);
        return s;
    }
    let ext_v = read_io_encoding(globals, io, false);
    let int_v = read_io_encoding(globals, io, true);
    let ext = enc_obj_to_enum(globals, ext_v).unwrap_or(E::Utf8);
    let intl = if int_v.is_nil() {
        None
    } else {
        enc_obj_to_enum(globals, int_v)
    };
    let (out, final_enc) = match intl {
        Some(i) if i != ext => {
            let opts = super::encoding::TranscodeOpts {
                invalid_replace: false,
                undef_replace: false,
                replace: None,
            };
            match super::encoding::transcode_bytes_with_opts(
                &bytes,
                ext,
                i,
                &opts,
                &globals.store,
            ) {
                Ok(b) => (b, i),
                Err(_) => (bytes, ext),
            }
        }
        _ => (bytes, ext),
    };
    let mut s = Value::string_from_vec(out);
    s.as_rstring_inner_mut().set_encoding(final_enc);
    s
}

/// Read `/enc_ext` / `/enc_int`, resolving the live-default marker.
fn read_io_encoding(globals: &mut Globals, io: Value, internal: bool) -> Value {
    let id = IdentId::get_id(if internal { ENC_INT_IVAR } else { ENC_EXT_IVAR });
    match globals.store.get_ivar(io, id) {
        None => {
            // IOs not created through our path (pipe/popen/stdio):
            // external defaults to default_external, internal to nil.
            if internal {
                Value::nil()
            } else {
                enc_default_external_obj(globals)
            }
        }
        Some(v) => {
            if !internal && v.try_symbol() == Some(IdentId::get_id(ENC_DYN_MARKER)) {
                enc_default_external_obj(globals)
            } else {
                v
            }
        }
    }
}

///
/// ### IO#external_encoding
/// - external_encoding -> Encoding | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/external_encoding.html]
#[monoruby_builtin]
fn external_encoding(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    Ok(read_io_encoding(globals, lfp.self_val(), false))
}

///
/// ### IO#internal_encoding
/// - internal_encoding -> Encoding | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/internal_encoding.html]
#[monoruby_builtin]
fn internal_encoding(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    Ok(read_io_encoding(globals, lfp.self_val(), true))
}

///

/// Parse the optional timeout argument shared by `IO#wait`, `#wait_readable`,
/// `#wait_writable`, and `#wait_priority`. `nil` / missing means block
/// indefinitely; a numeric value is converted to a `poll(2)` millisecond
/// timeout (capped at `i32::MAX`).
fn parse_wait_timeout(
    vm: &mut Executor,
    globals: &mut Globals,
    arg: Option<Value>,
) -> Result<i32> {
    let Some(v) = arg else {
        return Ok(-1);
    };
    if v.is_nil() {
        return Ok(-1);
    }
    let f = v.coerce_to_f64(vm, globals)?;
    if f.is_nan() {
        return Err(MonorubyErr::rangeerr("NaN out of Time range"));
    }
    if f < 0.0 {
        return Err(MonorubyErr::argumenterr(
            "time interval must not be negative",
        ));
    }
    let ms = (f * 1000.0).ceil();
    let capped = if ms > i32::MAX as f64 {
        i32::MAX
    } else {
        ms as i32
    };
    Ok(capped)
}

/// Raise `IOError: closed stream` if the receiver IO is closed.
///
/// Shared guard for the query / positioning methods CRuby rejects on a
/// closed stream (`#pos`, `#pos=`, `#seek`, `#rewind`, `#sysseek`,
/// `#lineno`, `#lineno=`, `#pid`, `#tty?`, `#binmode`, `#binmode?`,
/// `#autoclose?`, `#autoclose=`, `#advise`, `#sync=`). Without this the
/// underlying syscalls fail with `Errno::EBADF` or silently return.
fn ensure_io_open(self_val: Value) -> Result<()> {
    if self_val.as_io_inner().is_closed() {
        return Err(MonorubyErr::ioerr("closed stream"));
    }
    Ok(())
}

/// Wait on a single fd via `poll(2)` with the given `events` mask (mixing
/// `POLLIN`/`POLLPRI`/`POLLOUT`) and a timeout in milliseconds. Returns the
/// revents from the kernel, or 0 on timeout. Closed / invalid fds raise
/// `IOError: closed stream`.
fn poll_single_fd(self_val: Value, events: i16, timeout_ms: i32) -> Result<i16> {
    if self_val.as_io_inner().is_closed() {
        return Err(MonorubyErr::ioerr("closed stream"));
    }
    let fd = self_val.as_io_inner().fileno()?;
    let mut pfd = libc::pollfd {
        fd,
        events,
        revents: 0,
    };
    // SAFETY: `poll` accepts a pointer to an array of pollfd; we pass
    // exactly one element with length 1.
    let ret = unsafe { libc::poll(&mut pfd, 1, timeout_ms) };
    if ret < 0 {
        let err = std::io::Error::last_os_error();
        if err.raw_os_error() == Some(libc::EINTR) {
            return Ok(0);
        }
        return Err(MonorubyErr::ioerr(format!("poll failed: {err}")));
    }
    if (pfd.revents & (libc::POLLNVAL as i16)) != 0 {
        return Err(MonorubyErr::ioerr("closed stream"));
    }
    Ok(pfd.revents)
}

/// Parse an `IO#wait` mode Symbol (`:r`, `:read`, `:readable`,
/// `:w`, `:write`, `:writable`, `:rw`, `:read_write`,
/// `:readable_writable`, `:p`, `:priority`) into the corresponding
/// `IO::READABLE`/`WRITABLE`/`PRIORITY` event bits.
fn mode_symbol_to_events(name: &str) -> Option<i64> {
    match name {
        "r" | "read" | "readable" => Some(1),
        "w" | "write" | "writable" => Some(4),
        "p" | "priority" => Some(2),
        "rw" | "read_write" | "readable_writable" => Some(1 | 4),
        _ => None,
    }
}

///
/// ### IO#wait
///
/// - wait(events, timeout = nil) -> Integer or nil
/// - wait(timeout = nil, *modes) -> self or nil
///
/// If called with an Integer event mask, returns the ready events mask
/// (or `nil` on timeout). If called with Symbol mode(s) (`:r`, `:w`,
/// `:rw`, ...) — optionally with a numeric timeout — returns `self`
/// on ready, `nil` on timeout.
#[monoruby_builtin]
fn io_wait(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    if self_val.as_io_inner().is_closed() {
        return Err(MonorubyErr::ioerr("closed stream"));
    }
    let args = lfp.arg(0).as_array();

    if args.is_empty() {
        let revents = poll_single_fd(self_val, libc::POLLIN as i16, -1)?;
        return if revents == 0 {
            Ok(Value::nil())
        } else {
            Ok(self_val)
        };
    }

    // `wait` supports two shapes:
    //   * traditional: `wait(events, timeout=nil)` with events an Integer mask.
    //   * modern:      `wait(timeout=nil, *modes)` where modes are Symbols.
    // If any argument is a Symbol, use the modern form; otherwise traditional.
    let has_symbol = args.iter().any(|a| a.try_symbol().is_some());

    let mut events: i64 = 0;
    let mut timeout: Option<Value> = None;

    if has_symbol {
        for arg in args.iter().copied() {
            if let Some(sym) = arg.try_symbol() {
                let name = sym.get_name();
                match mode_symbol_to_events(&name) {
                    Some(bits) => events |= bits,
                    None => {
                        return Err(MonorubyErr::argumenterr(format!(
                            "unsupported mode: {name}"
                        )));
                    }
                }
            } else {
                if timeout.is_some() {
                    return Err(MonorubyErr::argumenterr("timeout given more than once"));
                }
                timeout = Some(arg);
            }
        }
    } else {
        if args.len() > 2 {
            return Err(MonorubyErr::argumenterr(format!(
                "wrong number of arguments (given {}, expected 0..2)",
                args.len()
            )));
        }
        events = args[0].coerce_to_int_i64(vm, globals)?;
        if events <= 0 {
            return Err(MonorubyErr::argumenterr(
                "Events must be positive integer!",
            ));
        }
        if args.len() == 2 {
            timeout = Some(args[1]);
        }
    }

    let timeout_ms = parse_wait_timeout(vm, globals, timeout)?;

    let mut poll_events: i16 = 0;
    if events & 1 != 0 {
        poll_events |= libc::POLLIN as i16;
    }
    if events & 2 != 0 {
        poll_events |= libc::POLLPRI as i16;
    }
    if events & 4 != 0 {
        poll_events |= libc::POLLOUT as i16;
    }

    let revents = poll_single_fd(self_val, poll_events, timeout_ms)?;

    if has_symbol {
        let ready = (revents & libc::POLLIN as i16) != 0
            || (revents & libc::POLLOUT as i16) != 0
            || (revents & libc::POLLPRI as i16) != 0;
        return if ready {
            Ok(self_val)
        } else {
            Ok(Value::nil())
        };
    }

    let mut mask: i64 = 0;
    if (revents & libc::POLLIN as i16) != 0 {
        mask |= 1;
    }
    if (revents & libc::POLLPRI as i16) != 0 {
        mask |= 2;
    }
    if (revents & libc::POLLOUT as i16) != 0 {
        mask |= 4;
    }
    if mask == 0 {
        return Ok(Value::nil());
    }
    Ok(Value::integer(mask))
}

///
/// ### IO#wait_readable
///
/// - wait_readable(timeout = nil) -> self or nil
///
#[monoruby_builtin]
fn io_wait_readable(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let self_val = lfp.self_val();
    let timeout_ms = parse_wait_timeout(vm, globals, lfp.try_arg(0))?;
    let revents = poll_single_fd(self_val, libc::POLLIN as i16, timeout_ms)?;
    if revents == 0 {
        return Ok(Value::nil());
    }
    Ok(self_val)
}

///
/// ### IO#wait_writable
///
/// - wait_writable(timeout = nil) -> self or nil
///
#[monoruby_builtin]
fn io_wait_writable(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let self_val = lfp.self_val();
    let timeout_ms = parse_wait_timeout(vm, globals, lfp.try_arg(0))?;
    let revents = poll_single_fd(self_val, libc::POLLOUT as i16, timeout_ms)?;
    if revents == 0 {
        return Ok(Value::nil());
    }
    Ok(self_val)
}

///
/// ### IO#wait_priority
///
/// - wait_priority(timeout = nil) -> self or nil
///
#[monoruby_builtin]
fn io_wait_priority(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let self_val = lfp.self_val();
    let timeout_ms = parse_wait_timeout(vm, globals, lfp.try_arg(0))?;
    let revents = poll_single_fd(self_val, libc::POLLPRI as i16, timeout_ms)?;
    if revents == 0 {
        return Ok(Value::nil());
    }
    Ok(self_val)
}

///
/// ### IO.copy_stream
///
/// - IO.copy_stream(src, dst) -> Integer
/// - IO.copy_stream(src, dst, copy_length) -> Integer
/// - IO.copy_stream(src, dst, copy_length, src_offset) -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/s/copy_stream.html]
#[monoruby_builtin]
fn io_copy_stream(_: &mut Executor, _: &mut Globals, _: Lfp, _: BytecodePtr) -> Result<Value> {
    return Err(MonorubyErr::runtimeerr(
        "IO.copy_stream is not yet supported",
    ));
    //let copy_length: Option<i64> = lfp
    //    .try_arg(2)
    //    .and_then(|v| if v.is_nil() { None } else { Some(v) })
    //    .map(|v| v.coerce_to_int_i64(vm, globals))
    //    .transpose()?;
    //let src_offset: Option<i64> = lfp
    //    .try_arg(3)
    //    .and_then(|v| if v.is_nil() { None } else { Some(v) })
    //    .map(|v| v.coerce_to_int_i64(vm, globals))
    //    .transpose()?;

    // Helper: duplicate an fd from an IO Value for use as a std::fs::File.
    //fn dup_fd_from_io(io_val: Value, globals: &Globals) -> Result<File> {
    //    let mut v = io_val;
    //    if v.try_rvalue().map_or(true, |rv| rv.ty() != ObjTy::IO) {
    //        return Err(MonorubyErr::typeerr(format!(
    //            "no implicit conversion of {} into IO",
    //            v.get_real_class_name(&globals.store)
    //        )));
    //    }
    //    let fd = v.as_io_inner_mut().fileno()?;
    //    // SAFETY: dup the fd so that the original IO still owns its fd.
    //    let new_fd = unsafe { libc::dup(fd) };
    //    if new_fd == -1 {
    //        return Err(MonorubyErr::runtimeerr("dup failed"));
    //    }
    //    // SAFETY: new_fd is a valid, newly duplicated file descriptor.
    //    Ok(unsafe { File::from_raw_fd(new_fd) })
    //}

    // Open source
    //let src_val = lfp.arg(0);
    //let mut src_owned: File;
    //let src_is_path = src_val.try_bytes().is_some();
    //if src_is_path {
    //    let path = src_val.coerce_to_string(vm, globals)?;
    //    src_owned = File::open(&path)
    //        .map_err(|e| MonorubyErr::errno_with_path(&globals.store, &e, "rb_sysopen", &path))?;
    //    if let Some(offset) = src_offset {
    //        use std::io::Seek;
    //        src_owned
    //            .seek(std::io::SeekFrom::Start(offset as u64))
    //            .map_err(|e| MonorubyErr::runtimeerr(e.to_string()))?;
    //    }
    //} else {
    //    src_owned = dup_fd_from_io(src_val, globals)?;
    //    if let Some(offset) = src_offset {
    //        use std::io::Seek;
    //        src_owned
    //            .seek(std::io::SeekFrom::Start(offset as u64))
    //            .map_err(|e| MonorubyErr::runtimeerr(e.to_string()))?;
    //    }
    //}

    // Open destination
    //let dst_val = lfp.arg(1);
    //let dst_is_path = dst_val.try_bytes().is_some();
    //let mut dst_owned: File;
    //if dst_is_path {
    //    let path = dst_val.coerce_to_string(vm, globals)?;
    //    dst_owned = File::create(&path)
    //        .map_err(|e| MonorubyErr::errno_with_path(&globals.store, &e, "rb_sysopen", &path))?;
    //} else {
    //    dst_owned = dup_fd_from_io(dst_val, globals)?;
    //}

    // Copy
    //use std::io::Read;
    //let copied = if let Some(length) = copy_length {
    //    let mut limited = (&mut src_owned).take(length as u64);
    //    std::io::copy(&mut limited, &mut dst_owned)
    //        .map_err(|e| MonorubyErr::runtimeerr(e.to_string()))?
    //} else {
    //    std::io::copy(&mut src_owned, &mut dst_owned)
    //        .map_err(|e| MonorubyErr::runtimeerr(e.to_string()))?
    //};

    //Ok(Value::integer(copied as i64))
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn io_test() {
        run_test_no_result_check(
            r#"
            $stdout << "a"
            $stdout << 5
            $stdout.puts 100
            $stdout.print 100
            $stdout.printf("%b%10.5f", 100, 3.14)
            File.open("/dev/null", "w") << 5
            File.open("/dev/null", "w+") << 5
            $stdin.sync
            $stdin.sync = true
            [$stdin.isatty, $stdout.isatty, $stderr.isatty]
        "#,
        );
    }

    #[test]
    fn read() {
        run_test_error(r#"$stdout.read"#);
        run_test_error(r#"$stderr.read"#);
        run_test_error(r#"File.open("")"#);
        run_test(
            r#"
            f = File.open("Cargo.toml", "r")
            f.read
        "#,
        );
        run_test(
            r#"
            f = File.open("Cargo.toml", "r")
            f.read(17)
        "#,
        );
        run_test(
            r#"
            f = File.open("/dev/null")
            f.read
        "#,
        );
        run_test(
            r#"
            f = File.open("/dev/null", "r")
            f.read(0)
        "#,
        );
        run_test(
            r#"
            f = File.open("/dev/null", "r+")
            f.read(nil)
        "#,
        );
        run_test(
            r#"
            f = File.open("Cargo.toml", "r")
            f.readline
        "#,
        );
    }

    #[test]
    fn each_line() {
        run_test(
            r##"
        f = File.open("a.rb");
        res = []
        f.each_line do |line|
            res << line
        end
        res
        "##,
        );
    }

    #[test]
    fn stringio_write() {
        run_tests(&[
            r#"
            require "stringio"
            sio = StringIO.new
            sio.write("hello", "", " world")
            sio.string
        "#,
            r#"
            require "stringio"
            sio = StringIO.new
            n = sio.write("ab", "cd", "ef")
            [sio.string, n]
        "#,
            r#"
            require "stringio"
            sio = StringIO.new
            n = sio.write("", "hello", "")
            [sio.string, n]
        "#,
        ]);
    }

    #[test]
    fn io_sysopen() {
        run_test_no_result_check(
            r#"
            fd = IO.sysopen("Cargo.toml", "r")
            raise "should be integer" unless fd.is_a?(Integer)
            raise "should be positive" unless fd > 0
            "#,
        );
    }

    #[test]
    fn external_encoding() {
        run_test_no_result_check(
            r#"
            enc = $stdout.external_encoding
            raise "should be Encoding" unless enc.is_a?(Encoding)
            enc.name
            "#,
        );
    }

    #[test]
    fn internal_encoding() {
        run_test_no_result_check(
            r#"
            $stdout.internal_encoding
            "#,
        );
    }

    #[test]
    fn io_encoding_state() {
        run_test(
            r#"
            File.write("/tmp/mr_enc.txt", "")
            r = []
            File.open("/tmp/mr_enc.txt", "r:utf-8:euc-jp") do |f|
              r << f.external_encoding.equal?(Encoding::UTF_8)
              r << f.internal_encoding.equal?(Encoding::EUC_JP)
            end
            File.open("/tmp/mr_enc.txt", "rb") do |f|
              r << f.external_encoding.equal?(Encoding::BINARY)
              r << f.internal_encoding.nil?
              r << f.binmode?
            end
            File.open("/tmp/mr_enc.txt", "w") do |f|
              r << f.external_encoding.nil?
              r << f.internal_encoding.nil?
              f.set_encoding(Encoding::US_ASCII, Encoding::IBM437)
              r << f.external_encoding.equal?(Encoding::US_ASCII)
              r << f.internal_encoding.equal?(Encoding::IBM437)
              f.binmode
              r << f.external_encoding.equal?(Encoding::BINARY)
              r << f.internal_encoding.nil?
              r << f.binmode?
            end
            File.unlink("/tmp/mr_enc.txt")
            r
            "#,
        );
    }

    #[test]
    fn io_encoding_resolution() {
        // default_internal unset, read mode: external tracks
        // default_external live; internal nil.
        run_test_once(
            r#"
            File.write("/tmp/mr_enc2.txt", "")
            Encoding.default_internal = nil
            Encoding.default_external = Encoding::IBM866
            r = []
            File.open("/tmp/mr_enc2.txt", "r") do |f|
              r << f.external_encoding.equal?(Encoding::IBM866)
              r << f.internal_encoding.nil?
              Encoding.default_external = Encoding::IBM437
              r << f.external_encoding.equal?(Encoding::IBM437)
            end
            Encoding.default_external = Encoding::UTF_8
            File.unlink("/tmp/mr_enc2.txt")
            r
            "#,
        );
        // default_internal set: external is snapshot of default_external
        // at creation (frozen); internal = default_internal.
        run_test_once(
            r#"
            File.write("/tmp/mr_enc3.txt", "")
            Encoding.default_external = Encoding::IBM437
            Encoding.default_internal = Encoding::IBM866
            r = []
            File.open("/tmp/mr_enc3.txt", "r") do |f|
              Encoding.default_external = Encoding::UTF_8
              r << f.external_encoding.equal?(Encoding::IBM437)
              r << f.internal_encoding.equal?(Encoding::IBM866)
              f.set_encoding(nil, nil)
              r << f.external_encoding.equal?(Encoding::UTF_8)
              r << f.internal_encoding.nil?
            end
            Encoding.default_external = Encoding::UTF_8
            Encoding.default_internal = nil
            File.unlink("/tmp/mr_enc3.txt")
            r
            "#,
        );
        // di == de => internal nil.
        run_test_once(
            r#"
            File.write("/tmp/mr_enc4.txt", "")
            Encoding.default_external = Encoding::IBM866
            Encoding.default_internal = Encoding::IBM866
            r = []
            File.open("/tmp/mr_enc4.txt", "r") do |f|
              r << f.external_encoding.equal?(Encoding::IBM866)
              r << f.internal_encoding.nil?
            end
            Encoding.default_external = Encoding::UTF_8
            Encoding.default_internal = nil
            File.unlink("/tmp/mr_enc4.txt")
            r
            "#,
        );
        // IO.new with :external_encoding / :internal_encoding options;
        // set_encoding with a string name (enc_same name fallback).
        run_test_once(
            r#"
            File.write("/tmp/mr_enc5.txt", "")
            r = []
            f = File.open("/tmp/mr_enc5.txt", "r")
            io = IO.new(f.fileno, "r",
                        external_encoding: "EUC-JP",
                        internal_encoding: Encoding::UTF_8,
                        autoclose: false)
            r << io.external_encoding.equal?(Encoding::EUC_JP)
            r << io.internal_encoding.equal?(Encoding::UTF_8)
            io.set_encoding("shift_jis")
            r << io.external_encoding.equal?(Encoding::SHIFT_JIS)
            r << io.internal_encoding.nil?
            f.close
            File.unlink("/tmp/mr_enc5.txt")
            r
            "#,
        );
        // Pipe / stdio not created through the open path: external
        // reports default_external, internal nil.
        run_test(
            r#"
            r, w = IO.pipe
            v = [r.external_encoding.equal?(Encoding.default_external),
                 r.internal_encoding.nil?, $stdin.internal_encoding.nil?]
            r.close; w.close
            v
            "#,
        );
    }

    #[test]
    fn io_read_encoding_tagging() {
        // NB: only explicit encodings are asserted — the default
        // external encoding differs between the sandbox CRuby (LANG
        // unset => US-ASCII) and CI, so asserting it would be flaky.
        run_test(
            r#"
            File.write("/tmp/mr_rd.txt", "hello world\nsecond\n")
            r = []
            File.open("/tmp/mr_rd.txt", "r") do |f|
              r << f.read   # content only (encoding is env-dependent)
            end
            File.open("/tmp/mr_rd.txt", "r") do |f|
              r << f.read(5).encoding.name
              r << f.read(0).encoding.name
            end
            File.open("/tmp/mr_rd.txt", "r:iso-8859-1") do |f|
              r << f.read.encoding.name
            end
            File.open("/tmp/mr_rd.txt", "r:iso-8859-1") do |f|
              r << f.gets.encoding.name
            end
            File.open("/tmp/mr_rd.txt", "r:iso-8859-1") do |f|
              r << f.readlines.map { |l| l.encoding.name }
            end
            File.unlink("/tmp/mr_rd.txt")
            r
            "#,
        );
        run_test(
            r#"
            File.write("/tmp/mr_rd2.txt", "abc")
            r = []
            File.open("/tmp/mr_rd2.txt", "r:utf-8:utf-16le") do |f|
              s = f.read
              r << s.encoding.name
              r << s.bytes
            end
            File.unlink("/tmp/mr_rd2.txt")
            r
            "#,
        );
    }

    #[test]
    fn io_getc_readchar_encoding() {
        // EUC-JP external, no internal: one EUC-JP char, tagged EUC-JP.
        run_test(
            r#"
            File.binwrite("/tmp/mr_gc.txt", [164, 162, 164, 164].pack("C*"))
            r = []
            File.open("/tmp/mr_gc.txt", "r:euc-jp") do |f|
              c = f.getc
              r << c.bytes
              r << c.encoding.name
              r << f.readchar.bytes
            end
            File.unlink("/tmp/mr_gc.txt")
            r
            "#,
        );
        // external EUC-JP -> internal UTF-8: transcoded char, UTF-8 tag.
        // (Assert bytes/name only — printing the raw char hits a
        // sandbox-CRuby locale inspect artifact.)
        run_test(
            r#"
            File.binwrite("/tmp/mr_gc2.txt", [164, 162].pack("C*"))
            r = []
            File.open("/tmp/mr_gc2.txt", "r:euc-jp:utf-8") do |f|
              c = f.readchar
              r << c.encoding.name
              r << c.bytes
            end
            File.unlink("/tmp/mr_gc2.txt")
            r
            "#,
        );
        // Plain UTF-8 multibyte still works (regression guard).
        run_test(
            r#"
            File.write("/tmp/mr_gc3.txt", "A" + [0xC3, 0xA8].pack("C*").force_encoding("UTF-8"))
            r = File.open("/tmp/mr_gc3.txt", "r:utf-8") do |f|
              [f.getc.bytes, f.getc.bytes, f.getc]
            end
            File.unlink("/tmp/mr_gc3.txt")
            r
            "#,
        );
        // Shift_JIS (2-byte lead), UTF-16LE (2-byte units), UTF-32LE
        // (4-byte units) and a single-byte encoding (ISO-8859-1)
        // exercise the remaining per-encoding width arms.
        run_test(
            r#"
            File.binwrite("/tmp/mr_gc4.txt", [0x82, 0xA0, 0x41].pack("C*"))
            r = []
            File.open("/tmp/mr_gc4.txt", "r:shift_jis") do |f|
              c = f.getc
              r << [c.bytes, c.encoding.name]
              r << f.getc.bytes
            end
            File.binwrite("/tmp/mr_gc4.txt", [0x41, 0x00, 0x42, 0x00].pack("C*"))
            File.open("/tmp/mr_gc4.txt", "rb:utf-16le") do |f|
              r << f.getc.bytes
            end
            File.binwrite("/tmp/mr_gc4.txt", [0x41, 0, 0, 0].pack("C*"))
            File.open("/tmp/mr_gc4.txt", "rb:utf-32le") do |f|
              r << f.getc.bytes
            end
            File.binwrite("/tmp/mr_gc4.txt", [0xE9, 0x41].pack("C*"))
            File.open("/tmp/mr_gc4.txt", "r:iso-8859-1") do |f|
              c = f.getc
              r << [c.bytes, c.encoding.name]
            end
            File.unlink("/tmp/mr_gc4.txt")
            r
            "#,
        );
    }

    #[test]
    fn io_foreach_test() {
        run_test(
            r#"
            path = "/tmp/monoruby_test_foreach_#{Process.pid}"
            File.write(path, "hello\nworld\nfoo\n")
            res = []
            IO.foreach(path) { |line| res << line }
            File.delete(path)
            res
            "#,
        );
    }

    #[test]
    fn file_foreach_test() {
        run_test(
            r#"
            path = "/tmp/monoruby_test_file_foreach_#{Process.pid}"
            File.write(path, "aaa\nbbb\n")
            res = []
            File.foreach(path) { |line| res << line }
            File.delete(path)
            res
            "#,
        );
    }

    #[test]
    fn io_new_with_fd() {
        run_test_no_result_check(
            r#"
            path = "/tmp/monoruby_test_ionew_#{Process.pid}"
            File.write(path, "io new fd")
            fd = IO.sysopen(path)
            io = IO.new(fd)
            content = io.read
            io.close
            raise "expected 'io new fd'" unless content == "io new fd"
            File.delete(path)
            "#,
        );
    }

    #[test]
    fn io_new_invalid_fd() {
        run_test_error(r#"IO.new(-1)"#);
        run_test_error(r#"IO.new(9999)"#);
    }

    #[test]
    fn io_popen_read_stdout() {
        run_test(
            r#"
            io = IO.popen("echo hello")
            s = io.read
            io.close
            s
            "#,
        );
    }

    #[test]
    fn io_popen_with_block_returns_block_result() {
        run_test(
            r#"
            IO.popen("echo world") { |io| io.read.chomp }
            "#,
        );
    }

    #[test]
    fn io_popen_block_sets_last_status() {
        run_test(
            r#"
            IO.popen(["true"]) { |io| io.read }
            [$?.class.to_s, $?.exitstatus]
            "#,
        );
        run_test(
            r#"
            IO.popen(["false"]) { |io| io.read }
            $?.exitstatus
            "#,
        );
    }

    #[test]
    fn io_popen_write_mode() {
        run_test_no_result_check(
            r#"
            io = IO.popen("cat >/dev/null", "w")
            io.write("bar")
            io.close
            "#,
        );
    }

    #[test]
    fn io_popen_array_command_avoids_shell() {
        // Array form should pass args directly without going through sh -c.
        run_test(
            r#"
            IO.popen(["echo", "a b"]) { |io| io.read }
            "#,
        );
    }

    #[test]
    fn io_popen_stderr_to_stdout() {
        run_test(
            r#"
            IO.popen(["sh", "-c", "echo out; echo err 1>&2"], err: [:child, :out]) { |io| io.read }
            "#,
        );
    }

    #[test]
    fn io_popen_pid_is_integer() {
        run_test(
            r#"
            io = IO.popen("true")
            pid = io.pid
            io.close
            pid.is_a?(Integer)
            "#,
        );
    }

    #[test]
    fn io_popen_empty_args() {
        run_test_error(r#"IO.popen"#);
    }

    #[test]
    fn io_wait_constants() {
        run_test(
            r#"
            [IO::READABLE, IO::WRITABLE, IO::PRIORITY]
            "#,
        );
    }

    #[test]
    fn io_wait_require() {
        // `require "io/wait"` should succeed (the methods are built-in,
        // but the file must exist so libraries that require it don't
        // fail with LoadError).
        run_test(
            r#"
            require "io/wait"
            [IO.instance_method(:wait_readable).is_a?(UnboundMethod),
             IO.instance_method(:wait_writable).is_a?(UnboundMethod)]
            "#,
        );
    }

    #[test]
    fn io_wait_pipe_readable() {
        // wait(events, timeout): when readable within the timeout, returns
        // the event mask (Integer form). When not, returns nil.
        run_test(
            r#"
            r, w = IO.pipe
            w.write("hi")
            a = r.wait(IO::READABLE, 1)
            b = r.wait_readable(1)
            [a, b == r]
            "#,
        );
        run_test(
            r#"
            r, _w = IO.pipe
            [r.wait(IO::READABLE, 0), r.wait_readable(0)]
            "#,
        );
    }

    #[test]
    fn io_wait_pipe_writable() {
        // Fresh pipe writer is writable immediately.
        run_test(
            r#"
            _r, w = IO.pipe
            [w.wait(IO::WRITABLE, 0), w.wait_writable(0) == w]
            "#,
        );
    }

    #[test]
    fn io_wait_returns_self_in_symbol_form() {
        // Any accepted symbol combination returns `self` on ready, `nil`
        // on timeout.
        run_test(
            r#"
            r, w = IO.pipe
            w.write("x")
            [
              r.wait(:r, 0) == r,
              r.wait(:read, 0) == r,
              r.wait(:readable, 0) == r,
              r.wait(0, :r) == r,
              r.wait(:r, 0, :w) == r,
            ]
            "#,
        );
        run_test(
            r#"
            r, _w = IO.pipe
            r.wait(:r, 0).nil?
            "#,
        );
    }

    #[test]
    fn io_wait_writable_symbols() {
        run_test(
            r#"
            _r, w = IO.pipe
            [
              w.wait(:w, 0) == w,
              w.wait(:write, 0) == w,
              w.wait(:writable, 0) == w,
              w.wait(0, :rw) == w,
              w.wait(:read_write, 0) == w,
              w.wait(:readable_writable, 0) == w,
            ]
            "#,
        );
    }

    #[test]
    fn io_wait_event_mask_not_ready() {
        // wait(events, 0) on a not-yet-readable pipe returns nil.
        run_test(
            r#"
            r, _w = IO.pipe
            r.wait(IO::READABLE, 0)
            "#,
        );
    }

    #[test]
    fn io_wait_rejects_nonpositive_events() {
        run_test_error(r#"r, _w = IO.pipe; r.wait(0, 0)"#);
        run_test_error(r#"r, _w = IO.pipe; r.wait(-1, 0)"#);
    }

    #[test]
    fn io_wait_rejects_unknown_symbol_mode() {
        run_test_error(r#"r, _w = IO.pipe; r.wait(:wrong, 0)"#);
    }

    #[test]
    fn io_wait_rejects_multiple_timeouts() {
        run_test_error(r#"r, _w = IO.pipe; r.wait(0, 10, :r)"#);
    }

    #[test]
    fn io_wait_rejects_too_many_positional_args() {
        run_test_error(r#"r, _w = IO.pipe; r.wait(IO::READABLE, 0, 1)"#);
    }

    #[test]
    fn io_wait_on_closed_stream_raises() {
        run_test_error(
            r#"
            r, _w = IO.pipe
            r.close
            r.wait(IO::READABLE, 0)
            "#,
        );
        run_test_error(
            r#"
            r, _w = IO.pipe
            r.close
            r.wait_readable(0)
            "#,
        );
    }

    #[test]
    fn io_wait_priority() {
        // `wait_priority` is a read-side event; call on the read end of the
        // pipe so it doesn't raise "not opened for reading" on CRuby.
        run_test(
            r#"
            r, _w = IO.pipe
            r.wait_priority(0).nil?
            "#,
        );
    }

    #[test]
    fn io_seek() {
        run_test_no_result_check(
            r#"
            path = "/tmp/monoruby_test_seek_#{Process.pid}"
            File.write(path, "0123456789")
            f = File.open(path)
            f.seek(3)
            s = f.read
            raise "expected '3456789' but got '#{s}'" unless s == "3456789"
            f.seek(0, IO::SEEK_SET)
            s = f.read(2)
            raise "expected '01' but got '#{s}'" unless s == "01"
            f.seek(2, IO::SEEK_CUR)
            s = f.read(2)
            raise "expected '45' but got '#{s}'" unless s == "45"
            f.seek(-3, IO::SEEK_END)
            s = f.read
            raise "expected '789' but got '#{s}'" unless s == "789"
            raise "seek should return 0" unless f.seek(0) == 0
            f.seek(0, :SET)
            raise "symbol :SET" unless f.read(1) == "0"
            f.seek(0, :END)
            raise "symbol :END" unless f.read == ""
            f.close
            File.delete(path)
            "#,
        );
    }

    #[test]
    fn io_seek_errors() {
        run_test_error(
            r#"
            path = "/tmp/monoruby_test_seek_neg_#{Process.pid}"
            File.write(path, "abc")
            f = File.open(path)
            begin
              f.seek(-1)
            ensure
              f.close
              File.delete(path)
            end
            "#,
        );
        run_test_error(r#"$stdout.seek(0)"#);
    }

    #[test]
    fn io_try_convert() {
        run_tests(&[
            r#"IO.try_convert($stdout).equal?($stdout)"#,
            r#"IO.try_convert("hello")"#,
            r#"IO.try_convert(42)"#,
            r#"IO.try_convert(nil)"#,
            // Custom #to_io that returns self.
            r#"
            klass = Class.new do
              def initialize(io); @io = io; end
              def to_io; @io; end
            end
            obj = klass.new($stdout)
            IO.try_convert(obj).equal?($stdout)
            "#,
        ]);
    }

    #[test]
    fn io_class_write_binread_binwrite() {
        run_test_once(
            r#"
            base = "/tmp/monoruby_io_write_test_#{Process.pid}_#{rand(100000)}"
            paths = [base + ".w", base + ".bw"]
            begin
              n1 = IO.write(paths[0], "abc\ndef")
              n2 = IO.binwrite(paths[1], "binary\0content")
              [n1, n2, IO.binread(paths[0]), IO.binread(paths[1]),
               IO.binread(paths[0], 3), IO.binread(paths[0], 3, 4)]
            ensure
              paths.each { |p| File.unlink(p) rescue nil }
            end
            "#,
        );
    }

    #[test]
    fn io_open_with_mode() {
        // IO.open routes through the same opener as File.open / File.new.
        // Exercise both fd and path forms with a string mode and an
        // Integer flag bag.
        run_test_once(
            r#"
            path = "/tmp/monoruby_test_io_open_#{Process.pid}_#{rand(100000)}"
            begin
              f = IO.open(IO.sysopen(path, "w"), "w")
              f.write("a-")
              f.close
              IO.open(IO.sysopen(path, "a"), File::WRONLY) { |g| g.write("b") }
              File.read(path)
            ensure
              File.unlink(path) rescue nil
            end
            "#,
        );
    }

    #[test]
    fn io_binmode_and_autoclose() {
        run_test(
            r#"
            f = File.open("Cargo.toml")
            begin
              [f.binmode.equal?(f), f.binmode?, f.autoclose?, (f.autoclose = false)]
            ensure
              f.close
            end
            "#,
        );
    }

    #[test]
    fn io_autoclose_round_trip() {
        // The flag must actually be tracked, not silently coerced. The
        // `File.new(other.fileno, autoclose: false)` pattern from
        // `logger/log_device.rb` aborts with "IO Safety violation" on GC if
        // the new wrapper still claims ownership of the borrowed fd.
        run_test(
            r#"
            File.open("Cargo.toml") do |f|
              before = f.autoclose?
              f.autoclose = false
              after = f.autoclose?
              [before, after, f.autoclose?]
            end
            "#,
        );
    }

    #[test]
    fn io_new_with_autoclose_false() {
        // `File.new(integer_fd, autoclose: false, path: ...)` must parse the
        // options Hash so the wrapper does not double-close the borrowed fd.
        // This is the crash-on-GC pattern from `logger/log_device.rb`.
        run_test(
            r#"
            File.open("Cargo.toml") do |f|
              g = File.new(f.fileno, autoclose: false, path: "borrowed")
              ac = g.autoclose?
              g.close
              ac
            end
            "#,
        );
    }

    #[test]
    fn io_advise() {
        run_test(
            r#"
            f = File.open("Cargo.toml")
            begin
              [
                f.advise(:normal),
                f.advise(:sequential, 0, 0),
                f.advise(:willneed, 0, 16),
              ]
            ensure
              f.close
            end
            "#,
        );
    }

    #[test]
    fn io_advise_unknown() {
        run_test_error(
            r#"
            f = File.open("Cargo.toml")
            begin
              f.advise(:bogus)
            ensure
              f.close
            end
            "#,
        );
    }

    #[test]
    fn io_pos_tell_rewind() {
        run_test(
            r#"
            f = File.open("Cargo.toml")
            begin
              a = f.pos
              f.read(5)
              b = f.pos
              c = f.tell
              f.rewind
              [a, b, c, f.pos]
            ensure
              f.close
            end
            "#,
        );
    }

    #[test]
    fn io_pos_set() {
        run_test(
            r#"
            f = File.open("Cargo.toml")
            begin
              f.pos = 4
              [f.pos, f.read(2)]
            ensure
              f.close
            end
            "#,
        );
    }

    #[test]
    fn io_eof() {
        run_test_once(
            r#"
            path = "/tmp/monoruby_io_eof_#{Process.pid}_#{rand(100000)}"
            begin
              File.write(path, "ab")
              f = File.open(path)
              before = f.eof?
              f.read
              after = f.eof?
              [before, after, f.eof]
            ensure
              f&.close
              File.unlink(path) rescue nil
            end
            "#,
        );
    }

    #[test]
    fn io_getbyte_readbyte() {
        run_test(
            r#"
            f = File.open("Cargo.toml")
            begin
              [f.getbyte, f.getbyte, f.readbyte]
            ensure
              f.close
            end
            "#,
        );
    }

    #[test]
    fn io_readbyte_eof_raises() {
        run_test_error(
            r#"
            path = "/tmp/monoruby_io_readbyte_#{Process.pid}_#{rand(100000)}"
            begin
              File.write(path, "")
              f = File.open(path)
              begin
                f.readbyte
              ensure
                f.close
              end
            ensure
              File.unlink(path) rescue nil
            end
            "#,
        );
    }

    #[test]
    fn io_getc_readchar() {
        run_test(
            r#"
            f = File.open("Cargo.toml")
            begin
              [f.getc, f.getc, f.readchar]
            ensure
              f.close
            end
            "#,
        );
    }

    #[test]
    fn io_readchar_eof_raises() {
        run_test_error(
            r#"
            path = "/tmp/monoruby_io_readchar_#{Process.pid}_#{rand(100000)}"
            begin
              File.write(path, "")
              f = File.open(path)
              begin
                f.readchar
              ensure
                f.close
              end
            ensure
              File.unlink(path) rescue nil
            end
            "#,
        );
    }

    #[test]
    fn io_sysseek() {
        run_test(
            r#"
            f = File.open("Cargo.toml")
            begin
              [f.sysseek(0), f.sysseek(3), f.sysseek(0, IO::SEEK_END) >= 0]
            ensure
              f.close
            end
            "#,
        );
    }

    #[test]
    fn io_lineno_accessors() {
        run_test(
            r#"
            f = File.open("Cargo.toml")
            begin
              before = f.lineno
              f.lineno = 10
              [before, f.lineno]
            ensure
              f.close
            end
            "#,
        );
    }

    #[test]
    fn io_to_io_to_i() {
        run_test(
            r#"
            f = File.open("Cargo.toml")
            begin
              [f.to_io.equal?(f), f.to_i == f.fileno]
            ensure
              f.close
            end
            "#,
        );
    }

    #[test]
    fn io_inst_readlines() {
        run_test_once(
            r#"
            path = "/tmp/monoruby_io_readlines_#{Process.pid}_#{rand(100000)}"
            begin
              File.write(path, "line1\nline2\nline3\n")
              f = File.open(path)
              lines = f.readlines
              f.close
              lines
            ensure
              File.unlink(path) rescue nil
            end
            "#,
        );
    }

    #[test]
    fn io_class_readlines() {
        run_test_once(
            r#"
            path = "/tmp/monoruby_io_classreadlines_#{Process.pid}_#{rand(100000)}"
            begin
              File.write(path, "a\nb\nc")
              IO.readlines(path)
            ensure
              File.unlink(path) rescue nil
            end
            "#,
        );
    }

    #[test]
    fn io_gets() {
        // Read line-by-line via gets and stop at EOF (gets returns nil).
        run_test_once(
            r#"
            path = "/tmp/monoruby_io_gets_#{Process.pid}_#{rand(100000)}"
            begin
              File.write(path, "alpha\nbeta\ngamma\n")
              f = File.open(path)
              lines = []
              while (line = f.gets); lines << line; end
              tail = f.gets
              f.close
              [lines, tail]
            ensure
              File.unlink(path) rescue nil
            end
            "#,
        );
    }

    #[test]
    fn io_flush() {
        // `IO#flush` returns self, but the IO instance identity differs
        // between monoruby and CRuby runs, so reduce the result to a
        // boolean.
        run_test(
            r#"
            f = File.open("Cargo.toml")
            begin
              f.flush.equal?(f)
            ensure
              f.close
            end
            "#,
        );
    }

    #[test]
    fn io_closed_predicate() {
        run_test_once(
            r#"
            path = "/tmp/monoruby_io_closed_#{Process.pid}_#{rand(100000)}"
            begin
              File.write(path, "x")
              f = File.open(path)
              before = f.closed?
              f.close
              [before, f.closed?]
            ensure
              File.unlink(path) rescue nil
            end
            "#,
        );
    }

    #[test]
    fn io_close_read_close_write() {
        // close_read / close_write are only valid for duplex IOs. Use a
        // bidirectional `popen` to exercise both halves.
        run_test_once(
            r#"
            io = IO.popen("cat", "r+")
            begin
              io.close_write
              writable_after = io.closed?
              io.close_read
              [writable_after, io.closed?]
            ensure
              io.close rescue nil
            end
            "#,
        );
    }

    #[test]
    fn io_close_read_on_nonduplex_raises() {
        run_test_error(
            r#"
            r, w = IO.pipe
            begin
              w.close_read
            ensure
              r.close
              w.close
            end
            "#,
        );
    }

    #[test]
    fn io_syswrite_returns_bytes_written() {
        run_test_once(
            r#"
            path = "/tmp/monoruby_io_syswrite_#{Process.pid}_#{rand(100000)}"
            begin
              f = File.open(path, "w")
              n = f.syswrite("hello")
              f.close
              [n, File.read(path)]
            ensure
              File.unlink(path) rescue nil
            end
            "#,
        );
    }

    #[test]
    fn io_select_immediate_timeout_pipe() {
        // A freshly-created pipe with no data buffered should report no
        // ready descriptors when polled with a zero-second timeout.
        run_test_once(
            r#"
            r, w = IO.pipe
            begin
              IO.select([r], nil, nil, 0)
            ensure
              r.close
              w.close
            end
            "#,
        );
    }

    #[test]
    fn io_select_returns_writable_pipe() {
        // The write end of a freshly-created pipe is always immediately
        // writable.
        run_test_once(
            r#"
            r, w = IO.pipe
            begin
              writers = IO.select(nil, [w], nil, 0)
              writers && writers[1].include?(w)
            ensure
              r.close
              w.close
            end
            "#,
        );
    }

    #[test]
    fn io_set_encoding_returns_self() {
        run_test(
            r#"
            f = File.open("Cargo.toml")
            begin
              [f.set_encoding("ASCII-8BIT").equal?(f),
               f.set_encoding("UTF-8", "UTF-8").equal?(f)]
            ensure
              f.close
            end
            "#,
        );
    }

    // ----- error patterns --------------------------------------------------

    #[test]
    fn io_try_convert_to_io_returns_non_io_raises() {
        // If the object responds to #to_io but the call yields a non-IO,
        // CRuby raises TypeError. monoruby matches.
        run_test_error(
            r#"
            klass = Class.new do
              def to_io; "not an IO"; end
            end
            IO.try_convert(klass.new)
            "#,
        );
    }

    #[test]
    fn io_class_read_missing_path_raises() {
        run_test_error(r#"IO.read("monoruby_no_such_file_xyz_qq")"#);
    }

    #[test]
    fn io_class_binread_negative_length_raises() {
        run_test_error(r#"IO.binread("Cargo.toml", -1)"#);
    }

    #[test]
    fn io_class_read_length_offset_encoding() {
        run_test(
            r#"
            File.write("/tmp/mr_cr.txt", "1234567890")
            r = []
            r << IO.read("/tmp/mr_cr.txt")
            r << IO.read("/tmp/mr_cr.txt", 5)
            r << IO.read("/tmp/mr_cr.txt", 5, 3)
            r << IO.read("/tmp/mr_cr.txt", 5).encoding.name
            r << IO.read("/tmp/mr_cr.txt", 0)
            r << IO.read("/tmp/mr_cr.txt", 0).encoding.name
            r << IO.read("/tmp/mr_cr.txt", 1, 100)
            r << IO.read("/tmp/mr_cr.txt", nil, 5)
            r << IO.read("/tmp/mr_cr.txt", mode: "r")
            r << IO.read("/tmp/mr_cr.txt", encoding: Encoding::ISO_8859_1).encoding.name
            r << IO.read("/tmp/mr_cr.txt", external_encoding: "EUC-JP").encoding.name
            r << IO.read("/tmp/mr_cr.txt", nil, 0,
                         open_args: [{ encoding: Encoding::US_ASCII }]).encoding.name
            r << IO.read("/tmp/mr_cr.txt", mode: "w",
                         open_args: ["r", encoding: "UTF-8"]).encoding.name
            File.unlink("/tmp/mr_cr.txt")
            r
            "#,
        );
        run_test_error(
            r#"File.write("/tmp/mr_cr2.txt","x"); IO.read("/tmp/mr_cr2.txt", mode: "w")"#,
        );
        run_test_error(
            r#"File.write("/tmp/mr_cr3.txt","x"); IO.read("/tmp/mr_cr3.txt", 1, 0, mode: "a")"#,
        );
        run_test_error(r#"IO.read("Cargo.toml", -1)"#);
        run_test_error(r#"IO.read("Cargo.toml", 0, -1)"#);
        run_test_error(r#"IO.read(nil)"#);
    }

    #[test]
    fn io_class_read_mode_and_transcode_paths() {
        run_test(
            r#"
            File.write("/tmp/mr_cr4.txt", "abcdef")
            r = []
            # :mode string carrying an encoding suffix
            s = IO.read("/tmp/mr_cr4.txt", mode: "r:iso-8859-1")
            r << [s, s.encoding.name]
            # explicit external + internal via options -> transcoded
            t = IO.read("/tmp/mr_cr4.txt", external_encoding: "utf-8",
                        internal_encoding: "utf-16le")
            r << [t.encoding.name, t.bytes]
            # mode encoding suffix with both ext:int
            u = IO.read("/tmp/mr_cr4.txt", mode: "r:utf-8:utf-16le")
            r << [u.encoding.name, u.bytes]
            # :open_args hash carrying a mode that itself has a suffix
            v = IO.read("/tmp/mr_cr4.txt", open_args: [{ mode: "r:euc-jp" }])
            r << v.encoding.name
            # offset past data with no length -> empty string
            w = IO.read("/tmp/mr_cr4.txt", nil, 100)
            r << w
            File.unlink("/tmp/mr_cr4.txt")
            r
            "#,
        );
        // ENOENT path (open failure map_err).
        run_test_error(r#"IO.read("/tmp/mr_no_such_dir_zzz/missing.txt", 4)"#);
    }

    #[test]
    fn io_open_invalid_fd_raises() {
        run_test_error(r#"IO.open(-1)"#);
        run_test_error(r#"IO.open(99999)"#);
    }

    #[test]
    fn io_advise_with_non_symbol_raises() {
        run_test_error(
            r#"
            f = File.open("Cargo.toml")
            begin
              f.advise("normal")
            ensure
              f.close
            end
            "#,
        );
    }

    #[test]
    fn io_select_read_array_must_contain_io() {
        run_test_error(r#"IO.select(["not an io"], nil, nil, 0)"#);
    }

    #[test]
    fn io_pos_on_closed_raises() {
        run_test_error(
            r#"
            f = File.open("Cargo.toml")
            f.close
            f.pos
            "#,
        );
    }

    #[test]
    fn io_syswrite_on_closed_raises() {
        run_test_error(
            r#"
            f = File.open("/tmp/monoruby_syswr_closed_#{Process.pid}", "w")
            path = "/tmp/monoruby_syswr_closed_#{Process.pid}"
            f.close
            begin
              f.syswrite("hello")
            ensure
              File.unlink(path) rescue nil
            end
            "#,
        );
    }

    #[test]
    fn io_gets_on_closed_raises() {
        run_test_error(
            r#"
            f = File.open("Cargo.toml")
            f.close
            f.gets
            "#,
        );
    }

    #[test]
    fn io_path_to_path() {
        run_test(r#"File.open("Cargo.toml") { |f| f.path }"#);
        run_test(r#"File.open("Cargo.toml") { |f| f.to_path }"#);
        run_test(r#"$stdin.path"#);
        run_test(r#"$stdout.path"#);
        run_test(r#"$stderr.path"#);
        run_test(r#"r, w = IO.pipe; v = [r.path, w.path]; r.close; w.close; v"#);
        run_test(
            r#"File.open("Cargo.toml") { |f| IO.new(f.fileno, path: "X", autoclose: false).path }"#,
        );
        run_test(
            r#"File.open("Cargo.toml") { |f| IO.new(f.fileno, autoclose: false).path }"#,
        );
    }

    #[test]
    fn io_fsync_fdatasync() {
        run_test_once(
            r#"
            path = "/tmp/mr_fsync_test.txt"
            r = File.open(path, "w") { |f| f.write("payload"); [f.fsync, f.fdatasync] }
            File.unlink(path)
            r
            "#,
        );
        run_test_error(
            r#"
            f = File.open("Cargo.toml")
            f.close
            f.fsync
            "#,
        );
    }

    #[test]
    fn io_close_on_exec() {
        run_test(
            r#"
            File.open("Cargo.toml") do |f|
              before = f.close_on_exec?
              f.close_on_exec = false
              a = f.close_on_exec?
              f.close_on_exec = true
              b = f.close_on_exec?
              f.close_on_exec = nil
              [before, a, b, f.close_on_exec?]
            end
            "#,
        );
        run_test_error(
            r#"
            f = File.open("Cargo.toml")
            f.close
            f.close_on_exec?
            "#,
        );
    }

    #[test]
    fn io_ungetbyte() {
        run_test(
            r#"
            File.open("Cargo.toml") do |f|
              f.getbyte
              r = f.ungetbyte("cat")
              [r, f.getbyte, f.getbyte, f.getbyte, f.getbyte]
            end
            "#,
        );
        run_test(
            r#"
            File.open("Cargo.toml") do |f|
              [f.ungetbyte(nil), f.ungetbyte(4095), f.getbyte,
               f.ungetbyte(0x4f7574206f6620636861722072616e67ff), f.getbyte]
            end
            "#,
        );
    }

    #[test]
    fn io_ungetc() {
        run_test(
            r#"
            File.open("Cargo.toml") do |f|
              c = f.getc
              r = f.ungetc(c)
              p = f.pos
              [r, p, f.getc, f.eof?]
            end
            "#,
        );
        run_test(
            r#"
            File.open("Cargo.toml") do |f|
              f.read
              f.ungetc(100)
              [f.eof?, f.getc]
            end
            "#,
        );
        run_test(r#"File.open("Cargo.toml") { |f| f.ungetc(100) }"#);
    }

    #[test]
    fn io_ungetc_errors() {
        run_test_error(r#"$stdout.ungetc(100)"#);
        run_test_error(r#"$stdout.ungetbyte(42)"#);
        run_test_error(r#"File.open("Cargo.toml") { |f| f.getc; f.ungetc(nil) }"#);
        run_test_error(
            r#"
            f = File.open("Cargo.toml")
            f.close
            f.ungetc(100)
            "#,
        );
    }

    #[test]
    fn io_lineno_autoincrement() {
        run_test(
            r#"
            File.open("Cargo.toml") do |f|
              a = f.lineno
              f.gets; f.gets
              b = f.lineno
              line = f.gets
              [a, b, f.lineno, $. , $_ == line, f.readline ? f.lineno : nil]
            end
            "#,
        );
        run_test(
            r#"
            File.open("Cargo.toml") do |f|
              f.gets; f.gets
              n = f.lineno
              f.rewind
              [n, f.lineno, f.gets.nil?]
            end
            "#,
        );
        run_test_error(r#"File.open("Cargo.toml") { |f| f.lineno = 4294967296 }"#);
    }

    #[test]
    fn io_directional_mode_errors() {
        // Read ops on a write-only stream raise IOError.
        run_test_error(r#"File.open("/tmp/mr_mode_w.txt", "w") { |f| f.read }"#);
        run_test_error(r#"File.open("/tmp/mr_mode_w.txt", "w") { |f| f.gets }"#);
        run_test_error(r#"File.open("/tmp/mr_mode_w.txt", "w") { |f| f.lineno }"#);
        run_test_error(r#"File.open("/tmp/mr_mode_w.txt", "w") { |f| f.lineno = 1 }"#);
        run_test_error(r#"File.open("/tmp/mr_mode_w.txt", "w") { |f| f.ungetc(65) }"#);
        // Write op on a read-only stream raises IOError.
        run_test_error(r#"File.open("Cargo.toml", "r") { |f| f.write("x") }"#);
        // Read/write modes still work both ways.
        run_test(
            r#"
            path = "/tmp/mr_mode_rw.txt"
            r = File.open(path, "w+") { |f| f.write("hello"); f.rewind; f.read }
            File.unlink(path)
            r
            "#,
        );
    }

    #[test]
    fn io_directional_mode_errors_streams() {
        // ensure_readable: closed stream -> "closed stream".
        run_test_error(
            r#"
            f = File.open("Cargo.toml")
            f.close
            f.lineno
            "#,
        );
        // is_readable Stdout arm + ensure_readable "not opened for reading".
        run_test_error(r#"$stdout.lineno"#);
        run_test_error(r#"$stderr.lineno"#);
        // ensure_writable Stdin arm -> "not opened for writing".
        run_test_error(r#"$stdin.write("x")"#);
        // fd_rw_mode through IO.pipe: read end is RO, write end is WO.
        run_test_error(r#"r, w = IO.pipe; begin; w.gets; ensure; r.close; w.close; end"#);
        run_test_error(r#"r, w = IO.pipe; begin; r.write("x"); ensure; r.close; w.close; end"#);
        // is_readable / is_writable Popen arms.
        run_test_error(r#"IO.popen("cat", "w") { |p| p.gets }"#);
        run_test_error(r#"IO.popen("cat", "r") { |p| p.write("x") }"#);
    }
}
