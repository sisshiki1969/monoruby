use super::*;
use std::fs::File;
use std::os::unix::process::CommandExt;
use std::process::{Command, Stdio};


//
// IO class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_under_obj("IO", IO_CLASS, ObjTy::IO);
    globals.define_builtin_class_func_with_kw(IO_CLASS, "new", io_new, 1, 2, false, &[], true);
    globals.define_private_builtin_func_with_kw(
        IO_CLASS,
        "initialize",
        io_initialize,
        1,
        2,
        false,
        &[],
        true,
    );
    globals.store[IO_CLASS].set_alloc_func(io_alloc_func);
    globals.define_builtin_func(IO_CLASS, "<<", shl, 1);
    globals.define_builtin_func_with(IO_CLASS, "puts", puts, 0, 0, true);
    globals.define_builtin_func_with(IO_CLASS, "print", print, 0, 0, true);
    globals.define_builtin_func_with(IO_CLASS, "printf", printf, 1, 1, true);
    globals.define_builtin_func(IO_CLASS, "flush", flush, 0);
    globals.define_builtin_func_with_kw(IO_CLASS, "gets", gets, 0, 2, false, &["chomp"], true);
    globals.define_builtin_func_with_kw(
        IO_CLASS,
        "readline",
        readline,
        0,
        2,
        false,
        &["chomp"],
        true,
    );
    globals.define_builtin_funcs(IO_CLASS, "isatty", &["tty?"], isatty, 0);
    globals.define_builtin_func(IO_CLASS, "close", close, 0);
    globals.define_builtin_func(IO_CLASS, "close_write", close_write, 0);
    globals.define_builtin_func(IO_CLASS, "close_read", close_read, 0);
    globals.define_builtin_func(IO_CLASS, "closed?", closed_, 0);
    globals.define_builtin_func(IO_CLASS, "sync", io_sync, 0);
    globals.define_builtin_func(IO_CLASS, "sync=", assign_sync, 1);
    globals.define_builtin_func_with(IO_CLASS, "seek", seek, 1, 2, false);
    globals.define_builtin_func_with(IO_CLASS, "read", read, 0, 2, false);
    globals.define_builtin_class_func_with(IO_CLASS, "read", io_class_read, 1, 4, false);
    globals.define_builtin_class_func_with_kw(
        IO_CLASS,
        "readlines",
        io_class_readlines,
        1,
        3,
        false,
        &[],
        true,
    );
    globals.define_builtin_class_func_with(IO_CLASS, "sysopen", io_sysopen, 1, 3, false);
    globals.define_builtin_class_func_with(IO_CLASS, "pipe", io_pipe, 0, 3, false);
    globals.define_builtin_class_func_rest(IO_CLASS, "popen", io_popen);
    globals.define_builtin_func(IO_CLASS, "pid", io_pid, 0);
    globals.define_builtin_funcs(IO_CLASS, "fileno", &["to_i"], io_fileno, 0);
    globals.define_builtin_func_with(IO_CLASS, "fcntl", io_fcntl, 1, 2, false);
    globals.define_builtin_func_with(IO_CLASS, "ioctl", io_ioctl, 1, 2, false);
    globals.define_builtin_func(IO_CLASS, "initialize_copy", io_initialize_copy, 1);
    globals.define_builtin_func_with(IO_CLASS, "reopen", io_reopen, 1, 2, false);
    globals.define_builtin_func(IO_CLASS, "to_io", io_to_io, 0);
    globals.define_builtin_func_with(IO_CLASS, "write", io_write_method, 0, 0, true);
    globals.define_builtin_func_with(IO_CLASS, "syswrite", io_syswrite, 1, 1, false);
    globals.define_builtin_func_with(IO_CLASS, "pread", io_pread, 2, 3, false);
    globals.define_builtin_func(IO_CLASS, "pwrite", io_pwrite, 2);
    globals.define_builtin_func_with(IO_CLASS, "sysread", io_sysread, 1, 2, false);
    globals.define_builtin_func_with(IO_CLASS, "readpartial", io_readpartial, 1, 2, false);
    globals.define_builtin_func_with_kw(
        IO_CLASS, "read_nonblock", io_read_nonblock, 1, 2, false, &["exception"], false,
    );
    globals.define_builtin_func_with_kw(
        IO_CLASS, "write_nonblock", io_write_nonblock, 1, 1, false, &["exception"], false,
    );
    globals.define_builtin_func_with_kw(
        IO_CLASS,
        "readlines",
        io_readlines,
        0,
        2,
        false,
        &["chomp"],
        true,
    );
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
    globals.define_builtin_class_func_with_kw(
        IO_CLASS,
        "foreach",
        io_foreach,
        1,
        3,
        false,
        &[],
        true,
    );
    globals.define_builtin_class_func_with(IO_CLASS, "copy_stream", io_copy_stream, 2, 4, false);
    globals.define_builtin_func_with(IO_CLASS, "set_encoding", set_encoding, 1, 3, false);
    globals.define_builtin_func(IO_CLASS, "set_encoding_by_bom", set_encoding_by_bom, 0);
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
    // POSIX `open(2)` mode flags (used by File.open / IO#reopen mode
    // forms and propagated to StringIO#reopen via ruby/spec). Values
    // come from `libc::O_*`; they must compare with `File::WRONLY`
    // etc. (File inherits IO so the same constants resolve).
    globals.set_constant_by_str(IO_CLASS, "RDONLY", Value::integer(libc::O_RDONLY as i64));
    globals.set_constant_by_str(IO_CLASS, "WRONLY", Value::integer(libc::O_WRONLY as i64));
    globals.set_constant_by_str(IO_CLASS, "RDWR", Value::integer(libc::O_RDWR as i64));
    globals.set_constant_by_str(IO_CLASS, "APPEND", Value::integer(libc::O_APPEND as i64));
    globals.set_constant_by_str(IO_CLASS, "CREAT", Value::integer(libc::O_CREAT as i64));
    globals.set_constant_by_str(IO_CLASS, "EXCL", Value::integer(libc::O_EXCL as i64));
    globals.set_constant_by_str(IO_CLASS, "TRUNC", Value::integer(libc::O_TRUNC as i64));
    globals.set_constant_by_str(IO_CLASS, "NOCTTY", Value::integer(libc::O_NOCTTY as i64));
    globals.set_constant_by_str(IO_CLASS, "NONBLOCK", Value::integer(libc::O_NONBLOCK as i64));
    globals.set_constant_by_str(IO_CLASS, "SYNC", Value::integer(libc::O_SYNC as i64));
    globals.set_constant_by_str(IO_CLASS, "DSYNC", Value::integer(libc::O_DSYNC as i64));
    globals.set_constant_by_str(IO_CLASS, "NOFOLLOW", Value::integer(libc::O_NOFOLLOW as i64));
    // Linux-only bits — absent from Darwin's open(2) (and from CRuby
    // on Darwin, which defines these constants only `#ifdef O_*`).
    #[cfg(target_os = "linux")]
    {
        globals.set_constant_by_str(IO_CLASS, "RSYNC", Value::integer(libc::O_RSYNC as i64));
        globals.set_constant_by_str(IO_CLASS, "DIRECT", Value::integer(libc::O_DIRECT as i64));
    }
    // (IO::SEEK_SET / SEEK_CUR / SEEK_END are already defined elsewhere
    //  in startup; do not re-define here.)
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
    let class = lfp.self_val().as_class_id();
    let obj = Value::new_io_with_class(IoInner::closed(None), class);
    io_init_from_fd(vm, globals, lfp, obj)?;
    if lfp.block().is_some() {
        // CRuby warns through rb_warn; route via Kernel#warn so the
        // Ruby-level $stderr (which specs capture) receives it.
        let msg = Value::string_from_str(
            "warning: IO::new() does not take block; use IO::open() instead",
        );
        let main = globals.main_object;
        let _ = vm.invoke_method_inner(globals, IdentId::get_id("warn"), main, &[msg], None, None);
    }
    Ok(obj)
}

///
/// ### IO#initialize
///
/// - initialize(fd, mode = nil, **opts) -> self
///
/// (Re)associates the receiver with the file descriptor `fd`.
#[monoruby_builtin]
fn io_initialize(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    io_init_from_fd(vm, globals, lfp, lfp.self_val())?;
    Ok(lfp.self_val())
}

/// Fetch a keyword from the (optional) kwrest Hash of `IO.new`-family
/// callsites, mapping an explicit `nil` to `None`.
fn kw_opt(
    vm: &mut Executor,
    globals: &mut Globals,
    kw: &Option<crate::value::Hashmap>,
    name: &str,
) -> Result<Option<Value>> {
    let Some(h) = kw else { return Ok(None) };
    Ok(h.get(Value::symbol(IdentId::get_id(name)), vm, globals)?
        .filter(|v| !v.is_nil()))
}

/// Parse a CRuby fmode string `[rwa]['+'|'b'|'t'|'x']*[:ext[:int]]`.
/// Returns `(base ("r"/"w"/"a" [+ "+"]), binmode, textmode, enc_part)`.
fn parse_fmode(s: &str) -> Result<(String, bool, bool, Option<String>)> {
    let invalid = || MonorubyErr::argumenterr(format!("invalid access mode {s}"));
    let (m, enc) = match s.split_once(':') {
        Some((a, b)) => (a, Some(b.to_string())),
        None => (s, None),
    };
    let mut chars = m.chars();
    let first = chars.next().ok_or_else(invalid)?;
    if !matches!(first, 'r' | 'w' | 'a') {
        return Err(invalid());
    }
    let (mut plus, mut bin, mut text) = (false, false, false);
    for c in chars {
        match c {
            '+' if !plus => plus = true,
            'b' if !bin && !text => bin = true,
            't' if !text && !bin => text = true,
            'x' if first == 'w' => {}
            _ => return Err(invalid()),
        }
    }
    let mut base = first.to_string();
    if plus {
        base.push('+');
    }
    Ok((base, bin, text, enc))
}

/// The shared body of `IO.new` / `IO.for_fd` / `IO#initialize` / `IO.open`
/// (CRuby's `rb_io_initialize` + `rb_io_extract_modeenc`): validates the
/// fd, resolves mode/binmode/encodings from the positional mode argument
/// and keyword options (with all the "specified twice" conflict checks),
/// and (re)associates `obj` with the descriptor.
pub(super) fn io_init_from_fd(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    mut obj: Value,
) -> Result<()> {
    // fd: implicit #to_int conversion (TypeError for nil/String/IO).
    let fd = lfp.arg(0).coerce_to_int_i64(vm, globals)?;
    let fd_i32 = fd as i32;
    if fd_i32 < 0 || unsafe { libc::fcntl(fd_i32, libc::F_GETFD) } == -1 {
        let err = std::io::Error::from_raw_os_error(libc::EBADF);
        // CRuby's message is the bare strerror ("Bad file descriptor").
        return Err(MonorubyErr::errno_plain(&globals.store, &err));
    }

    let kw: Option<crate::value::Hashmap> = lfp.try_arg(2).and_then(|v| v.try_hash_ty());

    // Mode: positional arg vs `mode:` option — both given is an error.
    let pos_mode = lfp.try_arg(1).filter(|v| !v.is_nil());
    let kw_mode = kw_opt(vm, globals, &kw, "mode")?;
    if pos_mode.is_some() && kw_mode.is_some() {
        return Err(MonorubyErr::argumenterr("mode specified twice"));
    }
    // A mode value is a String (#to_str) or an Integer of File::Constants
    // flags (#to_int); try string conversion first, like CRuby.
    let mode_str: Option<String> = match pos_mode.or(kw_mode) {
        None => None,
        Some(v) => {
            if let Some(s) = v.is_str() {
                Some(s.to_string())
            } else if globals.check_method(v, IdentId::TO_STR).is_some() {
                Some(v.coerce_to_string(vm, globals)?)
            } else {
                let flags = v.coerce_to_int_i64(vm, globals)?;
                let m = super::file::mode_string_from_flags(flags);
                // The internal no-truncate spellings ("w-"/"-w") only
                // affect File-open truncation; wrapping an existing fd
                // never truncates, so they mean plain write-only here.
                Some(match m.as_str() {
                    "w-" | "-w" => "w".to_string(),
                    _ => m,
                })
            }
        }
    };

    let (fd_r, fd_w) = fd_rw_mode(fd_i32);
    let (base, bin_str, text_str, menc) = match &mode_str {
        Some(s) => parse_fmode(s)?,
        None => {
            // Default mode from the descriptor's own access mode.
            let base = match (fd_r, fd_w) {
                (true, true) => "r+",
                (false, true) => "w",
                _ => "r",
            };
            (base.to_string(), false, false, None)
        }
    };

    // binmode/textmode keywords, replicating CRuby's `extract_binmode`
    // exactly: a keyword matching a mode-string flag is "specified
    // twice", a keyword against the *other* flag is "both textmode and
    // binmode specified", and a `false` keyword sets nothing.
    let kw_bin = kw_opt(vm, globals, &kw, "binmode")?;
    let kw_text = kw_opt(vm, globals, &kw, "textmode")?;
    let mut fmode_bin = bin_str;
    let mut fmode_text = text_str;
    if let Some(v) = kw_text {
        if fmode_text {
            return Err(MonorubyErr::argumenterr("textmode specified twice"));
        }
        if fmode_bin {
            return Err(MonorubyErr::argumenterr(
                "both textmode and binmode specified",
            ));
        }
        if v.as_bool() {
            fmode_text = true;
        }
    }
    if let Some(v) = kw_bin {
        if fmode_bin {
            return Err(MonorubyErr::argumenterr("binmode specified twice"));
        }
        if fmode_text {
            return Err(MonorubyErr::argumenterr(
                "both textmode and binmode specified",
            ));
        }
        if v.as_bool() {
            fmode_bin = true;
        }
    }
    if fmode_bin && fmode_text {
        return Err(MonorubyErr::argumenterr(
            "both textmode and binmode specified",
        ));
    }
    let binmode = fmode_bin;

    // Encodings: the mode-string suffix conflicts with any encoding
    // keyword; `encoding:` is ignored (with a warning) when an explicit
    // external/internal_encoding is also given.
    let kw_enc = kw_opt(vm, globals, &kw, "encoding")?;
    let kw_ext = kw_opt(vm, globals, &kw, "external_encoding")?;
    let kw_int = kw_opt(vm, globals, &kw, "internal_encoding")?;
    if menc.is_some() && (kw_enc.is_some() || kw_ext.is_some() || kw_int.is_some()) {
        return Err(MonorubyErr::argumenterr("encoding specified twice"));
    }
    let kw_enc = match kw_enc {
        Some(e) if kw_ext.is_some() || kw_int.is_some() => {
            let enc_desc = if let Some(s) = e.is_str() {
                s.to_string()
            } else {
                e.to_s(globals)
            };
            let used = if kw_ext.is_some() {
                "external_encoding"
            } else {
                "internal_encoding"
            };
            let msg = Value::string(format!(
                "warning: Ignoring encoding parameter '{enc_desc}': {used} is used"
            ));
            let main = globals.main_object;
            let _ =
                vm.invoke_method_inner(globals, IdentId::get_id("warn"), main, &[msg], None, None);
            None
        }
        other => other,
    };

    // Resolve explicit encoding objects. String-ish values go through
    // #to_str (implicit conversion); `internal_encoding: "-"` means "no
    // conversion" (nil internal).
    fn enc_value_to_obj(
        vm: &mut Executor,
        globals: &mut Globals,
        v: Value,
    ) -> Result<Option<Value>> {
        if v.class() == super::encoding::encoding_class(globals) {
            return Ok(Some(v));
        }
        let name = v.coerce_to_string(vm, globals)?;
        Ok(enc_by_name(globals, &name))
    }
    let mut ext_obj: Option<Value> = None;
    let mut int_obj: Option<Value> = None;
    if let Some(enc_part) = &menc {
        let mut parts = enc_part.splitn(2, ':');
        ext_obj = parts
            .next()
            .filter(|x| !x.is_empty())
            .and_then(|x| enc_by_name(globals, x));
        int_obj = parts
            .next()
            .filter(|x| !x.is_empty() && *x != "-")
            .and_then(|x| enc_by_name(globals, x));
    }
    if let Some(e) = kw_enc {
        if e.class() == super::encoding::encoding_class(globals) {
            ext_obj = Some(e);
        } else {
            let s = e.coerce_to_string(vm, globals)?;
            let mut parts = s.splitn(2, ':');
            ext_obj = parts
                .next()
                .filter(|x| !x.is_empty())
                .and_then(|x| enc_by_name(globals, x));
            int_obj = parts
                .next()
                .filter(|x| !x.is_empty() && *x != "-")
                .and_then(|x| enc_by_name(globals, x));
        }
    }
    if let Some(e) = kw_ext {
        ext_obj = enc_value_to_obj(vm, globals, e)?;
    }
    if let Some(i) = kw_int {
        int_obj = if i.is_str().is_some_and(|s| s == "-") {
            None
        } else {
            enc_value_to_obj(vm, globals, i)?
        };
    }

    // The requested access mode must be satisfiable by the fd itself.
    let (readable, writable) = match base.as_str() {
        "r" => (true, false),
        "w" | "a" => (false, true),
        _ => (true, true), // "r+", "w+", "a+"
    };
    if (readable && !fd_r) || (writable && !fd_w) {
        let err = std::io::Error::from_raw_os_error(libc::EINVAL);
        return Err(MonorubyErr::errno_plain(&globals.store, &err));
    }

    // `:autoclose` / `:path` options; never double-own an fd another
    // monoruby IO already closes (see OWNED_FDS in value/rvalue/io.rs).
    let autoclose = kw_opt(vm, globals, &kw, "autoclose")?
        .map(|v| v.as_bool())
        .unwrap_or(true);
    let (name, has_path) = match kw_opt(vm, globals, &kw, "path")? {
        Some(p) => (p.coerce_to_string(vm, globals)?, true),
        None => (format!("fd {}", fd), false),
    };
    let effective_autoclose = autoclose && !fd_is_owned(fd_i32);
    let io_inner = IoInner::from_raw_fd_autoclose(
        fd_i32,
        name,
        has_path,
        readable,
        writable,
        effective_autoclose,
    );
    *obj.as_io_inner_mut() = io_inner;

    let de = enc_default_external_obj(globals);
    let di = enc_default_internal_obj(globals);
    let (slot, i) =
        resolve_io_encodings(globals, ext_obj, int_obj, binmode, readable, writable, de, di);
    store_io_encodings(globals, obj, slot, i);
    if binmode {
        obj.as_io_inner_mut().set_binmode();
    }
    Ok(())
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
    extra: Option<Value>,
) -> Result<(String, bool, bool)> {
    let mut autoclose = true;
    let mut name = format!("fd {}", fd);
    let mut has_path = false;
    let candidates = range
        .filter_map(|i| lfp.try_arg(i))
        .chain(extra)
        .collect::<Vec<_>>();
    for arg in candidates {
        if let Some(h) = arg.try_hash_ty() {
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

/// `IdentId`s the read paths would otherwise re-intern on every call.
/// Interning takes the global identifier table's lock and hashes the
/// name; `IO#gets` did it six or seven times per line.


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
///
/// The unset fallback resolves `Encoding::UTF_8` by constant id — NOT
/// through `find_encoding_object`, which scans every constant under
/// `Encoding` normalizing names and made each read on an
/// `ExtEnc::Dynamic` stream (any plain `File.open(name)`) ~40× slower.
pub(super) fn enc_default_external_obj(globals: &mut Globals) -> Value {
    if let Some(v) = globals.get_gvar(IdentId::get_id("$DEFAULT_EXTERNAL"))
        && !v.is_nil()
    {
        return v;
    }
    let enc_class = super::encoding::encoding_class(globals);
    globals
        .store
        .get_constant_noautoload(enc_class, IdentId::UTF_8)
        .unwrap_or(Value::nil())
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

/// Normalize a user-facing encoding argument: Encoding objects, nil and
/// Strings pass through; anything else is offered `#to_str` (the String
/// result feeds the by-name lookup downstream).
fn coerce_enc_arg(vm: &mut Executor, globals: &mut Globals, v: Value) -> Result<Value> {
    if v.is_nil()
        || v.is_str().is_some()
        || v.class() == super::encoding::encoding_class(globals)
    {
        return Ok(v);
    }
    if globals.store.check_method(v, IdentId::TO_STR).is_some() {
        let s = v.coerce_to_str(vm, globals)?;
        return Ok(Value::string(s));
    }
    Ok(v)
}

/// Argument `Value` -> Encoding object: an `Encoding` is taken as-is, a
/// String is resolved by name; anything else (incl. `nil`) -> `None`.
pub(super) fn arg_to_enc_obj(globals: &Globals, v: Value) -> Option<Value> {
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
    writable: bool,
    de: Value,
    di: Option<Value>,
) -> (ExtSlot, Option<Value>) {
    // `b`/binmode forces a BINARY external only when no encoding at all
    // was given (CRuby's `has_enc` check: an explicit *internal* encoding
    // also disables the binary default).
    let ext = if explicit_ext.is_none() && explicit_int.is_none() && binmode {
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
                // Only a read-*only* stream inherits (and keeps
                // tracking) Encoding.default_external; read-write and
                // write-only streams report nil until an encoding is
                // set explicitly (CRuby).
                if readable && !writable {
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

/// Persist a resolved encoding pair on the IO object.
fn store_io_encodings(_globals: &mut Globals, mut io: Value, ext: ExtSlot, int: Option<Value>) {
    let (state, obj) = match ext {
        ExtSlot::Fixed(v) => (ExtEnc::Fixed, Some(v)),
        ExtSlot::Dynamic => (ExtEnc::Dynamic, None),
        ExtSlot::Nil => (ExtEnc::Nil, None),
    };
    let inner = io.as_io_inner_mut();
    inner.set_ext_enc(state, obj);
    inner.set_int_enc(int.filter(|v| !v.is_nil()));
}

/// The stream's external encoding when it is a *concrete, non-binary*
/// one — i.e. neither unset, nor nil, nor "follow
/// `Encoding.default_external`", nor ASCII-8BIT. Both callers use it to
/// answer "has a real external encoding already been chosen?".
fn fixed_ext_encoding(globals: &mut Globals, io: Value) -> Option<Value> {
    let inner = io.as_io_inner();
    if inner.ext_state() != ExtEnc::Fixed {
        return None;
    }
    let e = inner.ext_enc()?;
    if e.is_nil() || enc_is_binary(globals, e) {
        None
    } else {
        Some(e)
    }
}

/// Strip a leading "BOM|" (case-insensitive) from an external-encoding
/// name, reporting whether it was present.
fn split_bom_prefix(name: &str) -> (&str, bool) {
    if name.len() >= 4 && name[..4].eq_ignore_ascii_case("bom|") {
        (&name[4..], true)
    } else {
        (name, false)
    }
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
    extra: Option<Value>,
) -> Result<(Option<Value>, Option<Value>, bool, bool)> {
    let base = mode.split(':').next().unwrap_or("");
    let binmode = base.contains('b');
    let (mext, mint) = mode_encoding_names(mode);
    // A "BOM|utf-..." external requests byte-order-mark detection; the
    // name after the prefix is the fallback when no BOM is present.
    let (mext, bom) = match mext {
        Some(e) => {
            let (n, b) = split_bom_prefix(e);
            (Some(n), b)
        }
        None => (None, false),
    };
    let mut ext = mext.and_then(|s| enc_by_name(globals, s));
    let mut int = mint.and_then(|s| enc_by_name(globals, s));

    let candidates = opt_range
        .filter_map(|i| lfp.try_arg(i))
        .chain(extra)
        .collect::<Vec<_>>();
    for arg in candidates {
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
    Ok((ext, int, binmode, bom))
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
    bom: &mut bool,
) -> Result<()> {
    if let Some(m) = h.get(Value::symbol(IdentId::get_id("mode")), vm, globals)?
        && let Some(s) = m.is_str()
    {
        *mode = s.to_string();
        let (me, mi) = mode_encoding_names(mode);
        if let Some(e) = me {
            let (e, b) = split_bom_prefix(e);
            *bom = b;
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
) -> Result<(String, Option<Value>, Option<Value>, bool)> {
    let mut mode = "r".to_string();
    let mut ext = None;
    let mut int = None;
    let mut bom = false;
    let Some(h) = opts else {
        return Ok((mode, ext, int, bom));
    };
    if let Some(oa) = h.get(Value::symbol(IdentId::get_id("open_args")), vm, globals)?
        && let Some(ary) = oa.try_array_ty()
    {
        for v in ary.iter() {
            if let Some(s) = v.is_str() {
                mode = s.to_string();
                let (me, mi) = mode_encoding_names(&mode);
                if let Some(e) = me {
                    let (e, b) = split_bom_prefix(e);
                    bom = b;
                    ext = enc_by_name(globals, e);
                }
                if let Some(i) = mi {
                    int = enc_by_name(globals, i);
                }
            } else if let Some(hh) = v.try_hash_ty() {
                read_opts_from_hash(vm, globals, hh, &mut mode, &mut ext, &mut int, &mut bom)?;
            }
        }
        return Ok((mode, ext, int, bom));
    }
    read_opts_from_hash(vm, globals, h, &mut mode, &mut ext, &mut int, &mut bom)?;
    Ok((mode, ext, int, bom))
}

/// Resolve + store IO encodings at creation time. `opt_range` is the
/// `lfp` arg span to scan for a trailing options Hash.
pub(super) fn init_io_encodings(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    mut io: Value,
    mode: &str,
    readable: bool,
    opt_range: std::ops::Range<usize>,
    extra: Option<Value>,
) -> Result<()> {
    let (ext, int, binmode, bom) =
        parse_open_encodings(vm, globals, lfp, opt_range, mode, extra)?;
    // The stream's write-ability decides whether a missing external
    // encoding tracks default_external (read-only) or stays nil.
    let base = mode.split(':').next().unwrap_or("");
    let writable = base.contains('w') || base.contains('a') || base.contains('+');
    let de = enc_default_external_obj(globals);
    let di = enc_default_internal_obj(globals);
    let (slot, i) = resolve_io_encodings(globals, ext, int, binmode, readable, writable, de, di);
    store_io_encodings(globals, io, slot, i);
    if binmode {
        io.as_io_inner_mut().set_binmode();
    }
    // A "BOM|utf-..." encoding: peek the stream for a byte-order mark;
    // a detected BOM is consumed and *overrides* the external encoding,
    // otherwise the declared one stays.
    if bom && readable {
        detect_and_consume_bom(vm, globals, io)?;
    }
    Ok(())
}

/// Allocator for `IO` and its subclasses.
pub(crate) extern "C" fn io_alloc_func(class_id: ClassId, _: &mut Globals) -> Value {
    Value::new_io_with_class(IoInner::closed(None), class_id)
}


/// Apply the IO's external-encoding conversion to a to-be-written value
/// (CRuby's `do_writeconv`): with a fixed, non-BINARY external encoding
/// that differs from the string's own, the string is transcoded through
/// `String#encode` dispatch — so the conversion rules and error classes
/// (Encoding::UndefinedConversionError for an unconvertible BINARY
/// source, invalid-byte errors, ...) match String#encode exactly.
/// Returns the bytes to write. `syswrite`/`write_nonblock` deliberately
/// bypass this (they are raw fd writes).
pub(super) fn bytes_for_write(
    vm: &mut Executor,
    globals: &mut Globals,
    io: Value,
    v: Value,
) -> Result<Vec<u8>> {
    let sval = if v.is_rstring().is_some() {
        v
    } else {
        Value::string(vm.to_s(globals, v)?)
    };
    let ext = match fixed_ext_encoding(globals, io) {
        Some(e) => e,
        None => return Ok(sval.is_rstring().unwrap().to_vec()),
    };
    let same = match (
        sval.is_rstring().map(|r| r.encoding()),
        enc_obj_to_enum(globals, ext),
    ) {
        (Some(se), Some(ee)) => se == ee,
        _ => false,
    };
    if same {
        return Ok(sval.is_rstring().unwrap().to_vec());
    }
    let encoded =
        vm.invoke_method_inner(globals, IdentId::get_id("encode"), sval, &[ext], None, None)?;
    Ok(match encoded.is_rstring() {
        Some(r) => r.to_vec(),
        None => encoded.to_s(&globals.store).into_bytes(),
    })
}

///
/// ### IO#<<
///
/// - self << object -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/=3c=3c.html]
#[monoruby_builtin]
fn shl(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let bytes = bytes_for_write(vm, globals, lfp.self_val(), lfp.arg(0))?;
    let mut done = 0;
    blocking_io_region(vm, globals, lfp.self_val(), libc::POLLOUT, |_store| {
        lfp.self_val().as_io_inner_mut().write(&bytes, &mut done, _store)
    })?;
    lfp.self_val().as_io_inner_mut().flush(&globals.store)?;
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
    // CRuby's rb_io_puts: a real Array recurses; any other non-String is
    // first offered `#to_ary` (an Array result recurses, `nil` falls back
    // to `#to_s`, anything else is the usual conversion TypeError).
    fn decompose(
        vm: &mut Executor,
        globals: &mut Globals,
        collector: &mut Vec<Value>,
        val: Value,
        seen: &mut Vec<u64>,
    ) -> Result<()> {
        if let Some(ary) = val.try_array_ty() {
            let id = val.id();
            if seen.contains(&id) {
                collector.push(Value::string("[...]".to_string()));
            } else {
                seen.push(id);
                for v in ary.iter() {
                    decompose(vm, globals, collector, *v, seen)?;
                }
                seen.pop();
            }
            return Ok(());
        }
        if val.is_rstring().is_none()
            && globals
                .store
                .check_method(val, IdentId::get_id("to_ary"))
                .is_some()
        {
            let converted =
                vm.invoke_method_inner(globals, IdentId::get_id("to_ary"), val, &[], None, None)?;
            if converted.try_array_ty().is_some() {
                return decompose(vm, globals, collector, converted, seen);
            }
            if !converted.is_nil() {
                return Err(MonorubyErr::typeerr(format!(
                    "can't convert {} to Array ({}#to_ary gives {})",
                    val.get_real_class_name(&globals.store),
                    val.get_real_class_name(&globals.store),
                    converted.get_real_class_name(&globals.store)
                )));
            }
        }
        collector.push(val);
        Ok(())
    }
    let mut collector = Vec::new();
    let mut seen = Vec::new();
    let args = lfp.arg(0).as_array();
    let no_args = args.is_empty();
    for v in args.iter().cloned() {
        decompose(vm, globals, &mut collector, v, &mut seen)?;
    }

    let self_val = lfp.self_val();
    let write_id = IdentId::get_id("write");
    if no_args {
        // puts with no args writes a newline. (An empty *array* argument
        // decomposes to nothing and writes nothing — only the genuinely
        // argument-less call gets the bare newline.)
        let newline = Value::string_from_str("\n");
        vm.invoke_method_inner(globals, write_id, self_val, &[newline], None, None)?;
    } else {
        for v in collector {
            let s = if v.is_nil() {
                String::new()
            } else if let Some(rs) = v.is_rstring() {
                String::from_utf8_lossy(rs.as_bytes()).into_owned()
            } else {
                // Stringify through the value's (possibly Ruby-defined)
                // `to_s`, matching CRuby's `rb_obj_as_string`; fall back
                // to the Rust-side formatter when `to_s` misbehaves.
                let sv =
                    vm.invoke_method_inner(globals, IdentId::get_id("to_s"), v, &[], None, None)?;
                match sv.is_rstring() {
                    Some(rs) => String::from_utf8_lossy(rs.as_bytes()).into_owned(),
                    None => v.to_s(globals),
                }
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
    self_.as_io_inner_mut().flush(&globals.store)?;
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
    // Each object goes through `#to_s` (never `#to_str`); the output
    // field separator `$,` goes between objects and the output record
    // separator `$\` is appended, both only when non-nil (CRuby).
    let to_str_val = |vm: &mut Executor, globals: &mut Globals, v: Value| -> Result<Value> {
        if v.is_rstring().is_some() {
            Ok(v)
        } else {
            let s = vm.to_s(globals, v)?;
            Ok(Value::string(s))
        }
    };
    let sep = Some(globals.ofs()).filter(|v| !v.is_nil());
    let rs = Some(globals.ors()).filter(|v| !v.is_nil());
    let args = lfp.arg(0).as_array();
    if args.is_empty() {
        // `print` with no arguments writes `$_` (the caller's frame-local
        // last-read line; resolved past this native frame). A nil `$_`
        // still writes `$\`.
        let lastline = vm.get_last_read_line();
        if !lastline.is_nil() {
            let sv = to_str_val(vm, globals, lastline)?;
            vm.invoke_method_inner(globals, write_id, self_val, &[sv], None, None)?;
        }
    } else {
        for (i, v) in args.iter().cloned().enumerate() {
            if i > 0 && let Some(sep) = sep {
                let sv = to_str_val(vm, globals, sep)?;
                vm.invoke_method_inner(globals, write_id, self_val, &[sv], None, None)?;
            }
            let str_val = to_str_val(vm, globals, v)?;
            vm.invoke_method_inner(globals, write_id, self_val, &[str_val], None, None)?;
        }
    }
    if let Some(rs) = rs {
        let sv = to_str_val(vm, globals, rs)?;
        vm.invoke_method_inner(globals, write_id, self_val, &[sv], None, None)?;
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
    let args = lfp.arg(1).as_array();
    // Format through the shared encoding negotiation so a binary or
    // broken argument reaches the fd as its own bytes.
    let fmt_inner = match lfp.arg(0).is_rstring_inner() {
        Some(inner) => inner.clone(),
        None => {
            let s = lfp.arg(0).coerce_to_string(vm, globals)?;
            RStringInner::from_encoding_scanned(s.as_bytes(), Encoding::Utf8)
        }
    };
    let ctx = crate::executor::format::negotiate_format(&globals.store, &fmt_inner, &args)?;
    let format_str = ctx.view_owned(&fmt_inner)?;
    let out = vm.format_by_args(globals, &format_str, &args, ctx)?;
    let buf = ctx.finish(&out).as_rstring_inner().as_bytes().to_vec();
    let mut done = 0;
    blocking_io_region(vm, globals, lfp.self_val(), libc::POLLOUT, |_store| {
        lfp.self_val()
            .as_io_inner_mut()
            .write(&buf, &mut done, _store)
    })?;

    Ok(Value::nil())
}

///
/// ### IO#flush
///
/// - flush -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/flush.html]
#[monoruby_builtin]
fn flush(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let mut self_ = lfp.self_val();
    self_.as_io_inner_mut().flush(&globals.store)?;

    Ok(lfp.self_val())
}

/// Parse the `(sep = $/, limit = nil)` positional arguments shared by
/// `IO#gets` and the other line readers, plus the `chomp:` keyword sitting
/// in arg slot `kw_chomp_idx`.
///
/// Returns `(separator, limit, chomp)`: `separator == None` slurps to EOF
/// (explicit `nil`, or `$/ == nil`); `limit == None` is unlimited (absent
/// or negative). A single non-String argument is taken as the limit
/// (coerced with `#to_int`); with two arguments the first must be a
/// String (`#to_str`). A limit outside `i64` raises `RangeError`.
pub(super) fn getline_args(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    kw_chomp_idx: usize,
) -> Result<(Option<Vec<u8>>, Option<usize>, bool)> {
    // Default separator: `$/` — "\n" unless reassigned; nil slurps.
    let mut sep: Option<Vec<u8>> = globals.rs_bytes();
    let mut limit_arg: Option<Value> = None;
    match (lfp.try_arg(0), lfp.try_arg(1)) {
        (None, _) => {}
        (Some(v0), None) => {
            if v0.is_nil() {
                sep = None;
            } else if let Some(rs) = v0.is_rstring() {
                sep = Some(rs.as_bytes().to_vec());
            } else if globals.check_method(v0, IdentId::TO_STR).is_some() {
                sep = Some(v0.coerce_to_rstring(vm, globals)?.as_bytes().to_vec());
            } else {
                limit_arg = Some(v0);
            }
        }
        (Some(v0), Some(v1)) => {
            if v0.is_nil() {
                sep = None;
            } else {
                sep = Some(v0.coerce_to_rstring(vm, globals)?.as_bytes().to_vec());
            }
            limit_arg = Some(v1);
        }
    }
    let limit = match limit_arg {
        None => None,
        Some(v) => {
            let l = v.coerce_to_int_i64(vm, globals)?;
            if l < 0 { None } else { Some(l as usize) }
        }
    };
    let chomp = lfp.try_arg(kw_chomp_idx).is_some_and(|v| v.as_bool());
    Ok((sep, limit, chomp))
}

/// Apply `chomp: true` to a line read with `sep`.
///
/// - default/custom separator: remove one trailing `sep` (for `"\n"`,
///   `"\r\n"` is removed as a unit, like `String#chomp`);
/// - paragraph mode (`""`): remove the one `"\n\n"` that terminated the
///   paragraph (a final paragraph cut short by EOF keeps its bytes);
/// - `nil` separator: no-op — CRuby only chomps at a separator boundary,
///   and a slurped read has none.
pub(super) fn chomp_line(buf: &mut Vec<u8>, sep: Option<&[u8]>, _limit: Option<usize>) {
    match sep {
        None => {}
        Some([]) => {
            if buf.ends_with(b"\n\n") {
                buf.truncate(buf.len() - 2);
            }
        }
        Some(s) => {
            if buf.ends_with(s) {
                buf.truncate(buf.len() - s.len());
                if s == b"\n" && buf.ends_with(b"\r") {
                    buf.pop();
                }
            }
        }
    }
}

/// Whether reads on this IO should complete UTF-8 characters cut by a
/// byte limit (true unless the external encoding is a single-byte one).
fn io_completes_utf8(globals: &mut Globals, io: Value) -> bool {
    use crate::value::Encoding as E;
    let ext_v = read_io_encoding(globals, io, false);
    matches!(enc_obj_to_enum(globals, ext_v).unwrap_or(E::Utf8), E::Utf8)
}

/// Bump the IO's line counter and `$.` after a successful line read.
fn bump_lineno(globals: &mut Globals, mut io: Value) -> Result<()> {
    let n = io.as_io_inner_mut().bump_lineno();
    globals.set_lineno(Value::integer(n));
    Ok(())
}

/// Run a blocking IO operation with CRuby-like interrupt semantics.
///
/// The interruptible read primitives (see value/rvalue/io.rs) surface
/// "EINTR with a pending signal" as the internal
/// `MonorubyErr::signal_interrupt` marker instead of silently retrying the
/// syscall (which is what makes a blocked read un-killable by SIGTERM).
/// This wrapper catches the marker and runs the VM poll point: the default
/// disposition raises the converted `SignalException` out of the read,
/// while a `Signal.trap` handler runs and — when it returns normally —
/// the operation is restarted (bytes consumed before the interrupt were
/// pushed back by the primitives, so no data is lost).
///
/// A signal that was already pending on entry is drained up front: it was
/// delivered before the syscall (re-)entered the kernel, so it can never
/// EINTR it, and the operation could otherwise block forever with the
/// signal sitting in the bitmap (same reasoning as `Kernel#sleep`).
/// Additionally, for fd-backed operations: when other green
/// threads are live and the operation would block (fd not ready, nothing
/// buffered), park the current thread on the scheduler's fd poller so
/// the other threads run in the meantime, instead of blocking the whole
/// process. Readiness is re-checked after every wake (spurious wakeups
/// and error/hangup revents fall through to `f`, which surfaces the real
/// outcome).
///
/// The entry check alone would leave an operation that consumes the
/// available data and then needs more (e.g. `read(n)` beyond what a pipe
/// holds) blocking the process mid-way. To cover the middle of the
/// operation too, `f` runs with the fd in temporary non-blocking mode
/// (`NonblockGuard`) while other green threads are live: a kernel entry
/// that would block returns `EAGAIN`, the primitives push back what was
/// already consumed and surface the internal would-block marker, and the
/// thread parks on the fd poller before restarting `f`. With no other
/// live threads the marker can still surface — from an fd left
/// permanently non-blocking by `read_nonblock`/`write_nonblock` — and is
/// waited out with a plain (signal-interruptible) `poll(2)`, matching
/// CRuby, where buffered IO on a non-blocking fd still blocks the caller.
/// Bridge for error classes: the read/write primitives below `IoInner`
/// have no `Store`, so raw OS errors surface as
/// `RuntimeError("… (os error N)")` instead of the `Errno::*` class
/// CRuby raises (e.g. `read` on a listening socket must be
/// `Errno::ENOTCONN`). Rebuild such errors here, where `globals` is in
/// reach, by parsing the errno std::io::Error stably formats into the
/// message. A structural fix (errors carrying their errno) can replace
/// this; until then every blocking-region operation gets the right
/// class on every platform.
fn errnoify(globals: &Globals, err: MonorubyErr) -> MonorubyErr {
    if !matches!(err.kind(), MonorubyErrKind::Runtime) {
        return err;
    }
    let msg = err.message();
    if let Some(pos) = msg.rfind("(os error ")
        && let Some(end) = msg[pos..].find(')')
        && let Ok(errno) = msg[pos + 10..pos + end].parse::<i32>()
    {
        let io_err = std::io::Error::from_raw_os_error(errno);
        return MonorubyErr::from_io_err(&globals.store, &io_err, msg.to_string());
    }
    err
}

pub(crate) fn blocking_io_region<T>(
    vm: &mut Executor,
    globals: &mut Globals,
    io: Value,
    events: i16,
    mut f: impl FnMut(&Store) -> Result<T>,
) -> Result<T> {
    loop {
        // Entry parking (wait for readiness *before* attempting the
        // operation) is kept only for the std streams: their open file
        // description is shared with the parent shell, so the
        // would-block emulation's non-blocking flag must never touch
        // them, and parking first is the only way not to block the
        // process. Every other fd attempts the operation first — a
        // would-block parks below, and an fd that can never signal
        // readiness (e.g. `read` on a *listening* socket, which poll
        // only wakes for an incoming connection) surfaces its real
        // error — `Errno::ENOTCONN` / `EINVAL` per platform — instead
        // of hanging (ruby/spec: `TCPServer#gets raises
        // Errno::ENOTCONN`).
        if crate::scheduler::has_other_live_threads()
            && !(events & libc::POLLIN != 0 && io.as_io_inner().has_buffered_data())
            && !io.as_io_inner().is_closed()
            && let Ok(fd) = io.as_io_inner().wait_fd_for(events)
            && (0..=2).contains(&fd)
            && poll_single_fd(io, events, 0)? == 0
        {
            crate::scheduler::wait_fd(vm, globals, fd, events, None)?;
            continue;
        }
        if crate::codegen::signal_table::PENDING_SIGNALS.load(std::sync::atomic::Ordering::Relaxed)
            != 0
            && crate::executor::execute_gc(vm, globals).is_none()
        {
            return Err(vm.take_error());
        }
        // Mid-operation would-block emulation. Skipped for the std streams
        // (fds 0-2): their open file description is shared with the parent
        // shell and other processes, and a non-blocking flag leaked by an
        // abnormal exit mid-operation would corrupt *their* IO — the classic
        // "nonblocking stdout" bug. Std streams keep entry-only parking.
        let guard = if crate::scheduler::has_other_live_threads()
            && !io.as_io_inner().is_closed()
            && let Ok(fd) = io.as_io_inner().wait_fd_for(events)
            && fd > 2
        {
            crate::value::rvalue::NonblockGuard::set(fd)
        } else {
            None
        };
        let res = f(&globals.store);
        // Restore the fd's blocking mode *before* parking: while this
        // thread is parked, other green threads (or a spawned child) may
        // use the same fd and must see it in its original mode.
        drop(guard);
        match res {
            Err(err) if err.is_signal_interrupt() => {
                if crate::executor::execute_gc(vm, globals).is_none() {
                    return Err(vm.take_error());
                }
            }
            Err(err) if err.is_would_block_interrupt() => {
                let fd = io.as_io_inner().wait_fd_for(events)?;
                if crate::scheduler::has_other_live_threads() {
                    crate::scheduler::wait_fd(vm, globals, fd, events, None)?;
                } else {
                    wait_fd_single(vm, globals, fd, events)?;
                }
            }
            Err(err) => return Err(errnoify(globals, err)),
            res => return res,
        }
    }
}

/// Wait until `fd` reports one of `events`, blocking the whole process —
/// used when a would-block interrupt surfaces with no other green threads
/// to run (an fd left permanently non-blocking by `read_nonblock` /
/// `write_nonblock`). Signal-interruptible: `EINTR` (and a signal already
/// pending on entry) runs the VM poll point, then re-polls.
pub(crate) fn wait_fd_single(
    vm: &mut Executor,
    globals: &mut Globals,
    fd: i32,
    events: i16,
) -> Result<()> {
    loop {
        if crate::codegen::signal_table::PENDING_SIGNALS.load(std::sync::atomic::Ordering::Relaxed)
            != 0
            && crate::executor::execute_gc(vm, globals).is_none()
        {
            return Err(vm.take_error());
        }
        let mut pfd = libc::pollfd {
            fd,
            events,
            revents: 0,
        };
        // SAFETY: `poll` accepts a pointer to an array of pollfd; we pass
        // exactly one element with length 1.
        let ret = unsafe { libc::poll(&mut pfd, 1, -1) };
        if ret < 0 {
            let err = std::io::Error::last_os_error();
            if err.raw_os_error() == Some(libc::EINTR) {
                continue;
            }
            return Err(MonorubyErr::ioerr(format!("poll failed: {err}")));
        }
        // Ready (or an error/hangup revent — the retried operation
        // surfaces the real outcome).
        return Ok(());
    }
}

/// Shared body of `IO#gets` / `IO#readline`: read one line per the parsed
/// arguments, update lineno / `$.` / `$_`, and return the tagged String
/// (nil at EOF).
fn gets_inner(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let (sep, limit, chomp) = getline_args(vm, globals, lfp, 2)?;
    let self_ = lfp.self_val();
    let (ext, intl) = io_encodings(globals, self_);
    let complete_utf8 = ext == crate::value::Encoding::Utf8;
    let line = blocking_io_region(vm, globals, lfp.self_val(), libc::POLLIN, |_store| {
        lfp.self_val()
            .as_io_inner_mut()
            .getline(sep.as_deref(), limit, complete_utf8)
    })?;
    match line {
        Some(mut buf) => {
            if chomp {
                chomp_line(&mut buf, sep.as_deref(), limit);
            }
            bump_lineno(globals, self_)?;
            let s = tagged_read_string_with(globals, buf, ext, intl);
            // `$_` is frame-local: set it on the calling Ruby scope's
            // LEP (the builtin's own native frame is skipped).
            vm.set_last_read_line(s);
            Ok(s)
        }
        None => {
            vm.set_last_read_line(Value::nil());
            Ok(Value::nil())
        }
    }
}

///
/// ### IO#gets
///
/// - gets(rs = $/, limit = nil, chomp: false) -> String | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/gets.html]
#[monoruby_builtin]
fn gets(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    gets_inner(vm, globals, lfp)
}

///
/// ### IO#readline
///
/// - readline(rs = $/, limit = nil, chomp: false) -> String
///
/// Like `#gets`, but raises `EOFError` at end of file. Implemented
/// natively (not as a Ruby wrapper over `#gets`) so `$_` is assigned in
/// the *caller's* frame, like CRuby's C implementation.
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/readline.html]
#[monoruby_builtin]
fn readline(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let line = gets_inner(vm, globals, lfp)?;
    if line.is_nil() {
        return Err(MonorubyErr::eoferr(&globals.store, "end of file reached"));
    }
    Ok(line)
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
    // CRuby's `IO#close` is idempotent — closing an already-closed
    // stream is a no-op that returns nil (no IOError).
    if lfp.self_val().as_io_inner().is_closed() {
        return Ok(Value::nil());
    }
    let popen_result = lfp.self_val().as_io_inner_mut().close(&globals.store)?;
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
        crate::scheduler::set_last_status(vm, status_obj);
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
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let mut self_ = lfp.self_val();
    let io = self_.as_io_inner_mut();
    match io.kind() {
        IoKind::Popen(_) => {
            io.popen_close_write();
        }
        // CRuby: no-op on an already-closed stream.
        IoKind::Closed(..) => {}
        // CRuby: a stream that is not readable (nothing left once the
        // write side goes) is simply closed; only a readable non-duplex
        // stream refuses.
        _ if io.is_readable() => {
            return Err(MonorubyErr::ioerr("closing non-duplex IO for writing"));
        }
        _ => {
            io.close(&globals.store)?;
        }
    }
    Ok(Value::nil())
}

/// ### IO#close_read
#[monoruby_builtin]
fn close_read(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let mut self_ = lfp.self_val();
    let io = self_.as_io_inner_mut();
    match io.kind() {
        IoKind::Popen(_) => {
            io.popen_close_read();
        }
        // CRuby: no-op on an already-closed stream.
        IoKind::Closed(..) => {}
        // CRuby: a stream that is not writable is simply closed; only a
        // writable non-duplex stream refuses.
        _ if io.is_writable() => {
            return Err(MonorubyErr::ioerr("closing non-duplex IO for reading"));
        }
        _ => {
            io.close(&globals.store)?;
        }
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

///
/// ### IO#sync
///
/// - sync -> bool
///
/// True when every write reaches the fd before returning. Defaults to
/// true only for the process's stderr, as in CRuby. A TTY writes through
/// regardless (CRuby's `FMODE_TTY`), which `#sync` does not report.
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/sync.html]
#[monoruby_builtin]
fn io_sync(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    ensure_io_open(lfp.self_val())?;
    Ok(Value::bool(lfp.self_val().as_io_inner().sync()))
}

///
/// ### IO#sync=
///
/// - sync=(bool) -> bool
///
/// Changes the policy for *subsequent* writes; anything already buffered
/// stays buffered until the next flush, matching CRuby.
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/sync=3d.html]
#[monoruby_builtin]
fn assign_sync(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    ensure_io_open(lfp.self_val())?;
    let sync = lfp.arg(0).as_bool();
    lfp.self_val().as_io_inner_mut().set_sync(sync);
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
    let whence = parse_whence(vm, globals, lfp.try_arg(1))?;
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
/// - read(length = nil, outbuf = "") -> String | nil
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
    // Output buffer: its content is replaced with the read bytes, and it
    // becomes the return value. Frozen buffers raise even when there is
    // nothing to read (CRuby checks before reading).
    let outbuf = match lfp.try_arg(1) {
        Some(v) if !v.is_nil() => {
            // A #to_str-convertible object: the converted String becomes
            // the buffer (and the return value).
            let mut b = v.coerce_to_rstring(vm, globals)?.as_val();
            b.ensure_string_mutable(vm, globals)?;
            Some(b)
        }
        _ => None,
    };
    let buf = if length == Some(0) {
        // `read(0)` never blocks and returns "" (CRuby) — entering the
        // blocking region would park on POLLIN waiting for data that a
        // zero-length read must not consume (the net-http fixture calls
        // `client.read(0)` for `Content-Length: 0` requests).
        lfp.self_val().as_io_inner().ensure_readable()?;
        Vec::new()
    } else {
        blocking_io_region(vm, globals, lfp.self_val(), libc::POLLIN, |_store| {
            lfp.self_val().as_io_inner_mut().read(length)
        })?
    };
    let eof_nil = buf.is_empty() && length.is_some() && length != Some(0);
    match outbuf {
        Some(mut out) => {
            if length.is_some() {
                // Sized read: raw bytes; the buffer keeps its own encoding.
                let enc = out.as_rstring_inner().encoding();
                *out.as_rstring_inner_mut() = RStringInner::from_encoding(&buf, enc);
            } else {
                // Unsized read: the buffer takes the transcoded content
                // and its encoding (like the no-buffer return value).
                let tagged = tagged_read_string(globals, lfp.self_val(), buf, false);
                *out.as_rstring_inner_mut() = (*tagged.as_rstring_inner()).clone();
            }
            if eof_nil { Ok(Value::nil()) } else { Ok(out) }
        }
        None => {
            if eof_nil {
                return Ok(Value::nil());
            }
            Ok(tagged_read_string(
                globals,
                lfp.self_val(),
                buf,
                length.is_some(),
            ))
        }
    }
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

    let (mode, ext_obj, int_obj, bom) = class_read_opts(vm, globals, opts)?;
    // A write/append-only mode can't be used for reading.
    reject_unreadable_mode(&filename, &mode)?;

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
    // A "BOM|..." mode: a leading byte-order mark is consumed and
    // overrides the declared external encoding.
    let mut ext_obj = ext_obj;
    if bom {
        let (consume, name): (usize, &str) = match buf.first() {
            Some(0xEF) if buf.len() >= 3 && buf[1] == 0xBB && buf[2] == 0xBF => (3, "UTF-8"),
            Some(0x00) if buf.len() >= 4 && buf[1] == 0x00 && buf[2] == 0xFE && buf[3] == 0xFF => {
                (4, "UTF-32BE")
            }
            Some(0xFF) if buf.len() >= 2 && buf[1] == 0xFE => {
                if buf.len() >= 4 && buf[2] == 0x00 && buf[3] == 0x00 {
                    (4, "UTF-32LE")
                } else {
                    (2, "UTF-16LE")
                }
            }
            Some(0xFE) if buf.len() >= 2 && buf[1] == 0xFF => (2, "UTF-16BE"),
            _ => (0, ""),
        };
        if consume > 0 {
            buf.drain(..consume);
            ext_obj = enc_by_name(globals, name);
        }
    }
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
            let topts = super::encoding::TranscodeOpts::default();
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
    let path = lfp
        .arg(0)
        .coerce_to_path_rstring(vm, globals)?
        .to_str()?
        .to_string();
    let (sep, limit, chomp, (mode, ext_obj, int_obj, _bom)) = class_getline_args(vm, globals, lfp)?;
    if limit == Some(0) {
        return Err(MonorubyErr::argumenterr("invalid limit: 0 for readlines"));
    }
    reject_unreadable_mode(&path, &mode)?;
    let file = std::fs::File::open(&path)
        .map_err(|e| MonorubyErr::errno_with_path(&globals.store, &e, "rb_sysopen", &path))?;
    let complete_utf8 = ext_completes_utf8(globals, ext_obj);
    let mut io_val = Value::new_file(file, path, None, true, false);
    vm.with_temp_scope(|vm| {
        // Root the transient File IO (and the result lines) across the
        // per-line allocations below.
        vm.temp_push(io_val);
        vm.temp_array_new(None);
        let result = (|| {
            while let Some(mut buf) = blocking_io_region(vm, globals, io_val, libc::POLLIN, |_store| {
                io_val
                    .as_io_inner_mut()
                    .getline(sep.as_deref(), limit, complete_utf8)
            })? {
                if chomp {
                    chomp_line(&mut buf, sep.as_deref(), limit);
                }
                let line = tag_with_encs(globals, buf, ext_obj, int_obj);
                vm.temp_array_push(line);
            }
            Ok(vm.temp_pop())
        })();
        // Close the fd now — leaving it to GC lets descriptors pile up.
        let _ = io_val.as_io_inner_mut().close(&globals.store);
        result
    })
}

/// A write/append-only open mode can't be read from. CRuby still *opens*
/// the file with that mode first — truncating it for "w", creating it if
/// absent — before raising, so replicate the side effect and then fail.
fn reject_unreadable_mode(path: &str, mode: &str) -> Result<()> {
    let base = mode.split(':').next().unwrap_or("").replace('b', "");
    if base == "w" || base == "a" {
        let mut o = std::fs::OpenOptions::new();
        o.write(true).create(true);
        if base == "w" {
            o.truncate(true);
        } else {
            o.append(true);
        }
        let _ = o.open(path); // open errors are moot; the read error wins
        return Err(MonorubyErr::ioerr("not opened for reading"));
    }
    Ok(())
}

/// Parse `(sep, limit, chomp, open-options)` for `IO.foreach` /
/// `IO.readlines`.
///
/// Positional `sep` / `limit` sit in arg slots 1..3 with the same rules
/// as `IO#gets` — so a *positional* options Hash lands in the sep/limit
/// coercions and raises TypeError, like CRuby. Keywords arrive as the
/// `**kwrest` Hash in slot 3 (registration `&[], kw_rest = true`), which
/// keeps them distinguishable from a positional Hash, tolerates unknown
/// keys, and feeds `class_read_opts` directly (`chomp:` plus the open
/// options `mode:` / `encoding:` / …).
fn class_getline_args(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
) -> Result<(
    Option<Vec<u8>>,
    Option<usize>,
    bool,
    (String, Option<Value>, Option<Value>, bool),
)> {
    let opts = lfp.try_arg(3).and_then(|v| v.try_hash_ty());
    let mut chomp = false;
    if let Some(h) = &opts
        && let Some(c) = h.get(Value::symbol(IdentId::get_id("chomp")), vm, globals)?
    {
        chomp = c.as_bool();
    }
    // Default separator: `$/` — "\n" unless reassigned; nil slurps.
    let mut sep: Option<Vec<u8>> = globals.rs_bytes();
    let mut limit_arg: Option<Value> = None;
    match (lfp.try_arg(1), lfp.try_arg(2)) {
        (None, _) => {}
        (Some(v0), None) => {
            if v0.is_nil() {
                sep = None;
            } else if let Some(rs) = v0.is_rstring() {
                sep = Some(rs.as_bytes().to_vec());
            } else if globals.check_method(v0, IdentId::TO_STR).is_some() {
                sep = Some(v0.coerce_to_rstring(vm, globals)?.as_bytes().to_vec());
            } else {
                limit_arg = Some(v0);
            }
        }
        (Some(v0), Some(v1)) => {
            if v0.is_nil() {
                sep = None;
            } else {
                sep = Some(v0.coerce_to_rstring(vm, globals)?.as_bytes().to_vec());
            }
            limit_arg = Some(v1);
        }
    }
    let limit = match limit_arg {
        None => None,
        Some(v) => {
            let l = v.coerce_to_int_i64(vm, globals)?;
            if l < 0 { None } else { Some(l as usize) }
        }
    };
    let open = class_read_opts(vm, globals, opts)?;
    Ok((sep, limit, chomp, open))
}

/// Whether limit-cut reads should complete UTF-8 characters, given the
/// resolved external encoding of a class-level read (`None` = the
/// default external encoding).
fn ext_completes_utf8(globals: &mut Globals, ext_obj: Option<Value>) -> bool {
    use crate::value::Encoding as E;
    let ext = match ext_obj {
        Some(o) => enc_obj_to_enum(globals, o).unwrap_or(E::Utf8),
        None => {
            let de = enc_default_external_obj(globals);
            enc_obj_to_enum(globals, de).unwrap_or(E::Utf8)
        }
    };
    matches!(ext, E::Utf8)
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
fn io_foreach(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, pc: BytecodePtr) -> Result<Value> {
    // Without a block, return an Enumerator replaying this method
    // (size nil, like CRuby). Only the *passed* positional slots are
    // captured, and the `**kwrest` Hash is re-sent as keywords — passing
    // it positionally would hit the sep/limit TypeError coercions.
    let bh = match lfp.block() {
        Some(bh) => bh,
        None => {
            let mut args = vec![lfp.arg(0)];
            for i in 1..3 {
                if let Some(v) = lfp.try_arg(i) {
                    args.push(v);
                }
            }
            let kw = lfp.try_arg(3).and_then(|v| v.try_hash_ty());
            return vm.generate_enumerator_with_kw(
                IdentId::get_id("foreach"),
                lfp.self_val(),
                args,
                kw,
                pc,
            );
        }
    };
    let path = lfp
        .arg(0)
        .coerce_to_path_rstring(vm, globals)?
        .to_str()?
        .to_string();
    let p = vm.get_block_data(globals, bh)?;
    let (sep, limit, chomp, (mode, ext_obj, int_obj, _bom)) = class_getline_args(vm, globals, lfp)?;
    if limit == Some(0) {
        return Err(MonorubyErr::argumenterr("invalid limit: 0 for foreach"));
    }
    reject_unreadable_mode(&path, &mode)?;
    let file = std::fs::File::open(&path)
        .map_err(|e| MonorubyErr::errno_with_path(&globals.store, &e, "rb_sysopen", &path))?;
    let complete_utf8 = ext_completes_utf8(globals, ext_obj);
    let mut io_val = Value::new_file(file, path, None, true, false);
    vm.with_temp_scope(|vm| {
        // Root the transient File IO across the block calls below.
        vm.temp_push(io_val);
        let mut lineno = 0i64;
        let result = (|| {
            while let Some(mut buf) = blocking_io_region(vm, globals, io_val, libc::POLLIN, |_store| {
                io_val
                    .as_io_inner_mut()
                    .getline(sep.as_deref(), limit, complete_utf8)
            })? {
                if chomp {
                    chomp_line(&mut buf, sep.as_deref(), limit);
                }
                let line = tag_with_encs(globals, buf, ext_obj, int_obj);
                // CRuby's foreach reads through the getline machinery,
                // which updates `$.` and `$_` per line (and leaves `$_`
                // nil at EOF).
                lineno += 1;
                globals.set_lineno(Value::integer(lineno));
                vm.set_last_read_line(line);
                vm.invoke_block(globals, &p, &[line])?;
            }
            vm.set_last_read_line(Value::nil());
            Ok(Value::nil())
        })();
        // Close the fd now — leaving it to GC lets descriptors pile up.
        let _ = io_val.as_io_inner_mut().close(&globals.store);
        result
    })
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
fn io_pipe(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let class = lfp.self_val().as_class_id();

    // Optional read-end encodings: (ext), (ext, int), ("ext:int"), each a
    // String (#to_str) or an Encoding; a trailing options Hash (transcode
    // options like `invalid: :replace`) is accepted and ignored.
    let mut ext_obj: Option<Value> = None;
    let mut int_obj: Option<Value> = None;
    let mut enc_args: Vec<Value> = Vec::new();
    for i in 0..3 {
        if let Some(v) = lfp.try_arg(i)
            && !v.is_nil()
            && v.try_hash_ty().is_none()
        {
            enc_args.push(v);
        }
    }
    if let Some(&v) = enc_args.first() {
        if v.class() == super::encoding::encoding_class(globals) {
            ext_obj = Some(v);
        } else {
            let s = v.coerce_to_string(vm, globals)?;
            // A "BOM|" prefix requests BOM detection on read; the base
            // encoding is what IO#external_encoding reports.
            let s = s
                .strip_prefix("BOM|")
                .or_else(|| s.strip_prefix("bom|"))
                .unwrap_or(&s);
            let mut parts = s.splitn(2, ':');
            ext_obj = parts
                .next()
                .filter(|x| !x.is_empty())
                .and_then(|x| enc_by_name(globals, x));
            int_obj = parts
                .next()
                .filter(|x| !x.is_empty() && *x != "-")
                .and_then(|x| enc_by_name(globals, x));
        }
    }
    if let Some(&v) = enc_args.get(1) {
        int_obj = if v.class() == super::encoding::encoding_class(globals) {
            Some(v)
        } else {
            let s = v.coerce_to_string(vm, globals)?;
            if s == "-" {
                None
            } else {
                enc_by_name(globals, &s)
            }
        };
    }

    let mut fds: [libc::c_int; 2] = [0; 2];
    // SAFETY: fds is a valid pointer to a 2-element array of c_int.
    // O_CLOEXEC by default, like CRuby: pipe ends do not leak across
    // exec unless explicitly redirected (or close_on_exec = false).
    let ret = unsafe { libc::pipe2(fds.as_mut_ptr(), libc::O_CLOEXEC) };
    if ret == -1 {
        let err = std::io::Error::last_os_error();
        return Err(MonorubyErr::errno_with_msg(&globals.store, &err, "pipe(2)"));
    }

    // Allocate + dispatch #initialize (NOT `new`: a redefined `new` must
    // not be called, but a redefined #initialize must — see
    // core/io/pipe_spec "does not use IO.new method").
    let init_id = IdentId::get_id("initialize");
    let make_end = |vm: &mut Executor, globals: &mut Globals, fd: i32, mode: &str| -> Result<Value> {
        let obj = Value::new_io_with_class(IoInner::closed(None), class);
        vm.invoke_method_inner(
            globals,
            init_id,
            obj,
            &[Value::integer(fd as i64), Value::string_from_str(mode)],
            None,
            None,
        )?;
        Ok(obj)
    };
    let mut read_io = make_end(vm, globals, fds[0], "r")?;
    let mut write_io = make_end(vm, globals, fds[1], "w")?;
    // CRuby's `rb_io_s_pipe` marks the write end synchronous. It is not a
    // nicety: both ends usually live in one process, so a buffered write
    // would leave the reader blocked on data that never reaches the pipe.
    write_io.as_io_inner_mut().set_sync(true);

    // The read end carries the requested (or default) encodings; the
    // write end carries none — `#initialize` with mode "w" already left
    // it encoding-less.
    if ext_obj.is_some() || int_obj.is_some() {
        let de = enc_default_external_obj(globals);
        let di = enc_default_internal_obj(globals);
        let (slot, i) =
            resolve_io_encodings(globals, ext_obj, int_obj, false, true, false, de, di);
        store_io_encodings(globals, read_io, slot, i);
    }

    match lfp.block() {
        Some(bh) => {
            let r = vm.invoke_block_once(globals, bh, &[read_io, write_io]);
            // Both ends close at block exit (idempotent for ends the
            // block already closed), success or raise.
            let _ = read_io.as_io_inner_mut().close(&globals.store);
            let _ = write_io.as_io_inner_mut().close(&globals.store);
            r
        }
        None => Ok(Value::array2(read_io, write_io)),
    }
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
    // Array elements pass their BYTES through verbatim (OsString): an
    // argv entry may legitimately hold non-UTF-8 bytes (e.g. a
    // `# encoding: big5` script passed via `-e`), and `to_s` would
    // mangle them.
    fn arg_os(v: Value, globals: &Globals) -> std::ffi::OsString {
        use std::os::unix::ffi::OsStringExt;
        if let Some(inner) = v.is_rstring_inner() {
            std::ffi::OsString::from_vec(inner.as_bytes().to_vec())
        } else {
            v.to_s(globals).into()
        }
    }
    // CRuby's Array form is `[env, cmd, arg1, ..., exec_opts]`: a leading
    // Hash inside the Array is the environment, a trailing Hash the exec
    // options.
    let mut array_opts: Option<Value> = None;
    let (mut command, cmd_name) = if let Some(ary) = cmd_val.try_array_ty() {
        let mut elems: Vec<Value> = ary.iter().cloned().collect();
        if elems.first().is_some_and(|v| v.try_hash_ty().is_some()) {
            let h = elems.remove(0);
            if env_hash.is_none() {
                env_hash = Some(h);
            }
        }
        if elems.len() > 1 && elems.last().is_some_and(|v| v.try_hash_ty().is_some()) {
            array_opts = elems.pop();
        }
        let parts: Vec<std::ffi::OsString> = elems.iter().map(|v| arg_os(*v, globals)).collect();
        if parts.is_empty() {
            return Err(MonorubyErr::argumenterr("popen: empty command array"));
        }
        let name = parts[0].to_string_lossy().into_owned();
        let mut cmd = Command::new(&parts[0]);
        for part in &parts[1..] {
            cmd.arg(part);
        }
        (cmd, name)
    } else {
        // String command: like CRuby, only go through the shell when the
        // string actually needs one (metacharacters); otherwise split and
        // exec directly. The interposed `sh` matters for more than speed:
        // when the child dies by a signal, a wrapping shell exits *normally*
        // with 128+signo, destroying `$?.signaled?` / `termsig` for the
        // caller (mspec's ruby_exe checks exactly that).
        let cmd_str = cmd_val.coerce_to_str(vm, globals)?;
        let (program, args) = super::kernel::prepare_command_arg(&cmd_str);
        let mut cmd = Command::new(program);
        cmd.args(&args);
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

    // Process the exec-option hashes: the Array-trailing one and the
    // positional/keyword one both apply.
    let mut stderr_to_stdout = false;
    let mut ext_obj: Option<Value> = None;
    let mut int_obj: Option<Value> = None;
    for opts in [array_opts, opts_hash].into_iter().flatten() {
        let h = opts.as_hash();
        // err: [:child, :out] redirects the child's stderr to its stdout.
        if let Ok(Some(err_val)) = h.get(Value::symbol_from_str("err"), vm, globals) {
            if let Some(ary) = err_val.try_array_ty() {
                if ary.len() == 2
                    && ary[0].try_symbol_or_string() == Some(IdentId::get_id("child"))
                    && ary[1].try_symbol_or_string() == Some(IdentId::get_id("out"))
                {
                    stderr_to_stdout = true;
                }
            }
        }
        // chdir: the child's working directory (String / #to_path / #to_str).
        if let Some(dir) = h.get(Value::symbol_from_str("chdir"), vm, globals)?
            && !dir.is_nil()
        {
            let path = super::file::to_path_str(vm, globals, dir)?;
            command.current_dir(path);
        }
        // encoding options for the read end.
        if let Some(v) = h.get(Value::symbol(IdentId::get_id("encoding")), vm, globals)?
            && !v.is_nil()
        {
            if let Some(sv) = v.is_str() {
                let mut parts = sv.split(':');
                ext_obj = parts
                    .next()
                    .filter(|x| !x.is_empty())
                    .and_then(|x| enc_by_name(globals, x));
                int_obj = parts
                    .next()
                    .filter(|x| !x.is_empty())
                    .and_then(|x| enc_by_name(globals, x));
            } else {
                ext_obj = arg_to_enc_obj(globals, v);
            }
        }
        if let Some(v) = h.get(Value::symbol(IdentId::get_id("external_encoding")), vm, globals)?
            && !v.is_nil()
        {
            ext_obj = arg_to_enc_obj(globals, v);
        }
        if let Some(v) = h.get(Value::symbol(IdentId::get_id("internal_encoding")), vm, globals)?
            && !v.is_nil()
        {
            int_obj = arg_to_enc_obj(globals, v);
        }
    }
    // Mode-string encoding suffix ("r:ext:int") — explicit options win.
    {
        let (me, mi) = mode_encoding_names(&mode);
        if ext_obj.is_none() {
            ext_obj = me.and_then(|e| enc_by_name(globals, e));
        }
        if int_obj.is_none() {
            int_obj = mi.and_then(|i| enc_by_name(globals, i));
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

    // Instantiate as the receiver class (a subclass' popen yields the
    // subclass), and install the resolved encodings on the read end.
    let class_id = lfp.self_val().as_class_id();
    let io_val = Value::new_io_with_class(IoInner::popen(child), class_id);
    {
        let de = enc_default_external_obj(globals);
        let di = enc_default_internal_obj(globals);
        let (slot, i) =
            resolve_io_encodings(globals, ext_obj, int_obj, false, readable, !readable, de, di);
        store_io_encodings(globals, io_val, slot, i);
    }

    if let Some(bh) = lfp.block() {
        let data = vm.get_block_data(globals, bh)?;
        let res = vm.invoke_block(globals, &data, &[io_val]);
        let mut io_close = io_val;
        if let Ok(Some((exit_status, pid))) = io_close.as_io_inner_mut().close(&globals.store) {
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
                    crate::scheduler::set_last_status(vm, status_obj);
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
/// ### IO#fcntl
/// - fcntl(cmd, arg = 0) -> Integer
///
/// Thin wrapper over fcntl(2). `arg` must be an Integer (the String
/// forms some exotic commands take are not supported). Raises `IOError`
/// on a closed stream and the matching `Errno::*` on syscall failure.
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/fcntl.html]
#[monoruby_builtin]
fn io_fcntl(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let fd = self_.as_io_inner().fileno()?;
    let cmd = lfp.arg(0).coerce_to_int_i64(vm, globals)? as i32;
    let arg = match lfp.try_arg(1) {
        Some(v) if !v.is_nil() => v.coerce_to_int_i64(vm, globals)?,
        _ => 0,
    };
    // SAFETY: fcntl(2) with an integer third argument; no memory is
    // passed, so there are no aliasing concerns.
    let ret = unsafe { libc::fcntl(fd, cmd, arg as libc::c_long) };
    if ret == -1 {
        let err = std::io::Error::last_os_error();
        return Err(MonorubyErr::errno_with_msg(&globals.store, &err, "fcntl"));
    }
    Ok(Value::integer(ret as i64))
}

///
/// ### IO#ioctl
/// - ioctl(cmd, arg = 0) -> Integer
///
/// Thin wrapper over ioctl(2) with an integer argument (the
/// memory-backed String forms are not supported). Raises `IOError` on a
/// closed stream and the matching `Errno::*` (e.g. `Errno::ENOTTY`) on
/// syscall failure.
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/ioctl.html]
#[monoruby_builtin]
fn io_ioctl(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let fd = self_.as_io_inner().fileno()?;
    let cmd = lfp.arg(0).coerce_to_int_i64(vm, globals)?;
    let arg = match lfp.try_arg(1) {
        Some(v) if !v.is_nil() => v.coerce_to_int_i64(vm, globals)?,
        _ => 0,
    };
    // SAFETY: ioctl(2) with an integer third argument; no memory is
    // passed, so there are no aliasing concerns.
    let ret = unsafe { libc::ioctl(fd, cmd as libc::c_ulong, arg as libc::c_long) };
    if ret == -1 {
        let err = std::io::Error::last_os_error();
        return Err(MonorubyErr::errno_with_msg(&globals.store, &err, "ioctl"));
    }
    Ok(Value::integer(ret as i64))
}

///
/// ### IO#initialize_copy (the #dup / #clone hook)
///
/// A duplicated IO wraps a dup(2) of the descriptor: a distinct fd
/// sharing the open file description (offset, status flags) with the
/// original, close-on-exec and autoclose both set (CRuby). Raises
/// IOError when the source is closed.
#[monoruby_builtin]
fn io_initialize_copy(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let mut self_ = lfp.self_val();
    let other = lfp.arg(0);
    let fd = other.as_io_inner().fileno()?;
    // SAFETY: dup(2) of a validated fd.
    let newfd = unsafe { libc::dup(fd) };
    if newfd == -1 {
        let err = std::io::Error::last_os_error();
        return Err(MonorubyErr::errno_with_msg(&globals.store, &err, "dup"));
    }
    // SAFETY: setting FD_CLOEXEC on the fd we just created.
    unsafe { libc::fcntl(newfd, libc::F_SETFD, libc::FD_CLOEXEC) };
    let (readable, writable) = {
        let inner = other.as_io_inner();
        (inner.is_readable(), inner.is_writable())
    };
    let path = other.as_io_inner().path();
    let has_path = path.is_some();
    let mut dup = IoInner::from_raw_fd_autoclose(
        newfd,
        path.unwrap_or_default(),
        has_path,
        readable,
        writable,
        true,
    );
    // `#lineno`, the encodings and binmode are per-object state that the
    // copy inherits (and then advances independently).
    dup.copy_state_from(other.as_io_inner());
    *self_.as_io_inner_mut() = dup;
    Ok(self_)
}

///
/// ### IO#reopen
/// - reopen(io) -> self
/// - reopen(path, mode = <receiver's current mode>) -> self
///
/// Re-associates the receiver with another stream. The IO form
/// `dup2(2)`s the other stream's fd onto the receiver's fd number
/// (sharing the open file description, hence the position) and the
/// receiver takes on the other IO's class/path/mode; the path form
/// has `freopen(3)` semantics — the receiver's fd number is
/// re-pointed at a fresh open of `path`, positioned at 0.
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/reopen.html]
#[monoruby_builtin]
fn io_reopen(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let arg0 = lfp.arg(0);
    let is_io = |v: Value| v.try_rvalue().is_some_and(|rv| rv.ty() == ObjTy::IO);
    if lfp.try_arg(1).is_none() {
        // IO form. A real IO argument is used as-is — CRuby's
        // rb_io_check_io short-circuits on T_FILE without calling
        // #to_io (the reopen spec pins this down). Other objects
        // convert via #to_io when they respond; a wrong-typed result
        // raises TypeError. Objects with neither fall through to the
        // path form (#to_path / #to_str coercion).
        if is_io(arg0) {
            return io_reopen_io(globals, lfp.self_val(), arg0);
        }
        if let Some(fid) = globals.check_method(arg0, IdentId::get_id("to_io")) {
            let conv = vm.invoke_func_inner(globals, fid, arg0, &[], None, None)?;
            if !is_io(conv) {
                // CRuby's conversion error spells this "to IO", not
                // the generic coercion helper's "into", and names the
                // real class (not a singleton).
                let class = globals.store.get_class_name(arg0.real_class(&globals.store).id());
                let gives = globals.store.get_class_name(conv.real_class(&globals.store).id());
                return Err(MonorubyErr::typeerr(format!(
                    "can't convert {class} to IO ({class}#to_io gives {gives})"
                )));
            }
            return io_reopen_io(globals, lfp.self_val(), conv);
        }
    }
    io_reopen_path(vm, globals, lfp)
}

/// `IO#reopen(other_io)`: `dup2(2)` the other stream's fd onto the
/// receiver's fd number and adopt the other IO's mode, path, and
/// class.
fn io_reopen_io(globals: &mut Globals, self_: Value, other: Value) -> Result<Value> {
    let mut self_ = self_;
    let mut other = other;
    ensure_io_open(other)?;
    ensure_io_open(self_)?;
    if self_.id() == other.id() {
        return Ok(self_);
    }
    // Logical position of the other stream (fd offset minus its
    // buffered lookahead / pushback), captured before the fd surgery.
    // CRuby's io_reopen seeks both streams there afterwards so the
    // shared description continues from the *visible* position, not
    // from wherever buffered readahead left the fd.
    let other_pos = if other.as_io_inner().is_readable() {
        let pb = other.as_io_inner().pushback_len() as i64;
        other
            .as_io_inner_mut()
            .seek(0, 1)
            .ok()
            .map(|p| (p as i64 - pb).max(0))
    } else {
        None
    };
    // Flush both write sides before re-pointing the descriptor.
    if self_.as_io_inner().is_writable() {
        self_.as_io_inner_mut().flush(&globals.store)?;
    }
    if other.as_io_inner().is_writable() {
        other.as_io_inner_mut().flush(&globals.store)?;
    }
    let fd1 = self_.as_io_inner().fileno()?;
    let fd2 = other.as_io_inner().fileno()?;
    if fd1 != fd2 {
        // SAFETY: dup2 of two validated open fds — atomically closes
        // the receiver's old description and re-points its fd number.
        if unsafe { libc::dup2(fd2, fd1) } == -1 {
            let err = std::io::Error::last_os_error();
            return Err(MonorubyErr::errno_with_msg(&globals.store, &err, "dup2"));
        }
        if fd1 > 2 {
            // CRuby resets close-on-exec to true on non-STDIO fds
            // (dup2 always clears it).
            // SAFETY: setting FD_CLOEXEC on the fd we just dup2'ed.
            unsafe { libc::fcntl(fd1, libc::F_SETFD, libc::FD_CLOEXEC) };
        }
    }
    let (readable, writable) = {
        let o = other.as_io_inner();
        (o.is_readable(), o.is_writable())
    };
    rebuild_reopened_inner(&mut self_, fd1, other.as_io_inner().path(), readable, writable);
    if let Some(pos) = other_pos {
        // Both fds now share one description; seeking re-syncs the
        // kernel offset to the logical position and drops both sides'
        // stale read buffers.
        let _ = self_.as_io_inner_mut().seek(pos, 0);
        let _ = other.as_io_inner_mut().seek(pos, 0);
    }
    // The receiver takes on the other IO's (real) class — CRuby's
    // RBASIC_SET_CLASS in io_reopen; `@io.reopen(file).should.instance_of?(File)`.
    let real = other.real_class(&globals.store).id();
    if self_.class() != real {
        self_.change_class(real);
    }
    Ok(self_)
}

/// `IO#reopen(path, mode)`: freopen(3) semantics. Opens `path` and
/// re-points the receiver's fd number at it (or installs a fresh fd
/// when the receiver is closed — allowed for the path form).
fn io_reopen_path(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let mut self_ = lfp.self_val();
    let path = super::file::to_path_str(vm, globals, lfp.arg(0))?;
    let was_open = !self_.as_io_inner().is_closed();
    // Resolve the mode: an explicit argument wins; otherwise
    // reconstruct it from the receiver's current state (CRuby reuses
    // fptr->mode). F_GETFL recovers O_APPEND, which monoruby's inner
    // doesn't store; write-only non-append receivers are treated as
    // "w" (create + truncate), matching the common `File.open(_, "w")`
    // origin.
    let mode_base = if let Some(m) = lfp.try_arg(1) {
        let mode = if let Some(n) = m.try_fixnum() {
            super::file::mode_string_from_flags(n)
        } else {
            m.coerce_to_string(vm, globals)?
        };
        mode.split(':').next().unwrap().replace('b', "")
    } else {
        let (readable, writable) = {
            let i = self_.as_io_inner();
            (i.is_readable(), i.is_writable())
        };
        let append = if was_open {
            let fd = self_.as_io_inner().fileno()?;
            // SAFETY: F_GETFL on the receiver's open fd; reads no memory.
            unsafe { libc::fcntl(fd, libc::F_GETFL) & libc::O_APPEND != 0 }
        } else {
            false
        };
        match (readable, writable, append) {
            (true, false, _) | (false, false, _) => "r".to_string(),
            (false, true, true) => "a".to_string(),
            (false, true, false) => "w".to_string(),
            (true, true, true) => "a+".to_string(),
            (true, true, false) => "r+".to_string(),
        }
    };
    let (readable, writable) = match mode_base.as_str() {
        "r" => (true, false),
        "w" | "a" | "w-" | "-w" => (false, true),
        "r+" | "+r" | "w+" | "+w" | "a+" | "+a" => (true, true),
        _ => {
            return Err(MonorubyErr::argumenterr(format!(
                "invalid access mode {mode_base}"
            )));
        }
    };
    let mut opt = std::fs::OpenOptions::new();
    match mode_base.as_str() {
        "r" => opt.read(true),
        "w" => opt.write(true).create(true).truncate(true),
        "w-" => opt.write(true).create(true),
        "-w" => opt.write(true),
        "a" => opt.write(true).create(true).append(true),
        "r+" | "+r" => opt.read(true).write(true),
        "w+" | "+w" => opt.read(true).write(true).create(true).truncate(true),
        "a+" | "+a" => opt.read(true).write(true).create(true).append(true),
        _ => unreachable!(),
    };
    std::os::unix::fs::OpenOptionsExt::custom_flags(&mut opt, libc::O_CLOEXEC);
    let file = opt.open(&path).map_err(|e| {
        MonorubyErr::errno_with_path(&globals.store, &e, "rb_sysopen", &path)
    })?;
    if was_open {
        if self_.as_io_inner().is_writable() {
            self_.as_io_inner_mut().flush(&globals.store)?;
        }
        let fd1 = self_.as_io_inner().fileno()?;
        let tmpfd = std::os::fd::AsRawFd::as_raw_fd(&file);
        if fd1 != tmpfd {
            // SAFETY: dup2 onto the receiver's own open fd number;
            // `file` (tmpfd) is closed when it drops below.
            if unsafe { libc::dup2(tmpfd, fd1) } == -1 {
                let err = std::io::Error::last_os_error();
                return Err(MonorubyErr::errno_with_msg(&globals.store, &err, "dup2"));
            }
        }
        if fd1 > 2 {
            // SAFETY: setting FD_CLOEXEC on the receiver's fd.
            unsafe { libc::fcntl(fd1, libc::F_SETFD, libc::FD_CLOEXEC) };
        }
        rebuild_reopened_inner(&mut self_, fd1, Some(path), readable, writable);
    } else {
        // Closed receiver: adopt the fresh fd directly.
        let fd = std::os::fd::IntoRawFd::into_raw_fd(file);
        let inner = self_.as_io_inner_mut();
        let mut fresh = IoInner::from_raw_fd_autoclose(fd, path, true, readable, writable, true);
        fresh.copy_state_from(inner);
        *inner = fresh;
    }
    Ok(self_)
}

/// Swap the receiver's `IoInner` for a fresh wrapper of `fd` (new
/// buffer, no pushback, new mode/path) without disturbing fd-close
/// responsibility. Only File-backed receivers are rebuilt: the stdio
/// singletons keep their variant (their writes go through the process
/// fd, which dup2 already re-pointed — the redirection works without
/// touching the wrapper), and popen wrappers keep their child
/// bookkeeping.
fn rebuild_reopened_inner(
    self_: &mut Value,
    fd: i32,
    path: Option<String>,
    readable: bool,
    writable: bool,
) {
    let inner = self_.as_io_inner_mut();
    if !matches!(inner.kind(), IoKind::File(_)) {
        return;
    }
    let was_autoclose = inner.is_autoclose();
    // Release the old wrapper's claim on the fd number without closing
    // it (the number now holds the new description); the new wrapper
    // re-registers ownership if it had it.
    inner.set_autoclose(false);
    let has_path = path.is_some();
    let mut fresh = IoInner::from_raw_fd_autoclose(
        fd,
        path.unwrap_or_default(),
        has_path,
        readable,
        writable,
        was_autoclose,
    );
    // The stream is replaced, the *object* is not: `#lineno`, the
    // encodings and binmode survive a reopen (they were instance
    // variables before, which this swap left untouched).
    fresh.copy_state_from(inner);
    *inner = fresh;
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
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let (sep, limit, chomp) = getline_args(vm, globals, lfp, 2)?;
    if limit == Some(0) {
        return Err(MonorubyErr::argumenterr("invalid limit: 0 for readlines"));
    }
    let self_ = lfp.self_val();
    let complete_utf8 = io_completes_utf8(globals, self_);
    let mut lines = Vec::new();
    while let Some(mut buf) = blocking_io_region(vm, globals, lfp.self_val(), libc::POLLIN, |_store| {
        lfp.self_val()
            .as_io_inner_mut()
            .getline(sep.as_deref(), limit, complete_utf8)
    })? {
        if chomp {
            chomp_line(&mut buf, sep.as_deref(), limit);
        }
        bump_lineno(globals, self_)?;
        lines.push(tagged_read_string(globals, self_, buf, false));
    }
    Ok(Value::array_from_vec(lines))
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
    let mut self_ = lfp.self_val();
    ensure_io_open(self_)?;
    // CRuby: binmode forces external = ASCII-8BIT, internal = nil.
    let bin = enc_by_name(globals, "ASCII-8BIT")
        .map(ExtSlot::Fixed)
        .unwrap_or(ExtSlot::Nil);
    store_io_encodings(globals, self_, bin, None);
    self_.as_io_inner_mut().set_binmode();
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
    _globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    ensure_io_open(lfp.self_val())?;
    Ok(Value::bool(lfp.self_val().as_io_inner().binmode()))
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
            return Err(MonorubyErr::not_implemented_err(
                &globals.store,
                format!("Unsupported advice: :{}", name),
            ));
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
    self_.as_io_inner_mut().set_lineno(0);
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
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let mut self_ = lfp.self_val();
    let io = self_.as_io_inner_mut();
    if io.has_buffered_data() {
        return Ok(Value::bool(false));
    }
    // Read 1 byte; if empty, we're at EOF.
    let buf = blocking_io_region(vm, globals, lfp.self_val(), libc::POLLIN, |_store| {
        lfp.self_val().as_io_inner_mut().read(Some(1))
    })?;
    let io = self_.as_io_inner_mut();
    if buf.is_empty() {
        return Ok(Value::bool(true));
    }
    // Make the probe non-destructive: seek back where possible, and on an
    // unseekable stream (pipe/socket/stdin) unread the byte into the
    // pushback buffer. CRuby's eof? may block reading a pipe but keeps
    // what it read in the IO buffer for the next read — discarding it
    // here made `expect`'s select/eof?/getc loop lose the first byte of
    // every pattern and drain the stream without ever matching.
    if io.seek(-1, 1).is_err() {
        io.unget(&buf)?;
    }
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
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let buf = blocking_io_region(vm, globals, lfp.self_val(), libc::POLLIN, |_store| {
        lfp.self_val().as_io_inner_mut().read_buffered(Some(1))
    })?;
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
    let first = io.read_buffered(Some(1))?;
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
        let next = read_more_char_bytes(io, &buf, total)?;
        if next.is_empty() {
            break;
        }
        buf.extend_from_slice(&next);
    }
    Ok(buf)
}

/// Continuation read for `read_one_char{,_enc}`: on a signal interrupt,
/// push the partial character bytes back so the retried getc re-reads a
/// whole character.
fn read_more_char_bytes(io: &mut IoInner, acc: &[u8], total: usize) -> Result<Vec<u8>> {
    match io.read_buffered(Some(total - acc.len())) {
        Err(err) => {
            if err.is_signal_interrupt() && !acc.is_empty() {
                let _ = io.unget(acc);
            }
            Err(err)
        }
        ok => ok,
    }
}

/// Per-encoding character width from the lead byte (mirrors
/// `CharByteIter::next`). UTF-8 is handled separately because it needs
/// post-read validation; this returns 0 for it.
fn char_width_from_lead(enc: crate::value::Encoding, b: u8) -> usize {
    use crate::value::Encoding as E;
    match enc {
        E::Ascii8 | E::UsAscii | E::Iso8859(_) | E::Other(_) | E::NamedByte(_) | E::Iso2022Jp => 1,
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
    let first = io.read_buffered(Some(1))?;
    if first.is_empty() {
        return Ok(vec![]);
    }
    let total = char_width_from_lead(enc, first[0]);
    let mut buf = first;
    while buf.len() < total {
        let next = read_more_char_bytes(io, &buf, total)?;
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
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let self_ = lfp.self_val();
    let (ext, intl) = io_encodings(globals, self_);
    let buf = blocking_io_region(vm, globals, lfp.self_val(), libc::POLLIN, |_store| {
        read_one_char_enc(lfp.self_val().as_io_inner_mut(), ext)
    })?;
    if buf.is_empty() {
        Ok(Value::nil())
    } else {
        Ok(tagged_read_string_with(globals, buf, ext, intl))
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
    let whence = parse_whence(vm, globals, lfp.try_arg(1))?;
    let mut self_ = lfp.self_val();
    let pos = self_
        .as_io_inner_mut()
        .seek(offset, whence)
        .map_err(|e| MonorubyErr::errno_with_msg(&globals.store, &e, ""))?;
    Ok(Value::integer(pos as i64))
}

/// Parse a `whence` argument for `#seek` / `#sysseek`: an Integer
/// (`IO::SEEK_SET` = 0, `IO::SEEK_CUR` = 1, `IO::SEEK_END` = 2) or one of
/// the symbols `:SET` / `:START` / `:BEGIN` / `:CUR` / `:END`.
fn parse_whence(vm: &mut Executor, globals: &mut Globals, arg: Option<Value>) -> Result<i32> {
    match arg {
        None => Ok(0),
        Some(v) if v.is_nil() => Ok(0),
        Some(v) => {
            if let Some(sym) = v.try_symbol() {
                let name = sym.get_name();
                match name.as_str() {
                    "SET" | "START" | "BEGIN" => Ok(0),
                    "CUR" => Ok(1),
                    "END" => Ok(2),
                    // CRuby falls through to #to_int for unknown symbols.
                    _ => Err(MonorubyErr::typeerr(
                        "no implicit conversion of Symbol into Integer",
                    )),
                }
            } else {
                Ok(v.coerce_to_int_i64(vm, globals)? as i32)
            }
        }
    }
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
    _globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    lfp.self_val().as_io_inner().ensure_readable()?;
    Ok(Value::integer(lfp.self_val().as_io_inner().lineno()))
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
    lfp.self_val().as_io_inner_mut().set_lineno(n);
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
    let io = lfp.self_val();
    if let Some((bytes, enc)) = io.as_io_inner().path_raw() {
        return Ok(super::file::path_value(bytes, *enc));
    }
    Ok(match io.as_io_inner().path() {
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
fn io_fsync(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let ret = lfp.self_val().as_io_inner_mut().fsync(false, &globals.store)?;
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
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let ret = lfp.self_val().as_io_inner_mut().fsync(true, &globals.store)?;
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
    let args = lfp.arg(0).as_array();
    let mut total = 0i64;
    for v in args.iter().cloned() {
        let bytes = bytes_for_write(vm, globals, lfp.self_val(), v)?;
        total += bytes.len() as i64;
        let mut done = 0;
        blocking_io_region(vm, globals, lfp.self_val(), libc::POLLOUT, |_store| {
            lfp.self_val().as_io_inner_mut().write(&bytes, &mut done, _store)
        })?;
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
    let written = loop {
        // Signal handlers are installed without SA_RESTART; run the VM
        // poll point on EINTR (raise the SignalException or run the trap
        // handler and retry), like the buffered write path.
        if crate::codegen::signal_table::PENDING_SIGNALS
            .load(std::sync::atomic::Ordering::Relaxed)
            != 0
            && crate::executor::execute_gc(vm, globals).is_none()
        {
            return Err(vm.take_error());
        }
        // SAFETY: fd is a valid file descriptor, bytes is a valid buffer.
        let written =
            unsafe { libc::write(fd, bytes.as_ptr() as *const libc::c_void, bytes.len()) };
        if written >= 0 {
            break written;
        }
        let err = std::io::Error::last_os_error();
        if err.raw_os_error() == Some(libc::EINTR) {
            if crate::executor::execute_gc(vm, globals).is_none() {
                return Err(vm.take_error());
            }
            continue;
        }
        return Err(MonorubyErr::errno_with_msg(
            &globals.store,
            &err,
            "syswrite",
        ));
    };
    Ok(Value::integer(written as i64))
}

///
/// ### IO#pread
/// - pread(maxlen, offset) -> String
/// - pread(maxlen, offset, out_buffer) -> out_buffer
///
/// Reads `maxlen` bytes starting at `offset` without changing the
/// stream's file position (pread(2)).
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/pread.html]
#[monoruby_builtin]
fn io_pread(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let maxlen = lfp.arg(0).coerce_to_int_i64(vm, globals)?;
    if maxlen < 0 {
        return Err(MonorubyErr::argumenterr(
            "negative string size (or size too big)",
        ));
    }
    let offset = lfp.arg(1).coerce_to_int_i64(vm, globals)?;
    // Coerce the optional output buffer to a String up front (via
    // #to_str) so a non-String buffer raises before any I/O. CRuby
    // fills and returns the coerced String, not the original arg.
    let buffer: Option<Value> = match lfp.try_arg(2) {
        Some(v) if !v.is_nil() => Some(v.coerce_to_rstring(vm, globals)?.as_val()),
        _ => None,
    };
    lfp.self_val().as_io_inner().ensure_readable()?;
    // A positional read goes to the fd directly, so anything still sitting
    // in the write buffer has to land first or `pread` reads stale bytes
    // (CRuby flushes via `rb_io_check_char_readable`).
    lfp.self_val().as_io_inner_mut().flush(&globals.store)?;

    // maxlen == 0: return "" (or the buffer unchanged) without
    // touching the file — the offset is ignored even if out of range.
    if maxlen == 0 {
        return Ok(match buffer {
            Some(v) => v,
            None => Value::string_from_vec(vec![]),
        });
    }

    let fd = lfp.self_val().as_io_inner().fileno()?;
    let mut buf = vec![0u8; maxlen as usize];
    // SAFETY: fd is valid, buf has `maxlen` bytes of capacity.
    let n = unsafe {
        libc::pread(
            fd,
            buf.as_mut_ptr() as *mut libc::c_void,
            maxlen as usize,
            offset as libc::off_t,
        )
    };
    if n < 0 {
        let err = std::io::Error::last_os_error();
        return Err(MonorubyErr::errno_with_msg(&globals.store, &err, "pread"));
    }
    if n == 0 {
        return Err(MonorubyErr::eoferr(&globals.store, "end of file reached"));
    }
    buf.truncate(n as usize);
    match buffer {
        Some(mut v) => {
            // Preserve the supplied buffer's encoding (CRuby contract).
            let enc = v.as_rstring_inner().encoding();
            *v.as_rstring_inner_mut() = RStringInner::from_encoding(&buf, enc);
            Ok(v)
        }
        None => Ok(Value::bytes(buf)),
    }
}

///
/// ### IO#pwrite
/// - pwrite(string, offset) -> Integer
///
/// Writes `string` at `offset` without changing the stream's file
/// position (pwrite(2)); returns the number of bytes written.
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/pwrite.html]
#[monoruby_builtin]
fn io_pwrite(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let bytes = if let Some(b) = lfp.arg(0).try_bytes() {
        b.to_vec()
    } else {
        vm.to_s(globals, lfp.arg(0))?.into_bytes()
    };
    let offset = lfp.arg(1).coerce_to_int_i64(vm, globals)?;
    lfp.self_val().as_io_inner().ensure_writable()?;
    // Flush any buffered writes so the file offset/content the OS sees
    // is consistent before the positional write.
    lfp.self_val().as_io_inner_mut().flush(&globals.store)?;
    let fd = lfp.self_val().as_io_inner().fileno()?;
    // SAFETY: fd is valid, bytes is a valid buffer of `bytes.len()`.
    let n = unsafe {
        libc::pwrite(
            fd,
            bytes.as_ptr() as *const libc::c_void,
            bytes.len(),
            offset as libc::off_t,
        )
    };
    if n < 0 {
        let err = std::io::Error::last_os_error();
        return Err(MonorubyErr::errno_with_msg(&globals.store, &err, "pwrite"));
    }
    Ok(Value::integer(n as i64))
}

///
/// ### IO#sysread
/// - sysread(maxlen) -> String
/// - sysread(maxlen, outbuf) -> outbuf
///
/// Low-level read of up to `maxlen` bytes via a single underlying
/// read. Raises `EOFError` at end of file.
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/sysread.html]
#[monoruby_builtin]
fn io_sysread(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let maxlen = lfp.arg(0).coerce_to_int_i64(vm, globals)?;
    if maxlen < 0 {
        return Err(MonorubyErr::argumenterr(format!(
            "negative length {} given",
            maxlen
        )));
    }
    let buffer: Option<Value> = match lfp.try_arg(1) {
        Some(v) if !v.is_nil() => Some(v.coerce_to_rstring(vm, globals)?.as_val()),
        _ => None,
    };
    if lfp.self_val().as_io_inner().is_closed() {
        return Err(MonorubyErr::ioerr("closed stream"));
    }
    if maxlen == 0 {
        // CRuby returns "" — and leaves a supplied buffer untouched.
        return Ok(match buffer {
            Some(v) => v,
            None => Value::string_from_vec(vec![]),
        });
    }
    let buf = blocking_io_region(vm, globals, lfp.self_val(), libc::POLLIN, |_store| {
        lfp.self_val().as_io_inner_mut().sysread(maxlen as usize)
    })?;
    if buf.is_empty() {
        if let Some(mut v) = buffer {
            let enc = v.as_rstring_inner().encoding();
            *v.as_rstring_inner_mut() = RStringInner::from_encoding(&[], enc);
        }
        return Err(MonorubyErr::eoferr(&globals.store, "end of file reached"));
    }
    match buffer {
        Some(mut v) => {
            let enc = v.as_rstring_inner().encoding();
            *v.as_rstring_inner_mut() = RStringInner::from_encoding(&buf, enc);
            Ok(v)
        }
        None => Ok(Value::bytes(buf)),
    }
}

///
/// ### IO#readpartial
/// - readpartial(maxlen, outbuf = nil) -> String | outbuf
///
/// Reads at most `maxlen` bytes, returning as soon as any data is
/// available (blocking only when nothing is buffered yet). Raises
/// `EOFError` at end of file. Reads through the buffered reader, so
/// ungetc pushback and already-buffered bytes are honoured.
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/readpartial.html]
#[monoruby_builtin]
fn io_readpartial(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let maxlen = lfp.arg(0).coerce_to_int_i64(vm, globals)?;
    if maxlen < 0 {
        return Err(MonorubyErr::argumenterr("negative length"));
    }
    let buffer: Option<Value> = match lfp.try_arg(1) {
        Some(v) if !v.is_nil() => Some(v.coerce_to_rstring(vm, globals)?.as_val()),
        _ => None,
    };
    if lfp.self_val().as_io_inner().is_closed() {
        return Err(MonorubyErr::ioerr("closed stream"));
    }
    if maxlen == 0 {
        // CRuby's readpartial(0) clears and returns a supplied buffer.
        return Ok(match buffer {
            Some(mut v) => {
                let enc = v.as_rstring_inner().encoding();
                *v.as_rstring_inner_mut() = RStringInner::from_encoding(&[], enc);
                v
            }
            None => Value::string_from_vec(vec![]),
        });
    }
    let buf = blocking_io_region(vm, globals, lfp.self_val(), libc::POLLIN, |_store| {
        lfp.self_val().as_io_inner_mut().readpartial(maxlen as usize)
    })?;
    if buf.is_empty() {
        if let Some(mut v) = buffer {
            let enc = v.as_rstring_inner().encoding();
            *v.as_rstring_inner_mut() = RStringInner::from_encoding(&[], enc);
        }
        return Err(MonorubyErr::eoferr(&globals.store, "end of file reached"));
    }
    match buffer {
        Some(mut v) => {
            let enc = v.as_rstring_inner().encoding();
            *v.as_rstring_inner_mut() = RStringInner::from_encoding(&buf, enc);
            Ok(v)
        }
        None => Ok(Value::bytes(buf)),
    }
}

/// Resolve a nested `IO::<name>` exception class id (for the
/// `*WaitReadable` / `*WaitWritable` classes defined in startup.rb).
/// Shared with the socket builtins' `*_nonblock` methods.
pub(super) fn io_wait_class(globals: &Globals, name: &str) -> Option<ClassId> {
    let io = globals
        .store
        .get_constant_noautoload(OBJECT_CLASS, IdentId::get_id("IO"))?
        .is_class_or_module()?;
    globals
        .store
        .get_constant(io.id(), IdentId::get_id(name))?
        .loaded_value()?
        .is_class_or_module()
        .map(|m| m.id())
}

///
/// ### IO#read_nonblock
/// - read_nonblock(maxlen, outbuf = nil, exception: true) -> String | outbuf | :wait_readable | nil
///
/// A single non-blocking read. When the read would block: raises
/// `IO::EAGAINWaitReadable` (default) or returns `:wait_readable`
/// (`exception: false`). At end of file: raises `EOFError` (default)
/// or returns `nil`.
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/read_nonblock.html]
/// Validate the `exception:` keyword of `read_nonblock`/`write_nonblock`:
/// CRuby (`rb_opts_exception_p`) accepts only literal `true`/`false` and
/// raises ArgumentError otherwise — before any IO is attempted, so the
/// ArgumentError takes precedence over EOF/closed-stream conditions.
fn nonblock_exception_kw(store: &Store, v: Option<Value>) -> Result<bool> {
    let Some(v) = v else { return Ok(true) };
    if v == Value::bool(true) {
        Ok(true)
    } else if v == Value::bool(false) {
        Ok(false)
    } else {
        Err(MonorubyErr::argumenterr(format!(
            "expected true or false as exception: {}",
            v.inspect(store)
        )))
    }
}

#[monoruby_builtin]
fn io_read_nonblock(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let maxlen = lfp.arg(0).coerce_to_int_i64(vm, globals)?;
    if maxlen < 0 {
        return Err(MonorubyErr::argumenterr(format!(
            "negative length {} given",
            maxlen
        )));
    }
    let buffer: Option<Value> = match lfp.try_arg(1) {
        Some(v) if !v.is_nil() => Some(v.coerce_to_rstring(vm, globals)?.as_val()),
        _ => None,
    };
    // `exception:` keyword is at slot 2 (positional max 2).
    let exception = nonblock_exception_kw(&globals.store, lfp.try_arg(2))?;
    if lfp.self_val().as_io_inner().is_closed() {
        return Err(MonorubyErr::ioerr("closed stream"));
    }
    if maxlen == 0 {
        return Ok(match buffer {
            Some(v) => v,
            None => Value::string_from_vec(vec![]),
        });
    }
    let outcome = lfp
        .self_val()
        .as_io_inner_mut()
        .read_nonblock(maxlen as usize, &globals.store)?;
    match outcome {
        NonblockRead::Data(buf) => match buffer {
            Some(mut v) => {
                let enc = v.as_rstring_inner().encoding();
                *v.as_rstring_inner_mut() = RStringInner::from_encoding(&buf, enc);
                Ok(v)
            }
            None => Ok(Value::bytes(buf)),
        },
        NonblockRead::Eof => {
            // CRuby empties a supplied buffer before signalling EOF.
            if let Some(mut v) = buffer {
                let enc = v.as_rstring_inner().encoding();
                *v.as_rstring_inner_mut() = RStringInner::from_encoding(&[], enc);
            }
            if exception {
                Err(MonorubyErr::eoferr(&globals.store, "end of file reached"))
            } else {
                Ok(Value::nil())
            }
        }
        NonblockRead::WouldBlock => {
            if exception {
                let cid = io_wait_class(globals, "EAGAINWaitReadable")
                    .ok_or_else(|| MonorubyErr::ioerr("IO::EAGAINWaitReadable not defined"))?;
                Err(MonorubyErr::new(
                    MonorubyErrKind::Other(cid),
                    "Resource temporarily unavailable - read would block".to_string(),
                ))
            } else {
                Ok(Value::symbol(IdentId::get_id("wait_readable")))
            }
        }
    }
}

///
/// ### IO#write_nonblock
/// - write_nonblock(string, exception: true) -> Integer | :wait_writable
///
/// A single non-blocking write; returns the number of bytes written.
/// When the write would block: raises `IO::EAGAINWaitWritable`
/// (default) or returns `:wait_writable` (`exception: false`).
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/write_nonblock.html]
#[monoruby_builtin]
fn io_write_nonblock(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let bytes = if let Some(b) = lfp.arg(0).try_bytes() {
        b.to_vec()
    } else {
        vm.to_s(globals, lfp.arg(0))?.into_bytes()
    };
    // `exception:` keyword is at slot 1 (positional max 1).
    let exception = nonblock_exception_kw(&globals.store, lfp.try_arg(1))?;
    if lfp.self_val().as_io_inner().is_closed() {
        return Err(MonorubyErr::ioerr("closed stream"));
    }
    let outcome = lfp
        .self_val()
        .as_io_inner_mut()
        .write_nonblock(&bytes, &globals.store)?;
    match outcome {
        NonblockWrite::Written(n) => Ok(Value::integer(n as i64)),
        NonblockWrite::WouldBlock => {
            if exception {
                let cid = io_wait_class(globals, "EAGAINWaitWritable")
                    .ok_or_else(|| MonorubyErr::ioerr("IO::EAGAINWaitWritable not defined"))?;
                Err(MonorubyErr::new(
                    MonorubyErrKind::Other(cid),
                    "Resource temporarily unavailable - write would block".to_string(),
                ))
            } else {
                Ok(Value::symbol(IdentId::get_id("wait_writable")))
            }
        }
    }
}

/// Helper: extract raw fd from a Value that is an IO (or responds to to_io).
fn value_to_fd(vm: &mut Executor, globals: &mut Globals, v: Value) -> Result<i32> {
    if let Some(rv) = v.try_rvalue() {
        if rv.ty() == ObjTy::IO {
            return v.as_io_inner().fileno();
        }
    }
    // Duck typing: CRuby converts non-IO entries through #to_io.
    let to_io = IdentId::get_id("to_io");
    if globals.check_method(v, to_io).is_some() {
        let io = vm.invoke_method_inner(globals, to_io, v, &[], None, None)?;
        if let Some(rv) = io.try_rvalue()
            && rv.ty() == ObjTy::IO
        {
            return io.as_io_inner().fileno();
        }
    }
    Err(MonorubyErr::typeerr(format!(
        "no implicit conversion of {} into IO",
        globals.get_class_name(v.class())
    )))
}

/// Whether `v` is an IO whose userspace read buffer already holds data.
/// CRuby's `IO.select` reports such IOs as ready immediately
/// (`rb_io_read_pending`): the kernel fd may be drained while unread
/// bytes sit in the buffer — waiting on the fd would sleep through data
/// the caller could read right now (the `expect` library's
/// select-then-getc loop hangs exactly there).
fn io_read_pending(v: Value) -> bool {
    v.ty() == Some(crate::value::rvalue::ObjTy::IO) && v.as_io_inner().has_buffered_data()
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

    // Get fds (non-IO entries convert through #to_io).
    let mut read_fds: Vec<i32> = Vec::with_capacity(read_ios.len());
    for v in &read_ios {
        read_fds.push(value_to_fd(vm, globals, *v)?);
    }
    let mut write_fds: Vec<i32> = Vec::with_capacity(write_ios.len());
    for v in &write_ios {
        write_fds.push(value_to_fd(vm, globals, *v)?);
    }
    let mut error_fds: Vec<i32> = Vec::with_capacity(error_ios.len());
    for v in &error_ios {
        error_fds.push(value_to_fd(vm, globals, *v)?);
    }

    // All sets empty, no timeout: with other live threads this parks
    // forever (until killed/woken) with status "sleep", like CRuby; on a
    // lone thread nothing could ever wake us, so return nil immediately.
    if read_ios.is_empty() && write_ios.is_empty() && error_ios.is_empty() {
        if crate::scheduler::has_other_live_threads() {
            let dur = timeout.map(|tv| {
                std::time::Duration::new(
                    tv.tv_sec.max(0) as u64,
                    (tv.tv_usec.clamp(0, 999_999) as u32) * 1000,
                )
            });
            crate::scheduler::sleep(vm, globals, dur)?;
            return Ok(Value::nil());
        }
        if timeout.is_none() {
            return Ok(Value::nil());
        }
    }

    // Green threads: wait on the scheduler's fd poller instead of
    // blocking the whole process in select(2), so other threads keep
    // running while this one waits.
    if crate::scheduler::has_other_live_threads() {
        // An enormous timeout (e.g. select(..., 2**62)) must not
        // overflow Instant arithmetic — treat it as "no deadline".
        let deadline = timeout.and_then(|tv| {
            std::time::Instant::now().checked_add(std::time::Duration::new(
                tv.tv_sec.max(0) as u64,
                (tv.tv_usec.clamp(0, 999_999) as u32) * 1000,
            ))
        });
        // Probe raw fds (the io lists may hold non-IO objects converted
        // through #to_io — the fd lists are the source of truth).
        let probe = |fd: i32, events: i16| -> Result<bool> {
            let mut pfd = libc::pollfd {
                fd,
                events,
                revents: 0,
            };
            // SAFETY: one valid pollfd, length 1, zero timeout.
            let ret = unsafe { libc::poll(&mut pfd, 1, 0) };
            if ret < 0 {
                let err = std::io::Error::last_os_error();
                if err.raw_os_error() == Some(libc::EINTR) {
                    return Ok(false);
                }
                return Err(MonorubyErr::ioerr(format!("poll failed: {err}")));
            }
            Ok(pfd.revents != 0)
        };
        loop {
            let mut ready_read = vec![];
            for (i, &fd) in read_fds.iter().enumerate() {
                if io_read_pending(read_ios[i]) || probe(fd, libc::POLLIN | libc::POLLPRI)? {
                    ready_read.push(read_ios[i]);
                }
            }
            let mut ready_write = vec![];
            for (i, &fd) in write_fds.iter().enumerate() {
                if probe(fd, libc::POLLOUT)? {
                    ready_write.push(write_ios[i]);
                }
            }
            let mut ready_error = vec![];
            for (i, &fd) in error_fds.iter().enumerate() {
                if probe(fd, libc::POLLPRI)? {
                    ready_error.push(error_ios[i]);
                }
            }
            if !ready_read.is_empty() || !ready_write.is_empty() || !ready_error.is_empty() {
                let r = Value::array_from_vec(ready_read);
                let w = Value::array_from_vec(ready_write);
                let e = Value::array_from_vec(ready_error);
                return Ok(Value::array_from_vec(vec![r, w, e]));
            }
            if let Some(dl) = deadline
                && std::time::Instant::now() >= dl
            {
                return Ok(Value::nil());
            }
            let mut fds: Vec<(i32, i16)> = vec![];
            for &fd in &read_fds {
                fds.push((fd, libc::POLLIN | libc::POLLPRI));
            }
            for &fd in &write_fds {
                fds.push((fd, libc::POLLOUT));
            }
            for &fd in &error_fds {
                fds.push((fd, libc::POLLPRI));
            }
            crate::scheduler::wait_fds(vm, globals, &fds, deadline)?;
        }
    }

    // Buffered read data makes an IO immediately ready (see
    // `io_read_pending`): report those without entering select(2),
    // adding any other fds that are kernel-ready right now.
    if read_ios.iter().any(|v| io_read_pending(*v)) {
        let poll0 = |fd: i32, events: i16| -> bool {
            let mut pfd = libc::pollfd {
                fd,
                events,
                revents: 0,
            };
            // SAFETY: one valid pollfd, length 1, zero timeout.
            let ret = unsafe { libc::poll(&mut pfd, 1, 0) };
            ret > 0 && pfd.revents != 0
        };
        let mut ready_read = vec![];
        for (i, &fd) in read_fds.iter().enumerate() {
            if io_read_pending(read_ios[i]) || poll0(fd, libc::POLLIN | libc::POLLPRI) {
                ready_read.push(read_ios[i]);
            }
        }
        let mut ready_write = vec![];
        for (i, &fd) in write_fds.iter().enumerate() {
            if poll0(fd, libc::POLLOUT) {
                ready_write.push(write_ios[i]);
            }
        }
        let mut ready_error = vec![];
        for (i, &fd) in error_fds.iter().enumerate() {
            if poll0(fd, libc::POLLPRI) {
                ready_error.push(error_ios[i]);
            }
        }
        return Ok(Value::array_from_vec(vec![
            Value::array_from_vec(ready_read),
            Value::array_from_vec(ready_write),
            Value::array_from_vec(ready_error),
        ]));
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

        // On Linux, select(2) updates the timeval to the time not slept,
        // so re-entering after an EINTR (below) continues with the
        // remaining timeout rather than starting over.
        let mut tv_storage = timeout.unwrap_or(libc::timeval {
            tv_sec: 0,
            tv_usec: 0,
        });

        let ret = loop {
            // select(2) mutates the fd sets, so rebuild them each attempt.
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
                if err.raw_os_error() == Some(libc::EINTR) {
                    // A signal interrupted the wait: run the VM poll
                    // point (raise the converted SignalException, or run
                    // the trap handler and retry the select), matching
                    // CRuby, which restarts select after processing
                    // interrupts instead of surfacing Errno::EINTR.
                    if crate::executor::execute_gc(vm, globals).is_none() {
                        return Err(vm.take_error());
                    }
                    continue;
                }
                return Err(MonorubyErr::errno_with_msg(
                    &globals.store,
                    &err,
                    "select(2)",
                ));
            }
            break ret;
        };

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
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let self_ = lfp.self_val();
    // Signature: set_encoding(ext, int = nil, **opts) — a third
    // positional is only legal as the options Hash.
    if let Some(a2) = lfp.try_arg(2)
        && a2.try_hash_ty().is_none()
    {
        return Err(MonorubyErr::argumenterr(
            "wrong number of arguments (given 3, expected 1..2)",
        ));
    }
    let arg0 = coerce_enc_arg(vm, globals, lfp.arg(0))?;
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
        let arg1 = coerce_enc_arg(vm, globals, arg1)?;
        int = arg_to_enc_obj(globals, arg1);
    }
    let readable = self_.as_io_inner().is_readable();
    let writable = self_.as_io_inner().is_writable();
    let de = enc_default_external_obj(globals);
    let di = enc_default_internal_obj(globals);
    let (slot, i) = resolve_io_encodings(globals, ext, int, false, readable, writable, de, di);
    store_io_encodings(globals, self_, slot, i);
    Ok(self_)
}

///
/// ### IO#set_encoding_by_bom
/// - set_encoding_by_bom -> Encoding | nil
///
/// Peeks the leading byte-order mark, and if one is present sets the
/// external encoding accordingly and consumes the BOM bytes (leaving
/// the rest of the stream readable). Returns the detected `Encoding`,
/// or `nil` when there is no BOM or the stream isn't readable.
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/set_encoding_by_bom.html]
#[monoruby_builtin]
fn set_encoding_by_bom(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let self_ = lfp.self_val();
    if !self_.as_io_inner().is_readable() {
        return Ok(Value::nil());
    }
    // Preconditions (CRuby `rb_io_set_encoding_by_bom`): the stream
    // must be in binmode, with no external/internal encoding already
    // chosen — a BOM determines the external encoding from scratch.
    let binmode = self_.as_io_inner().binmode();
    if !binmode {
        return Err(MonorubyErr::argumenterr(
            "ASCII incompatible encoding needs binmode",
        ));
    }
    if self_.as_io_inner().int_enc().is_some() {
        return Err(MonorubyErr::argumenterr("encoding conversion is set"));
    }
    if let Some(ext) = fixed_ext_encoding(globals, self_) {
        let name = super::encoding::encoding_object_name(globals, ext)
            .unwrap_or_else(|| "UTF-8".to_string());
        return Err(MonorubyErr::argumenterr(format!(
            "encoding is set to {name} already"
        )));
    }
    match detect_and_consume_bom(vm, globals, self_)? {
        Some(enc) => Ok(enc),
        None => Ok(Value::nil()),
    }
}

/// Peek the stream head for a byte-order mark. A detected BOM is
/// consumed and installs its encoding as the external encoding
/// (returned as `Some`); otherwise every byte is pushed back and the
/// stored encodings stay untouched.
fn detect_and_consume_bom(
    vm: &mut Executor,
    globals: &mut Globals,
    mut io: Value,
) -> Result<Option<Value>> {
    // Read up to 4 bytes — enough to disambiguate every BOM (and to
    // tell UTF-16LE from UTF-32LE, which share the FF FE prefix).
    let head = blocking_io_region(vm, globals, io, libc::POLLIN, |_store| {
        io.as_io_inner_mut().read(Some(4))
    })?;
    let (consume, enc_name): (usize, &str) = match head.first() {
        Some(0xEF) if head.len() >= 3 && head[1] == 0xBB && head[2] == 0xBF => (3, "UTF-8"),
        Some(0x00) if head.len() >= 4 && head[1] == 0x00 && head[2] == 0xFE && head[3] == 0xFF => {
            (4, "UTF-32BE")
        }
        Some(0xFF) if head.len() >= 2 && head[1] == 0xFE => {
            if head.len() >= 4 && head[2] == 0x00 && head[3] == 0x00 {
                (4, "UTF-32LE")
            } else {
                (2, "UTF-16LE")
            }
        }
        Some(0xFE) if head.len() >= 2 && head[1] == 0xFF => (2, "UTF-16BE"),
        _ => {
            // No BOM: push everything back and report nothing.
            io.as_io_inner_mut().unget(&head)?;
            return Ok(None);
        }
    };
    // Consume the BOM, push the trailing bytes back for later reads.
    io.as_io_inner_mut().unget(&head[consume..])?;
    let enc = enc_by_name(globals, enc_name)
        .ok_or_else(|| MonorubyErr::runtimeerr(format!("encoding {enc_name} not found")))?;
    store_io_encodings(globals, io, ExtSlot::Fixed(enc), None);
    Ok(Some(enc))
}

/// An Encoding object -> the internal `Encoding` enum (folding the
/// names the enum doesn't represent distinctly down to ASCII-8BIT).
pub(super) fn enc_obj_to_enum(globals: &Globals, v: Value) -> Option<crate::value::Encoding> {
    // Borrow the name out of the Encoding object's ivar rather than
    // cloning it: this runs three times per `IO#getc` / `IO#gets`.
    let name_v = globals.store.get_ivar(v, IdentId::_ENCODING)?;
    let name = name_v.is_str()?;
    crate::value::Encoding::try_from_str(name).ok()
}

/// Tag `bytes` with the resolved external encoding (defaulting to
/// `Encoding.default_external`), transcoding external -> internal when
/// an internal encoding is set and differs and the external is not
/// BINARY. Used by the `IO.readlines` / `IO.foreach` class methods.
pub(super) fn tag_with_encs(
    globals: &mut Globals,
    bytes: Vec<u8>,
    ext_obj: Option<Value>,
    int_obj: Option<Value>,
) -> Value {
    use crate::value::Encoding as E;
    let ext = match ext_obj {
        Some(o) => enc_obj_to_enum(globals, o).unwrap_or(E::Utf8),
        None => {
            let de = enc_default_external_obj(globals);
            enc_obj_to_enum(globals, de).unwrap_or(E::Utf8)
        }
    };
    // An unset internal encoding falls back to Encoding.default_internal
    // (this is what makes IO.readlines & friends honor it).
    let intl = int_obj
        .or_else(|| enc_default_internal_obj(globals))
        .and_then(|o| enc_obj_to_enum(globals, o));
    let (out, final_enc) = match intl {
        Some(i) if i != ext && !matches!(ext, E::Ascii8) => {
            let topts = super::encoding::TranscodeOpts::default();
            match super::encoding::transcode_bytes_with_opts(&bytes, ext, i, &topts, &globals.store)
            {
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
    let (ext, intl) = io_encodings(globals, io);
    tagged_read_string_with(globals, bytes, ext, intl)
}

/// The stream's `(external, internal)` encodings, resolved once.
///
/// Callers that need them anyway (`IO#getc`, `IO#gets`) resolve here and
/// hand the result to [`tagged_read_string_with`]: going back through
/// `tagged_read_string` would repeat the ivar reads and the name parse
/// two more times per character or line.
pub(super) fn io_encodings(
    globals: &mut Globals,
    io: Value,
) -> (crate::value::Encoding, Option<crate::value::Encoding>) {
    use crate::value::Encoding as E;
    let ext_v = read_io_encoding(globals, io, false);
    let int_v = read_io_encoding(globals, io, true);
    let ext = enc_obj_to_enum(globals, ext_v).unwrap_or(E::Utf8);
    let intl = if int_v.is_nil() {
        None
    } else {
        enc_obj_to_enum(globals, int_v)
    };
    (ext, intl)
}

/// [`tagged_read_string`] with the encodings already resolved.
pub(super) fn tagged_read_string_with(
    globals: &mut Globals,
    bytes: Vec<u8>,
    ext: crate::value::Encoding,
    intl: Option<crate::value::Encoding>,
) -> Value {
    let (out, final_enc) = match intl {
        Some(i) if i != ext => {
            let opts = super::encoding::TranscodeOpts::default();
            match super::encoding::transcode_bytes_with_opts(&bytes, ext, i, &opts, &globals.store)
            {
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
    let inner = io.as_io_inner();
    if internal {
        return inner.int_enc().unwrap_or_default();
    }
    match inner.ext_state() {
        ExtEnc::Fixed => inner.ext_enc().unwrap_or_default(),
        ExtEnc::Nil => Value::nil(),
        // Explicitly following `Encoding.default_external`, or never set
        // at all — both resolve it now, so a later change to it is seen.
        // Exception: the write-only stdio streams, where CRuby reports
        // `nil` until one is set explicitly with `#set_encoding`.
        ExtEnc::Dynamic => enc_default_external_obj(globals),
        ExtEnc::Unset => {
            if matches!(inner.kind(), IoKind::Stdout | IoKind::Stderr) {
                Value::nil()
            } else {
                enc_default_external_obj(globals)
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
/// Green-thread-aware fd wait for the `IO#wait*` family: when other green
/// threads are live, park on the scheduler's fd poller (deadline-bounded)
/// instead of issuing a process-blocking poll(2) — a plain poll freezes
/// every other green thread for the whole timeout, and the peer this wait
/// is waiting *for* is typically one of them (e.g. net/protocol's
/// `rbuf_fill` waiting on a spec's in-process server thread). Returns the
/// ready revents, 0 on timeout.
fn wait_single_fd_events(
    vm: &mut Executor,
    globals: &mut Globals,
    self_val: Value,
    events: i16,
    timeout_ms: i32,
) -> Result<i16> {
    if !crate::scheduler::has_other_live_threads() {
        return poll_single_fd(self_val, events, timeout_ms);
    }
    let deadline = if timeout_ms >= 0 {
        Some(std::time::Instant::now() + std::time::Duration::from_millis(timeout_ms as u64))
    } else {
        None
    };
    loop {
        // Zero-timeout probe; also raises IOError once the stream is
        // closed (including by another thread while we were parked).
        let revents = poll_single_fd(self_val, events, 0)?;
        if revents != 0 {
            return Ok(revents);
        }
        if let Some(dl) = deadline
            && std::time::Instant::now() >= dl
        {
            return Ok(0);
        }
        let fd = self_val.as_io_inner().wait_fd_for(events)?;
        crate::scheduler::wait_fd(vm, globals, fd, events, deadline)?;
    }
}

fn poll_single_fd(self_val: Value, events: i16, timeout_ms: i32) -> Result<i16> {
    if self_val.as_io_inner().is_closed() {
        return Err(MonorubyErr::ioerr("closed stream"));
    }
    let fd = self_val.as_io_inner().wait_fd_for(events)?;
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
        let revents = wait_single_fd_events(vm, globals, self_val, libc::POLLIN as i16, -1)?;
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

    let revents = wait_single_fd_events(vm, globals, self_val, poll_events, timeout_ms)?;

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
    let revents = wait_single_fd_events(vm, globals, self_val, libc::POLLIN as i16, timeout_ms)?;
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
    let revents = wait_single_fd_events(vm, globals, self_val, libc::POLLOUT as i16, timeout_ms)?;
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
    let revents = wait_single_fd_events(vm, globals, self_val, libc::POLLPRI as i16, timeout_ms)?;
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
/// Streams from `src` to `dst` in chunks: `src`/`dst` are IOs, file names
/// (String / `#to_path`), or arbitrary objects speaking the read
/// (`#readpartial(len, buf)` / `#read(len, buf)`) or write (`#write`)
/// protocol. Returns the number of bytes copied.
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/s/copy_stream.html]
#[monoruby_builtin]
fn io_copy_stream(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    const CHUNK: usize = 16 * 1024;

    let copy_length: Option<u64> = match lfp.try_arg(2) {
        Some(v) if !v.is_nil() => {
            let l = v.coerce_to_int_i64(vm, globals)?;
            if l < 0 {
                return Err(MonorubyErr::argumenterr(format!("negative length {l} given")));
            }
            Some(l as u64)
        }
        _ => None,
    };
    let src_offset: Option<u64> = match lfp.try_arg(3) {
        Some(v) if !v.is_nil() => {
            let o = v.coerce_to_int_i64(vm, globals)?;
            if o < 0 {
                return Err(MonorubyErr::argumenterr("negative offset"));
            }
            Some(o as u64)
        }
        _ => None,
    };

    // Length 0: copy nothing — the source is not read and the
    // destination is not written (per spec, not even dispatched to).
    if copy_length == Some(0) {
        return Ok(Value::integer(0));
    }

    // What a copy endpoint is: a real IO, a file path, or an object
    // speaking the read/write protocol.
    enum Src {
        Io(Value),
        Path(String),
        Obj(Value, IdentId), // reader method: readpartial or read
    }
    enum Dst {
        Io(Value),
        Path(String),
        Obj(Value),
    }

    let is_io = |v: Value| v.try_rvalue().is_some_and(|rv| rv.ty() == ObjTy::IO);
    // CRuby's endpoint resolution order: an IO (or anything `#to_io`
    // converts, e.g. Tempfile) is used as an IO *before* the `#to_path` /
    // String file-name form is considered.
    let to_io = |vm: &mut Executor, globals: &mut Globals, v: Value| -> Result<Value> {
        if !is_io(v) && globals.check_method(v, IdentId::get_id("to_io")).is_some() {
            let converted =
                vm.invoke_method_inner(globals, IdentId::get_id("to_io"), v, &[], None, None)?;
            if is_io(converted) {
                return Ok(converted);
            }
        }
        Ok(v)
    };
    let src_v = to_io(vm, globals, lfp.arg(0))?;
    let src = if is_io(src_v) {
        src_v.as_io_inner().ensure_readable()?;
        Src::Io(src_v)
    } else if src_v.is_rstring().is_some()
        || globals.check_method(src_v, IdentId::TO_PATH).is_some()
    {
        Src::Path(
            src_v
                .coerce_to_path_rstring(vm, globals)?
                .to_str()?
                .to_string(),
        )
    } else {
        // Object protocol: #readpartial is preferred over #read.
        let rp = IdentId::get_id("readpartial");
        let rd = IdentId::get_id("read");
        let m = if globals.check_method(src_v, rp).is_some() {
            rp
        } else if globals.check_method(src_v, rd).is_some() {
            rd
        } else {
            return Err(MonorubyErr::typeerr(format!(
                "no implicit conversion of {} into IO",
                src_v.get_real_class_name(globals)
            )));
        };
        Src::Obj(src_v, m)
    };

    let dst_v = to_io(vm, globals, lfp.arg(1))?;
    let dst = if is_io(dst_v) {
        dst_v.as_io_inner().ensure_writable()?;
        Dst::Io(dst_v)
    } else if dst_v.is_rstring().is_some()
        || globals.check_method(dst_v, IdentId::TO_PATH).is_some()
    {
        Dst::Path(
            dst_v
                .coerce_to_path_rstring(vm, globals)?
                .to_str()?
                .to_string(),
        )
    } else if globals.check_method(dst_v, IdentId::get_id("write")).is_some() {
        Dst::Obj(dst_v)
    } else {
        return Err(MonorubyErr::typeerr(format!(
            "no implicit conversion of {} into IO",
            dst_v.get_real_class_name(globals)
        )));
    };

    // A path source is opened (and closed on scope exit) as a plain
    // read-only IO; the transient Value is GC-rooted below.
    let mut src_io_owned: Option<Value> = None;
    let mut src_val = match &src {
        Src::Io(v) => Some(*v),
        Src::Path(p) => {
            let file = std::fs::File::open(p)
                .map_err(|e| MonorubyErr::errno_with_path(&globals.store, &e, "rb_sysopen", p))?;
            let v = Value::new_file(file, p.clone(), None, true, false);
            src_io_owned = Some(v);
            Some(v)
        }
        Src::Obj(..) => None,
    };
    let mut dst_io_owned: Option<Value> = None;
    let mut dst_val = match &dst {
        Dst::Io(v) => Some(*v),
        Dst::Path(p) => {
            let file = std::fs::File::options()
                .write(true)
                .create(true)
                .truncate(true)
                .open(p)
                .map_err(|e| MonorubyErr::errno_with_path(&globals.store, &e, "rb_sysopen", p))?;
            let v = Value::new_file(file, p.clone(), None, false, true);
            dst_io_owned = Some(v);
            Some(v)
        }
        Dst::Obj(_) => None,
    };

    let result = vm.with_temp_scope(|vm| {
        if let Some(v) = src_io_owned {
            vm.temp_push(v);
        }
        if let Some(v) = dst_io_owned {
            vm.temp_push(v);
        }

        // Write one chunk to the destination, looping over partial
        // writes. Object destinations dispatch #write and read back the
        // written byte count.
        let mut write_chunk = |vm: &mut Executor,
                               globals: &mut Globals,
                               chunk: &[u8]|
         -> Result<()> {
            match (&dst, &mut dst_val) {
                (Dst::Obj(obj), _) => {
                    let mut rest = chunk;
                    while !rest.is_empty() {
                        let s = Value::bytes(rest.to_vec());
                        let n = vm
                            .invoke_method_inner(
                                globals,
                                IdentId::get_id("write"),
                                *obj,
                                &[s],
                                None,
                                None,
                            )?
                            .coerce_to_int_i64(vm, globals)?;
                        let n = (n.max(0) as usize).min(rest.len());
                        if n == 0 {
                            return Err(MonorubyErr::ioerr("write failed"));
                        }
                        rest = &rest[n..];
                    }
                    Ok(())
                }
                (_, Some(v)) => {
                    // Flush per chunk: copy_stream must not buffer (a
                    // pipe reader on the other side expects the data
                    // immediately — Rust's stdout is block-buffered).
                    let mut vv = *v;
                    let mut done = 0;
                    blocking_io_region(vm, globals, vv, libc::POLLOUT, |_store| {
                        vv.as_io_inner_mut().write(chunk, &mut done, _store)
                    })?;
                    vv.as_io_inner_mut().flush(&globals.store)
                }
                _ => unreachable!(),
            }
        };

        let mut copied: u64 = 0;
        match (&src, &mut src_val) {
            // IO source with an explicit offset: pread(2)-style reads on
            // the raw fd, leaving the source's own position untouched.
            (_, Some(v)) if src_offset.is_some() => {
                let fd = v.as_io_inner().fileno()?;
                let mut offset = src_offset.unwrap() as libc::off_t;
                loop {
                    let want = match copy_length {
                        Some(l) => ((l - copied) as usize).min(CHUNK),
                        None => CHUNK,
                    };
                    if want == 0 {
                        break;
                    }
                    let mut buf = vec![0u8; want];
                    // SAFETY: fd is a live descriptor; buf is a valid
                    // writable buffer of `want` bytes.
                    let n = unsafe {
                        libc::pread(fd, buf.as_mut_ptr() as *mut libc::c_void, want, offset)
                    };
                    if n < 0 {
                        let err = std::io::Error::last_os_error();
                        // A pipe/socket source cannot seek: ESPIPE.
                        return Err(MonorubyErr::errno_plain(&globals.store, &err));
                    }
                    if n == 0 {
                        break;
                    }
                    buf.truncate(n as usize);
                    write_chunk(vm, globals, &buf)?;
                    copied += n as u64;
                    offset += n as libc::off_t;
                }
            }
            // IO source at its current position: readpartial-style
            // streaming so pipe data flows through without waiting for
            // EOF (and the source position advances).
            (_, Some(v)) => {
                loop {
                    let want = match copy_length {
                        Some(l) => ((l - copied) as usize).min(CHUNK),
                        None => CHUNK,
                    };
                    if want == 0 {
                        break;
                    }
                    let mut vv = *v;
                    let chunk = blocking_io_region(vm, globals, vv, libc::POLLIN, |_store| {
                        vv.as_io_inner_mut().readpartial(want)
                    })?;
                    if chunk.is_empty() {
                        break;
                    }
                    write_chunk(vm, globals, &chunk)?;
                    copied += chunk.len() as u64;
                }
            }
            // Object source: dispatch #readpartial/#read(len, buf) until
            // nil (read) or EOFError (readpartial).
            (Src::Obj(obj, meth), _) => {
                let meth = *meth;
                let is_readpartial = meth == IdentId::get_id("readpartial");
                loop {
                    let want = match copy_length {
                        Some(l) => ((l - copied) as usize).min(CHUNK),
                        None => CHUNK,
                    };
                    if want == 0 {
                        break;
                    }
                    let buf = Value::string_from_str("");
                    let r = vm.invoke_method_inner(
                        globals,
                        meth,
                        *obj,
                        &[Value::integer(want as i64), buf],
                        None,
                        None,
                    );
                    let chunk = match r {
                        Ok(v) if v.is_nil() => break,
                        Ok(v) => v,
                        Err(err) => {
                            // #readpartial signals EOF by raising EOFError.
                            if is_readpartial && err.message().contains("end of file") {
                                break;
                            }
                            return Err(err);
                        }
                    };
                    let Some(bytes) = chunk.is_rstring() else {
                        return Err(MonorubyErr::typeerr("read should return a String"));
                    };
                    let bytes = bytes.as_bytes().to_vec();
                    if bytes.is_empty() {
                        break;
                    }
                    write_chunk(vm, globals, &bytes)?;
                    copied += bytes.len() as u64;
                }
            }
            _ => unreachable!(),
        }
        Ok(copied)
    });

    // Close only the endpoints this call opened (path forms); IO
    // arguments are left open, per CRuby.
    if let Some(mut v) = src_io_owned {
        let _ = v.as_io_inner_mut().close(&globals.store);
    }
    if let Some(mut v) = dst_io_owned {
        let _ = v.as_io_inner_mut().close(&globals.store);
    }
    Ok(Value::integer(result? as i64))
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn io_sysseek_whence_symbols_and_advise() {
        // sysseek accepts :SET/:CUR/:END (and integers); an unknown symbol
        // is ArgumentError; an unknown advice is NotImplementedError.
        run_test_once(
            r##"(f="/tmp/mono_ss_#{Process.pid}"; File.write(f,"0123456789"); io=File.open(f); a=io.sysseek(3, :SET); b=io.sysseek(2, :CUR); c=io.sysseek(-1, :END); d=io.sysseek(4, IO::SEEK_SET); e2=io.sysseek(1); g=(begin; io.sysseek(0, :XX); rescue => e; e.class; end); h=(begin; io.advise(:bogus); rescue Exception => e; [e.class, e.message]; end); io.close; File.delete(f); [a,b,c,d,e2,g,h])"##,
        );
    }

    #[test]
    fn io_close_halves_on_non_duplex() {
        // close_read/close_write: no-op on a closed stream; fully close a
        // non-duplex stream missing the other side; refuse when the other
        // side is live.
        run_test_once(
            r##"(f="/tmp/mono_ch_#{Process.pid}"; File.write(f,"x"); r=File.open(f); r.close_read; a=r.closed?; r.close_read; b=File.open(f){|x| begin; x.close_write; rescue => e; e.class; end}; w=File.open(f,"w"); w.close_write; c=w.closed?; w.close_write; d=File.open(f,"w"){|x| begin; x.close_read; rescue => e; e.class; end}; e2=File.open(f){|x| x.close_read; x.closed?}; File.delete(f); [a,b,c,d,e2])"##,
        );
    }

    #[test]
    fn io_sync_putc_to_i() {
        // #sync round-trips (default: true only for stderr), #putc handles
        // String/Integer/#to_int and closed streams, #to_i aliases #fileno.
        run_test_once(
            r##"(f="/tmp/mono_sy_#{Process.pid}"; io=File.open(f,"w"); a=[io.sync, STDERR.sync]; io.sync="x"; b=io.sync; io.putc("AB"); io.putc(66); o=Object.new; def o.to_int; 67; end; io.putc(o); c=(io.to_i==io.fileno); d=(IO.instance_method(:to_i)==IO.instance_method(:fileno)); io.close; e2=(begin; io.putc("a"); rescue => e; e.class; end); g=File.read(f); h=[IO.include?(File::Constants), IO.include?(Enumerable), File::SYNC.is_a?(Integer), File::Constants::SHARE_DELETE, File::NOCTTY.is_a?(Integer), File::Constants::FNM_DOTMATCH]; File.delete(f); [a,b,c,d,e2,g,h])"##,
        );
    }

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
            # CRuby: STDOUT/STDERR have no external encoding until one
            # is set explicitly; STDIN defaults to default_external.
            raise "stdout should be nil" unless $stdout.external_encoding.nil?
            raise "stderr should be nil" unless $stderr.external_encoding.nil?
            raise "stdin should be Encoding" unless $stdin.external_encoding.is_a?(Encoding)
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
    fn gets_separator_limit_chomp() {
        // Full IO#gets / #readline argument semantics: separator
        // (default/$-slash/custom/paragraph/nil), byte limit (with UTF-8
        // character completion), chomp:, argument coercion, and the
        // lineno / `$.` / `$_` bookkeeping.
        run_test_once(
            r##"
            require "tempfile"
            t = Tempfile.new("mrb_gets")
            path = t.path
            File.write(path, "one x\ntwo x\n\n\n\nthree\nfour\n")
            r = []
            f = File.open(path); r << f.gets << f.gets << f.lineno << $.; f.close
            f = File.open(path); r << f.gets(nil); f.close
            f = File.open(path); r << f.gets("") << f.gets("") << f.gets(""); f.close
            f = File.open(path); r << f.gets("x") << f.gets("x", 2); f.close
            f = File.open(path); r << f.gets(4) << f.gets(0) << f.gets; f.close
            f = File.open(path); r << f.gets(chomp: true) << f.gets("", chomp: true); f.close
            f = File.open(path); r << f.gets("x", chomp: true) << f.gets(nil, chomp: true); f.close
            f = File.open(path); f.gets(""); r << f.gets; f.close # positioned at next paragraph
            f = File.open(path); r << f.readline(chomp: true); f.close
            f = File.open(path); f.readline; r << $_; f.close
            f = File.open(path); 9.times { f.readline } rescue r << :eof; f.close
            sep = Object.new; def sep.to_str = "x"
            lim = Object.new; def lim.to_int = 3
            f = File.open(path); r << f.gets(sep, lim) << f.gets(lim); f.close
            f = File.open(path); begin; f.gets(2**64); rescue RangeError; r << :range; end; f.close
            f = File.open(path); begin; f.gets({chomp: true}); rescue TypeError; r << :tyerr; end; f.close
            f = File.open(path); r << f.gets(foo: 1); f.close # unknown kwargs tolerated
            $/ = "o"
            f = File.open(path); r << f.gets; f.close
            $/ = nil
            f = File.open(path); r << f.gets&.bytesize; f.close
            $/ = "\n"
            # UTF-8 character completion when the limit cuts mid-character
            File.binwrite(path, "朝日")
            f = File.open(path); r << [f.gets(1)&.bytes, f.gets(1)&.bytes]; f.close
            t.close!
            r
            "##,
        );
    }

    #[test]
    fn lines_family_separator_limit_chomp() {
        // IO#each_line / IO#readlines / IO.foreach / IO.readlines share
        // the gets argument semantics; foreach also maintains `$.`/`$_`
        // and returns an Enumerator without a block.
        run_test_once(
            r##"
            require "tempfile"
            t = Tempfile.new("mrb_lines")
            path = t.path
            File.write(path, "one x\ntwo x\n\n\n\nthree\nfour\n")
            r = []
            f = File.open(path); r << f.each_line.to_a; f.close
            f = File.open(path); a = []; f.each_line("") { |l| a << l }; r << a; f.close
            f = File.open(path); a = []; f.each_line(4) { |l| a << l }; r << a; f.close
            f = File.open(path); a = []; f.each_line("x", chomp: true) { |l| a << l }; r << a; f.close
            f = File.open(path); begin; f.each_line(0) {}; rescue ArgumentError => e; r << e.message; end; f.close
            f = File.open(path); r << f.readlines(chomp: true) << f.readlines; f.close
            f = File.open(path); r << f.readlines("", 8).first(2); f.close
            f = File.open(path); r << f.readlines(foo: 70).size; f.close
            r << IO.readlines(path, chomp: true).first(3)
            r << IO.readlines(path, "x", 3)
            r << IO.readlines(path, nil).size
            begin; IO.readlines(path, {chomp: true}); rescue TypeError; r << :t1; end
            begin; IO.readlines(path, "", {chomp: true}); rescue TypeError; r << :t2; end
            a = []; IO.foreach(path) { |l| a << [l, $.] }; r << a << $_.inspect
            r << IO.foreach(path).to_a
            r << IO.foreach(path, "", chomp: true).to_a
            r << IO.foreach(path).size.inspect
            mock = Object.new
            mock.define_singleton_method(:to_path) { path }
            r << IO.foreach(mock).to_a.size << IO.readlines(mock).size
            begin; IO.readlines(path, mode: "w"); rescue IOError; r << :notread; end
            r << File.read(path) # mode "w" rejection truncates first, like CRuby
            t.close!
            r
            "##,
        );
    }

    #[test]
    fn read_with_output_buffer() {
        // IO#read(length, outbuf): the buffer is replaced with the read
        // bytes and returned; frozen buffers raise up front; a sized read
        // keeps the buffer's encoding, an unsized read retags it.
        run_test_once(
            r##"
            require "tempfile"
            t = Tempfile.new("mrb_readbuf")
            path = t.path
            File.write(path, "1234567890")
            r = []
            f = File.open(path); b = +"seed"; ret = f.read(6, b); r << [ret.equal?(b), b]; f.close
            f = File.open(path); b = +"a much longer buffer"; f.read(3, b); r << b; f.close
            f = File.open(path); b = +"z"; ret = f.read(nil, b); r << [ret.equal?(b), b, b.encoding.name]; f.close
            f = File.open(path); f.read; b = +"left"; r << [f.read(nil, b), b]; f.close
            f = File.open(path); f.read; b = +"left"; r << [f.read(5, b), b]; f.close
            f = File.open(path); begin; f.read(5, "s".freeze); rescue FrozenError; r << :frozen; end; f.close
            f = File.open(path)
            m = Object.new; inner = +"m"; m.define_singleton_method(:to_str) { inner }
            ret = f.read(4, m); r << [ret.equal?(inner), inner]; f.close
            f = File.open(path); b = (+"x").force_encoding("ISO-8859-1"); f.read(3, b); r << b.encoding.name; f.close
            t.close!
            r
            "##,
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
    fn copy_stream_semantics() {
        // IO.copy_stream: endpoint forms (IO / path / #to_path / object
        // protocol), length/offset handling (offset leaves the source
        // position untouched), open/close responsibilities, and the
        // zero-length no-dispatch rule.
        run_test_once(
            r##"
            require "tempfile"
            src = Tempfile.new("mrb_cs_src"); to = Tempfile.new("mrb_cs_dst")
            content = "Line one\nLine two\nLine three\nAnd so on...\n"
            File.write(src.path, content)
            r = []
            er = ->(&b) { begin; b.call; :no_raise; rescue => e; [e.class]; end }
            r << IO.copy_stream(src.path, to.path) << File.read(to.path)
            r << IO.copy_stream(src.path, to.path, 8) << File.read(to.path)
            r << IO.copy_stream(src.path, to.path, 8, 4) << File.read(to.path)
            m = Object.new; sp = src.path; m.define_singleton_method(:to_path) { sp }
            r << IO.copy_stream(m, to.path)
            bad = Object.new; bad.define_singleton_method(:to_path) { 42 }
            r << er.call { IO.copy_stream(bad, to.path) }
            f = File.open(src.path, "rb"); f.pos = 10
            r << IO.copy_stream(f, to.path, 8, 4) << f.pos
            r << IO.copy_stream(f, to.path) << f.pos << f.closed?; f.close
            f = File.open(to.path, "a")
            r << er.call { IO.copy_stream(f, src.path) }; f.close
            File.write(to.path, "0123456789")
            f = File.open(src.path, "rb"); g = File.open(to.path, "rb+"); g.pos = 3
            r << IO.copy_stream(f, g, 4) << g.pos
            g.seek(0, IO::SEEK_SET); r << g.read << g.closed?
            f.close; g.close
            rd, wr = IO.pipe
            wr.write "pipe data"; wr.close
            r << IO.copy_stream(rd, to.path) << File.read(to.path) << rd.closed?
            rd.close
            rd, wr = IO.pipe
            wr.write "x"; wr.close
            r << er.call { IO.copy_stream(rd, to.path, 1, 0) }; rd.close
            reader = Object.new
            fsrc = File.open(src.path, "rb")
            reader.define_singleton_method(:read) { |size, buf| fsrc.read(size, buf) }
            r << IO.copy_stream(reader, to.path) << File.read(to.path)
            fsrc.close
            writer = Object.new; sink = +""
            writer.define_singleton_method(:write) { |s| sink << s; s.bytesize }
            r << IO.copy_stream(src.path, writer) << sink
            zr = Object.new; zr.define_singleton_method(:read) { |*| raise "read called" }
            zw = Object.new; zw.define_singleton_method(:write) { |*| raise "write called" }
            r << IO.copy_stream(zr, zw, 0)
            File.write(src.path, "A" * 17_000)
            r << IO.copy_stream(src.path, to.path) << File.read(to.path).bytesize
            src.close!; to.close!
            r
            "##,
        );
    }

    #[test]
    fn io_new_mode_and_option_semantics() {
        // IO.new / IO.for_fd / IO#initialize argument semantics
        // (CRuby's rb_io_extract_modeenc): mode conflicts, binmode /
        // textmode keyword interactions, encoding resolution and
        // conflicts, EINVAL for an incompatible fd mode, and coercions.
        run_test_once(
            r##"
            require "tempfile"
            t = Tempfile.new("mrb_ionew")
            path = t.path
            File.write(path, "x")
            r = []
            er = ->(&b) { begin; b.call; :no_raise; rescue => e; [e.class, e.message]; end }
            wfd = -> { IO.sysopen(path, "w") }
            drain = ->(fd) { IO.new(fd, "w").close }
            io = IO.new(wfd.call, "w"); r << io.class; io.close
            m = Object.new; fdv = wfd.call; m.define_singleton_method(:to_int) { fdv }
            io = IO.new(m, "w"); r << (io.fileno == fdv); io.close
            fd = wfd.call; r << er.call { IO.new(fd, "w", {flags: 1}) }; drain.(fd)
            fd = wfd.call; r << er.call { IO.new(fd, "w", nil) }; drain.(fd)
            fd = wfd.call; r << er.call { IO.new(fd, "w", mode: "w") }; drain.(fd)
            fd = wfd.call; r << er.call { IO.new(fd, "") }; drain.(fd)
            fd = wfd.call; r << er.call { IO.new(fd, "r") }; drain.(fd)   # EINVAL
            r << er.call { IO.new("4", "w") } << er.call { IO.new(STDOUT, "w") }
            r << er.call { IO.new(-1, "w") }.first
            io = IO.new(wfd.call, nil, mode: "w"); r << :mode_kw; io.close
            io = IO.new(wfd.call, File::WRONLY); r << :int_mode; io.close
            mi = Object.new; mi.define_singleton_method(:to_int) { File::Constants::WRONLY }
            io = IO.new(wfd.call, mode: mi); r << :toint_kw; io.close
            io = IO.new(wfd.call, 'w:utf-8:ISO-8859-1')
            r << [io.external_encoding.to_s, io.internal_encoding.to_s]; io.close
            io = IO.new(wfd.call, 'w', encoding: 'utf-8:ISO-8859-1')
            r << [io.external_encoding.to_s, io.internal_encoding.to_s]; io.close
            io = IO.new(wfd.call, 'w', external_encoding: 'utf-8', internal_encoding: 'utf-8')
            r << [io.external_encoding.to_s, io.internal_encoding.inspect]; io.close
            io = IO.new(wfd.call, 'w', external_encoding: 'utf-8', internal_encoding: '-')
            r << io.internal_encoding.inspect; io.close
            fd = wfd.call; r << er.call { IO.new(fd, 'w:ISO-8859-1', encoding: 'ISO-8859-1') }; drain.(fd)
            io = IO.new(wfd.call, 'wb'); r << [io.binmode?, io.external_encoding.to_s]; io.close
            io = IO.new(wfd.call, 'w', binmode: true); r << [io.binmode?, io.external_encoding.to_s]; io.close
            io = IO.new(wfd.call, 'wb:iso-8859-1'); r << io.external_encoding.to_s; io.close
            io = IO.new(wfd.call, 'w:iso-8859-1', binmode: true); r << io.external_encoding.to_s; io.close
            fd = wfd.call; r << er.call { IO.new(fd, "wb", binmode: true) }; drain.(fd)
            fd = wfd.call; r << er.call { IO.new(fd, "wb", textmode: false) }; drain.(fd)
            fd = wfd.call; r << er.call { IO.new(fd, "wt", binmode: true) }; drain.(fd)
            fd = wfd.call; r << er.call { IO.new(fd, "w", textmode: true, binmode: true) }; drain.(fd)
            io = IO.new(wfd.call, 'w', binmode: false); r << io.binmode?; io.close
            io = IO.new(wfd.call, "w", flags: File::CREAT); r << io.write("foo"); io.close
            io = IO.new(wfd.call, 'w', autoclose: false); r << io.autoclose?; io.autoclose = true; io.close
            io = IO.new(wfd.call, "w"); fd2 = wfd.call
            io.send(:initialize, fd2, "w"); r << (io.fileno == fd2); io.close
            io = IO.for_fd(wfd.call, "w"); r << io.class; io.close
            r << IO.open(wfd.call, "w") { :blockval }
            io2 = nil; IO.open(wfd.call, "w") { |i| io2 = i }; r << io2.closed?
            r << er.call { IO.open(wfd.call, "w") { raise ArgumentError, "boom" } }
            t.close!
            r
            "##,
        );
    }

    #[test]
    fn io_pipe_encodings_subclass_block() {
        // IO.pipe: read-end encodings from Encoding/String/"ext:int"/
        // "BOM|" arguments (write end has none), block form closing both
        // ends, subclass allocation via #initialize (never `new`).
        run_test_once(
            r##"
            r = []
            IO.pipe { |a, b| r << [a.external_encoding&.to_s, a.internal_encoding&.to_s,
                                   b.external_encoding&.to_s, b.internal_encoding&.to_s] }
            IO.pipe(Encoding::ISO_8859_1) { |a, b| r << [a.external_encoding.to_s, a.internal_encoding.inspect] }
            IO.pipe("ISO-8859-1", "UTF-8") { |a, b| r << [a.external_encoding.to_s, a.internal_encoding.to_s] }
            IO.pipe("ISO-8859-1:UTF-8") { |a, b| r << [a.external_encoding.to_s, a.internal_encoding.to_s] }
            IO.pipe("BOM|UTF-8:ISO-8859-1") { |a, b| r << [a.external_encoding.to_s, a.internal_encoding.to_s] }
            IO.pipe("UTF-8", "UTF-8") { |a, b| r << a.internal_encoding.inspect }
            IO.pipe("ISO-8859-1", "UTF-8", invalid: :replace) { |a, b| r << a.external_encoding.to_s }
            mo = Object.new; mo.define_singleton_method(:to_str) { "ISO-8859-1:UTF-8" }
            IO.pipe(mo) { |a, b| r << a.external_encoding.to_s }
            opts = Object.new; opts.define_singleton_method(:to_hash) { { invalid: :replace } }
            IO.pipe("UTF-8", "ISO-8859-1", **opts) { |a, b| r << :tohash_ok }
            r << IO.pipe { :blockresult }
            a2 = b2 = nil; IO.pipe { |x, y| a2 = x; b2 = y }; r << [a2.closed?, b2.closed?]
            begin
              IO.pipe { |x, y| a2 = x; b2 = y; raise "boom" }
            rescue RuntimeError
              r << [a2.closed?, b2.closed?]
            end
            IO.pipe { |x, y| x.close; y.close; r << :inner_close_ok }
            class MrbPipeSub < IO; end
            a3, b3 = MrbPipeSub.pipe
            r << [a3.class, b3.class]; a3.close; b3.close
            $rec = []
            class MrbPipeSub2 < IO
              def self.new(...); $rec << :new; super; end
              def initialize(...); $rec << :init; super; end
            end
            a4, b4 = MrbPipeSub2.pipe
            r << $rec << [a4.class, b4.class]; a4.close; b4.close
            rd, wr = IO.pipe
            wr.puts "through"; wr.close
            r << rd.gets; rd.close
            r
            "##,
        );
    }

    #[test]
    fn hash_splat_calls_to_hash() {
        // `f(**obj)` implicitly converts obj with #to_hash (and TypeErrors
        // when absent or when the result is not a Hash).
        run_test(
            r##"
            def f(**o) = o
            def g(a, **o) = [a, o]
            m = Object.new
            def m.to_hash; { a: 1 }; end
            r = [f(**m), g(1, **m), f(**{})]
            begin; f(**Object.new); rescue TypeError => e; r << e.message; end
            bad = Object.new
            def bad.to_hash; 42; end
            begin; f(**bad); rescue TypeError => e; r << e.message; end
            r
            "##,
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

    // The `IO.popen` tests below use `run_test_once` rather than `run_test`
    // because `run_test` wraps the body in `for __i in 0..24` for JIT warmup,
    // which would spawn 25 subprocesses per test — popen exercises Rust-side
    // `posix_spawn`/`Command`, not the JIT, so the warmup loop adds no
    // coverage but multiplies subprocess pressure by 25× and drags whole-
    // suite wall time past minutes under default 8-thread parallelism on
    // macOS. CRuby comparison still runs once for output parity.

    /// `#lineno`, the encodings and binmode live in `IoInner` rather
    /// than in hidden instance variables, so the paths that build a
    /// *fresh* descriptor for an existing object — `#dup` /
    /// `#initialize_copy` and `#reopen` — have to carry them over
    /// explicitly. They used to ride along for free.
    #[test]
    fn io_per_object_state_survives_dup_and_reopen() {
        run_test_once(
            r##"
            path = "/tmp/monoruby_test_iostate_#{Process.pid}_#{rand(100000)}"
            File.write(path, "one\ntwo\nthree\n")
            r = []
            f = File.open(path, "r:UTF-8:EUC-JP")
            f.gets
            r << [f.external_encoding.to_s, f.internal_encoding.to_s, f.lineno, f.binmode?]
            g = f.dup
            r << [g.external_encoding.to_s, g.internal_encoding.to_s, g.lineno, g.binmode?]
            g.close
            f.close
            # The encodings stay readable after close (CRuby keeps them).
            r << [f.external_encoding.to_s, f.internal_encoding.to_s]
            # binmode is per object and survives a dup too.
            h = File.open(path)
            h.binmode
            r << [h.binmode?, h.external_encoding.to_s, h.dup.binmode?]
            h.close
            # reopen swaps the stream, not the object.
            i = File.open(path)
            i.gets
            i.reopen(path)
            r << i.lineno
            i.close
            # #lineno= round-trips, and none of these are visible as
            # instance variables.
            j = File.open(path)
            j.lineno = 41
            j.gets
            r << [j.lineno, j.instance_variables]
            j.close
            File.unlink(path)
            r
            "##,
        );
    }

    #[test]
        fn io_dup_ioctl_and_exact_reads() {
        // #dup wraps a dup(2)'d fd (distinct fileno, shared offset,
        // cloexec+autoclose set, IOError after close / on closed source);
        // sized reads consume the fd exactly, so syswrite lands at the
        // logical position; ioctl surfaces ENOTTY on a regular file and
        // IOError on a closed stream.
        run_test_once(
            r##"
            path = "/tmp/monoruby_test_iodup_#{Process.pid}_#{rand(100000)}"
            File.write(path, "0123456789")
            r = []
            File.open(path) do |f|
              g = f.dup
              r << [g.fileno != f.fileno, g.autoclose?, g.close_on_exec?]
              f.read(3)
              r << g.read(3)
              g.close
              r << (begin; g.read(1); rescue IOError => e; e.message; end)
            end
            io = File.open(path); io.close
            r << (begin; io.dup; rescue IOError => e; e.message; end)
            File.open(path, "r+") { |f| f.read(2); f.syswrite("XY") }
            r << File.read(path)
            File.open(path) { |f| r << (begin; f.ioctl(0x5401); rescue SystemCallError => e; e.class.to_s; end) }
            r << (begin; io.ioctl(1); rescue IOError => e; e.message; end)
            r
            "##,
        );
    }

    #[test]
    fn io_reopen_with_io() {
        // IO form: dup2 semantics — shared position, class/path
        // adoption, EOF reset, write redirection, error cases, #to_io
        // conversion, and the same-object no-op.
        run_test_once(
            r##"
            base = "/tmp/monoruby_test_reopen_#{Process.pid}_#{rand(100000)}"
            a = base + "_a"; b = base + "_b"
            File.write(a, "Line 1\nLine 2\n"); File.write(b, "Other 1\nOther 2\n")
            r = []
            # position of the other stream is adopted
            io = File.open(a); other = File.open(b)
            other.gets
            r << [io.reopen(other).equal?(io), io.gets, io.class.to_s, io.path == other.path]
            io.close; other.close
            # from the beginning when the other stream is unread; EOF resets
            io = File.open(a); other = File.open(b)
            io.read
            r << [io.eof?, io.reopen(other).eof?, io.gets]
            io.close; other.close
            # write redirection
            w1 = base + "_w1"; w2 = base + "_w2"
            io = File.open(w1, "w"); other = File.open(w2, "w")
            io.print "one"; io.reopen(other); io.print "two"; io.flush
            io.close; (other.close rescue nil)
            r << [File.read(w1), File.read(w2)]
            # closed argument / closed receiver raise IOError
            closed = File.open(a); closed.close
            io = File.open(a)
            r << (begin; io.reopen(closed); rescue IOError => e; e.message; end)
            io.close
            r << (begin; io.reopen(File.open(b)); rescue IOError => e; e.message; end)
            # to_io conversion; wrong-typed result is a TypeError
            conv = Object.new
            other = File.open(b)
            conv.define_singleton_method(:to_io) { other }
            io = File.open(a)
            r << [io.reopen(conv).class.to_s, io.gets]
            io.close; other.close
            bad = Object.new
            bad.define_singleton_method(:to_io) { "nope" }
            io = File.open(a)
            r << (begin; io.reopen(bad); rescue TypeError => e; e.message; end)
            # same-object reopen is a no-op
            r << io.reopen(io).equal?(io)
            io.close
            File.delete(a, b, w1, w2)
            r
            "##,
        );
    }

    #[test]
    fn io_reopen_with_path() {
        // Path form: freopen semantics — rewind to 0, path update,
        // mode inference from the receiver (w/a create, r -> ENOENT),
        // explicit mode flags (append) pass through, #to_path
        // coercion, closed receivers allowed, close-on-exec reset.
        run_test_once(
            r##"
            require 'fcntl'
            base = "/tmp/monoruby_test_reopenp_#{Process.pid}_#{rand(100000)}"
            a = base + "_a"; b = base + "_b"; c = base + "_c"; d = base + "_d"
            File.write(a, "Line 1\nLine 2\n"); File.write(b, "Other 1\n")
            r = []
            io = File.open(a); io.gets
            io.reopen(b)
            r << [io.gets, File.basename(io.path) == File.basename(b)]
            io.close
            # closed receiver with explicit mode reopens fine
            io = File.open(a); io.close
            io.reopen(a, "r")
            r << [io.closed?, io.gets]
            io.close
            # to_path coercion
            pth = Object.new
            pth.define_singleton_method(:to_path) { b }
            io = File.open(a); io.reopen(pth); r << io.gets; io.close
            # mode inference: write-mode receivers create the target
            io = File.open(c, "w"); io.reopen(d); r << File.exist?(d); io.close
            io = File.open(c, "a"); File.delete(d); io.reopen(d); r << File.exist?(d); io.close
            # read-mode receiver + missing file -> ENOENT
            io = File.open(a)
            r << (begin; io.reopen(base + "_missing"); rescue Errno::ENOENT => e; e.class.to_s; end)
            io.close
            # explicit "ab" passes O_APPEND through; original data intact
            io = File.open(a); io.reopen(c, "ab")
            r << ((io.fcntl(Fcntl::F_GETFL) & File::APPEND) == File::APPEND)
            io.close
            # write flushed before switching; cloexec reset to true
            e1 = base + "_e1"; e2 = base + "_e2"
            io = File.open(e1, "w"); io.print "original data"
            io.close_on_exec = false
            io.reopen(e2)
            r << io.close_on_exec?
            io.print "new data"; io.flush; io.close
            r << [File.read(e1), File.read(e2)]
            File.delete(a, b, c, d, e1, e2)
            r
            "##,
        );
    }

    #[test]
    fn io_reopen_path_modes() {
        // Explicit mode variants of the path form: "r+"/"w+"/"a+"
        // string modes, an integer-flags mode, the invalid-mode
        // ArgumentError, and mode inference from an "r+" receiver.
        run_test_once(
            r##"
            base = "/tmp/monoruby_test_reopenm_#{Process.pid}_#{rand(100000)}"
            a = base + "_a"; b = base + "_b"
            File.write(a, "aaaa"); File.write(b, "bbbb")
            r = []
            # "r+": read/write, no truncation, positioned at 0
            io = File.open(a); io.reopen(b, "r+")
            r << io.read(2); io.print "XX"; io.flush; io.close
            r << File.read(b)
            # "w+": truncates
            File.write(b, "bbbb")
            io = File.open(a); io.reopen(b, "w+")
            r << [io.read, File.read(b).bytesize]
            io.close
            # "a+": appends
            File.write(b, "bb")
            io = File.open(a); io.reopen(b, "a+")
            io.print "cc"; io.flush
            r << File.read(b)
            io.close
            # integer flags mode (WRONLY|CREAT|TRUNC == "w")
            File.write(b, "bbbb")
            io = File.open(a); io.reopen(b, File::WRONLY | File::CREAT | File::TRUNC)
            io.print "ii"; io.flush; io.close
            r << File.read(b)
            # invalid mode string
            io = File.open(a)
            r << (begin; io.reopen(b, "z"); rescue ArgumentError => e; e.class.to_s; end)
            io.close
            # mode inference from an "r+" receiver: no truncation
            File.write(b, "keep")
            io = File.open(a, "r+"); io.reopen(b)
            r << [io.read, File.read(b)]
            io.close
            File.delete(a, b)
            r
            "##,
        );
    }

    #[test]
    fn io_popen_argv_env_opts_forms() {
        // Array form [env, cmd, arg..., exec_opts], outside env hash,
        // chdir: (String and #to_path), subclass instantiation, and
        // encoding options on the read end.
        run_test_once(
            r##"
            r = []
            IO.popen([{"PV" => "pv"}, "sh", "-c", "echo $PV"]) { |io| r << io.read }
            IO.popen([{"PV" => "pv2"}, "sh", "-c", "echo $PV 1>&2", {err: [:child, :out]}], "r") { |io| r << io.read }
            IO.popen({"PV" => "pv3"}, ["sh", "-c", "echo $PV"], "r") { |io| r << io.read }
            IO.popen(["pwd"], "r", chdir: "/tmp") { |io| r << io.read }
            o = Object.new
            def o.to_path; "/tmp"; end
            IO.popen(["pwd"], chdir: o) { |io| r << io.read }
            myio = Class.new(IO)
            r << (myio.popen("echo sub") { |io| io.class == myio })
            IO.popen("echo enc", external_encoding: "iso-8859-1") { |io| r << io.read.encoding.name }
            IO.popen("echo enc2", "r:utf-8:iso-8859-1") { |io| r << [io.external_encoding.name, io.internal_encoding.name] }
            r
            "##,
        );
    }

    #[test]
    fn io_popen_read_stdout() {
        run_test_once(
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
        run_test_once(
            r#"
            IO.popen("echo world") { |io| io.read.chomp }
            "#,
        );
    }

    #[test]
    fn io_popen_block_sets_last_status() {
        run_test_once(
            r#"
            IO.popen(["true"]) { |io| io.read }
            [$?.class.to_s, $?.exitstatus]
            "#,
        );
        run_test_once(
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
        run_test_once(
            r#"
            IO.popen(["echo", "a b"]) { |io| io.read }
            "#,
        );
    }

    #[test]
    fn io_popen_stderr_to_stdout() {
        run_test_once(
            r#"
            IO.popen(["sh", "-c", "echo out; echo err 1>&2"], err: [:child, :out]) { |io| io.read }
            "#,
        );
    }

    #[test]
    fn io_popen_pid_is_integer() {
        run_test_once(
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
    fn io_new_on_existing_fd_no_double_close() {
        // `IO.new(other.fileno)` / `IO.open(other.fileno)` must not create a
        // second *closing* owner of the fd: monoruby stores fds in Rust
        // `OwnedFd`s, which abort the process on a double close. The new IO
        // borrows the fd (same fileno) and only the original owner closes it.
        // Regression: previously this aborted with "IO Safety violation".
        run_test_once(
            r#"
            r, w = IO.pipe
            io2 = IO.new(w.fileno)
            same = (io2.fileno == w.fileno)
            w.close
            r.close
            GC.start
            same
            "#,
        );
        run_test_once(
            r#"
            path = "/tmp/monoruby_io_dc_#{Process.pid}_#{rand(100000)}"
            begin
              f = File.open(path, "w")
              g = File.open(f.fileno, "w")
              same = (g.fileno == f.fileno)
              f.close
              GC.start
              same
            ensure
              File.unlink(path) rescue nil
            end
            "#,
        );
    }

    #[test]
    fn file_path_survives_close_and_nul_paths_rejected() {
        // security/cve_2018_6914 + cve_2018_8779: tempfile.rb's cleanup
        // runs `File.unlink(file.path)` on a *closed* file, so `File#path`
        // must stay readable after close (CRuby keeps it; we previously
        // returned nil, giving "no implicit conversion of NilClass").
        run_test(
            r#"
            f = File.open("Cargo.toml")
            f.close
            [f.path, f.closed?]
            "#,
        );
        // Tempfile#close! end-to-end (unlink of the closed file).
        run_test_once(
            r#"
            require "tempfile"
            t = Tempfile.new("mrb_sec")
            path = t.path
            t.close!
            [path.nil?, File.exist?(path)]
            "#,
        );
        // security/cve_2018_8780: a NUL byte anywhere in a path raises
        // ArgumentError up front (previously an opaque RuntimeError from
        // the failed CString conversion).
        run_test(
            r#"
            %w[entries foreach children].map { |m|
              begin
                Dir.send(m, "/tmp\0x")
                :no_raise
              rescue ArgumentError => e
                e.message
              end
            } + [begin; File.open("/tmp\0x"); :no_raise; rescue ArgumentError => e; e.message; end]
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
    fn io_class_readlines_foreach_encoding() {
        run_test(
            r#"
            File.write("/tmp/mr_rlf.txt", "a\nb\nc\n")
            r = []
            r << IO.readlines("/tmp/mr_rlf.txt")
            # (default-external name is locale-dependent in the sandbox
            # oracle, so only explicit encodings are asserted)
            r << IO.readlines("/tmp/mr_rlf.txt", encoding: "iso-8859-1")
                     .map { |l| l.encoding.name }.uniq
            r << IO.readlines("/tmp/mr_rlf.txt", nil)            # whole file, one elem
            r << IO.readlines("/tmp/mr_rlf.txt", 2)              # limit (ignored)
            r << IO.readlines("/tmp/mr_rlf.txt",
                              open_args: ["r:euc-jp"]).first.encoding.name
            acc = []
            IO.foreach("/tmp/mr_rlf.txt", encoding: "iso-8859-1") do |line|
              acc << [line, line.encoding.name]
            end
            r << acc
            ext = []
            IO.foreach("/tmp/mr_rlf.txt", external_encoding: "utf-8",
                       internal_encoding: "utf-16le") { |l| ext << l.encoding.name }
            r << ext.uniq
            File.unlink("/tmp/mr_rlf.txt")
            r
            "#,
        );
        // write/append-only mode (no :open_args) -> IOError.
        run_test_error(
            r#"File.write("/tmp/mr_rlf2.txt","x"); IO.readlines("/tmp/mr_rlf2.txt", mode: "w")"#,
        );
        run_test_error(
            r#"File.write("/tmp/mr_rlf3.txt","x"); IO.foreach("/tmp/mr_rlf3.txt", mode: "a") { |l| l }"#,
        );
        // missing file -> Errno::ENOENT.
        run_test_error(r#"IO.readlines("/tmp/mr_no_dir_qq/none.txt")"#);
    }

    #[test]
    fn io_class_readlines_foreach_more_paths() {
        run_test(
            r#"
            File.write("/tmp/mr_rlf4.txt", "x\ny\nz\n")
            r = []
            # foreach: whole content when separator is nil
            acc = []
            IO.foreach("/tmp/mr_rlf4.txt", nil) { |c| acc << c }
            r << acc
            # limit object: #to_int is invoked (value not enforced) and
            # the lines are still yielded -> no error, full lines.
            lim = Object.new
            def lim.to_int; 3; end
            cnt = []
            IO.foreach("/tmp/mr_rlf4.txt", "\n", lim) { |l| cnt << l }
            r << cnt
            r << IO.readlines("/tmp/mr_rlf4.txt", "\n", lim)
            # tag_with_encs: external BINARY => internal ignored.
            s = IO.readlines("/tmp/mr_rlf4.txt", mode: "rb:binary:utf-8").first
            r << s.encoding.name
            File.unlink("/tmp/mr_rlf4.txt")
            r
            "#,
        );
        // A limit object that can't convert -> TypeError.
        run_test_error(
            r#"File.write("/tmp/mr_rlf5.txt","a\n"); IO.foreach("/tmp/mr_rlf5.txt", "\n", Object.new) { |l| l }"#,
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
    fn io_pread_pwrite() {
        run_test_once(
            r#"
            path = "/tmp/monoruby_io_prw_#{Process.pid}_#{rand(100000)}"
            begin
              File.write(path, "1234567890")
              f = File.open(path, "r+")
              res = []
              res << f.pread(4, 0)          # "1234"
              res << f.pread(3, 4)          # "567"
              buf = +"foo"
              res << f.pread(5, 0, buf).equal?(buf)  # true
              res << buf                    # "12345"
              res << f.read                 # whole file (pread didn't move pos)
              res << f.pwrite("XY", 2)      # 2
              res << f.pread(10, 0)         # "12XY567890"
              res << (f.pread(0, 400) == "")  # maxlen 0 ignores offset
              f.close
              res
            ensure
              File.unlink(path) rescue nil
            end
            "#,
        );
    }

    #[test]
    fn io_pread_errors() {
        // EOFError past end-of-file.
        run_test_error(
            r#"
            path = "/tmp/monoruby_io_pread_eof_#{Process.pid}_#{rand(100000)}"
            File.write(path, "abc")
            f = File.open(path, "r")
            begin
              f.pread(1, 100)
            ensure
              f.close
              File.unlink(path) rescue nil
            end
            "#,
        );
        // Negative maxlen ⇒ ArgumentError.
        run_test_error(
            r#"
            path = "/tmp/monoruby_io_pread_neg_#{Process.pid}_#{rand(100000)}"
            File.write(path, "abc")
            f = File.open(path, "r")
            begin
              f.pread(-1, 0)
            ensure
              f.close
              File.unlink(path) rescue nil
            end
            "#,
        );
    }

    #[test]
    fn io_set_encoding_by_bom_basic() {
        run_test_once(
            r#"
            path = "/tmp/monoruby_io_bom_#{Process.pid}_#{rand(100000)}"
            begin
              File.binwrite(path, "\xEF\xBB\xBFabc")
              f = File.open(path, "rb")
              res = []
              res << f.set_encoding_by_bom.name      # "UTF-8"
              res << f.external_encoding.name        # "UTF-8"
              res << f.read                          # "abc" (BOM consumed)
              f.close
              # No BOM ⇒ nil, bytes preserved.
              File.binwrite(path, "plain")
              g = File.open(path, "rb")
              res << g.set_encoding_by_bom.inspect   # "nil"
              res << g.read(5)                       # "plain"
              g.close
              res
            ensure
              File.unlink(path) rescue nil
            end
            "#,
        );
    }

    #[test]
    fn io_set_encoding_by_bom_guards() {
        // Not in binmode ⇒ ArgumentError.
        run_test_error(
            r#"
            path = "/tmp/monoruby_io_bomg_#{Process.pid}_#{rand(100000)}"
            begin
              File.binwrite(path, "\xEF\xBB\xBF")
              f = File.open(path, "r")          # text mode
              begin; f.set_encoding_by_bom; ensure; f.close; end
            ensure
              File.unlink(path) rescue nil
            end
            "#,
        );
        // External encoding already set ⇒ ArgumentError.
        run_test_error(
            r#"
            path = "/tmp/monoruby_io_bomg2_#{Process.pid}_#{rand(100000)}"
            begin
              File.binwrite(path, "\xEF\xBB\xBF")
              f = File.open(path, "rb")
              f.set_encoding("utf-8")
              begin; f.set_encoding_by_bom; ensure; f.close; end
            ensure
              File.unlink(path) rescue nil
            end
            "#,
        );
        // Not readable ⇒ nil (write-only).
        run_test_once(
            r#"
            path = "/tmp/monoruby_io_bomg3_#{Process.pid}_#{rand(100000)}"
            begin
              f = File.open(path, "wb")
              r = f.set_encoding_by_bom
              f.close
              r.inspect
            ensure
              File.unlink(path) rescue nil
            end
            "#,
        );
    }

    #[test]
    fn io_readpartial_basic() {
        run_test_no_result_check(
            r#"
            r, w = IO.pipe
            w.write("foobar")
            # reads at most maxlen, returning available data.
            raise "1" unless r.read(1) == "f"
            raise "2" unless r.readpartial(1) == "o"
            # ungetc pushback combines with buffered data.
            c = r.getc
            r.ungetc(c)
            raise "3" unless r.readpartial(3) == "oba"
            raise "4" unless r.readpartial(3) == "r"
            # maxlen 0 clears + returns the buffer.
            buf = +"existing"
            raise "5" unless r.readpartial(0, buf).equal?(buf) && buf.empty?
            # into a buffer, then EOF raises (clearing the buffer).
            w.write("hi"); w.close
            out = +""
            raise "6" unless r.readpartial(10, out).equal?(out) && out == "hi"
            begin
              r.readpartial(5, out)
              raise "no eof"
            rescue EOFError
              raise "7" unless out.empty?
            end
            r.close
            "#,
        );
        // negative length ⇒ ArgumentError; closed ⇒ IOError.
        run_test_error(
            r#"
            r, w = IO.pipe
            begin; r.readpartial(-1); ensure; r.close; w.close; end
            "#,
        );
        run_test_error(
            r#"
            r, w = IO.pipe
            r.close; w.close
            r.readpartial(1)
            "#,
        );
        // not opened for reading ⇒ IOError.
        run_test_error(r#"$stdout.readpartial(1)"#);
    }

    #[test]
    fn io_sysread_basic() {
        run_test_once(
            r#"
            path = "/tmp/monoruby_io_sysread_#{Process.pid}_#{rand(100000)}"
            begin
              File.write(path, "012345678901234567890123456789\nabcdef")
              f = File.open(path, "r+")
              res = []
              res << f.sysread(15)            # "012345678901234"
              res << f.sysread(5)             # "56789" (position advanced)
              buf = +"ABCDE"
              res << f.sysread(6, buf).equal?(buf)  # true
              res << buf                      # bytes 20..25
              res << f.sysread(0)             # "" immediately
              f.close
              res
            ensure
              File.unlink(path) rescue nil
            end
            "#,
        );
    }

    #[test]
    fn io_sysread_eof_and_pipe() {
        // Smaller string from a pipe when fewer bytes are available.
        run_test_once(
            r#"
            r, w = IO.pipe
            w.write("ab")
            out = r.sysread(3)
            r.close; w.close
            out
            "#,
        );
        // EOFError past end of file.
        run_test_error(
            r#"
            path = "/tmp/monoruby_io_sysread_eof_#{Process.pid}_#{rand(100000)}"
            File.write(path, "abc")
            f = File.open(path, "r")
            begin
              f.seek(0, IO::SEEK_END)
              f.sysread(1)
            ensure
              f.close
              File.unlink(path) rescue nil
            end
            "#,
        );
        // Negative length ⇒ ArgumentError.
        run_test_error(
            r#"
            r, w = IO.pipe
            begin
              r.sysread(-1)
            ensure
              r.close; w.close
            end
            "#,
        );
    }

    #[test]
    fn io_sysread_popen_pushback_and_errors() {
        // Popen reader arm: read from a child's stdout (monoruby-only;
        // the popen harness output isn't comparable via run_test). The
        // Ruby code asserts the result itself.
        run_test_no_result_check(
            r#"
            io = IO.popen("printf hello")
            out = io.sysread(5)
            io.close
            raise "popen sysread: #{out.inspect}" unless out == "hello"
            "#,
        );
        // ungetc pushback is drained before the underlying read.
        run_test_no_result_check(
            r#"
            path = "/tmp/monoruby_io_sysread_pb_#{Process.pid}_#{rand(100000)}"
            begin
              File.write(path, "abcdef")
              f = File.open(path, "r")
              c = f.getc          # "a"
              f.ungetc(c)         # push "a" back
              out = f.sysread(3)  # drains pushback then reads
              f.close
              raise "pushback sysread: #{out.inspect}" unless out == "abc"
            ensure
              File.unlink(path) rescue nil
            end
            "#,
        );
        // sysread on a write-only stream ⇒ IOError (not opened for reading).
        run_test_error(
            r#"
            path = "/tmp/monoruby_io_sysread_wo_#{Process.pid}_#{rand(100000)}"
            begin
              f = File.open(path, "w")
              begin
                f.sysread(1)
              ensure
                f.close
              end
            ensure
              File.unlink(path) rescue nil
            end
            "#,
        );
        // sysread on $stdout ⇒ IOError (not opened for reading).
        run_test_error(r#"$stdout.sysread(1)"#);
        // sysread on a closed stream ⇒ IOError.
        run_test_error(
            r#"
            r, w = IO.pipe
            r.close; w.close
            r.sysread(1)
            "#,
        );
    }

    #[test]
    fn io_read_write_nonblock() {
        run_test_no_result_check(
            r#"
            r, w = IO.pipe
            res = []
            # No data: exception: false ⇒ :wait_readable.
            res << r.read_nonblock(5, exception: false)
            # No data: default ⇒ IO::WaitReadable (is_a? Errno::EAGAIN).
            begin
              r.read_nonblock(5)
            rescue IO::WaitReadable => e
              res << [:rescued, e.is_a?(Errno::EAGAIN)]
            end
            # write_nonblock returns bytes written; read sees them.
            res << w.write_nonblock("hello")
            res << r.read_nonblock(4)          # "hell"
            res << r.read_nonblock(10)         # "o"
            res << r.read_nonblock(0)          # "" immediately
            w.close
            begin
              r.read_nonblock(10)
            rescue EOFError
              res << :eof
            end
            r.close
            raise "unexpected: #{res.inspect}" unless res == [:wait_readable, [:rescued, true], 5, "hell", "o", "", :eof]
            "#,
        );
    }

    #[test]
    fn io_read_write_nonblock_buffer_pushback_full() {
        // read_nonblock with an output buffer (data + maxlen 0 + EOF arms),
        // ungetc pushback, write_nonblock to_s coercion, and the
        // would-block paths on a saturated pipe.
        run_test_no_result_check(
            r#"
            r, w = IO.pipe
            w.write("foobar")
            buf = +"xxxx"
            r.read_nonblock(3, buf)                 # fills buf with "foo"
            raise "buf #{buf.inspect}" unless buf == "foo" && r.read_nonblock(3, buf).equal?(buf) && buf == "bar"
            # maxlen 0 with buffer returns the buffer untouched.
            raise "zero" unless r.read_nonblock(0, buf).equal?(buf)
            # ungetc pushback drained by read_nonblock.
            w.write("Z")
            c = r.read_nonblock(1)                   # "Z"
            r.ungetc(c)
            raise "pushback" unless r.read_nonblock(5) == "Z"
            w.close
            # EOF with a buffer clears it then returns nil (exception: false).
            raise "eofbuf" unless r.read_nonblock(5, buf, exception: false).nil? && buf.empty?
            r.close
            "#,
        );
        // write_nonblock #to_s-coerces a non-String, and the saturated
        // pipe yields :wait_writable / IO::WaitWritable.
        run_test_no_result_check(
            r#"
            r, w = IO.pipe
            n = w.write_nonblock(12345)             # to_s ⇒ "12345"
            raise "to_s #{n}" unless n == 5
            blocked = false
            10000.times do
              break (blocked = true) if w.write_nonblock("a" * 10000, exception: false) == :wait_writable
            end
            raise "no block" unless blocked
            ok = false
            begin
              loop { w.write_nonblock("a" * 10000) }
            rescue IO::WaitWritable => e
              ok = e.is_a?(Errno::EAGAIN)
            end
            raise "no wait-writable raise" unless ok
            r.close; w.close
            "#,
        );
        // read_nonblock on a regular File exercises the seek-sync path.
        run_test_no_result_check(
            r#"
            path = "/tmp/monoruby_io_rnb_file_#{Process.pid}_#{rand(100000)}"
            begin
              File.write(path, "abcdef")
              f = File.open(path, "r")
              f.read(2)                              # buffered read
              raise "file rnb" unless f.read_nonblock(2) == "cd"
              f.close
            ensure
              File.unlink(path) rescue nil
            end
            "#,
        );
        // write_nonblock / read_nonblock on closed streams ⇒ IOError.
        run_test_error(
            r#"
            r, w = IO.pipe
            r.close; w.close
            w.write_nonblock("x")
            "#,
        );
    }

    #[test]
    fn io_nonblock_error_arms() {
        // read_nonblock on a closed stream ⇒ IOError.
        run_test_error(
            r#"
            r, w = IO.pipe
            r.close; w.close
            r.read_nonblock(1)
            "#,
        );
        // read_nonblock on a non-readable stream ($stdout) ⇒ IOError.
        run_test_error(r#"$stdout.read_nonblock(1)"#);
        // write_nonblock on a non-writable stream (pipe read end) ⇒ IOError.
        run_test_error(
            r#"
            r, w = IO.pipe
            begin
              r.write_nonblock("x")
            ensure
              r.close; w.close
            end
            "#,
        );
        // write_nonblock to a pipe whose read end is closed ⇒ Errno::EPIPE
        // (the hard-error arm; SIGPIPE is ignored by the runtime).
        run_test_error(
            r#"
            r, w = IO.pipe
            r.close
            begin
              w.write_nonblock("x")
            ensure
              w.close
            end
            "#,
        );
    }

    #[test]
    fn io_read_nonblock_negative_and_eof() {
        // Negative length ⇒ ArgumentError.
        run_test_error(
            r#"
            r, w = IO.pipe
            begin
              r.read_nonblock(-1)
            ensure
              r.close; w.close
            end
            "#,
        );
        // EOF with exception: false ⇒ nil.
        run_test_no_result_check(
            r#"
            r, w = IO.pipe
            w.write("ab"); w.close
            r.read_nonblock(5)                 # "ab"
            v = r.read_nonblock(5, exception: false)
            r.close
            raise "expected nil, got #{v.inspect}" unless v.nil?
            "#,
        );
    }

    #[test]
    fn io_nonblock_exception_kw_validation() {
        // `exception:` accepts only literal true/false — validated before
        // any IO, so it wins over EOF/closed-stream conditions.
        run_test_error(r#"r, w = IO.pipe; r.read_nonblock(4, exception: 0)"#);
        run_test_error(r#"r, w = IO.pipe; r.read_nonblock(4, exception: nil)"#);
        run_test_error(r#"r, w = IO.pipe; w.write_nonblock("x", exception: "false")"#);
    }

    #[test]
    fn io_pipe_close_on_exec_default() {
        // CRuby marks pipe fds CLOEXEC; `close_on_exec=` can clear it.
        run_test_once(
            r#"
            r, w = IO.pipe
            a = [r.close_on_exec?, w.close_on_exec?]
            w.close_on_exec = false
            a << w.close_on_exec?
            r.close; w.close
            a
            "#,
        );
    }

    #[test]
    fn io_close_is_idempotent() {
        // CRuby: closing an already-closed stream is a no-op (no IOError).
        run_test_once(
            r#"
            r, w = IO.pipe
            w.close
            w.close
            r.close
            r.close
            "ok"
            "#,
        );
    }

    #[test]
    fn io_select_reports_buffered_read_data() {
        // CRuby: an IO whose userspace read buffer holds unread bytes is
        // immediately ready in IO.select, even when the kernel fd is
        // drained (rb_io_read_pending). The expect library's
        // select-then-getc loop hangs forever without this — getc
        // buffers the whole pipe payload on the first call, and every
        // later select waits on an empty fd.
        run_test_once(
            r#"
            require "expect"
            res = []
            r, w = IO.pipe
            w << "prompt> hello"
            r.getc                       # slurps the payload into the buffer
            sel = IO.select([r], nil, nil, 0)
            res << sel.class.to_s
            res << (sel && sel[0][0].equal?(r))
            # the actual ruby/spec expect scenario, end to end
            r2, w2 = IO.pipe
            w2 << "prompt> hello"
            res << r2.expect(/[pf]rompt>/)
            # threaded variant exercises the scheduler wait path
            r3, w3 = IO.pipe
            w3 << "prompt> hello"
            t = Thread.new { sleep }
            res << r3.expect("prompt>")
            t.kill; t.join
            [r, w, r2, w2, r3, w3].each(&:close)
            res
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
    fn io_class_read_misc_arg_forms() {
        run_test(
            r#"
            File.write("/tmp/mr_cr5.txt", "abcdef")
            r = []
            # integer :mode (File::RDONLY) -> not a String, mode stays "r"
            r << IO.read("/tmp/mr_cr5.txt", mode: File::RDONLY)
            # options Hash at the 3rd positional slot (length nil, no offset)
            r << IO.read("/tmp/mr_cr5.txt", nil, mode: "r")
            # empty options Hash with a length
            r << IO.read("/tmp/mr_cr5.txt", 3, **{})
            # internal == external -> no transcode, tagged that encoding
            s = IO.read("/tmp/mr_cr5.txt", mode: "r:utf-8:utf-8")
            r << [s, s.encoding.name]
            # path object that responds to #to_path
            klass = Class.new { def initialize(p); @p = p; end; def to_path; @p; end }
            r << IO.read(klass.new("/tmp/mr_cr5.txt"))
            File.unlink("/tmp/mr_cr5.txt")
            r
            "#,
        );
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

    #[test]
    fn io_posix_open_constants() {
        // POSIX open(2) flags resolve and are integers — File/IO mode
        // forms (and StringIO#reopen) depend on these being defined.
        run_test(
            r##"
            [IO::RDONLY, IO::WRONLY, IO::RDWR,
             IO::APPEND, IO::CREAT, IO::EXCL, IO::TRUNC,
             IO::NOCTTY, IO::NONBLOCK, IO::SYNC].map { |c| c.is_a?(Integer) }.uniq
            "##,
        );
    }

    #[test]
    fn stringio_open_class_method() {
        run_test_with_prelude(
            r##"
            res = []
            ret = StringIO.open("hello") { |io| io.read }
            res << ret
            # Block-less form: returns a StringIO; caller must close.
            io = StringIO.open("world", "r")
            res << io.read
            io.close
            # Ensure-close even on raise.
            begin
              StringIO.open("x") { |io| raise "stop" }
            rescue
            end
            res
            "##,
            r##"require "stringio""##,
        );
    }

    #[test]
    fn stringio_reopen() {
        run_test_with_prelude(
            r##"
            res = []
            a = StringIO.new("orig")
            a.read(2)
            # String form resets the backing string + position.
            a.reopen("fresh")
            res << [a.pos, a.read]
            # StringIO form copies content from another StringIO.
            b = StringIO.new("other")
            a.reopen(b)
            res << [a.pos, a.read]
            res
            "##,
            r##"require "stringio""##,
        );
    }

    #[test]
    fn stringio_sysread_and_nonblock() {
        run_test_with_prelude(
            r##"
            res = []
            io = StringIO.new("abcdef")
            res << io.sysread(3)
            res << io.read_nonblock(2)
            res << io.write_nonblock("X")
            # write_nonblock returns the byte count (CRuby).
            # negative length must raise ArgumentError on both.
            begin; io.sysread(-1); rescue ArgumentError => e; res << :sys_neg_arg; end
            begin; io.read_nonblock(-1); rescue ArgumentError => e; res << :rnb_neg_arg; end
            # EOFError when reading past end of file.
            r = StringIO.new("")
            begin; r.sysread(1); rescue EOFError; res << :sys_eof; end
            begin; r.read_nonblock(1); rescue EOFError; res << :rnb_eof; end
            # exception: false at EOF -> nil instead of raising.
            res << r.read_nonblock(1, exception: false)
            res
            "##,
            r##"require "stringio""##,
        );
    }

    #[test]
    fn stringio_external_internal_encoding() {
        run_test_with_prelude(
            r##"
            io = StringIO.new("hi")
            [io.external_encoding == io.string.encoding, io.internal_encoding]
            "##,
            r##"require "stringio""##,
        );
    }

    #[test]
    fn io_wait_readable_writable() {
        run_test_once(
            r#"
            require "io/wait"
            r, w = IO.pipe
            res = []
            res << r.wait_readable(0)          # no data yet -> nil
            w.write "x"
            res << (r.wait_readable(1) == r)
            res << (w.wait_writable(1) == w)
            res << (r.wait(1, :read) == r)
            res << (r.read(0))                 # length 0 never blocks
            res << (r.read(1))
            r.close
            w.close
            res
            "#,
        );
    }


    #[test]
    fn io_puts_to_ary_protocol() {
        // A non-String object is offered #to_ary first; an empty Array
        // writes nothing (only the argument-less call writes "\n").
        run_test_once(
            r##"
            r, w = IO.pipe
            w.puts([])
            w.puts
            o = Object.new
            def o.to_ary; ["x", [1, [2]]]; end
            w.puts(o)
            n = Object.new
            def n.to_s; "S"; end
            w.puts(n)
            w.close
            r.read
            "##,
        );
    }

    #[test]
    fn io_print_field_and_record_separators() {
        run_test_once(
            r##"
            r, w = IO.pipe
            begin
              $, = "-"
              $\ = "!"
              w.print("a", "b")
              $, = nil
              $\ = nil
              o = Object.new
              def o.to_s; "OBJ"; end
              w.print(o)
            ensure
              $, = nil
              $\ = nil
            end
            w.close
            r.read
            "##,
        );
    }

    #[test]
    fn io_class_write_options() {
        run_test_once(
            r##"
            path = "/tmp/monoruby_test_iowrite_#{Process.pid}_#{rand(100000)}"
            r = []
            begin
              r << IO.write(path, "hello")
              # An offset suppresses truncation.
              r << IO.write(path, "AB", 1)
              r << File.read(path)
              r << IO.binwrite(path, "zz", 3)
              r << File.read(path)
              r << (begin; IO.write(path, "hi", mode: "r"); rescue IOError => e; e.message; end)
              r << IO.write(path, "hi", 2, mode: "r", open_args: ["w"])
              r << File.read(path).bytes
              r << (begin; IO.write(path, "hi", open_args: [{binmode: true}]); rescue IOError => e; e.message; end)
              r << IO.write(path, "x", flags: File::CREAT | File::WRONLY | File::TRUNC)
              r << IO.write(path, 123, mode: "a")
              r << File.read(path)
            ensure
              File.unlink(path) rescue nil
            end
            r
            "##,
        );
    }

    #[test]
    fn io_write_closed_pipe_epipe() {
        // SIGPIPE is ignored at startup; a write to a closed pipe
        // surfaces Errno::EPIPE instead of killing the process.
        run_test_once(
            r##"
            r, w = IO.pipe
            r.close
            begin
              w.write("x")
              :no_error
            rescue Errno::EPIPE => e
              e.class.to_s
            end
            "##,
        );
    }

    #[test]
    fn io_fcntl_and_nonblock_stub() {
        run_test_once(
            r##"
            require 'fcntl'
            require 'io/nonblock'
            res = []
            File.open("/tmp") { |f| res << (f.fcntl(Fcntl::F_GETFD) >= 0) }
            r, w = IO.pipe
            r.nonblock = false
            res << r.nonblock?
            r.nonblock(true) { res << r.nonblock? }
            res << r.nonblock?
            f = File.open("/tmp"); f.close
            res << (begin; f.fcntl(Fcntl::F_GETFL); rescue IOError => e; e.message; end)
            res
            "##,
        );
    }


    #[test]
    fn io_puts_to_ary_edge_cases() {
        // #to_ary returning a non-Array non-nil is the conversion
        // TypeError; nil falls back to #to_s; a self-referencing Array
        // renders as "[...]".
        run_test_once(
            r##"
            r, w = IO.pipe
            bad = Object.new
            def bad.to_ary; 42; end
            a = begin; w.puts(bad); rescue TypeError => e; e.message; end
            nilly = Object.new
            def nilly.to_ary; nil; end
            def nilly.to_s; "N"; end
            w.puts(nilly)
            circ = ["a"]; circ << circ
            w.puts(circ)
            w.close
            [a, r.read]
            "##,
        );
    }

    #[test]
    fn io_fcntl_error_paths() {
        // An invalid fcntl command surfaces the OS Errno; an explicit
        // nil arg is treated as 0.
        run_test_once(
            r##"
            require 'fcntl'
            File.open("/tmp") do |f|
              a = begin; f.fcntl(-1); rescue SystemCallError => e; e.class.to_s; end
              [a, f.fcntl(Fcntl::F_GETFD, nil) >= 0]
            end
            "##,
        );
    }

    #[test]
    fn io_print_writes_nil_lastline_with_rs() {
        // Argument-less print with a nil $_ still writes $\ (via
        // Kernel#print delegating the explicit lastline down).
        run_test_once(
            r##"
            r, w = IO.pipe
            begin
              $\ = "!"
              w.print
            ensure
              $\ = nil
            end
            w.close
            r.read
            "##,
        );
    }


    #[test]
    fn io_encoding_plumbing() {
        // set_encoding #to_str coercion + 3-arg rejection; read-write
        // modes report a nil external encoding until one is set.
        run_test_once(
            r##"
            path = "/tmp/monoruby_test_ioenc_#{Process.pid}_#{rand(100000)}"
            File.write(path, "abc")
            r = []
            File.open(path) do |f|
              o = Object.new
              def o.to_str; "utf-8"; end
              i = Object.new
              def i.to_str; "iso-8859-1"; end
              f.set_encoding(o, i)
              r << [f.external_encoding.name, f.internal_encoding.name]
              r << (begin; f.set_encoding("a", "b", "c"); rescue ArgumentError => e; e.message; end)
            end
            File.open(path, "r+") { |f| r << [f.external_encoding, f.internal_encoding] }
            File.open(path, "w+") { |f| r << f.external_encoding }
            File.open(path, "a+") { |f| r << f.external_encoding }
            File.open(path, "r+") { |f| f.set_encoding(nil, nil); r << f.external_encoding }
            File.unlink(path)
            r
            "##,
        );
    }

    #[test]
    fn io_read_bom_modes() {
        run_test_once(
            r##"
            path = "/tmp/monoruby_test_iobom_#{Process.pid}_#{rand(100000)}"
            r = []
            File.binwrite(path, "\xEF\xBB\xBFdata")
            r << IO.read(path, mode: "rb:BOM|utf-8")
            File.binwrite(path, "\xFF\xFEd\x00")
            r << IO.read(path, mode: "rb:BOM|utf-16le").bytes
            File.binwrite(path, "\xFE\xFF\x00d")
            r << IO.read(path, mode: "rb:BOM|utf-16be").bytes
            File.binwrite(path, "\xFF\xFE\x00\x00d\x00\x00\x00")
            r << IO.read(path, mode: "rb:BOM|utf-32le").bytes
            File.binwrite(path, "\x00\x00\xFE\xFF\x00\x00\x00d")
            r << IO.read(path, mode: "rb:BOM|utf-32be").bytes
            File.binwrite(path, "plain")
            r << IO.read(path, mode: "rb:BOM|utf-8")
            # File.open form consumes the BOM through the stream.
            File.binwrite(path, "\xEF\xBB\xBFxy")
            File.open(path, "rb:BOM|utf-8") { |f| r << f.read }
            File.binwrite(path, "\xFF\xFEd\x00")
            File.open(path, "rb:BOM|utf-16le") { |f| r << f.read.bytes }
            File.binwrite(path, "\xFE\xFF\x00d")
            File.open(path, "rb:BOM|utf-16be") { |f| r << f.read.bytes }
            File.binwrite(path, "\xFF\xFE\x00\x00d\x00\x00\x00")
            File.open(path, "rb:BOM|utf-32le") { |f| r << f.read.bytes }
            File.binwrite(path, "\x00\x00\xFE\xFF\x00\x00\x00d")
            File.open(path, "rb:BOM|utf-32be") { |f| r << f.read.bytes }
            File.binwrite(path, "nobom")
            File.open(path, "rb:BOM|utf-8") { |f| r << f.read }
            File.unlink(path)
            r
            "##,
        );
    }

    #[test]
    fn io_write_external_encoding_transcode() {
        run_test_once(
            r##"
            path = "/tmp/monoruby_test_iowt_#{Process.pid}_#{rand(100000)}"
            r = []
            File.open(path, "w", external_encoding: Encoding::UTF_16BE) { |f| r << f.write("hi") }
            r << File.binread(path).bytes
            File.open(path, "w:iso-8859-1") { |f| f.write("h\u00e9") }
            r << File.binread(path).bytes
            File.open(path, "w", external_encoding: Encoding::UTF_16BE) do |f|
              r << (begin; f.write("\xC3\xA9".b); rescue Encoding::UndefinedConversionError => e; e.class.to_s; end)
            end
            File.unlink(path)
            r
            "##,
        );
    }

    #[test]
    fn io_readlines_default_internal() {
        run_test_once(
            r##"
            path = "/tmp/monoruby_test_iodi_#{Process.pid}_#{rand(100000)}"
            File.write(path, "line\n")
            was = Encoding.default_internal
            begin
              Encoding.default_internal = Encoding::ISO_8859_1
              r = IO.readlines(path).first.encoding.name
            ensure
              Encoding.default_internal = was
            end
            File.unlink(path)
            r
            "##,
        );
    }

    /// monoruby buffers IO itself (see `value/rvalue/io/buf.rs`), so the
    /// points at which bytes actually reach the fd are observable. Each
    /// case is compared against CRuby by the harness.
    #[test]
    fn io_write_buffering_points() {
        // A buffered write is invisible until the buffer is flushed,
        // and `#flush` / `#close` both push it out.
        run_test_once(
            r##"
            path = "/tmp/mr_buf_points.txt"
            res = []
            f = File.open(path, "w")
            f.write("abc")
            res << File.size?(path)     # nil: still buffered
            f.flush
            res << File.size?(path)     # 3
            f.write("de")
            f.close
            res << File.size?(path)     # 5: close flushes
            res << File.read(path)
            File.unlink(path)
            res
            "##,
        );
        // `sync = true` writes through; `#sync` round-trips.
        run_test_once(
            r##"
            path = "/tmp/mr_buf_sync.txt"
            res = []
            f = File.open(path, "w")
            res << f.sync
            f.sync = true
            res << f.sync
            f.write("xyz")
            res << File.size?(path)     # 3: written through
            f.sync = false
            res << f.sync
            f.write("pq")
            res << File.size?(path)     # still 3
            f.close
            res << File.size?(path)
            File.unlink(path)
            res
            "##,
        );
        // A read through the same descriptor sees the pending write, and
        // so does a positional read; seeking flushes first too.
        run_test_once(
            r##"
            path = "/tmp/mr_buf_rw.txt"
            res = []
            f = File.open(path, "w+")
            f.write("hello")
            f.rewind
            res << f.read              # "hello"
            f.write("world")
            res << f.pread(10, 0)      # "helloworld"
            f.seek(0)
            res << f.read
            f.close
            File.unlink(path)
            res
            "##,
        );
        // A write bigger than the 8KiB buffer goes straight out, and the
        // content still comes back in order.
        run_test_once(
            r##"
            path = "/tmp/mr_buf_big.txt"
            f = File.open(path, "w")
            f.write("head")
            f.write("z" * 20000)
            f.write("tail")
            f.close
            s = File.read(path)
            res = [s.size, s[0, 4], s[-4, 4], s.count("z")]
            File.unlink(path)
            res
            "##,
        );
    }

    /// `#sync` defaults follow CRuby: only the process's stderr, a pipe's
    /// write end, a socket and a popen stream start synchronized.
    #[test]
    fn io_sync_defaults() {
        run_test_once(
            r##"
            r, w = IO.pipe
            res = [$stdout.sync, $stderr.sync, $stdin.sync, r.sync, w.sync]
            r.close
            w.close
            res
            "##,
        );
        // A pipe's write end is synchronized, so the reader in the same
        // process sees the bytes without an explicit flush.
        run_test_once(
            r##"
            r, w = IO.pipe
            w.write("through")
            res = r.read(7)
            r.close
            w.close
            res
            "##,
        );
        run_test_once(
            r##"
            io = IO.popen("cat", "r+")
            res = [io.sync]
            io.close
            res
            "##,
        );
        // `#sync` / `#sync=` on a closed stream raise IOError.
        run_test_error(r##"f = File.open("/tmp/mr_sync_closed.txt", "w"); f.close; f.sync"##);
        run_test_error(r##"f = File.open("/tmp/mr_sync_closed2.txt", "w"); f.close; f.sync = true"##);
    }

    /// `Kernel#p` / `#print` and `$stdout.write` share one buffer, so
    /// their output cannot be reordered against each other.
    #[test]
    fn stdout_writers_share_one_buffer() {
        run_test_once(
            r##"
            $stdout.write("a")
            print "b"
            $stdout.write("c")
            $stdout.flush
            nil
            "##,
        );
    }
}
