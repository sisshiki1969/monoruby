//! ARGF — the virtual concatenation of the files named in `ARGV`
//! (reading stdin when `ARGV` starts out empty).
//!
//! The process-wide `ARGF` object is created here at init time and bound
//! in `Globals::argf`; its file queue is the very array behind `ARGV` /
//! `$*` (`Globals::argv`). mspec's `argf` helper builds independent
//! instances via `ARGF.class.new(*names)`, each with a queue of its own.
//!
//! The walk state lives in [`ArgfInner`] (`ObjTy::ARGF`). Line reading
//! goes straight through the `IoInner` machinery (the same path as
//! `IO#gets`), so `Kernel#gets` — which lowers onto [`argf_getline_raw`]
//! — pays no method dispatch per line; the byte/char/positioning
//! operations delegate to the current `File` / `$stdin` object by
//! ordinary method invocation.

use super::*;
use crate::value::rvalue::{ArgfInner, ArgfState, IoKind};

//
// ── class construction ────────────────────────────────────────────────
//

pub(super) fn init(globals: &mut Globals) {
    // `ARGF.class` — named for display but bound to no constant
    // (CRuby's rb_cARGF).
    let object_class = globals.store.object_class();
    let klass = globals
        .store
        .define_dotted_class("ARGF.class", Some(object_class), ObjTy::ARGF);
    let argf_class = klass.id();

    globals.define_builtin_class_func_rest(argf_class, "new", argf_new);

    globals.define_builtin_func_with_kw(argf_class, "gets", gets, 0, 2, false, &["chomp"], true);
    globals.define_builtin_func_with_kw(
        argf_class,
        "readline",
        readline,
        0,
        2,
        false,
        &["chomp"],
        true,
    );
    globals.define_builtin_funcs_with_kw(
        argf_class,
        "readlines",
        &["to_a"],
        readlines,
        0,
        2,
        false,
        &["chomp"],
        true,
    );
    globals.define_builtin_funcs_with_kw(
        argf_class,
        "each_line",
        &["each", "lines"],
        each_line,
        0,
        2,
        false,
        &["chomp"],
        true,
    );
    globals.define_builtin_func_with(argf_class, "read", read, 0, 2, false);
    globals.define_builtin_func_with(argf_class, "readpartial", readpartial, 1, 2, false);
    globals.define_builtin_func_with_kw(
        argf_class,
        "read_nonblock",
        read_nonblock,
        1,
        2,
        false,
        &["exception"],
        false,
    );
    globals.define_builtin_func(argf_class, "getc", getc, 0);
    globals.define_builtin_func(argf_class, "readchar", readchar, 0);
    globals.define_builtin_func(argf_class, "getbyte", getbyte, 0);
    globals.define_builtin_func(argf_class, "readbyte", readbyte, 0);
    globals.define_builtin_func(argf_class, "each_byte", each_byte, 0);
    globals.define_builtin_func(argf_class, "each_char", each_char, 0);
    globals.define_builtin_func(argf_class, "each_codepoint", each_codepoint, 0);

    globals.define_builtin_funcs(argf_class, "eof?", &["eof"], eof_p, 0);
    globals.define_builtin_funcs(argf_class, "pos", &["tell"], pos, 0);
    globals.define_builtin_func(argf_class, "pos=", pos_assign, 1);
    globals.define_builtin_func_with(argf_class, "seek", seek, 1, 2, false);
    globals.define_builtin_func(argf_class, "rewind", rewind, 0);
    globals.define_builtin_func(argf_class, "close", close, 0);
    globals.define_builtin_func(argf_class, "closed?", closed_p, 0);
    globals.define_builtin_func(argf_class, "skip", skip, 0);

    globals.define_builtin_func(argf_class, "argv", argv, 0);
    globals.define_builtin_funcs(argf_class, "filename", &["path"], filename, 0);
    globals.define_builtin_funcs(argf_class, "fileno", &["to_i"], fileno, 0);
    globals.define_builtin_funcs(argf_class, "file", &["to_io"], file, 0);
    globals.define_builtin_func(argf_class, "lineno", lineno, 0);
    globals.define_builtin_func(argf_class, "lineno=", lineno_assign, 1);
    globals.define_builtin_funcs(argf_class, "to_s", &["inspect"], to_s, 0);

    globals.define_builtin_func(argf_class, "binmode", binmode, 0);
    globals.define_builtin_func(argf_class, "binmode?", binmode_p, 0);
    globals.define_builtin_func_rest(argf_class, "set_encoding", set_encoding);
    globals.define_builtin_func(argf_class, "external_encoding", external_encoding, 0);
    globals.define_builtin_func(argf_class, "internal_encoding", internal_encoding, 0);
    globals.define_builtin_func(argf_class, "inplace_mode", inplace_mode, 0);
    globals.define_builtin_func(argf_class, "inplace_mode=", inplace_mode_assign, 1);

    globals.define_builtin_func_rest(argf_class, "write", write);
    globals.define_builtin_func_rest(argf_class, "print", print);
    globals.define_builtin_func_rest(argf_class, "printf", printf);
    globals.define_builtin_func_rest(argf_class, "puts", puts);

    // The one process-wide instance, sharing the `ARGV` array as its
    // queue. Bound in `Globals::argf` so `Kernel#gets` and the
    // `$<` / `$FILENAME` hooks reach it without a constant lookup.
    let argf = Value::new_argf(argf_class, ArgfInner::new(globals.argv()));
    globals.set_constant_by_str(OBJECT_CLASS, "ARGF", argf);
    globals.argf = Some(argf);
}

#[monoruby_builtin]
fn argf_new(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let class_id = lfp.self_val().as_class_id();
    let args = lfp.arg(0).as_array();
    let queue = if args.is_empty() {
        globals.argv()
    } else {
        Value::array_from_iter(args.iter().cloned())
    };
    Ok(Value::new_argf(class_id, ArgfInner::new(queue)))
}

//
// ── inner-state plumbing ──────────────────────────────────────────────
//

/// The receiver's [`ArgfInner`], or a TypeError for a non-ARGF receiver
/// (e.g. a bare `ARGF.class.allocate`).
fn expect_inner(mut v: Value) -> Result<&'static mut ArgfInner> {
    // SAFETY: ArgfInner lives in the RValue heap cell; the reference is
    // valid for the duration of the builtin call (no GC compaction).
    match v.try_argf_inner_mut() {
        Some(inner) => Ok(unsafe { &mut *(inner as *mut ArgfInner) }),
        None => Err(MonorubyErr::typeerr("not an initialized ARGF object")),
    }
}

/// Whether `v` is the stdin IO object (never closed by ARGF).
fn is_stdin(v: Value) -> bool {
    v.ty() == Some(ObjTy::IO) && matches!(v.as_io_inner().kind(), IoKind::Stdin)
}

fn stdin_value(globals: &mut Globals) -> Value {
    globals
        .get_gvar(IdentId::get_id("$stdin"))
        .unwrap_or_default()
}

/// Shift the next file name off the queue as a (String-ish) Value.
fn shift_queue(argf: Value) -> Result<Option<Value>> {
    let inner = expect_inner(argf)?;
    let Some(mut ary) = inner.argv.try_array_ty() else {
        return Ok(None);
    };
    if ary.is_empty() {
        return Ok(None);
    }
    let first = ary[0];
    ary.remove(0);
    Ok(Some(first))
}

/// Open `name` for reading via `File.open` (so Errno errors, encoding
/// defaults and fd bookkeeping match `File` exactly).
fn open_file(vm: &mut Executor, globals: &mut Globals, name: Value) -> Result<Value> {
    let file_class = globals.store.get_module(FILE_CLASS).get();
    vm.invoke_method_inner(
        globals,
        IdentId::get_id("open"),
        file_class,
        &[name],
        None,
        None,
    )
}

fn invoke0(vm: &mut Executor, globals: &mut Globals, recv: Value, name: &str) -> Result<Value> {
    vm.invoke_method_inner(globals, IdentId::get_id(name), recv, &[], None, None)
}

/// The stream to read from next, advancing through the queue as needed.
/// `None` once every input is exhausted. Does not skip past a stream at
/// EOF — callers do that with [`finish_stream`] when a read comes back
/// empty (so `ARGF.eof?` can still see the just-drained file).
fn force_stream(vm: &mut Executor, globals: &mut Globals, argf: Value) -> Result<Option<Value>> {
    let inner = expect_inner(argf)?;
    if let Some(cur) = inner.current {
        return Ok(Some(cur));
    }
    if inner.state == ArgfState::Done {
        return Ok(None);
    }
    loop {
        match shift_queue(argf)? {
            None => {
                let inner = expect_inner(argf)?;
                if inner.state == ArgfState::Init {
                    // ARGV was empty from the start: bind stdin.
                    inner.state = ArgfState::Reading;
                    inner.filename = Some(Value::string_from_str("-"));
                    inner.file_start_lineno = inner.lineno;
                    let stdin = stdin_value(globals);
                    inner.current = Some(stdin);
                    return Ok(Some(stdin));
                }
                // Named files were consumed earlier; no more input.
                inner.state = ArgfState::Done;
                inplace_finish(vm, globals, argf)?;
                return Ok(None);
            }
            Some(name) => {
                let is_dash = name.is_str().is_some_and(|s| s == "-");
                let stream = if is_dash {
                    stdin_value(globals)
                } else {
                    let f = open_file(vm, globals, name)?;
                    inplace_redirect(vm, globals, argf, name)?;
                    f
                };
                let inner = expect_inner(argf)?;
                inner.state = ArgfState::Reading;
                inner.filename = Some(if is_dash {
                    Value::string_from_str("-")
                } else {
                    name
                });
                inner.file_start_lineno = inner.lineno;
                inner.cur_encs = None;
                inner.current = Some(stream);
                // Re-apply remembered `set_encoding` / `binmode` to the
                // fresh stream (CRuby keeps them across file switches).
                let enc_args = inner.enc_args.clone();
                let bin = inner.binmode;
                if !enc_args.is_empty() {
                    vm.invoke_method_inner(
                        globals,
                        IdentId::get_id("set_encoding"),
                        stream,
                        &enc_args,
                        None,
                        None,
                    )?;
                }
                if bin {
                    invoke0(vm, globals, stream, "binmode")?;
                }
                return Ok(Some(stream));
            }
        }
    }
}

/// Retire the current stream (it hit EOF, or `skip`/`close` was called):
/// close it unless it is stdin, and remember it so `ARGF.file` keeps
/// answering after the walk moved on.
fn finish_stream(vm: &mut Executor, globals: &mut Globals, argf: Value) -> Result<()> {
    let inner = expect_inner(argf)?;
    let Some(cur) = inner.current.take() else {
        return Ok(());
    };
    inner.cur_encs = None;
    inner.last_current = Some(cur);
    if !is_stdin(cur) {
        // A double close is harmless; other errors propagate.
        let _ = invoke0(vm, globals, cur, "close");
    }
    Ok(())
}

//
// ── in-place edit (-i / ARGF.inplace_mode=) ───────────────────────────
//

/// Entering file `name` with in-place mode on: move the original aside
/// (rename to `name + ext`, or unlink for a bare `-i`) and point
/// `$stdout` at a fresh `name` so `print`/`puts` rewrite the file. The
/// read stream was opened *before* this runs, so it keeps feeding the
/// original bytes.
fn inplace_redirect(
    vm: &mut Executor,
    globals: &mut Globals,
    argf: Value,
    name: Value,
) -> Result<()> {
    let inner = expect_inner(argf)?;
    let Some(ext) = inner.inplace.clone() else {
        return Ok(());
    };
    let stdout_id = IdentId::get_id("$stdout");
    let cur_stdout = globals.get_gvar(stdout_id).unwrap_or_default();
    match inner.inplace_saved_stdout {
        None => inner.inplace_saved_stdout = Some(cur_stdout),
        // Moving on from a previous in-place file: close its output.
        Some(saved) if saved != cur_stdout => {
            let _ = invoke0(vm, globals, cur_stdout, "close");
        }
        Some(_) => {}
    }
    let path = name.is_str().unwrap_or_default().to_string();
    if ext.is_empty() {
        let _ = std::fs::remove_file(&path);
    } else {
        std::fs::rename(&path, format!("{path}{ext}")).map_err(|e| {
            MonorubyErr::runtimeerr(format!("in-place edit: cannot rename {path}: {e}"))
        })?;
    }
    let file_class = globals.store.get_module(FILE_CLASS).get();
    let out = vm.invoke_method_inner(
        globals,
        IdentId::get_id("open"),
        file_class,
        &[name, Value::string_from_str("w")],
        None,
        None,
    )?;
    globals.set_gvar(stdout_id, out);
    Ok(())
}

/// The walk finished: close the last in-place output and put the real
/// `$stdout` back.
fn inplace_finish(vm: &mut Executor, globals: &mut Globals, argf: Value) -> Result<()> {
    let inner = expect_inner(argf)?;
    let Some(saved) = inner.inplace_saved_stdout.take() else {
        return Ok(());
    };
    let stdout_id = IdentId::get_id("$stdout");
    let cur_stdout = globals.get_gvar(stdout_id).unwrap_or_default();
    if cur_stdout != saved {
        let _ = invoke0(vm, globals, cur_stdout, "close");
        globals.set_gvar(stdout_id, saved);
    }
    Ok(())
}

//
// ── line reading ──────────────────────────────────────────────────────
//

/// One record from the ARGF walk: the engine behind `ARGF#gets`,
/// `#readline`, `#each_line` and `Kernel#gets`. Advances `ARGF.lineno`
/// and `$.`, sets `$_`, and rolls over to the next file when the
/// current one is drained.
pub(crate) fn argf_getline_raw(
    vm: &mut Executor,
    globals: &mut Globals,
    argf: Value,
    sep: Option<&[u8]>,
    limit: Option<usize>,
    chomp: bool,
) -> Result<Option<Value>> {
    loop {
        let Some(stream) = force_stream(vm, globals, argf)? else {
            vm.set_last_read_line(Value::nil());
            return Ok(None);
        };
        let (ext, intl) = match expect_inner(argf)?.cur_encs {
            Some(pair) => pair,
            None => {
                let pair = super::io::io_encodings(globals, stream);
                expect_inner(argf)?.cur_encs = Some(pair);
                pair
            }
        };
        let complete_utf8 = ext == crate::value::Encoding::Utf8;
        let line = super::io::blocking_io_region(vm, globals, stream, libc::POLLIN, |_store| {
            let mut s = stream;
            s.as_io_inner_mut().getline(sep, limit, complete_utf8)
        })?;
        match line {
            Some(mut buf) => {
                if chomp {
                    super::io::chomp_line(&mut buf, sep, limit);
                }
                let inner = expect_inner(argf)?;
                inner.lineno += 1;
                let n = inner.lineno;
                globals.set_lineno(Value::integer(n));
                let s = super::io::tagged_read_string_with(globals, buf, ext, intl);
                vm.set_last_read_line(s);
                return Ok(Some(s));
            }
            None => {
                finish_stream(vm, globals, argf)?;
            }
        }
    }
}

fn getline_from_lfp(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
) -> Result<Option<Value>> {
    let (sep, limit, chomp) = super::io::getline_args(vm, globals, lfp, 2)?;
    argf_getline_raw(vm, globals, lfp.self_val(), sep.as_deref(), limit, chomp)
}

#[monoruby_builtin]
fn gets(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(getline_from_lfp(vm, globals, lfp)?.unwrap_or_default())
}

#[monoruby_builtin]
fn readline(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    getline_from_lfp(vm, globals, lfp)?
        .ok_or_else(|| MonorubyErr::eoferr(&globals.store, "end of file reached"))
}

#[monoruby_builtin]
fn readlines(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let (sep, limit, chomp) = super::io::getline_args(vm, globals, lfp, 2)?;
    let argf = lfp.self_val();
    let mut lines = vec![];
    while let Some(line) = argf_getline_raw(vm, globals, argf, sep.as_deref(), limit, chomp)? {
        lines.push(line);
    }
    Ok(Value::array_from_vec(lines))
}

#[monoruby_builtin]
fn each_line(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, pc: BytecodePtr) -> Result<Value> {
    let argf = lfp.self_val();
    let Some(bh) = lfp.block() else {
        let mut args = vec![];
        if let Some(a) = lfp.try_arg(0) {
            args.push(a);
        }
        if let Some(a) = lfp.try_arg(1) {
            args.push(a);
        }
        return vm.generate_enumerator(IdentId::get_id("each_line"), argf, args, pc);
    };
    let (sep, limit, chomp) = super::io::getline_args(vm, globals, lfp, 2)?;
    let data = vm.get_block_data(globals, bh)?;
    while let Some(line) = argf_getline_raw(vm, globals, argf, sep.as_deref(), limit, chomp)? {
        vm.invoke_block(globals, &data, &[line])?;
    }
    Ok(argf)
}

//
// ── bulk / byte / char reading ────────────────────────────────────────
//

/// `ARGF.read` (verified against CRuby 4.0.2):
/// - `read`           → concatenation of every remaining stream; `""`
///                      for empty input; `nil` once exhausted.
/// - `read(len)`      → up to `len` bytes, continuing across file
///                      boundaries; `nil` at EOF; `""` for `len == 0`.
/// - `read(len, buf)` → fills and returns `buf`.
#[monoruby_builtin]
fn read(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let argf = lfp.self_val();
    let length = match lfp.try_arg(0) {
        None => None,
        Some(v) if v.is_nil() => None,
        Some(v) => {
            let l = v.coerce_to_int_i64(vm, globals)?;
            if l < 0 {
                return Err(MonorubyErr::argumenterr(format!(
                    "negative length {l} given"
                )));
            }
            Some(l as usize)
        }
    };
    let outbuf = lfp.try_arg(1).filter(|v| !v.is_nil());
    let mut collected: Vec<u8> = vec![];
    let mut enc: Option<(crate::value::Encoding, Option<crate::value::Encoding>)> = None;
    let mut had_stream = false;
    let mut read_any = false;
    if length != Some(0) {
        loop {
            if let Some(l) = length
                && collected.len() >= l
            {
                break;
            }
            let Some(stream) = force_stream(vm, globals, argf)? else {
                break;
            };
            had_stream = true;
            if enc.is_none() {
                enc = Some(super::io::io_encodings(globals, stream));
            }
            let need = length.map(|l| l - collected.len());
            let chunk = match need {
                Some(n) => vm.invoke_method_inner(
                    globals,
                    IdentId::get_id("read"),
                    stream,
                    &[Value::integer(n as i64)],
                    None,
                    None,
                )?,
                None => invoke0(vm, globals, stream, "read")?,
            };
            let mut chunk_len = 0;
            if let Some(s) = chunk.is_rstring_inner() {
                let bytes = s.as_bytes();
                chunk_len = bytes.len();
                if chunk_len > 0 {
                    read_any = true;
                    collected.extend_from_slice(bytes);
                }
            }
            // A short (or nil) chunk means this stream hit EOF; an
            // unbounded read always consumes the stream.
            match need {
                None => finish_stream(vm, globals, argf)?,
                Some(n) if chunk.is_nil() || chunk_len < n => {
                    finish_stream(vm, globals, argf)?
                }
                Some(_) => {}
            }
        }
    }
    let result = if length.is_none() {
        had_stream.then_some(collected)
    } else if length == Some(0) {
        Some(vec![])
    } else {
        read_any.then_some(collected)
    };
    let strval = result.map(|bytes| match (length, enc) {
        // An unbounded read is text in the stream's encoding; a bounded
        // read is binary, like `IO#read(len)`.
        (None, Some((ext, intl))) => {
            super::io::tagged_read_string_with(globals, bytes, ext, intl)
        }
        (None, None) => {
            let (ext, intl) = default_encodings(globals);
            super::io::tagged_read_string_with(globals, bytes, ext, intl)
        }
        _ => Value::bytes(bytes),
    });
    finish_read(vm, globals, strval, outbuf)
}

/// The (external, internal) encoding pair `Encoding.default_external` /
/// `default_internal` currently name.
fn default_encodings(
    globals: &mut Globals,
) -> (crate::value::Encoding, Option<crate::value::Encoding>) {
    use crate::value::Encoding as E;
    let ext_v = super::io::enc_default_external_obj(globals);
    let ext = super::io::enc_obj_to_enum(globals, ext_v).unwrap_or(E::Utf8);
    (ext, None)
}

/// Whether `e` is an `EOFError` (raised by the delegated stream read).
fn is_eof_error(globals: &Globals, e: &MonorubyErr) -> bool {
    let eof_cid = globals
        .store
        .get_constant_noautoload(OBJECT_CLASS, IdentId::get_id("EOFError"))
        .and_then(|v| v.is_class_or_module())
        .map(|m| m.id());
    matches!(&e.kind, MonorubyErrKind::Other(c) if Some(*c) == eof_cid)
}

/// Apply the `outbuf` protocol shared by `read`/`readpartial`/
/// `read_nonblock`: the buffer is always cleared, filled on success,
/// and returned in place of the fresh string.
fn finish_read(
    vm: &mut Executor,
    globals: &mut Globals,
    result: Option<Value>,
    outbuf: Option<Value>,
) -> Result<Value> {
    let Some(v) = outbuf else {
        return Ok(result.unwrap_or_default());
    };
    let mut out = v.coerce_to_rstring(vm, globals)?.as_val();
    out.ensure_string_mutable(vm, globals)?;
    match result.and_then(|r| r.is_rstring_inner().map(|s| (*s).clone())) {
        Some(inner) => {
            *out.as_rstring_inner_mut() = inner;
            Ok(out)
        }
        None => {
            let enc = out.as_rstring_inner().encoding();
            *out.as_rstring_inner_mut() = RStringInner::from_encoding(b"", enc);
            Ok(Value::nil())
        }
    }
}

/// Shared engine for `readpartial` / `read_nonblock`: read from the
/// current stream; at EOF return `""` when more files remain, raise
/// `EOFError` at the true end (CRuby-verified).
fn partial_read(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    method: &str,
    kw: Option<(IdentId, Value)>,
) -> Result<Value> {
    let argf = lfp.self_val();
    let maxlen = lfp.arg(0);
    let outbuf = lfp.try_arg(1).filter(|v| !v.is_nil());
    let Some(stream) = force_stream(vm, globals, argf)? else {
        // Clear the buffer even when raising (spec-mandated).
        finish_read(vm, globals, None, outbuf)?;
        return Err(MonorubyErr::eoferr(&globals.store, "end of file reached"));
    };
    let kw_args = match kw {
        Some((k, v)) => {
            let mut map = RubyMap::default();
            map.insert(Value::symbol(k), v, vm, globals)
                .map_err(|e| e)?;
            Some(Hashmap::new(Value::hash(map)))
        }
        None => None,
    };
    let res = vm.invoke_method_inner(
        globals,
        IdentId::get_id(method),
        stream,
        &[maxlen],
        None,
        kw_args,
    );
    match res {
        Ok(v) => {
            // Zero bytes at EOF (some IO impls return "" rather than
            // raising): treat like EOFError below.
            finish_read(vm, globals, Some(v), outbuf)
        }
        Err(e) if is_eof_error(globals, &e) => {
            finish_stream(vm, globals, argf)?;
            let inner = expect_inner(argf)?;
            let more = inner
                .argv
                .try_array_ty()
                .is_some_and(|a| !a.is_empty())
                || inner.state == ArgfState::Init;
            if more {
                finish_read(vm, globals, Some(Value::string_from_str("")), outbuf)
            } else {
                expect_inner(argf)?.state = ArgfState::Done;
                finish_read(vm, globals, None, outbuf)?;
                Err(e)
            }
        }
        Err(e) => Err(e),
    }
}

#[monoruby_builtin]
fn readpartial(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    partial_read(vm, globals, lfp, "readpartial", None)
}

#[monoruby_builtin]
fn read_nonblock(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let kw = lfp
        .try_arg(2)
        .map(|v| (IdentId::get_id("exception"), v));
    partial_read(vm, globals, lfp, "read_nonblock", kw)
}

/// One unit (a char or a byte, per `method`) from the walk, rolling
/// over to the next file on EOF. `None` at the true end.
fn next_unit(
    vm: &mut Executor,
    globals: &mut Globals,
    argf: Value,
    method: &str,
) -> Result<Option<Value>> {
    loop {
        let Some(stream) = force_stream(vm, globals, argf)? else {
            return Ok(None);
        };
        let v = invoke0(vm, globals, stream, method)?;
        if v.is_nil() {
            finish_stream(vm, globals, argf)?;
            continue;
        }
        return Ok(Some(v));
    }
}

#[monoruby_builtin]
fn getc(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(next_unit(vm, globals, lfp.self_val(), "getc")?.unwrap_or_default())
}

#[monoruby_builtin]
fn readchar(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    next_unit(vm, globals, lfp.self_val(), "getc")?
        .ok_or_else(|| MonorubyErr::eoferr(&globals.store, "end of file reached"))
}

#[monoruby_builtin]
fn getbyte(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(next_unit(vm, globals, lfp.self_val(), "getbyte")?.unwrap_or_default())
}

#[monoruby_builtin]
fn readbyte(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    next_unit(vm, globals, lfp.self_val(), "getbyte")?
        .ok_or_else(|| MonorubyErr::eoferr(&globals.store, "end of file reached"))
}

fn each_unit(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    pc: BytecodePtr,
    method_name: &str,
    unit: &str,
    codepoint: bool,
) -> Result<Value> {
    let argf = lfp.self_val();
    let Some(bh) = lfp.block() else {
        return vm.generate_enumerator(IdentId::get_id(method_name), argf, vec![], pc);
    };
    let data = vm.get_block_data(globals, bh)?;
    while let Some(v) = next_unit(vm, globals, argf, unit)? {
        let v = if codepoint {
            invoke0(vm, globals, v, "ord")?
        } else {
            v
        };
        vm.invoke_block(globals, &data, &[v])?;
    }
    Ok(argf)
}

#[monoruby_builtin]
fn each_byte(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, pc: BytecodePtr) -> Result<Value> {
    each_unit(vm, globals, lfp, pc, "each_byte", "getbyte", false)
}

#[monoruby_builtin]
fn each_char(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, pc: BytecodePtr) -> Result<Value> {
    each_unit(vm, globals, lfp, pc, "each_char", "getc", false)
}

#[monoruby_builtin]
fn each_codepoint(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    pc: BytecodePtr,
) -> Result<Value> {
    each_unit(vm, globals, lfp, pc, "each_codepoint", "getc", true)
}

//
// ── stream state / positioning ────────────────────────────────────────
//

/// The current stream for a positioning operation, or the CRuby
/// `ArgumentError` naming `what` once the walk is over.
fn stream_or_argument_error(
    vm: &mut Executor,
    globals: &mut Globals,
    argf: Value,
    what: &str,
) -> Result<Value> {
    if let Some(stream) = force_stream(vm, globals, argf)? {
        Ok(stream)
    } else {
        Err(MonorubyErr::argumenterr(format!("no stream{what}")))
    }
}

#[monoruby_builtin]
fn eof_p(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let argf = lfp.self_val();
    let Some(stream) = force_stream(vm, globals, argf)? else {
        return Err(MonorubyErr::ioerr("closed stream"));
    };
    invoke0(vm, globals, stream, "eof?")
}

#[monoruby_builtin]
fn pos(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let stream = stream_or_argument_error(vm, globals, lfp.self_val(), " to tell")?;
    invoke0(vm, globals, stream, "pos")
}

#[monoruby_builtin]
fn pos_assign(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let stream = stream_or_argument_error(vm, globals, lfp.self_val(), " to set position")?;
    vm.invoke_method_inner(
        globals,
        IdentId::get_id("pos="),
        stream,
        &[lfp.arg(0)],
        None,
        None,
    )
}

#[monoruby_builtin]
fn seek(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let stream = stream_or_argument_error(vm, globals, lfp.self_val(), " to seek")?;
    let mut args = vec![lfp.arg(0)];
    if let Some(w) = lfp.try_arg(1) {
        args.push(w);
    }
    vm.invoke_method_inner(globals, IdentId::get_id("seek"), stream, &args, None, None)
}

#[monoruby_builtin]
fn rewind(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let argf = lfp.self_val();
    let stream = stream_or_argument_error(vm, globals, argf, " to rewind")?;
    let res = invoke0(vm, globals, stream, "rewind")?;
    // Positioned back to the top of the current file: the line counter
    // returns to what it was when this file became current.
    let inner = expect_inner(argf)?;
    inner.lineno = inner.file_start_lineno;
    let n = inner.lineno;
    globals.set_lineno(Value::integer(n));
    Ok(res)
}

#[monoruby_builtin]
fn close(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let argf = lfp.self_val();
    let inner = expect_inner(argf)?;
    // Closing never touches stdin (CRuby-verified) and never raises on
    // an already-finished walk.
    if let Some(cur) = inner.current
        && !is_stdin(cur)
    {
        finish_stream(vm, globals, argf)?;
    }
    Ok(argf)
}

#[monoruby_builtin]
fn closed_p(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    // Forces the walk like CRuby's next_argv: with pending input,
    // `closed?` answers for the stream about to be read (so
    // `ARGV = ['-']; ARGF.close; ARGF.closed?` is false — stdin stays
    // open); only a finished walk reads as closed.
    match force_stream(vm, globals, lfp.self_val())? {
        Some(cur) => invoke0(vm, globals, cur, "closed?"),
        None => Ok(Value::bool(true)),
    }
}

#[monoruby_builtin]
fn skip(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let argf = lfp.self_val();
    finish_stream(vm, globals, argf)?;
    Ok(argf)
}

//
// ── identity / bookkeeping ────────────────────────────────────────────
//

#[monoruby_builtin]
fn argv(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(expect_inner(lfp.self_val())?.argv)
}

/// The logic behind both `ARGF.filename` and `$FILENAME`: the current
/// (or, once finished, the last) file's name; the next queued name
/// before the walk opens it; `"-"` for stdin / no input.
pub(crate) fn filename_of(argf: Value) -> Value {
    let Ok(inner) = expect_inner(argf) else {
        return Value::string_from_str("-");
    };
    if let Some(name) = inner.filename {
        return name;
    }
    if let Some(ary) = inner.argv.try_array_ty()
        && !ary.is_empty()
    {
        return ary[0];
    }
    Value::string_from_str("-")
}

#[monoruby_builtin]
fn filename(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(filename_of(lfp.self_val()))
}

#[monoruby_builtin]
fn fileno(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let stream = stream_or_argument_error(vm, globals, lfp.self_val(), "")?;
    invoke0(vm, globals, stream, "fileno")
}

#[monoruby_builtin]
fn file(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let argf = lfp.self_val();
    if let Some(stream) = force_stream(vm, globals, argf)? {
        return Ok(stream);
    }
    // After the walk: the last stream (CRuby keeps answering with it).
    let inner = expect_inner(argf)?;
    Ok(match inner.last_current {
        Some(v) => v,
        None => stdin_value(globals),
    })
}

#[monoruby_builtin]
fn lineno(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::integer(expect_inner(lfp.self_val())?.lineno))
}

#[monoruby_builtin]
fn lineno_assign(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let n = lfp.arg(0).coerce_to_int_i64(vm, globals)?;
    let inner = expect_inner(lfp.self_val())?;
    inner.lineno = n;
    globals.set_lineno(Value::integer(n));
    Ok(lfp.arg(0))
}

#[monoruby_builtin]
fn to_s(_vm: &mut Executor, _globals: &mut Globals, _lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::string_from_str("ARGF"))
}

//
// ── encodings / modes ─────────────────────────────────────────────────
//

#[monoruby_builtin]
fn binmode(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let argf = lfp.self_val();
    let inner = expect_inner(argf)?;
    inner.binmode = true;
    inner.cur_encs = None;
    // Applies to the file being read right now, too (CRuby-verified).
    if let Some(cur) = inner.current {
        invoke0(vm, globals, cur, "binmode")?;
    }
    Ok(argf)
}

#[monoruby_builtin]
fn binmode_p(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::bool(expect_inner(lfp.self_val())?.binmode))
}

#[monoruby_builtin]
fn set_encoding(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let argf = lfp.self_val();
    let args: Vec<Value> = lfp.arg(0).as_array().iter().cloned().collect();
    let inner = expect_inner(argf)?;
    inner.enc_args = args.clone();
    inner.cur_encs = None;
    if let Some(cur) = inner.current {
        vm.invoke_method_inner(
            globals,
            IdentId::get_id("set_encoding"),
            cur,
            &args,
            None,
            None,
        )?;
    }
    Ok(argf)
}

#[monoruby_builtin]
fn external_encoding(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let inner = expect_inner(lfp.self_val())?;
    match inner.current {
        Some(cur) => invoke0(vm, globals, cur, "external_encoding"),
        None => Ok(super::io::enc_default_external_obj(globals)),
    }
}

#[monoruby_builtin]
fn internal_encoding(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let inner = expect_inner(lfp.self_val())?;
    match inner.current {
        Some(cur) => invoke0(vm, globals, cur, "internal_encoding"),
        // No stream open yet. CRuby answers with the *pending* internal
        // encoding: the one a prior `set_encoding(ext, int)` recorded,
        // or — a CRuby quirk — the default external object.
        None => {
            if let Some(int_arg) = inner.enc_args.get(1).copied() {
                let enc_class = globals
                    .store
                    .get_constant_noautoload(OBJECT_CLASS, IdentId::get_id("Encoding"))
                    .unwrap_or_default();
                return vm.invoke_method_inner(
                    globals,
                    IdentId::get_id("find"),
                    enc_class,
                    &[int_arg],
                    None,
                    None,
                );
            }
            Ok(super::io::enc_default_external_obj(globals))
        }
    }
}

#[monoruby_builtin]
fn inplace_mode(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    Ok(match &expect_inner(lfp.self_val())?.inplace {
        Some(ext) => Value::string(ext.clone()),
        None => Value::nil(),
    })
}

#[monoruby_builtin]
fn inplace_mode_assign(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let arg = lfp.arg(0);
    let inner = expect_inner(lfp.self_val())?;
    if arg.is_nil() {
        inner.inplace = None;
    } else if let Some(s) = arg.is_str() {
        inner.inplace = Some(s.to_string());
    } else {
        return Err(MonorubyErr::no_implicit_conversion(
            &globals.store,
            arg,
            STRING_CLASS,
        ));
    }
    Ok(arg)
}

//
// ── writing (in-place edit output) ────────────────────────────────────
//

/// `ARGF`'s write family targets `$stdout` — which in-place edit mode
/// points at the file being rewritten.
fn stdout_delegate(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    method: &str,
) -> Result<Value> {
    let out = globals
        .get_gvar(IdentId::get_id("$stdout"))
        .unwrap_or_default();
    let args: Vec<Value> = lfp.arg(0).as_array().iter().cloned().collect();
    vm.invoke_method_inner(globals, IdentId::get_id(method), out, &args, None, None)
}

#[monoruby_builtin]
fn write(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    stdout_delegate(vm, globals, lfp, "write")
}

#[monoruby_builtin]
fn print(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    stdout_delegate(vm, globals, lfp, "print")
}

#[monoruby_builtin]
fn printf(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    stdout_delegate(vm, globals, lfp, "printf")
}

#[monoruby_builtin]
fn puts(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    stdout_delegate(vm, globals, lfp, "puts")
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    /// A fresh two-file walk for each expression: `f1` holds "A1\nA2\n",
    /// `f2` holds "B1\nB2\n", and `a` is `ARGF.class.new(f1, f2)`.
    fn with_two_files(body: &str) -> String {
        format!(
            r#"
            require 'tmpdir'
            f1 = File.join(Dir.tmpdir, "argf_t1_#{{Process.pid}}_#{{rand(100000)}}.txt")
            f2 = File.join(Dir.tmpdir, "argf_t2_#{{Process.pid}}_#{{rand(100000)}}.txt")
            File.write(f1, "A1\nA2\n"); File.write(f2, "B1\nB2\n")
            begin
              a = ARGF.class.new(f1, f2)
              {body}
            ensure
              File.unlink(f1, f2) rescue nil
            end
            "#
        )
    }

    #[test]
    fn argf_gets_family() {
        run_test_once(&with_two_files(
            r#"
            r = []
            r << a.gets
            r << a.gets("2\n")
            r << a.gets(nil)
            r << a.gets
            b = ARGF.class.new(f1, f2)
            r << b.gets(3)
            r << b.gets("1", 1)
            r << b.gets(chomp: true)
            r << ARGF.class.new(f1).read(2).encoding.to_s
            c = ARGF.class.new(f1)
            r << c.readline
            c.read
            r << (begin; c.readline; rescue EOFError => e; e.message; end)
            r
            "#,
        ));
    }

    #[test]
    fn argf_readlines_each_line() {
        run_test_once(&with_two_files(
            r#"
            r = []
            r << a.readlines
            b = ARGF.class.new(f1, f2)
            r << b.readlines("1\n", chomp: true)
            c = ARGF.class.new(f1, f2)
            acc = []
            ret = c.each_line { |l| acc << l }
            r << acc
            r << ret.equal?(c)
            d = ARGF.class.new(f1)
            e = d.each_line
            r << e.class.to_s
            r << e.to_a
            r << ARGF.class.new(f1).to_a
            r << ARGF.class.new(f1).each_line("1\n").to_a
            r
            "#,
        ));
    }

    #[test]
    fn argf_read_partial_nonblock() {
        run_test_once(&with_two_files(
            r#"
            r = []
            r << a.read(2)
            r << a.read
            b = ARGF.class.new(f1, f2)
            r << b.read(7)          # spans the file boundary
            r << b.read
            r << b.read             # exhausted -> nil
            r << ARGF.class.new(f1).read(0)
            buf = +"zz"
            r << [ARGF.class.new(f1).read(3, buf), buf]
            r << (begin; ARGF.class.new(f1).read(-1); rescue ArgumentError => e; e.message; end)
            c = ARGF.class.new(f1, f2)
            r << c.readpartial(100)
            r << c.readpartial(100) # EOF of f1, more files -> ""
            r << c.readpartial(100)
            r << (begin; c.readpartial(100); rescue EOFError => e; e.message; end)
            d = ARGF.class.new(f1)
            r << d.read_nonblock(2)
            rbuf = +"ww"
            r << [d.read_nonblock(100, rbuf), rbuf]
            r << (begin; d.read_nonblock(4); rescue EOFError => e; e.message; end)
            r
            "#,
        ));
    }

    #[test]
    fn argf_char_byte_readers() {
        run_test_once(&with_two_files(
            r#"
            r = []
            r << a.getc
            r << a.getbyte
            r << a.readchar
            r << a.readbyte
            b = ARGF.class.new(f1)
            b.read
            r << b.getc
            r << b.getbyte
            r << (begin; b.readchar; rescue EOFError => e; e.message; end)
            c = ARGF.class.new(f1, f2)
            bytes = []; c.each_byte { |x| bytes << x }
            r << bytes
            d = ARGF.class.new(f1)
            chars = []; d.each_char { |x| chars << x }
            r << chars
            e = ARGF.class.new(f1)
            cps = []; e.each_codepoint { |x| cps << x }
            r << cps
            r << ARGF.class.new(f1).each_byte.first(3)
            r << ARGF.class.new(f1).each_char.class.to_s
            r
            "#,
        ));
    }

    #[test]
    fn argf_positioning() {
        run_test_once(&with_two_files(
            r#"
            r = []
            r << a.pos
            r << a.read(2)
            r << a.tell
            a.pos = 0
            r << a.gets
            a.seek(3)
            r << a.gets
            a.seek(0, IO::SEEK_END)
            r << a.gets           # rolls to f2
            a.rewind
            r << a.gets
            b = ARGF.class.new(f1)
            b.read
            r << (begin; b.pos; rescue ArgumentError => e; e.message; end)
            r << (begin; b.seek(0); rescue ArgumentError => e; e.message; end)
            r << (begin; b.rewind; rescue ArgumentError => e; e.message; end)
            r << (begin; b.fileno; rescue ArgumentError => e; e.message; end)
            r << (begin; b.eof?; rescue IOError => e; e.message; end)
            r
            "#,
        ));
    }

    #[test]
    fn argf_lineno_rewind_and_state() {
        run_test_once(&with_two_files(
            r#"
            r = []
            a.gets
            r << a.lineno
            a.rewind
            r << a.lineno
            a.lineno = 7
            r << a.lineno
            a.gets
            r << a.lineno
            b = ARGF.class.new(f1, f2)
            r << b.eof?
            b.gets; b.gets
            r << b.eof?
            b.gets
            r << b.eof?
            c = ARGF.class.new(f1, f2)
            io = c.to_io
            r << io.class.to_s
            c.close
            r << io.closed?
            r << c.close.equal?(c)
            d = ARGF.class.new(f1, f2)
            r << d.getc
            d.skip
            r << d.getc
            r
            "#,
        ));
    }

    #[test]
    fn argf_identity_and_filename() {
        run_test_once(&with_two_files(
            r#"
            r = []
            r << (a.filename == f1)
            r << (a.path == f1)
            r << (a.gets; a.filename == f1)
            2.times { a.gets }
            r << (a.filename == f2)
            r << a.argv
            r << a.to_s
            r << a.inspect
            r << (a.fileno.is_a?(Integer))
            r << (a.file.is_a?(File))
            r << ARGF.class.new(f1).to_i.is_a?(Integer)
            r
            "#,
        ));
    }

    #[test]
    fn argf_encodings_and_modes() {
        run_test_once(&with_two_files(
            r#"
            r = []
            r << a.binmode?
            a.binmode
            r << a.binmode?
            r << a.gets.encoding.to_s
            b = ARGF.class.new(f1, f2)
            b.set_encoding("US-ASCII")
            r << b.gets.encoding.to_s
            b.gets; b.gets            # roll into f2
            r << b.gets.encoding.to_s # remembered across files
            c = ARGF.class.new(f1)
            r << (c.external_encoding == Encoding.default_external)
            r << (c.internal_encoding == Encoding.default_external)
            c.gets
            r << c.internal_encoding
            ie = ARGF.class.new(f1)
            ie.set_encoding("UTF-8", "UTF-16")
            r << ie.internal_encoding.to_s
            r << c.inplace_mode
            c.inplace_mode = ".bak"
            r << c.inplace_mode
            c.inplace_mode = nil
            r << c.inplace_mode
            r << (begin; c.inplace_mode = 1; rescue TypeError => e; e.message; end)
            r
            "#,
        ));
    }

    #[test]
    fn file_read_and_readlines_args() {
        run_test_once(
            r#"
            require 'tmpdir'
            f = File.join(Dir.tmpdir, "file_read_args_#{Process.pid}.txt")
            File.write(f, "para1a\npara1b\n\npara2\n")
            begin
              r = []
              r << File.read(f)
              r << File.read(f, 4)
              r << File.read(f, 4).encoding.to_s
              r << File.read(f, 4, 2)
              r << File.read(f, nil, 7)
              r << File.read(f, 1000)
              r << File.read(f, 4, 1000)
              r << File.read(f, encoding: "US-ASCII").encoding.to_s
              r << File.readlines(f)
              r << File.readlines(f, "a\n")
              r << File.readlines(f, nil)
              r << File.readlines(f, chomp: true)
              r << File.readlines(f, "")            # paragraph mode
              r << File.readlines(f, "1", 2)
              r
            ensure
              File.unlink(f) rescue nil
            end
            "#,
        );
    }
}
