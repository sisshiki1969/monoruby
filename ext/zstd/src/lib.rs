//! The zstd-ruby gem's native half (`zstd_native.so`) as a monoruby
//! extension. `Zstd` (gem/zstd-ruby/zstdruby.rb, monoruby's stand-in for
//! zstdruby.so) is a Ruby shell over the bundled libzstd (zstd-sys, built
//! from source and linked statically into this library — the same 1.5.7
//! the gem links), the same split as `Zlib` over `String.__zstream_*`.
//! Compression contexts, decompression contexts and dictionaries live in
//! per-thread handle tables; the Ruby object owns its handle and releases
//! it through an `ObjectSpace` finalizer.
//!
//! The primitives call libzstd exactly as the extension does
//! (`ZSTD_compress2` after `ZSTD_CCtx_refCDict` / `ZSTD_CCtx_loadDictionary`,
//! the same streaming loops) so that the bytes come out identical, and
//! keep its error wording ("compress error error code: …", "not
//! compressed by zstd: …").
//!
//! This is `src/builtins/zstd.rs` moved out of the interpreter
//! (doc/native_extension_loading.md, step 3).

use monoruby_ext::*;
use std::ffi::c_int;
use zstd_safe::zstd_sys as ffi;

#[unsafe(no_mangle)]
pub unsafe extern "C" fn Init_zstd_native(ctx: *mut MrContext) -> c_int {
    // SAFETY: the interpreter's contract for `Init_`.
    unsafe { init(ctx, init_zstd) }
}

fn init_zstd(ctx: &mut Ctx) -> Result<()> {
    let string = ctx.const_get(Value::UNDEF, "String").ok_or(Error)?;
    let s = MR_METHOD_SINGLETON;
    ctx.define_method(string, "__zstd_version", method!(version), 0, s);
    ctx.define_method(string, "__zstd_compress", method!(compress), 3, s);
    ctx.define_method(string, "__zstd_decompress", method!(decompress), 2, s);
    ctx.define_method(string, "__zstd_cdict_new", method!(cdict_new), 2, s);
    ctx.define_method(string, "__zstd_ddict_new", method!(ddict_new), 1, s);
    ctx.define_method(string, "__zstd_dict_free", method!(dict_free), 1, s);
    ctx.define_method(string, "__zstd_dict_id", method!(dict_id), 1, s);
    ctx.define_method(string, "__zstd_frame_dict_id", method!(frame_dict_id), 1, s);
    ctx.define_method(string, "__zstd_cstream_new", method!(cstream_new), 2, s);
    ctx.define_method(string, "__zstd_cstream_run", method!(cstream_run), 3, s);
    ctx.define_method(string, "__zstd_dstream_new", method!(dstream_new), 1, s);
    ctx.define_method(string, "__zstd_dstream_run", method!(dstream_run), 2, s);
    ctx.define_method(string, "__zstd_stream_free", method!(stream_free), 1, s);
    Ok(())
}

/// `ZSTD_CLEVEL_DEFAULT`.
const DEFAULT_LEVEL: i32 = 3;
const CONTENTSIZE_UNKNOWN: u64 = u64::MAX;
const CONTENTSIZE_ERROR: u64 = u64::MAX - 1;

/// A `ZSTD_CDict*` / `ZSTD_DDict*` owned by a table entry.
enum DictEntry {
    C(*mut ffi::ZSTD_CDict),
    D(*mut ffi::ZSTD_DDict),
}

impl Drop for DictEntry {
    fn drop(&mut self) {
        // SAFETY: the pointer came from `ZSTD_createCDict` / `ZSTD_createDDict`
        // and is freed exactly once, here.
        unsafe {
            match self {
                DictEntry::C(p) => {
                    ffi::ZSTD_freeCDict(*p);
                }
                DictEntry::D(p) => {
                    ffi::ZSTD_freeDDict(*p);
                }
            }
        }
    }
}

/// A `ZSTD_CCtx*` / `ZSTD_DCtx*` owned by a table entry.
enum StreamEntry {
    C(*mut ffi::ZSTD_CCtx),
    D(*mut ffi::ZSTD_DCtx),
}

impl Drop for StreamEntry {
    fn drop(&mut self) {
        // SAFETY: the pointer came from `ZSTD_createCCtx` / `ZSTD_createDCtx`
        // and is freed exactly once, here.
        unsafe {
            match self {
                StreamEntry::C(p) => {
                    ffi::ZSTD_freeCCtx(*p);
                }
                StreamEntry::D(p) => {
                    ffi::ZSTD_freeDCtx(*p);
                }
            }
        }
    }
}

thread_local! {
    static DICTS: std::cell::RefCell<Vec<Option<DictEntry>>> =
        const { std::cell::RefCell::new(Vec::new()) };
    static STREAMS: std::cell::RefCell<Vec<Option<StreamEntry>>> =
        const { std::cell::RefCell::new(Vec::new()) };
}

fn insert<T>(
    table: &'static std::thread::LocalKey<std::cell::RefCell<Vec<Option<T>>>>,
    entry: T,
) -> i64 {
    table.with(|t| {
        let mut t = t.borrow_mut();
        if let Some(i) = t.iter().position(|e| e.is_none()) {
            t[i] = Some(entry);
            i as i64
        } else {
            t.push(Some(entry));
            (t.len() - 1) as i64
        }
    })
}

fn handle_of(ctx: &mut Ctx, v: Value) -> Result<usize> {
    let h = ctx.int(v)?;
    if h < 0 {
        return Err(ctx.argument_error("closed handle"));
    }
    Ok(h as usize)
}

fn with_dict<T>(ctx: &mut Ctx, handle: usize, f: impl FnOnce(&DictEntry) -> T) -> Result<T> {
    let r = DICTS.with(|t| {
        let t = t.borrow();
        t.get(handle).and_then(|e| e.as_ref()).map(f)
    });
    r.ok_or_else(|| ctx.argument_error("closed dictionary"))
}

fn with_stream<T>(ctx: &mut Ctx, handle: usize, f: impl FnOnce(&StreamEntry) -> T) -> Result<T> {
    let r = STREAMS.with(|t| {
        let t = t.borrow();
        t.get(handle).and_then(|e| e.as_ref()).map(f)
    });
    r.ok_or_else(|| ctx.argument_error("closed stream"))
}

fn is_error(code: usize) -> bool {
    // SAFETY: `ZSTD_isError` has no preconditions.
    unsafe { ffi::ZSTD_isError(code) != 0 }
}

fn err_name(code: usize) -> String {
    // SAFETY: `ZSTD_getErrorName` answers a static NUL-terminated string for
    // any code.
    unsafe { std::ffi::CStr::from_ptr(ffi::ZSTD_getErrorName(code)) }
        .to_string_lossy()
        .into_owned()
}

/// The `dict:` argument as the Ruby side hands it down: nil, a CDict /
/// DDict handle (Integer) or the dictionary bytes (String).
enum DictArg {
    None,
    Handle(usize),
    Bytes(Vec<u8>),
}

fn dict_arg(ctx: &mut Ctx, v: Value) -> Result<DictArg> {
    match ctx.type_of(v) {
        MrType::Nil => Ok(DictArg::None),
        MrType::Integer => {
            let h = ctx.int(v)?;
            if h < 0 {
                return Err(ctx.argument_error("closed dictionary"));
            }
            Ok(DictArg::Handle(h as usize))
        }
        _ => Ok(DictArg::Bytes(ctx.str_vec(v)?)),
    }
}

/// An owned compression context, released on drop unless moved into the
/// stream table.
struct CCtx(*mut ffi::ZSTD_CCtx);

impl CCtx {
    fn create(ctx: &mut Ctx) -> Result<Self> {
        // SAFETY: `ZSTD_createCCtx` has no preconditions.
        let p = unsafe { ffi::ZSTD_createCCtx() };
        if p.is_null() {
            return Err(ctx.runtime_error("ZSTD_createCCtx error"));
        }
        Ok(CCtx(p))
    }

    /// `set_compress_params`: the level, then the dictionary. A referenced
    /// CDict must outlive the context; the Ruby side keeps the dictionary
    /// object (hence its table entry) alive as long as a stream that
    /// references it is open.
    fn setup(&mut self, ctx: &mut Ctx, level: i32, dict: &DictArg) -> Result<()> {
        // SAFETY: `self.0` is a live context; the dictionary pointer comes
        // from a live table entry and the byte slice outlives the call
        // (`ZSTD_CCtx_loadDictionary` copies it).
        unsafe {
            ffi::ZSTD_CCtx_setParameter(
                self.0,
                ffi::ZSTD_cParameter::ZSTD_c_compressionLevel,
                level,
            );
            match dict {
                DictArg::None => {}
                DictArg::Handle(h) => {
                    let p = with_dict(ctx, *h, |e| match e {
                        DictEntry::C(p) => Some(*p),
                        DictEntry::D(_) => None,
                    })?;
                    let Some(p) = p else {
                        return Err(ctx.runtime_error("ZSTD_CCtx_refCDict failed"));
                    };
                    if is_error(ffi::ZSTD_CCtx_refCDict(self.0, p)) {
                        return Err(ctx.runtime_error("ZSTD_CCtx_refCDict failed"));
                    }
                }
                DictArg::Bytes(b) => {
                    if is_error(ffi::ZSTD_CCtx_loadDictionary(
                        self.0,
                        b.as_ptr() as *const libc::c_void,
                        b.len(),
                    )) {
                        return Err(ctx.runtime_error("ZSTD_CCtx_loadDictionary failed"));
                    }
                }
            }
        }
        Ok(())
    }

    fn into_entry(self) -> StreamEntry {
        let p = self.0;
        std::mem::forget(self);
        StreamEntry::C(p)
    }
}

impl Drop for CCtx {
    fn drop(&mut self) {
        // SAFETY: a live context created by `create`, freed once.
        unsafe {
            ffi::ZSTD_freeCCtx(self.0);
        }
    }
}

/// An owned decompression context.
struct DCtx(*mut ffi::ZSTD_DCtx);

impl DCtx {
    fn create(ctx: &mut Ctx) -> Result<Self> {
        // SAFETY: `ZSTD_createDCtx` has no preconditions.
        let p = unsafe { ffi::ZSTD_createDCtx() };
        if p.is_null() {
            return Err(ctx.runtime_error("ZSTD_createDCtx error"));
        }
        Ok(DCtx(p))
    }

    /// `set_decompress_params`.
    fn setup(&mut self, ctx: &mut Ctx, dict: &DictArg) -> Result<()> {
        // SAFETY: as `CCtx::setup`.
        unsafe {
            match dict {
                DictArg::None => {}
                DictArg::Handle(h) => {
                    let p = with_dict(ctx, *h, |e| match e {
                        DictEntry::D(p) => Some(*p),
                        DictEntry::C(_) => None,
                    })?;
                    let Some(p) = p else {
                        return Err(ctx.runtime_error("ZSTD_DCtx_refDDict failed"));
                    };
                    if is_error(ffi::ZSTD_DCtx_refDDict(self.0, p)) {
                        return Err(ctx.runtime_error("ZSTD_DCtx_refDDict failed"));
                    }
                }
                DictArg::Bytes(b) => {
                    if is_error(ffi::ZSTD_DCtx_loadDictionary(
                        self.0,
                        b.as_ptr() as *const libc::c_void,
                        b.len(),
                    )) {
                        return Err(ctx.runtime_error("ZSTD_CCtx_loadDictionary failed"));
                    }
                }
            }
        }
        Ok(())
    }

    fn into_entry(self) -> StreamEntry {
        let p = self.0;
        std::mem::forget(self);
        StreamEntry::D(p)
    }
}

impl Drop for DCtx {
    fn drop(&mut self) {
        // SAFETY: a live context created by `create`, freed once.
        unsafe {
            ffi::ZSTD_freeDCtx(self.0);
        }
    }
}

fn cstream_out_size() -> usize {
    // SAFETY: no preconditions.
    unsafe { ffi::ZSTD_CStreamOutSize() }
}

fn dstream_out_size() -> usize {
    // SAFETY: no preconditions.
    unsafe { ffi::ZSTD_DStreamOutSize() }
}

/// `ZSTD_compressStream2` over the whole of `input` (or, with an empty
/// input, repeated until the context reports nothing left), collecting
/// the output. `Err(code)` carries the first libzstd error.
fn compress_stream(
    cctx: *mut ffi::ZSTD_CCtx,
    input: &[u8],
    end_op: ffi::ZSTD_EndDirective,
) -> std::result::Result<Vec<u8>, usize> {
    let chunk = cstream_out_size();
    let mut buf: Vec<u8> = vec![0u8; chunk];
    let mut result: Vec<u8> = Vec::new();
    let mut inb = ffi::ZSTD_inBuffer {
        src: input.as_ptr() as *const libc::c_void,
        size: input.len(),
        pos: 0,
    };
    loop {
        let mut outb = ffi::ZSTD_outBuffer {
            dst: buf.as_mut_ptr() as *mut libc::c_void,
            size: chunk,
            pos: 0,
        };
        // SAFETY: `cctx` is a live context; the buffers point at live
        // allocations of the stated sizes for the duration of the call.
        let ret = unsafe { ffi::ZSTD_compressStream2(cctx, &mut outb, &mut inb, end_op) };
        if is_error(ret) {
            return Err(ret);
        }
        result.extend_from_slice(&buf[..outb.pos]);
        if input.is_empty() {
            if ret == 0 {
                break;
            }
        } else if inb.pos >= inb.size {
            break;
        }
    }
    Ok(result)
}

/// `ZSTD_decompressStream` over the whole of `input`.
fn decompress_stream(
    dctx: *mut ffi::ZSTD_DCtx,
    input: &[u8],
) -> std::result::Result<Vec<u8>, usize> {
    let chunk = dstream_out_size();
    let mut buf: Vec<u8> = vec![0u8; chunk];
    let mut result: Vec<u8> = Vec::new();
    let mut inb = ffi::ZSTD_inBuffer {
        src: input.as_ptr() as *const libc::c_void,
        size: input.len(),
        pos: 0,
    };
    while inb.pos < inb.size {
        let mut outb = ffi::ZSTD_outBuffer {
            dst: buf.as_mut_ptr() as *mut libc::c_void,
            size: chunk,
            pos: 0,
        };
        // SAFETY: as `compress_stream`.
        let ret = unsafe { ffi::ZSTD_decompressStream(dctx, &mut outb, &mut inb) };
        if is_error(ret) {
            return Err(ret);
        }
        result.extend_from_slice(&buf[..outb.pos]);
    }
    Ok(result)
}

/// String.__zstd_version -> Integer
fn version(_: &mut Ctx, _: Value, _: &[Value], _: Block) -> Result<Value> {
    // SAFETY: no preconditions.
    Ok(Value::int(unsafe { ffi::ZSTD_versionNumber() } as i64))
}

/// String.__zstd_compress(input, level, dict) -> String
///
/// `ZSTD_compress2` into a `ZSTD_compressBound`-sized buffer.
fn compress(ctx: &mut Ctx, _: Value, args: &[Value], _: Block) -> Result<Value> {
    let level = ctx.int(args[1])? as i32;
    let dict = dict_arg(ctx, args[2])?;
    let mut cctx = CCtx::create(ctx)?;
    cctx.setup(ctx, level, &dict)?;
    let input: Vec<u8> = ctx.str_vec(args[0])?;
    // SAFETY: no preconditions.
    let bound = unsafe { ffi::ZSTD_compressBound(input.len()) };
    let mut out: Vec<u8> = vec![0u8; bound];
    // SAFETY: a live context; `out` and `input` are live allocations of
    // the stated sizes.
    let ret = unsafe {
        ffi::ZSTD_compress2(
            cctx.0,
            out.as_mut_ptr() as *mut libc::c_void,
            bound,
            input.as_ptr() as *const libc::c_void,
            input.len(),
        )
    };
    if is_error(ret) {
        return Err(ctx.runtime_error(format!("compress error error code: {}", err_name(ret))));
    }
    out.truncate(ret);
    Ok(ctx.bytes(&out))
}

/// String.__zstd_decompress(input, dict) -> String
///
/// A frame with a known content size is decompressed in one call; one
/// without is streamed out in `ZSTD_DStreamOutSize` chunks.
fn decompress(ctx: &mut Ctx, _: Value, args: &[Value], _: Block) -> Result<Value> {
    let input: Vec<u8> = ctx.str_vec(args[0])?;
    let dict = dict_arg(ctx, args[1])?;
    let mut dctx = DCtx::create(ctx)?;
    dctx.setup(ctx, &dict)?;
    // SAFETY: `input` is a live slice of the stated length.
    let size = unsafe {
        ffi::ZSTD_getFrameContentSize(input.as_ptr() as *const libc::c_void, input.len())
    };
    if size == CONTENTSIZE_ERROR {
        return Err(ctx.runtime_error(format!(
            "not compressed by zstd: {}",
            err_name(CONTENTSIZE_ERROR as usize)
        )));
    }
    if size == CONTENTSIZE_UNKNOWN {
        return match decompress_stream(dctx.0, &input) {
            Ok(out) => Ok(ctx.bytes(&out)),
            Err(code) => {
                Err(ctx.runtime_error(format!("ZSTD_decompressStream failed: {}", err_name(code))))
            }
        };
    }
    let mut out: Vec<u8> = vec![0u8; size as usize];
    // SAFETY: a live context; `out` and `input` are live allocations of
    // the stated sizes.
    let ret = unsafe {
        ffi::ZSTD_decompressDCtx(
            dctx.0,
            out.as_mut_ptr() as *mut libc::c_void,
            out.len(),
            input.as_ptr() as *const libc::c_void,
            input.len(),
        )
    };
    if is_error(ret) {
        return Err(ctx.runtime_error(format!("decompress error: {}", err_name(ret))));
    }
    out.truncate(ret);
    Ok(ctx.bytes(&out))
}

/// String.__zstd_cdict_new(dict, level) -> Integer
fn cdict_new(ctx: &mut Ctx, _: Value, args: &[Value], _: Block) -> Result<Value> {
    let bytes: Vec<u8> = ctx.str_vec(args[0])?;
    let level = if args[1].is_nil() {
        DEFAULT_LEVEL
    } else {
        ctx.int(args[1])? as i32
    };
    // SAFETY: `bytes` is a live slice of the stated length; libzstd copies it.
    let p =
        unsafe { ffi::ZSTD_createCDict(bytes.as_ptr() as *const libc::c_void, bytes.len(), level) };
    if p.is_null() {
        return Err(ctx.runtime_error("ZSTD_createCDict failed"));
    }
    Ok(Value::int(insert(&DICTS, DictEntry::C(p))))
}

/// String.__zstd_ddict_new(dict) -> Integer
fn ddict_new(ctx: &mut Ctx, _: Value, args: &[Value], _: Block) -> Result<Value> {
    let bytes: Vec<u8> = ctx.str_vec(args[0])?;
    // SAFETY: as `cdict_new`.
    let p = unsafe { ffi::ZSTD_createDDict(bytes.as_ptr() as *const libc::c_void, bytes.len()) };
    if p.is_null() {
        return Err(ctx.runtime_error("ZSTD_createDDict failed"));
    }
    Ok(Value::int(insert(&DICTS, DictEntry::D(p))))
}

/// String.__zstd_dict_free(handle) -> nil
fn dict_free(ctx: &mut Ctx, _: Value, args: &[Value], _: Block) -> Result<Value> {
    let h = ctx.int(args[0])?;
    if h >= 0 {
        DICTS.with(|t| {
            let mut t = t.borrow_mut();
            if let Some(slot) = t.get_mut(h as usize) {
                slot.take();
            }
        });
    }
    Ok(Value::nil())
}

/// String.__zstd_dict_id(handle) -> Integer (0 when the dictionary has none)
fn dict_id(ctx: &mut Ctx, _: Value, args: &[Value], _: Block) -> Result<Value> {
    let h = handle_of(ctx, args[0])?;
    // SAFETY: live dictionary pointers from the table.
    let id = with_dict(ctx, h, |e| unsafe {
        match e {
            DictEntry::C(p) => ffi::ZSTD_getDictID_fromCDict(*p),
            DictEntry::D(p) => ffi::ZSTD_getDictID_fromDDict(*p),
        }
    })?;
    Ok(Value::int(id as i64))
}

/// String.__zstd_frame_dict_id(input) -> Integer
fn frame_dict_id(ctx: &mut Ctx, _: Value, args: &[Value], _: Block) -> Result<Value> {
    let input: Vec<u8> = ctx.str_vec(args[0])?;
    // SAFETY: `input` is a live slice of the stated length.
    let id = unsafe {
        ffi::ZSTD_getDictID_fromFrame(input.as_ptr() as *const libc::c_void, input.len())
    };
    Ok(Value::int(id as i64))
}

/// String.__zstd_cstream_new(level, dict) -> Integer
fn cstream_new(ctx: &mut Ctx, _: Value, args: &[Value], _: Block) -> Result<Value> {
    let level = ctx.int(args[0])? as i32;
    let dict = dict_arg(ctx, args[1])?;
    let mut cctx = CCtx::create(ctx)?;
    cctx.setup(ctx, level, &dict)?;
    Ok(Value::int(insert(&STREAMS, cctx.into_entry())))
}

/// String.__zstd_cstream_run(handle, input, end_op) -> String
///
/// With input: `ZSTD_compressStream2(…, end_op)` until it is all consumed.
/// Without: the directive (flush / end) repeated until nothing is left in
/// the context's buffers. Answers whatever came out.
fn cstream_run(ctx: &mut Ctx, _: Value, args: &[Value], _: Block) -> Result<Value> {
    let h = handle_of(ctx, args[0])?;
    let input: Vec<u8> = ctx.str_vec(args[1])?;
    let end_op = match ctx.int(args[2])? {
        0 => ffi::ZSTD_EndDirective::ZSTD_e_continue,
        1 => ffi::ZSTD_EndDirective::ZSTD_e_flush,
        2 => ffi::ZSTD_EndDirective::ZSTD_e_end,
        _ => return Err(ctx.argument_error("invalid end directive")),
    };
    let res = with_stream(ctx, h, |e| match e {
        StreamEntry::C(p) => compress_stream(*p, &input, end_op),
        StreamEntry::D(_) => Err(0),
    })?;
    match res {
        Ok(out) => Ok(ctx.bytes(&out)),
        Err(code) => Err(ctx.runtime_error(format!(
            "{} error error code: {}",
            if input.is_empty() {
                "flush"
            } else {
                "compress"
            },
            err_name(code)
        ))),
    }
}

/// String.__zstd_dstream_new(dict) -> Integer
fn dstream_new(ctx: &mut Ctx, _: Value, args: &[Value], _: Block) -> Result<Value> {
    let dict = dict_arg(ctx, args[0])?;
    let mut dctx = DCtx::create(ctx)?;
    dctx.setup(ctx, &dict)?;
    Ok(Value::int(insert(&STREAMS, dctx.into_entry())))
}

/// String.__zstd_dstream_run(handle, input) -> String
fn dstream_run(ctx: &mut Ctx, _: Value, args: &[Value], _: Block) -> Result<Value> {
    let h = handle_of(ctx, args[0])?;
    let input: Vec<u8> = ctx.str_vec(args[1])?;
    let res = with_stream(ctx, h, |e| match e {
        StreamEntry::D(p) => decompress_stream(*p, &input),
        StreamEntry::C(_) => Err(0),
    })?;
    match res {
        Ok(out) => Ok(ctx.bytes(&out)),
        Err(code) => {
            Err(ctx.runtime_error(format!("decompress error error code: {}", err_name(code))))
        }
    }
}

/// String.__zstd_stream_free(handle) -> nil
fn stream_free(ctx: &mut Ctx, _: Value, args: &[Value], _: Block) -> Result<Value> {
    let h = ctx.int(args[0])?;
    if h >= 0 {
        STREAMS.with(|t| {
            let mut t = t.borrow_mut();
            if let Some(slot) = t.get_mut(h as usize) {
                slot.take();
            }
        });
    }
    Ok(Value::nil())
}
