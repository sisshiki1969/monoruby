use super::*;
use std::sync::LazyLock;

//
// Zlib checksum backend.
//
// `Zlib` itself is the pure-Ruby stub in `stdlib/zlib.rb` (monoruby cannot
// load zlib.so). Its `Zlib.crc32` / `Zlib.adler32` used to be Ruby loops
// over `each_byte` — 50 ns a byte, against the 0.3 ns of zlib's table
// walk — and chunky_png runs a CRC over every 170 KB IDAT it writes. The
// stub keeps the argument semantics (`nil`, `to_str`, the 32-bit mask on
// the seed) and hands the byte walk to these two helpers, the same split
// as `String.__digest` for `Digest`.
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_func(STRING_CLASS, "__crc32", crc32, 2);
    globals.define_builtin_class_func(STRING_CLASS, "__adler32", adler32, 2);
    globals.define_builtin_class_func(STRING_CLASS, "__zstream_new", zstream_new, 5);
    globals.define_builtin_class_func(STRING_CLASS, "__zstream_run", zstream_run, 3);
    globals.define_builtin_class_func(STRING_CLASS, "__zstream_reset", zstream_reset, 1);
    globals.define_builtin_class_func(STRING_CLASS, "__zstream_close", zstream_close, 1);
    globals.define_builtin_class_func(STRING_CLASS, "__zstream_totals", zstream_totals, 1);
    globals.define_builtin_class_func(STRING_CLASS, "__zstream_params", zstream_params, 3);
    globals.define_builtin_class_func(STRING_CLASS, "__zstream_dictionary", zstream_dictionary, 2);
    globals.define_builtin_class_func(STRING_CLASS, "__zlib_version", zlib_version, 0);
}

//
// Native deflate / inflate streams.
//
// `Zlib::Deflate` / `Zlib::Inflate` (stdlib/zlib.rb) are thin Ruby shells
// over a `z_stream` of the bundled zlib (libz-sys, built from source and
// linked statically). Compression is therefore the real thing — the same
// algorithm, the same bytes, as the zlib CRuby's zlib.so links — which a
// PDF writer comparing output sizes against a CRuby run depends on.
//
// A stream is addressed from Ruby by an integer handle into a per-thread
// table; the Ruby object owns the handle and closes it (`close` /
// `finish`), with an `ObjectSpace` finalizer as the backstop.
//

/// One open zlib stream: the `z_stream` (boxed, so the pointer zlib keeps
/// to it stays put) and which direction it runs.
struct ZStreamEntry {
    /// Kept as `MaybeUninit`: zlib wants the allocator callbacks NULL to
    /// select its defaults, and Rust's `z_stream` declares them as
    /// non-nullable fn pointers — so the struct is never materialized as a
    /// whole; every field goes through `addr_of_mut!` on `strm()`.
    strm: Box<std::mem::MaybeUninit<libz_sys::z_stream>>,
    inflate: bool,
    /// `Z_STREAM_END` was reported.
    ended: bool,
}

impl ZStreamEntry {
    fn strm(&mut self) -> *mut libz_sys::z_stream {
        self.strm.as_mut_ptr()
    }
}

/// Read one `z_stream` field through the raw pointer (no reference to the
/// whole struct is ever formed — see `ZStreamEntry::strm`).
macro_rules! zs_get {
    ($p:expr, $field:ident) => {
        // SAFETY: `$p` points at a zlib-initialized stream owned by an entry.
        unsafe { std::ptr::addr_of!((*$p).$field).read() }
    };
}
macro_rules! zs_set {
    ($p:expr, $field:ident, $v:expr) => {
        // SAFETY: as `zs_get`.
        unsafe { std::ptr::addr_of_mut!((*$p).$field).write($v) }
    };
}

thread_local! {
    static ZSTREAMS: std::cell::RefCell<Vec<Option<ZStreamEntry>>> =
        const { std::cell::RefCell::new(Vec::new()) };
}

fn zstream_handle(v: Value, store: &Store) -> Result<usize> {
    let h = v.expect_integer(store)?;
    if h < 0 {
        return Err(MonorubyErr::argumenterr("closed stream"));
    }
    Ok(h as usize)
}

fn with_zstream<T>(
    handle: usize,
    f: impl FnOnce(&mut ZStreamEntry) -> T,
) -> Result<T> {
    ZSTREAMS.with(|t| {
        let mut t = t.borrow_mut();
        match t.get_mut(handle).and_then(|e| e.as_mut()) {
            Some(e) => Ok(f(e)),
            None => Err(MonorubyErr::argumenterr("closed stream")),
        }
    })
}

/// String.__zstream_new(inflate, level, window_bits, mem_level, strategy) -> Integer | [code, msg]
///
/// `deflateInit2` / `inflateInit2` with zlib's own argument semantics
/// (`window_bits`: 8..15 zlib wrapper, negative raw, +16 gzip, +32
/// auto-detect on inflate). Returns the handle, or a `[status, message]`
/// pair for the Ruby side to raise as the matching `Zlib::*Error`.
#[monoruby_builtin]
fn zstream_new(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let inflate = lfp.arg(0).as_bool();
    let level = lfp.arg(1).expect_integer(&globals.store)? as libc::c_int;
    let wbits = lfp.arg(2).expect_integer(&globals.store)? as libc::c_int;
    let mem_level = lfp.arg(3).expect_integer(&globals.store)? as libc::c_int;
    let strategy = lfp.arg(4).expect_integer(&globals.store)? as libc::c_int;
    // An all-zero z_stream is the documented initial state (zalloc / zfree
    // / opaque NULL select zlib's default allocator).
    let mut strm: Box<std::mem::MaybeUninit<libz_sys::z_stream>> =
        Box::new(std::mem::MaybeUninit::zeroed());
    let p = strm.as_mut_ptr();
    // SAFETY: zlibVersion has no preconditions; it returns a static string.
    let version = unsafe { libz_sys::zlibVersion() };
    let size = std::mem::size_of::<libz_sys::z_stream>() as libc::c_int;
    // SAFETY: `p` points at a zeroed z_stream that outlives the call; the
    // version / size pair is what zlib's init macros pass.
    let rc = unsafe {
        if inflate {
            libz_sys::inflateInit2_(p, wbits, version, size)
        } else {
            libz_sys::deflateInit2_(
                p,
                level,
                libz_sys::Z_DEFLATED,
                wbits,
                mem_level,
                strategy,
                version,
                size,
            )
        }
    };
    if rc != libz_sys::Z_OK {
        return Ok(zstream_status(rc, p));
    }
    let entry = ZStreamEntry {
        strm,
        inflate,
        ended: false,
    };
    let handle = ZSTREAMS.with(|t| {
        let mut t = t.borrow_mut();
        if let Some(i) = t.iter().position(|e| e.is_none()) {
            t[i] = Some(entry);
            i
        } else {
            t.push(Some(entry));
            t.len() - 1
        }
    });
    Ok(Value::integer(handle as i64))
}

/// `[status, message]` for a non-OK zlib return code (`strm.msg` when
/// zlib set one, else zlib's generic wording).
fn zstream_status(rc: libc::c_int, strm: *mut libz_sys::z_stream) -> Value {
    let msg_ptr: *mut libc::c_char = zs_get!(strm, msg);
    let msg = if msg_ptr.is_null() {
        match rc {
            libz_sys::Z_STREAM_END => "stream end",
            libz_sys::Z_NEED_DICT => "need dictionary",
            libz_sys::Z_STREAM_ERROR => "stream error",
            libz_sys::Z_DATA_ERROR => "data error",
            libz_sys::Z_MEM_ERROR => "insufficient memory",
            libz_sys::Z_BUF_ERROR => "buffer error",
            libz_sys::Z_VERSION_ERROR => "incompatible version",
            _ => "unknown error",
        }
        .to_string()
    } else {
        // SAFETY: zlib's `msg` is a NUL-terminated static string.
        unsafe { std::ffi::CStr::from_ptr(msg_ptr) }
            .to_string_lossy()
            .into_owned()
    };
    Value::array_from_vec(vec![Value::integer(rc as i64), Value::string(msg)])
}

/// String.__zstream_run(handle, input, flush) -> [status, output, consumed]
///
/// Feed `input` and collect every byte zlib produces. `status` is zlib's
/// last return code (`Z_OK`, `Z_STREAM_END`, `Z_BUF_ERROR` when the input
/// ran out with nothing more to say, or an error), `consumed` how many
/// input bytes were taken (an inflate stops at the end of a member, so the
/// caller can find the trailing bytes). For inflate, `flush` other than
/// `Z_NO_FLUSH` after the stream ended is a no-op.
#[monoruby_builtin]
fn zstream_run(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let handle = zstream_handle(lfp.arg(0), &globals.store)?;
    let input_v = lfp.arg(1);
    let input: Vec<u8> = input_v.expect_bytes(&globals.store)?.to_vec();
    let flush = lfp.arg(2).expect_integer(&globals.store)? as libc::c_int;
    let (rc, out, consumed) = with_zstream(handle, |e| {
        let mut out: Vec<u8> = Vec::with_capacity(if e.inflate {
            input.len() * 3 + 64
        } else {
            input.len() / 2 + 64
        });
        let mut chunk = vec![0u8; 64 * 1024];
        let inflate = e.inflate;
        let p = e.strm();
        zs_set!(p, next_in, input.as_ptr() as *mut u8);
        zs_set!(p, avail_in, input.len() as libz_sys::uInt);
        let mut rc;
        loop {
            zs_set!(p, next_out, chunk.as_mut_ptr());
            zs_set!(p, avail_out, chunk.len() as libz_sys::uInt);
            // SAFETY: next_in/avail_in and next_out/avail_out point at
            // live buffers for the duration of the call; the stream was
            // initialized by `zstream_new` and not yet ended.
            rc = unsafe {
                if inflate {
                    libz_sys::inflate(p, flush)
                } else {
                    libz_sys::deflate(p, flush)
                }
            };
            let avail_out: libz_sys::uInt = zs_get!(p, avail_out);
            let produced = chunk.len() - avail_out as usize;
            out.extend_from_slice(&chunk[..produced]);
            match rc {
                libz_sys::Z_OK => {
                    // Keep going while zlib filled the whole output buffer
                    // or still holds input; otherwise it has said all it
                    // can for this flush level.
                    let avail_in: libz_sys::uInt = zs_get!(p, avail_in);
                    if avail_out != 0 && avail_in == 0 {
                        break;
                    }
                }
                libz_sys::Z_STREAM_END => {
                    e.ended = true;
                    break;
                }
                libz_sys::Z_BUF_ERROR => {
                    // No progress possible: input exhausted (or, for
                    // deflate with Z_FINISH, the buffer was exactly filled
                    // — then loop once more).
                    if produced == chunk.len() {
                        continue;
                    }
                    break;
                }
                _ => break,
            }
        }
        let avail_in: libz_sys::uInt = zs_get!(p, avail_in);
        let consumed = input.len() - avail_in as usize;
        zs_set!(p, next_in, std::ptr::null_mut());
        zs_set!(p, avail_in, 0);
        zs_set!(p, next_out, std::ptr::null_mut());
        zs_set!(p, avail_out, 0);
        let status = if rc == libz_sys::Z_OK
            || rc == libz_sys::Z_STREAM_END
            || rc == libz_sys::Z_BUF_ERROR
        {
            Value::integer(rc as i64)
        } else {
            zstream_status(rc, p)
        };
        (status, out, consumed)
    })?;
    Ok(Value::array_from_vec(vec![
        rc,
        Value::bytes(out),
        Value::integer(consumed as i64),
    ]))
}

/// String.__zstream_reset(handle) -> nil
#[monoruby_builtin]
fn zstream_reset(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let handle = zstream_handle(lfp.arg(0), &globals.store)?;
    with_zstream(handle, |e| {
        let inflate = e.inflate;
        let p = e.strm();
        // SAFETY: an initialized stream owned by this entry.
        unsafe {
            if inflate {
                libz_sys::inflateReset(p);
            } else {
                libz_sys::deflateReset(p);
            }
        }
        e.ended = false;
    })?;
    Ok(Value::nil())
}

/// String.__zstream_close(handle) -> nil
///
/// `deflateEnd` / `inflateEnd` and release the handle; closing an already
/// closed handle is a no-op (the finalizer may race an explicit close).
#[monoruby_builtin]
fn zstream_close(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let h = lfp.arg(0).expect_integer(&globals.store)?;
    if h < 0 {
        return Ok(Value::nil());
    }
    ZSTREAMS.with(|t| {
        let mut t = t.borrow_mut();
        if let Some(slot) = t.get_mut(h as usize)
            && let Some(mut e) = slot.take()
        {
            let inflate = e.inflate;
            let p = e.strm();
            // SAFETY: an initialized stream owned by this entry, ended here.
            unsafe {
                if inflate {
                    libz_sys::inflateEnd(p);
                } else {
                    libz_sys::deflateEnd(p);
                }
            }
        }
    });
    Ok(Value::nil())
}

/// String.__zstream_totals(handle) -> [total_in, total_out, ended, adler, data_type]
#[monoruby_builtin]
fn zstream_totals(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let handle = zstream_handle(lfp.arg(0), &globals.store)?;
    with_zstream(handle, |e| {
        let ended = e.ended;
        let p = e.strm();
        let total_in: libz_sys::uLong = zs_get!(p, total_in);
        let total_out: libz_sys::uLong = zs_get!(p, total_out);
        let adler: libz_sys::uLong = zs_get!(p, adler);
        let data_type: libc::c_int = zs_get!(p, data_type);
        Value::array_from_vec(vec![
            Value::integer(total_in as i64),
            Value::integer(total_out as i64),
            Value::bool(ended),
            Value::integer(adler as i64),
            Value::integer(data_type as i64),
        ])
    })
}

/// String.__zstream_params(handle, level, strategy) -> [status, output]
///
/// `deflateParams`: flushes what the old settings produced (returned as
/// `output`) and switches.
#[monoruby_builtin]
fn zstream_params(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let handle = zstream_handle(lfp.arg(0), &globals.store)?;
    let level = lfp.arg(1).expect_integer(&globals.store)? as libc::c_int;
    let strategy = lfp.arg(2).expect_integer(&globals.store)? as libc::c_int;
    let (status, out) = with_zstream(handle, |e| {
        let mut out: Vec<u8> = vec![];
        let mut chunk = vec![0u8; 64 * 1024];
        let p = e.strm();
        zs_set!(p, next_in, std::ptr::null_mut());
        zs_set!(p, avail_in, 0);
        let mut rc;
        loop {
            zs_set!(p, next_out, chunk.as_mut_ptr());
            zs_set!(p, avail_out, chunk.len() as libz_sys::uInt);
            // SAFETY: as in `zstream_run`.
            rc = unsafe { libz_sys::deflateParams(p, level, strategy) };
            let avail_out: libz_sys::uInt = zs_get!(p, avail_out);
            let produced = chunk.len() - avail_out as usize;
            out.extend_from_slice(&chunk[..produced]);
            // Z_BUF_ERROR with a full buffer means "call again".
            if rc == libz_sys::Z_BUF_ERROR && produced == chunk.len() {
                continue;
            }
            break;
        }
        zs_set!(p, next_out, std::ptr::null_mut());
        zs_set!(p, avail_out, 0);
        let status = if rc == libz_sys::Z_OK || rc == libz_sys::Z_BUF_ERROR {
            Value::integer(libz_sys::Z_OK as i64)
        } else {
            zstream_status(rc, p)
        };
        (status, out)
    })?;
    Ok(Value::array_from_vec(vec![status, Value::bytes(out)]))
}

/// String.__zstream_dictionary(handle, dict) -> status
#[monoruby_builtin]
fn zstream_dictionary(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let handle = zstream_handle(lfp.arg(0), &globals.store)?;
    let dict_v = lfp.arg(1);
    let dict: Vec<u8> = dict_v.expect_bytes(&globals.store)?.to_vec();
    with_zstream(handle, |e| {
        let inflate = e.inflate;
        let p = e.strm();
        // SAFETY: `dict` outlives the call; the stream is initialized.
        let rc = unsafe {
            if inflate {
                libz_sys::inflateSetDictionary(p, dict.as_ptr(), dict.len() as libz_sys::uInt)
            } else {
                libz_sys::deflateSetDictionary(p, dict.as_ptr(), dict.len() as libz_sys::uInt)
            }
        };
        if rc == libz_sys::Z_OK {
            Value::integer(0)
        } else {
            zstream_status(rc, p)
        }
    })
}

/// String.__zlib_version -> String
#[monoruby_builtin]
fn zlib_version(_vm: &mut Executor, _globals: &mut Globals, _lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    // SAFETY: zlibVersion returns a static NUL-terminated string.
    let v = unsafe { std::ffi::CStr::from_ptr(libz_sys::zlibVersion()) };
    Ok(Value::string(v.to_string_lossy().into_owned()))
}

/// String.__crc32(data, crc) -> Integer
///
/// zlib's `crc32(crc, data)`: `data` must be a String and `crc` an
/// Integer already reduced to 32 bits.
#[monoruby_builtin]
fn crc32(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let data_v = lfp.arg(0);
    let data = data_v.expect_bytes(&globals.store)?;
    let seed = lfp.arg(1).expect_integer(&globals.store)? as u32;
    Ok(Value::integer(crc32_update(seed, data) as i64))
}

/// String.__adler32(data, adler) -> Integer
///
/// zlib's `adler32(adler, data)`, same contract as `__crc32`.
#[monoruby_builtin]
fn adler32(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let data_v = lfp.arg(0);
    let data = data_v.expect_bytes(&globals.store)?;
    let seed = lfp.arg(1).expect_integer(&globals.store)? as u32;
    Ok(Value::integer(adler32_update(seed, data) as i64))
}

/// Slicing-by-8 tables for the reflected CRC-32 (polynomial 0xEDB88320):
/// `TABLES[k][b]` is the CRC contribution of byte `b` sitting `k` bytes
/// before the end of an 8-byte word.
static CRC_TABLES: LazyLock<[[u32; 256]; 8]> = LazyLock::new(|| {
    let mut t = [[0u32; 256]; 8];
    for i in 0..256u32 {
        let mut c = i;
        for _ in 0..8 {
            c = if c & 1 == 1 { 0xEDB8_8320 ^ (c >> 1) } else { c >> 1 };
        }
        t[0][i as usize] = c;
    }
    for k in 1..8 {
        for i in 0..256 {
            let prev = t[k - 1][i];
            t[k][i] = t[0][(prev & 0xff) as usize] ^ (prev >> 8);
        }
    }
    t
});

/// `crc32(crc, buf, len)`: continue the CRC-32 `crc` over `data`.
pub(crate) fn crc32_update(crc: u32, data: &[u8]) -> u32 {
    let t = &*CRC_TABLES;
    let mut crc = !crc;
    let mut chunks = data.chunks_exact(8);
    for c in &mut chunks {
        let lo = u32::from_le_bytes([c[0], c[1], c[2], c[3]]) ^ crc;
        let hi = u32::from_le_bytes([c[4], c[5], c[6], c[7]]);
        crc = t[7][(lo & 0xff) as usize]
            ^ t[6][((lo >> 8) & 0xff) as usize]
            ^ t[5][((lo >> 16) & 0xff) as usize]
            ^ t[4][(lo >> 24) as usize]
            ^ t[3][(hi & 0xff) as usize]
            ^ t[2][((hi >> 8) & 0xff) as usize]
            ^ t[1][((hi >> 16) & 0xff) as usize]
            ^ t[0][(hi >> 24) as usize];
    }
    for &b in chunks.remainder() {
        crc = t[0][((crc ^ b as u32) & 0xff) as usize] ^ (crc >> 8);
    }
    !crc
}

/// `adler32(adler, buf, len)`: continue the Adler-32 `adler` over `data`.
pub(crate) fn adler32_update(adler: u32, data: &[u8]) -> u32 {
    const BASE: u32 = 65521;
    // The largest run of bytes whose sums stay inside a u32 (zlib's NMAX).
    const NMAX: usize = 5552;
    let mut a = adler & 0xffff;
    let mut b = (adler >> 16) & 0xffff;
    for run in data.chunks(NMAX) {
        for &byte in run {
            a += byte as u32;
            b += a;
        }
        a %= BASE;
        b %= BASE;
    }
    (b << 16) | a
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::*;

    fn crc32_naive(crc: u32, data: &[u8]) -> u32 {
        let mut c = !crc;
        for &b in data {
            c ^= b as u32;
            for _ in 0..8 {
                c = if c & 1 == 1 { 0xEDB8_8320 ^ (c >> 1) } else { c >> 1 };
            }
        }
        !c
    }

    fn adler32_naive(adler: u32, data: &[u8]) -> u32 {
        let mut a = (adler & 0xffff) as u64;
        let mut b = ((adler >> 16) & 0xffff) as u64;
        for &byte in data {
            a = (a + byte as u64) % 65521;
            b = (b + a) % 65521;
        }
        ((b as u32) << 16) | a as u32
    }

    #[test]
    fn checksums_match_the_reference_values() {
        assert_eq!(crc32_update(0, b"123456789"), 0xCBF4_3926);
        assert_eq!(crc32_update(0, b""), 0);
        assert_eq!(adler32_update(1, b"Wikipedia"), 0x11E6_0398);
        assert_eq!(adler32_update(1, b""), 1);
    }

    #[test]
    fn sliced_crc_and_chunked_adler_agree_with_the_byte_loops() {
        // Every remainder length around the 8-byte slice, a seed, and a
        // run long enough to cross the Adler NMAX boundary.
        let data: Vec<u8> = (0..20_000u32)
            .map(|i| (i.wrapping_mul(2654435761) >> 13) as u8)
            .collect();
        for len in (0..40).chain([5551, 5552, 5553, 11104, 20_000]) {
            let d = &data[..len];
            assert_eq!(crc32_update(0, d), crc32_naive(0, d), "crc len {len}");
            assert_eq!(crc32_update(0xDEAD_BEEF, d), crc32_naive(0xDEAD_BEEF, d), "crc seed len {len}");
            assert_eq!(adler32_update(1, d), adler32_naive(1, d), "adler len {len}");
            assert_eq!(adler32_update(0x1234_5678, d), adler32_naive(0x1234_5678, d), "adler seed len {len}");
        }
        // Splitting a run continues the checksum exactly.
        let (l, r) = data.split_at(777);
        assert_eq!(crc32_update(crc32_update(0, l), r), crc32_update(0, &data));
        assert_eq!(adler32_update(adler32_update(1, l), r), adler32_update(1, &data));
    }

    #[test]
    fn zlib_checksums() {
        run_tests(&[
            r#"require "zlib"; [Zlib.crc32, Zlib.adler32, Zlib.crc32(nil), Zlib.crc32(nil, 5), Zlib.adler32(nil, 5), Zlib.crc32("", 5), Zlib.adler32("", 5)]"#,
            r#"require "zlib"; [Zlib.crc32("abc"), Zlib.adler32("abc"), Zlib.crc32("abc", 2**32 - 1), Zlib.crc32("abc", 2**32), Zlib.crc32("abc", 2**40 + 7), Zlib.crc32("abc", -1), Zlib.crc32("abc", 1.5)]"#,
            r#"require "zlib"; [Zlib.crc32("あ"), Zlib.adler32("あ"), Zlib.crc32("abc", Zlib.crc32("IDAT"))]"#,
            r#"require "zlib"; o = Object.new; def o.to_str; "abc"; end; [Zlib.crc32(o), Zlib.adler32(o)]"#,
            r#"require "zlib"; s = ("x" * 7000) + (0..255).map(&:chr).join; [Zlib.crc32(s), Zlib.adler32(s), Zlib.crc32(s, Zlib.crc32(s))]"#,
            // `crc32_combine(_, crc2, 0)` only with `crc2 == 0`: zlib < 1.2.12
            // short-circuits a zero `len2` to `crc1` where 1.2.12+ still XORs
            // `crc2` in, so any other seed pair depends on the host's zlib.
            r#"require "zlib"; a = "abc" * 10; b = "defg" * 500; [Zlib.crc32_combine(Zlib.crc32(a), Zlib.crc32(b), b.bytesize) == Zlib.crc32(a + b), Zlib.adler32_combine(Zlib.adler32(a), Zlib.adler32(b), b.bytesize) == Zlib.adler32(a + b), Zlib.crc32_combine(7, 0, 0), Zlib.adler32_combine(7, 9, 0)]"#,
        ]);
        // The stub follows zlib 1.2.12+ (`crc1 ^ crc2` even for a zero
        // `len2`); pin that without consulting the host's CRuby, whose
        // linked zlib may be older and answer `crc1`.
        assert_eq!(
            run_test_no_result_check(r#"require "zlib"; Zlib.crc32_combine(7, 9, 0)"#),
            Value::integer(14)
        );
        run_test_error(r#"require "zlib"; Zlib.crc32("abc", "1")"#);
        run_test_error(r#"require "zlib"; Zlib.crc32(123)"#);
        run_test_error(r#"require "zlib"; Zlib.adler32(:abc)"#);
    }

    #[test]
    fn zlib_deflate_stored() {
        // Up to one stored block the NO_COMPRESSION output is
        // byte-identical to CRuby's. Past that the split point moved
        // between zlib 1.3 and 1.3.1 (`deflate_stored` keeps a few more
        // bytes back), and which one the host CRuby links varies, so from
        // 65530 bytes on only the framing, the trailer and the round trip
        // are compared. The other levels differ in the header's FLEVEL
        // bits alone, which is all a stored stream can carry of them.
        run_tests(&[
            r#"require "zlib"; [0, 1, 5, 100, 65529].map { |n| s = "x" * n; d = Zlib::Deflate.deflate(s, 0); [d.bytesize, d.encoding.name, d[0, 7].unpack("C*"), d[-4..].unpack("C*"), Zlib::Inflate.inflate(d) == s] }"#,
            r#"require "zlib"; [65530, 65531, 65532, 70000, 200000].map { |n| s = "x" * n; d = Zlib::Deflate.deflate(s, 0); [d.encoding.name, d[0, 2].unpack("C*"), d[-4..].unpack("C*"), Zlib::Inflate.inflate(d) == s] }"#,
            r#"require "zlib"; s = (0..255).map(&:chr).join * 3; d = Zlib::Deflate.deflate(s, Zlib::NO_COMPRESSION); [d == Zlib::Deflate.deflate(s, 0), Zlib::Inflate.inflate(d) == s.b, Zlib::Inflate.inflate(d).encoding.name]"#,
            r#"require "zlib"; [-1, 0, 1, 2, 5, 6, 7, 9, nil].map { |l| d = l.nil? ? Zlib::Deflate.deflate("abc") : Zlib::Deflate.deflate("abc", l); [d[0, 2].unpack("C*"), Zlib::Inflate.inflate(d)] }"#,
            r#"require "zlib"; d = Zlib::Deflate.new(Zlib::NO_COMPRESSION); d << "abc"; r = [d.finished?, d.total_in]; d << "def"; out = d.finish; r << d.finished? << d.total_out; d.close; r << d.closed?; [out.unpack("C*"), r]"#,
            r#"require "zlib"; d = Zlib::Deflate.new(0); out = d.deflate("hello", Zlib::FINISH); [out.unpack("C*"), Zlib::Inflate.inflate(out)]"#,
            r#"require "zlib"; o = Object.new; def o.to_str; "abc"; end; Zlib::Inflate.inflate(Zlib::Deflate.deflate(o, 0))"#,
        ]);
        run_test_error(r#"require "zlib"; Zlib::Deflate.deflate("abc", 10)"#);
        run_test_error(r#"require "zlib"; Zlib::Deflate.deflate("abc", -2)"#);
        run_test_error(r#"require "zlib"; Zlib::Deflate.deflate(nil)"#);
        run_test_error(r#"require "zlib"; Zlib::Deflate.deflate(123)"#);
        // A closed stream answers nothing but `closed?`.
        run_test_error(r#"require "zlib"; d = Zlib::Deflate.new; d.close; d.finished?"#);
        run_test_error(r#"require "zlib"; d = Zlib::Deflate.new; d.close; d << "x""#);
    }

    #[test]
    fn zlib_inflate() {
        // Streams a real zlib produced: a fixed-Huffman block, a
        // dynamic-Huffman block, and stored blocks with a multi-block
        // split. Each is decoded and compared with its plaintext.
        run_tests(&[
            r#"require "zlib"; Zlib::Inflate.inflate([120, 156, 203, 72, 205, 201, 201, 87, 200, 64, 39, 117, 20, 202, 243, 139, 114, 82, 20, 1, 184, 181, 11, 70].pack("C*"))"#,
            r#"require "zlib"; text = (1..20).map { |i| "line #{i}: #{i * i} #{(i * 7919) % 1000}\n" }.join; d = [120, 218, 45, 207, 203, 13, 67, 49, 8, 68, 209, 253, 171, 98, 74, 240, 240, 179, 113, 63, 89, 68, 122, 74, 255, 203, 96, 153, 229, 69, 8, 29, 222, 239, 239, 3, 110, 16, 201, 124, 222, 83, 178, 97, 88, 186, 110, 233, 70, 98, 250, 188, 101, 181, 25, 136, 25, 55, 125, 67, 28, 158, 126, 51, 54, 52, 224, 180, 155, 179, 14, 37, 76, 245, 230, 218, 8, 131, 186, 220, 204, 141, 69, 200, 228, 77, 142, 58, 61, 6, 152, 163, 7, 71, 37, 172, 97, 187, 88, 48, 154, 65, 90, 70, 61, 152, 68, 90, 227, 120, 116, 25, 88, 209, 60, 30, 95, 1, 231, 106, 32, 227, 128, 3, 115, 52, 145, 101, 148, 149, 8, 105, 36, 75, 169, 98, 112, 107, 38, 243, 60, 69, 88, 52, 84, 10, 106, 5, 213, 53, 158, 63, 235, 56, 74, 2].pack("C*"); [Zlib::Inflate.inflate(d) == text, Zlib::Inflate.inflate(d).encoding.name]"#,
            r#"require "zlib"; s = ("ab" * 40000) + "\x00\xff".b * 10; d = Zlib::Deflate.deflate(s, 0); i = Zlib::Inflate.new; i << d[0, 1000]; i << d[1000..]; out = i.finish; i.close; [out == s.b, out.bytesize, i.closed?]"#,
            r#"require "zlib"; Zlib::Inflate.inflate("\x78\x01\x01\x03\x00\xfc\xffabc\x02\x4d\x01\x27".b)"#,
            r#"require "zlib"; [Zlib::Inflate.inflate("\x78\x01\x03\x00\x00\x00\x00\x01".b), Zlib::Inflate.inflate(Zlib::Deflate.deflate("", 0))]"#,
        ]);
        // Bad header, bad Adler-32, truncated stream, preset dictionary.
        run_test_error(r#"require "zlib"; Zlib::Inflate.inflate("garbage")"#);
        run_test_error(r#"require "zlib"; Zlib::Inflate.inflate("\x78\x01\x01\x03\x00\xfc\xffabc\x02\x4d\x01\x28".b)"#);
        run_test_error(r#"require "zlib"; Zlib::Inflate.inflate("\x78\x01\x01\x03\x00\xfc\xffab".b)"#);
        run_test_error(r#"require "zlib"; Zlib::Inflate.inflate("\x78\x20\x01\x03\x00\xfc\xffabc\x02\x4d\x01\x27".b)"#);
        run_test_error(r#"require "zlib"; Zlib::Inflate.inflate(nil)"#);
    }
}
