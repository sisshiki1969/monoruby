//! Zlib's native half (`zlib_native.so`) as a monoruby extension. `Zlib`
//! itself is the pure-Ruby `stdlib/zlib.rb`; this library provides the
//! primitives it runs on, registered as singleton methods of `String`:
//!
//! - `String.__crc32` / `String.__adler32` — the byte walks behind
//!   `Zlib.crc32` / `Zlib.adler32` (a Ruby loop over `each_byte` was 50 ns
//!   a byte against the 0.3 ns of a table walk, and chunky_png runs a CRC
//!   over every 170 KB IDAT it writes).
//! - `String.__zstream_*` — `Zlib::Deflate` / `Zlib::Inflate` over a
//!   `z_stream` of the bundled zlib (libz-sys, built from source and
//!   linked statically into this library). Compression is therefore the
//!   real thing — the same algorithm, the same bytes, as the zlib CRuby's
//!   zlib.so links — which a PDF writer comparing output sizes against a
//!   CRuby run depends on.
//!
//! A stream is addressed from Ruby by an integer handle into a per-thread
//! table; the Ruby object owns the handle and closes it (`close` /
//! `finish`), with an `ObjectSpace` finalizer as the backstop.
//!
//! This is `src/builtins/zlib.rs` moved out of the interpreter
//! (doc/native_extension_loading.md, step 3).

use monoruby_ext::*;
use std::ffi::c_int;
use std::sync::LazyLock;

#[unsafe(no_mangle)]
pub unsafe extern "C" fn Init_zlib_native(ctx: *mut MrContext) -> c_int {
    // SAFETY: the interpreter's contract for `Init_`.
    unsafe { init(ctx, init_zlib) }
}

fn init_zlib(ctx: &mut Ctx) -> Result<()> {
    let string = ctx.const_get(Value::UNDEF, "String").ok_or(Error)?;
    let s = MR_METHOD_SINGLETON;
    ctx.define_method(string, "__crc32", method!(crc32), 2, s);
    ctx.define_method(string, "__adler32", method!(adler32), 2, s);
    ctx.define_method(string, "__zstream_new", method!(zstream_new), 5, s);
    ctx.define_method(string, "__zstream_run", method!(zstream_run), 3, s);
    ctx.define_method(string, "__zstream_reset", method!(zstream_reset), 1, s);
    ctx.define_method(string, "__zstream_close", method!(zstream_close), 1, s);
    ctx.define_method(string, "__zstream_totals", method!(zstream_totals), 1, s);
    ctx.define_method(string, "__zstream_params", method!(zstream_params), 3, s);
    ctx.define_method(
        string,
        "__zstream_dictionary",
        method!(zstream_dictionary),
        2,
        s,
    );
    ctx.define_method(string, "__zlib_version", method!(zlib_version), 0, s);
    Ok(())
}

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

fn zstream_handle(ctx: &mut Ctx, v: Value) -> Result<usize> {
    let h = ctx.int(v)?;
    if h < 0 {
        return Err(ctx.argument_error("closed stream"));
    }
    Ok(h as usize)
}

fn with_zstream<T>(
    ctx: &mut Ctx,
    handle: usize,
    f: impl FnOnce(&mut ZStreamEntry) -> T,
) -> Result<T> {
    let r = ZSTREAMS.with(|t| {
        let mut t = t.borrow_mut();
        t.get_mut(handle).and_then(|e| e.as_mut()).map(f)
    });
    r.ok_or_else(|| ctx.argument_error("closed stream"))
}

/// String.__zstream_new(inflate, level, window_bits, mem_level, strategy) -> Integer | [code, msg]
///
/// `deflateInit2` / `inflateInit2` with zlib's own argument semantics
/// (`window_bits`: 8..15 zlib wrapper, negative raw, +16 gzip, +32
/// auto-detect on inflate). Returns the handle, or a `[status, message]`
/// pair for the Ruby side to raise as the matching `Zlib::*Error`.
fn zstream_new(ctx: &mut Ctx, _: Value, args: &[Value], _: Block) -> Result<Value> {
    let inflate = args[0].truthy();
    let level = ctx.int(args[1])? as c_int;
    let wbits = ctx.int(args[2])? as c_int;
    let mem_level = ctx.int(args[3])? as c_int;
    let strategy = ctx.int(args[4])? as c_int;
    // An all-zero z_stream is the documented initial state (zalloc / zfree
    // / opaque NULL select zlib's default allocator).
    let mut strm: Box<std::mem::MaybeUninit<libz_sys::z_stream>> =
        Box::new(std::mem::MaybeUninit::zeroed());
    let p = strm.as_mut_ptr();
    // SAFETY: zlibVersion has no preconditions; it returns a static string.
    let version = unsafe { libz_sys::zlibVersion() };
    let size = std::mem::size_of::<libz_sys::z_stream>() as c_int;
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
        return Ok(zstream_status(ctx, rc, p));
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
    Ok(Value::int(handle as i64))
}

/// `[status, message]` for a non-OK zlib return code (`strm.msg` when
/// zlib set one, else zlib's generic wording).
fn zstream_status(ctx: &Ctx, rc: c_int, strm: *mut libz_sys::z_stream) -> Value {
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
    let m = ctx.str(msg);
    ctx.ary_from(&[Value::int(rc as i64), m])
}

/// String.__zstream_run(handle, input, flush) -> [status, output, consumed]
///
/// Feed `input` and collect every byte zlib produces. `status` is zlib's
/// last return code (`Z_OK`, `Z_STREAM_END`, `Z_BUF_ERROR` when the input
/// ran out with nothing more to say, or an error), `consumed` how many
/// input bytes were taken (an inflate stops at the end of a member, so the
/// caller can find the trailing bytes). For inflate, `flush` other than
/// `Z_NO_FLUSH` after the stream ended is a no-op.
fn zstream_run(ctx: &mut Ctx, _: Value, args: &[Value], _: Block) -> Result<Value> {
    let handle = zstream_handle(ctx, args[0])?;
    let input: Vec<u8> = ctx.str_vec(args[1])?;
    let flush = ctx.int(args[2])? as c_int;
    let (rc, out, consumed, err) = with_zstream(ctx, handle, |e| {
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
        // The status Value is built outside the table borrow: a non-OK
        // code needs the stream's message, read here.
        let err = if rc == libz_sys::Z_OK
            || rc == libz_sys::Z_STREAM_END
            || rc == libz_sys::Z_BUF_ERROR
        {
            None
        } else {
            Some(p)
        };
        (rc, out, consumed, err)
    })?;
    let status = match err {
        None => Value::int(rc as i64),
        Some(p) => zstream_status(ctx, rc, p),
    };
    let out = ctx.bytes(&out);
    Ok(ctx.ary_from(&[status, out, Value::int(consumed as i64)]))
}

/// String.__zstream_reset(handle) -> nil
fn zstream_reset(ctx: &mut Ctx, _: Value, args: &[Value], _: Block) -> Result<Value> {
    let handle = zstream_handle(ctx, args[0])?;
    with_zstream(ctx, handle, |e| {
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
fn zstream_close(ctx: &mut Ctx, _: Value, args: &[Value], _: Block) -> Result<Value> {
    let h = ctx.int(args[0])?;
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
fn zstream_totals(ctx: &mut Ctx, _: Value, args: &[Value], _: Block) -> Result<Value> {
    let handle = zstream_handle(ctx, args[0])?;
    let (total_in, total_out, ended, adler, data_type) = with_zstream(ctx, handle, |e| {
        let ended = e.ended;
        let p = e.strm();
        let total_in: libz_sys::uLong = zs_get!(p, total_in);
        let total_out: libz_sys::uLong = zs_get!(p, total_out);
        let adler: libz_sys::uLong = zs_get!(p, adler);
        let data_type: c_int = zs_get!(p, data_type);
        (total_in, total_out, ended, adler, data_type)
    })?;
    Ok(ctx.ary_from(&[
        Value::int(total_in as i64),
        Value::int(total_out as i64),
        Value::bool(ended),
        Value::int(adler as i64),
        Value::int(data_type as i64),
    ]))
}

/// String.__zstream_params(handle, level, strategy) -> [status, output]
///
/// `deflateParams`: flushes what the old settings produced (returned as
/// `output`) and switches.
fn zstream_params(ctx: &mut Ctx, _: Value, args: &[Value], _: Block) -> Result<Value> {
    let handle = zstream_handle(ctx, args[0])?;
    let level = ctx.int(args[1])? as c_int;
    let strategy = ctx.int(args[2])? as c_int;
    let (rc, out, err) = with_zstream(ctx, handle, |e| {
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
        let err = if rc == libz_sys::Z_OK || rc == libz_sys::Z_BUF_ERROR {
            None
        } else {
            Some(p)
        };
        (rc, out, err)
    })?;
    let status = match err {
        None => Value::int(libz_sys::Z_OK as i64),
        Some(p) => zstream_status(ctx, rc, p),
    };
    let out = ctx.bytes(&out);
    Ok(ctx.ary_from(&[status, out]))
}

/// String.__zstream_dictionary(handle, dict) -> status
fn zstream_dictionary(ctx: &mut Ctx, _: Value, args: &[Value], _: Block) -> Result<Value> {
    let handle = zstream_handle(ctx, args[0])?;
    let dict: Vec<u8> = ctx.str_vec(args[1])?;
    let (rc, err) = with_zstream(ctx, handle, |e| {
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
        (rc, if rc == libz_sys::Z_OK { None } else { Some(p) })
    })?;
    Ok(match err {
        None => Value::int(0),
        Some(p) => zstream_status(ctx, rc, p),
    })
}

/// String.__zlib_version -> String
fn zlib_version(ctx: &mut Ctx, _: Value, _: &[Value], _: Block) -> Result<Value> {
    // SAFETY: zlibVersion returns a static NUL-terminated string.
    let v = unsafe { std::ffi::CStr::from_ptr(libz_sys::zlibVersion()) };
    Ok(ctx.str(v.to_bytes()))
}

/// String.__crc32(data, crc) -> Integer
///
/// zlib's `crc32(crc, data)`: `data` must be a String and `crc` an
/// Integer already reduced to 32 bits.
fn crc32(ctx: &mut Ctx, _: Value, args: &[Value], _: Block) -> Result<Value> {
    let seed = ctx.int(args[1])? as u32;
    let data = ctx.str_bytes(args[0])?;
    Ok(Value::int(crc32_update(seed, data) as i64))
}

/// String.__adler32(data, adler) -> Integer
///
/// zlib's `adler32(adler, data)`, same contract as `__crc32`.
fn adler32(ctx: &mut Ctx, _: Value, args: &[Value], _: Block) -> Result<Value> {
    let seed = ctx.int(args[1])? as u32;
    let data = ctx.str_bytes(args[0])?;
    Ok(Value::int(adler32_update(seed, data) as i64))
}

/// Slicing-by-8 tables for the reflected CRC-32 (polynomial 0xEDB88320):
/// `TABLES[k][b]` is the CRC contribution of byte `b` sitting `k` bytes
/// before the end of an 8-byte word.
static CRC_TABLES: LazyLock<[[u32; 256]; 8]> = LazyLock::new(|| {
    let mut t = [[0u32; 256]; 8];
    for i in 0..256u32 {
        let mut c = i;
        for _ in 0..8 {
            c = if c & 1 == 1 {
                0xEDB8_8320 ^ (c >> 1)
            } else {
                c >> 1
            };
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
fn crc32_update(crc: u32, data: &[u8]) -> u32 {
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
fn adler32_update(adler: u32, data: &[u8]) -> u32 {
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

    fn crc32_naive(crc: u32, data: &[u8]) -> u32 {
        let mut c = !crc;
        for &b in data {
            c ^= b as u32;
            for _ in 0..8 {
                c = if c & 1 == 1 {
                    0xEDB8_8320 ^ (c >> 1)
                } else {
                    c >> 1
                };
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
            assert_eq!(
                crc32_update(0xDEAD_BEEF, d),
                crc32_naive(0xDEAD_BEEF, d),
                "crc seed len {len}"
            );
            assert_eq!(adler32_update(1, d), adler32_naive(1, d), "adler len {len}");
            assert_eq!(
                adler32_update(0x1234_5678, d),
                adler32_naive(0x1234_5678, d),
                "adler seed len {len}"
            );
        }
        // Splitting a run continues the checksum exactly.
        let (l, r) = data.split_at(777);
        assert_eq!(crc32_update(crc32_update(0, l), r), crc32_update(0, &data));
        assert_eq!(
            adler32_update(adler32_update(1, l), r),
            adler32_update(1, &data)
        );
    }
}
