//! Digest's native half (`digest_native.so`) as a monoruby extension:
//! one-shot hashing, HMAC and PBKDF2 on the RustCrypto crates, registered
//! as singleton methods of `String`.
//!
//! The Ruby side (`stdlib/digest*.rb`) implements the full `Digest::*`
//! class API (`update` / `<<` / `reset` / `digest` / `hexdigest` / …) by
//! buffering the input and calling `String.__digest` to finalize.
//! Streaming over a buffer is equivalent to one-shot hashing of the
//! concatenated data, so no native per-instance state is needed.
//!
//! `__hmac` and `__pbkdf2_hmac` are the same arrangement one level up:
//! `stdlib/openssl.rb` keeps the class shape (`OpenSSL::HMAC`,
//! `OpenSSL::PKCS5` / `KDF`) and hands the bytes here. Both used to run
//! as Ruby — an HMAC cost two `Digest` passes over `ipad`/`opad` copies
//! built with `String#bytes.map.pack`, and PBKDF2 ran that whole thing
//! plus an Array-of-Fixnum xor once per iteration. Rails derives keys
//! with `2**16` iterations, so that loop alone was the largest single
//! item in a railsbench request.
//!
//! rubygems hashes `.gem` contents with `Digest::SHA256`, so like
//! `zlib_native` this library is part of every install.
//!
//! This is `src/builtins/digest.rs` moved out of the interpreter
//! (doc/native_extension_loading.md, step 3).

use hmac::{Hmac, Mac};
use md5::Md5;
use monoruby_ext::*;
use sha1::Sha1;
use sha2::{Digest, Sha256, Sha384, Sha512};
use std::ffi::c_int;

#[unsafe(no_mangle)]
pub unsafe extern "C" fn Init_digest_native(ctx: *mut MrContext) -> c_int {
    // SAFETY: the interpreter's contract for `Init_`.
    unsafe { init(ctx, init_digest) }
}

fn init_digest(ctx: &mut Ctx) -> Result<()> {
    let string = ctx.const_get(Value::UNDEF, "String").ok_or(Error)?;
    let s = MR_METHOD_SINGLETON;
    ctx.define_method(string, "__digest", method!(digest_hash), 2, s);
    ctx.define_method(string, "__hmac", method!(hmac_digest), 3, s);
    ctx.define_method(string, "__pbkdf2_hmac", method!(pbkdf2_hmac), 5, s);
    Ok(())
}

fn unsupported(ctx: &mut Ctx, algo: &str) -> Error {
    ctx.argument_error(format!("unsupported digest algorithm: {algo}"))
}

/// String.__digest(algorithm, data) -> binary String
///
/// `algorithm` is "md5" / "sha1" / "sha256" / "sha384" / "sha512".
/// Returns the raw (ASCII-8BIT) digest of `data`.
fn digest_hash(ctx: &mut Ctx, _: Value, args: &[Value], _: Block) -> Result<Value> {
    let algo = ctx.str_string(args[0])?;
    let data = ctx.str_bytes(args[1])?;
    let out: Vec<u8> = match algo.as_str() {
        "md5" => Md5::digest(data).to_vec(),
        "sha1" => Sha1::digest(data).to_vec(),
        "sha256" => Sha256::digest(data).to_vec(),
        "sha384" => Sha384::digest(data).to_vec(),
        "sha512" => Sha512::digest(data).to_vec(),
        other => return Err(unsupported(ctx, other)),
    };
    Ok(ctx.bytes(out))
}

/// String.__hmac(algorithm, key, data) -> binary String
///
/// Raw HMAC (RFC 2104) of `data` under `key`, for the same algorithm
/// names `__digest` takes. The key is used as given: HMAC itself hashes
/// one longer than the block and zero-pads a shorter one.
fn hmac_digest(ctx: &mut Ctx, _: Value, args: &[Value], _: Block) -> Result<Value> {
    let algo = ctx.str_string(args[0])?;
    let key = ctx.str_bytes(args[1])?;
    let data = ctx.str_bytes(args[2])?;

    macro_rules! mac {
        ($d:ty) => {{
            // `new_from_slice` only fails for a fixed-size key type; the
            // HMAC construction accepts any length.
            let mut mac = <Hmac<$d>>::new_from_slice(key).unwrap();
            mac.update(data);
            mac.finalize().into_bytes().to_vec()
        }};
    }
    let out: Vec<u8> = match algo.as_str() {
        "md5" => mac!(Md5),
        "sha1" => mac!(Sha1),
        "sha256" => mac!(Sha256),
        "sha384" => mac!(Sha384),
        "sha512" => mac!(Sha512),
        other => return Err(unsupported(ctx, other)),
    };
    Ok(ctx.bytes(out))
}

/// String.__pbkdf2_hmac(algorithm, pass, salt, iterations, keylen) -> binary String
///
/// PBKDF2 (RFC 8018 §5.2) with HMAC as the PRF. `iterations` below 1 is
/// treated as 1, matching what the Ruby loop this replaces did (and what
/// the `pbkdf2` crate does for a zero round count).
fn pbkdf2_hmac(ctx: &mut Ctx, _: Value, args: &[Value], _: Block) -> Result<Value> {
    let algo = ctx.str_string(args[0])?;
    // Copied: the borrowed bytes would not survive the `&mut` calls below.
    let pass = ctx.str_vec(args[1])?;
    let salt = ctx.str_vec(args[2])?;
    let iter = ctx.int(args[3])?;
    let keylen = ctx.int(args[4])?;

    if keylen < 0 {
        return Err(ctx.argument_error("negative key length"));
    }
    let Ok(keylen) = usize::try_from(keylen) else {
        return Err(ctx.argument_error("key length too large"));
    };
    // The round count is a u32 in the PRF; anything above that would run
    // for longer than a process lives, so reject it rather than wrap.
    let Ok(rounds) = u32::try_from(iter.max(1)) else {
        return Err(ctx.argument_error("iteration count too large"));
    };

    let mut out = vec![0u8; keylen];
    macro_rules! derive {
        ($d:ty) => {
            pbkdf2::pbkdf2_hmac::<$d>(&pass, &salt, rounds, &mut out)
        };
    }
    match algo.as_str() {
        "md5" => derive!(Md5),
        "sha1" => derive!(Sha1),
        "sha256" => derive!(Sha256),
        "sha384" => derive!(Sha384),
        "sha512" => derive!(Sha512),
        other => return Err(unsupported(ctx, other)),
    }
    Ok(ctx.bytes(out))
}
