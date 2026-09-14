use super::*;
use hmac::{Hmac, Mac};
use md5::Md5;
use sha1::Sha1;
use sha2::{Digest, Sha256, Sha384, Sha512};

//
// Digest module — native one-shot hashing backend.
//
// The Ruby side (stdlib/digest*.rb) implements the full `Digest::*` class
// API (`update` / `<<` / `reset` / `digest` / `hexdigest` / …) by buffering
// the input and calling this single native helper to finalize. Streaming
// over a buffer is equivalent to one-shot hashing of the concatenated data,
// so no native per-instance state is needed.
//
// `__hmac` and `__pbkdf2_hmac` are the same arrangement one level up:
// `stdlib/openssl.rb` keeps the class shape (`OpenSSL::HMAC`,
// `OpenSSL::PKCS5` / `KDF`) and hands the bytes here. Both used to run
// as Ruby — an HMAC cost two `Digest` passes over `ipad`/`opad` copies
// built with `String#bytes.map.pack`, and PBKDF2 ran that whole thing
// plus an Array-of-Fixnum xor once per iteration. Rails derives keys
// with `2**16` iterations, so that loop alone was the largest single
// item in a railsbench request.
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_func(STRING_CLASS, "__digest", digest_hash, 2);
    globals.define_builtin_class_func(STRING_CLASS, "__hmac", hmac_digest, 3);
    globals.define_builtin_class_func(STRING_CLASS, "__pbkdf2_hmac", pbkdf2_hmac, 5);
}

fn unsupported(algo: &str) -> MonorubyErr {
    MonorubyErr::argumenterr(format!("unsupported digest algorithm: {algo}"))
}

/// String.__digest(algorithm, data) -> binary String
///
/// `algorithm` is "md5" / "sha1" / "sha256" / "sha384" / "sha512".
/// Returns the raw (ASCII-8BIT) digest of `data`.
#[monoruby_builtin]
fn digest_hash(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let algo = lfp.arg(0).expect_string(&globals.store)?;
    let data_v = lfp.arg(1);
    let data = data_v.expect_bytes(&globals.store)?;
    let out: Vec<u8> = match algo.as_str() {
        "md5" => Md5::digest(data).to_vec(),
        "sha1" => Sha1::digest(data).to_vec(),
        "sha256" => Sha256::digest(data).to_vec(),
        "sha384" => Sha384::digest(data).to_vec(),
        "sha512" => Sha512::digest(data).to_vec(),
        other => {
            return Err(MonorubyErr::argumenterr(format!(
                "unsupported digest algorithm: {other}"
            )));
        }
    };
    Ok(Value::bytes(out))
}

/// String.__hmac(algorithm, key, data) -> binary String
///
/// Raw HMAC (RFC 2104) of `data` under `key`, for the same algorithm
/// names `__digest` takes. The key is used as given: HMAC itself hashes
/// one longer than the block and zero-pads a shorter one.
#[monoruby_builtin]
fn hmac_digest(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let algo = lfp.arg(0).expect_string(&globals.store)?;
    let key_v = lfp.arg(1);
    let key = key_v.expect_bytes(&globals.store)?;
    let data_v = lfp.arg(2);
    let data = data_v.expect_bytes(&globals.store)?;

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
        other => return Err(unsupported(other)),
    };
    Ok(Value::bytes(out))
}

/// String.__pbkdf2_hmac(algorithm, pass, salt, iterations, keylen) -> binary String
///
/// PBKDF2 (RFC 8018 §5.2) with HMAC as the PRF. `iterations` below 1 is
/// treated as 1, matching what the Ruby loop this replaces did (and what
/// the `pbkdf2` crate does for a zero round count).
#[monoruby_builtin]
fn pbkdf2_hmac(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let algo = lfp.arg(0).expect_string(&globals.store)?;
    let pass_v = lfp.arg(1);
    let pass = pass_v.expect_bytes(&globals.store)?;
    let salt_v = lfp.arg(2);
    let salt = salt_v.expect_bytes(&globals.store)?;
    let iter = lfp.arg(3).expect_integer(&globals.store)?;
    let keylen = lfp.arg(4).expect_integer(&globals.store)?;

    if keylen < 0 {
        return Err(MonorubyErr::argumenterr("negative key length"));
    }
    let keylen = usize::try_from(keylen)
        .map_err(|_| MonorubyErr::argumenterr("key length too large"))?;
    // The round count is a u32 in the PRF; anything above that would run
    // for longer than a process lives, so reject it rather than wrap.
    let rounds = u32::try_from(iter.max(1))
        .map_err(|_| MonorubyErr::argumenterr("iteration count too large"))?;

    let mut out = vec![0u8; keylen];
    macro_rules! derive {
        ($d:ty) => {
            pbkdf2::pbkdf2_hmac::<$d>(pass, salt, rounds, &mut out)
        };
    }
    match algo.as_str() {
        "md5" => derive!(Md5),
        "sha1" => derive!(Sha1),
        "sha256" => derive!(Sha256),
        "sha384" => derive!(Sha384),
        "sha512" => derive!(Sha512),
        other => return Err(unsupported(other)),
    }
    Ok(Value::bytes(out))
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn digest() {
        run_tests(&[
            r#"require 'digest'; Digest::MD5.hexdigest("The quick brown fox jumps over the lazy dog")"#,
            r#"require 'digest'; Digest::MD5.hexdigest("")"#,
            r#"require 'digest/md5'; Digest::MD5.hexdigest("abc")"#,
            r#"require 'digest/sha1'; Digest::SHA1.hexdigest("abc")"#,
            r#"require 'digest/sha2'; Digest::SHA256.hexdigest("abc")"#,
            r#"require 'digest/sha2'; Digest::SHA384.hexdigest("abc")"#,
            r#"require 'digest/sha2'; Digest::SHA512.hexdigest("abc")"#,
            // const_missing autoload (SHA256 referenced via Digest:: only)
            r#"require 'digest'; Digest.const_get(:SHA256).hexdigest("abc")"#,
            // streaming via update / <<
            r#"require 'digest'; Digest::SHA256.new.update("ab").update("c").hexdigest"#,
            r#"require 'digest'; d = Digest::MD5.new; d << "a"; d << "bc"; d.hexdigest"#,
            // binary digest length, base64, metadata
            r#"require 'digest'; Digest::SHA256.digest("abc").length"#,
            r#"require 'digest'; Digest::MD5.base64digest("abc")"#,
            r#"require 'digest'; Digest::SHA512.new.digest_length"#,
            r#"require 'digest'; Digest::SHA1.new.block_length"#,
            // SHA2 bit-length wrapper
            r#"require 'digest/sha2'; Digest::SHA2.new(512).update("abc").hexdigest"#,
        ]);
    }

    #[test]
    fn digest_unsupported_algorithm() {
        run_test_error(r#"String.__digest("bogus", "data")"#);
    }
}
