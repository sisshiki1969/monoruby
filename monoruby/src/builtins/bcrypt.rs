//! The native half of the `bcrypt` gem (`bcrypt_ext.so`): the two entry
//! points its `BCrypt::Engine` calls, `__bc_salt` and `__bc_crypt`, on
//! the `bcrypt` crate. The gem's Ruby half is the installed gem's own;
//! `gem/bcrypt_ext.rb` stands in for the extension and forwards here.
//!
//! The extension is crypt_blowfish, whose output the crate reproduces
//! byte for byte: a `$2a$` / `$2b$` / `$2x$` / `$2y$` prefix, the
//! two-digit cost, the 22-character salt (re-encoded from its 128 bits,
//! so its last character is normalized as crypt_blowfish normalizes
//! it) and the 31-character hash, in bcrypt's own base64 alphabet.

use super::*;

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_func(STRING_CLASS, "__bcrypt_salt", bcrypt_salt, 3);
    globals.define_builtin_class_func(STRING_CLASS, "__bcrypt_crypt", bcrypt_crypt, 2);
}

/// The extension's strings are `rb_str_new` results: BINARY.
fn binary(s: String) -> Value {
    Value::string_from_inner(RStringInner::from_encoding(s.as_bytes(), Encoding::Ascii8))
}

const ALPHABET: &[u8; 64] =
    b"./ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";

/// bcrypt's base64 (crypt_blowfish `BF_encode`): the alphabet above, no
/// padding, the trailing bits of the last character zero.
fn encode(bytes: &[u8]) -> String {
    let mut out = String::with_capacity(bytes.len().div_ceil(3) * 4);
    let mut i = 0;
    while i < bytes.len() {
        let c1 = bytes[i];
        out.push(ALPHABET[(c1 >> 2) as usize] as char);
        let mut c = (c1 & 0x03) << 4;
        if i + 1 >= bytes.len() {
            out.push(ALPHABET[c as usize] as char);
            break;
        }
        let c2 = bytes[i + 1];
        c |= c2 >> 4;
        out.push(ALPHABET[c as usize] as char);
        c = (c2 & 0x0f) << 2;
        if i + 2 >= bytes.len() {
            out.push(ALPHABET[c as usize] as char);
            break;
        }
        let c3 = bytes[i + 2];
        c |= c3 >> 6;
        out.push(ALPHABET[c as usize] as char);
        out.push(ALPHABET[(c3 & 0x3f) as usize] as char);
        i += 3;
    }
    out
}

/// The 16 salt bytes of a 22-character bcrypt-base64 salt
/// (crypt_blowfish `BF_decode`), `None` for a character outside the
/// alphabet.
fn decode_salt(s: &[u8]) -> Option<[u8; 16]> {
    if s.len() < 22 {
        return None;
    }
    let val = |c: u8| ALPHABET.iter().position(|&a| a == c).map(|p| p as u8);
    let mut out = [0u8; 16];
    let mut o = 0;
    let mut i = 0;
    while o < 16 {
        let c1 = val(s[i])?;
        let c2 = val(s[i + 1])?;
        out[o] = (c1 << 2) | ((c2 & 0x30) >> 4);
        o += 1;
        if o >= 16 {
            break;
        }
        let c3 = val(s[i + 2])?;
        out[o] = ((c2 & 0x0f) << 4) | ((c3 & 0x3c) >> 2);
        o += 1;
        if o >= 16 {
            break;
        }
        let c4 = val(s[i + 3])?;
        out[o] = ((c3 & 0x03) << 6) | c4;
        o += 1;
        i += 4;
    }
    Some(out)
}

///
/// ### String.__bcrypt_salt
///
/// - __bcrypt_salt(prefix, cost, random_bytes) -> String | nil
///
/// `BCrypt::Engine.__bc_salt`: the salt string `prefix` + two-digit
/// `cost` + the 22-character encoding of the first 16 of
/// `random_bytes`, or `nil` when the arguments are not usable (the gem
/// raises on `nil`).
///
#[monoruby_builtin]
fn bcrypt_salt(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let (a0, a2) = (lfp.arg(0), lfp.arg(2));
    let prefix = a0.expect_str(&globals.store)?;
    let cost = lfp.arg(1).expect_integer(&globals.store)?;
    let bytes = a2.expect_bytes(&globals.store)?;
    if prefix.contains('\0') {
        return Err(MonorubyErr::argumenterr("string contains null byte"));
    }
    if !matches!(prefix, "$2a$" | "$2b$" | "$2x$" | "$2y$") || !(4..=31).contains(&cost) || bytes.len() < 16 {
        return Ok(Value::nil());
    }
    Ok(binary(format!("{prefix}{cost:02}${}", encode(&bytes[..16]))))
}

///
/// ### String.__bcrypt_crypt
///
/// - __bcrypt_crypt(secret, salt) -> String | nil
///
/// `BCrypt::Engine.__bc_crypt`: the 60-character hash of `secret` under
/// `salt` (a salt string or a full hash, whose salt part is used), or
/// `nil` for a salt the algorithm does not accept. The extension takes
/// both as C strings (`StringValueCStr`): a NUL byte in either is an
/// `ArgumentError`. The secret is read as crypt_blowfish reads it: at
/// most 72 bytes.
///
#[monoruby_builtin]
fn bcrypt_crypt(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let (a0, a1) = (lfp.arg(0), lfp.arg(1));
    let secret = a0.expect_bytes(&globals.store)?;
    let salt = a1.expect_bytes(&globals.store)?;
    if secret.contains(&0) || salt.contains(&0) {
        return Err(MonorubyErr::argumenterr("string contains null byte"));
    }
    let Some(parsed) = parse_salt(salt) else {
        return Ok(Value::nil());
    };
    let (version, cost, salt16) = parsed;
    let end = secret.len().min(72);
    match ::bcrypt::hash_with_salt(&secret[..end], cost, salt16) {
        Ok(parts) => Ok(binary(parts.format_for_version(version))),
        Err(_) => Ok(Value::nil()),
    }
}

/// `$2?$NN$<22 chars>...` → its version, cost and 16 salt bytes.
fn parse_salt(s: &[u8]) -> Option<(::bcrypt::Version, u32, [u8; 16])> {
    if s.len() < 29 || s[0] != b'$' || s[1] != b'2' || s[3] != b'$' || s[6] != b'$' {
        return None;
    }
    let version = match s[2] {
        b'a' => ::bcrypt::Version::TwoA,
        b'b' => ::bcrypt::Version::TwoB,
        b'x' => ::bcrypt::Version::TwoX,
        b'y' => ::bcrypt::Version::TwoY,
        _ => return None,
    };
    if !s[4].is_ascii_digit() || !s[5].is_ascii_digit() {
        return None;
    }
    let cost = ((s[4] - b'0') as u32) * 10 + (s[5] - b'0') as u32;
    if !(4..=31).contains(&cost) {
        return None;
    }
    Some((version, cost, decode_salt(&s[7..])?))
}
