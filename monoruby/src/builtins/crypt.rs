//! `String#crypt` — the traditional `crypt(3)`, a modified DES.
//!
//! CRuby hands this to the C library, which on every platform the
//! `core/string/crypt` spec runs on answers with the algorithm Morris
//! and Thompson published in 1979: the password's first eight
//! characters become a 56-bit key, the salt's first two characters a
//! 12-bit value that permutes the expansion box, and a block of zeros
//! is encrypted twenty-five times under that key. The result is the
//! salt followed by eleven characters of the `./0-9A-Za-z` alphabet.
//!
//! The salt permutation is the whole point of the design: it makes a
//! precomputed dictionary useless against 4096 variants of the same
//! password, and it is why this cannot be a stock DES.
//!
//! Only the traditional two-character salt is implemented. A salt the
//! algorithm cannot use — a modular one (`$1$`, `$5$`, `$6$`, `$2a$` …)
//! or a character outside the alphabet — answers `"*0"`, the failure
//! marker glibc returns for a salt it cannot honour: never a valid
//! hash, so it can never be mistaken for one. (The BSD `crypt(3)` macOS
//! carries reads an out-of-alphabet character as zero and hashes
//! anyway, so that one case is platform-dependent in CRuby and is not
//! pinned by the tests.)

use super::*;

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_func(STRING_CLASS, "crypt", crypt, 1);
}

/// The alphabet both the salt and the output are written in.
const ALPHABET: &[u8; 64] =
    b"./0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";

/// The salt character's six-bit value, or `None` when it is not one.
fn a64(c: u8) -> Option<u32> {
    ALPHABET.iter().position(|&a| a == c).map(|i| i as u32)
}

// The DES tables, as FIPS 46 numbers them: bit 1 is the most
// significant of the block, so a table entry `n` selects
// `1 << (width - n)`.

#[rustfmt::skip]
const IP: [u8; 64] = [
    58, 50, 42, 34, 26, 18, 10, 2, 60, 52, 44, 36, 28, 20, 12, 4,
    62, 54, 46, 38, 30, 22, 14, 6, 64, 56, 48, 40, 32, 24, 16, 8,
    57, 49, 41, 33, 25, 17,  9, 1, 59, 51, 43, 35, 27, 19, 11, 3,
    61, 53, 45, 37, 29, 21, 13, 5, 63, 55, 47, 39, 31, 23, 15, 7,
];

#[rustfmt::skip]
const FP: [u8; 64] = [
    40, 8, 48, 16, 56, 24, 64, 32, 39, 7, 47, 15, 55, 23, 63, 31,
    38, 6, 46, 14, 54, 22, 62, 30, 37, 5, 45, 13, 53, 21, 61, 29,
    36, 4, 44, 12, 52, 20, 60, 28, 35, 3, 43, 11, 51, 19, 59, 27,
    34, 2, 42, 10, 50, 18, 58, 26, 33, 1, 41,  9, 49, 17, 57, 25,
];

#[rustfmt::skip]
const E: [u8; 48] = [
    32,  1,  2,  3,  4,  5,  4,  5,  6,  7,  8,  9,
     8,  9, 10, 11, 12, 13, 12, 13, 14, 15, 16, 17,
    16, 17, 18, 19, 20, 21, 20, 21, 22, 23, 24, 25,
    24, 25, 26, 27, 28, 29, 28, 29, 30, 31, 32,  1,
];

#[rustfmt::skip]
const P: [u8; 32] = [
    16,  7, 20, 21, 29, 12, 28, 17, 1, 15, 23, 26,  5, 18, 31, 10,
     2,  8, 24, 14, 32, 27,  3,  9, 19, 13, 30,  6, 22, 11,  4, 25,
];

#[rustfmt::skip]
const PC1: [u8; 56] = [
    57, 49, 41, 33, 25, 17,  9,  1, 58, 50, 42, 34, 26, 18,
    10,  2, 59, 51, 43, 35, 27, 19, 11,  3, 60, 52, 44, 36,
    63, 55, 47, 39, 31, 23, 15,  7, 62, 54, 46, 38, 30, 22,
    14,  6, 61, 53, 45, 37, 29, 21, 13,  5, 28, 20, 12,  4,
];

#[rustfmt::skip]
const PC2: [u8; 48] = [
    14, 17, 11, 24,  1,  5,  3, 28, 15,  6, 21, 10,
    23, 19, 12,  4, 26,  8, 16,  7, 27, 20, 13,  2,
    41, 52, 31, 37, 47, 55, 30, 40, 51, 45, 33, 48,
    44, 49, 39, 56, 34, 53, 46, 42, 50, 36, 29, 32,
];

const SHIFTS: [u32; 16] = [1, 1, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 1];

#[rustfmt::skip]
const S: [[u8; 64]; 8] = [
    [
        14,  4, 13,  1,  2, 15, 11,  8,  3, 10,  6, 12,  5,  9,  0,  7,
         0, 15,  7,  4, 14,  2, 13,  1, 10,  6, 12, 11,  9,  5,  3,  8,
         4,  1, 14,  8, 13,  6,  2, 11, 15, 12,  9,  7,  3, 10,  5,  0,
        15, 12,  8,  2,  4,  9,  1,  7,  5, 11,  3, 14, 10,  0,  6, 13,
    ],
    [
        15,  1,  8, 14,  6, 11,  3,  4,  9,  7,  2, 13, 12,  0,  5, 10,
         3, 13,  4,  7, 15,  2,  8, 14, 12,  0,  1, 10,  6,  9, 11,  5,
         0, 14,  7, 11, 10,  4, 13,  1,  5,  8, 12,  6,  9,  3,  2, 15,
        13,  8, 10,  1,  3, 15,  4,  2, 11,  6,  7, 12,  0,  5, 14,  9,
    ],
    [
        10,  0,  9, 14,  6,  3, 15,  5,  1, 13, 12,  7, 11,  4,  2,  8,
        13,  7,  0,  9,  3,  4,  6, 10,  2,  8,  5, 14, 12, 11, 15,  1,
        13,  6,  4,  9,  8, 15,  3,  0, 11,  1,  2, 12,  5, 10, 14,  7,
         1, 10, 13,  0,  6,  9,  8,  7,  4, 15, 14,  3, 11,  5,  2, 12,
    ],
    [
         7, 13, 14,  3,  0,  6,  9, 10,  1,  2,  8,  5, 11, 12,  4, 15,
        13,  8, 11,  5,  6, 15,  0,  3,  4,  7,  2, 12,  1, 10, 14,  9,
        10,  6,  9,  0, 12, 11,  7, 13, 15,  1,  3, 14,  5,  2,  8,  4,
         3, 15,  0,  6, 10,  1, 13,  8,  9,  4,  5, 11, 12,  7,  2, 14,
    ],
    [
         2, 12,  4,  1,  7, 10, 11,  6,  8,  5,  3, 15, 13,  0, 14,  9,
        14, 11,  2, 12,  4,  7, 13,  1,  5,  0, 15, 10,  3,  9,  8,  6,
         4,  2,  1, 11, 10, 13,  7,  8, 15,  9, 12,  5,  6,  3,  0, 14,
        11,  8, 12,  7,  1, 14,  2, 13,  6, 15,  0,  9, 10,  4,  5,  3,
    ],
    [
        12,  1, 10, 15,  9,  2,  6,  8,  0, 13,  3,  4, 14,  7,  5, 11,
        10, 15,  4,  2,  7, 12,  9,  5,  6,  1, 13, 14,  0, 11,  3,  8,
         9, 14, 15,  5,  2,  8, 12,  3,  7,  0,  4, 10,  1, 13, 11,  6,
         4,  3,  2, 12,  9,  5, 15, 10, 11, 14,  1,  7,  6,  0,  8, 13,
    ],
    [
         4, 11,  2, 14, 15,  0,  8, 13,  3, 12,  9,  7,  5, 10,  6,  1,
        13,  0, 11,  7,  4,  9,  1, 10, 14,  3,  5, 12,  2, 15,  8,  6,
         1,  4, 11, 13, 12,  3,  7, 14, 10, 15,  6,  8,  0,  5,  9,  2,
         6, 11, 13,  8,  1,  4, 10,  7,  9,  5,  0, 15, 14,  2,  3, 12,
    ],
    [
        13,  2,  8,  4,  6, 15, 11,  1, 10,  9,  3, 14,  5,  0, 12,  7,
         1, 15, 13,  8, 10,  3,  7,  4, 12,  5,  6, 11,  0, 14,  9,  2,
         7, 11,  4,  1,  9, 12, 14,  2,  0,  6, 10, 13, 15,  3,  5,  8,
         2,  1, 14,  7,  4, 10,  8, 13, 15, 12,  9,  0,  3,  5,  6, 11,
    ],
];

/// Permute `value` (`in_bits` wide) through a FIPS-numbered table.
fn permute(value: u64, table: &[u8], in_bits: u32) -> u64 {
    let mut out = 0u64;
    for (i, &from) in table.iter().enumerate() {
        let bit = (value >> (in_bits - from as u32)) & 1;
        out |= bit << (table.len() - 1 - i);
    }
    out
}

/// The sixteen 48-bit round keys of a 64-bit key. The parity bits are
/// dropped by PC1, so only 56 of the 64 matter.
fn key_schedule(key: u64) -> [u64; 16] {
    let pc1 = permute(key, &PC1, 64);
    let (mut c, mut d) = ((pc1 >> 28) as u32 & 0x0fff_ffff, pc1 as u32 & 0x0fff_ffff);
    let mut keys = [0u64; 16];
    for (round, key) in keys.iter_mut().enumerate() {
        let n = SHIFTS[round];
        c = ((c << n) | (c >> (28 - n))) & 0x0fff_ffff;
        d = ((d << n) | (d >> (28 - n))) & 0x0fff_ffff;
        *key = permute(((c as u64) << 28) | d as u64, &PC2, 56);
    }
    keys
}

/// Encrypt `block` under `keys`, `count` passes of the sixteen rounds,
/// with `salt` swapping the expansion box's two halves bit by bit — the
/// one thing that is not stock DES.
///
/// The initial and final permutations bracket the *whole* run, not each
/// pass: that is what makes twenty-five passes twenty-five times the
/// work rather than twenty-five separate encryptions.
fn des_cipher(block: u64, keys: &[u64; 16], salt: u32, count: u32) -> u64 {
    let ip = permute(block, &IP, 64);
    let (mut l, mut r) = ((ip >> 32) as u32, ip as u32);
    for _ in 0..count {
        for key in keys {
            let mut e = permute(r as u64, &E, 32);
            // Salt bit `i` trades the expansion's bit `i + 1` (counting
            // from the top, as FIPS numbers them) with the bit
            // twenty-four places along — the high half's bit with the
            // low half's.
            for i in 0..12 {
                if salt & (1 << i) != 0 {
                    let hi = (e >> (47 - i)) & 1;
                    let lo = (e >> (23 - i)) & 1;
                    if hi != lo {
                        e ^= (1 << (47 - i)) | (1 << (23 - i));
                    }
                }
            }
            e ^= key;
            let mut sout = 0u32;
            for (box_index, sbox) in S.iter().enumerate() {
                let six = ((e >> (42 - 6 * box_index)) & 0x3f) as usize;
                // The outer bits pick the row, the inner four the column.
                let row = ((six & 0x20) >> 4) | (six & 1);
                let col = (six >> 1) & 0x0f;
                sout |= (sbox[row * 16 + col] as u32) << (28 - 4 * box_index);
            }
            let f = permute(sout as u64, &P, 32) as u32;
            let next = l ^ f;
            l = r;
            r = next;
        }
        // Each pass ends with the halves swapped back.
        std::mem::swap(&mut l, &mut r);
    }
    permute(((l as u64) << 32) | r as u64, &FP, 64)
}

/// `crypt(3)` proper: `Some(hash)` for a traditional salt, `None` when
/// the salt is not one the algorithm can use.
fn des_crypt(password: &[u8], salt: &[u8]) -> Option<String> {
    let (s0, s1) = (a64(*salt.first()?)?, a64(*salt.get(1)?)?);
    let salt_bits = s0 | (s1 << 6);
    // Eight characters, seven bits each, in the top of every byte.
    let mut key = 0u64;
    for i in 0..8 {
        let c = password.get(i).copied().unwrap_or(0) as u64;
        key |= ((c & 0x7f) << 1) << (56 - 8 * i);
    }
    let keys = key_schedule(key);
    let block = des_cipher(0, &keys, salt_bits, 25);
    // Eleven characters of six bits, most significant first, over the
    // block padded out to 66 bits.
    let mut out = String::with_capacity(13);
    out.push(salt[0] as char);
    out.push(salt[1] as char);
    let padded = (block as u128) << 2;
    for i in 0..11 {
        let six = ((padded >> (60 - 6 * i)) & 0x3f) as usize;
        out.push(ALPHABET[six] as char);
    }
    Some(out)
}

///
/// ### String#crypt
/// - crypt(salt) -> String
///
/// The traditional `crypt(3)` hash of `self` under `salt`: DES with the
/// salt folded into the expansion box, twenty-five times over. Only the
/// first eight bytes of the receiver and the first two of the salt
/// count, and the answer is BINARY, as the C library's is.
///
/// [https://docs.ruby-lang.org/ja/latest/method/String/i/crypt.html]
#[monoruby_builtin]
fn crypt(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let password = self_.expect_bytes(&globals.store)?.to_vec();
    // `#to_str` is honoured on the salt, and only on the salt.
    let salt = lfp
        .arg(0)
        .coerce_to_rstring(vm, globals)?
        .as_bytes()
        .to_vec();
    // The C `crypt` takes NUL-terminated strings, so a NUL anywhere in
    // the password would silently truncate it, and a salt shorter than
    // two *non-NUL* bytes is no salt at all.
    if password.contains(&0) {
        return Err(MonorubyErr::argumenterr("string contains null byte"));
    }
    if salt.len() < 2 || salt[0] == 0 || salt[1] == 0 {
        return Err(MonorubyErr::argumenterr(
            "salt too short (need >=2 bytes)",
        ));
    }
    // `"*0"` is what the C library answers for a salt it cannot use: a
    // string no hash can equal, so a comparison against it fails rather
    // than succeeding by accident.
    let hashed = des_crypt(&password, &salt).unwrap_or_else(|| "*0".to_string());
    Ok(Value::string_from_inner(RStringInner::from_encoding(
        hashed.as_bytes(),
        Encoding::Ascii8,
    )))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::*;

    /// With no salt and one pass this is stock DES, so FIPS 46's own
    /// worked example pins the tables and the round structure on their
    /// own — before the salt and the twenty-five passes are layered on.
    #[test]
    fn plain_des_vector() {
        let keys = key_schedule(0x1334_5779_9BBC_DFF1);
        let out = des_cipher(0x0123_4567_89AB_CDEF, &keys, 0, 1);
        assert_eq!(format!("{out:016X}"), "85E813540F0AB405");
    }

    /// `String#crypt` (#1425).
    #[test]
    fn string_crypt() {
        let mut v: Vec<String> = vec![];
        // Every salt character in both positions, against passwords
        // shorter and longer than the eight bytes that count.
        let alphabet: Vec<char> = ALPHABET.iter().map(|&b| b as char).collect();
        for c in &alphabet {
            for pw in ["", "a", "password", "123456789"] {
                v.push(format!(r#""{pw}".crypt("{c}a")"#));
                v.push(format!(r#""{pw}".crypt("a{c}")"#));
            }
        }
        let refs: Vec<&str> = v.iter().map(|s| s.as_str()).collect();
        run_tests(&refs);
        run_tests(&[
            // The published vectors.
            r#"["".crypt("aa"), "nutmeg".crypt("Mi"), "ellen1".crypt("ri"),
                "Sharon".crypt("./"), "norahs".crypt("am"), "norahs".crypt("7a")]"#,
            // Only the first eight bytes of the receiver count…
            r#"["01234567".crypt("aa"), "012345678".crypt("aa"), "0123456789".crypt("aa")]"#,
            // …and only the first two of the salt.
            r#"["hello world".crypt("aa"), "hello world".crypt("aab"),
                "hello world".crypt("aabc")]"#,
            // The result is BINARY and unfrozen, and never a subclass.
            r#"s = "".crypt("aa"); [s.encoding.name, s.frozen?, s.class.to_s]"#,
            r#"class CryptStr < String; end
               [CryptStr.new("hello").crypt("aa").class.to_s,
                "hello".crypt(CryptStr.new("aa")).class.to_s]"#,
            // `#to_str` is honoured on the salt.
            r#"o = Object.new; def o.to_str = "aa"; "".crypt(o)"#,
            // High bytes go through as bytes.
            r#"[0xFF, 0xFE].pack("C*").crypt("aa")"#,
            r#""héllo".crypt("aa")"#,
        ]);
        // A NUL in the receiver, and a salt with fewer than two
        // non-NUL bytes, are both rejected.
        for bad in [r#""poison null".crypt("aa")"#, r#""hello".crypt("")"#,
                    r#""hello".crypt("f")"#, r#""hello".crypt("  ")"#,
                    r#""hello".crypt(" a")"#, r#""hello".crypt("a ")"#,
                    r#""".crypt(5)"#, r#""".crypt(Object.new)"#] {
            run_test_error(bad);
        }
    }
}
