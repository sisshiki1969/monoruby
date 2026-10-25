//! The native half of `OpenSSL::Cipher` (`stdlib/openssl.rb`) as a
//! monoruby extension (`openssl_native.so`): AES-GCM and AES-CBC on the
//! RustCrypto crates (`aes-gcm`, `aes` + `cbc`), so output is
//! byte-identical to openssl.
//!
//! The Ruby side buffers the whole message (`update`) and runs one of
//! these one-shot helpers at `final`, so the native state is a call, not
//! an object: the modes Rails uses (`aes-*-gcm` for cookies and
//! ActiveRecord encryption, `aes-*-cbc` as the legacy fallback) are
//! both computed over the complete input anyway (GCM needs it for the
//! tag, CBC for the padding). Digests, HMAC and PBKDF2 are
//! `digest_native.so`'s.
//!
//! This is `src/builtins/cipher.rs` moved out of the interpreter
//! (doc/native_extension_loading.md, step 3).

use aes_gcm::aead::{Aead, KeyInit, Payload};
use aes_gcm::{Aes128Gcm, Aes256Gcm, AesGcm, Nonce};
use cbc::cipher::block_padding::Pkcs7;
use cbc::cipher::{BlockDecryptMut, BlockEncryptMut, KeyIvInit};
use monoruby_ext::*;
use std::ffi::c_int;

#[unsafe(no_mangle)]
pub unsafe extern "C" fn Init_openssl_native(ctx: *mut MrContext) -> c_int {
    // SAFETY: the interpreter's contract for `Init_`.
    unsafe { init(ctx, init_openssl) }
}

fn init_openssl(ctx: &mut Ctx) -> Result<()> {
    let string = ctx.const_get(Value::UNDEF, "String").ok_or(Error)?;
    let s = MR_METHOD_SINGLETON;
    ctx.define_method(string, "__aes_gcm", method!(aes_gcm), 6, s);
    ctx.define_method(string, "__aes_cbc", method!(aes_cbc), 4, s);
    Ok(())
}

fn cipher_err(ctx: &mut Ctx, msg: &str) -> Error {
    // Raised as the Ruby-side `OpenSSL::Cipher::CipherError`.
    ctx.runtime_error(format!("__cipher__:{msg}"))
}

type Aes192Gcm = AesGcm<aes::Aes192, aes_gcm::aead::consts::U12>;

/// String.__aes_gcm(encrypt, key, iv, aad, data, tag) -> [out, tag]
///
/// AES-GCM with a 96-bit nonce. Encrypting answers `[ciphertext,
/// 16-byte tag]` (`tag` ignored); decrypting answers `[plaintext,
/// tag]` and fails on a tag mismatch. The key length (16 / 24 / 32)
/// picks AES-128 / -192 / -256.
fn aes_gcm(ctx: &mut Ctx, _: Value, args: &[Value], _: Block) -> Result<Value> {
    let encrypt = args[0].truthy();
    let key = ctx.str_vec(args[1])?;
    let iv = ctx.str_vec(args[2])?;
    let aad = ctx.str_vec(args[3])?;
    let data = ctx.str_vec(args[4])?;
    let tag = ctx.str_vec(args[5])?;
    if iv.len() != 12 {
        return Err(cipher_err(ctx, "iv must be 12 bytes"));
    }
    let nonce = Nonce::from_slice(&iv);
    // In the RustCrypto API the tag trails the ciphertext.
    let input: Vec<u8> = if encrypt {
        data
    } else {
        if tag.len() != 16 {
            return Err(cipher_err(ctx, "auth_tag must be 16 bytes"));
        }
        let mut v = data;
        v.extend_from_slice(&tag);
        v
    };
    let payload = Payload { msg: &input, aad: &aad };
    let res = match key.len() {
        16 => {
            let c = Aes128Gcm::new_from_slice(&key).unwrap();
            if encrypt { c.encrypt(nonce, payload) } else { c.decrypt(nonce, payload) }
        }
        24 => {
            let c = Aes192Gcm::new_from_slice(&key).unwrap();
            if encrypt { c.encrypt(nonce, payload) } else { c.decrypt(nonce, payload) }
        }
        32 => {
            let c = Aes256Gcm::new_from_slice(&key).unwrap();
            if encrypt { c.encrypt(nonce, payload) } else { c.decrypt(nonce, payload) }
        }
        _ => return Err(cipher_err(ctx, "key must be 16, 24 or 32 bytes")),
    };
    let Ok(out) = res else {
        return Err(cipher_err(ctx, "bad decrypt"));
    };
    if encrypt {
        let (ct, t) = out.split_at(out.len() - 16);
        Ok(ctx.ary_from(&[ctx.bytes(ct), ctx.bytes(t)]))
    } else {
        Ok(ctx.ary_from(&[ctx.bytes(out), ctx.bytes(tag)]))
    }
}

/// String.__aes_cbc(encrypt, key, iv, data) -> out
///
/// AES-CBC with PKCS#7 padding (openssl's default); decrypting fails
/// on a padding error, as openssl does.
fn aes_cbc(ctx: &mut Ctx, _: Value, args: &[Value], _: Block) -> Result<Value> {
    let encrypt = args[0].truthy();
    let key = ctx.str_vec(args[1])?;
    let iv = ctx.str_vec(args[2])?;
    let data = ctx.str_vec(args[3])?;
    if iv.len() != 16 {
        return Err(cipher_err(ctx, "iv must be 16 bytes"));
    }
    macro_rules! run {
        ($aes:ty) => {{
            if encrypt {
                Some(
                    cbc::Encryptor::<$aes>::new_from_slices(&key, &iv)
                        .unwrap()
                        .encrypt_padded_vec_mut::<Pkcs7>(&data),
                )
            } else {
                cbc::Decryptor::<$aes>::new_from_slices(&key, &iv)
                    .unwrap()
                    .decrypt_padded_vec_mut::<Pkcs7>(&data)
                    .ok()
            }
        }};
    }
    let out = match key.len() {
        16 => run!(aes::Aes128),
        24 => run!(aes::Aes192),
        32 => run!(aes::Aes256),
        _ => return Err(cipher_err(ctx, "key must be 16, 24 or 32 bytes")),
    };
    match out {
        Some(out) => Ok(ctx.bytes(out)),
        None => Err(cipher_err(ctx, "bad decrypt")),
    }
}
