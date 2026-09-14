use super::*;
use aes_gcm::aead::{Aead, KeyInit, Payload};
use aes_gcm::{Aes128Gcm, Aes256Gcm, AesGcm, Nonce};
use cbc::cipher::block_padding::Pkcs7;
use cbc::cipher::{BlockDecryptMut, BlockEncryptMut, KeyIvInit};

//
// AES — the native half of `OpenSSL::Cipher` (stdlib/openssl.rb).
//
// The Ruby side buffers the whole message (`update`) and runs one of
// these one-shot helpers at `final`, so the native state is a call, not
// an object: the modes Rails uses (`aes-*-gcm` for cookies and
// ActiveRecord encryption, `aes-*-cbc` as the legacy fallback) are
// both computed over the complete input anyway (GCM needs it for the
// tag, CBC for the padding). Implemented with the RustCrypto crates
// (`aes-gcm`, `aes` + `cbc`), so output is byte-identical to openssl.
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_func(STRING_CLASS, "__aes_gcm", aes_gcm, 6);
    globals.define_builtin_class_func(STRING_CLASS, "__aes_cbc", aes_cbc, 4);
}

fn cipher_err(msg: &str) -> MonorubyErr {
    // Raised as the Ruby-side `OpenSSL::Cipher::CipherError`.
    MonorubyErr::runtimeerr(format!("__cipher__:{msg}"))
}

type Aes192Gcm = AesGcm<aes::Aes192, aes_gcm::aead::consts::U12>;

/// String.__aes_gcm(encrypt, key, iv, aad, data, tag) -> [out, tag]
///
/// AES-GCM with a 96-bit nonce. Encrypting answers `[ciphertext,
/// 16-byte tag]` (`tag` ignored); decrypting answers `[plaintext,
/// tag]` and fails on a tag mismatch. The key length (16 / 24 / 32)
/// picks AES-128 / -192 / -256.
#[monoruby_builtin]
fn aes_gcm(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let encrypt = lfp.arg(0).as_bool();
    let (key_v, iv_v, aad_v, data_v, tag_v) =
        (lfp.arg(1), lfp.arg(2), lfp.arg(3), lfp.arg(4), lfp.arg(5));
    let key = key_v.expect_bytes(&globals.store)?;
    let iv = iv_v.expect_bytes(&globals.store)?;
    let aad = aad_v.expect_bytes(&globals.store)?;
    let data = data_v.expect_bytes(&globals.store)?;
    let tag = tag_v.expect_bytes(&globals.store)?;
    if iv.len() != 12 {
        return Err(cipher_err("iv must be 12 bytes"));
    }
    let nonce = Nonce::from_slice(iv);
    // In the RustCrypto API the tag trails the ciphertext.
    let input: Vec<u8> = if encrypt {
        data.to_vec()
    } else {
        if tag.len() != 16 {
            return Err(cipher_err("auth_tag must be 16 bytes"));
        }
        let mut v = Vec::with_capacity(data.len() + 16);
        v.extend_from_slice(data);
        v.extend_from_slice(tag);
        v
    };
    let payload = Payload { msg: &input, aad };
    let res = match key.len() {
        16 => {
            let c = Aes128Gcm::new_from_slice(key).unwrap();
            if encrypt { c.encrypt(nonce, payload) } else { c.decrypt(nonce, payload) }
        }
        24 => {
            let c = Aes192Gcm::new_from_slice(key).unwrap();
            if encrypt { c.encrypt(nonce, payload) } else { c.decrypt(nonce, payload) }
        }
        32 => {
            let c = Aes256Gcm::new_from_slice(key).unwrap();
            if encrypt { c.encrypt(nonce, payload) } else { c.decrypt(nonce, payload) }
        }
        _ => return Err(cipher_err("key must be 16, 24 or 32 bytes")),
    };
    let out = match res {
        Ok(out) => out,
        Err(_) => return Err(cipher_err("bad decrypt")),
    };
    if encrypt {
        let (ct, t) = out.split_at(out.len() - 16);
        Ok(Value::array2(Value::bytes(ct.to_vec()), Value::bytes(t.to_vec())))
    } else {
        Ok(Value::array2(Value::bytes(out), Value::bytes(tag.to_vec())))
    }
}

/// String.__aes_cbc(encrypt, key, iv, data) -> out
///
/// AES-CBC with PKCS#7 padding (openssl's default); decrypting fails
/// on a padding error, as openssl does.
#[monoruby_builtin]
fn aes_cbc(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let encrypt = lfp.arg(0).as_bool();
    let (key_v, iv_v, data_v) = (lfp.arg(1), lfp.arg(2), lfp.arg(3));
    let key = key_v.expect_bytes(&globals.store)?;
    let iv = iv_v.expect_bytes(&globals.store)?;
    let data = data_v.expect_bytes(&globals.store)?;
    if iv.len() != 16 {
        return Err(cipher_err("iv must be 16 bytes"));
    }
    macro_rules! run {
        ($aes:ty) => {{
            if encrypt {
                Ok(cbc::Encryptor::<$aes>::new_from_slices(key, iv)
                    .unwrap()
                    .encrypt_padded_vec_mut::<Pkcs7>(data))
            } else {
                cbc::Decryptor::<$aes>::new_from_slices(key, iv)
                    .unwrap()
                    .decrypt_padded_vec_mut::<Pkcs7>(data)
                    .map_err(|_| cipher_err("bad decrypt"))
            }
        }};
    }
    let out = match key.len() {
        16 => run!(aes::Aes128),
        24 => run!(aes::Aes192),
        32 => run!(aes::Aes256),
        _ => return Err(cipher_err("key must be 16, 24 or 32 bytes")),
    }?;
    Ok(Value::bytes(out))
}
