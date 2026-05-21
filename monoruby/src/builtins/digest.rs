use super::*;
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

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_func(STRING_CLASS, "__digest", digest_hash, 2);
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
}
