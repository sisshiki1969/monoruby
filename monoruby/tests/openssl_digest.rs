extern crate monoruby;
use monoruby::tests::*;

// `stdlib/openssl.rb`: `OpenSSL::Digest`, `OpenSSL::HMAC`,
// `OpenSSL::PKCS5.pbkdf2_hmac` and `OpenSSL::KDF` are real, computed on
// monoruby's native `Digest`, and answer byte for byte what CRuby's
// openssl does (Rails signs cookies and derives keys through them).

#[test]
fn openssl_digest_hmac_and_kdf_match_cruby() {
    run_test_once(
        r##"
        require "openssl"
        r = []
        r << OpenSSL::Digest.ancestors.first(3).map(&:to_s) << OpenSSL::Digest::SHA256.superclass.to_s
        r << OpenSSL::Digest::SHA256.hexdigest("abc") << OpenSSL::Digest.new("sha256").name
        r << OpenSSL::Digest.new("SHA-256").hexdigest("abc") << OpenSSL::Digest::SHA256.new("abc").hexdigest
        r << [OpenSSL::Digest::SHA256.new.block_length, OpenSSL::Digest::SHA256.new.digest_length]
        r << OpenSSL::Digest.digest("SHA256", "abc").bytesize << OpenSSL::Digest.hexdigest("MD5", "abc")
        r << OpenSSL::Digest::MD5.base64digest("a") << OpenSSL::Digest.base64digest("SHA1", "abc")
        r << OpenSSL::Digest::SHA512.hexdigest("x") << OpenSSL::Digest::SHA384.hexdigest("x") << OpenSSL::Digest::SHA1.hexdigest("x")
        d = OpenSSL::Digest::SHA1.new; d << "ab"; e = d.dup; e << "c"
        r << d.hexdigest << e.hexdigest << d.reset.update("abc").hexdigest
        r << (begin; OpenSSL::Digest.new("nope"); rescue OpenSSL::Digest::DigestError => e; e.class.to_s; end)
        r << OpenSSL::HMAC.hexdigest("SHA256", "key", "data")
        r << OpenSSL::HMAC.hexdigest(OpenSSL::Digest::SHA256.new, "key" * 30, "data")
        r << OpenSSL::HMAC.digest("SHA1", "k", "d").bytesize
        r << (begin; OpenSSL::HMAC.hexdigest(OpenSSL::Digest::SHA1, "k", "d"); rescue TypeError => e; e.message; end)
        r << OpenSSL::HMAC.new("key", "SHA256").update("da").update("ta").hexdigest
        r << OpenSSL::HMAC.base64digest("SHA256", "key", "data")
        h = OpenSSL::HMAC.new("k", "SHA256"); h << "z"
        r << h.reset.hexdigest << (h.to_s == h.hexdigest) << (h == OpenSSL::HMAC.new("k", "SHA256"))
        r << OpenSSL::PKCS5.pbkdf2_hmac("pass", "salt", 1000, 32, "SHA256").unpack1("H*")
        r << OpenSSL::PKCS5.pbkdf2_hmac_sha1("p", "s", 3, 20).unpack1("H*")
        r << OpenSSL::KDF.pbkdf2_hmac("pass", salt: "salt", iterations: 2, length: 50, hash: "SHA1").unpack1("H*")
        r << OpenSSL::KDF.hkdf("ikm", salt: "s", info: "i", length: 42, hash: "SHA256").unpack1("H*")
        r << OpenSSL.fixed_length_secure_compare("ab", "ab") << OpenSSL.secure_compare("a", "b")
        r
        "##,
    );
}
