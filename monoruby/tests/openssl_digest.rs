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

// HMAC and PBKDF2 are computed natively (`String.__hmac` /
// `String.__pbkdf2_hmac`). The key-length boundaries are the interesting
// part: HMAC hashes a key longer than the block and zero-pads a shorter
// one, which used to be done in Ruby and is now the crate's job.
#[test]
fn openssl_hmac_key_lengths_match_cruby() {
    run_test_once(
        r##"
        require "openssl"
        r = []
        %w[MD5 SHA1 SHA256 SHA384 SHA512].each do |algo|
          block = OpenSSL::Digest.new(algo).block_length
          ["", "k", "a" * (block - 1), "a" * block, "a" * (block + 1), "a" * 200, "\xff\x00\x01".b].each do |key|
            ["", "abc", "\x00\xff".b, "x" * 1000].each do |data|
              r << OpenSSL::HMAC.hexdigest(algo, key, data)
            end
          end
        end
        r << OpenSSL::HMAC.hexdigest("sha-256", "k", "d")
        r << OpenSSL::HMAC.hexdigest("sha256", "k", "d")
        r << OpenSSL::HMAC.digest("SHA256", "k", "d").encoding.to_s
        r
        "##,
    );
}

// PBKDF2 against the RFC 6070 vectors plus the shapes Rails asks for.
#[test]
fn openssl_pbkdf2_matches_cruby() {
    run_test_once(
        r##"
        require "openssl"
        r = []
        [
          ["password", "salt", 1, 20, "SHA1"],
          ["password", "salt", 2, 20, "SHA1"],
          ["password", "salt", 4096, 20, "SHA1"],
          ["passwordPASSWORDpassword", "saltSALTsaltSALTsaltSALTsaltSALTsalt", 4096, 25, "SHA1"],
          ["pass\0word", "sa\0lt", 4096, 16, "SHA1"],
          ["password", "salt", 1, 32, "SHA256"],
          ["password", "salt", 1000, 64, "SHA256"],
          ["p", "s", 10, 0, "SHA256"],
          ["p", "s", 10, 1, "SHA512"],
          ["p", "s", 10, 100, "MD5"],
          ["", "", 5, 16, "SHA256"],
        ].each do |pass, salt, iter, len, algo|
          d = OpenSSL::PKCS5.pbkdf2_hmac(pass, salt, iter, len, algo)
          r << [d.unpack1("H*"), d.bytesize, d.encoding.to_s]
        end
        r << OpenSSL::PKCS5.pbkdf2_hmac("p", "s", 5, 20, OpenSSL::Digest.new("SHA1")).unpack1("H*")
        r << OpenSSL::KDF.hkdf("ikm", salt: "", info: "", length: 16, hash: "SHA1").unpack1("H*")
        r
        "##,
    );
}

// The native helpers reject an algorithm they do not implement rather
// than hashing with the wrong one.
#[test]
fn native_hmac_pbkdf2_reject_unknown_algorithm() {
    run_test_error(r#"String.__hmac("bogus", "k", "d")"#);
    run_test_error(r#"String.__pbkdf2_hmac("bogus", "p", "s", 1, 16)"#);
    run_test_error(r#"String.__pbkdf2_hmac("sha256", "p", "s", 1, -1)"#);
}
