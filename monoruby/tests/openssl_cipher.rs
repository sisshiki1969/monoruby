extern crate monoruby;
use monoruby::tests::*;

// `OpenSSL::Cipher` (stdlib/openssl.rb over src/builtins/cipher.rs): AES
// GCM / CBC for 128 / 192 / 256-bit keys, byte-identical to openssl,
// with openssl's class shape (`AES256.new(:GCM)`, `CipherError`,
// `AuthTagError` on a tag mismatch). Rails' cookie jar
// (`ActiveSupport::MessageEncryptor`, aes-256-gcm) runs on this.

#[test]
fn openssl_cipher_aes_matches_cruby() {
    run_test_once(
        r##"
        require "openssl"
        r = []
        c = OpenSSL::Cipher.new("aes-256-gcm")
        r << [c.name, c.key_len, c.iv_len, c.block_size, c.authenticated?]
        c2 = OpenSSL::Cipher.new("aes-256-cbc")
        r << [c2.name, c2.key_len, c2.iv_len, c2.block_size, c2.authenticated?]
        r << OpenSSL::Cipher::AES256.new(:GCM).name << OpenSSL::Cipher::AES.new(128, :CBC).name << OpenSSL::Cipher::AES.new("256-GCM").name
        r << (begin; OpenSSL::Cipher.new("nope"); rescue OpenSSL::Cipher::CipherError => e; e.class.to_s; end)
        r << OpenSSL::Cipher.ciphers.include?("aes-256-gcm")
        key = "k" * 32
        iv = "i" * 12
        c = OpenSSL::Cipher.new("aes-256-gcm").encrypt
        c.key = key
        c.iv = iv
        c.auth_data = "aad"
        ct = c.update("hello world") + c.final
        r << ct.unpack1("H*") << c.auth_tag.unpack1("H*") << c.auth_tag(12).bytesize << ct.encoding.to_s
        d = OpenSSL::Cipher.new("aes-256-gcm").decrypt
        d.key = key
        d.iv = iv
        d.auth_tag = c.auth_tag
        d.auth_data = "aad"
        r << d.update(ct) + d.final
        d = OpenSSL::Cipher.new("aes-256-gcm").decrypt
        d.key = key
        d.iv = iv
        d.auth_tag = "x" * 16
        d.auth_data = "aad"
        r << (begin; d.update(ct) + d.final; rescue OpenSSL::Cipher::CipherError => e; e.class.to_s; end)
        # Empty plaintext, empty aad; 128- and 192-bit keys.
        [16, 24].each do |klen|
          e = OpenSSL::Cipher.new("aes-#{klen * 8}-gcm").encrypt
          e.key = "q" * klen
          e.iv = "n" * 12
          e.auth_data = ""
          out = e.update("") + e.final
          r << [out, e.auth_tag.unpack1("H*")]
        end
        c = OpenSSL::Cipher.new("aes-128-cbc").encrypt
        c.key = "k" * 16
        c.iv = "i" * 16
        ct = c.update("hello world, cbc!") + c.final
        r << ct.unpack1("H*")
        d = OpenSSL::Cipher.new("aes-128-cbc").decrypt
        d.key = "k" * 16
        d.iv = "i" * 16
        r << d.update(ct) + d.final
        d = OpenSSL::Cipher.new("aes-128-cbc").decrypt
        d.key = "z" * 16
        d.iv = "i" * 16
        r << (begin; d.update(ct) + d.final; rescue OpenSSL::Cipher::CipherError => e; e.class.to_s; end)
        c = OpenSSL::Cipher.new("aes-256-cbc").encrypt
        c.key = "k" * 32
        c.iv = "i" * 16
        r << (c.update("0123456789abcdef") + c.final).unpack1("H*")
        r << (begin; c.key = "short"; rescue ArgumentError => e; e.message; end)
        r << (begin; c.iv = "short"; rescue ArgumentError => e; e.message; end)
        e = OpenSSL::Cipher.new("aes-256-gcm").encrypt
        r << e.random_key.bytesize << e.random_iv.bytesize
        r << OpenSSL::Cipher::CipherError.ancestors.include?(OpenSSL::OpenSSLError)
        r << OpenSSL::Cipher::AuthTagError.superclass.to_s
        r
        "##,
    );
}

#[test]
fn message_encryptor_round_trip_like_rails() {
    // The exact shape of ActiveSupport::MessageEncryptor's aes-256-gcm
    // path: PBKDF2-derived key, random iv, empty aad, 16-byte tag,
    // and a decrypt that must reproduce the message.
    run_test_once(
        r##"
        require "openssl"
        secret = OpenSSL::PKCS5.pbkdf2_hmac("secret_key_base", "encrypted cookie", 1000, 32, "SHA256")
        cipher = OpenSSL::Cipher.new("aes-256-gcm")
        cipher.encrypt
        cipher.key = secret
        iv = cipher.random_iv
        cipher.auth_data = ""
        data = cipher.update('{"session_id":"abc","flash":{"notice":"ok"}}')
        data << cipher.final
        tag = cipher.auth_tag(16)
        dec = OpenSSL::Cipher.new("aes-256-gcm")
        dec.decrypt
        dec.key = secret
        dec.iv = iv
        dec.auth_tag = tag
        dec.auth_data = ""
        out = dec.update(data)
        out << dec.final
        [secret.unpack1("H*"), iv.bytesize, tag.bytesize, data.bytesize, out]
        "##,
    );
}
