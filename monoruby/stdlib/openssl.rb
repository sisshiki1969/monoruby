# Minimal OpenSSL stub for monoruby.
#
# The real openssl is a C extension. ActiveRecord::Encryption requires it
# for random bytes + AES-GCM, but the module is only referenced during
# class loading; the code paths that actually perform encryption are not
# triggered by `establish_connection` or basic CRUD.
#
# We provide empty shells for the constants and a `Random.random_bytes`
# that falls back to SecureRandom so that anything relying on it for
# tokens keeps working.

module OpenSSL
  VERSION = "3.0.0"
  OPENSSL_VERSION = "OpenSSL 3.0.0 (stub)"
  OPENSSL_VERSION_NUMBER = 0x30000000
  OPENSSL_LIBRARY_VERSION = OPENSSL_VERSION

  class OpenSSLError < StandardError; end

  module Random
    def self.random_bytes(n)
      # Fall back to SecureRandom if available, otherwise fabricate bytes
      # from Kernel#rand. Either way, this is NOT cryptographically safe;
      # it exists purely to let AR finish loading.
      if defined?(SecureRandom)
        SecureRandom.random_bytes(n)
      else
        Array.new(n) { rand(256) }.pack("C*")
      end
    end

    def self.pseudo_bytes(n); random_bytes(n); end
  end

  module Digest
    class Digest
      def initialize(*); end
      def update(*); self; end
      def digest; ""; end
      def hexdigest; ""; end
      alias << update
    end
    class SHA1   < Digest; end
    class SHA256 < Digest; end
    class SHA384 < Digest; end
    class SHA512 < Digest; end
    class MD5    < Digest; end
  end

  module HMAC
    def self.digest(_digest, _key, _data); ""; end
    def self.hexdigest(_digest, _key, _data); ""; end
  end

  module KDF
    def self.pbkdf2_hmac(_pass, salt:, iterations:, length:, hash:)
      "\x00" * length
    end
    def self.scrypt(*_args, **_opts)
      length = _opts[:length] || 0
      "\x00" * length
    end
    def self.hkdf(_ikm, salt:, info:, length:, hash:)
      "\x00" * length
    end
  end

  module Cipher
    class CipherError < OpenSSLError; end

    class Cipher
      def initialize(*); end
      def encrypt; self; end
      def decrypt; self; end
      def key=(*); end
      def iv=(*); end
      def iv_len; 12; end
      def auth_data=(*); end
      def auth_tag; ""; end
      def auth_tag=(*); end
      def update(data); data; end
      def final; ""; end
      def random_key; "\x00" * 32; end
      def random_iv; "\x00" * 12; end
    end

    def self.new(name)
      Cipher.new(name)
    end

    class AES < Cipher; end
  end

  module PKey
    class PKey
      def initialize(*); end
    end
    class RSA < PKey
      def self.generate(*); new; end
      def public_key; self; end
      def private_key; self; end
      def to_pem; ""; end
    end
    class EC < PKey
      def self.generate(*); new; end
    end
    class DH < PKey; end
    class DSA < PKey; end
  end

  module X509
    class Certificate
      def initialize(*); end
    end
    class Name
      def initialize(*); end
    end
    class Store
      def initialize(*); end
      def add_cert(*); self; end
      def add_file(*); self; end
      def add_path(*); self; end
      def verify(*); true; end
    end
    class ExtensionFactory; end
    class Extension; end
    class Request; end
    class Revoked; end
    class CRL; end
    class Attribute; end
  end

  module SSL
    class SSLContext
      def initialize(*); end
      def set_params(*); end
      def min_version=(*); end
      def max_version=(*); end
      def verify_mode=(*); end
      def cert_store=(*); end
    end
    class SSLSocket
      def initialize(*); end
    end
    VERIFY_NONE = 0
    VERIFY_PEER = 1
  end

  def self.secure_compare(a, b)
    return false unless a.bytesize == b.bytesize
    a == b
  end

  def self.fixed_length_secure_compare(a, b)
    secure_compare(a, b)
  end
end
