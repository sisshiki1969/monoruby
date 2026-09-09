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

require "digest"

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

  # Digests, HMAC and PBKDF2 are real: they run on monoruby's native
  # `Digest` (`String.__digest`, src/builtins/digest.rs), so cookie
  # signing, `ActiveSupport::KeyGenerator` and ETags produce the same
  # bytes as CRuby's openssl. `OpenSSL::Digest` has openssl's class
  # shape (`< ::Digest::Class`, algorithm subclasses, `new("sha256")`).
  class Digest < ::Digest::Class
    class DigestError < OpenSSLError; end

    # Algorithm name (as openssl reports it) => the native class.
    ALGORITHMS = {
      "MD5" => "MD5", "SHA1" => "SHA1", "SHA256" => "SHA256",
      "SHA384" => "SHA384", "SHA512" => "SHA512",
    }.freeze

    def self.canonical_name(name)
      n = name.to_s.upcase.delete("-")
      return n if ALGORITHMS.key?(n)
      raise DigestError, "Unsupported digest algorithm (#{name}).: unknown object name"
    end

    def self.digest(name, data)
      new(name).update(data).digest!
    end

    def self.hexdigest(name, data)
      new(name).update(data).hexdigest!
    end

    def self.base64digest(name, data)
      [digest(name, data)].pack("m0")
    end

    def initialize(name, data = nil)
      @name = Digest.canonical_name(name)
      @impl = ::Digest.const_get(@name).new
      @buffer = "".b
      update(data) if data
    end

    def initialize_copy(other)
      @name = other.name
      @impl = ::Digest.const_get(@name).new
      @buffer = other.instance_variable_get(:@buffer).dup
    end

    def name = @name
    def digest_length = @impl.digest_length
    def block_length = @impl.block_length

    def finish
      String.__digest(@name.downcase, @buffer)
    end
    private :finish

    # Each algorithm class takes the data only; the name is fixed.
    ALGORITHMS.each_key do |algo|
      klass = Class.new(self) do
        define_method(:initialize) { |data = nil| super(algo, data) }
        define_singleton_method(:digest) { |data| new(data).digest! }
        define_singleton_method(:hexdigest) { |data| new(data).hexdigest! }
        define_singleton_method(:base64digest) { |data| [new(data).digest!].pack("m0") }
      end
      const_set(algo, klass)
    end
  end

  class HMAC
    def self.digest(digest, key, data)
      new(key, digest).update(data).digest
    end

    def self.hexdigest(digest, key, data)
      new(key, digest).update(data).hexdigest
    end

    def self.base64digest(digest, key, data)
      [digest(digest, key, data)].pack("m0")
    end

    # `digest` is an algorithm name or an `OpenSSL::Digest` instance (a
    # class is a TypeError, as in openssl).
    def initialize(key, digest)
      @name = HMAC.digest_name(digest)
      @block = ::OpenSSL::Digest.new(@name).block_length
      key = key.b
      key = ::OpenSSL::Digest.digest(@name, key) if key.bytesize > @block
      key = key.ljust(@block, "\0")
      @ipad = key.bytes.map { |b| b ^ 0x36 }.pack("C*")
      @opad = key.bytes.map { |b| b ^ 0x5c }.pack("C*")
      @data = "".b
    end

    def self.digest_name(digest)
      case digest
      when ::OpenSSL::Digest then digest.name
      when ::String, ::Symbol then digest.to_s
      else raise TypeError, "no implicit conversion of #{digest.class} into String"
      end
    end

    def update(data)
      @data << data.b
      self
    end
    alias << update

    def reset
      @data = "".b
      self
    end

    def digest
      inner = ::OpenSSL::Digest.digest(@name, @ipad + @data)
      ::OpenSSL::Digest.digest(@name, @opad + inner)
    end

    def hexdigest = digest.unpack1("H*")
    alias to_s hexdigest
    alias inspect hexdigest

    def base64digest = [digest].pack("m0")

    def ==(other)
      other.is_a?(HMAC) && OpenSSL.fixed_length_secure_compare(digest, other.digest)
    end
  end

  module PKCS5
    # PBKDF2 (RFC 8018 §5.2) over `HMAC`.
    def self.pbkdf2_hmac(pass, salt, iter, keylen, digest)
      name = HMAC.digest_name(digest)
      hlen = ::OpenSSL::Digest.new(name).digest_length
      out = "".b
      block = 1
      while out.bytesize < keylen
        u = HMAC.digest(name, pass, salt.b + [block].pack("N"))
        t = u.bytes
        (iter - 1).times do
          u = HMAC.digest(name, pass, u)
          ub = u.bytes
          t.each_index { |i| t[i] ^= ub[i] }
        end
        out << t.pack("C*")
        block += 1
      end
      out.byteslice(0, keylen)
    end

    def self.pbkdf2_hmac_sha1(pass, salt, iter, keylen)
      pbkdf2_hmac(pass, salt, iter, keylen, "SHA1")
    end
  end

  module KDF
    class KDFError < OpenSSLError; end

    def self.pbkdf2_hmac(pass, salt:, iterations:, length:, hash:)
      PKCS5.pbkdf2_hmac(pass, salt, iterations, length, hash)
    end

    def self.hkdf(ikm, salt:, info:, length:, hash:)
      name = HMAC.digest_name(hash)
      hlen = ::OpenSSL::Digest.new(name).digest_length
      salt = "\0" * hlen if salt.nil? || salt.empty?
      prk = HMAC.digest(name, salt, ikm)
      out = "".b
      t = "".b
      i = 1
      while out.bytesize < length
        t = HMAC.digest(name, prk, t + info.b + [i].pack("C"))
        out << t
        i += 1
      end
      out.byteslice(0, length)
    end

    def self.scrypt(*_args, **_opts)
      raise KDFError, "scrypt is not available in monoruby's openssl"
    end
  end

  # AES, for real: `String.__aes_gcm` / `__aes_cbc` (src/builtins/cipher.rs,
  # the RustCrypto crates) do the work at `final`, over the whole message
  # buffered by `update`. The modes Rails uses are covered: `aes-*-gcm`
  # (cookies, ActiveRecord encryption) and `aes-*-cbc` (the legacy
  # fallback), for 128 / 192 / 256-bit keys, PKCS#7 padding.
  class Cipher
    class CipherError < OpenSSLError; end
    class AuthTagError < CipherError; end

    MODES = { "GCM" => [12, 1], "CBC" => [16, 16] }.freeze # iv_len, block_size

    def self.ciphers
      %w[128 192 256].product(MODES.keys.map(&:downcase)).map { |bits, mode| "aes-#{bits}-#{mode}" }
    end

    def initialize(name)
      n = name.to_s.upcase
      unless n =~ /\AAES-?(128|192|256)-(GCM|CBC)\z/
        raise CipherError, "unsupported cipher algorithm (#{name})"
      end
      @bits = $1.to_i
      @mode = $2
      @name = "AES-#{@bits}-#{@mode}"
      @encrypt = true
      @key = @iv = nil
      @auth_data = "".b
      @auth_tag = nil
      @buffer = "".b
    end

    attr_reader :name

    def key_len = @bits / 8
    def iv_len = MODES[@mode][0]
    def block_size = MODES[@mode][1]
    def authenticated? = @mode == "GCM"

    def encrypt(*)
      @encrypt = true
      reset
    end

    def decrypt(*)
      @encrypt = false
      reset
    end

    def reset
      @buffer = "".b
      self
    end

    def key=(key)
      key = key.b
      raise ArgumentError, "key must be #{key_len} bytes" unless key.bytesize == key_len
      @key = key
    end

    def iv=(iv)
      iv = iv.b
      raise ArgumentError, "iv must be #{iv_len} bytes" unless iv.bytesize == iv_len
      @iv = iv
    end

    def random_key = (self.key = Random.random_bytes(key_len))
    def random_iv = (self.iv = Random.random_bytes(iv_len))

    # PKCS#7 padding is always on (openssl's default); Rails never
    # turns it off.
    def padding=(pad)
      raise CipherError, "disabling padding is not supported" if pad == 0
      pad
    end

    def key_len=(*)
      raise CipherError, "key_len= is not supported for AES"
    end

    def auth_data=(data)
      raise CipherError, "authentication data is only for AEAD ciphers" unless authenticated?
      @auth_data = data.b
    end

    def auth_tag(len = 16)
      raise CipherError, "authentication tag is only for AEAD ciphers" unless authenticated?
      raise CipherError, "retrieving the authentication tag failed" if @auth_tag.nil?
      @auth_tag.byteslice(0, len)
    end

    def auth_tag=(tag)
      raise CipherError, "authentication tag is only for AEAD ciphers" unless authenticated?
      @auth_tag = tag.b
    end

    # The whole message is transformed at `final`; `update` only
    # buffers (its "" answer concatenated with `final` is the same
    # stream openssl produces piecewise).
    def update(data, buffer = nil)
      raise CipherError, "key not set" if @key.nil?
      @buffer << data.b
      out = "".b
      buffer ? buffer.replace(out) : out
    end
    alias << update

    def final
      raise CipherError, "key not set" if @key.nil?
      iv = @iv || ("\0" * iv_len).b
      if @mode == "GCM"
        tag = @auth_tag || "".b
        if !@encrypt && tag.bytesize != 16
          raise AuthTagError, "tag must be 16 bytes"
        end
        out, tag = String.__aes_gcm(@encrypt, @key, iv, @auth_data, @buffer, tag)
        @auth_tag = tag if @encrypt
      else
        out = String.__aes_cbc(@encrypt, @key, iv, @buffer)
      end
      @buffer = "".b
      out
    rescue RuntimeError => e
      raise e unless e.message.start_with?("__cipher__:")
      msg = e.message.delete_prefix("__cipher__:")
      raise(@mode == "GCM" && !@encrypt ? AuthTagError : CipherError, msg)
    end

    # `OpenSSL::Cipher::AES.new(256, :GCM)` / `AES256.new(:GCM)` /
    # `AES.new("256-GCM")`.
    class AES < Cipher
      def initialize(*args)
        super("aes-#{args.join("-")}")
      end
    end

    %w[128 192 256].each do |bits|
      const_set("AES#{bits}", Class.new(Cipher) do
        define_method(:initialize) { |mode| super("aes-#{bits}-#{mode}") }
      end)
    end
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
    # net/http's rescue clauses name these even for plain-HTTP requests;
    # a missing constant turns any transport error into a NameError.
    class SSLError < OpenSSLError; end
    class SSLErrorWaitReadable < SSLError; end
    class SSLErrorWaitWritable < SSLError; end
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
