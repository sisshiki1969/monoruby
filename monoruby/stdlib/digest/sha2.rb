# frozen_string_literal: true
# monoruby-hidden-ivars: true
require 'digest'

module Digest
  class SHA256 < Base
    ALGORITHM = 'sha256'
    DIGEST_LENGTH = 32
    BLOCK_LENGTH = 64
  end

  class SHA384 < Base
    ALGORITHM = 'sha384'
    DIGEST_LENGTH = 48
    BLOCK_LENGTH = 128
  end

  class SHA512 < Base
    ALGORITHM = 'sha512'
    DIGEST_LENGTH = 64
    BLOCK_LENGTH = 128
  end

  # Digest::SHA2.new(bitlen) — CRuby's bit-length-parameterized wrapper
  # (ext/digest/sha2/lib/sha2.rb, as it is): it delegates to an instance
  # of the fixed-width class, kept in `@sha2` (#2006).
  class SHA2 < Digest::Class
    def initialize(bitlen = 256)
      case bitlen
      when 256
        @sha2 = Digest::SHA256.new
      when 384
        @sha2 = Digest::SHA384.new
      when 512
        @sha2 = Digest::SHA512.new
      else
        raise ArgumentError, "unsupported bit length: %s" % bitlen.inspect
      end
      @bitlen = bitlen
    end

    def reset
      @sha2.reset
      self
    end

    def update(str)
      @sha2.update(str)
      self
    end
    alias << update

    def finish # :nodoc:
      @sha2.digest!
    end
    private :finish

    def block_length
      @sha2.block_length
    end

    def digest_length
      @sha2.digest_length
    end

    def initialize_copy(other) # :nodoc:
      @sha2 = other.instance_eval { @sha2.clone }
    end

    def inspect # :nodoc:
      "#<%s:%d %s>" % [self.class.name, @bitlen, hexdigest]
    end
  end
end
