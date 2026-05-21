# frozen_string_literal: true
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
  # (defaults to 256). Delegates to the fixed-width algorithm above.
  class SHA2 < Base
    def initialize(bitlen = 256)
      @algorithm, @digest_length, @block_length =
        case bitlen
        when 256 then ['sha256', 32, 64]
        when 384 then ['sha384', 48, 128]
        when 512 then ['sha512', 64, 128]
        else raise ArgumentError, "unsupported bit length: #{bitlen.inspect}"
        end
      @bitlen = bitlen
      super()
    end

    def finish
      String.__digest(@algorithm, @buffer)
    end
    private :finish

    def digest_length
      @digest_length
    end
    alias length digest_length
    alias size digest_length

    def block_length
      @block_length
    end

    def inspect
      "#<#{self.class.name}:#{@bitlen}: #{hexdigest}>"
    end
  end
end
