# frozen_string_literal: true
#
# SecureRandom implementation for monoruby.
# Uses /dev/urandom on Linux for randomness.
#

module SecureRandom
  def self.random_bytes(n = nil)
    n = n ? n.to_i : 16
    raise ArgumentError, "negative string size" if n < 0
    return '' if n == 0

    # Read from /dev/urandom
    File.open('/dev/urandom', 'rb') do |f|
      f.read(n)
    end
  end

  def self.hex(n = nil)
    n = n ? n.to_i : 16
    random_bytes(n).unpack('H*')[0]
  end

  def self.uuid
    # Version 4 UUID (random)
    bytes = random_bytes(16)
    ary = bytes.unpack('NnnnnN')

    # Set version to 4
    ary[2] = (ary[2] & 0x0fff) | 0x4000
    # Set variant to RFC 4122
    ary[3] = (ary[3] & 0x3fff) | 0x8000

    "%08x-%04x-%04x-%04x-%04x%08x" % ary
  end

  def self.random_number(n = 0)
    if n.is_a?(Float) || (n.is_a?(Integer) && n == 0)
      # Return a random float in [0.0, 1.0)
      bytes = random_bytes(8)
      # Use 53 bits for double precision
      hi = bytes[0, 4].unpack('N')[0]
      lo = bytes[4, 4].unpack('N')[0]
      ((hi >> 5) * (1 << 26) + (lo >> 6)).to_f / (1 << 53)
    elsif n.is_a?(Range)
      lo = n.min
      hi = n.max
      lo + random_number(hi - lo + 1)
    else
      n = n.to_i
      raise ArgumentError, "invalid argument - #{n}" if n < 0
      return 0 if n == 0

      # Generate a random integer in [0, n)
      # Simple rejection sampling
      bytes_needed = 0
      temp = n - 1
      while temp > 0
        bytes_needed += 1
        temp >>= 8
      end
      bytes_needed = 1 if bytes_needed == 0

      loop do
        result = 0
        random_bytes(bytes_needed).each_byte do |b|
          result = (result << 8) | b
        end
        return result % n if result < n * 2  # Simple bias reduction
      end
    end
  end

  def self.alphanumeric(n = nil)
    n = n ? n.to_i : 16
    chars = ('A'..'Z').to_a + ('a'..'z').to_a + ('0'..'9').to_a
    result = ''
    while result.length < n
      random_bytes(n).each_byte do |b|
        break if result.length >= n
        idx = b % 64
        result << chars[idx] if idx < chars.length
      end
    end
    result[0, n]
  end

  def self.base64(n = nil)
    n = n ? n.to_i : 16
    # Simple Base64 encoding
    bytes = random_bytes(n)
    _encode_base64(bytes)
  end

  def self.urlsafe_base64(n = nil, padding = false)
    s = base64(n)
    s = s.tr('+/', '-_')
    s = s.delete('=') unless padding
    s
  end

  private

  B64_CHARS = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/'

  def self._encode_base64(data)
    result = ''
    i = 0
    while i < data.length
      b0 = data.getbyte(i) || 0
      b1 = (i + 1 < data.length) ? data.getbyte(i + 1) : 0
      b2 = (i + 2 < data.length) ? data.getbyte(i + 2) : 0

      result << B64_CHARS[(b0 >> 2) & 0x3f]
      result << B64_CHARS[((b0 << 4) | (b1 >> 4)) & 0x3f]

      if i + 1 < data.length
        result << B64_CHARS[((b1 << 2) | (b2 >> 6)) & 0x3f]
      else
        result << '='
      end

      if i + 2 < data.length
        result << B64_CHARS[b2 & 0x3f]
      else
        result << '='
      end

      i += 3
    end
    result
  end
end
