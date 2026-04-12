# Minimal Zlib stub for monoruby.
#
# Zlib is a C extension (zlib.so) that monoruby cannot load. ActiveRecord
# only uses Zlib.crc32 for a couple of hash helpers (fixtures.rb,
# migration.rb), so this stub provides a pure-Ruby CRC-32 plus no-op
# versions of the surface area commonly referenced by Rails.

module Zlib
  VERSION = "3.1.0"
  ZLIB_VERSION = "1.3"

  class Error < StandardError; end
  class StreamError < Error; end
  class DataError < Error; end
  class BufError < Error; end
  class VersionError < Error; end
  class MemError < Error; end
  class NeedDict < Error; end

  # Standard CRC-32 (polynomial 0xEDB88320), computed lazily.
  CRC_TABLE = begin
    table = Array.new(256)
    256.times do |i|
      c = i
      8.times { c = (c & 1 == 1 ? (0xEDB88320 ^ (c >> 1)) : (c >> 1)) }
      table[i] = c
    end
    table.freeze
  end

  def self.crc32(string = "", crc = 0)
    c = crc ^ 0xFFFFFFFF
    string.to_s.each_byte do |b|
      c = CRC_TABLE[(c ^ b) & 0xFF] ^ (c >> 8)
    end
    c ^ 0xFFFFFFFF
  end

  def self.adler32(string = "", adler = 1)
    s1 = adler & 0xFFFF
    s2 = (adler >> 16) & 0xFFFF
    string.to_s.each_byte do |b|
      s1 = (s1 + b) % 65521
      s2 = (s2 + s1) % 65521
    end
    (s2 << 16) | s1
  end

  def self.crc32_combine(crc1, crc2, len2)
    crc1 ^ crc2
  end

  def self.adler32_combine(adler1, adler2, len2)
    adler1 ^ adler2
  end

  class Deflate
    def self.deflate(string, level = nil)
      string.dup
    end
  end

  class Inflate
    def self.inflate(string)
      string.dup
    end
  end

  class GzipFile
    class Error < Zlib::Error; end
    class CRCError < Error; end
    class NoFooter < Error; end
    class LengthError < Error; end
  end

  class GzipReader < GzipFile
    def self.open(filename, &block)
      File.open(filename, "rb", &block)
    end
  end

  class GzipWriter < GzipFile
    def self.open(filename, level = nil, &block)
      File.open(filename, "wb", &block)
    end
  end
end
