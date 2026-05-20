# Minimal Zlib stub for monoruby.
#
# Zlib is a C extension (zlib.so) that monoruby cannot load. ActiveRecord
# only uses Zlib.crc32 for a couple of hash helpers (fixtures.rb,
# migration.rb), so this stub provides a pure-Ruby CRC-32 plus no-op
# versions of the surface area commonly referenced by Rails.

module Zlib
  VERSION = "3.1.0"
  ZLIB_VERSION = "1.3"

  # Compression levels (zlib.h Z_NO_COMPRESSION ... Z_BEST_COMPRESSION).
  NO_COMPRESSION      = 0
  BEST_SPEED          = 1
  DEFAULT_COMPRESSION = -1
  BEST_COMPRESSION    = 9

  # Compression strategies (zlib.h Z_FILTERED ... Z_DEFAULT_STRATEGY).
  FILTERED         = 1
  HUFFMAN_ONLY     = 2
  RLE              = 3
  FIXED            = 4
  DEFAULT_STRATEGY = 0

  # Flush values (zlib.h Z_NO_FLUSH ... Z_FINISH).
  NO_FLUSH    = 0
  SYNC_FLUSH  = 2
  FULL_FLUSH  = 3
  FINISH      = 4

  # Window bits and memory level (zlib.h MAX_WBITS, DEF_MEM_LEVEL, MAX_MEM_LEVEL).
  MAX_WBITS     = 15
  DEF_MEM_LEVEL = 8
  MAX_MEM_LEVEL = 9

  # Data type hints (zlib.h Z_BINARY ... Z_UNKNOWN).
  BINARY  = 0
  TEXT    = 1
  ASCII   = 1
  UNKNOWN = 2

  # gzip OS codes (RFC 1952 §2.3.1).
  OS_MSDOS   = 0
  OS_AMIGA   = 1
  OS_VMS     = 2
  OS_UNIX    = 3
  OS_VMCMS   = 4
  OS_ATARI   = 5
  OS_OS2     = 6
  OS_MACOS   = 7
  OS_ZSYSTEM = 8
  OS_CPM     = 9
  OS_TOPS20  = 10
  OS_WIN32   = 11
  OS_QDOS    = 12
  OS_RISCOS  = 13
  OS_UNKNOWN = 255
  OS_CODE    = OS_UNIX

  class Error < StandardError; end
  class StreamError < Error; end
  class DataError < Error; end
  class BufError < Error; end
  class VersionError < Error; end
  class MemError < Error; end
  class NeedDict < Error; end
  class StreamEnd < Error; end
  class InProgressError < Error; end

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
