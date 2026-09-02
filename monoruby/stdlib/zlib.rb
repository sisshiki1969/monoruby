# Zlib for monoruby.
#
# Zlib is a C extension (zlib.so) that monoruby cannot load, so this file
# provides the module in Ruby. What it offers:
#
# - `Zlib.crc32` / `Zlib.adler32` (and the `_combine` variants) with the
#   real argument semantics, over the native byte walk in
#   `src/builtins/zlib.rs` (`String.__crc32` / `String.__adler32`).
# - `Zlib::Deflate`, which writes a genuine zlib stream — header, stored
#   (uncompressed) deflate blocks, Adler-32 trailer — that any zlib can
#   inflate. No compression is performed at any level; the level only
#   picks the header's FLEVEL bits and is validated as zlib would.
# - `Zlib::Inflate`, a complete RFC 1951 decoder (stored, fixed-Huffman
#   and dynamic-Huffman blocks) inside the RFC 1950 wrapper, so streams
#   produced by a real zlib (PNG image data, compressed text chunks)
#   decode correctly.
# - The streaming `Deflate.new` / `Inflate.new` objects, buffered: input
#   accumulates and the whole stream is processed at `finish`.
#
# Not provided: gzip framing (`GzipReader` / `GzipWriter` fall back to a
# plain file), preset dictionaries, and incremental output before
# `finish`.

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

  # ---------------------------------------------------------------------
  # Checksums
  #
  # `do_checksum` in zlib.c: the seed is `NUM2ULONG`'d (so it is reduced
  # to 32 bits and a Float is truncated), a nil string yields the
  # checksum of nothing regardless of the seed (0 for CRC-32, 1 for
  # Adler-32), anything else goes through `to_str`.

  def self.crc32(string = nil, crc = nil)
    if string.nil?
      0
    else
      String.__crc32(__checksum_input(string), crc.nil? ? 0 : __checksum_seed(crc))
    end
  end

  def self.adler32(string = nil, adler = nil)
    if string.nil?
      1
    else
      String.__adler32(__checksum_input(string), adler.nil? ? 1 : __checksum_seed(adler))
    end
  end

  def self.__checksum_input(string)
    return string if string.is_a?(String)
    converted = String.try_convert(string)
    if converted.nil?
      raise TypeError, "no implicit conversion of #{string.nil? ? "nil" : string.class} into String"
    end
    converted
  end

  def self.__checksum_seed(seed)
    unless seed.is_a?(Integer)
      seed = seed.to_int if seed.is_a?(Float)
      unless seed.is_a?(Integer)
        raise TypeError, "no implicit conversion of #{seed.class} into Integer"
      end
    end
    seed & 0xFFFFFFFF
  end

  # `crc32_combine` (crc32.c): advance `crc1` over `len2` zero bytes with
  # GF(2) matrix exponentiation, then fold `crc2` in (a zero `len2`
  # still folds, as in zlib 1.3).
  def self.crc32_combine(crc1, crc2, len2)
    len2 = 0 if len2 < 0
    odd = Array.new(32, 0)
    even = Array.new(32, 0)
    odd[0] = 0xEDB88320
    row = 1
    n = 1
    while n < 32
      odd[n] = row
      row <<= 1
      n += 1
    end
    __gf2_matrix_square(even, odd)
    __gf2_matrix_square(odd, even)
    loop do
      __gf2_matrix_square(even, odd)
      crc1 = __gf2_matrix_times(even, crc1) if len2 & 1 == 1
      len2 >>= 1
      break if len2 == 0
      __gf2_matrix_square(odd, even)
      crc1 = __gf2_matrix_times(odd, crc1) if len2 & 1 == 1
      len2 >>= 1
      break if len2 == 0
    end
    crc1 ^ crc2
  end

  def self.__gf2_matrix_times(mat, vec)
    sum = 0
    i = 0
    while vec != 0
      sum ^= mat[i] if vec & 1 == 1
      vec >>= 1
      i += 1
    end
    sum
  end

  def self.__gf2_matrix_square(square, mat)
    n = 0
    while n < 32
      square[n] = __gf2_matrix_times(mat, mat[n])
      n += 1
    end
  end

  # `adler32_combine` (adler32.c).
  def self.adler32_combine(adler1, adler2, len2)
    base = 65521
    return 0xFFFFFFFF if len2 < 0
    rem = len2 % base
    sum1 = adler1 & 0xFFFF
    sum2 = (rem * sum1) % base
    sum1 += (adler2 & 0xFFFF) + base - 1
    sum2 += ((adler1 >> 16) & 0xFFFF) + ((adler2 >> 16) & 0xFFFF) + base - rem
    sum1 -= base if sum1 >= base
    sum1 -= base if sum1 >= base
    sum2 -= base << 1 if sum2 >= base << 1
    sum2 -= base if sum2 >= base
    sum1 | (sum2 << 16)
  end

  # ---------------------------------------------------------------------
  # Deflate — stored blocks only.

  class ZStream
    def initialize
      @input = "".b
      @output = nil
      @closed = false
      @finished = false
    end

    def <<(string)
      __check_open
      @input << __coerce_string(string).b
      self
    end

    def finish
      __check_open
      __finish_stream unless @finished
      @finished = true
      @output
    end

    def finished?
      __check_open
      @finished
    end
    alias stream_end? finished?

    def close
      @closed = true
      nil
    end
    alias end close

    def closed?
      @closed
    end
    alias ended? closed?

    def reset
      __check_open
      @input = "".b
      @output = nil
      @finished = false
      nil
    end

    def total_in
      __check_open
      @input.bytesize
    end

    def total_out
      __check_open
      @output ? @output.bytesize : 0
    end

    private

    # Everything but `closed?` is an error on a closed stream
    # (`zstream_ensure_valid` in zlib.c).
    def __check_open
      raise Zlib::Error, "stream is not ready" if @closed
    end

    def __coerce_string(string)
      return string if string.is_a?(String)
      converted = String.try_convert(string)
      if converted.nil?
        raise TypeError, "no implicit conversion of #{string.nil? ? "nil" : string.class} into String"
      end
      converted
    end
  end

  class Deflate < ZStream
    # The largest stored block zlib itself emits when its output buffer
    # allows (deflate_stored keeps four bytes of the 65535 maximum for
    # the pending block header), so up to this size the
    # `deflate(s, NO_COMPRESSION)` output is byte-identical to CRuby's.
    STORED_BLOCK_MAX = 65531

    def self.deflate(string, level = DEFAULT_COMPRESSION)
      d = new(level)
      d << string
      d.finish
    end

    def initialize(level = DEFAULT_COMPRESSION, window_bits = MAX_WBITS,
                   mem_level = DEF_MEM_LEVEL, strategy = DEFAULT_STRATEGY)
      super()
      level = __to_level(level)
      raise Zlib::StreamError, "stream error" if level < -1 || level > 9
      @level = level
    end

    def deflate(string, flush = NO_FLUSH)
      self << string
      flush == FINISH ? finish : "".b
    end

    def flush(flush = SYNC_FLUSH)
      flush == FINISH ? finish : "".b
    end

    private

    def __to_level(level)
      return level if level.is_a?(Integer)
      return DEFAULT_COMPRESSION if level.nil?
      unless level.respond_to?(:to_int)
        raise TypeError, "no implicit conversion of #{level.class} into Integer"
      end
      level.to_int
    end

    # RFC 1950 header, RFC 1951 stored blocks, Adler-32 trailer.
    def __finish_stream
      data = @input
      out = "".b
      flevel = case @level
               when 0, 1 then 0
               when 2, 3, 4, 5 then 1
               when 7, 8, 9 then 3
               else 2
               end
      cmf = 0x78
      flg = flevel << 6
      flg += 31 - ((cmf << 8) | flg) % 31
      out << cmf.chr << flg.chr
      size = data.bytesize
      pos = 0
      loop do
        len = size - pos
        len = STORED_BLOCK_MAX if len > STORED_BLOCK_MAX
        final = pos + len >= size
        out << (final ? 1 : 0).chr
        out << (len & 0xFF).chr << (len >> 8).chr
        nlen = len ^ 0xFFFF
        out << (nlen & 0xFF).chr << (nlen >> 8).chr
        out << data.byteslice(pos, len) if len > 0
        pos += len
        break if final
      end
      out << [Zlib.adler32(data)].pack("N")
      @output = out
    end
  end

  # ---------------------------------------------------------------------
  # Inflate — a complete RFC 1951 decoder (after Mark Adler's puff.c).

  class Inflate < ZStream
    def self.inflate(string)
      i = new
      i << string
      i.finish
    end

    def initialize(window_bits = MAX_WBITS)
      super()
    end

    def inflate(string = nil)
      self << string unless string.nil?
      "".b
    end

    def sync_point?
      false
    end

    private

    # Base lengths / distances and their extra-bit counts, indexed by
    # symbol (RFC 1951 §3.2.5).
    LENGTH_BASE = [3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 15, 17, 19, 23, 27, 31,
                   35, 43, 51, 59, 67, 83, 99, 115, 131, 163, 195, 227, 258].freeze
    LENGTH_EXTRA = [0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2,
                    3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 0].freeze
    DIST_BASE = [1, 2, 3, 4, 5, 7, 9, 13, 17, 25, 33, 49, 65, 97, 129, 193,
                 257, 385, 513, 769, 1025, 1537, 2049, 3073, 4097, 6145,
                 8193, 12289, 16385, 24577].freeze
    DIST_EXTRA = [0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6,
                  7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13].freeze
    # Order in which code-length code lengths are transmitted.
    CLEN_ORDER = [16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15].freeze

    # The fixed-Huffman tables (§3.2.6), built once.
    FIXED_TABLES = begin
      lengths = Array.new(288)
      288.times do |i|
        lengths[i] = if i < 144 then 8
                     elsif i < 256 then 9
                     elsif i < 280 then 7
                     else 8
                     end
      end
      [lengths, Array.new(30, 5)]
    end

    def __finish_stream
      @data = @input
      @pos = 0
      @bitbuf = 0
      @bitcnt = 0
      size = @data.bytesize
      raise Zlib::BufError, "buffer error" if size < 2
      cmf = @data.getbyte(0)
      flg = @data.getbyte(1)
      if ((cmf << 8) | flg) % 31 != 0 || cmf & 0x0F != 8 || (cmf >> 4) > 7
        raise Zlib::DataError, "incorrect header check"
      end
      raise Zlib::NeedDict, "need dictionary" if flg & 0x20 != 0
      @pos = 2
      out = "".b
      loop do
        final = __bits(1)
        case __bits(2)
        when 0 then __stored(out)
        when 1 then __codes(out, __huffman(FIXED_TABLES[0]), __huffman(FIXED_TABLES[1]))
        when 2 then __dynamic(out)
        else raise Zlib::DataError, "invalid block type"
        end
        break if final == 1
      end
      # Drop to a byte boundary, then check the Adler-32 trailer.
      @bitbuf = 0
      @bitcnt = 0
      raise Zlib::BufError, "buffer error" if @pos + 4 > size
      expected = (@data.getbyte(@pos) << 24) | (@data.getbyte(@pos + 1) << 16) |
                 (@data.getbyte(@pos + 2) << 8) | @data.getbyte(@pos + 3)
      raise Zlib::DataError, "incorrect data check" if Zlib.adler32(out) != expected
      @output = out
    end

    def __bits(need)
      while @bitcnt < need
        b = @data.getbyte(@pos)
        raise Zlib::BufError, "buffer error" if b.nil?
        @pos += 1
        @bitbuf |= b << @bitcnt
        @bitcnt += 8
      end
      v = @bitbuf & ((1 << need) - 1)
      @bitbuf >>= need
      @bitcnt -= need
      v
    end

    def __stored(out)
      @bitbuf = 0
      @bitcnt = 0
      raise Zlib::BufError, "buffer error" if @pos + 4 > @data.bytesize
      len = @data.getbyte(@pos) | (@data.getbyte(@pos + 1) << 8)
      nlen = @data.getbyte(@pos + 2) | (@data.getbyte(@pos + 3) << 8)
      raise Zlib::DataError, "invalid stored block lengths" if len != (nlen ^ 0xFFFF)
      @pos += 4
      raise Zlib::BufError, "buffer error" if @pos + len > @data.bytesize
      out << @data.byteslice(@pos, len)
      @pos += len
    end

    # Canonical Huffman table from code lengths: `[count, symbol]`, where
    # `count[len]` is how many codes have that length and `symbol` lists
    # the symbols in code order.
    def __huffman(lengths)
      count = Array.new(16, 0)
      lengths.each { |l| count[l] += 1 }
      count[0] = 0
      offs = Array.new(16, 0)
      len = 1
      while len < 15
        offs[len + 1] = offs[len] + count[len]
        len += 1
      end
      symbol = Array.new(lengths.size, 0)
      lengths.each_with_index do |l, s|
        next if l == 0
        symbol[offs[l]] = s
        offs[l] += 1
      end
      [count, symbol]
    end

    def __decode(table)
      count, symbol = table
      code = 0
      first = 0
      index = 0
      len = 1
      while len <= 15
        code |= __bits(1)
        c = count[len]
        return symbol[index + (code - first)] if code - c < first
        index += c
        first += c
        first <<= 1
        code <<= 1
        len += 1
      end
      raise Zlib::DataError, "invalid code"
    end

    def __codes(out, lencode, distcode)
      loop do
        sym = __decode(lencode)
        if sym < 256
          out << sym.chr
        elsif sym == 256
          return
        else
          sym -= 257
          raise Zlib::DataError, "invalid literal/length code" if sym >= 29
          len = LENGTH_BASE[sym] + __bits(LENGTH_EXTRA[sym])
          dsym = __decode(distcode)
          raise Zlib::DataError, "invalid distance code" if dsym >= 30
          dist = DIST_BASE[dsym] + __bits(DIST_EXTRA[dsym])
          raise Zlib::DataError, "invalid distance too far back" if dist > out.bytesize
          start = out.bytesize - dist
          if dist >= len
            out << out.byteslice(start, len)
          else
            # Overlapping copy: the run repeats itself.
            len.times { |k| out << out.getbyte(start + k).chr }
          end
        end
      end
    end

    def __dynamic(out)
      nlen = __bits(5) + 257
      ndist = __bits(5) + 1
      ncode = __bits(4) + 4
      raise Zlib::DataError, "too many length or distance symbols" if nlen > 286 || ndist > 30
      lengths = Array.new(19, 0)
      ncode.times { |i| lengths[CLEN_ORDER[i]] = __bits(3) }
      lencode = __huffman(lengths)
      lengths = []
      while lengths.size < nlen + ndist
        sym = __decode(lencode)
        if sym < 16
          lengths << sym
        else
          if sym == 16
            raise Zlib::DataError, "invalid bit length repeat" if lengths.empty?
            rep = lengths.last
            n = 3 + __bits(2)
          elsif sym == 17
            rep = 0
            n = 3 + __bits(3)
          else
            rep = 0
            n = 11 + __bits(7)
          end
          raise Zlib::DataError, "invalid bit length repeat" if lengths.size + n > nlen + ndist
          n.times { lengths << rep }
        end
      end
      raise Zlib::DataError, "invalid code -- missing end-of-block" if lengths[256] == 0
      __codes(out, __huffman(lengths[0, nlen]), __huffman(lengths[nlen, ndist]))
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
