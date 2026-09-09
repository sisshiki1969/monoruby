# Zlib for monoruby.
#
# Zlib is a C extension (zlib.so) that monoruby cannot load, so this file
# provides the module in Ruby, over two native backends:
#
# - `Zlib.crc32` / `Zlib.adler32` (and the `_combine` variants) with the
#   real argument semantics, over the native byte walk in
#   `src/builtins/zlib.rs` (`String.__crc32` / `String.__adler32`).
# - `Zlib::Deflate` / `Zlib::Inflate` (and everything built on them:
#   `Zlib.deflate` / `Zlib.inflate`, the gzip framing, `GzipReader` /
#   `GzipWriter`) as thin shells over a `z_stream` of the bundled zlib
#   (`String.__zstream_*`, libz-sys built from source). Compression is
#   real DEFLATE with zlib's own algorithm, so the bytes are the ones
#   CRuby's zlib.so produces for the same level / strategy / window —
#   PDF writers that compare output sizes with a CRuby run depend on it.
#   The `window_bits` argument is zlib's: 8..15 for the zlib wrapper,
#   negative for a raw stream, +16 for gzip, +32 (inflate) to auto-detect.
#
# Not provided: `Zlib::GzipFile` over arbitrary IO objects with
# streaming (a reader slurps its IO, a writer emits at `close`).

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
  # ZStream — the shell over a native zlib stream.

  class ZStream
    # zlib status => the Zlib::Error subclass CRuby raises for it.
    ERROR_CLASSES = {
      1 => StreamEnd, 2 => NeedDict, -2 => StreamError, -3 => DataError,
      -4 => MemError, -5 => BufError, -6 => VersionError,
    }.freeze

    def initialize
      @handle = nil
      @buffer = "".b
      @closed = false
      @finished = false
    end

    # Bytes of input consumed / output produced by zlib so far.
    def total_in
      __check_open
      String.__zstream_totals(@handle)[0]
    end

    def total_out
      __check_open
      String.__zstream_totals(@handle)[1]
    end

    # zlib's running checksum (Adler-32 for the zlib wrapper, CRC-32 for
    # gzip, unused for raw streams).
    def adler
      __check_open
      String.__zstream_totals(@handle)[3]
    end

    def data_type
      __check_open
      String.__zstream_totals(@handle)[4]
    end

    # No input is ever held back: everything handed over is consumed by
    # the same call (an inflate keeps the bytes past the end of the stream
    # in `@unused`, see Inflate).
    def avail_in
      __check_open
      0
    end

    def avail_out
      __check_open
      0
    end

    def avail_out=(_size)
      __check_open
    end

    def flush_next_in
      __check_open
      "".b
    end

    # Output produced by `<<` / `flush` that has not been returned yet.
    def flush_next_out
      __check_open
      out = @buffer
      @buffer = "".b
      out
    end

    def finished?
      __check_open
      @finished
    end
    alias stream_end? finished?

    def finish
      __check_open
      out = @buffer + __run("".b, FINISH)
      @buffer = "".b
      @finished = true
      out
    end

    def close
      unless @closed
        String.__zstream_close(@handle) if @handle
        @closed = true
      end
      nil
    end
    alias end close

    def closed?
      @closed
    end
    alias ended? closed?

    def reset
      __check_open
      String.__zstream_reset(@handle)
      @buffer = "".b
      @finished = false
      nil
    end

    private

    def __open(inflate, level, window_bits, mem_level, strategy)
      r = String.__zstream_new(inflate, __to_int(level), __to_int(window_bits),
                               __to_int(mem_level), __to_int(strategy))
      __check_status(r)
      @handle = r
    end

    # zlib's `deflate` / `inflate` over `data` at `flush`; returns the
    # bytes produced. Errors raise their `Zlib::*Error`.
    def __run(data, flush)
      status, out, consumed = String.__zstream_run(@handle, data, flush)
      __after_run(status, data, consumed)
      out
    end

    def __after_run(status, _data, _consumed)
      __check_status(status)
      @finished = true if status == 1
    end

    def __check_status(status)
      return status if status.is_a?(Integer)
      code, msg = status
      raise ERROR_CLASSES.fetch(code, Zlib::Error), msg
    end

    # Everything but `closed?` is an error on a closed stream
    # (`zstream_ensure_valid` in zlib.c).
    def __check_open
      raise Zlib::Error, "stream is not ready" if @closed || @handle.nil?
    end

    def __coerce_string(string)
      return string if string.is_a?(String)
      converted = String.try_convert(string)
      if converted.nil?
        raise TypeError, "no implicit conversion of #{string.nil? ? "nil" : string.class} into String"
      end
      converted
    end

    def __to_int(v)
      return v if v.is_a?(Integer)
      unless v.respond_to?(:to_int)
        raise TypeError, "no implicit conversion of #{v.nil? ? "nil" : v.class} into Integer"
      end
      v.to_int
    end
  end

  # ---------------------------------------------------------------------
  # Deflate

  class Deflate < ZStream
    def self.deflate(string, level = DEFAULT_COMPRESSION)
      d = new(level)
      begin
        d.deflate(string, FINISH)
      ensure
        d.close
      end
    end

    def initialize(level = DEFAULT_COMPRESSION, window_bits = MAX_WBITS,
                   mem_level = DEF_MEM_LEVEL, strategy = DEFAULT_STRATEGY)
      super()
      __open(false, level.nil? ? DEFAULT_COMPRESSION : level, window_bits.nil? ? MAX_WBITS : window_bits,
             mem_level.nil? ? DEF_MEM_LEVEL : mem_level, strategy.nil? ? DEFAULT_STRATEGY : strategy)
    end

    # Compress `string`; with `flush` other than NO_FLUSH the output is
    # flushed accordingly (`FINISH` ends the stream). Returns the bytes
    # produced, together with anything `<<` had buffered.
    def deflate(string, flush = NO_FLUSH)
      __check_open
      data = __coerce_string(string).b
      out = @buffer + __run(data, flush)
      @buffer = "".b
      @finished = true if flush == FINISH
      out
    end

    def <<(string)
      __check_open
      @buffer << __run(__coerce_string(string).b, NO_FLUSH)
      self
    end

    def flush(flush = SYNC_FLUSH)
      __check_open
      out = @buffer
      @buffer = "".b
      out << __run("".b, flush) unless flush == NO_FLUSH
      @finished = true if flush == FINISH
      out
    end

    # Change level / strategy mid-stream; output produced by the old
    # settings is flushed into the buffer `<<` / `finish` return.
    def params(level, strategy)
      __check_open
      status, out = String.__zstream_params(@handle, __to_int(level), __to_int(strategy))
      __check_status(status)
      @buffer << out
      nil
    end

    def set_dictionary(dict)
      __check_open
      __check_status(String.__zstream_dictionary(@handle, __coerce_string(dict).b))
      dict
    end
  end

  # ---------------------------------------------------------------------
  # Inflate

  class Inflate < ZStream
    # One shot: decode and finish — a truncated stream raises BufError
    # exactly as CRuby's `Zlib::Inflate.inflate` does.
    def self.inflate(string)
      # The class method takes a String (nil is a TypeError here, unlike the
      # instance method's `inflate(nil)` = finish).
      unless string.is_a?(String) || String.try_convert(string)
        raise TypeError, "no implicit conversion of #{string.nil? ? "nil" : string.class} into String"
      end
      i = new
      begin
        out = i.inflate(string)
        out << i.finish
        out
      ensure
        i.close
      end
    end

    def initialize(window_bits = MAX_WBITS)
      super()
      @unused = nil
      @pending = nil
      __open(true, 0, window_bits.nil? ? MAX_WBITS : window_bits, 0, 0)
    end

    # Decompress `string` (nil ⇒ finish). Returns the bytes produced plus
    # anything `<<` had buffered. Raises `DataError` on a corrupt stream,
    # `NeedDict` when a preset dictionary is required.
    def inflate(string)
      __check_open
      return finish if string.nil?
      data = __coerce_string(string).b
      out = @buffer + __feed(data)
      @buffer = "".b
      out
    end

    def <<(string)
      __check_open
      @buffer << __feed(__coerce_string(string).b)
      self
    end

    def finish
      __check_open
      out = @buffer
      @buffer = "".b
      unless @finished
        status, more, _consumed = String.__zstream_run(@handle, "".b, FINISH)
        out << more
        if status == 1
          @finished = true
        elsif status.is_a?(Integer)
          # The stream was cut short: the same "buffer error" zlib.c raises.
          raise BufError, "buffer error" if total_in > 0 || !out.empty?
        else
          __check_status(status)
        end
      end
      @finished = true
      out
    end

    # Bytes handed over after the end of the stream (nil if none). Not a
    # CRuby API (`GzipReader#unused` is); kept for the gzip framing code.
    def __unused
      @unused
    end

    def set_dictionary(dict)
      __check_open
      __check_status(String.__zstream_dictionary(@handle, __coerce_string(dict).b))
      dict
    end

    def sync(string)
      __check_open
      false
    end

    def sync_point?
      __check_open
      false
    end

    private

    def __feed(data)
      if @finished
        @unused = (@unused || "".b) + data unless data.empty?
        return "".b
      end
      # Input zlib could not take yet (it stopped for a preset dictionary)
      # goes first once `set_dictionary` has been called.
      if @pending
        data = @pending + data
        @pending = nil
      end
      status, out, consumed = String.__zstream_run(@handle, data, NO_FLUSH)
      if status.is_a?(Array) && status[0] == 2
        @pending = data.byteslice(consumed..)
      end
      __check_status(status)
      if status == 1
        @finished = true
        @unused = data.byteslice(consumed..) if consumed < data.bytesize
      end
      out
    end
  end

  def self.deflate(string, level = DEFAULT_COMPRESSION)
    Deflate.deflate(string, level)
  end

  def self.inflate(string)
    Inflate.inflate(string)
  end

  # Raw DEFLATE (no wrapper) of `data`, one shot.
  def self.__deflate_raw(data, level = DEFAULT_COMPRESSION)
    d = Deflate.new(level, -MAX_WBITS)
    begin
      d.deflate(data, FINISH)
    ensure
      d.close
    end
  end

  # Raw inflate of the DEFLATE blocks starting at byte `pos` of `data`;
  # returns `[decoded, position just past the final block]`, or raises
  # `GzipFile::NoFooter`-style truncation as `[nil, nil]`.
  def self.__inflate_raw(data, pos = 0)
    i = Inflate.new(-MAX_WBITS)
    begin
      out = i.inflate(data.byteslice(pos..))
      return [nil, nil] unless i.finished?
      [out, pos + i.total_in]
    ensure
      i.close
    end
  end

  # ---------------------------------------------------------------------
  # gzip framing (RFC 1952).

  def self.__gzip_frame(data, level = DEFAULT_COMPRESSION, mtime: 0, orig_name: nil, comment: nil, os_code: OS_CODE)
    out = "".b
    flg = 0
    flg |= 0x08 if orig_name
    flg |= 0x10 if comment
    xfl = case level
          when 1 then 4
          when 9 then 2
          else 0
          end
    out << [0x1f, 0x8b, 8, flg, mtime.to_i, xfl, os_code].pack("CCCCVCC")
    out << orig_name.b << "\0" if orig_name
    out << comment.b << "\0" if comment
    out << __deflate_raw(data, level)
    out << [Zlib.crc32(data), data.bytesize & 0xFFFFFFFF].pack("VV")
    out
  end

  # Parse one gzip member: `[data, header, bytes consumed]`. `header` is
  # `{mtime:, orig_name:, comment:, os_code:, level:}`.
  def self.__gunzip_frame(data)
    data = data.b
    raise GzipFile::Error, "not in gzip format" if data.bytesize < 10 ||
                                                   data.getbyte(0) != 0x1f || data.getbyte(1) != 0x8b
    raise GzipFile::Error, "unsupported compression method #{data.getbyte(2)}" if data.getbyte(2) != 8
    flg = data.getbyte(3)
    mtime = data.byteslice(4, 4).unpack1("V")
    xfl = data.getbyte(8)
    os_code = data.getbyte(9)
    pos = 10
    if flg & 0x04 != 0
      xlen = data.byteslice(pos, 2).unpack1("v")
      pos += 2 + xlen
    end
    orig_name = nil
    if flg & 0x08 != 0
      nul = data.index("\0", pos)
      raise GzipFile::Error, "unexpected end of file" if nul.nil?
      orig_name = data.byteslice(pos, nul - pos)
      pos = nul + 1
    end
    comment = nil
    if flg & 0x10 != 0
      nul = data.index("\0", pos)
      raise GzipFile::Error, "unexpected end of file" if nul.nil?
      comment = data.byteslice(pos, nul - pos)
      pos = nul + 1
    end
    pos += 2 if flg & 0x02 != 0
    raise GzipFile::Error, "unexpected end of file" if pos > data.bytesize
    out, pos = __inflate_raw(data, pos)
    raise GzipFile::NoFooter, "footer is not found" if out.nil? || pos + 8 > data.bytesize
    crc, isize = data.byteslice(pos, 8).unpack("VV")
    raise GzipFile::CRCError, "invalid compressed data -- crc error" if crc != Zlib.crc32(out)
    raise GzipFile::LengthError, "invalid compressed data -- length error" if isize != (out.bytesize & 0xFFFFFFFF)
    level = case xfl
            when 2 then BEST_COMPRESSION
            when 4 then BEST_SPEED
            else DEFAULT_COMPRESSION
            end
    [out, { mtime: mtime, orig_name: orig_name, comment: comment, os_code: os_code, level: level }, pos + 8]
  end

  def self.gzip(src, level: nil, strategy: nil)
    src = String.try_convert(src) || raise(TypeError, "no implicit conversion of #{src.class} into String")
    __gzip_frame(src.b, level || DEFAULT_COMPRESSION)
  end

  def self.gunzip(src)
    src = String.try_convert(src) || raise(TypeError, "no implicit conversion of #{src.class} into String")
    # The String API reports a truncated frame (or one too short to even
    # hold a header) as a plain GzipFile::Error.
    raise GzipFile::Error, "unexpected end of string" if src.bytesize < 10
    begin
      __gunzip_frame(src)[0]
    rescue GzipFile::NoFooter
      raise GzipFile::Error, "unexpected end of string"
    end
  end

  class GzipFile
    class Error < Zlib::Error
      attr_reader :input
    end
    class CRCError < Error; end
    class NoFooter < Error; end
    class LengthError < Error; end

    # Run the block with a fresh reader/writer over `io`, closing it
    # afterwards; without a block just return the object.
    def self.wrap(io, *args, **opts)
      obj = new(io, *args, **opts)
      return obj unless block_given?
      begin
        yield obj
      ensure
        obj.close unless obj.closed?
      end
    end

    attr_reader :os_code, :orig_name, :comment, :level

    def mtime
      Time.at(@mtime || 0)
    end

    def to_io
      @io
    end

    def closed?
      @closed
    end

    def sync
      @sync ||= false
    end

    def sync=(flag)
      @sync = flag
    end

    def crc
      @crc || 0
    end

    private

    def __check_open
      raise GzipFile::Error, "closed gzip stream" if @closed
    end
  end

  class GzipReader < GzipFile
    include Enumerable

    def self.open(filename, **opts, &block)
      io = File.open(filename, "rb")
      wrap(io, **opts, &block)
    end

    # Decompress every gzip member of `io` and write the result to `out`
    # (or return it as a String).
    def self.zcat(io, out = +"", **opts)
      data = io.read.b
      until data.empty?
        body, _hdr, used = Zlib.__gunzip_frame(data)
        out << body
        data = data.byteslice(used..)
      end
      out
    end

    def initialize(io, external_encoding: nil, internal_encoding: nil, encoding: nil, **_opts)
      @io = io
      @closed = false
      raw = io.read
      raw = raw.nil? ? "".b : raw.b
      body, header, used = Zlib.__gunzip_frame(raw)
      @unused = used < raw.bytesize ? raw.byteslice(used..) : nil
      @mtime = header[:mtime]
      @orig_name = header[:orig_name]
      @comment = header[:comment]
      @os_code = header[:os_code]
      @level = header[:level]
      @crc = Zlib.crc32(body)
      enc = external_encoding || encoding || Encoding.default_external
      enc = Encoding.find(enc) if enc.is_a?(String)
      @encoding = enc
      @data = body.force_encoding(enc)
      @pos = 0
      @lineno = 0
    end

    attr_accessor :lineno

    def unused
      @unused
    end

    def pos
      @pos
    end
    alias tell pos

    def eof?
      __check_open
      @pos >= @data.bytesize
    end
    alias eof eof?

    def rewind
      __check_open
      @pos = 0
      @lineno = 0
      0
    end

    def read(length = nil, outbuf = nil)
      __check_open
      if length.nil?
        s = @data.byteslice(@pos..).force_encoding(@encoding)
        @pos = @data.bytesize
      else
        raise ArgumentError, "negative length #{length} given" if length < 0
        return (outbuf ? outbuf.replace("") : "".b) if length == 0
        return nil if eof?
        s = @data.byteslice(@pos, length)
        @pos += s.bytesize
      end
      outbuf ? outbuf.replace(s) : s
    end

    def readpartial(maxlen, outbuf = nil)
      __check_open
      raise ArgumentError, "negative length #{maxlen} given" if maxlen < 0
      raise EOFError, "end of file reached" if eof? && maxlen > 0
      read(maxlen, outbuf)
    end

    def getc
      __check_open
      return nil if eof?
      c = @data.byteslice(@pos..).force_encoding(@encoding)[0]
      @pos += c.bytesize
      c
    end

    def readchar
      getc || raise(EOFError, "end of file reached")
    end

    def getbyte
      __check_open
      return nil if eof?
      b = @data.getbyte(@pos)
      @pos += 1
      b
    end

    def readbyte
      getbyte || raise(EOFError, "end of file reached")
    end

    def each_byte
      return enum_for(:each_byte) unless block_given?
      while (b = getbyte)
        yield b
      end
      nil
    end

    def each_char
      return enum_for(:each_char) unless block_given?
      while (c = getc)
        yield c
      end
      nil
    end

    def ungetc(s)
      __check_open
      s = s.chr if s.is_a?(Integer)
      s = s.to_s.b
      @data = (@data.byteslice(0, @pos) + s + @data.byteslice(@pos..)).force_encoding(@encoding)
      nil
    end

    def ungetbyte(b)
      ungetc(b.is_a?(Integer) ? (b & 0xFF).chr : b)
    end

    def gets(sep = $/, limit = nil, chomp: false)
      __check_open
      if sep.is_a?(Integer) && limit.nil?
        limit = sep
        sep = $/
      end
      return nil if eof?
      rest = @data.byteslice(@pos..)
      line = if sep.nil?
               rest
             elsif sep == ""
               # Paragraph mode: up to the next run of blank lines.
               rest = rest.sub(/\A\n+/, "")
               @pos += (@data.bytesize - @pos) - rest.bytesize
               idx = rest.index(/\n\n+/)
               idx ? rest[0, idx + 2] : rest
             else
               idx = rest.index(sep)
               idx ? rest.byteslice(0, idx + sep.bytesize) : rest
             end
      line = line.byteslice(0, limit) if limit && limit >= 0 && line.bytesize > limit
      @pos += line.bytesize
      @lineno += 1
      line = line.force_encoding(@encoding)
      line = line.chomp(sep == "" ? "\n" : sep) if chomp && !sep.nil?
      line
    end

    def readline(*args, **opts)
      gets(*args, **opts) || raise(EOFError, "end of file reached")
    end

    def each_line(*args, **opts)
      return enum_for(:each_line, *args, **opts) unless block_given?
      while (line = gets(*args, **opts))
        yield line
      end
      self
    end
    alias each each_line

    def readlines(*args, **opts)
      lines = []
      while (line = gets(*args, **opts))
        lines << line
      end
      lines
    end

    def external_encoding
      @encoding
    end

    def close
      __check_open
      @closed = true
      @io.close if @io.respond_to?(:close)
      @io
    end

    def finish
      __check_open
      @closed = true
      @io
    end
  end

  class GzipWriter < GzipFile
    def self.open(filename, level = nil, strategy = nil, **opts, &block)
      io = File.open(filename, "wb")
      wrap(io, level, strategy, **opts, &block)
    end

    def initialize(io, level = nil, strategy = nil, **_opts)
      @io = io
      @level = level.nil? ? DEFAULT_COMPRESSION : level
      @closed = false
      @buffer = "".b
      @mtime = nil
      @orig_name = nil
      @comment = nil
      @os_code = OS_CODE
      @sync = false
    end

    def mtime=(time)
      __check_open
      @mtime = time.to_i
      time
    end

    def orig_name=(name)
      __check_open
      @orig_name = name.to_str
    end

    def comment=(text)
      __check_open
      @comment = text.to_str
    end

    def write(*strs)
      __check_open
      n = 0
      strs.each do |s|
        s = s.to_s
        @buffer << s.b
        n += s.bytesize
      end
      n
    end

    def <<(obj)
      write(obj)
      self
    end

    def print(*args)
      args = [$_] if args.empty?
      args.each { |a| write(a.to_s) }
      nil
    end

    def puts(*args)
      if args.empty?
        write("\n")
      else
        args.flatten.each do |a|
          s = a.to_s
          write(s.end_with?("\n") ? s : s + "\n")
        end
      end
      nil
    end

    def printf(fmt, *args)
      write(format(fmt, *args))
      nil
    end

    def putc(ch)
      write(ch.is_a?(Integer) ? (ch & 0xFF).chr : ch.to_s[0])
      ch
    end

    def pos
      @buffer.bytesize
    end
    alias tell pos

    def flush(_flush = SYNC_FLUSH)
      __check_open
      self
    end

    # Write the gzip frame to the IO and close it; returns the IO.
    def close
      __check_open
      __emit
      @closed = true
      @io.close if @io.respond_to?(:close)
      @io
    end

    # Like `close`, but leaves the IO open.
    def finish
      __check_open
      __emit
      @closed = true
      @io
    end

    private

    def __emit
      @crc = Zlib.crc32(@buffer)
      @io.write(Zlib.__gzip_frame(@buffer, @level, mtime: @mtime || Time.now.to_i,
                                  orig_name: @orig_name, comment: @comment, os_code: @os_code))
      @io.flush if @io.respond_to?(:flush)
    end
  end
end
