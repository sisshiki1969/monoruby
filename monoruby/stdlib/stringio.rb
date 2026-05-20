# frozen_string_literal: true
#
# StringIO implementation for monoruby.
# Provides an IO-like interface for strings.
#

class StringIO
  include Enumerable

  # ruby/spec asserts `StringIO::VERSION` is a String of digits and
  # dots (it does not pin the value). Match CRuby's bundled gem
  # shape; bump if/when monoruby tracks the upstream gem.
  VERSION = "3.1.7"

  # StringIO.open(string = "", mode = "r+") { |io| ... } -> obj
  # StringIO.open(string = "", mode = "r+") -> stringio
  #
  # With a block, yields a freshly-built StringIO, ensures it is
  # `close`d after the block returns (raising or not), and returns
  # the block's value. Without a block, returns a StringIO -- in
  # which case the caller is responsible for closing it.
  def self.open(string = "", mode = "r+", &block)
    io = new(string, mode)
    return io unless block
    begin
      yield io
    ensure
      io.close unless io.closed?
    end
  end

  def initialize(string = "", mode = "r+")
    @string = string.is_a?(String) ? string : string.to_s
    @pos = 0
    @lineno = 0
    @closed_read = false
    @closed_write = false
    _parse_mode(mode)
  end

  attr_reader :lineno
  attr_writer :lineno

  def string
    @string
  end

  def string=(str)
    @string = str
    @pos = 0
  end

  # --- Position ---

  def pos
    @pos
  end
  alias tell pos

  def pos=(n)
    raise Errno::EINVAL, "Invalid argument" if n < 0
    @pos = n
  end

  def rewind
    @pos = 0
    @lineno = 0
    0
  end

  def seek(offset, whence = 0)  # IO::SEEK_SET = 0
    case whence
    when 0  # SEEK_SET
      @pos = offset
    when 1  # SEEK_CUR
      @pos += offset
    when 2  # SEEK_END
      @pos = @string.length + offset
    else
      raise Errno::EINVAL, "Invalid argument"
    end
    @pos = 0 if @pos < 0
    0
  end

  def eof?
    @pos >= @string.length
  end
  alias eof eof?

  # --- Reading ---

  def read(length = nil, outbuf = nil)
    _check_readable
    if length.nil?
      if @pos >= @string.length
        result = ""
      else
        result = @string[@pos..-1]
        @pos = @string.length
      end
    else
      return nil if @pos >= @string.length && length > 0
      result = @string[@pos, length] || ""
      @pos += result.length
    end
    if outbuf
      outbuf.replace(result)
      outbuf
    else
      result
    end
  end

  # sysread(maxlen, outbuf = nil) -> string
  #
  # Like `read`, but raises `EOFError` at end-of-stream (CRuby
  # treats sysread as "must return at least one byte or raise").
  # StringIO has no real syscall layer; we just delegate to `read`
  # and surface the EOF case as the exception.
  def sysread(maxlen, outbuf = nil)
    _check_readable
    maxlen = maxlen.to_int if maxlen.respond_to?(:to_int) && !maxlen.is_a?(Integer)
    raise ArgumentError, "negative length #{maxlen} given" if maxlen.is_a?(Integer) && maxlen < 0
    raise EOFError, "end of file reached" if @pos >= @string.length && maxlen.to_i != 0
    result = read(maxlen, outbuf)
    raise EOFError, "end of file reached" if result.nil?
    result
  end
  alias readpartial sysread

  # read_nonblock(maxlen, outbuf = nil, exception: true) -> string | :wait_readable | nil
  #
  # StringIO has no non-blocking layer; the CRuby contract is that
  # this acts like `sysread` (EOFError on end-of-stream). When
  # `exception: false` is requested and we're at EOF, CRuby returns
  # nil instead of raising — preserve that for codepaths that rely
  # on the keyword form.
  def read_nonblock(maxlen, outbuf = nil, exception: true)
    _check_readable
    maxlen = maxlen.to_int if maxlen.respond_to?(:to_int) && !maxlen.is_a?(Integer)
    raise ArgumentError, "negative length #{maxlen} given" if maxlen.is_a?(Integer) && maxlen < 0
    if @pos >= @string.length && maxlen.to_i != 0
      return nil unless exception
      raise EOFError, "end of file reached"
    end
    read(maxlen, outbuf)
  end

  # write_nonblock(string, exception: true) -> Integer
  #
  # StringIO writes never block; `write_nonblock` is just an alias
  # for `write` with the same return shape (number of bytes
  # written). The `exception:` keyword is accepted for API parity
  # with `IO#write_nonblock` (it has no effect here).
  def write_nonblock(string, exception: true)
    write(string)
  end

  def getc
    _check_readable
    return nil if @pos >= @string.length
    c = @string[@pos]
    @pos += 1
    c
  end

  def readchar
    c = getc
    raise EOFError, "end of file reached" if c.nil?
    c
  end

  def getbyte
    _check_readable
    return nil if @pos >= @string.length
    b = @string[@pos].ord
    @pos += 1
    b
  end

  def readbyte
    b = getbyte
    raise EOFError, "end of file reached" if b.nil?
    b
  end

  def ungetc(c)
    _check_readable
    c = c.chr if c.is_a?(Integer)
    if @pos == 0
      @string = c + @string
    elsif @pos > @string.length
      @string = @string + "\0" * (@pos - @string.length)
      @pos -= c.length
      @string[@pos, c.length] = c
    else
      @pos -= c.length
      @pos = 0 if @pos < 0
      @string[@pos, c.length] = c
    end
    nil
  end

  def gets(sep = "\n", limit = nil)
    _check_readable
    return nil if eof?

    if sep.is_a?(Integer)
      limit = sep
      sep = "\n"
    end

    if sep.nil?
      line = @string[@pos..-1]
      @pos = @string.length
    elsif sep.empty?
      # Paragraph mode: read until double newline
      # Skip leading newlines
      while @pos < @string.length && @string[@pos] == "\n"
        @pos += 1
      end
      return nil if eof?
      idx = @string.index("\n\n", @pos)
      if idx
        line = @string[@pos..idx]
        @pos = idx + 1
        # Skip trailing newlines
        while @pos < @string.length && @string[@pos] == "\n"
          @pos += 1
        end
      else
        line = @string[@pos..-1]
        @pos = @string.length
      end
    else
      idx = @string.index(sep, @pos)
      if idx
        line = @string[@pos..idx + sep.length - 1]
        @pos = idx + sep.length
      else
        line = @string[@pos..-1]
        @pos = @string.length
      end
    end

    if limit && line && line.length > limit
      line = line[0, limit]
      @pos = @pos - (line.length - limit) if sep
    end

    @lineno += 1
    line
  end

  def readline(*args)
    line = gets(*args)
    raise EOFError, "end of file reached" if line.nil?
    line
  end

  def readlines(sep = "\n", limit = nil)
    _check_readable
    lines = []
    while (line = gets(sep, limit))
      lines << line
    end
    lines
  end

  def each(sep = "\n", limit = nil, &block)
    return to_enum(:each, sep, limit) unless block
    while (line = gets(sep, limit))
      block.call(line)
    end
    self
  end
  alias each_line each

  # --- Writing ---

  def write(*strs)
    _check_writable
    total = 0
    strs.each do |str|
      str = str.to_s unless str.is_a?(String)
      next if str.empty?

      if @pos == @string.length
        @string << str
      elsif @pos > @string.length
        @string << "\0" * (@pos - @string.length) << str
      else
        @string[@pos, str.length] = str
      end
      @pos += str.length
      total += str.length
    end
    total
  end

  def <<(str)
    write(str)
    self
  end

  def print(*args)
    _check_writable
    args = [$_] if args.empty?
    args.each_with_index do |arg, i|
      write($, || "") if i > 0
      write(arg.to_s)
    end
    write($\ || "")
    nil
  end

  def puts(*args)
    _check_writable
    if args.empty?
      write("\n")
    else
      args.each do |arg|
        if arg.nil?
          write("\n")
        elsif arg.is_a?(Array)
          arg.each { |a| puts(a) }
        else
          s = arg.to_s
          write(s)
          write("\n") unless s.end_with?("\n")
        end
      end
    end
    nil
  end

  def printf(fmt, *args)
    write(fmt % args)
    nil
  end

  def putc(ch)
    _check_writable
    if ch.is_a?(Integer)
      write(ch.chr)
    else
      write(ch.to_s[0] || "")
    end
    ch
  end

  # --- Truncate ---

  def truncate(len)
    _check_writable
    raise Errno::EINVAL, "Invalid argument" if len < 0
    if len < @string.length
      @string = @string[0, len]
    elsif len > @string.length
      @string << "\0" * (len - @string.length)
    end
    len
  end

  # --- Close ---

  def close
    @closed_read = true
    @closed_write = true
    nil
  end

  def close_read
    @closed_read = true
    nil
  end

  def close_write
    @closed_write = true
    nil
  end

  def closed?
    @closed_read && @closed_write
  end

  def closed_read?
    @closed_read
  end

  def closed_write?
    @closed_write
  end

  # --- Misc ---

  def size
    @string.length
  end
  alias length size

  def flush
    self
  end

  def set_encoding(ext_enc, int_enc = nil, **opts)
    # No-op for now (single encoding)
    self
  end

  # set_encoding_by_bom -> Encoding | nil
  #
  # Inspect the bytes at the current position for one of the
  # documented BOM (byte-order mark) sequences. On a match: slice
  # the BOM off the backing string, reset `@pos` to 0 (avoids
  # char-vs-byte position drift after the encoding changes), force
  # the new encoding, and return the `Encoding`. Otherwise
  # (incomplete BOM, no BOM, or not opened for reading) leave the
  # state untouched and return `nil`.
  #
  # CRuby checks longest BOM first (UTF-32LE must precede
  # UTF-16LE because both start with `\xFF\xFE`).
  def set_encoding_by_bom
    # CRuby raises FrozenError on a frozen StringIO *before* the
    # readable-state check (the spec asserts the error class even
    # when the underlying state would otherwise return nil).
    raise FrozenError, "can't modify frozen StringIO" if frozen?
    # CRuby returns `nil` (not IOError) when the StringIO was
    # opened write-only; probe the close flag directly instead of
    # going through `_check_readable`.
    return nil if @closed_read
    bytes = @string.bytes
    return nil if bytes.empty?
    bom_len, enc =
      if bytes[0] == 0xEF
        return nil unless bytes.length >= 3 && bytes[1] == 0xBB && bytes[2] == 0xBF
        [3, Encoding::UTF_8]
      elsif bytes[0] == 0x00
        return nil unless bytes.length >= 4 && bytes[1] == 0x00 && bytes[2] == 0xFE && bytes[3] == 0xFF
        [4, Encoding::UTF_32BE]
      elsif bytes[0] == 0xFF && bytes.length >= 2 && bytes[1] == 0xFE
        if bytes.length >= 4 && bytes[2] == 0x00 && bytes[3] == 0x00
          [4, Encoding::UTF_32LE]
        else
          [2, Encoding::UTF_16LE]
        end
      elsif bytes[0] == 0xFE
        return nil unless bytes.length >= 2 && bytes[1] == 0xFF
        [2, Encoding::UTF_16BE]
      else
        return nil
      end
    # Slice BOM off the backing string and reset @pos. Using a
    # byte-level slice on an ASCII-8BIT string keeps the bytes
    # exact; we then force the new encoding on the remaining bytes.
    @string = bytes[bom_len..].pack("C*")
    @string.force_encoding(enc)
    @pos = 0
    enc
  end

  # external_encoding -> Encoding
  #
  # CRuby's StringIO mirrors the encoding of the underlying String
  # (the only encoding state a StringIO carries). monoruby's String
  # already tracks an encoding, so just forward to it.
  def external_encoding
    @string.encoding
  end

  # internal_encoding -> nil
  #
  # CRuby StringIO has no separate internal encoding; the value is
  # always nil unless `set_encoding(ext, int)` configured one. The
  # current no-op `set_encoding` discards the int arg, so return nil.
  def internal_encoding
    nil
  end

  # reopen(other_stringio)             -> stringio
  # reopen(string = "", mode = "r+")   -> stringio
  #
  # Re-attaches the StringIO to either another StringIO's content
  # (single StringIO arg) or to a new backing string with an
  # optional mode (CRuby semantics). The position counters are
  # reset; the mode flags are re-parsed.
  def reopen(*args)
    if args.size == 1 && args[0].is_a?(StringIO)
      other = args[0]
      @string = other.string
      @pos = 0
      @lineno = 0
      @closed_read = false
      @closed_write = false
      # Mirror the source's writability; CRuby's reopen(other)
      # copies the mode state.
      _parse_mode(other.closed_write? ? "r" : "r+")
      return self
    end
    string = args[0] || ""
    mode = args[1] || "r+"
    raise TypeError, "no implicit conversion of #{string.class} into String" \
      unless string.is_a?(String)
    @string = string
    @pos = 0
    @lineno = 0
    @closed_read = false
    @closed_write = false
    _parse_mode(mode)
    self
  end

  def sync
    true
  end

  def sync=(val)
    val
  end

  def isatty
    false
  end
  alias tty? isatty

  def binmode
    self
  end

  def fcntl(*args)
    raise NotImplementedError, "fcntl not supported on StringIO"
  end

  def fileno
    nil
  end
  alias to_i fileno

  def pid
    nil
  end

  def path
    nil
  end
  alias to_path path

  private

  def _parse_mode(mode)
    case mode
    when 'r'
      @closed_write = true
    when 'w', 'w+'
      @string = "" if mode == 'w'
    when 'a', 'a+'
      @pos = @string.length
    when 'r+', 'r+b', 'rb+'
      # default, both read and write
    end
  end

  def _check_readable
    raise IOError, "not opened for reading" if @closed_read
  end

  def _check_writable
    raise IOError, "not opened for writing" if @closed_write
  end
end
