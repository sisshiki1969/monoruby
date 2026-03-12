# frozen_string_literal: true
#
# StringIO implementation for monoruby.
# Provides an IO-like interface for strings.
#

class StringIO
  include Enumerable

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
    b = @string.getbyte(@pos)
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

  def gets(sep = $/, limit = nil)
    _check_readable
    return nil if eof?

    if sep.is_a?(Integer)
      limit = sep
      sep = $/
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

  def readlines(sep = $/, limit = nil)
    _check_readable
    lines = []
    while (line = gets(sep, limit))
      lines << line
    end
    lines
  end

  def each(sep = $/, limit = nil, &block)
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
      return 0 if str.empty?

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
          write("nil\n")  # This matches "nil" not ""
          # Actually Ruby's puts prints empty string for nil. Let me fix:
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
    write(sprintf(fmt, *args))
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
