class IO
  include File::Constants
  # Enumerable is mixed in after `require_relative 'enumerable'` at the
  # bottom of this file (the module is not defined yet at this point).

  SEEK_SET = 0
  SEEK_CUR = 1
  SEEK_END = 2

  # Mix-ins tagging the exceptions raised by `*_nonblock` when the
  # operation would block. The concrete classes subclass the matching
  # Errno (on Linux EAGAIN == EWOULDBLOCK) and include these so
  # `rescue IO::WaitReadable` / `rescue IO::WaitWritable` work.
  module WaitReadable; end
  module WaitWritable; end
  class EAGAINWaitReadable < Errno::EAGAIN
    include IO::WaitReadable
  end
  class EAGAINWaitWritable < Errno::EAGAIN
    include IO::WaitWritable
  end
  EWOULDBLOCKWaitReadable = EAGAINWaitReadable
  EWOULDBLOCKWaitWritable = EAGAINWaitWritable
  # Raised by Socket#connect_nonblock while the connect is in flight
  # (`rescue IO::WaitWritable`, then IO.select writable and retry).
  class EINPROGRESSWaitWritable < Errno::EINPROGRESS
    include IO::WaitWritable
  end

  # The sync flag is per-object bookkeeping only: monoruby's writes are
  # unbuffered, so semantically every IO behaves as if sync were on. The
  # flag still round-trips through #sync= and defaults to true for the
  # process's stderr (fd 2), matching CRuby.
  def sync
    raise IOError, "closed stream" if closed?
    s = @sync
    s.nil? ? fileno == 2 : s
  end

  def sync=(v)
    raise IOError, "closed stream" if closed?
    @sync = v ? true : false
    v
  end

  # CRuby's IO#putc: a String argument writes its first character, any
  # other argument is converted with #to_int and the low byte is written.
  def putc(ch)
    if ch.is_a?(String)
      write(ch[0])
    else
      i = ch.is_a?(Integer) ? ch : (ch.respond_to?(:to_int) ? ch.to_int : nil)
      raise TypeError, "no implicit conversion of #{ch.class} into Integer" if i.nil?
      write((i & 0xff).chr)
    end
    ch
  end

  # CRuby raises EOFError (not RuntimeError) at end of stream for the
  # `read*` family; #getbyte/#getc return nil at EOF.
  def readbyte
    raise IOError, "closed stream" if closed?
    b = getbyte
    raise EOFError, "end of file reached" if b.nil?
    b
  end

  def readchar
    raise IOError, "closed stream" if closed?
    c = getc
    raise EOFError, "end of file reached" if c.nil?
    c
  end

  def each_byte
    raise IOError, "closed stream" if closed?
    return to_enum(:each_byte) { nil } unless block_given?
    while (b = getbyte)
      yield b
    end
    self
  end

  def each_char
    raise IOError, "closed stream" if closed?
    return to_enum(:each_char) { nil } unless block_given?
    while (c = getc)
      yield c
    end
    self
  end

  def each_codepoint
    raise IOError, "closed stream" if closed?
    return to_enum(:each_codepoint) { nil } unless block_given?
    each_char { |c| yield c.ord }
    self
  end

  # Enumerator-replay helper: positional-only arguments survive the
  # to_enum round-trip (a `chomp:` keyword would come back as a
  # positional Hash and be misparsed as a separator/limit).
  def __each_line(args, chomp)
    # #gets implements the full (sep, limit, chomp:) semantics natively.
    while (line = gets(*args, chomp: chomp))
      yield line
    end
    self
  end
  private :__each_line

  def each_line(*args, chomp: false, **)
    raise IOError, "closed stream" if closed?
    # CRuby rejects a zero limit up front (a zero-limit #gets would
    # return "" forever). The limit is the second positional argument,
    # or the first when it is not a (nil/String) separator.
    lim = if args.size >= 2
            args[1]
          elsif args.size == 1 && !args[0].nil? && !args[0].is_a?(String)
            args[0]
          end
    if lim.is_a?(Integer) && lim == 0
      raise ArgumentError, "invalid limit: 0 for each_line"
    end
    unless block_given?
      return to_enum(:__each_line, args, chomp) { nil }
    end
    __each_line(args, chomp) { |line| yield line }
  end
  alias each each_line

  def self.for_fd(fd, mode = nil, **opts)
    new(fd, mode, **opts)
  end

  # CRuby's rb_io_s_open: `new(*args)` plus block handling — the IO is
  # closed when the block exits (via #close dispatch, so overrides run);
  # an IOError meaning "already closed" is swallowed, everything else
  # (StandardError or not) propagates.
  def self.open(*args, **opts)
    io = new(*args, **opts)
    return io unless block_given?
    begin
      yield io
    ensure
      begin
        io.close
      rescue IOError => e
        raise unless e.message.include?("closed stream")
      end
    end
  end
end
