# frozen_string_literal: true
#
# `io/console` for monoruby.
#
# CRuby ships this as a C extension (ext/io/console/console.c). Here the
# public surface is Ruby, over a handful of hidden builtins that wrap
# termios(3) and the winsize ioctls (`src/builtins/io_console.rs`):
#
#   IO#__tcgetattr(with_path)        -> opaque termios blob (String)
#   IO#__tcsetattr(blob, with_path)
#   IO.__termios_raw(blob, min, time, intr) / __termios_cooked(blob)
#   IO.__termios_echo(blob, flag) / __termios_echo?(blob)
#   IO#__tcflush(queue) / IO#__winsize / IO#__set_winsize(r, c, x, y)
#   IO#__ttyname
#
# `with_path` picks the Errno message form: console.c's `sys_fail(io)`
# appends the stream's path (`Inappropriate ioctl for device - <STDOUT>`)
# for the direct setters (`raw!`, `echo=`, `winsize`, …), while the
# `ttymode` wrapper used by the block forms (`raw`, `noecho`, `getch`, …)
# raises the bare description.
#
# The escape-sequence methods (`goto`, `cursor`, `erase_line`, …) write
# the same CSI sequences as the non-Windows branches of console.c.
# `pressed?` and `check_winsize_changed` are Windows-only there and raise
# NotImplementedError, as they do in CRuby on Unix.

class IO
  # An opaque terminal attribute set (`struct termios`), obtained from
  # `IO#console_mode` and applied with `IO#console_mode=`.
  class ConsoleMode
    class << self
      undef_method :new

      def __new(termios) # :nodoc:
        mode = allocate
        mode.__send__(:__set_termios, termios)
        mode
      end
    end

    def initialize_copy(other)
      __set_termios(other.instance_variable_get(:@__termios).dup)
      self
    end

    # Enables or disables echo back.
    def echo=(flag)
      __set_termios(IO.__termios_echo(@__termios, flag))
      flag
    end

    # Returns a copy in raw mode.
    def raw(min: nil, time: nil, intr: nil)
      dup.raw!(min: min, time: time, intr: intr)
    end

    # Switches this mode to raw mode.
    def raw!(min: nil, time: nil, intr: nil)
      __set_termios(IO.__termios_raw(@__termios, *IO.__rawmode_opt(min, time, intr)))
      self
    end

    private

    def __set_termios(termios)
      @__termios = termios
    end
  end

  class << self
    # console.c's `rawmode_opt`: `min` / `time` (seconds, 1/10 s
    # precision) become VMIN / VTIME bytes, `intr` must be true, false or
    # nil. nil leaves cfmakeraw's defaults (1 / 0) in place.
    def __rawmode_opt(min, time, intr) # :nodoc:
      unless intr.nil? || intr == true || intr == false
        raise ArgumentError, "true or false expected as intr: #{intr}"
      end
      min = __num2int(min) unless min.nil?
      time = __num2int(time * 10) unless time.nil?
      [min, time, intr == true]
    end

    def __num2int(v) # :nodoc:
      case v
      when Integer then v
      when Float then v.to_i
      else
        raise TypeError, "no implicit conversion of #{v.nil? ? 'nil' : v.class} into Integer" unless v.respond_to?(:to_int)
        v.to_int
      end
    end

    # Returns an File instance opened console (`/dev/tty`), memoized;
    # nil when it cannot be opened. With a Symbol, forwards the call to
    # the console (`IO.console(:winsize)`); `IO.console(:close)` closes
    # and forgets it.
    def console(*args)
      sym = args[0]
      unless args.empty? || sym.is_a?(Symbol)
        raise TypeError, "wrong argument type #{sym.class} (expected Symbol)"
      end
      con = File.instance_variable_get(:@__console)
      if con && (!con.is_a?(File) || con.closed?)
        File.instance_variable_set(:@__console, nil)
        con = nil
      end
      if sym == :close && args.size == 1
        if con
          con.close
          File.instance_variable_set(:@__console, nil)
        end
        return nil
      end
      unless con
        begin
          con = File.open("/dev/tty", "r+")
        rescue SystemCallError
          return nil
        end
        con.sync = true
        File.instance_variable_set(:@__console, con)
      end
      sym ? con.__send__(*args) : con
    end
  end

  # ---- terminal modes ----------------------------------------------

  # console.c's `ttymode`: apply a transformed attribute set for the
  # duration of the block and restore the original afterwards. A failed
  # restore raises even when the block itself raised.
  def __ttymode(transform) # :nodoc:
    saved = __tcgetattr(false)
    __tcsetattr(transform.call(saved), false)
    begin
      yield self
    ensure
      __tcsetattr(saved, false)
    end
  end
  private :__ttymode

  # Yields self within raw mode and returns the block's result.
  def raw(min: nil, time: nil, intr: nil, &block)
    opts = IO.__rawmode_opt(min, time, intr)
    __ttymode(->(t) { IO.__termios_raw(t, *opts) }, &block)
  end

  # Enables raw mode; returns self.
  def raw!(min: nil, time: nil, intr: nil)
    opts = IO.__rawmode_opt(min, time, intr)
    __tcsetattr(IO.__termios_raw(__tcgetattr(true), *opts), true)
    self
  end

  # Yields self within cooked mode.
  def cooked(&block)
    __ttymode(->(t) { IO.__termios_cooked(t) }, &block)
  end

  # Enables cooked mode; returns self.
  def cooked!
    __tcsetattr(IO.__termios_cooked(__tcgetattr(true)), true)
    self
  end

  # Reads and returns a character in raw mode.
  def getch(min: nil, time: nil, intr: nil)
    raw(min: min, time: time, intr: intr) { getc }
  end

  # Yields self with echo back disabled.
  def noecho(&block)
    __ttymode(->(t) { IO.__termios_echo(t, false) }, &block)
  end

  # Enables/disables echo back.
  def echo=(flag)
    __tcsetattr(IO.__termios_echo(__tcgetattr(true), flag), true)
    flag
  end

  # Returns true if echo back is enabled.
  def echo?
    IO.__termios_echo?(__tcgetattr(true))
  end

  # Returns the current console mode as an IO::ConsoleMode.
  def console_mode
    IO::ConsoleMode.__new(__tcgetattr(true))
  end

  # Sets the console mode.
  def console_mode=(mode)
    unless mode.is_a?(IO::ConsoleMode)
      raise TypeError, "wrong argument type #{mode.class} (expected console-mode)"
    end
    __tcsetattr(mode.instance_variable_get(:@__termios), true)
    mode
  end

  # ---- window size, flushing, tty name ------------------------------

  # Returns console size as `[rows, columns]`.
  def winsize
    __winsize
  end

  # Tries to set console size: `[rows, columns]` or
  # `[rows, columns, xpixels, ypixels]`.
  def winsize=(size)
    size = Array(size)
    unless size.size == 2 || size.size == 4
      raise ArgumentError, "wrong number of arguments (given #{size.size}, expected 2 or 4)"
    end
    row, col, xpixel, ypixel = size
    __set_winsize(*[row, col, xpixel, ypixel].map { |v| v.nil? ? 0 : IO.__num2int(v) })
    size
  end

  # Flushes input buffer in kernel.
  def iflush
    __tcflush(0)
    self
  end

  # Flushes output buffer in kernel.
  def oflush
    __tcflush(1)
    self
  end

  # Flushes input and output buffers in kernel.
  def ioflush
    __tcflush(2)
    self
  end

  # Returns name of associated terminal (tty) if self is a tty, or nil.
  def ttyname
    __ttyname
  end

  # ---- escape sequences ---------------------------------------------

  CSI = "\e[" # :nodoc:

  def __mode_in_range(val, high, modename) # :nodoc:
    return 0 if val.nil?
    unless val.is_a?(Integer) && val >= 0 && val <= high
      raise ArgumentError, "wrong #{modename} mode: #{val}"
    end
    val
  end
  private :__mode_in_range

  def __move(y, x) # :nodoc:
    if x != 0 || y != 0
      s = +""
      s << CSI << y.abs.to_s << (y < 0 ? "A" : "B") if y != 0
      s << CSI << x.abs.to_s << (x < 0 ? "D" : "C") if x != 0
      write(s)
      flush
    end
    self
  end
  private :__move

  def __scroll(line) # :nodoc:
    write("#{CSI}#{line.abs}#{line < 0 ? 'T' : 'S'}") if line != 0
    self
  end
  private :__scroll

  # Writes a query and parses the terminal's `ESC [ n ; m c` reply into
  # `[n, m, "c"]` (console.c's `read_vt_response`); nil on a malformed
  # reply.
  def __read_vt_response(query) # :nodoc:
    write(query)
    flush
    return nil unless getbyte == 0x1b
    return nil unless getbyte == 0x5b
    result = []
    num = 0
    while (b = getbyte)
      if b == 0x3b
        result << num
        num = 0
      elsif b >= 0x30 && b <= 0x39
        num = num * 10 + b - 0x30
      else
        result << num
        b = b.chr
        break
      end
    end
    result << b
  end
  private :__read_vt_response

  # Beeps on the output console.
  def beep
    write("\a")
    self
  end

  # Returns the current cursor position as `[row, column]` (zero-based).
  def cursor
    resp = raw { __read_vt_response("#{CSI}6n") }
    return nil unless resp.is_a?(Array) && resp.size == 3
    term = resp[2]
    return nil unless term.is_a?(String) && term == "R"
    [resp[0] - 1, resp[1] - 1]
  end

  # Moves the cursor to `[row, column]`.
  def cursor=(pos)
    ary = Array.try_convert(pos)
    raise TypeError, "no implicit conversion of #{pos.class} into Array" unless ary
    raise ArgumentError, "expected 2D coordinate" unless ary.size == 2
    goto(ary[0], ary[1])
  end

  # Moves the cursor to the given row and column (zero-based).
  def goto(y, x)
    write("#{CSI}#{IO.__num2int(y) + 1};#{IO.__num2int(x) + 1}H")
    self
  end

  # Moves the cursor to the given column (zero-based).
  def goto_column(x)
    write("#{CSI}#{IO.__num2int(x) + 1}G")
    self
  end

  def cursor_up(n)
    __move(-IO.__num2int(n), 0)
  end

  def cursor_down(n)
    __move(IO.__num2int(n), 0)
  end

  def cursor_left(n)
    __move(0, -IO.__num2int(n))
  end

  def cursor_right(n)
    __move(0, IO.__num2int(n))
  end

  # Erases the line: 0 = after cursor, 1 = before and cursor, 2 = whole.
  def erase_line(mode)
    write("#{CSI}#{__mode_in_range(mode, 2, 'line erase')}K")
    self
  end

  # Erases the screen: 0 = after cursor, 1 = before and cursor,
  # 2 = whole screen, 3 = whole screen and scrollback.
  def erase_screen(mode)
    write("#{CSI}#{__mode_in_range(mode, 3, 'screen erase')}J")
    self
  end

  def scroll_forward(n)
    __scroll(IO.__num2int(n))
  end

  def scroll_backward(n)
    __scroll(-IO.__num2int(n))
  end

  # Clears the entire screen and moves the cursor top-left.
  def clear_screen
    erase_screen(2)
    goto(0, 0)
  end

  # ---- input ----------------------------------------------------------

  # Reads and returns a line without echo back; the prompt (if any) and
  # the trailing newline go to the stream itself, or to $stderr when
  # reading $stdin.
  def getpass(prompt = nil)
    wio = equal?($stdin) ? $stderr : self
    unless prompt.nil?
      raise TypeError, "no implicit conversion of #{prompt.class} into String" unless prompt.is_a?(String)
      raise ArgumentError, "string contains null byte" if prompt.include?("\0")
      wio.write(prompt)
    end
    wio.flush
    begin
      str = noecho { gets }
    ensure
      wio.write("\n")
    end
    str&.chomp!
    str
  end

  # Windows only in CRuby; unimplemented here as there.
  def pressed?(_key)
    raise NotImplementedError, "pressed?() function is unimplemented on this machine"
  end

  def check_winsize_changed
    raise NotImplementedError, "check_winsize_changed() function is unimplemented on this machine"
  end
end
