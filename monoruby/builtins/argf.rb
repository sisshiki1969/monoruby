# Minimal ARGF implementation. Full semantics (transparent line-by-line
# reading across ARGV files with $_/$.) are only partially implemented;
# enough shape is provided for specs to load and for simple cases to
# proceed. Deliberately defined after enumerable is loaded so the
# `include Enumerable` on the class body resolves.
class ARGFClass
  include Enumerable

  def initialize(*argv)
    @argv = argv.empty? ? (defined?(::ARGV) ? ::ARGV : []) : argv
    @current_file = nil
    @current_name = nil
    @lineno = 0
    # Set once every input stream has been consumed. ARGF must NOT fall
    # back to $stdin after named ARGV files are exhausted (CRuby reads
    # stdin only when ARGV was empty to begin with) — doing so made
    # `ARGF.read; ARGF.readlines` block forever on stdin.
    @exhausted = false
  end

  def argv
    @argv
  end

  def filename
    @current_name || (@argv.first || '-')
  end
  alias_method :path, :filename

  def file
    __stream || @current_file || $stdin
  end

  # The current open stream, opening the next ARGV file (or binding
  # $stdin when ARGV was empty from the start) as needed. Returns nil
  # once every input has been consumed — never falling back to $stdin
  # after named files are done.
  def __stream
    return @current_file if @current_file && !@current_file.closed?
    return nil if @exhausted
    if @argv.empty?
      if @current_name
        # Named files were consumed earlier; there is no more input.
        @exhausted = true
        return nil
      end
      @current_name = '-'
      @current_file = $stdin
    else
      @current_name = @argv.shift
      @current_file = @current_name == '-' ? $stdin : File.open(@current_name)
    end
    @current_file
  end
  private :__stream

  # Close out the current stream (it hit EOF) and mark ARGF exhausted
  # when no further ARGV entries remain.
  def __finish_stream
    f = @current_file
    f.close if f && !f.closed? && !f.equal?($stdin)
    @current_file = nil
    @exhausted = true if @argv.empty?
  end
  private :__finish_stream

  def advance
    !!__stream
  end

  def lineno;     @lineno; end
  def lineno=(n); @lineno = n; end
  def pos;        0;       end
  alias_method :tell, :pos
  def pos=(n);    n;       end
  def closed?
    !@current_file || @current_file.closed?
  end
  def close
    @current_file.close if @current_file && !@current_file.closed? && @current_file != $stdin
    self
  end
  def eof?
    @argv.empty? && (@current_file.nil? || @current_file.closed?)
  end
  alias_method :eof, :eof?
  def skip
    if @current_file && !@current_file.closed? && @current_file != $stdin
      @current_file.close
    end
    @current_file = nil
    self
  end
  def rewind
    @current_file.rewind if @current_file && !@current_file.closed?
    @lineno = 0
  end
  def each
    return to_enum(:each) unless block_given?
    while (f = __stream)
      f.each_line do |line|
        @lineno += 1
        yield line
      end
      __finish_stream
    end
    self
  end
  alias_method :each_line, :each
  alias_method :lines, :each
  def readlines(*args)
    result = []
    each { |line| result << line }
    result
  end
  # CRuby-compatible ARGF.read (verified against CRuby 4.0.2):
  # - read           -> the concatenation of every remaining stream; ""
  #                     for empty input; nil once ARGF is exhausted.
  # - read(len)      -> up to len bytes, continuing across file
  #                     boundaries; stops at exactly len (this bound is
  #                     what keeps `ARGF.read(100)` on /dev/zero from
  #                     reading forever); nil at EOF; "" for len == 0.
  # - read(len, buf) -> fills and returns buf (same object).
  def read(length = nil, outbuf = nil)
    if length && length < 0
      raise ArgumentError, "negative length #{length} given"
    end
    buf = String.new
    had_stream = false
    read_any = false
    unless length == 0
      loop do
        break if length && buf.bytesize >= length
        f = __stream
        break unless f
        had_stream = true
        need = length && length - buf.bytesize
        chunk = need ? f.read(need) : f.read
        if chunk && !chunk.empty?
          read_any = true
          buf << chunk
        end
        # A short (or nil) chunk means this stream hit EOF; a full read
        # without length always consumes the stream.
        if need.nil? || chunk.nil? || chunk.bytesize < need
          __finish_stream
        end
      end
    end
    result =
      if length.nil?
        had_stream ? buf : nil
      elsif length == 0
        ""
      else
        read_any ? buf : nil
      end
    if outbuf
      outbuf.replace(result || "")
      result = outbuf if result
    end
    result
  end
  def readline(*args)
    line = nil
    each { |l| line = l; break }
    raise EOFError, "end of file reached" if line.nil?
    line
  end
  def gets(*args)
    line = nil
    each { |l| line = l; break }
    line
  end
  def getc;     nil; end
  def readchar; raise EOFError; end
  def getbyte;  nil; end
  def readbyte; raise EOFError; end
  def inspect;  'ARGF'; end
  def to_s;     'ARGF'; end
  def to_a
    readlines
  end
  def fileno
    file.fileno
  end
  alias_method :to_i, :fileno
  def to_io
    file
  end
  def binmode;  self; end
  def binmode?; false; end
  def external_encoding; Encoding.default_external; end
  def internal_encoding; Encoding.default_internal; end
  def inplace_mode;  nil; end
  def inplace_mode=(v); v; end
  def set_encoding(*); self; end
  def write(*args); args.map(&:to_s).join.bytesize; end
  def print(*args); $stdout.print(*args); end
  def puts(*args);  $stdout.puts(*args); end
end

ARGF = ARGFClass.new
