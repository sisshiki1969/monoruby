# Raised when a `case/in` exhausts its branches or an `expr => pattern`
# match fails (the pattern-matching desugar references these by name).
class NoMatchingPatternError < StandardError
end

class NoMatchingPatternKeyError < NoMatchingPatternError
  def initialize(message = nil, matchee: nil, key: nil)
    @matchee = matchee
    @key = key
    super(message)
  end

  attr_reader :matchee, :key
end

class Exception
  class << self
    # `Exception.exception(...)` is an alias of `.new`, inherited by every
    # exception subclass through the singleton-class chain. Defined here
    # (rather than as a Rust class func) so it rides the generic
    # `Class#new` dispatch path, which honors subclass-overridden
    # `initialize` including keyword arguments.
    def exception(...) = new(...)
  end

  def backtrace_locations
    # Locations come from the raise-time capture, independent of any
    # string backtrace installed via #set_backtrace (CRuby: after
    # `set_backtrace(strings)` on a never-raised exception,
    # #backtrace_locations stays nil). Memoized so repeated calls
    # return the same, mutable Array.
    # A prior #backtrace_locations memo, or locations installed via
    # set_backtrace(locations), win over re-deriving from the capture.
    return @backtrace_locations if defined?(@backtrace_locations) && @backtrace_locations
    frames = __raise_backtrace
    return nil if frames.nil?
    @backtrace_locations = frames.map { |f| Thread::Backtrace::Location.new(f) }
  end

  # CRuby 3.4+: `set_backtrace` (and thus `$@ =`) also accepts an Array
  # of `Thread::Backtrace::Location`, stored as their string forms. Mixed
  # or otherwise invalid arrays fall through to the native checker, which
  # raises the usual TypeError.
  def set_backtrace(bt)
    if bt.is_a?(Array) && !bt.empty? && bt.all? { |e| Thread::Backtrace::Location === e }
      # CRuby 3.4+: an Array of Locations sets both the string backtrace
      # and #backtrace_locations.
      __set_backtrace(bt.map(&:to_s))
      @backtrace_locations = bt
    else
      # Setting a string backtrace leaves #backtrace_locations as it was
      # (the raise-time capture, or nil), so don't touch the memo here.
      __set_backtrace(bt)
    end
  end

  # CRuby's Exception#inspect goes through #to_s (which user classes
  # may override): `#<ClassName: to_s>`, or just the class name when
  # #to_s returns an empty string.
  def inspect
    s = to_s
    if s.nil? || s.empty?
      self.class.name || self.class.to_s
    else
      "#<#{self.class}: #{s}>"
    end
  end

  # Whether the uncaught-exception report would be written to a tty —
  # the default for `full_message`'s :highlight option.
  # `highlight:` is a strict boolean in CRuby — anything but `true`,
  # `false` or `nil` is an ArgumentError naming the value. `nil` means
  # "not given", so each caller applies its own default.
  def self.__check_highlight(highlight)
    unless highlight == true || highlight == false || highlight.nil?
      raise ArgumentError, "expected true or false as highlight: #{highlight.inspect}"
    end
    highlight
  end

  def self.to_tty?
    $stderr.equal?(STDERR) && STDERR.tty?
  rescue NoMethodError
    false
  end

  def full_message(highlight: nil, order: :top, **opts)
    # CRuby's first line is `bt[0]: <detailed_message>` (i.e.
    # "message (ClassName)"), followed by `\tfrom <frame>` lines.
    # `--backtrace-limit=N` truncates the from-lines to N entries plus
    # a `\t ... K levels...` marker, exactly like the top-level
    # uncaught-error report. All keyword arguments except :order are
    # forwarded to #detailed_message, with :highlight resolved to its
    # default first.
    highlight = Exception.__check_highlight(highlight)
    highlight = Exception.to_tty? if highlight.nil?
    msg = nil
    if respond_to?(:detailed_message)
      dm = detailed_message(highlight: highlight, **opts)
      dm = dm.to_str if !dm.is_a?(String) && dm.respond_to?(:to_str)
      msg = dm if dm.is_a?(String)
    end
    if msg.nil?
      # No usable #detailed_message: fall back to the class name.
      msg = highlight ? "\e[1;4m#{self.class}\e[m" : self.class.to_s
    end
    bt = backtrace
    # An exception that was never raised has no backtrace; CRuby shows
    # the caller of full_message instead.
    bt = caller if bt.nil? || bt.empty?
    if bt && !bt.empty?
      rest = bt[1..]
      limit = Kernel.__backtrace_limit
      trailer = nil
      if limit && rest.size > limit
        trailer = "\t ... #{rest.size - limit} levels...\n"
        rest = rest[0, limit]
      end
      out = if order == :bottom
        header = highlight ? "\e[1mTraceback\e[m (most recent call last):\n"
                           : "Traceback (most recent call last):\n"
        # CRuby numbers the reversed frames by their distance from the
        # error line: `\tN: from <frame>` counting down to 1.
        numbered = []
        i = rest.size
        rest.each do |l|
          numbered << "\t#{i}: from #{l}\n"
          i -= 1
        end
        numbered << trailer if trailer
        header + numbered.reverse.join + "#{bt[0]}: #{msg}\n"
      else
        lines = rest.map { |l| "\tfrom #{l}\n" }
        lines << trailer if trailer
        "#{bt[0]}: #{msg}\n" + lines.join
      end
    else
      out = msg + "\n"
    end
    # Chain the cause's report so every exception in the chain appears.
    c = cause
    out += c.full_message(highlight: highlight, order: :top) if c
    out
  end

  # CRuby `Exception#detailed_message(highlight: false, **)`:
  # decorate the message with the class name; empty-message and
  # anonymous-class cases have special forms.
  def detailed_message(highlight: false, **)
    highlight = Exception.__check_highlight(highlight) || false
    msg = message.to_s
    cls = self.class
    if msg.empty?
      base = instance_of?(::RuntimeError) ? "unhandled exception" : (cls.name || cls.to_s)
      return highlight ? "\e[1;4m#{base}\e[m" : base
    end
    first, *rest = msg.split("\n", -1)
    # With :highlight every line of a multi-line message is bolded
    # individually (`\e[1m…\e[m` per line).
    if cls.name.nil?
      head = highlight ? "\e[1m#{first}\e[m" : first
    elsif highlight
      head = "\e[1m#{first} (\e[1;4m#{cls}\e[m\e[1m)\e[m"
    else
      head = "#{first} (#{cls})"
    end
    if highlight
      rest = rest.map { |l| l.empty? ? l : "\e[1m#{l}\e[m" }
    end
    ([head] + rest).join("\n")
  end
end

class FloatDomainError < RangeError; end
