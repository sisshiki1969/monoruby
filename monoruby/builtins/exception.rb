# monoruby-hidden-ivars: true

# Raised when a `case/in` exhausts its branches or an `expr => pattern`
# match fails (the pattern-matching desugar references these by name).
class NoMatchingPatternError < StandardError
end

class NoMatchingPatternKeyError < NoMatchingPatternError
  def initialize(message = nil, matchee: nil, key: nil)
    @__matchee = matchee
    @__key = key
    super(message)
  end

  def matchee
    @__matchee
  end

  def key
    @__key
  end
end

class UncaughtThrowError
  # `UncaughtThrowError.new(tag, value, message = nil)`
  # (`uncaught_throw_init`: at least two arguments, the rest handed to
  # `Exception#initialize`).
  def initialize(*args)
    if args.size < 2
      raise ArgumentError, "wrong number of arguments (given #{args.size}, expected 2+)"
    end
    @__tag = args[0]
    @__value = args[1]
    super(*args[2..])
  end

  # `uncaught_throw_to_s`: the message is a format, applied to the tag
  # each time it is read (`rb_str_format(1, &tag, mesg)`) — `throw`
  # raises this with "uncaught throw %p". With no message there is no
  # format, which is a TypeError as it is in CRuby.
  def to_s
    Kernel.format(__mesg, @__tag)
  end
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
    return @__backtrace_locations if defined?(@__backtrace_locations) && @__backtrace_locations
    frames = __raise_backtrace
    return nil if frames.nil?
    sites = __raise_backtrace_sites
    locs = []
    i = 0
    n = frames.size
    while i < n
      locs << Thread::Backtrace::Location.new(frames[i], sites[i])
      i += 1
    end
    @__backtrace_locations = locs
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
      @__backtrace_locations = bt
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
      __sprintf_repr("#<#{self.class}: #{s}>")
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
      msg = __check_str(detailed_message(highlight: highlight, **opts))
    end
    if msg.nil?
      # No usable #detailed_message: fall back to the class name.
      msg = highlight ? "\e[1;4m#{self.class}\e[m" : self.class.to_s
    end
    bt = backtrace
    # An exception that was never raised has no backtrace: the report
    # then has the one position `error_pos_str` gives, where this method
    # was called, labelled with the name it was called by — and no
    # further frames.
    if bt.nil? || bt.empty?
      loc = caller_locations(1, 1)
      loc = loc[0] if loc
      bt = loc ? ["#{loc.path}:#{loc.lineno}:in '#{__callee__}'"] : nil
    end
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
    # `rb_str_new` grown by appends: BINARY while it is all ASCII (#2090).
    __sprintf_repr(out)
  end

  # CRuby `Exception#detailed_message(highlight: false, **)`:
  # decorate the message with the class name; empty-message and
  # anonymous-class cases have special forms. The result is US-ASCII
  # while it is all ASCII, whatever the message's encoding (#2090).
  def detailed_message(highlight: (default_highlight = true; false), **opts)
    if (::NameError === self || ::KeyError === self || ::LoadError === self ||
        ::NoMatchingPatternKeyError === self) && ::Object.autoload?(:DidYouMean)
      m = __did_you_mean_message(default_highlight, highlight, opts)
      return m if m
    end
    if (::NameError === self || ::TypeError === self || ::ArgumentError === self) &&
       ::Object.autoload?(:ErrorHighlight)
      m = __error_highlight_message(highlight, opts)
      return m if m
    end
    __usascii_repr(__detailed_message(highlight))
  end

  # did_you_mean is loaded here, by the first message it would have
  # added a suggestion to, rather than at boot
  # (builtins/did_you_mean_prelude.rb). As with error_highlight below,
  # the module it prepends to the receiver's class comes too late for
  # the call under way, so this once its method is called directly; its
  # `super` goes on to `ErrorHighlight::CoreExt` (loaded underneath it,
  # see `Module#prepend`) and back to `#detailed_message` above.
  #
  # `Correctable#detailed_message` defaults `highlight:` to true, where
  # `Exception#detailed_message` defaults it to false, so a call that
  # did not say is passed on without one.
  private def __did_you_mean_message(default_highlight, highlight, opts)
    return nil unless ::Exception.__load_did_you_mean(self)
    m = ::DidYouMean::Correctable.instance_method(:detailed_message)
    default_highlight ? m.bind_call(self, **opts) : m.bind_call(self, highlight: highlight, **opts)
  end

  # Run the pending autoload of `DidYouMean`, if there is one, and say
  # whether *exc* (when given) is one the library corrects by now.
  # Called for the first message it would add a suggestion to (above),
  # by the stand-ins for `Correctable`'s methods
  # (builtins/did_you_mean_prelude.rb) and by `Module#prepend` on one of
  # the classes it prepends its module to, so that the module ends up
  # underneath the others, where a library loaded at boot has it.
  def self.__load_did_you_mean(exc = nil)
    if ::Object.autoload?(:DidYouMean)
      begin
        ::DidYouMean
      rescue ::LoadError
        # CRuby's gem_prelude says so once and goes on without it.
        ::Object.__send__(:remove_const, :DidYouMean) if ::Object.autoload?(:DidYouMean)
        warn "`did_you_mean' was not loaded."
        return false
      end
    end
    !!(defined?(::DidYouMean::Correctable) && ::DidYouMean::Correctable === exc)
  end

  # error_highlight is loaded here, by the first message it would have
  # decorated, rather than at boot (builtins/gem_prelude.rb). Loading it
  # prepends `ErrorHighlight::CoreExt` to the receiver's class — too late
  # for the call that is already under way, which was dispatched past
  # where the module now sits. So this once the module's method is
  # called directly; its `super` comes back to `#detailed_message`
  # above, which by then finds nothing left to load.
  private def __error_highlight_message(highlight, opts)
    return nil unless ::Exception.__load_error_highlight
    return nil unless defined?(::ErrorHighlight::CoreExt) && ::ErrorHighlight::CoreExt === self
    ::ErrorHighlight::CoreExt.instance_method(:detailed_message)
      .bind_call(self, highlight: highlight, **opts)
  end

  # Run the pending autoload of `ErrorHighlight`, if there is one, and
  # say whether it ran. Called for the first message the library would
  # decorate (above) and by `Module#prepend` on one of the three classes
  # it prepends its module to, so that the module ends up underneath the
  # others, where a library loaded at boot has it.
  def self.__load_error_highlight
    return false unless ::Object.autoload?(:ErrorHighlight)
    begin
      ::ErrorHighlight
    rescue ::LoadError
      # CRuby's gem_prelude says so once and goes on without it.
      ::Object.__send__(:remove_const, :ErrorHighlight) if ::Object.autoload?(:ErrorHighlight)
      warn "`error_highlight' was not loaded."
      return false
    end
    true
  end

  private def __detailed_message(highlight)
    highlight = Exception.__check_highlight(highlight) || false
    msg = message.to_s
    cls = self.class
    if msg.empty?
      base = instance_of?(::RuntimeError) ? "unhandled exception" : (cls.name || cls.to_s)
      return highlight ? "\e[1;4m#{base}\e[m" : base
    end
    # Lines are told apart by the byte, as `rb_decorate_message` does:
    # a message need not be valid in its encoding (a SyntaxError quotes
    # its source).
    enc = msg.encoding
    first, *rest = msg.b.split("\n".b, -1).map { |l| l.force_encoding(enc) }
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

  # What the report of an uncaught exception prints for this one
  # (`rb_error_write`): its #detailed_message, when that is a String.
  def __report_message
    m = detailed_message(highlight: false)
    m if ::String === m
  end
  private :__report_message
end

class FloatDomainError < RangeError; end
