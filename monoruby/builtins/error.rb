# `Errno::E*` exception classes and their `Errno` constants are populated
# by Rust-side `errno::init` (see monoruby/src/builtins/errno.rs), which
# walks the host's `libc::E*` table at startup so any errno present on the
# system (`Errno::ENETDOWN`, `Errno::EPROTOTYPE`, ...) is reachable.
# Here we attach CRuby-compatible construction on top:
#   SystemCallError.new(errno)            -> Errno::X instance (by errno)
#   SystemCallError.new(msg, errno[, loc])-> Errno::X instance
#   Errno::X.new([msg[, location]])       -> "strerror [@ loc] - msg"
#   SystemCallError#errno                 -> the errno Integer
class SystemCallError
  # Append `str` to the message `buf` as CRuby's `rb_str_catf` appends a
  # `%PRIsVALUE` argument: the result takes the encoding the two are
  # compatible in — so ASCII text keeps the locale encoding and non-ASCII
  # text brings its own — and an incompatible `str` is converted into
  # `buf`'s encoding, with replacement characters for what does not map.
  def self.__syserr_cat(buf, str)
    if (enc = Encoding.compatible?(buf, str))
      buf.force_encoding(enc)
    else
      str = str.encode(buf.encoding, invalid: :replace, undef: :replace)
    end
    buf << str
  end

  def self.__errno_class(n)
    @__errno_map ||= begin
      map = {}
      Errno.constants.each do |c|
        k = Errno.const_get(c)
        next unless k.is_a?(Class) && k < SystemCallError
        e = k.const_defined?(:Errno) ? k.const_get(:Errno) : nil
        map[e] = k if e.is_a?(Integer) && !map.key?(e)
      end
      map
    end
    @__errno_map[n]
  end

  # `SystemCallError.new(errno)` / `.new(msg, errno[, location])`
  # constructs the matching `Errno::*` subclass; an unknown errno stays
  # a plain SystemCallError. Subclasses (including user-defined ones)
  # construct normally through their own #initialize.
  # CRuby coercion for the errno argument: Integers pass through,
  # other Numerics truncate (a Complex must have zero imaginary part),
  # anything else needs #to_int.
  def self.__coerce_errno(errno)
    return errno if errno.is_a?(Integer)
    if errno.is_a?(Complex)
      unless errno.imaginary == 0
        raise RangeError, "can't convert #{errno} into Integer"
      end
      return errno.real.to_i
    end
    return errno.to_i if errno.is_a?(Numeric)
    unless errno.respond_to?(:to_int)
      raise TypeError, "no implicit conversion of #{__builtin_class_name(errno)} into Integer"
    end
    errno.to_int
  end

  def self.new(*args)
    return super unless self.equal?(SystemCallError)
    if args.empty?
      raise ArgumentError, "wrong number of arguments (given 0, expected 1..3)"
    end
    if args.size >= 2
      msg, orig, loc = args[0], args[1], args[2]
    elsif args[0].is_a?(String) || args[0].nil?
      msg, orig, loc = args[0], nil, nil
    else
      msg, orig, loc = nil, args[0], nil
    end
    errno = orig.nil? ? nil : __coerce_errno(orig)
    klass = errno.nil? ? nil : __errno_class(errno)
    obj = (klass || self).allocate
    # #errno reports the errno as given (2.9 stays 2.9); only the
    # class lookup and the message use the Integer coercion.
    obj.__send__(:__syserr_init, msg, errno, loc, orig)
    obj
  end

  # CRuby reports arity -1 for SystemCallError#initialize.
  def initialize(*args)
    if args.size >= 2
      msg, orig, loc = args[0], args[1], args[2]
    elsif args[0].is_a?(String) || args[0].nil?
      msg, orig, loc = args[0], nil, nil
    else
      msg, orig, loc = nil, args[0], nil
    end
    errno = orig.nil? ? nil : SystemCallError.__coerce_errno(orig)
    if errno.nil? && self.class.const_defined?(:Errno)
      e = self.class.const_get(:Errno)
      errno = e if e.is_a?(Integer)
      orig = errno
    end
    __syserr_init(msg, errno, loc, orig)
  end

  def errno
    defined?(@__errno) ? @__errno : nil
  end

  private def __syserr_init(msg, errno, loc, reported_errno = nil)
    unless msg.nil? || msg.is_a?(String)
      if msg.respond_to?(:to_str)
        msg = msg.to_str
      else
        raise TypeError, "no implicit conversion of #{__builtin_class_name(msg)} into String"
      end
    end
    @__errno = reported_errno.nil? ? errno : reported_errno
    # CRuby's `NUM2INT(error)` before `strerror`.
    if errno && (errno < -0x80000000 || errno > 0x7fffffff)
      raise RangeError, "integer #{errno} too #{errno > 0 ? 'big' : 'small'} to convert to 'int'"
    end
    # `strerror(errno)` as a String in the locale encoding, and the
    # location and message appended the way `rb_str_catf` appends a
    # `%PRIsVALUE` (see `__syserr_cat`). CRuby names the location only
    # together with a message.
    base = SystemCallError.__strerror(errno)
    unless msg.nil?
      unless loc.nil?
        base << " @ "
        SystemCallError.__syserr_cat(base, loc.to_s)
      end
      base << " - "
      SystemCallError.__syserr_cat(base, msg)
    end
    ::Exception.instance_method(:initialize).bind(self).call(base)
  end
end

module Errno
  # Give every generated Errno::E* class the CRuby constructor
  # signature `new(msg = nil, location = nil)`.
  constants.each do |c|
    k = const_get(c)
    next unless k.is_a?(Class) && k < SystemCallError
    k.class_eval do
      def initialize(msg = nil, loc = nil)
        e = self.class.const_defined?(:Errno) ? self.class.const_get(:Errno) : nil
        e = nil unless e.is_a?(Integer)
        __syserr_init(msg, e, loc, e)
      end
    end
  end
end

class SignalException < Exception
  # SignalException.new(signo), .new(:TERM), .new("SIGTERM", msg), ...
  def initialize(sig = nil, message = nil)
    if sig.is_a?(Integer)
      name = Signal.signame(sig)
      raise ArgumentError, "invalid signal number (#{sig})" unless name
      @__signo = sig
      super(message || "SIG#{name}")
    elsif !sig.nil?
      # CRuby: a message is only accepted together with a *numeric* signal;
      # a signal name plus a message is an ArgumentError.
      unless message.nil?
        raise ArgumentError, "wrong number of arguments (given 2, expected 1)"
      end
      name = sig.to_s.sub(/\ASIG/, "")
      signo = Signal.list[name]
      raise ArgumentError, "unsupported signal `SIG#{name}'" unless signo
      @__signo = signo
      super("SIG#{name}")
    else
      super("SignalException")
    end
  end

  def signo
    return @__signo if defined?(@__signo) && @__signo
    # A SignalException materialized by the runtime's signal conversion
    # (delivered SIGTERM etc.) is built without #initialize, so derive the
    # signo from the message ("SIGTERM" -> 15). Interrupt's message is
    # "Interrupt", not a signal name; it is always SIGINT.
    return Signal.list["INT"] if is_a?(Interrupt)
    Signal.list[message.to_s.sub(/\ASIG/, "")]
  end

  # The signal description is the exception message.
  def signm
    message
  end
end

class Interrupt < SignalException
  def initialize(message = nil)
    @__signo = Signal.list["INT"]
    # Interrupt's default message is its class name, not "SIGINT";
    # bypass SignalException#initialize.
    ::Exception.instance_method(:initialize).bind(self).call(message || "Interrupt")
  end
end
