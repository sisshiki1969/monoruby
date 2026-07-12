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
  # strerror(3) text for the errnos whose message CRuby tests exercise;
  # anything else falls back to "Unknown error N".
  STRERROR = {
    "ENOENT" => "No such file or directory",
    "EEXIST" => "File exists",
    "EACCES" => "Permission denied",
    "ENOTEMPTY" => "Directory not empty",
    "ECONNRESET" => "Connection reset by peer",
    "ETIMEDOUT" => "Connection timed out",
    "ECONNREFUSED" => "Connection refused",
    "EISDIR" => "Is a directory",
    "ENOTDIR" => "Not a directory",
    "EPERM" => "Operation not permitted",
    "EINVAL" => "Invalid argument",
    "EIO" => "Input/output error",
    "EAGAIN" => "Resource temporarily unavailable",
    "EPIPE" => "Broken pipe",
    "EBADF" => "Bad file descriptor",
    "ENOMEM" => "Cannot allocate memory",
    "EBUSY" => "Device or resource busy",
    "EROFS" => "Read-only file system",
    "ENOSPC" => "No space left on device",
    "EADDRINUSE" => "Address already in use",
    "ECHILD" => "No child processes",
    "EINTR" => "Interrupted system call",
    "EDOM" => "Numerical argument out of domain",
    "ERANGE" => "Numerical result out of range",
    "ESRCH" => "No such process",
    "ENXIO" => "No such device or address",
    "ESPIPE" => "Illegal seek",
    "EXDEV" => "Invalid cross-device link",
    "ENODEV" => "No such device",
    "EMFILE" => "Too many open files",
    "ELOOP" => "Too many levels of symbolic links",
    "ENAMETOOLONG" => "File name too long",
  }

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
  def self.new(*args)
    return super unless self.equal?(SystemCallError)
    if args.empty?
      raise ArgumentError, "wrong number of arguments (given 0, expected 1..3)"
    end
    if args[0].is_a?(String)
      msg, errno, loc = args[0], args[1], args[2]
    else
      errno, msg, loc = args[0], nil, nil
    end
    errno = errno.to_int if errno.is_a?(Float)
    klass = __errno_class(errno)
    obj = (klass || self).allocate
    obj.__send__(:__syserr_init, msg, errno, loc)
    obj
  end

  # CRuby reports arity -1 for SystemCallError#initialize.
  def initialize(*args)
    if args[0].is_a?(String)
      msg, errno, loc = args[0], args[1], args[2]
    else
      errno, msg, loc = args[0], nil, nil
    end
    errno = errno.to_int if errno.is_a?(Float)
    if errno.nil? && self.class.const_defined?(:Errno)
      e = self.class.const_get(:Errno)
      errno = e if e.is_a?(Integer)
    end
    __syserr_init(msg, errno, loc)
  end

  def errno
    defined?(@__errno) ? @__errno : nil
  end

  private def __syserr_init(msg, errno, loc)
    @__errno = errno
    name = self.class.name.to_s.split("::").last
    base = STRERROR[name]
    base ||= errno ? "Unknown error #{errno}" : (name.empty? ? "unknown error" : name)
    base = "#{base} @ #{loc}" unless loc.nil?
    base = "#{base} - #{msg}" unless msg.nil?
    ::Exception.instance_method(:initialize).bind(self).call(base)
  end
end

class Errno
  # Give every generated Errno::E* class the CRuby constructor
  # signature `new(msg = nil, location = nil)`.
  constants.each do |c|
    k = const_get(c)
    next unless k.is_a?(Class) && k < SystemCallError
    k.class_eval do
      def initialize(msg = nil, loc = nil)
        e = self.class.const_defined?(:Errno) ? self.class.const_get(:Errno) : nil
        __syserr_init(msg, e.is_a?(Integer) ? e : nil, loc)
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
      name = sig.to_s.sub(/\ASIG/, "")
      signo = Signal.list[name]
      raise ArgumentError, "unsupported signal `SIG#{name}'" unless signo
      @__signo = signo
      super(message || "SIG#{name}")
    else
      super("SignalException")
    end
  end

  def signo
    defined?(@__signo) ? @__signo : nil
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
