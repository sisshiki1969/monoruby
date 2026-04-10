RUBY_PLATFORM = "x86_64-linux"
RUBY_PATCHLEVEL = 0

require 'rbconfig'

class BasicObject
  private
  def singleton_method_added(name)
  end

  def singleton_method_removed(name)
  end

  def singleton_method_undefined(name)
  end
end

module Kernel
  private

  # Internal helper: coerce value to Integer via to_int.
  # Raises TypeError with CRuby-compatible message if conversion fails.
  def __to_int(val)
    return val if val.is_a?(Integer)
    if val.respond_to?(:to_int)
      result = val.to_int
      unless result.is_a?(Integer)
        raise TypeError, "can't convert #{val.class} into Integer (#{val.class}#to_int gives #{result.class})"
      end
      result
    else
      raise TypeError, "no implicit conversion of #{val.class} into Integer"
    end
  end

  # Internal helper: coerce value to String via to_str.
  # Raises TypeError with CRuby-compatible message if conversion fails.
  def __to_str(val)
    return val if val.is_a?(String)
    if val.respond_to?(:to_str)
      result = val.to_str
      unless result.is_a?(String)
        raise TypeError, "can't convert #{val.class} into String (#{val.class}#to_str gives #{result.class})"
      end
      result
    else
      raise TypeError, "no implicit conversion of #{val.class} into String"
    end
  end
end

class Object
  def initialize(...)
  end

  def tap
    yield self
    self
  end

  def itself
    self
  end

  def then
    yield self
  end
  alias yield_self then

  def respond_to_missing?(method_name, include_private = false)
    false
  end

  def <=>(other)
    0 if equal?(other)
  end
end

class Class
  private
  def inherited(subclass)
  end
end

class Regexp
  IGNORECASE = 1
  EXTENDED = 2
  MULTILINE = 4
end

class Module
  private
  def extended(mod)
  end

  def prepended(mod)
  end

  def included(mod)
  end

  public
  def method_added(name)
  end

  def method_removed(name)
  end

  def method_undefined(name)
  end

  def const_added(name)
  end

  def const_missing(name)
    raise NameError, "uninitialized constant #{self}::#{name}"
  end

  def include?(mod)
    unless mod.is_a?(Module)
      raise TypeError, "wrong argument type #{mod.class} (expected Module)"
    end
    ancestors.include?(mod)
  end
end

module Warning
  @categories = { deprecated: false, experimental: true, performance: false }

  def self.warn(*x)
  end

  def self.[](category)
    category = category.to_sym if category.is_a?(String)
    unless @categories.key?(category)
      raise ArgumentError, "unknown category: #{category}"
    end
    @categories[category]
  end

  def self.[]=(category, value)
    category = category.to_sym if category.is_a?(String)
    unless @categories.key?(category)
      raise ArgumentError, "unknown category: #{category}"
    end
    @categories[category] = value ? true : false
  end
end

class Process
  CLOCK_REALTIME = 0
  CLOCK_MONOTONIC = 1
  CLOCK_PROCESS_CPUTIME_ID = 2
  CLOCK_THREAD_CPUTIME_ID	= 3
  CLOCK_MONOTONIC_RAW	= 4
  CLOCK_REALTIME_COARSE	= 5
  CLOCK_MONOTONIC_COARSE = 6
  CLOCK_BOOTTIME = 7
  CLOCK_REALTIME_ALARM = 8
  CLOCK_BOOTTIME_ALARM = 9
  class Tms
    attr_accessor :utime, :stime, :cutime, :cstime
  end

  class Status
    attr_reader :exitstatus, :pid

    def initialize(exitstatus, pid)
      @exitstatus = exitstatus
      @pid = pid
    end

    def success?
      @exitstatus == 0
    end

    def exited?
      true
    end

    def signaled?
      false
    end

    def to_i
      @exitstatus << 8
    end

    def inspect
      "#<Process::Status: pid #{@pid} exit #{@exitstatus}>"
    end
    alias to_s inspect

    def ==(other)
      if other.is_a?(Integer)
        to_i == other
      else
        super
      end
    end
  end
end

module File::Constants
  FNM_SYSCASE = 0
  FNM_NOESCAPE = 1
  FNM_PATHNAME = 2
  FNM_CASEFOLD = 8
  FNM_EXTGLOB = 16
end

class File
  include File::Constants
  FNM_SYSCASE = 0
  FNM_NOESCAPE = 1
  FNM_PATHNAME = 2
  FNM_DOTMATCH = 4
  FNM_CASEFOLD = 8
  FNM_EXTGLOB = 16
  NULL = "/dev/null"
  BINARY = 0

  RDONLY   = 0
  WRONLY   = 1
  RDWR     = 2
  APPEND   = 1024
  CREAT    = 64
  EXCL     = 128
  TRUNC    = 512
  NONBLOCK = 2048
  LOCK_SH  = 1
  LOCK_EX  = 2
  LOCK_UN  = 8
  LOCK_NB  = 4

  Separator = "/"
  SEPARATOR = "/"
  ALT_SEPARATOR = nil
  PATH_SEPARATOR = ":"
end

class Fiber
  @@main_fiber = nil
  def self.current
    # Return the main fiber (monoruby is single-threaded and doesn't track
    # the current fiber across resume/yield).
    @@main_fiber ||= Fiber.new {}
  end
end

class Signal
  def self.trap(signal, command = nil, &block)
    # No-op for now.
  end
end

class Thread
  # monoruby is single-threaded. Thread.new does NOT execute blocks
  # concurrently. Only Thread.current (the main thread) is meaningful.
  # Methods that require actual concurrency raise NoMethodError.

  def self.current
    @@current
  end

  @@pass_count = 0
  def self.pass
    # monoruby is single-threaded. Thread.pass is a no-op, but if called
    # repeatedly (e.g., `Thread.pass until condition`), the condition will
    # never change. Raise after a safety limit to prevent infinite loops.
    @@pass_count += 1
    if @@pass_count > 1000
      @@pass_count = 0
      raise ThreadError, "Thread.pass called too many times (monoruby is single-threaded)"
    end
  end

  def initialize(*args, &block)
    @value = nil
    @exception = nil
    @alive = false
    @keys = {}
    @thread_local = {}
  end

  @@current = Thread.new
  @@current.instance_variable_set(:@alive, true)

  def value
    @value
  end

  def join(limit = nil)
    self
  end

  def kill
    self
  end

  def alive?
    @alive
  end

  def stop?
    !@alive
  end

  def status
    if @alive
      "run"
    else
      false
    end
  end

  def [](key)
    @keys[key]
  end

  def []=(key, value)
    @keys[key] = value
  end

  def thread_variable_set(key, value)
    @thread_local[key] = value
  end

  def thread_variable_get(key)
    @thread_local[key]
  end

  def self.each_caller_location
    caller_locations(1).each do |loc|
      yield loc
    end
  end

  class Mutex
    def synchronize
      yield
    end
    
    def owned?
      false
    end
  end

  class Queue
    def initialize
      @items = []
    end

    def push(item)
      @items.push(item)
      self
    end
    alias << push
    alias enq push

    def pop(non_block = false, timeout: nil)
      if @items.empty?
        raise ThreadError, "queue empty" if non_block
        # With timeout, return nil (no threading in monoruby)
        return nil
      end
      @items.shift
    end
    alias shift pop
    alias deq pop

    def empty?
      @items.empty?
    end

    def size
      @items.size
    end
    alias length size

    def clear
      @items.clear
      self
    end

    def close
      @closed = true
      self
    end

    def closed?
      @closed || false
    end

    def num_waiting
      0
    end
  end

  class SizedQueue < Queue
    def initialize(max = nil)
      super()
      @max = max
    end

    def max
      @max
    end

    def max=(new_max)
      @max = new_max
    end
  end

  class ConditionVariable
    def initialize
      @waiters = []
    end

    def wait(mutex, timeout = nil)
      self
    end

    def signal
      self
    end

    def broadcast
      self
    end
  end

  class Backtrace
    class Location
      def initialize(frame)
        @frame = frame
      end

      def path
        @frame
      end
    end
  end
end

# Top-level aliases (CRuby compatibility)
class ThreadError < StandardError; end unless defined?(::ThreadError)
Queue = Thread::Queue
SizedQueue = Thread::SizedQueue
ConditionVariable = Thread::ConditionVariable

class Exception
  def backtrace_locations
    backtrace.map do |frame|
      Thread::Backtrace::Location.new(frame)
    end
  end

  def full_message(highlight: nil, order: :top)
    msg = "#{self.class}: #{message}"
    bt = backtrace
    if bt && !bt.empty?
      if order == :top
        "#{bt[0]}: #{msg}\n" + bt[1..].map{|l| "\tfrom #{l}\n"}.join
      else
        bt.reverse.map{|l| "\tfrom #{l}\n"}.join + "#{bt[0]}: #{msg}\n"
      end
    else
      msg + "\n"
    end
  end
end

Mutex = Thread::Mutex

class TrueClass
  class << self
    undef_method :new
  end
  TRUE_TO_S = "true".freeze
  def to_s
    TRUE_TO_S
  end
end

class FalseClass
  class << self
    undef_method :new
  end
  FALSE_TO_S = "false".freeze
  def to_s
    FALSE_TO_S
  end
end

class NilClass
  class << self
    undef_method :new
  end
  NIL_TO_S = "".freeze
  def to_s
    NIL_TO_S
  end

  def to_a
    []
  end

  def to_i
    0
  end

  def to_f
    0.0
  end

  def to_h
    {}
  end

  def =~(_other)
    nil
  end

  def to_c
    Complex(0, 0)
  end

  def to_r
    Rational(0)
  end

  def rationalize(*args)
    if args.length > 1
      raise ArgumentError, "wrong number of arguments (given #{args.length}, expected 0..1)"
    end
    Rational(0)
  end
end

class Marshal
  MAJOR_VERSION = 4
  MINOR_VERSION = 8
end

module Kernel
  module_function

  def puts(*args)
    $stdout.puts(*args)
    nil
  end

  def print(*args)
    $stdout.print(*args)
    nil
  end

  def p(*args)
    if args.size == 1
      $stdout.puts(args[0].inspect)
      args[0]
    elsif args.empty?
      nil
    else
      args.each { |a| $stdout.puts(a.inspect) }
      args
    end
  end

  def printf(fmt = nil, *args)
    if fmt.respond_to?(:write)
      fmt.write(format(*args))
    elsif fmt
      $stdout.write(format(fmt, *args))
    end
    nil
  end

  def at_exit(&block)
  end

  # Prevent CRuby's bundled_gems.rb from patching require with warning
  # logic.  monoruby provides its own implementations of formerly-bundled
  # gems (fiddle, strscan, etc.) so the warnings are not applicable.
  # bundled_gems.rb's replace_require checks for this method and returns
  # early if it already exists.
  alias no_warning_require require
  module_function :no_warning_require
end

module Kernel
  module_function

  def open(name, *args, &block)
    if name.respond_to?(:to_open)
      name.to_open(*args, &block)
    else
      name = name.to_path if name.respond_to?(:to_path)
      name = name.to_str if name.respond_to?(:to_str)
      raise TypeError, "no implicit conversion of #{name.class} into String" unless name.is_a?(String)
      File.open(name, *args, &block)
    end
  end
end

class GC
  def self.auto_compact=(x)
  end

  def self.count
    0
  end

  def self.start(**opts)
  end

  module Profiler
  end
end

class IO
  SEEK_SET = 0
  SEEK_CUR = 1
  SEEK_END = 2

  def sync
    false
  end

  def internal_encoding
    nil
  end

  def self.for_fd(fd, mode = nil, **opts)
    new(fd, mode, **opts)
  end
end

class FloatDomainError < RangeError; end

class Encoding
  class CompatibilityError < EncodingError; end
  class InvalidByteSequenceError < EncodingError; end
  class UndefinedConversionError < EncodingError; end
  class Converter; end

  def self.default_internal
    $DEFAULT_INTERNAL
  end
end

TOPLEVEL_BINDING = binding

require_relative 'comparable'

class Numeric
  include Comparable

  def +@
    self
  end
end

require_relative 'enumerable'
require_relative 'integer'
require_relative 'range'
require_relative 'array'
require_relative 'rational'
require_relative 'float'
require_relative 'numeric'
require_relative 'string'
require_relative 'symbol'
require_relative 'error'
require_relative 'builtins'
require_relative 'pathname_builtins'