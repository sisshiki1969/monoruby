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

  # CRuby keeps these as private instance methods of Module so subclasses
  # override them with `def method_added(name); …; end` (i.e. via the
  # default-private visibility inside a `class M`). `public` here would
  # surface them as `Module.public_instance_methods` entries, which the
  # spec explicitly disallows ("is a private instance method").
  def method_added(name)
  end

  def method_removed(name)
  end

  def method_undefined(name)
  end

  def const_added(name)
  end

  public
  def const_missing(name)
    # Drop the implicit `Object::` prefix so a top-level miss reads
    # `uninitialized constant Foo` (not `Object::Foo`), matching CRuby.
    qual = self.equal?(Object) ? name.to_s : "#{self}::#{name}"
    raise NameError.new("uninitialized constant #{qual}", name)
  end

  def include?(mod)
    # CRuby `Module#include?` accepts only true Modules, not Classes (even
    # though `Class < Module`). And the receiver is *not* counted as one of
    # its own included modules — `M.include?(M)` is `false`.
    if !mod.is_a?(Module) || mod.is_a?(Class)
      raise TypeError, "wrong argument type #{mod.class} (expected Module)"
    end
    return false if equal?(mod)
    ancestors.include?(mod)
  end

  # `Module.used_refinements` returns the refinements active in the
  # current scope. monoruby has no refinement support, so this returns
  # an empty Array as a permissive mock — gems and code that
  # defensively read this list (RSpec, Sorbet) won't crash. Defined in
  # Ruby (not Rust) so the user can override it in specs that actually
  # exercise refinements. CRuby only exposes the class form (no
  # `Module#used_refinements` instance method), so we follow suit.
  def self.used_refinements
    []
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

  # Wraps the raw POSIX wait(2) status word (as returned by `waitpid`).
  # Callers pass the raw int and pid; all predicates decode the bit layout:
  #   bits 0..6  = termination signal (0 = normal exit)
  #   bit  7     = core dumped flag
  #   bits 8..15 = exit code (for normal exit)
  #   low byte 0x7F = stopped (SIGSTOP etc.)
  class Status
    attr_reader :pid

    def initialize(raw_status, pid)
      @status = raw_status
      @pid = pid
    end

    def exited?
      (@status & 0x7F) == 0
    end

    def exitstatus
      exited? ? (@status >> 8) & 0xFF : nil
    end

    def signaled?
      low = @status & 0x7F
      low != 0 && low != 0x7F
    end

    def termsig
      signaled? ? @status & 0x7F : nil
    end

    def stopped?
      (@status & 0xFF) == 0x7F
    end

    def stopsig
      stopped? ? (@status >> 8) & 0xFF : nil
    end

    def coredump?
      (@status & 0x80) != 0
    end

    def success?
      exited? ? exitstatus == 0 : nil
    end

    def to_i
      @status
    end

    def to_s
      if signaled?
        "pid #{@pid} SIG#{termsig}#{coredump? ? ' (core dumped)' : ''}"
      elsif stopped?
        "pid #{@pid} stopped SIG#{stopsig}"
      else
        "pid #{@pid} exit #{exitstatus}"
      end
    end

    def inspect
      "#<Process::Status: #{to_s}>"
    end

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
    @block = block
    @args = args
    @value = nil
    @exception = nil
    @ran = block.nil?
    @alive = !@ran
    @keys = {}
    @thread_local = {}
  end

  @@current = Thread.new
  @@current.instance_variable_set(:@alive, true)

  def __run
    return if @ran
    @ran = true
    begin
      @value = @block.call(*@args)
    rescue => e
      @exception = e
    ensure
      @alive = false
    end
    self
  end
  private :__run

  def value
    __run
    raise @exception if @exception
    @value
  end

  def join(limit = nil)
    __run
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
    bt = backtrace
    return nil if bt.nil?
    bt.map do |frame|
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

  def String(arg)
    return arg if arg.is_a?(::String)
    if arg.respond_to?(:to_str)
      result = arg.to_str
      return result if result.is_a?(::String)
      raise TypeError, "can't convert #{arg.class} to String (#{arg.class}#to_str gives #{result.class})"
    end
    result = arg.to_s
    unless result.is_a?(::String)
      raise TypeError, "can't convert #{arg.class} to String (#{arg.class}#to_s gives #{result.class})"
    end
    result
  end

  def Hash(arg)
    return {} if arg.nil?
    return {} if arg.is_a?(::Array) && arg.empty?
    return arg if arg.is_a?(::Hash)
    if arg.respond_to?(:to_hash)
      result = arg.to_hash
      return {} if result.nil?
      return result if result.is_a?(::Hash)
      raise TypeError, "can't convert #{arg.class} to Hash (#{arg.class}#to_hash gives #{result.class})"
    end
    raise TypeError, "can't convert #{arg.class} into Hash"
  end

  def srand(*args)
    Random.srand(*args)
  end

  def putc(ch)
    s = ch.is_a?(Integer) ? (ch & 0xff).chr : ch.to_s[0]
    $stdout.write(s)
    ch
  end

  # Kernel#test(cmd, file1[, file2]) — minimal subset of CRuby's file-test
  # operator. `cmd` accepts either a single-character string or its integer
  # code-point.
  def test(cmd, file1, file2 = nil)
    c = cmd.is_a?(Integer) ? cmd : cmd.to_s.ord
    case c
    when ?e.ord then File.exist?(file1)
    when ?f.ord then File.file?(file1)
    when ?d.ord then File.directory?(file1)
    when ?r.ord then File.readable?(file1)
    when ?R.ord then File.readable_real?(file1)
    when ?w.ord then File.writable?(file1)
    when ?W.ord then File.writable_real?(file1)
    when ?x.ord then File.executable?(file1)
    when ?X.ord then File.executable_real?(file1)
    when ?l.ord then File.symlink?(file1)
    when ?p.ord then File.pipe?(file1)
    when ?S.ord then File.socket?(file1)
    when ?b.ord then File.blockdev?(file1)
    when ?c.ord then File.chardev?(file1)
    when ?u.ord then File.setuid?(file1)
    when ?g.ord then File.setgid?(file1)
    when ?k.ord then File.sticky?(file1)
    when ?o.ord then File.owned?(file1)
    when ?G.ord then File.grpowned?(file1)
    when ?s.ord
      sz = File.size?(file1)
      sz && sz > 0 ? sz : nil
    when ?z.ord then File.zero?(file1)
    when ?M.ord then File.mtime(file1)
    when ?A.ord then File.atime(file1)
    when ?C.ord then File.ctime(file1)
    when ?-.ord then File.identical?(file1, file2)
    when ?=.ord then File.mtime(file1) == File.mtime(file2)
    when ?<.ord then File.mtime(file1) < File.mtime(file2)
    when ?>.ord then File.mtime(file1) > File.mtime(file2)
    else
      raise ArgumentError, "unknown command '#{cmd.is_a?(Integer) ? cmd.chr : cmd}'"
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

# Minimal Data class (Ruby 3.2+). Modeled on Struct but conceptually
# immutable. Full semantics (keyword-only initializer, with-method, frozen
# instances) are not yet implemented; this is enough for fixtures that
# reference `Data.define`.
class Data
  def self.define(*members, &block)
    klass = ::Struct.new(*members)
    klass.class_eval(&block) if block
    klass
  end
end unless defined?(::Data)

require_relative 'enumerable'
require_relative 'numeric'
require_relative 'integer'
require_relative 'range'
require_relative 'array'
require_relative 'rational'
require_relative 'complex'
require_relative 'float'
require_relative 'string'
require_relative 'symbol'
require_relative 'error'
require_relative 'set'
require_relative 'struct'
require_relative 'builtins'
require_relative 'pathname_builtins'

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
  end

  def argv
    @argv
  end

  def filename
    @current_name || (@argv.first || '-')
  end
  alias_method :path, :filename

  def file
    advance
    @current_file || $stdin
  end

  def advance
    return true if @current_file && !@current_file.closed?
    if @argv.empty?
      @current_file ||= $stdin
      @current_name ||= '-'
      false
    else
      @current_name = @argv.shift
      @current_file = @current_name == '-' ? $stdin : File.open(@current_name)
      true
    end
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
    while advance && @current_file
      @current_file.each_line do |line|
        @lineno += 1
        yield line
      end
      @current_file.close if @current_file && !@current_file.closed? && @current_file != $stdin
      @current_file = nil
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
  def read(*)
    buf = String.new
    while advance && @current_file
      buf << @current_file.read.to_s
      @current_file.close if @current_file != $stdin
      @current_file = nil
    end
    buf
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