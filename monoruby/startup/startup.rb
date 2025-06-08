RUBY_PLATFORM = "x86_64-linux"
RUBY_PATCHLEVEL = 0

require 'rbconfig' 

class Object
  def freeze
    self
  end

  def initialize(*arg)
  end

  def tap
    yield self
    self
  end
end

#class Class
#  def new(...)
#    o = allocate
#    o.initialize(...)
#    o
#  end
#end

module Comparable
  def ==(other)
    case res = self <=> other
    when Integer
      res == 0
    else
      raise ArgumentError, "comparison of #{self.class} with #{other.class} failed"
    end
  end

  def !=(other)
    case res = self <=> other
    when Integer
      res != 0
    else
      raise ArgumentError, "comparison of #{self.class} with #{other.class} failed"
    end
  end

  def >=(other)
    case res = self <=> other
    when Integer
      res >= 0
    else
      raise ArgumentError, "comparison of #{self.class} with #{other.class} failed"
    end
  end

  def >(other)
    case res = self <=> other
    when Integer
      res > 0
    else
      raise ArgumentError, "comparison of #{self.class} with #{other.class} failed"
    end
  end

  def <=(other)
    case res = self <=> other
    when Integer
      res <= 0
    else
      raise ArgumentError, "comparison of #{self.class} with #{other.class} failed"
    end
  end
  
  def <(other)
    case res = self <=> other
    when Integer
      res < 0
    else
      raise ArgumentError, "comparison of #{self.class} with #{other.class} failed"
    end
  end
end


module Enumerable
  def inject(init = nil)
    acc = init
    if init.nil?
      flag = true
      self.each do |x|
        if flag
          acc = x
          flag = false
        else
          acc = yield(acc, x)
        end
      end
    else
      self.each do |x|
        acc = yield(acc, x)
      end
    end
    acc
  end
  alias reduce inject

  def each_slice(n)
    if n == 0 || n < 0
      raise ArgumentError, "invalid slice size"
    end
    return self.to_enum(:each_slice, n) if !block_given?
    slice = []
    self.each do |x|
      slice << x
      if slice.size == n
        yield slice
        slice = []
      end
    end
    yield slice if !slice.empty?
    self
  end
  
  def each_with_index
    return self.to_enum(:each_with_index) if !block_given?
    i = 0
    self.each do |x|
      yield x, i
      i += 1
    end
    self
  end

  def each_with_object(obj)
    return self.to_enum(:each_with_object) if !block_given?
    self.each do |x|
      yield x, obj
    end
    obj
  end

  def map
    return self.to_enum(:map) if !block_given?
    res = []
    self.each do |x|
      res << yield(x)
    end
    res
  end
  alias collect map

  def find(ifnone = nil)
    return self.to_enum(:find) if !block_given?
    self.each do |x|
      if yield(x)
        return x
      end
    end
    if ifnone.nil?
      nil
    else
      ifnone.call
    end
  end

  def any?
    if block_given?
      self.each do |x|
        return true if yield(x)
      end
    else
      self.each do |x|
        return true if x
      end
    end
    false
  end

  def to_a(*args)
    res = []
    self.each(*args) do |x|
      res << x
    end
    res
  end
end

class Enumerator
  include Enumerable
end

class TrueClass
  def ^(other)
    !other
  end

  def |(other)
    true
  end

  def &(other)
    !!other
  end
end

class FalseClass
  def ^(other)
    !!other
  end

  def |(other)
    !!other
  end

  def &(other)
    false
  end
end

class NilClass
  def ^(other)
    !!other
  end

  def |(other)
    !!other
  end

  def &(other)
    false
  end
end

class Array
  include Enumerable

  def self.new(...)
    o = allocate
    o.initialize(...) if o.respond_to?(:initialize)
    o
  end

  def sample
    return nil if self.empty?
    self[rand(self.size)]
  end

  def each
    return self.to_enum(:each) if !block_given?
    i = 0
    while i < self.size
      yield self[i]
      i += 1
    end
    self
  end

  def reverse_each
    return self.to_enum(:reverse_each) if !block_given?
    len = self.size
    if len == 0
      return self
    end
    i = len - 1
    while i >= 0
      yield self[i]
      i -= 1
    end
    self
  end

  def each_with_index
    return self.to_enum(:each_with_index) if !block_given?
    i = 0
    while i < self.size
      yield self[i], i
      i += 1
    end
    self
  end

  def map!
    return self.to_enum(:map!) if !block_given?
    i = 0
    while i < self.size
      self[i] = yield(self[i])
      i += 1
    end
    self
  end
  alias collect! map!

  def map
    return self.to_enum(:map) if !block_given?
    res = self.dup
    i = 0
    while i < self.size
      res[i] = yield(self[i])
      i += 1
    end
    res
  end
  alias collect map

  #def product(*lists)
  #  if lists.empty?
  #    return self.map {|x| [x] }
  #  end
  #  l = lists.shift
  #  res = []
  #  for e1 in self
  #    for e2 in l.product(*lists)
  #      res << [e1, *e2]
  #    end
  #  end
  #  res
  #end
end

class Hash
  include Enumerable

  # Hash#to_h
  # to_h -> self
  # to_h {|key, value| block } -> Hash
  def to_h
    return self if !block_given?
    h = {}
    self.each {|k, v|
      new_kv = yield k, v
      new_k = new_kv[0]
      new_v = new_kv[1]
      h[new_k] = new_v
    }
    h
  end
end

class Integer
  def succ
    self + 1
  end

  def positive?
    self > 0
  end

  def floor
    self
  end

  def times
    return self.to_enum(:times) if !block_given?
    i = 0
    while i < self
      yield i
      i += 1
    end
    self
  end

  def step(limit, step = 1)
    return self.to_enum(:step, limit, step) if !block_given?
    i = self
    while i <= limit
      yield i
      i += step
    end
    self
  end

  def digits(base = 10)
    if base < 0
      raise Math::DomainError, "out of domain"
    elsif base == 0
      raise ArgumentError, "invalid radix #{base}"
    end
    res = []
    n = self
    while n > 0
      res << n % base
      n /= base
    end
    res
  end
end

class Float
  def zero?
    self == 0.0
  end
end

class Symbol
  def match(other)
    self.to_s.match(other)
  end

  def to_proc
    Proc.new do |*args|
      if args.size == 0
        raise ArgumentError, "no receiver given"
      end
      slf, *args = args
      slf.send(self, *args)
    end
  end

  def to_sym
    self
  end
end

class String
  def +@
    self
  end
  def -@
    self
  end
end

class Range
  include Enumerable
  
  alias first begin
  
  def reject
    return self.to_enum(:reject) if !block_given?
    elem = self.begin
    end_ = self.end
    res = []
    while elem != end_
      if !yield(elem)
        res << elem
      end
      elem = elem.succ
    end
    res
  end
end

class Regexp
  IGNORECASE = 1
  EXTENDED = 2
  MULTILINE = 4
end

class Module
  def private_constant(*x)
  end

  def method_added(name)
  end
end

module Warning
  def self.warn(*x)
  end
  
  def self.[](category)
    true
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
end

module File::Constants
  FNM_SYSCASE = 0
  FNM_CASEFOLD = 8
end

class File
  include File::Constants
  FNM_SYSCASE = 0
  FNM_CASEFOLD = 8
  NULL = "/dev/null"

  Separator = "/"
  SEPARATOR = "/"
  ALT_SEPARATOR = nil
  PATH_SEPARATOR = ":"
end

class Thread
  @@current = Thread.new
  def self.current
    @@current
  end
  def [](key)
    @keys ||= {}
    @keys[key]
  end
  def []=(key, value)
    @keys ||= {}
    @keys[key] = value
  end

  class Mutex
    def synchronize
      yield
    end
    
    def owned?
      false
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

class Exception
  def backtrace_locations
    backtrace.map do |frame|
      Thread::Backtrace::Location.new(frame)
    end
  end
end

Mutex = Thread::Mutex

class Marshal
  MAJOR_VERSION = 3
  MINOR_VERSION = 0
end

class Errno
  ENOENT = 2
  EAGAIN = 11
  EPROTO = 32
  ENOSPC = 28
  EACCES = 13
  EEXIST = 17
  ENOLCK = 37
  ENOSYS = 38
  ENOTSUP = 95
end

class GC
  def self.auto_compact=(x)
  end

  def self.start
  end
end

