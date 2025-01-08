RUBY_PLATFORM = "x86_64-linux"

require 'rbconfig' 
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
    if block_given?
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
    else
      self.to_enum(:each_slice, n)
    end
  end
  
  def each_with_index
    if block_given?
      i = 0
      self.each do |x|
        yield x, i
        i += 1
      end
      self
    else
      self.to_enum(:each_with_index)
    end
  end

  def map
    return self if !block_given?
    res = []
    self.each do |x|
      res << yield(x)
    end
    res
  end
  alias collect map
end

class Enumerator
  include Enumerable
end

class Array
  include Enumerable
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

  #def times
  #  if block_given?
  #    i = 0
  #    while i < self
  #      yield i
  #      i += 1
  #    end
  #    self
  #  else
  #    self.to_enum(:times)
  #  end
  #end
end

class Symbol
  def match(other)
    self.to_s.match(other)
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

class Object
  def freeze
    self
  end
end

class Module
  def private_constant(*x)
  end
end

module Warning
  def self.warn(*x)
  end
  
  def self.[](category)
    true
  end
end

class Symbol
  def to_proc
    Proc.new do |slf, *args|
      slf.send(self, *args)
    end
  end
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
end

class Marshal
  MAJOR_VERSION = 3
  MINOR_VERSION = 0
end

class Range
  include Enumerable
  
  alias first begin
  
  def reject
    if block_given?
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
    else
      self.to_enum(:reject)
    end
  end
end

class Errno
  ENOLCK = 37
  ENOSYS = 38
  ENOTSUP = 95
  ENOENT = 2
end

class GC
  def self.auto_compact=(x)
  end

  def self.start
  end
end

File::ALT_SEPARATOR = nil
