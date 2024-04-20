require 'rbconfig' # this destroys tests.
module RbConfig
  SIZEOF = eval(`ruby -e 'require "rbconfig/sizeof"; puts RbConfig::SIZEOF'`)
  #CONFIG = eval(`ruby -e 'require "rbconfig"; puts RbConfig::CONFIG'`)
  def self.ruby
    @ruby ||= `ruby -e 'print RbConfig.ruby'`
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

class Integer
  def zero?
    self == 0
  end
end

class Hash
  # Hash#to_h
  # to_h -> self
  # to_h {|key, value| block } -> Hash
  def to_h
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

class GC
  def self.auto_compact=(x)
  end
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
  def b
    self
  end
end

class Object
  def freeze
    self
  end

  #def to_enum
  #  Enumerator.new do
  #    self.each do |x|
  #      Fiber.yield x
  #    end
  #  end
  #end
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
  class Mutex
  end
end

class Marshal
  MAJOR_VERSION = 3
  MINOR_VERSION = 0
end

module Enumerable
end

module Comparable
end

module Fiddle
  SIZEOF_LONG = 8
  WINDOWS = false
  module_function
  def dlopen(lib)
    h = Kernel.___dlopen(lib)
    raise DLError.new("dlopen failed") if h == 0
    Handle.new(h)
  end
  module Types
    VOID = 0
    VOIDP = 1
    CHAR = 2
    UCHAR = -2
    INT = 4
    UINT = -4
  end
  class Handle
    RTLD_GLOBAL = 0
    RTLD_LAZY = 0
    RTLD_NOW = 0
    def initialize(handle)
      @handle = handle
    end
    def [](name)
      ptr = Kernel.___dlsym(@handle, name)
      raise DLError.new("dlsym failed") if ptr == 0
      ptr
    end
  end
  class Error < StandardError
  end
  class DLError < Error
  end
  class Function
    def initialize(ptr, args_type, ret_type)
      @ptr = ptr
      @args_type = args_type
      @ret_type = ret_type
    end
    def call(*arg)
      Kernel.___call(@ptr, arg, @args_type, @ret_type)
    end
  end
end

