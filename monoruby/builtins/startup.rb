RUBY_PLATFORM = "x86_64-linux"
RUBY_PATCHLEVEL = 0

require 'rbconfig' 

class BasicObject
  def method_missing(name, *args)
    raise NoMethodError, "undefined method '#{name}' for an instance of #{self.class}"
  end
end

class Object
  def freeze
    self
  end

  def initialize(...)
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
  FNM_DOTMATCH = 4
  FNM_CASEFOLD = 8
  NULL = "/dev/null"

  Separator = "/"
  SEPARATOR = "/"
  ALT_SEPARATOR = nil
  PATH_SEPARATOR = ":"
end

class Signal
  def self.trap(signal, command = nil, &block)
    # No-op for now.
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
  def thread_variable_set(key, value)
    @thread_local ||= {}
    @thread_local[key] = value
  end
  def thread_variable_get(key)
    @thread_local ||= {}
    @thread_local[key]
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
  EPERM = 1
  ENOENT = 2
  EAGAIN = 11
  EPROTO = 32
  ENOSPC = 28
  EACCES = 13
  EEXIST = 17
  EROFS = 30
  ENOLCK = 37
  ENOSYS = 38
  ENOTSUP = 95
  ENOTDIR = 20
end

module Kernel
  module_function
  def at_exit(&block)
  end
end

class GC
  def self.auto_compact=(x)
  end

  def self.start
  end
end

require_relative 'comparable'
require_relative 'enumerable'
require_relative 'integer'
require_relative 'range'
require_relative 'array'
require_relative 'builtins'
require_relative 'pathname_builtins'