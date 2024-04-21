
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
