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
    return self.to_enum(:each) unless block_given?
    i = 0
    while i < self.size
      yield self[i]
      i += 1
    end
    self
  end

  def reverse_each
    return self.to_enum(:reverse_each) unless block_given?
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
    return self.to_enum(:each_with_index) unless block_given?
    i = 0
    while i < self.size
      yield self[i], i
      i += 1
    end
    self
  end

  def map!
    return self.to_enum(:map!) unless block_given?
    i = 0
    while i < self.size
      self[i] = yield(self[i])
      i += 1
    end
    self
  end
  alias collect! map!

  def map
    return self.to_enum(:map) unless block_given?
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

  def bsearch
    return to_enum(:bsearch) unless block_given?
    low = 0
    high = size
    # 判定モードを最初の呼び出しで決定
    mode = nil
    while low < high
      mid = (low + high) / 2
      val = self[mid]
      res = yield(val)

      if mode.nil?
        if res == true || res == false
          mode = :find_min
        elsif res.is_a?(Numeric)
          mode = :find_exact
        else
          raise TypeError, "unexpected block result #{res.inspect}"
        end
      end

      case mode
      when :find_min
        if res
          high = mid
        else
          low = mid + 1
        end
      when :find_exact
        if res < 0
          low = mid + 1
        elsif res > 0
          high = mid
        else
          return val
        end
      end
    end
    mode == :find_min ? self[low] : nil
  end
end

class Hash
  include Enumerable

  # Hash#to_h
  # to_h -> self
  # to_h {|key, value| block } -> Hash
  def to_h
    return self unless block_given?
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
    return self.to_enum(:times) unless block_given?
    i = 0
    while i < self
      yield i
      i += 1
    end
    self
  end

  def step(limit, step = 1)
    return self.to_enum(:step, limit, step) unless block_given?
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
    return self.to_enum(:reject) unless block_given?
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

  def bsearch
    return to_enum(:bsearch) unless block_given?
    self.to_a.bsearch do |x|
      yield(x)
    end
  end
end