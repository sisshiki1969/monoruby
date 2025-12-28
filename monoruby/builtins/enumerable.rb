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

  def filter
    return self.to_enum(:filter) if !block_given?
    res = []
    self.each do |x|
      if yield(x)
        res << x
      end
    end
    res
  end
  alias select filter

  def filter_map
    return self.to_enum(:filter_map) if !block_given?
    res = []
    self.each do |x|
      y = yield(x)
      res << y if y
    end
    res
  end

  def take_while
    return self.to_enum(:take_while) if !block_given?
    res = []
    self.each do |x|
      break unless yield(x)
      res << x
    end
    res
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

  def one?
    n = 0
    if block_given?
      self.each do |x|
        if yield(x)
          n += 1
          return false if n > 1
        end
      end
    else
      self.each do |x|
        if x
          n += 1
          return false if n > 1
        end
      end
    end
    n == 1
  end

  def min_by
    return self.to_enum(:min_by) if !block_given?
    elem = nil
    res = nil
    self.each do |x|
      r = yield(x)
      if res.nil?
        elem = x
        res = r
      else
        if r < res
          elem = x
          res = r
        end
      end
    end
    elem
  end

  def to_a(*args)
    res = []
    self.each(*args) do |x|
      res << x
    end
    res
  end
end