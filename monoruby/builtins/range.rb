class Range
  include Enumerable
  
  def each
    return self.to_enum(:each) unless block_given?
    i = self.begin
    raise TypeError,"can't iterate from NilClass" if i.nil?
    e = self.end
    if e.nil?
      while true
        yield i
        i = i.succ
      end
    else
      e = e.succ unless self.exclude_end?
      while i < e
        yield i
        i = i.succ
      end
    end
    self
  end
  
  def reject
    return self.to_enum(:reject) unless block_given?
    i = self.begin
    raise TypeError,"can't iterate from NilClass" if i.nil?
    e = self.end
    res = []
    if e.nil?
      while true
        unless yield(i)
          res << i
        end
        i = i.succ
      end
    else
      e = e.succ unless self.exclude_end?
      while i < e
        unless yield(i)
          res << i
        end
        i = i.succ
      end
    end
    res
  end

  def bsearch
    return to_enum(:bsearch) unless block_given?
    self.to_a.bsearch do |x|
      yield(x)
    end
  end

  def count(*args)
    if !block_given? && args.empty?
      if self.end.nil? || self.begin.nil?
        return Float::INFINITY
      end
    end
    if block_given?
      n = 0
      self.each { |x| n += 1 if yield(x) }
      n
    elsif args.empty?
      n = 0
      self.each { |_| n += 1 }
      n
    else
      target = args[0]
      n = 0
      self.each { |x| n += 1 if x == target }
      n
    end
  end

  def min
    if block_given?
      if self.begin.nil?
        raise RangeError, "cannot get the minimum of beginless range"
      end
      if self.end.nil?
        raise RangeError, "cannot get the minimum of endless range"
      end
      m = nil
      self.each do |x|
        if m.nil? || yield(x, m) < 0
          m = x
        end
      end
      return m
    end
    if self.begin.nil?
      raise RangeError, "cannot get the minimum of beginless range"
    end
    b = self.begin
    e = self.end
    # endless range: just return begin
    return b if e.nil?
    # empty range check
    cmp = (b <=> e)
    return nil if cmp.nil? || cmp > 0
    return nil if cmp == 0 && self.exclude_end?
    b
  end

  def max
    if block_given?
      if self.end.nil?
        raise RangeError, "cannot get the maximum of endless range"
      end
      if self.begin.nil?
        raise RangeError, "cannot get the maximum of beginless range with custom comparison method"
      end
      m = nil
      self.each do |x|
        if m.nil? || yield(x, m) > 0
          m = x
        end
      end
      return m
    end
    if self.end.nil?
      raise RangeError, "cannot get the maximum of endless range"
    end
    b = self.begin
    e = self.end
    # beginless range
    if b.nil?
      if self.exclude_end?
        if e.is_a?(Integer)
          return e - 1
        elsif e.is_a?(Numeric)
          raise TypeError, "cannot exclude non Integer end value"
        else
          raise TypeError, "cannot exclude end value with non Integer begin value"
        end
      end
      return e
    end
    # empty range check
    cmp = (b <=> e)
    return nil if cmp.nil? || cmp > 0
    return nil if cmp == 0 && self.exclude_end?
    if self.exclude_end?
      if e.is_a?(Integer)
        return e - 1
      elsif e.is_a?(Numeric)
        raise TypeError, "cannot exclude non Integer end value"
      else
        # Non-numeric exclusive range (e.g. String): iterate to find max
        m = nil
        self.each do |x|
          m = x
        end
        return m
      end
    end
    e
  end

  def minmax
    if block_given?
      if self.end.nil?
        raise RangeError, "cannot get the maximum of endless range"
      end
      if self.begin.nil?
        raise RangeError, "cannot get the minimum of beginless range"
      end
      mn = nil
      mx = nil
      self.each do |x|
        if mn.nil? || yield(x, mn) < 0
          mn = x
        end
        if mx.nil? || yield(x, mx) > 0
          mx = x
        end
      end
      return [mn, mx]
    end
    [self.min, self.max]
  end
end