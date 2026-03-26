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

  def cover?(val)
    if val.is_a?(Range)
      # Range covers Range
      return false if val.begin && self.begin && val.begin < self.begin
      if self.exclude_end?
        if val.end && self.end
          return false if val.exclude_end? ? val.end > self.end : val.end >= self.end
        end
      else
        return false if val.end && self.end && val.end > self.end
      end
      # Also check that val.end is not before self.begin
      return false if val.end && self.begin && val.end < self.begin
      # Check that val.begin is not after self.end
      if self.end
        if self.exclude_end?
          return false if val.begin && val.begin >= self.end
        else
          return false if val.begin && val.begin > self.end
        end
      end
      true
    else
      return false if self.begin && val < self.begin
      if self.end
        if self.exclude_end?
          val < self.end
        else
          val <= self.end
        end
      else
        true
      end
    end
  end

  def lazy
    Enumerator::Lazy.new(self)
  end
end