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
    elem = self.begin
    e = self.end
    e = e.succ unless self.exclude_end?
    res = []
    while elem != e
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