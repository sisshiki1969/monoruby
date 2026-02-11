class Integer
  #def succ
  #  self + 1
  #end

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
    raise ArgumentError, "step can't be 0" if step == 0
    return self.to_enum(:step, limit, step) unless block_given?
    i = self
    if step > 0
      while i <= limit
        yield i
        i += step
      end
    else # step < 0
      while i >= limit
        yield i
        i += step
      end
    end
    self
  end

  def digits(base = 10)
    if self < 0
      raise Math::DomainError, "out of domain"
    end
    if base < 2
      raise ArgumentError, "invalid radix #{base}"
    end
    if self == 0
      return [0]
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
