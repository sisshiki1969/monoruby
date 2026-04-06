class Numeric
  def positive?
    self > 0
  end

  def negative?
    self < 0
  end

  def abs
    self < 0 ? -self : self
  end
  alias magnitude abs

  def to_int
    to_i
  end

  def to_c
    Complex(self, 0)
  end

  def to_r
    Rational(self)
  end

  def ceil(ndigits = 0)
    to_f.ceil(ndigits)
  end

  def floor(ndigits = 0)
    to_f.floor(ndigits)
  end

  def round(ndigits = 0)
    to_f.round(ndigits)
  end

  def truncate(ndigits = 0)
    to_f.truncate(ndigits)
  end

  def step(limit = nil, step = nil, by: nil, to: nil)
    limit = to if to
    step = by if by
    step ||= 1
    raise ArgumentError, "step can't be 0" if step == 0
    return to_enum(:step, limit, step) unless block_given?
    i = self
    if step > 0
      while limit.nil? || i <= limit
        yield i
        i += step
      end
    else
      while limit.nil? || i >= limit
        yield i
        i += step
      end
    end
    self
  end
end
