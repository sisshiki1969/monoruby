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

  def negative?
    self < 0
  end

  def abs
    self < 0 ? -self : self
  end
  alias magnitude abs

  def integer?
    true
  end

  def ord
    self
  end

  def ceil(ndigits = 0)
    return self if ndigits >= 0
    d = 10 ** (-ndigits)
    if self >= 0
      ((self + d - 1) / d) * d
    else
      -((-self) / d) * d
    end
  end

  def round(ndigits = 0)
    return self if ndigits >= 0
    d = 10 ** (-ndigits)
    if self >= 0
      (self + d / 2) / d * d
    else
      -((-self + d / 2) / d * d)
    end
  end

  def truncate(ndigits = 0)
    return self if ndigits >= 0
    d = 10 ** (-ndigits)
    if self >= 0
      (self / d) * d
    else
      -((-self) / d) * d
    end
  end

  def next
    self + 1
  end
  alias succ next

  def pred
    self - 1
  end

  def remainder(other)
    r = self % other
    if r != 0 && (self < 0) != (other < 0)
      r - other
    else
      r
    end
  end

  def fdiv(other)
    self.to_f / other.to_f
  end

  def gcd(other)
    a = self.abs
    b = other.abs
    while b != 0
      a, b = b, a % b
    end
    a
  end

  def lcm(other)
    return 0 if self == 0 || other == 0
    (self / self.gcd(other) * other).abs
  end

  def gcdlcm(other)
    [gcd(other), lcm(other)]
  end

  def pow(exp, mod = nil)
    if mod
      base = self % mod
      result = 1
      e = exp
      while e > 0
        result = result * base % mod if e.odd?
        base = base * base % mod
        e >>= 1
      end
      result
    else
      self ** exp
    end
  end

  def allbits?(mask)
    (self & mask) == mask
  end

  def anybits?(mask)
    (self & mask) != 0
  end

  def nobits?(mask)
    (self & mask) == 0
  end

  # bit_length is defined in Rust for performance with BigInt

  def coerce(other)
    if other.is_a?(Integer)
      [other, self]
    elsif other.is_a?(Float)
      [other, self.to_f]
    elsif other.respond_to?(:to_f)
      [other.to_f, self.to_f]
    else
      raise TypeError, "#{other.class} can't be coerced into Integer"
    end
  end

  def numerator
    self
  end

  def denominator
    1
  end

  #def to_r
  #  Rational(self, 1)
  #end

  #def rationalize(eps = nil)
  #  Rational(self, 1)
  #end

  def self.sqrt(n)
    n = n.to_int
    raise Math::DomainError, "out of domain - isqrt" if n < 0
    Math.sqrt(n).floor
  end

  def self.try_convert(obj)
    if obj.is_a?(Integer)
      obj
    elsif obj.respond_to?(:to_int)
      result = obj.to_int
      if result.nil?
        nil
      elsif result.is_a?(Integer)
        result
      else
        raise TypeError, "can't convert #{obj.class} to Integer (#{obj.class}#to_int gives #{result.class})"
      end
    else
      nil
    end
  end

  def digits(base = 10)
    base = base.to_int
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
