class Integer
  #def succ
  #  self + 1
  #end

  def dup
    self
  end

  def positive?
    self > 0
  end

  def floor(ndigits = 0)
    ndigits = __coerce_ndigits(ndigits)
    if ndigits >= 0
      self
    else
      d = 10 ** (-ndigits)
      if self >= 0
        (self / d) * d
      else
        ((self - d + 1) / d) * d
      end
    end
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

  def negative?
    self < 0
  end

  # abs and magnitude are defined in Rust for correctness with BigInt edge cases
  # def abs
  #   self < 0 ? -self : self
  # end
  # alias magnitude abs

  def integer?
    true
  end

  def ord
    self
  end

  def ceil(ndigits = 0)
    ndigits = __coerce_ndigits(ndigits)
    return self if ndigits >= 0
    d = 10 ** (-ndigits)
    if self >= 0
      ((self + d - 1) / d) * d
    else
      -((-self) / d) * d
    end
  end

  def round(ndigits = 0, **kw)
    half = kw[:half]
    if half != nil && half != :up && half != :down && half != :even
      raise ArgumentError, "invalid rounding mode: #{half}"
    end
    ndigits = __coerce_ndigits(ndigits)
    return self if ndigits >= 0
    d = 10 ** (-ndigits)
    abs_val = self < 0 ? -self : self
    remainder = abs_val % d
    truncated = abs_val - remainder
    halfway = d / 2
    result = if remainder > halfway
      truncated + d
    elsif remainder < halfway
      truncated
    else
      # Exactly halfway
      case half
      when :up
        truncated + d
      when :down
        truncated
      when :even
        # Round to nearest even multiple of d
        if (truncated / d).even?
          truncated
        else
          truncated + d
        end
      else
        # Default: round half up (away from zero)
        truncated + d
      end
    end
    self < 0 ? -result : result
  end

  def truncate(ndigits = 0)
    ndigits = __coerce_ndigits(ndigits)
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
    sf = self.to_f
    of = other.to_f
    # If both convert to Infinity (very large BigInts), compute via bit shifting
    if sf.infinite? && of.infinite?
      # Use BigInt division with extra precision bits
      shift = 1024
      (self << shift).to_f / (other << shift).to_f
    elsif sf.infinite? && of == 0.0
      sf / of
    else
      sf / of
    end
  end

  def gcd(other)
    unless other.is_a?(Integer)
      raise TypeError, "#{other.class} is not an integer"
    end
    a = self.abs
    b = other.abs
    while b != 0
      a, b = b, a % b
    end
    a
  end

  def lcm(other)
    unless other.is_a?(Integer)
      raise TypeError, "#{other.class} is not an integer"
    end
    return 0 if self == 0 || other == 0
    (self / self.gcd(other) * other).abs
  end

  def gcdlcm(other)
    unless other.is_a?(Integer)
      raise TypeError, "#{other.class} is not an integer"
    end
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
    unless mask.respond_to?(:to_int)
      raise TypeError, "no implicit conversion of #{mask.class} into Integer"
    end
    mask = mask.to_int
    (self & mask) == mask
  end

  def anybits?(mask)
    unless mask.respond_to?(:to_int)
      raise TypeError, "no implicit conversion of #{mask.class} into Integer"
    end
    mask = mask.to_int
    (self & mask) != 0
  end

  def nobits?(mask)
    unless mask.respond_to?(:to_int)
      raise TypeError, "no implicit conversion of #{mask.class} into Integer"
    end
    mask = mask.to_int
    (self & mask) == 0
  end

  # bit_length is defined in Rust for performance with BigInt

  def coerce(other)
    if other.is_a?(Integer)
      [other, self]
    elsif other.is_a?(Float)
      [other, self.to_f]
    elsif defined?(Rational) && other.is_a?(Rational)
      [other, Rational(self, 1)]
    elsif other.is_a?(String)
      # CRuby uses Float(other) which raises ArgumentError for non-numeric strings
      # We need to validate the string is a valid float representation
      stripped = other.strip
      if stripped.empty? || stripped !~ /\A[+-]?(\d+\.?\d*|\d*\.?\d+)([eE][+-]?\d+)?\z/
        raise ArgumentError, "invalid value for Float(): #{other.inspect}"
      end
      [Float(other), self.to_f]
    elsif other.respond_to?(:to_f)
      result = other.to_f
      unless result.is_a?(Float)
        raise TypeError, "can't convert #{other.class} to Float (#{other.class}#to_f gives #{result.class})"
      end
      [result, self.to_f]
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

  def to_r
    if defined?(Rational)
      Rational(self, 1)
    else
      self
    end
  end

  def rationalize(eps = nil)
    if defined?(Rational)
      Rational(self, 1)
    else
      self
    end
  end

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
        raise TypeError, "can't convert #{obj.class} into Integer"
      end
    else
      nil
    end
  end

  private

  def __coerce_ndigits(ndigits)
    if ndigits.is_a?(Integer)
      # Check for BigInt values that are too large for round/floor/ceil/truncate
      if ndigits > 0x3FFFFFFF || ndigits < -0x40000000
        raise RangeError, "integer #{ndigits} too big to convert to `int'"
      end
      return ndigits
    elsif ndigits.is_a?(Float)
      if ndigits.infinite?
        raise RangeError, "float #{ndigits > 0 ? 'Inf' : '-Inf'} out of range of integer"
      end
      if ndigits.nan?
        raise RangeError, "float NaN out of range of integer"
      end
      ndigits = ndigits.to_int
    elsif ndigits.respond_to?(:to_int)
      result = ndigits.to_int
      if result.nil?
        raise TypeError, "no implicit conversion of #{ndigits.class} into Integer"
      end
      unless result.is_a?(Integer)
        raise TypeError, "can't convert #{ndigits.class} to Integer (#{ndigits.class}#to_int gives #{result.class})"
      end
      ndigits = result
    else
      raise TypeError, "no implicit conversion of #{ndigits.class} into Integer"
    end
    # Check for large values after conversion
    if ndigits > 0x3FFFFFFF || ndigits < -0x40000000
      raise RangeError, "integer #{ndigits} too big to convert to `int'"
    end
    ndigits
  end

  public

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
