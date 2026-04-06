class Integer
  #def succ
  #  self + 1
  #end

  def to_i
    self
  end
  alias to_int to_i

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
      # Ruby's integer division floors toward negative infinity,
      # so (self / d) * d always rounds toward negative infinity.
      (self / d) * d
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

  def div(other)
    raise ZeroDivisionError, "divided by 0" if other == 0 || (other.is_a?(Float) && other == 0.0)
    if other.respond_to?(:coerce) && !other.is_a?(Integer) && !other.is_a?(Float) && !other.is_a?(Rational)
      a, b = other.coerce(self)
      return a.div(b)
    end
    (self / other).floor
  end

  def ceildiv(other)
    -(-self).div(other)
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
    if other.is_a?(Integer)
      sf = self.to_f
      of = other.to_f
      if self.bit_length > 53 || other.bit_length > 53
        # When either operand exceeds f64 mantissa precision (53 bits),
        # use BigInt division to produce the correct IEEE 754 double.
        # We scale the numerator up so that the integer quotient has
        # ~54 significant bits, then use Math.ldexp to apply the
        # 2^(-extra) scaling without overflow.
        return 0.0 if self == 0
        if other == 0
          return sf.infinite? ? sf / 0.0 : 1.0 / 0.0 * (self < 0 ? -1 : 1)
        end
        sign = (self < 0) != (other < 0) ? -1 : 1
        a = self.abs
        b = other.abs
        lbits = a.bit_length
        rbits = b.bit_length
        # Target ~56 significant bits in quotient (3 extra beyond f64's
        # 53-bit mantissa) so to_f has guard, round, and sticky bits.
        extra = 56 - lbits + rbits
        if extra > 0
          q, r = (a << extra).divmod(b)
        elsif extra < 0
          q, r = a.divmod(b << -extra)
        else
          q, r = a.divmod(b)
        end
        # Set sticky bit: if remainder is nonzero, ensure LSB is 1
        # so that to_f rounds correctly (IEEE 754 round-to-nearest-even)
        q |= 1 if r != 0
        result = Math.ldexp(q.to_f, -extra)
        sign < 0 ? -result : result
      else
        sf / of
      end
    elsif other.is_a?(Float)
      self.to_f / other
    elsif other.respond_to?(:coerce)
      a, b = other.coerce(self)
      a.fdiv(b)
    else
      raise TypeError, "#{other.class} can't be coerced into Integer"
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
    elsif other.is_a?(Complex)
      [other, Complex(self)]
    elsif other.nil? || other.equal?(true) || other.equal?(false) || other.is_a?(Symbol)
      raise TypeError, "#{other.class} can't be coerced into Integer"
    elsif other.is_a?(String)
      stripped = other.strip
      if stripped.empty? || stripped !~ /\A[+-]?(\d+\.?\d*|\d*\.?\d+)([eE][+-]?\d+)?\z/
        raise ArgumentError, "invalid value for Float(): #{other.inspect}"
      end
      [Float(other), self.to_f]
    elsif other.respond_to?(:to_f)
      result = other.to_f
      unless result.is_a?(Float)
        raise TypeError, "can't convert #{other.class} into Float (#{other.class}#to_f gives #{result.class})"
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
    unless n.is_a?(Integer)
      if n.respond_to?(:to_int)
        n = n.to_int
        unless n.is_a?(Integer)
          raise TypeError, "can't convert #{n.class} to Integer (#{n.class}#to_int gives #{n.class})"
        end
      else
        raise TypeError, "no implicit conversion of #{n.class} into Integer"
      end
    end
    raise Math::DomainError, "out of domain - isqrt" if n < 0
    return 0 if n == 0
    # Newton's method for integer square root
    x = 1 << ((n.bit_length + 1) / 2)
    loop do
      y = (x + n / x) / 2
      break x if y >= x
      x = y
    end
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
        raise TypeError, "can't convert #{obj.class} into Integer (#{obj.class}#to_int gives #{result.class})"
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
