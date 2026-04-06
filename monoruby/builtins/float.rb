class Float
  def to_f
    self
  end

  def zero?
    self == 0.0
  end

  def positive?
    self > 0.0
  end

  def negative?
    self < 0.0
  end

  # truncate and ceil are defined in Rust for JIT compatibility

  def integer?
    false
  end

  def coerce(other)
    if other.is_a?(Integer)
      [other.to_f, self]
    elsif other.is_a?(Float)
      [other, self]
    elsif other.is_a?(String)
      [Float(other), self]
    else
      raise TypeError, "#{other.class} can't be coerced into Float"
    end
  end

  def remainder(other)
    r = self % other
    if r != 0 && (self < 0) != (other < 0)
      r - other
    else
      r
    end
  end

  def numerator
    return self if nan? || infinite?
    if defined?(Rational)
      to_r.numerator
    else
      raise TypeError, "can't convert Float into Rational"
    end
  end

  def denominator
    return 1 if nan?
    return 1 if infinite?
    if defined?(Rational)
      to_r.denominator
    else
      raise TypeError, "can't convert Float into Rational"
    end
  end

  def to_r
    raise FloatDomainError, "NaN" if nan?
    raise FloatDomainError, (self > 0 ? "Infinity" : "-Infinity") if infinite?
    if defined?(Rational)
      Rational.__float_to_rational(self)
    else
      self
    end
  end

  def rationalize(eps = nil)
    raise FloatDomainError, "NaN" if nan?
    raise FloatDomainError, (self > 0 ? "Infinity" : "-Infinity") if infinite?
    return Rational(0) if self == 0.0
    if eps
      return Rational.__float_find_simplest(self, eps)
    end
    # No eps: find simplest rational that rounds back to this float.
    # Use the float's inherent precision as eps.
    # eps = 2^(exp - mantissa_bits - 1) where exp is the binary exponent
    _mant, exp = Math.frexp(self)
    # Float::MANT_DIG == 53 for IEEE 754 double
    eps = Rational(1, 2 ** (Float::MANT_DIG - exp + 1))
    Rational.__float_find_simplest(to_r, eps)
  end

  def fdiv(other)
    self / other
  end
end
