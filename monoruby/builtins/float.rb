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
    to_r.numerator
  end

  def denominator
    return 1 if nan?
    return 1 if infinite?
    to_r.denominator
  end

  def fdiv(other)
    self / other
  end
end
