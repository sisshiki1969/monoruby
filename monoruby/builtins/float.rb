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

  # `flo_coerce`: the argument through `rb_Float`.
  def coerce(other)
    return [other, self] if other.is_a?(Float)
    return [other.to_f, self] if other.is_a?(Integer)
    [Float(other), self]
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

# True alias required by ruby/spec's strict
# `Klass.instance_method(:a) == Klass.instance_method(:b)` identity checks
# (the 2026-06 "rely less on shared examples" refactoring wave). The
# aliased original must already be defined (natively or above) by now.

class Float
  alias modulo %
  alias quo fdiv
end
