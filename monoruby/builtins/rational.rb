# Rational class is now implemented in Rust (builtins/numeric/rational.rs).
# This file provides Kernel#Rational conversion method and helper methods
# that are more natural to implement in Ruby.

class Rational
  include Comparable

  class << self
    undef_method :new
  end

  def to_r
    self
  end

  def +@
    self
  end

  def freeze
    self
  end

  def frozen?
    true
  end

  def finite?
    true
  end

  def infinite?
    nil
  end

  def integer?
    false
  end

  def dup
    self
  end

  def nonzero?
    zero? ? nil : self
  end

  # `numeric_to_c`: the Rational itself is the real part.
  def to_c
    Complex.rect(self, 0)
  end

  def coerce(other)
    if other.is_a?(Integer)
      [Rational(other, 1), self]
    elsif other.is_a?(Float)
      [other, to_f]
    elsif other.is_a?(Rational)
      [other, self]
    elsif other.is_a?(Complex)
      # CRuby's nurat_coerce: a Complex with an exact-zero imaginary part
      # coerces to a Rational of its real part, anything else to Complex.
      imag = other.imaginary
      if imag.is_a?(Integer) && imag == 0 || imag.is_a?(Rational) && imag == 0
        real = other.real
        [real.is_a?(Float) ? real.to_r : Rational(real), self]
      else
        [other, Complex(self, 0)]
      end
    else
      raise TypeError, "#{other.class} can't be coerced into Rational"
    end
  end

  # `rb_rational_fdiv`: the quotient is Rational's own (which coerces
  # an argument that is not a number), converted afterwards.
  def fdiv(other)
    return self / 0.0 if other == 0
    return to_f if other.equal?(1)
    quo = self / other
    quo.is_a?(Float) ? quo : quo.to_f
  end

  # `div`, `divmod`, `%` / `modulo` and `remainder` are Numeric's, as in
  # CRuby (rational.c defines none of them).

  # An epsilon that is given is asked for its `abs`, nil included
  # (`nurat_rationalize` hands `argv[0]` to `f_abs`).
  def rationalize(eps = (no_eps = true; nil))
    return self if no_eps
    eps = eps.abs
    # Stern-Brocot operates on non-negative rationals; handle sign outside.
    return -(-self).rationalize(eps) if self < 0
    lo = self - eps
    hi = self + eps
    # If 0 is within [lo, hi] the simplest answer is 0/1.
    return Rational(0, 1) if lo <= 0
    p0, q0 = 0, 1
    p1, q1 = 1, 0
    loop do
      pm = p0 + p1
      qm = q0 + q1
      med = Rational(pm, qm)
      if med < lo
        k = ((lo * qm - pm) / (p1 - lo * q1)).ceil
        p0 = p0 + k * p1
        q0 = q0 + k * q1
      elsif med > hi
        k = ((pm - hi * qm) / (hi * q0 - p0)).ceil
        p1 = p1 + k * p0
        q1 = q1 + k * q0
      else
        return Rational(pm, qm)
      end
    end
  end

  def gcd(other)
    numerator.gcd(other)
  end

  def lcm(other)
    numerator.lcm(other)
  end

  private

  def marshal_dump
    [numerator, denominator]
  end
end

# True alias required by ruby/spec's strict
# `Klass.instance_method(:a) == Klass.instance_method(:b)` identity checks
# (the 2026-06 "rely less on shared examples" refactoring wave). The
# aliased original must already be defined (natively or above) by now.

class Rational
  alias magnitude abs
end
