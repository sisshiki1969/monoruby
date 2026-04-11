# Rational class is now implemented in Rust (builtins/numeric/rational.rs).
# This file provides Kernel#Rational conversion method and helper methods
# that are more natural to implement in Ruby.

class Rational
  include Comparable

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

  def to_c
    Complex(to_f, 0)
  end

  def coerce(other)
    if other.is_a?(Integer)
      [Rational(other, 1), self]
    elsif other.is_a?(Float)
      [other, to_f]
    elsif other.is_a?(Rational)
      [other, self]
    else
      raise TypeError, "#{other.class} can't be coerced into Rational"
    end
  end

  def fdiv(other)
    to_f / other.to_f
  end

  def remainder(other)
    self - (self / other).truncate * other
  end

  def div(other)
    (self / other).floor
  end

  def divmod(other)
    q = (self / other).floor
    [q, self - q * other]
  end

  def %(other)
    divmod(other)[1]
  end
  alias modulo %

  def rationalize(eps = nil)
    return self if eps.nil?
    eps = eps.abs
    lo = self - eps
    hi = self + eps
    # Stern-Brocot search for simplest rational in [lo, hi]
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
end
