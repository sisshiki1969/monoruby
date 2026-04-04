# Rational class is now implemented in Rust (builtins/numeric/rational.rs).
# This file provides Kernel#Rational conversion method and helper methods
# that are more natural to implement in Ruby.

class Rational
  include Comparable

  def dup
    self
  end

  def nonzero?
    zero? ? nil : self
  end

  def floor(ndigits = 0)
    if ndigits == 0
      (numerator / denominator)
    else
      to_f.floor(ndigits)
    end
  end

  def ceil(ndigits = 0)
    if ndigits == 0
      -((-numerator) / denominator)
    else
      to_f.ceil(ndigits)
    end
  end

  def round(ndigits = 0)
    if ndigits == 0
      if denominator == 1
        numerator
      else
        q, r = numerator.divmod(denominator)
        doubled = r * 2
        if doubled > denominator
          q + 1
        elsif doubled < denominator
          q
        else
          q.even? ? q : q + 1
        end
      end
    else
      to_f.round(ndigits)
    end
  end

  def truncate(ndigits = 0)
    if ndigits == 0
      to_i
    elsif ndigits > 0
      (self * 10**ndigits).to_i.to_r / 10**ndigits
    else
      d = 10 ** (-ndigits)
      (to_i / d) * d
    end
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

  def divmod(other)
    q = (self / other).floor
    [q, self - q * other]
  end

  def %(other)
    divmod(other)[1]
  end
  alias modulo %

  def rationalize(eps = nil)
    self
  end

  def gcd(other)
    numerator.gcd(other)
  end

  def lcm(other)
    numerator.lcm(other)
  end
end

# Kernel#Rational conversion method
def Rational(a, b = nil)
  if b
    if a.is_a?(Float) || b.is_a?(Float)
      ar = a.is_a?(Float) ? Rational.__float_to_rational(a) : Rational(a)
      br = b.is_a?(Float) ? Rational.__float_to_rational(b) : Rational(b)
      return ar / br
    end
    if a.is_a?(Rational) || b.is_a?(Rational)
      return Rational(a) / Rational(b)
    end
    Rational.__new(a, b)
  else
    if a.is_a?(Rational)
      a
    elsif a.is_a?(Integer)
      Rational.__new(a, 1)
    elsif a.is_a?(Float)
      Rational.__float_to_rational(a)
    else
      raise TypeError, "can't convert #{a.class} into Rational"
    end
  end
end

class Rational
  def self.__new(num, den)
    raise TypeError, "not an integer" unless num.is_a?(Integer) && den.is_a?(Integer)
    raise ZeroDivisionError, "divided by 0" if den == 0
    # Allocate via Rust — creates a properly normalized RationalInner
    __allocate(num, den)
  end

  def self.__float_to_rational(f)
    raise FloatDomainError, "Infinity" if f.infinite?
    raise FloatDomainError, "NaN" if f.respond_to?(:nan?) && f.nan?
    return Rational.__new(0, 1) if f == 0.0

    negative = f < 0
    f = -f if negative

    n = 0
    x = f
    while x != x.floor
      x = x * 2
      n += 1
      break if n > 1074
    end

    numerator = x.to_i
    denominator = 1 << n

    numerator = -numerator if negative
    Rational.__new(numerator, denominator)
  end

  # Stern-Brocot algorithm to find simplest rational within eps of f
  def self.__float_find_simplest(f, eps)
    eps = eps.abs
    lo = f - eps
    hi = f + eps
    # Use mediant-based search (Stern-Brocot tree)
    p0, q0 = 0, 1
    p1, q1 = 1, 0
    loop do
      # Compute mediant
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
end
