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

  def floor(ndigits = 0)
    ndigits = __coerce_ndigits(ndigits)
    if ndigits == 0
      (numerator / denominator)
    elsif ndigits > 0
      return self if __ndigits_sufficient?(ndigits)
      d = 10 ** ndigits
      Rational((self * d).floor, d)
    else
      d = 10 ** (-ndigits)
      (to_i / d) * d
    end
  end

  def ceil(ndigits = 0)
    ndigits = __coerce_ndigits(ndigits)
    if ndigits == 0
      -((-numerator) / denominator)
    elsif ndigits > 0
      return self if __ndigits_sufficient?(ndigits)
      d = 10 ** ndigits
      Rational((self * d).ceil, d)
    else
      d = 10 ** (-ndigits)
      (to_i.ceil(-ndigits))
    end
  end

  def round(ndigits = 0, **kw)
    half = kw[:half]
    if half != nil && half != :up && half != :down && half != :even
      raise ArgumentError, "invalid rounding mode: #{half}"
    end
    ndigits = __coerce_ndigits(ndigits)
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
          case half
          when :up
            q + 1
          when :down
            q
          when :even
            q.even? ? q : q + 1
          else
            # Default: round half up (away from zero for positive, toward zero for negative)
            q + 1
          end
        end
      end
    elsif ndigits > 0
      return self if __ndigits_sufficient?(ndigits)
      d = 10 ** ndigits
      scaled = self * d
      rounded = Rational(scaled).round(0, half: half)
      Rational(rounded, d)
    else
      d = 10 ** (-ndigits)
      (to_i).round(ndigits, half: half)
    end
  end

  def truncate(ndigits = 0)
    ndigits = __coerce_ndigits(ndigits)
    if ndigits == 0
      to_i
    elsif ndigits > 0
      return self if __ndigits_sufficient?(ndigits)
      d = 10 ** ndigits
      Rational((self * d).truncate, d)
    else
      d = 10 ** (-ndigits)
      (to_i / d) * d
    end
  end

  private

  # Check if ndigits is sufficient to represent this Rational exactly
  # in decimal. Returns true if rounding at ndigits would not change the value.
  def __ndigits_sufficient?(ndigits)
    return true if denominator == 1
    t = denominator
    while t % 2 == 0; t /= 2; end
    while t % 5 == 0; t /= 5; end
    return false unless t == 1
    # Terminating decimal: count factors of 2 and 5 in denominator
    d2, d5 = 0, 0
    t = denominator
    while t % 2 == 0; t /= 2; d2 += 1; end
    while t % 5 == 0; t /= 5; d5 += 1; end
    ndigits >= (d2 > d5 ? d2 : d5)
  end

  def __coerce_ndigits(ndigits)
    if ndigits.is_a?(Integer)
      return ndigits
    elsif ndigits.respond_to?(:to_int)
      result = ndigits.to_int
      if result.nil?
        raise TypeError, "no implicit conversion of #{ndigits.class} into Integer"
      end
      unless result.is_a?(Integer)
        raise TypeError, "can't convert #{ndigits.class} into Integer (#{ndigits.class}#to_int gives #{result.class})"
      end
      return result
    else
      raise TypeError, "no implicit conversion of #{ndigits.class} into Integer"
    end
  end

  public

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
  # Works for both Float and Rational values of f
  def self.__float_find_simplest(f, eps)
    eps = eps.abs
    # Handle negative values: negate, find simplest, negate back
    if f < 0
      return -__float_find_simplest(-f, eps)
    end
    lo = f - eps
    hi = f + eps
    # If range includes zero or negative, return 0
    if hi < 0
      return Rational(0)
    end
    lo = Rational(0) if lo < 0
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
