class Rational < Numeric
  include Comparable

  attr_reader :numerator, :denominator

  def initialize(numerator, denominator = 1)
    raise TypeError, "not an integer" unless numerator.is_a?(Integer) && denominator.is_a?(Integer)
    raise ZeroDivisionError, "divided by 0" if denominator == 0

    if denominator < 0
      numerator = -numerator
      denominator = -denominator
    end

    g = numerator.gcd(denominator)
    @numerator = numerator / g
    @denominator = denominator / g
  end

  def +(other)
    if other.is_a?(Rational)
      Rational(@numerator * other.denominator + other.numerator * @denominator,
               @denominator * other.denominator)
    elsif other.is_a?(Integer)
      Rational(@numerator + other * @denominator, @denominator)
    elsif other.is_a?(Float)
      to_f + other
    else
      a, b = other.coerce(self)
      a + b
    end
  end

  def -(other)
    if other.is_a?(Rational)
      Rational(@numerator * other.denominator - other.numerator * @denominator,
               @denominator * other.denominator)
    elsif other.is_a?(Integer)
      Rational(@numerator - other * @denominator, @denominator)
    elsif other.is_a?(Float)
      to_f - other
    else
      a, b = other.coerce(self)
      a - b
    end
  end

  def *(other)
    if other.is_a?(Rational)
      Rational(@numerator * other.numerator, @denominator * other.denominator)
    elsif other.is_a?(Integer)
      Rational(@numerator * other, @denominator)
    elsif other.is_a?(Float)
      to_f * other
    else
      a, b = other.coerce(self)
      a * b
    end
  end

  def /(other)
    if other.is_a?(Rational)
      raise ZeroDivisionError, "divided by 0" if other.numerator == 0
      Rational(@numerator * other.denominator, @denominator * other.numerator)
    elsif other.is_a?(Integer)
      raise ZeroDivisionError, "divided by 0" if other == 0
      Rational(@numerator, @denominator * other)
    elsif other.is_a?(Float)
      to_f / other
    else
      a, b = other.coerce(self)
      a / b
    end
  end

  def -@
    Rational(-@numerator, @denominator)
  end

  def +@
    self
  end

  def abs
    Rational(@numerator.abs, @denominator)
  end

  def **(other)
    if other.is_a?(Integer)
      if other >= 0
        Rational(@numerator ** other, @denominator ** other)
      else
        Rational(@denominator ** (-other), @numerator ** (-other))
      end
    elsif other.is_a?(Float)
      to_f ** other
    elsif other.is_a?(Rational)
      to_f ** other.to_f
    else
      a, b = other.coerce(self)
      a ** b
    end
  end

  def ==(other)
    if other.is_a?(Rational)
      @numerator == other.numerator && @denominator == other.denominator
    elsif other.is_a?(Integer)
      @denominator == 1 && @numerator == other
    elsif other.is_a?(Float)
      to_f == other
    else
      other == self
    end
  end

  def <=>(other)
    if other.is_a?(Rational)
      (@numerator * other.denominator) <=> (other.numerator * @denominator)
    elsif other.is_a?(Integer)
      @numerator <=> (other * @denominator)
    elsif other.is_a?(Float)
      to_f <=> other
    else
      nil
    end
  end

  def hash
    @numerator.hash ^ @denominator.hash
  end

  def eql?(other)
    other.is_a?(Rational) && @numerator == other.numerator && @denominator == other.denominator
  end

  def to_i
    @numerator / @denominator
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

  def to_f
    @numerator.to_f / @denominator.to_f
  end

  def to_r
    self
  end

  def to_s
    "#{@numerator}/#{@denominator}"
  end

  def inspect
    "(#{@numerator}/#{@denominator})"
  end

  def to_c
    Complex(@numerator.to_f / @denominator.to_f, 0)
  end

  def zero?
    @numerator == 0
  end

  def positive?
    @numerator > 0
  end

  def negative?
    @numerator < 0
  end

  def nonzero?
    zero? ? nil : self
  end

  def integer?
    false
  end

  def finite?
    true
  end

  def infinite?
    nil
  end

  def frozen?
    true
  end

  def freeze
    self
  end

  def floor(ndigits = 0)
    if ndigits == 0
      @numerator / @denominator  # Ruby integer division floors
    else
      to_f.floor(ndigits)
    end
  end

  def ceil(ndigits = 0)
    if ndigits == 0
      -(-@numerator / @denominator)
    else
      to_f.ceil(ndigits)
    end
  end

  def round(ndigits = 0)
    if ndigits == 0
      if @denominator == 1
        @numerator
      else
        # Banker's rounding for halfway cases
        q, r = @numerator.divmod(@denominator)
        doubled = r * 2
        if doubled > @denominator
          q + 1
        elsif doubled < @denominator
          q
        else
          # Halfway: round to even
          q.even? ? q : q + 1
        end
      end
    else
      to_f.round(ndigits)
    end
  end

  def rationalize(eps = nil)
    if eps
      # Return simplest rational within eps of self
      self
    else
      self
    end
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

  def quo(other)
    self / other
  end

  def remainder(other)
    r = self - (self / other).truncate * other
    r
  end

  def divmod(other)
    q = (self / other).floor
    [q, self - q * other]
  end

  def %(other)
    divmod(other)[1]
  end
  alias modulo %

  def gcd(other)
    @numerator.gcd(other)
  end

  def lcm(other)
    @numerator.lcm(other)
  end
end

# Kernel#Rational conversion method
def Rational(a, b = nil)
  if b
    if a.is_a?(Rational)
      a = a
    end
    if b.is_a?(Rational)
      # Rational(Rational(a,b), Rational(c,d)) = Rational(a*d, b*c)
      return Rational(a) / Rational(b) if a.is_a?(Rational) || b.is_a?(Rational)
    end
    if a.is_a?(Float) || b.is_a?(Float)
      # Convert floats to rationals first, then combine
      ar = a.is_a?(Float) ? Rational.__float_to_rational(a) : Rational(a)
      br = b.is_a?(Float) ? Rational.__float_to_rational(b) : Rational(b)
      return ar / br
    end
    Rational.new(a, b)
  else
    if a.is_a?(Rational)
      a
    elsif a.is_a?(Integer)
      Rational.new(a, 1)
    elsif a.is_a?(Float)
      Rational.__float_to_rational(a)
    else
      raise TypeError, "can't convert #{a.class} into Rational"
    end
  end
end

class Rational
  # Convert a Float to a Rational using the exact binary representation.
  # Float is represented as m * 2^e where m is the integer significand.
  def self.__float_to_rational(f)
    raise FloatDomainError, "Infinity" if f.infinite?
    raise FloatDomainError, "NaN" if f.respond_to?(:nan?) && f.nan?
    return Rational.new(0, 1) if f == 0.0

    negative = f < 0
    f = -f if negative

    # Use the standard approach: multiply by 2^53 to get the integer significand
    # then adjust the exponent
    # f = significand * 2^exponent where significand is an integer

    # Find exponent such that f * 2^-exponent is an integer
    # We use the fact that a Float has 53 bits of precision
    n = 0
    x = f
    while x != x.floor
      x = x * 2
      n += 1
      break if n > 1074  # max possible for double
    end

    numerator = x.to_i
    denominator = 1 << n

    numerator = -numerator if negative
    Rational.new(numerator, denominator)
  end
end
