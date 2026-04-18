class Numeric
  include Comparable

  def +@
    self
  end

  def -@
    a, b = coerce(0)
    a - b
  end

  def positive?
    self > 0
  end

  def negative?
    self < 0
  end

  def abs
    self < 0 ? -self : self
  end
  alias magnitude abs

  def abs2
    self * self
  end

  def zero?
    self == 0
  end

  def nonzero?
    zero? ? nil : self
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

  def real?
    true
  end

  def real
    self
  end

  def imag
    0
  end
  alias imaginary imag

  def conj
    self
  end
  alias conjugate conj

  def rect
    [self, 0]
  end
  alias rectangular rect

  def polar
    [abs, arg]
  end

  def i
    Complex(0, self)
  end

  def numerator
    to_r.numerator
  end

  def denominator
    to_r.denominator
  end

  def quo(other)
    Rational(self) / other
  end

  def fdiv(other)
    to_f / other
  end

  def div(other)
    raise ZeroDivisionError, "divided by 0" if other == 0
    (self / other).floor
  end

  def modulo(other)
    self - other * div(other)
  end
  alias % modulo

  def remainder(other)
    mod = self % other
    if mod != 0 && ((self < 0 && other > 0) || (self > 0 && other < 0))
      mod - other
    else
      mod
    end
  end

  def eql?(other)
    self.class == other.class && self == other
  end

  def divmod(other)
    [div(other), modulo(other)]
  end

  def coerce(other)
    if self.class == other.class
      [other, self]
    else
      if other.nil? || other.equal?(true) || other.equal?(false) || other.is_a?(Symbol)
        raise TypeError, "#{other.class} can't be coerced into #{self.class}"
      end
      begin
        [Float(other), Float(self)]
      rescue TypeError
        raise TypeError, "#{other.class} can't be coerced into #{self.class}"
      end
    end
  end

  def clone(freeze: true)
    raise ArgumentError, "can't unfreeze #{self.class}" if freeze == false
    self
  end

  def dup
    self
  end

  def to_int
    to_i
  end

  def to_c
    Complex(self, 0)
  end

  def to_r
    Rational(self)
  end

  def ceil(ndigits = 0)
    to_f.ceil(ndigits)
  end

  def floor(ndigits = 0)
    to_f.floor(ndigits)
  end

  def round(ndigits = 0)
    to_f.round(ndigits)
  end

  def truncate(ndigits = 0)
    to_f.truncate(ndigits)
  end

  def step(limit = nil, step = nil, by: nil, to: nil)
    if !to.nil? && !limit.nil?
      raise ArgumentError, "wrong number of arguments (given 2, expected 0..1)"
    end
    if !by.nil? && !step.nil?
      raise ArgumentError, "wrong number of arguments (given 2, expected 0..1)"
    end
    limit = to unless to.nil?
    step = by unless by.nil?
    step ||= 1
    raise ArgumentError, "step can't be 0" if step == 0
    return to_enum(:step, limit, step) unless block_given?

    # Non-numeric `step` (e.g. `"foo"`) passes through `to_enum`
    # unchanged -- matching CRuby, which only raises at iteration time
    # -- but must fail loudly once we actually start iterating.
    unless step.is_a?(Numeric)
      raise ArgumentError, "step must be numeric"
    end

    use_float = self.is_a?(Float) ||
                (!limit.nil? && limit.is_a?(Float)) ||
                step.is_a?(Float)

    if use_float
      beg  = self.to_f
      unit = step.to_f

      # `unit` is +/- Infinity: at most one yield at `beg`, if the
      # direction matches.
      if unit.infinite?
        if limit.nil?
          yield beg
        else
          endv = limit.to_f
          if unit > 0 ? beg <= endv : beg >= endv
            yield beg
          end
        end
        return self
      end

      if limit.nil?
        # Unbounded: counted loop to avoid accumulated FP drift.
        i = 0
        loop do
          yield beg + i * unit
          i += 1
        end
      else
        endv = limit.to_f
        n = (endv - beg) / unit
        if n.nan? || n < 0
          # Direction mismatch or NaN: no yield.
          return self
        end
        if n.infinite?
          # `endv` is +/- Infinity in the iteration direction: yield
          # forever, computing each value from the index to avoid
          # cumulative rounding error.
          i = 0
          loop do
            yield beg + i * unit
            i += 1
          end
        end
        # Tolerance mimicking CRuby's ruby_float_step:
        #   err = (|beg| + |end| + |end-beg|) / |unit| * DBL_EPSILON
        err = (beg.abs + endv.abs + (endv - beg).abs) / unit.abs * Float::EPSILON
        err = 0.5 if err > 0.5
        n = (n + err).floor
        (0..n).each do |i|
          d = beg + i * unit
          # Clamp overshoot past the boundary (floating-point rounding).
          if unit > 0 ? d > endv : d < endv
            d = endv
          end
          yield d
        end
      end
    else
      i = self
      if step > 0
        while limit.nil? || i <= limit
          yield i
          i += step
        end
      else
        while limit.nil? || i >= limit
          yield i
          i += step
        end
      end
    end
    self
  end
end
