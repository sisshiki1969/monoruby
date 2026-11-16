class Numeric
  include Comparable

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
    unless other.is_a?(Numeric)
      raise TypeError, "#{__coerce_failed_name(other)} can't be coerced into Rational"
    end
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
    if other.respond_to?(:coerce)
      a, b = other.coerce(self)
    else
      a, b = self, other
    end
    mod = a % b
    if mod == 0
      mod
    elsif (a < 0 && b > 0) || (a > 0 && b < 0)
      mod - b
    else
      mod
    end
  end

  def singleton_method_added(name)
    raise TypeError, "can't define singleton method \"#{name}\" for #{self.class}"
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
      # CRuby's num_coerce goes through rb_Float() on both sides: its
      # error ("can't convert nil into Float"), not "can't be coerced".
      if other.nil? || other.equal?(true) || other.equal?(false) || other.is_a?(Symbol)
        raise TypeError, "can't convert #{__builtin_class_name(other)} into Float"
      end
      begin
        [Float(other), Float(self)]
      rescue TypeError
        raise TypeError, "can't convert #{other.class} into Float"
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

  def step(limit = nil, step = (no_step = true; nil), by: nil, to: nil)
    # The positional spelling as written, kept for
    # `ArithmeticSequence#inspect` (`1.step(10, 3)` prints itself back).
    given_args = no_step ? (limit.nil? ? [] : [limit]) : [limit, step]
    if !to.nil? && !limit.nil?
      raise ArgumentError, "to is given twice"
    end
    if !by.nil? && !step.nil?
      raise ArgumentError, "step is given twice"
    end
    limit = to unless to.nil?
    # `1.step(5, nil)` with a block: the nil was written, so it is not
    # the default (CRuby's "compatibility" check in
    # `num_step_check_fix_args`). Without a block it reads as 1.
    if step.nil? && by.nil? && !no_step && block_given?
      raise TypeError, "step must be numeric"
    end
    step = by unless by.nil?
    step ||= 1
    raise ArgumentError, "step can't be 0" if step == 0
    unless block_given?
      # Numeric step and limit ⇒ ArithmeticSequence (introspectable via
      # `aseq.begin/end/step`, matches CRuby and `Range#step`).
      # Anything else keeps the plain-Enumerator contract: nothing is
      # checked until it is iterated or asked its `size`.
      if step.is_a?(Numeric) && (limit.nil? || limit.is_a?(Numeric))
        return Enumerator::ArithmeticSequence
          .__build(self, limit, step, false)
          .__set_origin(self, :step, given_args)
      else
        return to_enum(:step, limit, step) { __step_size(limit, step) }
      end
    end

    # A step that is not a Numeric is not rejected by its class: CRuby
    # asks it `step > 0` for the direction (`num_step_negative_p`) and
    # then counts with `+` and `>` / `<`, so what is raised is whatever
    # those raise — `0 can't be coerced into Object` for an object with
    # no `>`, String's own comparison failure for `"a"` (#1988).
    unless step.is_a?(Numeric)
      desc = __step_negative_p(step)
      # No limit is ±Infinity, and a Float anywhere takes the float
      # loop, which wants the step as a C double.
      if limit.nil? || self.is_a?(Float) || limit.is_a?(Float)
        raise TypeError, "can't convert #{step.class} into Float"
      end
      i = self
      if desc
        until i < limit
          yield i
          i += step
        end
      else
        until i > limit
          yield i
          i += step
        end
      end
      return self
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

  private

  # Whether a non-Numeric `step` counts downwards: `!(step > 0)`, asked
  # of the step itself. An object that has no `>` at all is the
  # TypeError a failed coercion of `0` with it would be (CRuby's
  # `num_step_negative_p`).
  def __step_negative_p(step)
    unless step.respond_to?(:>, true)
      raise TypeError, "0 can't be coerced into #{step.class}"
    end
    !(step > 0)
  end

  # Compute the size that `step(limit, step)` will yield, without
  # actually iterating. Invoked lazily via the size block attached to
  # `to_enum(:step, ...)`. Returns:
  #   - `Float::INFINITY` when iteration is unbounded in the step's
  #     direction (`limit == nil` or limit = +/-INFINITY aligned with
  #     step's sign),
  #   - `1` / `0` for infinite step, depending on whether the first
  #     yield occurs,
  #   - `floor((limit - self)/step + err) + 1` otherwise,
  #   - raises for a non-numeric step what `.each` raises for it.
  def __step_size(limit, step)
    unless step.is_a?(Numeric)
      __step_negative_p(step)
      raise TypeError, "can't convert #{step.class} into Float"
    end
    return Float::INFINITY if limit.nil?
    beg  = self.to_f
    unit = step.to_f
    endv = limit.to_f
    if unit.infinite?
      return (unit > 0 ? (beg <= endv ? 1 : 0) : (beg >= endv ? 1 : 0))
    end
    if endv.infinite?
      dir = (endv > 0 && unit > 0) || (endv < 0 && unit < 0)
      return dir ? Float::INFINITY : 0
    end
    n = (endv - beg) / unit
    return 0 if n.nan? || n < 0
    err = (beg.abs + endv.abs + (endv - beg).abs) / unit.abs * Float::EPSILON
    err = 0.5 if err > 0.5
    (n + err).floor + 1
  end
end
