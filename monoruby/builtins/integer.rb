class Integer
  # succ / next are implemented in Rust (integer.rs) with a JIT inline
  # specialization.

  def to_i
    self
  end
  alias to_int to_i

  def dup
    self
  end

  def positive?
    self > 0
  end

  # floor, ceil, round, truncate are implemented in Rust (integer.rs)

  def times
    return self.to_enum(:times) { self > 0 ? self : 0 } unless block_given?
    i = 0
    while i < self
      yield i
      i += 1
    end
    self
  end

  # Pure Ruby, like #times above: the JIT compiles the while loop per
  # call site with the yield lowered to a near-direct block call, which
  # beats a native loop crossing the Rust->block invoker boundary every
  # iteration for the common short spans (and allocates nothing — a
  # `&block` parameter would materialize a Proc per call).
  def upto(limit)
    unless block_given?
      return to_enum(:upto, limit) { self <= limit ? limit - self + 1 : 0 }
    end
    i = self
    while i <= limit
      yield i
      i += 1
    end
    self
  end

  def downto(limit)
    unless block_given?
      return to_enum(:downto, limit) { self >= limit ? self - limit + 1 : 0 }
    end
    i = self
    while i >= limit
      yield i
      i -= 1
    end
    self
  end

  def negative?
    self < 0
  end

  # abs and magnitude are defined in Rust for correctness with BigInt edge cases
  # def abs
  #   self < 0 ? -self : self
  # end
  # alias magnitude abs

  def integer?
    true
  end

  def ord
    self
  end


  def pred
    self - 1
  end

  def div(other)
    unless other.is_a?(Integer) || other.is_a?(Float) || other.is_a?(Rational)
      # `rb_num_coerce_bin(x, y, id_div)`
      a, b = __num_coerce(self, other)
      return a.div(b)
    end
    raise ZeroDivisionError, "divided by 0" if other == 0
    # A bignum receiver over a Float is `rb_big_divide`'s `div` row,
    # `rb_dbl2big(x / y)`: the quotient is truncated toward zero, where
    # a fixnum receiver (`fix_divide`) floors it (#2045).
    if other.is_a?(Float) && (self > 0x3fff_ffff_ffff_ffff || self < -0x4000_0000_0000_0000)
      return (self.to_f / other).truncate
    end
    (self / other).floor
  end

  def ceildiv(other)
    -(-self).div(other)
  end

  # `int_remainder`. The Integer case stays this small so that the JIT
  # inlines it; everything else is in `__remainder_slow`.
  def remainder(other)
    return __remainder_slow(other) unless other.is_a?(Integer)
    r = self % other
    if r != 0 && (self < 0) != (other < 0)
      r - other
    else
      r
    end
  end

  # A Fixnum goes on to `num_remainder`; a Bignum coerces and asks again
  # (`rb_big_remainder`).
  # An Integer `<=>` answer is read by its sign: itself (see
  # `BasicObject#__cmpint_of` in src/builtins/kernel.rs).
  def __cmpint_of(recv, other) = self

  def __remainder_slow(other)
    if self <= 0x3fff_ffff_ffff_ffff && self >= -0x4000_0000_0000_0000
      return __num_remainder(other)
    end
    a, b = __num_coerce(self, other)
    a.remainder(b)
  end
  private :__remainder_slow

  def fdiv(other)
    if other.is_a?(Integer)
      sf = self.to_f
      of = other.to_f
      if self.bit_length > 53 || other.bit_length > 53
        # When either operand exceeds f64 mantissa precision (53 bits),
        # use BigInt division to produce the correct IEEE 754 double.
        # We scale the numerator up so that the integer quotient has
        # ~54 significant bits, then use Math.ldexp to apply the
        # 2^(-extra) scaling without overflow.
        # A zero numerator keeps the divisor's sign (`0.0 / -x`).
        return (other < 0 ? -0.0 : 0.0) if self == 0
        if other == 0
          return sf.infinite? ? sf / 0.0 : 1.0 / 0.0 * (self < 0 ? -1 : 1)
        end
        sign = (self < 0) != (other < 0) ? -1 : 1
        a = self.abs
        b = other.abs
        lbits = a.bit_length
        rbits = b.bit_length
        # Target ~56 significant bits in quotient (3 extra beyond f64's
        # 53-bit mantissa) so to_f has guard, round, and sticky bits.
        extra = 56 - lbits + rbits
        if extra > 0
          q, r = (a << extra).divmod(b)
        elsif extra < 0
          q, r = a.divmod(b << -extra)
        else
          q, r = a.divmod(b)
        end
        # Set sticky bit: if remainder is nonzero, ensure LSB is 1
        # so that to_f rounds correctly (IEEE 754 round-to-nearest-even)
        q |= 1 if r != 0
        result = Math.ldexp(q.to_f, -extra)
        sign < 0 ? -result : result
      else
        sf / of
      end
    elsif other.is_a?(Float)
      self.to_f / other
    else
      # `rb_int_fdiv_double`: `NUM2DBL(rb_num_coerce_bin(x, y, idFdiv))`,
      # so whatever the coerced receiver answers is made a Float (#2045).
      __fdiv_coerce(other)
    end
  end

  def gcd(other)
    unless other.is_a?(Integer)
      raise TypeError, "not an integer"
    end
    a = self.abs
    b = other.abs
    while b != 0
      a, b = b, a % b
    end
    a
  end

  def lcm(other)
    unless other.is_a?(Integer)
      raise TypeError, "not an integer"
    end
    return 0 if self == 0 || other == 0
    (self / self.gcd(other) * other).abs
  end

  def gcdlcm(other)
    unless other.is_a?(Integer)
      raise TypeError, "not an integer"
    end
    [gcd(other), lcm(other)]
  end


  def allbits?(mask)
    mask = __to_int(mask)
    (self & mask) == mask
  end

  def anybits?(mask)
    mask = __to_int(mask)
    (self & mask) != 0
  end

  def nobits?(mask)
    mask = __to_int(mask)
    (self & mask) == 0
  end

  # bit_length is defined in Rust for performance with BigInt

  # `rb_int_coerce`: an Integer is paired as it is; for anything else
  # both sides go through `rb_Float`, so a Rational or a Complex argument
  # is a pair of Floats (and a Complex with an imaginary part the
  # RangeError of its `to_f`).
  def coerce(other)
    return [other, self] if other.is_a?(Integer)
    return [other, to_f] if other.is_a?(Float)
    [Float(other), to_f]
  end

  def numerator
    self
  end

  def denominator
    1
  end

  def to_r
    Rational(self, 1)
  end

  def rationalize(eps = nil)
    Rational(self, 1)
  end

  def self.sqrt(n)
    n = __to_int(n)
    raise Math::DomainError, "out of domain - isqrt" if n < 0
    return 0 if n == 0
    # Newton's method for integer square root
    x = 1 << ((n.bit_length + 1) / 2)
    loop do
      y = (x + n / x) / 2
      break x if y >= x
      x = y
    end
  end

  def self.try_convert(obj)
    if obj.is_a?(Integer)
      obj
    else
      # `rb_check_integer_type`: nil for no answer or a nil one.
      result = __check_funcall(obj, :to_int, nil)
      unless result.nil? || result.is_a?(Integer)
        raise TypeError, "can't convert #{obj.class} to Integer (#{obj.class}#to_int gives #{result.class})"
      end
      result
    end
  end

  private

  def __coerce_ndigits(ndigits)
    if ndigits.is_a?(Integer)
      # Check for BigInt values that are too large for round/floor/ceil/truncate
      if ndigits > 0x3FFFFFFF || ndigits < -0x40000000
        raise RangeError, "integer #{ndigits} too big to convert to `int'"
      end
      return ndigits
    elsif ndigits.is_a?(Float)
      if ndigits.infinite?
        raise RangeError, "float #{ndigits > 0 ? 'Inf' : '-Inf'} out of range of integer"
      end
      if ndigits.nan?
        raise RangeError, "float NaN out of range of integer"
      end
      ndigits = ndigits.to_int
    else
      ndigits = __to_int(ndigits)
    end
    # Check for large values after conversion
    if ndigits > 0x3FFFFFFF || ndigits < -0x40000000
      raise RangeError, "integer #{ndigits} too big to convert to `int'"
    end
    ndigits
  end

  public

  def digits(base = 10)
    # CRuby's `rb_int_digits`: the receiver's sign first, then the radix
    # through `rb_to_int`, a negative one named as such (#1930).
    if self < 0
      raise Math::DomainError, "out of domain"
    end
    base = __to_int(base)
    raise ArgumentError, "negative radix" if base < 0
    if base < 2
      raise ArgumentError, "invalid radix #{base}"
    end
    if self == 0
      return [0]
    end
    res = []
    n = self
    while n > 0
      res << n % base
      n /= base
    end
    res
  end
end
