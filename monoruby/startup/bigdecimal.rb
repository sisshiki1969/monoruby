# frozen_string_literal: true
#
# BigDecimal implementation for monoruby.
# Pure-Ruby arbitrary-precision decimal arithmetic using integer arithmetic.
#
# Internal representation:
#   @sign   - :pos, :neg, :nan, :pos_inf, :neg_inf, :pos_zero, :neg_zero
#   @coeff  - Non-negative Integer (coefficient / significant digits)
#   @exp    - Integer exponent: value = sign * @coeff * 10^@exp
#

class BigDecimal
  include Comparable

  # Sign constants
  SIGN_NaN                = 0
  SIGN_POSITIVE_ZERO      = 1
  SIGN_NEGATIVE_ZERO      = -1
  SIGN_POSITIVE_FINITE    = 2
  SIGN_NEGATIVE_FINITE    = -2
  SIGN_POSITIVE_INFINITE  = 3
  SIGN_NEGATIVE_INFINITE  = -3

  # Exception mode constants
  EXCEPTION_NaN        = 2
  EXCEPTION_INFINITY   = 1
  EXCEPTION_UNDERFLOW  = 4
  EXCEPTION_OVERFLOW   = 1
  EXCEPTION_ZERODIVIDE = 16
  EXCEPTION_ALL        = 255

  # Rounding mode constants
  ROUND_UP        = 1
  ROUND_DOWN      = 2
  ROUND_HALF_UP   = 3
  ROUND_HALF_DOWN = 4
  ROUND_CEILING   = 5
  ROUND_FLOOR     = 6
  ROUND_HALF_EVEN = 7

  ROUND_MODE = 256

  @@limit = 0
  @@mode = 0
  @@rounding_mode = ROUND_HALF_UP

  def self.limit(n = nil)
    if n
      old = @@limit
      @@limit = n
      old
    else
      @@limit
    end
  end

  def self.mode(type, value = nil)
    if type == ROUND_MODE
      if value
        @@rounding_mode = value
      end
      return @@rounding_mode
    end
    if value != nil
      if value
        @@mode |= type
      else
        @@mode &= ~type
      end
    end
    @@mode & type
  end

  def self.save_limit
    old = @@limit
    begin
      yield
    ensure
      @@limit = old
    end
  end

  def self.save_rounding_mode
    old = @@rounding_mode
    begin
      yield
    ensure
      @@rounding_mode = old
    end
  end

  def self.save_exception_mode
    old = @@mode
    begin
      yield
    ensure
      @@mode = old
    end
  end

  def self.double_fig
    16
  end

  # Internal constructor: sign, coeff (Integer >= 0), exp (Integer)
  # value = sign * coeff * 10^exp
  def initialize(sign, coeff, exp)
    @sign = sign
    @coeff = coeff
    @exp = exp
    _normalize!
  end

  def _normalize!
    return if @sign == :nan || @sign == :pos_inf || @sign == :neg_inf
    return if @sign == :pos_zero || @sign == :neg_zero
    if @coeff == 0
      @sign = @sign == :neg ? :neg_zero : :pos_zero
      @exp = 0
      return
    end
    # Remove trailing zeros from coeff, adjusting exp
    while @coeff > 0 && @coeff % 10 == 0
      @coeff /= 10
      @exp += 1
    end
  end
  private :_normalize!

  # Parse a value into BigDecimal
  def self.parse_value(val, prec = 0)
    case val
    when BigDecimal
      return prec > 0 ? val._round_to_sig(prec) : val
    when Integer
      if val == 0
        return new(:pos_zero, 0, 0)
      end
      neg = val < 0
      sign = neg ? :neg : :pos
      bd = new(sign, neg ? -val : val, 0)
      prec > 0 ? bd._round_to_sig(prec) : bd
    when Float
      if val.nan?
        return new(:nan, 0, 0)
      elsif val.infinite?
        return val > 0 ? new(:pos_inf, 0, 0) : new(:neg_inf, 0, 0)
      elsif val == 0.0
        return (1.0 / val) < 0 ? new(:neg_zero, 0, 0) : new(:pos_zero, 0, 0)
      end
      s = "%.20f" % val
      # Remove trailing zeros after decimal point
      if s.include?('.')
        s = s.sub(/0+\z/, '')
        s = s.sub(/\.\z/, '')
      end
      _parse_string(s, prec)
    when String
      _parse_string(val, prec)
    else
      if val.respond_to?(:to_str)
        return _parse_string(val.to_str, prec)
      end
      raise TypeError, "can't convert #{val.class} into BigDecimal"
    end
  end

  def self._parse_string(str, prec = 0)
    str = str.to_s.strip

    case str
    when /\A[+-]?NaN\z/i
      return new(:nan, 0, 0)
    when /\A\+?Infinity\z/i
      return new(:pos_inf, 0, 0)
    when /\A-Infinity\z/i
      return new(:neg_inf, 0, 0)
    end

    neg = false
    if str.start_with?('-')
      neg = true
      str = str[1..-1]
    elsif str.start_with?('+')
      str = str[1..-1]
    end

    exp_val = 0
    if str =~ /[eE]([+-]?\d+)\z/
      exp_val = $1.to_i
      str = str.sub(/[eE][+-]?\d+\z/, '')
    end

    str = str.delete('_')

    if str.include?('.')
      int_part, frac_part = str.split('.', 2)
      frac_part ||= ""
    else
      int_part = str
      frac_part = ""
    end

    int_part = "0" if int_part.nil? || int_part.empty?
    all_digits = int_part + frac_part

    coeff = all_digits.to_i
    if coeff == 0
      return neg ? new(:neg_zero, 0, 0) : new(:pos_zero, 0, 0)
    end

    # exp: the value is coeff * 10^(exp_val - frac_part.length)
    exp = exp_val - frac_part.length

    sign = neg ? :neg : :pos
    bd = new(sign, coeff, exp)
    prec > 0 ? bd._round_to_sig(prec) : bd
  end

  def nan?
    @sign == :nan
  end

  def infinite?
    case @sign
    when :pos_inf then 1
    when :neg_inf then -1
    else nil
    end
  end

  def finite?
    !nan? && !infinite?
  end

  def zero?
    @sign == :pos_zero || @sign == :neg_zero
  end

  def nonzero?
    zero? ? nil : self
  end

  def positive?
    @sign == :pos || @sign == :pos_zero || @sign == :pos_inf
  end

  def negative?
    @sign == :neg || @sign == :neg_zero || @sign == :neg_inf
  end

  def sign
    case @sign
    when :nan then SIGN_NaN
    when :pos_zero then SIGN_POSITIVE_ZERO
    when :neg_zero then SIGN_NEGATIVE_ZERO
    when :pos then SIGN_POSITIVE_FINITE
    when :neg then SIGN_NEGATIVE_FINITE
    when :pos_inf then SIGN_POSITIVE_INFINITE
    when :neg_inf then SIGN_NEGATIVE_INFINITE
    end
  end

  # Returns the exponent as CRuby defines it:
  # For value 0.{digits}e{N}, exponent is N
  # value = coeff * 10^@exp, so in scientific form: coeff has _ndigits digits
  # value = 0.{coeff} * 10^(@exp + _ndigits)
  def exponent
    return 0 if nan? || infinite? || zero?
    @exp + _ndigits
  end

  def n_significant_digits
    return 0 if nan? || infinite? || zero?
    _ndigits
  end

  def _ndigits
    @coeff.to_s.length
  end

  def to_f
    return 0.0 if @sign == :pos_zero
    return -0.0 if @sign == :neg_zero
    return Float::NAN if nan?
    return Float::INFINITY if @sign == :pos_inf
    return -Float::INFINITY if @sign == :neg_inf
    # Build float from string representation
    s = @coeff.to_s
    v = "#{s}e#{@exp}".to_f
    negative? ? -v : v
  end

  def to_i
    return 0 if zero?
    raise FloatDomainError, "NaN" if nan?
    raise FloatDomainError, "Infinity" if infinite?
    v = if @exp >= 0
      @coeff * (10 ** @exp)
    else
      @coeff / (10 ** (-@exp))
    end
    negative? ? -v : v
  end

  def to_d
    self
  end

  def to_s(fmt = '')
    return "NaN" if nan?
    prefix = negative? ? "-" : ""
    return "#{prefix}Infinity" if infinite?
    return "#{prefix}0.0" if zero?

    digits_str = @coeff.to_s
    e = @exp + digits_str.length  # exponent in 0.{digits}e{E} form

    if fmt.is_a?(String) && fmt =~ /(\d+)/
      group = $1.to_i
    else
      group = 0
    end

    if group > 0 && digits_str.length > group
      parts = []
      i = 0
      while i < digits_str.length
        parts << digits_str[i, group]
        i += group
      end
      "#{prefix}0.#{parts.join(' ')}e#{e}"
    else
      "#{prefix}0.#{digits_str}e#{e}"
    end
  end

  def inspect
    to_s
  end

  def coerce(other)
    case other
    when BigDecimal
      [other, self]
    when Integer, String
      [BigDecimal(other.to_s), self]
    when Float
      [BigDecimal(other, 0), self]
    else
      raise TypeError, "#{other.class} can't be coerced into BigDecimal"
    end
  end

  def -@
    case @sign
    when :pos then BigDecimal.new(:neg, @coeff, @exp)
    when :neg then BigDecimal.new(:pos, @coeff, @exp)
    when :pos_zero then BigDecimal.new(:neg_zero, 0, 0)
    when :neg_zero then BigDecimal.new(:pos_zero, 0, 0)
    when :pos_inf then BigDecimal.new(:neg_inf, 0, 0)
    when :neg_inf then BigDecimal.new(:pos_inf, 0, 0)
    when :nan then BigDecimal.new(:nan, 0, 0)
    end
  end

  def +@
    self
  end

  def abs
    negative? ? -self : self
  end

  def <=>(other)
    case other
    when BigDecimal
      return nil if nan? || other.nan?
      return 0 if infinite? && other.infinite? && infinite? == other.infinite?
      return 1 if @sign == :pos_inf
      return -1 if @sign == :neg_inf
      return -1 if other.infinite? == 1
      return 1 if other.infinite? == -1
      if zero? && other.zero?
        return 0
      elsif zero?
        return other.negative? ? 1 : -1
      elsif other.zero?
        return negative? ? -1 : 1
      end
      if negative? && !other.negative?
        return -1
      elsif !negative? && other.negative?
        return 1
      end
      # Same sign: compare absolute values
      # value = coeff * 10^exp
      # Normalize to same exp to compare
      cmp = _compare_abs(other)
      negative? ? -cmp : cmp
    when Integer, Float
      self <=> BigDecimal._coerce(other)
    else
      if other.respond_to?(:coerce)
        a, b = other.coerce(self)
        a <=> b
      else
        nil
      end
    end
  end

  def _compare_abs(other)
    # Compare |self| vs |other|
    # self = @coeff * 10^@exp, other = other.coeff * 10^other.exp
    e1 = @exp
    e2 = other._exp
    c1 = @coeff
    c2 = other._coeff
    if e1 > e2
      c1 = c1 * (10 ** (e1 - e2))
    elsif e2 > e1
      c2 = c2 * (10 ** (e2 - e1))
    end
    c1 <=> c2
  end

  def ==(other)
    cmp = (self <=> other)
    cmp.nil? ? false : cmp == 0
  end

  def eql?(other)
    other.is_a?(BigDecimal) && self == other
  end

  def hash
    return nan?.hash if nan?
    to_f.hash
  end

  def +(other)
    add(other, 0)
  end

  def -(other)
    add(-BigDecimal._coerce(other), 0)
  end

  def *(other)
    mult(other, 0)
  end

  def /(other)
    other = BigDecimal._coerce(other)
    return BigDecimal.new(:nan, 0, 0) if nan? || other.nan?
    if other.zero?
      return BigDecimal.new(:nan, 0, 0) if zero? || infinite?
      neg = negative? ^ other.negative?
      return neg ? BigDecimal.new(:neg_inf, 0, 0) : BigDecimal.new(:pos_inf, 0, 0)
    end
    if infinite?
      return BigDecimal.new(:nan, 0, 0) if other.infinite?
      neg = negative? ^ other.negative?
      return neg ? BigDecimal.new(:neg_inf, 0, 0) : BigDecimal.new(:pos_inf, 0, 0)
    end
    return BigDecimal._zero(negative? ^ other.negative?) if other.infinite?
    return BigDecimal._zero(negative? ^ other.negative?) if zero?

    # Use enough precision for a reasonable result
    prec = [n_significant_digits, other.n_significant_digits, BigDecimal.double_fig].max
    div(other, prec)
  end

  def %(other)
    other = BigDecimal._coerce(other)
    return BigDecimal.new(:nan, 0, 0) if nan? || other.nan? || other.zero?
    return BigDecimal.new(:nan, 0, 0) if infinite?
    return dup if zero?
    q = (self / other).fix
    self - q * other
  end
  alias modulo %

  def divmod(other)
    other = BigDecimal._coerce(other)
    q = (self / other).floor
    r = self - q * other
    [q, r]
  end

  def add(other, prec = 0)
    other = BigDecimal._coerce(other)
    return BigDecimal.new(:nan, 0, 0) if nan? || other.nan?
    if infinite?
      if other.infinite? && infinite? != other.infinite?
        return BigDecimal.new(:nan, 0, 0)
      end
      return dup
    end
    return other.dup if other.infinite?
    if zero?
      return other.zero? ? BigDecimal._zero(negative? && other.negative?) : other.dup
    end
    return dup if other.zero?

    # Align exponents
    e1 = @exp
    e2 = other._exp
    c1 = @coeff
    c2 = other._coeff
    min_exp = e1 < e2 ? e1 : e2
    c1 = c1 * (10 ** (e1 - min_exp)) if e1 > min_exp
    c2 = c2 * (10 ** (e2 - min_exp)) if e2 > min_exp

    v1 = negative? ? -c1 : c1
    v2 = other.negative? ? -c2 : c2
    result = v1 + v2

    if result == 0
      bd = BigDecimal._zero(false)
    elsif result > 0
      bd = BigDecimal.new(:pos, result, min_exp)
    else
      bd = BigDecimal.new(:neg, -result, min_exp)
    end
    prec > 0 ? bd._round_to_sig(prec) : bd
  end

  def sub(other, prec = 0)
    add(-BigDecimal._coerce(other), prec)
  end

  def mult(other, prec = 0)
    other = BigDecimal._coerce(other)
    return BigDecimal.new(:nan, 0, 0) if nan? || other.nan?
    if infinite? || other.infinite?
      return BigDecimal.new(:nan, 0, 0) if zero? || other.zero?
      neg = negative? ^ other.negative?
      return neg ? BigDecimal.new(:neg_inf, 0, 0) : BigDecimal.new(:pos_inf, 0, 0)
    end
    if zero? || other.zero?
      return BigDecimal._zero(negative? ^ other.negative?)
    end

    new_coeff = @coeff * other._coeff
    new_exp = @exp + other._exp
    neg = negative? ^ other.negative?
    bd = BigDecimal.new(neg ? :neg : :pos, new_coeff, new_exp)
    prec > 0 ? bd._round_to_sig(prec) : bd
  end

  def div(other, prec = 0)
    other = BigDecimal._coerce(other)
    return BigDecimal.new(:nan, 0, 0) if nan? || other.nan?
    if other.zero?
      return BigDecimal.new(:nan, 0, 0) if zero? || infinite?
      neg = negative? ^ other.negative?
      return neg ? BigDecimal.new(:neg_inf, 0, 0) : BigDecimal.new(:pos_inf, 0, 0)
    end
    if infinite?
      return BigDecimal.new(:nan, 0, 0) if other.infinite?
      neg = negative? ^ other.negative?
      return neg ? BigDecimal.new(:neg_inf, 0, 0) : BigDecimal.new(:pos_inf, 0, 0)
    end
    return BigDecimal._zero(negative? ^ other.negative?) if other.infinite?
    return BigDecimal._zero(negative? ^ other.negative?) if zero?

    neg = negative? ^ other.negative?

    if prec == 0
      # Integer division
      # Align to compute quotient
      p = [n_significant_digits, other.n_significant_digits, BigDecimal.double_fig].max
      # Scale numerator for precision
      scale = p + other._ndigits
      num = @coeff * (10 ** scale)
      q = num / other._coeff
      new_exp = @exp - other._exp - scale
      bd = BigDecimal.new(neg ? :neg : :pos, q, new_exp)
      return bd
    end

    # Scale numerator to get 'prec' significant digits in quotient
    # We need q to have at least 'prec' digits
    # q = (coeff1 * 10^extra) / coeff2, exp = exp1 - exp2 - extra
    extra = prec + other._ndigits
    num = @coeff * (10 ** extra)
    q = num / other._coeff
    r = num % other._coeff
    # Round: check if remainder >= half divisor
    if r * 2 >= other._coeff
      q += 1
    end
    new_exp = @exp - other._exp - extra
    bd = BigDecimal.new(neg ? :neg : :pos, q, new_exp)
    bd._round_to_sig(prec)
  end

  def fix
    return dup if nan? || infinite? || zero?
    # Integer part: coeff * 10^exp, keep only integer portion
    if @exp >= 0
      return dup  # already an integer
    end
    ndig = _ndigits
    int_digits = ndig + @exp  # number of integer digits
    if int_digits <= 0
      return BigDecimal._zero(negative?)
    end
    s = @coeff.to_s
    int_str = s[0, int_digits]
    BigDecimal.new(@sign, int_str.to_i, 0)
  end

  def frac
    return dup if nan? || infinite? || zero?
    if @exp >= 0
      return BigDecimal._zero(negative?)
    end
    int_part = fix
    self - int_part
  end

  def floor(n = 0)
    return dup if nan? || infinite? || zero?
    _round_at_decimal(n, :floor)
  end

  def ceil(n = 0)
    return dup if nan? || infinite? || zero?
    _round_at_decimal(n, :ceil)
  end

  def round(n = 0, mode = nil)
    return dup if nan? || infinite? || zero?
    mode ||= @@rounding_mode
    rmode = case mode
    when ROUND_UP then :up
    when ROUND_DOWN then :down
    when ROUND_HALF_UP then :half_up
    when ROUND_HALF_DOWN then :half_down
    when ROUND_CEILING then :ceil
    when ROUND_FLOOR then :floor
    when ROUND_HALF_EVEN then :half_even
    else :half_up
    end
    result = _round_at_decimal(n, rmode)
    if n <= 0
      result.to_i
    else
      result
    end
  end

  # Round to n decimal places (n can be negative)
  def _round_at_decimal(n, mode)
    # We want to round to 10^(-n) place
    # value = coeff * 10^exp
    # Shift so that the rounding position aligns
    target_exp = -n
    if @exp >= target_exp
      return dup  # no fractional part at this precision
    end

    shift = target_exp - @exp
    s = @coeff.to_s
    if shift >= s.length
      # All digits are below rounding position
      should_up = _should_round_digit(0, @coeff > 0, mode)
      if should_up
        return BigDecimal.new(@sign, 1, target_exp)
      else
        return BigDecimal._zero(negative?)
      end
    end

    keep_len = s.length - shift
    kept = s[0, keep_len].to_i
    rest_str = s[keep_len..-1]
    first_dropped = rest_str[0].to_i
    has_more = rest_str.length > 1 && rest_str[1..-1].chars.any? { |c| c != '0' }

    should_up = _should_round_up_check(first_dropped, has_more, kept, mode)

    if should_up
      kept += 1
    end

    if kept == 0
      return BigDecimal._zero(negative?)
    end

    BigDecimal.new(@sign, kept, target_exp)
  end

  def _should_round_digit(digit, has_more, mode)
    return false if digit == 0 && !has_more
    case mode
    when :up then true
    when :down then false
    when :half_up then digit >= 5
    when :half_down then digit > 5 || (digit == 5 && has_more)
    when :ceil then !negative?
    when :floor then negative?
    when :half_even then digit >= 5
    else digit >= 5
    end
  end

  def _should_round_up_check(first_dropped, has_more, kept, mode)
    return false if first_dropped == 0 && !has_more
    case mode
    when :up then true
    when :down then false
    when :half_up then first_dropped >= 5
    when :half_down then first_dropped > 5 || (first_dropped == 5 && has_more)
    when :ceil then !negative?
    when :floor then negative?
    when :half_even
      if first_dropped > 5
        true
      elsif first_dropped < 5
        false
      else
        has_more ? true : (kept % 2 != 0)
      end
    else first_dropped >= 5
    end
  end

  # Round to n significant digits
  def _round_to_sig(n)
    return dup if nan? || infinite? || zero?
    return dup if n <= 0
    ndig = _ndigits
    return dup if ndig <= n

    s = @coeff.to_s
    kept = s[0, n].to_i
    first_dropped = s[n].to_i
    has_more = n + 1 < s.length && s[(n + 1)..-1].chars.any? { |c| c != '0' }

    if first_dropped > 5 || (first_dropped == 5 && has_more)
      kept += 1
    elsif first_dropped == 5 && !has_more
      kept += 1 if kept % 2 != 0  # half-even
    end

    # Adjust exp: we removed (ndig - n) least significant digits
    new_exp = @exp + (ndig - n)
    # But if carry increased digit count, adjust
    if kept.to_s.length > n
      new_exp += 1
      kept /= 10
    end

    BigDecimal.new(@sign, kept, new_exp)
  end

  def _decimal_shift(n)
    return dup if nan? || infinite? || zero?
    BigDecimal.new(@sign, @coeff, @exp + n)
  end

  def dup
    BigDecimal.new(@sign, @coeff, @exp)
  end

  # Accessors for internal use
  def _coeff; @coeff; end
  def _exp; @exp; end
  def _sign_sym; @sign; end

  def self._coerce(val)
    case val
    when BigDecimal then val
    when Integer then parse_value(val)
    when Float then parse_value(val)
    when String then parse_value(val)
    else
      raise TypeError, "#{val.class} can't be coerced into BigDecimal"
    end
  end

  def self._zero(neg)
    neg ? new(:neg_zero, 0, 0) : new(:pos_zero, 0, 0)
  end

  def **(y)
    case y
    when BigDecimal, Integer, Float
      power(y)
    when nil
      raise TypeError, 'wrong argument type NilClass'
    else
      x, y = y.coerce(self)
      x ** y
    end
  end

  def power(y, prec = 0)
    y = BigDecimal._coerce(y) unless y.is_a?(Integer)
    return BigDecimal.new(:nan, 0, 0) if nan?
    return BigDecimal.new(:nan, 0, 0) if y.is_a?(BigDecimal) && y.nan?

    if y.is_a?(Integer)
      return BigDecimal(1) if y == 0
      if y < 0
        return BigDecimal(1).div(power(-y, prec), prec > 0 ? prec : [n_significant_digits, BigDecimal.double_fig].max)
      end
      # Exponentiation by squaring
      result = BigDecimal(1)
      base = self
      n = y
      while n > 0
        if n % 2 == 1
          result = result.mult(base, prec > 0 ? prec + 5 : 0)
        end
        base = base.mult(base, prec > 0 ? prec + 5 : 0)
        n /= 2
      end
      return prec > 0 ? result._round_to_sig(prec) : result
    end

    if y.is_a?(BigDecimal)
      if y.zero?
        return BigDecimal(1)
      end
      if y.infinite?
        if zero?
          return y.positive? ? BigDecimal._zero(false) : BigDecimal.new(:pos_inf, 0, 0)
        end
        return BigDecimal(1) if self == BigDecimal(1)
        a = self.abs
        if a > BigDecimal(1)
          return y.positive? ? BigDecimal.new(:pos_inf, 0, 0) : BigDecimal._zero(false)
        else
          return y.positive? ? BigDecimal._zero(false) : BigDecimal.new(:pos_inf, 0, 0)
        end
      end
      if infinite?
        if y < BigDecimal(0)
          return BigDecimal._zero(negative? && y.fix.to_i % 2 == 1)
        end
        neg = negative? && y.fix.to_i % 2 == 1
        return neg ? BigDecimal.new(:neg_inf, 0, 0) : BigDecimal.new(:pos_inf, 0, 0)
      end
      if zero?
        if y > BigDecimal(0)
          return BigDecimal._zero(false)
        end
        return BigDecimal.new(:pos_inf, 0, 0)
      end
      # Check if y is an integer value
      if y.frac.zero?
        return power(y.to_i, prec)
      end
      # For non-integer exponents, use exp(y * log(x))
      # This requires BigMath.exp and BigMath.log
      p = prec > 0 ? prec : [n_significant_digits, BigDecimal.double_fig].max
      # Fallback: convert to float
      result = to_f ** y.to_f
      return BigDecimal(result, p)
    end
    raise TypeError, "#{y.class} can't be coerced into BigDecimal"
  end

  def sqrt(prec)
    return BigDecimal.new(:pos_inf, 0, 0) if infinite? == 1
    raise FloatDomainError, 'sqrt of negative value' if negative? && !zero?
    raise FloatDomainError, "sqrt of 'NaN'(Not a Number)" if nan?
    return self if zero?

    if prec == 0
      prec = [n_significant_digits, BigDecimal.double_fig].max
    end

    # Newton's method for square root
    # Start with float approximation
    x = BigDecimal(Math.sqrt(to_f), prec + 2)
    10.times do
      x_new = x.add(self.div(x, prec + 2), prec + 2).div(BigDecimal(2), prec + 2)
      break if x == x_new
      x = x_new
    end
    x._round_to_sig(prec)
  end

  INFINITY = BigDecimal.new(:pos_inf, 0, 0)
  NAN = BigDecimal.new(:nan, 0, 0)
end

module Kernel
  def BigDecimal(val, prec = 0, exception: true)
    BigDecimal.parse_value(val, prec)
  rescue => e
    raise e if exception
    nil
  end
end

class Integer
  def to_d
    BigDecimal(self)
  end
end

class Float
  def to_d(prec = 0)
    BigDecimal(self, prec)
  end
end

class String
  def to_d
    BigDecimal(self)
  end
end
