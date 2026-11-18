module Comparable
  # CRuby's `cmp_equal`: whatever `<=>` raises is the caller's to see
  # (it stopped being rescued in Ruby 2.3), and its answer is read as
  # `rb_cmpint` reads it -- an Integer by its sign, anything else by
  # asking it `> 0` and `< 0`.
  def ==(other)
    return true if equal?(other)
    res = self <=> other
    return false if res.nil?
    return res == 0 if res.is_a?(Integer)
    !(res > 0) && !(res < 0)
  end

  # CRuby's `rb_cmpint` for a `<=>` answer that is not an Integer: nil
  # is the comparison failure, anything else is asked `> 0` and then
  # `< 0`, and whatever those raise is the caller's to see (a String
  # answer is "comparison of String with 0 failed", an Object a
  # NoMethodError for `>`). An answer that is neither -- NaN -- reads
  # as 0.
  def __cmpint(res, recv, other)
    if res.nil?
      raise ArgumentError, "comparison of #{recv.class} with #{__coerce_failed_name(other)} failed"
    end
    return 1 if res > 0
    return -1 if res < 0
    0
  end
  private :__cmpint

  def >=(other)
    res = self <=> other
    return res >= 0 if res.is_a?(Integer)
    __cmpint(res, self, other) >= 0
  end

  def >(other)
    res = self <=> other
    return res > 0 if res.is_a?(Integer)
    __cmpint(res, self, other) > 0
  end

  def <=(other)
    res = self <=> other
    return res <= 0 if res.is_a?(Integer)
    __cmpint(res, self, other) <= 0
  end

  def <(other)
    res = self <=> other
    return res < 0 if res.is_a?(Integer)
    __cmpint(res, self, other) < 0
  end

  def clamp(min_val = nil, max_val = nil)
    if min_val.is_a?(Range)
      range = min_val
      # Use `#begin` / `#end` (raw endpoint accessors) rather than
      # `#first` / `#last`. The latter raise RangeError on
      # beginless / endless ranges in Ruby 3.x+, but `clamp` is
      # supposed to interpret a missing endpoint as "no bound on
      # that side", matching CRuby's C-level implementation.
      min_val = range.begin
      max_val = range.end
      if max_val && range.exclude_end?
        raise ArgumentError, "cannot clamp with an exclusive range"
      end
    end
    if min_val && max_val
      cmp = min_val <=> max_val
      # CRuby compares the bounds with OPTIMIZED_CMP: an incomparable
      # pair is the usual comparison failure, naming the bounds.
      raise ArgumentError, "comparison of #{min_val.class} with #{__coerce_failed_name(max_val)} failed" if cmp.nil?
      raise ArgumentError, "min argument must be less than or equal to max argument" if cmp > 0
    end
    # `self <=> bound`, as CRuby's cmp_clamp: a nil answer is
    # "comparison of X with Y failed", never a NoMethodError on nil.
    if min_val
      c = (self <=> min_val)
      raise ArgumentError, "comparison of #{self.class} with #{__coerce_failed_name(min_val)} failed" if c.nil?
      return self if c == 0
      return min_val if c < 0
    end
    if max_val
      c = (self <=> max_val)
      raise ArgumentError, "comparison of #{self.class} with #{__coerce_failed_name(max_val)} failed" if c.nil?
      return max_val if c > 0
    end
    self
  end

  # CRuby asks `self <=> min` / `self <=> max` (never the operands'
  # `<=>`), so a receiver whose `<=>` answers for foreign operands —
  # ActiveSupport's Time, say — is honoured, and a nil answer is the
  # usual comparison failure.
  #
  # Each answer is read as `rb_cmpint` reads it (#2046): an Integer by its
  # sign, anything else by `__cmpint` (nil is the comparison failure, then
  # `> 0` and `< 0` are asked, and an answer that is neither -- NaN -- is
  # 0).
  def between?(min, max)
    return false if (self <=> min).__cmpint_of(self, min) < 0
    (self <=> max).__cmpint_of(self, max) <= 0
  end
end
