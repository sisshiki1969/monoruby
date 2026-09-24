module Comparable
  def ==(other)
    return true if equal?(other)
    begin
      res = self <=> other
    rescue NoMethodError, ArgumentError
      return false
    end
    if res.nil?
      false
    elsif res.is_a?(Numeric)
      res == 0
    else
      raise ArgumentError, "comparison of #{self.class} with #{__coerce_failed_name(other)} failed"
    end
  end

  def >=(other)
    res = self <=> other
    if res.nil?
      raise ArgumentError, "comparison of #{self.class} with #{__coerce_failed_name(other)} failed"
    elsif res.is_a?(Numeric)
      res >= 0
    else
      raise ArgumentError, "comparison of #{self.class} with #{__coerce_failed_name(other)} failed"
    end
  end

  def >(other)
    res = self <=> other
    if res.nil?
      raise ArgumentError, "comparison of #{self.class} with #{__coerce_failed_name(other)} failed"
    elsif res.is_a?(Numeric)
      res > 0
    else
      raise ArgumentError, "comparison of #{self.class} with #{__coerce_failed_name(other)} failed"
    end
  end

  def <=(other)
    res = self <=> other
    if res.nil?
      raise ArgumentError, "comparison of #{self.class} with #{__coerce_failed_name(other)} failed"
    elsif res.is_a?(Numeric)
      res <= 0
    else
      raise ArgumentError, "comparison of #{self.class} with #{__coerce_failed_name(other)} failed"
    end
  end

  def <(other)
    res = self <=> other
    if res.nil?
      raise ArgumentError, "comparison of #{self.class} with #{__coerce_failed_name(other)} failed"
    elsif res.is_a?(Numeric)
      res < 0
    else
      raise ArgumentError, "comparison of #{self.class} with #{__coerce_failed_name(other)} failed"
    end
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
  def between?(min, max)
    c = (self <=> min)
    raise ArgumentError, "comparison of #{self.class} with #{__coerce_failed_name(min)} failed" if c.nil?
    return false if c < 0
    c = (self <=> max)
    raise ArgumentError, "comparison of #{self.class} with #{__coerce_failed_name(max)} failed" if c.nil?
    c <= 0
  end
end
