module Comparable
  def ==(other)
    return true if equal?(other)
    # A class that includes Comparable but never defines its own
    # `<=>` inherits `Object#<=>`, which falls back to `self ==
    # other`; calling `<=>` here would then recurse infinitely. Such
    # an object has no ordering, so it is only `==` to itself
    # (matches CRuby, which uses an equivalent recursion guard).
    return false if self.class.instance_method(:<=>).owner == ::Object
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
      raise ArgumentError, "comparison of #{self.class} with #{other.class} failed"
    end
  end

  def !=(other)
    res = self <=> other
    if res.nil?
      raise ArgumentError, "comparison of #{self.class} with #{other.class} failed"
    elsif res.is_a?(Numeric)
      res != 0
    else
      raise ArgumentError, "comparison of #{self.class} with #{other.class} failed"
    end
  end

  def >=(other)
    res = self <=> other
    if res.nil?
      raise ArgumentError, "comparison of #{self.class} with #{other.class} failed"
    elsif res.is_a?(Numeric)
      res >= 0
    else
      raise ArgumentError, "comparison of #{self.class} with #{other.class} failed"
    end
  end

  def >(other)
    res = self <=> other
    if res.nil?
      raise ArgumentError, "comparison of #{self.class} with #{other.class} failed"
    elsif res.is_a?(Numeric)
      res > 0
    else
      raise ArgumentError, "comparison of #{self.class} with #{other.class} failed"
    end
  end

  def <=(other)
    res = self <=> other
    if res.nil?
      raise ArgumentError, "comparison of #{self.class} with #{other.class} failed"
    elsif res.is_a?(Numeric)
      res <= 0
    else
      raise ArgumentError, "comparison of #{self.class} with #{other.class} failed"
    end
  end

  def <(other)
    res = self <=> other
    if res.nil?
      raise ArgumentError, "comparison of #{self.class} with #{other.class} failed"
    elsif res.is_a?(Numeric)
      res < 0
    else
      raise ArgumentError, "comparison of #{self.class} with #{other.class} failed"
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
      if cmp.nil? || cmp > 0
        raise ArgumentError, "min argument must be less than or equal to max argument"
      end
    end
    if min_val && (self <=> min_val) < 0
      min_val
    elsif max_val && (self <=> max_val) > 0
      max_val
    else
      self
    end
  end

  def between?(min, max)
    (min <=> self) <= 0 && (self <=> max) <= 0
  end
end
