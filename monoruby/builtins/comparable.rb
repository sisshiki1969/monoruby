module Comparable
  def ==(other)
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
      min_val = range.first
      max_val = range.last
      # Exclude end not supported for clamp with range that has exclude_end
      if max_val && range.exclude_end?
        raise ArgumentError, "cannot clamp with an exclusive range"
      end
    end
    if min_val && max_val && (min_val <=> max_val) > 0
      raise ArgumentError, "min argument must be less than or equal to max argument"
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
