class Range
  include Enumerable
  
  def each
    return self.to_enum(:each) unless block_given?
    i = self.begin
    raise TypeError, "can't iterate from NilClass" if i.nil?
    e = self.end
    excl = self.exclude_end?
    if e.nil?
      loop do
        yield i
        i = i.succ
      end
    else
      loop do
        c = (i <=> e)
        break if c.nil?
        if excl
          break if c >= 0
        else
          break if c > 0
        end
        yield i
        # Avoid calling #succ on the final element (some custom types
        # only define succ up to the last value).
        break if !excl && c == 0
        i = i.succ
      end
    end
    self
  end

  def reverse_each(&block)
    b = self.begin
    e = self.end
    excl = self.exclude_end?

    raise TypeError, "can't iterate from NilClass" if e.nil?

    if (e.is_a?(Integer) || e.is_a?(Float)) && (b.nil? || b.is_a?(Integer))
      if e.is_a?(Float)
        if e.infinite?
          raise TypeError, "can't iterate from #{e.class}"
        end
        top = e.floor
        top -= 1 if excl && e == top
      else
        top = excl ? e - 1 : e
      end
      unless block_given?
        return to_enum(:reverse_each) do
          if b.nil?
            Float::INFINITY
          else
            top >= b ? top - b + 1 : 0
          end
        end
      end
      i = top
      if b.nil?
        loop do
          yield i
          i -= 1
        end
      else
        while i >= b
          yield i
          i -= 1
        end
      end
      return self
    end

    if b.nil?
      raise TypeError, "can't iterate from NilClass"
    end

    unless block_given?
      return to_enum(:reverse_each) do
        if b.is_a?(Numeric) || e.is_a?(Numeric)
          raise TypeError, "can't iterate from #{b.class}"
        else
          nil
        end
      end
    end

    unless b.respond_to?(:succ)
      raise TypeError, "can't iterate from #{b.class}"
    end
    arr = []
    self.each { |x| arr << x }
    arr.reverse_each(&block)
    self
  end
  
  def reject
    return self.to_enum(:reject) unless block_given?
    i = self.begin
    raise TypeError,"can't iterate from NilClass" if i.nil?
    e = self.end
    res = []
    if e.nil?
      while true
        unless yield(i)
          res << i
        end
        i = i.succ
      end
    else
      e = e.succ unless self.exclude_end?
      while i < e
        unless yield(i)
          res << i
        end
        i = i.succ
      end
    end
    res
  end

  def bsearch
    return to_enum(:bsearch) unless block_given?
    self.to_a.bsearch do |x|
      yield(x)
    end
  end

  def cover?(val)
    if val.is_a?(Range)
      __cover_range_q(val)
    else
      __cover_val_q(val)
    end
  end

  def ===(val)
    cover?(val)
  end

  def include?(val)
    b = self.begin
    e = self.end
    if (b.nil? || b.is_a?(Numeric)) && (e.nil? || e.is_a?(Numeric))
      return cover?(val)
    end
    if !b.nil? && b.respond_to?(:succ)
      self.each do |x|
        return true if x == val
      end
      return false
    end
    cover?(val)
  end
  alias member? include?

  def lazy
    Enumerator::Lazy.new(self)
  end

  def overlap?(other)
    unless other.is_a?(Range)
      raise TypeError, "wrong argument type #{other.class} (expected Range)"
    end

    return false if __range_empty_q(self)
    return false if __range_empty_q(other)

    s_b = self.begin
    s_e = self.end
    o_b = other.begin
    o_e = other.end

    if !s_e.nil? && !o_b.nil?
      cmp = (s_e <=> o_b)
      return false if cmp.nil?
      if self.exclude_end?
        return false if cmp <= 0
      else
        return false if cmp < 0
      end
    end

    if !o_e.nil? && !s_b.nil?
      cmp = (o_e <=> s_b)
      return false if cmp.nil?
      if other.exclude_end?
        return false if cmp <= 0
      else
        return false if cmp < 0
      end
    end

    true
  end

  def step(step_arg = nil, &block)
    b = self.begin
    e = self.end
    excl = self.exclude_end?

    b_numeric = b.is_a?(Numeric)
    e_compat  = e.nil? || e.is_a?(Numeric)
    numeric_range = b_numeric && e_compat

    if b.nil?
      step_is_num = step_arg.nil? || step_arg.is_a?(Numeric)
      if !e.is_a?(Numeric) || !step_is_num
        raise ArgumentError, "#step for non-numeric beginless ranges is meaningless"
      end
      if block_given?
        raise ArgumentError, "#step iteration for beginless ranges is meaningless"
      end
      return to_enum(:step, step_arg) { nil }
    end

    if numeric_range
      if step_arg.is_a?(Numeric) && step_arg == 0
        raise ArgumentError, "step can't be 0"
      end
      unless block_given?
        return to_enum(:step, step_arg) { __range_step_size(step_arg) }
      end
      step_val = __range_step_coerce(step_arg, b)
      __range_step_numeric(step_val, &block)
    else
      unless block_given?
        # Probe <=> once at call time so mocks that expect exactly one
        # call are satisfied regardless of later #size invocations.
        (b <=> e) rescue nil unless e.nil?
        return to_enum(:step, step_arg) { nil }
      end
      __range_step_non_numeric(step_arg, &block)
    end
    self
  end

  alias % step

  private

  def __cover_val_q(val)
    b = self.begin
    e = self.end
    unless b.nil?
      c = (b <=> val)
      return false if c.nil?
      return false if c > 0
    end
    unless e.nil?
      c = (val <=> e)
      return false if c.nil?
      if self.exclude_end?
        return false if c >= 0
      else
        return false if c > 0
      end
    end
    true
  end

  def __cover_range_q(val)
    s_b = self.begin
    s_e = self.end
    v_b = val.begin
    v_e = val.end

    return false if v_b.nil? && !s_b.nil?
    return false if v_e.nil? && !s_e.nil?

    if !s_b.nil? && !v_b.nil?
      c = (s_b <=> v_b)
      return false if c.nil?
      return false if c > 0
    end

    # Normalise exclusive Integer endpoints to inclusive (so `...11`
    # matches `..10`), then compare and handle equality for strict Floats.
    if !s_e.nil? && !v_e.nil?
      s_eff = (self.exclude_end? && s_e.is_a?(Integer)) ? s_e - 1 : s_e
      v_eff = (val.exclude_end?  && v_e.is_a?(Integer)) ? v_e - 1 : v_e
      c = (v_eff <=> s_eff)
      return false if c.nil?
      return false if c > 0
      if c == 0
        s_strict = self.exclude_end? && !s_e.is_a?(Integer)
        v_strict = val.exclude_end?  && !v_e.is_a?(Integer)
        return false if s_strict && !v_strict
      end
    end

    true
  end

  def __range_empty_q(r)
    b = r.begin
    e = r.end
    return false if b.nil? || e.nil?
    cmp = (b <=> e)
    return true if cmp.nil?
    r.exclude_end? ? cmp >= 0 : cmp > 0
  end

  # Validate and, if needed, coerce `step_arg` for a numeric range.
  # Returns the resulting numeric step.
  def __range_step_coerce(step_arg, b)
    if step_arg.nil?
      1
    elsif step_arg.is_a?(Numeric)
      step_arg
    elsif step_arg.respond_to?(:coerce)
      _, s = step_arg.coerce(b)
      unless s.is_a?(Numeric)
        raise TypeError, "no implicit conversion of #{step_arg.class} into Integer"
      end
      s
    else
      raise TypeError, "#{step_arg.class} can't be coerced into Integer"
    end
  end

  def __range_step_numeric(step_arg, &block)
    b = self.begin
    e = self.end
    excl = self.exclude_end?
    use_float = b.is_a?(Float) || e.is_a?(Float) || step_arg.is_a?(Float)

    if use_float
      beg  = b.to_f
      unit = step_arg.to_f
      if unit.infinite?
        if e.nil?
          yield beg
        else
          endv = e.to_f
          cond = if unit > 0
                   excl ? beg < endv : beg <= endv
                 else
                   excl ? beg > endv : beg >= endv
                 end
          yield beg if cond
        end
        return
      end
      if e.nil?
        i = 0
        loop do
          yield beg + i * unit
          i += 1
        end
      else
        endv = e.to_f
        n = (endv - beg) / unit
        return if n.nan? || n < 0
        if n.infinite?
          i = 0
          loop do
            yield beg + i * unit
            i += 1
          end
        end
        err = (beg.abs + endv.abs + (endv - beg).abs) / unit.abs * Float::EPSILON
        err = 0.5 if err > 0.5
        n_int = (n + err).floor
        i = 0
        while i <= n_int
          d = beg + i * unit
          # Avoid the `d = endv` after `if d > endv` pattern; it
          # currently triggers a JIT misoptimisation that freezes `d`
          # at its initial loop value. Branch straight to yielding
          # `endv` in the overshoot case.
          if unit > 0 && d > endv
            break if excl
            yield endv
            break
          elsif unit < 0 && d < endv
            break if excl
            yield endv
            break
          end
          break if excl && d == endv
          yield d
          i += 1
        end
      end
    else
      i = b
      if e.nil?
        loop do
          yield i
          i += step_arg
        end
      elsif step_arg > 0
        if excl
          while i < e
            yield i
            i += step_arg
          end
        else
          while i <= e
            yield i
            i += step_arg
          end
        end
      else
        if excl
          while i > e
            yield i
            i += step_arg
          end
        else
          while i >= e
            yield i
            i += step_arg
          end
        end
      end
    end
  end

  def __range_step_non_numeric(step_arg, &block)
    b = self.begin
    e = self.end
    excl = self.exclude_end?

    if step_arg.is_a?(Float)
      raise TypeError, "no implicit conversion of Float into Integer"
    end

    if step_arg.nil? || step_arg.is_a?(Integer)
      n = step_arg.nil? ? 1 : step_arg
      return if n == 0
      i = b
      if e.nil?
        loop do
          yield i
          n.times { i = i.succ }
        end
      else
        loop do
          c = (i <=> e)
          break if c.nil?
          if excl
            break if c >= 0
          else
            break if c > 0
          end
          yield i
          break if !excl && c == 0
          n.times { i = i.succ }
        end
      end
    else
      if e.nil?
        i = b
        loop do
          yield i
          i = i + step_arg
        end
      else
        dir = (b <=> e)
        raise ArgumentError, "bad value for range" if dir.nil?
        next_val = b + step_arg
        step_dir = (b <=> next_val)
        # Step moving opposite to the range direction yields nothing.
        if dir != 0 && step_dir != 0 && (dir < 0) != (step_dir < 0)
          return
        end
        i = b
        loop do
          c = (i <=> e)
          break if c.nil?
          if dir < 0
            break if excl ? c >= 0 : c > 0
          elsif dir > 0
            break if excl ? c <= 0 : c < 0
          else
            break if excl
          end
          yield i
          break if !excl && c == 0
          i = i + step_arg
        end
      end
    end
  end

  def __range_step_size(step_arg)
    b = self.begin
    e = self.end
    excl = self.exclude_end?
    return Float::INFINITY if e.nil?
    return nil unless b.is_a?(Numeric)
    return nil unless step_arg.nil? || step_arg.is_a?(Numeric)
    unit = (step_arg || 1).to_f
    beg  = b.to_f
    endv = e.to_f
    return 0 if unit.nan?
    if unit.infinite?
      if unit > 0
        return excl ? (beg < endv ? 1 : 0) : (beg <= endv ? 1 : 0)
      else
        return excl ? (beg > endv ? 1 : 0) : (beg >= endv ? 1 : 0)
      end
    end
    if endv.infinite?
      dir = (endv > 0 && unit > 0) || (endv < 0 && unit < 0)
      return dir ? Float::INFINITY : 0
    end
    n = (endv - beg) / unit
    return 0 if n.nan? || n < 0
    err = (beg.abs + endv.abs + (endv - beg).abs) / unit.abs * Float::EPSILON
    err = 0.5 if err > 0.5
    n_int = (n + err).floor
    if excl
      last = beg + n_int * unit
      overshoot = unit > 0 ? last >= endv : last <= endv
      n_int -= 1 if overshoot
    end
    n_int + 1
  end

  public

  def to_set(klass = Set, *args, &block)
    if self.end.nil?
      raise RangeError, "cannot convert endless range to a set"
    end
    klass.new(self, *args, &block)
  end
end