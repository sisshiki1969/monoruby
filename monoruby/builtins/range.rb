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
            # Beginless is only iterable when end is Integer.
            unless e.is_a?(Integer)
              raise TypeError, "can't iterate from #{e.class}"
            end
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
          # Match CRuby: report the end's class (falling back to begin's).
          reported = e.nil? ? b.class : e.class
          raise TypeError, "can't iterate from #{reported}"
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

  def bsearch(&block)
    b = self.begin
    e = self.end
    excl = self.exclude_end?

    # Reject non-numeric ranges unconditionally (including when no block).
    numeric_b = b.nil? || b.is_a?(Numeric)
    numeric_e = e.nil? || e.is_a?(Numeric)
    unless numeric_b && numeric_e
      sample = numeric_b ? e : b
      raise TypeError, "can't do binary search for #{sample.class}"
    end

    unless block_given?
      return to_enum(:bsearch)
    end

    has_float = b.is_a?(Float) || e.is_a?(Float) ||
                (b.is_a?(Float) && b.infinite?) || (e.is_a?(Float) && e.infinite?)

    if has_float
      __bsearch_float(b, e, excl, &block)
    else
      __bsearch_int(b, e, excl, &block)
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

  # Classify block-return value for bsearch.
  # Returns :satisfied, :unsatisfied, :equal, :less, :greater, or raises.
  def __bsearch_classify(v)
    case v
    when true
      :satisfied
    when nil, false
      :unsatisfied
    when Numeric
      if v == 0
        :equal
      elsif v > 0
        :greater # current element is too low; search upward
      else
        :less    # current element is too high; search downward
      end
    else
      raise TypeError, "wrong argument type #{v.class} (must be numeric, true, false or nil)"
    end
  end

  def __bsearch_int(b, e, excl)
    # Convert exclusive end to inclusive.
    hi = if e.nil?
           nil
         elsif excl
           e - 1
         else
           e
         end

    # Beginless: scan downward with doubling steps until block transitions
    # to "before the target" (false/nil for find-min, positive for find-any),
    # giving us a real lower search bound. If it never transitions, the
    # answer (if any) lives at the bottom of the explored range; fall back
    # to a regular search starting from the last probed candidate.
    if b.nil?
      return nil if hi.nil?
      step = 1
      b = hi
      loop do
        candidate = hi - step
        v = yield(candidate)
        c = __bsearch_classify(v)
        if c == :unsatisfied || c == :greater
          b = candidate
          break
        end
        step *= 2
      end
    end

    # Endless: scan up with doubling steps until we find an upper bound
    # that is "at or past the target".
    if hi.nil?
      step = 1
      hi = b
      loop do
        candidate = b + step
        v = yield(candidate)
        c = __bsearch_classify(v)
        if c == :satisfied || c == :equal || c == :less
          hi = candidate
          break
        end
        step *= 2
      end
    end

    return nil if b > hi

    satisfied = nil
    lo = b
    while lo <= hi
      mid = lo + (hi - lo) / 2
      v = yield(mid)
      c = __bsearch_classify(v)
      case c
      when :equal
        return mid
      when :satisfied
        satisfied = mid
        hi = mid - 1
      when :less
        hi = mid - 1
      when :greater, :unsatisfied
        lo = mid + 1
      end
    end
    satisfied
  end

  def __bsearch_float(b, e, excl)
    inf = Float::INFINITY
    # Empty cases
    return nil if !b.nil? && !e.nil? && b > e
    return nil if !b.nil? && !e.nil? && excl && b == e

    # Normalise endpoints.
    lo_f = b.nil? ? -inf : b.to_f
    hi_f = e.nil? ?  inf : e.to_f
    return nil if lo_f > hi_f

    # Treat each float bit-pattern as a sortable u64 (IEEE 754 trick).
    mask = (1 << 64) - 1
    sign = 1 << 63
    to_key = ->(f) {
      signed = [f].pack('D').unpack1('q')
      bits = signed & mask
      if (bits >> 63) == 0
        bits | sign
      else
        (~bits) & mask
      end
    }
    from_key = ->(k) {
      bits = if (k >> 63) == 1
               k & ~sign
             else
               (~k) & mask
             end
      signed = bits >= sign ? bits - (1 << 64) : bits
      [signed].pack('q').unpack1('D')
    }

    # If exclusive, drop one ULP from the upper bound.
    lo_k = to_key.call(lo_f)
    hi_k = to_key.call(hi_f)
    hi_k -= 1 if excl

    return nil if lo_k > hi_k

    satisfied = nil
    while lo_k <= hi_k
      mid_k = lo_k + (hi_k - lo_k) / 2
      mid = from_key.call(mid_k)
      v = yield(mid)
      c = __bsearch_classify(v)
      case c
      when :equal
        return mid
      when :satisfied
        satisfied = mid
        hi_k = mid_k - 1
      when :less
        hi_k = mid_k - 1
      when :greater, :unsatisfied
        lo_k = mid_k + 1
      end
    end
    satisfied
  end

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