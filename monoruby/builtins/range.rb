class Range
  include Enumerable
  
  def each
    return self.to_enum(:each) { size rescue nil } unless block_given?
    i = self.begin
    raise TypeError, "can't iterate from NilClass" if i.nil?
    e = self.end
    # No `start <=> end` probe here: `range_each` does not compare, and
    # building the Range already did (see `gen_range`).
    # Reject elements that have no `succ`, even if the range is empty —
    # CRuby validates this up front.
    unless i.respond_to?(:succ)
      raise TypeError, "can't iterate from #{i.class}"
    end
    excl = self.exclude_end?
    if e.nil?
      # Use `while true`, not `Kernel#loop`: `loop` rescues StopIteration,
      # which would swallow a StopIteration raised by the yielded block
      # instead of letting it propagate out of `each`. CRuby's C
      # `Range#each` does not intercept it.
      while true
        yield i
        i = i.succ
      end
    else
      # Integer fast path: both endpoints are Integers — by far the most
      # common shape. A plain `<=` / `+ 1` replaces the generic `<=>` /
      # `succ` dispatch, and iterating without `Kernel#loop` both lets a
      # StopIteration from the block propagate (CRuby-compatible) and keeps
      # the `yield` eligible for JIT block specialization.
      if i.is_a?(Integer) && e.is_a?(Integer)
        last = excl ? e - 1 : e
        while i <= last
          yield i
          i += 1
        end
        return self
      end
      # String / Symbol ranges are `String#upto`, as CRuby's `range_each`
      # has them: it calls `rb_str_upto_each(beg, end, excl, …)` for a
      # String range and the same over `to_s` for a Symbol one. That
      # brings the single-ASCII-character walk (`("A".."z")` yields the
      # punctuation between `Z` and `a`, which `#succ` would skip), the
      # all-digit walk, the same-length stopping rule, and — the reason
      # this stopped being a loop written out here — the termination
      # guards an empty endpoint needs, since `"".succ` is `""` (#1480).
      if i.is_a?(Symbol) && e.is_a?(Symbol)
        i.to_s.upto(e.to_s, excl) { |s| yield s.to_sym }
        return self
      end
      if i.is_a?(String) && e.is_a?(String)
        i.upto(e, excl) { |s| yield s }
        return self
      end
      # `while true`, not `Kernel#loop`: see the `e.nil?` branch above —
      # `loop` would swallow a StopIteration raised by the yielded block.
      while true
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
  
  # `Enumerable#map` / `#flat_map` with an Integer fast path in front.
  # `__builtin_map` runs the two-Fixnum case as a counted loop, with no
  # `<=>` / `succ` dispatch per element, and answers `nil` for every other
  # shape — a String or Symbol range, an endless one, a Bignum endpoint, a
  # type that only has `succ` — which then walks `Range#each` by way of
  # `Enumerable`. `map` always builds an Array, so `nil` is unambiguous.
  def map(&block)
    return to_enum(__callee__) { size rescue nil } unless block
    res = __builtin_map(&block)
    res.nil? ? super : res
  end
  alias collect map

  def flat_map(&block)
    return to_enum(__callee__) { size rescue nil } unless block
    res = __builtin_flat_map(&block)
    res.nil? ? super : res
  end
  alias collect_concat flat_map

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
    # The numeric case (both endpoints and the argument) is two
    # comparisons in Rust; `nil` means "not that shape", and the general
    # implementation below decides.
    fast = __cover_num_q(val)
    return fast unless fast.nil?
    if val.is_a?(Range)
      __cover_range_q(val)
    else
      __cover_val_q(val)
    end
  end

  # `range_eqq` is `r_cover_p`, the value-only test: a Range argument is
  # compared like any other value (so it is not "inside" a numeric
  # range), and a redefined `cover?` is not consulted. Only `cover?`
  # itself has the range-in-range rule.
  def ===(val)
    fast = __cover_num_q(val)
    return fast unless fast.nil?
    __cover_val_q(val)
  end

  def include?(val)
    fast = __cover_num_q(val)
    return fast unless fast.nil?
    b = self.begin
    e = self.end
    # Fully-open range: accept "linear" values (CRuby's linear_object_p:
    # Numeric and Time), reject anything else as "inclusion
    # undecidable".
    if b.nil? && e.nil?
      return true if val.is_a?(Numeric) || val.is_a?(Time)
      raise TypeError, "cannot determine inclusion in beginless/endless ranges"
    end
    # A range with a linear endpoint is continuous: `r_cover_p`, the
    # value-only test (not `cover?`, see `===`).
    # So is one with an endpoint that answers `to_int`
    # (`rb_check_to_integer` in `range_include_internal`).
    if b.is_a?(Numeric) || e.is_a?(Numeric) || b.is_a?(Time) || e.is_a?(Time) ||
       !__check_int(b).nil? || !__check_int(e).nil?
      return __cover_val_q(val)
    end
    # Non-linear ranges with a nil endpoint cannot decide inclusion.
    if b.nil? || e.nil?
      raise TypeError, "cannot determine inclusion in beginless/endless ranges"
    end
    # String ranges convert the argument with #to_str first (CRuby's
    # rb_check_string_type in rb_str_include_range_p).
    if b.is_a?(String) && e.is_a?(String) && !val.is_a?(String)
      val = __check_str(val)
      return false if val.nil?
    end
    # Both endpoints present and non-linear: `range_include` gives up
    # and calls super, `Enumerable#include?`, which walks `each` — and
    # `each` raises TypeError for a begin without `succ`.
    self.each do |x|
      return true if x == val
    end
    false
  end
  alias member? include?

  # An Integer range without a block is an arithmetic series, which
  # `__builtin_sum` answers in closed form instead of adding up every term
  # through Enumerable#sum — the difference between O(1) and O(size), and
  # between an answer and no answer at all for a range like (1..10**20).
  # It returns nil when the shape does not apply (non-Integer endpoints or
  # init), which is never a real sum, so anything else falls back here.
  def sum(init = 0)
    unless block_given?
      res = __builtin_sum(init)
      return res unless res.nil?
      # The same formula adds an initial value that is neither an Integer
      # nor a Float by `rb_int_plus(init, sum)`, which coerces: the sum is
      # asked to `coerce` the initial value, so `(1..3).sum(nil)` is
      # Integer#coerce's TypeError and `(1..3).sum(1r)` is 7.0.
      b = self.begin
      e = self.end
      if ::Integer === b && ::Integer === e && !(::Float === init)
        e -= 1 if exclude_end?
        return init if e < b
        x, y = ((e - b + 1) * (e + b) / 2).coerce(init)
        return x + y
      end
    end
    super
  end

  # The n argument of min(n)/max(n) without a block: CRuby makes the
  # result with `rb_ary_new2(NUM2LONG(n))`. (With a block it hands over
  # to Enumerable#min / #max, whose checks are `__nmin_n`.)
  def __minmax_n(n)
    n = __num2long(n)
    raise ArgumentError, "negative array size (or size too big)" if n < 0
    __ary_capa(n)
  end

  # Without a block, a count that is *given* is converted even when it
  # is nil (`(1..3).min(nil)` is a TypeError: CRuby's `range_min` hands
  # any argument to `range_first`); with one, Enumerable's `min` takes
  # nil for "no count".
  def min(n = (no_n = true; nil), &block)
    raise RangeError, "cannot get the minimum of beginless range" if self.begin.nil?
    if block
      if self.end.nil?
        raise RangeError, "cannot get the minimum of endless range with custom comparison method"
      end
      # `range_min` hands a call with a block to Enumerable#min.
      # So does one with a count (`rb_nmin_run` over `each`).
      return super(&block) if n.nil?
      return super(n, &block)
    end
    return __builtin_min if no_n
    n = __minmax_n(n)
    # min(n) == the first n elements in iteration order: works lazily for
    # endless Integer ranges, yields [] for empty/backward ones, and
    # raises TypeError via #each when the begin has no #succ (Float,
    # Time, Array, ...).
    res = []
    each do |x|
      break if res.size == n
      res << x
    end
    res
  end

  # CRuby's `range_minmax`: with a block it is Enumerable#minmax; without
  # one, `[min, max]`, computed from the endpoints.
  def minmax(&block)
    return super if block
    __builtin_minmax
  end

  def max(n = (no_n = true; nil), &block)
    e = self.end
    raise RangeError, "cannot get the maximum of endless range" if e.nil?
    if block
      if self.begin.nil?
        raise RangeError, "cannot get the maximum of beginless range with custom comparison method"
      end
      return super(&block) if n.nil?
      return super(n, &block)
    end
    if n.nil?
      if exclude_end? && !e.is_a?(Numeric)
        # CRuby's Enumerable fallback: an exclusive range with a
        # non-numeric, succ-iterable end is iterated to its rightmost
        # element (the numeric fast path below instead raises
        # "cannot exclude non Integer end value" for e.g. Float ends —
        # which cover?'s range-argument rescue relies on).
        if self.begin.nil?
          raise RangeError, "cannot get the maximum of beginless range with custom comparison method"
        end
        m = nil
        each { |x| m = x }
        return m
      end
      return __builtin_max if no_n
    end
    n = __minmax_n(n)
    if e.is_a?(Integer)
      # Arithmetic from the top: handles beginless ends and huge spans
      # ((0...2**64).max(2)) without iterating from the start.
      hi = exclude_end? ? e - 1 : e
      b = self.begin
      lo = hi - n + 1
      unless b.nil?
        c = (b <=> hi)
        return [] if c.nil? || c > 0
        lo = b if b.is_a?(Integer) && lo < b
      end
      res = []
      v = hi
      while v >= lo
        res << v
        v -= 1
      end
      return res
    end
    # Non-Integer end: materialize the range (raises TypeError via #each
    # for begins that cannot be iterated — Float, Time, Array, nil).
    to_a.last(n).reverse
  end

  # `first(n)` fallback for ranges the native fast path cannot index
  # (endless, `Float::INFINITY`-ended, String, …): walk `#each` and break
  # after `n` elements, so an infinite range terminates. Called from
  # `Range#first` in Rust; not part of the public API.
  #
  # `#each` runs even for `first(0)`, as CRuby's `range_first` runs it:
  # a range that cannot be iterated — `(1.0..3.0)`, a Time range — is a
  # TypeError whatever the count (#1930). The count is tested before an
  # element is taken, as `first_i` tests it.
  private def __range_first_by_each(n)
    res = []
    each do |x|
      break if res.size >= n
      res << x
    end
    res
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

  # Range#step, as CRuby's `range_step` (range.c) decides it.
  #
  # The step is not converted (`to_int` plays no part since Ruby 3.4): a
  # numeric range with a numeric step is an arithmetic progression, a
  # String or Symbol range with an Integer step keeps its old reading
  # ("every n-th element by #succ"), and everything else is `begin + step`
  # repeated — so a step that `Integer#+` cannot take is that method's
  # TypeError (`Object can't be coerced into Integer`), raised when the
  # iteration starts, not when the Enumerator is made (#1988).
  def step(step_arg = (no_step = true; nil), &block)
    b = self.begin
    e = self.end
    b_num = b.is_a?(Numeric)
    e_num = e.is_a?(Numeric)
    str_b = b.is_a?(String) || b.is_a?(Symbol)

    if no_step
      unless b_num || str_b || (b.nil? && e_num)
        raise ArgumentError, "step is required for non-numeric ranges"
      end
      step_arg = 1
    end
    step_num = step_arg.is_a?(Numeric)

    if step_num && b_num && step_arg == 0
      raise ArgumentError, "step can't be 0"
    end

    # `block_given?`, not `block`: reading the parameter would make the
    # Proc, and forwarding it with `&block` below does not.
    unless block_given?
      # An ArithmeticSequence only for what is one: numeric endpoints
      # (a beginless range included — `ary[(..-1) % 3]` slices with it)
      # *and* a numeric step. Any other step makes a plain Enumerator,
      # whose size is unknown.
      if step_num && ((b_num && (e.nil? || e_num)) || (b.nil? && e_num))
        return Enumerator::ArithmeticSequence.__build(b, e, step_arg, self.exclude_end?)
          .__set_origin(self, :step, no_step ? [] : [step_arg])
      end
      if b.nil?
        raise ArgumentError, "#step for non-numeric beginless ranges is meaningless"
      end
      # No `<=>` probe: building the Range already compared the
      # endpoints (see `Range#each`).
      return no_step ? to_enum(:step) { nil } : to_enum(:step, step_arg) { nil }
    end

    if b.nil?
      raise ArgumentError, "#step iteration for beginless ranges is meaningless"
    end

    if b_num && step_num && (e.nil? || e_num)
      __range_step_numeric(step_arg, &block)
    elsif str_b && step_arg.is_a?(Integer) && step_arg.bit_length < 63
      __range_step_succ(step_arg, &block)
    else
      __range_step_generic(step_arg, &block)
    end
    self
  end

  # `Range#%` is `Range#step` under another name: the same iteration
  # with a block, the same sequence without one, and only the Enumerator
  # (`#<Enumerator: 1..5:%(x)>`) or ArithmeticSequence (`((1..10).%(2))`)
  # it returns remembers which spelling made it.
  def %(step_arg, &block)
    return step(step_arg, &block) if block_given?
    seq = step(step_arg)
    if seq.is_a?(Enumerator::ArithmeticSequence)
      seq.__set_origin(self, :%, [step_arg])
    else
      to_enum(:%, step_arg) { nil }
    end
  end

  private :__builtin_map, :__builtin_flat_map

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

  # Port of CRuby's r_cover_range_p (range.c).
  def __cover_range_q(val)
    s_e = self.end
    v_b = val.begin
    v_e = val.end

    return false if v_b.nil? && !self.begin.nil?
    return false if v_e.nil? && !s_e.nil?

    # An empty or backward other is never covered.
    if !v_b.nil? && !v_e.nil?
      c = (v_b <=> v_e)
      return false if c.nil?
      return false if c > (val.exclude_end? ? -1 : 0)
    end

    return false if !v_b.nil? && !__cover_val_q(v_b)

    if !v_e.nil? && !s_e.nil?
      cmp_end = (s_e <=> v_e)
      return false if cmp_end.nil?
    elsif s_e.nil? && !v_e.nil?
      # self is endless, other is bounded: self's end (+∞) is greater,
      # so even an exclusive self covers other's end.
      cmp_end = 1
    else
      # Both ends nil (a bounded self with an endless other already
      # returned false above): equal.
      cmp_end = 0
    end

    if exclude_end? == val.exclude_end?
      return cmp_end >= 0
    elsif exclude_end?
      return cmp_end > 0
    elsif cmp_end >= 0
      return true
    end

    # self is inclusive, other is exclusive, and other's raw endpoint
    # lies past self.end: other may still be covered if its actual
    # maximum element is — e.g. (1..2).cover?(1...3), or a generic
    # succ-iterable other whose rightmost value <= self.end.
    v_max = begin
      val.max
    rescue TypeError
      nil
    end
    return false if v_max.nil?
    c = (s_e <=> v_max)
    !c.nil? && c >= 0
  end

  def __range_empty_q(r)
    b = r.begin
    e = r.end
    return false if b.nil? || e.nil?
    cmp = (b <=> e)
    return true if cmp.nil?
    r.exclude_end? ? cmp >= 0 : cmp > 0
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

  # A String or Symbol range with a Fixnum step: every `n`-th element of
  # the sequence `String#upto` walks (the pre-3.4 behaviour, kept for
  # these two). The count is CRuby's `step_i`: a countdown to the next
  # element to yield, which starts at 1 and is reset to `n` — so the
  # first element is always yielded, and a step of zero or less never
  # counts down to another one (the walk still runs to the end).
  def __range_step_succ(n)
    b = self.begin
    e = self.end
    sym = b.is_a?(Symbol)
    count = 1
    if e.nil?
      s = sym ? b.name : b.dup
      while true
        count -= 1
        if count == 0
          yield(sym ? s.to_sym : s)
          count = n
        end
        s = s.succ
      end
    else
      (sym ? b.name : b).upto(sym ? e.name : e, self.exclude_end?) do |s|
        count -= 1
        if count == 0
          yield(sym ? s.to_sym : s)
          count = n
        end
      end
    end
  end

  # `a <=> b` as CRuby's `r_less` reads it: -1, 0 or 1, and a value that
  # is none of them (`INT_MAX` there) when the two do not compare.
  def __range_r_less(a, b)
    c = (a <=> b)
    return 2 if c.nil?
    c > 0 ? 1 : (c < 0 ? -1 : 0)
  end

  # Everything else: `begin + step` repeated, in the direction the range
  # runs. This is where a numeric range ends up with a step that is not
  # a Numeric — the first `+` raises what `Integer#+` / `Float#+` raise
  # for it, or goes through its `coerce`.
  def __range_step_generic(step)
    b = self.begin
    e = self.end
    excl = self.exclude_end?
    v = b

    if e.nil?
      while true
        yield v
        v = v + step
      end
    elsif b.is_a?(Numeric) && step.is_a?(Numeric) && __range_r_less(step, 0) < 0
      # Backwards, for consistency with ArithmeticSequence.
      if excl
        while __range_r_less(e, v) < 0
          yield v
          v = v + step
        end
      else
        while (c = __range_r_less(e, v)) <= 0
          yield v
          break if c == 0
          v = v + step
        end
      end
    elsif (dir = __range_r_less(b, e)) == 0
      yield v unless excl
    elsif dir == __range_r_less(b, b + step)
      # `dir` is the comparison the loop keeps while it runs: `<` for a
      # forward range, `>` for a backward one. The one addition above
      # checks that the step moves the same way; if it does not, nothing
      # is yielded.
      if excl
        while __range_r_less(v, e) == dir
          yield v
          v = v + step
        end
      else
        while (c = __range_r_less(v, e)) == dir || c == 0
          yield v
          break if c == 0
          v = v + step
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

  # NOTE: `Range#initialize` is the Rust builtin (`range_initialize` in
  # builtins/range.rs): `Range.new` routes through the generic `Class#new`
  # (allocate + initialize), which really populates the RangeInner,
  # enforces the argument/comparability checks, rejects a second
  # initialize, and freezes direct Range instances.

  # Override the Rust `to_a` so endpoints that lack a fast path (e.g.
  # Symbol ranges, Float ranges, beginless) get routed through `each`,
  # producing the proper TypeError / RangeError instead of the bare
  # "not supported" runtime error.
  def to_a
    raise RangeError, "cannot convert endless range to an array" if self.end.nil?
    res = []
    each { |x| res << x }
    res
  end
  alias entries to_a

  def ==(other)
    return false unless other.is_a?(Range)
    return false unless self.exclude_end? == other.exclude_end?
    self.begin == other.begin && self.end == other.end
  end

  def eql?(other)
    return false unless other.is_a?(Range)
    return false unless self.exclude_end? == other.exclude_end?
    self.begin.eql?(other.begin) && self.end.eql?(other.end)
  end

  def hash
    [self.begin, self.end, self.exclude_end?].hash
  end

  def to_set(*args, &block)
    if self.end.nil?
      raise RangeError, "cannot convert endless range to a set"
    end
    if args.empty? && block.nil?
      ::Set.new(self)
    else
      # Ruby 4.0 deprecates passing arguments to Enumerable#to_set; emit
      # the same warning so callers see the migration notice. The first
      # positional argument is still treated as the set class.
      Kernel.warn("warning: passing arguments to Enumerable#to_set is deprecated")
      klass = args.shift || ::Set
      klass.new(self, *args, &block)
    end
  end

  # Ruby 3.4 semantics: only Integer-bounded ranges (and Integer..±Infinity)
  # are iterable; anything else either raises TypeError or returns nil.
  def size
    b = self.begin
    e = self.end
    excl = self.exclude_end?

    raise TypeError, "can't iterate from NilClass" if b.nil?
    raise TypeError, "can't iterate from Float" if b.is_a?(Float)

    if e.nil?
      return Float::INFINITY if b.is_a?(Integer)
      return nil if b.respond_to?(:succ)
      raise TypeError, "can't iterate from #{b.class}"
    end

    if b.is_a?(Integer)
      if e.is_a?(Integer)
        diff = e - b
        return 0 if diff < 0
        return excl ? diff : diff + 1
      end
      if e.is_a?(Float)
        if e.infinite?
          return e > 0 ? Float::INFINITY : 0
        end
        raise TypeError, "can't iterate from Float"
      end
      return nil if e.respond_to?(:succ)
      raise TypeError, "can't iterate from #{e.class}"
    end

    return nil if b.respond_to?(:succ)
    raise TypeError, "can't iterate from #{b.class}"
  end
end