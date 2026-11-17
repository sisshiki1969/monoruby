# monoruby-hidden-ivars: true
#
# Enumerator::ArithmeticSequence is the value `Numeric#step` and
# `Range#step` (and `Range#%`) return when called without a block:
# a thin holder for `(begin, end, step, exclude_end?)` that
# behaves like an Enumerator over the sequence and is also
# accepted by `Array#[]` / `Array#slice` as a stride-aware index.
#
# It is a subclass of Enumerator, as in CRuby, but not an Enumerator
# by representation: what Enumerator's own methods need (a source to
# call with a block, a Fiber for external iteration) is held by a real
# Enumerator the sequence builds on demand (`__enum`, `__source_enum`;
# see `Backing` in `src/builtins/enumerator.rs`).
#
# Storage lives in a dedicated `RValue` variant
# (`ObjTy::ARITHMETIC_SEQUENCE`); ALL behavioural methods —
# `__build`, `begin`, `end`, `step`, `exclude_end?`,
# `size`, `first`, `last` — are Rust builtins (see
# `src/builtins/arithmetic_sequence.rs`). What remains here is
# inspect / to_a / each — Ruby-level conveniences that don't benefit from
# native code. The origin `#inspect` reports is kept in hidden instance
# variables (doc/hidden_ivars.md): CRuby keeps it in the C struct.
class Enumerator
  class ArithmeticSequence
    # Records how the sequence was produced, which is all `#inspect`
    # reports: `1.step(10)` prints `(1.step(10))` while `(1..10).step`
    # prints `((1..10).step)` even though both describe 1, 2, … 10.
    # CRuby keeps the same three fields (`receiver`, `meth`, `arguments`)
    # on the object. Called by `Numeric#step` / `Range#step` / `Range#%`.
    def __set_origin(receiver, meth, args, kw = nil)
      @__receiver = receiver
      @__meth = meth
      @__args = args
      @__kw = kw
      self
    end

    # CRuby's `arith_seq_inspect`: `(<receiver>.<meth>(<args>))`, with the
    # receiver parenthesised when it is a Range.
    def inspect
      recv = @__receiver
      if recv.nil?
        # No recorded origin (an ArithmeticSequence built by some other
        # route): fall back to the canonical Range-step spelling.
        b = self.begin
        e = self.end
        s = self.step
        lo = b.nil? ? "" : b.inspect
        hi = e.nil? ? "" : e.inspect
        sep = exclude_end? ? "..." : ".."
        step_part = s.nil? ? "" : s.inspect
        return __sprintf_repr("((#{lo}#{sep}#{hi}).step(#{step_part}))")
      end
      # The receiver and the keyword values are shown by `to_s`
      # (`"%"PRIsVALUE`), the positional arguments by `inspect`:
      # `(1/1.step(to: 3, by: 2/1))` but `(1.step((3/1)))`.
      body = recv.is_a?(Range) ? "(#{recv})" : "#{recv}"
      out = "(#{body}.#{@__meth}"
      shown = (@__args || []).map { |a| a.inspect }
      # Keywords are shown as written, in the order written:
      # `(1.step(by: 2, to: 10))`, and `(1.step(to: ))` for a nil.
      @__kw&.each { |k, v| shown << "#{k}: #{v}" }
      out = out + "(#{shown.join(', ')})" unless shown.empty?
      __sprintf_repr(out + ")")
    end

    # Two sequences are equal when they describe the same progression —
    # how each was spelled (`1.step(10)` vs `(1..10).step`) is irrelevant.
    def ==(other)
      return true if equal?(other)
      return false unless other.is_a?(Enumerator::ArithmeticSequence)
      self.begin == other.begin &&
        self.end == other.end &&
        self.step == other.step &&
        exclude_end? == other.exclude_end?
    end
    alias === ==
    alias eql? ==

    def hash
      [Enumerator::ArithmeticSequence, self.begin, self.end, self.step, exclude_end?].hash
    end

    # The terms of an Integer sequence, which have a closed form
    # (`Array.new(n) { ... }` JIT-compiles to a tight loop), or nil for
    # any other sequence: a Float one must go through `#each`, which is
    # what knows how its terms are counted and rounded. `Enumerable#to_a`
    # asks this first.
    def __integer_terms
      b = self.begin
      e = self.end
      s = self.step
      if b.is_a?(Integer) && e.is_a?(Integer) && s.is_a?(Integer)
        Array.new(size) { |i| b + i * s }
      end
    end
    private :__integer_terms

    # CRuby's `arith_seq_each`, in Ruby (rather than Rust) so that
    # monoruby's JIT can inline the block dispatch directly into the
    # loop.
    #
    # A sequence with a Float anywhere — begin, end or step — is a Float
    # sequence: every term is the Float `i * step + begin`, counted by
    # `ruby_float_step_size` (`__float_step`), so `(1..3.0).step(1)`
    # yields `1.0, 2.0, 3.0` (#1990). Any other sequence is walked with
    # the numbers' own arithmetic, from begin by repeated addition up to
    # the last term `begin + step * ((end - begin).div(step))`.
    def each
      # `block_given?`, not a `&block` parameter: reading the parameter
      # would make the Proc. Without a block the sequence is its own
      # enumerator.
      return self unless block_given?
      if (fl = __float_step)
        beg, unit, last, n = fl
        if unit.infinite?
          # `i * unit + beg` would be NaN.
          yield beg if n > 0
        elsif unit == 0
          yield beg while true
        else
          # Each term is computed from its index (no accumulated error)
          # and clamped to the end, which a rounding error can overshoot.
          i = 0
          if unit >= 0
            while i < n
              d = i * unit + beg
              d = last if last < d
              yield d
              i += 1
            end
          else
            while i < n
              d = i * unit + beg
              d = last if d < last
              yield d
              i += 1
            end
          end
        end
        return self
      end
      c = self.begin
      e = self.end
      s = self.step
      if e.nil?
        if c.is_a?(Integer)
          while true
            yield c
            c += s
          end
        end
        # CRuby's `rb_int_plus` on a begin that is no Integer: the step
        # coerces it (`rb_num_coerce_bin`).
        while true
          yield c
          c = if c.is_a?(Integer)
                c + s
              else
                x, y = s.coerce(c)
                x + y
              end
        end
      end
      if s == 0
        yield c while true
      end
      last = c + s * __idiv(e - c, s)
      last -= s if exclude_end? && last == e
      if s < 0
        while c >= last
          yield c
          c += s
        end
      else
        while last >= c
          yield c
          c += s
        end
      end
      self
    end

    private

    # External iteration (`next`, `peek`, `rewind`, …, Enumerator's own)
    # runs over the sequence's `#each`.
    def __enum
      @__enum ||= to_enum(:each) { size }
    end

    # Enumerator's iterators (`with_index`, `with_object`,
    # `each_with_index`) call what made the sequence, with a block —
    # `1.step(10, 3) { … }`, `(..3).step(1) { … }` — as CRuby's do with
    # the receiver, method and arguments an Enumerator records.
    def __source_enum
      @__source_enum ||=
        if @__receiver.nil?
          to_enum(:each) { size }
        else
          seq = self
          @__receiver.to_enum(@__meth, *(@__args || []), **(@__kw || {})) { seq.size }
        end
    end

    # CRuby's `num_idiv`: the floored quotient as an Integer.
    def __idiv(a, b)
      q = a.is_a?(Float) || a.is_a?(Rational) ? a / b : a.div(b)
      q.is_a?(Integer) ? q : q.floor
    end
  end
end
