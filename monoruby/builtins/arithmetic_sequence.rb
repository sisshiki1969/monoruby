# monoruby-hidden-ivars: true
#
# Enumerator::ArithmeticSequence is the value `Numeric#step` and
# `Range#step` (and `Range#%`) return when called without a block:
# a thin holder for `(begin, end, step, exclude_end?)` that
# behaves like an Enumerator over the sequence and is also
# accepted by `Array#[]` / `Array#slice` as a stride-aware index.
#
# We don't subclass Enumerator (its Fiber machinery only works
# for Enumerators built from a real iterator), so `is_a?` /
# `kind_of?` are overridden to keep `is_a?(Enumerator)` truthy
# for spec compatibility.
#
# Storage lives in a dedicated `RValue` variant
# (`ObjTy::ARITHMETIC_SEQUENCE`); ALL behavioural methods —
# `__build`, `begin`, `end`, `step`, `exclude_end?`,
# `size`, `first`, `last`, and `[]` — are Rust builtins (see
# `src/builtins/arithmetic_sequence.rs`). What remains here is
# the Enumerable include, the `is_a?` override (so `is_a?(Enumerator)`
# stays truthy without making AS a real Enumerator subclass), and
# inspect / to_a / each — Ruby-level conveniences that don't benefit from
# native code. The origin `#inspect` reports is kept in hidden instance
# variables (doc/hidden_ivars.md): CRuby keeps it in the C struct.
class Enumerator
  class ArithmeticSequence
    include Enumerable

    def is_a?(klass)
      return true if klass == Enumerator::ArithmeticSequence
      return true if klass == Enumerator
      return true if klass == Enumerable
      super
    end
    alias kind_of? is_a?

    # Records how the sequence was produced, which is all `#inspect`
    # reports: `1.step(10)` prints `(1.step(10))` while `(1..10).step`
    # prints `((1..10).step)` even though both describe 1, 2, … 10.
    # CRuby keeps the same three fields (`receiver`, `meth`, `arguments`)
    # on the object. Called by `Numeric#step` / `Range#step` / `Range#%`.
    def __set_origin(receiver, meth, args)
      @__receiver = receiver
      @__meth = meth
      @__args = args
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
      body = recv.is_a?(Range) ? "(#{recv.inspect})" : recv.inspect
      out = "(#{body}.#{@__meth}"
      args = @__args
      if args && !args.empty?
        out = out + "(#{args.map { |a| a.inspect }.join(', ')})"
      end
      __sprintf_repr(out + ")")
    end
    alias to_s inspect

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
    alias eql? ==

    def hash
      [Enumerator::ArithmeticSequence, self.begin, self.end, self.step, exclude_end?].hash
    end

    # `Enumerable#to_a` over `#each`, with a shortcut for the Integer
    # sequence, whose terms have a closed form: `Array.new(n) { ... }`
    # JIT-compiles to a tight loop. A Float sequence must go through
    # `#each`, which is what knows how its terms are counted and rounded.
    def to_a
      b = self.begin
      e = self.end
      s = self.step
      if b.is_a?(Integer) && e.is_a?(Integer) && s.is_a?(Integer)
        return Array.new(size) { |i| b + i * s }
      end
      raise RangeError, "cannot convert endless arithmetic sequence to array" if size == Float::INFINITY
      ary = []
      each { |v| ary << v }
      ary
    end
    alias entries to_a

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

    # In CRuby the sequence *is* an Enumerator, and `#each` without a
    # block answers the sequence itself; here it is a class of its own
    # (see the top of the file), so what a caller then asks of that
    # enumerator — external iteration, `with_index`, `+` — is forwarded
    # to a real one over `#each`.
    def next = __enum.next
    def peek = __enum.peek
    def next_values = __enum.next_values
    def peek_values = __enum.peek_values
    def feed(v) = __enum.feed(v)

    def rewind
      @__enum&.rewind
      self
    end

    def with_index(offset = 0, &block)
      return to_enum(:with_index, offset) { size } unless block
      to_enum(:each).with_index(offset, &block)
      self
    end

    def with_object(memo, &block)
      return to_enum(:with_object, memo) { size } unless block
      each { |v| block.call(v, memo) }
      memo
    end

    def +(other)
      Enumerator::Chain.new(self, other)
    end

    private

    def __enum
      @__enum ||= to_enum(:each) { size }
    end

    # CRuby's `num_idiv`: the floored quotient as an Integer.
    def __idiv(a, b)
      q = a.is_a?(Float) || a.is_a?(Rational) ? a / b : a.div(b)
      q.is_a?(Integer) ? q : q.floor
    end
  end
end
