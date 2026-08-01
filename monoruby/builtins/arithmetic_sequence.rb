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
# `__build`, `begin`, `end`, `step`, `exclude_end?`, `each`,
# `size`, `first`, `last`, and `[]` — are Rust builtins (see
# `src/builtins/arithmetic_sequence.rs`). What remains here is
# the Enumerable include, the `is_a?` override (so `is_a?(Enumerator)`
# stays truthy without making AS a real Enumerator subclass), and
# inspect / to_a — Ruby-level conveniences that don't benefit from
# native code.
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
        return "((#{lo}#{sep}#{hi}).step(#{step_part}))"
      end
      body = recv.is_a?(Range) ? "(#{recv.inspect})" : recv.inspect
      out = "(#{body}.#{@__meth}"
      args = @__args
      if args && !args.empty?
        out = out + "(#{args.map { |a| a.inspect }.join(', ')})"
      end
      out + ")"
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

    # `to_a` over the same closed-form `b + i * s` formula that `last`
    # and `first(n)` use. Stays in Ruby because `Array.new(n) { ... }`
    # already JIT-compiles to a tight loop and the savings from a
    # native rewrite are smaller than the maintenance cost.
    def to_a
      cnt = size
      raise RangeError, "cannot convert endless arithmetic sequence to array" if cnt.is_a?(Float)
      b = self.begin
      s = self.step
      Array.new(cnt) { |i| b + i * s }
    end
    alias entries to_a

    # `each` is implemented in Ruby (rather than Rust) so monoruby's
    # JIT can inline the block dispatch directly into the loop —
    # benchmarks show the JIT'd Ruby version beats the Rust+invoke_block
    # path. `size` is native (Rust), so the count itself is O(1).
    def each(&block)
      return self.to_enum(:each) { size } unless block
      b = self.begin
      s = self.step
      raise TypeError, "step can't be 0" if s == 0
      raise ArgumentError, "#each for beginless arithmetic sequences is meaningless" if b.nil?
      cnt = self.end.nil? ? Float::INFINITY : size
      if cnt.is_a?(Float) && cnt.infinite?
        # Endless / e == Float::INFINITY: loop until the caller breaks.
        i = 0
        loop do
          yield b + i * s
          i += 1
        end
      else
        i = 0
        while i < cnt
          yield b + i * s
          i += 1
        end
      end
      self
    end
  end
end
