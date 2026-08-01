# Enumerator::Lazy — a chain of *pending* transformations over a source
# enumerable.
#
# Representation
# --------------
# A Lazy is a real Enumerator (ENUMERATOR-backed RValue, class
# `Enumerator::Lazy`), so every inherited method — `#each`, `#next`,
# `#peek`, `#rewind`, `#size` — works on it unchanged. Two shapes exist,
# mirroring CRuby:
#
#   * a *generator* stage (`Lazy.new`, and every operator below): the
#     Enumerator's source is a Generator whose body pulls from the
#     upstream stage and pushes transformed values into the Yielder;
#   * a *method* stage (`#to_enum` / `#enum_for`): the Enumerator wraps
#     `obj.meth(*args)` directly, so `each(&blk)` re-dispatches that call
#     with the caller's block (CRuby's `lazy_to_enum_i`).
#
# Since PR #1041/#1043 an Enumerator's `#each` runs its source on the
# *caller's* stack rather than in a fiber, so a chain is just nested
# block calls. That is what makes early exit work: `Enumerable#first` and
# `Lazy#take` simply `break` out of the upstream `each`, unwinding the
# whole chain, so an infinite source terminates.
#
# Value packing
# -------------
# Every stage receives the upstream yield's arguments raw (`|y, *vals|`)
# and emits exactly one value, so packing is decided per operator, as in
# CRuby:
#
#   * `map` / `flat_map` / `take_while` / `drop_while` call the user
#     block with the *initial* values (`blk.call(*vals)`);
#   * everything else calls it with the *gathered* value
#     (`rb_enum_values_pack`: nothing -> nil, one -> itself, several ->
#     Array).
class Enumerator
  class Lazy < Enumerator
    # `Lazy.new(obj, size = nil) { |yielder, *values| ... }`
    #
    # Builds the head of a chain. The block is invoked once per source
    # yield and decides what (if anything) reaches the yielder.
    def initialize(obj, size = nil, &block)
      raise ArgumentError, "tried to call lazy new without a block" unless block
      @__lazy_source = obj
      @__lazy_method = nil
      @__lazy_args = nil
      # `*args` are the ones `#each` / `#force` were called with; CRuby's
      # `lazy_init_block` forwards them to the source's `each` too.
      __enumerator_init__(size) do |y, *args|
        obj.each(*args) { |*vals| block.call(y, *vals) }
      end
      self
    end

    # ---------------------------------------------------------------
    # internal helpers
    # ---------------------------------------------------------------

    # `rb_enum_values_pack`.
    private def __lazy_pack(vals)
      if vals.size == 1
        vals[0]
      elsif vals.empty?
        nil
      else
        vals
      end
    end

    # Build the next stage. `meth`/`args` are recorded for `#inspect`
    # only; `gen` is the generator body and receives the Yielder.
    private def __lazy_stage(meth, args, size, &gen)
      lz = Lazy.allocate
      lz.instance_variable_set(:@__lazy_source, self)
      lz.instance_variable_set(:@__lazy_method, meth)
      lz.instance_variable_set(:@__lazy_args, args)
      lz.__send__(:__enumerator_init__, size, &gen)
      lz
    end

    # The common shape: one generator that walks the upstream stage and
    # hands each yield to `tr` together with the yielder.
    private def __lazy_step(meth, args, size, &tr)
      src = self
      __lazy_stage(meth, args, size) do |y|
        src.each { |*vals| tr.call(y, *vals) }
      end
    end

    private def __lazy_to_int(n)
      return n if n.is_a?(Integer)
      unless n.respond_to?(:to_int)
        raise TypeError, "no implicit conversion of #{n.class} into Integer"
      end
      i = n.to_int
      unless i.is_a?(Integer)
        raise TypeError, "can't convert #{n.class} to Integer (#{n.class}#to_int gives #{i.class})"
      end
      i
    end

    # ---------------------------------------------------------------
    # transformations
    # ---------------------------------------------------------------

    def map(&block)
      raise ArgumentError, "tried to call lazy map without a block" unless block
      src = self
      __lazy_step(:map, nil, -> { src.size }) { |y, *vals| y << block.call(*vals) }
    end
    alias collect map

    def flat_map(&block)
      raise ArgumentError, "tried to call lazy flat_map without a block" unless block
      __lazy_step(:flat_map, nil, nil) do |y, *vals|
        v = block.call(*vals)
        # CRuby flattens an Array, and anything that is itself lazy
        # (`respond_to?(:force) && respond_to?(:each)`), but leaves a
        # plain Enumerator alone.
        if v.is_a?(Array) || (v.respond_to?(:force) && v.respond_to?(:each))
          v.each { |e| y << e }
        else
          y << v
        end
      end
    end
    alias collect_concat flat_map

    def select(&block)
      raise ArgumentError, "tried to call lazy select without a block" unless block
      __lazy_step(:select, nil, nil) do |y, *vals|
        v = __lazy_pack(vals)
        y << v if block.call(v)
      end
    end
    alias filter select
    alias find_all select

    def reject(&block)
      raise ArgumentError, "tried to call lazy reject without a block" unless block
      __lazy_step(:reject, nil, nil) do |y, *vals|
        v = __lazy_pack(vals)
        y << v unless block.call(v)
      end
    end

    def filter_map(&block)
      raise ArgumentError, "tried to call lazy filter_map without a block" unless block
      __lazy_step(:filter_map, nil, nil) do |y, *vals|
        v = block.call(__lazy_pack(vals))
        y << v if v
      end
    end

    def grep(pattern, &block)
      __lazy_step(:grep, [pattern], nil) do |y, *vals|
        v = __lazy_pack(vals)
        if pattern === v
          y << (block ? block.call(v) : v)
        end
      end
    end

    def grep_v(pattern, &block)
      __lazy_step(:grep_v, [pattern], nil) do |y, *vals|
        v = __lazy_pack(vals)
        unless pattern === v
          y << (block ? block.call(v) : v)
        end
      end
    end

    def compact
      __lazy_step(:compact, nil, nil) do |y, *vals|
        v = __lazy_pack(vals)
        y << v unless v.nil?
      end
    end

    def uniq(&block)
      src = self
      __lazy_stage(:uniq, nil, nil) do |y|
        seen = {}
        src.each do |*vals|
          v = __lazy_pack(vals)
          key = block ? block.call(*vals) : v
          unless seen.key?(key)
            seen[key] = true
            y << v
          end
        end
      end
    end

    def take(n)
      n = __lazy_to_int(n)
      raise ArgumentError, "attempt to take negative size" if n < 0
      src = self
      # CRuby's `lazy_take_size`: an unknown (nil) upstream size stays
      # unknown, a smaller known size wins, anything else (including
      # Infinity) becomes `n`.
      size = lambda do
        sz = src.size
        if sz.nil? || (sz.is_a?(Integer) && sz < n)
          sz
        else
          n
        end
      end
      # `take(0)` must not touch the source at all.
      return __lazy_stage(:take, [n], size) { |_y| } if n == 0
      __lazy_stage(:take, [n], size) do |y|
        taken = 0
        src.each do |*vals|
          y << __lazy_pack(vals)
          taken += 1
          break if taken >= n
        end
      end
    end

    def take_while(&block)
      raise ArgumentError, "tried to call lazy take_while without a block" unless block
      src = self
      __lazy_stage(:take_while, nil, nil) do |y|
        src.each do |*vals|
          break unless block.call(*vals)
          y << __lazy_pack(vals)
        end
      end
    end

    def drop(n)
      n = __lazy_to_int(n)
      raise ArgumentError, "attempt to drop negative size" if n < 0
      src = self
      size = lambda do
        sz = src.size
        if sz.nil? || (sz.is_a?(Float) && sz.infinite?)
          sz
        else
          d = sz - n
          d < 0 ? 0 : d
        end
      end
      __lazy_stage(:drop, [n], size) do |y|
        seen = 0
        src.each do |*vals|
          y << __lazy_pack(vals) if seen >= n
          seen += 1
        end
      end
    end

    def drop_while(&block)
      raise ArgumentError, "tried to call lazy drop_while without a block" unless block
      src = self
      __lazy_stage(:drop_while, nil, nil) do |y|
        dropping = true
        src.each do |*vals|
          if dropping
            next if block.call(*vals)
            dropping = false
          end
          y << __lazy_pack(vals)
        end
      end
    end

    # `with_index(offset = 0)` — without a block the index is paired with
    # the value (`[value, index]`); with a block the block is called with
    # `(value, index)` and the *value* (not the block's result) is passed
    # on, as in CRuby's `lazy_with_index_proc`.
    def with_index(offset = 0, &block)
      offset = offset.nil? ? 0 : __lazy_to_int(offset)
      src = self
      __lazy_stage(:with_index, [offset], nil) do |y|
        i = offset
        src.each do |*vals|
          v = __lazy_pack(vals)
          if block
            block.call(v, i)
            y << v
          else
            y << [v, i]
          end
          i += 1
        end
      end
    end

    def zip(*others, &block)
      # With a block `zip` is not lazy at all — CRuby falls straight
      # through to `Enumerable#zip`.
      return super if block
      lists = others.map do |o|
        if o.respond_to?(:to_ary)
          o.to_ary
        elsif o.respond_to?(:each)
          o
        else
          raise TypeError, "wrong argument type #{o.class} (must respond to :each)"
        end
      end
      src = self
      __lazy_stage(:zip, others, -> { src.size }) do |y|
        # Arrays are indexed; anything else is pulled one element at a
        # time so an infinite companion works.
        cursors = lists.map { |o| o.is_a?(Array) ? o : o.to_enum(:each) }
        i = 0
        src.each do |*vals|
          row = [__lazy_pack(vals)]
          cursors.each do |c|
            if c.is_a?(Array)
              row << c[i]
            else
              begin
                row << c.next
              rescue StopIteration
                row << nil
              end
            end
          end
          y << row
          i += 1
        end
      end
    end

    # `chunk` / `chunk_while` / `slice_*` reuse the `Enumerable`
    # implementations (which stream through an Enumerator) and simply
    # re-wrap the result — CRuby's `lazy_super`.
    def chunk(*args, &block)
      super(*args, &block).lazy
    end

    def chunk_while(&block)
      super(&block).lazy
    end

    def slice_before(*args, &block)
      super(*args, &block).lazy
    end

    def slice_after(*args, &block)
      super(*args, &block).lazy
    end

    def slice_when(&block)
      super(&block).lazy
    end

    # ---------------------------------------------------------------
    # terminals / conversions
    # ---------------------------------------------------------------

    def lazy
      self
    end

    # A plain (non-lazy) Enumerator over the same chain. Declaring it
    # does not iterate anything.
    def eager
      src = self
      Enumerator.new(-> { src.size }) do |y|
        src.each { |*vals| y.yield(*vals) }
      end
    end

    def force(*args)
      to_a(*args)
    end

    # `to_enum(:map)` must enumerate — not build another pending stage —
    # so for the methods Lazy overrides it targets the *non-lazy*
    # implementation. CRuby keeps the same mapping in
    # `lazy_use_super_method`, pointing at private `_enumerable_*`
    # aliases of the original C functions; these forward to the same
    # place through the owning module's UnboundMethod.
    {
      map: Enumerable, collect: Enumerable, flat_map: Enumerable,
      collect_concat: Enumerable, select: Enumerable, filter: Enumerable,
      filter_map: Enumerable, find_all: Enumerable, reject: Enumerable,
      grep: Enumerable, grep_v: Enumerable, zip: Enumerable,
      take: Enumerable, take_while: Enumerable, drop: Enumerable,
      drop_while: Enumerable, uniq: Enumerable, compact: Enumerable,
      with_index: Enumerator, with_object: Enumerator
    }.each_pair do |name, owner|
      um = owner.instance_method(name)
      define_method(:"_enumerable_#{name}") do |*a, &b|
        um.bind(self).call(*a, &b)
      end
      private :"_enumerable_#{name}"
    end

    LAZY_SUPER_METHOD = {
      map: :_enumerable_map, collect: :_enumerable_collect,
      flat_map: :_enumerable_flat_map, collect_concat: :_enumerable_collect_concat,
      select: :_enumerable_select, filter: :_enumerable_filter,
      filter_map: :_enumerable_filter_map, find_all: :_enumerable_find_all,
      reject: :_enumerable_reject, grep: :_enumerable_grep,
      grep_v: :_enumerable_grep_v, zip: :_enumerable_zip,
      take: :_enumerable_take, take_while: :_enumerable_take_while,
      drop: :_enumerable_drop, drop_while: :_enumerable_drop_while,
      uniq: :_enumerable_uniq, compact: :_enumerable_compact,
      with_index: :_enumerable_with_index, with_object: :_enumerable_with_object
    }.freeze
    private_constant :LAZY_SUPER_METHOD

    # Unlike `Enumerable#to_enum`, this yields a *Lazy* wrapping
    # `self.meth(*args)`, so `each_slice`, `cycle`, `each_cons`, … stay
    # lazy when called on a Lazy without a block.
    def to_enum(meth = :each, *args, &size_block)
      meth = LAZY_SUPER_METHOD[meth] || meth
      lz = Lazy.allocate
      lz.instance_variable_set(:@__lazy_source, self)
      lz.instance_variable_set(:@__lazy_method, meth)
      lz.instance_variable_set(:@__lazy_args, args)
      lz.__send__(:__enum_init_method__, self, meth, args, size_block)
      lz
    end
    alias enum_for to_enum

    def with_object(obj, &block)
      return to_enum(:with_object, obj) unless block
      super
    end
    alias each_with_object with_object

    def inspect
      src = @__lazy_source
      return "#<#{self.class}: uninitialized>" if src.nil? && @__lazy_method.nil?
      s = "#<#{self.class}: #{src.inspect}"
      if @__lazy_method
        s = s + ":#{@__lazy_method}"
        args = @__lazy_args
        if args && !args.empty?
          s = s + "(#{args.map { |a| a.inspect }.join(', ')})"
        end
      end
      s + ">"
    end
    alias to_s inspect
  end
end
