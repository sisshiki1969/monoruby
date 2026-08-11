class Hash
  include Enumerable

  def to_hash
    self
  end

  # Hash#each / Hash#each_pair
  # each {|key, value| block } -> self
  # each -> Enumerator
  #
  # Written in Ruby, not Rust, so that a `h.each { .. }` call site can
  # inline it: the JIT only specializes `FuncKind::ISeq` callees, and
  # specializing `each` is what lets the block be inlined into the `yield`
  # in turn. A Rust builtin can never reach that, so its block pays a full
  # invocation per entry.
  #
  # Deleting during iteration is allowed and follows CRuby: while the
  # traversal is live a delete tombstones its entry in place instead of
  # compacting, so positions stay stable, a deleted not-yet-visited entry
  # is not yielded, and deleting the current or a visited one skips
  # nothing. An iteration reference is held across the yields, which is
  # what makes *adding* a key during iteration raise; a single
  # `[key, value]` array is yielded, so an arity-1 block receives the
  # pair (CRuby's `rb_yield(rb_assoc_new(k, v))`).
  def each
    return to_enum(:each) { size } unless block_given?
    guard = __iter_begin
    begin
      # Walk the live entry vector by position. A delete during the loop
      # tombstones its entry in place (positions stay stable, `__live_at`
      # turns false for it), so a deleted not-yet-visited entry is skipped
      # and deleting the current or an already-visited one skips nothing —
      # CRuby's semantics. `__entry_count` is the raw bound, tombstones
      # included; *adding* a key still raises, so it cannot grow mid-loop.
      i = 0
      while i < __entry_count
        yield [__key_at(i), __value_at(i)] if __live_at(i)
        i += 1
      end
    ensure
      __iter_end(guard)
    end
    self
  end

  alias each_pair each

  # Hash#to_h
  # to_h -> self
  # to_h {|key, value| block } -> Hash
  def to_h
    unless block_given?
      return self if self.instance_of?(Hash)
      h = {}
      # Set identity comparison BEFORE filling: `Hash[self]` would insert
      # under normal `eql?` comparison first, collapsing keys that are equal
      # but not identical, and turning it on afterwards cannot separate them
      # again. (`{"k" => 1}` and a distinct `"k" => 2` became one entry.)
      h.compare_by_identity if compare_by_identity?
      each { |k, v| h[k] = v }
      if (dp = default_proc)
        h.default_proc = dp
      else
        h.default = default
      end
      return h
    end
    h = {}
    # Iterate the entries here rather than through `each`. Going through
    # `each` costs two block entries per pair — `each`'s own block and then
    # this method's `yield` — where one is enough. It also lets the pair be
    # yielded as two values, which is what `to_h` does anyway (unlike `each`,
    # whose block receives a single `[k, v]`), so no array has to be built
    # per element at all.
    #
    # The traversal is the same live positional walk as `each` — deleting
    # during the block tombstones in place, so a deleted not-yet-visited
    # entry is skipped (CRuby) — and an iteration reference is held so that
    # *adding* a key raises. `ensure` balances the reference when the block
    # raises or breaks.
    guard = __iter_begin
    begin
      i = 0
      while i < __entry_count
        if __live_at(i)
          pair = yield __key_at(i), __value_at(i)
          pair = pair.to_ary if !pair.is_a?(Array) && pair.respond_to?(:to_ary)
          raise TypeError, "wrong element type #{pair.class} (expected array)" unless pair.is_a?(Array)
          raise ArgumentError, "element has wrong array length (expected 2, was #{pair.size})" unless pair.size == 2
          h[pair[0]] = pair[1]
        end
        i += 1
      end
    ensure
      __iter_end(guard)
    end
    h
  end

  def transform_keys(hash = nil, &block)
    return to_enum(:transform_keys) { size } unless block || hash
    h = {}
    if hash
      each do |k, v|
        new_k = hash.key?(k) ? hash[k] : (block ? block.call(k) : k)
        h[new_k] = v
      end
    else
      each { |k, v| h[block.call(k)] = v }
    end
    h
  end

  def transform_keys!(hash = nil, &block)
    return to_enum(:transform_keys!) { size } unless block || hash
    raise FrozenError.new("can't modify frozen Hash: #{inspect}", receiver: self) if frozen?
    # Snapshot the original pairs up front so a new key that collides with
    # a not-yet-processed original key can't corrupt its value, and track
    # the keys we produce so we never delete one we just created (e.g.
    # `{a:1,b:2}.transform_keys!(&:succ)` must not let `a`→`b` clobber the
    # original `b`). A `break` in the block exits mid-loop, leaving the
    # partial in-place result — matching CRuby.
    new_keys = {}
    new_keys.compare_by_identity if compare_by_identity?
    to_a.each do |k, v|
      nk = if hash&.key?(k)
        hash[k]
      elsif block
        block.call(k)
      else
        k
      end
      delete(k) unless new_keys.key?(k)
      self[nk] = v
      new_keys[nk] = true
    end
    self
  end

  def transform_values(&block)
    return to_enum(:transform_values) { size } unless block
    h = {}
    h.compare_by_identity if compare_by_identity?
    each { |k, v| h[k] = block.call(v) }
    h
  end

  def transform_values!(&block)
    return to_enum(:transform_values!) { size } unless block
    raise FrozenError.new("can't modify frozen Hash: #{inspect}", receiver: self) if frozen?
    each { |k, v| self[k] = block.call(v) }
    self
  end

  def slice(*keys)
    h = {}
    h.compare_by_identity if compare_by_identity?
    # CRuby's Hash#slice uses the internal element reference, not a
    # subclass-overridden #[], so bind the base Hash#[].
    aref = ::Hash.instance_method(:[])
    keys.each { |k| h[k] = aref.bind(self).call(k) if key?(k) }
    h
  end

  def except(*keys)
    h = dup
    h.default = nil
    keys.each { |k| h.delete(k) }
    h
  end

  def dig(key, *rest)
    val = self[key]
    return val if rest.empty? || val.nil?
    raise TypeError, "#{val.class} does not have #dig method" unless val.respond_to?(:dig)
    val.dig(*rest)
  end

  def each_with_object(obj)
    return to_enum(:each_with_object, obj) unless block_given?
    each { |k, v| yield [k, v], obj }
    obj
  end

  def any?(*pattern)
    if !pattern.empty?
      raise ArgumentError, "wrong number of arguments (given #{pattern.size}, expected 0..1)" if pattern.size != 1
      warn "warning: given block not used" if block_given?
      pat = pattern[0]
      each { |k, v| return true if pat === [k, v] }
    elsif block_given?
      each { |k, v| return true if yield([k, v]) }
    else
      return !empty?
    end
    false
  end

  def all?(*pattern)
    if !pattern.empty?
      raise ArgumentError, "wrong number of arguments (given #{pattern.size}, expected 0..1)" if pattern.size != 1
      warn "warning: given block not used" if block_given?
      pat = pattern[0]
      each { |k, v| return false unless pat === [k, v] }
    elsif block_given?
      each { |k, v| return false unless yield([k, v]) }
    else
      each { |k, v| return false unless [k, v] }
    end
    true
  end

  def count(*args)
    if block_given?
      n = 0
      each { |k, v| n += 1 if yield([k, v]) }
      n
    elsif args.empty?
      size
    else
      n = 0
      target = args[0]
      each { |k, v| n += 1 if [k, v] == target }
      n
    end
  end

  def map(&blk)
    return to_enum(:map) { size } unless blk
    result = []
    if method(:each).owner == ::Hash
      # Own `each` — not overridden by a subclass or a singleton method —
      # so every element is exactly one [k, v] pair, and the arity dispatch
      # can happen once out here, leaving plain blocks in the loops. That
      # matters for speed: a `|*vs|` rest parameter disqualifies a block
      # from the specialized-yield fast path (simple parameter lists are
      # bound in line by the JIT; rest params take the generic runtime
      # binding on every element).
      #
      # Whether the pair is passed whole or split in two follows CRuby
      # (each_pair_i_fast / rb_block_pair_yield_optimizable), where procs
      # and lambdas differ: a proc is split whenever it can take more than
      # one positional argument (`{ |a, *b| }` splits, bare `{ |*a| }`
      # receives the pair whole), a lambda — including a Symbol proc —
      # only when it *requires* at least two (`->(a, b, *c)` splits,
      # `->(a, *b)` receives the pair whole).
      ar = blk.arity
      wide = blk.lambda? ? (ar >= 2 || ar <= -3) : (ar > 1 || ar < -1)
      if wide
        each { |k, v| result << yield(k, v) }
      else
        each { |pair| result << yield(pair) }
      end
    else
      # Overridden `each`: pass whatever it yields straight through —
      # CRuby's Enumerable#map does no repacking. A proc auto-splats a
      # single-array yield across its parameters as any proc call would;
      # a lambda receives it as one argument and may raise. (The previous
      # arity-based normalization here re-split the element for every
      # non-arity-1 block, which wrongly let `->(k, v)` accept a
      # single-array yield and handed `{ |*a| }` the pair pre-splatted.)
      each { |*vs| result << blk.call(*vs) }
    end
    result
  end
  alias collect map

  def flat_map
    return to_enum(:flat_map) unless block_given?
    res = []
    each { |k, v|
      r = yield(k, v)
      if r.is_a?(Array)
        res.concat(r)
      else
        res << r
      end
    }
    res
  end

  def min_by
    return to_enum(:min_by) unless block_given?
    min_entry = nil
    min_val = nil
    each do |k, v|
      val = yield(k, v)
      if min_val.nil? || (val <=> min_val) < 0
        min_entry = [k, v]
        min_val = val
      end
    end
    min_entry
  end

  def has_value?(value)
    each_value { |v| return true if v == value }
    false
  end
  alias value? has_value?

  def deconstruct_keys(keys)
    self
  end

  def compact
    h = {}
    h.compare_by_identity if compare_by_identity?
    each { |k, v| h[k] = v unless v.nil? }
    if (dp = default_proc)
      h.default_proc = dp
    else
      h.default = default
    end
    h
  end

  def compact!
    raise FrozenError.new("can't modify frozen Hash: #{inspect}", receiver: self) if frozen?
    drop = []
    each { |k, v| drop << k if v.nil? }
    return nil if drop.empty?
    drop.each { |k| delete(k) }
    self
  end

  def flatten(level = 1)
    level = level.to_int if level.respond_to?(:to_int) && !level.is_a?(Integer)
    raise TypeError, "no implicit conversion of #{level.class} into Integer" unless level.is_a?(Integer)
    to_a.flatten(level)
  end

  def fetch_values(*keys, &block)
    keys.map { |k| fetch(k, &block) }
  end

  def to_proc
    hash = self
    ->(k) { hash[k] }
  end

  def rehash
    raise FrozenError.new("can't modify frozen Hash: #{inspect}", receiver: self) if frozen?
    pairs = to_a
    clear
    pairs.each { |k, v| self[k] = v }
    self
  end

end

# True alias required by ruby/spec's strict
# `Klass.instance_method(:a) == Klass.instance_method(:b)` identity checks
# (the 2026-06 "rely less on shared examples" refactoring wave). The
# aliased original must already be defined (natively or above) by now.

class Hash
  alias store []=
end
