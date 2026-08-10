class Hash
  include Enumerable

  def to_hash
    self
  end

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
    self.each {|k, v|
      pair = yield k, v
      pair = pair.to_ary if !pair.is_a?(Array) && pair.respond_to?(:to_ary)
      raise TypeError, "wrong element type #{pair.class} (expected array)" unless pair.is_a?(Array)
      raise ArgumentError, "element has wrong array length (expected 2, was #{pair.size})" unless pair.size == 2
      h[pair[0]] = pair[1]
    }
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
    # Capture every value `each` yields. A plain Hash yields a single
    # [k, v] pair, but a subclass may override `each` to `yield k, v` as
    # two separate values (or `yield [k, v]` as one) — normalise both.
    # An arity-1 block/proc sees the element as-is (the pair for a plain
    # Hash, the first value for a two-value yield); anything else has the
    # pair splatted, so a strict arity-2 block/Method receives k and v
    # (matches CRuby rb_yield_values2 / the enumerable map specs).
    each do |*vs|
      if blk.arity == 1
        result << blk.call(vs[0])
      else
        pair = vs.size == 1 ? vs[0] : vs
        result << blk.call(*pair)
      end
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
