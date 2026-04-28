class Struct
  include Enumerable

  # Number of attributes.
  def size
    members.size
  end
  alias length size

  # Yield each value.
  # Returns self when a block is given, an Enumerator (sized to size)
  # otherwise.
  def each
    return to_enum(:each) { size } unless block_given?
    members.each { |m| yield send(m) }
    self
  end

  # Yield [name, value] for each member.
  def each_pair
    return to_enum(:each_pair) { size } unless block_given?
    members.each { |m| yield m, send(m) }
    self
  end

  # Array of values, in member order.
  def to_a
    members.map { |m| send(m) }
  end
  alias values to_a
  alias deconstruct to_a

  # Hash of {member => value}. Block form mirrors Hash#to_h.
  def to_h
    return Hash[each_pair.to_a] unless block_given?
    h = {}
    each_pair do |k, v|
      pair = yield k, v
      pair = pair.to_ary if !pair.is_a?(Array) && pair.respond_to?(:to_ary)
      raise TypeError, "wrong element type #{pair.class} (expected Array)" unless pair.is_a?(Array)
      raise ArgumentError, "element has wrong array length (expected 2, was #{pair.size})" unless pair.size == 2
      h[pair[0]] = pair[1]
    end
    h
  end

  # Index access: by Integer, Symbol, or String. Negative indices count
  # from the end (matching CRuby's `rb_struct_aref`).
  def [](key)
    if key.is_a?(Integer)
      i = key
      i += size if i < 0
      raise IndexError, "offset #{key} too small for struct (size:#{size})" if i < 0
      raise IndexError, "offset #{key} too large for struct (size:#{size})" if i >= size
      send(members[i])
    elsif key.is_a?(Symbol) || key.is_a?(String)
      sym = key.to_sym
      raise NameError, "no member '#{key}' in struct" unless members.include?(sym)
      send(sym)
    else
      raise TypeError, "no implicit conversion of #{key.class} into Integer"
    end
  end

  def []=(key, value)
    raise FrozenError.new("can't modify frozen #{self.class}: #{inspect}", receiver: self) if frozen?
    if key.is_a?(Integer)
      i = key
      i += size if i < 0
      raise IndexError, "offset #{key} too small for struct (size:#{size})" if i < 0
      raise IndexError, "offset #{key} too large for struct (size:#{size})" if i >= size
      send("#{members[i]}=", value)
    elsif key.is_a?(Symbol) || key.is_a?(String)
      sym = key.to_sym
      raise NameError, "no member '#{key}' in struct" unless members.include?(sym)
      send("#{sym}=", value)
    else
      raise TypeError, "no implicit conversion of #{key.class} into Integer"
    end
  end

  def values_at(*indices)
    result = []
    indices.each do |idx|
      if idx.is_a?(Range)
        first = idx.begin
        last = idx.end
        first = 0 if first.nil?
        last = size - 1 if last.nil?
        first += size if first < 0
        raise RangeError, "#{idx} out of range" if first < 0
        last += size if last < 0
        last -= 1 if idx.exclude_end?
        (first..last).each do |i|
          result << (i < size ? send(members[i]) : nil)
        end
      else
        i = if idx.is_a?(Integer)
              idx
            elsif idx.respond_to?(:to_int)
              c = idx.to_int
              raise TypeError, "no implicit conversion of #{idx.class} into Integer" unless c.is_a?(Integer)
              c
            else
              raise TypeError, "no implicit conversion of #{idx.class} into Integer"
            end
        adj = i
        adj += size if adj < 0
        if adj < 0
          raise IndexError, "offset #{i} too small for struct(size:#{size})"
        elsif adj >= size
          raise IndexError, "offset #{i} too large for struct(size:#{size})"
        end
        result << send(members[adj])
      end
    end
    result
  end

  def dig(key, *rest)
    raise ArgumentError, "wrong number of arguments (given 0, expected 1+)" if key.nil? && rest.empty?
    val =
      if key.is_a?(Integer)
        idx = key
        idx += size if idx < 0
        if idx < 0 || idx >= size
          nil
        else
          send(members[idx])
        end
      elsif key.is_a?(Symbol) || key.is_a?(String)
        sym = key.to_sym
        members.include?(sym) ? send(sym) : nil
      else
        # Mirror Struct#[] coercion path; unsupported types raise TypeError.
        self[key]
      end
    return val if rest.empty? || val.nil?
    raise TypeError, "#{val.class} does not have #dig method" unless val.respond_to?(:dig)
    val.dig(*rest)
  end

  def deconstruct_keys(keys)
    return to_h if keys.nil?
    raise TypeError, "wrong argument type #{keys.class} (expected Array or nil)" unless keys.is_a?(Array)
    # CRuby drops the partial result entirely when the number of requested
    # keys exceeds the struct size.
    return {} if keys.size > size
    h = {}
    keys.each do |k|
      if k.is_a?(Symbol) || k.is_a?(String)
        sym = k.to_sym
        return h unless members.include?(sym)
        # Preserve the caller-supplied key (Symbol stays Symbol, String
        # stays String) per CRuby `rb_struct_deconstruct_keys`.
        h[k] = send(sym)
      else
        i = if k.is_a?(Integer)
              k
            elsif k.respond_to?(:to_int)
              c = k.to_int
              raise TypeError, "can't convert #{k.class} into Integer (#{k.class}#to_int gives #{c.class})" unless c.is_a?(Integer)
              c
            else
              raise TypeError, "no implicit conversion of #{k.class} into Integer"
            end
        idx = i
        idx += size if idx < 0
        return h if idx < 0 || idx >= size
        # Position numbers are returned AS the original key in the output.
        h[k] = send(members[idx])
      end
    end
    h
  end

  # Filter / select: Enumerable's version returns Array; Struct's spec
  # also expects an Array (not a Struct).
  def select(&block)
    return to_enum(:select) { size } unless block
    to_a.select(&block)
  end
  alias filter select
end
