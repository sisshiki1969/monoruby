class String
  def to_s
    self
  end
  alias to_str to_s

  def insert(index, other)
    index = index.is_a?(Integer) ? index : __to_int(index)
    if index < 0
      index = self.size + 1 + index
    end
    if index < 0 || index > self.size
      raise IndexError, "index #{index} out of string"
    end
    self[index, 0] = other
    self
  end

  def concat(*args)
    args.each do |arg|
      self << arg
    end
    self
  end
  alias append concat

  def prepend(*args)
    args.reverse_each do |arg|
      self[0, 0] = arg
    end
    self
  end

  def reverse
    chars.reverse.join
  end

  def reverse!
    replace(reverse)
  end

  def chop
    return "" if empty?
    if self[-1] == "\n" && length > 1 && self[-2] == "\r"
      self[0..-3]
    else
      self[0..-2]
    end
  end

  def chop!
    return nil if empty?
    result = chop
    replace(result)
    self
  end

  def delete_suffix(suffix)
    s = suffix.is_a?(String) ? suffix : __to_str(suffix)
    if end_with?(s)
      self[0, length - s.length]
    else
      dup
    end
  end

  def delete_suffix!(suffix)
    s = suffix.is_a?(String) ? suffix : __to_str(suffix)
    if end_with?(s)
      result = self[0, length - s.length]
      replace(result)
      self
    else
      nil
    end
  end

  def partition(sep)
    empty = "".dup.force_encoding(self.encoding)
    if sep.is_a?(Regexp)
      m = match(sep)
      if m
        [m.pre_match, m[0], m.post_match]
      else
        [self.dup, empty, empty.dup]
      end
    else
      s = sep.is_a?(String) ? sep : __to_str(sep)
      i = index(s)
      if i
        [self[0, i], s, self[i + s.length..-1]]
      else
        [self.dup, empty, empty.dup]
      end
    end
  end

  def rpartition(sep)
    empty = "".dup.force_encoding(self.encoding)
    if sep.is_a?(Regexp)
      # Find the last match
      last_match = nil
      pos = 0
      while (m = match(sep, pos))
        last_match = m
        pos = m.begin(0) + 1
        break if pos > length
      end
      if last_match
        [last_match.pre_match, last_match[0], last_match.post_match]
      else
        [empty, empty.dup, self.dup]
      end
    else
      s = sep.is_a?(String) ? sep : __to_str(sep)
      i = rindex(s)
      if i
        [self[0, i], s, self[i + s.length..-1]]
      else
        [empty, empty.dup, self.dup]
      end
    end
  end

  def each_byte(&block)
    return to_enum(:each_byte) unless block
    bytes.each(&block)
    self
  end

  # Empties the string in place. Encoding is preserved.
  def clear
    bytesplice(0, bytesize, "")
    self
  end

  def upto(max, exclusive = false, &block)
    # CRuby coerces `max` via `to_str`; anything else (Integer,
    # Symbol, an object without `to_str`) raises TypeError before
    # we look at length / `<=>`.
    unless max.is_a?(String)
      if max.respond_to?(:to_str)
        max = max.to_str
        unless max.is_a?(String)
          raise TypeError, "no implicit conversion of #{max.class} into String"
        end
      else
        raise TypeError, "no implicit conversion of #{max.class} into String"
      end
    end
    return to_enum(:upto, max, exclusive) unless block
    # Encoding compatibility: CRuby raises `Encoding::CompatibilityError`
    # when the receiver and `max` have incompatible encodings (matters
    # for non-ASCII content; pure-ASCII strings remain compatible
    # across all ASCII-compatible encodings).
    if Encoding.compatible?(self, max).nil?
      raise Encoding::CompatibilityError,
            "incompatible character encodings: #{self.encoding} and #{max.encoding}"
    end
    # Two CRuby special cases:
    #   1. Both ends are all-digit strings → iterate as integers
    #      (`"8".upto("11")` yields "8".."11"). Falls through to
    #      `Integer#upto` so we get the same iteration count even when
    #      the strings differ in length.
    #   2. Both ends are single ASCII characters → iterate by byte
    #      (`"9".upto("A")` yields "9", ":", ";", "<", "=", ">", "?",
    #      "@", "A"). The default `succ` would jump "9"→"10", which
    #      would never reach a single-char max.
    if !empty? && bytes.all? { |b| (48..57).cover?(b) } &&
       !max.empty? && max.bytes.all? { |b| (48..57).cover?(b) }
      from = self.to_i
      to = max.to_i
      width = self.length
      if exclusive
        from.upto(to - 1) { |i| block.call(i.to_s.rjust(width, "0")) }
      else
        from.upto(to) { |i| block.call(i.to_s.rjust(width, "0")) }
      end
      return self
    end
    if length == 1 && max.length == 1 && ascii_only? && max.ascii_only?
      from = bytes[0]
      to = max.bytes[0]
      stop = exclusive ? to - 1 : to
      (from..stop).each { |b| block.call(b.chr) }
      return self
    end
    current = self
    if exclusive
      while current < max && current.length <= max.length
        block.call(current)
        current = current.succ
      end
    else
      while (current <=> max) <= 0 && current.length <= max.length
        block.call(current)
        current = current.succ
      end
    end
    self
  end

  def codepoints
    each_codepoint.to_a
  end

  def each_codepoint(&block)
    return enum_for(:each_codepoint) unless block
    each_char { |c| block.call(c.ord) }
    self
  end

  def +@
    frozen? ? dup : self
  end
  def -@
    frozen? ? self : dup.freeze
  end
  alias_method :dedup, :-@

end
