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

  def squeeze(*args)
    if args.empty?
      gsub(/(.)\1+/, '\1')
    else
      chars_to_squeeze = args.join
      escaped = chars_to_squeeze.gsub(/[\\\[\]\-\^]/) { |c| "\\#{c}" }
      gsub(/([#{escaped}])\1+/, '\1')
    end
  end

  def squeeze!(*args)
    result = squeeze(*args)
    if result == self
      nil
    else
      replace(result)
      self
    end
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
    if sep.is_a?(Regexp)
      m = match(sep)
      if m
        [m.pre_match, m[0], m.post_match]
      else
        [self, "", ""]
      end
    else
      s = sep.is_a?(String) ? sep : __to_str(sep)
      i = index(s)
      if i
        [self[0, i], s, self[i + s.length..-1]]
      else
        [self, "", ""]
      end
    end
  end

  def rpartition(sep)
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
        ["", "", self]
      end
    else
      s = sep.is_a?(String) ? sep : __to_str(sep)
      i = rindex(s)
      if i
        [self[0, i], s, self[i + s.length..-1]]
      else
        ["", "", self]
      end
    end
  end

  def each_byte(&block)
    return to_enum(:each_byte) unless block
    bytes.each(&block)
    self
  end

  def upto(max, exclusive = false, &block)
    return to_enum(:upto, max, exclusive) unless block
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
    self
  end
  def -@
    self
  end

  def encode(*args, **opts)
    if opts[:xml] == :attr
      s = gsub("&", "&amp;")
      s = s.gsub("<", "&lt;")
      s = s.gsub(">", "&gt;")
      s = s.gsub("\"", "&quot;")
      "\"#{s}\""
    elsif opts[:xml] == :text
      s = gsub("&", "&amp;")
      s = s.gsub("<", "&lt;")
      s = s.gsub(">", "&gt;")
      s
    elsif args.empty?
      dup
    else
      dup.force_encoding(args[0])
    end
  end
end
