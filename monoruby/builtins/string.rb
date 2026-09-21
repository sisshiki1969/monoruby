class String
  # CRuby's String is Comparable, and `between?` / `clamp` come from it —
  # they were simply missing here. The ordering operators stay native
  # (builtins/string.rs): a method defined on the class wins over the
  # module's, so including this adds the two methods without moving
  # `<` / `<=` / `>` / `>=` onto the slower `<=>`-based path.
  include Comparable

  # `rb_str_to_s`: a String is its own `to_s`, but a *subclass*
  # instance converts to a plain String.
  def to_s
    instance_of?(String) ? self : self[0, length]
  end
  alias to_str to_s

  def insert(index, other)
    index = index.is_a?(Integer) ? index : __to_int(index)
    # `rb_str_update` converts the replacement before it range-checks
    # the index, so an unconvertible `other` is a TypeError even when
    # the index is out of range.
    other = other.is_a?(String) ? other : __to_str(other)
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
    # `rb_str_concat_multi`: with more than one argument the parts are
    # gathered into a buffer first and appended in one go, so an
    # argument that *is* the receiver contributes the value it had on
    # entry — `str.concat str, str` triples it rather than quadrupling
    # it. The buffer carries the receiver's encoding so a codepoint
    # argument is interpreted as it would have been against `self`.
    #
    # The append goes through the hidden `__shl` primitive rather than
    # `<<`: a subclass may alias `<<` to `concat` and call `super` from
    # there (ActiveSupport::SafeBuffer), which would recurse forever.
    if args.size > 1
      buf = +""
      buf.force_encoding(encoding)
      args.each do |arg|
        buf.__shl(arg)
      end
      __shl(buf)
    elsif args.size == 1
      __shl(args[0])
    end
    self
  end

  def prepend(*args)
    return self if args.empty?
    # Build the whole head first: splicing one argument at a time
    # re-reads an argument that *is* the receiver after it has already
    # grown (`s.prepend(s, s)`).
    head = +""
    args.each { |arg| head << (arg.is_a?(String) ? arg : __to_str(arg)) }
    self[0, 0] = head
    self
  end

  def chop
    return "" if empty?
    # Compare code points, not bytes: in a fixed-width encoding a
    # terminator is several bytes wide, so `self[-1] == "\n"` (a UTF-8
    # literal) would never match.
    last = self[-1]
    if length > 1 && last.valid_encoding? && last.ord == 10
      prev = self[-2]
      return self[0..-3] if prev.valid_encoding? && prev.ord == 13
    end
    self[0..-2]
  end

  def chop!
    # A frozen receiver raises even when there is nothing to chop.
    raise FrozenError.new("can't modify frozen String: #{inspect}", receiver: self) if frozen?
    return nil if empty?
    result = chop
    replace(result)
    self
  end

  def delete_suffix(suffix)
    s = suffix.is_a?(String) ? suffix : __to_str(suffix)
    # `deleted_suffix_length` refuses a suffix that is broken in its
    # own encoding before it compares anything, so the bytes may match
    # (`end_with?` says so) and still not be deleted.
    return dup unless s.valid_encoding?
    if end_with?(s)
      self[0, length - s.length]
    else
      dup
    end
  end

  def delete_suffix!(suffix)
    # A frozen receiver raises whether or not the suffix matches.
    raise FrozenError.new("can't modify frozen String: #{inspect}", receiver: self) if frozen?
    s = suffix.is_a?(String) ? suffix : __to_str(suffix)
    # Deleting an empty suffix changes nothing, so the bang form
    # reports "no change" rather than returning self; a broken suffix
    # deletes nothing either (see `delete_suffix`).
    if !s.empty? && s.valid_encoding? && end_with?(s)
      result = self[0, length - s.length]
      replace(result)
      self
    else
      nil
    end
  end

  # The non-Regexp arm of `partition`; the Regexp arm is native (see
  # `partition_main` in builtins/string.rs), because the `$~` its search
  # sets has to land on the caller's frame, not on this method's.
  def __partition_str(sep)
    empty = "".dup.force_encoding(self.encoding)
    s = sep.is_a?(String) ? sep : __to_str(sep)
    i = index(s)
    if i
      [self[0, i], s, self[i + s.length..-1]]
    else
      [self[0, length], empty, empty.dup]
    end
  end

  # The non-Regexp arm of `rpartition`; see `__partition_str`.
  def __rpartition_str(sep)
    empty = "".dup.force_encoding(self.encoding)
    s = sep.is_a?(String) ? sep : __to_str(sep)
    i = rindex(s)
    if i
      [self[0, i], s, self[i + s.length..-1]]
    else
      [empty, empty.dup, self[0, length]]
    end
  end

  # `rb_str_each_byte`: the length is re-read every iteration, so a
  # block that shrinks the receiver stops early, as in CRuby. Written as
  # a plain loop rather than `bytes.each(&block)` — that built a
  # bytesize-element Array and a Proc per call, and the JIT inlines
  # `bytesize` / `getbyte` / `yield` here, which makes this about 3x
  # faster per byte.
  def each_byte
    return to_enum(:each_byte) { bytesize } unless block_given?
    i = 0
    while i < bytesize
      yield getbyte(i)
      i += 1
    end
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
          raise TypeError, "no implicit conversion of #{__builtin_class_name(max)} into String"
        end
      else
        raise TypeError, "no implicit conversion of #{__builtin_class_name(max)} into String"
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
    # CRuby's `rb_str_upto_each` walks until the current value equals
    # `max.succ`, not until it passes `max`, and stops as soon as the
    # successor outgrows `max` or comes back empty. Both guards matter
    # for an empty receiver: `"".succ` is `""`, so `"".upto("")` would
    # otherwise yield for ever (it is `[]` in CRuby, since `""` is
    # already `max.succ`), and `"".upto("a")` yields exactly once.
    n = (self <=> max)
    return self if n > 0 || (exclusive && n == 0)
    after_end = max.succ
    current = self
    while current != after_end
      # The successor is taken before the block runs, as CRuby does, so
      # a block that mutates the yielded string cannot steer the walk.
      nxt = (exclusive || current != max) ? current.succ : nil
      block.call(current)
      break if nxt.nil?
      current = nxt
      break if exclusive && current == max
      break if current.length > max.length || current.empty?
    end
    self
  end

  # +@ is a Rust builtin (it must detect chilled strings, which have
  # no Ruby-level predicate).
end
