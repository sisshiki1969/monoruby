# frozen_string_literal: true
#
# StringScanner implementation for monoruby.
# Provides a simple lexical scanning interface for strings.
#
# Note: pos is always a byte offset, consistent with CRuby's StringScanner.
#
# Like CRuby's C strscan, the scanner keeps its match registers to
# itself: a scan stores only byte offsets (via the allocation-lean
# `String#__strscan_match` primitive on the ASCII fast path — a plain
# Fixnum for group-less patterns), never builds a MatchData, and leaves
# `$~` untouched. The accessors (`matched`, `[]`, `captures`, …)
# materialize strings lazily from the registers. Non-ASCII subjects fall
# back to a copied rest-slice matched with `String#match`, whose
# MatchData then backs the same accessors.

class StringScanner
  def initialize(str)
    @str = str.is_a?(String) ? str : str.to_s
    @pos = 0
    @prev_pos = 0
    _clear_match
  end

  attr_reader :pos
  alias pointer pos

  def pos=(n)
    raise RangeError, "index out of range" if n < 0 || n > @str.bytesize
    @pos = n
  end
  alias pointer= pos=

  def string
    @str
  end

  def string=(str)
    @str = str
    @pos = 0
    _clear_match
  end

  def concat(str)
    @str << str
    self
  end
  alias << concat

  def reset
    @pos = 0
    _clear_match
    self
  end

  def terminate
    @pos = @str.bytesize
    _clear_match
    self
  end
  alias clear terminate

  def eos?
    @pos >= @str.bytesize
  end

  def rest?
    !eos?
  end

  def rest
    @str.byteslice(@pos..-1) || ""
  end

  def rest_size
    @str.bytesize - @pos
  end

  # --- Scanning methods ---

  def scan(pattern)
    len = _match_len_at_pos(pattern, true)
    len ? @str.byteslice(@pos - len, len) : nil
  end

  def skip(pattern)
    _match_len_at_pos(pattern, true)
  end

  def match?(pattern)
    _match_len_at_pos(pattern, false)
  end

  def check(pattern)
    len = _match_len_at_pos(pattern, false)
    len ? @str.byteslice(@pos, len) : nil
  end

  def scan_until(pattern)
    end_pos = _match_forward_end(pattern, true)
    end_pos ? @str.byteslice(@prev_pos, end_pos) : nil
  end

  def skip_until(pattern)
    _match_forward_end(pattern, true)
  end

  def check_until(pattern)
    end_pos = _match_forward_end(pattern, false)
    end_pos ? @str.byteslice(@pos, end_pos) : nil
  end

  def exist?(pattern)
    _match_forward_end(pattern, false)
  end

  def peek(len)
    @str.byteslice(@pos, len) || ""
  end
  alias peep peek

  def getch
    return nil if eos?
    ch = @str.byteslice(@pos, 1)
    @prev_pos = @pos
    @pos += 1
    # A successful getch records the char as the whole match (CRuby).
    _clear_match
    @match_spans = @match_end = ch.bytesize
    @match_begin = 0
    ch
  end

  def get_byte
    return nil if eos?
    byte = @str.byteslice(@pos, 1)
    @prev_pos = @pos
    @pos += 1
    _clear_match
    @match_spans = @match_end = 1
    @match_begin = 0
    byte
  end
  alias getbyte get_byte

  def unscan
    raise "unscan failed: previous match record not exist" unless matched?
    @pos = @prev_pos
    _clear_match
    self
  end

  # --- Match data ---
  #
  # On the register (spans) path, @match_begin/@match_end are byte
  # offsets of the whole match relative to the scan origin (@prev_pos),
  # and group contents are byteslices of @str. On the fallback path the
  # stored MatchData answers instead.

  def matched
    if @match_spans
      @str.byteslice(@prev_pos + @match_begin, @match_end - @match_begin)
    elsif @match_md
      @match_md[0]
    end
  end

  def matched?
    !(@match_spans.nil? && @match_md.nil?)
  end

  def matched_size
    if @match_spans
      @match_end - @match_begin
    elsif @match_md
      @match_md[0].bytesize
    end
  end

  def [](n)
    if @match_spans
      if n.is_a?(Integer)
        count = _group_count
        n += count if n < 0
        return nil if n < 0 || n >= count
        if Integer === @match_spans
          matched # count == 1, so n == 0: the whole match
        else
          b = @match_spans[2 * n]
          b ? @str.byteslice(@prev_pos + b, @match_spans[2 * n + 1] - b) : nil
        end
      elsif n.is_a?(String) || n.is_a?(Symbol)
        name = n.to_s
        idx = @match_re && @match_re.named_captures[name]&.last
        raise IndexError, "undefined group name reference: #{name}" unless idx
        self[idx]
      elsif n.respond_to?(:to_int)
        self[n.to_int]
      else
        raise TypeError, "no implicit conversion of #{n.class} into Integer"
      end
    elsif @match_md
      @match_md[n]
    end
  end

  def pre_match
    if @match_spans
      @str.byteslice(0, @prev_pos + @match_begin)
    elsif @match_md
      @str.byteslice(0, @pos - @match_md[0].bytesize)
    end
  end

  def post_match
    if @match_spans
      @str.byteslice(@prev_pos + @match_end..-1)
    elsif @match_md
      @str.byteslice(@pos..-1)
    end
  end

  # --- Misc ---

  def beginning_of_line?
    @pos == 0 || @str.byteslice(@pos - 1, 1) == "\n"
  end
  alias bol? beginning_of_line?

  def charpos
    @str.byteslice(0, @pos).length
  end

  def size
    if @match_spans
      _group_count
    elsif @match_md
      @match_md.size
    end
  end

  def captures
    return nil unless matched?
    (1..._group_total).map { |i| self[i] }
  end

  def values_at(*indices)
    return nil unless matched?
    indices.map { |i| self[i] }
  end

  def inspect
    if eos?
      "#<StringScanner fin>"
    else
      before = @pos > 5 ? "...#{@str.byteslice(@pos-5, 5).inspect}" : @str.byteslice(0, @pos).inspect
      after = rest_size > 5 ? "#{@str.byteslice(@pos, 5).inspect}..." : rest.inspect
      "#<StringScanner #{@pos}/#{@str.bytesize} #{before} @ #{after}>"
    end
  end

  def to_s
    matched.to_s
  end

  # `\A`-anchored counterparts of caller patterns, built once per
  # distinct pattern instead of on every scan. Regexp patterns are
  # identity-keyed (a regex literal is the same object on every
  # evaluation, so a lexer's fixed pattern set hits after the first
  # scan); String patterns are value-keyed via their escaped form
  # (CRuby's C strscan treats them as literal bytes). All caches are
  # size-capped so callers that generate patterns dynamically cannot
  # grow them without bound.
  ANCHORED_RE = {}.compare_by_identity
  ANCHORED_STR = {}
  PLAIN_STR = {}
  CACHE_LIMIT = 512
  private_constant :ANCHORED_RE, :ANCHORED_STR, :PLAIN_STR, :CACHE_LIMIT

  private

  def _clear_match
    @match_spans = @match_md = @match_re = @match_begin = @match_end = nil
  end

  def _group_count
    Integer === @match_spans ? 1 : @match_spans.size / 2
  end

  # Group count across both representations (for `captures`).
  def _group_total
    @match_spans ? _group_count : @match_md.size
  end

  # Both match paths hand the engine only the rest of the string, so
  # `\A` anchors at the scan position — CRuby strscan's default
  # (fixed_anchor: false) semantics.
  def _anchored(pattern)
    if pattern.is_a?(String)
      ANCHORED_STR.clear if ANCHORED_STR.size > CACHE_LIMIT
      ANCHORED_STR[pattern] ||= Regexp.new("\\A#{Regexp.escape(pattern)}")
    else
      ANCHORED_RE.clear if ANCHORED_RE.size > CACHE_LIMIT
      ANCHORED_RE[pattern] ||= Regexp.new("\\A(?:#{pattern.source})", pattern.options)
    end
  end

  # Match the anchored form of `pattern` at the scan position and store
  # the registers. Returns the byte length of the match, or nil.
  #
  # ASCII-only content (an O(1) check on the cached code range; byte and
  # char offsets coincide) matches the byte suffix in place via
  # `__strscan_match` — a group-less hit costs no allocation at all.
  # Everything else copies the rest and matches it, as before.
  def _match_len_at_pos(pattern, advance)
    anchored = _anchored(pattern)
    @prev_pos = @pos
    if @str.ascii_only?
      spans = @str.__strscan_match(anchored, @pos)
      @match_md = nil
      unless spans
        @match_spans = @match_re = @match_begin = @match_end = nil
        return nil
      end
      @match_spans = spans
      @match_re = anchored
      @match_begin = 0
      len = @match_end = (Integer === spans ? spans : spans[1])
    else
      rest_str = @str.byteslice(@pos..-1)
      m = rest_str&.match(anchored)
      @match_spans = @match_re = nil
      @match_md = m
      unless m
        @match_begin = @match_end = nil
        return nil
      end
      @match_begin = 0
      len = @match_end = m[0].bytesize
    end
    @pos += len if advance
    len
  end

  # Search `pattern` in the rest of the string and store the registers.
  # Returns the byte offset of the match END relative to the scan
  # position (== the number of bytes an advancing variant consumes), or
  # nil.
  def _match_forward_end(pattern, advance)
    if pattern.is_a?(String)
      PLAIN_STR.clear if PLAIN_STR.size > CACHE_LIMIT
      pattern = PLAIN_STR[pattern] ||= Regexp.new(Regexp.escape(pattern))
    end
    @prev_pos = @pos
    if @str.ascii_only?
      spans = @str.__strscan_match(pattern, @pos)
      @match_md = nil
      unless spans
        @match_spans = @match_re = @match_begin = @match_end = nil
        return nil
      end
      @match_spans = spans
      @match_re = pattern
      if Integer === spans
        @match_begin = 0
        end_pos = @match_end = spans
      else
        @match_begin = spans[0]
        end_pos = @match_end = spans[1]
      end
    else
      rest_str = @str.byteslice(@pos..-1)
      m = rest_str&.match(pattern)
      @match_spans = @match_re = nil
      @match_md = m
      unless m
        @match_begin = @match_end = nil
        return nil
      end
      end_pos = m.end(0)
      @match_begin = end_pos - m[0].bytesize
      @match_end = end_pos
    end
    @pos += end_pos if advance
    end_pos
  end
end
