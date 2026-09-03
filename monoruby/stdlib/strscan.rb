# frozen_string_literal: true
#
# StringScanner implementation for monoruby.
# Provides a simple lexical scanning interface for strings.
#
# Note: pos is always a byte offset, consistent with CRuby's StringScanner.
#
# Like CRuby's C strscan, the scanner keeps its match registers to
# itself: a scan stores only byte offsets (via the allocation-lean
# `String#__strscan_match` primitive, matched in place on the byte
# suffix at the scan position; a plain Fixnum for group-less patterns),
# never builds a MatchData, and leaves `$~` untouched. The
# accessors (`matched`, `[]`, `captures`, …) materialize strings lazily
# from the registers. Byte-oriented subjects with 8-bit content (whose
# engine view is a remapped copy) fall back to a copied rest-slice
# matched with `String#match`, whose MatchData then backs the same
# accessors.

class StringScanner
  class Error < StandardError; end

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
    # One character (up to 4 bytes of UTF-8), not one byte.
    ch = @str.byteslice(@pos, 4).chr
    @prev_pos = @pos
    @pos += ch.bytesize
    # A successful getch records the char as the whole match (CRuby).
    _clear_match
    @match_spans = ch.bytesize
    ch
  end

  def get_byte
    return nil if eos?
    byte = @str.byteslice(@pos, 1)
    @prev_pos = @pos
    @pos += 1
    _clear_match
    @match_spans = 1
    byte
  end
  alias getbyte get_byte

  def unscan
    raise Error, "unscan failed: previous match record not exist" unless matched?
    @pos = @prev_pos
    _clear_match
    self
  end

  # --- Match data ---
  #
  # On the register path @match_spans is either an Integer (the byte end
  # of a group-less whole match that starts at the scan origin
  # @prev_pos) or an Array of byte offset pairs relative to @prev_pos;
  # group contents are byteslices of @str. On the fallback path the
  # stored MatchData answers instead.

  def matched
    if @match_spans
      b = _match_begin
      @str.byteslice(@prev_pos + b, _match_end - b)
    elsif @match_md
      @match_md[0]
    end
  end

  def matched?
    !(@match_spans.nil? && @match_md.nil?)
  end

  def matched_size
    if @match_spans
      _match_end - _match_begin
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
        idx = Regexp === @match_re && @match_re.named_captures[name]&.last
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
      @str.byteslice(0, @prev_pos + _match_begin)
    elsif @match_md
      @str.byteslice(0, @pos - @match_md[0].bytesize)
    end
  end

  def post_match
    if @match_spans
      @str.byteslice(@prev_pos + _match_end..-1)
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

  # `\A`-anchored counterparts of caller Regexp patterns, built once per
  # distinct pattern instead of on every scan; identity-keyed (a regex
  # literal is the same object on every evaluation, so a lexer's fixed
  # pattern set hits after the first scan). String patterns never come
  # here: the primitive treats them as literal bytes (CRuby's C strscan
  # does too), and only the MatchData fallback needs their escaped
  # Regexp forms. All caches are size-capped so callers that generate
  # patterns dynamically cannot grow them without bound.
  ANCHORED_RE = {}.compare_by_identity
  ANCHORED_STR = {}
  PLAIN_STR = {}
  CACHE_LIMIT = 512
  private_constant :ANCHORED_RE, :ANCHORED_STR, :PLAIN_STR, :CACHE_LIMIT

  private

  def _clear_match
    @match_spans = @match_md = @match_re = nil
  end

  def _group_count
    Integer === @match_spans ? 1 : @match_spans.size / 2
  end

  # Group count across both representations (for `captures`).
  def _group_total
    @match_spans ? _group_count : @match_md.size
  end

  # Whole-match byte offsets relative to @prev_pos (register path).
  def _match_begin
    Integer === @match_spans ? 0 : @match_spans[0]
  end

  def _match_end
    Integer === @match_spans ? @match_spans : @match_spans[1]
  end

  # Both match paths hand the engine only the rest of the string, so
  # `\A` anchors at the scan position — CRuby strscan's default
  # (fixed_anchor: false) semantics.
  def _anchored(pattern)
    if pattern.is_a?(String)
      ANCHORED_STR.clear if ANCHORED_STR.size > CACHE_LIMIT
      ANCHORED_STR[pattern] ||= Regexp.new("\\A#{Regexp.escape(pattern)}")
    else
      unless pattern.is_a?(Regexp)
        raise TypeError, "no implicit conversion of #{pattern.class} into String"
      end
      ANCHORED_RE.clear if ANCHORED_RE.size > CACHE_LIMIT
      ANCHORED_RE[pattern] ||= Regexp.new("\\A(?:#{pattern.source})", pattern.options)
    end
  end

  # Match `pattern` at the scan position and store the registers.
  # Returns the byte length of the match, or nil.
  #
  # `__strscan_match` matches the byte suffix in place (a group-less hit
  # is a bare Fixnum) and answers `false` only for a subject it cannot
  # view in place, which takes the MatchData fallback. The engine sees
  # only the suffix, so the cached `\A(?:…)` form of a Regexp pattern
  # anchors at the scan position; a String pattern is a literal prefix.
  def _match_len_at_pos(pattern, advance)
    @prev_pos = @pos
    probe = pattern.is_a?(String) ? pattern : _anchored(pattern)
    spans = @str.__strscan_match(probe, @pos, true)
    return _fallback_at_pos(pattern, advance) if false == spans
    @match_md = nil
    @match_spans = spans
    return nil unless spans
    @match_re = pattern
    len = Integer === spans ? spans : spans[1]
    @pos += len if advance
    len
  end

  # Search `pattern` in the rest of the string and store the registers.
  # Returns the byte offset of the match END relative to the scan
  # position (== the number of bytes an advancing variant consumes), or
  # nil.
  def _match_forward_end(pattern, advance)
    @prev_pos = @pos
    spans = @str.__strscan_match(pattern, @pos, false)
    return _fallback_forward(pattern, advance) if false == spans
    @match_md = nil
    @match_spans = spans
    return nil unless spans
    @match_re = pattern
    end_pos = Integer === spans ? spans : spans[1]
    @pos += end_pos if advance
    end_pos
  end

  # MatchData fallbacks for subjects the primitive cannot match in
  # place: copy the rest and match it, so `\A` anchors at the scan
  # position.
  def _fallback_at_pos(pattern, advance)
    m = _fallback_match(_anchored(pattern))
    return nil unless m
    len = m[0].bytesize
    @pos += len if advance
    len
  end

  def _fallback_forward(pattern, advance)
    if pattern.is_a?(String)
      PLAIN_STR.clear if PLAIN_STR.size > CACHE_LIMIT
      pattern = PLAIN_STR[pattern] ||= Regexp.new(Regexp.escape(pattern))
    end
    m = _fallback_match(pattern)
    return nil unless m
    # Char == byte offsets on this path (a byte-oriented subject views
    # one char per byte).
    end_pos = m.end(0)
    @pos += end_pos if advance
    end_pos
  end

  def _fallback_match(re)
    rest_str = @str.byteslice(@pos..-1)
    m = rest_str&.match(re)
    @match_spans = @match_re = nil
    @match_md = m
  end
end
