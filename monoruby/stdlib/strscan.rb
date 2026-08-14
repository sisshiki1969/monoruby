# frozen_string_literal: true
#
# StringScanner implementation for monoruby.
# Provides a simple lexical scanning interface for strings.
#
# Note: pos is always a byte offset, consistent with CRuby's StringScanner.

class StringScanner
  def initialize(str)
    @str = str.is_a?(String) ? str : str.to_s
    @pos = 0
    @match = nil
    @prev_pos = 0
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
    @match = nil
  end

  def concat(str)
    @str << str
    self
  end
  alias << concat

  def reset
    @pos = 0
    @match = nil
    self
  end

  def terminate
    @pos = @str.bytesize
    @match = nil
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
    _match_at_pos(pattern, true, true)
  end

  def scan_until(pattern)
    _match_forward(pattern, true, true)
  end

  def skip(pattern)
    result = _match_at_pos(pattern, true, false)
    result ? result.bytesize : nil
  end

  def skip_until(pattern)
    result = _match_forward(pattern, true, false)
    result ? @pos - @prev_pos : nil
  end

  def match?(pattern)
    result = _match_at_pos(pattern, false, false)
    result ? result.bytesize : nil
  end

  def check(pattern)
    _match_at_pos(pattern, false, true)
  end

  def check_until(pattern)
    _match_forward(pattern, false, true)
  end

  def exist?(pattern)
    _match_forward(pattern, false, false)
    @match ? @match.end(0) : nil
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
    @match = nil
    ch
  end

  def get_byte
    return nil if eos?
    byte = @str.byteslice(@pos, 1)
    @prev_pos = @pos
    @pos += 1
    @match = nil
    byte
  end
  alias getbyte get_byte

  def unscan
    raise "unscan failed: previous match record not exist" unless @match
    @pos = @prev_pos
    @match = nil
    self
  end

  # --- Match data ---

  def matched
    @match ? @match[0] : nil
  end

  def matched?
    !@match.nil?
  end

  def matched_size
    @match ? @match[0].bytesize : nil
  end

  def [](n)
    @match ? @match[n] : nil
  end

  def pre_match
    @match ? @str.byteslice(0, @pos - @match[0].bytesize) : nil
  end

  def post_match
    @match ? @str.byteslice(@pos..-1) : nil
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
    @match ? @match.size : nil
  end

  def captures
    return nil unless @match
    result = []
    (1...@match.size).each { |i| result << @match[i] }
    result
  end

  def values_at(*indices)
    return nil unless @match
    indices.map { |i| @match[i] }
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

  # Match `pattern` against the rest of the string. The MatchData's
  # offsets are relative to the scan position on both paths.
  #
  # ASCII-only content (an O(1) check on the cached code range; byte and
  # char offsets coincide) uses the zero-copy `__strscan_match`
  # primitive, which matches against the byte suffix in place and
  # snapshots the haystack as a CoW substring. Everything else copies
  # the rest, exactly as before.
  def _match_rest(pattern)
    if @str.ascii_only?
      @str.__strscan_match(pattern, @pos)
    else
      rest_str = @str.byteslice(@pos..-1)
      rest_str&.match(pattern)
    end
  end

  def _match_at_pos(pattern, advance, return_string)
    anchored = _anchored(pattern)
    @prev_pos = @pos
    m = _match_rest(anchored)
    if m
      @match = m
      matched_str = m[0]
      @pos += matched_str.bytesize if advance
      matched_str
    else
      @match = nil
      nil
    end
  end

  def _match_forward(pattern, advance, return_string)
    if pattern.is_a?(String)
      PLAIN_STR.clear if PLAIN_STR.size > CACHE_LIMIT
      pattern = PLAIN_STR[pattern] ||= Regexp.new(Regexp.escape(pattern))
    end
    @prev_pos = @pos
    m = _match_rest(pattern)
    if m
      @match = m
      end_pos = m.end(0) # relative to the scan position
      if advance
        @pos += end_pos
      end
      return_string ? @str.byteslice(@prev_pos, end_pos) : true
    else
      @match = nil
      nil
    end
  end
end
