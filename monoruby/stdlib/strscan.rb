# frozen_string_literal: true
# monoruby-hidden-ivars: true
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
# accessors. A `fixed_anchor: true` scanner matches Regexps against the
# whole string from the scan position (so `\A` and `^` keep their
# string/line meaning) through the same MatchData accessors.
#
# This is the Ruby implementation the stub tree has always shipped:
# CRuby's ext/strscan built on the C API layer passes the same specs but
# costs a C-API crossing per call, which tokenizers (json, graphql,
# liquid) make millions of times; this one is JIT-inlined Ruby over one
# primitive call per scan.

class StringScanner
  # The strscan version CRuby 4.0.2 ships (rexml 3.4 branches on it).
  Version = "3.1.6"

  class Error < StandardError; end

  def self.must_C_version
    self
  end

  # CRuby's `rb_scan_args(argc, argv, "11")`: the optional second
  # argument is the options Hash (`fixed_anchor:` arrives here as one,
  # since this method takes no keywords) or CRuby's obsolete `dup` flag,
  # which is ignored.
  def initialize(str, options = nil)
    @__str = String === str ? str : _string_value(str)
    @__pos = 0
    @__prev_pos = 0
    _clear_match
    # Kept off the default path entirely: a fixed-anchor scanner gets
    # its own matchers from a singleton module, so an ordinary one pays
    # no flag test per scan and keeps its six hot ivars inline.
    extend FixedAnchor if Hash === options && _fixed_anchor_option(options)
  end

  def fixed_anchor?
    false
  end

  def pos
    @__pos
  end
  alias pointer pos

  def pos=(n)
    n = n.to_int
    n += @__str.bytesize if n < 0
    raise RangeError, "index out of range" if n < 0 || n > @__str.bytesize
    @__pos = n
  end
  alias pointer= pos=

  def string
    @__str
  end

  def string=(str)
    @__str = _string_value(str)
    @__pos = 0
    _clear_match
    str
  end

  def concat(str)
    @__str << _string_value(str)
    self
  end
  alias << concat

  def reset
    @__pos = 0
    _clear_match
    self
  end

  def terminate
    @__pos = @__str.bytesize
    _clear_match
    self
  end
  alias clear terminate

  def eos?
    @__pos >= @__str.bytesize
  end

  def rest?
    !eos?
  end

  def rest
    @__str.byteslice(@__pos..-1) || ""
  end

  def rest_size
    @__str.bytesize - @__pos
  end

  # --- Scanning methods ---

  def scan(pattern)
    len = _match_len_at_pos(pattern, true)
    len ? @__str.byteslice(@__pos - len, len) : nil
  end

  def skip(pattern)
    _match_len_at_pos(pattern, true)
  end

  def match?(pattern)
    _match_len_at_pos(pattern, false)
  end

  def check(pattern)
    len = _match_len_at_pos(pattern, false)
    len ? @__str.byteslice(@__pos, len) : nil
  end

  def scan_full(pattern, advance, return_string)
    len = _match_len_at_pos(pattern, advance)
    len && return_string ? @__str.byteslice(@__prev_pos, len) : len
  end

  def scan_until(pattern)
    end_pos = _match_forward_end(pattern, true)
    end_pos ? @__str.byteslice(@__prev_pos, end_pos) : nil
  end

  def skip_until(pattern)
    _match_forward_end(pattern, true)
  end

  def check_until(pattern)
    end_pos = _match_forward_end(pattern, false)
    end_pos ? @__str.byteslice(@__pos, end_pos) : nil
  end

  def exist?(pattern)
    _match_forward_end(pattern, false)
  end

  def search_full(pattern, advance, return_string)
    end_pos = _match_forward_end(pattern, advance)
    end_pos && return_string ? @__str.byteslice(@__prev_pos, end_pos) : end_pos
  end

  def peek(len)
    len = len.to_int
    return "" if eos?
    raise ArgumentError, "negative string size (or size too big)" if len < 0
    @__str.byteslice(@__pos, len)
  end
  alias peep peek

  def peek_byte
    @__str.getbyte(@__pos)
  end

  # The byte/char readers record what they read as the whole match,
  # with no pattern behind it (so a group name raises IndexError).
  def getch
    _clear_match
    return nil if eos?
    # One character (up to 4 bytes of UTF-8), not one byte.
    ch = @__str.byteslice(@__pos, 4).chr
    @__prev_pos = @__pos
    @__pos += ch.bytesize
    @__match_spans = ch.bytesize
    ch
  end

  def get_byte
    _clear_match
    return nil if eos?
    byte = @__str.byteslice(@__pos, 1)
    @__prev_pos = @__pos
    @__pos += 1
    @__match_spans = 1
    byte
  end
  alias getbyte get_byte

  def scan_byte
    _clear_match
    return nil if eos?
    byte = @__str.getbyte(@__pos)
    @__prev_pos = @__pos
    @__pos += 1
    @__match_spans = 1
    byte
  end

  # strscan 3.1.6's `scan_integer`: an optional sign and decimal digits,
  # or (base 16) an optional sign, an optional `0x` that is followed by
  # a hex digit, and hex digits. ASCII only, like CRuby's byte loop.
  INTEGER_RE = {
    10 => /[+-]?[0-9]+/,
    16 => /[+-]?(?:0x(?=[0-9a-fA-F]))?[0-9a-fA-F]+/,
  }.freeze
  private_constant :INTEGER_RE

  def scan_integer(base: 10)
    re = INTEGER_RE[base]
    unless re
      raise ArgumentError, "Unsupported integer base: #{base.inspect}, expected 10 or 16"
    end
    _clear_match
    unless @__str.encoding.ascii_compatible?
      raise Encoding::CompatibilityError, "ASCII incompatible encoding: #{@__str.encoding}"
    end
    len = _match_len_at_pos(re, true)
    @__match_re = nil
    len && @__str.byteslice(@__prev_pos, len).to_i(base)
  end

  def unscan
    raise Error, "unscan failed: previous match record not exist" unless matched?
    @__pos = @__prev_pos
    _clear_match
    self
  end

  # --- Match data ---
  #
  # On the register path @__match_spans is either an Integer (the byte end
  # of a group-less whole match that starts at the scan origin
  # @__prev_pos) or an Array of byte offset pairs relative to @__prev_pos;
  # group contents are byteslices of @__str. On the fallback path the
  # stored MatchData answers instead.

  def matched
    if @__match_spans
      b = _match_begin
      @__str.byteslice(@__prev_pos + b, _match_end - b)
    elsif @__match_md
      @__match_md[0]
    end
  end

  def matched?
    !(@__match_spans.nil? && @__match_md.nil?)
  end

  def matched_size
    if @__match_spans
      _match_end - _match_begin
    elsif @__match_md
      @__match_md[0].bytesize
    end
  end

  def [](n)
    if @__match_spans
      if n.is_a?(Integer)
        count = _group_count
        n += count if n < 0
        return nil if n < 0 || n >= count
        if @__match_spans.is_a?(Integer)
          matched # count == 1, so n == 0: the whole match
        else
          b = @__match_spans[2 * n]
          b ? @__str.byteslice(@__prev_pos + b, @__match_spans[2 * n + 1] - b) : nil
        end
      elsif n.is_a?(String) || n.is_a?(Symbol)
        name = n.to_s
        idx = Regexp === @__match_re && @__match_re.named_captures[name]&.last
        raise IndexError, "undefined group name reference: #{name}" unless idx
        self[idx]
      elsif n.respond_to?(:to_int)
        self[n.to_int]
      else
        raise TypeError, "no implicit conversion of #{n.class} into Integer"
      end
    elsif @__match_md
      @__match_md[n]
    end
  end

  def pre_match
    if @__match_spans
      @__str.byteslice(0, @__prev_pos + _match_begin)
    elsif @__match_md
      @__str.byteslice(0, _md_base + @__match_md.byteoffset(0)[0])
    end
  end

  def post_match
    if @__match_spans
      @__str.byteslice(@__prev_pos + _match_end..-1)
    elsif @__match_md
      @__str.byteslice(_md_base + @__match_md.byteoffset(0)[1]..-1)
    end
  end

  # --- Misc ---

  def beginning_of_line?
    @__pos == 0 || @__str.byteslice(@__pos - 1, 1) == "\n"
  end
  alias bol? beginning_of_line?

  def charpos
    @__str.byteslice(0, @__pos).length
  end

  def size
    if @__match_spans
      _group_count
    elsif @__match_md
      @__match_md.size
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

  # Name => the last non-nil group of that name, for the pattern of the
  # most recent match attempt (CRuby keeps it even when the attempt
  # failed, so every name maps to nil then).
  def named_captures
    re = @__match_re
    return {} unless Regexp === re
    h = {}
    re.named_captures.each do |name, indices|
      v = nil
      indices.each do |i|
        x = self[i]
        v = x unless x.nil?
      end
      h[name] = v
    end
    h
  end

  # CRuby's layout: up to 5 bytes either side of the scan position,
  # dumped as binary (`rb_str_new` + `rb_str_dump`).
  def inspect
    return "#<#{self.class} fin>" if eos?
    rest_part = rest_size > 5 ? @__str.byteslice(@__pos, 5).b << "..." : rest.b
    if @__pos == 0
      "#<#{self.class} #{@__pos}/#{@__str.bytesize} @ #{rest_part.dump}>"
    else
      before = @__pos > 5 ? "...".b << @__str.byteslice(@__pos - 5, 5).b : @__str.byteslice(0, @__pos).b
      "#<#{self.class} #{@__pos}/#{@__str.bytesize} #{before.dump} @ #{rest_part.dump}>"
    end
  end

  def to_s
    matched.to_s
  end

  # `\A`-anchored counterparts of caller patterns for the MatchData
  # fallback path only (the primitive anchors natively), built once per
  # distinct pattern; identity-keyed for Regexps, value-keyed via the
  # escaped form for Strings. All caches are size-capped so callers that
  # generate patterns dynamically cannot grow them without bound.
  ANCHORED_RE = {}.compare_by_identity
  ANCHORED_STR = {}
  PLAIN_STR = {}
  # `\G`-anchored counterparts for `fixed_anchor: true` scans.
  G_ANCHORED_RE = {}.compare_by_identity
  CACHE_LIMIT = 512
  private_constant :ANCHORED_RE, :ANCHORED_STR, :PLAIN_STR, :G_ANCHORED_RE, :CACHE_LIMIT

  private

  def _clear_match
    @__match_spans = @__match_md = @__match_re = nil
  end

  def _fixed_anchor_option(options)
    options.each_key do |k|
      raise ArgumentError, "unknown keyword: #{k.inspect}" unless k == :fixed_anchor
    end
    options[:fixed_anchor]
  end

  # CRuby's StringValue: a String, or what `to_str` makes of the object.
  def _string_value(obj)
    return obj if String === obj
    s = String.try_convert(obj)
    return s if s
    name = obj.nil? || obj == true || obj == false ? obj.inspect : obj.class
    raise TypeError, "no implicit conversion of #{name} into String"
  end

  def _group_count
    @__match_spans.is_a?(Integer) ? 1 : @__match_spans.size / 2
  end

  # Group count across both representations (for `captures`).
  def _group_total
    @__match_spans ? _group_count : @__match_md.size
  end

  # Whole-match byte offsets relative to @__prev_pos (register path).
  def _match_begin
    @__match_spans.is_a?(Integer) ? 0 : @__match_spans[0]
  end

  def _match_end
    @__match_spans.is_a?(Integer) ? @__match_spans : @__match_spans[1]
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
  # view in place, which takes the MatchData fallback. The primitive
  # anchors natively at the scan position (`onig_match` on the suffix);
  # a String pattern is a literal prefix.
  def _match_len_at_pos(pattern, advance)
    @__prev_pos = @__pos
    spans = @__str.__strscan_match(pattern, @__pos, true)
    return _fallback_at_pos(pattern, advance) if false == spans
    @__match_md = nil
    @__match_spans = spans
    @__match_re = pattern
    return nil unless spans
    len = spans.is_a?(Integer) ? spans : spans[1]
    @__pos += len if advance
    len
  end

  # Search `pattern` in the rest of the string and store the registers.
  # Returns the byte offset of the match END relative to the scan
  # position (== the number of bytes an advancing variant consumes), or
  # nil.
  def _match_forward_end(pattern, advance)
    @__prev_pos = @__pos
    spans = @__str.__strscan_match(pattern, @__pos, false)
    return _fallback_forward(pattern, advance) if false == spans
    @__match_md = nil
    @__match_spans = spans
    @__match_re = pattern
    return nil unless spans
    end_pos = spans.is_a?(Integer) ? spans : spans[1]
    @__pos += end_pos if advance
    end_pos
  end

  # MatchData fallbacks for subjects the primitive cannot match in
  # place: copy the rest and match it, so `\A` anchors at the scan
  # position.
  def _fallback_at_pos(pattern, advance)
    m = _fallback_match(_anchored(pattern), pattern)
    return nil unless m
    len = m[0].bytesize
    @__pos += len if advance
    len
  end

  def _fallback_forward(pattern, advance)
    orig = pattern
    if pattern.is_a?(String)
      PLAIN_STR.clear if PLAIN_STR.size > CACHE_LIMIT
      pattern = PLAIN_STR[pattern] ||= Regexp.new(Regexp.escape(pattern))
    end
    m = _fallback_match(pattern, orig)
    return nil unless m
    end_pos = m.byteoffset(0)[1]
    @__pos += end_pos if advance
    end_pos
  end

  def _fallback_match(re, orig)
    rest_str = @__str.byteslice(@__pos..-1)
    m = rest_str&.match(re)
    @__match_spans = nil
    @__match_re = orig
    @__match_md = m
  end

  # Where the MatchData's byte offsets start in @__str: the rest-slice
  # fallback matched from the scan origin.
  def _md_base
    @__prev_pos
  end


  # `fixed_anchor: true`: Regexps match against the whole string from the
  # scan position (so `\A` and `^` keep their string/line meaning); an
  # anchored scan pins the match to the position with `\G`. String
  # patterns are literal either way and take the ordinary path.
  module FixedAnchor
    def fixed_anchor?
      true
    end

    private

    def _md_base
      0
    end

    def _match_len_at_pos(pattern, advance)
      return super unless Regexp === pattern
      @__prev_pos = @__pos
      _fixed_match(pattern, true, advance)
    end

    def _match_forward_end(pattern, advance)
      return super unless Regexp === pattern
      @__prev_pos = @__pos
      _fixed_match(pattern, false, advance)
    end

    def _fixed_match(re, anchored, advance)
      if anchored
        G_ANCHORED_RE.clear if G_ANCHORED_RE.size > CACHE_LIMIT
        search = G_ANCHORED_RE[re] ||= Regexp.new("\\G(?:#{re.source})", re.options)
      else
        search = re
      end
      m = search.match(@__str, @__str.byteslice(0, @__pos).length)
      @__match_spans = nil
      @__match_re = re
      @__match_md = m
      return nil unless m
      end_pos = m.byteoffset(0)[1] - @__pos
      @__pos += end_pos if advance
      end_pos
    end
  end
  private_constant :FixedAnchor
end
