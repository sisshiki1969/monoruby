# Psych / YAML stub for monoruby.
#
# Implements a subset of YAML 1.1 sufficient for config files and
# benchmark data: block mappings/sequences, flow collections, anchors
# (&name) and aliases (*name), and common scalar types.

module Psych
  VERSION = "5.0.0"
  LIBYAML_VERSION = "0.2.5"

  class SyntaxError < StandardError; end
  class DisallowedClass < StandardError
    def initialize(action, klass); super("Tried to #{action} unspecified class: #{klass}"); end
  end
  class BadAlias < StandardError; end
  class AliasesNotEnabled < StandardError; end

  @load_tags = {}
  @dump_tags = {}
  @domain_types = {}

  class << self
    attr_accessor :load_tags, :dump_tags, :domain_types
  end

  def self.add_tag(tag, klass)
    @load_tags[klass] = tag
    @dump_tags[tag] = klass
  end

  def self.remove_type(type_tag)
    @domain_types.delete(type_tag)
  end

  def self.add_domain_type(domain, type_tag)
    @domain_types[[domain, type_tag]] = nil
  end

  def self.load(yaml, permitted_classes: [], permitted_symbols: [], aliases: false, filename: nil, fallback: nil, symbolize_names: false, strict_integer: false, freeze: false)
    return fallback if yaml.nil? || yaml.empty?
    Parser.new(yaml).parse
  end

  def self.unsafe_load(yaml, filename: nil, fallback: nil, symbolize_names: false, strict_integer: false, freeze: false)
    return fallback if yaml.nil? || yaml.empty?
    Parser.new(yaml).parse
  end

  def self.safe_load(yaml, permitted_classes: [], permitted_symbols: [], aliases: false, filename: nil, fallback: nil, symbolize_names: false, strict_integer: false, freeze: false)
    load(yaml, permitted_classes: permitted_classes, fallback: fallback, symbolize_names: symbolize_names)
  end

  def self.load_file(filename, **opts)
    load(File.read(filename), filename: filename, **opts)
  end

  def self.safe_load_file(filename, **opts)
    safe_load(File.read(filename), filename: filename, **opts)
  end

  def self.unsafe_load_file(filename, **opts)
    unsafe_load(File.read(filename), filename: filename, **opts)
  end

  def self.dump(obj, io = nil, options = {})
    out = "--- #{dump_scalar(obj)}\n"
    if io
      io.write(out)
      io
    else
      out
    end
  end

  def self.dump_stream(*objects)
    objects.map { |o| dump(o) }.join
  end

  def self.dump_scalar(obj)
    case obj
    when nil then "~"
    when true, false then obj.to_s
    when Numeric then obj.to_s
    when Symbol then ":#{obj}"
    when String then obj.inspect
    when Array then "[" + obj.map { |e| dump_scalar(e) }.join(", ") + "]"
    when Hash then "{" + obj.map { |k, v| "#{dump_scalar(k)}: #{dump_scalar(v)}" }.join(", ") + "}"
    else obj.inspect
    end
  end

  # ------------------------------------------------------------------
  # Block-style YAML parser
  # ------------------------------------------------------------------
  class Parser
    def initialize(yaml)
      # `gsub` with a String pattern goes through the regexp engine and
      # scans the whole document; skip it when there is no CR at all
      # (`include?` is a plain substring search).
      yaml = yaml.gsub("\r\n", "\n") if yaml.include?("\r")
      @lines = yaml.split("\n")
      @pos = 0
      @anchors = {}
    end

    def parse
      skip_header
      parse_value(-1)
    end

    private

    def skip_header
      while @pos < @lines.size
        line = @lines[@pos]
        if line.start_with?("---") || line.start_with?("%")
          @pos += 1
        else
          break
        end
      end
    end

    def current_line
      @pos < @lines.size ? @lines[@pos] : nil
    end

    def indent_of(line)
      return -1 if line.nil?
      i = 0
      len = line.size
      while i < len
        b = line.getbyte(i)
        break unless b == 0x20 || b == 0x09
        i += 1
      end
      i
    end

    # `seq_at_parent` is set by a mapping whose key carried no inline
    # value: YAML's compact form lets the block sequence that is the
    # value sit at the key's own indentation (`x:\n- 1\n- 2`), so a
    # sequence entry there is the value, not the end of the node. A
    # sequence item never passes it — a same-indent `- ` after an empty
    # item is the next sibling.
    def parse_value(parent_indent, seq_at_parent = false)
      skip_blanks_and_comments
      line = current_line
      return nil if line.nil?

      ind = indent_of(line)
      stripped = line.strip
      if parent_indent >= 0 && ind <= parent_indent
        if seq_at_parent && ind == parent_indent &&
           (stripped.start_with?("- ") || stripped == "-")
          return parse_block_sequence(ind)
        end
        return nil
      end

      if stripped.start_with?("- ")
        return parse_block_sequence(ind)
      end

      if stripped == "-"
        return parse_block_sequence(ind)
      end

      if stripped.start_with?("{")
        @pos += 1 if block_mapping_line?(line)
        return parse_flow_mapping(stripped)
      end

      if stripped.start_with?("[")
        @pos += 1 if block_mapping_line?(line)
        return parse_flow_sequence(stripped)
      end

      if block_mapping_line?(line)
        return parse_block_mapping(ind)
      end

      @pos += 1
      parts = [stripped]
      while @pos < @lines.size
        nxt = @lines[@pos]
        break if nxt.strip.empty?
        nxt_ind = indent_of(nxt)
        break if nxt_ind < ind
        break if block_mapping_line?(nxt)
        break if nxt.strip.start_with?("- ")
        parts << nxt.strip
        @pos += 1
      end
      resolve_scalar(parts.join("\n"))
    end

    def block_mapping_line?(line)
      # Byte scan via `getbyte`: no per-line Integer array.
      len = line.bytesize
      i = 0
      while i < len && (line.getbyte(i) == 0x20 || line.getbyte(i) == 0x09)
        i += 1
      end
      return false if i == len
      first = line.getbyte(i)
      return false if first == 0x23 # '#'
      return false if first == 0x2D && i + 1 < len && line.getbyte(i + 1) == 0x20 # "- "
      # A block-mapping line is a key followed by ':' that is either
      # at end of line or followed by whitespace. Strings escape the
      # match (so we have to skip past balanced "..." or '...').
      while i < len
        b = line.getbyte(i)
        case b
        when 0x22 # '"'
          i += 1
          while i < len && line.getbyte(i) != 0x22
            i += 1
          end
          i += 1 # past closing "
        when 0x27 # "'"
          i += 1
          while i < len && line.getbyte(i) != 0x27
            i += 1
          end
          i += 1 # past closing '
        when 0x23 # '#'  inline comment ⇒ no mapping
          return false
        when 0x3A # ':'
          # ':' followed by whitespace or end-of-line is a mapping
          # separator. Anything else (e.g. ::, :foo) is not.
          if i + 1 == len
            return true
          end
          nx = line.getbyte(i + 1)
          return true if nx == 0x20 || nx == 0x09
          i += 1
        else
          i += 1
        end
      end
      false
    end

    def parse_block_mapping(base_indent)
      map = {}
      while (line = current_line)
        skip_blanks_and_comments
        line = current_line
        break if line.nil?
        ind = indent_of(line)
        break if ind < base_indent
        break if ind > base_indent && map.size > 0

        stripped = line.strip
        break unless block_mapping_line?(line)

        key, rest = split_mapping_key(stripped)
        @pos += 1

        anchor = nil
        # Anchored keys (`&name key: v`) are rare; only run the regexp
        # when the key actually starts with '&'.
        if key.getbyte(0) == 0x26 && key =~ /\A&(\S+)\s+(.*)/
          anchor = $1
          key = $2
        end

        key = resolve_scalar(key)

        if rest.nil? || rest.empty? || rest.start_with?("#")
          # `key: # comment` carries no inline value; the value is on
          # the following indented line(s) — or a block sequence at the
          # key's own indentation (`seq_at_parent`).
          value = parse_value(ind, true)
        elsif rest.start_with?("*")
          alias_name = rest[1..-1].strip
          value = @anchors[alias_name]
        elsif rest.start_with?("&")
          if rest =~ /\A&(\S+)\s*(.*)/
            val_anchor = $1
            val_rest = $2
            if val_rest.empty?
              value = parse_value(ind, true)
            else
              value = resolve_scalar(val_rest)
            end
            @anchors[val_anchor] = value
          else
            value = resolve_scalar(rest)
          end
        elsif rest.start_with?("{")
          value = parse_flow_mapping(rest)
        elsif rest.start_with?("[")
          value = parse_flow_sequence(rest)
        elsif rest =~ /\A([|>])(\d*)([+-]?)\z/
          block_type = $1
          explicit_n = $2.empty? ? nil : $2.to_i
          chomp = $3 # "" (clip), "-" (strip), or "+" (keep)
          if block_type == "|"
            value = parse_literal_block(ind, explicit_n, chomp)
          else
            value = parse_folded_block(ind, explicit_n, chomp)
          end
        else
          # YAML allows "..." / '...' values to span multiple lines; the
          # closing quote may be on a later line. Pull continuation
          # lines into `rest` (folding line breaks into single spaces
          # per YAML 1.1) before handing off to resolve_scalar.
          if (rest.start_with?('"') || rest.start_with?("'")) &&
             !quoted_string_terminated?(rest, rest.getbyte(0))
            rest = consume_multiline_quoted(rest)
          end
          value = resolve_scalar(rest)
        end

        if anchor
          @anchors[anchor] = value
        end

        map[key] = value
      end
      map
    end

    def parse_block_sequence(base_indent)
      arr = []
      while (line = current_line)
        skip_blanks_and_comments
        line = current_line
        break if line.nil?
        ind = indent_of(line)
        break if ind < base_indent
        break if ind > base_indent && base_indent >= 0 && arr.size > 0

        stripped = line.strip
        break unless stripped.start_with?("-")

        after_dash = stripped[1..-1]

        if after_dash.nil? || after_dash.strip.empty?
          @pos += 1
          value = parse_value(ind)
        elsif after_dash =~ /\A\s+\*(\S+)\s*\z/
          @pos += 1
          alias_name = $1
          value = @anchors[alias_name]
        elsif after_dash =~ /\A\s+&(\S+)\s*(.*)/
          anchor = $1
          rest = $2.strip
          @pos += 1
          if rest.empty?
            value = parse_value(ind)
          else
            value = resolve_scalar(rest)
          end
          @anchors[anchor] = value
        elsif after_dash =~ /\A(\s+)(.*)/
          item_text = $2
          # The item's own indentation is where its text starts: past the
          # dash and the whitespace run (`-  a: 1` puts `a` at column
          # ind + 3). Continuation lines of a nested node align to it.
          inner_indent = ind + 1 + $1.size
          # Flow collections first: `- {a: 1}` contains a `: ` and would
          # otherwise be taken for a block mapping with the key `{a`.
          if item_text.start_with?("{")
            @pos += 1
            value = parse_flow_mapping(item_text)
          elsif item_text.start_with?("[")
            @pos += 1
            value = parse_flow_sequence(item_text)
          elsif block_mapping_line?(" " * inner_indent + item_text)
            fake_line = " " * inner_indent + item_text
            @lines[@pos] = fake_line
            value = parse_block_mapping(inner_indent)
          elsif item_text.start_with?("- ") || item_text == "-"
            # A nested sequence opened on the dash line (`- - a`): reparse
            # the line as its first entry at the inner indentation.
            fake_line = " " * inner_indent + item_text
            @lines[@pos] = fake_line
            value = parse_block_sequence(inner_indent)
          else
            @pos += 1
            value = resolve_scalar(item_text)
          end
        else
          @pos += 1
          value = nil
        end

        arr << value
      end
      arr
    end

    def split_mapping_key(stripped)
      # For quoted keys, keep the quotes so that resolve_scalar
      # treats `"1"` as the string "1" rather than the integer 1.
      if stripped.start_with?('"')
        if stripped =~ /\A("(?:[^"\\]|\\.)*")\s*:\s*(.*)\z/
          return [$1, $2]
        end
      elsif stripped.start_with?("'")
        if stripped =~ /\A('[^']*')\s*:\s*(.*)\z/
          return [$1, $2]
        end
      end
      # The unquoted case, by byte scan — the two lazy-`.*?` regexps
      # it replaces (`/\A(.*?)\s*:\s+(.*)\z/`, `/\A(.*?)\s*:\z/`) ran
      # per mapping line and were the single largest cost of a load.
      # Same matching: the first ':' followed by whitespace splits
      # (key rstripped, value past the whitespace run); a ':' that is
      # the last byte yields a nil value. The indices are byte offsets,
      # so slice with `byteslice`: `String#[]` counts characters and
      # would cut a non-ASCII key (`日本: 1`) in the wrong place.
      i = 0
      len = stripped.bytesize
      while i < len
        if stripped.getbyte(i) == 0x3A # ':'
          if i + 1 == len
            return [stripped.byteslice(0, i).rstrip, nil]
          end
          nx = stripped.getbyte(i + 1)
          if nx == 0x20 || nx == 0x09
            j = i + 1
            while j < len && ((b = stripped.getbyte(j)) == 0x20 || b == 0x09)
              j += 1
            end
            return [stripped.byteslice(0, i).rstrip, stripped.byteslice(j, len - j)]
          end
        end
        i += 1
      end
      [stripped, nil]
    end

    # True iff the quoted scalar `s` (which starts with `quote`) has a
    # matching unescaped closing `quote` on the same line. Mirrors
    # YAML 1.1 escapes: `\` consumes the next byte inside `"..."` and
    # `''` is the in-string escape for `'` inside `'...'`.
    def quoted_string_terminated?(s, quote)
      return false if s.bytesize < 2
      i = 1
      len = s.bytesize
      while i < len
        c = s.getbyte(i)
        if c == quote
          if quote == 0x27 && i + 1 < len && s.getbyte(i + 1) == 0x27
            i += 2
            next
          end
          return true
        end
        if quote == 0x22 && c == 0x5C # backslash escape inside "..."
          i += 2
          next
        end
        i += 1
      end
      false
    end

    # `rest` starts with `"` / `'` whose closing quote is on a later
    # line. Pull continuation lines into a single buffer, folding the
    # joining line break into a single space (YAML 1.1 rule for
    # double/single-quoted multi-line scalars on contiguous non-blank
    # lines), and advance @pos past the consumed lines.
    def consume_multiline_quoted(rest)
      quote = rest.getbyte(0)
      buf = rest.dup
      while @pos < @lines.size
        nxt = @lines[@pos]
        @pos += 1
        buf = buf.rstrip + " " + nxt.lstrip
        break if quoted_string_terminated?(buf, quote)
      end
      buf
    end

    # `chomp` is one of "" (clip — single trailing newline), "-"
    # (strip — no trailing newline), "+" (keep — preserve all
    # trailing newlines). `explicit_indent`, when given, fixes the
    # content indent at parent_indent + explicit_indent (e.g. `|2-`).
    def parse_literal_block(parent_indent, explicit_indent, chomp)
      content_indent = explicit_indent ? parent_indent + explicit_indent : parent_indent + 2
      lines = []
      while @pos < @lines.size
        line = @lines[@pos]
        if line.strip.empty?
          lines << ""
          @pos += 1
          next
        end
        ind = indent_of(line)
        break if ind <= parent_indent
        lines << line[content_indent..-1].to_s
        @pos += 1
      end
      # Pop trailing empty lines (the chomp indicator decides what to
      # do with them).
      trailing = 0
      while !lines.empty? && lines.last.empty?
        lines.pop
        trailing += 1
      end
      body = lines.join("\n")
      apply_chomp(body, trailing, chomp)
    end

    def parse_folded_block(parent_indent, explicit_indent, chomp)
      content_indent = explicit_indent ? parent_indent + explicit_indent : parent_indent + 2
      lines = []
      while @pos < @lines.size
        line = @lines[@pos]
        if line.strip.empty?
          lines << ""
          @pos += 1
          next
        end
        ind = indent_of(line)
        break if ind <= parent_indent
        lines << line[content_indent..-1].to_s
        @pos += 1
      end
      trailing = 0
      while !lines.empty? && lines.last.empty?
        lines.pop
        trailing += 1
      end
      # Folded: line breaks fold to a single space (collapsing any
      # double-spaces that arise from joining).
      body = lines.join(" ").gsub("  ", " ")
      apply_chomp(body, trailing, chomp)
    end

    # Block-scalar chomp indicator:
    #   ""  (clip)  -> exactly one trailing newline
    #   "-" (strip) -> no trailing newlines
    #   "+" (keep)  -> the original trailing-empty-line count, plus the
    #                   final block-end newline
    def apply_chomp(body, trailing, chomp)
      case chomp
      when "-" then body
      when "+" then body + ("\n" * (trailing + 1))
      else          body + "\n"
      end
    end

    def parse_flow_mapping(str)
      str = str.strip
      str = str[1..-1] if str.start_with?("{")
      str = str[0..-2] if str.end_with?("}")
      map = {}
      str.strip.split(/,\s*/).each do |pair|
        next if pair.strip.empty?
        if pair =~ /\A\s*(.*?)\s*:\s*(.*)\z/
          k = resolve_scalar($1)
          v = resolve_scalar($2)
          map[k] = v
        end
      end
      map
    end

    def parse_flow_sequence(str)
      str = str.strip
      str = str[1..-1] if str.start_with?("[")
      str = str[0..-2] if str.end_with?("]")
      str.strip.split(/,\s*/).map { |item| resolve_scalar(item.strip) }.reject { |x| x.is_a?(String) && x.empty? }
    end

    def resolve_scalar(str)
      return nil if str.nil?
      str = str.strip
      return nil if str.empty?

      # Most YAML scalars are plain strings. Dispatch on the first
      # byte to skip the per-call regex/case scan whenever possible.
      first = str.getbyte(0)
      # Cheap leading-comment trim, but only for unquoted plain
      # scalars (quoted strings may legally contain " #").
      if first != 0x22 && first != 0x27 && (idx = str.index(" #"))
        str = str[0, idx].rstrip
        return nil if str.empty?
        first = str.getbyte(0)
      end

      case first
      when 0x22 # "..."  double-quoted
        if str.end_with?('"') && str.size >= 2
          inner = str[1..-2]
          # Cheap path: no escape sequences ⇒ return as-is.
          if inner.include?("\\")
            return inner.gsub('\\n', "\n").gsub('\\t', "\t")
                        .gsub('\\"', '"').gsub('\\\\', '\\')
          end
          return inner
        end
      when 0x27 # '...'  single-quoted
        return str[1..-2] if str.end_with?("'") && str.size >= 2
      when 0x2A # '*'    alias
        return @anchors[str[1..-1]] if str.size > 1 && !str.include?(" ")
      when 0x7E # '~'
        return nil if str.size == 1
      when 0x6E # 'n'
        return nil if str == "null"
      when 0x4E # 'N'
        return nil if str == "Null" || str == "NULL"
      when 0x74 # 't'
        return true if str == "true"
      when 0x54 # 'T'
        return true if str == "True" || str == "TRUE"
      when 0x66 # 'f'
        return false if str == "false"
      when 0x46 # 'F'
        return false if str == "False" || str == "FALSE"
      end

      # Numbers: only worth a regex when the leading byte could
      # plausibly start one.
      if first == 0x2D || (first >= 0x30 && first <= 0x39) # '-' or '0'..'9'
        if str =~ /\A-?\d+\z/
          return str.to_i
        elsif str =~ /\A-?\d+\.\d+(?:[eE][+-]?\d+)?\z/
          return str.to_f
        end
      end

      str
    end

    def skip_blanks_and_comments
      while @pos < @lines.size
        line = @lines[@pos]
        # Avoid two strip allocations per line (the old code stripped
        # twice). Walk leading whitespace inline; lines that only have
        # whitespace, or whose first non-space byte is '#', are skipped.
        i = 0
        len = line.size
        while i < len
          b = line.getbyte(i)
          break unless b == 0x20 || b == 0x09
          i += 1
        end
        if i == len || line.getbyte(i) == 0x23 # '#'
          @pos += 1
        else
          break
        end
      end
    end
  end
end

YAML = Psych unless defined?(YAML)
