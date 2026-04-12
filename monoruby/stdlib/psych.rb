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
      @lines = yaml.gsub("\r\n", "\n").split("\n")
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
      line ? (line =~ /\S/ || line.size) : -1
    end

    def parse_value(parent_indent)
      skip_blanks_and_comments
      line = current_line
      return nil if line.nil?

      ind = indent_of(line)
      return nil if parent_indent >= 0 && ind <= parent_indent

      stripped = line.strip

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
      s = line.strip
      return false if s.start_with?("- ")
      return false if s.start_with?("#")
      if s =~ /\A(?:["'].*?["']|[^:#]*?)\s*:\s/
        return true
      end
      if s =~ /\A(?:["'].*?["']|[^:#]*?)\s*:\z/
        return true
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
        if key =~ /\A&(\S+)\s+(.*)/
          anchor = $1
          key = $2
        end

        key = resolve_scalar(key)

        if rest.nil? || rest.empty?
          value = parse_value(ind)
        elsif rest.start_with?("*")
          alias_name = rest[1..-1].strip
          value = @anchors[alias_name]
        elsif rest.start_with?("&")
          if rest =~ /\A&(\S+)\s*(.*)/
            val_anchor = $1
            val_rest = $2
            if val_rest.empty?
              value = parse_value(ind)
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
        elsif rest == "|" || rest == "|-" || rest == "|+"
          value = parse_literal_block(ind, rest)
        elsif rest == ">" || rest == ">-" || rest == ">+"
          value = parse_folded_block(ind, rest)
        else
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
        elsif after_dash =~ /\A\s+(.*)/
          item_text = $1
          if block_mapping_line?("  " * (ind + 2) + item_text)
            inner_indent = ind + 2
            fake_line = " " * inner_indent + item_text
            @lines[@pos] = fake_line
            value = parse_block_mapping(inner_indent)
          elsif item_text.start_with?("- ")
            @pos += 1
            inner_arr = [resolve_scalar(item_text[2..-1].strip)]
            value = inner_arr
          elsif item_text.start_with?("{")
            @pos += 1
            value = parse_flow_mapping(item_text)
          elsif item_text.start_with?("[")
            @pos += 1
            value = parse_flow_sequence(item_text)
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
      if stripped.start_with?('"')
        if stripped =~ /\A"((?:[^"\\]|\\.)*)"\s*:\s*(.*)\z/
          return [$1, $2]
        end
      elsif stripped.start_with?("'")
        if stripped =~ /\A'([^']*)'\s*:\s*(.*)\z/
          return [$1, $2]
        end
      end
      if stripped =~ /\A(.*?)\s*:\s+(.*)\z/
        return [$1, $2]
      end
      if stripped =~ /\A(.*?)\s*:\z/
        return [$1, nil]
      end
      [stripped, nil]
    end

    def parse_literal_block(parent_indent, chomp)
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
        lines << line[parent_indent + 2..-1].to_s
        @pos += 1
      end
      result = lines.join("\n")
      case chomp
      when "|"  then result + "\n"
      when "|-" then result
      when "|+" then result + "\n"
      else result + "\n"
      end
    end

    def parse_folded_block(parent_indent, chomp)
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
        lines << line[parent_indent + 2..-1].to_s
        @pos += 1
      end
      result = lines.join(" ").gsub("  ", " ")
      case chomp
      when ">"  then result + "\n"
      when ">-" then result
      when ">+" then result + "\n"
      else result + "\n"
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
      str = $1 if str =~ /\A(.*?)\s+#.*\z/ && !str.start_with?('"') && !str.start_with?("'")

      if str =~ /\A\*(\S+)\z/
        return @anchors[$1]
      end

      case str
      when "", "~", "null", "Null", "NULL"
        nil
      when "true", "True", "TRUE"
        true
      when "false", "False", "FALSE"
        false
      when /\A-?\d+\z/
        str.to_i
      when /\A-?\d+\.\d+(?:[eE][+-]?\d+)?\z/
        str.to_f
      when /\A"(.*)"\z/
        $1.gsub('\\n', "\n").gsub('\\t', "\t").gsub('\\"', '"').gsub('\\\\', '\\')
      when /\A'(.*)'\z/
        $1
      else
        str
      end
    end

    def skip_blanks_and_comments
      while @pos < @lines.size
        line = @lines[@pos]
        if line.strip.empty? || line.strip.start_with?("#")
          @pos += 1
        else
          break
        end
      end
    end
  end
end

YAML = Psych unless defined?(YAML)
