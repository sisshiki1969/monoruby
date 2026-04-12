# Minimal Psych / YAML stub for monoruby.
#
# The real Psych library is a C extension that monoruby cannot load. Much
# of Rails only needs YAML to serialize simple structures (arrays, hashes,
# scalars) for configuration and schema dumps. This stub provides just
# enough of Psych/YAML's surface area for ActiveRecord's load-time code to
# succeed; it does NOT implement full YAML 1.1/1.2 semantics.

module Psych
  VERSION = "5.0.0"
  LIBYAML_VERSION = "0.2.5"

  class SyntaxError < StandardError; end
  class DisallowedClass < StandardError
    def initialize(action, klass); super("Tried to #{action} unspecified class: #{klass}"); end
  end
  class BadAlias < StandardError; end
  class AliasesNotEnabled < StandardError; end

  # YAML tag registry. Real Psych uses this to map custom YAML tags
  # (`!ruby/object:Foo`) to Ruby classes. The stub cannot actually
  # instantiate anything from a YAML tag, but code that merely writes to
  # the hash (as ActiveRecord does during load) must not blow up.
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

  # Very small scanner: supports a handful of flow-style values commonly
  # appearing in config/schema files.
  def self.load(yaml, permitted_classes: [], permitted_symbols: [], aliases: false, filename: nil, fallback: nil, symbolize_names: false, strict_integer: false, freeze: false)
    return fallback if yaml.nil? || yaml.empty?
    parse_scalar(yaml.strip)
  end

  def self.unsafe_load(yaml, filename: nil, fallback: nil, symbolize_names: false, strict_integer: false, freeze: false)
    load(yaml, fallback: fallback, symbolize_names: symbolize_names)
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

  def self.parse_scalar(str)
    case str
    when "", "~", "null", "Null", "NULL"
      nil
    when "true", "True", "TRUE"
      true
    when "false", "False", "FALSE"
      false
    when /\A-?\d+\z/
      str.to_i
    when /\A-?\d+\.\d+\z/
      str.to_f
    when /\A"(.*)"\z/, /\A'(.*)'\z/
      $1
    else
      str
    end
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
end

YAML = Psych unless defined?(YAML)
