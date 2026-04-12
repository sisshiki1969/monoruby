# frozen_string_literal: true
#
# JSON implementation for monoruby.
# Provides JSON.parse, JSON.generate, JSON.dump, JSON.pretty_generate
# and #to_json methods on core classes.
#

require 'strscan'

module JSON
  class JSONError < StandardError; end
  class ParserError < JSONError; end
  class GeneratorError < JSONError; end
  class NestingError < JSONError; end

  NaN = Float::NAN
  Infinity = Float::INFINITY
  MinusInfinity = -Float::INFINITY

  @create_id = 'json_class'

  class << self
    attr_accessor :create_id
  end

  def self.deep_const_get(path)
    path.split('::').inject(Object) { |p, c| p.const_get(c) }
  end
end

require 'json/ext/generator'
require 'json/ext/parser'

# Setup the main JSON module API
module JSON
  # Use the State from our generator
  State = JSON::Generator::State

  SAFE_STATE_PROTOTYPE = State.new
  FAST_STATE_PROTOTYPE = State.new(
    indent: '',
    space: '',
    object_nl: '',
    array_nl: '',
    max_nesting: false
  )
  PRETTY_STATE_PROTOTYPE = State.new(
    indent: '  ',
    space: ' ',
    object_nl: "\n",
    array_nl: "\n",
    max_nesting: false
  )

  # Use Ext::Parser as the default Parser
  Parser = Ext::Parser

  # --- Module methods ---

  def self.parse(source, opts = {})
    Parser.new(source, opts).parse
  end

  def self.parse!(source, opts = {})
    opts = {
      max_nesting: false,
      allow_nan: true
    }.merge(opts)
    Parser.new(source, opts).parse
  end

  def self.generate(obj, opts = nil)
    if opts
      State.generate(obj, opts)
    else
      State.generate(obj)
    end
  end

  def self.fast_generate(obj, opts = nil)
    state = FAST_STATE_PROTOTYPE.dup
    state.configure(opts) if opts
    state.generate(obj)
  end
  class << self
    alias fast_unparse fast_generate
  end

  def self.pretty_generate(obj, opts = nil)
    state = PRETTY_STATE_PROTOTYPE.dup
    state.configure(opts) if opts
    state.generate(obj)
  end
  class << self
    alias pretty_unparse pretty_generate
  end

  def self.dump(obj, anIO = nil, limit = nil)
    if anIO.is_a?(Integer)
      limit = anIO
      anIO = nil
    end
    opts = {}
    opts[:max_nesting] = limit if limit
    result = generate(obj, opts)
    if anIO
      anIO.write(result)
      anIO
    else
      result
    end
  end

  def self.load(source, proc = nil, **opts)
    if source.respond_to?(:to_str)
      source = source.to_str
    elsif source.respond_to?(:to_io)
      source = source.to_io.read
    elsif source.respond_to?(:read)
      source = source.read
    end
    result = parse(source, **opts)
    recurse_proc(result, &proc) if proc
    result
  end
  class << self
    alias restore load
  end

  def self.recurse_proc(result, &proc)
    case result
    when Array
      result.each { |x| recurse_proc(x, &proc) }
      proc.call(result)
    when Hash
      result.each_value { |x| recurse_proc(x, &proc) }
      proc.call(result)
    else
      proc.call(result)
    end
  end

  def self.[](object, opts = {})
    if object.is_a?(String)
      parse(object, opts)
    else
      generate(object, opts)
    end
  end
end

# Include to_json methods in core classes
class Hash
  include JSON::Generator::GeneratorMethods::Hash
end

class Array
  include JSON::Generator::GeneratorMethods::Array
end

class Integer
  def to_json(*)
    to_s
  end
end

class Float
  include JSON::Generator::GeneratorMethods::Float
end

class String
  include JSON::Generator::GeneratorMethods::String
end

class TrueClass
  def to_json(*)
    'true'
  end
end

class FalseClass
  def to_json(*)
    'false'
  end
end

class NilClass
  def to_json(*)
    'null'
  end
end
