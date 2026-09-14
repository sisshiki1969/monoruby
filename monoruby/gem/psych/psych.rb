# psych/psych.rb – monoruby's stand-in for the psych gem's C extension
# (psych.so), loaded by gem/psych.rb in its place.
#
# The extension is small: it drives libyaml's parser, calling the
# `Psych::Handler` methods for each event, and feeds `Psych::Emitter`
# events to libyaml's emitter. Everything else in psych (the node tree,
# `TreeBuilder`, `ToRuby`, `YAMLTree`, the scalar scanner, ...) is the
# gem's own Ruby, vendored unchanged under gem/psych/. The native
# pieces here are `String.__yaml_parse` / `__yaml_emit` /
# `__yaml_libyaml_version` (src/builtins/yaml.rs, over `libyaml-safer`,
# a port of libyaml 0.2.5), so events and emitted text are the ones
# CRuby's psych produces.
#
# Defined here, before the gem's files reopen them: `Psych.libyaml_version`,
# `Psych::Parser` (`_native_parse`, `mark`, the encoding constants),
# `Psych::Emitter` (< `Psych::Handler`), `Psych::ClassLoader#path2class`,
# `Psych::Visitors::ToRuby#build_exception` and
# `Psych::Visitors::YAMLTree#private_iv_get`.

require_relative 'handler'
require_relative 'syntax_error'

module Psych
  def self.libyaml_version
    String.__yaml_libyaml_version
  end

  class Parser
    ANY     = 0
    UTF8    = 1
    UTF16LE = 2
    UTF16BE = 3

    # The position of the last event, as `Psych::Parser::Mark`.
    def mark
      Mark.new(@__mark_index || 0, @__mark_line || 0, @__mark_column || 0)
    end

    private

    # Parse `yaml` (a String, or an IO read to its end, as psych does),
    # dispatching each event to `handler`; a syntax error is raised as
    # `Psych::SyntaxError` naming `path`.
    def _native_parse(handler, yaml, path)
      yaml = yaml.read if yaml.respond_to?(:read)
      raise TypeError, "no implicit conversion of #{yaml.class} into String" unless yaml.respond_to?(:to_str)
      yaml = yaml.to_str
      # libyaml reads UTF-8 (or UTF-16 with a BOM); psych transcodes any
      # other encoding to UTF-8 first.
      enc = yaml.encoding
      unless enc == Encoding::UTF_8 || enc == Encoding::ASCII_8BIT || enc == Encoding::US_ASCII ||
             enc == Encoding::UTF_16LE || enc == Encoding::UTF_16BE
        yaml = yaml.encode(Encoding::UTF_8)
      end
      err = String.__yaml_parse(handler, yaml)
      if err
        line, column, offset, problem, context = err
        raise Psych::SyntaxError.new(path, line, column, offset, problem, context)
      end
      nil
    end
  end

  # The event-driven emitter: `Psych::Handler`'s methods are the events,
  # each fed to the native libyaml emitter as it arrives and whatever it
  # wrote passed on to the IO, as psych's C extension does. The native
  # emitter is freed at `end_stream` and, failing that, by a finalizer.
  class Emitter < Psych::Handler
    # A finalizer that frees `handle`, built outside the instance so it
    # captures no reference to the emitter itself.
    def self.__finalizer(handle)
      proc { String.__yaml_emitter_free(handle) }
    end

    def initialize(io, options = nil)
      @io = io
      @canonical = false
      @indentation = 2
      @line_width = 0
      if options
        @line_width = options.line_width
        @indentation = options.indentation
        @canonical = options.canonical
      end
      @handle = String.__yaml_emitter_new(@canonical, @indentation, @line_width)
      ObjectSpace.define_finalizer(self, Emitter.__finalizer(@handle))
    end

    attr_reader :canonical, :indentation, :line_width

    def canonical=(bool)
      @canonical = bool ? true : false
    end

    def indentation=(level)
      @indentation = Integer(level)
    end

    def line_width=(width)
      @line_width = Integer(width)
    end

    private def __emit(event)
      raise RuntimeError, "emitter is closed" unless @handle
      out = String.__yaml_emit(@handle, event)
      @io.write(out) unless out.empty?
      self
    end

    def start_stream(encoding)
      __emit([0, Integer(encoding)])
    end

    def end_stream
      __emit([1])
      String.__yaml_emitter_free(@handle)
      @handle = nil
      self
    end

    def start_document(version, tags, imp)
      raise TypeError, "wrong argument type #{version.class} (expected Array)" unless version.is_a?(Array)
      raise TypeError, "wrong argument type #{tags.class} (expected Array)" unless tags.is_a?(Array)
      tags.each do |tuple|
        raise TypeError, "wrong argument type #{tuple.class} (expected Array)" unless tuple.is_a?(Array)
        raise RuntimeError, "tag tuple must be of length 2" unless tuple.size == 2
      end
      __emit([2, version, tags.map { |h, p| [h.to_str, p.to_str] }, imp ? true : false])
    end

    def end_document(imp)
      __emit([3, imp ? true : false])
    end

    def scalar(value, anchor, tag, plain, quoted, style)
      __emit([4, value.to_str, anchor && anchor.to_str, tag && tag.to_str, plain ? true : false, quoted ? true : false, Integer(style)])
    end

    def start_sequence(anchor, tag, implicit, style)
      __emit([5, anchor && anchor.to_str, tag && tag.to_str, implicit ? true : false, Integer(style)])
    end

    def end_sequence
      __emit([6])
    end

    def start_mapping(anchor, tag, implicit, style)
      __emit([7, anchor && anchor.to_str, tag && tag.to_str, implicit ? true : false, Integer(style)])
    end

    def end_mapping
      __emit([8])
    end

    def alias(anchor)
      __emit([9, anchor.to_str])
    end
  end

  class ClassLoader
    private

    # `rb_path_to_class`: every segment must be a constant of the
    # previous one (no inheritance); a missing one is an ArgumentError,
    # a non-class one a TypeError.
    def path2class(path)
      path = path.to_str
      path.split("::").inject(Object) do |mod, name|
        unless mod.const_defined?(name, false)
          raise ArgumentError, "undefined class/module #{path}"
        end
        mod.const_get(name, false)
      end.tap do |klass|
        raise TypeError, "#{path} does not refer to class/module" unless klass.is_a?(Module)
      end
    end
  end

  module Visitors
    class Visitor; end

    class ToRuby < Visitor
      private

      # An exception of `klass` with `mesg`, built without running the
      # class's own `initialize` (as psych's C does).
      def build_exception(klass, mesg)
        e = klass.allocate
        ::Exception.instance_method(:initialize).bind_call(e, mesg)
        e
      end
    end

    class YAMLTree < Visitor
      private

      # `rb_attr_get(target, prop)`: the raw ivar, which for an
      # exception's `mesg` is the message as constructed.
      def private_iv_get(target, prop)
        if prop == 'mesg' && target.is_a?(::Exception)
          msg = ::Exception.instance_method(:to_s).bind_call(target)
          return msg == target.class.name ? nil : msg
        end
        target.instance_variable_get("@#{prop}")
      end
    end
  end
end
