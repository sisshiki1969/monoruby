# frozen_string_literal: true
#
# Prism's native backend for monoruby, standing in for the gem's C
# extension (`prism/prism.so`, which `prism.rb` requires as `prism/prism`
# when RUBY_ENGINE is "ruby").
#
# monoruby links libprism for its own parser, and `src/builtins/prism.rs`
# exposes its serializers as `Prism.__serialize` / `__parse_success?` /
# `__string_query` / `__version`. Everything else the gem's C extension
# provides is built here the way the gem's FFI backend (`prism/ffi.rb`)
# builds it: the source is parsed into the serialized form and
# `Prism::Serialize` (pure Ruby, part of the gem) turns that into the node
# tree. `dump_options` below is the FFI backend's, verbatim: it packs the
# options struct `pm_options_read` decodes.
#
# The serialization format is that of the linked libprism, so the gem and
# the library must be the same version; `Prism::VERSION` reports the
# library's.

module Prism
  VERSION = __version.freeze

  SERIALIZE_PARSE = 0
  SERIALIZE_LEX = 1
  SERIALIZE_PARSE_LEX = 2
  SERIALIZE_PARSE_COMMENTS = 3
  private_constant :SERIALIZE_PARSE, :SERIALIZE_LEX, :SERIALIZE_PARSE_LEX, :SERIALIZE_PARSE_COMMENTS

  class << self
    # Mirror the Prism.dump API by using the serialization API.
    def dump(source, **options)
      dump_common(source, options)
    end

    # Mirror the Prism.dump_file API by using the serialization API.
    def dump_file(filepath, **options)
      options[:filepath] = filepath
      dump_common(read_file(filepath), options)
    end

    # Mirror the Prism.lex API by using the serialization API.
    def lex(code, **options)
      lex_common(code, options)
    end

    # Mirror the Prism.lex_file API by using the serialization API.
    def lex_file(filepath, **options)
      options[:filepath] = filepath
      lex_common(read_file(filepath), options)
    end

    # Mirror the Prism.parse API by using the serialization API.
    def parse(code, **options)
      parse_common(code, options)
    end

    # Mirror the Prism.parse_file API by using the serialization API.
    def parse_file(filepath, **options)
      options[:filepath] = filepath
      parse_common(read_file(filepath), options)
    end

    # Mirror the Prism.parse_stream API: the whole stream is read, then
    # parsed (the C extension parses it line by line as it reads).
    def parse_stream(stream, **options)
      parse_common(stream.read, options)
    end

    # Mirror the Prism.parse_comments API by using the serialization API.
    def parse_comments(code, **options)
      parse_comments_common(code, options)
    end

    # Mirror the Prism.parse_file_comments API by using the serialization API.
    def parse_file_comments(filepath, **options)
      options[:filepath] = filepath
      parse_comments_common(read_file(filepath), options)
    end

    # Mirror the Prism.parse_lex API by using the serialization API.
    def parse_lex(code, **options)
      parse_lex_common(code, options)
    end

    # Mirror the Prism.parse_lex_file API by using the serialization API.
    def parse_lex_file(filepath, **options)
      options[:filepath] = filepath
      parse_lex_common(read_file(filepath), options)
    end

    # Mirror the Prism.parse_success? API by using the serialization API.
    def parse_success?(code, **options)
      __parse_success?(code, dump_options(options))
    end

    # Mirror the Prism.parse_failure? API by using the serialization API.
    def parse_failure?(code, **options)
      !parse_success?(code, **options)
    end

    # Mirror the Prism.parse_file_success? API by using the serialization API.
    def parse_file_success?(filepath, **options)
      options[:filepath] = filepath
      __parse_success?(read_file(filepath), dump_options(options))
    end

    # Mirror the Prism.parse_file_failure? API by using the serialization API.
    def parse_file_failure?(filepath, **options)
      !parse_file_success?(filepath, **options)
    end

    # Mirror the Prism.profile API by using the serialization API.
    def profile(source, **options)
      __serialize(SERIALIZE_PARSE, source, dump_options(options))
      nil
    end

    # Mirror the Prism.profile_file API by using the serialization API.
    def profile_file(filepath, **options)
      options[:filepath] = filepath
      __serialize(SERIALIZE_PARSE, read_file(filepath), dump_options(options))
      nil
    end

    private

    # The C extension maps the file and hands prism the raw bytes; the
    # loader then tags the source with the encoding prism detected.
    def read_file(filepath)
      raise TypeError, "wrong argument type #{filepath.class} (expected String)" unless filepath.is_a?(String)
      raise Errno::EISDIR, filepath if File.directory?(filepath)
      File.binread(filepath)
    end

    def dump_common(code, options) # :nodoc:
      dumped = __serialize(SERIALIZE_PARSE, code, dump_options(options))
      dumped.freeze if options.fetch(:freeze, false)
      dumped
    end

    def lex_common(code, options) # :nodoc:
      serialized = __serialize(SERIALIZE_LEX, code, dump_options(options))
      Serialize.load_lex(code, serialized, options.fetch(:freeze, false))
    end

    def parse_common(code, options) # :nodoc:
      serialized = dump_common(code, options)
      Serialize.load_parse(code, serialized, options.fetch(:freeze, false))
    end

    def parse_comments_common(code, options) # :nodoc:
      serialized = __serialize(SERIALIZE_PARSE_COMMENTS, code, dump_options(options))
      Serialize.load_parse_comments(code, serialized, options.fetch(:freeze, false))
    end

    def parse_lex_common(code, options) # :nodoc:
      serialized = __serialize(SERIALIZE_PARSE_LEX, code, dump_options(options))
      Serialize.load_parse_lex(code, serialized, options.fetch(:freeze, false))
    end

    # Return the value that should be dumped for the command_line option.
    def dump_options_command_line(options)
      command_line = options.fetch(:command_line, "")
      raise ArgumentError, "command_line must be a string" unless command_line.is_a?(String)

      command_line.each_char.inject(0) do |value, char|
        case char
        when "a" then value | 0b000001
        when "e" then value | 0b000010
        when "l" then value | 0b000100
        when "n" then value | 0b001000
        when "p" then value | 0b010000
        when "x" then value | 0b100000
        else raise ArgumentError, "invalid command_line option: #{char}"
        end
      end
    end

    # Return the value that should be dumped for the version option.
    def dump_options_version(version)
      current = version == "current"

      case current ? RUBY_VERSION : version
      when nil, "latest"
        0 # Handled in pm_parser_init
      when /\A3\.3(\.\d+)?\z/
        1
      when /\A3\.4(\.\d+)?\z/
        2
      when /\A3\.5(\.\d+)?\z/, /\A4\.0(\.\d+)?\z/
        3
      when /\A4\.1(\.\d+)?\z/
        4
      else
        if current
          raise CurrentVersionError, RUBY_VERSION
        else
          raise ArgumentError, "invalid version: #{version}"
        end
      end
    end

    # Convert the given options into a serialized options string.
    def dump_options(options)
      template = +""
      values = []

      template << "L"
      if (filepath = options[:filepath])
        values.push(filepath.bytesize, filepath.b)
        template << "A*"
      else
        values << 0
      end

      template << "l"
      values << options.fetch(:line, 1)

      template << "L"
      if (encoding = options[:encoding])
        name = encoding.is_a?(Encoding) ? encoding.name : encoding
        values.push(name.bytesize, name.b)
        template << "A*"
      else
        values << 0
      end

      template << "C"
      values << (options.fetch(:frozen_string_literal, false) ? 1 : 0)

      template << "C"
      values << dump_options_command_line(options)

      template << "C"
      values << dump_options_version(options[:version])

      template << "C"
      values << (options[:encoding] == false ? 1 : 0)

      template << "C"
      values << (options.fetch(:main_script, false) ? 1 : 0)

      template << "C"
      values << (options.fetch(:partial_script, false) ? 1 : 0)

      template << "C"
      values << (options.fetch(:freeze, false) ? 1 : 0)

      template << "L"
      if (scopes = options[:scopes])
        values << scopes.length

        scopes.each do |scope|
          locals = nil
          forwarding = 0

          case scope
          when Array
            locals = scope
          when Scope
            locals = scope.locals

            scope.forwarding.each do |forward|
              case forward
              when :*     then forwarding |= 0x1
              when :**    then forwarding |= 0x2
              when :&     then forwarding |= 0x4
              when :"..." then forwarding |= 0x8
              else raise ArgumentError, "invalid forwarding value: #{forward}"
              end
            end
          else
            raise TypeError, "wrong argument type #{scope.class.inspect} (expected Array or Prism::Scope)"
          end

          template << "L"
          values << locals.length

          template << "C"
          values << forwarding

          locals.each do |local|
            name = local.name
            template << "L"
            values << name.bytesize

            template << "A*"
            values << name.b
          end
        end
      else
        values << 0
      end

      values.pack(template)
    end
  end

  # The class-level queries the C extension adds to StringQuery.
  class StringQuery
    class << self
      # Mirrors the C extension's StringQuery::local? method.
      def local?(string)
        query(Prism.__string_query(0, string, string.encoding.name))
      end

      # Mirrors the C extension's StringQuery::constant? method.
      def constant?(string)
        query(Prism.__string_query(1, string, string.encoding.name))
      end

      # Mirrors the C extension's StringQuery::method_name? method.
      def method_name?(string)
        query(Prism.__string_query(2, string, string.encoding.name))
      end

      private

      def query(result)
        case result
        when -1 then raise ArgumentError, "Invalid or non ascii-compatible encoding"
        when 0 then false
        else true
        end
      end
    end
  end
end
