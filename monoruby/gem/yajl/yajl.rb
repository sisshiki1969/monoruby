# yajl/yajl.rb – monoruby's stand-in for yajl.so (the yajl-ruby gem)
#
# The yajl-ruby gem is `lib/yajl.rb` (the `Yajl.load` / `Yajl.dump`
# helpers and the class-method conveniences) over a C extension that
# binds the YAJL streaming JSON library: `Yajl::Parser`, `Yajl::Encoder`,
# `Yajl::Projector`, the two error classes and `Yajl::MAX_DEPTH`. This
# file defines those in Ruby, porting the parts of yajl 1.x that decide
# what a caller observes:
#
# - the lexer and its error texts ("lexical error: invalid char in json
#   text.", "invalid bytes in UTF8 string." and so on), including the
#   structural UTF-8 check, `/* */` and `//` comments, and the way a
#   number is only complete once the byte after it has been seen;
# - the parser's state machine and its error texts ("parse error:
#   unallowed token at this point in JSON text" ...);
# - `yajl_render_error_string`: the message is the error line, then the
#   surrounding input text padded so that the failing position sits at
#   column 41, then the `(right here) ------^` arrow;
# - the gem's builder callbacks (`yajl_set_static_value`): the object is
#   assembled on a stack, `parse` answers the stack's last entry (so an
#   unterminated `{` answers `{}` — the C extension ignores the status of
#   the final flush), a second top-level value without a block or
#   `on_parse_complete` is "Found multiple JSON objects in the stream ...",
#   and with one every complete top-level value is handed to it;
# - the generator (`yajl_gen`): `pretty` layout with a configurable
#   indent, `html_safe` (`\/`), `entities` (`<`, `>`, `&`, U+2028 and
#   U+2029 as `\uXXXX`), control characters as `\u00XX`, everything else
#   byte for byte; numbers through `to_s` with NaN / Infinity refused;
#   objects through `to_json` (emitted raw) when they respond to it, else
#   `to_s` as a JSON string.

module Yajl
  MAX_DEPTH = 256
  READ_BUFSIZE = 8192
  WRITE_BUFSIZE = 8192

  class ParseError < StandardError; end
  class EncodeError < StandardError; end

  # A streaming parser: feed it text with `<<` (with `on_parse_complete`
  # set) or hand it a whole String / IO with `parse`.
  class Parser
    # Lexer token kinds.
    T_EOF = :eof
    T_ERROR = :error
    T_STRING = :string
    T_STRING_ESC = :string_esc
    T_INTEGER = :integer
    T_DOUBLE = :double
    T_BOOL = :bool
    T_NULL = :null
    T_LBRACE = :lbrace     # {
    T_RBRACE = :rbrace     # }
    T_LBRACKET = :lbracket # [
    T_RBRACKET = :rbracket # ]
    T_COMMA = :comma
    T_COLON = :colon

    LEX_ERRORS = {
      invalid_utf8: "invalid bytes in UTF8 string.",
      invalid_escaped_char: "inside a string, '\\' occurs before a character which it may not.",
      invalid_json_char: "invalid character inside string.",
      invalid_hex_char: "invalid (non-hex) character occurs after '\\u' inside string.",
      invalid_char: "invalid char in json text.",
      invalid_string: "invalid string in json text.",
      missing_integer_after_exponent: "malformed number, a digit is required after the exponent.",
      missing_integer_after_decimal: "malformed number, a digit is required after the decimal point.",
      missing_integer_after_minus: "malformed number, a digit is required after the minus sign.",
      unallowed_comment: "probable comment found in input text, comments are not enabled.",
    }.freeze

    ARROW = "                     (right here) ------^\n"

    def self.parse(str_or_io, options = {}, read_bufsize = nil, &block)
      new(options).parse(str_or_io, read_bufsize, &block)
    end

    def initialize(opts = nil)
      @allow_comments = true
      @check_utf8 = true
      @symbolize_keys = false
      unless opts.nil?
        unless opts.is_a?(Hash)
          raise TypeError, "wrong argument type #{opts.class} (expected Hash)"
        end
        @allow_comments = false if opts[:allow_comments] == false
        @check_utf8 = false if opts[:check_utf8] == false
        @symbolize_keys = true if opts[:symbolize_keys] == true || opts[:symbolize_names] == true
      end
      @stack = []
      @nested_array_level = 0
      @nested_hash_level = 0
      @objects_found = 0
      @callback = nil
      @states = [:start]
      @pending = "".b
      @failed = nil
    end

    def on_parse_complete=(callback)
      @callback = callback
      nil
    end

    def parse(*args, &block)
      unless (1..2).cover?(args.size)
        raise ArgumentError, "wrong number of arguments (given #{args.size}, expected 1..2)"
      end
      input, bufsize = args
      if bufsize.nil?
        bufsize = READ_BUFSIZE
      elsif !bufsize.is_a?(Integer)
        raise TypeError, "wrong argument type #{bufsize.class} (expected Integer)"
      end
      @callback = block if block
      if input.is_a?(String)
        __parse_chunk(input)
      elsif input.respond_to?(:read)
        buf = String.new(capacity: bufsize)
        while input.read(bufsize, buf)
          __parse_chunk(buf)
        end
      else
        raise ParseError, "input must be a string or IO"
      end
      __parse_complete
      if @callback
        __check_and_fire_callback
        nil
      else
        @stack.pop
      end
    end

    def parse_chunk(chunk)
      raise ParseError, "Can't parse a nil string." if chunk.nil?
      unless @callback
        raise ParseError, "The on_parse_complete callback isn't setup, parsing useless."
      end
      __parse_chunk(chunk)
      nil
    end
    alias << parse_chunk

    private

    # --- builder (the gem's yajl callbacks) --------------------------------

    def __set_static_value(val)
      last = @stack.last
      case last
      when nil
        @stack.push(val)
      when Array
        last.push(val)
        @stack.push(val) if val.is_a?(Hash) || val.is_a?(Array)
      when Hash
        last[val] = nil
        @stack.push(val)
      when String, Symbol
        hash = @stack[-2]
        if hash.is_a?(Hash)
          hash[last] = val
          @stack.pop
          @stack.push(val) if val.is_a?(Hash) || val.is_a?(Array)
        end
      end
    end

    def __check_and_fire_callback
      if @stack.size == 1 && @nested_array_level == 0 && @nested_hash_level == 0
        if @callback
          @callback.call(@stack.pop)
        else
          @objects_found += 1
          if @objects_found > 1
            raise ParseError, "Found multiple JSON objects in the stream but no block or the on_parse_complete callback was assigned to handle them."
          end
        end
      end
    end

    def __found_value(val)
      __set_static_value(val)
      __check_and_fire_callback
    end

    def __found_string(bytes)
      __found_value(bytes.force_encoding(Encoding::UTF_8))
    end

    def __found_hash_key(bytes)
      key = bytes.force_encoding(Encoding::UTF_8)
      key = key.to_sym if @symbolize_keys
      __found_value(key)
    end

    # A number does not fire the completion callback (the gem's
    # `yajl_found_number` is the one callback that skips the check), so a
    # top-level number waits on the stack for the next value or the end of
    # `parse`.
    def __found_number(text)
      if text.include?(".") || text.include?("e") || text.include?("E")
        __set_static_value(text.to_f)
      else
        __set_static_value(Integer(text, 10))
      end
    end

    def __found_start_hash
      @nested_hash_level += 1
      __set_static_value({})
    end

    def __found_end_hash
      @nested_hash_level -= 1
      @stack.pop if @stack.size > 1
      __check_and_fire_callback
    end

    def __found_start_array
      @nested_array_level += 1
      __set_static_value([])
    end

    def __found_end_array
      @nested_array_level -= 1
      @stack.pop if @stack.size > 1
      __check_and_fire_callback
    end

    # --- chunk handling ----------------------------------------------------

    def __parse_chunk(chunk)
      raise @failed if @failed
      text = @pending.empty? ? chunk.b : @pending + chunk.b
      @pending = "".b
      __do_parse(text)
    end

    # yajl's `yajl_parse_complete` feeds a single space so that a number
    # pending at the end of the input terminates; the gem ignores the
    # status, so an unfinished document is silently left as built.
    def __parse_complete
      return if @failed
      return if @pending.empty?
      text = @pending + " ".b
      @pending = "".b
      begin
        __do_parse(text)
      rescue ParseError
        # ignored, as the C extension ignores the final status
      end
    end

    def __fail(type, message, offset, text)
      msg = __render_error(type, message, offset, text)
      @failed = ParseError.new(msg)
      raise @failed
    end

    def __render_error(type, message, offset, text)
      # assembled as bytes: the context line may carry the invalid input
      str = "#{type} error".b
      str << ": " << message.b if message
      str << "\n"
      len = text.bytesize
      spaces = offset < 30 ? 40 - offset : 10
      start = offset >= 30 ? offset - 30 : 0
      stop = offset + 30 > len ? len : offset + 30
      ctx = " " * spaces
      ctx << text.byteslice(start, stop - start).tr("\n\r", "  ")
      ctx << "\n"
      str << ctx << ARROW
      str.force_encoding(Encoding::UTF_8)
    end

    # --- lexer -------------------------------------------------------------

    def __space?(b)
      b == 0x20 || b == 0x09 || b == 0x0a || b == 0x0b || b == 0x0c || b == 0x0d
    end

    def __digit?(b)
      b && b >= 0x30 && b <= 0x39
    end

    def __hex?(b)
      b && ((b >= 0x30 && b <= 0x39) || (b >= 0x41 && b <= 0x46) || (b >= 0x61 && b <= 0x66))
    end

    # Lex one token from `text` at byte offset `i`. Answers
    # [kind, token_start, token_end] (`token_end` is the offset after the
    # token), [T_EOF, start] when the input ends before a token completes
    # (`start` is where the incomplete token began), or
    # [T_ERROR, error_key, offset].
    def __lex(text, i)
      len = text.bytesize
      loop do
        return [T_EOF, i] if i >= len
        start = i
        b = text.getbyte(i)
        i += 1
        case b
        when 0x7b then return [T_LBRACE, start, i]
        when 0x7d then return [T_RBRACE, start, i]
        when 0x5b then return [T_LBRACKET, start, i]
        when 0x5d then return [T_RBRACKET, start, i]
        when 0x2c then return [T_COMMA, start, i]
        when 0x3a then return [T_COLON, start, i]
        when 0x20, 0x09, 0x0a, 0x0b, 0x0c, 0x0d
          next
        when 0x74 # t
          return __lex_literal(text, i, start, "rue", T_BOOL)
        when 0x66 # f
          return __lex_literal(text, i, start, "alse", T_BOOL)
        when 0x6e # n
          return __lex_literal(text, i, start, "ull", T_NULL)
        when 0x22 # "
          return __lex_string(text, i, start)
        when 0x2d, 0x30..0x39
          return __lex_number(text, start)
        when 0x2f # /
          return [T_ERROR, :unallowed_comment, start] unless @allow_comments
          r = __lex_comment(text, i, start)
          return r if r
          i = @comment_end
          next
        else
          return [T_ERROR, :invalid_char, i]
        end
      end
    end

    def __lex_literal(text, i, start, want, kind)
      len = text.bytesize
      want.each_byte do |w|
        return [T_EOF, start] if i >= len
        return [T_ERROR, :invalid_string, i] if text.getbyte(i) != w
        i += 1
      end
      [kind, start, i]
    end

    def __lex_string(text, i, start)
      len = text.bytesize
      escapes = false
      loop do
        return [T_EOF, start] if i >= len
        b = text.getbyte(i)
        i += 1
        if b == 0x22
          return [escapes ? T_STRING_ESC : T_STRING, start, i]
        elsif b == 0x5c
          escapes = true
          return [T_EOF, start] if i >= len
          e = text.getbyte(i)
          i += 1
          if e == 0x75 # u
            4.times do
              return [T_EOF, start] if i >= len
              h = text.getbyte(i)
              return [T_ERROR, :invalid_hex_char, i] unless __hex?(h)
              i += 1
            end
          elsif !(e == 0x22 || e == 0x5c || e == 0x2f || e == 0x62 || e == 0x66 ||
                  e == 0x6e || e == 0x72 || e == 0x74)
            return [T_ERROR, :invalid_escaped_char, i - 1]
          end
        elsif b < 0x20
          return [T_ERROR, :invalid_json_char, i - 1]
        elsif @check_utf8 && b >= 0x80
          need = if b >> 5 == 0x6 then 1
                 elsif b >> 4 == 0xe then 2
                 elsif b >> 3 == 0x1e then 3
                 else return [T_ERROR, :invalid_utf8, i]
                 end
          need.times do
            return [T_EOF, start] if i >= len
            c = text.getbyte(i)
            i += 1
            return [T_ERROR, :invalid_utf8, i] unless c >> 6 == 0x2
          end
        end
      end
    end

    # Numbers are lexed one byte beyond their end, so a number that runs
    # into the end of the text is incomplete.
    def __lex_number(text, start)
      len = text.bytesize
      i = start
      kind = T_INTEGER
      return [T_EOF, start] if i >= len
      b = text.getbyte(i)
      i += 1
      if b == 0x2d
        return [T_EOF, start] if i >= len
        b = text.getbyte(i)
        i += 1
      end
      if b == 0x30
        return [T_EOF, start] if i >= len
        b = text.getbyte(i)
        i += 1
      elsif b >= 0x31 && b <= 0x39
        loop do
          return [T_EOF, start] if i >= len
          b = text.getbyte(i)
          i += 1
          break unless __digit?(b)
        end
      else
        return [T_ERROR, :missing_integer_after_minus, i - 1]
      end
      if b == 0x2e
        n = 0
        return [T_EOF, start] if i >= len
        b = text.getbyte(i)
        i += 1
        while __digit?(b)
          n += 1
          return [T_EOF, start] if i >= len
          b = text.getbyte(i)
          i += 1
        end
        return [T_ERROR, :missing_integer_after_decimal, i - 1] if n == 0
        kind = T_DOUBLE
      end
      if b == 0x65 || b == 0x45
        return [T_EOF, start] if i >= len
        b = text.getbyte(i)
        i += 1
        if b == 0x2b || b == 0x2d
          return [T_EOF, start] if i >= len
          b = text.getbyte(i)
          i += 1
        end
        if __digit?(b)
          loop do
            return [T_EOF, start] if i >= len
            b = text.getbyte(i)
            i += 1
            break unless __digit?(b)
          end
        else
          return [T_ERROR, :missing_integer_after_exponent, i - 1]
        end
        kind = T_DOUBLE
      end
      [kind, start, i - 1]
    end

    # Answers nil when the comment was skipped (with `@comment_end` set),
    # else the T_EOF / T_ERROR token to return.
    def __lex_comment(text, i, start)
      len = text.bytesize
      return [T_EOF, start] if i >= len
      b = text.getbyte(i)
      i += 1
      if b == 0x2f
        loop do
          return [T_EOF, start] if i >= len
          b = text.getbyte(i)
          i += 1
          break if b == 0x0a
        end
      elsif b == 0x2a
        loop do
          return [T_EOF, start] if i >= len
          b = text.getbyte(i)
          i += 1
          if b == 0x2a
            return [T_EOF, start] if i >= len
            b = text.getbyte(i)
            i += 1
            break if b == 0x2f
            i -= 1
          end
        end
      else
        return [T_ERROR, :invalid_char, i]
      end
      @comment_end = i
      nil
    end

    def __decode_string(raw)
      out = String.new(capacity: raw.bytesize)
      out.force_encoding(Encoding::BINARY)
      i = 0
      len = raw.bytesize
      beg = 0
      while i < len
        if raw.getbyte(i) == 0x5c
          out << raw.byteslice(beg, i - beg) if i > beg
          e = raw.getbyte(i + 1)
          i += 2
          case e
          when 0x72 then out << "\r"
          when 0x6e then out << "\n"
          when 0x5c then out << "\\"
          when 0x2f then out << "/"
          when 0x22 then out << "\""
          when 0x66 then out << "\f"
          when 0x62 then out << "\b"
          when 0x74 then out << "\t"
          when 0x75
            cp = raw.byteslice(i, 4).to_i(16)
            i += 4
            if cp >= 0xD800 && cp <= 0xDBFF
              # a surrogate pair needs a \uDC00..\uDFFF right behind
              if raw.getbyte(i) == 0x5c && raw.getbyte(i + 1) == 0x75
                lo = raw.byteslice(i + 2, 4).to_i(16)
                if lo >= 0xDC00 && lo <= 0xDFFF
                  cp = 0x10000 + ((cp - 0xD800) << 10) + (lo - 0xDC00)
                  i += 6
                else
                  cp = nil
                end
              else
                cp = nil
              end
            elsif cp >= 0xDC00 && cp <= 0xDFFF
              cp = nil
            end
            out << (cp ? [cp].pack("U").b : "?")
          else
            out << "?"
          end
          beg = i
        else
          i += 1
        end
      end
      out << raw.byteslice(beg, len - beg) if len > beg
      out
    end

    # --- parser ------------------------------------------------------------

    def __do_parse(text)
      i = 0
      loop do
        state = @states.last
        case state
        when :lexical_error, :parse_error
          return
        when :start, :map_need_val, :array_need_val, :array_start
          tok = __lex(text, i)
          kind = tok[0]
          case kind
          when T_EOF
            @pending = text.byteslice(tok[1], text.bytesize - tok[1])
            return
          when T_ERROR
            @states[-1] = :lexical_error
            __fail("lexical", LEX_ERRORS[tok[1]], tok[2], text)
          end
          i = tok[2]
          push = nil
          case kind
          when T_STRING
            __found_string(text.byteslice(tok[1] + 1, tok[2] - tok[1] - 2))
          when T_STRING_ESC
            __found_string(__decode_string(text.byteslice(tok[1] + 1, tok[2] - tok[1] - 2)))
          when T_BOOL
            __found_value(text.getbyte(tok[1]) == 0x74)
          when T_NULL
            __found_value(nil)
          when T_LBRACE
            __found_start_hash
            push = :map_start
          when T_LBRACKET
            __found_start_array
            push = :array_start
          when T_INTEGER, T_DOUBLE
            __found_number(text.byteslice(tok[1], tok[2] - tok[1]))
          when T_RBRACKET
            if state == :array_start
              __found_end_array
              @states.pop
              next
            end
            @states[-1] = :parse_error
            __fail("parse", "unallowed token at this point in JSON text", i, text)
          else # colon, comma, rbrace
            @states[-1] = :parse_error
            __fail("parse", "unallowed token at this point in JSON text", i, text)
          end
          # got a value: the transition depends on the state
          case state
          when :start
            # stays :start — the gem's yajl accepts a stream of values
          when :map_need_val
            @states[-1] = :map_got_val
          else
            @states[-1] = :array_got_val
          end
          @states.push(push) if push
        when :map_start, :map_need_key
          tok = __lex(text, i)
          kind = tok[0]
          case kind
          when T_EOF
            @pending = text.byteslice(tok[1], text.bytesize - tok[1])
            return
          when T_ERROR
            @states[-1] = :lexical_error
            __fail("lexical", LEX_ERRORS[tok[1]], tok[2], text)
          when T_STRING, T_STRING_ESC
            i = tok[2]
            raw = text.byteslice(tok[1] + 1, tok[2] - tok[1] - 2)
            raw = __decode_string(raw) if kind == T_STRING_ESC
            __found_hash_key(raw)
            @states[-1] = :map_sep
          when T_RBRACE
            i = tok[2]
            if state == :map_start
              __found_end_hash
              @states.pop
            else
              @states[-1] = :parse_error
              __fail("parse", "invalid object key (must be a string)", i, text)
            end
          else
            i = tok[2]
            @states[-1] = :parse_error
            __fail("parse", "invalid object key (must be a string)", i, text)
          end
        when :map_sep
          tok = __lex(text, i)
          case tok[0]
          when T_COLON
            i = tok[2]
            @states[-1] = :map_need_val
          when T_EOF
            @pending = text.byteslice(tok[1], text.bytesize - tok[1])
            return
          when T_ERROR
            @states[-1] = :lexical_error
            __fail("lexical", LEX_ERRORS[tok[1]], tok[2], text)
          else
            i = tok[2]
            @states[-1] = :parse_error
            __fail("parse", "object key and value must be separated by a colon (':')", i, text)
          end
        when :map_got_val
          tok = __lex(text, i)
          case tok[0]
          when T_RBRACE
            i = tok[2]
            __found_end_hash
            @states.pop
          when T_COMMA
            i = tok[2]
            @states[-1] = :map_need_key
          when T_EOF
            @pending = text.byteslice(tok[1], text.bytesize - tok[1])
            return
          when T_ERROR
            @states[-1] = :lexical_error
            __fail("lexical", LEX_ERRORS[tok[1]], tok[2], text)
          else
            i = tok[2]
            @states[-1] = :parse_error
            # yajl backs the offset up over the token here
            buf_len = tok[2] - tok[1]
            buf_len -= 2 if tok[0] == T_STRING || tok[0] == T_STRING_ESC
            off = i >= buf_len ? i - buf_len : 0
            __fail("parse", "after key and value, inside map, I expect ',' or '}'", off, text)
          end
        when :array_got_val
          tok = __lex(text, i)
          case tok[0]
          when T_RBRACKET
            i = tok[2]
            __found_end_array
            @states.pop
          when T_COMMA
            i = tok[2]
            @states[-1] = :array_need_val
          when T_EOF
            @pending = text.byteslice(tok[1], text.bytesize - tok[1])
            return
          when T_ERROR
            @states[-1] = :lexical_error
            __fail("lexical", LEX_ERRORS[tok[1]], tok[2], text)
          else
            i = tok[2]
            @states[-1] = :parse_error
            __fail("parse", "after array element, I expect ',' or ']'", i, text)
          end
        end
      end
    end
  end

  # A JSON generator with yajl's layout options.
  class Encoder
    def self.encode(obj, *args, &block)
      args.flatten!
      options = {}
      io = nil
      if args.any?
        args.each do |arg|
          if arg.is_a?(Hash)
            options = arg
          elsif arg.respond_to?(:write)
            io = arg
          end
        end
      end
      new(options).encode(obj, io, &block)
    end

    # Define `to_json` on the core classes, as the json gem does, in terms
    # of this encoder.
    def self.enable_json_gem_compatability
      [Hash, Array, Integer, Float, String, TrueClass, FalseClass, NilClass].each do |klass|
        klass.class_eval do
          def to_json(*args)
            encoder = args[0]
            encoder = Yajl::Encoder.new unless encoder.class == Yajl::Encoder
            encoder.encode(self)
          end
        end
      end
      nil
    end

    def initialize(opts = nil)
      @pretty = false
      @indent = "  "
      @html_safe = 0
      @terminator = :none
      @on_progress = nil
      unless opts.nil?
        unless opts.is_a?(Hash)
          raise TypeError, "wrong argument type #{opts.class} (expected Hash)"
        end
        if opts[:pretty] == true
          @pretty = true
          indent = opts[:indent]
          unless indent.nil?
            unless indent.is_a?(String)
              raise TypeError, "wrong argument type #{indent.class} (expected String)"
            end
            @indent = indent.encode(Encoding::UTF_8).b
          end
        end
        @html_safe = 1 if opts[:html_safe] == true
        @html_safe = 2 if opts[:entities] == true
        if opts.has_key?(:terminator)
          t = opts[:terminator]
          t = t.encode(Encoding::UTF_8) if t.is_a?(String)
          @terminator = t
        end
      end
      @indent = @indent.b
      @buf = "".b
      @states = [:start]
      @depth = 0
    end

    def on_progress=(callback)
      @on_progress = callback
      nil
    end

    def encode(*args, &block)
      unless (1..2).cover?(args.size)
        raise ArgumentError, "wrong number of arguments (given #{args.size}, expected 1..2)"
      end
      obj, io = args
      @on_progress = block if block
      __encode_part(obj, io)
      out = @buf.force_encoding(Encoding::UTF_8)
      @buf = "".b
      if io
        io.write(out)
        io.write(@terminator) if @terminator != :none && !@terminator.nil?
        nil
      elsif block
        block.call(out)
        block.call(@terminator) if @terminator != :none
        nil
      else
        out << @terminator if @terminator != :none && !@terminator.nil?
        out
      end
    end

    private

    def __encode_part(obj, io)
      if (io || @on_progress) && @buf.bytesize >= WRITE_BUFSIZE
        chunk = @buf.force_encoding(Encoding::UTF_8)
        @buf = "".b
        if io
          io.write(chunk)
        else
          @on_progress.call(chunk)
        end
      end
      case obj
      when Hash
        __gen_map_open
        obj.each do |k, v|
          key = case k
                when String then k
                when Symbol then k.name
                else k.to_s
                end
          __encode_part(key, io)
          __encode_part(v, io)
        end
        __gen_map_close
      when Array
        __gen_array_open
        obj.each { |e| __encode_part(e, io) }
        __gen_array_close
      when nil
        __gen_atom("null")
      when true
        __gen_atom("true")
      when false
        __gen_atom("false")
      when Integer
        __gen_atom(obj.to_s)
      when Float
        s = obj.to_s
        if s.start_with?("NaN") || s.start_with?("Infinity") || s.start_with?("-Infinity")
          raise EncodeError, "'#{s}' is an invalid number"
        end
        __gen_atom(s)
      when String
        __gen_string(obj)
      when Symbol
        __gen_string(obj.name)
      else
        if obj.respond_to?(:to_json)
          s = obj.to_json
          unless s.is_a?(String)
            raise TypeError, "wrong argument type #{s.class} (expected String)"
          end
          __gen_atom(s)
        else
          s = obj.to_s
          unless s.is_a?(String)
            raise TypeError, "wrong argument type #{s.class} (expected String)"
          end
          __gen_string(s)
        end
      end
    end

    def __ensure_not_key
      st = @states[@depth]
      if st == :map_key || st == :map_start
        raise EncodeError, "YAJL internal error: attempted use of non-string object as key"
      end
    end

    def __insert_sep
      case @states[@depth]
      when :map_key, :in_array
        @buf << ","
        @buf << "\n" if @pretty
      when :map_val
        @buf << ":"
        @buf << " " if @pretty
      end
    end

    def __insert_whitespace
      if @pretty && @states[@depth] != :map_val
        @depth.times { @buf << @indent }
      end
    end

    def __appended_atom
      case @states[@depth]
      when :map_start, :map_key then @states[@depth] = :map_val
      when :array_start then @states[@depth] = :in_array
      when :map_val then @states[@depth] = :map_key
      end
    end

    def __gen_atom(text)
      __ensure_not_key
      __insert_sep
      __insert_whitespace
      @buf << text.b
      __appended_atom
    end

    def __gen_string(str)
      __insert_sep
      __insert_whitespace
      @buf << "\""
      __escape(str.b)
      @buf << "\""
      __appended_atom
    end

    def __escape(s)
      beg = 0
      i = 0
      len = s.bytesize
      while i < len
        b = s.getbyte(i)
        esc = nil
        inc = 1
        case b
        when 0x0d then esc = "\\r"
        when 0x0a then esc = "\\n"
        when 0x5c then esc = "\\\\"
        when 0x22 then esc = "\\\""
        when 0x0c then esc = "\\f"
        when 0x08 then esc = "\\b"
        when 0x09 then esc = "\\t"
        when 0x2f
          esc = "\\/" if @html_safe != 0
        when 0xe2
          if @html_safe == 2 && s.getbyte(i + 1) == 0x80
            c = s.getbyte(i + 2)
            if c == 0xa8
              esc = "\\u2028"
              inc = 3
            elsif c == 0xa9
              esc = "\\u2029"
              inc = 3
            end
          end
        when 0x3c, 0x3e, 0x26
          esc = format("\\u00%02X", b) if @html_safe == 2
        else
          esc = format("\\u00%02X", b) if b < 0x20
        end
        if esc
          @buf << s.byteslice(beg, i - beg) if i > beg
          @buf << esc
          i += inc
          beg = i
        else
          i += 1
        end
      end
      @buf << s.byteslice(beg, len - beg) if len > beg
    end

    def __increment_depth
      @depth += 1
      if @depth >= MAX_DEPTH
        @depth -= 1
        raise EncodeError, "Max nesting depth of #{MAX_DEPTH} exceeded"
      end
    end

    def __gen_map_open
      __ensure_not_key
      __insert_sep
      __insert_whitespace
      __increment_depth
      @states[@depth] = :map_start
      @buf << "{"
      @buf << "\n" if @pretty
    end

    def __gen_map_close
      @depth -= 1
      @buf << "\n" if @pretty
      __appended_atom
      __insert_whitespace
      @buf << "}"
    end

    def __gen_array_open
      __ensure_not_key
      __insert_sep
      __insert_whitespace
      __increment_depth
      @states[@depth] = :array_start
      @buf << "["
      @buf << "\n" if @pretty
    end

    def __gen_array_close
      @depth -= 1
      @buf << "\n" if @pretty
      __appended_atom
      __insert_whitespace
      @buf << "]"
    end
  end

  # Parses a stream and keeps only the keys a schema names (`nil` keeps a
  # value whole, a nested Hash narrows it further).
  class Projector
    def initialize(stream, read_bufsize = 4096)
      @stream = stream
      @buffer_size = read_bufsize
    end

    def project(schema)
      value = Parser.new.parse(@stream, @buffer_size)
      __project(value, schema)
    end

    private

    def __project(value, schema)
      return value if schema.nil?
      case value
      when Hash
        out = {}
        schema.each do |key, sub|
          out[key] = __project(value[key], sub) if value.key?(key)
        end
        out
      when Array
        value.map { |v| __project(v, schema) }
      else
        value
      end
    end
  end
end
