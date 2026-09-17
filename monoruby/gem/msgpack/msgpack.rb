# msgpack/msgpack.rb – monoruby's stand-in for msgpack.so
#
# The msgpack gem's Ruby half (`lib/msgpack.rb`, `lib/msgpack/*.rb`:
# `Factory#register_type`, `Timestamp`, `Time`, `Bigint`, the `to_msgpack`
# core extensions, `Factory::Pool`) reopens the classes its C extension
# defines — `Buffer`, `Packer`, `Unpacker`, `Factory`, `ExtensionValue`
# and the error classes. This file defines those in Ruby, with the C
# extension's argument handling, wire format and error texts, so the gem
# loads unchanged on top of it (`require "msgpack/msgpack"` resolves here
# before the `.so`).
#
# What the C extension does that this mirrors:
#
# - The packer's type dispatch (`msgpack_packer_write_value`): nil / true
#   / false / Integer / Float / Symbol / String / Array / Hash have fixed
#   encodings; a String / Array / Hash whose class is exactly the core
#   class never consults the extension-type registry, a subclass does;
#   every other object is looked up in the registry and falls back to
#   `to_msgpack(packer)`.
# - A String's encoding decides between the `bin` and `str` families:
#   ASCII-8BIT is `bin`, UTF-8 / US-ASCII / an ASCII-only string is `str`
#   as is, anything else is transcoded to UTF-8. In compatibility mode
#   every string is `str` and `str8` is never emitted.
# - Registry lookup order: the object's class, then a cached ancestor
#   match, then a scan of the registered classes in registration order
#   (the first `is_a?` match wins and is cached).
# - The unpacker is a resumable state machine over its buffer: a `read`
#   that runs out of bytes leaves everything in place, so `feed` + `each`
#   accumulate a partial object across chunks, and `each` ends quietly at
#   the buffer's end (or the IO's end of file) instead of raising.
# - `str` payloads come back UTF-8, `bin` payloads ASCII-8BIT; map keys are
#   frozen; `freeze: true` freezes (and dedups) every produced object;
#   `symbolize_keys: true` interns String keys.

module MessagePack
  DEFAULT_EMPTY_PARAMS = {}.freeze unless const_defined?(:DEFAULT_EMPTY_PARAMS)

  module TypeError; end

  class UnpackError < StandardError; end
  class MalformedFormatError < UnpackError; end
  class StackError < UnpackError; end
  class UnexpectedTypeError < UnpackError
    include MessagePack::TypeError
  end
  class UnknownExtTypeError < UnpackError
    include MessagePack::TypeError
  end

  ExtensionValue = Struct.new(:type, :payload) unless const_defined?(:ExtensionValue)

  class HeldBuffer < BasicObject; end

  # Argument checks with the C API's wording.
  module Check # :nodoc:
    module_function

    def type(v, klass, name = klass.name)
      return v if v.is_a?(klass)
      raise ::TypeError, "wrong argument type #{Check.class_name(v)} (expected #{name})"
    end

    def string_value(v)
      return v if v.is_a?(String)
      if v.respond_to?(:to_str)
        s = v.to_str
        return s if s.is_a?(String)
      end
      raise ::TypeError, "no implicit conversion of #{Check.implicit_name(v)} into String"
    end

    def integer(v)
      return v if v.is_a?(Integer)
      if v.is_a?(Float)
        raise ::RangeError, "float #{v} out of range of integer" if v.nan? || v.infinite?
        return v.to_i
      end
      raise ::TypeError, "no implicit conversion from nil to integer" if v.nil?
      return v.to_int if v.respond_to?(:to_int)
      raise ::TypeError, "no implicit conversion of #{Check.implicit_name(v)} into Integer"
    end

    def ext_type(v)
      t = integer(v)
      if t < -128 || t > 127
        raise ::RangeError, "integer #{t} too big to convert to `signed char'"
      end
      t
    end

    def class_name(v)
      case v
      when nil then "nil"
      when true then "true"
      when false then "false"
      else v.class.name
      end
    end

    def implicit_name(v)
      class_name(v)
    end
  end

  # An in-memory byte queue, optionally in front of an IO: the packer
  # accumulates into it and flushes to the IO, the unpacker consumes from
  # it and refills from the IO.
  class Buffer
    DEFAULT_IO_BUFFER_SIZE = 32 * 1024

    def initialize(*args)
      io = nil
      options = nil
      case args.size
      when 0
        # nothing
      when 1
        v = args[0]
        if v.is_a?(Hash)
          options = v
        else
          io = v
        end
      when 2
        io, options = args
        unless options.is_a?(Hash)
          raise ArgumentError, "expected Hash but found #{Check.class_name(io)}."
        end
      else
        raise ArgumentError, "wrong number of arguments (#{args.size} for 0..1)"
      end
      __set_options(io, options)
    end

    def __set_options(io, options) # :nodoc:
      @s = "".b
      @io = io
      @io_buffer_size = DEFAULT_IO_BUFFER_SIZE
      @read_method = if io && io.respond_to?(:readpartial) then :readpartial else :read end
      @write_method = if io && !io.respond_to?(:write) && io.respond_to?(:<<) then :<< else :write end
      if options
        if (v = options[:io_buffer_size])
          @io_buffer_size = Check.integer(v)
        end
        # :read_reference_threshold / :write_reference_threshold tune the
        # C extension's zero-copy paths; there is nothing to tune here.
      end
    end

    def io
      @io
    end

    def clear
      @s = "".b
      nil
    end

    def size
      @s.bytesize
    end

    def empty?
      @s.empty?
    end

    def write(data)
      data = Check.string_value(data)
      @s << data.b
      data.bytesize
    end

    def <<(data)
      write(data)
      self
    end

    def __append_bytes(bytes) # :nodoc:
      @s << bytes
    end

    def __str # :nodoc:
      @s
    end

    def __consume(n) # :nodoc:
      @s = @s.byteslice(n, @s.bytesize - n) || "".b
    end

    # Pull one chunk from the IO. Returns false at end of file (the C
    # extension raises EOFError "IO reached end of file" when `read`
    # answers nil; `readpartial` raises its own EOFError).
    def __fill_from_io # :nodoc:
      return false unless @io
      chunk = @io.__send__(@read_method, @io_buffer_size)
      raise EOFError, "IO reached end of file" if chunk.nil?
      return false if chunk.empty?
      @s << chunk.b
      true
    end

    # Make at least `n` bytes readable, refilling from the IO; false when
    # the data runs out. A strict caller (`read_all`, `skip_all`) lets the
    # IO's own EOFError through, as the extension does.
    def __ensure_readable(n, strict = false) # :nodoc:
      while @s.bytesize < n
        if @io
          begin
            return false unless __fill_from_io
          rescue EOFError
            raise if strict
            return false
          end
        else
          return false
        end
      end
      true
    end

    # Swap the queued bytes out (and back in) — the packer runs a
    # recursive extension type's proc against an empty buffer this way.
    def __swap_string(s) # :nodoc:
      old = @s
      @s = s
      old
    end

    def __read_until_eof(n) # :nodoc:
      __ensure_readable(n)
      take = n < @s.bytesize ? n : @s.bytesize
      out = @s.byteslice(0, take)
      __consume(take)
      out
    end

    def __read_all # :nodoc:
      if @io
        loop do
          begin
            break unless __fill_from_io
          rescue EOFError
            break
          end
        end
      end
      out = @s
      @s = "".b
      out
    end

    def read(*args)
      out = nil
      n = nil
      case args.size
      when 0
        # all
      when 1
        n = Check.integer(args[0])
      when 2
        n = Check.integer(args[0])
        out = Check.type(args[1], String)
      else
        raise ArgumentError, "wrong number of arguments (#{args.size} for 0..2)"
      end
      if n.nil?
        data = __read_all
        return out ? out.replace(data) : data
      end
      if n == 0
        return out ? out.replace("".b) : "".b
      end
      if !@io && out.nil? && @s.bytesize <= n
        data = @s
        @s = "".b
        return data.empty? ? nil : data
      end
      data = __read_until_eof(n)
      data = out.replace(data) if out
      data.empty? ? nil : data
    end

    def read_all(*args)
      out = nil
      n = nil
      case args.size
      when 0
        # all
      when 1
        n = Check.integer(args[0])
      when 2
        n = Check.integer(args[0])
        out = Check.type(args[1], String)
      else
        raise ArgumentError, "wrong number of arguments (#{args.size} for 0..2)"
      end
      if n.nil?
        data = __read_all
        return out ? out.replace(data) : data
      end
      if n == 0
        return out ? out.replace("".b) : "".b
      end
      raise EOFError, "end of buffer reached" unless __ensure_readable(n, true)
      data = @s.byteslice(0, n)
      __consume(n)
      out ? out.replace(data) : data
    end

    def skip(n)
      n = Check.integer(n)
      return 0 if n == 0
      __read_until_eof(n).bytesize
    end

    def skip_all(n)
      n = Check.integer(n)
      return self if n == 0
      raise EOFError, "end of buffer reached" unless __ensure_readable(n, true)
      __consume(n)
      self
    end

    def flush
      if @io && !@s.empty?
        @io.__send__(@write_method, @s)
        @s = "".b
      end
      self
    end

    def close
      @io ? @io.close : nil
    end

    def write_to(io)
      n = @s.bytesize
      io.write(@s)
      @s = "".b
      n
    end

    def to_str
      @s.dup
    end
    alias to_s to_str

    def to_a
      [@s.dup]
    end
  end

  # Serializer. Wire format per the MessagePack spec; see the file header
  # for the type dispatch rules.
  class Packer
    def initialize(*args)
      if args.size > 2
        raise ArgumentError, "wrong number of arguments (#{args.size} for 0..2)"
      end
      io = args[0]
      options = args[1]
      if options.nil? && io.is_a?(Hash)
        options = io
        io = nil
      end
      Check.type(options, Hash) unless options.nil?
      @buffer = Buffer.new
      @buffer.__set_options(io, options)
      @io = io
      @compat = options ? !!options[:compatibility_mode] : false
      @ext_hash = {}
      @ext_cache = {}
      @has_symbol_ext_type = false
      @has_bigint_ext_type = false
    end

    def __adopt_registry(hash, has_symbol, has_bigint) # :nodoc:
      @ext_hash = hash.dup
      @ext_cache = {}
      @has_symbol_ext_type = has_symbol
      @has_bigint_ext_type = has_bigint
    end

    def compatibility_mode?
      @compat
    end

    def buffer
      @buffer
    end

    def write(v)
      __write_value(v)
      self
    end
    alias pack write

    def write_nil
      @buffer.__append_bytes("\xc0".b)
      self
    end

    def write_true
      @buffer.__append_bytes("\xc3".b)
      self
    end

    def write_false
      @buffer.__append_bytes("\xc2".b)
      self
    end

    def write_float(f)
      f = Float(f) unless f.is_a?(Float)
      @buffer.__append_bytes([0xcb, f].pack("CG"))
      self
    end

    def write_float32(f)
      f = Float(f) unless f.is_a?(Float)
      @buffer.__append_bytes([0xca, f].pack("Cg"))
      self
    end

    def write_string(s)
      __write_string_value(Check.type(s, String))
      self
    end

    def write_bin(s)
      Check.type(s, String)
      write_bin_header(s.bytesize)
      @buffer.__append_bytes(s.b)
      self
    end

    def write_array(a)
      __write_array_value(Check.type(a, Array))
      self
    end

    def write_hash(h)
      __write_hash_value(Check.type(h, Hash))
      self
    end

    def write_symbol(s)
      __write_symbol_value(Check.type(s, Symbol))
      self
    end

    def write_int(i)
      __write_integer_value(Check.type(i, Integer))
      self
    end

    def write_extension(ev)
      Check.type(ev, Struct)
      type = ev[0]
      unless type.is_a?(Integer)
        raise RangeError, "integer #{type} too big to convert to `signed char'"
      end
      type = Check.ext_type(type)
      payload = Check.string_value(ev[1])
      __write_ext(type, payload)
      self
    end

    def write_ext(type, payload)
      type = Check.ext_type(type)
      payload = Check.string_value(payload)
      __write_ext(type, payload)
      self
    end

    def write_array_header(n)
      n = Check.integer(n)
      @buffer.__append_bytes(
        if n < 16 then [0x90 | n].pack("C")
        elsif n < 0x10000 then [0xdc, n].pack("Cn")
        else [0xdd, n].pack("CN")
        end
      )
      self
    end

    def write_map_header(n)
      n = Check.integer(n)
      @buffer.__append_bytes(
        if n < 16 then [0x80 | n].pack("C")
        elsif n < 0x10000 then [0xde, n].pack("Cn")
        else [0xdf, n].pack("CN")
        end
      )
      self
    end

    def write_bin_header(n)
      n = Check.integer(n)
      @buffer.__append_bytes(
        if n < 0x100 then [0xc4, n].pack("CC")
        elsif n < 0x10000 then [0xc5, n].pack("Cn")
        else [0xc6, n].pack("CN")
        end
      )
      self
    end

    def flush
      @buffer.flush
      self
    end

    def reset
      @buffer.clear
      nil
    end
    alias clear reset

    def size
      @buffer.size
    end

    def empty?
      @buffer.empty?
    end

    def write_to(io)
      @buffer.write_to(io)
    end

    def to_str
      @buffer.to_str
    end
    alias to_s to_str

    def to_a
      @buffer.to_a
    end

    def full_pack
      if @io
        flush
        nil
      else
        s = to_str
        reset
        s
      end
    end

    def register_type_internal(type, klass, proc)
      raise FrozenError, "can't modify frozen MessagePack::Packer" if frozen?
      type = Check.ext_type(type)
      @ext_cache.clear
      @ext_hash[klass] = [type, proc, 0]
      @has_symbol_ext_type = true if klass == Symbol
      nil
    end

    private

    def registered_types_internal
      @ext_hash.dup
    end

    def __write_value(v)
      case v
      when nil then write_nil
      when true then write_true
      when false then write_false
      when Integer then __write_integer_value(v)
      when Float then @buffer.__append_bytes([0xcb, v].pack("CG"))
      when Symbol then __write_symbol_value(v)
      when String
        __write_string_value(v) if v.class == String || !__try_write_ext(v)
      when Array
        __write_array_value(v) if v.class == Array || !__try_write_ext(v)
      when Hash
        __write_hash_value(v) if v.class == Hash || !__try_write_ext(v)
      else
        __write_other_value(v)
      end
    end

    def __write_other_value(v)
      v.to_msgpack(self) unless __try_write_ext(v)
    end

    def __write_symbol_value(v)
      if @has_symbol_ext_type
        __write_other_value(v)
      else
        __write_string_value(v.name)
      end
    end

    def __write_integer_value(i)
      if i >= 0
        if i < 0x80 then @buffer.__append_bytes([i].pack("C"))
        elsif i < 0x100 then @buffer.__append_bytes([0xcc, i].pack("CC"))
        elsif i < 0x10000 then @buffer.__append_bytes([0xcd, i].pack("Cn"))
        elsif i < 0x100000000 then @buffer.__append_bytes([0xce, i].pack("CN"))
        elsif i < 0x10000000000000000 then @buffer.__append_bytes([0xcf, i].pack("CQ>"))
        else
          # rb_big2ull's RangeError, unless an oversized-integer extension
          # type takes it.
          return if @has_bigint_ext_type && __try_write_ext(i)
          raise RangeError, "bignum too big to convert into 'unsigned long long'"
        end
      else
        if i >= -32 then @buffer.__append_bytes([i].pack("c"))
        elsif i >= -0x80 then @buffer.__append_bytes([0xd0, i].pack("Cc"))
        elsif i >= -0x8000 then @buffer.__append_bytes([0xd1, i].pack("Cs>"))
        elsif i >= -0x80000000 then @buffer.__append_bytes([0xd2, i].pack("Cl>"))
        else
          # The C extension routes a negative number whose magnitude needs
          # the top bit of an 8-byte word (-2**63 included) to the
          # extension type before trying the 64-bit encoding.
          if @has_bigint_ext_type && (-i).bit_length > 63 && __try_write_ext(i)
            return
          end
          if i >= -0x8000000000000000
            @buffer.__append_bytes([0xd3, i].pack("Cq>"))
          else
            raise RangeError, "bignum too big to convert into 'long long'"
          end
        end
      end
    end

    def __write_string_value(s)
      len = s.bytesize
      if len > 0xffffffff
        raise ArgumentError, "size of string is too long to pack: #{len} bytes should be <= 4294967295"
      end
      if @compat
        __write_raw_header(len)
        @buffer.__append_bytes(s.b)
        return
      end
      enc = s.encoding
      if enc == Encoding::ASCII_8BIT
        write_bin_header(len)
        @buffer.__append_bytes(s)
      else
        unless enc == Encoding::UTF_8 || enc == Encoding::US_ASCII || s.ascii_only?
          s = s.encode(Encoding::UTF_8)
          len = s.bytesize
        end
        __write_raw_header(len)
        @buffer.__append_bytes(s.b)
      end
    end

    def __write_raw_header(n)
      @buffer.__append_bytes(
        if n < 32 then [0xa0 | n].pack("C")
        elsif n < 0x100 && !@compat then [0xd9, n].pack("CC")
        elsif n < 0x10000 then [0xda, n].pack("Cn")
        else [0xdb, n].pack("CN")
        end
      )
    end

    def __write_array_value(a)
      len = a.size
      if len > 0xffffffff
        raise ArgumentError, "size of array is too long to pack: #{len} bytes should be <= 4294967295"
      end
      write_array_header(len)
      a.each { |e| __write_value(e) }
    end

    def __write_hash_value(h)
      len = h.size
      if len > 0xffffffff
        raise ArgumentError, "size of array is too long to pack: #{len} bytes should be <= 4294967295"
      end
      write_map_header(len)
      h.each { |k, v| __write_value(k); __write_value(v) }
    end

    def __write_ext(type, payload)
      n = payload.bytesize
      hdr = case n
            when 1 then [0xd4, type].pack("Cc")
            when 2 then [0xd5, type].pack("Cc")
            when 4 then [0xd6, type].pack("Cc")
            when 8 then [0xd7, type].pack("Cc")
            when 16 then [0xd8, type].pack("Cc")
            else
              if n < 0x100 then [0xc7, n, type].pack("CCc")
              elsif n < 0x10000 then [0xc8, n, type].pack("Cnc")
              else [0xc9, n, type].pack("CNc")
              end
            end
      @buffer.__append_bytes(hdr)
      @buffer.__append_bytes(payload.b)
    end

    # The extension-type registry lookup; returns false when no type is
    # registered for the object's class.
    def __ext_lookup(v)
      return nil if @ext_hash.empty?
      klass = v.class
      entry = @ext_hash[klass]
      return entry if entry
      entry = @ext_cache[klass]
      return entry if entry
      @ext_hash.each_key do |k|
        if k.is_a?(Module) && v.is_a?(k)
          entry = @ext_hash[k]
          @ext_cache[klass] = entry
          return entry
        end
      end
      nil
    end

    def __try_write_ext(v)
      entry = __ext_lookup(v)
      return false unless entry
      type, proc, flags = entry
      if flags & Factory::EXT_RECURSIVE != 0
        # The proc writes the payload through this packer into an empty
        # buffer; the outer bytes are parked meanwhile (no instance
        # variable changes, so a frozen packer works too).
        saved = @buffer.__swap_string("".b)
        begin
          proc.call(v, self)
          payload = @buffer.__swap_string(saved)
        rescue Exception
          @buffer.__swap_string(saved)
          raise
        end
        __write_ext(type, payload)
      else
        payload = Check.string_value(proc.call(v))
        __write_ext(type, payload)
      end
      true
    end
  end

  # Deserializer: a resumable reader over its buffer.
  class Unpacker
    STACK_CAPACITY = 128

    # The reader's position, nesting depth and re-entrancy flag live in a
    # separate object so that a frozen unpacker (as `Factory::Pool` keeps
    # them) can still read.
    ReadState = Struct.new(:pos, :depth, :reading)

    def initialize(*args)
      io = nil
      options = nil
      case args.size
      when 0
        # nothing
      when 1
        v = args[0]
        if v.is_a?(Hash)
          options = v
        else
          io = v
        end
      when 2
        io, options = args
        if !options.nil? && !options.is_a?(Hash)
          raise ArgumentError, "expected Hash but found #{Check.class_name(options)}."
        end
      else
        raise ArgumentError, "wrong number of arguments (#{args.size} for 0..2)"
      end
      @buffer = Buffer.new
      @buffer.__set_options(io, options)
      @io = io
      @symbolize_keys = false
      @freeze = false
      @allow_unknown_ext = false
      if options
        @symbolize_keys = !!options[:symbolize_keys]
        @freeze = !!options[:freeze]
        @allow_unknown_ext = !!options[:allow_unknown_ext]
      end
      @ext_types = {}
      @optimized_symbol_ext_type = false
      @symbol_ext_type = nil
      @st = ReadState.new(0, 0, false)
    end

    def __adopt_registry(hash, optimized_symbol, symbol_type) # :nodoc:
      @ext_types = hash.dup
      @optimized_symbol_ext_type = optimized_symbol
      @symbol_ext_type = symbol_type
    end

    def symbolize_keys?
      @symbolize_keys
    end

    def freeze?
      @freeze
    end

    def allow_unknown_ext?
      @allow_unknown_ext
    end

    def buffer
      @buffer
    end

    def feed(data)
      data = Check.string_value(data)
      @buffer.__append_bytes(data.b)
      self
    end
    alias feed_reference feed

    def read
      __read_object
    end
    alias unpack read

    def skip
      __read_object
      nil
    end

    # Answers whether a nil is next. The extension keeps the byte it
    # looked at as the pending head byte, so the nil is not consumed: the
    # next `read` still answers it.
    def skip_nil
      __ensure(1)
      @buffer.__str.getbyte(0) == 0xc0
    end

    def read_array_header
      __ensure(1)
      b = @buffer.__str.getbyte(0)
      n, len =
        if b & 0xf0 == 0x90 then [b & 0x0f, 1]
        elsif b == 0xdc then __ensure(3); [@buffer.__str.byteslice(1, 2).unpack1("n"), 3]
        elsif b == 0xdd then __ensure(5); [@buffer.__str.byteslice(1, 4).unpack1("N"), 5]
        else raise UnexpectedTypeError, "unexpected type"
        end
      @buffer.__consume(len)
      n
    end

    def read_map_header
      __ensure(1)
      b = @buffer.__str.getbyte(0)
      n, len =
        if b & 0xf0 == 0x80 then [b & 0x0f, 1]
        elsif b == 0xde then __ensure(3); [@buffer.__str.byteslice(1, 2).unpack1("n"), 3]
        elsif b == 0xdf then __ensure(5); [@buffer.__str.byteslice(1, 4).unpack1("N"), 5]
        else raise UnexpectedTypeError, "unexpected type"
        end
      @buffer.__consume(len)
      n
    end

    def each
      return to_enum(:each) unless block_given?
      loop do
        begin
          v = __read_object
        rescue EOFError
          return nil
        end
        yield v
      end
    end

    def feed_each(data, &block)
      return to_enum(:feed_each, data) unless block
      feed(data)
      each(&block)
    end

    def reset
      @buffer.clear
      @st.pos = 0
      @st.depth = 0
      nil
    end

    def full_unpack
      v = __read_object
      extra = @buffer.size
      if extra > 0
        raise MalformedFormatError, "#{extra} extra bytes after the deserialized object"
      end
      v
    end

    private

    def register_type_internal(type, klass, proc)
      raise FrozenError, "can't modify frozen MessagePack::Unpacker" if frozen?
      type = Check.ext_type(type)
      @ext_types[type] = [klass, proc, 0]
      nil
    end

    def registered_types_internal
      @ext_types.dup
    end

    # Bytes [0, n) of the buffer, refilling from the IO; EOFError when they
    # cannot be had.
    def __ensure(n)
      return if @buffer.__str.bytesize >= n
      loop do
        if @io
          # A refill that answers nothing raises its own EOFError, which
          # `each` turns into a quiet stop when an IO is set.
          raise EOFError, "end of buffer reached" unless @buffer.__fill_from_io
        else
          raise EOFError, "end of buffer reached"
        end
        return if @buffer.__str.bytesize >= n
      end
    end

    # Read one complete object from the front of the buffer, consuming it.
    # When the bytes run out, nothing is consumed and EOFError propagates,
    # so the next `feed` + `read` resumes on the whole object. A `read`
    # issued by a recursive extension type's proc continues the read in
    # progress instead.
    def __read_object
      return __parse if @st.reading
      @st.pos = 0
      @st.depth = 0
      @st.reading = true
      begin
        v = __parse
      rescue EOFError
        @st.pos = 0
        @st.depth = 0
        raise
      ensure
        @st.reading = false
      end
      @buffer.__consume(@st.pos)
      @st.pos = 0
      v
    end

    def __take(n)
      s = @buffer.__str
      pos = @st.pos
      if pos + n > s.bytesize
        __ensure(pos + n)
        s = @buffer.__str
      end
      r = s.byteslice(pos, n)
      @st.pos = pos + n
      r
    end

    def __byte
      s = @buffer.__str
      pos = @st.pos
      if pos >= s.bytesize
        __ensure(pos + 1)
        s = @buffer.__str
      end
      b = s.getbyte(pos)
      @st.pos = pos + 1
      b
    end

    def __parse(key = false)
      b = __byte
      case b
      when 0x00..0x7f then b
      when 0x80..0x8f then __map(b & 0x0f)
      when 0x90..0x9f then __array(b & 0x0f)
      when 0xa0..0xbf then __str(b & 0x1f, key)
      when 0xc0 then nil
      when 0xc2 then false
      when 0xc3 then true
      when 0xc4 then __bin(__take(1).unpack1("C"), key)
      when 0xc5 then __bin(__take(2).unpack1("n"), key)
      when 0xc6 then __bin(__take(4).unpack1("N"), key)
      when 0xc7 then n = __take(1).unpack1("C"); __ext(__take(1).unpack1("c"), n)
      when 0xc8 then n = __take(2).unpack1("n"); __ext(__take(1).unpack1("c"), n)
      when 0xc9 then n = __take(4).unpack1("N"); __ext(__take(1).unpack1("c"), n)
      when 0xca then __take(4).unpack1("g")
      when 0xcb then __take(8).unpack1("G")
      when 0xcc then __take(1).unpack1("C")
      when 0xcd then __take(2).unpack1("n")
      when 0xce then __take(4).unpack1("N")
      when 0xcf then __take(8).unpack1("Q>")
      when 0xd0 then __take(1).unpack1("c")
      when 0xd1 then __take(2).unpack1("s>")
      when 0xd2 then __take(4).unpack1("l>")
      when 0xd3 then __take(8).unpack1("q>")
      when 0xd4 then __ext(__take(1).unpack1("c"), 1)
      when 0xd5 then __ext(__take(1).unpack1("c"), 2)
      when 0xd6 then __ext(__take(1).unpack1("c"), 4)
      when 0xd7 then __ext(__take(1).unpack1("c"), 8)
      when 0xd8 then __ext(__take(1).unpack1("c"), 16)
      when 0xd9 then __str(__take(1).unpack1("C"), key)
      when 0xda then __str(__take(2).unpack1("n"), key)
      when 0xdb then __str(__take(4).unpack1("N"), key)
      when 0xdc then __array(__take(2).unpack1("n"))
      when 0xdd then __array(__take(4).unpack1("N"))
      when 0xde then __map(__take(2).unpack1("n"))
      when 0xdf then __map(__take(4).unpack1("N"))
      when 0xe0..0xff then b - 0x100
      else raise MalformedFormatError, "invalid byte"
      end
    end

    def __complete(obj)
      obj.freeze if @freeze
      obj
    end

    def __str(n, key)
      s = __take(n).force_encoding(Encoding::UTF_8)
      if key
        @symbolize_keys ? s.to_sym : -s
      elsif @freeze
        -s
      else
        s
      end
    end

    def __bin(n, key)
      s = __take(n)
      if key
        @symbolize_keys ? s.to_sym : -s
      elsif @freeze
        -s
      else
        s
      end
    end

    def __nest
      @st.depth += 1
      raise StackError, "stack level too deep" if @st.depth > STACK_CAPACITY
      r = yield
      @st.depth -= 1
      r
    end

    def __array(n)
      __nest do
        a = Array.new(n) { __parse }
        __complete(a)
      end
    end

    def __map(n)
      __nest do
        h = {}
        n.times do
          k = __parse(true)
          k = k.to_sym if @symbolize_keys && k.is_a?(String)
          h[k] = __parse
        end
        __complete(h)
      end
    end

    def __ext(type, n)
      if @optimized_symbol_ext_type && type == @symbol_ext_type
        return __complete(__take(n).force_encoding(Encoding::UTF_8).to_sym)
      end
      entry = @ext_types[type]
      if entry
        _klass, proc, flags = entry
        if flags & Factory::EXT_RECURSIVE != 0
          # The extension's payload is itself MessagePack; the proc reads
          # it through this unpacker.
          obj = __nest do
            proc.call(self)
          end
          return __complete(obj)
        end
        payload = __take(n)
        return __complete(proc.call(payload))
      end
      payload = __take(n)
      if @allow_unknown_ext
        return __complete(ExtensionValue.new(type, payload))
      end
      raise UnknownExtTypeError, "unexpected extension type"
    end
  end

  # A registry of extension types that stamps out packers and unpackers.
  class Factory
    EXT_RECURSIVE = 0x1

    def initialize(*args)
      unless args.empty?
        raise ArgumentError, "wrong number of arguments (#{args.size} for 0)"
      end
      @pk_hash = {}
      @uk_hash = {}
      @has_symbol_ext_type = false
      @has_bigint_ext_type = false
      @optimized_symbol_ext_type = false
      @symbol_ext_type = nil
    end

    def dup
      clone = self.class.allocate
      clone.__copy_from(@pk_hash, @uk_hash, @has_symbol_ext_type, @has_bigint_ext_type,
                        @optimized_symbol_ext_type, @symbol_ext_type)
      clone
    end

    def __copy_from(pk, uk, has_symbol, has_bigint, optimized, symbol_type) # :nodoc:
      @pk_hash = pk.dup
      @uk_hash = uk.dup
      @has_symbol_ext_type = has_symbol
      @has_bigint_ext_type = has_bigint
      @optimized_symbol_ext_type = optimized
      @symbol_ext_type = symbol_type
    end

    def freeze
      @pk_hash.freeze
      super
    end

    def packer(*args)
      pk = Packer.new(*args)
      pk.__adopt_registry(@pk_hash, @has_symbol_ext_type, @has_bigint_ext_type)
      pk
    end

    def unpacker(*args)
      uk = Unpacker.new(*args)
      uk.__adopt_registry(@uk_hash, @optimized_symbol_ext_type, @symbol_ext_type)
      uk
    end

    private

    def registered_types_internal
      [@pk_hash.dup, @uk_hash.dup]
    end

    def register_type_internal(type, klass, options)
      Check.type(type, Integer)
      unless klass.is_a?(Module)
        raise ArgumentError, "expected Module/Class but found #{Check.class_name(klass)}."
      end
      flags = 0
      packer_proc = nil
      unpacker_proc = nil
      if options
        Check.type(options, Hash)
        packer_proc = options[:packer]
        unpacker_proc = options[:unpacker]
      end
      raise FrozenError, "can't modify frozen MessagePack::Factory" if frozen?
      type = Check.ext_type(type)
      if klass == Symbol
        @has_symbol_ext_type = true if options.nil? || options[:packer]
        if options && options[:optimized_symbols_parsing]
          @optimized_symbol_ext_type = true
          @symbol_ext_type = type
        end
      end
      if options
        if options[:oversized_integer_extension]
          if klass == Integer
            @has_bigint_ext_type = true
          else
            raise ArgumentError, "oversized_integer_extension: true is only for Integer class"
          end
        end
        flags |= EXT_RECURSIVE if options[:recursive]
      end
      @pk_hash[klass] = [type, packer_proc, flags]
      @uk_hash[type] = [klass, unpacker_proc, flags]
      nil
    end
  end
end
