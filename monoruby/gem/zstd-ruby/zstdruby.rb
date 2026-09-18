# zstd-ruby/zstdruby.rb – monoruby's stand-in for zstdruby.so
#
# The zstd-ruby gem is `lib/zstd-ruby.rb` (plus the `StreamWriter` /
# `StreamReader` conveniences) over a C extension that binds libzstd:
# `Zstd.compress` / `decompress` (with `level:` / `dict:`), the deprecated
# `*_using_dict` forms, `Zstd::CDict` / `DDict`, `Zstd::StreamingCompress`
# / `StreamingDecompress` and the skippable-frame helpers. This file is
# that extension in Ruby over `String.__zstd_*` (the `ext/zstd` extension,
# libzstd_native.so, required just below — the bundled libzstd 1.5.7, the
# version the gem links), the same split as `Zlib` over
# `String.__zstream_*`.
#
# Contexts and dictionaries are integer handles into per-thread tables on
# the Rust side; each object frees its handle from an `ObjectSpace`
# finalizer. A stream keeps a reference to the `CDict` / `DDict` it was
# given so that the dictionary outlives the context that references it.
#
# Argument handling follows the extension: `level` may be a positional
# argument (deprecated, warned about) or the `level:` keyword, `dict:` is a
# `Zstd::CDict` / `DDict` or the dictionary bytes (anything else is
# "`dict:` must be a Zstd::CDict or a String"), `StreamingCompress#write`
# answers the number of bytes taken and keeps the output for `flush` /
# `finish`, `<<` / `print` / `puts` / `printf` are IO's on top of `write`.

require "zstd_native.so"

module Zstd
  DEFAULT_LEVEL = 3 # ZSTD_CLEVEL_DEFAULT
  SKIPPABLE_MAGIC_START = 0x184D2A50
  SKIPPABLE_HEADER_SIZE = 8

  def self.zstd_version
    String.__zstd_version
  end

  def self.compress(input, level_in_args = nil, **kwargs)
    level, dict = __compress_params(level_in_args, kwargs)
    String.__zstd_compress(__string_value(input), level, dict)
  end

  def self.compress_using_dict(input, dict, level = nil)
    warn "Zstd.compress_using_dict is deprecated; use Zstd.compress with `dict:` instead.", uplevel: 1
    level = level.nil? ? DEFAULT_LEVEL : Integer(level)
    input = __string_value(input)
    cdict = CDict.new(dict, level)
    String.__zstd_compress(input, level, cdict.__handle)
  ensure
    cdict&.__free
  end

  def self.decompress(input, **kwargs)
    input = __string_value(input)
    dict = __decompress_params(kwargs)
    String.__zstd_decompress(input, dict)
  end

  def self.decompress_using_dict(input, dict)
    warn "Zstd.decompress_using_dict is deprecated; use Zstd.decompress with `dict:` instead.", uplevel: 1
    input = __string_value(input)
    ddict = DDict.new(dict)
    if String.__zstd_dict_id(ddict.__handle) != String.__zstd_frame_dict_id(input)
      raise RuntimeError, "DictID mismatch"
    end
    String.__zstd_decompress(input, ddict.__handle)
  ensure
    ddict&.__free
  end

  # `ZSTD_writeSkippableFrame`: the frame is `magic (LE32) | size (LE32) |
  # data`; the extension writes it over the start of its output buffer, so
  # `input` only sizes that buffer and the result is the frame alone.
  def self.write_skippable_frame(input, skip, **kwargs)
    magic_variant = 0
    kwargs.each_key do |k|
      raise ArgumentError, "unknown keyword: #{k.inspect}" unless k == :magic_variant
    end
    magic_variant = Integer(kwargs[:magic_variant]) if kwargs.key?(:magic_variant)
    __string_value(input)
    skip = __string_value(skip)
    if magic_variant > 15
      raise RuntimeError, "write skippable frame failed: Parameter is out of bound"
    end
    [SKIPPABLE_MAGIC_START + magic_variant, skip.bytesize].pack("VV") + skip.b
  end

  # `ZSTD_readSkippableFrame`: the payload of a skippable frame at the
  # start of `input`, nil when there is none.
  def self.read_skippable_frame(input)
    input = __string_value(input)
    return nil if input.bytesize < 4
    magic = input.byteslice(0, 4).unpack1("V")
    return nil unless (magic & 0xFFFFFFF0) == SKIPPABLE_MAGIC_START
    if input.bytesize < SKIPPABLE_HEADER_SIZE
      raise RuntimeError, "read skippable frame failed: Src size is incorrect"
    end
    size = input.byteslice(4, 4).unpack1("V")
    if SKIPPABLE_HEADER_SIZE + size > input.bytesize
      raise RuntimeError, "read skippable frame failed: Src size is incorrect"
    end
    if size > 129 * 1024
      raise RuntimeError, "read skippable frame failed: Destination buffer is too small"
    end
    input.byteslice(SKIPPABLE_HEADER_SIZE, size)
  end

  def self.__string_value(v) # :nodoc:
    return v if v.is_a?(String)
    if v.respond_to?(:to_str)
      s = v.to_str
      return s if s.is_a?(String)
    end
    name = case v
           when nil then "nil"
           when true then "true"
           when false then "false"
           else v.class.name
           end
    raise TypeError, "no implicit conversion of #{name} into String"
  end

  # `set_compress_params`: answers [level, dict argument for the builtin].
  def self.__compress_params(level_in_args, kwargs) # :nodoc:
    kwargs.each_key do |k|
      raise ArgumentError, "unknown keyword: #{k.inspect}" unless k == :level || k == :dict
    end
    level = DEFAULT_LEVEL
    if !kwargs[:level].nil?
      level = Integer(kwargs[:level])
    elsif !level_in_args.nil?
      warn "`level` in args is deprecated; use keyword args `level:` instead.", uplevel: 2
      level = Integer(level_in_args)
    end
    [level, __dict_param(kwargs[:dict], CDict)]
  end

  def self.__decompress_params(kwargs) # :nodoc:
    kwargs.each_key do |k|
      raise ArgumentError, "unknown keyword: #{k.inspect}" unless k == :dict
    end
    __dict_param(kwargs[:dict], DDict)
  end

  def self.__dict_param(dict, klass) # :nodoc:
    case dict
    when nil then nil
    when klass then dict.__handle
    when String then dict
    else raise ArgumentError, "`dict:` must be a Zstd::#{klass.name.split('::').last} or a String"
    end
  end

  def self.__handle_finalizer(kind, handle) # :nodoc:
    if kind == :dict
      proc { String.__zstd_dict_free(handle) }
    else
      proc { String.__zstd_stream_free(handle) }
    end
  end

  # A pre-digested compression dictionary (`ZSTD_CDict`).
  class CDict
    def initialize(dict, level = nil)
      dict = Zstd.__string_value(dict)
      level = level.nil? ? nil : Integer(level)
      @handle = String.__zstd_cdict_new(dict, level)
      ObjectSpace.define_finalizer(self, Zstd.__handle_finalizer(:dict, @handle))
    end
    private :initialize

    def initialize_copy(other)
      raise RuntimeError, "CDict cannot be duplicated"
    end

    def __handle # :nodoc:
      @handle
    end

    def __free # :nodoc:
      ObjectSpace.undefine_finalizer(self)
      String.__zstd_dict_free(@handle)
      @handle = -1
    end
  end

  # A pre-digested decompression dictionary (`ZSTD_DDict`).
  class DDict
    def initialize(dict)
      dict = Zstd.__string_value(dict)
      @handle = String.__zstd_ddict_new(dict)
      ObjectSpace.define_finalizer(self, Zstd.__handle_finalizer(:dict, @handle))
    end
    private :initialize

    def initialize_copy(other)
      raise RuntimeError, "CDict cannot be duplicated"
    end

    def __handle # :nodoc:
      @handle
    end

    def __free # :nodoc:
      ObjectSpace.undefine_finalizer(self)
      String.__zstd_dict_free(@handle)
      @handle = -1
    end
  end

  # A streaming compressor over one `ZSTD_CCtx`.
  class StreamingCompress
    CONTINUE = 0
    FLUSH = 1
    const_set(:END, 2) # `END` is a keyword, so no bare assignment

    def initialize(level_in_args = nil, **kwargs)
      level, dict = Zstd.__compress_params(level_in_args, kwargs)
      @dict = kwargs[:dict]
      @handle = String.__zstd_cstream_new(level, dict)
      ObjectSpace.define_finalizer(self, Zstd.__handle_finalizer(:stream, @handle))
    end

    def compress(src)
      src = Zstd.__string_value(src)
      String.__zstd_cstream_run(@handle, src, CONTINUE)
    end

    # Takes the strings in; what libzstd emits meanwhile is dropped, as the
    # extension does, and the rest comes out of `flush` / `finish`.
    def write(*args)
      total = 0
      args.each do |str|
        str = Zstd.__string_value(str)
        String.__zstd_cstream_run(@handle, str, CONTINUE)
        total += str.bytesize
      end
      total
    end

    def <<(obj)
      write(obj.to_s)
      self
    end

    def print(*args)
      args = [$_] if args.empty?
      args.each_with_index do |a, i|
        write($,) if i > 0 && $,
        write(a.to_s)
      end
      write($\) if $\
      nil
    end

    def puts(*args)
      if args.empty?
        write("\n")
        return nil
      end
      args.flatten.each do |a|
        s = a.to_s
        write(s)
        write("\n") unless s.end_with?("\n")
      end
      nil
    end

    def printf(fmt, *args)
      write(format(fmt, *args))
      nil
    end

    def flush
      String.__zstd_cstream_run(@handle, "".b, FLUSH)
    end

    def finish
      String.__zstd_cstream_run(@handle, "".b, 2) # ZSTD_e_end
    end
  end

  # A streaming decompressor over one `ZSTD_DCtx`.
  class StreamingDecompress
    def initialize(**kwargs)
      dict = Zstd.__decompress_params(kwargs)
      @dict = kwargs[:dict]
      @handle = String.__zstd_dstream_new(dict)
      ObjectSpace.define_finalizer(self, Zstd.__handle_finalizer(:stream, @handle))
    end

    def decompress(src)
      src = Zstd.__string_value(src)
      String.__zstd_dstream_run(@handle, src)
    end
  end
end
