# ffi_c.rb – monoruby replacement for ffi_c.so (the C extension of the ffi gem)
#
# When monoruby finds ffi_c.so in the load path it loads this file instead.
# This file defines all Ruby-visible classes / constants that ffi_c.so would
# have provided, building on top of Fiddle (which is already built-in).
#
# The pure-Ruby files shipped with the ffi gem
# (lib/ffi/*.rb – library.rb, struct.rb, pointer.rb, …) are loaded afterwards
# and add further methods to these base classes.

require 'fiddle'

module FFI
  VERSION = '1.17.0'  # reported version – purely informational

  # =========================================================================
  # FFI::Platform
  # =========================================================================
  module Platform
    BYTE_ORDER      = :little
    ADDRESS_SIZE    = 64
    LONG_SIZE       = 64
    INT_SIZE        = 32
    FLOAT_SIZE      = 32
    DOUBLE_SIZE     = 64
    OS              = 'linux'
    CPU             = 'x86_64'
    ARCH            = 'x86_64'
    IS_LITTLE_ENDIAN = true
    IS_BIG_ENDIAN   = false
    IS_WINDOWS      = false
    NULL            = 0

    LIBSUFFIX       = 'so'
    LIBPREFIX       = 'lib'
  end

  # =========================================================================
  # FFI::Type – type descriptor objects
  # =========================================================================
  class Type
    attr_reader :name, :size, :alignment, :native_type

    def initialize(name, size, alignment, native_type = nil)
      @name        = name.freeze
      @size        = size
      @alignment   = alignment
      @native_type = native_type || name
    end

    def to_s;   @name; end
    def inspect; "#<FFI::Type #{@name}>"; end

    # Subclass used for every built-in primitive type
    class Builtin < Type
      # type_code: Fiddle TYPE_* constant used when calling Fiddle.___call
      attr_reader :type_code

      def initialize(name, size, alignment, type_code)
        super(name, size, alignment)
        @type_code = type_code
      end
    end

    # ---------- built-in type constants ------------------------------------

    VOID     = Builtin.new('void',      0, 1,  Fiddle::TYPE_VOID)
    BOOL     = Builtin.new('bool',      4, 4,  Fiddle::TYPE_BOOL)

    INT8     = Builtin.new('int8',      1, 1,  Fiddle::TYPE_CHAR)
    UINT8    = Builtin.new('uint8',     1, 1,  Fiddle::TYPE_UCHAR)
    INT16    = Builtin.new('int16',     2, 2,  Fiddle::TYPE_SHORT)
    UINT16   = Builtin.new('uint16',    2, 2,  Fiddle::TYPE_USHORT)
    INT32    = Builtin.new('int32',     4, 4,  Fiddle::TYPE_INT)
    UINT32   = Builtin.new('uint32',    4, 4,  Fiddle::TYPE_UINT)
    INT64    = Builtin.new('int64',     8, 8,  Fiddle::TYPE_LONG_LONG)
    UINT64   = Builtin.new('uint64',    8, 8,  Fiddle::TYPE_ULONG_LONG)
    FLOAT32  = Builtin.new('float',     4, 4,  Fiddle::TYPE_FLOAT)
    FLOAT64  = Builtin.new('double',    8, 8,  Fiddle::TYPE_DOUBLE)
    POINTER  = Builtin.new('pointer',   8, 8,  Fiddle::TYPE_VOIDP)
    STRING   = Builtin.new('string',    8, 8,  Fiddle::TYPE_VOIDP)  # char*

    LONG     = Builtin.new('long',      8, 8,  Fiddle::TYPE_LONG)
    ULONG    = Builtin.new('ulong',     8, 8,  Fiddle::TYPE_ULONG)
    LONGLONG = INT64
    ULONGLONG = UINT64

    # C convenience aliases
    CHAR     = INT8
    UCHAR    = UINT8
    SHORT    = INT16
    USHORT   = UINT16
    INT      = INT32
    UINT     = UINT32

    BUFFER_IN    = POINTER
    BUFFER_OUT   = POINTER
    BUFFER_INOUT = POINTER

    # Map from ffi gem symbol names to Type objects
    NAMES = {
      void:       VOID,
      bool:       BOOL,
      int8:       INT8,   char:      INT8,
      uint8:      UINT8,  uchar:     UINT8,
      int16:      INT16,  short:     INT16,
      uint16:     UINT16, ushort:    UINT16,
      int32:      INT32,  int:       INT32,
      uint32:     UINT32, uint:      UINT32,
      int64:      INT64,  long_long: INT64,  longlong:  INT64,
      uint64:     UINT64, ulong_long: UINT64, ulonglong: UINT64,
      float:      FLOAT32, float32: FLOAT32,
      double:     FLOAT64, float64: FLOAT64,
      pointer:    POINTER, voidp:   POINTER,
      string:     STRING,
      long:       LONG,
      ulong:      ULONG,
      buffer_in:  BUFFER_IN,
      buffer_out: BUFFER_OUT,
      buffer_inout: BUFFER_INOUT,
    }.freeze

    def self.[](name)
      NAMES[name] or raise TypeError, "unknown FFI type: #{name.inspect}"
    end

    # Resolve a type descriptor to an FFI::Type
    def self.find(type, type_map = nil)
      return type if type.is_a?(FFI::Type)
      if type_map
        mapped = type_map[type]
        return find(mapped, nil) if mapped
      end
      NAMES.fetch(type.to_sym) { raise TypeError, "unknown type '#{type}'" }
    end
  end

  # =========================================================================
  # FFI::AbstractMemory – base class for memory-backed objects
  # =========================================================================
  class AbstractMemory
    attr_reader :size, :type_size

    def initialize(address, size = 0)
      @address   = address.respond_to?(:to_i) ? address.to_i : 0
      @size      = size
      @type_size = 1
    end

    def address
      @address
    end
    alias to_i address

    def null?
      @address == 0
    end

    # ---- typed readers ----

    def get_int8(offset);    Fiddle.___read(@address + offset, Fiddle::TYPE_CHAR);       end
    def get_uint8(offset);   Fiddle.___read(@address + offset, Fiddle::TYPE_UCHAR);      end
    def get_int16(offset);   Fiddle.___read(@address + offset, Fiddle::TYPE_SHORT);      end
    def get_uint16(offset);  Fiddle.___read(@address + offset, Fiddle::TYPE_USHORT);     end
    def get_int32(offset);   Fiddle.___read(@address + offset, Fiddle::TYPE_INT);        end
    def get_uint32(offset);  Fiddle.___read(@address + offset, Fiddle::TYPE_UINT);       end
    def get_int64(offset);   Fiddle.___read(@address + offset, Fiddle::TYPE_LONG_LONG);  end
    def get_uint64(offset);  Fiddle.___read(@address + offset, Fiddle::TYPE_ULONG_LONG); end
    def get_float32(offset); Fiddle.___read(@address + offset, Fiddle::TYPE_FLOAT);      end
    def get_float64(offset); Fiddle.___read(@address + offset, Fiddle::TYPE_DOUBLE);     end
    def get_long(offset);    Fiddle.___read(@address + offset, Fiddle::TYPE_LONG);       end
    def get_ulong(offset);   Fiddle.___read(@address + offset, Fiddle::TYPE_ULONG);      end

    def get_pointer(offset)
      addr = Fiddle.___read(@address + offset, Fiddle::TYPE_VOIDP)
      FFI::Pointer.new(addr)
    end

    def get_string(offset, length = nil)
      if length
        Fiddle.___read_bytes(@address + offset, length)
      else
        Fiddle.___read_string(@address + offset)
      end
    end

    def get_bytes(offset, length)
      Fiddle.___read_bytes(@address + offset, length)
    end

    # ---- typed writers ----

    def put_int8(offset, v);    Fiddle.___write(@address + offset, Fiddle::TYPE_CHAR,       v); end
    def put_uint8(offset, v);   Fiddle.___write(@address + offset, Fiddle::TYPE_UCHAR,      v); end
    def put_int16(offset, v);   Fiddle.___write(@address + offset, Fiddle::TYPE_SHORT,      v); end
    def put_uint16(offset, v);  Fiddle.___write(@address + offset, Fiddle::TYPE_USHORT,     v); end
    def put_int32(offset, v);   Fiddle.___write(@address + offset, Fiddle::TYPE_INT,        v); end
    def put_uint32(offset, v);  Fiddle.___write(@address + offset, Fiddle::TYPE_UINT,       v); end
    def put_int64(offset, v);   Fiddle.___write(@address + offset, Fiddle::TYPE_LONG_LONG,  v); end
    def put_uint64(offset, v);  Fiddle.___write(@address + offset, Fiddle::TYPE_ULONG_LONG, v); end
    def put_float32(offset, v); Fiddle.___write(@address + offset, Fiddle::TYPE_FLOAT,      v); end
    def put_float64(offset, v); Fiddle.___write(@address + offset, Fiddle::TYPE_DOUBLE,     v); end
    def put_long(offset, v);    Fiddle.___write(@address + offset, Fiddle::TYPE_LONG,       v); end
    def put_ulong(offset, v);   Fiddle.___write(@address + offset, Fiddle::TYPE_ULONG,      v); end

    def put_pointer(offset, ptr)
      Fiddle.___write(@address + offset, Fiddle::TYPE_VOIDP, ptr.to_i)
    end

    def put_bytes(offset, str, start = 0, length = str.bytesize - start)
      Fiddle.___write_bytes(@address + offset, str[start, length])
    end

    def put_string(offset, str)
      Fiddle.___write_bytes(@address + offset, str + "\x00")
    end

    # ---- plural reads (arrays) ----

    def get_array_of_int32(offset, count)
      Array.new(count) { |i| get_int32(offset + i * 4) }
    end

    def get_array_of_uint32(offset, count)
      Array.new(count) { |i| get_uint32(offset + i * 4) }
    end

    def get_array_of_int64(offset, count)
      Array.new(count) { |i| get_int64(offset + i * 8) }
    end

    def get_array_of_uint64(offset, count)
      Array.new(count) { |i| get_uint64(offset + i * 8) }
    end

    def get_array_of_float32(offset, count)
      Array.new(count) { |i| get_float32(offset + i * 4) }
    end

    def get_array_of_float64(offset, count)
      Array.new(count) { |i| get_float64(offset + i * 8) }
    end

    def get_array_of_pointer(offset, count)
      Array.new(count) { |i| get_pointer(offset + i * 8) }
    end

    def get_array_of_string(offset, count)
      Array.new(count) { |i| get_string(offset + i * 8) }
    end

    # ---- plural writes ----

    def put_array_of_int32(offset, ary)
      ary.each_with_index { |v, i| put_int32(offset + i * 4, v) }
    end

    def put_array_of_uint32(offset, ary)
      ary.each_with_index { |v, i| put_uint32(offset + i * 4, v) }
    end

    def put_array_of_int64(offset, ary)
      ary.each_with_index { |v, i| put_int64(offset + i * 8, v) }
    end

    def put_array_of_uint64(offset, ary)
      ary.each_with_index { |v, i| put_uint64(offset + i * 8, v) }
    end

    def put_array_of_float64(offset, ary)
      ary.each_with_index { |v, i| put_float64(offset + i * 8, v) }
    end

    def put_array_of_pointer(offset, ary)
      ary.each_with_index { |v, i| put_pointer(offset + i * 8, v) }
    end

    # read_array_of_* (no-offset convenience wrappers)
    alias read_array_of_int32   get_array_of_int32
    alias read_array_of_uint32  get_array_of_uint32
    alias read_array_of_int64   get_array_of_int64
    alias read_array_of_uint64  get_array_of_uint64
    alias read_array_of_float32 get_array_of_float32
    alias read_array_of_float64 get_array_of_float64
    alias read_array_of_pointer get_array_of_pointer
    alias read_array_of_string  get_array_of_string

    def read_array_of_int32(count);   get_array_of_int32(0, count);   end
    def read_array_of_uint32(count);  get_array_of_uint32(0, count);  end
    def read_array_of_int64(count);   get_array_of_int64(0, count);   end
    def read_array_of_uint64(count);  get_array_of_uint64(0, count);  end
    def read_array_of_float32(count); get_array_of_float32(0, count); end
    def read_array_of_float64(count); get_array_of_float64(0, count); end
    def read_array_of_pointer(count); get_array_of_pointer(0, count); end
    def read_array_of_string(count);  get_array_of_string(0, count);  end

    # no-offset read/write shortcuts
    def read_int8;    get_int8(0);    end
    def read_uint8;   get_uint8(0);   end
    def read_int16;   get_int16(0);   end
    def read_uint16;  get_uint16(0);  end
    def read_int32;   get_int32(0);   end
    def read_uint32;  get_uint32(0);  end
    def read_int64;   get_int64(0);   end
    def read_uint64;  get_uint64(0);  end
    def read_float;   get_float32(0); end
    def read_double;  get_float64(0); end
    def read_long;    get_long(0);    end
    def read_ulong;   get_ulong(0);   end
    def read_pointer; get_pointer(0); end
    def read_bytes(length); get_bytes(0, length); end
    def read_string(length = nil)
      length ? get_bytes(0, length) : get_string(0)
    end

    def write_int8(v);   put_int8(0, v);   end
    def write_uint8(v);  put_uint8(0, v);  end
    def write_int16(v);  put_int16(0, v);  end
    def write_uint16(v); put_uint16(0, v); end
    def write_int32(v);  put_int32(0, v);  end
    def write_uint32(v); put_uint32(0, v); end
    def write_int64(v);  put_int64(0, v);  end
    def write_uint64(v); put_uint64(0, v); end
    def write_float(v);  put_float32(0, v); end
    def write_double(v); put_float64(0, v); end
    def write_long(v);   put_long(0, v);   end
    def write_ulong(v);  put_ulong(0, v);  end
    def write_pointer(v); put_pointer(0, v); end
    def write_bytes(str, start = 0, length = str.bytesize - start)
      put_bytes(0, str, start, length)
    end
    def write_string(str, length = str.bytesize)
      put_bytes(0, str, 0, length)
    end

    def [](index)
      if index.is_a?(Range)
        get_bytes(index.begin, index.size)
      else
        get_uint8(index)
      end
    end

    def []=(index, value)
      put_uint8(index, value)
    end

    def +(offset)
      self.class.new(@address + offset, @size)
    end

    def ==(other)
      return false unless other.respond_to?(:address)
      @address == other.address
    end

    def inspect
      "#<#{self.class} address=0x#{@address.to_s(16)} size=#{@size}>"
    end
    alias to_s inspect
  end

  # =========================================================================
  # FFI::Pointer < AbstractMemory
  # =========================================================================
  class Pointer < AbstractMemory
    NULL = nil  # filled in below after class definition

    def initialize(type_or_address = 0, address = nil)
      if address.nil?
        addr = type_or_address.respond_to?(:to_i) ? type_or_address.to_i : 0
        size = 0
      else
        # FFI::Pointer.new(type, address)  – type gives element size
        addr = address.respond_to?(:to_i) ? address.to_i : 0
        size = type_or_address.is_a?(FFI::Type) ? type_or_address.size : 0
      end
      super(addr, size)
    end

    def to_ptr; self; end

    def +(offset)
      FFI::Pointer.new(@address + offset)
    end

    def -(other)
      if other.is_a?(FFI::Pointer)
        @address - other.address
      else
        FFI::Pointer.new(@address - other)
      end
    end

    def slice(offset, length)
      FFI::Pointer.new(@address + offset)
    end

    def autorelease=(flag); end  # stub – managed externally

    def free
      Fiddle.___free(@address)
      @address = 0
    end

    def self.size
      8  # pointer size on x86-64
    end
  end

  Pointer::NULL = Pointer.new(0)

  # =========================================================================
  # FFI::MemoryPointer < Pointer  – auto-allocated heap memory
  # =========================================================================
  class MemoryPointer < Pointer
    def initialize(type, count = 1, clear = true)
      type_obj = case type
                 when FFI::Type then type
                 when Symbol, String then FFI::Type::NAMES.fetch(type.to_sym, FFI::Type::VOID)
                 when Integer then nil
                 end
      elem_size = if type_obj
                   type_obj.size
                 elsif type.is_a?(Integer)
                   type  # treat integer as byte count per element
                 else
                   1
                 end
      total = elem_size * count
      addr = Fiddle.malloc(total)
      super(addr, total)
      @total = total
    end

    def self.new(type, count = 1, clear = true)
      ptr = super
      if block_given?
        begin
          yield ptr
        ensure
          ptr.free
        end
        nil
      else
        ptr
      end
    end

    def free
      Fiddle.___free(@address) if @address != 0
      @address = 0
    end

    def autorelease=(flag); end  # manual for now

    def inspect
      "#<#{self.class} address=0x#{@address.to_s(16)} size=#{@size}>"
    end
  end

  # =========================================================================
  # FFI::Buffer < Pointer  – fixed-size buffer (stack or heap)
  # =========================================================================
  class Buffer < MemoryPointer
    alias_method :initialize, :initialize  # same as MemoryPointer
  end

  # =========================================================================
  # FFI::DynamicLibrary – dlopen wrapper
  # =========================================================================
  class DynamicLibrary
    RTLD_LAZY   = Fiddle::Handle::RTLD_LAZY
    RTLD_NOW    = Fiddle::Handle::RTLD_NOW
    RTLD_GLOBAL = Fiddle::Handle::RTLD_GLOBAL
    RTLD_LOCAL  = Fiddle::Handle::RTLD_LOCAL

    attr_reader :name

    def initialize(name, flags)
      @name   = name
      @handle = Kernel.___dlopen(name, flags)
      if @handle.nil? || @handle == 0
        raise LoadError, "Could not open library '#{name}'"
      end
    end

    # Returns an FFI::DynamicLibrary::Symbol or nil
    def find_function(name)
      ptr = Kernel.___dlsym(@handle, name.to_s)
      return nil if ptr.nil? || ptr == 0
      Symbol.new(name.to_s, ptr)
    end

    alias find_symbol   find_function
    alias find_variable find_function

    def self.open(name, flags)
      new(name, flags)
    end

    def to_i
      @handle
    end

    # Symbol returned by find_function / find_symbol
    class Symbol < FFI::Pointer
      attr_reader :name

      def initialize(name, address)
        super(address)
        @name = name
      end

      def to_i
        @address
      end
      alias address to_i
    end
  end

  # =========================================================================
  # FFI::Function < Pointer  – callable function wrapper
  # =========================================================================
  class Function < Pointer
    attr_reader :return_type, :param_types

    def initialize(return_type, param_types, address_or_proc, options = {})
      @return_type = FFI::Type.find(return_type)
      @param_types = param_types.map { |t| FFI::Type.find(t) }

      addr = if address_or_proc.respond_to?(:to_i)
               address_or_proc.to_i
             elsif address_or_proc.is_a?(Proc) || address_or_proc.is_a?(Method)
               raise NotImplementedError, "Proc/Method FFI::Function not yet supported"
             else
               0
             end
      super(addr)
    end

    def call(*args)
      if args.length != @param_types.length
        raise ArgumentError,
          "wrong number of arguments (given #{args.length}, expected #{@param_types.length})"
      end

      converted_args = @param_types.zip(args).map { |type, arg|
        convert_arg_for_fiddle(type, arg)
      }
      type_codes = @param_types.map(&:type_code)
      ret_code   = @return_type.type_code

      result = Fiddle.___call(@address, converted_args, type_codes, ret_code)
      convert_result(@return_type, result)
    end

    def arity
      @param_types.length
    end

    def free
      # no-op for non-closure functions
    end

    private

    def convert_arg_for_fiddle(type, arg)
      case type
      when FFI::Type::STRING.class
        # Pass Ruby String as raw char* pointer
        arg.is_a?(String) ? arg : arg.to_s
      else
        if arg.is_a?(FFI::Pointer)
          arg.address
        else
          arg
        end
      end
    end

    def convert_result(type, result)
      case type
      when FFI::Type::VOID    then nil
      when FFI::Type::STRING.class
        return nil if result.nil? || result == 0
        Fiddle.___read_string(result)
      when FFI::Type::POINTER.class
        FFI::Pointer.new(result.to_i)
      else
        result
      end
    end
  end

  # =========================================================================
  # FFI::VariadicInvoker – variadic C function wrapper
  # =========================================================================
  class VariadicInvoker
    def initialize(func, param_types, return_type, options = {})
      @func         = func.respond_to?(:to_i) ? func.to_i : 0
      @param_types  = param_types.map { |t| FFI::Type.find(t) }
      @return_type  = FFI::Type.find(return_type)
    end

    def invoke(param_types, args)
      all_types = (@param_types + param_types.map { |t| FFI::Type.find(t) })
      type_codes = all_types.map(&:type_code)
      ret_code   = @return_type.type_code
      Fiddle.___call(@func, args, type_codes, ret_code)
    end

    def call(*args)
      invoke([], args)
    end
  end

  # =========================================================================
  # FFI::Closure < Pointer – callback stub
  # =========================================================================
  class Closure < Pointer
    def initialize(return_type, param_types, abi = :default, &block)
      @return_type  = FFI::Type.find(return_type)
      @param_types  = param_types.map { |t| FFI::Type.find(t) }
      @block        = block
      super(0)
      raise NotImplementedError, "FFI::Closure is not yet supported in monoruby"
    end
  end

  # =========================================================================
  # FFI::Struct / FFI::Union – basic struct layout support
  # =========================================================================

  class StructLayout
    attr_reader :size, :alignment, :fields

    Field = Struct.new(:name, :type, :offset, :size)

    def initialize(fields, size, alignment)
      @fields    = fields
      @size      = size
      @alignment = alignment
      @by_name   = fields.each_with_object({}) { |f, h| h[f.name] = f }
    end

    def [](name)
      @by_name[name]
    end

    def field_names
      @fields.map(&:name)
    end
  end

  class Struct < AbstractMemory
    class << self
      attr_reader :layout

      # Define layout via DSL: layout :x, :int32, :y, :int32
      def layout(*spec)
        fields = []
        offset = 0
        max_align = 1

        spec.each_slice(2) do |name, type_sym|
          type = FFI::Type.find(type_sym)
          align = type.alignment
          offset = (offset + align - 1) & ~(align - 1)  # align up
          fields << StructLayout::Field.new(name, type, offset, type.size)
          offset += type.size
          max_align = [max_align, align].max
        end

        total = (offset + max_align - 1) & ~(max_align - 1)
        @layout = StructLayout.new(fields, total, max_align)

        # Generate accessor methods
        fields.each do |field|
          define_method(field.name) { self[field.name] }
          define_method(:"#{field.name}=") { |v| self[field.name] = v }
        end

        @layout
      end

      def size
        @layout ? @layout.size : 0
      end

      def alignment
        @layout ? @layout.alignment : 1
      end

      def members
        @layout ? @layout.field_names : []
      end

      def new(*args)
        obj = allocate
        if args.empty?
          # allocate fresh memory
          obj.instance_variable_set(:@address, Fiddle.malloc(size))
          obj.instance_variable_set(:@size,    size)
          obj.instance_variable_set(:@owned,   true)
        else
          ptr = args.first
          obj.instance_variable_set(:@address, ptr.respond_to?(:to_i) ? ptr.to_i : 0)
          obj.instance_variable_set(:@size,    size)
          obj.instance_variable_set(:@owned,   false)
        end
        obj
      end
    end

    def [](name)
      field = self.class.layout[name]
      raise ArgumentError, "unknown field: #{name}" unless field
      read_field(field)
    end

    def []=(name, value)
      field = self.class.layout[name]
      raise ArgumentError, "unknown field: #{name}" unless field
      write_field(field, value)
    end

    def to_ptr
      FFI::Pointer.new(@address)
    end

    def members
      self.class.members
    end

    def values
      members.map { |m| self[m] }
    end

    def to_h
      members.each_with_object({}) { |m, h| h[m] = self[m] }
    end

    def free
      if @owned && @address != 0
        Fiddle.___free(@address)
        @address = 0
      end
    end

    private

    def read_field(field)
      Fiddle.___read(@address + field.offset, field.type.type_code)
    end

    def write_field(field, value)
      Fiddle.___write(@address + field.offset, field.type.type_code, value)
    end
  end

  class Union < Struct
    class << self
      def layout(*spec)
        fields = []
        max_size  = 0
        max_align = 1

        spec.each_slice(2) do |name, type_sym|
          type = FFI::Type.find(type_sym)
          fields << StructLayout::Field.new(name, type, 0, type.size)
          max_size  = [max_size, type.size].max
          max_align = [max_align, type.alignment].max
        end

        total = (max_size + max_align - 1) & ~(max_align - 1)
        @layout = StructLayout.new(fields, total, max_align)

        fields.each do |field|
          define_method(field.name) { self[field.name] }
          define_method(:"#{field.name}=") { |v| self[field.name] = v }
        end

        @layout
      end
    end
  end

  # =========================================================================
  # FFI::TypeDefs – populated from Type::NAMES
  # =========================================================================
  TypeDefs = FFI::Type::NAMES.dup.freeze

  # =========================================================================
  # Module-level helpers used by FFI::Library
  # =========================================================================

  def self.find_type(name, type_map = nil)
    FFI::Type.find(name, type_map)
  end

  def self.type_size(type)
    FFI::Type.find(type).size
  end

  class NotFoundError < LoadError; end
end
