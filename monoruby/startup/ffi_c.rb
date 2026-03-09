# ffi_c.rb – monoruby replacement for ffi_c.so (the C extension of the ffi gem)
#
# When monoruby finds ffi_c.so in the load path it loads this file instead.
# This file defines all Ruby-visible classes / constants that ffi_c.so would
# have provided, using builtin primitives registered directly on the FFI module
# by the Rust backend (src/builtins/ffi.rs).
#
# The pure-Ruby files shipped with the ffi gem
# (lib/ffi/*.rb – library.rb, struct.rb, pointer.rb, …) are loaded afterwards
# and add further methods to these base classes.

module FFI
  VERSION = '1.17.0'  # reported version – purely informational

  # =========================================================================
  # Type codes – integer constants matching the Rust backend (builtins/ffi.rs)
  # =========================================================================
  TYPE_VOID       =  0
  TYPE_VOIDP      = -1
  TYPE_CHAR       = -2
  TYPE_UCHAR      = -3
  TYPE_SHORT      = -4
  TYPE_USHORT     = -5
  TYPE_INT        = -6
  TYPE_UINT       = -7
  TYPE_LONG       = -8
  TYPE_ULONG      = -9
  TYPE_LONG_LONG  = -10
  TYPE_ULONG_LONG = -11
  TYPE_FLOAT      = -12
  TYPE_DOUBLE     = -13
  TYPE_BOOL       = -14
  TYPE_INTPTR_T   = -15
  TYPE_UINTPTR_T  = -16
  TYPE_PTRDIFF_T  = -17
  TYPE_SIZE_T     = -18
  TYPE_SSIZE_T    = -19

  # =========================================================================
  # FFI::Platform – minimal constants needed before the gem's platform.rb
  # The gem's lib/ffi/platform.rb will add/override additional constants.
  # =========================================================================
  module Platform
    ADDRESS_SIZE    = 64
    LONG_SIZE       = 64
  end

  # =========================================================================
  # Stubs expected by the gem's Ruby files
  # =========================================================================
  def self._async_cb_dispatcher_atfork_child; end

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
      # type_code: integer constant used when calling FFI.___call
      attr_reader :type_code

      def initialize(name, size, alignment, type_code)
        super(name, size, alignment)
        @type_code = type_code
      end
    end

    # ---------- built-in type constants ------------------------------------

    VOID     = Builtin.new('void',      0, 1,  TYPE_VOID)
    BOOL     = Builtin.new('bool',      4, 4,  TYPE_BOOL)

    INT8     = Builtin.new('int8',      1, 1,  TYPE_CHAR)
    UINT8    = Builtin.new('uint8',     1, 1,  TYPE_UCHAR)
    INT16    = Builtin.new('int16',     2, 2,  TYPE_SHORT)
    UINT16   = Builtin.new('uint16',    2, 2,  TYPE_USHORT)
    INT32    = Builtin.new('int32',     4, 4,  TYPE_INT)
    UINT32   = Builtin.new('uint32',    4, 4,  TYPE_UINT)
    INT64    = Builtin.new('int64',     8, 8,  TYPE_LONG_LONG)
    UINT64   = Builtin.new('uint64',    8, 8,  TYPE_ULONG_LONG)
    FLOAT32  = Builtin.new('float',     4, 4,  TYPE_FLOAT)
    FLOAT64  = Builtin.new('double',    8, 8,  TYPE_DOUBLE)
    POINTER  = Builtin.new('pointer',   8, 8,  TYPE_VOIDP)
    STRING   = Builtin.new('string',    8, 8,  TYPE_VOIDP)  # char*

    LONG     = Builtin.new('long',      8, 8,  TYPE_LONG)
    ULONG    = Builtin.new('ulong',     8, 8,  TYPE_ULONG)
    LONG_LONG = INT64
    ULONG_LONG = UINT64

    # long double – on x86-64 Linux, sizeof(long double) == 16 (80-bit extended, 16-byte aligned)
    # We map it to TYPE_DOUBLE (precision loss) since Ruby Float is a C double.
    LONGDOUBLE = Builtin.new('long_double', 16, 16, TYPE_DOUBLE)

    # Variadic marker – used by attach_function to detect varargs
    VARARGS = Builtin.new('varargs', 0, 1, TYPE_VOID)

    # C convenience aliases
    CHAR     = INT8
    UCHAR    = UINT8
    SHORT    = INT16
    USHORT   = UINT16
    INT      = INT32
    UINT     = UINT32
    FLOAT    = FLOAT32
    DOUBLE   = FLOAT64

    BUFFER_IN    = POINTER
    BUFFER_OUT   = POINTER
    BUFFER_INOUT = POINTER

    # Type::Mapped – wraps a DataConverter to add to_native / from_native
    class Mapped < Type
      attr_reader :converter

      def initialize(converter)
        @converter = converter
        nt = converter.native_type
        super("mapped(#{nt.name})", nt.size, nt.alignment, nt)
      end

      def to_native(value, ctx = nil)
        @converter.to_native(value, ctx)
      end

      def from_native(value, ctx = nil)
        @converter.from_native(value, ctx)
      end
    end

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
      long_double: LONGDOUBLE,
      buffer_in:  BUFFER_IN,
      buffer_out: BUFFER_OUT,
      buffer_inout: BUFFER_INOUT,
      varargs:    VARARGS,
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

    def get_int8(offset);    FFI.___read(@address + offset, TYPE_CHAR);       end
    def get_uint8(offset);   FFI.___read(@address + offset, TYPE_UCHAR);      end
    def get_int16(offset);   FFI.___read(@address + offset, TYPE_SHORT);      end
    def get_uint16(offset);  FFI.___read(@address + offset, TYPE_USHORT);     end
    def get_int32(offset);   FFI.___read(@address + offset, TYPE_INT);        end
    def get_uint32(offset);  FFI.___read(@address + offset, TYPE_UINT);       end
    def get_int64(offset);   FFI.___read(@address + offset, TYPE_LONG_LONG);  end
    def get_uint64(offset);  FFI.___read(@address + offset, TYPE_ULONG_LONG); end
    def get_float32(offset); FFI.___read(@address + offset, TYPE_FLOAT);      end
    def get_float64(offset); FFI.___read(@address + offset, TYPE_DOUBLE);     end
    def get_long(offset);    FFI.___read(@address + offset, TYPE_LONG);       end
    def get_ulong(offset);   FFI.___read(@address + offset, TYPE_ULONG);      end

    def get_pointer(offset)
      addr = FFI.___read(@address + offset, TYPE_VOIDP)
      FFI::Pointer.new(addr)
    end

    def get_string(offset, length = nil)
      if length
        FFI.___read_bytes(@address + offset, length)
      else
        FFI.___read_string(@address + offset)
      end
    end

    def get_bytes(offset, length)
      FFI.___read_bytes(@address + offset, length)
    end

    # ---- typed writers ----

    def put_int8(offset, v);    FFI.___write(@address + offset, TYPE_CHAR,       v); end
    def put_uint8(offset, v);   FFI.___write(@address + offset, TYPE_UCHAR,      v); end
    def put_int16(offset, v);   FFI.___write(@address + offset, TYPE_SHORT,      v); end
    def put_uint16(offset, v);  FFI.___write(@address + offset, TYPE_USHORT,     v); end
    def put_int32(offset, v);   FFI.___write(@address + offset, TYPE_INT,        v); end
    def put_uint32(offset, v);  FFI.___write(@address + offset, TYPE_UINT,       v); end
    def put_int64(offset, v);   FFI.___write(@address + offset, TYPE_LONG_LONG,  v); end
    def put_uint64(offset, v);  FFI.___write(@address + offset, TYPE_ULONG_LONG, v); end
    def put_float32(offset, v); FFI.___write(@address + offset, TYPE_FLOAT,      v); end
    def put_float64(offset, v); FFI.___write(@address + offset, TYPE_DOUBLE,     v); end
    def put_long(offset, v);    FFI.___write(@address + offset, TYPE_LONG,       v); end
    def put_ulong(offset, v);   FFI.___write(@address + offset, TYPE_ULONG,      v); end

    def put_pointer(offset, ptr)
      FFI.___write(@address + offset, TYPE_VOIDP, ptr.to_i)
    end

    def put_bytes(offset, str, start = 0, length = str.bytesize - start)
      FFI.___write_bytes(@address + offset, str[start, length])
    end

    def put_string(offset, str)
      FFI.___write_bytes(@address + offset, str + "\x00")
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
      FFI.___free(@address)
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
      addr = FFI.___malloc(total, true)
      raise NoMemoryError, "FFI::MemoryPointer malloc(#{total}) failed" if addr == 0
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
      FFI.___free(@address) if @address != 0
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
    RTLD_LAZY   = 1
    RTLD_NOW    = 2
    RTLD_GLOBAL = 256
    RTLD_LOCAL  = 0

    attr_reader :name

    def initialize(name, flags)
      @name   = name
      @handle = FFI.___dlopen(name, flags)
      if @handle.nil? || @handle == 0
        raise LoadError, "Could not open library '#{name}'"
      end
    end

    # Returns an FFI::DynamicLibrary::Symbol or nil
    def find_function(name)
      ptr = FFI.___dlsym(@handle, name.to_s)
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
        convert_arg(type, arg)
      }
      type_codes = @param_types.map(&:type_code)
      ret_code   = @return_type.type_code

      result = FFI.___call(@address, converted_args, type_codes, ret_code)
      convert_result(@return_type, result)
    end

    def arity
      @param_types.length
    end

    def free
      # no-op for non-closure functions
    end

    private

    def convert_arg(type, arg)
      if type.equal?(FFI::Type::STRING)
        # Pass Ruby String as raw char* pointer
        arg.is_a?(String) ? arg : arg.to_s
      elsif arg.is_a?(FFI::Pointer)
        arg.address
      else
        arg
      end
    end

    def convert_result(type, result)
      if type.equal?(FFI::Type::VOID)
        nil
      elsif type.equal?(FFI::Type::STRING)
        return nil if result.nil? || result == 0
        FFI.___read_string(result)
      elsif type.equal?(FFI::Type::POINTER)
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
      FFI.___call(@func, args, type_codes, ret_code)
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

    # Base field class – mirrors FFI::StructLayout::Field from ffi_c.so.
    # Constructor: Field.new(name, offset, type)
    class Field
      attr_reader :name, :offset, :type

      def initialize(name, offset, type)
        @name   = name
        @offset = offset
        @type   = type
      end

      def size
        @type.size
      end

      def alignment
        @type.alignment
      end
    end

    # Primitive numeric fields (int8 … float64)
    class Number  < Field; end
    # Pointer-sized fields
    class Pointer < Field; end
    # char* / string fields
    class String  < Field; end
    # Function pointer fields
    class Function < Field; end
    # Inline array fields
    class Array   < Field; end
    # Enum fields – also defined with get/put in struct_layout.rb loaded by gem
    class Enum    < Field; end
    # Nested struct fields – also defined with get/put in struct_layout.rb
    class InnerStruct < Field; end
    # Mapped (DataConverter) fields – also defined in struct_layout.rb
    class Mapped  < Field
      def initialize(name, offset, type, orig_field)
        @orig_field = orig_field
        super(name, offset, type)
      end
    end

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
          fields << StructLayout::Field.new(name, offset, type)
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
          addr = FFI.___malloc(size, true)
          raise NoMemoryError, "FFI::Struct malloc(#{size}) failed" if addr == 0
          obj.instance_variable_set(:@address, addr)
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
        FFI.___free(@address)
        @address = 0
      end
    end

    private

    def read_field(field)
      FFI.___read(@address + field.offset, field.type.type_code)
    end

    def write_field(field, value)
      FFI.___write(@address + field.offset, field.type.type_code, value)
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
          fields << StructLayout::Field.new(name, 0, type)
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
  TypeDefs = FFI::Type::NAMES.dup

  # =========================================================================
  # Module-level helpers used by FFI::Library
  # =========================================================================

  def self.find_type(name, type_map = nil)
    FFI::Type.find(name, type_map)
  end

  def self.type_size(type)
    FFI::Type.find(type).size
  end

  # =========================================================================
  # FFI::NativeType – alias module for Type constants (used by library.rb)
  # =========================================================================
  module NativeType
    VOID       = Type::VOID
    BOOL       = Type::BOOL
    INT8       = Type::INT8
    UINT8      = Type::UINT8
    INT16      = Type::INT16
    UINT16     = Type::UINT16
    INT32      = Type::INT32
    UINT32     = Type::UINT32
    INT64      = Type::INT64
    UINT64     = Type::UINT64
    FLOAT32    = Type::FLOAT32
    FLOAT64    = Type::FLOAT64
    POINTER    = Type::POINTER
    STRING     = Type::STRING
    LONG       = Type::LONG
    ULONG      = Type::ULONG
    LONGDOUBLE = Type::LONGDOUBLE
    VARARGS    = Type::VARARGS
    BUFFER_IN  = Type::BUFFER_IN
    BUFFER_OUT = Type::BUFFER_OUT
    BUFFER_INOUT = Type::BUFFER_INOUT
  end

  # =========================================================================
  # FFI::FunctionType – function/callback type descriptor
  # Also aliased as CallbackInfo, FunctionInfo, and Type::Function (matching ffi_c.so behaviour)
  # =========================================================================
  class FunctionType < Type
    attr_reader :return_type, :param_types

    def initialize(return_type, param_types, options = {})
      @return_type = return_type
      @param_types = param_types
      super('callback', 8, 8)
    end
  end

  # Aliases registered by the C extension
  CallbackInfo = FunctionType
  FunctionInfo = FunctionType

  # FFI::Type::Function = FFI::FunctionType  (used by struct_layout_builder.rb)
  class Type
    Function = ::FFI::FunctionType
  end

  class NotFoundError < LoadError; end
end
