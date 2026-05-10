# Fiddle - Foreign Function Interface for Ruby
#
# This implementation mirrors CRuby's Fiddle standard library.
# Low-level primitives (___call, ___read, ___write, etc.) are provided
# by the Rust builtin module in src/builtins/fiddle.rs.
# dlopen / dlsym / malloc are still delegated to Kernel.___dlopen etc.
#
# TYPE_* constants are defined at the top level of Fiddle below,
# mirrored from Fiddle::Types via const_set.
# Internal code may also use Fiddle::Types::* directly.

module Fiddle
  # ---------------------------------------------------------------------------
  # Fiddle::Types — authoritative type-code constants.
  #
  # Uses CRuby's Fiddle convention: each canonical type has a small positive
  # integer code, and the unsigned variant is its arithmetic negation. Code
  # written for CRuby (e.g. opengl-bindings2) commonly writes
  # `-Fiddle::TYPE_INT` to mean "unsigned int", which only works under this
  # convention. The Rust backend (src/builtins/fiddle.rs) decodes the same
  # values.
  # ---------------------------------------------------------------------------
  module Types
    VOID       =  0
    VOIDP      =  1
    CHAR       =  2
    SHORT      =  3
    INT        =  4
    LONG       =  5
    LONG_LONG  =  6
    FLOAT      =  7
    DOUBLE     =  8

    # Unsigned variants: the negation of the corresponding signed code.
    UCHAR      = -CHAR
    USHORT     = -SHORT
    UINT       = -INT
    ULONG      = -LONG
    ULONG_LONG = -LONG_LONG

    # Platform aliases — on x86-64 Linux these all resolve to LONG/ULONG
    # (8-byte) so the Rust backend's match arms collapse cleanly.
    INTPTR_T   = LONG
    UINTPTR_T  = ULONG
    PTRDIFF_T  = LONG
    SIZE_T     = ULONG
    SSIZE_T    = LONG

    # CRuby's Fiddle (1.1) doesn't expose TYPE_BOOL but FFI's `:bool`
    # arg goes through Fiddle. Picked an unused positive value (9) so it
    # doesn't clash with the canonical signed/unsigned codes above.
    BOOL       =  9
  end

  # ---------------------------------------------------------------------------
  # Top-level TYPE_* constants (mirrors CRuby's fiddle gem behaviour)
  # ---------------------------------------------------------------------------
  Types.constants.each do |name|
    const_set("TYPE_#{name}", Types.const_get(name))
  end

  # WINDOWS flag (always false on Linux)
  WINDOWS = false

  # ---------------------------------------------------------------------------
  # ALIGN_* — C type alignments for x86-64 Linux. fiddle/pack.rb relies on
  # them. SIZEOF_* counterparts come from src/builtins/fiddle.rs.
  # ---------------------------------------------------------------------------
  ALIGN_VOIDP         = 8
  ALIGN_CHAR          = 1
  ALIGN_SHORT         = 2
  ALIGN_INT           = 4
  ALIGN_LONG          = 8
  ALIGN_LONG_LONG     = 8
  ALIGN_FLOAT         = 4
  ALIGN_DOUBLE        = 8
  ALIGN_BOOL          = 1
  ALIGN_INTPTR_T      = 8
  ALIGN_UINTPTR_T     = 8
  ALIGN_PTRDIFF_T     = 8
  ALIGN_SIZE_T        = 8
  ALIGN_SSIZE_T       = 8

  # ---------------------------------------------------------------------------
  # Errors
  # ---------------------------------------------------------------------------
  class Error < StandardError; end
  class DLError < Error; end

  # ---------------------------------------------------------------------------
  # Fiddle::Pointer – wraps a raw memory address
  # ---------------------------------------------------------------------------
  class Pointer
    # CRuby's Fiddle::Pointer exposes `size` both for read and write —
    # fiddle/struct.rb's CStructEntity does `undef_method :size=` which
    # requires the writer to exist. We follow the same shape.
    attr_accessor :size

    def initialize(addr, size = 0)
      @ptr  = addr.respond_to?(:to_i) ? addr.to_i : 0
      @size = size
    end

    # -- address accessors ---------------------------------------------------

    def to_i
      @ptr
    end
    alias address to_i

    def to_ptr
      self
    end

    def null?
      @ptr == 0
    end

    # -- pointer arithmetic --------------------------------------------------

    def +(offset)
      Fiddle::Pointer.new(@ptr + offset, @size)
    end

    def -(other)
      if other.is_a?(Fiddle::Pointer)
        @ptr - other.to_i
      else
        Fiddle::Pointer.new(@ptr - other, @size)
      end
    end

    def ==(other)
      other.is_a?(Fiddle::Pointer) ? @ptr == other.to_i : @ptr == other
    end

    # -- memory read ---------------------------------------------------------

    def [](offset, len = nil)
      if len
        Fiddle.___read_bytes(@ptr + offset, len)
      else
        Fiddle.___read(@ptr + offset, Fiddle::Types::UCHAR).chr
      end
    end

    # -- memory write --------------------------------------------------------

    def []=(offset, len_or_str, str = nil)
      bytes = str.nil? ? len_or_str : str
      Fiddle.___write_bytes(@ptr + offset, bytes)
    end

    # -- typed reads ---------------------------------------------------------

    def read_int8;    Fiddle.___read(@ptr, Fiddle::Types::CHAR);       end
    def read_uint8;   Fiddle.___read(@ptr, Fiddle::Types::UCHAR);      end
    def read_int16;   Fiddle.___read(@ptr, Fiddle::Types::SHORT);      end
    def read_uint16;  Fiddle.___read(@ptr, Fiddle::Types::USHORT);     end
    def read_int32;   Fiddle.___read(@ptr, Fiddle::Types::INT);        end
    def read_uint32;  Fiddle.___read(@ptr, Fiddle::Types::UINT);       end
    def read_int64;   Fiddle.___read(@ptr, Fiddle::Types::LONG_LONG);  end
    def read_uint64;  Fiddle.___read(@ptr, Fiddle::Types::ULONG_LONG); end
    def read_float;   Fiddle.___read(@ptr, Fiddle::Types::FLOAT);      end
    def read_double;  Fiddle.___read(@ptr, Fiddle::Types::DOUBLE);     end

    def read_pointer
      Fiddle::Pointer.new(Fiddle.___read(@ptr, Fiddle::Types::VOIDP))
    end

    def read_string(len = nil)
      len ? Fiddle.___read_bytes(@ptr, len) : Fiddle.___read_string(@ptr)
    end

    # -- typed writes --------------------------------------------------------

    def write_int8(v);    Fiddle.___write(@ptr, Fiddle::Types::CHAR,       v); end
    def write_uint8(v);   Fiddle.___write(@ptr, Fiddle::Types::UCHAR,      v); end
    def write_int16(v);   Fiddle.___write(@ptr, Fiddle::Types::SHORT,      v); end
    def write_uint16(v);  Fiddle.___write(@ptr, Fiddle::Types::USHORT,     v); end
    def write_int32(v);   Fiddle.___write(@ptr, Fiddle::Types::INT,        v); end
    def write_uint32(v);  Fiddle.___write(@ptr, Fiddle::Types::UINT,       v); end
    def write_int64(v);   Fiddle.___write(@ptr, Fiddle::Types::LONG_LONG,  v); end
    def write_uint64(v);  Fiddle.___write(@ptr, Fiddle::Types::ULONG_LONG, v); end
    def write_float(v);   Fiddle.___write(@ptr, Fiddle::Types::FLOAT,      v); end
    def write_double(v);  Fiddle.___write(@ptr, Fiddle::Types::DOUBLE,     v); end

    def write_pointer(ptr_val)
      Fiddle.___write(@ptr, Fiddle::Types::VOIDP, ptr_val.to_i)
    end

    def write_string(str, len = str.bytesize)
      Fiddle.___write_bytes(@ptr, str[0, len])
    end

    # -- get_*/put_* API -----------------------------------------------------

    def get_int8(offset = 0);    Fiddle.___read(@ptr + offset, Fiddle::Types::CHAR);       end
    def get_uint8(offset = 0);   Fiddle.___read(@ptr + offset, Fiddle::Types::UCHAR);      end
    def get_int16(offset = 0);   Fiddle.___read(@ptr + offset, Fiddle::Types::SHORT);      end
    def get_uint16(offset = 0);  Fiddle.___read(@ptr + offset, Fiddle::Types::USHORT);     end
    def get_int32(offset = 0);   Fiddle.___read(@ptr + offset, Fiddle::Types::INT);        end
    def get_uint32(offset = 0);  Fiddle.___read(@ptr + offset, Fiddle::Types::UINT);       end
    def get_int64(offset = 0);   Fiddle.___read(@ptr + offset, Fiddle::Types::LONG_LONG);  end
    def get_uint64(offset = 0);  Fiddle.___read(@ptr + offset, Fiddle::Types::ULONG_LONG); end
    def get_float(offset = 0);   Fiddle.___read(@ptr + offset, Fiddle::Types::FLOAT);      end
    def get_double(offset = 0);  Fiddle.___read(@ptr + offset, Fiddle::Types::DOUBLE);     end

    def get_pointer(offset = 0)
      Fiddle::Pointer.new(Fiddle.___read(@ptr + offset, Fiddle::Types::VOIDP))
    end

    def get_bytes(offset, len)
      Fiddle.___read_bytes(@ptr + offset, len)
    end

    def get_string(offset = 0, len = nil)
      len ? Fiddle.___read_bytes(@ptr + offset, len) : Fiddle.___read_string(@ptr + offset)
    end

    def put_int8(offset, v);    Fiddle.___write(@ptr + offset, Fiddle::Types::CHAR,       v); end
    def put_uint8(offset, v);   Fiddle.___write(@ptr + offset, Fiddle::Types::UCHAR,      v); end
    def put_int16(offset, v);   Fiddle.___write(@ptr + offset, Fiddle::Types::SHORT,      v); end
    def put_uint16(offset, v);  Fiddle.___write(@ptr + offset, Fiddle::Types::USHORT,     v); end
    def put_int32(offset, v);   Fiddle.___write(@ptr + offset, Fiddle::Types::INT,        v); end
    def put_uint32(offset, v);  Fiddle.___write(@ptr + offset, Fiddle::Types::UINT,       v); end
    def put_int64(offset, v);   Fiddle.___write(@ptr + offset, Fiddle::Types::LONG_LONG,  v); end
    def put_uint64(offset, v);  Fiddle.___write(@ptr + offset, Fiddle::Types::ULONG_LONG, v); end
    def put_float(offset, v);   Fiddle.___write(@ptr + offset, Fiddle::Types::FLOAT,      v); end
    def put_double(offset, v);  Fiddle.___write(@ptr + offset, Fiddle::Types::DOUBLE,     v); end

    def put_bytes(offset, str, start = 0, len = str.bytesize - start)
      Fiddle.___write_bytes(@ptr + offset, str[start, len])
    end

    def inspect
      "#<#{self.class} address=0x#{@ptr.to_s(16)}>"
    end
    alias to_s inspect
  end

  # Defined once, after Pointer class — no re-assignment warning.
  Pointer::NULL = Pointer.new(0)

  # ---------------------------------------------------------------------------
  # Fiddle::CParser – stub for compatibility
  # ---------------------------------------------------------------------------
  module CParser
    def parse_struct_signature(signature, tymap = nil)
      raise NotImplementedError, "Fiddle::CParser not fully supported"
    end
  end

  # ---------------------------------------------------------------------------
  # Fiddle::Handle – wraps a shared-library handle
  # ---------------------------------------------------------------------------
  class Handle
    RTLD_LAZY     = 1
    RTLD_NOW      = 2
    RTLD_GLOBAL   = 256
    RTLD_LOCAL    = 0
    RTLD_NOLOAD   = 4
    RTLD_NODELETE = 4096

    def initialize(library = nil, flags = RTLD_LAZY)
      @library = library
      @handle  = Kernel.___dlopen(library, flags)
      if @handle.nil? || @handle == 0
        raise Fiddle::DLError, "dlopen failed: #{library.inspect}"
      end
    end

    def [](name)
      ptr = Kernel.___dlsym(@handle, name.to_s)
      if ptr.nil? || ptr == 0
        raise Fiddle::DLError, "unknown symbol \"#{name}\""
      end
      ptr
    end
    alias sym []

    def sym?(name)
      ptr = Kernel.___dlsym(@handle, name.to_s)
      (ptr.nil? || ptr == 0) ? nil : ptr
    end

    def close
      nil
    end

    def to_i
      @handle
    end
  end

  # Defined once, after Handle class — no re-assignment warning.
  Handle::DEFAULT = Handle.new(nil)

  # ---------------------------------------------------------------------------
  # Fiddle::Function – callable wrapper around a C function pointer
  # ---------------------------------------------------------------------------
  class Function
    DEFAULT_ABI = :default
    # Alias used by fiddle/closure.rb
    DEFAULT = DEFAULT_ABI

    attr_reader :ptr, :return_type, :argument_types

    def initialize(ptr, args_type, ret_type, abi = DEFAULT_ABI, kwargs = {})
      @ptr            = ptr.respond_to?(:to_i) ? ptr.to_i : Integer(ptr)
      @argument_types = args_type.map { |t| resolve_type(t) }
      @return_type    = resolve_type(ret_type)
    end

    def call(*args)
      if args.length != @argument_types.length
        raise ArgumentError,
          "wrong number of arguments (given #{args.length}, expected #{@argument_types.length})"
      end
      Fiddle.___call(@ptr, args, @argument_types, @return_type)
    end

    def arity
      @argument_types.length
    end

    private

    # Accept both Integer type codes and symbols/strings like :int, :void, etc.
    def resolve_type(t)
      case t
      when Integer then t
      when Symbol, String then Fiddle::Function.type_from_symbol(t.to_sym)
      else
        raise TypeError, "invalid type: #{t.inspect}"
      end
    end

    # Use Fiddle::Types::* (always available) instead of Fiddle::TYPE_*
    # (which are set later by the gem's fiddle.rb via const_set).
    SYMBOL_TO_TYPE = {
      void:        Fiddle::Types::VOID,
      voidp:       Fiddle::Types::VOIDP,
      pointer:     Fiddle::Types::VOIDP,
      char:        Fiddle::Types::CHAR,
      uchar:       Fiddle::Types::UCHAR,
      short:       Fiddle::Types::SHORT,
      ushort:      Fiddle::Types::USHORT,
      int:         Fiddle::Types::INT,
      int8:        Fiddle::Types::CHAR,
      int16:       Fiddle::Types::SHORT,
      int32:       Fiddle::Types::INT,
      int64:       Fiddle::Types::LONG_LONG,
      uint:        Fiddle::Types::UINT,
      uint8:       Fiddle::Types::UCHAR,
      uint16:      Fiddle::Types::USHORT,
      uint32:      Fiddle::Types::UINT,
      uint64:      Fiddle::Types::ULONG_LONG,
      long:        Fiddle::Types::LONG,
      ulong:       Fiddle::Types::ULONG,
      long_long:   Fiddle::Types::LONG_LONG,
      ulong_long:  Fiddle::Types::ULONG_LONG,
      float:       Fiddle::Types::FLOAT,
      float32:     Fiddle::Types::FLOAT,
      double:      Fiddle::Types::DOUBLE,
      float64:     Fiddle::Types::DOUBLE,
      bool:        Fiddle::Types::BOOL,
      string:      Fiddle::Types::VOIDP,   # char* treated as pointer
      size_t:      Fiddle::Types::SIZE_T,
      ssize_t:     Fiddle::Types::SSIZE_T,
      ptrdiff_t:   Fiddle::Types::PTRDIFF_T,
      intptr_t:    Fiddle::Types::INTPTR_T,
      uintptr_t:   Fiddle::Types::UINTPTR_T,
    }.freeze

    def self.type_from_symbol(sym)
      SYMBOL_TO_TYPE.fetch(sym) do
        raise TypeError, "unknown type symbol: #{sym.inspect}"
      end
    end
  end

  # ---------------------------------------------------------------------------
  # Fiddle::Closure – callback (proc called from C)
  # Minimal stub: real closure support requires mmap + trampoline generation.
  # ---------------------------------------------------------------------------
  class Closure
    # fiddle/closure.rb reads these attributes.
    attr_reader :ctype, :args

    def initialize(ret, args, abi = :default)
      @ctype = ret
      @args  = args
      raise NotImplementedError, "Fiddle::Closure is not yet supported"
    end

    def to_i
      0
    end
  end

  # ---------------------------------------------------------------------------
  # Module-level convenience methods
  # ---------------------------------------------------------------------------

  def self.dlopen(library, flags = Fiddle::Handle::RTLD_LAZY)
    Fiddle::Handle.new(library, flags)
  end

  def self.malloc(size)
    ptr = Kernel.___malloc(size, true)
    raise Fiddle::DLError, "malloc(#{size}) failed" if ptr == 0
    ptr
  end

  def self.free(ptr)
    Fiddle.___free(ptr.respond_to?(:to_i) ? ptr.to_i : ptr)
  end

  def self.realloc(ptr, size)
    raise NotImplementedError, "Fiddle.realloc is not yet supported"
  end
end
