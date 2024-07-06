#
# https://github.com/ffi/ffi/blob/ecfb225096ae76ba2a5e8115f046bd0ac23095e6/ext/ffi_c/Type.c#L305
#
module FFI
  # typedef struct _ffi_type
  # {
  #   size_t size;
  #   unsigned short alignment;
  #   unsigned short type;
  #   struct _ffi_type **elements;
  # } ffi_type;
  class FFIType
    def initialize(size, alignment, type, elements = nil)
      @size = size
      @alignment = alignment
      @type = type
      @elements = elements
    end
    attr_reader :size, :alignment

    # https://github.com/libffi/libffi/blob/9c9e8368e49804c4f7c35ac9f0d7c1d0d533308b/include/ffi.h.in#L60
    VOID = FFIType.new(0, 0, 0)
    # https://github.com/libffi/libffi/blob/9c9e8368e49804c4f7c35ac9f0d7c1d0d533308b/src/types.c#L69
    UCHAR = UINT8 = FFIType.new(1, 1, 5)
    SINT8 = FFIType.new(1, 1, 6)
    UINT16 = FFIType.new(2, 2, 7)
    SINT16 = FFIType.new(2, 2, 8)
    UINT32 = FFIType.new(4, 4, 9)
    SINT32 = FFIType.new(4, 4, 10)
    ULONG = UINT64 = FFIType.new(8, 8, 11)
    LONG = SINT64 = FFIType.new(8, 8, 12)

    POINTER = FFIType.new(8, 8, 14)
    FLOAT = FFIType.new(4, 4, 2)
    DOUBLE = FFIType.new(8, 8, 3)
    LONGDOUBLE = FFIType.new(16, 16, 4)
  end

  # https://github.com/ffi/ffi/blob/ecfb225096ae76ba2a5e8115f046bd0ac23095e6/ext/ffi_c/Type.c#L314
  # struct Type_ {
  #     NativeType nativeType;
  #     ffi_type* ffiType;
  # };

  class Type
    # https://github.com/ffi/ffi/blob/ecfb225096ae76ba2a5e8115f046bd0ac23095e6/ext/ffi_c/Type.c#L96
    def self.allocate
      obj = super
      obj.instance_variable_set(:@native_type, -1)
      obj.instance_variable_set(:@ffi_type, FFIType::VOID)
      obj
    end

    # https://github.com/ffi/ffi/blob/ecfb225096ae76ba2a5e8115f046bd0ac23095e6/ext/ffi_c/Type.c#L114
    def initialize(val)
      if val.is_a?(Integer)
        @native_type = val;
        @ffi_type = FFIType::VOID
      elsif val.is_a?(Type)
        @native_type = val.native_type
        @ffi_type = val.ffi_type
      else
        raise ArgumentError, "wrong type"
      end
    end

    attr_reader :native_type, :ffi_type

    # https://github.com/ffi/ffi/blob/ecfb225096ae76ba2a5e8115f046bd0ac23095e6/ext/ffi_c/Type.c#L172
    def inspect
      "#<#{self.class}::%p size=#{@ffi_type.size} alignment=#{@ffi_type.alignment}>"
    end

    # https://github.com/ffi/ffi/blob/ecfb225096ae76ba2a5e8115f046bd0ac23095e6/ext/ffi_c/Type.c#L142
    def size
      @ffi_type.size
    end

    # https://github.com/ffi/ffi/blob/ecfb225096ae76ba2a5e8115f046bd0ac23095e6/ext/ffi_c/Type.c#L157
    def alignment
      @ffi_type.alignment
    end
    
    # https://github.com/ffi/ffi/blob/ecfb225096ae76ba2a5e8115f046bd0ac23095e6/ext/ffi_c/Type.c#L333
    class Builtin < Type
      def initialize(native, ffi_type, name)
        @native_type = native
        @ffi_type = ffi_type
        @name = name
      end
    end
    
    # https://github.com/ffi/ffi/blob/ecfb225096ae76ba2a5e8115f046bd0ac23095e6/ext/ffi_c/Type.c#L372
    module NativeType
    end
    
    class Mapped
      def initialize(converter)
        if !converter.respond_to?(:native_type)
          raise NoMethodError, "native_type method not implemented"
        end
        if !converter.respond_to?(:to_native)
          raise NoMethodError, "to_native method not implemented"
        end
        if !converter.respond_to?(:from_native)
          raise NoMethodError, "from_native method not implemented"
        end
        @type = converter.native_type
        if !@type.is_a? Type
          raise TypeError, "native_type did not return instance of FFI::Type"
        end
        @converter = converter
      end
    end    
  end

  # https://github.com/ffi/ffi/blob/ecfb225096ae76ba2a5e8115f046bd0ac23095e6/ext/ffi_c/FunctionInfo.c#L292
  class FunctionType < Type
  end
  
  CallbackInfo = FunctionType
  FunctionInfo = FunctionType
  Type::Function = FunctionType

  # https://github.com/ffi/ffi/blob/ecfb225096ae76ba2a5e8115f046bd0ac23095e6/ext/ffi_c/Struct.c#L820
  class Struct
    class InlineArray
    end
  end

  # https://github.com/ffi/ffi/blob/ecfb225096ae76ba2a5e8115f046bd0ac23095e6/ext/ffi_c/StructByValue.c#L166
  class StructByValue < Type
  end

  Type::Struct = StructByValue

  # https://github.com/ffi/ffi/blob/ecfb225096ae76ba2a5e8115f046bd0ac23095e6/ext/ffi_c/ArrayType.c#L172
  class ArrayType < Type
  end

  Type::Array = ArrayType


  
  # https://github.com/ffi/ffi/blob/ecfb225096ae76ba2a5e8115f046bd0ac23095e6/ext/ffi_c/Type.c#L319
  TypeDefs = {}

  # https://github.com/ffi/ffi/blob/ecfb225096ae76ba2a5e8115f046bd0ac23095e6/ext/ffi_c/Types.h#L38
  NATIVE_VOID = 0
  NATIVE_INT8 = 1
  NATIVE_UINT8 = 2
  NATIVE_INT16 = 3
  NATIVE_UINT16 = 4
  NATIVE_INT32 = 5
  NATIVE_UINT32 = 6
  NATIVE_INT64 = 7
  NATIVE_UINT64 = 8
  NATIVE_LONG = 9
  NATIVE_ULONG = 10
  NATIVE_FLOAT32 = 11
  NATIVE_FLOAT64 = 12
  NATIVE_LONGDOUBLE = 13
  NATIVE_POINTER = 14
  NATIVE_FUNCTION = 15
  NATIVE_BUFFER_IN = 16
  NATIVE_BUFFER_OUT = 17
  NATIVE_BUFFER_INOUT = 18
  NATIVE_BOOL = 19
  # An immutable string.  Nul terminated, but only copies in to the native function
  NATIVE_STRING = 20
  # The function takes a variable number of arguments
  NATIVE_VARARGS = 21
  # Struct-by-value param or result
  NATIVE_STRUCT = 22
  # An array type definition
  NATIVE_ARRAY = 23
  # Custom native type
  NATIVE_MAPPED = 24

#  #define T(x, ffiType) do { \
#      VALUE t = Qnil; \
#      rb_define_const(rbffi_TypeClass, #x, t = builtin_type_new(classBuiltinType, NATIVE_##x, ffiType, #x)); \
#      rb_define_const(moduleNativeType, #x, t); \
#      rb_define_const(moduleFFI, "TYPE_" #x, t); \
#  } while(0)

#  #define A(old_type, new_type) do { \
#      VALUE t = rb_const_get(rbffi_TypeClass, rb_intern(#old_type)); \
#      rb_const_set(rbffi_TypeClass, rb_intern(#new_type), t); \
#  } while(0)

  def self.T(x, ffi_type)
    eval "FFI::TYPE_#{x} = Type::NativeType::#{x} = Type::#{x} = Type::Builtin.new(NATIVE_#{x}, FFIType::#{ffi_type}, \"#{x}\")"
  end

  def self.A(old_type, new_type)
    eval "Type::#{new_type} = Type::#{old_type}"
  end

  T("VOID", "VOID")
  T("INT8", "SINT8")
  A("INT8", "SCHAR")
  A("INT8", "CHAR")
  T("UINT8", "UINT8")
  A("UINT8", "UCHAR")

  T("INT16", "SINT16");
  A("INT16", "SHORT");
  A("INT16", "SSHORT");
  T("UINT16", "UINT16");
  A("UINT16", "USHORT");
  T("INT32", "SINT32");
  A("INT32", "INT");
  A("INT32", "SINT");
  T("UINT32", "UINT32");
  A("UINT32", "UINT");
  T("INT64", "SINT64");
  A("INT64", "LONG_LONG");
  A("INT64", "SLONG_LONG");
  T("UINT64", "UINT64");
  A("UINT64", "ULONG_LONG");
  T("LONG", "LONG");
  A("LONG", "SLONG");
  T("ULONG", "ULONG");
  T("FLOAT32", "FLOAT");
  A("FLOAT32", "FLOAT");
  T("FLOAT64", "DOUBLE");
  A("FLOAT64", "DOUBLE");
  T("LONGDOUBLE", "LONGDOUBLE");
  T("POINTER", "POINTER");
  T("STRING", "POINTER");
  T("BUFFER_IN", "POINTER");
  T("BUFFER_OUT", "POINTER");
  T("BUFFER_INOUT", "POINTER");
  T("BOOL", "UCHAR");
  T("VARARGS", "VOID");

  # https://github.com/ffi/ffi/blob/ecfb225096ae76ba2a5e8115f046bd0ac23095e6/ext/ffi_c/Type.c#L259
  module_function
  def custom_typedefs
    {}
  end
end




# typedef struct BuiltinType_ {
#     Type type;
#     const char* name;
# } BuiltinType;

# static VALUE
# builtin_type_new(VALUE klass, int nativeType, ffi_type* ffiType, const char* name)
# {
#     BuiltinType* type;
#     VALUE obj = Qnil;
# 
#     obj = TypedData_Make_Struct(klass, BuiltinType, &builtin_type_data_type, type);
# 
#     type->name = name;
#     type->type.nativeType = nativeType;
#     type->type.ffiType = ffiType;
# 
#     rb_obj_freeze(obj);
# 
#     return obj;
# }

