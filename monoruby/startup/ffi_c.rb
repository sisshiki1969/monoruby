require_relative 'ffi/type.rb'
require_relative 'ffi/dynamic_library.rb'

#
# https://github.com/ffi/ffi/blob/ecfb225096ae76ba2a5e8115f046bd0ac23095e6/ext/ffi_c/Type.c#L305
#
module FFI
  # struct AbstractMemory_ {
  #     char* address; /* Use char* instead of void* to ensure adding to it works correctly */
  #     long size;
  #     int flags;
  #     int typeSize;
  # };

  class AbstractMemory
    def initialize(address, size)
      @address = address
      @size = size
    end
    attr_reader :address, :size
  end

  # typedef struct Pointer {
  #     AbstractMemory memory;
  #     VALUE rbParent;
  #     char* storage; /* start of malloc area */
  #     bool autorelease;
  #     bool allocated;
  # } Pointer;

  class Pointer < AbstractMemory
    def initialize(type, address = type)
      @type = type
      @address = address
    end
  end

  # https://github.com/ffi/ffi/blob/master/ext/ffi_c/Platform.c
  class Platform
    #define S(name, T) do { \
    #    typedef struct { char c; T v; } s; \
    #    rb_define_const(module, #name "_ALIGN", INT2NUM((sizeof(s) - sizeof(T)) * 8)); \
    #    rb_define_const(module, #name "_SIZE", INT2NUM(sizeof(T)* 8)); \
    #} while(0)
    #    S(INT8, char);
    #    S(INT16, short);
    #    S(INT32, int);
    #    S(INT64, long long);
    #    S(LONG, long);
    #    S(FLOAT, float);
    #    S(DOUBLE, double);
    #    S(LONG_DOUBLE, long double);
    #    S(ADDRESS, void*);
    #undef S
    ADDRESS_ALIGN = 8
    ADDRESS_SIZE = 64
  end

  Type::VOID = Type.new(0,0)
  Type::CHAR = Type::SCHAR = Type::INT8 = Type.new(1,1)
  Type::UCHAR = Type::UINT8 = Type.new(1,1)
  Type::SHORT = Type::SSHORT = Type::INT16 = Type.new(2,2)
  Type::USHORT = Type::UINT16 = Type.new(2,2)
  Type::INT = Type::SINT = Type::INT32 = Type.new(4,4)
  Type::UINT = Type::UINT32 = Type.new(4,4)
  Type::LONG_LONG = Type::SLONG_LONG = Type::INT64 = Type.new(8,8)
  Type::LONG = Type::UINT64 = Type.new(8,8)

  Type::BOOL = Type.new(1,1)
  Type::STRING = Type.new(8,8)
  Type::ULONG = Type.new(8,8)
  Type::ULONG_LONG = Type.new(8,8)
  Type::FLOAT = Type.new(4,4)
  Type::DOUBLE = Type.new(8,8)
  Type::LONGDOUBLE = Type.new(16,16)
  Type::POINTER = Type.new(8,8)
  Type::BUFFER_IN = Type.new(8,8)
  Type::BUFFER_OUT = Type.new(8,8)
  Type::BUFFER_INOUT = Type.new(8,8)
  Type::VARARGS = Type.new(0,0)
end


# struct Type_ {
#     NativeType nativeType;
#     ffi_type* ffiType;
# };

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

#  /*
#   * Document-constant: FFI::Type::Builtin::VOID
#   */
#  T(VOID, &ffi_type_void);
#  T(INT8, &ffi_type_sint8);
#  A(INT8, SCHAR);
#  A(INT8, CHAR);
#  T(UINT8, &ffi_type_uint8);
#  A(UINT8, UCHAR);

#  T(INT16, &ffi_type_sint16);
#  A(INT16, SHORT);
#  A(INT16, SSHORT);
#  T(UINT16, &ffi_type_uint16);
#  A(UINT16, USHORT);
#  T(INT32, &ffi_type_sint32);
#  A(INT32, INT);
#  A(INT32, SINT);
#  T(UINT32, &ffi_type_uint32);
#  A(UINT32, UINT);
#  T(INT64, &ffi_type_sint64);
#  A(INT64, LONG_LONG);
#  A(INT64, SLONG_LONG);
#  T(UINT64, &ffi_type_uint64);
#  A(UINT64, ULONG_LONG);
#  T(LONG, &ffi_type_slong);
#  A(LONG, SLONG);
#  T(ULONG, &ffi_type_ulong);
#  T(FLOAT32, &ffi_type_float);
#  A(FLOAT32, FLOAT);
#  T(FLOAT64, &ffi_type_double);
#  A(FLOAT64, DOUBLE);
#  T(LONGDOUBLE, &ffi_type_longdouble);
#  T(POINTER, &ffi_type_pointer);
#  T(STRING, &ffi_type_pointer);
#  T(BUFFER_IN, &ffi_type_pointer);
#  T(BUFFER_OUT, &ffi_type_pointer);
#  T(BUFFER_INOUT, &ffi_type_pointer);
#  T(BOOL, &ffi_type_uchar);
#  T(VARARGS, &ffi_type_void);
