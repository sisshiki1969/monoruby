#
# https://github.com/ffi/ffi/blob/ecfb225096ae76ba2a5e8115f046bd0ac23095e6/ext/ffi_c/Type.c#L305
#
module FFI
  # https://github.com/ffi/ffi/blob/ecfb225096ae76ba2a5e8115f046bd0ac23095e6/ext/ffi_c/Type.c#L319
  TypeDefs = {}
  # https://github.com/ffi/ffi/blob/ecfb225096ae76ba2a5e8115f046bd0ac23095e6/ext/ffi_c/Type.c#L314
  class Type
    class Builtin
      def initialize(native, ffi_type, name)
        @native_type = native
        @ffi_type = ffi_type
        @name = name
      end
    end
    module NativeType
    end
    VOID = 0
    BOOL = 1
    STRING = 2
    CHAR = 3
    UCHAR = 4
    SHORT = 5
    USHORT = 6
    INT = 7
    UINT = 8
    LONG = 9
    ULONG = 10
    LONG_LONG = 11
    ULONG_LONG = 12
    FLOAT = 13
    DOUBLE = 14
    LONGDOUBLE = 15
    POINTER = 16
    INT8 = 17
    UINT8 = 18
    INT16 = 19
    UINT16 = 20
    INT32 = 21
    UINT32 = 22
    INT64 = 23
    UINT64 = 24
    BUFFER_IN = 25
    BUFFER_OUT = 26
    BUFFER_INOUT = 27
    VARARGS = 28
  end

  def initialize(size, alignment)
    @size = size
    @alignment = alignment
  end

  attr_reader :size, :alignment
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
