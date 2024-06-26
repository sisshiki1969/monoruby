require_relative 'ffi/type.rb'
require_relative 'ffi/dynamic_library.rb'
require_relative 'ffi/struct_layout.rb'

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
end
