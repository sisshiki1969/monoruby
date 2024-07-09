module FFI

  # typedef struct Pointer {
  #     AbstractMemory memory;
  #     VALUE rbParent;
  #     char* storage; /* start of malloc area */
  #     bool autorelease;
  #     bool allocated;
  # } Pointer;


  # https://github.com/ffi/ffi/blob/ecfb225096ae76ba2a5e8115f046bd0ac23095e6/ext/ffi_c/Pointer.c#L504
  class Pointer < AbstractMemory
    # https://github.com/ffi/ffi/blob/ecfb225096ae76ba2a5e8115f046bd0ac23095e6/ext/ffi_c/Pointer.c#L85
    def allocate
      obj = super
      obj.instance_variable_set(:@parent, nil)
      obj.instance_variable_set(:@memory_flags, MEM_RD | MEM_WR)
      obj
    end

    # https://github.com/ffi/ffi/blob/ecfb225096ae76ba2a5e8115f046bd0ac23095e6/ext/ffi_c/Pointer.c#L107
    # @overload initialize(address)
    # @overload initialize(type, address)
    #  @param [Type] type Optional type for pointer (defaults to byte)
    #  @param [Pointer, Integer] address base address for pointer
    #  Create a new pointer from a {Type} and a base address
    # @return [self]
    # A new instance of Pointer.
    def initialize(type, address = nil)
      type_size = if address
        FFI.ffi_type_size(type)
      else
        address = type
        1
      end
      case address
      when Integer
        @memory_address = address
        @memory_size = 2147483647
        if @memory_address == 0
          @memory_flags = 0
        end
      when Pointer
        @parent = address
        @memory_address = address.memory_address
        @memory_size = address.memory_size
        @memory_flags = address.memory_flags
      else
        raise TypeError, "expected Integer or Pointer, got #{address.class}"
      end
      @memory_type_size = type_size
      self
    end
  end
end
