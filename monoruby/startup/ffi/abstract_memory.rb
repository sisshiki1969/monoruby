module FFI
  # struct AbstractMemory_ {
  #     char* address; /* Use char* instead of void* to ensure adding to it works correctly */
  #     long size;
  #     int flags;
  #     int typeSize;
  # };

  class AbstractMemory
    attr_reader :memory_address, :memory_size, :memory_flags, :memory_type_size
    MEM_RD = 1
    MEM_WR = 2
    MEM_CODE = 4
    MEM_SWAP = 8
    MEM_EMBED = 16

    # https://github.com/ffi/ffi/blob/ecfb225096ae76ba2a5e8115f046bd0ac23095e6/ext/ffi_c/AbstractMemory.c#L330
    def size
      self.memory_size
    end

    # https://github.com/ffi/ffi/blob/ecfb225096ae76ba2a5e8115f046bd0ac23095e6/ext/ffi_c/AbstractMemory.c#L545
    def get_bytes(offset, length)
      checkRead
      checkBounds(offset, length)
      ___read_memory(@memory_address + offset, length)
    end

    # https://github.com/ffi/ffi/blob/ecfb225096ae76ba2a5e8115f046bd0ac23095e6/ext/ffi_c/AbstractMemory.c#L610
    def read_bytes(length)
      get_bytes(0, length)
    end

    def write_int32(value)
      memory_op_put_int32(0, value)
      self
    end

    def write_int16(value)
      memory_op_put_int16(0, value)
      self
    end

    def memory_op_put_int32(offset, value)
      # type tmp = (type) VAL(toNative(value), swap);
      checkWrite
      checkBounds(offset, 4)
      ___memcpyv(@memory_address + offset, value, 4)
    end

    def memory_op_put_int16(offset, value)
      # type tmp = (type) VAL(toNative(value), swap);
      checkWrite
      checkBounds(offset, 2)
      ___memcpyv(@memory_address + offset, value, 2)
    end

    def checkRead
      if (@memory_flags & MEM_RD) == 0
        raise RuntimeError, "invalid memory read at address=#{@memory_address}"
      end
    end

    def checkWrite
      if (@memory_flags & MEM_WR) == 0
        raise RuntimeError, "invalid memory write at address=#{@memory_address}"
      end
    end

    def checkBounds(offset, size) 
      if offset < 0 || size < 0 || offset + size > @memory_size
        raise RuntimeError, "Memory access offset=#{offset} size=#{size} out of bounds"
      end
    end
  end
end