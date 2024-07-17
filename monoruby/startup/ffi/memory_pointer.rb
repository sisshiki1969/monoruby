module FFI
  # https://github.com/ffi/ffi/blob/ecfb225096ae76ba2a5e8115f046bd0ac23095e6/ext/ffi_c/MemoryPointer.c#L193
  class MemoryPointer < Pointer

    def self.allocate
      obj = super
      obj.instance_variable_set(:@parent, nil)
      obj.instance_variable_set(:@memory_flags, MEM_RD | MEM_WR)
      obj
    end

    # https://github.com/ffi/ffi/blob/ecfb225096ae76ba2a5e8115f046bd0ac23095e6/ext/ffi_c/MemoryPointer.c#L90
    # call-seq: initialize(size, count=1, clear=true)
    # @param [Integer, Symbol, FFI::Type] size size of a memory cell (in bytes, or type whom size will be used)
    # @param [Integer] count number of cells in memory
    # @param [Boolean] clear set memory to all-zero if +true+
    # @return [self]
    # A new instance of FFI::MemoryPointer.
    def initialize(size, count = 1, clear = true)
      size = FFI.ffi_type_size(size)
      malloc(size, count, clear)
      self
    end
    
    # https://github.com/ffi/ffi/blob/ecfb225096ae76ba2a5e8115f046bd0ac23095e6/ext/ffi_c/MemoryPointer.c#L106
    def malloc(size, count, clear)
      msize = size * count
      @storage = ___malloc(msize + 7, clear)
      @autorelease = true
      @memory_address = (@storage + 0x7) & ~0x7
      @memory_size = size * count
      @memory_type_size = size
      @allocated = true
    end
  end
end