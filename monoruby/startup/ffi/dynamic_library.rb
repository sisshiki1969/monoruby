module FFI
  # typedef struct Library {
  #     void* handle;
  # } Library;

  # https://github.com/ffi/ffi/blob/ecfb225096ae76ba2a5e8115f046bd0ac23095e6/ext/ffi_c/DynamicLibrary.c
  class DynamicLibrary
    RTLD_LOCAL = 0
    RTLD_LAZY = 1
    RTLD_NOW = 2
    RTLD_GLOBAL = 256

    attr_reader :handle, :name

    def self.open(name, flags)
      handle = ___dlopen(name, flags)
      self.new(handle, name || "[current process]")
    end

    # https://github.com/ffi/ffi/blob/ecfb225096ae76ba2a5e8115f046bd0ac23095e6/ext/ffi_c/DynamicLibrary.c#L137
    def initialize(handle, name)
      @handle = handle
      @name = name
    end

    # Document-method: find_function
    # call-seq: find_function(name)
    # @param [String] name library function's name
    # @return [FFI::DynamicLibrary::Symbol] library function symbol
    def find_function(name)
      address = ___dlsym(@handle, name)
      if address == 0
        nil
      else
        symbol_new(address, name)
      end
    end

    def symbol_new(address, name)
      Symbol.new(self, address, name)
    end

    # typedef struct LibrarySymbol_ {
    #     Pointer base;
    #     VALUE name;
    # } LibrarySymbol;
    class Symbol < Pointer
      def allocate
        obj = super
        obj.instance_variable_set(:@name, nil)
        obj
      end

      def initialize(parent, address, name)
        @memory_address = address
        @memory_size = 2147483647
        @memory_type_size = 1
        @parent = parent
        @name = name
      end

      def inspect
        "#<FFI::DynamicLibrary::Symbol name=#{@name} address=#{@memory_address}>"
      end
    end
  end
end