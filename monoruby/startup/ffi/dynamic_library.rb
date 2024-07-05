  module FFI
    class DynamicLibrary
      RTLD_LOCAL = 0
      RTLD_LAZY = 1
      RTLD_NOW = 2
      RTLD_GLOBAL = 256

      attr_reader :handle, :name

      def initialize(handle, name)
        @handle = handle
        @name = name
      end

      def self.open(name, flags)
        handle = ___dlopen(name, flags)
        self.new(handle, name || "[current process]")
      end
    end
  end