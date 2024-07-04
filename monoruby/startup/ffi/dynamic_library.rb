  module FFI
    class DynamicLibrary
      RTLD_LAZY = 1
      RTLD_NOW = 2
      RTLD_LOCAL = 4
      RTLD_GLOBAL = 8

      attr_reader :handle, :name

      def initialize(handle, name)
        @handle = handle
        @name = name
      end

      def self.open(name, flags)
        handle = ___dlopen(name, flags)
        self.new(handle, name ? name : "")
      end
    end
  end