
module FFI  
  # https://github.com/ffi/ffi/blob/ecfb225096ae76ba2a5e8115f046bd0ac23095e6/ext/ffi_c/StructLayout.c#L690
  class StructLayout
    class Field
      # https://github.com/ffi/ffi/blob/ecfb225096ae76ba2a5e8115f046bd0ac23095e6/ext/ffi_c/StructLayout.c#L141
      def initialize(name, offset, type)
        name = if name.is_a?(Symbol)
          name
        elsif name.is_a?(String)
          name.to_sym
        else
          raise "wrong argument type #{name.class} (expected Symbol/String)"
        end
        if !offset.is_a?(Integer)
          raise "wrong argument type #{offset.class} (expected Integer)"
        end
        if !type.is_a?(Type)
          raise "wrong argument type #{type.class} (expected FFI::Type)"
        end
        @name = name
        @offset = offset
        @type = type
      end

      # https://github.com/ffi/ffi/blob/ecfb225096ae76ba2a5e8115f046bd0ac23095e6/ext/ffi_c/StructLayout.c#L195
      def offset
        @offset
      end

      # https://github.com/ffi/ffi/blob/ecfb225096ae76ba2a5e8115f046bd0ac23095e6/ext/ffi_c/StructLayout.c#L208
      def size
        @type.size
      end

      # https://github.com/ffi/ffi/blob/ecfb225096ae76ba2a5e8115f046bd0ac23095e6/ext/ffi_c/StructLayout.c#L221
      def alignment
        @type.alignment
      end

      # https://github.com/ffi/ffi/blob/ecfb225096ae76ba2a5e8115f046bd0ac23095e6/ext/ffi_c/StructLayout.c#L248
      def name
        @name
      end

      # https://github.com/ffi/ffi/blob/ecfb225096ae76ba2a5e8115f046bd0ac23095e6/ext/ffi_c/StructLayout.c#L234
      def type
        @type
      end
    end
    class Number
    end
    class String
    end
    class Pointer
    end
    class Function
    end
    class Array
    end
  end
end