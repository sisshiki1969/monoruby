
module FFI  
  # https://github.com/ffi/ffi/blob/ecfb225096ae76ba2a5e8115f046bd0ac23095e6/ext/ffi_c/StructLayout.c#L690
  class StructLayout < Type

    # https://github.com/ffi/ffi/blob/ecfb225096ae76ba2a5e8115f046bd0ac23095e6/ext/ffi_c/Struct.h#L65C1-L91C7

    # struct StructLayout_ {
    #     Type base;
    #     StructField** fields;
    #     int fieldCount;
    #     int size;
    #     int align;
    #     ffi_type** ffiTypes;
    # 
    #     /*
    #     * We use the fieldName's minor 8 Bits as index to a 256 entry cache.
    #     * This avoids full ruby hash lookups for repeated lookups.
    #     */
    #     #define FIELD_CACHE_LOOKUP(this, sym) ( &(this)->cache_row[((sym) >> 8) & 0xff] )
    #     #define FIELD_CACHE_ROWS 0x100
    # 
    #     struct field_cache_entry {
    #       VALUE fieldName;
    #       StructField *field;
    #     } cache_row[FIELD_CACHE_ROWS];
    # 
    #     /** The number of reference tracking fields in this struct */
    #     int referenceFieldCount;
    # 
    #     VALUE rbFieldNames;
    #     VALUE rbFieldMap;
    #     VALUE rbFields;
    # };

    # https://github.com/ffi/ffi/blob/ecfb225096ae76ba2a5e8115f046bd0ac23095e6/ext/ffi_c/StructLayout.c#L483
    def initialize(fields, size, align)
      @fieldcount = fields.size
      @field_map = {}
      @field_names = []
      @fields = []
      @size = size
      @align = align
      @ffi_types = []

      for i in 0...@fieldcount
        field = fields[i]
        name = field.name
        @field_map[name] = field
        @fields.push(field)
        @field_names.push(name)
      end
    end

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
    class Number < Field
    end
    class String < Field
    end
    class Pointer < Field
    end
    class Function < Field
    end
    class Array < Field
    end
    class CharArray < Struct::InlineArray
    end
  end
end