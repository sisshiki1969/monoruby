module FFI
  # https://github.com/ffi/ffi/blob/ecfb225096ae76ba2a5e8115f046bd0ac23095e6/ext/ffi_c/MemoryPointer.c#L193
  class MemoryPointer < Pointer

    # https://github.com/ffi/ffi/blob/ecfb225096ae76ba2a5e8115f046bd0ac23095e6/ext/ffi_c/MemoryPointer.c#L90

    # call-seq: initialize(size, count=1, clear=true)
    # @param [Integer, Symbol, FFI::Type] size size of a memory cell (in bytes, or type whom size will be used)
    # @param [Integer] count number of cells in memory
    # @param [Boolean] clear set memory to all-zero if +true+
    # @return [self]
    # A new instance of FFI::MemoryPointer.
    def initialize(size, count = 1, clear = true)
      size = case size
      when  Integer
        size
      when Symbol
        # VALUE nType;
        type = TypeDefs[size]
        # if ((nType = rb_hash_lookup(typeMap, type)) != Qnil) {
        if type
          type.size
        #     if (rb_obj_is_kind_of(nType, rbffi_TypeClass)) {
        elsif type.is_a?(Type)
          #         Type* type;
          #         TypedData_Get_Struct(nType, Type, &rbffi_type_data_type, type);
          #         return (int) type->ffiType->size;
          type.ffi_type.size
        elsif type.respond_to?(:size)
          #     } else if (rb_respond_to(nType, id_size)) {
          #         return NUM2INT(rb_funcall2(nType, id_size, 0, NULL));
          #     }
          # }
          type.size
        else
        # /* Not found - call up to the ruby version to resolve */
        # return NUM2INT(rb_funcall2(rbffi_FFIModule, id_type_size, 1, &type));
          FFI.type_size(size)
        end
      else
        size.size
      end
      malloc(size, count, clear)
    end

    def malloc(size, count, clear)
    # Pointer* p;
    # unsigned long msize;
    # TypedData_Get_Struct(self, Pointer, &memory_pointer_data_type, p);
    # msize = size * count;
    # p->storage = xmalloc(msize + 7);
    # if (p->storage == NULL) {
    #     rb_raise(rb_eNoMemError, "Failed to allocate memory size=%ld bytes", msize);
    #     return Qnil;
    # }
    # p->autorelease = true;
    # p->memory.typeSize = (int) size;
    # p->memory.size = msize;
    # /* ensure the memory is aligned on at least a 8 byte boundary */
    # p->memory.address = (char *) (((uintptr_t) p->storage + 0x7) & (uintptr_t) ~0x7ULL);
    # p->allocated = true;
    # if (clear && p->memory.size > 0) {
    #     memset(p->memory.address, 0, p->memory.size);
    # }
    end
  end
end