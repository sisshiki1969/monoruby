module FFI
  # struct Closure_ {
  #   void* info;      /* opaque handle for storing closure-instance specific data */
  #   void* function;  /* closure-instance specific function, called by custom trampoline */
  #   void* code;      /* Executable address for the native trampoline code location */
  #   void* pcl;       /* Writeable address for the native trampoline code location */
  #   struct ClosurePool_* pool;
  #   Closure* next;
  # };
  class Closure
    attr_accessor :info, :function, :code
    
    # https://github.com/ffi/ffi/blob/ecfb225096ae76ba2a5e8115f046bd0ac23095e6/ext/ffi_c/ClosurePool.c#L135
    def initialize
      self
    end
  end

  # struct MethodHandle {
  #     Closure* closure; // @closure
  # };
  class MethodHandle
    attr_reader :closure

    # https://github.com/ffi/ffi/blob/ecfb225096ae76ba2a5e8115f046bd0ac23095e6/ext/ffi_c/MethodHandle.c#L95
    def initialize(info, function)
      @closure = Closure.new
      @closure.info = info
      @closure.function = function
    end

    # https://github.com/ffi/ffi/blob/ecfb225096ae76ba2a5e8115f046bd0ac23095e6/ext/ffi_c/MethodHandle.c#L121
    def code
      @closure.code
    end
  end
end