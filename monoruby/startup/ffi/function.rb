module FFI
    # typedef struct Function_ {
    #   Pointer base;
    #   FunctionType* info;         // @info
    #   MethodHandle* methodHandle; // @method_handle
    #   bool autorelease;           // @autorelease
    #   Closure* closure;           // @closure
    #   VALUE rbProc;               // @proc
    #   VALUE rbFunctionInfo;       // @function_info
    # } Function;
  class Function < Pointer
    def allocate
      super
    end

    # @param [Type, Symbol] return_type return type for the function
    # @param [Array<Type, Symbol>] param_types array of parameters types
    # @param [Hash] options see {FFI::FunctionType} for available options
    # @return [self]
    # A new Function instance.
    #
    # Define a function from a Proc or a block.
    #
    # @overload initialize(return_type, param_types, options = {}) { |i| ... }
    #  @yieldparam i parameters for the function
    # @overload initialize(return_type, param_types, proc, options = {})
    #  @param [Proc] proc
    # https://github.com/ffi/ffi/blob/ecfb225096ae76ba2a5e8115f046bd0ac23095e6/ext/ffi_c/Function.c#L299
    def initialize(return_type, parameter_types, proc = nil, options = nil, &block)
      @proc = if block_given?
        if options
          raise ArgumentError, "cannot create function with both proc/address and block"
        end
        options = proc
        block
      else
        proc
      end
      @options = options || {}
      @info = FFI::FunctionType.new(return_type, parameter_types, @options)
      @closure = Closure.new
      @closure.info = self
      @memory_address = @closure.code
      @memory_size = 0
      @autorelease = true
      self
    end

    # call-seq: attach(m, name)
    # @param [Module] m
    # @param [String] name
    # @return [self]
    # Attach a Function to the Module +m+ as +name+.
    # https://github.com/ffi/ffi/blob/ecfb225096ae76ba2a5e8115f046bd0ac23095e6/ext/ffi_c/Function.c#L496
    def attach(mod, name)
      if @info.parameter_count == -1
        raise RuntimeError, "cannot attach variadic function"
      end
      if !mod.is_a?(Module)
        raise RuntimeError, "trying to attach function to non-module"
      end
      
      # if (fn->methodHandle == NULL) {
      #     fn->methodHandle = rbffi_MethodHandle_Alloc(fn->info, fn->base.memory.address);
      # }
      # rb_define_singleton_method(module, StringValueCStr(name), rbffi_MethodHandle_CodeAddress(fn->methodHandle), -1);
      # rb_define_method(module, StringValueCStr(name), rbffi_MethodHandle_CodeAddress(fn->methodHandle), -1);
      @method_handle ||= MethodHandle.new(@info, @memory_address)
      
      s = "def #{name}(*args); raise RuntimeError, \"#{name} invoked. \#{args}\"; end"
      # puts s

      mod.module_eval(s)
      mod.singleton_class.module_eval(s)

      self

      # MethodHandle*
      # rbffi_MethodHandle_Alloc(FunctionType* fnInfo, void* function)
      # {
      #     MethodHandle* handle;
      #     Closure* closure = rbffi_Closure_Alloc(defaultClosurePool);
      #     if (closure == NULL) {
      #         rb_raise(rb_eNoMemError, "failed to allocate closure from pool");
      #         return NULL;
      #     }
      # 
      #     handle = xcalloc(1, sizeof(*handle));
      #     handle->closure = closure;
      #     closure->info = fnInfo;
      #     closure->function = function;
      #
      #     return handle;
      # }

      # rbffi_function_anyargs rbffi_MethodHandle_CodeAddress(MethodHandle* handle)
      # {
      #     return (rbffi_function_anyargs) handle->closure->code;
      # }
    end
  end
end