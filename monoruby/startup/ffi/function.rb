module FFI
  class Function < Pointer

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
    def initialize(return_type, parameter_types, proc, options)
      FFI::FunctionType.new(return_type, parameter_types, options)
      self
    end
  end
end