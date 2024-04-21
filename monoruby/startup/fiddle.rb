module Fiddle
  SIZEOF_LONG = 8
  WINDOWS = false
  module_function
  def dlopen(lib)
    h = Kernel.___dlopen(lib)
    raise DLError.new("dlopen failed") if h == 0
    Handle.new(h)
  end
  module Types
    VOID = 0
    VOIDP = 1
    CHAR = 2
    UCHAR = -2
    INT = 4
    UINT = -4
  end
  class Handle
    RTLD_GLOBAL = 0
    RTLD_LAZY = 0
    RTLD_NOW = 0
    def initialize(handle)
      @handle = handle
    end
    def [](name)
      ptr = Kernel.___dlsym(@handle, name)
      raise DLError.new("dlsym failed") if ptr == 0
      ptr
    end
  end
  class Error < StandardError
  end
  class DLError < Error
  end
  class Function
    def initialize(ptr, args_type, ret_type)
      @ptr = ptr
      @args_type = args_type
      @ret_type = ret_type
    end
    def call(*arg)
      Kernel.___call(@ptr, arg, @args_type, @ret_type)
    end
  end
end