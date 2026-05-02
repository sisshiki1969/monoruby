# ffi.rb – monoruby replacement for the ffi gem's entry point
#
# When `require "ffi"` is called, monoruby loads this file instead of the
# ffi gem. It loads ffi_c.rb (which provides FFI types, Pointer,
# MemoryPointer, Function, DynamicLibrary, etc.) then defines FFI::Library
# so that `extend FFI::Library` + `ffi_lib` + `attach_function` work.

require "ffi_c"

module FFI
  VERSION = "1.17.0"

  module Library
    DEFAULT = nil

    def ffi_lib(*names)
      @ffi_libraries ||= []
      names.each do |name|
        if name == FFI::Library::DEFAULT || name == "c" || name == "libc.so.6"
          @ffi_libraries << FFI::DynamicLibrary.new("libc.so.6", FFI::DynamicLibrary::RTLD_LAZY)
        else
          @ffi_libraries << FFI::DynamicLibrary.new(name.to_s, FFI::DynamicLibrary::RTLD_LAZY)
        end
      end
      @ffi_functions ||= {}
    end

    def ffi_convention(convention)
      @ffi_convention = convention
    end

    def attach_function(*args)
      if args.length == 3
        ruby_name = args[0].to_sym
        c_name = args[0].to_s
        param_types = args[1]
        ret_type = args[2]
      else
        ruby_name = args[0].to_sym
        c_name = args[1].to_s
        param_types = args[2]
        ret_type = args[3]
      end

      sym = nil
      (@ffi_libraries || []).each do |lib|
        sym = lib.find_function(c_name)
        break if sym
      end
      raise FFI::NotFoundError, "Function '#{c_name}' not found in any library" unless sym

      ffi_ret_type = FFI::Type.find(ret_type)
      ffi_param_types = param_types.map { |t| FFI::Type.find(t) }

      func = FFI::Function.new(ffi_ret_type, ffi_param_types, sym)
      @ffi_functions ||= {}
      @ffi_functions[ruby_name] = func

      define_method(ruby_name) { |*call_args| func.call(*call_args) }
      module_function(ruby_name)
    end

    def callback(*args)
      # Stub: callback definitions are used for function pointer types.
      # For now, register the name so typedef can reference it.
      @ffi_callbacks ||= {}
      if args.length == 3
        @ffi_callbacks[args[0]] = { params: args[1], ret: args[2] }
      end
    end

    def typedef(old_type, new_name)
      @ffi_typedefs ||= {}
      @ffi_typedefs[new_name] = old_type
    end

    def enum(*args)
      # Minimal enum stub
    end
  end

  # DataConverter module (used by some gems for type mapping)
  module DataConverter
    def self.included(base)
      base.extend(self)
    end

    def native_type(type = nil)
      @native_type || FFI::Type::POINTER
    end

    def to_native(value, ctx = nil)
      value
    end

    def from_native(value, ctx = nil)
      value
    end
  end
end
