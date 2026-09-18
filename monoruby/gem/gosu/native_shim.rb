# Stand-in for the gosu gem's *native* half.
#
# monoruby serves `require "gosu"` from the pure-Ruby SDL2 port in
# `gosu.rb`, so no gosu gem is ever activated: `Gem.loaded_specs["gosu"]`
# stays nil and the `gosu.<dlext>` a real install would have sitting in
# the gem's `lib/` does not exist. Programs that only use Gosu's Ruby API
# never notice. Programs that reach past it do — Gosu exposes no way to
# grab the keyboard, so an app that wants one `dlopen`s the extension and
# calls `Gosu::shared_window()` plus SDL's own `SDL_SetWindowKeyboardGrab`
# through it. That dies here on `nil.full_gem_path`, and the feature is
# silently lost.
#
# Finish the impersonation instead. Publish a spec whose `full_gem_path`
# points at this stub tree, and register the extension's path as a
# virtual Fiddle library (see `Fiddle.register_virtual_library`) that
# exports what the real one exported.
module Gosu
  module NativeShim
    # The real `gosu.<dlext>` links SDL2 in and re-exports it, so any
    # symbol from these libraries is one a caller could legitimately have
    # pulled out of the extension. Everything else is answered with
    # "not exported", exactly as the real bundle would.
    FORWARDED_PREFIXES = %w[SDL_ IMG_ Mix_ TTF_].freeze

    # Where those symbols actually live: the `Gosu::SDL2*` modules, which
    # have already opened the libraries to attach their own functions.
    # They are asked in this order and the ones that have not been bound
    # yet are skipped -- images, audio and fonts open theirs on first use
    # -- so this is a list of names rather than of constants.
    FORWARDED_LIBRARIES = %i[SDL2 SDL2_image SDL2_mixer SDL2_ttf].freeze

    # C++ entry points, under the names the Itanium ABI gives them.
    #
    # `Gosu::shared_window()` returns the one `SDL_Window*` Gosu created.
    # This port creates its window through `Gosu::SDL2` instead, and SDL
    # itself can name it: `SDL_GetKeyboardFocus()` has the same signature
    # (no arguments, returns `SDL_Window*`) and, for a single-window
    # application, the same answer. The one difference is that it reports
    # NULL while the window is not focused — which is exactly when a
    # keyboard grab would be refused anyway.
    MANGLED_ALIASES = {
      "_ZN4Gosu13shared_windowEv" => "SDL_GetKeyboardFocus",
    }.freeze

    module_function

    # The directory a `gosu` gem would have been unpacked into — the stub
    # root, which is what a consumer wanting "where does this gem live"
    # should be pointed at. Nothing is claimed about its layout beyond
    # the virtual `lib/gosu.<dlext>` registered below.
    def gem_root
      File.expand_path("..", __dir__)
    end

    def install
      register_spec
      register_native_library
    end

    def register_spec
      return unless defined?(Gem) && Gem.respond_to?(:loaded_specs)
      return if Gem.loaded_specs["gosu"]

      root = gem_root
      spec = Gem::Specification.new
      spec.name = "gosu"
      spec.version = Gosu::VERSION
      # `full_gem_path` / `gem_dir` normally derive from the gem's
      # install directory under a `Gem.dir`; this one was never
      # installed, so state the answer directly.
      spec.define_singleton_method(:full_gem_path) { root }
      spec.define_singleton_method(:gem_dir) { root }
      Gem.loaded_specs["gosu"] = spec
    rescue StandardError
      # No RubyGems, or a version whose Specification does not take this
      # shape. The port still works; only the impersonation is partial.
      nil
    end

    def register_native_library
      require "fiddle"
      return unless Fiddle.respond_to?(:register_virtual_library)

      native_library_paths.each do |path|
        Fiddle.register_virtual_library(path) { |name| resolve(name) }
      end
    rescue LoadError, StandardError
      nil
    end

    # `gosu.<dlext>`, plus the names a caller may compute for itself.
    # `RbConfig::CONFIG["DLEXT"]` is the documented way to spell it, but
    # a program that hard-codes the platform suffix should find us too.
    def native_library_paths
      dlext = (defined?(RbConfig) && RbConfig::CONFIG["DLEXT"]) || "so"
      exts = [dlext, "bundle", "so", "dylib"].uniq
      exts.map { |ext| File.join(gem_root, "lib", "gosu.#{ext}") }
    end

    # Symbol lookup for the virtual library. `nil` means "not exported".
    def resolve(name)
      target = MANGLED_ALIASES[name] || name
      return nil unless FORWARDED_PREFIXES.any? { |p| target.start_with?(p) }

      # Ask the libraries this port itself has open. Going through
      # `Fiddle.dlopen(nil)` instead would find nothing: FFI opens them
      # RTLD_LOCAL, so their symbols never enter the global namespace,
      # and the real gosu.so's are reachable only because it linked SDL
      # in. Asking the modules also means never having to know what the
      # host calls libSDL2 or where it keeps it.
      attached_libraries.each do |lib|
        address = lib.find_symbol(target)&.to_i
        return address if address && address != 0
      end

      # A host that did put the symbols in the global namespace (SDL
      # linked into the program, or loaded RTLD_GLOBAL by something else).
      @global ||= Fiddle.dlopen(nil)
      @global.sym?(target)
    rescue StandardError
      nil
    end

    # The `FFI::DynamicLibrary`s behind the `Gosu::SDL2*` modules that
    # have been bound so far.
    def attached_libraries
      FORWARDED_LIBRARIES.flat_map { |name|
        next [] unless Gosu.const_defined?(name, false)

        mod = Gosu.const_get(name, false)
        next [] unless mod.respond_to?(:ffi_libraries)

        begin
          mod.ffi_libraries
        rescue StandardError
          # `ffi_libraries` raises until the module has opened one.
          []
        end
      }
    end
  end
end

Gosu::NativeShim.install
