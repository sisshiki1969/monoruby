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

      # The process already has SDL2 mapped — `Gosu::SDL2` attached its
      # functions at load — so the global handle can name it, and this
      # port never has to know where the host keeps libSDL2.
      @global ||= Fiddle.dlopen(nil)
      @global.sym?(target)
    rescue StandardError
      nil
    end
  end
end

Gosu::NativeShim.install
