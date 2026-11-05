# frozen_string_literal: true

# Loaded by monoruby right after Bundler's `bundler/runtime.rb`
# (`Executor::run_load_hooks`).
#
# monoruby's `require` falls back to the `lib/` of every gem the host has
# installed when `$LOAD_PATH` misses (`Globals::gem_lib_dirs`), standing in
# for rubygems' activation on a miss. Under Bundler that is wrong: once
# `Bundler.setup` has run, CRuby's `require` sees only the bundle, and a
# gem outside it raises LoadError — which libraries test for
# (ruby-lsp's `begin require "rubocop" rescue LoadError end`). So a
# finished setup turns the fallback off.
module Bundler
  class Runtime
    module MonorubyBundleOnly
      def setup(*)
        res = super
        __disable_host_gem_fallback
        res
      end
    end
    prepend MonorubyBundleOnly
  end
end
