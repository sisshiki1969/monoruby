# frozen_string_literal: true

# monoruby's rubygems defaults. rubygems requires this file (by name,
# from `$LOAD_PATH`) right after its own `rubygems/defaults.rb`; the
# install's stub tree pins this copy ahead of any the host CRuby's
# packager shipped.
#
# The gems monoruby installs go to a gem home of its own, never the host
# CRuby's. A C extension built by `gem install` on monoruby is linked
# against monoruby's C API layer (doc/c_api.md) and can only be loaded by
# monoruby, while rubygems keys a built extension on
# `extensions/<platform>/<api version>/<gem>` alone — the same path for
# both interpreters. Sharing the gem home would have each install
# overwrite the other's build. The host's gem directories stay on
# `Gem.path` for reading, as before: through `GEM_PATH`, and through
# `default_path` below when a program clears `GEM_PATH`.
module Gem
  ##
  # Where `gem install` puts gems: `~/.monoruby/gems/<api version>`, next
  # to monoruby's other per-user state. `GEM_HOME` overrides it.
  def self.default_dir
    @default_dir ||= File.join(
      Gem.user_home, ".monoruby", "gems", RbConfig::CONFIG["ruby_version"]
    )
  end

  ##
  # The default gems (bundler, json, ...) keep coming from the host CRuby
  # the runtime probe found, when there is one: its
  # `specifications/default` is what `Gem::Specification` enumerates
  # them from. (`MONORUBY_HOST_RUBYLIBPREFIX` is set by `Globals::new`
  # from `GEM_PATH`; see builtins/startup.rb.)
  def self.default_specifications_dir
    @default_specifications_dir ||= begin
      host = ENV["MONORUBY_HOST_RUBYLIBPREFIX"]
      base =
        if host && !host.empty?
          File.join(host, "gems", RbConfig::CONFIG["ruby_version"])
        else
          default_dir
        end
      File.join(base, "specifications", "default")
    end
  end

  class << self
    alias_method :__monoruby_rubygems_default_path, :default_path
  end

  ##
  # rubygems' own default path, then the host CRuby's gem directories:
  # the `GEM_PATH` monoruby started with (`MONORUBY_HOST_GEM_PATH`, set by
  # `Globals::new` from the runtime probe or the caller's `GEM_PATH`).
  # `Gem.path` falls back to this when `GEM_PATH` is unset, and a program
  # may unset it itself — ruby-bench's harness does, then has the host's
  # `bundle install` put the gems in the host's gem home. Before
  # `default_dir` moved to monoruby's own home that was the host's
  # directory, so the fallback found them; now this has to.
  def self.default_path
    host = ENV["MONORUBY_HOST_GEM_PATH"].to_s.split(File::PATH_SEPARATOR)
    (__monoruby_rubygems_default_path + host.reject(&:empty?)).uniq
  end

  ##
  # `--user-install` is kept apart from CRuby's user gems for the same
  # reason: `monoruby` in place of the engine name (`ruby`).
  def self.user_dir
    gem_dir = File.join(Gem.user_home, ".gem")
    gem_dir = File.join(Gem.data_home, "gem") unless File.exist?(gem_dir)
    File.join(gem_dir, "monoruby", RbConfig::CONFIG["ruby_version"])
  end
end
