# frozen_string_literal: true

# Loaded by monoruby right after Bundler's `bundler/source/git.rb`
# (`Executor::run_load_hooks`).
#
# Bundler checks a gem bundled from a git source out under
# `Bundler.install_path` — `Gem.dir/bundler/gems/<name>-<shortref>` — and
# looks for it there and nowhere else. `Gem.dir` is monoruby's own gem home
# (rubygems/defaults/operating_system.rb), so a checkout the host CRuby's
# `bundle install` made is under the *host's* gem home and Bundler on
# monoruby reports it "not yet checked out". ruby-bench installs every
# benchmark's gems that way. When the checkout is missing from monoruby's
# home, use the same checkout from a host gem directory instead: it is
# plain Ruby source pinned to the same revision.
module Bundler
  class Source
    class Git
      module MonorubyHostCheckout
        def install_path
          path = super
          return path if @monoruby_checkout_resolved
          @monoruby_checkout_resolved = true
          return path if path.exist?
          host = __monoruby_host_checkout(path.basename.to_s)
          host ? (@install_path = host) : path
        end

        # `path` is an alias of the original `install_path`, which the
        # prepended method above would not reach.
        def path
          install_path
        end

        private

        def __monoruby_host_checkout(dirname)
          own = Gem.dir
          dirs = Gem.path + ENV["MONORUBY_HOST_GEM_PATH"].to_s.split(File::PATH_SEPARATOR)
          dirs.uniq.each do |dir|
            next if dir.empty? || dir == own
            candidate = Pathname.new(dir).join("bundler", "gems", dirname)
            return candidate if candidate.directory?
          end
          nil
        end
      end

      prepend MonorubyHostCheckout
    end
  end
end
