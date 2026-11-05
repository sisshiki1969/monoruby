# frozen_string_literal: true

# Loaded by monoruby when a `require` resolves to a C extension built for
# CRuby (`Executor::require`, `RequireLoad::Foreign`): a `.so` that does not
# export the C API layer's entry point and has no Ruby stand-in.
#
# A gem installed by the host CRuby — ruby-bench has the host's
# `bundle install` set up every benchmark's bundle — carries its
# extension's C sources next to the `.so` the host built. monoruby cannot
# load that `.so` (doc/c_api.md §2: API- not ABI-compatible), but it can
# build the same sources against its own `ruby.h`, as `gem install` on
# monoruby would. This does that once per gem version, into monoruby's own
# gem home, and answers the path of the rebuilt library:
#
#   ~/.monoruby/gems/<api>/host_extensions/<platform>/<gem>-<version>/
#
# laid out like the gem's `extension_dir`, so the directory goes on
# `$LOAD_PATH` ahead of the gem's own and later requires of the gem's
# other extension files resolve to it directly. The host gem's directory
# is only read: its tree is copied to a scratch directory and built there.
# A `gem.build_complete` file records which C API layer the build linked
# (the archive's size and mtime), so a rebuilt monoruby rebuilds the
# extension rather than load one compiled against an older layer.
require "rubygems"

module Gem
  module MonorubyHostExtension
    @failed = {}

    class << self
      # The rebuilt counterpart of `so_path`, or nil when it is not a
      # host gem's extension or the build failed (warned once).
      def build(so_path)
        so_path = File.expand_path(so_path)
        return nil if @failed[so_path]
        spec, rel = owner(so_path)
        return nil unless spec
        exts = spec.extensions
        return nil if exts.empty? || !exts.all? { |e| File.basename(e) =~ /\Aextconf/ }
        dir = cache_dir(spec)
        target = File.join(dir, rel)
        unless built?(dir, spec) && File.exist?(target)
          unless build_into(dir, spec)
            @failed[so_path] = true
            return nil
          end
        end
        $LOAD_PATH.unshift(dir) unless $LOAD_PATH.include?(dir)
        File.exist?(target) ? target : nil
      end

      private

      # The installed gem whose tree holds `so_path`, and the path of the
      # library relative to the require path it was found in.
      def owner(so_path)
        specs = Gem.loaded_specs.values
        [specs, Gem::Specification.to_a].each do |list|
          list.each do |spec|
            next if spec.extensions.empty?
            dirs = [spec.extension_dir, *spec.full_require_paths]
            dirs.each do |d|
              prefix = File.expand_path(d) + "/"
              return [spec, so_path.delete_prefix(prefix)] if so_path.start_with?(prefix)
            end
          end
        end
        nil
      end

      def cache_dir(spec)
        File.join(Gem.default_dir, "host_extensions", Gem::Platform.local.to_s, spec.full_name)
      end

      # What the build depends on besides the gem's own sources.
      def stamp(spec)
        archive = File.join(
          File.dirname(RbConfig::CONFIG["rubyhdrdir"]), "capi", "libmonoruby_capi.a"
        )
        st = File.stat(archive) rescue nil
        "#{spec.full_gem_path}\n#{archive} #{st&.size} #{st&.mtime&.to_i}\n"
      end

      def built?(dir, spec)
        File.read(File.join(dir, "gem.build_complete")) == stamp(spec)
      rescue SystemCallError
        false
      end

      def build_into(dir, spec)
        require "fileutils"
        require "tmpdir"
        require "rubygems/ext"
        parent = File.dirname(dir)
        FileUtils.mkdir_p(parent)
        File.open(File.join(parent, ".#{spec.full_name}.lock"), File::RDWR | File::CREAT, 0o644) do |lock|
          lock.flock(File::LOCK_EX)
          # Another process may have built it while this one waited.
          return true if built?(dir, spec)
          work = Dir.mktmpdir(".#{spec.full_name}.", parent)
          begin
            src = File.join(work, "src")
            out = File.join(work, "out")
            FileUtils.mkdir_p(out)
            FileUtils.cp_r(spec.full_gem_path, src)
            results = []
            without_bundler do
              spec.extensions.each do |ext|
                ext_dir = File.join(src, File.dirname(ext))
                # The host's build left its Makefile (and maybe objects)
                # behind; extconf.rb rewrites the former, the latter must go.
                Dir.glob(File.join(ext_dir, "**", "*.{o,so,bundle}")).each { |f| File.delete(f) }
                Gem::Ext::ExtConfBuilder.build(ext, out, results, [], nil, ext_dir)
              end
            end
            File.write(File.join(out, "gem_make.out"), results.join("\n"))
            File.write(File.join(out, "gem.build_complete"), stamp(spec))
            FileUtils.rm_rf(dir)
            File.rename(out, dir)
            true
          rescue StandardError, ScriptError => e
            log = File.join(parent, "#{spec.full_name}.gem_make.out")
            File.write(log, [*results, e.full_message(highlight: false)].join("\n")) rescue nil
            warn "monoruby: could not build #{spec.full_name}'s C extension for monoruby (see #{log})"
            false
          ensure
            FileUtils.rm_rf(work)
          end
        end
      end

      # extconf.rb and make run under monoruby with the environment
      # Bundler.setup leaves behind (RUBYOPT=-rbundler/setup, BUNDLE_GEMFILE):
      # the child would set up the benchmark's bundle for nothing.
      def without_bundler(&blk)
        if defined?(::Bundler) && ::Bundler.respond_to?(:with_unbundled_env)
          ::Bundler.with_unbundled_env(&blk)
        else
          yield
        end
      end
    end
  end
end
