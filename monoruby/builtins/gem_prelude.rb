# The rubygems boot, deferred.
#
# CRuby's gem_prelude requires rubygems at every start. Loading the
# vendored rubygems (some twenty files, `Gem::Specification` among them)
# is most of monoruby's startup: about 8 MB of RSS and 90 ms for a
# program that never touches `Gem`. Register the constant as an autoload
# instead, so the first reference to `Gem` -- from a program, a library,
# or `require "rubygems"` itself -- pays for the boot, and nothing else
# does. `defined?(Gem)`, `Object.const_defined?(:Gem)` and
# `Object.constants` all report the constant as present without loading
# it, as with any autoload.
#
# `Kernel#gem` is the one rubygems entry point that is a method, not a
# constant. Until the boot runs it is this stub, which boots rubygems and
# re-dispatches to the definition `rubygems/core_ext/kernel_gem.rb` put
# in its place.
#
# Skipped entirely under `--disable-gems`, so `Gem` is then undefined.
autoload :Gem, "rubygems"

module Kernel
  private

  def gem(gem_name, *requirements)
    require "rubygems"
    gem(gem_name, *requirements)
  end
end
