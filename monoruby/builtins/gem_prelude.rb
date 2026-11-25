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

# error_highlight, deferred as well.
#
# CRuby's gem_prelude requires it at every start too, and it prepends
# `ErrorHighlight::CoreExt#detailed_message` to NameError, TypeError and
# ArgumentError: the source line with a caret under the report of an
# uncaught one. Loading it costs about 6% of monoruby's startup (some
# 14M of 226M instructions) for something only a failing program shows,
# so the constant is an autoload here, as `Gem` is: `defined?` and
# `Object.constants` report it, any reference loads it, and
# `Exception#detailed_message` loads it the first time it is asked for
# the message of an exception of one of those three classes
# (builtins/exception.rb). Until then the three classes' ancestors lack
# `ErrorHighlight::CoreExt`, which is the one visible difference.
#
# `--disable-error_highlight` (and `--disable-gems`, as in CRuby) leaves
# the constant undefined.
autoload :ErrorHighlight, "error_highlight"
