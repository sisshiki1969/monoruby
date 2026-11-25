# frozen_string_literal: true

# Loaded by monoruby right after `did_you_mean/core_ext/name_error.rb`
# (`Executor::run_load_hooks`), which did_you_mean.rb requires before
# anything else of its own, however the library came to be loaded: the
# first error report that wanted a suggestion, a reference to
# `DidYouMean`, or a `require`.
#
# monoruby registers `DidYouMean` as an autoload instead of loading it
# at boot (builtins/did_you_mean_prelude.rb), and until the load the
# four classes it corrects carry stand-ins for the methods of
# `DidYouMean::Correctable`. That module exists now and did_you_mean.rb
# is about to prepend it to them, so the stand-ins go. They are told
# from a method of the same name a program defined by where they were
# defined, which is why this runs before the prepend: after it,
# `instance_method` would answer the module's method.
[::NameError, ::KeyError, ::LoadError, ::NoMatchingPatternKeyError].each do |klass|
  %i[corrections spell_checker original_message].each do |name|
    next unless klass.instance_methods(false).include?(name)
    source = klass.instance_method(name).source_location
    klass.__send__(:remove_method, name) if source && source[0] == "<internal:did_you_mean_prelude>"
  end
end
