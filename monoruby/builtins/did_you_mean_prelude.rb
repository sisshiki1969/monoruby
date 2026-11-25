# did_you_mean, deferred.
#
# CRuby's gem_prelude requires it at every start, after error_highlight:
# it prepends `DidYouMean::Correctable` to NameError, KeyError, LoadError
# and NoMatchingPatternKeyError, whose `detailed_message` appends
# "Did you mean?  ..." to the report of an uncaught one. As with
# `ErrorHighlight` (builtins/gem_prelude.rb), only a failing program
# shows any of it, so the constant is an autoload: `defined?` and
# `Object.constants` report it, any reference loads it, and
# `Exception#detailed_message` loads it the first time it is asked for
# the message of an exception of one of the four classes
# (builtins/exception.rb).
#
# The three methods `Correctable` gives those classes are there from the
# start too, as stand-ins that load the library and call the real one.
# `stdlib/monoruby_hooks/did_you_mean.rb`, run while the library is
# being loaded, takes them away again.
#
# `--disable-did_you_mean` (and `--disable-gems`, as in CRuby) skips
# this file, so the constant is undefined and the methods are not there.
autoload :DidYouMean, "did_you_mean"

[::NameError, ::KeyError, ::LoadError, ::NoMatchingPatternKeyError].each do |klass|
  klass.class_eval do
    def corrections
      return corrections if ::Exception.__load_did_you_mean(self)
      ::Kernel.raise ::NoMethodError.new("undefined method 'corrections' for an instance of #{self.class}", :corrections, receiver: self)
    end

    def spell_checker
      return spell_checker if ::Exception.__load_did_you_mean(self)
      ::Kernel.raise ::NoMethodError.new("undefined method 'spell_checker' for an instance of #{self.class}", :spell_checker, receiver: self)
    end

    def original_message
      return original_message if ::Exception.__load_did_you_mean(self)
      ::Kernel.raise ::NoMethodError.new("undefined method 'original_message' for an instance of #{self.class}", :original_message, receiver: self)
    end
  end
end
