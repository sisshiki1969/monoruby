# Run after `gem install capi_gem`, on CRuby and on monoruby; the output
# must match except for the BUILT_WITH line, which names the interpreter.
require "capi_gem"
p CapiGem::VERSION
p CapiGem.greet("gem")
p CapiGem.sum(1, 2, 3), CapiGem.sum(4) { |s| s * 10 }, CapiGem.twice(21)
c = CapiGem::Counter.new
p [c.incr, c.incr, c.incr]
begin
  CapiGem.greet(1)
rescue TypeError => e
  p e.message
end
spec = Gem.loaded_specs["capi_gem"]
p spec.version.to_s, spec.extensions
p $LOADED_FEATURES.grep(/capi_gem\/capi_gem\.(so|bundle)\z/).size
