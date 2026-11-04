# Driven on CRuby and on monoruby by tests/capi.rs; the outputs must match.
require "capi_cxx"
C = CapiCxx

def t = (yield; :ok) rescue [$!.class, $!.message]

# new through the alloc function: keywords reach initialize, and so does
# the type tag the alloc function set
p C.alloc_api?
th = C::Thing.new(2, 3, k: 4)
p th.sum, th.args, th.tag, th.instance_variables
class Sub < C::Thing
  def initialize(*args)
    super
    @sub = true
  end
end
s = Sub.new(5, 6)
p s.sum, s.tag, s.instance_variables

# rb_rescue / rb_rescue2 / rb_ensure with ANYARGS-cast functions
p C.guard(false), C.guard(true)
p C.guard2(false), C.guard2(true)
p t { C.guard2(:other) }
p C.ensure([])

# st_foreach, rb_block_call, rb_proc_new, rb_catch, rb_ivar_foreach
p C.table
p C.block_call([1, 2, 3], 10)
pr = C.proc(3)
p pr.call(4)
p C.catch(:thrown)
class Box
  def initialize
    @secret = 21
    @other = :x
  end
end
p C.ieval(Box.new)
p C.ivars(Box.new)

# the wrapper's globals
p $capi_cxx_runtime, defined?($capi_cxx_runtime)
p t { $capi_cxx_runtime = 1 }
p $capi_cxx_trackings, $capi_cxx_trackings
p t { $capi_cxx_trackings = 5 }
