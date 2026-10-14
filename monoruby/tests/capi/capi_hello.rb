# Run by tests/capi.rs on both CRuby and monoruby, each with its own build
# of capi_hello.c on $LOAD_PATH; the two outputs must be identical.
p require "capi_hello"
p require "capi_hello"
H = CapiHello
p H::VERSION, H::VERSION.frozen?
p H.add(2, 40), H.add(2**40, 2**40)
p H.zero
p H.count, H.count(1, "a", :b)
p H.args_ary(1, 2, 3)
p H.six(1, 2, 3, 4, 5, 6)
p (begin; H.add(1); rescue ArgumentError => e; e.message; end)
p (begin; H.six(1); rescue ArgumentError => e; e.message; end)
p H.scan(1)
p H.scan(1, 2, 3, 4, 5)
p H.scan(1, 2, k: 3) { |x| x * 100 }
p (begin; H.scan; rescue ArgumentError => e; e.message; end)
p H.kwargs(name: "n")
p H.kwargs(name: "n", color: :red, size: 3)
p (begin; H.kwargs(size: 1); rescue ArgumentError => e; e.message; end)
p (begin; H.kwargs(name: 1, bad: 2); rescue ArgumentError => e; e.message; end)
p [nil, true, false, 1, 2**70, 1.5, 1e300 * 10, :s, "s", [], {}, String, Kernel, /re/, (1..2), Struct.new(:a).new(1), Object.new].map { |v| H.types(v) }
p [nil, false, true, 0, "", :x].map { |v| H.truthy(v) }
p H.nums(10), H.nums(-3)
p (begin; H.nums(nil); rescue TypeError => e; e.message; end)
p (begin; H.nums("1"); rescue TypeError => e; e.message; end)
p H.int(2**31 - 1)
p (begin; H.int(2**31); rescue RangeError => e; e.message; end)
p (begin; H.int(-2**31 - 1); rescue RangeError => e; e.message; end)
p (begin; H.int(2**64); rescue RangeError => e; e.message; end)
p H.uint(2**32 - 1), H.uint(-1)
p H.big
p H.ull(2**64 - 1), H.ull(12345), H.ull(2**63)
p H.float(2), H.float(0.5)
p H.greet("world")
p (begin; H.greet(1); rescue TypeError => e; e.message; end)
p H.upcase("hello, world")
o = Object.new; def o.to_str = "from to_str"
p H.upcase(o)
p (begin; H.upcase(1); rescue TypeError => e; e.message; end)
p H.cstr("abc")
p (begin; H.cstr("a\0b"); rescue ArgumentError => e; e.message; end)
s = H.strings
p s
p s.map { |x| x.is_a?(String) ? x.encoding.to_s : x }
p (begin; H.frozen_cat("lit".freeze); rescue FrozenError => e; e.message; end)
p H.frozen_cat(+"ok")
p H.syms(:foo), H.syms("bar")
p (begin; H.syms(1); rescue TypeError => e; e.message; end)
p H.sum([1, 2, 3, 4])
p (begin; H.sum("x"); rescue TypeError => e; e.message; end)
p H.arrays
h = {a: 1, b: 2, c: 3}
p H.hashes(h)
p H.call([3, 1, 2], :push, 9), H.call("abc", "center", 9)
p H.each_pair([1, 2, 3, 4, 5]) { |a, b| a * 10 + b }
p H.each_pair([1, 2])
p (begin; H.each_pair([1, 2]) { |a, b| raise IOError, "in block" }; rescue IOError => e; e.message; end)
p H.to_proc { |x| x + 1 }
5.times do |i|
  p (begin; H.raise(i); rescue StandardError => e; [e.class, e.message]; end)
end
p H.protect(false), H.protect(true)
p (begin; H.reraise; rescue => e; [e.class, e.message]; end)
p H.ensure(false), H.ensure_log
p (begin; H.ensure(true); rescue => e; e.message; end), H.ensure_log
p H.rescue(-> { raise ArgumentError, "arg" })
p H.rescue(-> { raise KeyError, "key" })
p H.rescue(-> { :fine })
p (begin; H.rescue(-> { raise IOError, "io" }); rescue IOError => e; [:passed_through, e.message]; end)
p H.check_frozen("x")
p (begin; H.check_frozen([1].freeze); rescue FrozenError => e; e.message; end)
pt = H::Point.new(3, 4)
p pt, pt.dist2, pt.x, pt.y, (pt.y = 10), pt.dist2
p (begin; pt.secret; rescue NoMethodError => e; e.message[0, 20]; end)
p pt.send(:secret)
p (begin; pt.x = 1; rescue NoMethodError => e; e.message[0, 20]; end)
p H.objects
p H::Error.ancestors.take(3)
p H.cached
p H.gc_churn(20000)
GC.start
p H.cached
p H.eval("[1, 2].sum + 3")
p H.range(1..5), H.range(1...5), H.range("a".."c"), H.range(3)
p H.nogvl(100000)
t = 0
3000.times { |i| t += H.add(i, 1) }
p t
