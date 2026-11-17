# Probes for #1998: the methods a builtin calls on its operands, and the
# calls a scope writes itself, with every one of those methods refined
# for `R` in the scope the probes are written in. Each probe runs
# `N1998` times (the harness sets it) so that the compiled code answers
# too; a probe whose answers differ between runs shows up as a row with
# more than one value.
$res1998 = []
module T
  def self.t(label)
    rs = []
    N1998.times do
      r = begin
        yield
      rescue Exception => e
        e.class
      end
      rs << r.inspect.gsub(/0x\h+/, "XX")
    end
    $res1998 << [label, rs.uniq]
  end
end
class R
  attr_reader :v
  def initialize(v) = @v = v
end
module M1
  refine R do
    def <=>(o) = v <=> o.v
    def coerce(o) = [1, 2]
    def to_ary = [1, 2]
    def to_a = [9]
    def to_s = "refined"
    def inspect = "refined-inspect"
    def =~(o) = true
    def succ = R.new(v + 1)
    def to_str = "refstr"
    def to_int = 7
    def to_i = 7
    def to_f = 7.5
    def to_r = 7r
    def to_hash = {a: 1}
    def to_h = {b: 2}
    def to_sym = :refsym
    def to_proc = proc { |x| [:refproc, x] }
    def to_path = "/refpath"
    def hash = 42
    def eql?(o) = true
    def ==(o) = true
    def ===(o) = true
    def each
      yield 1
      yield 2
    end
    def call(*a) = [:refcall, *a]
    def +(o) = :refplus
    def -@ = :refneg
    def [](i) = [:refidx, i]
    def !
      :refnot
    end
    def size = 3
    def foo = :reffoo
    def deconstruct = [1, 2]
    def deconstruct_keys(k) = {a: 1}
    def exception(*a) = RuntimeError.new("refexc")
    def begin = 1
    def end = 2
    def marshal_dump = [1]
    def write(s) = ($w = s; 1)
    def name = "refname"
    def <<(o) = :refshl
    def zero? = true
    def empty? = true
    def -(o) = 1
    def <(o) = true
    def >(o) = false
    def respond_to_missing?(n, p = false) = n == :zzz
    def method_missing(n, *a) = n == :zzz ? :refmm : super
    def initialize_copy(o)
      @v = :refcopy
    end
    def nil? = true
    def bar=(x)
      $bar = x
    end
    def to_enum(*) = :refenum
    def itself = :refitself
    def frozen? = true
    def lambda? = true
    def arity = 1
    def call2 = 1
    def length = 3
    def join(*) = "refjoin"
    def flatten = :refflatten
    def to_json(*) = "refjson"
    def then = :refthen
    def step = 1
    def divmod(o) = [:q, :r]
    def *(o) = :refmul
    def **(o) = :refpow
    def %(o) = :refmod
    def /(o) = :refdiv
    def >=(o) = true
    def <=(o) = true
    def !=(o) = false
    def &(o) = :refand
    def |(o) = :refor
    def ^(o) = :refxor
    def >>(o) = :refshr
    def ~ = :refbnot
    def +@ = :refpos
    def []=(i, x)
      $set = [i, x]
    end
    def to_c = Complex(7, 1)
    def real? = true
    def integer? = true
    def finite? = true
    def nan? = false
    def abs = 5
    def numerator = 3
    def denominator = 4
    def each_pair = :refep
    def keys = [:refk]
    def key?(k) = true
    def fetch(*) = :reffetch
    def first = 5
    def last = 6
    def min = 0
    def max = 9
    def include?(o) = true
    def member?(o) = true
    def close = ($closed = true)
    def read(*) = "refread"
    def getbyte = 1
    def path = "/refpath2"
    def fileno = 1
    def tr(*) = "reftr"
    def sub(*) = "refsub"
    def encoding = Encoding::UTF_8
    def force_encoding(*) = self
    def b = "refb"
    def unpack(*) = [1]
    def bytes = [1]
    def chars = ["c"]
    def strip = "refstrip"
    def match(*) = :refmatch
    def match?(*) = true
    def casecmp(o) = 0
    def start_with?(*) = true
    def to_time = :reftime
    def year = 2000
    def utc_offset = 0
    def round(*) = 1
    def floor(*) = 1
    def ceil(*) = 2
    def truncate(*) = 1
    def quo(o) = :refquo
    def fdiv(o) = :reffdiv
    def pred = R.new(v - 1)
    def lock = :reflock
    def owned? = true
    def pos = 0
    def rewind = 0
    def size_hint = 0
    def next = :refnext
    def peek = :refpeek
    def tty? = true
    def sync = true
    def flush = self
    def puts(*) = :refputs
    def print(*) = :refprint
    def <<(o) = :refshl
  end
end
module G1
  using M1
  r = R.new(2); r1 = R.new(1)
  T.t("sort") { [r, r1].sort.map(&:v) }
  T.t("sort!") { [r, r1].sort!.map(&:v) }
  T.t("min") { [r, r1].min.v }
  T.t("max") { [r, r1].max.v }
  T.t("minmax") { [r, r1].minmax.map(&:v) }
  T.t("sort_by") { [r, r1].sort_by { |x| x }.map(&:v) }
  T.t("min_by") { [r, r1].min_by { |x| x }.v }
  T.t("Comparable clamp") { 1.clamp(r1, r) }
  T.t("ary <=>") { [r] <=> [r1] }
  T.t("1 + r") { 1 + r }
  T.t("1.0 + r") { 1.0 + r }
  T.t("1 < r") { 1 < r }
  T.t("1 == r") { 1 == r }
  T.t("1.0 * r") { 1.0 * r }
  T.t("(2**70) + r") { (2**70) + r }
  T.t("1r + r") { 1r + r }
  T.t("sum") { [r].sum(0) }
  T.t("sum2") { [r, r].sum }
  T.t("inject(:+)") { [r, r].inject(:+) }
  T.t("inject sym op") { [1, r].inject(:+) }
  T.t("str =~") { "x" =~ r }
  T.t("sym =~") { :x =~ r }
  T.t("range to_a") { (r1..r).to_a.size }
  T.t("range each") { n = 0; (r1..r).each { n += 1 }; n }
  T.t("range include") { (r1..r).include?(r) }
  T.t("range ===") { (r1..r) === r }
  T.t("range cover") { (r1..r).cover?(r) }
  T.t("range step") { (r1..r).step(1).to_a.size }
  T.t("masgn") { a, b = r; [a, b] }
  T.t("masgn nested") { (a, b), c = r, 3; [a, b, c] }
  T.t("block param destructure") { [r].each { |a, b| break [a, b] } }
  T.t("block param destructure2") { [[r, 5]].each { |(a, b), c| break [a, b, c] } }
  T.t("proc call destructure") { proc { |a, b| [a, b] }.call(r) }
  T.t("yield destructure") { def self.y(x) = yield(x); y(r) { |a, b| [a, b] } }
  T.t("splat") { [*r].size }
  T.t("splat call") { def self.s(*a) = a.size; s(*r) }
  T.t("splat ary lit") { [0, *r].size }
  T.t("Array()") { Array(r).size }
  T.t("ary inspect") { [r].inspect }
  T.t("ary to_s") { [r].to_s }
  T.t("hash inspect") { {a: r}.inspect }
  T.t("struct inspect") { Struct.new(:a).new(r).inspect }
  T.t("p") { r.inspect }
  T.t("Kernel#inspect default") { Object.instance_method(:inspect).bind_call(r).sub(/0x\h+/, "XX") }
  T.t("interp") { "#{r}" }
  T.t("r.to_s") { r.to_s }
  T.t("String(r)") { String(r).sub(/0x\h+/, "XX") }
  T.t("format %s") { format("%s", r).sub(/0x\h+/, "XX") }
  T.t("format %p") { format("%p", r).sub(/0x\h+/, "XX") }
  T.t("format %d") { format("%d", r) }
  T.t("str % r") { ("%s" % r).sub(/0x\h+/, "XX") }
  T.t("join") { [r].join.sub(/0x\h+/, "XX") }
  T.t("str + r") { "a" + r }
  T.t("str << r") { +"a" << r }
  T.t("str == r") { "refstr" == r }
  T.t("str.concat") { (+"a").concat(r) }
  T.t("str * r") { "a" * r }
  T.t("ary[r]") { [0, 1, 2, 3, 4, 5, 6, 7, 8][r] }
  T.t("ary.first(r)") { [1, 2].first(r) }
  T.t("Integer(r)") { Integer(r) }
  T.t("Float(r)") { Float(r) }
  T.t("Rational(r)") { Rational(r) }
  T.t("Hash(r)") { Hash(r) }
  T.t("**r") { def self.k(**kw) = kw; k(**r) }
  T.t("hash merge") { {}.merge(r) }
  T.t("hash update") { {z: 1}.update(r) }
  T.t("ary + r") { [0] + r }
  T.t("ary concat") { [0].concat(r) }
  T.t("ary flatten") { [[r]].flatten.size }
  T.t("ary == r") { [1, 2] == r }
  T.t("ary product") { [1].product(r) }
  T.t("ary zip") { [1].zip(r) }
  T.t("ary transpose") { [r].transpose }
  T.t("ary to_h") { [r].to_h }
  T.t("hash key") { h = {r => 1}; h[R.new(2)] }
  T.t("uniq") { [r, r1].uniq.size }
  T.t("ary include") { [r1].include?(r) }
  T.t("ary index") { [r1].index(r) }
  T.t("ary delete") { [r1].delete(r).nil? }
  T.t("ary - ") { ([r1] - [r]).size }
  T.t("ary & ") { ([r1] & [r]).size }
  T.t("ary count") { [r1].count(r) }
  T.t("ary == ary") { [r1] == [r] }
  T.t("ary eql") { [r1].eql?([r]) }
  T.t("hash == hash") { {a: r1} == {a: r} }
  T.t("ary assoc") { [[r1, 1]].assoc(r) }
  T.t("r == r1 direct") { r == r1 }
  T.t("r != r1 direct") { r != r1 }
  T.t("r.eql direct") { r.eql?(r1) }
  T.t("r.equal?") { r.equal?(r1) }
  T.t("case when r") { case 5 when r then :hit else :miss end }
  T.t("case r when") { case r when R then :hit else :miss end }
  T.t("rescue r") { begin; raise "x"; rescue r; :hit; end }
  T.t("ary grep") { [5].grep(r) }
  T.t("any?(r)") { [5].any?(r) }
  T.t("all?(r)") { [5].all?(r) }
  T.t("none?(r)") { [5].none?(r) }
  T.t("slice_when etc: chunk_while") { [5, 6].slice_before(r).to_a }
  T.t("&r") { [5].map(&r) }
  T.t("for in r") { a = []; for x in r do a << x end; a }
  T.t("r.()") { r.(1) }
  T.t("r[1]") { r[1] }
  T.t("r[1]=") { r[1] = 2; $set }
  T.t("r.bar=") { r.bar = 2; $bar }
  T.t("r.bar op=") { $bar = nil; (r.foo rescue 1); $bar }
  T.t("-r") { -r }
  T.t("+r") { +r }
  T.t("!r") { !r }
  T.t("not r") { not r }
  T.t("~r") { ~r }
  T.t("r + 1") { r + 1 }
  T.t("r << 1") { r << 1 }
  T.t("r < 1") { r < 1 }
  T.t("r != 1") { r != 1 }
  T.t("r&.foo") { r&.foo }
  T.t("r.zzz mm") { r.zzz }
  T.t("r.respond_to zzz") { r.respond_to?(:zzz) }
  T.t("r.respond_to foo") { r.respond_to?(:foo) }
  T.t("r.send foo") { r.send(:foo) }
  T.t("r.public_send foo") { r.public_send(:foo) }
  T.t("r.__send__ foo") { r.__send__(:foo) }
  T.t("r.method foo") { r.method(:foo).call }
  T.t("r.method foo unbind") { r.method(:foo).unbind.bind_call(r) }
  T.t("R.instance_method foo") { R.instance_method(:foo).bind_call(r) }
  T.t("R.method_defined? foo") { R.method_defined?(:foo) }
  T.t("R.instance_methods foo") { R.instance_methods(false).include?(:foo) }
  T.t("r.methods foo") { r.methods.include?(:foo) }
  T.t("R.public_method_defined? foo") { R.public_method_defined?(:foo) }
  T.t("r.public_method foo") { r.public_method(:foo).call }
  T.t("defined? r.foo") { defined?(r.foo) }
  T.t("map(&:foo)") { [r].map(&:foo) }
  T.t("each(&:foo)") { x = nil; [r].each(&:foo); 1 }
  T.t(":foo.to_proc") { :foo.to_proc.call(r) }
  T.t("method(:x).to_proc &") { [r].map(&:v) }
  T.t("instance_eval str") { r.instance_eval("foo") }
  T.t("instance_eval blk") { r.instance_eval { foo } }
  T.t("instance_exec") { r.instance_exec { foo } }
  T.t("eval") { eval("r.foo") }
  T.t("binding.eval") { binding.eval("r.foo") }
  T.t("tap") { r.tap { |x| break x.foo } }
  T.t("then") { r.then { |x| x } .class }
  T.t("pattern ary") { case r; in [a, b] then [a, b]; else :miss; end }
  T.t("pattern hash") { case r; in {a:} then a; else :miss; end }
  T.t("pattern =>") { r => [a, b]; [a, b] }
  T.t("pattern in") { (r in [1, 2]) }
  T.t("raise r") { begin; raise r; rescue => e; e.message; end }
  T.t("raise R, msg") { begin; raise r, "m"; rescue => e; e.message; end }
  T.t("dup") { r.dup.v }
  T.t("clone") { r.clone.v }
  T.t("Integer#times w/ r") { 3.step(r) { break :stepped } }
  T.t("1.step(by: r)") { 1.step(3, r).first(1) }
  T.t("Array.new(r)") { Array.new(r).size }
  T.t("Array#*") { [1] * r }
  T.t("ary.fetch(r)") { [1, 2, 3, 4, 5, 6, 7, 8].fetch(r) }
  T.t("str[r]") { "abcdefghij"[r] }
  T.t("str.center(r)") { "a".center(r) }
  T.t("str.to_sym etc: send(r)") { 1.send(r) rescue $!.class }
  T.t("File.join(r)") { File.join(r) }
  T.t("File.basename(r)") { File.basename(r) }
  T.t("Comparable ==") { c = Class.new { include Comparable; def <=>(o) = 0 }.new; c == r }
  T.t("r.frozen?") { r.frozen? }
  T.t("r.nil?") { r.nil? }
  T.t("r.itself") { r.itself.class }
  T.t("if r.nil?") { r.nil? ? 1 : 2 }
  T.t("Hash#fetch default to_s err") { {}.fetch(r) rescue $!.message.sub(/0x\h+/, "XX") }
  T.t("NoMethodError msg") { r.nope rescue $!.message.sub(/0x\h+/, "XX") }
  T.t("ary.pack") { [r].pack("C") }
  T.t("ary.pack a") { [r].pack("a3") }
  T.t("Math.sqrt(r)") { Math.sqrt(r) }
  T.t("1.0.round(r)") { 1.23456789.round(r) }
  T.t("1 <=> r") { 1 <=> r }
  T.t("1.fdiv(r)") { 1.fdiv(r) }
  T.t("1.divmod(r)") { 1.divmod(r) }
  T.t("1 ** r") { 1 ** r }
  T.t("1 & r") { 1 & r }
  T.t("1 << r") { 1 << r }
  T.t("1[r]") { 1[r] }
  T.t("Complex(1,2)+r") { Complex(1, 2) + r }
  T.t("Time.at(r)") { Time.at(r).to_i }
  T.t("Struct#to_a ==") { s = Struct.new(:a); s.new(r1) == s.new(r) }
  T.t("Struct#hash") { s = Struct.new(:a); s.new(r1).hash == s.new(r1).hash }
  T.t("ary hash") { [r].hash == [r1].hash }
  T.t("Set include") { require "set"; Set[r1].include?(r) }
  T.t("ary.flatten(r)") { [[[1]]].flatten(r) }
  T.t("ary.rotate(r)") { [1, 2, 3].rotate(r) }
  T.t("ary.dig") { [r].dig(0, 1) }
  T.t("hash.dig") { {a: r}.dig(:a, 1) }
  T.t("obj.to_enum each") { r.enum_for(:each).to_a }
  T.t("Enumerator.new size") { Enumerator.new(r) { |y| y << 1 }.size }
  T.t("ObjectSpace etc. instance_variable_get") { r.instance_variable_get(:@v) }
  T.t("zip block") { [1].zip([r]) { |a, b| break b.foo } }
  T.t("ary.each_slice(r)") { [1, 2].each_slice(R.new(1)).to_a rescue $!.class }
  T.t("ary.first/take(r)") { [1, 2, 3, 4, 5, 6, 7, 8].take(r).size }
  T.t("ary * str") { [1, 2] * r }
  T.t("String.new(r)") { String.new(r) }
  T.t("Symbol cmp") { :a <=> r }
  T.t("str <=> r") { "a" <=> r }
  T.t("str.eql") { "a".eql?(r) }
  T.t("str.include") { "refstr".include?(r) }
  T.t("str.sub(r)") { "refstr".sub(r, "X") }
  T.t("str.start_with") { "refstr".start_with?(r) }
  T.t("str.tr") { "refstr".tr(r, "X") }
  T.t("str.split") { "arefstrb".split(r) }
  T.t("Regexp.new(r)") { Regexp.new(r).source }
  T.t("Regexp.union(r)") { Regexp.union(r).source }
  T.t("re =~ r") { /ref/ =~ r }
  T.t("re.match(r)") { /ref/.match(r).to_s }
  T.t("re === r") { /ref/ === r }
  T.t("define_method(:x, &r)") { Class.new { define_method(:x, &r) }.new.x(1) rescue $!.class }
  T.t("instance_variable_get(r)") { r.instance_variable_get(r) rescue $!.class }
  T.t("send(r)") { 1.send(r) rescue $!.class }
  T.t("const_get(r)") { Object.const_get(r) rescue $!.class }
  T.t("ary.sum float") { [1.0, r].sum }
  T.t("Integer.sqrt(r)") { Integer.sqrt(r) }
  T.t("r.then") { r.then }
  T.t("r.to_enum") { r.to_enum }
  T.t("Range.new(r1, r)") { Range.new(r1, r).class }
  T.t("r1..r lit") { (r1..r).class }
  T.t("(1..r)") { (1..r).class }
  T.t("ary.bsearch") { [1, 2, 3].bsearch { |x| r } }
  T.t("ary.sort block") { [2, 1].sort { |a, b| r } }
  T.t("ary.max block") { [2, 1].max { |a, b| r } }
  T.t("ary.sort_by block ret") { [2, 1].sort_by { |a| R.new(a) }  }
  T.t("ary.min(n)") { [r, r1].min(1).size }
  T.t("cycle(r)") { [1].cycle(r).to_a.size }
  T.t("ary.sample(r)") { [1, 2, 3, 4, 5, 6, 7, 8, 9].sample(r).size }
  T.t("IO.write to r") { $w = nil; $stdout.respond_to?(:write) }
  T.t("Hash#to_a") { {r => 1}.to_a.size }
  T.t("ary.join nested") { [[r]].join.sub(/0x\h+/, "XX") }
  T.t("puts r capture") { require "stringio"; s = StringIO.new; s.puts(r); s.string.sub(/0x\h+/, "XX") }
  T.t("print r capture") { s = StringIO.new; s.print(r); s.string.sub(/0x\h+/, "XX") }
  T.t("puts [r]") { s = StringIO.new; s.puts([r]); s.string.sub(/0x\h+/, "XX") }
  T.t("s << r") { s = StringIO.new; s << r; s.string.sub(/0x\h+/, "XX") }
  T.t("s.write r") { s = StringIO.new; s.write(r); s.string.sub(/0x\h+/, "XX") }
  T.t("p r capture") { o = $stdout; $stdout = s = StringIO.new; p(r); $stdout = o; s.string.sub(/0x\h+/, "XX") }
  T.t("puts r stdout capture") { o = $stdout; $stdout = s = StringIO.new; puts(r); $stdout = o; s.string.sub(/0x\h+/, "XX") }
  T.t("print r stdout capture") { o = $stdout; $stdout = s = StringIO.new; print(r); $stdout = o; s.string.sub(/0x\h+/, "XX") }
  T.t("warn r") { o = $stderr; $stderr = s = StringIO.new; warn(r); $stderr = o; s.string.sub(/0x\h+/, "XX") }
  T.t("ObjectSpace done") { :done }
end
$res1998
