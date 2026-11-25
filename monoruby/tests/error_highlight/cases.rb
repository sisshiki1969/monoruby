# What `Exception#detailed_message` answers for a NameError, a TypeError
# or an ArgumentError raised at a known place, one case per `t` block.
# cases.expected is this script's output on CRuby 4.0.9
# (`ruby --disable-did_you_mean cases.rb`); tests/error_highlight.rs runs
# it on monoruby and compares.
require_relative "lib"

$n = 0
def t
  $n += 1
  yield
  puts "#{$n}: no error"
rescue NameError, TypeError, ArgumentError, RuntimeError => e
  puts "#{$n}: #{e.class}", e.detailed_message(highlight: false), "--"
end

class Foo
  attr_accessor :bar
  def one(a) = a
  def kw(a:, b: 1) = a
  def self.cm(a, b) = a
  def blk = yield(1)
  def [](i) = raise(TypeError, "idx")
  def []=(i, v)
    raise TypeError, "idx="
  end
  def +(o) = raise(ArgumentError, "plus")
  def deep(x)
    x.undefined_thing
  end
end

foo = Foo.new
v = []
nilv = nil

# --- NoMethodError: the method name
t { nil.foo }
t { nilv.foo }
t { nilv.foo(1, 2) }
t { nilv&.itself.foo }
t { nilv.foo = 1 }
t { nilv.foo += 1 }
t { nilv.foo ||= 1 }
t { nilv[0] }
t { nilv[0] = 1 }
t { nilv[0] += 1 }
t { nilv + 1 }
t { nilv.+(1) }
t { -nilv }
t { !nilv.foo }
t { (nilv).foo }
t { nilv .foo }
t { nilv::foo }
t { nilv.foo() }
t { nilv.() }
t { nilv.foo { 1 } }
t { nilv.foo do 1 end }
t { foo.bar += 1 }
t { foo.bar.baz += 1 }
t { v[0] += 1 }
t { x = nil; x += 1 }
t { foo.deep(nil) }
t { [1].each { |x| x.nope } }
t { 3.times { |i| nilv.bar if i == 2 } }
t { proc { |a| a.nope }.call(nil) }
t { foo.blk { |a, b| nilv.zip } }
t { send(:nope_sent) }
t { super_duper rescue raise }

# --- NameError: variables and constants
t { foo_undefined }
t { foo_undefined(1) }
t { foo_undefined 1, 2 }
t { self.foo_undefined }
t { instance_eval { nopey } }
t { NotDefinedConst }
t { Foo::NotDefined }
t { Foo::NotDefined::Deeper }
t { ::NotDefinedTop }
t { Foo::NotDefined += 1 }
t { NotDefinedConst2 += 1 }
t {
  Foo::
    NotDefined3 += 1
}
t { raise NameError.new("ne", :xyz) }

# --- TypeError / ArgumentError: the arguments
t { 1 + nil }
t { 1 + nilv }
t { 1.+(nil) }
t { [1, 2].first(nil) }
t { Integer("z") }
t { Integer(nil) }
t { "a" + 1 }
t { [1] + nil }
t { foo[nil] }
t { foo[1] = 2 }
t { foo + 1 }
t { x = 1; x += nil }
t { [1, 2].sum("a") }
t { raise ArgumentError, "plain" }
t { raise TypeError }
t { raise(ArgumentError) }

# --- arity and keyword errors: the caller and the callee
t { foo.one }
t { foo.one(1, 2) }
t { foo.kw }
t { foo.kw(a: 1, c: 2) }
t { foo.kw(**{z: 1}) }
t { foo.one(*[]) }
t { foo.one(**{}) }
t { Foo.cm(1) }
t { foo.send(:one) }
t { foo.method(:one).call }
t { foo.public_send(:one, 1, 2) }
t { ->(a) { a }.call }
t { lambda { |a, b| a }.call(1) }
t { def self.tmp(a, b = 1, *c, d:); end; tmp }
t { define_singleton_method(:dm) { |a| a }; dm }
t { define_singleton_method(:dm2) do |a, b|
      a
    end; dm2(1) }
t { v.fetch(1, 2, 3) }
t { v.first(1, 2) }
t { {a: 1}.fetch }
t { lib_arity(1) }

# --- more than one line: only a one-line extent is shown
t {
  nilv
    .foo
}
t {
  nilv.
    foo(1)
}
t {
  foo.one(1,
          2)
}

# --- other sources
t { lib_call(nil) }
t { eval("nil.evaled", nil, __FILE__, __LINE__) }
t { eval("nil.evaled2") }
t { Foo.new.instance_eval("@a.nope") }
t { "é".nope }
t { nilv.nopé }
t { 	nilv.tabbed }
t { Dir.chdir("/") { nilv.chdired } }
t { Dir.chdir("/") { lib_call(nil) } }

# --- compiled code: the 30th and 40th calls run in the JIT
t { 30.times { |i| i == 29 ? nilv.jitted : i } }
def hot(x) = x.hotter
t { 40.times { |i| hot(i == 39 ? nil : 1) rescue raise } }
class Integer; def hotter = self; end
t { 40.times { |i| hot(i == 39 ? nil : 1) } }
def arity_hot(a) = a
t { 40.times { |i| i == 39 ? arity_hot : arity_hot(1) } }

# --- the method itself
t { ArgumentError.new("wrong number of arguments (given 1, expected 0)").detailed_message }
t { puts ArgumentError.new("plain never raised").detailed_message }
t { puts NameError.new("never raised", :x).detailed_message(highlight: false) }
class MyErr < NameError
  def detailed_message(highlight: false, **) = "[my] " + super
end
t { raise MyErr, "mine" }
class MyErr2 < TypeError
  def detailed_message(**) = "own"
end
t { raise MyErr2, "mine2" }
t { nil.foo rescue puts($!.detailed_message(highlight: false, error_highlight: false)) }
t { nil.foo rescue puts($!.full_message(highlight: false)) }
t { nil.foo rescue puts($!.full_message(highlight: true).inspect) }
t { nil.foo rescue p($!.detailed_message(highlight: false).encoding, $!.message) }
t { nil.foo rescue p($!.backtrace_locations[0].absolute_path == File.realpath(__FILE__)) }

# --- ErrorHighlight.spot
t { nil.foo rescue p(ErrorHighlight.spot($!).except(:script_lines)) }
t { nil.foo rescue p(ErrorHighlight.spot($!)[:script_lines].size == File.readlines(__FILE__).size) }
t { nil.foo rescue p(ErrorHighlight.spot($!, point_type: :args)) }
t { nil.foo(1 + 2) rescue p(ErrorHighlight.spot($!, point_type: :args)&.except(:script_lines)) }
def two(x) = x.bar(42)
t { two(nil) rescue p(ErrorHighlight.spot($!, backtrace_location: $!.backtrace_locations[1])&.except(:script_lines)) }
t { two(nil) rescue p(ErrorHighlight.spot($!, backtrace_location: lib_loc)&.except(:script_lines)) }
t { p ErrorHighlight.spot(RuntimeError.new) }
t { p ErrorHighlight.spot(RuntimeError.new, backtrace_location: 1) }

# --- the formatter
t { p ErrorHighlight.formatter, ErrorHighlight::DefaultFormatter.max_snippet_width }
ErrorHighlight::DefaultFormatter.max_snippet_width = 30
t { aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa = nil; bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb = 1; aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa.foooooooooooooo + bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb }
ErrorHighlight::DefaultFormatter.max_snippet_width = nil
fmt = Class.new { def self.message_for(spot) = "\n<<#{spot[:first_column]}..#{spot[:last_column]}>>" }
ErrorHighlight.formatter = fmt
t { nil.custom_fmt }
ErrorHighlight.formatter = ErrorHighlight::DefaultFormatter

# --- what loading it leaves behind
t { p ErrorHighlight::VERSION, ErrorHighlight.constants.sort }
t { p TypeError.ancestors.first(2), ArgumentError.ancestors.first(2), NameError.ancestors.first(2) }
t { p TypeError.instance_method(:detailed_message).owner }
