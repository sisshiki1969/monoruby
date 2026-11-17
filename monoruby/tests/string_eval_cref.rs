extern crate monoruby;
use monoruby::tests::*;

// Issue #1972 (1): `obj.instance_eval("...")` does not create the
// receiver's singleton class up front. The body's cref is the receiver's
// class "for definition" (CRuby's `CREF_CLASS_FOR_DEFINITION`): only a
// `def`, a constant assignment or a `class` / `module` statement in the
// string makes the singleton class, so an Integer or a Symbol can be the
// receiver of a string that defines nothing, and `Module.nesting` shows
// the class until then.
#[test]
fn string_instance_eval_creates_the_singleton_class_lazily() {
    run_test2(
        r##"
        # The rows are collected instead of printed: the harness compares
        # the value of the whole script.
        $o = []
        def row(*a) = a.each { |x| $o << x.to_s }
        def rowp(*a) = a.each { |x| $o << x.inspect }
        def t; yield.inspect.gsub(/0x\h+/, "0x"); rescue Exception => e; "#{e.class}: #{e.message}".gsub(/0x\h+/, "0x"); end
        def attrs; yield; rescue NameError => e; [e.class, e.message, e.name, (e.receiver rescue :no_receiver)].inspect; end
        class C; end
        # 1 string-form instance_eval
        row t { o = Object.new; o.instance_eval("foo") }
        row t { o = Object.new; o.instance_eval("1"); o.foo }
        row t { o = Object.new; o.instance_eval("Module.nesting") }
        row t { 1.instance_eval("XX_IE = 1"); Integer.const_defined?(:XX_IE) }
        # 2 singleton class of a Range / Complex
        row t { (1..2).singleton_class }
        row t { Complex(1, 2).singleton_class }
        # 3 names that are not constant / class variable / attribute names
        row attrs { C.send(:remove_const, :nope) }
        row attrs { C.send(:remove_class_variable, :nope) }
        row attrs { C.const_get(:nope) }
        row attrs { C.const_set(:bad, 1) }
        row attrs { C.send(:attr_accessor, "bad name") }
        # 4 explicit const_missing
        begin; Object.const_missing(:Zork); rescue NameError => e; rowp e.backtrace.size; end
        # 5 Hash key copy
        row t { h = {"k".dup => 1}; h.keys[0].equal?(-"k") }
        # 6
        $o
        "##,
    );
}

// Constants, class variables, nested definitions and blocks in the string.
#[test]
fn string_instance_eval_scopes() {
    run_test2(
        r##"
        # The rows are collected instead of printed: the harness compares
        # the value of the whole script.
        $o = []
        def row(*a) = a.each { |x| $o << x.to_s }
        def rowp(*a) = a.each { |x| $o << x.inspect }
        def t; yield.inspect.gsub(/0x\h+/, "0x"); rescue Exception => e; "#{e.class}: #{e.message}".gsub(/0x\h+/, "0x"); end
        class K; KC = :kc; @@kcv = :kcv; def self.cv = @@kcv; end
        TOP = :top
        row t { o = Object.new; o.instance_eval("foo") }
        row t { o = Object.new; o.instance_eval("1"); o.foo }
        row t { o = Object.new; o.instance_eval("Module.nesting") }
        row t { o = Object.new; o.singleton_class; o.instance_eval("Module.nesting") }
        row t { o = Object.new; o.instance_eval("def zz; end; Module.nesting") }
        row t { o = Object.new; o.instance_eval("[Module.nesting, singleton_class, Module.nesting]") }
        row t { 1.instance_eval("XX_IE = 1"); Integer.const_defined?(:XX_IE) }
        row t { :s.instance_eval("XX_IE2 = 1") }
        row t { 1.instance_eval("def foo; end") }
        row t { 1.instance_eval("class Foo1; end") }
        row t { 1.instance_eval("self + 1") }
        row t { 1.instance_eval("Module.nesting") }
        row t { 1.instance_eval("MAX rescue :nomax") }
        row t { 1.5.instance_eval("[EPSILON > 0, DIG]") }
        row t { o = K.new; o.instance_eval("[KC, TOP]") }
        row t { o = K.new; o.instance_eval("KD = 5; [KD, singleton_class.constants, K.constants.sort, singleton_class.const_get(:KD)]") }
        row t { o = K.new; o.instance_eval("KD = 5"); [o.singleton_class.constants, (K::KD rescue :none)] }
        row t { o = K.new; o.instance_eval("class Inner; def hi = :hi; end; [Inner.name, Inner.new.hi, singleton_class.constants]") }
        row t { o = K.new; o.instance_eval("module InM; end; InM.name") }
        row t { o = K.new; o.instance_eval("def sm = KC; sm") }
        row t { o = K.new; o.instance_eval("def sm2 = Module.nesting; sm2") }
        row t { o = K.new; o.instance_eval("KE = 7; def sm3 = KE; sm3") }
        row t { o = K.new; o.instance_eval("def sm4 = KF; KF = 8; sm4") }
        row t { o = K.new; o.instance_eval("@@kcv") }
        row t { o = K.new; o.instance_eval("@@new_cv = 1") ; K.class_variables.sort }
        row t { Object.class_variables }
        row t { o = K.new; o.instance_eval("defined?(KC)") }
        row t { o = K.new; o.instance_eval("defined?(NOPE)") }
        row t { o = K.new; o.instance_eval("self.class") }
        row t { o = K.new; o.instance_eval("singleton_methods") }
        row t { o = K.new; o.instance_eval("def a1; end; def b1; end; singleton_methods.sort") }
        row t { o = K.new; o.instance_eval("alias_method :x, :y rescue $!.class") }
        row t { o = K.new; o.instance_eval("alias ins inspect; ins.class") }
        row t { o = K.new; o.instance_eval("[1].each { KG = 3 }; [singleton_class.constants, (K::KG rescue :none), (Object.const_defined?(:KG))]") }
        row t { o = K.new; o.instance_eval("[1].map { Module.nesting }") }
        row t { o = K.new; o.instance_eval("x = 5; binding.local_variable_defined?(:x)") }
        row t { "str".instance_eval("size") }
        row t { "lit".freeze.instance_eval("def q; end") }
        row t { :sym.instance_eval("to_proc.call('a') rescue 1") }
        row t { nil.instance_eval("def nilm = 1; nilm") }
        row t { nil.instance_eval("NILC = 1; NilClass::NILC") }
        row t { K.instance_eval("def cm = 1; [cm, singleton_methods.sort, Module.nesting]") }
        row t { K.instance_eval("KH = 2; [K.singleton_class.constants.include?(:KH), K.constants.include?(:KH)]") }
        row t { K.instance_eval("@@kcv") }
        row t { K.class_eval("@@kcv") }
        row t { K.class_eval("KI = 3; Module.nesting") }
        row t { o = K.new; o.instance_eval("instance_eval('Module.nesting')") }
        row t { o = K.new; o.instance_eval("K.class_eval('Module.nesting')") }
        row t { o = Object.new; 3.times { o.instance_eval("1 + 1") }; o.singleton_methods }
        row t { o = Object.new; o.instance_eval("nope_method") }
        row t { o = Object.new; o.instance_eval { nope_method2 } }
        row t { r = (1..2); r.instance_eval("def rr; end") }
        row t { r = (1..2); r.instance_eval("RC = 1") }
        row t { o = Object.new.freeze; o.instance_eval("def fz; end") }
        row t { o = Object.new.freeze; o.instance_eval("FZ = 1; singleton_class.frozen?") }
        $o
        "##,
    );
}

// A block written inside a string eval body sees the body's lexical
// scope (`K.class_eval("[1].map { KC }")`), and assigning a constant or
// opening a class in a frozen class raises FrozenError.
#[test]
fn string_eval_blocks_and_frozen_targets() {
    run_test2(
        r##"
        # The rows are collected instead of printed: the harness compares
        # the value of the whole script.
        $o = []
        def row(*a) = a.each { |x| $o << x.to_s }
        def rowp(*a) = a.each { |x| $o << x.inspect }
        def t; yield.inspect.gsub(/0x\h+/, "0x"); rescue Exception => e; "#{e.class}: #{e.message}".gsub(/0x\h+/, "0x"); end
        class K; KC = :kc; end
        class Fz; end; Fz.freeze
        row t { class Fz; FX = 1; end }
        row t { Fz.const_set(:FY, 1) }
        row t { Fz.class_eval("FZ = 1") }
        row t { K.class_eval("[1].each { KI2 = 1 }; [K.constants.sort, Object.const_defined?(:KI2)]") }
        row t { K.class_eval("[1].map { Module.nesting }") }
        row t { K.class_eval("[1].map { KC }") }
        row t { K.class_eval("[1].each { def in_blk = 1 }; K.instance_methods(false)") }
        row t { K.class_eval("[1].each { class KInner; end }; K.constants.sort") }
        row t { K.new.instance_eval("[1].map { KC }") }
        row t { K.new.instance_eval("[1].map { [1].map { Module.nesting } }") }
        row t { K.class_eval { Module.nesting } }
        row t { K.class_eval { KJ = 1 }; Object.const_defined?(:KJ) }
        row t { eval("[1].map { Module.nesting }") }
        row t { K.class_eval("eval('[1].map { Module.nesting }')") }
        row t { K.class_eval("proc { Module.nesting }").call }
        row t { K.class_eval("lambda { KL = 1 }").call; K.const_defined?(:KL, false) }
        row t { K.class_eval("def mm = [1].map { Module.nesting }; self").new.mm }
        $o
        "##,
    );
}

// Constant assignment, `class` / `module` and `remove_const` on a frozen Module.
#[test]
fn frozen_module_refuses_constants() {
    run_test2(
        r##"
        # The rows are collected instead of printed: the harness compares
        # the value of the whole script.
        $o = []
        def row(*a) = a.each { |x| $o << x.to_s }
        def rowp(*a) = a.each { |x| $o << x.inspect }
        def t; yield.inspect.gsub(/0x\h+/, "0x"); rescue Exception => e; "#{e.class}: #{e.message}".gsub(/0x\h+/, "0x"); end
        class Fz; end; Fz.freeze
        row t { Fz.frozen? }
        row t { Fz.const_set(:FY, 1) }
        row t { class Fz; FX = 1; end }
        row t { Fz.class_eval("FZ = 1") }
        row t { Fz.class_eval { def zz; end } }
        row t { Fz::FW = 1 }
        row t { Fz.constants }
        m = Module.new.freeze
        row t { m.const_set(:A, 1) }
        row t { m::B = 1 }
        row t { class Fz; class Inner; end; end }
        row t { class Fz; module InnerM; end; end }
        row t { Fz.class_variable_set(:@@a, 1) }
        row t { Fz.send(:remove_const, :Nope) }
        row t { Fz.autoload(:AL, "x") }
        $o
        "##,
    );
}
