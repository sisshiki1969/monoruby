extern crate monoruby;
use monoruby::tests::*;

// A method is compiled once per class of `self`, and its entry tries the
// compiled classes' guards in turn. Once there are
// `CLASS_DISPATCH_THRESHOLD` of them the entry looks the class up in a
// table instead (`globals/store/class_dispatch.rs`). The cases here give
// one method enough self classes for that, warm each class in turn so
// that each gets its own body, and then enter the method by every route:
// a generic send, the interpreter, a native iterator's block invoker.

/// Calls per class while warming: enough for the stub at the end of the
/// chain to count the class hot twice.
const WARM: usize = 40;

#[test]
fn inherited_method_with_many_self_classes() {
    run_test_once(&format!(
        r#"
        class CdBase
          def initialize(a) = @a = a
          def tag = [self.class.name, @a]
        end
        classes = 40.times.map {{ |i| Object.const_set("CdSub#{{i}}", Class.new(CdBase)) }}
        objs = classes.each_with_index.map {{ |c, i| c.new(i) }}
        def cd_call(o) = o.tag
        objs.each {{ |o| {WARM}.times {{ cd_call(o) }} }}
        r = []
        3.times {{ objs.each {{ |o| r << cd_call(o) }} }}
        r
        "#
    ));
}

#[test]
fn class_new_with_many_receivers() {
    // `Class#new` is a Ruby method, so its self classes are the singleton
    // classes of everything that is instantiated.
    run_test_once(&format!(
        r#"
        class CdNode
          attr_reader :a, :b
          def initialize(a, b = nil) = (@a = a; @b = b)
        end
        classes = 40.times.map {{ Class.new(CdNode) }}
        def cd_make(k, i) = k.new(i, i + 1)
        def cd_fwd(k, ...) = k.new(...)
        classes.each {{ |k| {WARM}.times {{ |i| cd_make(k, i); cd_fwd(k, i) }} }}
        r = []
        3.times do
          classes.each_with_index do |k, i|
            o = cd_make(k, i)
            p = cd_fwd(k, i)
            r << [classes.index(o.class), o.a, o.b, classes.index(p.class), p.a, p.b]
          end
        end
        r
        "#
    ));
}

#[test]
fn immediates_and_heap_numbers_take_the_chain() {
    // The table holds the classes whose instances are all heap objects.
    // An immediate `self`, and a heap Integer or Float, go down the chain,
    // where the Integer guard sends a bignum to the interpreter.
    run_test_once(&format!(
        r#"
        class Object
          def cd_kind = [self.class, self.frozen?, self]
        end
        class CdK0; end; class CdK1; end; class CdK2; end; class CdK3; end
        class CdK4; end; class CdK5; end; class CdK6; end; class CdK7; end
        def cd_kind_of(o) = o.cd_kind[0, 2]
        heap = [CdK0, CdK1, CdK2, CdK3, CdK4, CdK5, CdK6, CdK7].map(&:new)
        vals = [1, nil, :sym, true, false, 1.5, 2**70, -(2**70), 1.0e300, "s", [1], {{a: 1}}, 1..2]
        (heap + vals + heap).each {{ |o| {WARM}.times {{ cd_kind_of(o) }} }}
        r = []
        3.times {{ (vals + heap + vals.reverse).each {{ |o| r << cd_kind_of(o) }} }}
        r
        "#
    ));
}

#[test]
fn classes_compiled_after_the_table_exists() {
    // More classes than the table's first size, met a few at a time: the
    // table grows, and a class it does not hold yet is counted and
    // compiled from the end of the chain.
    run_test_once(&format!(
        r#"
        class CdGrow
          def initialize(a) = @a = a
          def get = @a * 2
        end
        def cd_get(o) = o.get
        r = []
        objs = []
        12.times do |round|
          8.times {{ |i| objs << Class.new(CdGrow).new(round * 8 + i) }}
          objs.last(8).each {{ |o| {WARM}.times {{ cd_get(o) }} }}
          objs.each {{ |o| r << cd_get(o) }}
        end
        r.sum
        "#
    ));
}

#[test]
fn block_with_many_self_classes_from_a_native_iterator() {
    // A block is compiled per class of the `self` it closes over, and a
    // native iterator enters it through the block invoker.
    run_test_once(&format!(
        r#"
        class CdIter
          def initialize(a) = @a = a
          def pick(h) = h.reject {{ |k, v| v < @a }}.keys
        end
        objs = 12.times.map {{ |i| Class.new(CdIter).new(i % 4) }}
        h = {{a: 0, b: 1, c: 2, d: 3}}
        objs.each {{ |o| {WARM}.times {{ o.pick(h) }} }}
        r = []
        3.times {{ objs.each {{ |o| r << o.pick(h) }} }}
        r
        "#
    ));
}

#[test]
fn recompiled_class_is_entered_through_the_table() {
    // A definition that changes what a compiled body resolved makes it
    // recompile; the table has to enter the new body.
    run_test_once(&format!(
        r#"
        class CdRe
          def helper = 1
          def run = helper + 10
        end
        objs = 12.times.map {{ Class.new(CdRe).new }}
        def cd_run(o) = o.run
        objs.each {{ |o| {WARM}.times {{ cd_run(o) }} }}
        r = [objs.map {{ |o| cd_run(o) }}]
        class CdRe
          def helper = 2
        end
        objs.each {{ |o| {WARM}.times {{ cd_run(o) }} }}
        r << objs.map {{ |o| cd_run(o) }}
        objs[3].class.class_eval {{ def helper = 3 }}
        objs.each {{ |o| {WARM}.times {{ cd_run(o) }} }}
        r << objs.map {{ |o| cd_run(o) }}
        r
        "#
    ));
}

#[test]
fn basic_op_redefinition_drops_the_table() {
    // Redefining an operator the bodies inlined evicts the method's code
    // and puts the warm-up stub back at its entry; the classes then
    // compile again and a new table is built.
    run_test_once(&format!(
        r#"
        class CdBop
          def initialize(a) = @a = a
          def calc = @a + 1
        end
        objs = 12.times.map {{ |i| Class.new(CdBop).new(i) }}
        def cd_calc(o) = o.calc
        objs.each {{ |o| {WARM}.times {{ cd_calc(o) }} }}
        r = [objs.map {{ |o| cd_calc(o) }}]
        class Integer
          alias cd_plus +
          def +(o) = cd_plus(o).cd_plus(100)
        end
        r << objs.map {{ |o| cd_calc(o) }}
        objs.each {{ |o| {WARM}.times {{ cd_calc(o) }} }}
        r << objs.map {{ |o| cd_calc(o) }}
        r
        "#
    ));
}
