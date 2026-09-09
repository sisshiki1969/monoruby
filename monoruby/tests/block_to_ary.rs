extern crate monoruby;
use monoruby::tests::*;

// Block auto-splat through `#to_ary`, on every way a block can receive a
// lone non-Array argument: a `yield`, a builtin iterating (`map`, `each`,
// `to_h`), `Proc#call`. The object may answer `#to_ary` through
// `method_missing` + `respond_to_missing?` without having `respond_to?`
// at all (a `BasicObject` subclass: prism's `LexCompat::Token`, which is
// what `Ripper.lex(...).each { |pos, event, tok, state| }` destructures).

#[test]
fn to_ary_auto_splat_everywhere() {
    run_test(
        r##"
        class T
          def initialize(*a) = @a = a
          def to_ary = @a
        end
        class Bad; def to_ary = 42; end
        class NoAry; def to_a = [9, 8]; end
        class Tok < BasicObject
          def initialize(a) = @a = a
          def respond_to_missing?(name, include_private = false) = @a.respond_to?(name, include_private)
          def method_missing(name, ...) = @a.public_send(name, ...)
        end
        def y1; yield T.new(1, 2); end
        def y2; yield T.new(1, 2), 3; end
        r = []
        r << [T.new(1, 2)].map { |a, b| [a.class, b] }
        r << [T.new(1, 2)].each_with_index.map { |(a, b), i| [a, b, i] }
        r << y1 { |a, b| [a, b] } << y1 { |a| a.class }
        r << y2 { |a, b| [a.class, b] }
        r << [T.new(1, 2)].map { |a, *b| [a, b] } << [T.new(1, 2)].map { |*a| a.map(&:class) }
        r << [NoAry.new].map { |a, b| [a.class, b] }
        r << proc { |a, b| [a, b] }.call(T.new(5, 6))
        r << (begin; lambda { |a, b| [a, b] }.call(T.new(5, 6)); rescue ArgumentError => e; e.class; end)
        r << (begin; [Bad.new].map { |a, b| [a, b] }; rescue TypeError => e; e.message; end)
        r << [T.new(1, 2)].map { |a, b = :d| [a, b] } << [T.new(1)].map { |a, b| [a, b] }
        r << { T.new(1, 2) => 3 }.map { |(a, b), c| [a, b, c] }
        r << [T.new(:k, :v)].to_h { |a, b| [a, b] }
        t = Tok.new([[1, 0], :on_ident, "x"])
        r << [t].map { |a, b, c| [a, b, c] } << proc { |a, b| [a, b] }.call(t) << [t].to_h { |a, b| [a, b] }
        r << [t].each { |a, b| break [a, b] } << [t].map { |a, *rest| [a, rest.size] }
        plain = Class.new(BasicObject) { def initialize(x) = @x = x }.new(7)
        r << [plain].map { |a, b| [(a.equal?(plain)), b] }
        r
        "##,
    );
}
