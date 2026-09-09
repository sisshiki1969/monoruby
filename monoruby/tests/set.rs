extern crate monoruby;
use monoruby::tests::*;

#[test]
fn set() {
    run_tests(&[
        r##"
    s = Set.new
    s.add(1)
    s.add(2)
    s.add(1)
    s.size
    "##,
        r##"
    s = Set[1, 2, 3]
    s.to_a.sort
    "##,
        r##"
    s = Set[1, 2, 3]
    [s.include?(1), s.include?(4), s.member?(2)]
    "##,
        r##"
    s = Set[1, 2, 3]
    s.delete(2)
    s.to_a.sort
    "##,
        r##"
    s = Set[1, 2]
    [s.add?(3).nil?, s.add?(1).nil?]
    "##,
        r##"
    s = Set[1, 2, 3]
    [s.delete?(2).nil?, s.delete?(5).nil?]
    "##,
        r##"
    s = Set[3, 1, 2]
    a = []
    s.each { |x| a << x }
    a.sort
    "##,
        r##"
    s = Set[1, 2]
    res = [s.empty?]
    s.clear
    res << s.empty?
    res << s.size
    res
    "##,
        r##"
    a = Set[1, 2]
    b = Set[2, 3]
    (a | b).to_a.sort
    "##,
        r##"
    a = Set[1, 2]
    b = Set[2, 3]
    (a + b).to_a.sort
    "##,
        r##"
    a = Set[1, 2, 3]
    b = Set[2, 3, 4]
    (a & b).to_a.sort
    "##,
        r##"
    a = Set[1, 2, 3]
    b = Set[2, 3, 4]
    (a - b).to_a.sort
    "##,
        r##"
    a = Set[1, 2, 3]
    b = Set[2, 3, 4]
    (a ^ b).to_a.sort
    "##,
        r##"
    a = Set[1, 2]
    b = Set[1, 2, 3]
    [a.subset?(b), b.subset?(a), b.superset?(a), a.superset?(b)]
    "##,
        r##"
    a = Set[1, 2, 3]
    b = Set[3, 2, 1]
    c = Set[1, 2]
    [a == b, a == c, a == 42]
    "##,
        r##"
    s = Set[1, 2]
    s.merge([2, 3, 4])
    s.to_a.sort
    "##,
        r##"
    s = Set.new
    s << 1 << 2 << 1
    s.to_a.sort
    "##,
        r##"
    s = Set[1, 2, 3, 4, 5]
    s.select { |x| x > 3 }.sort
    "##,
        r##"
    s = Set[1, 2, 3]
    s.to_set.equal?(s)
    "##,
        r##"
    s = Set.new([1, 2, 3]) { |x| x * 2 }
    s.to_a.sort
    "##,
        r##"
    s = Set[1, 2, 3]
    [s.size, s.length]
    "##,
    ]);
}

#[test]
fn set_subclass_with_a_wider_initialize() {
    // `Set.new` hands every argument to `initialize`, so a subclass may
    // widen the constructor (chunky_png's `Palette < Set` takes a second
    // decoding map); the arity check is `initialize`'s.
    run_test(
        r#"
        class Palette < Set
          attr_reader :map
          def initialize(enum, map = nil)
            super(enum.sort)
            @map = map
          end
        end
        pal = Palette.new([3, 1, 2], { 1 => :a })
        e = begin
          Palette.new([1], 2, 3)
        rescue ArgumentError => e
          e.message
        end
        [pal.to_a, pal.map, pal.class, pal.include?(2), Set.new([1, 1, 2]).size, Set.new.size, e,
         Set.new([1, 2]) { |x| x * 10 }.to_a]
        "#,
    );
}
