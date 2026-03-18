extern crate monoruby;
use monoruby::tests::*;

#[test]
fn set_basic() {
    run_test(
        r##"
    s = Set.new
    s.add(1)
    s.add(2)
    s.add(1)
    s.size
    "##,
    );
}

#[test]
fn set_bracket() {
    run_test(
        r##"
    s = Set[1, 2, 3]
    s.to_a.sort
    "##,
    );
}

#[test]
fn set_include() {
    run_test(
        r##"
    s = Set[1, 2, 3]
    [s.include?(1), s.include?(4), s.member?(2)]
    "##,
    );
}

#[test]
fn set_delete() {
    run_test(
        r##"
    s = Set[1, 2, 3]
    s.delete(2)
    s.to_a.sort
    "##,
    );
}

#[test]
fn set_add_question() {
    run_test(
        r##"
    s = Set[1, 2]
    [s.add?(3).nil?, s.add?(1).nil?]
    "##,
    );
}

#[test]
fn set_delete_question() {
    run_test(
        r##"
    s = Set[1, 2, 3]
    [s.delete?(2).nil?, s.delete?(5).nil?]
    "##,
    );
}

#[test]
fn set_each() {
    run_test(
        r##"
    s = Set[3, 1, 2]
    a = []
    s.each { |x| a << x }
    a.sort
    "##,
    );
}

#[test]
fn set_empty_and_clear() {
    run_test(
        r##"
    s = Set[1, 2]
    res = [s.empty?]
    s.clear
    res << s.empty?
    res << s.size
    res
    "##,
    );
}

#[test]
fn set_union() {
    run_test(
        r##"
    a = Set[1, 2]
    b = Set[2, 3]
    (a | b).to_a.sort
    "##,
    );
    run_test(
        r##"
    a = Set[1, 2]
    b = Set[2, 3]
    (a + b).to_a.sort
    "##,
    );
}

#[test]
fn set_intersection() {
    run_test(
        r##"
    a = Set[1, 2, 3]
    b = Set[2, 3, 4]
    (a & b).to_a.sort
    "##,
    );
}

#[test]
fn set_difference() {
    run_test(
        r##"
    a = Set[1, 2, 3]
    b = Set[2, 3, 4]
    (a - b).to_a.sort
    "##,
    );
}

#[test]
fn set_xor() {
    run_test(
        r##"
    a = Set[1, 2, 3]
    b = Set[2, 3, 4]
    (a ^ b).to_a.sort
    "##,
    );
}

#[test]
fn set_subset_superset() {
    run_test(
        r##"
    a = Set[1, 2]
    b = Set[1, 2, 3]
    [a.subset?(b), b.subset?(a), b.superset?(a), a.superset?(b)]
    "##,
    );
}

#[test]
fn set_equality() {
    run_test(
        r##"
    a = Set[1, 2, 3]
    b = Set[3, 2, 1]
    c = Set[1, 2]
    [a == b, a == c, a == 42]
    "##,
    );
}

#[test]
fn set_merge() {
    run_test(
        r##"
    s = Set[1, 2]
    s.merge([2, 3, 4])
    s.to_a.sort
    "##,
    );
}

#[test]
fn set_shovel() {
    run_test(
        r##"
    s = Set.new
    s << 1 << 2 << 1
    s.to_a.sort
    "##,
    );
}

#[test]
fn set_enumerable() {
    run_test(
        r##"
    s = Set[1, 2, 3, 4, 5]
    s.select { |x| x > 3 }.sort
    "##,
    );
}

#[test]
fn set_to_set() {
    run_test(
        r##"
    s = Set[1, 2, 3]
    s.to_set.equal?(s)
    "##,
    );
}

#[test]
fn set_init_with_block() {
    run_test(
        r##"
    s = Set.new([1, 2, 3]) { |x| x * 2 }
    s.to_a.sort
    "##,
    );
}

#[test]
fn set_length() {
    run_test(
        r##"
    s = Set[1, 2, 3]
    [s.size, s.length]
    "##,
    );
}
