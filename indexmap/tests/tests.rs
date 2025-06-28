use indexmap::{indexmap, indexset, RubyMap};
struct E;
struct G;

#[test]
fn test_sort() {
    let mut e = E;
    let mut g = G;
    let m: RubyMap<_, _, E, G, ()> = indexmap! {
        &mut e; &mut g;
        1 => 2,
        7 => 1,
        2 => 2,
        3 => 3,
    };

    itertools::assert_equal(
        m.sorted_by(|_k1, v1, _k2, v2| v1.cmp(v2)),
        vec![(7, 1), (1, 2), (2, 2), (3, 3)],
    );
}

#[test]
fn test_sort_set() {
    let mut e = E;
    let mut g = G;
    let s: indexmap::RubySet<_, E, G, ()> = indexset! {
        &mut e; &mut g;
        1,
        7,
        2,
        3,
    };

    itertools::assert_equal(s.sorted_by(|v1, v2| v1.cmp(v2)), vec![1, 2, 3, 7]);
}
