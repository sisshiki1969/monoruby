use rubymap::{RubyMap, RubySet};

#[test]
fn test_create_map() {
    struct E;
    struct G;
    let mut e = E;
    let mut g = G;
    let _m: RubyMap<i32, i32, E, G, ()> = rubymap::indexmap! { &mut e; &mut g;
        1 => 2,
        7 => 1,
        2 => 2,
        3 => 3,
    };
}

#[test]
fn test_create_set() {
    struct E;
    struct G;
    let mut e = E;
    let mut g = G;
    let _s: RubySet<i32, E, G, ()> = rubymap::indexset! { &mut e; &mut g;
        1,
        7,
        2,
        3,
    };
}
