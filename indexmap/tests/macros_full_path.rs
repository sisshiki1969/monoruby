#[test]
fn test_create_map() {
    struct E;
    struct G;
    let mut e = E;
    let mut g = G;
    let _m: indexmap::RubyMap<i32, i32, E, G, ()> = indexmap::indexmap! { &mut e; &mut g;
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
    let _s: indexmap::RubySet<i32, E, G, ()> = indexmap::indexset! { &mut e; &mut g;
        1,
        7,
        2,
        3,
    };
}
