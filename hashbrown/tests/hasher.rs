//! Sanity check that alternate hashers work correctly.

#![cfg(not(miri))] // FIXME: takes too long

use hashbrown::HashSet;
use std::hash::{BuildHasher, BuildHasherDefault, Hasher};

struct E;
struct G;

fn check<S: BuildHasher + Default>() {
    let range = 0..1_000;
    let mut e = E;
    let mut g = G;

    let mut set = HashSet::<i32, E, G, (), S>::default();
    set.extend(range.clone(), &mut e, &mut g).unwrap();

    assert!(!set.contains(&i32::MIN, &mut e, &mut g).unwrap());
    assert!(!set.contains(&(range.start - 1), &mut e, &mut g).unwrap());
    for i in range.clone() {
        assert!(set.contains(&i, &mut e, &mut g).unwrap());
    }
    assert!(!set.contains(&range.end, &mut e, &mut g).unwrap());
    assert!(!set.contains(&i32::MAX, &mut e, &mut g).unwrap());
}

/// Use hashbrown's default hasher.
#[test]
fn default() {
    check::<hashbrown::DefaultHashBuilder>();
}

/// Use std's default hasher.
#[test]
fn random_state() {
    check::<std::collections::hash_map::RandomState>();
}

/// Use a constant 0 hash.
#[test]
fn zero() {
    #[derive(Default)]
    struct ZeroHasher;

    impl Hasher for ZeroHasher {
        fn finish(&self) -> u64 {
            0
        }
        fn write(&mut self, _: &[u8]) {}
    }

    check::<BuildHasherDefault<ZeroHasher>>();
}

/// Use a constant maximum hash.
#[test]
fn max() {
    #[derive(Default)]
    struct MaxHasher;

    impl Hasher for MaxHasher {
        fn finish(&self) -> u64 {
            u64::MAX
        }
        fn write(&mut self, _: &[u8]) {}
    }

    check::<BuildHasherDefault<MaxHasher>>();
}
